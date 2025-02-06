(ns org.scm.ssrv)

(require '[clojure.string :as str]
         '[clojure.java.io :as io]
         '[org.scm.scommon :refer :all]
         '[org.scm.sgit :refer :all])

(import '[java.util.concurrent ConcurrentLinkedQueue]
        '[java.net SocketTimeoutException]
        '[java.net DatagramSocket DatagramPacket InetAddress])

(def ^:const tag "[ssrv]")

;; 60 seconds
(def ^:const max-idle-time (* 60 1000 1000))

;; 10 seconds
(def ^:const server-sock-recv-timeout (* 10 1000))

(def cmd-handlers (atom {}))

(defn reg-cmd-handler [cmd handler]
  (println tag "reg cmd handler" cmd)
  (swap! cmd-handlers assoc cmd handler))

(defmacro defcmd [cmd arg & body]
  `(reg-cmd-handler ~cmd (fn ~arg (do ~@body))))

;; all clients
(def clients (atom {}))

(def send-serial (atom 0))

(def server-sock (atom nil))

(defn client-str [client]
  (str (:addr client) ":" (:port client)))

(defn client-key [addr port]
  (str addr ":" port))

(defn send-cmd [client cmd]
  (send-data @server-sock send-serial client (.getBytes cmd)))

(defn new-client [^InetAddress addr port]
  (println tag "new client" addr port)
  (let [client {:addr addr
                :port port
                :recvmaxserial (atom 0)
                :lastrecvtime (atom 0)
                :partialin (atom nil)
                :partialoffset (atom 0)
                :inqueue (make-queue)
                :outqueue (make-queue)
                :sending (atom nil)
                :lastsendtime (atom 0)}]
    (swap! clients assoc (client-key addr port) client)
    client))

(defn remove-client [addr port]
  (swap! clients dissoc (client-key addr port)))

(defn handle-cmd [client cmd]
  (println tag "handle cmd" cmd)
  (if-let [handler (get @cmd-handlers (nth cmd 0))]
    (apply handler (list client cmd))
    (println tag "unknown cmd" (client-str client) cmd)))

(defn put-in-queue [client cmd]
  (offer-queue (:inqueue client) cmd))

(defn handle-inqueue [client]
  (let [inqueue (:inqueue client)]
    (loop [size (size-queue inqueue)]
      (when (> size 0)
        (handle-cmd client (poll-queue inqueue))
        (recur (- size 1))))))

(defn check-resending [client]
  (when (and (not (nil? @(:sending client)))
             (> (- (get-sys-time) @(:lastsendtime client))
                max-sending-wait-time))
    (resending @server-sock client)))

(defn check-remove-client [client]
  (when (and (empty-queue? (:inqueue client))
             (empty-queue? (:outqueue client))
             (nil? @(:sending client))
             (> (- (get-sys-time) @(:lastrecvtime client)) max-idle-time)
             (> (- (get-sys-time) @(:lastsendtime client)) max-idle-time))
    (remove-client (:addr client) (:port client))))

(defn iterate-clients []
  (doseq [[k v] @clients]
    (handle-inqueue v)
    (check-resending v)
    (check-remove-client v)))

(defn handle-packet [packet]
  (let [addr (.getAddress packet)
        cport (.getPort packet)]
    (if-let [client (get @clients (client-key addr cport))]
      (recv-packet @server-sock client
                   (.getData packet)
                   (.getLength packet))

      (recv-packet @server-sock (new-client addr cport)
                   (.getData packet)
                   (.getLength packet)))))

(defn thread-recv-server-sock [port]
  (-> (Thread. (fn []
                 (reset! server-sock (DatagramSocket. port))
                 (.setSoTimeout @server-sock server-sock-recv-timeout)
                 (println tag "git server listen on udp" port)

                 (let [bs (bytes (byte-array MAXLEN))
                       packet (DatagramPacket. bs (alength bs))]
                   (while true
                     (try
                       (.receive @server-sock packet)
                       (handle-packet packet)

                       (catch SocketTimeoutException e
                         ;; do nothing
                         (Thread/sleep 1000)
                         ))))))
      (.start)))

(defn init [port gitpath]
  (sgit-init-repo gitpath)
  (thread-recv-server-sock port)

  (-> (Thread. (fn []
                 (while true
                   (iterate-clients)
                   (Thread/sleep 200))))
      (.start)))
