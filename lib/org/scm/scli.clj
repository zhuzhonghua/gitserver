(ns org.scm.scli)

(require '[clojure.string :as str]
         '[clojure.java.io :as io]
         '[org.scm.scommon :refer :all])

(import '[java.net DatagramSocket DatagramPacket InetAddress]
        '[java.util Scanner]
        '[java.net SocketTimeoutException])

(def send-serial (atom 0))

(def client (atom nil))

(def client-sock (atom nil))

(defn init-client [addr port]
  (reset! client-sock (DatagramSocket.))
  (.setSoTimeout @client-sock 5000)

  (reset! client {:addr (InetAddress/getByName addr)
                  :port port
                  :recvmaxserial (atom 0)
                  :lastrecvtime (atom 0)
                  :partialin (atom nil)
                  :partialoffset (atom 0)
                  :inqueue (make-queue)
                  :outqueue (make-queue)
                  :sending (atom nil)
                  :lastsendtime (atom 0)}))

(defn send-cmd [cmd]
  (println "->scli sending cmd[" cmd "]")
  (send-data @client-sock send-serial @client (.getBytes cmd)))

(defn handle-cmd [client cmd]
;;  (println "received" cmd)
  )

(defn check-resending []
  (when (and (not (nil? @(:sending @client)))
             (> (- (get-sys-time) @(:lastsendtime @client))
                max-sending-wait-time))
        (resending @client-sock @client)))

(defn thread-read-input []
  (-> (Thread.
       (fn [] (let [input (Scanner. System/in)]
                (while true
                  (let [cmd (.nextLine input)]
                    (when (.equals "QUIT" cmd)
                      (System/exit 0))

                    (send-cmd cmd))))))
      (.start)))

(defn thread-recv-sock []
  (-> (Thread.
       (fn [] (let [bs (bytes (byte-array MAXLEN))
                    packet (DatagramPacket. bs (alength bs))]
                   (while true
                     (try
                       (.receive @client-sock packet)

                       (recv-packet @client-sock @client
                                    (.getData packet) (.getLength packet))

                       (while (not (empty-queue? (:inqueue @client)))
                         (handle-cmd client (poll-queue (:inqueue @client))))

                       (catch SocketTimeoutException e
                         ;; do nothing
                         (Thread/sleep 1000)
                         ))))))
      (.start)))

(defn init [addr port]
  (init-client addr port)
  (thread-read-input)
  (thread-recv-sock)

  (-> (Thread.
       (fn [] (while true
                (check-resending)
                (Thread/sleep 1000))))
      (.start)))
