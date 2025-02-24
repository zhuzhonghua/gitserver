(ns org.scm.snet)

(require '[clojure.string :as str])

(import '[java.nio.channels Selector]
        '[java.nio.channels ServerSocketChannel]
        '[java.nio.channels SelectionKey]
        '[java.nio ByteBuffer]
        '[java.net InetSocketAddress]
        '[java.util.concurrent ConcurrentLinkedQueue])

(def conn-id (atom 1))

(defn new-client [channel selector]
  {:id (swap! conn-id inc)
   :addr (.getRemoteAddress channel)
   :channel channel
   :selector selector
   :read-state (atom 1) ;; 1 : read len 2 : read body
   :continue-read (atom true)
   :len-buf (ByteBuffer/allocate 4)
   :msg-buf (atom nil)
   :inqueue (ConcurrentLinkedQueue.)
   :outqueue (ConcurrentLinkedQueue.)
   :lastrecvtime (atom 0)
   :lastsendtime (atom 0)})

(defn send-to [client bytes-arr]
  (let [to-bytes (ByteBuffer/allocate (+ 4 (alength bytes-arr)))]
    (.putInt to-bytes (alength bytes-arr))
    (.put to-bytes bytes-arr)
    (.offer (:outqueue client) (.flip to-bytes))
    (.register (:channel client) (:selector client)
               (bit-or SelectionKey/OP_READ SelectionKey/OP_WRITE)
               client)))

(defn client-id [inst]
  (str "[:id " (:id inst) " :addr " (:addr inst) "]"))

(defn err-throw [& args]
  (apply println arg)
  (throw (Exception. (str/join " " arg))))

(defn err-cb [key inst fn-err]
  (.cancel key)
  (when inst
    (fn-err inst)))

;;(defmacro assert-throw [stat inst & args]
;;  `(when (not ~stat)
;;     (err-throw ~inst ~@args)))

(defn handle-accept [selector key srvchannel fn-new-conn]
  (let [client (.accept srvchannel)]
    (.configureBlocking client false)
    (let [inst (new-client client selector)]
      (.register client selector SelectionKey/OP_READ inst)
      (fn-new-conn inst))))

(defn read-len [key channel inst fn-err]
  (let [len-buf (:len-buf inst)
        rcount (.read channel len-buf)]
    (cond (< rcount 0)
          (do (reset! (:continue-read inst) false)
              (err-cb key inst fn-err)
              (err-throw "read-len count < 0" rcount (client-id inst))
              )

          (not (.hasRemaining len-buf))
          (do (reset! (:continue-read inst) true)
              (.flip len-buf)
              (let [len (.getInt len-buf)]
                (cond (<= len 0)
                      (do (reset! (:continue-read inst) false)
                          (err-cb key inst fn-err)
                          (err-throw "msg len less than or equal 0" len (client-id inst))
                          )

                      (>= len 65535)
                      (do (reset! (:continue-read inst) false)
                          (err-cb key inst fn-err)
                          (err-throw "msg len too large" len (client-id inst))
                          )

                      :else
                      (do (reset! (:msg-buf inst) (ByteBuffer/allocate len))
                          (reset! (:read-state inst) 2))
                      ))
              )

          :else (reset! (:continue-read inst) false)
          )))

(defn read-body [key channel inst fn-err fn-msg]
  (let [msg-buf @(:msg-buf inst)
        rcount (.read channel msg-buf)]
    (if (< rcount 0)
      (do (reset! (:continue-read inst) false)
          (err-cb key inst fn-err)
          (err-throw "read-body count < 0" rcount (client-id inst))
          )

      (when (not (.hasRemaining msg-buf))
        (reset! (:continue-read inst) true)
        (.flip msg-buf)
        (.flip (:len-buf inst))
        (reset! (:read-state inst) 1)
        (reset! (:msg-buf inst) nil)
        (fn-msg inst (.array msg-buf))
        ))))

(defn handle-read [selector key fn-err fn-msg]
  (let [client (.channel key)
        inst (.attachment key)]
    (while @(:continue-read inst)
      (case @(:read-state inst)
        1 (read-len key client inst fn-err)
        2 (read-body key client inst fn-err fn-msg)

        (do (reset! (:continue-read inst) false)
            (err-throw "internal error read-state" (:read-state inst) (client-id inst))
            )))))

(defn handle-write [selector key]
  (let [channel (.channel key)
        client (.attachment key)
        peek-bbuf (.peek (:outqueue client))]
    (when peek-bbuf
      (.write channel peek-bbuf)
      (when (not (.hasRemaining peek-bbuf))
        (.poll (:outqueue client))
        ))

    (when (.isEmpty? (:outqueue client))
      (.register channel selector SelectionKey/OP_READ client))
    ))

(defn handle-net-events [selector srvchannel fn-new-conn fn-err fn-msg]
  (while true
    (.select selector)

    (let [iter (-> selector
                   .selectedKeys
                   .iterator)]
      (while (.hasNext iter)
        (let [key (.next iter)]
          (.remove iter)
          (try
            (cond (.isAcceptable key) (handle-accept selector key srvchannel fn-new-conn)
                  (.isReadable key) (handle-read selector key fn-err fn-msg)
                  (.isWritable key) (handle-write selector key))
            (catch Exception e
              (println "exception" e)
              (err-cb key (.attachment key) fn-err)
              )))))
    (Thread/sleep 30)))

(defn init-server [port fn-new-conn fn-err fn-msg]
  (let [selector (Selector/open)
        srvchannel (ServerSocketChannel/open)]
    (.configureBlocking srvchannel false)
    (println "init-server listen on" port)
    (-> srvchannel
        .socket
        (.bind (InetSocketAddress. port)))
    (.register srvchannel selector SelectionKey/OP_ACCEPT)

    (-> (Thread. (fn [] (handle-net-events selector srvchannel
                                           fn-new-conn fn-err)))
        .start)))

(defn -main []
  (init-server 1666
               (fn [client]
                 (println "new conn " (client-id client)))
               (fn [client]
                 (println "error conn" (client-id client)))
               (fn [client msg-bytes]
                 (println "received" (String. msg-bytes) "from" (client-id client))
                 (println "send back")
                 (send-to client msg-bytes))))
