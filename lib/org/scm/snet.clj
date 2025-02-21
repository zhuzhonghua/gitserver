(ns org.scm.snet)

(require '[clojure.string :as str])

(import '[java.nio.channels Selector]
        '[java.nio.channels ServerSocketChannel]
        '[java.nio.channels SelectionKey]
        '[java.nio ByteBuffer]
        '[java.net InetSocketAddress]
        '[java.util.concurrent ConcurrentLinkedQueue])

(def conn-id (atom 1))

(defn new-client [channel]
  {:id (swap! conn-id inc)
   :addr (.getRemoteAddress channel)
   :channel channel
   :read-state (atom 1) ;; 1 : read len 2 : read body
   :continue-read (atom true)
   :len-buf (ByteBuffer/allocate 4)
   :msg-buf (atom nil)
   :inqueue (ConcurrentLinkedQueue.)
   :outqueue (ConcurrentLinkedQueue.)
   :lastrecvtime (atom 0)
   :lastsendtime (atom 0)})

(defn client-id [inst]
  (str "[:id " (:id inst) " :addr " (:addr inst) "]"))
;;
;;(defn err-throw [inst & args]
;;  (let [e-arg (cons (client-id inst) args)]
;;  (apply println e-arg)
;;  (throw (Exception. (str/join " " e-arg)))))
;;
;;(defmacro assert-throw [stat inst & args]
;;  `(when (not ~stat)
;;     (err-throw ~inst ~@args)))

(defn handle-accept [selector key srvchannel fn-new-conn]
  (let [client (.accept srvchannel)]
    (.configureBlocking client false)
    (let [inst (new-client client)]
      (.register client selector SelectionKey/OP_READ inst)
      (fn-new-conn inst))))

(defn read-len [key channel inst fn-err]
  (let [len-buf (:len-buf inst)
        rcount (.read channel len-buf)]
    (cond (< rcount 0)
          (do (println "read-len count < 0" rcount (client-id inst))
              (reset! (:continue-read inst) false)
              (.cancel key)
              (fn-err inst)
              )

          (not (.hasRemaining len-buf))
          (do (reset! (:continue-read inst) true)
              (.flip len-buf)
              (let [len (.getInt len-buf)]
                (cond (<= len 0)
                      (do (reset! (:continue-read inst) false)
                          (println "msg len less than or equal 0" len (client-id inst))
                          (.cancel key)
                          (fn-err inst))

                      (>= len 65535)
                      (do (reset! (:continue-read inst) false)
                          (println "msg len too large" len (client-id inst))
                          (.cancel key)
                          (fn-err inst))

                      :else
                      (do (reset! (:msg-buf inst) (ByteBuffer/allocate len))
                          (reset! (:read-state inst) 2))
                      ))
              )

          :else (reset! (:continue-read inst) false)
          )))

(defn read-body [key channel inst fn-err]
  (let [msg-buf @(:msg-buf inst)
        rcount (.read channel msg-buf)]
    (if (< rcount 0)
      (do (reset! (:continue-read inst) false)
          (println "read-body count < 0" rcount (client-id inst))
          (.cancel key)
          (fn-err inst))

      (when (not (.hasRemaining msg-buf))
        (reset! (:continue-read inst) true)
        (.flip msg-buf)
        (.flip (:len-buf inst))
        (reset! (:read-state inst) 1)
        (reset! (:msg-buf inst) nil)
        (println "recved" (String. (.array msg-buf)))
        ))))

(defn handle-read [selector key fn-err]
  (let [client (.channel key)
        inst (.attachment key)]
    (while @(:continue-read inst)
      (case @(:read-state inst)
        1 (read-len key client inst fn-err)
        2 (read-body key client inst fn-err)

        (do (reset! (:continue-read inst) false)
            (println "internal error read-state" (:read-state inst) (client-id inst))
            )))))

(defn handle-write [selector key]
  (let [client (.channel key)]))

(defn handle-net-events [selector srvchannel fn-new-conn fn-err]
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
                  (.isReadable key) (handle-read selector key fn-err)
                  (.isWritable key) (handle-write selector key))
            (catch Exception e
              (println "exception" e)
              (.cancel key)
              (when-let [inst (.attachment key)]
                (fn-err inst)))))))
    (Thread/sleep 30)))

(defn init-server [port fn-new-conn fn-err]
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
               ))
