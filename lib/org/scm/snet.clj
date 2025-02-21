(ns org.scm.snet)

(import '[java.nio.channels Selector]
        '[java.nio.channels ServerSocketChannel]
        '[java.nio.channels SelectionKey]
        '[java.nio ByteBuffer]
        '[java.net InetSocketAddress])

(def conn-id (atom 1))

(defn handle-accept [selector srvchannel fn-new-conn]
  (let [client (.accept srvchannel)]
    (.configureBlocking client false)
    (.register client selector SelectionKey/OP_READ)
    (fn-new-conn (swap! conn-id inc) client
                 (str (-> client .socket .getInetAddress)
                      ":"
                      (-> client .socket .getPort)))))

(defn handle-read [selector key]
  (let [client (.channel key)
        buffer (ByteBuffer/allocate 256)
        rcount (.read client buffer)]
    ))

(defn handle-write [selector key]
  (let [client (.channel key)]))

(defn handle-net-events [selector srvchannel fn-new-conn]
  (while true
    (.select selector)

    (let [iter (-> selector
                   .selectedKeys
                   .iterator)]
      (while (.hasNext iter)
        (let [key (.next iter)]
          (.remove iter)
          (cond (.isAcceptable key) (handle-accept selector srvchannel fn-new-conn)
                (.isReadable key) (handle-read selector key)
                (.isWritable key) (handle-write selector key)))))

    ))

(defn init-server [port fn-new-conn]
  (let [selector (Selector/open)
        srvchannel (ServerSocketChannel/open)]
    (.configureBlocking srvchannel false)
    (println "init-server listen on" port)
    (-> srvchannel
        .socket
        (.bind (InetSocketAddress. port)))
    (.register srvchannel selector SelectionKey/OP_ACCEPT)

    (-> (Thread. (fn [] (handle-net-events selector srvchannel
                                           fn-new-conn)))
        .start)))

(defn -main []
  (init-server 1666
               (fn [id client-sock-channel addr]
                 (println "new conn" id "addr" addr))))
