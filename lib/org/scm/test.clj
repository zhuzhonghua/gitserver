(ns org.scm.test)

(import '[java.nio.channels SocketChannel]
        '[java.nio ByteBuffer]
        '[java.net InetSocketAddress])

(defn byte->bytes [bs offset val]
  (let [low1 (bit-and val 0x7F)
        val (bit-shift-right val 7)]
    (if (= 0 val)
      (aset-byte bs offset (unchecked-byte low1))
      (aset-byte bs offset (unchecked-byte (bit-or low1 0x80))))

    val))

(defn uint->bytes [bs offset val]
  (let [val (byte->bytes bs offset val)]
    (if (= 0 val)
      1
      (+ 1 (uint->bytes bs (+ 1 offset) val)))))

(defn bytes->uint [bytes offset]
  (let [hi1 (aget bytes offset)
        val (bit-and hi1 0x7F)]
    (if (= 0 (bit-and hi1 0x80))
      (list val 1)
      (let [[val2 c] (bytes->uint bytes (+ 1 offset))]
        (list (bit-or val (bit-shift-left val2 7))
              (+ 1 c))))))

(defn client [host port]
  (let [send-to (fn [channel b-bytes]
                  (let [buf (ByteBuffer/allocate (+ 4 (alength b-bytes)))]
                    (.putInt buf (alength b-bytes))
                    (.put buf b-bytes)
                    (.write channel (.flip buf))))

        read-from (fn [channel]
                    (let [len-b-bytes (ByteBuffer/allocate 4)]
                      (.read channel len-b-bytes)
                      (while (.hasRemaining len-b-bytes)
                        (.read channel len-b-bytes)
                        )
                      (.flip len-b-bytes)

                      (let [len (.getInt len-b-bytes)
                            msg-buf (ByteBuffer/allocate len)]
                        (.read channel msg-buf)
                        (while (.hasRemaining msg-buf)
                          (.read channel msg-buf))

                        (.array msg-buf)
                        )))

        channel (SocketChannel/open)

        ]

    (println "connecting server" host port)
    (.configureBlocking channel true)
    (.connect channel (InetSocketAddress. host port))
    (while (not (.finishConnect channel))
      (Thread/sleep 500))
    (println "connect server success " host port)

    (send-to channel (.getBytes "Hello Server"))
    (println "write success")

    (println "recv" (String. (read-from channel)))

    (Thread/sleep 500000000)
    ))

(defn -main []
;;  (let [tbs (bytes (byte-array 10))]
;;    (println (uint->bytes tbs 0 566))
;;    (println (bytes->uint tbs 0)))

  (client "127.0.0.1" 1666)

  )
