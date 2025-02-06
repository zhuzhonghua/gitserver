(ns org.scm.scommon)

(require '[clojure.string :as str]
         '[clojure.java.io :as io])

(import '[java.util.concurrent ConcurrentLinkedQueue]
        '[java.util.zip CRC32]
        '[java.net DatagramPacket InetAddress])

(defn client-str [client]
  (str (:addr client) ":" (:port client)))

(defn client-key [addr port]
  (str addr ":" port))

(def ^:const MAXLEN 512)

(def ^:const recv-tag "<----")
(def ^:const send-tag "---->")

(def ^:const TYPE-ACK 0)
(def ^:const TYPE-DATA 1)
(def ^:const TYPE-MULTI-DATA 2)

(defn get-type [t]
  (cond (= t TYPE-ACK) "ACK"
        (= t TYPE-DATA) "DATA"
        (= t TYPE-MULTI-DATA) "MULTI-DATA"
        :else (str "UNKNOWN type" t)))

;; type lendata
(def ^:const max-multi-data-len (- MAXLEN 1 8 8 4))

;; type data
(def ^:const max-data-len (- MAXLEN 1 8 8))

;; 3 def
(def ^:const max-sending-wait-time (* 3 1000 1000))

(defn int->bytes [bs offset value]
  (aset-byte bs    offset    (unchecked-byte (bit-and (bit-shift-right value 24) 0xFF)))
  (aset-byte bs (+ offset 1) (unchecked-byte (bit-and (bit-shift-right value 16) 0xFF)))
  (aset-byte bs (+ offset 2) (unchecked-byte (bit-and (bit-shift-right value  8) 0xFF)))
  (aset-byte bs (+ offset 3) (unchecked-byte (bit-and                  value     0xFF))))

(defn bytes->int [bs offset]
  (bit-or (bit-shift-left (bit-and (aget bs    offset   ) 0xFF) 24)
          (bit-shift-left (bit-and (aget bs (+ offset 1)) 0xFF) 16)
          (bit-shift-left (bit-and (aget bs (+ offset 2)) 0xFF)  8)
                          (bit-and (aget bs (+ offset 3)) 0xFF)))

(defn long->bytes [bs offset ^long value]
  (aset-byte bs    offset    (unchecked-byte (bit-and (bit-shift-right value 56) 0xFF)))
  (aset-byte bs (+ offset 1) (unchecked-byte (bit-and (bit-shift-right value 48) 0xFF)))
  (aset-byte bs (+ offset 2) (unchecked-byte (bit-and (bit-shift-right value 40) 0xFF)))
  (aset-byte bs (+ offset 3) (unchecked-byte (bit-and (bit-shift-right value 32) 0xFF)))

  (aset-byte bs (+ offset 4) (unchecked-byte (bit-and (bit-shift-right value 24) 0xFF)))
  (aset-byte bs (+ offset 5) (unchecked-byte (bit-and (bit-shift-right value 16) 0xFF)))
  (aset-byte bs (+ offset 6) (unchecked-byte (bit-and (bit-shift-right value  8) 0xFF)))
  (aset-byte bs (+ offset 7) (unchecked-byte (bit-and                  value     0xFF))))

(defn bytes->long [bs offset]
  (bit-or (bit-shift-left (bit-and (aget bs    offset   ) 0xFF) 56)
          (bit-shift-left (bit-and (aget bs (+ offset 1)) 0xFF) 48)
          (bit-shift-left (bit-and (aget bs (+ offset 2)) 0xFF) 40)
          (bit-shift-left (bit-and (aget bs (+ offset 3)) 0xFF) 32)

          (bit-shift-left (bit-and (aget bs (+ offset 4)) 0xFF) 24)
          (bit-shift-left (bit-and (aget bs (+ offset 5)) 0xFF) 16)
          (bit-shift-left (bit-and (aget bs (+ offset 6)) 0xFF)  8)
                          (bit-and (aget bs (+ offset 7)) 0xFF)))

(defn check-sum [bs offset len]
  (let [check (CRC32.)]
    (.update check bs offset len)
    (.getValue check)))

(defn write-type [bs tp]
;;  (println send-tag "write-type" (get-type tp))
  (aset-byte bs 0 (unchecked-byte tp)))

(defn read-type [bs]
  (aget bs 0))

(defn write-serial [bs offset serial]
;;  (println send-tag "write-serial" serial)
  (long->bytes bs offset serial))

(defn read-serial [bs offset]
  (bytes->long bs offset))

(defn write-len [bs offset len]
  (int->bytes bs offset len))

(defn read-len [bs offset]
  (bytes->int bs offset))

(defn write-check-sum [bs offset cs]
;;  (println send-tag "write-check-sum" cs)
  (long->bytes bs offset cs))

(defn read-check-sum [bs offset]
  (bytes->long bs offset))

(defn write-bytes [bs-dst offset-dst bs-src offset-src len]
  (System/arraycopy bs-src offset-src bs-dst offset-dst len))

(defn get-sys-time []
  (System/currentTimeMillis))

(defn make-queue []
  (ConcurrentLinkedQueue.))

(defn empty-queue? [q]
  (.isEmpty q))

(defn poll-queue [q]
  (when (not (empty-queue? q))
    (.poll q)))

(defn offer-queue [q p]
  (.offer q p))

(defn size-queue [q]
  (.size q))

(defn write-sock-packet [sock client bs]
;;  (println send-tag "write-sock-packet size" (alength bs) "to client" (client-str client))
  (.send sock (DatagramPacket. bs (alength bs) (:addr client) (:port client))))

(defn write-sock-bytes [sock client bs serial cs]
  (if (nil? @(:sending client))
    (do (reset! (:sending client) (list serial cs bs))
        (write-sock-packet sock client bs))

    (do (offer-queue (:outqueue client) (list serial cs bs))
        (println send-tag "sending not over cache it size" (size-queue (:outqueue client))))))

(defn write-sock-ack [sock client serial cs]
;;  (println send-tag "write-sock-ack serial" serial)
  (let [bs (bytes (byte-array (+ 1 8 8)))]
    (write-type bs TYPE-ACK)
    (write-serial bs 1 serial)
    (write-check-sum bs (+ 1 8) cs)
    (write-sock-packet sock client bs)))

(defn resending [sock client]
  (let [[serial cs bs] @(:sending client)]
    (println send-tag "resending...")
    (write-sock-packet sock client bs)))

(defn send-next [sock client]
  (reset! (:sending client) nil)
  (when-let [msg (poll-queue (:outqueue client))]
    (println send-tag "send-next serial" (first msg))
    (reset! (:sending client) msg)
    (resending sock client)))

(defn send-type-data [sock client serial str-bytes]
  (let [tlen (+ 1 8 8 (alength str-bytes))
        tbytes (bytes (byte-array tlen))
        cs (check-sum str-bytes 0 (alength str-bytes))]
    (write-type tbytes TYPE-DATA)
    (write-serial tbytes 1 serial)
    (write-check-sum tbytes (+ 1 8) cs)
    (write-bytes tbytes (+ 1 8 8) str-bytes 0 (alength str-bytes))
    (write-sock-bytes sock client tbytes serial cs)))

(defn send-len-data [sock client send-serial str-bytes len]
  (loop [offset 0]
    (when (< offset len)
      (let [pack-size (if (< (+ offset max-multi-data-len) len)
                        max-multi-data-len
                        (- len offset))
            arr (bytes (byte-array (+ 1 8 8 4 pack-size)))
            serial (swap! send-serial inc)
            cs (check-sum str-bytes offset pack-size)]
        (write-type arr TYPE-MULTI-DATA)
        (write-serial arr 1 serial)
        (write-check-sum arr (+ 1 8) cs)
        (write-len arr (+ 1 8 8) len)
        (write-bytes arr (+ 1 8 8 4) str-bytes offset pack-size)
        (write-sock-bytes sock client arr serial cs)
        (recur (+ offset pack-size))))))

(defn send-data [sock send-serial client data]
  (if (> (alength data) max-data-len)
    (do (send-len-data sock client send-serial data (alength data))
;;        (println send-tag "send-len-data len" (alength data))
        )

    (do
;;      (println send-tag "send-type-data len" (alength data))
      (send-type-data sock client (swap! send-serial inc) data)
      )))

(defn accu-partial [client partialin partialoffset tlen bs offset len]
  (when (nil? @partialin)
    (reset! partialin (bytes (byte-array tlen)))
    (reset! partialoffset 0))

  (System/arraycopy bs offset @partialin @partialoffset len)
  (swap! partialoffset + len)

  (cond (= @partialoffset tlen)
        (do (let [cmd (String. @partialin)]
              (offer-queue (:inqueue client) cmd)
              (println recv-tag "partial recv full" tlen "cmd" cmd))

            (reset! partialin nil)
            (reset! partialoffset 0))
;;            (offer-queue (:inqueue client) (str/split (String. @partialin) #" ")))

        (< @partialoffset tlen)
        (println recv-tag "partial recv not over"
                 tlen
                 @partialoffset)

        :else
        (throw (Exception. (str "internal error" tlen @partialoffset)))))

(defn check-recv-serial [client serial]
  (let [recvmaxserial (:recvmaxserial client)]
    (if (>= @recvmaxserial serial)
      (println recv-tag "recv past serial just ignore"
               @recvmaxserial serial)

      (do
;;        (println recv-tag "update recv max serial" "last" @recvmaxserial "new" serial)
        (reset! (:recvmaxserial client) serial)
        (reset! (:lastrecvtime client) (get-sys-time))))))

(defn recv-partial [client serial tlen bs offset len]
  (when (check-recv-serial client serial)
    (accu-partial client (:partialin client) (:partialoffset client)
                  tlen bs offset len)))

(defn check-recv-multi-data [sock client serial cs bs offset len]
  (let [tlen (read-len bs offset)
        cmd-len (- len offset 4)
        self-cs (check-sum bs (+ offset 4) cmd-len)]
    (if (not= cs self-cs)
      (println recv-tag "recv-len-data check-sum not equal" cs self-cs bs offset len)

      (do (write-sock-ack sock client serial cs)
          (recv-partial client serial tlen bs (+ offset 4) cmd-len)))))

(defn recv-cmd [client serial cmd]
  (when (check-recv-serial client serial)
    ;; partial must be nil
    (when (not (nil? @(:partialin client)))
      (throw (Exception. (str "internal error partial not over" serial cmd))))

    (println recv-tag "recv-cmd" cmd)
    (offer-queue (:inqueue client) cmd)))

(defn recv-ack [sock client serial cs]
  (if @(:sending client)
    (let [[mserial mcs bs] @(:sending client)]
      (cond (and (= serial mserial) (= cs mcs))
            (do
;;              (println recv-tag "recv-ack serial" serial)
              (send-next sock client))

            (< serial mserial)
            (println recv-tag "ignore old serial may send multi times")

            :else
            (println recv-tag "recv serial greater than" serial mserial mcs bs)))

    (println recv-tag "no sending but recv ack" serial)))

(defn check-recv-data [sock client serial cs bs offset len]
  (let [cmd-len (- len offset)
        self-cs (check-sum bs offset cmd-len)]
    (if (not= cs self-cs)
      (println recv-tag "check-sum not equal" cs self-cs bs offset len)

      (do (write-sock-ack sock client serial cs)
          (recv-cmd client serial (String. bs offset cmd-len))
          ))))

(defn recv-packet [sock client bs len]
  (cond (< len 1)
        (println recv-tag "recv-packet no type" bs len)

        (< len (+ 1 8))
        (println recv-tag "recv-packet no serial" bs len)

        (< len (+ 1 8 8))
        (println recv-tag "recv-packet no checksum" bs len)

        :else
        (let [type (read-type bs)
              serial (read-serial bs 1)
              cs (read-check-sum bs (+ 1 8))]
;;          (println recv-tag "recv-packet type" (get-type type) "serial" serial "cs" cs)
          (condp = type
            TYPE-ACK (recv-ack sock client serial cs)
            TYPE-DATA (check-recv-data sock client serial cs bs (+ 1 8 8) len)
            TYPE-MULTI-DATA (check-recv-multi-data sock client serial cs bs (+ 1 8 8) len)

            (println recv-tag "recv-packet unknown type" type "serial" serial "cs" cs)))))
