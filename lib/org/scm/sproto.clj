(ns org.scm.sproto)

(import '[java.nio ByteBuffer])

;; 2^3
(def ^:const max-type-bits 3)
(def ^:const max-type-mask 0x07)

;; int is 64-bits
(def ^:const type-positive-int 0)
(def ^:const type-negative-int 1)
(def ^:const type-double 2)
(def ^:const type-string 3)
(def ^:const type-bytes 4)

(defn new-bytes [len]
  (bytes (byte-array len)))

(defn byte->bytes [bs offset val]
  (let [low1 (bit-and val 0x7F)
        val (bit-shift-right val 7)]
    (if (= 0 val)
      (aset-byte bs offset (unchecked-byte low1))
      (aset-byte bs offset (unchecked-byte (bit-or low1 0x80))))

    val))

(defn int->bytes [bs offset val]
  (let [val (byte->bytes bs offset val)]
    (if (= 0 val)
      (+ 1 offset)
      (+ 1 (int->bytes bs (+ 1 offset) val)))))

(defn bytes->int-check [bytes offset max]
  (when (<= max 0)
    (throw (Exception. (str "overrun the maximum size of a uint"))))

  (let [hi1 (aget bytes offset)
        val (bit-and hi1 0x7F)]
    (if (= 0 (bit-and hi1 0x80))
      (list val (+ 1 offset))
      (let [[val2 c] (bytes->int-check bytes (+ 1 offset) (- max 1))]
        (list (bit-or val (bit-shift-left val2 7))
              (+ 1 c))))))

(defn bytes->int [bytes offset]
  (bytes->int-check bytes offset 10))

;;(defn sint->zigzag [val]
;;  (bit-xor (bit-shift-left val 1) (unsigned-bit-shift-right val 63)))
;;
;;(defn zigzag->sint [val]
;;  (bit-xor (unsigned-bit-shift-right val 1) (bit-not (bit-and val 0x01))))

(defn negative-int->bytes [bytes offset val]
  (int->bytes bytes offset (* -1 val)))

(defn bytes->negative-int [bytes offset]
  (let [[val c] (bytes->int bytes offset)]
    (list (* -1 val) c)))

(defn double->bytes [bytes offset val]
  (System/arraycopy (-> (ByteBuffer/allocate 8) (.putDouble val)
                        (.array))
                    0
                    bytes offset 8))

(defn bytes->double [bytes offset]
  (-> (ByteBuffer/wrap bytes offset 8)
      (.getDouble)))

(defn write-int [bytes offset field-n val]
  (let [offset (int->bytes bytes offset
                           (bit-or (bit-shift-left field-n max-type-bits)
                                   (if (< val 0)
                                     type-negative-int
                                     type-positive-int)))]
    (if (< val 0)
      (negative-int->bytes bytes offset val)
      (int->bytes bytes offset val))))

(defn write-double [bytes offset field-n val]
  (let [offset (int->bytes bytes offset
                           (bit-or (bit-shift-left field-n max-type-bits)
                                   type-double))]
    (double->bytes bytes offset val)
    (+ offset 8)))

(defn write-string [bytes offset field-n val]
  (let [offset (int->bytes bytes offset
                           (bit-or (bit-shift-left field-n max-type-bits)
                                   type-string))
        offset2 (int->bytes bytes offset (.length val))]

    (System/arraycopy (.getBytes val) 0 bytes offset2 (.length val))
    (+ offset2 (.length val))))

(defn write-bytes [bytes offset field-n val]
  (let [alen (alength val)
        offset (int->bytes bytes offset
                           (bit-or (bit-shift-left field-n max-type-bits)
                                   type-bytes))
        offset2 (int->bytes bytes offset alen)]

    (System/arraycopy val 0 bytes offset2 alen)
    (+ offset2 alen)))

(defn read-tlv [bytes offset]
  (let [[field-wire offset] (bytes->int bytes offset)
        field (unsigned-bit-shift-right field-wire max-type-bits)
        wire (bit-and field-wire max-type-mask)]
    (cond (= wire type-positive-int)
          (cons field (bytes->int bytes offset))

          (= wire type-negative-int)
          (cons field (bytes->negative-int bytes offset))

          (= wire type-double)
          (cons field (list (bytes->double bytes offset) (+ offset 8)))

          (= wire type-string)
          (let [[len offset] (bytes->int bytes offset)]
            (cons field (list (String. bytes offset len)
                              (+ offset len))))

          (= wire type-bytes)
          (let [[len offset] (bytes->int bytes offset)
                bs (new-bytes len)]
            (System/arraycopy bytes offset bs 0 len)
            (cons field (list bs (+ offset len))))

          :else
          (throw (Exception. (str "unknown wire type " wire " field " field)))
          )))

(defn -main []
;;  (let [bs (bytes (byte-array 10))]
;;    (println (uint->bytes bs 0 56))
;;    (println (bytes->uint bs 0)))
;;
;;  (let [bs (bytes (byte-array 10))]
;;    (println (uint->bytes bs 0 566))
;;    (println (bytes->uint bs 0)))
;;
;;  (let [bs (bytes (byte-array 10))]
;;    (println (uint->bytes bs 0 9223372036854775807))
;;    (println (bytes->uint bs 0)))

;;  (let [bs (bytes (byte-array 10))]
;;    (println (negative-int->bytes bs 0 -5))
;;    (println (bytes->negative-int bs 0)))
;;
;;  (let [bs (bytes (byte-array 10))]
;;    (println (negative-int->bytes bs 0 -9223372036854775807))
;;    (println (bytes->negative-int bs 0)))

;;  (let [bs (bytes (byte-array 8))]
;;    (println (double->bytes bs 0 95.8))
;;    (println (bytes->double bs 0)))

;;  (let [bs (bytes (byte-array 10))]
;;    (println (write-int bs 0 1 95))
;;    (println (read-tlv bs 0)))
;;
;;  (let [bs (bytes (byte-array 10))]
;;    (println (write-int bs 0 1 -95))
;;    (println (read-tlv bs 0)))
;;
;;  (let [bs (bytes (byte-array 10))]
;;    (println (write-int bs 0 2 9223372036854775807))
;;    (println (read-tlv bs 0)))
;;
;;  (let [bs (bytes (byte-array 10))]
;;    (println (write-int bs 0 3 -9223372036854775807))
;;    (println (read-tlv bs 0)))
;;
;;  (let [bs (bytes (byte-array 10))]
;;    (println (write-double bs 0 4 95.8))
;;    (println (read-tlv bs 0)))
;;
;;  (let [bs (bytes (byte-array 10))]
;;    (println (write-double bs 0 5 -95.8))
;;    (println (read-tlv bs 0)))

;;  (let [bs (bytes (byte-array 100))]
;;    (println (write-string bs 0 6 "Hello World"))
;;    (println (read-tlv bs 0)))

  (let [bs (bytes (byte-array 100))
        bs2 (bytes (byte-array 3))]
    (aset-byte bs2 0 51)
    (aset-byte bs2 1 -1)
    (aset-byte bs2 2 127)

    (println (write-bytes bs 0 7 bs2))
    (let [[field-n val offset] (read-tlv bs 0)]
      (println field-n offset)
      (println (aget val 0) (aget val 1) (aget val 2))))

  )
