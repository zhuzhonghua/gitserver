(ns org.scm.test)


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

(defn -main []
  (let [tbs (bytes (byte-array 10))]
    (println (uint->bytes tbs 0 566))
    (println (bytes->uint tbs 0))))