(ns adad.util)

(def << bit-shift-left)
(def >> bit-shift-right)
(def & bit-and)
(def | bit-or)
(defn ! [b] (bit-and 255 (bit-not b)))

(defn twos-complement
  "Takes the two's complement of a byte by taking the
   one's complement first then adding one"
  [b]
  (-> b
    bit-not
    (+ 2r1)
    (bit-and 2r11111111)))

(defn zero
  "Returns a bit value instead of a boolean one"
  [b]
  (if (zero? (& b 0xff)) 0x01 0x00))

(defn parity* [b]
  (->> (range 8)
    (map #(& 0x01 (>> b %)))
    (reduce + 0x00)
    (& 0x01)
    zero))
(def parity
  "Counts the number of set bits"
  (memoize parity*))

(defn sign
  "Checks whether the last bit is set"
  [b]
  (-> b (& 0x80) (>> 7)))

(defn auxiliary-carry
  "Checks whether there were any carries during the addition
   of two bytes and alternatively with a carry bit"
  ([b1 b2]
  (auxiliary-carry b1 b2 0x0))
  ([b1 b2 c]
  (if (> (| (& b1 b2)
            (& c (+ b1 b2))) 0) 0x1 0x0)))

(defn carry
  "Checks where there will be a carry from the last bit during
   the addition of two bytes and alternatively with a carry bit"
  ([b1 b2]
  (carry b1 b2 2r0))
  ([b1 b2 c]
  (-> (+ b1 b2 c)
    (& 2r100000000)
    (>> 8))))
