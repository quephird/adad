(ns adad.util)

(def << bit-shift-left)
(def >> bit-shift-right)
(def & bit-and)
(def | bit-or)
(defn ! [b] (bit-and 255 (bit-not b)))

(defn zero [b]
  (if (zero? b) 0x01 0x00))

(defn parity* [b]
  (->> (range 8)
    (map #(& 0x01 (>> b %)))
    (reduce + 0x00)
    (& 0x01)
    zero))
(def parity (memoize parity*))

(defn sign [b]
  (-> b (& 0x80) (>> 7)))

(defn auxiliary-carry [b1 b2]
  (if (> (bit-and b1 b2) 0) 0x1 0x0))

(defn carry [b1 b2]
  (>> (bit-and b1 b2) 7))
