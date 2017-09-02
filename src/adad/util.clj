(ns adad.util)

(def << bit-shift-left)
(def >> bit-shift-right)
(def & bit-and)
(def | bit-or)
(defn ! [b] (bit-and 255 (bit-not b)))

(defn parity* [b]
  (->> (range 8)
    (map #(& 0x01 (>> b %)))
    (reduce + 0x00)
    (& 0x01)))
(def parity (memoize parity*))

(defn zero [b]
  (if (zero? b) 0x01 0x00))

(defn sign [b]
  (-> b (& 0x80) (>> 7)))
