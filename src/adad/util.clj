(ns adad.util)

(def << bit-shift-left)
(def >> bit-shift-right)
(def & bit-and)

(defn parity* [b]
  (->> (range 8)
    (map #(& 0x01 (>> b %)))
    (reduce + 0)
    (& 0x01)))

(def parity (memoize parity*))

(def fresh-cpu
  {:a 2r00000000
   :b 2r00000000
   :c 2r00000000
   :d 2r00000000
   :e 2r00000000
   :h 2r00000000
   :l 2r00000000
   :pc 2r00000000
   :sp 2r00000000
   :flags {:c  2r0
           :u1 2r0
           :p  2r0
           :u3 2r0
           :ac 2r0
           :u5 2r0
           :z  2r0
           :s  2r0}})

;; Uuuuggghhhhhhh there is no unsigned byte type in Clojure;
;; byte has range -128 <= b <= 127. ;_;
(def fresh-memory
  (apply vector-of :int (repeat 65536 0x00)))

(def fresh-computer
  {:cpu fresh-cpu :memory fresh-memory})
