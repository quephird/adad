(ns adad.cpu
  (:require [adad.util :refer [<< >> &]]))

;_; NOTA BENE: there is no unsigned byte type in Clojure,
;_; Java's `byte` is signed and has range -128 <= b <= 127.
;_; Uuuuuggghhhhhhhhh.

(def fresh-cpu
  {:a  2r00000000
   :b  2r00000000
   :c  2r00000000
   :d  2r00000000
   :e  2r00000000
   :h  2r00000000
   :l  2r00000000
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

(def fresh-memory
  (apply vector-of :int (repeat 65536 0x00)))

(def fresh-computer
  {:cpu fresh-cpu :memory fresh-memory})


(defn read-flag [computer flag]
  (get-in computer [:cpu :flags flag]))

(defn store-flag [computer flag value]
  (assoc-in computer [:cpu :flags flag] (& value 0x01)))


(defmulti read-register (fn [computer register] register))

(defmethod read-register :a [computer _]
  (get-in computer [:cpu :a]))
(defmethod read-register :b [computer _]
  (get-in computer [:cpu :b]))
(defmethod read-register :c [computer _]
  (get-in computer [:cpu :c]))
(defmethod read-register :d [computer _]
  (get-in computer [:cpu :d]))
(defmethod read-register :e [computer _]
  (get-in computer [:cpu :e]))
(defmethod read-register :h [computer _]
  (get-in computer [:cpu :h]))
(defmethod read-register :l [computer _]
  (get-in computer [:cpu :l]))
(defmethod read-register :bc [computer _]
  (+ (<< (read-register computer :b) 8)
     (read-register computer :c)))
(defmethod read-register :de [computer _]
 (+ (<< (read-register computer :d) 8)
    (read-register computer :e)))
(defmethod read-register :hl [computer _]
  (+ (<< (read-register computer :h) 8)
     (read-register computer :l)))
(defmethod read-register :flags [computer _]
  (let [[c p ac z s] (map #(read-flag computer %) [:c :p :ac :z :s])]
    (+ c (<< p 2) (<< ac 4) (<< z 6) (<< s 7))))

(defmulti store-register (fn [computer register value] register))

(defmethod store-register :a [computer _ value]
  (assoc-in computer [:cpu :a] value))
(defmethod store-register :b [computer _ value]
  (assoc-in computer [:cpu :b] value))
(defmethod store-register :c [computer _ value]
  (assoc-in computer [:cpu :c] value))
(defmethod store-register :d [computer _ value]
  (assoc-in computer [:cpu :d] value))
(defmethod store-register :e [computer _ value]
  (assoc-in computer [:cpu :e] value))
(defmethod store-register :h [computer _ value]
  (assoc-in computer [:cpu :h] value))
(defmethod store-register :l [computer _ value]
  (assoc-in computer [:cpu :l] value))
(defmethod store-register :bc [computer _ value]
  (let [b (-> value (>> 8) (& 0xff))
        c (& value 0xFF)]
    (-> computer
      (store-register :b b)
      (store-register :c c))))
(defmethod store-register :de [computer _ value]
  (let [d (-> value (>> 8) (& 0xff))
        e (& value 0xFF)]
    (-> computer
      (store-register :d d)
      (store-register :e e))))
(defmethod store-register :hl [computer _ value]
  (let [h (-> value (>> 8) (& 0xff))
        l (& value 0xFF)]
    (-> computer
      (store-register :h h)
      (store-register :l l))))
