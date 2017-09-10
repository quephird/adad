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
   :splo 2r00000000
   :sphi 2r00000000
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

;; TODO: Split out readers for pairs into new function read-register-pair
(defn read-register [computer register]
  (case register
    (:a :b :c :d :e :h :l :sphi :splo)
      (get-in computer [:cpu register])
    :flags
      (let [[c p ac z s] (map #(read-flag computer %) [:c :p :ac :z :s])]
        (+ c (<< p 2) (<< ac 4) (<< z 6) (<< s 7)))
    :bc
      (+ (<< (read-register computer :b) 8)
         (read-register computer :c))
    :de
      (+ (<< (read-register computer :d) 8)
         (read-register computer :e))
    :hl
      (+ (<< (read-register computer :h) 8)
         (read-register computer :l))
    :sp
      (+ (<< (read-register computer :sphi) 8)
         (read-register computer :splo))))

;; TODO: Split out implementations for pairs into new function store-register-pair
(defn store-register [computer register value]
  (case register
    (:a :b :c :d :e :h :l :sphi :splo)
      (assoc-in computer [:cpu register] value)
    :bc
      (let [b (-> value (>> 8) (& 0xff))
            c (& value 0xFF)]
        (-> computer
          (store-register :b b)
          (store-register :c c)))
    :de
      (let [d (-> value (>> 8) (& 0xff))
            e (& value 0xFF)]
        (-> computer
          (store-register :d d)
          (store-register :e e)))
    :hl
      (let [h (-> value (>> 8) (& 0xff))
            l (& value 0xFF)]
        (-> computer
          (store-register :h h)
          (store-register :l l)))
    :sp
      (let [sphi (-> value (>> 8) (& 0xff))
            splo (& value 0xFF)]
        (-> computer
          (store-register :sphi sphi)
          (store-register :splo splo)))))
