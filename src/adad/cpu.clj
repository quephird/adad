(ns adad.cpu
  (:require [adad.util :refer [<< >> &]]))

(def cpu
  (atom
    {:a 2r00000000
     :flags 2r00000000
     :b 2r00000000
     :c 2r00000000
     :d 2r00000000
     :e 2r00000000
     :h 2r00000000
     :l 2r00000000
     :pc 2r00000000
     :sp 2r00000000}))

(def memory
  (atom (byte-array 65536)))

(defn read-flag [computer flag]
  (get-in computer [:cpu :flags flag]))

(defmulti read (fn [computer register] register))

(defmethod read :a [computer _]
  (get-in computer [:cpu :a]))
(defmethod read :b [computer _]
  (get-in computer [:cpu :b]))
(defmethod read :c [computer _]
  (get-in computer [:cpu :c]))
(defmethod read :bc [computer _]
  (+ (<< (read computer :b) 8)
     (read computer :c)))
(defmethod read :flags [computer _]
  (let [[c p ac z s] (map #(read-flag computer %) [:c :p :ac :z :s])]
    (+ c (<< p 2) (<< ac 4) (<< z 6) (<< s 7))))

(defmulti store (fn [computer register value] register))

(defmethod store :a [computer _ value]
  (assoc-in computer [:cpu :a] value))
(defmethod store :b [computer _ value]
  (assoc-in computer [:cpu :b] value))
(defmethod store :c [computer _ value]
  (assoc-in computer [:cpu :c] value))
(defmethod store :bc [computer _ value]
  (let [b (>> value 8)
        c (& value 0xFF)]
    (-> computer
      (store :b b)
      (store :c c))))
