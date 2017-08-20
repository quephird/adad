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

(defmulti read (fn [computer register] register))

(defmethod read :a [computer _]
  (get-in computer [:cpu :a]))
(defmethod read :flags [computer _]
  (get-in computer [:cpu :flags]))
(defmethod read :b [computer _]
  (get-in computer [:cpu :b]))
(defmethod read :c [computer _]
  (get-in computer [:cpu :c]))
(defmethod read :bc [computer _]
  (+ (<< (read computer :b) 8)
     (read computer :c)))

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
