(ns adad.memory
  (:require [adad.cpu :as cpu]
            [adad.util :refer [& <<]]))

(defn read-memory [computer address]
  (get-in computer [:memory address]))

(defn read-memory-hl [computer]
  (let [h           (cpu/read-register computer :h)
        l           (cpu/read-register computer :l)
        address     (+ (<< h 8) l)]
    (read-memory computer address)))

(defn store-memory [computer address value]
  (assoc-in computer [:memory address] (& value 2r11111111)))

(defn store-memory-hl [computer value]
  (let [h           (cpu/read-register computer :h)
        l           (cpu/read-register computer :l)
        address     (+ (<< h 8) l)]
    (store-memory computer address value)))
