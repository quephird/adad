(ns adad.memory
  (:require [adad.util :refer [&]]))

(defn read-memory [computer address]
  (get-in computer [:memory address]))

(defn store-memory [computer address value]
  (assoc-in computer [:memory address] (& value 2r11111111)))
