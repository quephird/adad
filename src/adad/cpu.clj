(ns adad.cpu)

(def cpu
  (atom
    {:a 2r00000000
     :b 2r00000000
     :c 2r00000000
     :d 2r00000000
     :e 2r00000000
     :pc nil}))

(def memory
  (atom (byte-array 65536)))
