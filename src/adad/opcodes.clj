(ns adad.opcodes
  (:require [adad.cpu :as cpu]
            [adad.util :refer [<< & parity sign zero]]))

(defn nop
  "No operation"
  [computer]
  computer)

(defn lxi-b
  "Loads the BC register pair with two bytes,
   b2 gets assigned to B, b1 to C"
  [computer b1 b2]
  (cpu/store-register computer :bc (+ (<< b2 8) b1)))

(defn stax-b
  "Stores the contents of the accumulator A
   at the memory location in the BC register pair"
  [computer]
  (let [a       (cpu/read-register computer :a)
        address (cpu/read-register computer :bc)]
    (assoc-in computer [:memory address] a)))

(defn inx-b
  "Increments the value in the BC register pair;
   no flags affected"
  [computer]
  (let [bc (cpu/read-register computer :bc)]
    (cpu/store-register computer :bc (& (inc bc) 0xFFFF))))

(defn- inr
  "Helper function to increment the value in the register passed;
  flags affected: zero, sign, parity, auxiliary carry"
  [computer register]
  (let [reg-val     (cpu/read-register computer register)
        new-reg-val (& 0xff (inc reg-val))
        new-p       (parity new-reg-val)
        new-s       (sign new-reg-val)
        new-z       (zero new-reg-val)
        new-hc      (if (zero? (& new-reg-val 0x0f)) 0x01 0x00)]
    (-> computer
      (cpu/store-flag :hc new-hc)
      (cpu/store-flag :p new-p)
      (cpu/store-flag :s new-s)
      (cpu/store-flag :z new-z)
      (cpu/store-register register new-reg-val))))

(defn inr-a [computer] (inr computer :a))
(defn inr-b [computer] (inr computer :b))
(defn inr-c [computer] (inr computer :c))
(defn inr-d [computer] (inr computer :d))
(defn inr-e [computer] (inr computer :e))
(defn inr-h [computer] (inr computer :h))
(defn inr-l [computer] (inr computer :l))

(defn- dcr
  "Helper function to decrement the value in the register passed;
  flags affected: zero, sign, parity, auxiliary carry"
  [computer register]
  (let [reg-val     (cpu/read-register computer register)
        new-reg-val (& 0xff (dec reg-val))
        new-p       (parity new-reg-val)
        new-s       (sign new-reg-val)
        new-z       (zero new-reg-val)
        new-hc      (if (zero? (& new-reg-val 0x0f)) 0x01 0x00)] ; Not sure of this
    (-> computer
      (cpu/store-flag :hc new-hc)
      (cpu/store-flag :p new-p)
      (cpu/store-flag :s new-s)
      (cpu/store-flag :z new-z)
      (cpu/store-register register new-reg-val))))

(defn dcr-a [computer] (dcr computer :a))
(defn dcr-b [computer] (dcr computer :b))
(defn dcr-c [computer] (dcr computer :c))
(defn dcr-d [computer] (dcr computer :d))
(defn dcr-e [computer] (dcr computer :e))
(defn dcr-h [computer] (dcr computer :h))
(defn dcr-l [computer] (dcr computer :l))

(defn- mvi
  "Helper function to load the specified register
   with the byte value passed in; no flags affected"
  [computer register new-val]
  (cpu/store-register computer register new-val))

(defn mvi-a [computer new-val] (mvi computer :a new-val))
(defn mvi-b [computer new-val] (mvi computer :b new-val))
(defn mvi-c [computer new-val] (mvi computer :c new-val))
(defn mvi-d [computer new-val] (mvi computer :d new-val))
(defn mvi-e [computer new-val] (mvi computer :e new-val))
(defn mvi-h [computer new-val] (mvi computer :h new-val))
(defn mvi-l [computer new-val] (mvi computer :l new-val))

; Instead of simply making these a vector of hashes
; that can be looked up by a numeric index, I made it
; a nested hash so that 1) as I'm implementing opcodes
; I can have gaps in this structure, and 2) the relationship
; between the functions and the actual 8080 opcodes is far
; more explicit here.
(def opcodes
  {
   0x00 {:fn nop    :bytes 1 :cycles 1}
   0x01 {:fn lxi-b  :bytes 3 :cycles 3}
   0x02 {:fn stax-b :bytes 1 :cycles 2}
   0x03 {:fn inx-b  :bytes 1 :cycles 1}
   0x04 {:fn inr-b  :bytes 1 :cycles 1}
   0x05 {:fn dcr-b  :bytes 1 :cycles 1}
   0x06 {:fn mvi-b  :bytes 2 :cycles 2}

   0x08 {:fn nop    :bytes 1 :cycles 1}

   0x0c {:fn inr-c  :bytes 1 :cycles 1}
   0x0d {:fn dcr-c  :bytes 1 :cycles 1}
   0x0e {:fn mvi-c  :bytes 2 :cycles 2}

   0x10 {:fn nop    :bytes 1 :cycles 1}

   0x14 {:fn inr-d  :bytes 1 :cycles 1}
   0x15 {:fn dcr-d  :bytes 1 :cycles 1}
   0x16 {:fn mvi-d  :bytes 2 :cycles 2}
   0x1c {:fn inr-e  :bytes 1 :cycles 1}
   0x1d {:fn dcr-e  :bytes 1 :cycles 1}
   0x1e {:fn mvi-e  :bytes 2 :cycles 2}

   0x18 {:fn nop    :bytes 1 :cycles 1}

   0x24 {:fn inr-h  :bytes 1 :cycles 1}
   0x25 {:fn dcr-h  :bytes 1 :cycles 1}
   0x26 {:fn mvi-h  :bytes 2 :cycles 2}
   0x2c {:fn inr-l  :bytes 1 :cycles 1}
   0x2d {:fn dcr-l  :bytes 1 :cycles 1}
   0x2e {:fn mvi-l  :bytes 2 :cycles 2}

   0x28 {:fn nop    :bytes 1 :cycles 1}

   0x3c {:fn inr-a  :bytes 1 :cycles 1}
   0x3d {:fn dcr-a  :bytes 1 :cycles 1}
   0x3e {:fn mvi-a  :bytes 2 :cycles 2}

   0x38 {:fn nop    :bytes 1 :cycles 1}

   0xcb {:fn nop    :bytes 1 :cycles 1}

   0xd9 {:fn nop    :bytes 1 :cycles 1}

   0xdd {:fn nop    :bytes 1 :cycles 1}

   0xed {:fn nop    :bytes 1 :cycles 1}

   0xfd {:fn nop    :bytes 1 :cycles 1}
  })
