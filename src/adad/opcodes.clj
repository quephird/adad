(ns adad.opcodes
  (:require [adad.cpu :as cpu]
            [adad.util :refer [<< & parity sign zero]]))

(defn nop
  "No operation"
  [computer]
  computer)

(defn lxi-b-d16
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

#_(def opcodes
  (map
    (range 256)
    [nop, lxi-b-d16, stax-b, inx-b, inr-b]
    ))

; 0x00  NOP  1
; 0x01  LXI B,D16  3    B <- byte 3, C <- byte 2
; 0x02  STAX B  1    (BC) <- A
; 0x03  INX B  1    BC <- BC+1
; 0x04  INR B  1  Z, S, P, AC  B <- B+1
; 0x05  DCR B  1  Z, S, P, AC  B <- B-1
; 0x06  MVI B, D8  2    B <- byte 2
; 0x07  RLC  1  CY  A = A << 1; bit 0 = prev bit 7; CY = prev bit 7
; 0x08  -
; 0x09  DAD B  1  CY  HL = HL + BC
; 0x0a  LDAX B  1    A <- (BC)
