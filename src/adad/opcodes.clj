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

(defn inr-b
  "Increments the value in the B register;
  flags affected: zero, sign, parity, auxiliary carry"
  [computer]
  (let [b      (cpu/read-register computer :b)
        new-b  (& 0xff (inc b))
        new-p  (parity new-b)
        new-s  (sign new-b)
        new-z  (zero new-b)
        new-hc (if (zero? (& new-b 0x0f)) 0x01 0x00)]
    (-> computer
      (cpu/store-flag :hc new-hc)
      (cpu/store-flag :p new-p)
      (cpu/store-flag :s new-s)
      (cpu/store-flag :z new-z)
      (cpu/store-register :b new-b))))


(defn dcr-b
  "Decrements the value in the B register;
  flags affected: zero, sign, parity, auxiliary carry"
  [computer]
  (let [b      (cpu/read-register computer :b)
        new-b  (& 0xff (dec b))
        new-p  (parity new-b)
        new-s  (sign new-b)
        new-z  (zero new-b)
        new-hc (if (zero? (& new-b 0x0f)) 0x01 0x00)] ; Not sure of this
    (-> computer
      (cpu/store-flag :hc new-hc)
      (cpu/store-flag :p new-p)
      (cpu/store-flag :s new-s)
      (cpu/store-flag :z new-z)
      (cpu/store-register :b new-b))))

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
