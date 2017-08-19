(ns adad.opcodes
  (:require [adad.util :refer [<< & read store]]))

(defn nop
  "No operation"
  [computer]
  computer)

(defn lxi-b-d16
  "Loads the BC register pair with two bytes,
   b2 gets assigned to B, b1 to C"
  [computer b1 b2]
  (store computer :bc (+ (<< b2 8) b1)))

(defn stax-b
  "Stores the contents of the accumulator A
   at the memory location in the BC register pair"
  [computer]
  (let [a       (read computer :a)
        address (read computer :bc)]
    (assoc-in computer [:memory address] a)))

(defn inx-b
  "Stores the contents of the accumulator A
   at the memory location in the BC register pair"
  [computer]
  (let [bc (read computer :bc)]
    (store computer :bc (& (inc bc) 0xFFFF))))


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
