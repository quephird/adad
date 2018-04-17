(ns adad.opcodes-test
  (:require [adad.opcodes :as subject]
            [adad.cpu :as cpu]
            [adad.memory :as mem]
            [adad.util :refer [<<]]
            [clojure.test :refer :all]))

(deftest testing-lxi-b
  (let [b1          0x42
        b2          0xff
        updated-computer (subject/lxi-b cpu/fresh-computer b1 b2)]
    (is (= b2 (cpu/read-register updated-computer :b)))
    (is (= b1 (cpu/read-register updated-computer :c)))))

(deftest testing-stax-b
  (let [b1   0x23
        b2   0x45
        initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :a 0xFF)
                             (cpu/store-register :b b1)
                             (cpu/store-register :c b2))
        updated-computer (subject/stax-b initial-computer)]
    (is (= 0xFF (mem/read-memory updated-computer (+ (<< b1 8) b2))))))

(deftest testing-inx-b
  (testing "no overflow in c"
    (let [b    0x23
          c    0x45
          initial-computer (-> cpu/fresh-computer
                               (cpu/store-register :b b)
                               (cpu/store-register :c c))
          updated-computer (subject/inx-b initial-computer)]
      (is (= 0x23 (cpu/read-register updated-computer :b)))
      (is (= 0x46 (cpu/read-register updated-computer :c)))))

  (testing "overflow only in c"
    (let [b    0x23
          c    0xFF
          initial-computer (-> cpu/fresh-computer
                               (cpu/store-register :b b)
                               (cpu/store-register :c c))
          updated-computer (subject/inx-b initial-computer)]
      (is (= 0x24 (cpu/read-register updated-computer :b)))
      (is (= 0x00 (cpu/read-register updated-computer :c)))))

  (testing "overflow in bc"
    (let [b    0xFF
          c    0xFF
          initial-computer (-> cpu/fresh-computer
                               (cpu/store-register :b b)
                               (cpu/store-register :c c))
          updated-computer (subject/inx-b initial-computer)]
      (is (= 0x00 (cpu/read-register updated-computer :b)))
      (is (= 0x00 (cpu/read-register updated-computer :c))))))

(deftest testing-inr-b
  (testing "value in b register is incremented"
    (let [b   0x42
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :b b))
          updated-computer (subject/inr-b initial-computer)]
      (is (= 0x43 (cpu/read-register updated-computer :b)))))

  (testing "half carry flag is set"
    (let [b   0x4f
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :b b))
          updated-computer (subject/inr-b initial-computer)]
      (is (= 0x01 (cpu/read-flag updated-computer :hc)))))

  (testing "half carry flag is unset"
    (let [b   0x10
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :b b)
                             (cpu/store-flag :hc 0x01))
          updated-computer (subject/inr-b initial-computer)]
      (is (= 0x00 (cpu/read-flag updated-computer :hc)))))

  (testing "parity flag is set"
    (let [b   2r00000010
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :b b))
          updated-computer (subject/inr-b initial-computer)]
      (is (= 0x01 (cpu/read-flag updated-computer :p)))))

  (testing "parity flag is unset"
    (let [b   0x0f
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :b b)
                             (cpu/store-flag :p 0x01))
          updated-computer (subject/inr-b initial-computer)]
      (is (= 0x00 (cpu/read-flag updated-computer :p)))))

  (testing "sign flag is set"
    (let [b   0x7f
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :b b))
          updated-computer (subject/inr-b initial-computer)]
      (is (= 0x01 (cpu/read-flag updated-computer :s)))))

  (testing "sign flag is unset"
    (let [b   0xff
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :b b)
                             (cpu/store-flag :s 0x01))
          updated-computer (subject/inr-b initial-computer)]
      (is (= 0x00 (cpu/read-flag updated-computer :s)))))

  (testing "zero flag is set"
    (let [b   0xff
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :b b))
          updated-computer (subject/inr-b initial-computer)]
      (is (= 0x01 (cpu/read-flag updated-computer :z)))))

  (testing "zero flag is unset"
    (let [b   0x00
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :b b)
                             (cpu/store-flag :z 0x01))
          updated-computer (subject/inr-b initial-computer)]
      (is (= 0x00 (cpu/read-flag updated-computer :z))))))

(deftest testing-inr-m
  (testing "value in b register is incremented"
    (let [h       0x12
          l       0x34
          address (+ (<< h 8) l)
          old-val 0x42
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :h h)
                             (cpu/store-register :l l)
                             (mem/store-memory address old-val))
          updated-computer (subject/inr-m initial-computer)]
      (is (= 0x43 (mem/read-memory updated-computer address))))))

(deftest testing-dcr-b
  (testing "value in b register is decremented"
    (let [b   0x43
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :b b))
          updated-computer (subject/dcr-b initial-computer)]
      (is (= 0x42 (cpu/read-register updated-computer :b)))))

  ; TODO: Figure out if the HC flag is ever set in a DCR opcode
  (testing "half carry flag is unset"
    (let [b   0x10
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :b b)
                             (cpu/store-flag :hc 0x01))
          updated-computer (subject/dcr-b initial-computer)]
      (is (= 0x00 (cpu/read-flag updated-computer :hc)))))

  (testing "parity flag is set"
    (let [b   2r11111101
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :b b))
          updated-computer (subject/dcr-b initial-computer)]
      (is (= 0x01 (cpu/read-flag updated-computer :p)))))

  (testing "parity flag is unset"
    (let [b   2r11111111
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :b b)
                             (cpu/store-flag :p 0x01))
          updated-computer (subject/dcr-b initial-computer)]
      (is (= 0x00 (cpu/read-flag updated-computer :p)))))

  (testing "sign flag is set"
    (let [b   0x00
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :b b))
          updated-computer (subject/dcr-b initial-computer)]
      (is (= 0x01 (cpu/read-flag updated-computer :s)))))

  (testing "sign flag is unset"
    (let [b   0x7f
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :b b)
                             (cpu/store-flag :s 0x01))
          updated-computer (subject/dcr-b initial-computer)]
      (is (= 0x00 (cpu/read-flag updated-computer :s)))))

  (testing "zero flag is set"
    (let [b   0xff
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :b b))
          updated-computer (subject/inr-b initial-computer)]
      (is (= 0x01 (cpu/read-flag updated-computer :z)))))

  (testing "zero flag is unset"
    (let [b   0x00
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :b b)
                             (cpu/store-flag :z 0x01))
          updated-computer (subject/inr-b initial-computer)]
      (is (= 0x00 (cpu/read-flag updated-computer :z))))))

(deftest testing-mvi-m
  (testing "value in b register is incremented"
    (let [h       0x12
          l       0x34
          address (+ (<< h 8) l)
          b       0x42
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :h h)
                             (cpu/store-register :l l))
          updated-computer (subject/mvi-m initial-computer b)]
      (is (= 0x42 (mem/read-memory updated-computer address))))))

(deftest testing-rlc
  (testing "no carry"
    (let [a   2r00000001
          initial-computer (-> cpu/fresh-computer
                               (cpu/store-register :a a))
          updated-computer (subject/rlc initial-computer)]
      (is (= 2r00000010 (cpu/read-register updated-computer :a)))
      (is (= 2r0 (cpu/read-flag updated-computer :c)))))

  (testing "with carry"
    (let [a   2r11100111
          initial-computer (-> cpu/fresh-computer
                               (cpu/store-register :a a))
          updated-computer (subject/rlc initial-computer)]
      (is (= 2r11001111 (cpu/read-register updated-computer :a)))
      (is (= 2r1 (cpu/read-flag updated-computer :c))))))

(deftest testing-dad-b
  (testing "no carry"
    (let [bc  0x0001
          hl  0x0002
          initial-computer (-> cpu/fresh-computer
                               (cpu/store-register :bc bc)
                               (cpu/store-register :hl hl))
          updated-computer (subject/dad-b initial-computer)]
      (is (= 0x0003 (cpu/read-register updated-computer :hl)))
      (is (= 2r0 (cpu/read-flag updated-computer :c)))))

  (testing "with carry"
    (let [bc  0x9001
          hl  0xa002
          initial-computer (-> cpu/fresh-computer
                               (cpu/store-register :bc bc)
                               (cpu/store-register :hl hl))
          updated-computer (subject/dad-b initial-computer)]
      (is (= 0x3003 (cpu/read-register updated-computer :hl)))
      (is (= 2r1 (cpu/read-flag updated-computer :c))))))

(deftest testing-ldax-b
  (let [b1      0x23
        b2      0x45
        address (+ (<< b1 8) b2)
        initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :b b1)
                             (cpu/store-register :c b2)
                             (mem/store-memory address 0x42))
        updated-computer (subject/ldax-b initial-computer)]
    (is (= 0x42 (cpu/read-register updated-computer :a)))))

(deftest testing-dcx-b
  (let [b       0x23
        c       0x45
        initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :b b)
                             (cpu/store-register :c c))
        updated-computer (subject/dcx-b initial-computer)]
    (is (= 0x2344 (cpu/read-register updated-computer :bc)))))

(deftest testing-rrc
  (testing "no carry"
    (let [a   2r10000000
          initial-computer (-> cpu/fresh-computer
                               (cpu/store-register :a a))
          updated-computer (subject/rrc initial-computer)]
      (is (= 2r01000000 (cpu/read-register updated-computer :a)))
      (is (= 2r0 (cpu/read-flag updated-computer :c)))))

  (testing "with carry"
    (let [a   2r11100111
          initial-computer (-> cpu/fresh-computer
                               (cpu/store-register :a a))
          updated-computer (subject/rrc initial-computer)]
      (is (= 2r11110011 (cpu/read-register updated-computer :a)))
      (is (= 2r1 (cpu/read-flag updated-computer :c))))))

(deftest testing-ral
  (testing "no carry, no seventh bit"
    (let [a   2r00000001
          c   2r0
          initial-computer (-> cpu/fresh-computer
                               (cpu/store-register :a a)
                               (cpu/store-flag :c c))
          updated-computer (subject/ral initial-computer)]
      (is (= 2r00000010 (cpu/read-register updated-computer :a)))
      (is (= 2r0 (cpu/read-flag updated-computer :c)))))

  (testing "no carry, seventh bit on"
    (let [a   2r10000001
          c   2r0
          initial-computer (-> cpu/fresh-computer
                               (cpu/store-register :a a)
                               (cpu/store-flag :c c))
          updated-computer (subject/ral initial-computer)]
      (is (= 2r00000010 (cpu/read-register updated-computer :a)))
      (is (= 2r1 (cpu/read-flag updated-computer :c))))))

(deftest testing-rar
  (testing "no carry, no zeroth bit"
    (let [a   2r10000000
          c   2r0
          initial-computer (-> cpu/fresh-computer
                               (cpu/store-register :a a)
                               (cpu/store-flag :c c))
          updated-computer (subject/rar initial-computer)]
      (is (= 2r01000000 (cpu/read-register updated-computer :a)))
      (is (= 2r0 (cpu/read-flag updated-computer :c)))))

  (testing "no carry, zeroth bit on"
    (let [a   2r10000001
          c   2r0
          initial-computer (-> cpu/fresh-computer
                               (cpu/store-register :a a)
                               (cpu/store-flag :c c))
          updated-computer (subject/rar initial-computer)]
      (is (= 2r01000000 (cpu/read-register updated-computer :a)))
      (is (= 2r1 (cpu/read-flag updated-computer :c))))))

(deftest testing-shld
  (let [b1      0x23
        b2      0x45
        address (+ (<< b1 8) b2)
        initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :h 0x34)
                             (cpu/store-register :l 0x12))
        updated-computer (subject/shld initial-computer b1 b2)]
    (is (= 0x12 (mem/read-memory updated-computer address)))
    (is (= 0x34 (mem/read-memory updated-computer (inc address))))))

(deftest testing-lhld
  (let [b1      0x45
        b2      0x23
        address (+ (<< b2 8) b1)
        initial-computer (-> cpu/fresh-computer
                           (mem/store-memory address 0x12)
                           (mem/store-memory (inc address) 0x34))
        updated-computer (subject/lhld initial-computer b1 b2)]
    (is (= 0x12 (cpu/read-register updated-computer :l)))
    (is (= 0x34 (cpu/read-register updated-computer :h)))))

(deftest testing-cma
  (testing "value in accumulator is complemented"
    (let [a   2r11110000
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :a a))
          updated-computer (subject/cma initial-computer)]
      (is (= 2r00001111 (cpu/read-register updated-computer :a))))))

(deftest testing-stc
  (testing "carry flag is set properly")
    (let [updated-computer (subject/stc cpu/fresh-computer)]
      (is (= 2r1 (cpu/read-flag updated-computer :c)))))

(deftest testing-lda
  (let [b1      0x45
        b2      0x23
        address (+ (<< b2 8) b1)
        initial-computer (-> cpu/fresh-computer
                           (mem/store-memory address 0x42))
        updated-computer (subject/lda initial-computer b1 b2)]
    (is (= 0x42 (cpu/read-register updated-computer :a)))))

(deftest testing-sta
  (let [b1      0x45
        b2      0x23
        address (+ (<< b2 8) b1)
        initial-computer (-> cpu/fresh-computer
                           (cpu/store-register :a 0x42))
        updated-computer (subject/sta initial-computer b1 b2)]
    (is (= 0x42 (mem/read-memory updated-computer address)))))

(deftest testing-cmc
  (testing "value in carry flag is complemented"
    (let [c   2r1
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-flag :c c))
          updated-computer (subject/cmc initial-computer)]
      (is (= 2r0 (cpu/read-flag updated-computer :c))))))

(deftest testing-add-d
  (testing "add-d"
    (let [a  0x6c
          d  0x2e
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :a a)
                             (cpu/store-register :d d))
          updated-computer (subject/add-d initial-computer)]
      (is (= 0x9a (cpu/read-register updated-computer :a)))
      (is (= 2r1 (cpu/read-flag updated-computer :ac)))
      (is (= 2r0 (cpu/read-flag updated-computer :c)))
      (is (= 2r1 (cpu/read-flag updated-computer :p)))
      (is (= 2r1 (cpu/read-flag updated-computer :s)))
      (is (= 2r0 (cpu/read-flag updated-computer :z))))))

(deftest testing-adc-d
  (testing "adc-d with no carry bit initially set"
    (let [a  0x42
          d  0x3d
          c  2r0
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :a a)
                             (cpu/store-register :d d)
                             (cpu/store-flag :c c))
          updated-computer (subject/adc-d initial-computer)]
      (is (= 0x7f (cpu/read-register updated-computer :a)))
      (is (= 2r0 (cpu/read-flag updated-computer :ac)))
      (is (= 2r0 (cpu/read-flag updated-computer :c)))
      (is (= 2r0 (cpu/read-flag updated-computer :p)))
      (is (= 2r0 (cpu/read-flag updated-computer :s)))
      (is (= 2r0 (cpu/read-flag updated-computer :z)))))

  (testing "adc-d with the carry bit initially set but then reset"
    (let [a  0x42
          d  0x3d
          c  2r1
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :a a)
                             (cpu/store-register :d d)
                             (cpu/store-flag :c c))
          updated-computer (subject/adc-d initial-computer)]
      (is (= 0x80 (cpu/read-register updated-computer :a)))
      (is (= 2r1 (cpu/read-flag updated-computer :ac)))
      (is (= 2r0 (cpu/read-flag updated-computer :c)))
      (is (= 2r0 (cpu/read-flag updated-computer :p)))
      (is (= 2r1 (cpu/read-flag updated-computer :s)))
      (is (= 2r0 (cpu/read-flag updated-computer :z)))))

  (testing "adc-d with the carry bit initially set and set again"
    (let [a  0xf0
          d  0x0f
          c  2r1
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :a a)
                             (cpu/store-register :d d)
                             (cpu/store-flag :c c))
          updated-computer (subject/adc-d initial-computer)]
      (is (= 0x00 (cpu/read-register updated-computer :a)))
      (is (= 2r1 (cpu/read-flag updated-computer :ac)))
      (is (= 2r1 (cpu/read-flag updated-computer :c)))
      (is (= 2r1 (cpu/read-flag updated-computer :p)))
      (is (= 2r0 (cpu/read-flag updated-computer :s)))
      (is (= 2r1 (cpu/read-flag updated-computer :z))))))

(deftest testing-add-m
  (testing "add-m"
    (let [a  0x6c
          h  0x12
          l  0x34
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :a a)
                             (cpu/store-register :h h)
                             (cpu/store-register :l l)
                             (mem/store-memory-hl 0x2e))
          updated-computer (subject/add-m initial-computer)]
      (is (= 0x9a (cpu/read-register updated-computer :a)))
      (is (= 2r1 (cpu/read-flag updated-computer :ac)))
      (is (= 2r0 (cpu/read-flag updated-computer :c)))
      (is (= 2r1 (cpu/read-flag updated-computer :p)))
      (is (= 2r1 (cpu/read-flag updated-computer :s)))
      (is (= 2r0 (cpu/read-flag updated-computer :z))))))

(deftest testing-adc-m
  (testing "adc-m"
    (let [a  0xf0
          h  0x12
          l  0x34
          c  2r1
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :a a)
                             (mem/store-memory-hl 0x0f)
                             (cpu/store-flag :c c))
          updated-computer (subject/adc-m initial-computer)]
      (is (= 0x00 (cpu/read-register updated-computer :a)))
      (is (= 2r1 (cpu/read-flag updated-computer :ac)))
      (is (= 2r1 (cpu/read-flag updated-computer :c)))
      (is (= 2r1 (cpu/read-flag updated-computer :p)))
      (is (= 2r0 (cpu/read-flag updated-computer :s)))
      (is (= 2r1 (cpu/read-flag updated-computer :z))))))

(deftest testing-sub-a
  (testing "sub-a"
    (let [a  0x3e
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :a a))
          updated-computer (subject/sub-a initial-computer)]
      (is (= 0x00 (cpu/read-register updated-computer :a)))
      (is (= 2r1 (cpu/read-flag updated-computer :ac)))
      (is (= 2r1 (cpu/read-flag updated-computer :c)))
      (is (= 2r1 (cpu/read-flag updated-computer :p)))
      (is (= 2r0 (cpu/read-flag updated-computer :s)))
      (is (= 2r1 (cpu/read-flag updated-computer :z))))))

(deftest testing-sub-m
  (testing "sub-m"
    (let [a  0x01
          h  0x12
          l  0x34
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :a a)
                             (mem/store-memory-hl 0x01))
          updated-computer (subject/sub-m initial-computer)]
      (is (= 0x00 (cpu/read-register updated-computer :a)))
      (is (= 2r1 (cpu/read-flag updated-computer :ac)))
      (is (= 2r1 (cpu/read-flag updated-computer :c)))
      (is (= 2r1 (cpu/read-flag updated-computer :p)))
      (is (= 2r0 (cpu/read-flag updated-computer :s)))
      (is (= 2r1 (cpu/read-flag updated-computer :z))))))

(deftest testing-sbb-a
  (testing "sbb-l"
    (let [a  0x04
          l  0x02
          c  2r1
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :a a)
                             (cpu/store-register :l l)
                             (cpu/store-flag :c c))
          updated-computer (subject/sbb-l initial-computer)]
      (is (= 0x01 (cpu/read-register updated-computer :a)))
      (is (= 2r1 (cpu/read-flag updated-computer :ac)))
      (is (= 2r1 (cpu/read-flag updated-computer :c)))
      (is (= 2r0 (cpu/read-flag updated-computer :p)))
      (is (= 2r0 (cpu/read-flag updated-computer :s)))
      (is (= 2r0 (cpu/read-flag updated-computer :z))))))

(deftest testing-sbb-m
  (testing "sbb-m"
    (let [a  0x03
          h  0x12
          l  0x34
          c  2r1
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :a a)
                             (mem/store-memory-hl 0x00)
                             (cpu/store-flag :c c))
          updated-computer (subject/sbb-m initial-computer)]
      (is (= 0x02 (cpu/read-register updated-computer :a)))
      (is (= 2r1 (cpu/read-flag updated-computer :ac)))
      (is (= 2r1 (cpu/read-flag updated-computer :c)))
      (is (= 2r0 (cpu/read-flag updated-computer :p)))
      (is (= 2r0 (cpu/read-flag updated-computer :s)))
      (is (= 2r0 (cpu/read-flag updated-computer :z))))))

(deftest testing-ana-c
  (testing "ana-c"
    (let [a  0xfc
          c  0x0f
          cf 2r1
          initial-computer (-> cpu/fresh-computer
                             (cpu/store-register :a a)
                             (cpu/store-register :c c)
                             (cpu/store-flag :c cf))
          updated-computer (subject/ana-c initial-computer)]
      (is (= 0x0c (cpu/read-register updated-computer :a)))
      (is (= 2r0 (cpu/read-flag updated-computer :c)))
      (is (= 2r1 (cpu/read-flag updated-computer :p)))
      (is (= 2r0 (cpu/read-flag updated-computer :s)))
      (is (= 2r0 (cpu/read-flag updated-computer :z))))))
