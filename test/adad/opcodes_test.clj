(ns adad.opcodes-test
  (:require [adad.opcodes :as subject]
            [adad.cpu :as cpu]
            [adad.util :refer [<< fresh-computer]]
            [clojure.test :refer :all]))

(deftest testing-lxi-b
  (let [b1          0x42
        b2          0xff
        updated-computer (subject/lxi-b fresh-computer b1 b2)]
    (is (= b2 (cpu/read-register updated-computer :b)))
    (is (= b1 (cpu/read-register updated-computer :c)))))

(deftest testing-stax-b
  (let [b1   0x23
        b2   0x45
        initial-computer (-> fresh-computer
                             (cpu/store-register :a 0xFF)
                             (cpu/store-register :b b1)
                             (cpu/store-register :c b2))
        updated-computer (subject/stax-b initial-computer)]
    (is (= 0xFF (get-in updated-computer [:memory (+ (<< b1 8) b2)])))))

(deftest testing-inx-b
  (testing "no overflow in c"
    (let [b    0x23
          c    0x45
          initial-computer (-> fresh-computer
                               (cpu/store-register :b b)
                               (cpu/store-register :c c))
          updated-computer (subject/inx-b initial-computer)]
      (is (= 0x23 (cpu/read-register updated-computer :b)))
      (is (= 0x46 (cpu/read-register updated-computer :c)))))

  (testing "overflow only in c"
    (let [b    0x23
          c    0xFF
          initial-computer (-> fresh-computer
                               (cpu/store-register :b b)
                               (cpu/store-register :c c))
          updated-computer (subject/inx-b initial-computer)]
      (is (= 0x24 (cpu/read-register updated-computer :b)))
      (is (= 0x00 (cpu/read-register updated-computer :c)))))

  (testing "overflow in bc"
    (let [b    0xFF
          c    0xFF
          initial-computer (-> fresh-computer
                               (cpu/store-register :b b)
                               (cpu/store-register :c c))
          updated-computer (subject/inx-b initial-computer)]
      (is (= 0x00 (cpu/read-register updated-computer :b)))
      (is (= 0x00 (cpu/read-register updated-computer :c))))))

(deftest testing-inr-b
  (testing "value in b register is incremented"
    (let [b   0x42
          initial-computer (-> fresh-computer
                             (cpu/store-register :b b))
          updated-computer (subject/inr-b initial-computer)]
      (is (= 0x43 (cpu/read-register updated-computer :b)))))

  (testing "half carry flag is set"
    (let [b   0x4f
          initial-computer (-> fresh-computer
                             (cpu/store-register :b b))
          updated-computer (subject/inr-b initial-computer)]
      (is (= 0x01 (cpu/read-flag updated-computer :hc)))))

  (testing "half carry flag is unset"
    (let [b   0x10
          initial-computer (-> fresh-computer
                             (cpu/store-register :b b)
                             (cpu/store-flag :hc 0x01))
          updated-computer (subject/inr-b initial-computer)]
      (is (= 0x00 (cpu/read-flag updated-computer :hc)))))

  (testing "parity flag is set"
    (let [b   0x00
          initial-computer (-> fresh-computer
                             (cpu/store-register :b b))
          updated-computer (subject/inr-b initial-computer)]
      (is (= 0x01 (cpu/read-flag updated-computer :p)))))

  (testing "parity flag is unset"
    (let [b   0x10
          initial-computer (-> fresh-computer
                             (cpu/store-register :b b)
                             (cpu/store-flag :p 0x01))
          updated-computer (subject/inr-b initial-computer)]
      (is (= 0x00 (cpu/read-flag updated-computer :p)))))

  (testing "sign flag is set"
    (let [b   0x7f
          initial-computer (-> fresh-computer
                             (cpu/store-register :b b))
          updated-computer (subject/inr-b initial-computer)]
      (is (= 0x01 (cpu/read-flag updated-computer :s)))))

  (testing "sign flag is unset"
    (let [b   0xff
          initial-computer (-> fresh-computer
                             (cpu/store-register :b b)
                             (cpu/store-flag :s 0x01))
          updated-computer (subject/inr-b initial-computer)]
      (is (= 0x00 (cpu/read-flag updated-computer :s)))))

  (testing "zero flag is set"
    (let [b   0xff
          initial-computer (-> fresh-computer
                             (cpu/store-register :b b))
          updated-computer (subject/inr-b initial-computer)]
      (is (= 0x01 (cpu/read-flag updated-computer :z)))))

  (testing "zero flag is unset"
    (let [b   0x00
          initial-computer (-> fresh-computer
                             (cpu/store-register :b b)
                             (cpu/store-flag :z 0x01))
          updated-computer (subject/inr-b initial-computer)]
      (is (= 0x00 (cpu/read-flag updated-computer :z))))))

(deftest testing-dcr-b
  (testing "value in b register is decremented"
    (let [b   0x43
          initial-computer (-> fresh-computer
                             (cpu/store-register :b b))
          updated-computer (subject/dcr-b initial-computer)]
      (is (= 0x42 (cpu/read-register updated-computer :b)))))

  ; TODO: Figure out if the HC flag is ever set in a DCR opcode
  (testing "half carry flag is unset"
    (let [b   0x10
          initial-computer (-> fresh-computer
                             (cpu/store-register :b b)
                             (cpu/store-flag :hc 0x01))
          updated-computer (subject/dcr-b initial-computer)]
      (is (= 0x00 (cpu/read-flag updated-computer :hc)))))

  (testing "parity flag is set"
    (let [b   0xff
          initial-computer (-> fresh-computer
                             (cpu/store-register :b b))
          updated-computer (subject/dcr-b initial-computer)]
      (is (= 0x01 (cpu/read-flag updated-computer :p)))))

  (testing "parity flag is unset"
    (let [b   0x01
          initial-computer (-> fresh-computer
                             (cpu/store-register :b b)
                             (cpu/store-flag :p 0x01))
          updated-computer (subject/dcr-b initial-computer)]
      (is (= 0x00 (cpu/read-flag updated-computer :p)))))

  (testing "sign flag is set"
    (let [b   0x00
          initial-computer (-> fresh-computer
                             (cpu/store-register :b b))
          updated-computer (subject/dcr-b initial-computer)]
      (is (= 0x01 (cpu/read-flag updated-computer :s)))))

  (testing "sign flag is unset"
    (let [b   0x7f
          initial-computer (-> fresh-computer
                             (cpu/store-register :b b)
                             (cpu/store-flag :s 0x01))
          updated-computer (subject/dcr-b initial-computer)]
      (is (= 0x00 (cpu/read-flag updated-computer :s)))))

  (testing "zero flag is set"
    (let [b   0xff
          initial-computer (-> fresh-computer
                             (cpu/store-register :b b))
          updated-computer (subject/inr-b initial-computer)]
      (is (= 0x01 (cpu/read-flag updated-computer :z)))))

  (testing "zero flag is unset"
    (let [b   0x00
          initial-computer (-> fresh-computer
                             (cpu/store-register :b b)
                             (cpu/store-flag :z 0x01))
          updated-computer (subject/inr-b initial-computer)]
      (is (= 0x00 (cpu/read-flag updated-computer :z))))))
