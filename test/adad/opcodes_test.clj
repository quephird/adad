(ns adad.opcodes-test
  (:require [adad.opcodes :as subject]
            [adad.cpu :as cpu]
            [adad.util :refer [<< fresh-computer]]
            [clojure.test :refer :all]))

(deftest testing-lxi-b-d16
  (let [b1          0x42
        b2          0xff
        updated-computer (subject/lxi-b-d16 fresh-computer b1 b2)]
    (is (= b2 (cpu/read updated-computer :b)))
    (is (= b1 (cpu/read updated-computer :c)))))

(deftest testing-stax-b
  (let [b1   0x23
        b2   0x45
        initial-computer (-> fresh-computer
                             (cpu/store :a 0xFF)
                             (cpu/store :b b1)
                             (cpu/store :c b2))
        updated-computer (subject/stax-b initial-computer)]
    (is (= 0xFF (get-in updated-computer [:memory (+ (<< b1 8) b2)])))))

(deftest testing-inx-b
  (testing "no overflow in c"
    (let [b    0x23
          c    0x45
          initial-computer (-> fresh-computer
                               (cpu/store :b b)
                               (cpu/store :c c))
          updated-computer (subject/inx-b initial-computer)]
      (is (= 0x23 (cpu/read updated-computer :b)))
      (is (= 0x46 (cpu/read updated-computer :c)))))

  (testing "overflow only in c"
    (let [b    0x23
          c    0xFF
          initial-computer (-> fresh-computer
                               (cpu/store :b b)
                               (cpu/store :c c))
          updated-computer (subject/inx-b initial-computer)]
      (is (= 0x24 (cpu/read updated-computer :b)))
      (is (= 0x00 (cpu/read updated-computer :c)))))

  (testing "overflow in bc"
    (let [b    0xFF
          c    0xFF
          initial-computer (-> fresh-computer
                               (cpu/store :b b)
                               (cpu/store :c c))
          updated-computer (subject/inx-b initial-computer)]
      (is (= 0x00 (cpu/read updated-computer :b)))
      (is (= 0x00 (cpu/read updated-computer :c))))))

#_(deftest testing-inr-b
  (testing "no overflow in b"
    (let [b   0x42
          initial-computer (-> fresh-computer
                             (cpu/store :b b))
          updated-computer (subject/inr-b initial-computer)]
      (is = 0x43 (cpu/read updated-computer :b))))

)
