(ns adad.opcodes-test
  (:require [adad.opcodes :as subject]
            [adad.cpu :as cpu]
            [adad.util :refer [<< read store]]
            [clojure.test :refer :all]))

(def fresh-cpu
  {:a 2r00000000
   :b 2r00000000
   :c 2r00000000
   :d 2r00000000
   :e 2r00000000
   :pc nil})

;; Uuuuggghhhhhhh there is no unsigned byte type in Clojure;
;; byte has range -128 <= b <= 127. ;_;
(def fresh-memory
  (apply vector-of :int (repeat 65536 0x00)))

(def fresh-computer
  {:cpu fresh-cpu :memory fresh-memory})

(deftest testing-lxi-b-d16
  (let [b1          0x42
        b2          0xff
        updated-computer (subject/lxi-b-d16 fresh-computer b1 b2)]
    (is (= b2 (read updated-computer :b)))
    (is (= b1 (read updated-computer :c)))))

(deftest testing-stax-b
  (let [b1   0x23
        b2   0x45
        initial-computer (-> fresh-computer
                             (store :a 0xFF)
                             (store :b b1)
                             (store :c b2))
        updated-computer (subject/stax-b initial-computer)]
    (is (= 0xFF (get-in updated-computer [:memory (+ (<< b1 8) b2)])))))

(deftest testing-inx-b
  (testing "no overflow in c"
    (let [b    0x23
          c    0x45
          initial-computer (-> fresh-computer
                               (store :b b)
                               (store :c c))
          updated-computer (subject/inx-b initial-computer)]
      (is (= 0x23 (read updated-computer :b)))
      (is (= 0x46 (read updated-computer :c)))))

  (testing "overflow only in c"
    (let [b    0x23
          c    0xFF
          initial-computer (-> fresh-computer
                               (store :b b)
                               (store :c c))
          updated-computer (subject/inx-b initial-computer)]
      (is (= 0x24 (read updated-computer :b)))
      (is (= 0x00 (read updated-computer :c)))))

  (testing "overflow in bc"
    (let [b    0xFF
          c    0xFF
          initial-computer (-> fresh-computer
                               (store :b b)
                               (store :c c))
          updated-computer (subject/inx-b initial-computer)]
      (is (= 0x00 (read updated-computer :b)))
      (is (= 0x00 (read updated-computer :c))))))
