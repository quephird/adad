(ns adad.util-test
  (:require [adad.util :as subject]
            [clojure.test :refer :all]))

(deftest testing-complement
  (testing "single byte complement function returns correct values"
    (is (= 2r11111111 (subject/! 2r00000000)))
    (is (= 2r11111110 (subject/! 2r00000001)))
    (is (= 2r00000001 (subject/! 2r11111110)))
    (is (= 2r00000000 (subject/! 2r11111111)))
    (is (= 2r10101010 (subject/! 2r01010101)))))

(deftest testing-parity
  (testing "parity function returns the correct values"
    (is (= 0 (subject/parity 2r00000000)))
    (is (= 1 (subject/parity 2r00000001)))
    (is (= 1 (subject/parity 2r10000000)))
    (is (= 0 (subject/parity 2r11111111)))
    (is (= 1 (subject/parity 2r11111110)))))
