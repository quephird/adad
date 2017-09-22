(ns adad.core-test
(:require [adad.core :as subject]
          [adad.cpu :as cpu]
          [adad.memory :as mem]
          [clojure.test :refer :all]))

(deftest testing-load-file-into-memory
  (testing "file loads into memory starting at zeroth memory address"
    (let [test-filename  "./test/adad/test_rom.h"
          computer       cpu/fresh-computer
          new-computer   (subject/load-file-into-memory test-filename computer)]
      (is (= 0xff (mem/read-memory new-computer 0)))
      (is (= 0xff (mem/read-memory new-computer 15)))
      (is (= 0x00 (mem/read-memory new-computer 16)))))

  (testing "file loads into memory starting at specified memory address"
    (let [test-filename  "./test/adad/test_rom.h"
          computer       cpu/fresh-computer
          new-computer   (subject/load-file-into-memory test-filename computer 2048)]
      (is (= 0x00 (mem/read-memory new-computer 0)))
      (is (= 0x00 (mem/read-memory new-computer 2047)))
      (is (= 0xff (mem/read-memory new-computer 2048)))
      (is (= 0xff (mem/read-memory new-computer 2063)))
      (is (= 0x00 (mem/read-memory new-computer 2064))))))
