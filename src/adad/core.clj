(ns adad.core
  (:require [adad.cpu :as cpu]))

(defn load-file-into-memory
  "Reads the file referenced by the name passed in and loads
   it into the computer's memory beginning at the specified address."
  [filename computer address]
  (let [file       (java.io.File. filename)
        file-size  (.length file)
        new-memory (into-array (:memory computer))]
    ;; We first clone the current state of the computer's memory into a mutable array,
    ;; then mutate it one byte at a time, then clone the array back into a vector
    ;; and assoc it into the computer. We do this because we felt that loading the
    ;; the file into memory should be seen as a single atomic "transaction" versus
    ;; separate ones for each byte being loaded.
    (with-open [in (java.io.DataInputStream. (clojure.java.io/input-stream file))]
      (loop [i  0]
        (when (< i file-size)
          (do
            (aset new-memory (+ i address) (.readUnsignedByte in))
            (recur (inc i))))))
    (assoc computer :memory (vec new-memory))))

(defn -main [program-file]
  ;; initialize computer
  (loop [computer (cpu/fresh-computer)]
    )
  ;; read in entire rom into memory
  ;; start at memory location 0
  ;; loop
  ;; Read in one byte at a time
  ;; lookup opcode
  ;; get number of bytes the opcode represents, b
  ;; read the next b-1 bytes
  ;; execute opcode
  ;; continue if still more bytes
  ;; return current state of computer
  )
