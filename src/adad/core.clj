(ns adad.core
  (:require [adad.cpu :as cpu]))

(defn load-file
  "Reads the file referenced by the name passed in and returns a new
   state of the computer with the file loaded into its memory
   beginning at the specified address."
  ([filename computer]
    (load-file filename computer 0))
  ([filename computer address]
    (let [file       (java.io.File. filename)
          file-size  (.length file)]
      (with-open [dis (java.io.DataInputStream. (clojure.java.io/input-stream file))]
        (reduce (fn [acc val]
                  (assoc-in acc [:memory (+ val address)] (.readUnsignedByte dis)))
                computer
                (range file-size))))))

(defn -main [program-file]
  ;; initialize computer
  ; (loop [computer (cpu/fresh-computer)]
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
