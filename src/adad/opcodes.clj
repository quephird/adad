(ns adad.opcodes
  (:require [adad.cpu :as cpu]
            [adad.memory :as mem]
            [adad.util :refer [<< >> & ! auxiliary-carry carry parity sign twos-complement zero]]))

;; TODO: Should add metadata to all functions including:
;;        * flags set {:flags-set :ac :c :p :s :z}
;;        * number of cycles {:cycles 3}
;;        * number of bytes to move the program counter {:bytes 1}

(defn nop
  "No operation"
  {:flags-affected [] :cycles 1 :bytes 1}
  [computer]
  computer)

(defn- make-lxi-function
  "Makes a function that loads the passed in register pair with two bytes:
   b2 gets assigned to the first register, b1 to the second one;
   no flags affected"
  [register-pair]
  (fn [computer b1 b2]
    (cpu/store-register computer register-pair (+ (<< b2 8) b1))))

(doseq [[fn-name register-pair] [[:b :bc] [:d :de] [:h :hl] [:sp :sp]]]
  (intern *ns*
          (with-meta (symbol (format "lxi-%s" (name fn-name)))
                     {:flags-affected [] :cycles 3 :bytes 3})
          (make-lxi-function register-pair)))

(defn- make-stax-function
  "Makes a function that stores the contents of the accumulator A
   at the memory location in the register pair passed in;
   no flags affected"
  [register-pair]
  (fn [computer]
    (let [a       (cpu/read-register computer :a)
          address (cpu/read-register computer register-pair)]
      (mem/store-memory computer address a))))

(doseq [[fn-name register-pair] [[:b :bc] [:d :de]]]
  (intern *ns*
          (with-meta (symbol (format "stax-%s" (name fn-name)))
                     {:flags-affected [] :cycles 1 :bytes 2})
          (make-stax-function register-pair)))

(defn- make-inx-function
  "Makes a function that increments the value in the register pair passed in;
   no flags affected"
  [register-pair]
  (fn [computer]
    (->> register-pair
      (cpu/read-register computer)
      inc
      (cpu/store-register computer register-pair))))

(doseq [[fn-name register-pair] [[:b :bc] [:d :de] [:h :hl] [:sp :sp]]]
  (intern *ns*
          (with-meta (symbol (format "inx-%s" (name fn-name)))
                     {:flags-affected [] :cycles 1 :bytes 1})
          (make-inx-function register-pair)))

(defn- make-inr-function
  "Makes a function that increments the value in the register passed;
  flags affected: zero, sign, parity, auxiliary carry"
  [register]
  (fn [computer]
    (let [reg-val     (cpu/read-register computer register)
          new-reg-val (& 0xff (inc reg-val))
          new-p       (parity new-reg-val)
          new-s       (sign new-reg-val)
          new-z       (zero new-reg-val)
          new-ac      (auxiliary-carry reg-val 0x01)]
      (-> computer
        (cpu/store-flag :ac new-ac)
        (cpu/store-flag :p new-p)
        (cpu/store-flag :s new-s)
        (cpu/store-flag :z new-z)
        (cpu/store-register register new-reg-val)))))

(doseq [register [:a :b :c :d :e :h :l]]
  (intern *ns*
          (with-meta (symbol (format "inr-%s" (name register)))
                     {:flags-affected [:ac :p :s :z] :cycles 1 :bytes 1})
          (make-inr-function register)))

(defn inr-m
  "Increments the value contained at the memory location pointed
   to by the HL register pair; flags affected: zero, sign, parity,
   auxiliary carry"
   {:flags-affected [:ac :p :s :z] :cycles 1 :bytes 1}
  [computer]
  (let [h           (cpu/read-register computer :h)
        l           (cpu/read-register computer :l)
        address     (+ (<< h 8) l)
        mem-val     (mem/read-memory computer address)
        new-mem-val (& 0xff (inc mem-val))
        new-p       (parity new-mem-val)
        new-s       (sign new-mem-val)
        new-z       (zero new-mem-val)
        new-hc      (if (zero? (& new-mem-val 0x0f)) 0x01 0x00)]
    (-> computer
      (cpu/store-flag :hc new-hc)
      (cpu/store-flag :p new-p)
      (cpu/store-flag :s new-s)
      (cpu/store-flag :z new-z)
      (mem/store-memory address new-mem-val))))

(defn- make-dcr-function
  "Makes a function to decrement the value in the register passed;
  flags affected: zero, sign, parity, auxiliary carry"
  [register]
  (fn [computer]
    (let [reg-val     (cpu/read-register computer register)
          new-reg-val (& 0xff (dec reg-val))
          new-p       (parity new-reg-val)
          new-s       (sign new-reg-val)
          new-z       (zero new-reg-val)
          new-ac      (auxiliary-carry reg-val 0xFF)]
      (-> computer
        (cpu/store-flag :ac new-ac)
        (cpu/store-flag :p new-p)
        (cpu/store-flag :s new-s)
        (cpu/store-flag :z new-z)
        (cpu/store-register register new-reg-val)))))

(doseq [to-sym [:a :b :c :d :e :h :l]]
  (intern *ns*
          (with-meta (symbol (format "dcr-%s" (name to-sym)))
                     {:flags-affected [:ac :p :s :z] :cycles 1 :bytes 1})
          (make-dcr-function to-sym)))

(defn dcr-m
  "Decrements the value contained at the memory location pointed
   to by the HL register pair; flags affected: zero, sign, parity,
   auxiliary carry"
   {:flags-affected [:ac :p :s :z] :cycles 1 :bytes 1}
  [computer]
  (let [h           (cpu/read-register computer :h)
        l           (cpu/read-register computer :l)
        address     (+ (<< h 8) l)
        mem-val     (mem/read-memory computer address)
        new-mem-val (& 0xff (dec mem-val))
        new-p       (parity new-mem-val)
        new-s       (sign new-mem-val)
        new-z       (zero new-mem-val)
        new-hc      (if (zero? (& new-mem-val 0x0f)) 0x01 0x00)]
    (-> computer
      (cpu/store-flag :hc new-hc)
      (cpu/store-flag :p new-p)
      (cpu/store-flag :s new-s)
      (cpu/store-flag :z new-z)
      (mem/store-memory address new-mem-val))))

(defn- make-mvi-function
  "Helper function to load the specified register
   with the byte value passed in; no flags affected"
  [register]
  (fn [computer new-val]
    (cpu/store-register computer register new-val)))

(doseq [to-sym [:a :b :c :d :e :h :l]]
  (intern *ns*
          (with-meta (symbol (format "mvi-%s" (name to-sym)))
                     {:flags-affected [] :cycles 1 :bytes 1})
          (make-mvi-function to-sym)))

(defn mvi-m
  "Loads the smemory location pointed to by the HL register pair
   with the byte value passed in; no flags affected"
   {:flags-affected [] :cycles 1 :bytes 1}
  [computer b1]
  (let [h           (cpu/read-register computer :h)
        l           (cpu/read-register computer :l)
        address     (+ (<< h 8) l)]
    (-> computer
      (mem/store-memory address b1))))

(defn rlc
  "Rotates the bits in the accumulator A to the left;
   moves the old 7th bit into the carry flag and the 0th bit of A"
   {:flags-affected [:c] :cycles 1 :bytes 1}
  [computer]
  (let [a       (cpu/read-register computer :a)
        bit-7   (-> a (& 2r10000000) (>> 7))
        new-a   (-> a (<< 1) (+ bit-7) (& 2r11111111))]
    (-> computer
      (cpu/store-register :a new-a)
      (cpu/store-flag :c bit-7))))

(defn- make-dad-function
  "Adds the value of the register pair passed in
   to that of the HL register pair; carry flag affected."
  [reg-pair]
  (fn [computer]
    (let [reg-val (cpu/read-register computer reg-pair)
          hl      (cpu/read-register computer :hl)
          new-hl  (+ hl reg-val)
          new-c   (-> new-hl (& 0x10000) (>> 16))]
      (-> computer
        (cpu/store-register :hl new-hl)
        (cpu/store-flag :c new-c)))))

(doseq [[mnemonic reg-pair] [["b" :bc] ["d" :de] ["h" :hl] ["sp" :sp]]]
  (intern *ns*
          (with-meta (symbol (format "dad-%s" mnemonic))
                     {:flags-affected [:c] :cycles 3 :bytes 1})
          (make-dad-function reg-pair)))

(defn- make-ldax-function
  "Stores the contents of the memory location pointed to
   in the passed in register pair passed into the accumulator A"
  [reg-pair]
  (fn [computer]
    (let [a       (cpu/read-register computer :a)
          address (cpu/read-register computer reg-pair)
          new-a   (mem/read-memory computer address)]
      (cpu/store-register computer :a new-a))))

(doseq [[mnemonic reg-pair] [["b" :bc] ["d" :de]]]
  (intern *ns*
          (with-meta (symbol (format "ldax-%s" mnemonic))
                     {:flags-affected [:c] :cycles 2 :bytes 1})
          (make-ldax-function reg-pair)))

(defn- make-dcx-function
  "Decrements the contents of the register pair passed in;
   no carry flags affected"
  [reg-pair]
  (fn [computer]
    (->> reg-pair
      (cpu/read-register computer)
      dec
      (cpu/store-register computer reg-pair))))

(doseq [[mnemonic reg-pair] [["b" :bc] ["d" :de] ["h" :hl] ["sp" :sp]]]
  (intern *ns*
          (with-meta (symbol (format "dcx-%s" mnemonic))
                     {:flags-affected [] :cycles 1 :bytes 1})
          (make-dcx-function reg-pair)))

(defn rrc
  "Rotates the bits in the accumulator A to the right;
   moves the old 0th bit into the carry flag and the 7th bit of A"
  {:flags-affected [:c] :cycles 1 :bytes 1}
  [computer]
  (let [a       (cpu/read-register computer :a)
        bit-0   (& 2r00000001 a)
        new-a   (-> a (>> 1) (+ (<< bit-0 7)))]
    (-> computer
      (cpu/store-register :a new-a)
      (cpu/store-flag :c bit-0))))

(defn ral
  "Rotates the contents of the accumulator A together with
   the carry flag to the left"
  {:flags-affected [:c] :cycles 1 :bytes 1}
  [computer]
  (let [a       (cpu/read-register computer :a)
        bit-7   (-> a (& 2r10000000) (>> 7))
        c       (cpu/read-flag computer :c)
        new-a   (-> a (<< 1) (+ c) (& 2r11111111))]
    (-> computer
      (cpu/store-register :a new-a)
      (cpu/store-flag :c bit-7))))

(defn rar
  "Rotates the contents of the accumulator A together with
   the carry flag to the right"
  {:flags-affected [:c] :cycles 1 :bytes 1}
  [computer]
  (let [a       (cpu/read-register computer :a)
        bit-0   (& a 2r00000001)
        c       (cpu/read-flag computer :c)
        new-a   (-> a (>> 1) (+ (<< c 7)) (& 2r11111111))]
    (-> computer
      (cpu/store-register :a new-a)
      (cpu/store-flag :c bit-0))))

(defn shld
  "Loads the value in the H register into the memory location
   represented by the two bytes passed in, and the value in the
   L register into the very next memory location; no carry flags
   affected"
  {:flags-affected [] :cycles 5 :bytes 3}
  [computer b1 b2]
  (let [address (+ (<< b1 8) b2)
        h       (cpu/read-register computer :h)
        l       (cpu/read-register computer :l)]
    (-> computer
      (mem/store-memory address l)
      (mem/store-memory (inc address) h))))

(defn lhld
  "Loads the values in the memory address passed in, and the next one,
   into the H and L registers; no carry flags affected"
  {:flags-affected [] :cycles 5 :bytes 3}
  [computer b1 b2]
  (let [address (+ (<< b2 8) b1)
        h       (mem/read-memory computer (inc address))
        l       (mem/read-memory computer address)]
    (-> computer
      (cpu/store-register :h h)
      (cpu/store-register :l l))))

(defn cma
  "Sets the value of the accumulator to the complement its current value;
   no flags affected"
  {:flags-affected [] :cycles 1 :bytes 1}
  [computer]
  (as-> computer $
    (cpu/read-register $ :a)
    (! $)
    (cpu/store-register computer :a $)))

(defn lda
  "Stores the contents of the memory location represented
   by the two bytes passed into the accumulator A;
   no flags affected"
  {:flags-affected [] :cycles 4 :bytes 3}
  [computer b1 b2]
  (let [address (+ (<< b2 8) b1)
        a       (mem/read-memory computer address)]
    (cpu/store-register computer :a a)))

(defn sta
  "Stores the contents of the accumulator A into the
   memory location represented by the two bytes passed in;
   no flags affected"
  {:flags-affected [] :cycles 3 :bytes 3}
  [computer b1 b2]
  (let [address (+ (<< b2 8) b1)
        a       (cpu/read-register computer :a)]
    (mem/store-memory computer address a)))

(defn stc
  "Sets the carry bit to one unconditionally"
  {:flags-affected [] :cycles 1 :bytes 1}
  [computer]
  (cpu/store-flag computer :c 2r1))

(defn cmc
  "Updates the carry flag with the complement of its current value"
  {:flags-affected [] :cycles 1 :bytes 1}
  [computer]
  (let [c     (cpu/read-flag computer :c)
        new-c (& (! c) 0x01)]
    (cpu/store-flag computer :c new-c)))

(defn- make-mov-function
  "Makes a function that moves the value from the second register
   passed in to the first register passed in; no flags are affected
   in any of the resultant functions"
  [to-sym from-sym]
  (fn [computer]
    (let [from-sym-val (cpu/read-register computer from-sym)]
      (cpu/store-register computer to-sym from-sym-val))))

(doseq [from-sym [:a :b :c :d :e :h :l]
        to-sym   [:a :b :c :d :e :h :l]]
  (intern *ns*
          (with-meta (symbol (format "mov-%s-%s" (name to-sym) (name from-sym)))
                     {:flags-affected [] :cycles 1 :bytes 1})
          (make-mov-function to-sym from-sym)))

(defn- make-mov-from-m-function
  "Makes a function that moves the value from the memory location
   pointed to in the HL register pair to the first register passed in;
   no flags are affected in any of the resultant functions"
  [to-sym]
  (fn [computer]
    (let [mem-val (as-> computer $
                    (cpu/read-register $ :hl)
                    (mem/read-memory computer $))]
      (cpu/store-register computer to-sym mem-val))))

(doseq [to-sym [:a :b :c :d :e :h :l]]
  (intern *ns*
          (with-meta (symbol (format "mov-%s-m" (name to-sym)))
                     {:flags-affected [] :cycles 1 :bytes 1})
          (make-mov-from-m-function to-sym)))

(defn- make-mov-to-m-function
  "Makes a function that moves the value from a register
   to the memory location pointed to in the HL register pair;
   no flags are affected in any of the resultant functions"
  [from-sym]
  (fn [computer]
    (let [mem-val (as-> computer $
                    (cpu/read-register $ :hl)
                    (mem/read-memory computer $))]
      (cpu/store-register computer from-sym mem-val))))

(doseq [from-sym [:a :b :c :d :e :h :l]]
  (intern *ns*
          (with-meta (symbol (format "mov-m-%s" (name from-sym)))
                     {:flags-affected [] :cycles 1 :bytes 1})
          (make-mov-to-m-function from-sym)))

(defn- make-add-function
  "Makes a function that adds the value from the register
   passed in to the A register; flags affected:
   zero, sign, parity, carry, auxiliary carry"
  [from-sym]
  (fn [computer]
    (let [from-reg-val (cpu/read-register computer from-sym)
          old-a        (cpu/read-register computer :a)
          new-value    (+ old-a from-reg-val)
          new-a        (& new-value 0xff)
          new-ac       (auxiliary-carry from-reg-val old-a)
          new-c        (carry from-reg-val old-a)
          new-p        (parity new-value)
          new-s        (sign new-value)
          new-z        (zero new-value)]
      (-> computer
        (cpu/store-register :a new-a)
        (cpu/store-flag :ac new-ac)
        (cpu/store-flag :c new-c)
        (cpu/store-flag :p new-p)
        (cpu/store-flag :s new-s)
        (cpu/store-flag :z new-z)))))

(doseq [from-sym [:a :b :c :d :e :h :l]]
  (intern *ns*
          (with-meta (symbol (format "add-%s" (name from-sym)))
                     {:flags-affected [:ac :c :p :s :z] :cycles 1 :bytes 1})
          (make-add-function from-sym)))

(defn add-m
  "Adds the value contained in the memory location, pointed
   to by the HL register pair, to the A register; flags affected:
   zero, sign, parity, carry, auxiliary carry"
  {:flags-affected [:ac :c :p :s :z] :cycles 1 :bytes 1}
  [computer]
  (let [from-addr-val (mem/read-memory-hl computer)
        old-a         (cpu/read-register computer :a)
        new-value     (+ old-a from-addr-val)
        new-a         (& new-value 0xff)
        new-ac        (auxiliary-carry from-addr-val old-a)
        new-c         (carry from-addr-val old-a)
        new-p         (parity new-value)
        new-s         (sign new-value)
        new-z         (zero new-value)]
    (-> computer
      (cpu/store-register :a new-a)
      (cpu/store-flag :ac new-ac)
      (cpu/store-flag :c new-c)
      (cpu/store-flag :p new-p)
      (cpu/store-flag :s new-s)
      (cpu/store-flag :z new-z))))

(defn make-adc-function
  "Makes a function that adds the value from the register
   passed in, as well as the carry flag, to the A register;
   flags affected: zero, sign, parity, carry, auxiliary carry"
  [from-sym]
  (fn [computer]
    (let [from-reg-val (cpu/read-register computer from-sym)
          old-a        (cpu/read-register computer :a)
          old-c        (cpu/read-flag computer :c)
          new-value    (+ old-a from-reg-val old-c)
          new-a        (& new-value 0xff)
          new-ac       (auxiliary-carry from-reg-val old-a old-c)
          new-c        (carry from-reg-val old-a old-c)
          new-p        (parity new-value)
          new-s        (sign new-value)
          new-z        (zero new-value)]
      (-> computer
        (cpu/store-register :a new-a)
        (cpu/store-flag :ac new-ac)
        (cpu/store-flag :c new-c)
        (cpu/store-flag :p new-p)
        (cpu/store-flag :s new-s)
        (cpu/store-flag :z new-z)))))

(doseq [from-sym [:a :b :c :d :e :h :l]]
  (intern *ns*
          (with-meta (symbol (format "adc-%s" (name from-sym)))
                     {:flags-affected [:ac :c :p :s :z] :cycles 1 :bytes 1})
          (make-adc-function from-sym)))

(defn adc-m
  "Adds the value contained in the memory location, pointed
   to by the HL register pair, and the carry flag to the A register;
   flags affected: zero, sign, parity, carry, auxiliary carry"
  {:flags-affected [:ac :c :p :s :z] :cycles 1 :bytes 1}
  [computer]
  (let [from-addr-val (mem/read-memory-hl computer)
        old-a         (cpu/read-register computer :a)
        old-c         (cpu/read-flag computer :c)
        new-value     (+ old-a from-addr-val old-c)
        new-a         (& new-value 0xff)
        new-ac        (auxiliary-carry from-addr-val old-a old-c)
        new-c         (carry from-addr-val old-a old-c)
        new-p         (parity new-value)
        new-s         (sign new-value)
        new-z         (zero new-value)]
    (-> computer
      (cpu/store-register :a new-a)
      (cpu/store-flag :ac new-ac)
      (cpu/store-flag :c new-c)
      (cpu/store-flag :p new-p)
      (cpu/store-flag :s new-s)
      (cpu/store-flag :z new-z))))

(doseq [from-sym [:a :b :c :d :e :h :l]]
  (intern *ns*
          (symbol (format "adc-%s" (name from-sym)))
          (make-adc-function from-sym)))

(defn make-sub-function
  "Makes a function that subtracts the value from the register
   passed in from the A register; flags affected:
   zero, sign, parity, carry, auxiliary carry"
  [from-sym]
  (fn [computer]
    (let [reg-val      (cpu/read-register computer from-sym)
          neg-reg-val  (twos-complement reg-val)
          old-a        (cpu/read-register computer :a)
          new-value    (+ old-a neg-reg-val)
          new-a        (& new-value 0xff)
          new-ac       (auxiliary-carry neg-reg-val old-a)
          new-c        (carry neg-reg-val old-a)
          new-p        (parity new-value)
          new-s        (sign new-value)
          new-z        (zero new-value)]
      (-> computer
        (cpu/store-register :a new-a)
        (cpu/store-flag :ac new-ac)
        (cpu/store-flag :c new-c)
        (cpu/store-flag :p new-p)
        (cpu/store-flag :s new-s)
        (cpu/store-flag :z new-z)))))

(doseq [from-sym [:a :b :c :d :e :h :l]]
  (intern *ns*
          (symbol (format "sub-%s" (name from-sym)))
          (make-sub-function from-sym)))

(defn sub-m
  "Subtract the value contained in the memory location, pointed
   to by the HL register pair, from the A register;
   flags affected: zero, sign, parity, carry, auxiliary carry"
  [computer]
  (let [addr-val     (mem/read-memory-hl computer)
        neg-addr-val (twos-complement addr-val)
        old-a        (cpu/read-register computer :a)
        new-value    (+ old-a neg-addr-val)
        new-a        (& new-value 0xff)
        new-ac       (auxiliary-carry neg-addr-val old-a)
        new-c        (carry neg-addr-val old-a)
        new-p        (parity new-value)
        new-s        (sign new-value)
        new-z        (zero new-value)]
    (-> computer
      (cpu/store-register :a new-a)
      (cpu/store-flag :ac new-ac)
      (cpu/store-flag :c new-c)
      (cpu/store-flag :p new-p)
      (cpu/store-flag :s new-s)
      (cpu/store-flag :z new-z))))

(defn make-sbb-function
  "Makes a function that subtracts the value from the register
   passed in, as well as the carry bit, from the A register;
   flags affected: zero, sign, parity, carry, auxiliary carry"
  [from-sym]
  (fn [computer]
    (let [reg-val      (cpu/read-register computer from-sym)
          old-c        (cpu/read-flag computer :c)
          neg-val      (twos-complement (+ reg-val old-c))
          old-a        (cpu/read-register computer :a)
          new-value    (+ old-a neg-val)
          new-a        (& new-value 0xff)
          new-ac       (auxiliary-carry neg-val old-a)
          new-c        (carry neg-val old-a)
          new-p        (parity new-value)
          new-s        (sign new-value)
          new-z        (zero new-value)]
      (-> computer
        (cpu/store-register :a new-a)
        (cpu/store-flag :ac new-ac)
        (cpu/store-flag :c new-c)
        (cpu/store-flag :p new-p)
        (cpu/store-flag :s new-s)
        (cpu/store-flag :z new-z)))))

(doseq [from-sym [:a :b :c :d :e :h :l]]
  (intern *ns*
          (symbol (format "sbb-%s" (name from-sym)))
          (make-sbb-function from-sym)))

(defn sbb-m
  "Subtract the value contained in the memory location, pointed
   to by the HL register pair, and the carry bit from the A register;
   flags affected: zero, sign, parity, carry, auxiliary carry"
  [computer]
  (let [addr-val     (mem/read-memory-hl computer)
        old-c        (cpu/read-flag computer :c)
        neg-val      (twos-complement (+ addr-val old-c))
        old-a        (cpu/read-register computer :a)
        new-value    (+ old-a neg-val)
        new-a        (& new-value 0xff)
        new-ac       (auxiliary-carry neg-val old-a)
        new-c        (carry neg-val old-a)
        new-p        (parity new-value)
        new-s        (sign new-value)
        new-z        (zero new-value)]
    (-> computer
      (cpu/store-register :a new-a)
      (cpu/store-flag :ac new-ac)
      (cpu/store-flag :c new-c)
      (cpu/store-flag :p new-p)
      (cpu/store-flag :s new-s)
      (cpu/store-flag :z new-z))))

(defn make-ana-function
  "Makes a function that ANDs the value from the register
   passed in with the value in the A register. The carry bit
   is _always_ reset; flags affected: zero, sign, parity, carry"
  [from-sym]
  (fn [computer]
    (let [reg-val      (cpu/read-register computer from-sym)
          old-a        (cpu/read-register computer :a)
          new-val      (& reg-val old-a)
          new-p        (parity new-val)
          new-s        (sign new-val)
          new-z        (zero new-val)]
      (-> computer
        (cpu/store-register :a new-val)
        (cpu/store-flag :c 2r0)
        (cpu/store-flag :p new-p)
        (cpu/store-flag :s new-s)
        (cpu/store-flag :z new-z)))))

(doseq [from-sym [:a :b :c :d :e :h :l]]
  (intern *ns*
          (with-meta (symbol (format "ana-%s" (name from-sym)))
                     {:flags-set [:cf :pf :sf :zf] :cycles 1 :bytes 1})
          (make-ana-function from-sym)))


(defn hlt
  "Does nothing, and causes the program counter to remain in the same state"
  [computer]
  computer)

; Instead of simply making these a vector of hashes
; that can be looked up by a numeric index, I made it
; a nested hash so that 1) as I'm implementing opcodes
; I can have gaps in this structure, and 2) the relationship
; between the functions and the actual 8080 opcodes is far
; more explicit here.
(def opcodes
  {
   0x00 {:fn nop    :bytes 1 :cycles 1}
   0x01 {:fn lxi-b  :bytes 3 :cycles 3}
   0x02 {:fn stax-b :bytes 1 :cycles 2}
   0x03 {:fn inx-b  :bytes 1 :cycles 1}
   0x04 {:fn inr-b  :bytes 1 :cycles 1}
   0x05 {:fn dcr-b  :bytes 1 :cycles 1}
   0x06 {:fn mvi-b  :bytes 2 :cycles 2}
   0x07 {:fn rlc    :bytes 1 :cycles 1}
   0x08 {:fn nop    :bytes 1 :cycles 1}
   0x09 {:fn dad-b  :bytes 1 :cycles 3}
   0x0a {:fn ldax-b :bytes 1 :cycles 2}
   0x0b {:fn dcx-b  :bytes 1 :cycles 1}
   0x0c {:fn inr-c  :bytes 1 :cycles 1}
   0x0d {:fn dcr-c  :bytes 1 :cycles 1}
   0x0e {:fn mvi-c  :bytes 2 :cycles 2}
   0x0f {:fn rrc    :bytes 1 :cycles 1}
   0x10 {:fn nop    :bytes 1 :cycles 1}
   0x11 {:fn lxi-d  :bytes 3 :cycles 3}
   0x12 {:fn stax-d :bytes 1 :cycles 2}
   0x13 {:fn inx-d  :bytes 1 :cycles 1}
   0x14 {:fn inr-d  :bytes 1 :cycles 1}
   0x15 {:fn dcr-d  :bytes 1 :cycles 1}
   0x16 {:fn mvi-d  :bytes 2 :cycles 2}
   0x17 {:fn ral    :bytes 1 :cycles 1}
   0x18 {:fn nop    :bytes 1 :cycles 1}
   0x19 {:fn dad-d  :bytes 1 :cycles 3}
   0x1a {:fn ldax-d :bytes 1 :cycles 2}
   0x1b {:fn dcx-d  :bytes 1 :cycles 1}
   0x1c {:fn inr-e  :bytes 1 :cycles 1}
   0x1d {:fn dcr-e  :bytes 1 :cycles 1}
   0x1e {:fn mvi-e  :bytes 2 :cycles 2}
   0x1f {:fn rar    :bytes 1 :cycles 1}
   0x20 {:fn nop    :bytes 1 :cycles 1}
   0x21 {:fn lxi-h  :bytes 3 :cycles 3}
   0x22 {:fn shld   :bytes 3 :cycles 5}
   0x23 {:fn inx-h  :bytes 1 :cycles 1}
   0x24 {:fn inr-h  :bytes 1 :cycles 1}
   0x25 {:fn dcr-h  :bytes 1 :cycles 1}
   0x26 {:fn mvi-h  :bytes 2 :cycles 2}
   0x27 {:fn nop    :bytes 1 :cycles 1}
   0x28 {:fn nop    :bytes 1 :cycles 1}
   0x29 {:fn dad-h  :bytes 1 :cycles 3}
   0x2a {:fn lhld   :bytes 3 :cycles 5}
   0x2b {:fn dcx-h  :bytes 1 :cycles 1}
   0x2c {:fn inr-l  :bytes 1 :cycles 1}
   0x2d {:fn dcr-l  :bytes 1 :cycles 1}
   0x2e {:fn mvi-l  :bytes 2 :cycles 2}
   0x2f {:fn cma    :bytes 1 :cycles 1}
   0x30 {:fn nop    :bytes 1 :cycles 1}
   0x31 {:fn lxi-sp :bytes 3 :cycles 3}
   0x32 {:fn sta    :bytes 3 :cycles 3}
   0x33 {:fn inx-sp :bytes 1 :cycles 1}
   0x34 {:fn inr-m  :bytes 1 :cycles 1}
   0x35 {:fn dcr-m  :bytes 1 :cycles 1}
   0x36 {:fn mvi-m  :bytes 2 :cycles 2}
   0x37 {:fn stc    :bytes 1 :cycles 1}
   0x38 {:fn nop    :bytes 1 :cycles 1}
   0x39 {:fn dad-sp :bytes 1 :cycles 3}
   0x3a {:fn lda    :bytes 3 :cycles 4}
   0x3b {:fn dcx-sp :bytes 1 :cycles 1}
   0x3c {:fn inr-a  :bytes 1 :cycles 1}
   0x3d {:fn dcr-a  :bytes 1 :cycles 1}
   0x3e {:fn mvi-a  :bytes 2 :cycles 2}
   0x3f {:fn cmc    :bytes 1 :cycles 1}
   0x40 {:fn mov-b-b :bytes 1 :cycles 1}
   0x41 {:fn mov-b-c :bytes 1 :cycles 1}
   0x42 {:fn mov-b-d :bytes 1 :cycles 1}
   0x43 {:fn mov-b-e :bytes 1 :cycles 1}
   0x44 {:fn mov-b-h :bytes 1 :cycles 1}
   0x45 {:fn mov-b-l :bytes 1 :cycles 1}
   0x46 {:fn mov-b-m :bytes 1 :cycles 1}
   0x47 {:fn mov-b-a :bytes 1 :cycles 1}
   0x48 {:fn mov-c-b :bytes 1 :cycles 1}
   0x49 {:fn mov-c-c :bytes 1 :cycles 1}
   0x4a {:fn mov-c-d :bytes 1 :cycles 1}
   0x4b {:fn mov-c-e :bytes 1 :cycles 1}
   0x4c {:fn mov-c-h :bytes 1 :cycles 1}
   0x4d {:fn mov-c-l :bytes 1 :cycles 1}
   0x4e {:fn mov-c-m :bytes 1 :cycles 1}
   0x4f {:fn mov-c-a :bytes 1 :cycles 1}
   0x50 {:fn mov-d-b :bytes 1 :cycles 1}
   0x51 {:fn mov-d-c :bytes 1 :cycles 1}
   0x52 {:fn mov-d-d :bytes 1 :cycles 1}
   0x53 {:fn mov-d-e :bytes 1 :cycles 1}
   0x54 {:fn mov-d-h :bytes 1 :cycles 1}
   0x55 {:fn mov-d-l :bytes 1 :cycles 1}
   0x56 {:fn mov-d-m :bytes 1 :cycles 1}
   0x57 {:fn mov-d-a :bytes 1 :cycles 1}
   0x58 {:fn mov-e-b :bytes 1 :cycles 1}
   0x59 {:fn mov-e-c :bytes 1 :cycles 1}
   0x5a {:fn mov-e-d :bytes 1 :cycles 1}
   0x5b {:fn mov-e-e :bytes 1 :cycles 1}
   0x5c {:fn mov-e-h :bytes 1 :cycles 1}
   0x5d {:fn mov-e-l :bytes 1 :cycles 1}
   0x5e {:fn mov-e-m :bytes 1 :cycles 1}
   0x5f {:fn mov-e-a :bytes 1 :cycles 1}
   0x60 {:fn mov-h-b :bytes 1 :cycles 1}
   0x61 {:fn mov-h-c :bytes 1 :cycles 1}
   0x62 {:fn mov-h-d :bytes 1 :cycles 1}
   0x63 {:fn mov-h-e :bytes 1 :cycles 1}
   0x64 {:fn mov-h-h :bytes 1 :cycles 1}
   0x65 {:fn mov-h-l :bytes 1 :cycles 1}
   0x66 {:fn mov-h-m :bytes 1 :cycles 1}
   0x67 {:fn mov-h-a :bytes 1 :cycles 1}
   0x68 {:fn mov-l-b :bytes 1 :cycles 1}
   0x69 {:fn mov-l-c :bytes 1 :cycles 1}
   0x6a {:fn mov-l-d :bytes 1 :cycles 1}
   0x6b {:fn mov-l-e :bytes 1 :cycles 1}
   0x6c {:fn mov-l-h :bytes 1 :cycles 1}
   0x6d {:fn mov-l-l :bytes 1 :cycles 1}
   0x6e {:fn mov-l-m :bytes 1 :cycles 1}
   0x6f {:fn mov-l-a :bytes 1 :cycles 1}
   0x70 {:fn mov-m-b :bytes 1 :cycles 1}
   0x71 {:fn mov-m-c :bytes 1 :cycles 1}
   0x72 {:fn mov-m-d :bytes 1 :cycles 1}
   0x73 {:fn mov-m-e :bytes 1 :cycles 1}
   0x74 {:fn mov-m-h :bytes 1 :cycles 1}
   0x75 {:fn mov-m-l :bytes 1 :cycles 1}
   0x76 {:fn hlt     :bytes 0 :cycles 1}
   0x77 {:fn mov-m-a :bytes 1 :cycles 1}
   0x78 {:fn mov-a-b :bytes 1 :cycles 1}
   0x79 {:fn mov-a-c :bytes 1 :cycles 1}
   0x7a {:fn mov-a-d :bytes 1 :cycles 1}
   0x7b {:fn mov-a-e :bytes 1 :cycles 1}
   0x7c {:fn mov-a-h :bytes 1 :cycles 1}
   0x7d {:fn mov-a-l :bytes 1 :cycles 1}
   0x7e {:fn mov-a-m :bytes 1 :cycles 1}
   0x7f {:fn mov-a-a :bytes 1 :cycles 1}
   0x80 {:fn add-b   :bytes 1 :cycles 1}
   0x81 {:fn add-c   :bytes 1 :cycles 1}
   0x82 {:fn add-d   :bytes 1 :cycles 1}
   0x83 {:fn add-e   :bytes 1 :cycles 1}
   0x84 {:fn add-h   :bytes 1 :cycles 1}
   0x85 {:fn add-l   :bytes 1 :cycles 1}
   0x86 {:fn add-m   :bytes 1 :cycles 1}
   0x87 {:fn adc-a   :bytes 1 :cycles 1}
   0x88 {:fn adc-b   :bytes 1 :cycles 1}
   0x89 {:fn adc-c   :bytes 1 :cycles 1}
   0x8a {:fn adc-d   :bytes 1 :cycles 1}
   0x8b {:fn adc-e   :bytes 1 :cycles 1}
   0x8c {:fn adc-h   :bytes 1 :cycles 1}
   0x8d {:fn adc-l   :bytes 1 :cycles 1}
   0x8e {:fn adc-m   :bytes 1 :cycles 1}
   0x8f {:fn adc-a   :bytes 1 :cycles 1}
   0x90 {:fn sub-b   :bytes 1 :cycles 1}
   0x91 {:fn sub-c   :bytes 1 :cycles 1}
   0x92 {:fn sub-d   :bytes 1 :cycles 1}
   0x93 {:fn sub-e   :bytes 1 :cycles 1}
   0x94 {:fn sub-h   :bytes 1 :cycles 1}
   0x95 {:fn sub-l   :bytes 1 :cycles 1}
   0x96 {:fn sub-m   :bytes 1 :cycles 1}
   0x97 {:fn sub-a   :bytes 1 :cycles 1}
   0x98 {:fn sbb-b   :bytes 1 :cycles 1}
   0x99 {:fn sbb-c   :bytes 1 :cycles 1}
   0x9a {:fn sbb-d   :bytes 1 :cycles 1}
   0x9b {:fn sbb-e   :bytes 1 :cycles 1}
   0x9c {:fn sbb-h   :bytes 1 :cycles 1}
   0x9d {:fn sbb-l   :bytes 1 :cycles 1}
   0x9e {:fn sbb-m   :bytes 1 :cycles 1}
   0x9f {:fn sbb-a   :bytes 1 :cycles 1}
   0xa0 {:fn ana-b   :bytes 1 :cycles 1}
   0xa1 {:fn ana-c   :bytes 1 :cycles 1}
   0xa2 {:fn ana-d   :bytes 1 :cycles 1}
   0xa3 {:fn ana-e   :bytes 1 :cycles 1}
   0xa4 {:fn ana-h   :bytes 1 :cycles 1}
   0xa5 {:fn ana-l   :bytes 1 :cycles 1}
   ; 0xa6 {:fn sub-m   :bytes 1 :cycles 1}
   0xa7 {:fn ana-a   :bytes 1 :cycles 1}

   0xcb {:fn nop    :bytes 1 :cycles 1}

   0xd9 {:fn nop    :bytes 1 :cycles 1}

   0xdd {:fn nop    :bytes 1 :cycles 1}

   0xed {:fn nop    :bytes 1 :cycles 1}

   0xfd {:fn nop    :bytes 1 :cycles 1}
  })
