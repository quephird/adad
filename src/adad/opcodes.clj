(ns adad.opcodes
  (:require [adad.cpu :as cpu]
            [adad.memory :as mem]
            [adad.util :refer [<< >> & ! auxiliary-carry carry parity sign twos-complement zero]]))

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
        new-ac      (auxiliary-carry new-mem-val 0x01)]
    (-> computer
      (cpu/store-flag :ac new-ac)
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
        new-ac      (auxiliary-carry new-mem-val 0xff)]
    (-> computer
      (cpu/store-flag :ac new-ac)
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

(defn- make-adc-function
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

(defn- make-sub-function
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
          (with-meta (symbol (format "sub-%s" (name from-sym)))
                     {:flags-affected [:ac :c :p :s :z] :cycles 1 :bytes 1})
          (make-sub-function from-sym)))

(defn sub-m
  "Subtract the value contained in the memory location, pointed
   to by the HL register pair, from the A register;
   flags affected: zero, sign, parity, carry, auxiliary carry"
  {:flags-affected [:ac :c :p :s :z] :cycles 1 :bytes 1}
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

(defn- make-sbb-function
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
          (with-meta (symbol (format "sbb-%s" (name from-sym)))
                     {:flags-affected [:ac :c :p :s :z] :cycles 1 :bytes 1})
          (make-sbb-function from-sym)))

(defn sbb-m
  "Subtract the value contained in the memory location, pointed
   to by the HL register pair, and the carry bit from the A register;
   flags affected: zero, sign, parity, carry, auxiliary carry"
  {:flags-affected [:ac :c :p :s :z] :cycles 1 :bytes 1}
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

(defn- make-ana-function
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
                     {:flags-set [:c :p :s :z] :cycles 1 :bytes 1})
          (make-ana-function from-sym)))

(defn ana-m
  "ANDs the value contained in the memory location, pointed
   to by the HL register pair, with the value in the A register.
   The carry bit is _always_ reset; flags affected: zero, sign,
   parity, carry"
  {:flags-set [:c :p :s :z] :cycles 1 :bytes 1}
  [from-sym]
  (fn [computer]
    (let [addr-val     (mem/read-memory-hl computer)
          old-a        (cpu/read-register computer :a)
          new-val      (& addr-val old-a)
          new-p        (parity new-val)
          new-s        (sign new-val)
          new-z        (zero new-val)]
      (-> computer
        (cpu/store-register :a new-val)
        (cpu/store-flag :c 2r0)
        (cpu/store-flag :p new-p)
        (cpu/store-flag :s new-s)
        (cpu/store-flag :z new-z)))))

(defn hlt
  "Does nothing, and causes the program counter to remain in the same state"
  {:flags-affected [] :cycles 1 :bytes 0}
  [computer]
  computer)

(def opcodes
  [
    nop     lxi-b   stax-b  inx-b   inr-b   dcr-b   mvi-b   rlc
    nop     dad-b   ldax-b  dcx-b   inr-c   dcr-c   mvi-c   rrc
    nop     lxi-d   stax-d  inx-d   inr-d   dcr-d   mvi-d   ral
    nop     dad-d   ldax-d  dcx-d   inr-e   dcr-e   mvi-e   rar
    nop     lxi-h   shld    inx-h   inr-h   dcr-h   mvi-h   nop
    nop     dad-h   lhld    dcx-h   inr-l   dcr-l   mvi-l   cma
    nop     lxi-sp  sta     inx-sp  inr-m   dcr-m   mvi-m   stc
    nop     dad-sp  lda     dcx-sp  inr-a   dcr-a   mvi-a   cmc
    mov-b-b mov-b-c mov-b-d mov-b-e mov-b-h mov-b-l mov-b-m mov-b-a
    mov-c-b mov-c-c mov-c-d mov-c-e mov-c-h mov-c-l mov-c-m mov-c-a
    mov-d-b mov-d-c mov-d-d mov-d-e mov-d-h mov-d-l mov-d-m mov-d-a
    mov-e-b mov-e-c mov-e-d mov-e-e mov-e-h mov-e-l mov-e-m mov-e-a
    mov-h-b mov-h-c mov-h-d mov-h-e mov-h-h mov-h-l mov-h-m mov-h-a
    mov-l-b mov-l-c mov-l-d mov-l-e mov-l-h mov-l-l mov-l-m mov-l-a
    mov-m-b mov-m-c mov-m-d mov-m-e mov-m-h mov-m-l hlt     mov-m-a
    mov-a-b mov-a-c mov-a-d mov-a-e mov-a-h mov-a-l mov-a-m mov-a-a
    add-b   add-c   add-d   add-e   add-h   add-l   add-m   add-a
    adc-b   adc-c   adc-d   adc-e   adc-h   adc-l   adc-m   adc-a
    sub-b   sub-c   sub-d   sub-e   sub-h   sub-l   sub-m   sub-a
    sbb-b   sbb-c   sbb-d   sbb-e   sbb-h   sbb-l   sbb-m   sbb-a
    ana-b   ana-c   ana-d   ana-e   ana-h   ana-l   ana-m   ana-a
  ])

   ; 0xcb {:fn nop    :bytes 1 :cycles 1}
   ;
   ; 0xd9 {:fn nop    :bytes 1 :cycles 1}
   ;
   ; 0xdd {:fn nop    :bytes 1 :cycles 1}
   ;
   ; 0xed {:fn nop    :bytes 1 :cycles 1}
   ;
   ; 0xfd {:fn nop    :bytes 1 :cycles 1}
