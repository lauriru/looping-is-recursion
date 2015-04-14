(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [base exp acc]
                 (if (zero? exp) acc
                     (recur base (dec exp) (* base acc))))]
    (helper base exp 1)))

(defn last-element [a-seq]
  (let [helper (fn [a-seq acc]
                 (if (empty? a-seq) acc
                 (recur (rest a-seq) (first a-seq))))]
    (helper a-seq nil)))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    :else
    (if (= (first seq1) (first seq2))
      (recur (rest seq1) (rest seq2))
      false)))

(defn find-first-index [pred a-seq]
  (let [helper (fn [pred a-seq indx]
                 (cond
                   (empty? a-seq) nil
                   (pred (first a-seq)) indx
                   :else (recur pred (rest a-seq) (inc indx))))]
    (helper pred a-seq 0)))

(defn avg [a-seq]
  (let [helper (fn [a-seq sum]
                 (if (empty? a-seq) sum
                     (recur (rest a-seq) (+ sum (first a-seq)))))]
        (/ (helper a-seq 0) (count a-seq))))

(defn filter-if-odd [a-map]
  (loop [[keys vals] a-map  acc []]
    (if (empty? a-map) nil)))

(defn parity [a-seq]
  (let [keys-and-vals (seq (frequencies a-seq))]
    (loop [[seq & seqs] keys-and-vals acc []]
      (if (empty? seq) acc 
          (let [[key val] seq]
            (if (odd? val) (recur seqs (conj acc key))
                (recur seqs acc)))))))

(defn fast-fibo [n]
  (loop [fn1 0 fn2 1 l n]
    (if (zero? l) fn1
        (recur fn2 (+ fn1 fn2) (dec l)))))

(defn cut-at-repetition [a-seq]
  (loop [done [] left a-seq a-set #{}]
    (if (empty? left) done
        (let [f (first left)
              r (rest left)]
          (if (contains? a-set f) done
              (recur (conj done f) r (conj a-set f)))))))

