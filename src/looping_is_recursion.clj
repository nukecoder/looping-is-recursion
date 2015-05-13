(ns looping-is-recursion)

(defn power [base exp]
  (loop [b base
         e exp
         acc 1]
    (if (zero? e)
      acc
      (recur b (dec e) (* acc b)))))

(defn last-element [a-seq]
  (loop [s a-seq
         acc nil]
    (if (empty? s)
      acc
      (recur (rest s) (first s)))))

(defn seq= [seq1 seq2]
  (loop [a seq1
         b seq2]
    (cond (and (empty? a)
               (empty? b))
          true
          (or (empty? a) (empty? b)) false
          :else (if (= (first a) (first b))
                  (recur (rest a) (rest b))
                  false))))

(defn find-first-index [pred a-seq]
  (loop [p pred
         s a-seq
         inx 0]
    (cond (empty? s) nil
          (p (first s)) inx
          :else (recur p (rest s) (inc inx)))))

(defn avg [a-seq]
  (loop [s a-seq
         c nil
         acc 0]
    (cond (and (empty? s) (nil? c)) 0
          (empty? s) (/ acc c) 
          :else (recur (rest s) (inc (or c 0)) (+ acc (first s))))))


(defn parity [a-seq]
  (let [toggle (fn [a-set mem]
                 (if (contains? a-set mem)
                   (disj a-set mem)
                   (conj a-set mem)))]
    (loop [s a-seq
           res #{}]
      (if (empty? s)
        res
        (recur (rest s) (toggle res (first s)))))))

(defn fast-fibo [n]
  (loop [n n
         f-n 1
         f-n-1 0]
    (if (zero? n) f-n-1
        (recur (dec n) (+ f-n f-n-1) f-n))))

(defn cut-at-repetition [a-seq]
  (loop [s a-seq
         seen #{}
         res []]
    (if (or (contains? seen (first s)) (empty? s))
      res
      (recur (rest s) (conj seen (first s)) (conj res (first s))))))

