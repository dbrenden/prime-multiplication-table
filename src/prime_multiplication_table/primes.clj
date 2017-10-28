(ns prime-multiplication-table.primes
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.repl :as repl]))

(s/def ::pos-int
  (s/and int? pos?))
(s/def ::coll-of-pos-int
  (s/coll-of ::pos-int))

(s/fdef find-index-of-first-value-greater-than-x
        :args (s/cat :x ::pos-int
                     :nums ::coll-of-pos-int)
        :ret int?)

(defn find-index-of-first-value-greater-than-x
  [x nums]
  (loop [i 0]
    (if (> (nth nums i)
           x)
      i
      (recur (inc i)))))

(s/fdef take-primes-less-than-x
        :args (s/cat :x int?
                     ::primes ::coll-of-pos-int)
        :ret ::coll-of-pos-int)

(defn take-primes-less-than-x
  [x primes]
  (let [num-primes (find-index-of-first-value-greater-than-x x primes)]
    (take num-primes primes)))

(s/fdef x-is-next-largest-prime?
        :args (s/cat :x ::pos-int
                     :nums ::coll-of-pos-int)
        :ret boolean?)

(defn x-is-next-largest-prime?
  [x primes]
  (let [sqrt-x (java.lang.Math/sqrt ^Integer x)
        primes (take-primes-less-than-x sqrt-x primes)]
    (not-any? #(= (mod x %) 0) primes)))

(s/fdef generate-next-largest-prime
        :args (s/cat :primes ::coll-of-pos-int)
        :ret int?)

(defn generate-next-largest-prime
  "Given coll of primes, generates next largest prime"
  [primes]
  (if-not (seq primes)
    2
    (loop [i (inc (last primes))]
      (if (x-is-next-largest-prime? i primes)
        i
        (recur (inc i))))))

(s/fdef first-n-primes
        :args (s/cat :n ::pos-int)
        :ret ::coll-of-pos-int
        :fn #(= (count (:ret %)) (-> % :args :n)))

(defn first-n-primes
  "Returns first n primes"
  [n]
  (when (s/valid? ::pos-int n)
    (reduce (fn [coll _]
              (conj coll (generate-next-largest-prime coll))) [] (range n))))
