(ns prime-multiplication-table.primes
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.repl :as repl]))

(s/def ::pos-int
  (s/and int? pos?))
(s/def ::coll-of-pos-int
  (s/coll-of ::pos-int))

(s/fdef find-index-of-first-value-greater-than-x
        :args (s/cat :nums ::coll-of-pos-int
                     :x ::pos-int)
        :ret int?)

(defn find-index-of-first-value-greater-than-x
  "Given an coll of nums, returns index of first value greater than x"
  [nums x]
  (loop [i 0]
    (if (> (nth nums i)
           x)
      i
      (recur (inc i)))))

(s/fdef take-primes-less-than-x
        :args (s/cat :primes ::coll-of-pos-int
                     :x int?)
        :ret ::coll-of-pos-int)

(defn take-primes-less-than-x
  "Given an ordered coll of primes, takes all primes under a certain value"
  [primes x]
  (let [num-primes (find-index-of-first-value-greater-than-x x primes)]
    (take num-primes primes)))

(s/fdef x-is-next-largest-prime?
        :args (s/cat :nums ::coll-of-pos-int
                     :x ::pos-int)
        :ret boolean?)

(defn x-is-next-largest-prime?
  "Given an ordered coll of primes and a number,
  checks if number is next largest prime after the last prime in list"
  [primes x]
  (let [sqrt-x (java.lang.Math/sqrt ^Integer x)
        factors (take-primes-less-than-x sqrt-x primes)]
    (not-any? #(= (mod x %) 0) factors)))

(s/fdef generate-next-largest-prime
        :args (s/cat :primes ::coll-of-pos-int)
        :ret int?)

(defn gen-next-largest-prime
  "Given an ordered coll of primes, generates next largest prime"
  [primes]
  (if-not (seq primes)
    2
    (loop [i (inc (last primes))]
      (if (x-is-next-largest-prime? primes i)
        i
        (recur (inc i))))))

(s/fdef first-n-primes
        :args (s/cat :n ::pos-int)
        :ret ::coll-of-pos-int
        :fn #(= (count (:ret %)) (-> % :args :n)))

(defn first-n-primes
  "Returns vector of first n primes"
  [n]
  (when (s/valid? ::pos-int n)
    (reduce (fn [coll _]
              (conj coll (gen-next-largest-prime coll))) [] (range n))))
