(ns prime-multiplication-table.primes
  (:require [clojure.spec.alpha :as s]))

(s/def ::pos-int
  (s/and int? pos?))
(s/def ::vec-of-pos-int
  (s/coll-of ::pos-int
             :kind vector?))

(s/fdef find-index-of-first-value-greater-than-x
        :args (s/cat :nums ::vec-of-pos-int
                     :x int?)
        :ret int?)

(defn find-index-of-first-value-greater-than-x
  "Given a vector of nums, returns index of first value greater than x"
  [nums x]
  (loop [i 0]
    (if (> (nth nums i)
           x)
      i
      (recur (inc i)))))

(s/fdef take-primes-less-than-x
        :args (s/cat :primes ::vec-of-pos-int
                     :x int?)
        :ret ::vec-of-pos-int)

(defn take-primes-less-than-x
  "Given an ordered vector of primes, takes all primes under a certain value"
  [primes x]
  (let [num-primes (find-index-of-first-value-greater-than-x primes x)]
    (take num-primes primes)))

(s/fdef x-is-next-largest-prime?
        :args (s/and (s/cat :nums ::vec-of-pos-int
                            :x int?)
                     #(> (:x %) (last (:nums %))))
        :ret boolean?)

(defn x-is-next-largest-prime?
  "Given an ordered vector of primes and a number,
  checks if number is next largest prime after the last prime in list"
  [primes x]
  (let [sqrt-x (java.lang.Math/sqrt ^Integer x)
        factors (take-primes-less-than-x primes sqrt-x)]
    (not-any? #(= (mod x %) 0) factors)))

(s/fdef generate-next-largest-prime
        :args (s/cat :primes ::vec-of-pos-int)
        :ret int?)

(defn gen-next-largest-prime
  "Given an ordered vector of primes, generates next largest prime"
  [primes]
  (if-not (seq primes)
    2
    (loop [i (inc (peek primes))]
      (if (x-is-next-largest-prime? primes i)
        i
        (recur (inc i))))))

(s/fdef first-n-primes
        :args (s/cat :n ::pos-int)
        :ret ::vec-of-pos-int
        :fn #(= (count (:ret %)) (-> % :args :n)))

(defn first-n-primes
  "Returns vector of first n primes"
  [n]
  (reduce (fn [coll _]
            (conj coll (gen-next-largest-prime coll))) [] (range n)))
