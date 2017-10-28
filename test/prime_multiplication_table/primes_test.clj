(ns prime-multiplication-table.primes-test
  (:require [prime-multiplication-table.primes :as primes]
            [clojure.test :refer :all]
            [clojure.spec.test.alpha :as stest]
            [clojure.spec.alpha :as s]))

(stest/instrument `primes/find-index-of-first-value-greater-than-x)

(deftest test-find-index-of-first-value-greater-than-x
  (doseq [[nums x expected] [[[2 3 5 7 11 13] (java.lang.Math/sqrt 14) 5]
                             [[2 3 5 7] (java.lang.Math/sqrt 9) 5]
                             [[2 3 5 7 11 13 17 19 23 29 31] (java.lang.Math/sqrt 36) 7]]]
    (let [result (#'primes/find-index-of-first-value-greater-than-x nums x)]
      (is (> (nth nums result) x)))))

(stest/instrument `primes/x-is-next-largest-prime?)

(deftest test-x-is-next-largest-prime?
  (doseq [[primes x] [[[2 3 5 7 11 13] 17]
                      [[2 3 5 7 11 13 17 19] 23]
                      [[2 3 5 7 11 13 17 19 23 29 31 37] 41]]]
    (testing (format "case where %s is next largest prime in coll of primes %s" x primes)
      (is (#'primes/x-is-next-largest-prime? primes x))))
  (doseq [[primes x] [[[2 3 5 7 11 13] 16]
                      [[2 3 5 7 11 13] 14]
                      [[2 3 5 7 11 13 17 19] 20]
                      [[2 3 5 7 11 13 17 19] 21]
                      [[2 3 5 7 11 13 17 19 23 29 31 37] 38]
                      [[2 3 5 7 11 13 17 19 23 29 31 37] 39]]]
    (testing (format "case where %s is not next largest prime in coll of primes %s" x primes)
      (is (not (#'primes/x-is-next-largest-prime? primes x))))))

(stest/instrument `primes/gen-next-largest-prime)

(deftest test-gen-next-largest-prime
  (doseq [[primes result] [[[2 3 5 7 11 13] 17]
                           [[2 3 5 7 11 13 17 19] 23]
                           [[2 3 5 7 11 13 17 19 23 29 31 37] 41]]]
    (testing (format "case where %s is next largest prime in coll of primes %s" result primes)
      (is (= result (#'primes/gen-next-largest-prime primes))))))

(stest/instrument `primes/first-n-primes)

(deftest test-first-n-primes
  (doseq [[n result] [[6 [2 3 5 7 11 13]]
                      [8 [2 3 5 7 11 13 17 19]]
                      [12 [2 3 5 7 11 13 17 19 23 29 31 37]]]]
    (testing (format "case where we are producing first %s primes" n)
      (is (= result (primes/first-n-primes n))))))
