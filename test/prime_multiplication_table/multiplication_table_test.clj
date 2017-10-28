(ns prime-multiplication-table.multiplication-table-test
  (:require [prime-multiplication-table.multiplication-table :as mt]
            [clojure.test :refer :all]
            [clojure.spec.test.alpha :as stest]))

(stest/instrument `mt/gen-multiplication-row)

(deftest test-gen-multiplication-row
  (doseq [[nums x result] [[[2 3 5 7 11] 2 '(10 15 25 35 55)]
                           [[2 3 5 7 11] 3 '(14 21 35 49 77)]
                           [[2 3 5 7 11 13 17] 6 '(34 51 85 119 187 221 289)]
                           [[2 3 5 7 11 13 17] 5 '(26 39 65 91 143 169 221)]]]
    (testing (format "generating row number %s for coll of numbers %s" x nums)
      (is (= result (#'mt/gen-multiplication-row nums x))))))

(stest/instrument `mt/gen-multiplication-table)

(deftest test-gen-multiplication-table
  (testing (format "generating table for first 5 prime numbers")
    (is (= '((4 6 10 14 22)
             (6 9 15 21 33)
             (10 15 25 35 55)
             (14 21 35 49 77)
             (22 33 55 77 121))
           (mt/gen-multiplication-table [2 3 5 7 11]))))
  (testing (format "generating table for first 6 prime numbers")
    (is (= '((4 6 10 14 22 26)
             (6 9 15 21 33 39)
             (10 15 25 35 55 65)
             (14 21 35 49 77 91)
             (22 33 55 77 121 143)
             (26 39 65 91 143 169))
           (mt/gen-multiplication-table [2 3 5 7 11 13])))))
