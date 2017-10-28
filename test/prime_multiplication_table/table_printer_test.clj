(ns prime-multiplication-table.table-printer-test
  (:require [prime-multiplication-table.table-printer :as tp]
            [clojure.test :refer :all]
            [clojure.spec.test.alpha :as stest]))

(stest/instrument `tp/gen-padding-whitespace)

(deftest test-gen-padding-whitespace
  (doseq [[cell-size v result] [[4 12 "  "]
                                [5 12 "   "]
                                [3 123 ""]]]
    (testing (format "With cell-size %s and v %s, should generate %s blocks of whitespace" cell-size v (count result))
      (is (= (#'tp/gen-padding-whitespace cell-size v))))))

(stest/instrument `tp/gen-row-str)

(deftest test-gen-row-str
  (doseq [[row cell-size expected] [['(1 2 3) 3 "  1|  2|  3|"]
                                    ['(1 23 123) 4 "   1|  23| 123|"]
                                    ['(1 23 123 1234) 5 "    1|   23|  123| 1234|"]]]
    (testing (format "Testing gen-row-str with cell-size %s and a row of %s" cell-size row)
      (is (= (#'tp/gen-row-str row cell-size) expected)))))


(stest/instrument `tp/gen-below-row-str)

(deftest test-gen-below-row-str
  (doseq [[num-columns cell-size expected] [[3 3 "---|---|---|"]
                                            [4 3 "---|---|---|---|"]
                                            [10 5 "-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|"]]]
    (testing (format "Testing gen-below-row-str with cell-size %s and %s columns" cell-size num-columns)
      (is (= (#'tp/gen-below-row-str num-columns cell-size) expected)))))

(stest/instrument `tp/gen-table-str)

(deftest test-gen-table-str
  (testing (format "Testing printing a multiplication table produced by first 5 primes")
    (let [nums [2 3 5 7 11]
          table '((4 6 10 14 22) (6 9 15 21 33) (10 15 25 35 55) (14 21 35 49 77) (22 33 55 77 121))
          expected "   |  2|  3|  5|  7| 11|\n---|---|---|---|---|---|\n  2|  4|  6| 10| 14| 22|\n---|---|---|---|---|---|\n  3|  6|  9| 15| 21| 33|\n---|---|---|---|---|---|\n  5| 10| 15| 25| 35| 55|\n---|---|---|---|---|---|\n  7| 14| 21| 35| 49| 77|\n---|---|---|---|---|---|\n 11| 22| 33| 55| 77|121|\n---|---|---|---|---|---|\n"]
      (is (= (#'tp/gen-table-str nums table) expected)))))
