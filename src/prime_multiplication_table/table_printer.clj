(ns prime-multiplication-table.table-printer
  (:require [clojure.spec.alpha :as s]))

(s/def ::pos-int (s/and int? pos?))
(s/def ::seq-of-pos-ints (s/coll-of (s/nilable ::pos-int)))
(s/def ::seq-of-seq-of-pos-ints (s/coll-of ::seq-of-pos-ints))

(defn count-digits
  [num]
  (count (str num)))

(s/fdef gen-padding-whitespace
        :args (s/and (s/cat :cell-size ::pos-int
                            :v (s/nilable ::pos-int))
                     #(>= (:cell-size %) (count-digits (:v %)))))

(defn gen-padding-whitespace
  "Determines how much whitespace a cell should have to keep cells properly aligned"
  [cell-size v]
  (let [padding (- cell-size (count-digits v))]
    (apply str (repeat padding " "))))

(s/fdef gen-row-str
        :args (s/cat :row ::seq-of-pos-ints
                     :cell-size ::pos-int))

(defn- gen-row-str
  [row cell-size]
  "Generates a string of cells in a row from a seq of ints"
  (reduce (fn [output v]
            (str output (gen-padding-whitespace cell-size v) (str v) "|")) "" row))

(s/fdef gen-below-row-str
        :args (s/cat :num-columns ::pos-int
                     :cell-size ::pos-int))

(defn- gen-below-row-str
  [num-columns cell-size]
  "Generates a string of dashes and vertical bars that go below row of cells"
  (reduce (fn [output v]
            (str output (apply str (repeat cell-size "-")) "|")) "" (range num-columns)))

(s/fdef gen-table-str
        :args (s/cat :nums (s/coll-of ::pos-int)
                     :table ::seq-of-seq-of-pos-ints))

(defn- gen-table-str
  "Generates a string representation of a multiplication table,
  given multiplication table and the ordered coll of numbers that produced the table"
  [nums table]
  (let [cell-size (count-digits (last (last table)))
        num-columns (inc (count nums))
        num-rows num-columns]
    (str
     (gen-row-str (cons nil nums) cell-size)
     "\n"
     (gen-below-row-str num-columns cell-size)
     "\n"
     (reduce (fn [o x]
               (let [num (get nums x)]
                 (str o
                      (gen-row-str (cons num (nth table x)) cell-size)
                      "\n"
                      (gen-below-row-str num-columns cell-size)
                      "\n"))) "" (range (dec num-rows))))))

(defn print-multiplication-table
  [nums table]
  "Prints a multiplication table, given multiplication table and the ordered coll
of numbers that produced the table"
  (println (gen-table-str nums table)))
