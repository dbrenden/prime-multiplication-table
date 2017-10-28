(ns prime-multiplication-table.multiplication-table
  (:require [clojure.spec.alpha :as s]))


(s/def ::not-neg-int (s/and int? #(not (neg? %))))
(s/def ::coll-of-pos-int (s/coll-of (s/and int? pos?)))

(def get-length
  (memoize (fn [coll]
             (count coll))))

(s/fdef gen-multiplication-row
        :args (s/and (s/cat :nums ::coll-of-pos-int
                            :x ::not-neg-int)
                     #(< (:x %) (count (:nums %)))))

(defn gen-multiplication-row
  "Given an ordered coll of numbers, generates an ordered seq containing
  the products of the xth number multiplied by every number in the seq"
  [nums x]
  (let [length (get-length nums)]
    (map (fn [y]
           (* (nth nums x) (nth nums y))) (range length))))

(s/fdef gen-multiplication-table
        :args (s/cat :nums ::coll-of-pos-int))

(defn gen-multiplication-table
  [nums]
  "Given an order coll of numbers, generates a seq of seqs where the xth seq
  is an ordered seq containing the products of the xth number multiplied by
  every number in the seq"
  (let [length (get-length nums)]

    (map (fn [x]
           (gen-multiplication-row nums x)) (range length))))
