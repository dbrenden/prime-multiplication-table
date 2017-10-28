(ns prime-multiplication-table.core
  (:require [prime-multiplication-table.table :as table]
            [prime-multiplication-table.table-printer :as table-printer]
            [prime-multiplication-table.primes :as primes]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s])
  (:gen-class))

(s/def ::pos-int (s/and pos? int?))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [n (or (edn/read-string (first args)) 10)]
    (when (s/valid? ::pos-int n)
      (let [primes (primes/first-n-primes n)
            table (table/gen-prime-multiplication-table primes)]
        (table-printer/print-table primes table)))))
