(ns prime-multiplication-table.core
  (:require [prime-multiplication-table.multiplication-table :as mt]
            [prime-multiplication-table.table-printer :as table-printer]
            [prime-multiplication-table.primes :as primes]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s])
  (:gen-class))

(s/def ::pos-int (s/and int? pos?))

(def default-input 10)

(defn parse-input
  "Parses first command line arg, defaults to default-input"
  [args]
  (let [input (or (edn/read-string (first args)) default-input)]
    (if (s/valid? ::pos-int input)
      input
      (println (format "Error: Input must be a postive integer" input)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (when-let [n (parse-input args)]
    (let [primes (primes/first-n-primes n)
          multiplication-table (mt/gen-multiplication-table primes)]
      (table-printer/print-multiplication-table primes multiplication-table))))
