(ns prime-multiplication-table.table)

(def get-length
  (memoize (fn [coll]
             (count coll))))

(defn gen-multiplication-row
  [primes x]
  (let [length (get-length primes)]
    (map (fn [y]
           (* (nth primes x) (nth primes y))) (range length))))

(defn gen-prime-multiplication-table
  [primes]
  (let [length (get-length primes)
        multiplication-table (map (fn [x]
                                    (gen-multiplication-row primes x)) (range length))]

    multiplication-table))
