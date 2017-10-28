(ns prime-multiplication-table.table-printer)

(defn count-digits
  [num]
  (count (str num)))

(defn gen-padding-whitespace
  [last-num-size cell]
  (let [padding (- last-num-size (count-digits cell))]
    (apply str (repeat padding " "))))

(defn generate-row-str
  [row last-num-size]
  (reduce (fn [o cell]
            (str o (gen-padding-whitespace last-num-size cell) (str cell) "|")) "" row))

(defn generate-below-row-str
  [num-columns last-num-size]
  (reduce (fn [o cell]
            (str o (apply str (repeat last-num-size "-")) "|")) "" (range num-columns)))

(defn gen-table-str
  [primes table]
  (let [last-num-size (count-digits (last (last table)))
        num-columns (inc (count primes))
        num-rows num-columns]
    (str
     (generate-row-str (cons nil primes) last-num-size)
     "\n"
     (generate-below-row-str num-columns last-num-size)
     "\n"
     (reduce (fn [o x]
               (let [prime (get primes x)]
                 (str o
                      (generate-row-str (cons prime (nth table x)) last-num-size)
                      "\n"
                      (generate-below-row-str num-columns last-num-size)
                      "\n"))) "" (range (dec num-rows))))))


(defn print-table
  [primes table]
  (println (gen-table-str primes table)))
