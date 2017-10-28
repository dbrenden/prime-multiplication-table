(ns prime-multiplication-table.table-printer)

(defn count-digits
  [num]
  (count (str num)))

(defn gen-padding-whitespace
  [cell-size v]
  (let [padding (- cell-size (count-digits v))]
    (apply str (repeat padding " "))))

(defn gen-row-str
  [row cell-size]
  (reduce (fn [output v]
            (str output (gen-padding-whitespace cell-size v) (str v) "|")) "" row))

(defn gen-below-row-str
  [num-columns cell-size]
  (reduce (fn [output v]
            (str output (apply str (repeat cell-size "-")) "|")) "" (range num-columns)))

(defn gen-table-str
  [primes table]
  (let [cell-size (count-digits (last (last table)))
        num-columns (inc (count primes))
        num-rows num-columns]
    (str
     (gen-row-str (cons nil primes) cell-size)
     "\n"
     (gen-below-row-str num-columns cell-size)
     "\n"
     (reduce (fn [o x]
               (let [prime (get primes x)]
                 (str o
                      (gen-row-str (cons prime (nth table x)) cell-size)
                      "\n"
                      (gen-below-row-str num-columns cell-size)
                      "\n"))) "" (range (dec num-rows))))))


(defn print-table
  [primes table]
  (println (gen-table-str primes table)))
