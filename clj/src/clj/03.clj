(defn memory [] (slurp "../inputs/03.in"))

;; Part One
(->> (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" (memory))
     (map rest)
     (map #(map (fn [n] (Integer/parseInt n)) %))
     (map #(apply * %))
     (reduce +))

;; Part Two
(defn multi [in factor]
    (* (Integer/parseInt (nth in 2))
       (Integer/parseInt (nth in 3))
       factor))

(let [m (re-seq #"(mul\((\d{1,3}),(\d{1,3})\))|(do\(\))|(don't\(\))" (memory))]
  (def enabled (atom 1))
  (def sum (atom 0))
  (reduce + (map (fn [x]
         (cond
           (= (first x) "do()") (reset! enabled 1)
           (= (first x) "don't()") (reset! enabled 0)
           :else (swap! sum + (multi x @enabled)))) m))
  @sum)
