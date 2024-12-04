(require '[clojure.string :as str])

(defn get-lines [] (str/split-lines (slurp "../inputs/02.in")))

(defn reports []
  (map (fn [x] (map #(Integer/parseInt %) (str/split x #" "))) (get-lines)))

(defn safe? [report]
  (let [r (map #(apply - %) (partition 2 1 report))]
    (or (every? (fn [x] (and (> x 0) (<= x 3))) r)
        (every? (fn [x] (and (>= x -3) (< x 0))) r))))

(defn part-one [] (count (filter safe? (reports))))
(comment (part-one))

;; Part Two
(defn variants [report]
  (map (fn [n] (concat (take n report) (drop (inc n) report)))
       (range (count report))))

(defn safer? [report]
    (or (safe? report) (some safe? (variants report))))

(defn part-two [] (count (filter safer? (reports))))
(comment (part-two))
