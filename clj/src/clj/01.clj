(require '[clojure.string :as str])

(defn get-lines [] (str/split-lines (slurp "../inputs/01.in")))

(defn input-pairs []
    (let [pairs (map (fn [x] (str/split x #"   " 2)) (get-lines))
          l-pair (map (fn [n] (Integer/parseInt (first n))) pairs)
          r-pair (map (fn [n] (Integer/parseInt (second n))) pairs)]
      [l-pair r-pair]))

;; Part one
(defn distance [pair]
  (Math/abs (- (first pair) (second pair))))

(defn part-one [pairs]
  (let [l-sorted (sort (first pairs))
        r-sorted (sort (second pairs))
        lr (map vector l-sorted r-sorted)
        distances (map distance lr)
        sum (reduce + distances)]
    sum))

(comment (part-one (input-pairs)))

;; Part Two
(defn similarity [l r-list]
  (* l (count (filter #(= % l) r-list))))


(defn part-two [pairs]
  (let [l-pair (first pairs)
        r-pair (second pairs)
        similarities (map (fn [l] (similarity l r-pair)) l-pair)
        sum (reduce + similarities)]
    sum))

(comment (part-two (input-pairs)))
