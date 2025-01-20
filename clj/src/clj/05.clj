(ns clj.05
  (:require [clojure.string :as str])
  (:require [clojure.core.reducers :as r]))

(def input (slurp "../inputs/05.in"))
(def input
  "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(defn middle-mans [lsts]
  (defn middle [lst] (nth lst (quot (count lst) 2)))
  (->> lsts
       (map middle)
       (map #(Integer/parseInt %))
       (reduce +)))

(defn filter-valid [rules updates]
  (defn filter-out [valid-updates rule]
    (defn is-valid? [update]
      (defn index-of [elem]
        (first (keep-indexed (fn [idx x] (when (= x elem) idx)) update)))
      (let [[a b] (str/split rule #"\|")
            av (index-of a)
            bv (index-of b)]
        (if (and (some? av) (some? bv)) (< av bv) true)))
    (filter is-valid? valid-updates))
  (r/reduce filter-out updates rules))

;; Part One
(let [[rules u] (map str/split-lines (str/split input #"\n\n"))
      updates (map #(str/split % #",")  u)
      valid (filter-valid rules updates)]
  (middle-mans valid))

;; Part Two
(defn fix-invalid [rules updates]
  (defn fix [invalid-updates rule]
    (defn re-order [update]
      (defn index-of [elem]
        (first (keep-indexed (fn [idx x] (when (= x elem) idx)) update)))
      (defn swap-elems [lst idx1 idx2]
        (println "swap" lst idx1 idx2 rule)
        (let [vec-lst (vec lst)
              val1 (nth vec-lst idx1)
              val2 (nth vec-lst idx2)]
          (-> vec-lst
              (assoc idx1 val2)
              (assoc idx2 val1)
              seq)))

      (let [[a b] (str/split rule #"\|")
            av (index-of a)
            bv (index-of b)]
        (if (and (some? av) (some? bv))
          (if (> av bv) (swap-elems update av bv) update)
          update)))
    (map re-order invalid-updates))
  (r/reduce fix updates rules))

(let [[rules u] (map str/split-lines (str/split input #"\n\n"))
      sorted-rules (sort rules)
      updates (map #(str/split % #",")  u)
      valid (filter-valid rules updates)
      fixed (fix-invalid rules (filter (fn [x] (not (some #(= x %) valid))) updates))]
  (println fixed)
  (middle-mans fixed))
