(ns clj.04
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(def xmas-map (slurp "../inputs/04.in"))

;; Part One
(let [plot (map #(str/split % #"") (str/split-lines xmas-map))
      directions (remove #(= % '(0 0)) (combo/selections [-1 0 1] 2))]
  (defn v? [x y] (nth (nth plot x nil) y nil))
  (defn vn? [x y dir fact] (v? (+ x (* (first dir) fact))
                               (+ y (* (second dir) fact))))
  (defn calc-words [x y]
    (count (filter true?
                   (map (fn [dir] (let [M (vn? x y dir 1)
                                        A (vn? x y dir 2)
                                        S (vn? x y dir 3)]
                                    (= (str M A S) "MAS"))) directions))))
  (reduce +
          (for [[x row] (map-indexed vector plot)
                [y elem] (map-indexed vector row)]
            (if (= (v? x y) "X") (calc-words x y) 0))))

;; Part Two
(let [plot (map #(str/split % #"") (str/split-lines xmas-map))]
  (defn v? [x y] (nth (nth plot x nil) y nil))
  (defn is-mas? [x y]
    (let [c1 (v? (- x 1) (- y 1))
          c2 (v? (+ x 1) (+ y 1))
          c3 (v? (- x 1) (+ y 1))
          c4 (v? (+ x 1) (- y 1))]
      (defn is-ms? [s] (and (.contains s "M")
                            (.contains s "S")))
      (and (is-ms? (format "%s%s" c1 c2))
           (is-ms? (format "%s%s" c3 c4)))))
  (count (filter true?
                 (for [[x row] (map-indexed vector plot)
                       [y elem] (map-indexed vector row)]
                   (if (= (v? x y) "A") (is-mas? x y) false)))))


