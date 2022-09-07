(ns aoc2020_1
  (:require [clojure.java.io :as io]))

(defn read-input
  "parse file input into events
   Input: file path
   Output: (\"1721\" \"979\" \"366\" \"299\" \"675\" \"1456\")"
  [path]
  (->> path io/resource slurp clojure.string/split-lines))

(defn pairwise-sums
  "Returns sums of any two nums
   Input: (\"1721\" \"979\" \"366\" \"299\" \"675\" \"1456\")
   Output: ()"
  [num-strings] 
  (flatten
   (map-indexed
    (fn [first-idx first-num]
      (filter
       #(not (nil? %))
       (map-indexed (fn [second-idx second-num]
                      (if (> first-idx second-idx)
                        {:sum (+ (Integer/parseInt first-num) (Integer/parseInt second-num))
                         :first-num (Integer/parseInt first-num)
                         :second-num (Integer/parseInt second-num)}
                        nil)) num-strings))) num-strings))
  
  ;;  for [i (range (count num-strings))
  ;;       j (range (count num-strings))
  ;;       :let [first-num (Integer/parseInt (nth num-strings i))
  ;;             second-num (Integer/parseInt (nth num-strings j))]
  ;;       :when (> i j)]
  ;;   {:sum (+ first-num second-num) 
  ;;    :first-num first-num 
  ;;    :second-num second-num})
  )
;map-indexed

(defn tripple-sums
  "Returns sums of any tripple nums
   Input: (\"1721\" \"979\" \"366\" \"299\" \"675\" \"1456\")
   Output: ()"
  [num-strings]
  (for [i (range (count num-strings))
        j (range (count num-strings))
        k (range (count num-strings))
        :let [first-num (Integer/parseInt (nth num-strings i))
              second-num (Integer/parseInt (nth num-strings j))
              third-num (Integer/parseInt (nth num-strings k))]
        :when (and (> i j k))]
    {:sum (+ first-num second-num third-num) 
     :first-num first-num 
     :second-num second-num 
     :third-num third-num}))

(defn multiplication
  "Return the multiplication from the given input
   Input: {:sum 2020, :first-num 299, :second-num 1721}
   Output: 514579"
  [input]
  (let [first-num (:first-num (first input))
        second-num (:second-num (first input))
        third-num (if-not (nil? (:third-num (first input)))
                    (:third-num (first input))
                    1)]
   (* first-num second-num third-num))
  )

; part-one
(comment
  (->> '("1721" "979" "366" "299" "675" "1456")
       pairwise-sums
       (drop-while #(not= 2020 (:sum %)))
       (take 1)
       multiplication)
  (->> "aoc2020_1.input"
       read-input
       pairwise-sums
       (drop-while #(not= 2020 (:sum %)))
       (take 1)
       multiplication))

; part-two
(comment
  (->> '("1721" "979" "366" "299" "675" "1456")
       tripple-sums
       (drop-while #(not= 2020 (:sum %)))
       (take 1)
       multiplication)
  (->> "aoc2020_1.input"
       read-input
       tripple-sums
       (drop-while #(not= 2020 (:sum %)))
       (take 1)
       multiplication))

;(< num1 num2 num3)
; combination 