(ns aoc2018-1
  (:require [clojure.java.io :as io]))

;; Get input
(defn read-input
  "Input: a file path
   Output: line-by-line separated array"
  [path] (-> path 
             io/resource
             slurp
             clojure.string/split-lines))

(defn convert-into-integer
  "input: integer string array
   output: int array"
  [string-sequence]
  (map #(Integer/parseInt %) string-sequence))


(defn sum-all
 "sum all elements
  Input: [1 2 3 4]
  Output: 10"
  [int-sequence sum]
  (if (seq int-sequence) 
    (sum-all (rest int-sequence) (+ sum (first int-sequence)))
    sum)
)

;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.
;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력
(comment
;; part 1


(-> "aoc2018_1_1.input"
    read-input
    convert-into-integer
    (sum-all 0))
  )
;; ANS: 502

;; 파트 2
;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...
;; atom을 쓰면 될 거 같다는 걸 찾았으나 현재는 여기까지..

(defn sum-until-duplicate
  "Input: integer array
     Output: calculate sum of array until the same sum value found"
  [int-sequence visited-value sum]
  (if (not (contains? visited-value sum))
    (sum-until-duplicate (rest int-sequence) (conj visited-value sum) (+ (first int-sequence) sum))
    sum))

(comment
  
  (-> "aoc2018_1_1.input"
      read-input
      convert-into-integer
      cycle
      (sum-until-duplicate #{} 0))
  )

;; ANS: 71961

;; cycle reductions