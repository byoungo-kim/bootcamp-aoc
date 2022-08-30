(ns aoc2018-1
  (:require [clojure.java.io :as io]))

;; Get input
(def input (-> "aoc2018_1_1.input"
               (io/resource)
               (slurp)
               (clojure.string/split-lines)))

;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.
;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력

;; part 1
(reduce + (for [digit input] (Integer/parseInt digit)))

;; ANS: 502

;; 파트 2
;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...
;; atom을 쓰면 될 거 같다는 걸 찾았으나 현재는 여기까지..
(def sumList #{})

(for [number 
      (for [digit input] (Integer/parseInt digit))]
  (let [sum (+ number (if (empty sumList) 0 (last sumList)))]
    (if (contains? sumList sum)
      (prn sum)
      (conj sumList sum))))