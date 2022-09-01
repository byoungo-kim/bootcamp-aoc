(ns aoc2018_3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn read-input
  "Input: a file path
   Output: line-by-line separated array"
  [path] (-> path
             io/resource
             slurp
             clojure.string/split-lines))
;; 파트 1
;; 다음과 같은 입력이 주어짐.

;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2

;; # 뒤에 오는 숫자는 ID, @ 뒤에 오는 숫자 쌍 (a, b)는 시작 좌표, : 뒤에 오는 (c x d)는 격자를 나타냄.
;; 입력의 정보대로 격자 공간을 채우면 아래와 같이 됨.

;;      ........
;;      ...2222.
;;      ...2222.
;;      .11XX22.
;;      .11XX22.
;;      .111133.
;;      .111133.
;;      ........

;; 여기서 XX는 ID 1, 2, 3의 영역이 두번 이상 겹치는 지역.
;; 겹치는 지역의 갯수를 출력하시오. (위의 예시에서는 4)
(defn parse-string
  "parse string into [id position-x position-y width height]
   Input: #1 @ 1,3: 4x4
   Output: [1 1 3 4 4]"
[line-string]
(->> line-string
    (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")
     rest ;; delete pattern string
     (map #(Integer/parseInt %))
    ))
(defn parse-strings
  "parse strings by applying parse-string
   "
  [strings]
  (for [x strings
        :let [parsed-input (parse-string x)]]
    parsed-input))

(defn create-set-for-rectangle
  "Generate a list of flattened indexes (y*width + x) composing a rectangle
   Input: [x 1 3 4 4] 8 x is user id and i
   Output: #{26 27 28 29 34 35 36 37 42 43 44 45 50 51 52 53}"
  [rectangle-data width]
  (let [rectangle-point-set #{}]
    (into rectangle-point-set 
          (
           for [x (range (nth rectangle-data 3)) 
                y (range (nth rectangle-data 4)) 
                :let [offset-x (second rectangle-data)
                      offset-y (nth rectangle-data 2)
                      point-index (+ (* (+ y offset-y) width) (+ x offset-x))]] 
           point-index))))

(defn get-width
  "Get the max x-width from the array of rectangle info
   Input: [[1 1 3 4 4] [2 3 1 4 4]]
   Output: 8"
  [rectangle-data]
  (apply max (for [x rectangle-data
                   :let [
                         bottom-right-x (inc (+ (second x) (nth x 3)))]]
    bottom-right-x)))

(defn create-sets-from-rectangle-data
  "Create a number of sets corresponding to a given rectangle data
   Input: [[1 1 3 4 4] [2 3 1 4 4]]
   Output:(#{27 50 33 36 41 43 44 28 51 25 34 35 26 52 42 49} #{20 27 21 13 22 36 29 28 12 35 19 11 14 38 30 37})"
  [rectangle-info-list]
  (let [width (get-width rectangle-info-list)]
    (
     for [x rectangle-info-list
          :let [rectangle-point-set (create-set-for-rectangle x width)]]
     rectangle-point-set
    ))
  )
(defn find-intersections-of-any-two-rectangles
  "find an intersection of any two rectangle
   Input: [#{27 50 33 36 41 43 44 28 51 25 34 35 26 52 42 49} #{20 27 21 13 22 36 29 28 12 35 19 11 14 38 30 37}]
   Output: #{27 36 28 35}"
  [rectangle-point-set-list]
  (let [num-rectangles (count rectangle-point-set-list)]
    (for [i (range num-rectangles)
          j (range num-rectangles)
          :let [intersection-point-set (set/intersection 
                                        (nth rectangle-point-set-list i) 
                                        (nth rectangle-point-set-list j))]
          :when (> i j)] 
      intersection-point-set)))

(comment 
  (parse-string "#1 @ 1,3: 4x4")
  (parse-strings ["#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4"])
  (get-width [[1 1 3 4 4] [2 3 1 4 4]])
  (create-set-for-rectangle [1 1 3 4 4] 8)
  [(create-set-for-rectangle [1 1 3 4 4] 8) (create-set-for-rectangle [2 3 1 4 4] 8)]
  (create-sets-from-rectangle-data [[1 1 3 4 4] [2 3 1 4 4]])
  (set/intersection (create-set-for-rectangle [1 1 3 4 4] 8) (create-set-for-rectangle [2 3 1 4 4] 8))
  (count (set/intersection (create-set-for-rectangle [1 3 4 4] 7) (create-set-for-rectangle [3 1 4 4] 7)))
  (count (apply set/union (create-sets-from-rectangle-data [[1 3 4 4] [3 1 4 4]])))
  (find-intersections-of-any-two-rectangles [#{27 50 33 36 41 43 44 28 51 25 34 35 26 52 42 49} #{20 27 21 13 22 36 29 28 12 35 19 11 14 38 30 37}])
  (->> [#{24 15 21 31 22 36 29 28 17 23 35 14 16 38 30 37} #{24 4 25 17 3 12 2 23 19 11 9 5 26 16 10 18}]
       (apply set/intersection)
       count)
  (->> "aoc2018_3.input" 
       ;; Parse
       read-input 
       parse-strings ;; parse input line by line by using regular expression
       create-sets-from-rectangle-data ;; parse data into set for using set operation
       ;; Process
       find-intersections-of-any-two-rectangles
       ;; Aggregate
       (apply set/union)
       ;; Print
       count)
  ;; ANS: 116489
  )



;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)
(defn create-sets-with-id-from-rectangle-data
  "Create a number of sets corresponding to a given rectangle data
   Input: [[1 1 3 4 4] [2 3 1 4 4] [3 5 5 2 2]]
   Output: ([1 #{27 50 33 36 41 43 44 28 51 25 34 35 26 52 42 49}]
            [2 #{20 27 21 13 22 36 29 28 12 35 19 11 14 38 30 37}]
            [3 #{46 54 45 53}])"
  [rectangle-info-list]
  (let [width (get-width rectangle-info-list)]
    (for [x rectangle-info-list
          :let [rectangle-point-set-with-id [(first x) (create-set-for-rectangle x width)]]]
      rectangle-point-set-with-id)))
(defn find-union-of-intersctions-for-given-id
  "Find an union of all intersections relevant to the rectangle of given id
   Input: ([1 #{27 50 33 36 41 43 44 28 51 25 34 35 26 52 42 49}]
           [2 #{20 27 21 13 22 36 29 28 12 35 19 11 14 38 30 37}]
           [3 #{46 54 45 53}])
          3
          0
          [1 #{27 50 33 36 41 43 44 28 51 25 34 35 26 52 42 49}]
   Output: #{20 27 21 13 22 36 29 28 12 35 19 11 14 38 30 37}"
  [rectangle-point-set-list 
   num-rectangles
   target-index
   current-rectangle-info]
  (let [current-rectangle (second current-rectangle-info)]
   (apply set/union
          (for [j (range num-rectangles) 
                :let [jth-rectangle (nth rectangle-point-set-list j) 
                      intersection-point-set (set/intersection 
                                              current-rectangle 
                                              (second jth-rectangle))]
                :when (not= target-index j)] 
            intersection-point-set))))

(defn find-id-of-no-intersection
  ""
  [rectangle-point-set-list]
  (
   let [num-rectangles (count rectangle-point-set-list)] 
   (take 1 (for [i (range num-rectangles) 
                 :let [current-rectangle-info (nth rectangle-point-set-list i) 
                       rectangle-id (first current-rectangle-info) 
                       union-of-intersections (find-union-of-intersctions-for-given-id  
                                               rectangle-point-set-list 
                                               num-rectangles 
                                               i 
                                               current-rectangle-info)] 
                 :when (empty? union-of-intersections)] 
             rectangle-id)))
  )

(comment
  (->> [[1 1 3 4 4] [2 3 1 4 4] [3 5 5 2 2]]
       create-sets-with-id-from-rectangle-data)
  (let [rectangle-point-set-list (->> [[1 1 3 4 4] [2 3 1 4 4] [3 5 5 2 2]]
                                      create-sets-with-id-from-rectangle-data)
        num-rectangles (count rectangle-point-set-list)]
    (find-union-of-intersctions-for-given-id rectangle-point-set-list num-rectangles 0 (second rectangle-point-set-list)))
  (->> [[1 1 3 4 4] [2 3 1 4 4] [3 5 5 2 2]]
       create-sets-with-id-from-rectangle-data
       find-id-of-no-intersection)
  (let [rectangle-point-set-list (->> "aoc2018_3.input" 
                                      read-input 
                                      parse-strings 
                                      create-sets-with-id-from-rectangle-data)
        num-rectangles (count rectangle-point-set-list)
        ]
    (
     find-union-of-intersctions-for-given-id rectangle-point-set-list num-rectangles 0 (second rectangle-point-set-list)
    )
    )
  (->> "aoc2018_3.input" 
       ;; Parse
       read-input 
       parse-strings 
       ;; Process
       create-sets-with-id-from-rectangle-data
       ;; Aggregate
       find-id-of-no-intersection
       prn)
  ;; ANS 1260
  ;; too slow, how to boost up?
  )