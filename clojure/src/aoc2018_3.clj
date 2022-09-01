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
   Output: {:id 1 :start-x 1 :start-y 3 :width 4 :length 4}"
[line-string]
(let [[id start-x start-y width length]
      (->> line-string
           (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")
           rest
           (map #(Integer/parseInt %))
           )
      ]{:id id :start-x start-x :start-y start-y :width width :length length}))
(defn parse-strings
  "parse strings by applying parse-string
   Input: (\"#1 @ 1,3: 4x4\" \"#2 @ 3,1: 4x4\")
   Output: ({:id 1 :start-x 1 :start-y 3 :width 4 :length 4}
            {:id 2 :start-x 3 :start-y 1 :width 4 :length 4})"
  [strings]
  (map parse-string strings))

(defn create-fabric-points
  "Generate a list of flattened indexes (y*width + x) composing a rectangle
   Input: {:id 1 :start-x 1 :start-y 3 :width 4 :length 4} 8
   Output: #{26 27 28 29 34 35 36 37 42 43 44 45 50 51 52 53}"
  [fabric suit-width] 
    (set 
     (for [x (range (fabric :width)) 
           y (range (fabric :length)) 
           :let [offset-x (fabric :start-x) 
                 offset-y (fabric :start-y) 
                 point-index (+ (* (+ y offset-y) suit-width) (+ x offset-x))]] 
       point-index)))

#_(set 1 2 3 4 3)
(defn get-suit-width
  "Get the max x-width from the array of rectangle info
   Input: [{:id 1 :start-x 1 :start-y 3 :width 4 :length 4}
           {:id 2 :start-x 3 :start-y 1 :width 4 :length 4}]
   Output: 8"
  [fabric-list]
   (apply max 
          (map (fn [fabric]  
                 (inc (+ (fabric :start-x) (fabric :width)))) 
               fabric-list)))

(defn create-fabric-points-list
  "Create a number of sets corresponding to a given rectangle data
   Input: [[1 1 3 4 4] [2 3 1 4 4]]
   Output:({:id 1 :fabric-points #{27 50 33 36 41 43 44 28 51 25 34 35 26 52 42 49}}
           (:id 2 :fabric-points #{20 27 21 13 22 36 29 28 12 35 19 11 14 38 30 37}))"
  [fabric-list]
  (let [width (get-suit-width fabric-list)]
    (map (fn [fabric] {:id (fabric :id) :fabric-points (create-fabric-points fabric width)}) fabric-list)
    )
  )

(defn parse-fabrics
  "Parse fabrics in point"
  [path]
  (->> path
       read-input
       parse-strings
       create-fabric-points-list)
  )

(defn find-all-overlaps
  "find all overlaps of any two rectangle
   Input: [#{27 50 33 36 41 43 44 28 51 25 34 35 26 52 42 49} #{20 27 21 13 22 36 29 28 12 35 19 11 14 38 30 37}]
   Output: #{27 36 28 35}"
  [fabric-list]
  (
    for [first-fabric fabric-list
         second-fabric fabric-list
         :let [intersection-point-set (set/intersection 
                                        (first-fabric :fabric-points) 
                                        (second-fabric :fabric-points))]
          :when (> (first-fabric :id) (second-fabric :id))] 
      intersection-point-set))



(comment 
  (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" "#1 @ 1,3: 4x4")
  (parse-string "#1 @ 1,3: 4x4")
  (parse-strings ["#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4"])
  (get-suit-width (parse-strings ["#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4"]))
  (create-fabric-points {:id 1 :start-x 1 :start-y 3 :width 4 :length 4} 8)
  [(create-fabric-points {:id 1 :start-x 1 :start-y 3 :width 4 :length 4} 8) (create-fabric-points {:id 2 :start-x 3 :start-y 1 :width 4 :length 4} 8)]
  (create-fabric-points-list [{:id 1 :start-x 1 :start-y 3 :width 4 :length 4}
                              {:id 2 :start-x 3 :start-y 1 :width 4 :length 4}])
  (set/intersection (create-fabric-points [1 1 3 4 4] 8) (create-fabric-points [2 3 1 4 4] 8))
  (count (set/intersection (create-fabric-points [1 3 4 4] 7) (create-fabric-points [3 1 4 4] 7)))
  (count (apply set/union (create-fabric-points-list [[1 3 4 4] [3 1 4 4]])))
  (find-all-overlaps [{:id 1, :fabric-points #{27 50 33 36 41 43 44 28 51 25 34 35 26 52 42 49}}
                      {:id 2, :fabric-points #{20 27 21 13 22 36 29 28 12 35 19 11 14 38 30 37}}])
  (->> [#{24 15 21 31 22 36 29 28 17 23 35 14 16 38 30 37} #{24 4 25 17 3 12 2 23 19 11 9 5 26 16 10 18}]
       (apply set/intersection)
       count)

  ;; (->> input
  ;;     (map (fn [] )))     
  (->> "aoc2018_3.input" 
       ;; Parse
       parse-fabrics ;; parse data into set for using set operation
       ;; Process
       find-all-overlaps
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
   Output: ({:id 1 :fabric-points #{27 50 33 36 41 43 44 28 51 25 34 35 26 52 42 49}}
            {:id 2 :fabric-points #{20 27 21 13 22 36 29 28 12 35 19 11 14 38 30 37}}
            {:id 3 :fabric-points #{46 54 45 53}})"
  [fabric-list]
  (let [width (get-suit-width fabric-list)]
    (map (fn [fabric] {:id (fabric :id) :fabric-points (create-fabric-points fabric width)}) fabric-list
     )))

(defn find-union-of-intersctions-for-given-id
  "Find an union of all intersections relevant to the rectangle of given id
   Input: ({:id 1 :fabric-points #{27 50 33 36 41 43 44 28 51 25 34 35 26 52 42 49}}
           {:id 2 :fabric-points #{20 27 21 13 22 36 29 28 12 35 19 11 14 38 30 37}}
           {:id 3 :fabric-points #{46 54 45 53}})
          [1 #{27 50 33 36 41 43 44 28 51 25 34 35 26 52 42 49}]
   Output: #{20 27 21 13 22 36 29 28 12 35 19 11 14 38 30 37}"
  [fabric-points-list 
   target-fabric]
  
   (apply set/union
          (for [fabric fabric-points-list 
                :let [intersection-point-set (set/intersection 
                                              (target-fabric :fabric-points)
                                              (fabric :fabric-points))]
                :when (not= (target-fabric :id) (fabric :id))] 
            intersection-point-set)))

(defn discover-non-overlapped-fabric
  "find the id of the first rectange which does not intersect with any other rectangles
   Input: ({:id 1 :fabric-points #{27 50 33 36 41 43 44 28 51 25 34 35 26 52 42 49}}
           {:id 2 :fabric-points #{20 27 21 13 22 36 29 28 12 35 19 11 14 38 30 37}}
           {:id 3 :fabric-points #{46 54 45 53}})
   Output: 3"
  [fabric-point-set-list]
  (take 1
   (for [target-fabric fabric-point-set-list
         :let [union-of-intersections
               (find-union-of-intersctions-for-given-id
                fabric-point-set-list
                target-fabric)]
               :when (empty? union-of-intersections)]
           target-fabric))
  )

(comment
  (->> [[1 1 3 4 4] [2 3 1 4 4] [3 5 5 2 2]]
       create-sets-with-id-from-rectangle-data)
  (let [rectangle-point-set-list (->> [[1 1 3 4 4] [2 3 1 4 4] [3 5 5 2 2]]
                                      create-sets-with-id-from-rectangle-data)
        num-rectangles (count rectangle-point-set-list)]
    (find-union-of-intersctions-for-given-id rectangle-point-set-list num-rectangles 0 (second rectangle-point-set-list)))
  (->> [{:id 1 :start-x 1 :start-y 3 :width 4 :length 4}
        {:id 2 :start-x 3 :start-y 1 :width 4 :length 4}
        {:id 3 :start-x 5 :start-y 5 :width 2 :length 2}]
       create-sets-with-id-from-rectangle-data
       discover-non-overlapped-fabric)
  (let [fabric-points-list (->> "aoc2018_3.input" 
                                      read-input 
                                      parse-strings 
                                      create-sets-with-id-from-rectangle-data)
        ]
    (find-union-of-intersctions-for-given-id fabric-points-list (second fabric-points-list))
    )
  (-> "aoc2018_3.input" 
       ;; Parse
       parse-fabrics
       ;; Process
       ;; Aggregate
       discover-non-overlapped-fabric
       ;;Print
       (get :id))
  ;; ANS 1260
  ;; too slow, how to boost up?
  ;; https://bsless.github.io/code-smells/
  ;; https://www.notion.so/greenlabs/Clojure-Do-Don-t-01466ef3b1a34885a9663ff64a8b5255
  ;;-> iterate
  ;; take 5
  ;; drop-while, take-while
  )