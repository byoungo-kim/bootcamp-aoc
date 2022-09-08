(ns aoc2018_5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))
;; 파트 1
;; 입력: dabAcCaCBAcCcaDA
(defn read-input
  "parse file input into events
   Input: file path
   Output: \"dabAcCaCBAcCcaDA\""
  [path]
  (->> path io/resource slurp))

;; 같은 종류의 소문자와 대문자는 서로 ‘반응‘하여 사라짐. aABb -> ‘’
;; 사라진 자리는 진공이 되기 때문에 다른 문자들이 붙게 되고, 또 그 문자들끼리 반응할 수 있음.  abBA-> aA -> ‘’
;; 바로 옆에 붙어있어야만 서로 반응함. abAB -> abAB (반응 없음)
;; 대문자-대문자, 소문자-소문자는 서로 반응하지 않음. aabAAB-> aabAAB (반응 없음)
;; 예시 dabAcCaCBAcCcaDA => dabCBAcaDA

(defn check-polymers
  "Check whether the reaction is possible or not
   Input: c C
   Otput: true"
  [polymer-a polymer-b]
  (let [diff (- (int polymer-a) (int polymer-b))]
    (= (abs diff) 32) ; calculate the diff is 32 which is upper and lower case character value diff
    )
  )

(defn full-reaction
  "React all polymers
   Input: \"dabAcCaCBAcCcaDA\"
   Output: \"dabCBAcaDA\""
   [polymers]
   (str/join 
    (last 
     (reductions (fn [stack polymer]
                   (if-not (empty? stack)
                     (cond
                       (check-polymers (peek stack) polymer) (pop stack)
                       :else (conj stack polymer))
                     (conj stack polymer))) [] (char-array polymers)))))

;; 주어진 input 에서 최종으로 남는 문자열을 리턴하시오.
(comment
  (check-polymers \a \B)
  (count (str/join (full-reaction "dabAcCaCBAcCcaDA")))
  (->> "aoc2018_5.input"
       read-input
       full-reaction 
       count
       )
  )
;; 파트 2
;; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.
;; 예를 들어 dabAcCaCBAcCcaDA 에서 a/A를 없애고 모두 반응시키면 dbCBcD가 되고 길이는 6인데 비해,
;; 같은 문자열에서 c/C를 없애고 모두 반응시키면 daDA가 남고 길이가 4이므로 4가 가장 짧은 길이가 됨.


(defn non-collapsing-polymers 
  "Generate all non-collapsing-polymers
   Input: \"dabAcCaCBAcCcaDA\"
   Output: (\"dbcCCBcCcD\" \"daAcCaCAcCcaDA\" \"dabAaBAaDA\" \"abAcCaCBAcCcaA\")"
  [polymers]
  (let [collapsing-polymers (keys (frequencies polymers))]
    (for [collapsing-polymer collapsing-polymers]
      (-> polymers
          (str/replace (str collapsing-polymer) "")
          (str/replace (str/upper-case (str collapsing-polymer)) ""))
  )))

(defn non-collapsing-full-reactions
  "React all possible non-collapsing-full-reactions
   Input: \"dabAcCaCBAcCcaDA\"
   Output: (\"dbCBcD\" \"daCAcaDA\" \"daDA\" \"abAaCBAc\")"
  [polymers]
  (->> polymers
       non-collapsing-polymers
       (map full-reaction))
  )

(comment
  (frequencies "dabAcCaCBAcCcaDA")
  (apply min (map count (map str/join (map full-reaction (non-collapsing-polymers "dabAcCaCBAcCcaDA")))))
  (->> "aoc2018_5.input"
       read-input
       non-collapsing-full-reactions
       (map count)
       (apply min))
  )