(ns aoc2018-2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; Get input
(defn read-input
  "Input: a file path
   Output: line-by-line separated array"
  [path] (-> path
             (io/resource)
             (slurp)
             (clojure.string/split-lines)))
;; 파트 1
;; 주어진 각각의 문자열에서, 같은 문자가 두번 혹은 세번씩 나타난다면 각각을 한번씩 센다.
;; 두번 나타난 문자가 있는 문자열의 수 * 세번 나타난 문자가 있는 문자열의 수를 반환하시오.
;; 예)
;; abcdef 어떤 문자도 두번 혹은 세번 나타나지 않음 -> (두번 나오는 문자열 수: 0, 세번 나오는 문자열 수: 0)
;; bababc 2개의 a, 3개의 b -> (두번 나오는 문자열 수: 1, 세번 나오는 문자열 수: 1)
;; abbcde 2개의 b -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 1)
;; abcccd 3개의 c -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 2)
;; aabcdd 2개의 a, 2개의 d 이지만, 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 3, 세번 나오는 문자열 수: 2)
;; abcdee 2개의 e -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 2)
;; ababab 3개의 a, 3개의 b 지만 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 3)
;; 답 : 4 * 3 = 12

(defn aggregate-frequency-check
  "Input: The frequency of a letter
   Output: the aggregated array of duplicate check ()"
  [frequency checked-duplicates]
  (cond
    (= frequency 2) (map bit-or checked-duplicates [1 0])
    (= frequency 3) (map bit-or checked-duplicates [0 1])
    :else (map bit-or checked-duplicates[0 0])))

(defn check-frequencies
  "Input: frequencies of letters
     Output: existence of letters with two and three occurence"
  [letter-frequencies]
  (loop [letters (keys letter-frequencies)
         checked-duplicates [0 0]]
    (if (seq letters)
      (recur
       (rest letters)
       (aggregate-frequency-check (letter-frequencies (first letters)) checked-duplicates))
      checked-duplicates)))

(defn aggregate-string-check-result
  "Input: intermediary aggregated string check results and the accumulated check result
     Output: The newly accumulated check result"
  [checked-duplicates num-of-duplicates]
  (map + checked-duplicates num-of-duplicates))

(defn count-ids
  "Input: An array of strings
   Output: Two numbers, one is the number of ids which has the same letter twice, and the other number of whom has the same letter three times"
  [string-sequence]
  (loop [strings string-sequence num-of-duplicates [0 0]]
    (if-not (empty? strings)
      (recur (rest strings)
             (-> (first strings)
                 frequencies
                 check-frequencies
                 (aggregate-string-check-result num-of-duplicates)))
      num-of-duplicates)))


(comment
  (->> "aoc2018_2.input"
      read-input ;; read file as a string array
      count-ids
     (reduce *))) ;; count ids)
  


;; Parse
;; Process  [0 0] [1 0] [1 1] [0 1]
;; Aggregate * 
;; Print 

;; 파트 2
;; 여러개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오.
;; 예)
;; abcde
;; fghij
;; klmno
;; pqrst
;; fguij
;; axcye
;; wvxyz

;; 주어진 예시에서 fguij와 fghij는 같은 위치 (2번째 인덱스)에 정확히 한 문자 (u와 h)가 다름. 따라서 같은 부분인 fgij를 리턴하면 됨.

;; [f f] t
;; [g g] t
;; [u h] x
;; [i i] t
;; [j j] t
;; input ["Asdfdasf" "Adsfdas"]



;; (for [i (range length)
;;       j (range length)
;;       :when (> j i)
;;       :let [identical-part (->> (diff (nth input i) (nth input j)))])])

(defn get-common-string-and-original-diff
  "Get the common part from two strings and the number of differernt letters
   Input: [\"fghij\" \"fguij\"]
   Output: \"fgij\""
  [first-string second-string]
  (let [length (min (count first-string) (count second-string))]
    (str/join
     (for [i (range length)
          :let [letter-of-first (nth first-string i)]
          :let [letter-of-second (nth second-string i)]
          :let [identical-letter letter-of-first]
          :when (identical? letter-of-first letter-of-second)]
     identical-letter)
    )
    )
  )
(defn find-strings-of-one-letter-diff
  "Input: [\"fghij\" \"fguij\"]
   Output: \"fgij\""
  [string-sequence]
  (let [length (count string-sequence)]
    (
     take 1 (for [i (range length)  
          j (range length)  
          :let [first-string (nth string-sequence i)
                second-string (nth string-sequence j)
                common-string (get-common-string-and-original-diff  
                               first-string 
                               second-string
                               )]  
          :when (and (> i j) (= (count common-string) (dec (count first-string))))] 
     common-string)))
   )

(comment
  (get-common-string-and-original-diff "ybruvapdgixszyckwtfqjonsie" "mbruvapxghslyyckwtfqjonsie")
  (find-strings-of-one-letter-diff ["fghij" "abcde" "fguij" "fgabc"])
  (find-strings-of-one-letter-diff ["ybruvapdgixszyckwtfqjonsie" "mbruvapxghslyyckwtfqjonsie"])
  (->> "aoc2018_2.input" 
       read-input
       find-strings-of-one-letter-diff)
  )
  

;; #################################
;; ###        Refactoring        ###
;; #################################