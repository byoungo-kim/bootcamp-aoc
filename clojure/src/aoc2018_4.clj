(ns aoc2018_4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            ))
;; 파트 1
;; 입력:

;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up
;; [1518-11-01 23:58] Guard #99 begins shift
;; [1518-11-02 00:40] falls asleep
;; [1518-11-02 00:50] wakes up
;; [1518-11-03 00:05] Guard #10 begins shift
;; [1518-11-03 00:24] falls asleep
;; [1518-11-03 00:29] wakes up
;; [1518-11-04 00:02] Guard #99 begins shift
;; [1518-11-04 00:36] falls asleep
;; [1518-11-04 00:46] wakes up
;; [1518-11-05 00:03] Guard #99 begins shift
;; [1518-11-05 00:45] falls asleep
;; [1518-11-05 00:55] wakes up

;; 키워드: 가드(Guard) 번호, 자는 시간(falls asleep), 일어나는 시간(wakes up).
;; 각 가드들은 교대 근무를 시작하고 (begins shift) 졸았다가 일어났다를 반복함.
;; 위의 예시에서 10번 가드는 0시 5분에 잤다가 25분에 일어나고, 또 0시 30분에 잠들었다가 0시 55분에 깨어남.
;; 가드들에 대해서 자고 깨는 시간 정보들이 입력으로 주어짐.

;; 파트 1은 “주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라”
;; 만약 20번 가드가 0시 10분~36분, 다음날 0시 5분~11분, 다다음날 0시 11분~13분 이렇게 잠들어 있었다면, “11분“이 가장 빈번하게 잠들어 있던 ‘분’. 그럼 답은 20 * 11 = 220.
;; 

(defn parse-event
  "Convert an input string into event data
   Input: \"[1518-11-05 00:03] Guard #99 begins shift\"
   Output: {:year 1518 :month 11 :date 5 :hour 0 :minute 3 :event-type \"Guard\" :event-value \"#99\"}"
  [input-string]
  (let [[_ year month date hour minute event-type event-value]
        (re-find #"\[(\d+)-(\d+)-(\d+) (\d+)\:(\d+)\] (\S+) (\S+)" input-string)]
    {:year (Integer/parseInt year) :month (Integer/parseInt month) 
     :date (Integer/parseInt date) :hour (Integer/parseInt hour) 
     :minute (Integer/parseInt minute) :event-type event-type :event-value event-value}
                 
  ))

(defn parse-events
  "parse file input into events
   Input: file path
   Output: ({:year \"1518\", :month \"11\", :date \"23\", :hour \"00\", 
             :minute \"43\", :event-string \"wakes\", :event-value \"#99\"}
            ...)"
  [path]
  (->> path
       io/resource
       slurp
       str/split-lines
       sort
       (map parse-event)
       (map (fn [event] (conj (assoc event :timestamp 
                                     (->> (event :month)
                                          (* 30)
                                          (+ (event :date))
                                          (* 24)
                                          (+ (event :hour))
                                          (* 60)
                                          (+ (event :minute)))))))))

(defn find-guards
  "find all individual guards
   Input: [{:year \"1518\", :month \"11\", :date \"23\", :hour \"00\", 
             :minute \"43\", :event-string \"wakes\", event-value \"#99\"}
            ...)]
   Output: #{\"#99\", ...}"
  [events]
  (->> events
       seq
       (reductions
        (fn [guard-ids event]
          (cond
            (= (event :event-type) "Guard") (conj guard-ids (event :event-value))
            :else guard-ids)) #{})
       last ; guard ids)
       ))

(defn group-by-guard
  "find each guard
   Input: [{:year \"1518\", :month \"11\", :date \"23\", :hour \"00\", 
             :minute \"43\", :event-string \"wakes\", :guard-num \"p\"}
            ...)]"
  [events]
  (let [guard-ids (find-guards events)](
    for [guard-id guard-ids] 
    (->> events 
         (reductions 
          (fn [grouped-events event]
            (cond 
              (= (event :id) guard-id)
              (cond
                (= (event :event-type) "falls")
                (conj grouped-events
                      (conj (event :timestamp))) ;sleep
                (= (event :event-type) "wakes")
                (conj grouped-events
                      (conj (event :timestamp))) ;wake up
                :else grouped-events
                )
              :else grouped-events)
            {}))
         last
         ))))

(comment
  ;;need sort of records before parse event
  (parse-events "aoc2018_4.input")
  (group-by-guard [{:year 1518, :month 2, :day 10, :hour 23, :minute 47, :event-type "Guard", :event-value "#1033"}
{:year 1518, :month 2, :day 11, :hour 0, :minute 5, :event-type "falls", :event-value "asleep"}
{:year 1518, :month 2, :day 11, :hour 0, :minute 28, :event-type "wakes", :event-value "up"}])
  (->> "aoc2018_4.input" 
       parse-events
       group-by-guard
       )
)
;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.
