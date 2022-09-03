(ns aoc2018_4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))
            
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
  (let [[_
         _
         _
         _
         _
         minute
         event-type
         event-value]
        (re-find #"\[(\d+)-(\d+)-(\d+) (\d+)\:(\d+)\] (\S+) (\S+)" input-string)]
    {:minute (Integer/parseInt minute)
     :event-type event-type
     :event-value
     (if (= event-type "Guard")
       (str/join (rest event-value))
       event-value)}))
                 
  

(defn parse-events
  "parse file input into events
   Input: file path
   Output: ({:minute \"43\", :event-type \"wakes\", :event-value \"#99\"}
            ...)"
  [path]
  (->> path io/resource slurp str/split-lines sort
       (map parse-event)
       seq))


(defn daily-sleep-history
  "find each guard
   Input: ({:minute 0, :event-type \"Guard\", :event-value \"10\"}
           {:minute 5, :event-type \"falls\", :event-value \"asleep\"}
           {:minute 25, :event-type \"wakes\", :event-value \"up\"}
           {:minute 30, :event-type \"falls\", :event-value \"asleep\"}
           {:minute 55, :event-type \"wakes\", :event-value \"up\"}
           {:minute 58, :event-type \"Guard\", :event-value \"99\"}
           {:minute 40, :event-type \"falls\", :event-value \"asleep\"}
           {:minute 50, :event-type \"wakes\", :event-value \"up\"}
           {:minute 5, :event-type \"Guard\", :event-value \"10\"}
           {:minute 24, :event-type \"falls\", :event-value \"asleep\"}
           {:minute 29, :event-type \"wakes\", :event-value \"up\"}
           {:minute 2, :event-type \"Guard\", :event-value \"99\"}
           {:minute 36, :event-type \"falls\", :event-value \"asleep\"}
           {:minute 46, :event-type \"wakes\", :event-value \"up\"}
           {:minute 3, :event-type \"Guard\", :event-value \"99\"}
           {:minute 45, :event-type \"falls\", :event-value \"asleep\"}
           {:minute 55, :event-type \"wakes\", :event-value \"up\"}))
   Output: ({:id 10,
           :sleep-events
            (5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54)}
          {:id 99, :sleep-events (40 41 42 43 44 45 46 47 48 49)}
          {:id 10, :sleep-events (24 25 26 27 28)}
          {:id 99, :sleep-events (36 37 38 39 40 41 42 43 44 45)}
          {:id 99, :sleep-events (45 46 47 48 49 50 51 52 53 54)})"
  [events]
  (->> events 
       (partition-by #(= "Guard" (:event-type %))) 
       (partition 2)  
       (map (fn [item]  
              (let [start-event (first item) 
                    sleep-events (mapcat  
                                  (fn [sleep-event] 
                                    (let [first-minute (:minute (first sleep-event))
                                          second-minute (:minute (second sleep-event))] 
                                      (take-while
                                       #(> second-minute %)
                                       (iterate inc first-minute))))
                                  (partition 2 (second item)))] 
                {:id (Integer/parseInt
                      (:event-value (first start-event)))
                 :sleep-events sleep-events})))))
         

(defn find-guards
  "find all individual guards
   Input: ({:id 10,
           :sleep-events
            ((5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24)
            (30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54))}
          {:id 99, :sleep-events ((40 41 42 43 44 45 46 47 48 49))}
          {:id 10, :sleep-events ((24 25 26 27 28))}
          {:id 99, :sleep-events ((36 37 38 39 40 41 42 43 44 45))}
          {:id 99, :sleep-events ((45 46 47 48 49 50 51 52 53 54))})
   Output: #{99, ...}"
  [daily-sleep-history]
  (->> daily-sleep-history
       (reductions
        (fn [guard-ids daily-sleep]
          (let [id (:id daily-sleep)]
            (conj guard-ids id))) #{})
       last)) ; guard ids)
       

#_(defn find-longest-sleep-event
  "find the longest sleep event
   Input: ({:id 10,
           :sleep-events
            (5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54)}
          {:id 99, :sleep-events (40 41 42 43 44 45 46 47 48 49)}
          {:id 10, :sleep-events (24 25 26 27 28)}
          {:id 99, :sleep-events (36 37 38 39 40 41 42 43 44 45)}
          {:id 99, :sleep-events (45 46 47 48 49 50 51 52 53 54)})"
  [daily-sleep-history]
  (let [guard-ids (find-guards daily-sleep-history)
        guard-of-most-asleep (take 1
                                   (sort
                                    #(> (count (:total-sleep %1))
                                        (count (:total-sleep %2)))
                                    (for [id guard-ids
                                          :let [total-sleep
                                                (last
                                                 (reductions (fn [total-sleep daily-sleep]
                                                               (if (= id (:id daily-sleep))
                                                                 (concat total-sleep (:sleep-events daily-sleep))
                                                                 total-sleep)) () daily-sleep-history))
                                                most-frequent-sleep-minute (take 1 (sort #(> (second %1) (second %2)) (frequencies total-sleep)))
                                                frequencies (frequencies total-sleep)
                                                potential-answer (* (first (first most-frequent-sleep-minute)) id)
                                                total-sleep-minutes (count total-sleep)]]
                                      {:id id :total-sleep total-sleep 
                                       :most-frequent-sleep-minute most-frequent-sleep-minute
                                       :answer potential-answer
                                       :frequencies frequencies
                                       :total-sleep-minutes total-sleep-minutes})))
        ] 
    guard-of-most-asleep))

(defn group-events-by-guard
  "Group the sleep events for each guard
   Input: ({:id 10,
           :sleep-events
            (5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54)}
          {:id 99, :sleep-events (40 41 42 43 44 45 46 47 48 49)}
          {:id 10, :sleep-events (24 25 26 27 28)}
          {:id 99, :sleep-events (36 37 38 39 40 41 42 43 44 45)}
          {:id 99, :sleep-events (45 46 47 48 49 50 51 52 53 54)})
   Output: ({:guard-id 10 :sleeep-events (5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 24 25 26 27 28)}
            {:guard-id 99 :sleeep-events (40 41 42 43 44 45 46 47 48 49 36 37 38 39 40 41 42 43 44 45 45 46 47 48 49 50 51 52 53 54)})
   "
  [daily-sleep-history]
  (for [guard-id (find-guards daily-sleep-history)]
    {:guard-id guard-id
     :sleep-events (apply concat
                    (for [daily-sleep daily-sleep-history 
                          :let [target-id (:id daily-sleep)] 
                          :when (= target-id guard-id)] 
                      (:sleep-events daily-sleep)))})
  )

(defn find-longest-sleep-event
  "Find the longest sleep events
   Input: ({:guard-id 10 :sleeep-events (5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 24 25 26 27 28)}
           {:guard-id 99 :sleeep-events (40 41 42 43 44 45 46 47 48 49 36 37 38 39 40 41 42 43 44 45 45 46 47 48 49 50 51 52 53 54)})
   Output: {:guard-id 10 :sleeep-events (5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 24 25 26 27 28)}"
  [grouped-sleep-events]
  (->> grouped-sleep-events 
       (sort #(> (count (:sleep-events %1)) (count (:sleep-events %2))))
       (take 3)
       )
  )

(defn make-guard-statistics
  "Find the longest sleep event
   Input: ({:id 10,
           :sleep-events
            (5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54)}
          {:id 99, :sleep-events (40 41 42 43 44 45 46 47 48 49)}
          {:id 10, :sleep-events (24 25 26 27 28)}
          {:id 99, :sleep-events (36 37 38 39 40 41 42 43 44 45)}
          {:id 99, :sleep-events (45 46 47 48 49 50 51 52 53 54)})
   Output: ({:id 10, :total-sleep-minutes 50, :most-frequent-sleep-minute 24}
            {:id 99, :total-sleep-minutes 30, :most-frequent-sleep-minute 45})"
  [daily-sleep-history]
  (->> (for [target-id (find-guards daily-sleep-history)]
    {:id target-id
     :total-sleep 
     (flatten (for [daily-sleep daily-sleep-history
          :let [jth-id (:id daily-sleep)]
          :when (= target-id jth-id)]
      (:sleep-events daily-sleep)))})
      (map (fn [guard-history] {;;:total-sleep (:total-sleep guard-history)
                                :id (:id guard-history) 
                                :total-sleep-minutes (count (:total-sleep guard-history))
                                :sleep-frequencies (frequencies (:total-sleep guard-history))
                                :most-frequent-sleep-minute (first 
                                                             (first  
                                                              (sort  
                                                               #(> (second %1) (second %2))  
                                                               (frequencies (:total-sleep guard-history)))))
                                })) 
  ))

(defn find-the-most-frequent-minute
  "Find the most frequent minute on a given grouped sleep event
   Input: {:guard-id 10 :sleeep-events (5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 24 25 26 27 28)}
   Output: {:guard-id 10 :most-frequent-sleep-minute 24}"
  [grouped-sleep-event]
  {:guard-id (:guard-id grouped-sleep-event)
   :total-sleep-minutes (count (:sleep-events grouped-sleep-event))
   :most-frequent-sleep-minute (apply max-key val 
                                        (frequencies (:sleep-events grouped-sleep-event))
                                      )
   :top-3-frequencies (take 3 (sort #(> (val %1) (val %2)) 
                                    (frequencies (:sleep-events grouped-sleep-event))))}
  )

(defn answer-part1
  "calculate the answer to the part 1
   Input: {:guard-id 10 :sleeep-events (5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 24 25 26 27 28)}
   Output: 240"
  [answer-info]
  (* (:guard-id answer-info) (first (:most-frequent-sleep-minute answer-info))))

(comment
  ;;need sort of records before parse event
  (let [input-strings 
        '("[1518-11-01 00:00] Guard #10 begins shift"
          "[1518-11-01 00:05] falls asleep"
          "[1518-11-01 00:25] wakes up"
          "[1518-11-01 00:30] falls asleep"
          "[1518-11-01 00:55] wakes up"
          "[1518-11-01 23:58] Guard #99 begins shift"
          "[1518-11-02 00:40] falls asleep"
          "[1518-11-02 00:50] wakes up"
          "[1518-11-03 00:05] Guard #10 begins shift"
          "[1518-11-03 00:24] falls asleep"
          "[1518-11-03 00:29] wakes up"
          "[1518-11-04 00:02] Guard #99 begins shift"
          "[1518-11-04 00:36] falls asleep"
          "[1518-11-04 00:46] wakes up"
          "[1518-11-05 00:03] Guard #99 begins shift"
          "[1518-11-05 00:45] falls asleep"
          "[1518-11-05 00:55] wakes up"
          "[1518-11-06 00:00] Guard #10 begins shift"
          "[1518-11-06 00:05] falls asleep"
          "[1518-11-06 00:25] wakes up"
          "[1518-11-06 00:30] falls asleep"
          "[1518-11-06 00:55] wakes up"
          "[1518-11-06 23:58] Guard #99 begins shift"
          "[1518-11-07 00:40] falls asleep"
          "[1518-11-07 00:50] wakes up"
          "[1518-11-08 00:05] Guard #10 begins shift"
          "[1518-11-08 00:24] falls asleep"
          "[1518-11-08 00:29] wakes up"
          "[1518-11-09 00:02] Guard #99 begins shift"
          "[1518-11-09 00:36] falls asleep"
          "[1518-11-09 00:46] wakes up"
          "[1518-11-10 00:03] Guard #99 begins shift"
          "[1518-11-10 00:45] falls asleep"
          "[1518-11-10 00:55] wakes up")]
   (->> (map parse-event input-strings)
        ;; process
        daily-sleep-history
        ;;aggregate 
        group-events-by-guard
        find-longest-sleep-event
        find-the-most-frequent-minute 
        answer-part1
        ))
       
(* 1021 29)
  ;; (seq (find-guards (parse-events "aoc2018_4.input")))
  (daily-sleep-history [{:minute 0, :event-type "Guard", :event-value "10"}
                        {:minute 5, :event-type "falls", :event-value "asleep"}
                        {:minute 25, :event-type "wakes", :event-value "up"}
                        {:minute 30, :event-type "falls", :event-value "asleep"}
                        {:minute 55, :event-type "wakes", :event-value "up"}
                        {:minute 58, :event-type "Guard", :event-value "99"}
                        {:minute 40, :event-type "falls", :event-value "asleep"}
                        {:minute 50, :event-type "wakes", :event-value "up"}
                        {:minute 5, :event-type "Guard", :event-value "10"}
                        {:minute 24, :event-type "falls", :event-value "asleep"}
                        {:minute 29, :event-type "wakes", :event-value "up"}
                        {:minute 2, :event-type "Guard", :event-value "99"}
                        {:minute 36, :event-type "falls", :event-value "asleep"}
                        {:minute 46, :event-type "wakes", :event-value "up"}
                        {:minute 3, :event-type "Guard", :event-value "99"}
                        {:minute 45, :event-type "falls", :event-value "asleep"}
                        {:minute 55, :event-type "wakes", :event-value "up"}])
  (->> "aoc2018_4.input"
       parse-events
       daily-sleep-history
       group-events-by-guard
       find-longest-sleep-event
       (map find-the-most-frequent-minute)
       #_(map answer-part1)) ; ignore for testing current value
  )

;; ({:id 1021, :total-sleep-minutes 493, :most-frequent-sleep-minute 29}29609
;;  {:id 1901, :total-sleep-minutes 480, :most-frequent-sleep-minute 51}96951
;;  {:id 3371, :total-sleep-minutes 455, :most-frequent-sleep-minute 39}131469
;;  {:id 1033, :total-sleep-minutes 452, :most-frequent-sleep-minute 48}49584
;;  {:id 2729, :total-sleep-minutes 442, :most-frequent-sleep-minute 43}117347
;;  {:id 1193, :total-sleep-minutes 408, :most-frequent-sleep-minute 32}38176
;;  {:id 1231, :total-sleep-minutes 402, :most-frequent-sleep-minute 19}23389
;;  {:id 1171, :total-sleep-minutes 375, :most-frequent-sleep-minute 24}28104
;;  {:id 3119, :total-sleep-minutes 350, :most-frequent-sleep-minute 36}112284
;;  {:id 2707, :total-sleep-minutes 287, :most-frequent-sleep-minute 48}129936
;;  {:id 1297, :total-sleep-minutes 285, :most-frequent-sleep-minute 33}42801
;;  {:id 2447, :total-sleep-minutes 284, :most-frequent-sleep-minute 41}100327
;;  {:id 1993, :total-sleep-minutes 272, :most-frequent-sleep-minute 34}67762
;;  {:id 2129, :total-sleep-minutes 269, :most-frequent-sleep-minute 45}95805
;;  {:id 419, :total-sleep-minutes 262, :most-frequent-sleep-minute 24}10056
;;  {:id 2383, :total-sleep-minutes 182, :most-frequent-sleep-minute 20}47660
;;  {:id 1553, :total-sleep-minutes 166, :most-frequent-sleep-minute 17}26401
;;  {:id 3499, :total-sleep-minutes 166, :most-frequent-sleep-minute 36}125964
;;  {:id 2539, :total-sleep-minutes 155, :most-frequent-sleep-minute 44}111716
;;  {:id 2963, :total-sleep-minutes 136, :most-frequent-sleep-minute 13}38519
;;  {:id 283, :total-sleep-minutes 135, :most-frequent-sleep-minute 27}7641
;;  {:id 3533, :total-sleep-minutes 58, :most-frequent-sleep-minute 48}169584
;;  {:id 1723, :total-sleep-minutes 55, :most-frequent-sleep-minute 39})67197
;; ANS: 29609 1021 * 29

;; 이현님 답
;; 30630

;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.
;; 이현님 답
;; 96951

;; 96951
(defn solve-part-2
  "Input: {:id 99,
           :total-sleep-minutes 30,
           :sleep-frequencies
           {39 1, 46 2, 54 1, 48 2, 50 1, 40 2, 36 1, 41 2, 43 2, 44 2, 51 1, 47 2, 45 3, 53 1, 38 1, 52 1, 42 2, 37 1, 49 2},
           :most-frequent-sleep-minute 45}"
  [guard-statistic]
  (* (:id (first guard-statistic)) (:most-frequent-sleep-minute (first guard-statistic))))

(comment
  (->> "aoc2018_4.input"
       parse-events
       daily-sleep-history
       make-guard-statistics 
       ;;aggregate 
       (sort #(> (apply max (vals (:sleep-frequencies  %1))) (apply max (vals (:sleep-frequencies %2)))))
       (take 1)
       solve-part-2)
  )

