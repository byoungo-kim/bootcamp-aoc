(ns aoc2018_7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-steps
  "Parse the requirement from a string
   Input: \"Step C must be finished before step A can begin.\"
   Output: {:step \"C\" :requirement \"A\"}"
  [line-string]
  (let [[_ step next-step] (re-find #"Step (\S+) must be finished before step (\S+) can begin." line-string)]
    {:step step :next-step next-step}))

(defn count-next-steps
  "Find all existing steps
   Input: {:step \"C\" :next-step \"A\"}
          {:step \"A\" :next-step \"B\"}
          {:step \"A\" :next-step \"D\"}
          {:step \"B\" :next-step \"E\"}
          {:step \"D\" :next-step \"E\"}
          {:step \"F\" :next-step \"E\"}
          {:step \"C\" :next-step \"F\"}
   Output: {:all-steps [\"A\" \"B\" \"C\" \"D\" \"E\" \"F\"]}"
   [steps]
   (let [all-steps (->> steps 
                        (reduce 
                         (fn [all-steps item] 
                           (conj all-steps (:next-step item) (:step item))) 
                         #{})
                        sort
                        (map
                         (fn [key] {key 0}))
                        (reduce conj))]
     (->> steps 
          (map (fn [step] (:next-step step)))
          frequencies
          (merge all-steps)
          (sort-by key))))

(defn parse-input
  "Input: a file path
   Output: line-by-line separated array"
  [path] (->> path
              io/resource
              slurp
              clojure.string/split-lines
              (map parse-steps)
              (sort-by :step)))

(defn get-schedule
  "Generate a schedule upon n workers
   Input: steps
          workers ({:remained-time 0})}
   Output: (:schedule \\C \\A \\B \\D \\F \\E
            :working-time 21)"
  [workers steps]
  (
   loop [remained-steps steps
         next-step-counts (count-next-steps steps)
         workers workers
         taken-steps []]
    (let [sorted-steps (sort-by val next-step-counts)
          taken-step (key (first sorted-steps))
          new-remained-steps (filter 
                              (fn [step] (not= (:step step) taken-step))
                              remained-steps)
          next-taken-steps (conj taken-steps taken-step)
          next-steps (->> (rest sorted-steps) 
                          keys)]
      (if (> (count new-remained-steps) 0)
        (recur new-remained-steps
               (count-next-steps new-remained-steps)
               workers
               next-taken-steps)
        {:schedule (conj next-taken-steps (str/join next-steps))}))
   )
  )

;;part 1
(comment
  (->> ["Step C must be finished before step A can begin."
        "Step C must be finished before step F can begin."
        "Step A must be finished before step B can begin."
        "Step A must be finished before step D can begin."
        "Step B must be finished before step E can begin."
        "Step D must be finished before step E can begin."
        "Step F must be finished before step E can begin."]
       (map parse-steps)
       (sort-by :step) 
       (get-schedule [{:remained-time 0}])
       :schedule
       str/join
       )
  (->> "aoc2018_7.input"
       parse-input
       (get-schedule {:remained-time 0})
       :schedule
       str/join)
  )

;; part 2
(comment
  ;; work-in-progress
  (->> "aoc2018_7.input"
       parse-input
       (get-schedule {:remained-time 0})
       str/join)
  (->> "aoc2018_7.input"
       parse-input
       (get-schedule [{:remained-time 0} {:remained-time 0}])
       str/join)
  )