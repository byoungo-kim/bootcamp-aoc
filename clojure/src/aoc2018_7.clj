(ns aoc2018_7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-requirements
  "Parse the requirement from a string
   Input: \"Step C must be finished before step A can begin.\"
   Output: {:step \"C\" :requirement \"A\"}"
  [line-string]
  (let [[_ requirement step] (re-find #"Step (\S+) must be finished before step (\S+) can begin." line-string)]
    {:step step :requirement requirement}))

(defn merge-requirements
  "Merge requirements of a given step
   Input: {(\"I\"
            [{:step \"I\", :requirement \"J\"}
             {:step \"I\", :requirement \"O\"}
             {:step \"I\", :requirement \"A\"}
             {:step \"I\", :requirement \"S\"})}
   Output: {:step \"I\" :requirements [\"J\" \"O\" \"A\" \"S\"]}"
  [grouped-requirements]
  (let [requirement (seq grouped-requirements)
        requirements (reduce 
                      (fn [aggregated item] (conj aggregated (:requirement item)))
                      []
                      (second requirement))]
    {:step (first requirement) 
     :requirements requirements}))

(defn generate-requirements-graph
  "
   Input: {:step \"A\", :requirements [\"C\"]}
          {:step \"B\", :requirements [\"A\"]}
          {:step \"D\", :requirements [\"A\"]}
          {:step \"E\", :requirements [\"B\" \"D\" \"F\"]}
          {:step \"F\", :requirements [\"C\"]}
   Output: {:all-steps [\"A\" \"B\" \"C\" \"D\" \"E\"]}"
   [requirements]
   (let [steps (reduce (fn [reduced item] (merge reduced (:requirements item))) requirements)]
     {:steps steps :requirements requirements})
  )

(defn find-schedule
  "Find a schedule from a given sequence"
  [steps]
  (iterate 
   (fn [remained-steps] 
     (let [next-step (filter #(count (:requirements %)) remained-steps)]
       {})) 
   steps) ;iterate (fn [[a b]] [b (+' a b)]) [0 1]
)

(defn parse-input
  "Input: a file path
   Output: line-by-line separated array"
  [path] (->> path
              io/resource
              slurp
              clojure.string/split-lines
              (map parse-requirements)
              (group-by :step)
              (map merge-requirements)
              generate-requirements-graph))

(comment
  (->> ["Step C must be finished before step A can begin."
        "Step C must be finished before step F can begin."
        "Step A must be finished before step B can begin."
        "Step A must be finished before step D can begin."
        "Step B must be finished before step E can begin."
        "Step D must be finished before step E can begin."
        "Step F must be finished before step E can begin."]
       (map parse-requirements)
       (group-by :step)
       (map merge-requirements)
       (sort-by :step)
       generate-requirements-graph
       )
  (->> "aoc2018_7.input"
       parse-input
       (sort-by :step))
  )