(ns aoc2018_7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-steps
  "Parse the requirement from a string
   Input: \"Step C must be finished before step A can begin.\"
   Output: {:step \"C\" :requirement \"A\"}"
  [line-string]
  (let [[_ step next-step] (re-find #"Step (\S+) must be finished before step (\S+) can begin." line-string)]
    {:step step :next-step next-step}))

(defn find-all-steps
  "Find the all steps which need to be handled
   Input: {:step \"C\" :next-step \"A\"}
          {:step \"A\" :next-step \"B\"}
          {:step \"A\" :next-step \"D\"}
          {:step \"B\" :next-step \"E\"}
          {:step \"D\" :next-step \"E\"}
          {:step \"F\" :next-step \"E\"}
          {:step \"C\" :next-step \"F\"}
   Output: (\"A\" \"B\" \"C\" \"D\" \"E\"\"F\")"
  [steps]
  (->> steps
       (mapcat (fn [step] [(:step step) (:next-step step)]))
       set ; remove duplicates
       sort)) ; sort 
(defn count-next-steps
  "Find all existing steps
   Input: {:step \"C\" :next-step \"A\"}
          {:step \"A\" :next-step \"B\"}
          {:step \"A\" :next-step \"D\"}
          {:step \"B\" :next-step \"E\"}
          {:step \"D\" :next-step \"E\"}
          {:step \"F\" :next-step \"E\"}
          {:step \"C\" :next-step \"F\"}
   Output: ([\"A\" 1] [\"B\" 1] [\"C\" 0] [\"D\" 1] [\"E\" 3] [\"F\" 1])"
   [required-steps requirements]
   (let [all-steps (->> required-steps
                        (map (fn [key] {key 0})) ; initialize with zero
                        (reduce conj)) ; convert into
         ]
     (->> requirements 
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

(defn assign-steps
  "assign steps to worker
   Input: ({:remained-time 1} {:remained-time 0}) ; workers
          (\"C\") ; next-steps
   Output: {:workers ({:remained-time 1} {:step \"C\" :remained-time 63})}"
  [workers next-steps]
  (let [free-workers (filter (fn [worker] (< (:remained-time worker) 1)) workers)
        busy-workers (filter (fn [worker] (pos? (:remained-time worker))) workers)]
    (concat (filter identity
                    (for [i (range (count free-workers))]
                      (cond
                        (< i (count next-steps)) {:step (nth next-steps i) :remained-time (+ 61 (- (int (char (first (nth next-steps i)))) (int \A)))}
                        :else (nth free-workers i)))) busy-workers)
    ))

(defn next-snapshot
  "Make a next snapshot
   Input: {:remained-steps (\"A\" \"B\" \"C\" \"D\" \"E\" \"F\"),
           :remained-requirements
           ({:step \"A\", :next-step \"B\"}
            {:step \"A\", :next-step \"D\"}
            {:step \"B\", :next-step \"E\"}
            {:step \"C\", :next-step \"A\"}
            {:step \"C\", :next-step \"F\"}
            {:step \"D\", :next-step \"E\"}
            {:step \"F\", :next-step \"E\"}),
           :workers ({:step \"C\", :remained-time 2} {:remained-time -1}),
           :taken-steps (),
           :working-time 1}
   Output: {:remained-steps (\"A\" \"B\" \"C\" \"D\" \"E\" \"F\"),
           :remained-requirements
           ({:step \"A\", :next-step \"B\"}
            {:step \"A\", :next-step \"D\"}
            {:step \"B\", :next-step \"E\"}
            {:step \"C\", :next-step \"A\"}
            {:step \"C\", :next-step \"F\"}
            {:step \"D\", :next-step \"E\"}
            {:step \"F\", :next-step \"E\"}),
           :workers ({:step \"C\", :remained-time 1} {:remained-time -2}),
           :taken-steps (),
           :working-time 1}"
  [snapshot]
  (let [sorted-steps (count-next-steps (:remained-steps snapshot) (:remained-requirements snapshot))
        in-progress-steps (set (map :step
                                    (filter (fn [worker-status] (pos? (:remained-time worker-status))) (:workers snapshot))))
        worker-status (assign-steps
                       (:workers snapshot)
                       (keys (filter (fn [step] (and (not (in-progress-steps (key step))) (zero? (val step)))) sorted-steps)))
        updated-worker-status (->> worker-status
                                   (map
                                    (fn [worker] (update-in worker [:remained-time] dec))))
        completed-steps (set (map :step
                                  (filter (fn [worker-status] (zero? (:remained-time worker-status))) updated-worker-status)))] 
    {:remained-steps (remove #(completed-steps %) (:remained-steps snapshot))
      :remained-requirements (remove #(completed-steps (:step %)) (:remained-requirements snapshot)) ;completed-steps를 let 에서 한번에 set으로 만들기
      :workers updated-worker-status
      :taken-steps (concat (:taken-steps snapshot) (seq completed-steps))
      :working-time (inc (:working-time snapshot))}))

(defn run-steps
  "Generate a schedule upon n workers
   Input: ({:step \"A\", :next-step \"B\"}
          {:step \"A\", :next-step \"D\"}
          {:step \"B\", :next-step \"E\"}
          {:step \"C\", :next-step \"A\"}
          {:step \"C\", :next-step \"F\"}
          {:step \"D\", :next-step \"E\"}
          {:step \"F\", :next-step \"E\"})
          workers ({:remained-time 0})}
   Output: (:schedule \\C \\A \\B \\D \\F \\E
            :working-time 21)"
  [workers requirements]
  (let [all-steps (find-all-steps requirements)]
    (drop-while
     #(some? (seq (:remained-steps %)))
     (iterate next-snapshot {:remained-steps all-steps
                              :remained-requirements requirements
                              :workers workers
                              :taken-steps ()
                              :working-time 0}))))


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
       (run-steps [{:remained-time 0}]) 
       (take 1)
       first
       :taken-steps
       str/join)
  (->> "aoc2018_7.input"
       parse-input
       (run-steps [{:remained-time 0}])
       (take 1) 
       first
       :taken-steps
       str/join)
  )

;; part 2
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
       (run-steps [{:remained-time 0} {:remained-time 0}]) 
       (take 1)
       first
       :working-time)

  (->> "aoc2018_7.input"
       parse-input
       (run-steps [{:remained-time 0} {:remained-time 0} {:remained-time 0} {:remained-time 0} {:remained-time 0}]) 
       (take 1) 
       first
       :working-time)
  )