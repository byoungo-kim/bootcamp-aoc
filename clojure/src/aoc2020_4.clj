(ns aoc2020_4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.spec.alpha :as spec]))

(defn read-input
  "parse file input into events
   Input: file path
   Output: (\"ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\" \"byr:1937 iyr:2017 cid:147 hgt:183cm\" \"\n\")"
  [path]
  (->> path io/resource slurp clojure.string/split-lines))

(spec/def :passport/byr string?)
(spec/def :passport/ecl string?)
(spec/def :passport/eyr string?)
(spec/def :passport/hcl string?)
(spec/def :passport/hgt string?)
(spec/def :passport/iyr string?)
(spec/def :passport/pid string?)
(spec/def :passport/cid string?)
(spec/def :passport/option-cid
  (spec/keys :req [:passport/byr
                   :passport/ecl
                   :passport/eyr
                   :passport/hcl
                   :passport/hgt
                   :passport/iyr
                   :passport/pid]
             :opt [:passport/cid]))

(defn parse-passports
  "Group input strings for each passport
   Input: (\"ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\" \"byr:1937 iyr:2017 cid:147 hgt:183cm\" \"\n\")
   Output: (\"ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm\")"
  [input-strings]
  (->> input-strings
       (partition-by #(empty? %))
       (map #(str/join " " %)) ;merge multiple lines into single string
       (filter not-empty)
       (map #(re-seq #"(\S+):(\S+)" %)) ; use re-seq to remove unnecessary split 
       (map (fn [passport]
              (into {} ; merge maps using into
                    (map
                     (fn [[_ key val]] {(keyword "passport" key) val})
                     passport)))))
  )

(defn filter-invalid-passports
  "Filter invalid passport strings"
  [passports]
  (filter (fn [passport]
            (spec/valid? :passport/option-cid passport)) passports)
  )

;; part 1
(comment 
  (spec/valid? :passport/option-cid {:passport/byr "" :passport/cid "" :passport/ecl "" 
                                     :passport/eyr "" :passport/hcl "" :passport/hgt "" 
                                     :passport/iyr "" :passport/pid ""})
  (->> ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
        "byr:1937 iyr:2017 cid:147 hgt:183cm"
        ""
        "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
        "hcl:#cfa07d byr:1929"
        ""
        "hcl:#ae17e1 iyr:2013"
        "eyr:2024"
        "ecl:brn pid:760753108 byr:1931"
        "hgt:179cm"
        ""
        "hcl:#cfa07d eyr:2025 pid:166559648"
        "iyr:2011 ecl:brn hgt:59in"]
       parse-passports
       filter-invalid-passports
       count)
  (->> "aoc2020_4.input"
       read-input
       parse-passports
       filter-invalid-passports
       count)
  )

;; part 2

;; byr (Birth Year) - four digits; at least 1920 and at most 2002.
;; iyr (Issue Year) - four digits; at least 2010 and at most 2020.
;; eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
;; hgt (Height) - a number followed by either cm or in:
;; If cm, the number must be at least 150 and at most 193.
;; If in, the number must be at least 59 and at most 76.
;; hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
;; ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
;; pid (Passport ID) - a nine-digit number, including leading zeroes.
;; cid (Country ID) - ignored, missing or not.

(defn check-height
  "Check the validity of a height
   Input: \"153in\"
   Output: false"
  [height]
  (let [[_ height-string unit] (re-find #"(\d+)(\S+)" height)
        height-value (Integer/parseInt height-string)]
    (case unit
      "in" (< 58 height-value 77)
      "cm" (< 149 height-value 194)
      false)
    ))

;; int-in
(spec/def :tight-passport/byr (spec/and string? #(spec/int-in-range? 1920 2003 (Integer/parseInt %))))
(spec/def :tight-passport/ecl (spec/and string? #(#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} %)))
(spec/def :tight-passport/eyr (spec/and string? #(spec/int-in-range? 2020 2031 (Integer/parseInt %)))) 
(spec/def :tight-passport/hcl (spec/and string? #(re-matches #"#[a-f0-9]{6}" %)))
(spec/def :tight-passport/hgt (spec/and string? check-height))
(spec/def :tight-passport/iyr (spec/and string? #(spec/int-in-range? 2010 2021 (Integer/parseInt %))))
(spec/def :tight-passport/pid (spec/and string? #(re-matches #"[0-9]{9}" %)))
(spec/def :tight-passport/cid string?)
(spec/def :tight-passport/option-cid
  (spec/keys :req [:tight-passport/byr
                   :tight-passport/ecl
                   :tight-passport/eyr
                   :tight-passport/hcl
                   :tight-passport/hgt
                   :tight-passport/iyr
                   :tight-passport/pid]
             :opt [:tight-passport/cid]))

(defn parse-tight-passports
  "Group input strings for each passport
   Input: (\"ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm\" \"\n\")
   Output: (\"ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm\")"
  [input-strings]
  (->> input-strings
       (partition-by #(empty? %))
       (map #(str/join " " %)) ;merge multiple lines into single string
       (filter not-empty)
       (map #(re-seq #"(\S+):(\S+)" %)) ; use re-seq to remove unnecessary split 
       (map (fn [passport] ;; map->reduce->map refactoring
              (into {} ;merge, merge-with does not work for multi-level key
               (map
                (fn [[_ key val]]
                  {(keyword "tight-passport" key) val})
                passport))))))

(comment
  ;; = keyword - Example 1 = 

  ;; (keyword name): name can be string, symbol, or keyword.
  ;; 
  ;; (keyword ns name): ns and name must both be string.
  ;; 
  ;; A keyword string, like a symbol, begins with a non-numeric
  ;; character and can contain alphanumeric characters and *, +, !, -,
  ;; _, and ?.  (see http://clojure.org/reader for details).
  ;; 
  ;; keyword does not validate input strings for ns and name, and may
  ;; return improper keywords with undefined behavior for non-conformant
  ;; ns and name.

  user=> (keyword 'foo)
  :foo

  user=> (keyword "foo")
  :foo

  user=> (keyword "user" "foo")
  :user/foo

  ;; keyword in current namespace
  user=> (keyword (str *ns*) "foo")
  :user/foo
  ;; See also:
  name
  keyword?
  namespace
  find-keyword
  symbol
  )

(defn filter-invalid-tight-passports
  "Filter invalid passport strings"
  [passports]
  (filter (fn [passport]
            (spec/valid? :tight-passport/option-cid passport)) passports))

(comment
  (check-height "183cm")
  (spec/explain :tight-passport/option-cid {:tight-passport/byr "1937" :tight-passport/cid "147" :tight-passport/ecl "gry"
                                     :tight-passport/eyr "2020" :tight-passport/hcl "#fffffd" :tight-passport/hgt "193cm"
                                     :tight-passport/iyr "2017" :tight-passport/pid "060033327"})
  (->> ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
        "byr:1937 iyr:2017 cid:147 hgt:183cm"
        ""
        "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
        "hcl:#cfa07d byr:1929"
        ""
        "hcl:#ae17e1 iyr:2013"
        "eyr:2024"
        "ecl:brn pid:760753108 byr:1931"
        "hgt:179cm"
        ""
        "hcl:#cfa07d eyr:2025 pid:166559648"
        "iyr:2011 ecl:brn hgt:59in"]
       parse-tight-passports
       (filter #(spec/valid? :tight-passport/option-cid %))
       count)
  (->> "aoc2020_4.input"
       read-input
       parse-tight-passports 
       (filter #(spec/valid? :tight-passport/option-cid %))
       count) 
  )

;; refactoring: parsing -> map-> passport -> count