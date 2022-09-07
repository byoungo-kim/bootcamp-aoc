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
       (map #(str/split % #" ")) 
       (map (fn [passport]
              (reduce conj
                      (map
                       (fn [passport-item]
                         (cond
                           (str/starts-with? passport-item "byr")
                           {:passport/byr (let [[_ val] (re-find #"byr:(\S+)" passport-item)] val)}
                           (str/starts-with? passport-item "cid")
                           {:passport/cid (let [[_ val] (re-find #"cid:(\S+)" passport-item)] val)}
                           (str/starts-with? passport-item "ecl")
                           {:passport/ecl (let [[_ val] (re-find #"ecl:(\S+)" passport-item)] val)}
                           (str/starts-with? passport-item "eyr")
                           {:passport/eyr (let [[_ val] (re-find #"eyr:(\S+)" passport-item)] val)}
                           (str/starts-with? passport-item "hcl")
                           {:passport/hcl (let [[_ val] (re-find #"hcl:(\S+)" passport-item)] val)}
                           (str/starts-with? passport-item "hgt")
                           {:passport/hgt (let [[_ val] (re-find #"hgt:(\S+)" passport-item)] val)}
                           (str/starts-with? passport-item "iyr")
                           {:passport/iyr (let [[_ val] (re-find #"iyr:(\S+)" passport-item)] val)}
                           (str/starts-with? passport-item "pid")
                           {:passport/pid (let [[_ val] (re-find #"pid:(\S+)" passport-item)] val)})) passport)))))
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
  (let [[height-string] (re-find #"(\d+)" height)
        height-value (Integer/parseInt height-string)]
    (or (and (str/ends-with? height "in")
             (and (> height-value 58) (< height-value 77)))
        (and (str/ends-with? height "cm")
             (and (> height-value 149) (< height-value 194))))
    ))

(defn check-year-range
  "Check whether a given year value is in a given range
   Input: 2010 2010 2020
   Output: true"
  [year min-year max-year]
  (let [year-value (Integer/parseInt year)](and (>= year-value min-year) (<= year-value max-year))))

(def color-regex #"#[a-f0-9]{6}")
(spec/def :tight-passport/byr (spec/and string? #(check-year-range % 1920 2002)))
(spec/def :tight-passport/ecl (spec/and string? #(contains? (frequencies '("amb" "blu" "brn" "gry" "grn" "hzl" "oth")) %)))
(spec/def :tight-passport/eyr (spec/and string? #(check-year-range % 2020 2030)))
(spec/def :tight-passport/hcl (spec/and string? #(re-matches color-regex %)))
(spec/def :tight-passport/hgt (spec/and string? check-height))
(spec/def :tight-passport/iyr (spec/and string? #(check-year-range % 2010 2020)))
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
   Input: (\"ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\" \"byr:1937 iyr:2017 cid:147 hgt:183cm\" \"\n\")
   Output: (\"ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm\")"
  [input-strings]
  (->> input-strings
       (partition-by #(empty? %))
       (map #(str/join " " %)) ;merge multiple lines into single string
       (filter not-empty)
       (map #(str/split % #" "))
       (map (fn [passport]
              (reduce conj
                      (map
                       (fn [passport-item]
                         (cond
                           (str/starts-with? passport-item "byr")
                           {:tight-passport/byr (let [[_ val] (re-find #"byr:(\S+)" passport-item)] val)}
                           (str/starts-with? passport-item "cid")
                           {:tight-passport/cid (let [[_ val] (re-find #"cid:(\S+)" passport-item)] val)}
                           (str/starts-with? passport-item "ecl")
                           {:tight-passport/ecl (let [[_ val] (re-find #"ecl:(\S+)" passport-item)] val)}
                           (str/starts-with? passport-item "eyr")
                           {:tight-passport/eyr (let [[_ val] (re-find #"eyr:(\S+)" passport-item)] val)}
                           (str/starts-with? passport-item "hcl")
                           {:tight-passport/hcl (let [[_ val] (re-find #"hcl:(\S+)" passport-item)] val)}
                           (str/starts-with? passport-item "hgt")
                           {:tight-passport/hgt (let [[_ val] (re-find #"hgt:(\S+)" passport-item)] val)}
                           (str/starts-with? passport-item "iyr")
                           {:tight-passport/iyr (let [[_ val] (re-find #"iyr:(\S+)" passport-item)] val)}
                           (str/starts-with? passport-item "pid")
                           {:tight-passport/pid (let [[_ val] (re-find #"pid:(\S+)" passport-item)] val)})) passport))))))

(defn filter-invalid-tight-passports
  "Filter invalid passport strings"
  [passports]
  (filter (fn [passport]
            (spec/valid? :tight-passport/option-cid passport)) passports))

(comment
  (check-height "183cm")
  (spec/explain :tight-passport/option-cid {:tight-passport/byr "1937" :tight-passport/cid "147" :tight-passport/ecl "gry"
                                     :tight-passport/eyr "2020" :tight-passport/hcl "#fffffd" :tight-passport/hgt "194cm"
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
       filter-invalid-tight-passports
       count)
  (->> "aoc2020_4.input"
       read-input
       parse-tight-passports
       filter-invalid-tight-passports
       count))