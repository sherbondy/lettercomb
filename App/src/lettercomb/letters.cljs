(ns lettercomb.letters
  (:require [clojure.string :as str]))

;; HELPER FUNCTIONS

;; should eventually store the dictionary in a clever
;; data structure like a ternary search tree.

;; should make a binary "closest" search...

(defn binary-search*
  "Finds earliest occurrence of x in xs (a vector) using binary search.
  If x is between two values, return the larger index."
  [xs x]
  (loop [low 0 high (dec (count xs))]
    (if (<= high (inc low))
      (cond
        (== x (xs low)) low
        (== x (xs high)) high
        (and (< x (xs high))
             (> x (xs low))) high
        (<  x (xs low)) low
        (>  x (xs high)) high
        :else nil)
      (let [mid (+ low (bit-shift-right (- high low) 1))]
        (if (< (xs mid) x)
          (recur (inc mid) high)
          (recur low mid))))))

;; (binary-search* [1 2 3 4 5 6 7 8] 1)

;; based on
;; http://en.wikipedia.org/wiki/Letter_frequency#Relative_frequencies_of_letters_in_the_English_language
(def letter-freqs
  (sorted-map
   :A 8.167
   :B 1.492
   :C	2.782
   :D	4.253
   :E	12.702
   :F	2.228
   :G	2.015
   :H	6.094
   :I	6.966
   :J	0.153
   :K	0.772
   :L	4.025
   :M	2.406
   :N	6.749
   :O	7.507
   :P	1.929
   :Q	0.095
   :R	5.987
   :S	6.327
   :T	9.056
   :U	2.758
   :V	0.978
   :W	2.360
   :X	0.150
   :Y	1.974
   :Z	0.075))

(def item-freqs
  (sorted-map
   :bomb 100))

(def tile-freqs
  (sorted-map
   :letter 50
   :item 50))

(defn letter-at-index [i]
  (keyword (String/fromCharCode
            (+ 65 i))))

;; should really normalize this so that things don't have to be 1-100
(defn make-cumulative-freqs [freq-map i result-vec]
  "make a vector of cumulative frequencies for freq-map"
    (if (< i (count freq-map))
      (let [next-freq (nth (vals freq-map) i)
            total (+ (last result-vec) next-freq)]
        (make-cumulative-freqs freq-map
                               (inc i)
                               (conj result-vec total)))
      result-vec))

;;     (if (< i (count letter-freqs))
;;       (let [next-freq (letter-freqs (letter-at-index i))
;;             total     (+ (last result-vec) next-freq)]
;;         (make-cumulative-freqs letter-freqs
;;                                (inc i)
;;                                (conj result-vec total)))
;;       result-vec)))

(def cumulative-tile-freqs
  (make-cumulative-freqs tile-freqs 0 []))

(def cumulative-letter-freqs
  (make-cumulative-freqs letter-freqs 0 []))

(def point-letters
  {1 #{:E :A :I :O :N :R :T :L :S :U}
   2 #{:D :G}
   3 #{:B :C :M :P}
   4 #{:F :H :V :W :Y}
   5 #{:K}
   8 #{:J :X}
  10 #{:Q :Z}})

(def letter-points
  (apply merge
   (apply concat
     (for [[point letters] point-letters]
      (for [letter letters]
        {letter point})))))

(defn word-score [word-str]
  (reduce +
          (for [letter word-str]
            (let [kw (keyword (str/upper-case letter))]
              (letter-points kw)))))

(def point-colors
  {1  "#a00"
   2  "#a60"
   3  "#aa0"
   4  "#0a0"
   5  "#00a"
   8  "#60a"
   10 "#a0a"})

(defn darken [color]
  "fake color darkening"
  (->
   color
   (str/replace "6" "3")
   (str/replace "a" "7")))

(defn rand-100 []
  "return random number 0 to 100"
  (Math/floor
    (* (Math/random) 100)))

;; this should really be at the frequency of
;; letter appearances in english words
(defn rand-letter []
  "ascii lower case starts at 97"
  (letter-at-index
    (binary-search*
      cumulative-letter-freqs
     (rand-100))))

(defn rand-tile []
  (let [n (rand-100)
        tile-i (binary-search* cumulative-tile-freqs n)
        tile-type (nth (keys tile-freqs) tile-i)]
    tile-type))

;; (rand-tile)
