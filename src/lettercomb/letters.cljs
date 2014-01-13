(ns lettercomb.letters)

;; based on
;; http://en.wikipedia.org/wiki/Letter_frequency#Relative_frequencies_of_letters_in_the_English_language
(def letter-freqs
  {:A 8.167
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
   :Z	0.074})

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

(def point-colors
  {1  "#a00"
   2  "#a60"
   3  "#aa0"
   4  "#0a0"
   5  "#00a"
   8  "#60a"
   10 "#a0a"})

;; this should really be at the frequency of
;; letter appearances in english words
(defn rand-letter []
  "ascii lower case starts at 97"
  (keyword
   (String/fromCharCode
     (+ 65
        (Math/floor
         (* (Math/random) 26))))))