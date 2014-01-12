(ns lettercomb.core)

;; add a device orientation listenr and rotate
;; letters based on alpha

(def canvas (.getElementById js/document "canvas"))
(def ctx (.getContext canvas "2d"))

(defn blacken! [ctx]
  (set! (.-fillStyle ctx) "#000")
  (.fillRect ctx 0 0 640 960))

;; (blacken! ctx)

(defn rand-hex-str []
  (.toString (Math/round (* (Math/random) 15))
             16))

(defn rand-color-str []
  (str "#"
       (rand-hex-str)
       (rand-hex-str)
       (rand-hex-str)))

;; this should really be at the frequency of
;; letter appearances in english words
(defn rand-letter []
  "ascii lower case starts at 97"
  (String/fromCharCode
   (+ 65
      (Math/floor
       (* (Math/random) 26)))))

(defn hex-point [[cx cy] radius i]
  "Point i of a hexagon with radius and center [cx, cy]"
  (let [angle (* (/ Math/PI 3.0) (+ i 0.5))]
    [(+ cx (* radius (Math/cos angle)))
     (+ cy (* radius (Math/sin angle)))]))

(defn hexagon [center radius]
  (for [i (range 7)]
    (hex-point center radius i)))

;; (hexagon [50 50] 25)

(defn move-to! [ctx [x y]]
  (.moveTo ctx x y))

(defn line-to! [ctx [x y]]
  (.lineTo ctx x y))

;; eventually make drawing in terms of protocols
;; on structures
(defn draw-hexagon! [ctx center radius & [fill-color]]
  (.beginPath ctx)
  (set! (.-fillStyle ctx) (or fill-color "#000"))
  (move-to! ctx (hex-point center radius 0))
  (doseq [i (range 7)]
    (line-to! ctx (hex-point center radius i)))
  (.fill ctx)
  (.stroke ctx))

(def font-size 16)
(def q-font-size (/ font-size 4))

;; eventually map colors based on frequency/point-value
(defn letter-color [letter]
  (rand-color-str))

(defn draw-letter-hex! [ctx center radius letter]
  (draw-hexagon! ctx center radius
                 (letter-color letter))
  (set! (.-fillStyle ctx) "#fff")
  (.fillText ctx letter
             (- (center 0) q-font-size)
             (+ (center 1) q-font-size)))

(defn width [radius]
  (* 2.0 radius (Math/cos (/ Math/PI 6.0))))

(defn center-at [[col row] [left top] radius]
  (let [hex-w    (width radius)
        y-offset (* 3 0.5 radius)
        x-offset (if (odd? row)
                   (/ hex-w 2.0)
                   0)]
    [(+ left (* col hex-w) x-offset)
     (+ top  (* row y-offset))]))

;; define a board in terms of its top-left hexagon center
;; and the constituent columns and rows going down.
;; could compactly represent as a bit vector
;; max dims = 7 x 12 = 84 entries, 84 bits

;; grid terminology from
;; http://www.redblobgames.com/grids/hexagons/#conversions

;; probably want to represent as UInt8Array
(defn make-rect-board [cols rows]
  "boards are stored in odd-r offset coords"
  (vec (for [j (range rows)]
         (vec (for [i (range cols)]
                :blank)))))

(defn get-odd-r [board [col row]]
  "get hex at index col, row using odd-r offset coords"
  (get-in board [row col]))

(defn get-cube [board [x z]]
  "get hex at index x, z using cube coords"
  (let [q (+ x (/ (- z (bit-and z 1)) 2))
        r z]
    (get-in board [z q])))

(def board (atom (make-rect-board 7 12)))

(defn fill-board! [ctx board radius left-top]
  "left-top = the [left top] center point."
  (doseq [row (range (count board))
          col (range (count (nth board row)))]
    (let [center (center-at [col row] left-top radius)
          letter (get-odd-r board [col row])]
      (if (= :blank letter)
        (draw-hexagon! ctx center radius)
        (draw-letter-hex! ctx center radius
                          (name letter))))))

(def playing? (atom true))

(defn play! []
  (reset! playing? true))

(defn pause! []
  (reset! playing? false))

(set! (.-strokeStyle ctx) "#fff")
(set! (.-lineWidth ctx) 2)
(set! (.-font ctx)
      (str "bold " font-size "px Courier"))

(defn game-loop []
  (js/requestAnimationFrame game-loop)
  (when @playing?
    (blacken! ctx)
    (fill-board! ctx @board 24 [24 40])))

(game-loop)

(defn write-word! [board [start-col start-row] word]
  (let [up-word (.toUpperCase word)]
    (doseq [i (range (count up-word))]
      (swap! board assoc-in
             [start-row (+ i start-col)]
             (nth up-word i)))))

(write-word! board [0 1] "letter")
(write-word! board [1 2] "comb")

;; (swap! board assoc-in [0 0] :a)
;; (swap! board assoc-in [11 6] :z)

(pause!)
(play!)