(ns lettercomb.core)

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
   (+ 97
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

(def font-size 24)
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

(defn fill-board! [ctx [cols rows] radius left-top]
  "left-top = the [left top] center point."
  (doseq [i (range cols)
          j (range rows)]
    (let [center (center-at [i j] left-top radius)]
      (draw-letter-hex! ctx center radius
                        (rand-letter)))))

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
    (fill-board! ctx [10 19] 32 [60 48])))

(game-loop)

(pause!)
(play!)