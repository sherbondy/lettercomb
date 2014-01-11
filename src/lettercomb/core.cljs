(ns lettercomb.core)

(def canvas (.getElementById js/document "canvas"))
(def ctx (.getContext canvas "2d"))

(defn blacken [ctx]
  (set! (.-fillStyle ctx) "#000")
  (.fillRect ctx 0 0 640 960))

(blacken ctx)

(defn hex-point [[cx cy] radius i]
  "Point i of a hexagon with radius and center [cx, cy]"
  (let [angle (* (/ Math/PI 3.0) (+ i 0.5))]
    [(+ cx (* radius (Math/cos angle)))
     (+ cy (* radius (Math/sin angle)))]))

(defn hexagon [center radius]
  (for [i (range 7)]
    (hex-point center radius i)))

;; (hexagon [50 50] 25)

(defn move-to [ctx [x y]]
  (.moveTo ctx x y))

(defn line-to [ctx [x y]]
  (.lineTo ctx x y))

;; eventually make drawing in terms of protocols
;; on structures
(defn draw-hexagon [ctx center radius & [fill-color]]
  (.beginPath ctx)
  (set! (.-fillStyle ctx) (or fill-color "#000"))
  (move-to ctx (hex-point center radius 0))
  (doseq [i (range 7)]
    (line-to ctx (hex-point center radius i)))
  (.fill ctx)
  (.stroke ctx))

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

(defn rand-hex-str []
  (.toString (Math/round (* (Math/random) 15))
             16))

(defn rand-color-str []
  (str "#"
       (rand-hex-str)
       (rand-hex-str)
       (rand-hex-str)))

(defn fill-board [ctx [cols rows] radius left-top]
  "left-top = the [left top] center point."
  (doseq [i (range cols)
          j (range rows)]
    (let [center (center-at [i j] left-top radius)]
      (draw-hexagon ctx center radius (rand-color-str)))))

(set! (.-strokeStyle ctx) "#fff")
(set! (.-lineWidth ctx) 2)

(def playing? (atom true))

(defn play! []
  (reset! playing? true))

(defn pause! []
  (reset! playing? false))

(defn game-loop []
  (js/requestAnimationFrame game-loop)
  (when @playing?
    (blacken ctx)
    (fill-board ctx [10 19] 32 [60 48])))

(game-loop)

(pause!)
(play!)