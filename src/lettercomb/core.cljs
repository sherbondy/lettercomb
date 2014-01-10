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
    [(+ cx (* radius (js/Math.cos angle)))
     (+ cy (* radius (js/Math.sin angle)))]))

(defn hexagon [center radius]
  (for [i (range 7)]
    (hex-point center radius i)))

;; (hexagon [50 50] 25)

(defn move-to [ctx [x y]]
  (.moveTo ctx x y))

(defn line-to [ctx [x y]]
  (.lineTo ctx x y))

(defn draw-hexagon [ctx center radius]
  (.beginPath ctx)
  (move-to ctx (hex-point center radius 0))
  (doseq [i (range 7)]
    (line-to ctx (hex-point center radius i)))
  (.stroke ctx))

(set! (.-strokeStyle ctx) "#fff")
(set! (.-lineWidth ctx) 2)
(draw-hexagon ctx [32 32] 32)