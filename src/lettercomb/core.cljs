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

(defn draw-hexagon [ctx center radius]
  (.beginPath ctx)
  (move-to ctx (hex-point center radius 0))
  (doseq [i (range 7)]
    (line-to ctx (hex-point center radius i)))
  (.stroke ctx))

(defn width [radius]
  (* 2.0 radius (Math/cos (/ Math/PI 6.0))))

(defn fill-board [ctx rows cols radius top-left]
  "top-left = the top-left center point."
  (doseq [i (range cols)
          j (range rows)]
    (let [hex-w    (width radius)
          y-offset (* 3 0.5 radius)
          x-offset (if (odd? j)
                     (/ hex-w 2.0)
                     0)
          center   [(+ (top-left 0) (* i hex-w) x-offset)
                    (+ (top-left 1) (* j y-offset))]]
      (draw-hexagon ctx center radius))))

(set! (.-strokeStyle ctx) "#fff")
(set! (.-lineWidth ctx) 2)

(fill-board ctx 19 10 32 [60 48])