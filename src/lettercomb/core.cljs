(ns lettercomb.core
  (:require [lettercomb.letters :as l]
            [lettercomb.grid :as g]))

;; add a device orientation listener and rotate
;; letters based on alpha

(def left-top [24 40])
(def radius 24)

;; MASSACHUSETTS
(def board         (atom (g/make-rect-board 7 12)))
(def angle         (atom Math/PI))
(def hovered-cell  (atom [0 0]))
(def next-letter   (atom :A))

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

(defn draw-letter! [ctx [cx cy] letter]
  (.fillText ctx letter
             (- cx q-font-size)
             (+ cy q-font-size)))

;; eventually map colors based on frequency/point-value
(defn letter-color [letter]
  "expects a keyword"
  (l/point-colors
   (get l/letter-points letter "#000")))

(defn draw-letter-hex! [ctx center radius letter]
  (draw-hexagon! ctx center radius
                 (letter-color letter))
  (set! (.-fillStyle ctx) "#fff")
  (draw-letter! ctx center (name letter)))

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

(defn fill-board! [ctx board left-top radius]
  "left-top = the [left top] center point."
  (doseq [row (range (count board))
          col (range (count (nth board row)))]
    (let [center (center-at [col row] left-top radius)
          letter (g/get-odd-r board [col row])]
      (if (= :blank letter)
        (draw-hexagon! ctx center radius
                       (if (= @hovered-cell [col row])
                         "#fff" "#000"))
        (draw-letter-hex! ctx center radius
                          letter)))))

(defn board-center [board left-top radius]
  (let [mid-row (Math/floor (/ (count board) 2))
        mid-col (Math/floor (/ (count (board 0)) 2))]
    (center-at [mid-col mid-row] left-top radius)))

(defn draw-cannon! [ctx board left-top radius
                    angle next-letter]
  (let [center (board-center board left-top radius)]
    (.save ctx)
    (.translate ctx (center 0) (center 1))
    (.rotate ctx angle)
    (.translate ctx (* -1 (center 0)) (* -1 (center 1)))
    (draw-hexagon! ctx center radius "#fff")
    (draw-hexagon! ctx
                   [(center 0) (- (center 1) radius)]
                   radius "#fff")
    (.restore ctx)
    (set! (.-fillStyle ctx) "#000")
    (draw-letter! ctx center (name next-letter))))

(def playing? (atom true))

(defn play! []
  (reset! playing? true))

(defn pause! []
  (reset! playing? false))

(set! (.-strokeStyle ctx) "#fff")
(set! (.-lineWidth ctx) 2)
(set! (.-font ctx)
      (str "bold " font-size "px Courier"))


;; idea for intersection code:
;; shoot ray from center. Sample at regular interval
;; to ensure that we always sample every hexagon along
;; a path.
;; for each sample, determine if it is contained in
;; a hexagon. Then lookup the hexagon to see if it is
;; occupied. As we go along the path, build up
;; a list of visited hexagons. The moment we have
;; an intersection with an occupied hex, or
;; we have run out of valid hexagons, then pop
;; the last value to get the destination.
;; do not actually need a list, just need to keep
;; track of the last value...
;; pixel -> hex algo:

(defn e->v [e]
  "convert a js Event object to a location vector"
  [(.-pageX e) (.-pageY e)])

(defn ev-angle [[cx cy] [ex ey]]
  "given a center point and an event's coords,
   determine the angle between the two points"
  (Math/atan2 (- ex cx)
              (- cy ey)))

(defn v->angle [v]
  (let [center (board-center @board left-top radius)
        true-c [(+ (center 0) (.-offsetLeft canvas))
                (+ (center 1) (.-offsetTop canvas))]]
    (ev-angle center v)))

(defn v->odd-r [v]
  ((comp g/axial-to-odd-r
        (partial g/pixel-to-axial left-top radius))
    v))

(defn v->ray [v]
  v)

(defn handle-move [e]
  (let [v (e->v e)]
    (reset! angle (v->angle v))
    (reset! hovered-cell (v->odd-r v))))

(.addEventListener canvas "mousemove" handle-move)

(defn game-loop []
  (js/requestAnimationFrame game-loop)
  (when @playing?
    (blacken! ctx)
      (fill-board! ctx @board left-top radius)
      (draw-cannon! ctx @board left-top radius
                    @angle @next-letter)))

(game-loop)

;; @TODO: should do bounds checking and maybe
;; auto-wap to next row
(defn write-word! [board [start-col start-row] word]
  (let [up-word (.toUpperCase word)]
    (doseq [i (range (count up-word))]
      (swap! board assoc-in
             [start-row (+ i start-col)]
             (keyword (nth up-word i))))))

(write-word! board [0 0] "hello")
(write-word! board [1 1] "there")

;; (swap! board assoc-in [0 0] :a)
;; (swap! board assoc-in [11 6] :z)

;; (g/pixel-to-axial [100 100] left-top radius)

(pause!)
(play!)