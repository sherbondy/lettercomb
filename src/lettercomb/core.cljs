(ns lettercomb.core
  (:require [lettercomb.letters :as l]
            [lettercomb.grid :as g]))

;; add a device orientation listener and rotate
;; letters based on alpha

;; should be very careful to distinguish between
;; @board and board as arguments to functions...

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

(defn draw-cannon! [ctx center radius angle next-letter]
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
    (draw-letter! ctx center (name next-letter)))

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

(defn v->angle [[cx cy] [ex ey]]
  "given a center point and an event's coords,
   determine the angle between the two points"
  (Math/atan2 (- ex cx)
              (- cy ey)))

(defn v->odd-r [v]
  ((comp g/axial-to-odd-r
        (partial g/pixel-to-axial left-top radius))
    v))

;; needs better name
(defn next-coord [angle radius [x y]]
  "Given the [x y] position of a cell in odd-r,
   and an angle of movement, determine another next
   position in the same direction"
   (let [next-x (+ x (* (Math/sin angle) radius))
         next-y (- y (* (Math/cos angle) radius))]
     [next-x next-y]))

(defn out-of-bounds? [board point]
  (nil? (g/get-odd-r board point)))

(defn occupied? [board cell]
  (not= :blank (g/get-odd-r board cell)))

;; point = the origin point of the cannon
;; or the current point
(defn destination-cell [board angle radius point]
  (let [[x y :as dest-coords]   (next-coord angle radius point)
        [col row :as dest-cell] (v->odd-r dest-coords)
        current-cell            (v->odd-r point)]
    ;; if next cell is occupied or out of bounds,
    ;; destination is at the current point's cell
    (if (or (occupied? board dest-cell)
            (out-of-bounds? board dest-cell))
      (if (not (or (occupied? board current-cell)
                   (out-of-bounds? board current-cell)))
        current-cell
        nil)
      ;; otherwise, keep going down the line
      (recur board angle radius dest-coords))))

;; (next-cell 0 32 [0 250])
;; (destination-cell @board 0 radius
;;   (board-center @board left-top radius))

;; (board-center @board left-top radius)

(def canvas-offset
  [(.-offsetLeft canvas) (.-offsetTop canvas)])

(def the-center
  (board-center @board left-top radius))

;; the center of the board relative to the whole page
;; only useful in browser, not ejecta...
(def page-center
  [(+ (the-center 0) (canvas-offset 0))
   (+ (the-center 1) (canvas-offset 1))])

(defn write-letter! [a-board [col row] letter-kw]
  (swap! a-board assoc-in [row col]
         letter-kw))

;; @TODO: should do bounds checking and maybe
;; auto-wap to next row
(defn write-word! [a-board [start-col start-row] word]
  (let [up-word (.toUpperCase word)]
    (doseq [i (range (count up-word))]
      (write-letter! a-board
                     [(+ i start-col) start-row]
                     (keyword (nth up-word i))))))

(defn pick-random-letter! []
  (reset! next-letter (l/rand-letter)))

(defn handle-move [e]
  (let [v (e->v e)
        new-angle (v->angle page-center v)]
    (reset! angle new-angle)
    (let [dest (destination-cell @board new-angle radius v)]
      (reset! hovered-cell dest))))

(defn first-touch [e]
  (first (.-changedTouches e)))

(defn handle-touch-move [e]
  (let [touch (first-touch e)]
    (handle-move touch)))

(defn handle-release [e]
  (handle-move e)
  (when @hovered-cell
    (write-letter! board @hovered-cell @next-letter)
    (reset! hovered-cell nil))
  (pick-random-letter!))

(defn handle-touch-release [e]
  (let [touch (first-touch e)]
    (handle-release touch)))

(defn add-event-listeners []
  (if-not (.-ejecta js/window)
    (do
      (.addEventListener canvas "mousemove" handle-move)
      (.addEventListener canvas "mouseup" handle-release))
    (do
      (.addEventListener canvas "touchmove" handle-touch-move)
      (.addEventListener canvas "touchend" handle-touch-release))))

(defn game-loop []
  (js/requestAnimationFrame game-loop)
  (when @playing?
    (blacken! ctx)
      (fill-board! ctx @board left-top radius)
      (draw-cannon! ctx the-center radius
                    @angle @next-letter)))

(add-event-listeners)
(game-loop)

(write-word! board [0 0] "hello")
(write-word! board [1 1] "there")

;; (swap! board assoc-in [0 0] :a)
;; (swap! board assoc-in [11 6] :z)

;; (g/pixel-to-axial [100 100] left-top radius)

(pause!)
(play!)