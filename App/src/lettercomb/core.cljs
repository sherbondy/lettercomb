(ns lettercomb.core
  (:require [lettercomb.letters :as l]
            [lettercomb.grid :as g]
            [clojure.string :as str]))

(when (.-ejecta js/window)
  (.include js/ejecta "scrabble-words.js"))


;; add a device orientation listener and rotate
;; letters based on alpha

;; should be very careful to distinguish between
;; @board and board as arguments to functions...

(def left-top [24 80])
(def radius 24)

(def board               (atom (g/make-rect-board 7 12)))
(def angle               (atom Math/PI))
;; the currently hovered cell
(def hovered-cell        (atom [0 0]))
;; the cell in the cannon's trajectory
(def open-cell           (atom [0 0]))
(def next-letter         (atom :A))
;; maintain a vector of the currently hovered word cells
(def current-word-cells  (atom []))
;; should probably maintain a set of word cells
(def touch-down?         (atom false))
(def score               (atom 0))
(def start-time          (atom nil)) ;; in miliseconds

(def canvas (.getElementById js/document "canvas"))
(def ctx (.getContext canvas "2d"))

(def word-set (set js/WORDS))
(.log js/console (contains? word-set "hello"))

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

(def score-location [8 36])
(defn draw-score! [ctx score]
  (.save ctx)
  (set! (.-fillStyle ctx) "#fff")
  (.fillText ctx (str score " pts")
             (score-location 0)
             (score-location 1))
  (.restore ctx))

(defn pad [val padding size]
  (let [str-val  (str val)
        len      (count str-val)
        pad-size (max 0 (- size len))]
    (apply str (conj (vec (repeat pad-size padding))
                     str-val))))

(def timer-location [(- (.-width canvas) 8) 36])
;; (defn timer-redness [ms-left game-duration-ms]
;;   (.toString
;;    (Math/floor (* 16 (/ ms-left game-duration-ms)))
;;    16))

(defn draw-timer! [ctx seconds-left]
  (.save ctx)
  ;; color can be based on time left
  (set! (.-fillStyle ctx) "#fff")
  (set! (.-textAlign ctx) "end")
  (let [mins (Math/floor (/ seconds-left 60))
        secs (mod seconds-left 60)]
    (.fillText ctx (str (pad mins "0" 2)
                        ":"
                        (pad secs "0" 2))
               (timer-location 0)
               (timer-location 1)))
  (.restore ctx))

(def menu-size [96 32])
(def menu-position [(- (/ (.-width canvas) 2)
                       (/ (menu-size 0) 2))
                    (- (.-height canvas)
                       (menu-size 1)
                       16)])
(defn draw-menu! [ctx]
  (.save ctx)
  (set! (.-fillStyle ctx) "#000")
  (.beginPath ctx)
  (.rect ctx
         (menu-position 0) (menu-position 1)
         (menu-size 0) (menu-size 1))
  (set! (.-lineWidth ctx) 2)
  (set! (.-strokeStyle ctx) "#fff")
  (.fill ctx)
  (.stroke ctx)
  (set! (.-fillStyle ctx) "#fff")
  (set! (.-textAlign ctx) "middle")
  (.fillText ctx "MENU"
             (+ (menu-position 0) 28)
             (+ (menu-position 1) 20))
  (.restore ctx))

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

(defn draw-letter-hex! [ctx center radius letter color]
  (draw-hexagon! ctx center radius color)
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
                       (if (= @open-cell [col row])
                         "#fff" "#000"))
        (let [color (if (contains? (set @current-word-cells) [col row])
                      (l/darken (letter-color letter))
                      (letter-color letter))]
          (draw-letter-hex! ctx center radius
                            letter color))))))

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
  [(- (.-pageX e) (.-offsetLeft canvas))
   (- (.-pageY e) (.-offsetTop canvas))])

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

(defn clear-cell! [a-board cell]
  (write-letter! a-board cell :blank))

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

(defn hover-cell! [e]
  (let [v     (e->v e)
        coord (v->odd-r v)]
    (reset! hovered-cell coord)))

(defn handle-move [e]
  (let [v         (e->v e)
        new-angle (v->angle page-center v)]
    (reset! angle new-angle)
    (let [dest (destination-cell @board new-angle radius v)]
      (reset! open-cell dest)))

  (when @touch-down?
    (hover-cell! e)

    ;;continue building up the hovered word
    (if (occupied? @board @hovered-cell)
      (when (and
             (not (empty? @current-word-cells))
             (not (contains? (set @current-word-cells)
                             @hovered-cell)))
        (swap! current-word-cells conj @hovered-cell))
      ;; if hovered cell is unoccupied , reset current word
      (reset! current-word-cells []))))

(defn first-touch [e]
  (first (.-changedTouches e)))

(defn handle-touch-move [e]
  (let [touch (first-touch e)]
    (handle-move touch)))

(defn selected-word [board word-cells]
  (str/lower-case
   (apply str
          (for [cell word-cells]
            (name (g/get-odd-r board cell))))))

(defn clear-selected-word! [a-board word-cells]
  (doseq [cell word-cells]
    (clear-cell! a-board cell)))

(defn increase-score! [added-score]
  (swap! score + added-score))

(defn handle-release [e]
  (reset! touch-down? false)
  (handle-move e)
  (when (and @open-cell
             (empty? @current-word-cells))
    (write-letter! board @open-cell @next-letter)
    (reset! open-cell nil)
    (pick-random-letter!))

  (let [hovered-word (selected-word @board
                                    @current-word-cells)]
    (when (contains? word-set hovered-word)
      (.log js/console (str hovered-word " is a real word..."))
      (clear-selected-word! board @current-word-cells)
      (reset! current-word-cells [])
      (increase-score! (l/word-score hovered-word)))
  )
  (reset! hovered-cell nil))

(defn handle-touch-release [e]
  (let [touch (first-touch e)]
    (handle-release touch)))

(defn handle-start [e]
  (reset! touch-down? true)
  (hover-cell! e)
  (when (occupied? @board @hovered-cell)
    (reset! current-word-cells [@hovered-cell])))

(defn handle-touch-start [e]
  (let [touch (first-touch e)]
    (handle-start touch)))

;; should make events file

(add-watch current-word-cells :current-word
           (fn [k r o n]
             (.log js/console
                   (selected-word @board
                                  n))))

(defn add-event-listeners []
  (if-not (.-ejecta js/window)
    (do
      (.addEventListener canvas "mousemove" handle-move)
      (.addEventListener canvas "mouseup" handle-release)
      (.addEventListener canvas "mousedown" handle-start))
    (do
      (.addEventListener canvas "touchmove" handle-touch-move)
      (.addEventListener canvas "touchend" handle-touch-release)
      (.addEventListener canvas "touchstart" handle-touch-start))
      ))

;; 5 minutes, dog
(def game-duration-ms (* 5 60 1000))

;;; timer is currently in absolute time
(defn game-loop []
  (js/requestAnimationFrame game-loop)
  (when @playing?
    (blacken! ctx)
    (fill-board! ctx @board left-top radius)
    (draw-cannon! ctx the-center radius
                  @angle @next-letter)
    (draw-score! ctx @score)
    (let [time-left-ms (+ game-duration-ms
                          (- @start-time
                             (.getTime (js/Date.))))
          seconds-left (Math/floor (/ time-left-ms 1000))]
      (draw-timer! ctx seconds-left))
    (draw-menu! ctx)))

(defn init! []
  (reset! start-time (.getTime (js/Date.)))
  (write-word! board [0 0] "hello")
  (write-word! board [1 1] "there")

  (add-event-listeners)
  (game-loop))

;; (swap! board assoc-in [0 0] :a)
;; (swap! board assoc-in [11 6] :z)

;; (g/pixel-to-axial [100 100] left-top radius)

(init!)
(pause!)
(play!)
