(ns lettercomb.grid
  (:require-macros [cljs.core.match.macros :refer [match]])
  (:require [cljs.core.match]))

;; non-mutating game board/grid code goes here
;; defines the storage format and means of converting
;; between different representations

;; define a board in terms of its top-left hexagon center
;; and the constituent columns and rows going down.
;; could compactly represent as a bit vector
;; max dims = 7 x 12 = 84 entries, 84 bits

;; grid terminology from
;; http://www.redblobgames.com/grids/hexagons/#conversions

;; we store in *odd-r* representation
;; and use *axial/cube* conversions for convenience
;; in certain algorithms

;; probably want to represent as UInt8Array
;; @TODO: maybe we should redefine the board to be col, row
;; instead of row, col

;; maybe I should use logic programming
;; so I don't have to define inverse operations...

;; if we keep the grid as a vector, we gain the
;; advantage of structural sharing for the history
;; of a board, so replays become trivial!

(defn make-rect-board [cols rows]
  "boards are stored in odd-r offset coords"
  (vec (for [j (range rows)]
         (vec (for [i (range cols)]
                :blank)))))

(defn get-odd-r [board [col row]]
  "get hex at index col, row using odd-r offset coords"
  (get-in board [row col]))

(defn odd-r-to-cube [[q r]]
  "q = x (col), r = y (row)"
  (let [x (- q (/ (- r (bit-and r 1)) 2))
        z r
        y (- 0 x z)]
    [x y z]))

(defn axial-to-odd-r [[x z]]
  (let [q (+ x (/ (- z (bit-and z 1)) 2))
        r z]
    [q r]))

(defn cube-to-odd-r [[x y z]]
  (axial-to-odd-r [x z]))

;; (cube-to-odd-r (odd-r-to-cube [3 3]))
;; (get-in @board [1 3])
;; (odd-r-to-cube 3 3)

(defn axial-to-cube [[q r]]
  "[q r] -> [x y z]"
  [q (- 0 q r) r])

(defn cube-to-axial [[x y z]]
  "[x y z] -> [q r]"
  [x z])

;;(axial-to-cube 1 1)

(defn get-cube [board xz]
  "get hex at index x, z using cube coords"
  (let [[q r] (cube-to-odd-r xz)]
    (get-in board [r q])))

;; (pr-str (get-cube @board [0 0]))

;; q = (1/3*sqrt(3) * x - 1/3 * y) / size
;; = (x*sqrt(3) - y)/(3*size)
;; r = 2/3 * y / size
(defn pixel-to-axial [[left top] radius [x y]]
  "get the cube coordinate which contains a
   given pixel location [x y], provided the
   center point of the [left top] hexagon and
   the radius of each hexagon."
  (let [x (- x left)
        y (- y top)
        q (/ (- (* (Math/sqrt 3) x) y)
             (* 3 radius))
        r (/ (* 2 y)
             (* 3 radius))]
    [(Math/round q)
     (Math/round r)]))

;; need to truncate if cell is outside bounds
