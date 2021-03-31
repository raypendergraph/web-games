(ns reactris.app
  (:require
    [reagent.core :as r]
    [big-bang.core :refer [big-bang!]]
    [big-bang.events.browser :refer [which]])
  (:require-macros
    [cljs.core.async.macros :refer [go]]))

(defrecord Board [width height grid])
(defrecord Cartesian [x y])
(defrecord Rotation [shape profile])
(defrecord Piece [key size rotations])
(defrecord WorldState
  [; The grid
    grid
    ; A Cartesian representing the upper-left point of the piece.
    current-location
    ; Upper horizontal position for the grid for this Piece.
    max-x
    ; All augmented Pieces based on the config shapes.
    pieces
    ; The current Piece
    current-piece
    ; The rotation index (0-4)
    rotation
    ; The color of the current Piece.
    current-color])

(def directions
  {37 :left
   39 :right
   40 :drop
   32 :rotate})

(def config
  {:board  {:width  20
            :height 40}
   :pieces {:L [[1 0 0]
                [1 1 1]
                [0 0 0]]
            :O [[1 1]
                [1 1]]
            :I [[0 0 0 0]
                [0 0 0 0]
                [1 1 1 1]
                [0 0 0 0]]}})

(defn nth-column-vector [n matrix-like]
  "Plucks the nth column from a matrix like structure and returns it as a column vector."
  (vec
   (for [row shape]
     (nth row n))))

(defn cw-rotate-matrix [matrix-like]
  "Rotates a matrix-like value such that the result is the same matrix except 0,0 is now w, 0 and w, 0 is now at
   w, h.... etc.. Returns vector of rows (which is a vector of columns)"
  (apply mapv
         (fn [& colls] (apply vector (reverse colls)))
         matrix-like))

;; TODO doesn't take the profile into account.
(defn max-x-index
  "Calculates the maximum origin of the piece's sandbox based on the current rotation
  and the hit profile of the right or left side."
  [board piece current-rotation]
  (let [])
  (- (:width board)
     (:size piece)))

(defn create-hit-profile
  "Creates hit profile of the body of the shape from the bottom of the sandbox.
   A number will represent the offset of the body of the tetromino for this rotation.
   A nil represents there is no possible collision on this column."
  [shape]
  (let [; Since these are all square we just count the rows.
         column-count   (count shape)
         ; The shape's columns as lists
         columns        (for [n (range column-count)]
                          (rseq (nth-column-vector n shape)))
         ; A lazy list of the index of items that are not nil. We only care about the first one.
         indices        (for [c columns]
                          (map-indexed
                           (fn [i item]
                             (if (nil? item) nil i))
                           c))]
    (for [i indices]
      (first (filter (complement nil?) i)))))

(defn create-rotations
  "Reads a Tetromino configuration and provides Rotation snapshots of the four rotations: 0 (default) to 3."
  [tet-config]
  (map
   (fn [rotated-config]
     (let [hit-profile (create-hit-profile rotated-config)]
       (Rotation. rotated-config hit-profile)))
   (take 4 (iterate cw-rotate-matrix tet-config))))

(defn internalize-tetromino-config
  "Takes a Tetromino configuration and uses nil for blanks and true for the shape body as this is a more advantageous
  scenario for Clojure."
  [tet-config]
  (for [row tet-config]
    (map
     (fn [column] (if (= column 0) nil true))
     row)))

(defn create-pieces
  "Creates precomputed Pieces from the configuration passed in."
  [pieces-config]
  (map
   (fn [key tet-config]
     (let [shape     (internalize-tetromino-config tet-config)
           size      (count tet-config)
           rotations (create-rotations tet-config)]
       (Piece. key size rotations)))
   (seq pieces-config)))

(defn create-grid
  "Creates a `h` rows by `w` (a [][]) items grid of `empty-value` for storing gameplay state."
  [w h empty-value v-padding]
  (let [augmented-h (+ h v-padding)]
    (apply vector
           (take augmented-h
                 (repeat (apply vector (take w (repeat empty-value))))))))


(defn create-world-state [config]
  (let [{{width :width height :height} :board} config
        pieces                                 (create-pieces (:pieces config))
        grid                                   (create-grid width height nil (apply max-key :size pieces))
        board                                  (Board. width height grid)
        curent-piece                           (rand-nth pieces)
        max-x                                  (max-x-index board current-piece)
        color                                  nil]
    (WorldState. board (rand-int (max-x-index board piece)) max-x pieces current-piece color)))

(defn update-state [event world-state])

(defn move-piece [world-state direction])

(defn board-size [world-state]
  (let [board (world-state :board)]))

(defn move-right [world-state]
  (let [board  (:board world-state)
        height (:height board)
        width  (:width board)
        [x y]  (:position world-state)
        shape  (get-in world-state [:piece :shape])]
    (if (>= (:max-x world-state) x))))

(defn handle-key-down [event world-state]
  (if-let [direction (directions (which event))]
    (case direction
          :drop  (assoc world-state :direction direction)
          :left  (assoc world-state :direction direction)
          :right (assoc world-state :direction direction)
          world-state)
    world-state))

(defn draw! [world-state]
  (println "Render frame"))

(defn init []
  (go
   (let []
     (big-bang!
      :initial-state (create-world-state config)
      :on-tick       update-state
      :on-keydown    handle-key-down
      :to-draw       draw!))))