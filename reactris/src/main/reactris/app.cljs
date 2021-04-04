(ns reactris.app
  (:require
  ;;  [reagent.core :as r]
   [reagent.dom :as rdom]
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
    ; All augmented Pieces based on the config shapes.
            pieces
    ; The current Piece
            piece-idx
    ; The rotation index (0-4)
            rotation-idx
    ; The left, down and right hit profile for this piece in this rotation.
            profile
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
   (for [row matrix-like]
     (nth row n))))

(defn cw-rotate-matrix [matrix-like]
  "Rotates a matrix-like value such that the result is the same matrix except 0,0 is now w, 0 and w, 0 is now at
   w, h.... etc.. Returns vector of rows (which is a vector of columns)"
  (apply mapv (fn [& colls] 
                (apply vector (reverse colls)))
              matrix-like))

(defn movement-profile [rotations current]
  {:down (:profile (nth rotations
                        current))
   :left (:profile (nth rotations
                        (mod (- current 1) 4)))
   :right (:profile (nth rotations
                         (mod (+ current 1) 4)))})

(defn calculate-board-extents
  "Calculates the maximum extents of the piece's sandbox origin based on the current rotation
  and the hit profile of the right or left side. This can be precalculated per shape rotation."
  [board piece rotation-idx]
  (let [rotations (:rotations piece)
        profile         (movement-profile rotations rotation-idx)
        piece-size      (:size piece)
        board-width     (:width board)
        board-height    (:height board)
        right-adjust    (apply min (:right profile))
        left-adjust     (apply min (:left profile))
        vertical-adjust (apply min (:down profile))]
    {:left (- 0 left-adjust)
     :right (+ (- board-width
                  piece-size)
               right-adjust)
     :down (+ vertical-adjust
              (- board-height piece-size))}))

(defn create-hit-profile
  "Creates hit profile of the body of the shape from the bottom of the sandbox.
   A number will represent the offset of the body of the tetromino for this rotation.
   A nil represents there is no possible collision on this column."
  [shape]
  (let [; Since these are all square we just count the rows.
        column-count   (count shape)
         ; The shape's columns as reversed lists because we care about the bottom.
        columns        (for [n (range column-count)]
                         (rseq (nth-column-vector n shape)))
         ; A lazy list of the index of items that are not nil. We only care about the first one.
        indices        (for [c columns]
                         (map-indexed (fn [i item]
                                        (if (nil? item) nil i))
                                      c))]
    (for [i indices]
      (first (filter (complement nil?) i)))))

(defn create-rotations
  "Reads a Tetromino configuration and provides Rotation snapshots of the four rotations: 0 (default) to 3."
  [shape]
  (map (fn [rotated-shape]
         (let [hit-profile (create-hit-profile rotated-shape)]
           (Rotation. rotated-shape hit-profile)))
       (take 4 (iterate cw-rotate-matrix shape))))

(defn internalize-tetromino-config
  "Takes a Tetromino configuration and uses nil for blanks and true for the shape body as this is a more advantageous
  scenario for Clojure."
  [tet-config]
  (for [row tet-config]
    (map
     (fn [column]
       (if (= column 0) nil true))
     row)))

(defn create-pieces
  "Creates precomputed Pieces from the configuration passed in."
  [pieces-config]
  (map
   (fn [[key tet-config]]
     (let [shape     (internalize-tetromino-config tet-config)
           size      (count tet-config)
           rotations (create-rotations shape)]
       (Piece. key size rotations)))
   (seq pieces-config)))

(defn create-grid
  "Creates a `h` rows by `w` (a [][]) items grid of `empty-value` for storing gameplay state."
  [w h empty-value v-padding]
  (let [augmented-h (+ h v-padding)]
    (apply vector
           (take augmented-h
                 (repeat
                  (apply vector
                         (take w
                               (repeat empty-value))))))))

(defn create-world-state [config]
  (let [{{board-width :width
          board-height :height} :board} config
        pieces                           (create-pieces (:pieces config))
        grid                             (create-grid board-width
                                                      board-height
                                                      nil
                                                      (:size (apply max-key :size pieces)))
        piece-idx                        (rand-int (count pieces))
        initial-position                 (Cartesian. (rand-int (- board-width
                                                                  (:size (nth pieces piece-idx))))
                                                     0)
        board                            (Board. board-width board-height grid)

        rotation-idx                     0
        profile                          (movement-profile (:rotations (nth pieces piece-idx)) rotation-idx)
        color                            nil]
    (WorldState. board initial-position pieces piece-idx rotation-idx profile color)))

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
    (if (>= (:max-x world-state) x) nil nil)))

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

(defn formatted-json [world-state]
  (fn []
    [:pre (.stringify js/JSON (clj->js world-state) nil 4)]))

(defn init []
  (let [world-state (create-world-state config)]
    (rdom/render
     [formatted-json world-state]
     (.getElementById js/document "root"))))

;; (defn init []
;;   (go
;;    (let [world-state (create-world-state config)]
;;      (do (println "Hello" world-state)
;;      (big-bang!
;;       :initial-state (create-world-state config)
;;       :on-tick       update-state
;;       :on-keydown    handle-key-down
;;       :to-draw       draw!)))))