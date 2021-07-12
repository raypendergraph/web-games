(ns reactris.app
  (:require
    [clojure.spec.alpha :as s]
    [reagent.core :as r]
    [reagent.dom :as rdom]
    [big-bang.core :refer [big-bang!]]
    [big-bang.events.browser :refer [which]]
    [clojure.core.async :as async :refer [put! take! go-loop chan]])
  (:require-macros
    [cljs.core.async.macros :refer [go]]))

(defrecord Board [width height grid])
(defrecord Cartesian [x y])
(defrecord Player [position piece-idx rotation-idx profiles color])
(defrecord Rotation [shape profile])
(defrecord Piece [key size rotations])
(defrecord WorldState [board player])

(def directions
  {37 :left
   39 :right
   40 :down
   32 :down
   38 :rotate})

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

;;;;;;;;;;;;;;;;;;;
;; helper functions
;;;;;;;;;;;;;;;;;;;
(defn serial-assoc
      "associates values into `associative` (presumably a vector) in order starting at
      `start-index` and extending the vector if required. also works with maps, but why
      would you want to honeslty?"
      [associative start-index new-values]
      (let [indexes (range start-index
                           (+ start-index
                              (count new-values)))]
           (apply assoc associative (interleave indexes new-values))))

(defn first-when
      "returns a tuple of [index value] for the first position matching `predicate` for
      any `coll` where seqable? is true, else returns the value passed in. if the there are
      no matches for the predicate in `coll` then nil is returned."
      [predicate coll]
      (if (seqable? coll)
        (first (filter some?
                       (map-indexed (fn [i v]
                                        (when (predicate v) [i v]))
                                    coll)))
        coll))

(defn nth-column-vector
      "Plucks the nth column from a matrix like structure and returns it as a column vector.
      Additionally, passing `row-start` and `row-end` (exclusive) pulls a window."
      ;;returns all rows
      ([n matrix-vec]
       (nth-column-vector n matrix-vec 0 (count matrix-vec)))
      ;; returns a window of rows.
      ([n matrix-vec row-start row-end]
       (vec
         (for [row (subvec matrix-vec row-start row-end)]
              (nth row n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creation and config functions 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cw-rotate-matrix [matrix-like]
      "Rotates a matrix-like value such that the result is the same matrix except 0,0 is now w, 0 and w, 0 is now at
      w, h.... etc.. Returns vector of rows (which is a vector of columns)"
      (apply mapv (fn [& colls]
                      (apply vector (reverse colls)))
             matrix-like))

(defn internalize-tetromino-config
      "Takes a Tetromino configuration and uses nil for blanks and true for the shape body as this is a more advantageous
      scenario for Clojure."
      [tet-config]
      (vec (for [row tet-config]
                (map
                  (fn [column]
                      (if (= column 0) nil true))
                  row))))

(defn create-collision-profile
      "Creates hit profile of the body of the shape from the bottom of the sandbox.
      A number will represent the offset of the body of the tetromino for this rotation.
      A nil represents there is no possible collision on this column."
      [shape]
      (let [; Since these are all square we just count the rows.
            column-count (count shape)
            ; The shape's columns as reversed lists because we care about the bottom.
            columns (for [n (range column-count)]
                         (rseq (nth-column-vector n shape)))
            ; A lazy list of the index of items that are not nil. We only care about the first one.
            indices (for [c columns]
                         (map-indexed (fn [i item]
                                          (if (nil? item) nil i))
                                      c))]
           (for [i indices]
                (first (filter (complement nil?) i)))))

(defn create-rotations
      "Reads a Tetromino configuration and provides Rotation snapshots of the four rotations: 0 (default) to 3."
      [shape]
      (vec (map (fn [rotated-shape]
                    (let [hit-profile (create-collision-profile rotated-shape)]
                         (Rotation. rotated-shape hit-profile)))
                (take 4 (iterate cw-rotate-matrix shape)))))

(defn create-pieces
      "Creates precomputed Pieces from the configuration passed in."
      [pieces-config]
      (vec (map (fn [[key tet-config]]
                    (let [shape (internalize-tetromino-config tet-config)
                          size (count tet-config)
                          rotations (create-rotations shape)]
                         (Piece. key size rotations)))
                (seq pieces-config))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initial defines and channels
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def pieces (create-pieces (:pieces config)))
(def keydown-ch (chan))
(def tick-ch (chan))
(def state (r/atom nil))
(def grid-cursor (r/cursor state [:board :grid]))
(def player-cursor (r/cursor state [:player]))

;;;;;;;;;;;;;;;;;;;;;;;
;; View 
;;;;;;;;;;;;;;;;;;;;;;;
(defn cell [x y color]
      [:rect {:key    (str x ":" y)
              :x      x
              :y      y
              :width  "1"
              :height "1"
              :class  (str (name color) "cell")}])

(defn player []
      (let [{:keys [color piece-idx rotation-idx position]} @player-cursor
            shape (get-in pieces [piece-idx :rotations rotation-idx :shape])]
           [:g {:id "piece"}
            (for [[y row] (map-indexed vector shape)
                  [x fill?] (map-indexed vector row)
                  :when (true? fill?)]
                 (cell (+ x (:x position)) (+ y (:y position)) color))]))

(defn grid []
      [:g
       (for [[y row] (map-indexed vector @grid-cursor)
             [x color] (map-indexed vector row)
             :when ((complement nil?) color)]
            (cell x y color))])

(defn game []
      [:svg {:viewBox "0 0 20 40" :preservexmlns "http://www.w3.org/2000/svg"}
       [:defs
        [:pattern {:id "grid" :width "1" :height "1" :patternUnits "userSpaceOnUse"}
         [:path {:d "M 10 0 L 0 0 0 10" :fill "none" :stroke "gray" :stroke-width "0.1"}]]]
       [:rect {:width "100%" :height "100%" :fill "url(#grid)"}]
       [grid]
       [player]])



(defn movement-profile
      "A movement profile consists of hit-profiles for the left, right and down orientations for a given piece."
      [rotations current]
      {:down  (:profile (nth rotations
                             current))
       :left  (:profile (nth rotations
                             (mod (- current 1) 4)))
       :right (:profile (nth rotations
                             (mod (+ current 1) 4)))})

(defn calculate-board-extents
      "Calculates the maximum extents of the piece's sandbox origin based on the current rotation
      and the hit profile of the right or left side. This can be precalculated per shape rotation."
      [board piece rotation-idx]
      (let [rotations (:rotations piece)
            profile (movement-profile rotations rotation-idx)]
           {:left  (- 0 (apply min (:left profile)))
            :right (+ (- (:width board)
                         (:size piece))
                      (apply min (:right profile)))
            :down  (+ (apply min (:down profile))
                      (- (:height board)
                         (:size piece)))}))

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

(defn create-state [config]
      (let [{{board-width  :width
              board-height :height} :board} config
            grid (create-grid board-width
                              board-height
                              nil
                              (:size (apply max-key :size pieces)))
            piece-idx (rand-int (count pieces))
            position (Cartesian. (rand-int (- board-width
                                              (:size (nth pieces piece-idx))))
                                 0)
            board (Board. board-width board-height grid)
            rotation-idx 0
            profiles (movement-profile (:rotations (nth pieces piece-idx)) rotation-idx)
            ;; TODO color
            player (Player. position piece-idx rotation-idx profiles :blue)]
           (WorldState. board player)))

(defn overflow?  [state pieces direction]
      (let [{:keys [player board]}                             state
            {:keys [position piece-idx rotation-idx profiles]} player
            {:keys [width height]}                             board
            {size :size}                                       (nth pieces piece-idx)
            {x :x y :y}                                        position
            {:keys [left down right]}                          (:profiles player)
            not-nil?                                           (complement nil?)]
           (case direction
                 :left (> 0
                          (+ (dec x)
                             (apply min (filter not-nil? left))))
                 :down (< height
                          (+ (inc y)
                             (- size
                                (apply min (filter not-nil? down)))))
                 :right (< width
                           (+ (inc x)
                              (- size
                                 (apply min (filter not-nil? right)))))
                 false)))

(defn rotate-piece [state]
      (update-in state
                 [:player :rotation-idx]
                 (fn [n] (mod (inc n) 4))))


(defn directional-hit?
      [grid player direction pieces]
      (let [{:keys [position piece-idx profiles]} player
            piece                                 (nth pieces piece-idx)
            profile                               (get profiles direction)
            piece-size                            (:size piece)
            ;; the y index of the bottom of the piece in terms of the grid
            bottom-idx                            (dec (+ (:y position) (:size piece)))
            ;; zip of the  [x coordinate , coordinate of the profile in terms of the grid].
            column-defs                           (mapv vector (range (:x position) piece-size)
                                                       (map (fn [y-offset]
                                                                (- bottom-idx y-offset))
                                                            profile))
            not-nil?                              (complement nil?)
            _ (println column-defs)]

           ;; If any column in front of an occupied block is present then a hit will result.

           (not-nil? (some not-nil?
                           (flatten (for [[x y-edge] column-defs]
                                         (nth-column-vector x grid y-edge (inc y-edge))))))))

(defn update-grid
      "Stores a piece on the board, in its final resting place."
      [grid pieces player]
      (let [{:keys [color piece-idx rotation-idx position]} player
            piece (nth pieces piece-idx)
            rotation (nth (:rotations piece) rotation-idx)
            not-nil? (complement nil?)
            ;; Create cells to be rendered from shape of the current rotation.
            new-cells (for [row (:shape rotation)]
                           (for [cell row]
                                (when (not-nil? cell) color)))
            ;; Extract the rows that the shape will occupy depending on the size.
            shape-rows (subvec grid (:y position) (+ (:y position)
                                                     (:size piece)))
            ;; Put the new cells into those rows.
            new-rows (for [[cells row] (map vector new-cells shape-rows)]
                          (serial-assoc row (:x position) cells))]
           ;; Put the rows into the grid
           (serial-assoc grid (:y position) new-rows)))


(defn move-piece
      "Takes care of all concerns when a piece is moved either by the timer or by the user."
      [state direction]
      (println )
      (if (overflow? state pieces direction)
        state
        (let [grid (get-in [:board grid] state)
              is-hit? (directional-hit? (get-in state [:board :grid]) (:player state) direction pieces)]
             (if (and is-hit? (= direction :down))
               (update-grid grid pieces player)
               (case direction
                     :right (update-in state [:player :position :x] inc)
                     :left  (update-in state [:player :position :x] dec)
                     :down  (update-in state [:player :position :y] inc)
                     :rotate (rotate-piece state)
                     state)))))

(defn create-test-state [state]
      (let [{{grid :grid} :board} state
            new-grid (for [[r row] (map-indexed vector grid)]
                          (if (> r 5)
                            (let [i (rand-int (count row))
                                  color (rand-nth [:red :green :blue])]
                                 (assoc row i :red))
                            row))]
           (assoc-in state [:board :grid] new-grid)))

(defn init []
      (reset! state (create-test-state (create-state config)))
      (do
        (.addEventListener js/document
                           "keydown"
                           (fn [e]
                               (let [direction (get directions (.-which e))
                                     _ (println "Key -> " direction)]
                                    (when ((complement nil?) direction)
                                          (go (>! keydown-ch
                                                  direction))))))

        (js/setInterval (fn []
                            (go (>! tick-ch true))) 2000)

        ;; Map tick events into the mandatory down movement.
        (go-loop []
                 (<! tick-ch)
                 (println "Tick!")
                 (swap! state (fn [s] (move-piece s :down)))
                 (recur))

        ;; Convert key down event changes into potentially new state and push
        ;; that into the state atom.
        (go-loop []
                 (let [direction (<! keydown-ch)]
                      (r/rswap! state (fn [state]
                                          (println direction)
                                          (move-piece state direction)))
                      (recur)))

        (rdom/render [game] (.getElementById js/document "game"))))
