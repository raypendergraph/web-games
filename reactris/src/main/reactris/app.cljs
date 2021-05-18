(ns reactris.app
  (:require
    [reagent.core :as r]
    [reagent.dom :as rdom]
    [big-bang.core :refer [big-bang!]]
    [big-bang.events.browser :refer [which]]
    [clojure.core.async :as async :refer [put! take! go-loop go chan]])
  (:require-macros
    [cljs.core.async.macros :refer [go]]))

(defrecord Board [width height grid])
(defrecord Cartesian [x y])
(defrecord Rotation [shape profile])
(defrecord Piece [key size rotations])
(defrecord WorldState
  [; The board
   board
   ; A Cartesian representing the upper-left point of the piece.
   position
   ; All augmented Pieces based on the config shapes.
   pieces
   ; The current Piece
   piece-idx
   ; The rotation index (0-4)
   rotation-idx
   ; The left, down and right hit profile for this piece in this rotation.
   profile
   ; The color of the current Piece.
   color])

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

(def keydown-ch (chan))
(def tick-ch (chan))
(def state (r/atom nil))


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

(defn piece [profile {x :x y :y}]
  [:g {:id "piece"}
   (for [[y row] (map-indexed vector profile)
         [x fill?] (map-indexed vector row)
         :when (true? fill?)
         :let [_ (println x y fill?)]]
     (cell x y :green))])

(defn cell-group [grid]
  [:g
   (for [[y row] (map-indexed vector grid)
         [x color] (map-indexed vector row)
         :when ((complement nil?) color)]
     (cell x y color))])

(defn board [state]
  (let [{:keys [board position pieces piece-idx rotation-idx]} state]
    [:svg {:viewBox "0 0 20 40" :preservexmlns= "http://www.w3.org/2000/svg"}
     [:defs
      [:pattern {:id "grid" :width "1" :height "1" :patternUnits "userSpaceOnUse"}
       [:path {:d "M 10 0 L 0 0 0 10" :fill "none" :stroke "gray" :stroke-width "0.1"}]]]
     ;; draw the grid and occupied cells
     (cell-group (get-in state [:board :grid]))
     ;; draw the piece
     (println "board" position)
     (piece (get-in pieces [piece-idx :rotations rotation-idx :shape])
       position)
     [:rect {:width "100%" :height "100%" :fill "url(#grid)"}]]))

;;(defn init []
;;  (go
;;    (let [state (create-state config)
;;          test-state (create-test-state state)]
;;      (big-bang!
;;        :initial-state test-state
;;        :on-tick       update-state
;;        :on-keydown    handle-key-down
;;        :to-draw       draw!))))
;;

;;;;;;;;;;;;;;;;;;;
;; Helper functions
;;;;;;;;;;;;;;;;;;;
(defn serial-assoc
  "Associates values into `associative` (presumably a vector) in order starting at
  `start-index` and extending the vector if required. Also works with maps, but why
  would you want to honeslty?"
  [associative start-index new-values]
  (let [indexes (range start-index
                  (+ start-index
                    (count new-values)))]
    (apply assoc associative (interleave indexes new-values))))

(defn first-when
  "Returns a tuple of [index value] for the first position matching `predicate` for
  any `coll` where seqable? is true, else returns the value passed in. If the there are
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


(defn cw-rotate-matrix [matrix-like]
  "Rotates a matrix-like value such that the result is the same matrix except 0,0 is now w, 0 and w, 0 is now at
  w, h.... etc.. Returns vector of rows (which is a vector of columns)"
  (apply mapv (fn [& colls]
                (apply vector (reverse colls)))
    matrix-like))

(defn movement-profile [rotations current]
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

(defn create-hit-profile
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
              (let [hit-profile (create-hit-profile rotated-shape)]
                (Rotation. rotated-shape hit-profile)))
         (take 4 (iterate cw-rotate-matrix shape)))))

(defn internalize-tetromino-config
  "Takes a Tetromino configuration and uses nil for blanks and true for the shape body as this is a more advantageous
  scenario for Clojure."
  [tet-config]
  (vec (for [row tet-config]
         (map
           (fn [column]
             (if (= column 0) nil true))
           row))))

(defn create-pieces
  "Creates precomputed Pieces from the configuration passed in."
  [pieces-config]
  (vec (map (fn [[key tet-config]]
              (let [shape (internalize-tetromino-config tet-config)
                    size (count tet-config)
                    rotations (create-rotations shape)]
                (Piece. key size rotations)))
         (seq pieces-config))))

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
        pieces (create-pieces (:pieces config))
        grid (create-grid board-width
               board-height
               nil
               (:size (apply max-key :size pieces)))
        piece-idx (rand-int (count pieces))
        current-position (Cartesian. (rand-int (- board-width
                                                 (:size (nth pieces piece-idx))))
                           0)
        board (Board. board-width board-height grid)

        rotation-idx 0
        profile (movement-profile (:rotations (nth pieces piece-idx)) rotation-idx)
        color nil]
    (WorldState. board current-position pieces piece-idx rotation-idx profile color)))



(defn overflow?
  [state direction]
  (let [{:keys [board position profile
                pieces piece-idx]} state
        {:keys [width height]} board
        {size :size} (nth pieces piece-idx)
        {:keys [x y]} position
        {:keys [left down right]} profile
        not-nil? (complement nil?)]
    (case direction
      :left (> 0
              (+ (dec x)
                (apply min
                  (filter not-nil?
                    left))))
      :down (< height
              (+ (inc y)
                (- size
                  (apply min
                    (filter not-nil?
                      down)))))
      :right (< width
               (+ (inc x)
                 (- size
                   (apply min
                     (filter not-nil?
                       right)))))
      false)))

(defn move-piece [state direction]
  (do (println "move" direction (overflow? state direction))
      (case direction
        :right (if (overflow? state :right)
                 state
                 (update-in state [:position :x] inc))
        :left (if (overflow? state :left)
                state
                (update-in state [:position :x] dec))
        state)))

(defn directional-hit?
  [state direction]
  (let [grid (get-in state [:board :grid])
        position (:position state)
        piece (nth (:pieces state)
                (:piece-idx state))
        profile (get (:profile state) direction)
        ;; zip the column offsets with the x coordinate
        piece-size (:size piece)
        ;; the y index of the bottom of the piece in terms of the grid
        bottom-idx (dec (+ (:y position) (:size piece)))
        ;; zip of the  [x coordinate , coordinate of the profile in terms of the grid].
        column-defs (map vector (range (:x position) piece-size)
                      (map (fn [y-offset]
                             (- bottom-idx y-offset))
                        profile))
        not-nil? (complement nil?)]
    ;; If any column in front of an occupied block is present then a hit will result.
    (not-nil? (some not-nil?
                (flatten (for [[x y-edge] column-defs]
                           (nth-column-vector x grid y-edge (inc y-edge))))))))


(defn emplace-piece
  "Stores a piece on the board, in its final resting place."
  [state]
  (let [grid (get-in state [:board :grid])
        piece (nth (:pieces state) (:piece-idx state))
        rotation (nth (:rotations piece) (:rotation-idx state))
        position (:position state)
        color (:color state)
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
                   (serial-assoc row (:x position) cells))
        ;; Put the rows into the grid
        new-grid (serial-assoc grid (:y position) new-rows)]
    (assoc-in state [:board :grid] new-grid)))


;;;;;;;;;;;;;;;;;;;;;;
;; Big Bang callbacks
;;;;;;;;;;;;;;;;;;;;;;

(defn update-state [event state]
  state)

(defn handle-key-down [event state]
  (if-let [direction (get directions (which event))]
    (move-piece state direction)
    state))

(defn draw! [state]
  (do (println "draw!" (:position state))
      (let [root-element (.getElementById js/document "game")]
        (rdom/render (board state)
          root-element))))

(defn formatted-json [state]
  (fn []
    [:pre (.stringify js/JSON (clj->js state) nil 3)]))

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
  (do
    (.addEventListener js/document
      "keydown"
      (fn [e]
        (let [direction (get directions (.-which e))]
          (when ((complement nil?) direction)
            (go (>! keydown-ch
                    direction))))))

    (js/setInterval (fn []
                        (go (>! tick-ch true))) 2000)

    ;; Map tick events into the mandatory down movement.
    (go-loop []
      (<! tick-ch)
      (println "Tick!")
      (recur))
         ;; (swap! state (move-piece @state :down))

    ;; Convert key down event changes into potentially new state and push
    ;; that into the state atom.
    (go-loop []
             (let [direction (<! keydown-ch)]
               ;;(swap! state (move-piece @state direction))
               (println direction))
             (recur))))

;    (let [state (create-state config)
;          test-state (create-test-state state)))
;      (swap! state test-state))))
