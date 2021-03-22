(ns reactris.app
  (:require
    [reagent.core :as r]))

(def config {:board {:width 20
                     :height 40}
             :pieces {:L {:shape [[1 0 0]
                                  [1 1 1]
                                  [0 0 0]]}
                      :O {:shape [[1 1]
                                  [1 1]]}
                      :I {:shape [[0 0 0 0]
                                  [0 0 0 0]
                                  [1 1 1 1]
                                  [0 0 0 0]]}}})
(def piece-atom (atom nil))
(def position-atom (atom nil))
(def board-atom (atom nil))

(defn largest-size []
  (let [pieces (:pieces config)]
    (apply max (map (fn [[k v]]
                      (count (:shape v))) pieces))))

(defn new-random-piece []
  (let [key (rand-nth (keys (:pieces config)))
        pieces (:pieces config)]
    (vector key (key pieces))))


(defn create-grid [w h value]
  (let [augmented-h (+ h (largest-size))]
    (apply vector (take augmented-h
                  (repeat (apply vector (take w (repeat value))))))))


(defn rotate-shape [shape]
  (apply mapv (fn [& colls]
                (vector key (apply vector (reverse colls)))
                shape)))

(defn swap-rotate! [times]
  (fn [[key piece]]
    (let [ f  (fn [next _] ((rotate-shape next)))
           new-shape (reduce f (:shape piece) (range times))
           new-piece  (assoc piece :shape new-shape)]
      (vector key new-piece))))

(defn  max-x-index []
  (let [[_ {shape :shape}] @piece-atom
        [shape-row] shape
        [board-row] @board-atom]
    (- (count board-row) (count shape-row))
    ))

(defn init []
  (swap! board-atom (fn [_] (create-grid
                             (get-in config [:board :width])
                             (get-in config [:board :height])
                             0)))
  (swap! piece-atom (fn [_] (new-random-piece)))
  (swap! position-atom (fn [_] (rand-int (max-x-index))))
  )