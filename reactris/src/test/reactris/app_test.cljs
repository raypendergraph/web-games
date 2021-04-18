(ns reactris.app-test
  (:require [cljs.test :refer (deftest is )]
            [reactris.app :as app]))

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
                [0 0 0 0]]
            :TEST [[1 1 1]
                   [1 1 1]
                   [1 1 1]]}})
(def world-state (app/create-world-state config))

(defn  find-piece [key world-state]
  (first 
    (filter (complement nil?)
            (map-indexed (fn [i piece]
                           (if (= key 
                                  (:key piece)) 
                             i 
                             nil))
                         (:pieces world-state)))))
(deftest nth-column-vector__works
  (let [m           [[1 0 0 0]
                     [0 1 0 0]
                     [0 0 1 0]
                     [0 0 0 1]]
        range-m     (range (count m))
        columns     (for [ci range-m] 
                      (app/nth-column-vector ci m))
        equals-one? (partial = 1)
        ones        (for [c columns] 
                      (first (app/first-when equals-one? c)))]
    (is (= ones range-m))))


(deftest overflow__left
  (let [test-piece-idx (find-piece :TEST world-state)          
        test-state     (assoc world-state 
                              :position (app/Cartesian. 0 0)
                              :piece-idx test-piece-idx)]
    (do
      (is (false? (app/overflow? test-state :down)))
      (is (false? (app/overflow? test-state :right)))
      (is (true?  (app/overflow? test-state :left))))))


(deftest overflow__right
  (let [test-piece-idx (find-piece :TEST world-state)          
        piece-size     (:size (nth (:pieces world-state) 
                                   test-piece-idx))
        board-width    (get-in world-state [:board :width])
        test-state     (assoc world-state 
                              :position (app/Cartesian. (- board-width piece-size) 0)
                              :piece-idx test-piece-idx)]
    (do
      (is (false? (app/overflow? test-state :down)))
      (is (true?  (app/overflow? test-state :right)))
      (is (false? (app/overflow? test-state :left))))))

(deftest overflow__down
  (let [test-piece-idx (find-piece :TEST world-state)          
        piece-size     (:size (nth (:pieces world-state) 
                                   test-piece-idx))
        board-height   (get-in world-state [:board :height])
        test-state     (assoc world-state 
                              :position (app/Cartesian. piece-size (- board-height piece-size))
                              :piece-idx test-piece-idx)]
    (do
      (println (dissoc test-state :board))
      (is (true?  (app/overflow? test-state :down)))
      (is (false? (app/overflow? test-state :right)))
      (is (false? (app/overflow? test-state :left))))))
