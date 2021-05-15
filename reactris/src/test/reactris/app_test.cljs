(ns reactris.app-test
  (:require [cljs.test :refer (deftest is )]
            [reactris.app :as app]))

(def config
  {:board  {:width  20
            :height 40}
   :pieces {:L     [[1 0 0]
                    [1 1 1]
                    [0 0 0]]
            :O     [[1 1]
                    [1 1]]
            :I     [[0 0 0 0]
                    [0 0 0 0]
                    [1 1 1 1]
                    [0 0 0 0]]
            :TESTB [[0 0 0 0]
                    [0 0 1 1]
                    [0 1 0 0]
                    [1 0 0 0]]
            :TEST  [[1 1 1]
                    [1 1 1]
                    [1 1 1]]}})
(def world-state (app/create-world-state config))

(defn  index-of-piece [key world-state]
  (first 
    (filter (complement nil?)
            (map-indexed (fn [i piece]
                           (if (= key 
                                  (:key piece)) 
                             i 
                             nil))
                         (:pieces world-state)))))

(deftest nth-column-vector__default-works
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

(deftest nth-column-vector__window-works
  (let [m      [[1 2 3 4]
                [5 6 7 8]
                [8 7 6 5]
                [4 3 2 1]]]
    (is (= 
          (app/nth-column-vector 0 m 1 4 )
          [5 8 4]))
    (is (=
         (app/nth-column-vector 3 m 2 4)
         [5 1]))))

(deftest overflow__left
  (let [test-piece-idx (index-of-piece :TEST world-state)          
        test-state     (assoc world-state 
                              :position (app/Cartesian. 0 0)
                              :piece-idx test-piece-idx)]
    (do
      (is (false? (app/overflow? test-state :down)))
      (is (false? (app/overflow? test-state :right)))
      (is (true?  (app/overflow? test-state :left))))))


(deftest overflow__right
  (let [test-piece-idx (index-of-piece :TEST world-state)          
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
  (let [test-piece-idx (index-of-piece :TEST world-state)          
        piece-size     (:size (nth (:pieces world-state) 
                                   test-piece-idx))
        board-height   (get-in world-state [:board :height])
        test-state     (assoc world-state 
                              :position (app/Cartesian. piece-size (- board-height piece-size))
                              :piece-idx test-piece-idx)]
    (do
      (is (true?  (app/overflow? test-state :down)))
      (is (false? (app/overflow? test-state :right)))
      (is (false? (app/overflow? test-state :left))))))

(deftest create-hit-profile__works 
  (let [piece    (nth (:pieces world-state)
                      (index-of-piece :TESTB world-state))
        profiles  (for [rotation (:rotations piece)]
                    (:profile rotation))]
    (do
      (is (= '(0 1 2 2) 
             (nth profiles 0)))
      (is (= '(3 2 0 nil) 
             (nth profiles 1)))
      (is (= '(1 1 2 3) 
             (nth profiles 2)))
      (is (= '(nil 2 1 0) 
             (nth profiles 3))))))

(deftest emplace-piece__works
  (let [test-piece-idx (index-of-piece :TESTB world-state)          
        piece-size     (:size (nth (:pieces world-state) 
                                   test-piece-idx))
        board-width    (get-in world-state [:board :width])
        test-state     (assoc world-state 
                              :position (app/Cartesian. (- board-width piece-size) 0)
                              :color :red
                              :piece-idx test-piece-idx)]
    (app/emplace-piece test-state)))

(deftest serial-assoc__works 
  (do
    (is (= [1 2 "three" "four" 5] 
           (app/serial-assoc [1 2 3 4 5] 2 ["three" "four"]))))
  (is (= [1 2 "three" "four" "five" "six" "seven"] 
         (app/serial-assoc [1 2 3 4 5] 2 ["three" "four" "five" "six" "seven"])))
  (is (= ["one" "two" "three"] 
         (app/serial-assoc [1 2 3] 0 ["one" "two" "three"] ))))

(deftest directional-hit?__down-miss
  (let [test-piece-idx (index-of-piece :TESTB world-state)          
        pieces         (:pieces world-state)
        piece-size     (:size (nth pieces test-piece-idx))
        board-height   (get-in world-state [:board :height])
        test-state     (assoc world-state 
                              :position  (app/Cartesian. 0 (- board-height piece-size))
                              :profile   (app/movement-profile (:rotations (nth pieces test-piece-idx))
                                                               0)
                              :color     :red
                              :piece-idx test-piece-idx)]

    (is (false? (app/directional-hit? test-state :down)))))

(deftest directional-hit?__down-hit
  (let [test-piece-idx (index-of-piece :TEST world-state)          
        pieces         (:pieces world-state)
        piece-size     (:size (nth pieces test-piece-idx))
        board-height   (get-in world-state [:board :height])
        test-state     (app/emplace-piece (assoc world-state :position  (app/Cartesian. 0 (- board-height piece-size))
                                                             :profile   (app/movement-profile (:rotations (nth pieces test-piece-idx))
                                                                                        0)
                                                             :color     :red
                                                             :piece-idx test-piece-idx))]
    (is (true? (app/directional-hit? test-state :down)))))
