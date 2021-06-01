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
(def state (app/create-state config))
(def pieces (app/create-pieces (:pieces config)))
(defn  index-of-piece [key]
  (first 
    (filter (complement nil?)
            (map-indexed (fn [i piece]
                           (if (= key 
                                  (:key piece)) 
                             i 
                             nil))
                          pieces))))

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
  (let [test-piece-idx (index-of-piece :TEST)          
        rotations      (get-in pieces [test-piece-idx :rotations])
        rotation-idx   0
        piece          (get pieces test-piece-idx)
        rotation       (nth rotations rotation-idx)
        profile        (app/movement-profile rotations rotation-idx) 
        player         (app/Player. (app/Cartesian. 0 0)
                                    test-piece-idx
                                    0
                                    profile
                                    :blue)
        _ (println player)
        test-state     (assoc state :player player)]
    (do
      (is (false? (app/overflow? test-state pieces :down)))
      (is (false? (app/overflow? test-state pieces :right)))
      (is (true?  (app/overflow? test-state pieces :left))))))


 (deftest overflow__right
   (let [test-piece-idx (index-of-piece :TEST)          
         rotations      (get-in pieces [test-piece-idx :rotations])
         rotation-idx   0
         piece          (get pieces test-piece-idx)
         profile        (app/movement-profile rotations rotation-idx) 
         board          (:board state)
         player         (app/Player. (app/Cartesian. (- (:width board) (:size piece)) 0)
                                     test-piece-idx
                                     rotation-idx
                                     profile
                                     :blue)
         test-state     (assoc state :player player)]
     (do
       (is (false? (app/overflow? test-state pieces :down)))
       (is (true?  (app/overflow? test-state pieces :right)))
       (is (false? (app/overflow? test-state pieces :left))))))

 (deftest overflow__down
   (let [test-piece-idx (index-of-piece :TEST)          
         rotations      (get-in pieces [test-piece-idx :rotations])
         rotation-idx   0
         piece          (get pieces test-piece-idx)
         profile        (app/movement-profile rotations rotation-idx) 
         board          (:board state)
         player         (app/Player. (app/Cartesian. (:size piece) (- (:height board) (:size piece)))
                                     test-piece-idx
                                     rotation-idx
                                     profile
                                     :blue)
         test-state     (assoc state :player player)]
     (do
       (is (true?  (app/overflow? test-state pieces :down)))
       (is (false? (app/overflow? test-state pieces :right)))
       (is (false? (app/overflow? test-state pieces :left))))))

(deftest create-hit-profiles__works 
  (let [piece    (nth pieces
                      (index-of-piece :TESTB))
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

(deftest update-grid__works
  (let [test-piece-idx (index-of-piece :TESTB)          
        piece-size     (:size (nth pieces 
                                   test-piece-idx))
        board-width    (get-in state [:board :width])
        test-state     (assoc state 
                              :position (app/Cartesian. (- board-width piece-size) 0)
                              :color :red
                              :piece-idx test-piece-idx)]
    (app/update-grid (get-in test-state [:board :grid]) pieces (:player test-state))))

(deftest serial-assoc__works 
  (do
    (is (= [1 2 "three" "four" 5] 
           (app/serial-assoc [1 2 3 4 5] 2 ["three" "four"]))))
  (is (= [1 2 "three" "four" "five" "six" "seven"] 
         (app/serial-assoc [1 2 3 4 5] 2 ["three" "four" "five" "six" "seven"])))
  (is (= ["one" "two" "three"] 
         (app/serial-assoc [1 2 3] 0 ["one" "two" "three"] ))))

(deftest directional-hit?__down-miss
  (let [test-piece-idx        (index-of-piece :TESTB)          
        piece-size            (:size (nth pieces test-piece-idx))
        {:keys [grid height]} (:board state)
        player                (app/Player.  (app/Cartesian. 0 (- height piece-size))
                                                  test-piece-idx
                                                  0
                                                  (app/movement-profile (:rotations 
                                                              (nth pieces test-piece-idx)) 0)
                                                  :red)
        new-grid              (app/update-grid grid pieces player)]
    (is (true? (app/directional-hit? new-grid player :down pieces)))))

(deftest directional-hit?__down-hit
  (let [test-piece-idx (index-of-piece :TEST)          
        piece-size     (:size (nth pieces test-piece-idx))
        {:keys [grid height]} (:board state)
        player         (app/Player.  (app/Cartesian. 0 (- height piece-size))
                                     test-piece-idx
                                     0
                                     (app/movement-profile (:rotations (nth pieces test-piece-idx)) 0)
                                     :red)
        new-grid                     (app/update-grid grid pieces player)]
    (is (true? (app/directional-hit? new-grid player :down pieces)))))
