(ns latrunculi.model-test
  (:require [clojure.test :refer :all]
            [latrunculi.model :refer :all]))

(deftest query-tests
  (testing "get-cell handles invalid input."
    (let [board starting-board]
     (is (nil? (get-cell board [0 (+ +ROWS+ 1)])))
     (is (nil? (get-cell board [0 (+ +ROWS+ 10)])))
     (is (nil? (get-cell board [(+ +COLUMNS+ 1) 0])))
     (is (nil? (get-cell board [(+ +COLUMNS+ 10) 0])))
     (is (nil? (get-cell board [-1 0])))
     (is (nil? (get-cell board [0 -1])))
    ))
  (testing "get-cell returns expected values"
    (let [board starting-board]
     (is (= +BLACK_PAWN+ (get-cell board [0 0])))
     (is (= +EMPTY+ (get-cell board [3 0])))
    ))
  )
