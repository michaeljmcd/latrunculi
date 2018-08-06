(ns latrunculi.model-test
  (:require [clojure.test :refer :all]
            [taoensso.timbre :as timbre :refer [trace info with-level]]
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

(deftest validation-tests
 (testing "move-valid? detects invalid moves."
  (let [board starting-board]
   (is (true? (move-valid? board [[0 0] [3 0]] +BLACK+)))
   (is (false? (move-valid? board [[1 0] [2 0]] +BLACK+))) ; empty starting space
   (is (false? (move-valid? board [[7 0] [6 0]] +BLACK+))) ; empty starting space
   (is (false? (move-valid? board [[0 0] [1 1]] +BLACK+))) ; empty starting space
   (is (false? (move-valid? board [[0 0] [7 0]] +BLACK+))) ; empty starting space
  )
 ))

(deftest processing-tests
 (testing "move-piece swaps spaces correctly."
 ))
