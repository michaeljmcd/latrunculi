(ns latrunculi.model
 (:require [taoensso.timbre :as timbre :refer [trace info with-level]]))

(def +AI+ 0)
(def +HUMAN+ 1)

(def +BLACK+ 1)
(def +WHITE+ 0)

(def +COLUMNS+ 12)
(def +ROWS+ 8)

(def +EMPTY+ -1)
(def +WHITE_KING+ 0)
(def +WHITE_PAWN+ 2)
(def +BLACK_KING+ 1)
(def +BLACK_PAWN+ 3)

(def +INFINITY+ 1e38)
(def +NEGATIVE-INFINITY+ -1e37)

(def ai-settings0 {:search-depth 2 :own-pawn-value 95 :opponent-pawn-value 65 :own-king-mobility-value 0.0001 :opponent-king-mobility-value 0.0001})
(def ai-settings1 {:search-depth 2 :own-pawn-value 95 :opponent-pawn-value 65 :own-king-mobility-value 0.0001 :opponent-king-mobility-value 0.0001})

(def player0-initial-pieces 
        [[3 [0 0]] [3 [1 0]] 
         [3 [2 0]] [3 [3 0]]
         [3 [4 0]] [3 [5 0]]
         [3 [6 0]] [3 [7 0]]
         [3 [8 0]] [3 [9 0]]
         [3 [10 0]] [3 [11 0]]
         [1 [5 1]]]
)
 
(def starting-board
                [[3 3 3 3 3 3 3 3 3 3 3 3]
                  [-1 -1 -1 -1 -1 1 -1 -1 -1 -1 -1 -1]
                  [-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1]
                  [-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1]
                  [-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1]
                  [-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1]
                  [-1 -1 -1 -1 -1 -1 0 -1 -1 -1 -1 -1]
                  [2 2 2 2 2 2 2 2 2 2 2 2]]
 ) 

; create-game-board returns a 2D array representing the 12 x 8 board. 
; The board, 
; internally, will have the following representation: a list of lists, where 
; each list is a row. The list elements will be tuples having the 
; following legend:
;-1 - Empty 
; 0 - White King
; 1 - Black King
; 2 - White 'pawn'
; 3 - Black 'pawn'
;
; There is a reason for this. After doing this any white piece mod 2 = 0
; and any black piece mod 2 = 1. This provides a quick way to determine 
; color.
;
; A tuple will be of the form (L . I) where L is a value from the above
; legend and I is the ID for the piece if applicable and -1 if N/A. 

(defn create-default-game-state []
{:players [{:color +BLACK+ :name "Ajax" :controller +AI+ :pieces player0-initial-pieces :ai-settings ai-settings0}
           {:color +WHITE+ :name "Achilles" :controller +AI+ :pieces player0-initial-pieces :ai-settings ai-settings0}]
 :board starting-board}
)

(defn get-cell [board coordinates]
 (trace "Retrieving space value for" coordinates "in" board)
 (if (or (>= (second coordinates) +ROWS+)
         (>= (first coordinates) +COLUMNS+)
         (< (first coordinates) 0)
         (< (second coordinates) 0))
    (do (trace "Invalid coordinates, returning sentinel value.") nil)
    (do (trace "Valid coordinates, retrieving value.") 
     (let [res (nth (nth board (second coordinates)) (first coordinates))]
      (trace "Found value" res "at" (reverse coordinates))
      res)
     )))

; Returns whether or not a given path includes some piece between the start and end points
; We need to get a list of all the spaces in between the start and the 
; destination (non-inclusive) and check them for a piece.
(defn jumped? [board delta]
 (let [
  start-y (min (get-in delta [0 0]) (get-in delta [1 0]))
  end-y (max (get-in delta [0 0]) (get-in delta [1 0]))
  start-x (min (get-in delta [0 1]) (get-in delta [1 1]))
  end-x (max (get-in delta [0 1]) (get-in delta [1 1]))
  ]
  (trace "Start X:" 
         start-x 
         "end X:" 
         end-x 
         "start Y:"
         start-y
         "end Y:"
         end-y)
  (not (= (vec (distinct (rest (flatten
  (keep-indexed (fn [y-index row]
                 (keep-indexed (fn [x-index space]
                                (if (and (and (>= x-index start-x)
                                              (<= x-index end-x))
                                         (and (>= y-index start-y)
                                              (<= y-index end-y)))
                                  space
                                  nil)
                               ) row)
                ) board)))))
   [-1]))
))

; Given a move, delta, using the same form as above, validate-move determines whether or not the given move is legal.
; This is needed to check user moves, though not AI moves (because the move generator will only generate legal moves
; anyway). A move is illegal under the following conditions:
; 1. The starting space is empty.
; 2. The starting space contains an opposing piece.
; 3. The move is diagonal (i.e. /\x > 0 AND /\ y > 0)
; 4. The move jumps over a piece on either side.
; 5. The ending space already has a piece on it.

(defn move-valid? [board delta side]
 (trace "Entering move validation with delta" delta "and board" board "for side" side ".")
 (let [origin (get-cell board (first delta))
       destination (get-cell board (second delta))
       delta-y (Math/abs (- (first (second delta)) (first (first delta))))
       delta-x (Math/abs (- (second (second delta)) (second (first delta))))]
  (trace "Testing validity of move from " origin " to " destination " with delta x: " delta-x " and delta y: " delta-y)
  (not (or (= origin +EMPTY+) ; attempting to move an empty space
           (not (= side (mod origin 2))) ; attempting to move other player's piece
           (and (> delta-x 0) (> delta-y 0))
           (not (= destination +EMPTY+))
           ;(jumped? board delta)
        ))
 ))

(defn move-piece [board delta]
 (let [origin (first delta)
       destination (second delta)
       original-piece (get-cell board origin)]
 (-> board
  (update-in [(first origin) (second origin)] (constantly +EMPTY+))
  (update-in [(first destination) (second destination)] (constantly original-piece))
 )
))
