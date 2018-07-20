(ns latrunculi.model)

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
