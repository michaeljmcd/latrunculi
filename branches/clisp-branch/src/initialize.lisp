(require 'vector-lib)

(defconstant COLS 12)
(defconstant ROWS 8)

(defconstant EMPTY -1)
(defconstant WHITE_KING 0)
(defconstant WHITE_PAWN 2)
(defconstant BLACK_KING 1)
(defconstant BLACK_PAWN 3)

(define create-game-board (lambda ()
			    '#(#(3 3 3 3 3 3 3 3 3 3 3 3)
			       #(-1 -1 -1 -1 -1 1 -1 -1 -1 -1 -1 -1)
			       #(-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
			       #(-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
			       #(-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
			       #(-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
			       #(-1 -1 -1 -1 -1 -1 0 -1 -1 -1 -1 -1)
			       #(2 2 2 2 2 2 2 2 2 2 2 2)
			       )
			    ))
; Returns a freshly allocated copy of the default game board.
; The board will be represented as a vector of vectors.

; create-game-board returns a list of lists representing the 12 x 8 board. 
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

; ROWS, COLS used as CONSTANTS! These define the dimensions of the board.
