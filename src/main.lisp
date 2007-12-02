; Latrunculi
; (c) Michael J. McDermott, 2006
; Licensed under the GPL v.2

(load "gfx")
(load "ai")

(defconstant AI 0)
(defconstant HUMAN 1)

(defconstant BLACK 1)
(defconstant WHITE 0)

(defclass player ()
  ((color :accessor player-color)
   (name :accessor player-name)
   (controller :accessor player-controlled-by)
   (pieces :accessor piece-list)
   (ai-settings :accessor get-ai-settings))
  )

(defparameter player0 (list BLACK 
		      "Ajax" 
		      AI 
		      '((3 . (0 . 0)) (3 . (1 . 0)) (3 . (2 . 0))
                        (3 . (3 . 0)) (3 . (4 . 0))
                        (3 . (5 . 0)) (3 . (6 . 0))
                        (3 . (7 . 0)) (3 . (8 . 0))
                        (3 . (9 . 0)) (3 . (10 . 0))
                        (3 . (11 . 0)) (1 . (5 . 1)))
		      '#(40 95 65 0.0001 0.0001)
		      ))

(defparameter player1 (list WHITE 
		      "Achilles" 
		      HUMAN 
		      '((2 . (0 . 11)) (2 . (1 . 11)) (2 . (2 . 11))
                        (2 . (3 . 11)) (2 . (4 . 11))
                        (2 . (5 . 11)) (2 . (6 . 11))
                        (2 . (7 . 11)) (2 . (8 . 11))
                        (2 . (9 . 11)) (2 . (10 . 11))
                        (2 . (11 . 11)) (0 . (6 . 6)))
		      '#(40 25 25 0.05 0.05)
		      ))

; Player data is a tuple of the form (C N P M A) where C is the player's color, N
; is the player's name, P is an integer indicating whether the player is human
; controlled or AI, M (for material) is a list of tuples describing all of the pieces
; that the player has and their positions, and A is the player's AI settings (ignored
; if under player control). A is of the form:
; #(search-depth own-pawn-value opponent-pawn-value own-king-mobility-value opponent-king-mobility-value)

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

(initialize-display)
;(show-menu)
(initialize-game)
