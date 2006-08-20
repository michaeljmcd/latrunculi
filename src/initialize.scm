(require 'vector-lib)

(define COLS 12)
(define ROWS 8)

(define EMPTY -1)
(define WHITE_KING 0)
(define WHITE_PAWN 2)
(define BLACK_KING 1)
(define BLACK_PAWN 3)

(define AI 0)
(define HUMAN 1)

(define BLACK 1)
(define WHITE 0)

(define player0 (list BLACK "Ajax" AI))
(define player1 (list WHITE "Achilles" HUMAN)) 

; Player 0  == CPU, Player 1 == human

; Player data is a tuple of the form (C N P) where C is the player's color, N
; is the player's name, and P is an integer indicating whether the player is human
; controlled or AI. If the player is human controlled, the following settings are 
; irrelevant.

(define current-turn HUMAN)

(define create-game-board (lambda ()
			    (define board (make-vector ROWS))

			    ;(set! r (make-vector COLS (cons EMPTY EMPTY)))
			    (vector-set! board 2 (make-vector COLS EMPTY))
			    (vector-set! board 3 (make-vector COLS EMPTY))
			    (vector-set! board 4 (make-vector COLS EMPTY))
			    (vector-set! board 5 (make-vector COLS EMPTY))
			    ; create a completely empty board

			    (vector-set! board 0 (make-vector COLS BLACK_PAWN)) 
			    (vector-set! board (- ROWS 1) (make-vector COLS WHITE_PAWN))
			    ; and fill the upper and lower ranks with pawns
			
			    (define empty-row (make-vector COLS EMPTY))
			    (vector-set! empty-row 5 BLACK_KING)
			    (vector-set! board 1 empty-row)
			    ; Place the black king at (5, 1)

			    (define empty-row2 (make-vector COLS EMPTY))
			    (vector-set! empty-row2 6 WHITE_KING)
			    (vector-set! board 6 empty-row2)
			    ; Place the white king at (6, 6)
	
			    board
			    ))
; The board will be represented as a vector of vectors.

(define print-list (lambda (list)
		(when (not (null? list))
			(display (car list))
			(newline)
			(newline)
			(newline)
			(print-list (cdr list))
		)
	))
; For debugging purposes. Makes it easier to see the difference in rows on the board.

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
