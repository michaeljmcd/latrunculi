(declare (block) (usual-integrations))
; For the compiler (ignored by the interpreter).

(include "move.scm")

(require 'srfi-1 'vector-lib)
; Relies on SRFI-1 and SRFI-43 (vector library)

(define level 2)
; Determines what the search depth will be. Should be tunable within the program

(define negate-side (lambda (side)
		 (if (eq? side WHITE)
		   BLACK
		   WHITE
		   )))

(define position-eval (lambda (board side players)
			(let* ((current (if (eq? side (car (car players)))
					  (car players)
					  (cdr players)))
			       (opponent (if (not (eq? side (car (car players))))
					   (car players)
					   (cdr players)))
			      ; (current-material (length 
			       )
			  3
			  )
			))
; players is a pair of the form (player0 . player1)
; This function evaluates the value of a given position. It uses the function:
; __                                   __
; \   ai_pawn_val * # ai pawns     --  \   opponent_pawn_val * # opponent pawns + win/lose bonus +
; /                                    /
; --                                   --
;
; ai_king_mobility * king_mobil_val -- player_king_mobil * king_mobil_val + ai_overall_mobil - opponent_overall_mobil
;
; Where mobility in general is the number of moves possible in the position and the king's mobility is the number of 
; moves available to the king.

(define generate-piece-moves (lambda (pt board)
		(let ((lol
			(list (generate-vertical-down board pt (cons (car pt) (+ 1 (cdr pt))) '())
			      (generate-vertical-up board pt (cons (car pt) (- (cdr pt) 1)) '())
			      (generate-horizontal-left board pt (cons (- (car pt) 1) (cdr pt)) '())
			      (generate-horizontal-right board pt (cons (+ (car pt) 1) (cdr pt)) '()))
			))
		(concatenate lol)
		)
	     ))
; takes the space (pt) of a piece and generates all moves for it.

(define game-over? (lambda (board)
		     (let ((score (position-eval board BLACK)))
			     (if (or (eq? score +inf)
				     (eq? score -inf))
			       #t
			       #f)
		     )
		     ))
; If the game has either been won or lost by an arbitrary side, then the game is, 
; in fact, over.

(define generate-vertical-down (lambda (board origin curr moves)
				 (if (eq? (cdr curr)
					  ROWS)
				   moves
				   (let ((cell (get-cell board curr)))
				     (if (not (eq? EMPTY cell))
				       moves
				       (generate-vertical-down board 
							       origin 
							       (cons (car curr) 
								     (add1 (cdr curr))) 
							       (cons (cons origin (list curr)) moves))
				       )
			   	     )
			       )))
; Where board is the current state, origin is the space of origin for this move, curr
; is the current set of coordinates and moves is the list of moves generated so far.
; The spaces are numbered from zero.

(define generate-horizontal-right (lambda (board origin curr moves)
				 (if (eq? (car curr)
					  COLS)
				   moves
				   (let ((cell (get-cell board curr)))
				     (if (not (eq? EMPTY cell))
				       moves
				       (generate-horizontal-right board 
								  origin 
								  (cons (add1 (car curr))
									(cdr curr)) 
								  (cons (cons origin (list curr)) moves))
				       )
				   )
				  )
				 ))

(define generate-horizontal-left (lambda (board origin curr moves)
				 (if (eq? (car curr)
					  -1)
				   moves
				   (let ((cell (get-cell board curr)))
				     (if (not (eq? EMPTY cell))
				       moves
				       (generate-horizontal-left board 
								 origin 
								 (cons (sub1 (car curr)) (cdr curr)) 
								 (cons (cons origin (list curr)) moves))
				     )
				   )
				   )))

(define generate-vertical-up (lambda (board origin curr moves)
				 (if (eq? (cdr curr)
					  -1)
				   moves
				   (let ((cell (get-cell board curr)))
				     (if (not (eq? EMPTY cell))
				       moves
				       (generate-vertical-up board 
							     origin 
							     (cons (car curr) (sub1 (cdr curr))) 
							     (cons (cons origin (list curr)) moves))
				       )
				   )
			       )))
