; Relies on SRFI-1 and SRFI-43 (vector library)

(include "move.scm")

(declare (block) (usual-integrations))
; For the compiler (ignored by the interpreter).

(require 'srfi-1 'vector-lib)

(define ai-pawn-value 1550.5)
(define player-pawn-value 1550.5)

(define ai-mobil-val 0.0005)
(define player-mobil-val 0.0001)

; These values can be tweaked through program settings.
; A higher own-pawn-value will result in a more cautious opponent whereas
; a high oponent-pawn-value will result in a more aggressive opponent

(define EASY 2)
;(define MEDIUM 6)
;(define HARD 9)
; Search depth presets for difficulty level. Disabled for testing.

(define level EASY)

; Determines what the search depth will be. Should be tunable within the program

(define negate-side (lambda (side)
		 (if (eq? side WHITE)
		   BLACK
		   WHITE
		   )))

(define position-eval (lambda (board side)
			(define king? (lambda (piece)
					(if (or (eq? piece WHITE_KING)
						(eq? piece BLACK_KING))
					  #t
					  #f)
					))

			(let ((ai-sum 0)
			      (player-sum 0)
			      (ai-king-mobil 0)
			      (player-king-mobil 0)
			      (ai-pieces '())
			      (player-pieces '())
			      (ai-king '())
			      (player-king '())
			      (all-ai-moves '())
			      (all-player-moves '())
			      (ai-mobil 0)
			      (ai-king-mobil 0)
			      (player-mobil 0)
			      (player-king-mobil 0)
			      (win-bonus 0)
			      (lose-bonus 0)
			      )

			(vector-for-each (lambda (j row)
					   (vector-for-each (lambda (i col)
							      (if (not (eq? col EMPTY))
								(begin
								  (if (eq? side (modulo col 2))
								    (begin
								      (if (king? col)
									(set! ai-king (cons i j))
									(begin
									  (set! ai-pieces (cons (cons i j) ai-pieces))
									  (set! ai-sum (+ ai-sum 1))
									  )
									)
								      )
								    )
								    (begin
								      (if (king? col)
									(set! player-king (cons i j))
									(begin
									  (set! player-pieces (cons (cons i j) player-pieces))
									  (set! player-sum (+ player-sum 1))
									  )
									)
								      )
								  )
								)
							      )
							    row)
					   )
					 board)

			(set! ai-sum (* (+ ai-sum 1) ai-pawn-value))
			(set! player-sum (* (+ player-sum 1) player-pawn-value))
			; the one is added to take the king into account because, as you can see, he is not counted above.

			(set! all-ai-moves
			(map (lambda (pt)
			       (generate-piece-moves pt board)
			       )
			     ai-pieces
			     ))

			(set! ai-king-mobil (* (length (generate-piece-moves ai-king board))))
			(set! ai-mobil (* ai-mobil-val (length all-ai-moves)))

			(set! all-player-moves
			  (map (lambda (pt)
				 (generate-piece-moves pt board)
				 )
			       player-pieces
			       ))

			(set! player-king-mobil (* (length (generate-piece-moves player-king board))))
			(set! player-mobil (* player-mobil-val (length all-player-moves)))

			(set! win-bonus (if (eq? player-king-mobil 0)
					    +inf
					    0))

			(set! lose-bonus (if (eq? ai-king-mobil 0)
					     -inf
					     0))

			(- (+ ai-sum win-bonus ai-mobil)
			   (+ player-sum lose-bonus player-mobil))
			)
	))
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
		(define lol
			(list (generate-vertical-down board pt (cons (car pt) (+ 1 (cdr pt))) '())
			      (generate-vertical-up board pt (cons (car pt) (- (cdr pt) 1)) '())
			      (generate-horizontal-left board pt (cons (- (car pt) 1) (cdr pt)) '())
			      (generate-horizontal-right board pt (cons (+ (car pt) 1) (cdr pt)) '()))
			)

		(concatenate lol)
	     ))
; takes the space (pt) of a piece and generates all moves for it.

(define game-over? (lambda (board)
		     (define score (position-eval board BLACK))

		     (if (or (eq? score +inf)
			     (eq? score -inf))
		       #t
		       #f)
		     ))
; If the game has either been won or lost by an arbitrary side, then the game is, 
; in fact, over.

(define generate-moves (lambda (board side)
			 (define space-list (lambda (board side)
					      (define spaces '())
					      (vector-for-each (lambda (y row)
							    (vector-for-each (lambda (x col)
									  (if (and (not (eq? EMPTY
											     col))
										   (eq? (modulo col 2)
											side))
									    (set! spaces (append spaces (list (cons x y))))
									    )
									  )
									row)
							    )
							  board)
					      spaces
					      ))

			 (define piece-spaces (space-list board side)) 

			 (append-map (lambda (pc)
				       (generate-piece-moves pc board)
				       )
				     piece-spaces)
			 ))

(define nega-max (lambda (board depth side alpha beta)
           (define nega-inner (lambda (ply side alpha beta)
				(if (null? ply)
				  (position-eval board side)
				  (begin
				    (define next-alpha (max alpha (* -1
								     (nega-max (make-move board (car ply))
									       (- depth 1)
									       (negate-side side)
									       alpha
									       beta))
							    ))
				    (if (>= next-alpha beta)
				      beta
				      (nega-inner (cdr ply) side next-alpha beta)
				      )
				    )
				  )
				))

	   (if (or (eq? 0 depth)
		   (game-over? board))
		     (position-eval board side)
		     (begin
		       (define next-ply (generate-moves board side))
		       (nega-inner next-ply side alpha beta)
		       )
		     )
		   ))

(define find-ai-move (lambda (board side)
		       (define hi-score -inf)
		       (define move '())

		       (for-each (lambda (mv)
				   (display "Now probing: ") (display mv)
				   (newline)
				   ; debugging code.

				   (define score (nega-max (make-move board mv) (- level 1) side -inf +inf))
				   (when (> score
					  hi-score)
				     (set! hi-score score)
				     (set! move mv)
				     )
				   )
				 (generate-moves board side))
		       move
		       ))
; Primarily a driver function. Creates a list of scored moves (scoring is done by the recursive function nega-max) and 
; selects the one with the highest score.

(define generate-vertical-down (lambda (board origin curr moves)
				 (if (eq? (cdr curr)
					  ROWS)
				   moves
				   (begin
				     	 (define cell (get-cell board curr))
					 (if (not (eq? EMPTY cell))
					 moves
					 (generate-vertical-down board 
								 origin 
								 (cons (car curr) 
								       (add1 (cdr curr))) 
								 (cons (cons origin (list curr)) moves)
					 
					 ))
				 )
			       )))
; Where board is the current state, origin is the space of origin for this move, curr
; is the current set of coordinates and moves is the list of moves generated so far.
; The spaces are numbered from zero.

(define generate-horizontal-right (lambda (board origin curr moves)
				 (if (eq? (car curr)
					  COLS)
				   moves
				   (begin
					 (define cell (get-cell board curr))

					 (if (not (eq? EMPTY cell))
					 moves
					 (generate-horizontal-right board 
								 origin 
								 (cons (add1 (car curr))
								       (cdr curr)) 
								 (cons (cons origin (list curr)) moves)
					 
					    ))
					 )
				   )
				 ))

(define generate-horizontal-left (lambda (board origin curr moves)
				 (if (eq? (car curr)
					  -1)
				   moves
				   (begin
					 (define cell (get-cell board curr))

					 (if (not (eq? EMPTY cell))
					 moves
					 (generate-horizontal-left board 
								 origin 
								 (cons (sub1 (car curr))
								       (cdr curr)) 
								 (cons (cons origin (list curr)) moves)
					 
					    ))
					 )
				   )

				   ))

(define generate-vertical-up (lambda (board origin curr moves)
				 (if (eq? (cdr curr)
					  -1)
				   moves
				   (begin
				     	 (define cell (get-cell board curr))
					 (if (not (eq? EMPTY cell))
					 moves
					 (generate-vertical-up board 
								 origin 
								 (cons (car curr) 
								       (sub1 (cdr curr))) 
								 (cons (cons origin (list curr)) moves)
					 
					 ))
				 )
			       )))
