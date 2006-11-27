(declare (block) (usual-integrations))
; For the compiler (ignored by the interpreter).

(include "move.scm")

(require 'srfi-1 'vector-lib)
; Relies on SRFI-1 and SRFI-43 (vector library)

(define negate-side (lambda (side)
		 (if (eq? side WHITE)
		   BLACK
		   WHITE
		   )
		 ))

(define position-eval (lambda (board side players)
			(let* ((current (if (eq? side (car (car players)))
					  (car players)
					  (cdr players)))
			       (opponent (if (not (eq? side (car (car players))))
					   (car players)
					   (cdr players)))
			       (own-material-value (vector-ref (car (cddddr current)) 1))
			       (own-material (* own-material-value (length (cddr current))))
			       (opponent-material-value (vector-ref (car (cddddr opponent)) 1))
			       (opponent-material (* opponent-material-value (length (cddr opponent))))
			       (own-king-mobil-val (vector-ref (cadr (cdddr current)) 3))
			       (opp-king-mobil-val (vector-ref (cadr (cdddr current)) 4))
			       ;(own-king-mobil (* own-king-mobil-val (length
			       )
			  (- own-material opponent-material)
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

(define game-over? (lambda (board players)
		     (let ((king1 (find (lambda (pair)
					  (if (or (eqv? (car pair) BLACK_KING)
						  (eqv? (car pair) WHITE_KING))
					    #t
					    #f
					    )) 
					(car (cdddr (car players)))
					))
			   (king2 (find (lambda (pair)
					  (if (or (eqv? (car pair) BLACK_KING)
						  (eqv? (car pair) WHITE_KING))
					    #t
					    #f
					    )) 
				  (car (cddddr players)))
				  )
		       )
		       (if (or (eq? (length (generate-piece-moves (cdr king1) board)) 0)
			       (eq? (length (generate-piece-moves (cdr king2) board)) 0))
			 #t
			 #f
			 )
		       )
		     ))
; If one of the kings can't make a move, then the game is over.

(define nega-max (lambda (board depth side alpha beta players)
		   (if (or (eq? 0 depth)
			   (game-over? board))
		     (position-eval board side players)
		     (let ((next-ply (generate-moves board side)))
		       (nega-inner board next-ply side alpha beta players)
		       ))
		   ))

(define nega-inner (lambda (board ply side alpha beta players)
	(if (null? ply)
	  (position-eval board side players)
	  (let* ((piece (get-cell board (caar ply)))
		 (captured-vals (call-with-values (lambda () (affect-captures (move-piece board (car ply)))) list))
		 (next-alpha (max alpha (* -1
					  (nega-max (make-move board (car ply))
						    (- depth 1)
						    (negate-side side)
						    alpha
						    beta
						    (update-players (car ply) piece side captured-vals players))
				 )))
		 )
	    (if (>= next-alpha beta)
	      beta
	      (nega-inner (cdr ply) side next-alpha beta players)
	      ))
	  )
	))

(define find-ai-move (lambda (board side players)
		       (let ((self (if (eq? (caar players) side)
				      (car players)
				      (cadr players)
				      ))
			      (hi-score -inf)
			      (move '())
			      (level (cadddr players))
			     ) 
			 (for-each (lambda (mv)
				     (let* ((piece (get-cell board (car mv)))
					    (captured-vals (call-with-values (lambda () (affect-captures (move-piece board mv))) list))
					    (score (nega-max (make-move board mv) 
							    (- level 1) 
							    side 
							    -inf +inf 
							    (update-players mv piece side captured-vals players))
							    ))
				       (when (> score hi-score)
					 (set! hi-score score)
					 (set! move mv)))
				   )
				 (generate-moves board side players))
			 move)
		       ))
; Primarily a driver function. Creates a list of scored moves (scoring is done by the recursive function nega-max) and 
; selects the one with the highest score.

(define generate-moves (lambda (board side players)
			 (let ((pieces (if (eq? side (caar players))
					 (car (cdddr (car players)))
					 (car (cddddr players)))
				       ))
			   (append-map (lambda (piece)
					 (generate-piece-moves (cdr piece) board))
				       pieces)
			   )
			 ))

; This convention applies to all of the next four functions.
; Board is the current state, origin is the space of origin for this move, curr
; is the current set of coordinates and moves is the list of moves generated so far.
; The spaces are indexed starting at zero. 

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
