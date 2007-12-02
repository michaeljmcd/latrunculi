(include "move.scm")

(require 'srfi-1 'vector-lib)
; Relies on SRFI-1 and SRFI-43 (vector library)

(defun negate-side (side)
		 (if (eq? side WHITE)
		   BLACK
		   WHITE
		   ))

(defun king? (piece-id)
		(if (or (eq? piece-id BLACK_KING)
			(eq? piece-id WHITE_KING))
		  #t
		  #f
		  ))

(defun position-eval (board side players)
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
			       (own-king-loc (cdr (find (lambda (pc) 
							  (if (king? (car pc))
							    #t
							    #f))
							(cadddr current))))
			       (opp-king-loc (cdr (find (lambda (pc) 
							  (if (king? (car pc))
							    #t
							    #f))
							(cadddr opponent))))
			       (own-king-mobil (* own-king-mobil-val (length (generate-piece-moves own-king-loc board))))
			       (opp-king-mobil (* opp-king-mobil-val (length (generate-piece-moves opp-king-loc board))))
			       (win-lose (cond ((eq? own-king-mobil 0) => -inf)
					       ((eq? opp-king-mobil 0) => +inf)
					       (else 0))) 
			       )
			  (+ (- own-material opponent-material)
			     (- own-king-mobil opp-king-mobil)
			     win-lose)
			  )
			)
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

(defun generate-piece-moves (pt board)
		(let ((lol
			(list (generate-vertical-down board pt (cons (car pt) (+ 1 (cdr pt))) '())
			      (generate-vertical-up board pt (cons (car pt) (- (cdr pt) 1)) '())
			      (generate-horizontal-left board pt (cons (- (car pt) 1) (cdr pt)) '())
			      (generate-horizontal-right board pt (cons (+ (car pt) 1) (cdr pt)) '()))
			))
		(concatenate lol)
		))
; takes the space (pt) of a piece and generates all moves for it.

(defun game-over? (board players)
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
		       ))
; If one of the kings can't make a move, then the game is over.

(defun nega-eval (ply board side depth players alpha beta)
	      (if (null? ply)
		alpha
		(let* ((captures (call-with-values (lambda () (affect-captures (make-move board (car ply)))) list))
		       (new-alpha (max alpha (* -1
					       (nega-max (make-move board (car ply))
							 (negate-side side)
							 (- depth 1)
							 (update-players (car ply)
									 (get-cell board (caar ply))
									 side
									 captures
									 players)
							 (* -1 alpha)
							 (* -1 beta)
							 )
					       ))
				 ))
		  (if (> new-alpha beta)
		    beta
		    (nega-eval (cdr ply) board side depth players new-alpha beta)
		    )
		  )
		))

(defun nega-max (board side depth players alpha beta)
		   (if (or (eq? depth 0)
			   (game-over? board players))
		     (position-eval board side players)
		     (nega-eval (generate-moves board side players)
				board
				side
				depth
				players
				alpha
				beta)
		     ))

(defun find-ai-move (board side players)
		       (let ((self (if (eq? (caar players) side)
				      (car players)
				      (cadr players)
				      ))
			      (hi-score -inf)
			      (move '())
			      (level (cadddr players))
			     ) 
			 (newline)
			 (for-each (lambda (mv)
				     (display "Now probing: ")(display mv)
				     (newline)
				     (let* ((piece (get-cell board (car mv)))
					    (captured-vals (call-with-values (lambda () (affect-captures (move-piece board mv))) list))
					    (score (nega-max (make-move board mv) 
							    side 
							    (- level 1) 
							    (update-players mv piece side captured-vals players)
							    -inf +inf 
							    )
							    ))
				       (when (> score hi-score)
					 (set! hi-score score)
					 (set! move mv)))
				   )
				 (generate-moves board side players))
			 move))
; Primarily a driver function. Creates a list of scored moves (scoring is done by the recursive function nega-max) and 
; selects the one with the highest score.

(defun generate-moves (board side players)
			 (let ((pieces (if (eq? side (caar players))
					 (car (cdddr (car players)))
					 (car (cddddr players)))
				       ))
			   (append-map (lambda (piece)
					 (generate-piece-moves (cdr piece) board))
				       pieces)
			   ))

; This convention applies to all of the next four functions.
; Board is the current state, origin is the space of origin for this move, curr
; is the current set of coordinates and moves is the list of moves generated so far.
; The spaces are indexed starting at zero. 

(defun generate-vertical-down (board origin curr moves)
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
			       ))

(defun generate-horizontal-right (board origin curr moves)
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
				 ))

(defun generate-horizontal-left (board origin curr moves)
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
				   )))

(defun generate-vertical-up (board origin curr moves)
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
			       ))
