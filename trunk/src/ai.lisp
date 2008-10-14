(in-package #:latrunculi)

(defun negate-side (side)
		 (if (eql side +WHITE+)
		   +BLACK+
		   +WHITE+
		   ))

(defun king? (piece-id)
		(if (or (eq piece-id +BLACK_KING+)
                (eq piece-id +WHITE_KING+)) 
          t 
          nil
          )
        )

(defun contains-king? (board-space)
    (if (king? (car board-space))
        t
        nil)
  )

(defun position-eval (board side players)
			(let* ((current (if (eql side (player-color (car players)))
                              (car players)
                              (cdr players)
                              ))
			       (opponent (if (not (eql side (player-color (car players))))
					   (car players)
					   (cdr players)))
			       (own-material (* (gethash 'own-pawn-value (get-ai-settings current)) (length (piece-list current))))
			       (opponent-material (* (gethash 'opponent-pawn-value (get-ai-settings current)) (length (piece-list opponent))))
			       (own-king-mobil-val (gethash 'own-king-mobility-value (get-ai-settings current)))
			       (opp-king-mobil-val (gethash 'opponent-king-mobility-value (get-ai-settings current)))
			       (own-king-loc (cdr (find-if #'contains-king? (piece-list current))))
			       (opp-king-loc (cdr (find-if #'contains-king? (piece-list opponent))))
			       (own-king-mobil (* own-king-mobil-val (length (generate-piece-moves own-king-loc board))))
			       (opp-king-mobil (* opp-king-mobil-val (length (generate-piece-moves opp-king-loc board))))
			       (win-lose (cond ((eql own-king-mobil 0) +NEGATIVE-INFINITY+)
					       ((eql opp-king-mobil 0) +INFINITY+)
					       (t 0))))
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
		(let ((down (generate-vertical-down board pt (cons (car pt) (+ 1 (cdr pt))) '()))
              (up (generate-vertical-up board pt (cons (car pt) (- (cdr pt) 1)) '()))
              (left (generate-horizontal-left board pt (cons (- (car pt) 1) (cdr pt)) '()))
              (right (generate-horizontal-right board pt (cons (+ (car pt) 1) (cdr pt)) '())))
        (remove nil (append right left down up))
		))
; takes the space (pt) of a piece and generates all moves for it.

(defun game-over? (board players)
     (let ((king1 (find-if #'contains-king? (piece-list (car players)))) 
           (king2 (find-if #'contains-king?  (piece-list (cdr players))))
           )
       (if (or (eql (length (generate-piece-moves (cdr king1) board)) 0)
               (eql (length (generate-piece-moves (cdr king2) board)) 0)) 
         t 
         nil)
       ))
; If one of the kings can't make a move, then the game is over.

(defun nega-eval (ply board side depth players alpha beta)
      (if (null ply)
		alpha
		(let ((new-alpha (max alpha (* -1
                                       (multiple-value-bind (new-board captures)
                                         (make-move board (car ply) players)
                                         (nega-max new-board
                                                   (negate-side side)
                                                   (- depth 1)
                                                   (update-players captures players)
                                                   (* -1 alpha)
                                                   (* -1 beta))
                                         )
                                       )
                              )
				 ))
		  (if (> new-alpha beta)
		    beta
		    (nega-eval (cdr ply) board side depth players new-alpha beta)
		    )
		  )
		))

(defun nega-max (board side depth players alpha beta)
		   (if (or (eq depth 0) 
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
		       (let* ((self (if (eql (car players) side)
				      (car players)
				      (cdr players)
				      ))
			         (hi-score +NEGATIVE-INFINITY+)
			         (move '())
			         (level (gethash 'search-depth (get-ai-settings self)))
			     ) 
                 (reduce (lambda (pair1 pair2) (if (> (car pair1) (car pair2)) pair1 pair2))
                         (map 'list (lambda (mv)
                                      (let ((piece (get-cell board (car mv)))
                                            (captured-vals (multiple-value-list (affect-captures (move-piece board mv) players))))
                            (cons (nega-max (make-move board mv players) 
                                            side 
                                            (- level 1)
                                            (update-players (cadr captured-vals) players)
                                            +NEGATIVE-INFINITY+ 
                                            +INFINITY+)
                                  mv)
                            )
                                      )
                       (generate-moves board side players)))
                 ))
; Primarily a driver function. Creates a list of scored moves (scoring is done by the recursive function nega-max) and 
; selects the one with the highest score.

(defun generate-moves (board side players)
			 (let ((pieces (if (eq side (player-color (car players)))
					 (piece-list (car players))
					 (piece-list (cdr players))
				       )))
               (mapcan (lambda (piece) (generate-piece-moves (cdr piece) board)) pieces)
			   ))

; This convention applies to all of the next four functions.
; Board is the current state, origin is the space of origin for this move, curr
; is the current set of coordinates and moves is the list of moves generated so far.
; The spaces are indexed starting at zero. 

(defun generate-vertical-down (board origin curr moves)
				 (if (or (eq (cdr curr) +ROWS+)
                         (> (cdr curr) +ROWS+))
				   moves
				   (let ((cell (get-cell board curr)))
				     (if (not (eq +EMPTY+ cell))
				       moves
				       (generate-vertical-down board 
							       origin 
							       (cons (car curr) (add1 (cdr curr))) 
							       (cons (cons origin (list curr)) moves))
				       )
			   	     )
			       ))

(defun generate-horizontal-right (board origin curr moves)
				 (if (eq (car curr) +COLS+)
				   moves
				   (let ((cell (get-cell board curr)))
				     (if (not (eq +EMPTY+ cell))
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
				 (if (eq (car curr) -1)
				   moves
				   (let ((cell (get-cell board curr)))
				     (if (not (eq +EMPTY+ cell))
				       moves
				       (generate-horizontal-left board 
								 origin 
								 (cons (sub1 (car curr)) (cdr curr)) 
								 (cons (cons origin (list curr)) moves))
				     )
				   )))

(defun generate-vertical-up (board origin curr moves)
				 (if (eq (cdr curr)
					  -1)
				   moves
				   (let ((cell (get-cell board curr)))
				     (if (not (eq +EMPTY+ cell))
				       moves
				       (generate-vertical-up board 
							     origin 
							     (cons (car curr) (sub1 (cdr curr))) 
							     (cons (cons origin (list curr)) moves))
				       )
				   )
			       ))
