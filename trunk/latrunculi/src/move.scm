; move.scm is to hold the move-related functions, such as making a move and validating one.
; The reason for this is so that the click events in the OpenGL code can access these functions without having to include the rest of the AI code.

(require 'vector-lib 'srfi-1)

(define get-cell (lambda (board coor)
		   (if (or (>= (cdr coor) ROWS)
			   (>= (car coor) COLS)
			   (< (cdr coor) 0)
			   (< (car coor) 0))
		     EMPTY
		     (vector-ref (vector-ref board (cdr coor)) (car coor))
		    )
		   ))


(define duplicate-board (lambda (board)
			  (let ((new-board '#()))
			    (vector-for-each (lambda (i row)
					       (set! new-board (vector-append new-board (vector (vector-copy row))))) 
					     board)
			    new-board)
			  ))
; Creates a newly-allocated deep copy of board.
(define is-surrounded-horizontally? (lambda (board space)
				   (let* ((this-cell (get-cell board space))
					  (side (modulo this-cell 2)) 
					  (space-after (cons (+ (car space) 1) (cdr space))) 
					  ; the coordinates of the space to the right of the space under examination.  

					  (space-before (cons (- (car space) 1) (cdr space)))
					  ; The coordinates of the space to the left of the space under examination.  
					  (side-after (modulo (get-cell board space-after) 2))
					  (side-before (modulo (get-cell board space-before) 2))
					  ; The side (black or white) of the piece on the left and the right.  
					  )
				     (if (and (not (eq? (car space) 0))
					      (not (eq? (car space) (- COLS 1))) ; can't capture a piece on the sides
					      (not (eq? (get-cell board space-after) EMPTY))
					      (not (eq? (get-cell board space-before) EMPTY)) 
					      (not (eq? side side-after)) 
					      (not (eq? side side-before))
					      (not (eq? this-cell EMPTY))
					      (not (eq? this-cell BLACK_KING)) 
					      (not (eq? this-cell WHITE_KING)))
				       #t
				       #f
				       ))
				     ))

(define is-surrounded-vertically? (lambda (board space)
				    (let* ((this-cell (get-cell board space))
					   (side (modulo this-cell 2)) 
					   (space-after (cons (car space) (+ (cdr space) 1)))
					   (space-before (cons (car space) (- (cdr space) 1))) 
					   (side-after (modulo (get-cell board space-after) 2)) 
					   (side-before (modulo (get-cell board space-before) 2))) 
				      
				      (if (and (not (eq? (cdr space) 0))
					       (not (eq? (cdr space) (sub1 ROWS)))
					       (not (eq? side side-after))
					       (not (eq? side side-before))
					       (not (eq? (get-cell board space-after) EMPTY))
					       (not (eq? (get-cell board space-before) EMPTY))
					       (not (eq? this-cell EMPTY))
					       (not (eq? this-cell BLACK_KING))
					       (not (eq? this-cell WHITE_KING))) 
					#t
					#f
					))
				    ))

(define is-corner? (lambda (space)
		  (if (or (and (eq? (cdr space) 0) 
			   (or (eq? (car space) 0)
			       (eq? (car space) (sub1 COLS)))
			   ) ; deals with the upper two corners.
			  (and (eq? (cdr space) (sub1 ROWS))
			       (or (eq? (car space) 0)
				   (eq? (car space) (sub1 COLS))))
			  )
		    #t
		    #f
		    )
		  ))

(define is-cornered? (lambda (board space)
		       (let* ((UPPER_LEFT (cons '0 '0))
			      (UPPER_RIGHT (cons (sub1 COLS) '0))
			      (LOWER_LEFT (cons '0 (sub1 ROWS)))
			      (LOWER_RIGHT (cons (sub1 COLS) (sub1 ROWS))) 
			      (corner (get-cell board space))
			      (space-side (modulo corner 2)) 
			      (caught #f)) 

			 (when (equal? space UPPER_LEFT)
			   (let* ((below (get-cell board (cons '0 '1)))
				  (right (get-cell board (cons '1 '0))) 
				  (below-side (modulo below 2))
				  (right-side (modulo right 2))) 
			     
			     (if (and (not (eq? EMPTY below)) 
				      (not (eq? EMPTY right))
				      (not (eq? EMPTY corner))
				      (not (eq? below-side space-side))
				      (not (eq? right-side space-side)))
			       (set! caught #t) 
			       (set! caught #f)
			       )
			     ))

		       (when (equal? space LOWER_LEFT)
			 (let* ((above (get-cell board (cons '0 (- ROWS 2))))
				(right (get-cell board (cons '1 (- ROWS 1)))) 
				(above-side (modulo above 2))
				(right-side (modulo right 2))) 
			   
			   (if (and (not (eq? EMPTY above))
				    (not (eq? EMPTY right))
				    (not (eq? EMPTY corner))
				    (not (eq? above-side space-side))
				    (not (eq? right-side space-side)))
			     (set! caught #t)
			     (set! caught #f))
			   ))

		       (when (equal? space LOWER_RIGHT)
			 (let* ((above (get-cell board (cons (sub1 COLS) (- ROWS 2))))
				(left (get-cell board (cons (- COLS 2) (sub1 ROWS)))) 
				(above-side (modulo above 2))
				(left-side (modulo left 2))) 
			   
			   (if (and (not (eq? EMPTY above))
				    (not (eq? EMPTY left))
				    (not (eq? EMPTY corner))
				    (not (eq? above-side space-side))
				    (not (eq? left-side space-side))) 
			     (set! caught #t)
			     (set! caught #f))
			   ))
		       
		       (when (equal? space UPPER_RIGHT)
			 (let* ((below (get-cell board (cons (sub1 COLS) '1)))
				(left (get-cell board (cons (- COLS 2) '0))) 
				(below-side (modulo below 2)) 
				(left-side (modulo left 2)) ) 
			   
			   (if (and (not (eq? EMPTY below))
				    (not (eq? EMPTY left))
				    (not (eq? EMPTY corner))
				    (not (eq? below-side space-side)) 
				    (not (eq? left-side space-side)))
			     (set! caught #t)
			     (set! caught #f))
			   ))

		       (if (or (eq? corner WHITE_KING)
			       (eq? corner BLACK_KING))
			 (set! caught #f))

		       caught
		       )
		       ))

(define affect-captures (lambda (board)
			  (let ((captured-pieces '())) 
			    (vector-for-each (lambda (y row)
					       (vector-for-each (lambda (x col) 
								  (let ((pt (cons x y))) 
								    (when (and (not (is-corner? pt))
									       (or (is-surrounded-horizontally? board pt)
										   (is-surrounded-vertically? board pt)
										   (is-cornered? board pt)))
								      (set! captured-pieces (append captured-pieces (list pt))) 
								      (set! board (replace-space board pt EMPTY))
								      )
								    (when (and (is-corner? pt)
									       (is-cornered? board pt))
								      (set! captured-pieces (append captured-pieces (list pt)))
								      (set! board (replace-space board pt EMPTY))
								      a)
								    ))
								row)
					       )
					     board)
			    (values board captured-pieces)
			    )
			  ))
; This function removes any pieces that ought to be captured. The rules regarding captures are as follows:
; 1. If a pawn is surrounded on two opposite sides, it is captured.
; 2. An outside wall cannot be used to capture a pawn.
; 3. A stone in a corner can be taken by placing two stones across the corner.
; 4. Multiple stones may be captured along a line (really doesn't change anything, but should be stated).

(define replace-space (lambda (board space replacement)
			(let ((piece-cleared (get-cell board space))
			      (updated-board (duplicate-board board)))
			  (vector-set! (vector-ref updated-board (cdr space)) (car space) replacement) 
			  (values updated-board piece-cleared))
			))
; Clears the space. Returns the modified board and the tuple describing the piece
; removed from the board. 

(define move-piece (lambda (board delta)
		     (let* ((board-dup (duplicate-board board)) 
			    (after-move (call-with-values (lambda ()
							    (replace-space board-dup (car delta) EMPTY))
							  (lambda (brd pc)
							    (replace-space brd (car (cdr delta)) pc)
							    )))
			    )
			after-move
			)
		     ))

(define make-move (lambda (board delta)
		    (let ((after-move (move-piece board delta)))
		      (affect-captures (duplicate-board after-move)))
		    ))
; Applies change delta (of the form: ((X1 . Y1) . (X2 . Y2))) to the given board board. Must take captures
; into affect. Will be used for both AI moves and player moves. AI moves verified during generation, whereas
; player moves will have to be verified in another function. This function does no verification. It simply makes
; the move and, if a piece is captured, removes it.

(define unmake-move (lambda (board delta)
		      (make-move board (cons (car (cdr delta))
					     (list (car delta)))
				 )
		      ))
; Given a move delta, unmake-move reverses the move.

(define jumped? (lambda (board delta)
		; We need to get a list/vector of all the spaces in between the start and the 
		; destination (non-inclusive) and check them for a piece.
		  (let* ((ans #f) 
			 (diff-x (- (car (car (cdr delta)))
				    (car (car delta)))) 
			 (delta-x (abs diff-x)) ; The absolute value of the change in the x-direction 
			 (diff-y (- (cdr (car (cdr delta)))
				    (cdr (car delta))))
			 (delta-y (abs diff-y)) 
		      ; At this point, the move is presumed valid in all other respects.
		      ; If you only move one space, you can't possible have jumped another.
		      ) 
		    (if (> delta-x 1)
		      (let* ((x-start (min (car (car delta))
					   (car (car (cdr delta))
						)))
			     (y-coor (cdr (car delta))) 
			     (x-list (map (lambda (e)
					    (get-cell board (cons (+ e x-start 1) y-coor)))
					  (list-tabulate (- delta-x 1) values))) 
			     )
			(if (> (length (filter (lambda (i)
						 (if (> i 0)
						   #f
						   #t))
					       x-list))
			       0)
			  (set! ans #f)
			  (set! ans #t)
			  ))
		      )

			(if (> delta-y 1)
			  (let* ((y-start (min (cdr (car delta))
					       (cdr (car (cdr delta)))
					       ))
				 (x-coor (car (car delta)))
				 (y-list (map (lambda (e)
						(get-cell board (cons x-coor (+ e y-start 1))))
					      (list-tabulate (- delta-y 1) values)))
				 ) 
			    (if (and (not (null? y-list))
				     (> (length (filter (lambda (i)
							  (if (> i 0)
							    #f
							    #t))
							y-list))
					0)) 
			      (set! ans #f)
			      (set! ans #t)
			      ))
			 )
		      ans
		      )
		  ))

(define move-valid? (lambda (board delta side)
		      (let ((space (get-cell board (car delta))) ; move's origin.  
			    (end (get-cell board (car (cdr delta)))) ; move's destination.  
			    (delta-x (abs (- (car (car (cdr delta)))
					     (caar delta))
					  )) ; The absolute value of the change in the x-direction 
			    (delta-y (abs (- (cdr (car (cdr delta)))
					     (cdr (car delta)))
					  )) ; The absolute value of the change in the y-direction
			    ) 
			(if (or (eq? space EMPTY)   ; trying to move an empty space
				(not (eq? end EMPTY)) ; trying to move to an occupied space (and (> delta-x 0)
				(and (> delta-y 0) ; trying to move diagonally.
				     (> delta-x 0))
				(not (eq? side (modulo space 2))) ; trying to move an opposing piece.
				(jumped? board delta) 	; jumped a piece.
			      )
			  #f
			  #t
			  ))
		      ))
; Given a move, delta, using the same form as above, validate-move determines whether or not the given move is legal.
; This is needed to check user moves, though not AI moves (because the move generator will only generate legal moves
; anyway). A move is illegal under the following conditions:
; 1. The starting space is empty.
; 2. The starting space contains an opposing piece.
; 3. The move is diagonal (i.e. /\x > 0 AND /\ y > 0)
; 4. The move jumps over a piece on either side.
; 5. The ending space already has a piece on it.

(define update-players (lambda (move pc side captures players)
			 (let* ((self (if (eq? (caar players) side)
				       (car players)
				       (cdr players)))
				(other (if (not (eq? (caar players) side))
					 (car players)
					 (cdr players)))
				(updated-self-pcs (cons (cons pc (cadr move))
							(filter (lambda (piece-id)
								  (if (or (not (eq? (cdr piece-id)
										    (car move)))
									  (not (eq? (find (lambda (pc1)
											    (if (eq? (cdr piece-id)
												     pc1)
											      #t
											      #f
											    ))
											  captures)
										    #f)))
								    #t
								    #f
								  ))
								(cadddr self))
							))
				(updated-other-pcs (filter (lambda (piece-id)
							     (let ((result (find (lambda (pc)
										   (if (eq? pc (cdr piece-id))
										     #t
										     #f
										     ))
										 captures
										 ))
								   )
							     (if (eq? result #f)
							       #t
							       #f
							       )
							     ))
							   (cadddr other)
							   ))
				)
			   (cons (list (car self)
				       (cadr self)
				       (caddr self)
				       updated-self-pcs
				       (cadr (cdddr self)))
				 (list (car other)
				       (cadr other)
				       (caddr other)
				       updated-other-pcs
				       (cadr (cdddr other)))
				 )
			   )
			 ))
; Returns a tuple containing the updated player data. Takes into affect both captures and moves.
