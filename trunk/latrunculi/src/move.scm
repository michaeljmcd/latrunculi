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
			  (define new-board '#())

			  (vector-for-each (lambda (i row)
					     (set! new-board (vector-append new-board (vector (vector-copy row))))
					     )
					   board)
			  new-board
			  ))
; Creates a newly-allocated deep copy of board.

(define affect-captures (lambda (board)
     (define is-surrounded-horizontally? (lambda (board space)
					 (define this-cell (get-cell board space))
					 (define side (modulo this-cell 2))

					 (define space-after (cons (+ (car space) 1) (cdr space))) 
					 ; the coordinates of the space to the right of the space under examination.

					 (define space-before (cons (- (car space) 1) (cdr space)))
					 ; The coordinates of the space to the left of the space under examination.

					 (define side-after (modulo (get-cell board space-after) 2))
					 (define side-before (modulo (get-cell board space-before) 2))
					 ; The side (black or white) of the piece on the left and the right.

					 (if (and (not (eq? (car space) 0))
						  (not (eq? (car space) (- COLS 1))) ; can't capture a piece on the sides
						  (not (eq? (get-cell board space-after) EMPTY))
						  (not (eq? (get-cell board space-before) EMPTY))
						  (not (eq? side side-after))
						  (not (eq? side side-before))
						  (not (eq? this-cell EMPTY))
						  (not (eq? this-cell BLACK_KING))
						  (not (eq? this-cell WHITE_KING))
						  )
					   #t
					   #f
					   )
					 ))

     (define is-surrounded-vertically? (lambda (board space)
					 (define this-cell (get-cell board space))
					 (define side (modulo this-cell 2))

					 (define space-after (cons (car space) (+ (cdr space) 1)))
					 (define space-before (cons (car space) (- (cdr space) 1)))

					 (define side-after (modulo (get-cell board space-after) 2))
					 (define side-before (modulo (get-cell board space-before) 2))

					 (if (and (not (eq? (cdr space) 0))
						  (not (eq? (cdr space) (sub1 ROWS)))
						  (not (eq? side side-after))
						  (not (eq? side side-before))
						  (not (eq? (get-cell board space-after) EMPTY))
						  (not (eq? (get-cell board space-before) EMPTY))
						  (not (eq? this-cell EMPTY))
						  (not (eq? this-cell BLACK_KING))
						  (not (eq? this-cell WHITE_KING))
						  )
					   #t
					   #f
					   )
					 ))
     
	(define is-cornered? (lambda (board space)
			       (define UPPER_LEFT (cons '0 '0))
			       (define UPPER_RIGHT (cons (sub1 COLS) '0))
			       (define LOWER_LEFT (cons '0 (sub1 ROWS)))
			       (define LOWER_RIGHT (cons (sub1 COLS) (sub1 ROWS)))

			       (define corner (get-cell board space))
			       (define space-side (modulo corner 2))

			       (define caught #f)

			       (when (equal? space UPPER_LEFT)
					(define below (get-cell board (cons '0 '1)))
					(define right (get-cell board (cons '1 '0)))

					(define below-side (modulo below 2))
					(define right-side (modulo right 2))

					(if (and (not (eq? EMPTY below))
						 (not (eq? EMPTY right))
						 (not (eq? EMPTY corner))
						 (not (eq? below-side space-side))
						 (not (eq? right-side space-side)))
					  (set! caught #t)
					  (set! caught #f)
					  )
				)

			       (when (equal? space LOWER_LEFT)
					(define above (get-cell board (cons '0 (- ROWS 2))))
					(define right (get-cell board (cons '1 (- ROWS 1))))

					(define above-side (modulo above 2))
					(define right-side (modulo right 2))

					(if (and (not (eq? EMPTY above))
						 (not (eq? EMPTY right))
						 (not (eq? EMPTY corner))
						 (not (eq? above-side space-side))
						 (not (eq? right-side space-side)))
					  (set! caught #t)
					  (set! caught #f)
					)
				)

			       (when (equal? space LOWER_RIGHT)
					(define above (get-cell board (cons (sub1 COLS) (- ROWS 2))))
					(define left (get-cell board (cons (- COLS 2) (sub1 ROWS))))

					(define above-side (modulo above 2))
					(define left-side (modulo left 2))

					(if (and (not (eq? EMPTY above))
						 (not (eq? EMPTY left))
						 (not (eq? EMPTY corner))
						 (not (eq? above-side space-side))
						 (not (eq? left-side space-side)))
					  (set! caught #t)
					  (set! caught #f)
					)
				 )
			       
			       (when (equal? space UPPER_RIGHT)
					(define below (get-cell board (cons (sub1 COLS) '1)))
					(define left (get-cell board (cons (- COLS 2) '0)))

					(define below-side (modulo below 2))
					(define left-side (modulo left 2))

					(if (and (not (eq? EMPTY below))
						 (not (eq? EMPTY left))
						 (not (eq? EMPTY corner))
						 (not (eq? below-side space-side))
						 (not (eq? left-side space-side)))
					  (set! caught #t)
					  (set! caught #f)
					  )
				 )

			       (if (or (eq? corner WHITE_KING)
				       (eq? corner BLACK_KING))
				 (set! caught #f))

			       caught
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

	(define captured-pieces '())

	(vector-for-each (lambda (y row)
		      (vector-for-each (lambda (x col)
				    (define pt (cons x y))

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
				      )
				    )
				  row)
		      )
		    board)

	(values board captured-pieces)
			  ))
; This function removes any pieces that ought to be captured. The rules regarding captures are as follows:
; 1. If a pawn is surrounded on two opposite sides, it is captured.
; 2. An outside wall cannot be used to capture a pawn.
; 3. A stone in a corner can be taken by placing two stones across the corner.
; 4. Multiple stones may be captured along a line (really doesn't change anything, but should be stated).

(define replace-space (lambda (board space replacement)
		      (define piece-cleared (get-cell board space))
		      (define updated-board (duplicate-board board))
v v v v v v v
		      ;(define updated-board board)
^ ^ ^ ^ ^ ^ ^
		      (vector-set! (vector-ref updated-board (cdr space)) (car space) replacement)

		      (values updated-board piece-cleared)
	      ))
; Clears the space. Returns the modified board and the tuple describing the piece
; removed from the board. 

(define move-piece (lambda (board delta)
			(define board-dup (duplicate-board board))
v v v v v v v
			;(define board-dup board)
^ ^ ^ ^ ^ ^ ^
			(define after-move
				(call-with-values (lambda ()
						    (replace-space board-dup (car delta) EMPTY)
						    )
						  (lambda (brd pc)
						    (replace-space brd (car (cdr delta)) pc)
						    )
						  )
			)
			after-move
		     ))

(define make-move (lambda (board delta)
		    (define after-move (move-piece board delta))
		    (affect-captures (duplicate-board after-move))
v v v v v v v
		    ;(affect-captures after-move)
^ ^ ^ ^ ^ ^ ^
		    ))
; Applies change delta (of the form: ((X1 . Y1) . (X2 . Y2))) to the given board board. Must take captures
; into affect. Will be used for both AI moves and player moves. AI moves verified during generation, whereas
; player moves will have to be verified in another function. This function does no verification. It simply makes
; the move and, if a piece is captured, removes it.

v v v v v v v
(define unmake-move (lambda (board delta)
		      (make-move board (cons (car (cdr delta))
					     (list (car delta)))
				 )
		      ))
; Given a move delta, unmake-move reverses the move.

^ ^ ^ ^ ^ ^ ^
(define move-valid? (lambda (board delta side)
		      (define jumped? (lambda (board delta)
					; We need to get a list/vector of all the spaces in between the start and the 
					; destination (non-inclusive) and check them for a space.
					      (define ans #f)

					      (define diff-x (- (car (car (cdr delta)))
								      (car (car delta))))

					      (define delta-x (abs diff-x))
					      ; The absolute value of the change in the x-direction

					      (define diff-y (- (cdr (car (cdr delta)))
								      (cdr (car delta))))
					      (define delta-y (abs diff-y))

					      ; At this point, the move is presumed valid in all other respects.
					      ; If you only move one space, you can't possible have jumped another.
					      (if (> delta-x 1)
						(begin
						  (define x-start (min (car (car delta))
								       (car (car (cdr delta))
									    )))
						  (define y-coor (cdr (car delta)))

						  (define x-list (map (lambda (e)
									(get-cell board (cons (+ e x-start 1) y-coor))
									)
								      (list-tabulate (- delta-x 1) values)))

						  (if (> (length (filter (lambda (i)
									   (if (> i 0)
									     #f
									     #t))
									 x-list))
							 0)
						    (set! ans #f)
						    (set! ans #t)
						    )
						  )
						(if (> delta-y 1)
						  (begin
						  (define y-start (min (cdr (car delta))
								       (cdr (car (cdr delta)))
								       ))
						  (define x-coor (car (car delta)))
						  (define y-list (map (lambda (e)
									(get-cell board (cons x-coor (+ e y-start 1)))
									)
								      (list-tabulate (- delta-y 1) values)))

						  (if (and (not (null? y-list))
							(> (length (filter (lambda (i)
									   (if (> i 0)
									     #f
									     #t))
									 y-list))
							 0))
						    (set! ans #f)
						    (set! ans #t)
						    )
						  )
						 )
						)

					      ans
					))

		      (define space (get-cell board (car delta)))
		      ; move's origin.

		      (define end (get-cell board (car (cdr delta))))
		      ; move's destination.

		      (define delta-x (abs (- (car (car (cdr delta)))
					      (car (car delta)))
					   ))
		      ; The absolute value of the change in the x-direction

		      (define delta-y (abs (- (cdr (car (cdr delta)))
					      (cdr (car delta)))
					   ))
		      ; The absolute value of the change in the y-direction
		     
		      (if (or (eq? space EMPTY)   ; trying to move an empty space
			      (not (eq? end EMPTY)) ; trying to move to an occupied space
			      (and (> delta-x 0)
				   (> delta-y 0)) ; trying to move diagonally.
			      (not (eq? side (modulo space 2))) ; trying to move an opposing piece.
			      (jumped? board delta) 	; jumped a piece.
			      )
			#f
			#t
			)
			      ))
; Given a move, delta, using the same form as above, validate-move determines whether or not the given move is legal.
; This is needed to check user moves, though not AI moves (because the move generator will only generate legal moves
; anyway). A move is illegal under the following conditions:
; 1. The starting space is empty.
; 2. The starting space contains an opposing piece.
; 3. The move is diagonal (i.e. /\x > 0 AND /\ y > 0)
; 4. The move jumps over a piece on either side.
; 5. The ending space already has a piece on it.
