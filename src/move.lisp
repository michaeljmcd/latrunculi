; move.scm is to hold the move-related functions, such as making a move and validating one.
; The reason for this is so that the click events in the OpenGL code can access these functions without having to include the rest of the AI code.

(in-package #:latrunculi)

(defun sub1 (num) (- num 1))
(defun add1 (num) (+ num 1))

(defun get-cell (board coor)
		   (if (or (>= (cdr coor) +ROWS+)
			   (>= (car coor) +COLS+)
			   (< (cdr coor) 0)
			   (< (car coor) 0))
		     +EMPTY+
             (aref board (cdr coor) (car coor))
		   ))

; Creates a newly-allocated deep copy of board.
(defun duplicate-board (board)
  (let ((fresh-board (make-array '(8 12) :element-type 'integer)))
    (loop for i from 0 to (- +ROWS+ 1)
          do (loop for j from 0 to (- +COLS+ 1)
                do (setf (aref fresh-board i j) (aref board i j))
                )
          )
    fresh-board
    )
  )

(defun is-surrounded-horizontally? (board space)
				   (let* ((this-cell (get-cell board space))
					  (side (mod this-cell 2)) 
					  (space-after (cons (+ (car space) 1) (cdr space))) 
					  ; the coordinates of the space to the right of the space under examination.  

					  (space-before (cons (- (car space) 1) (cdr space)))
					  ; The coordinates of the space to the left of the space under examination.  
					  (side-after (mod (get-cell board space-after) 2))
					  (side-before (mod (get-cell board space-before) 2))
					  ; The side (black or white) of the piece on the left and the right.  
					  )
				     (if (and (not (eql (car space) 0))
					      (not (eql (car space) (- +COLS+ 1))) ; can't capture a piece on the sides
					      (not (eql (get-cell board space-after) +EMPTY+))
					      (not (eql (get-cell board space-before) +EMPTY+)) 
					      (not (eql side side-after)) 
					      (not (eql side side-before))
					      (not (eql this-cell +EMPTY+))
					      (not (eql this-cell +BLACK_KING+)) 
					      (not (eql this-cell +WHITE_KING+)))
				       t
				       nil
				       )
				     ))

(defun is-surrounded-vertically? (board space)
				    (let* ((this-cell (get-cell board space))
					   (side (mod this-cell 2)) 
					   (space-after (cons (car space) (+ (cdr space) 1)))
					   (space-before (cons (car space) (- (cdr space) 1))) 
					   (side-after (mod (get-cell board space-after) 2)) 
					   (side-before (mod (get-cell board space-before) 2))) 
				      
				      (if (and (not (eql (cdr space) 0))
					       (not (eql (cdr space) (sub1 +ROWS+)))
					       (not (eql side side-after))
					       (not (eql side side-before))
					       (not (eql (get-cell board space-after) +EMPTY+))
					       (not (eql (get-cell board space-before) +EMPTY+))
					       (not (eql this-cell +EMPTY+))
					       (not (eql this-cell +BLACK_KING+))
					       (not (eql this-cell +WHITE_KING+))) 
					t
					nil
					)
				    ))

(defun is-corner? (space)
		  (if (or (and (eql (cdr space) 0) 
			   (or (eql (car space) 0)
			       (eql (car space) (sub1 +COLS+)))
			   ) ; deals with the upper two corners.
			  (and (eql (cdr space) (sub1 +ROWS+))
			       (or (eql (car space) 0)
				   (eql (car space) (sub1 +COLS+))))
			  )
		    t
		    nil
		  ))

(defun is-cornered? (board space)
		       (let* ((UPPER_LEFT (cons '0 '0))
			      (UPPER_RIGHT (cons (sub1 +COLS+) '0))
			      (LOWER_LEFT (cons '0 (sub1 +ROWS+)))
			      (LOWER_RIGHT (cons (sub1 +COLS+) (sub1 +ROWS+))) 
			      (corner (get-cell board space))
			      (space-side (mod corner 2)) 
			      (caught nil)) 

			 (when (eql space UPPER_LEFT)
			   (let* ((below (get-cell board (cons '0 '1)))
				  (right (get-cell board (cons '1 '0))) 
				  (below-side (mod below 2))
				  (right-side (mod right 2))) 
			     
			     (if (and (not (eql +EMPTY+ below)) 
				      (not (eql +EMPTY+ right))
				      (not (eql +EMPTY+ corner))
				      (not (eql below-side space-side))
				      (not (eql right-side space-side)))
			       (setq caught t) 
			       (setq caught nil)
			       )
			     ))

		       (when (eql space LOWER_LEFT)
			 (let* ((above (get-cell board (cons '0 (- +ROWS+ 2))))
				(right (get-cell board (cons '1 (- +ROWS+ 1)))) 
				(above-side (mod above 2))
				(right-side (mod right 2))) 
			   
			   (if (and (not (eql +EMPTY+ above))
				    (not (eql +EMPTY+ right))
				    (not (eql +EMPTY+ corner))
				    (not (eql above-side space-side))
				    (not (eql right-side space-side)))
			     (setq caught t)
			     (setq caught nil))
			   ))

		       (when (eql space LOWER_RIGHT)
			 (let* ((above (get-cell board (cons (sub1 +COLS+) (- +ROWS+ 2))))
				(left (get-cell board (cons (- +COLS+ 2) (sub1 +ROWS+)))) 
				(above-side (mod above 2))
				(left-side (mod left 2))) 
			   
			   (if (and (not (eql +EMPTY+ above))
				    (not (eql +EMPTY+ left))
				    (not (eql +EMPTY+ corner))
				    (not (eql above-side space-side))
				    (not (eql left-side space-side))) 
			     (setq caught t)
			     (setq caught nil))
			   ))
		       
		       (when (eql space UPPER_RIGHT)
			 (let* ((below (get-cell board (cons (sub1 +COLS+) '1)))
				(left (get-cell board (cons (- +COLS+ 2) '0))) 
				(below-side (mod below 2)) 
				(left-side (mod left 2)) ) 
			   
			   (if (and (not (eql +EMPTY+ below))
				    (not (eql +EMPTY+ left))
				    (not (eql +EMPTY+ corner))
				    (not (eql below-side space-side)) 
				    (not (eql left-side space-side)))
			     (setq caught t)
			     (setq caught nil))
			   ))

		       (if (or (eql corner +WHITE_KING+)
			       (eql corner +BLACK_KING+))
			 (setq caught nil))

		       caught
		       ))

(defun affect-captures (board players)
  (let ((captured-pieces (find-if (lambda (piece)
                                    (let ((pt (cdr piece)))
                                        (if (or (and (not (is-corner? pt))
                                                     (or (is-surrounded-horizontally? board pt)
                                                         (is-surrounded-vertically? board pt)
                                                         (is-cornered? board pt)))
                                                (and (is-corner? pt) (is-cornered? board pt)))
                                          t
                                          nil)
                                        )
                                    )
                              (append (piece-list (car players)) (piece-list (cdr players))))
                         ))
    (if (> (length captured-pieces) 0)
        (values (cdr (last (map 'list (lambda (piece) (replace-space board (cdr piece) +EMPTY+)) captured-pieces)))
                captured-pieces)
        (values board captured-pieces))
  ))

; This function removes any pieces that ought to be captured. The rules regarding captures are as follows:
; 1. If a pawn is surrounded on two opposite sides, it is captured.
; 2. An outside wall cannot be used to capture a pawn.
; 3. A stone in a corner can be taken by placing two stones across the corner.
; 4. Multiple stones may be captured along a line (really doesn't change anything, but should be stated).

(defun replace-space (board space replacement)
  (let ((piece-cleared (get-cell board space))
        (updated-board (duplicate-board board)))
    (setf (aref updated-board (cdr space) (car space)) replacement)
    (values updated-board piece-cleared)
    )
  )
; Clears the space. Returns the modified board and the tuple describing the piece
; removed from the board. 

(defun move-piece (board delta)
  (multiple-value-bind (new-board piece) 
    (replace-space (duplicate-board board) (car delta) +EMPTY+) 
    (replace-space new-board (cadr delta) piece)
    )
  )

(defun make-move (board delta players)
  (multiple-value-bind (new-board)
    (move-piece board delta)
    (affect-captures new-board players))
  )
; Applies change delta (of the form: ((X1 . Y1) . (X2 . Y2))) to the given board board. Must take captures
; into affect. Will be used for both AI moves and player moves. AI moves verified during generation, whereas
; player moves will have to be verified in another function. This function does no verification. It simply makes
; the move and, if a piece is captured, removes it.

(defun unmake-move (board delta players)
		      (make-move board (cons (car (cdr delta))
                                     (list (car delta)) 
                                     players)
		      ))
; Given a move delta, unmake-move reverses the move.

; returns whether or not a given path includes some piece between the start and end points
; We need to get a list of all the spaces in between the start and the 
; destination (non-inclusive) and check them for a piece.
(defun jumped? (board delta)
  (let ((start-x (min (caadr delta) (caar delta)))
        (end-x (max (caadr delta (caar delta))))
        (start-y (min (cdadr delta) (cdar delta)))
        (end-y (max (cdadr delta) (cdar delta)))
        (answer nil))
    (loop for i from start-x to end-x
              do (loop for j from start-y to end-y
                    do (if (not (eql (aref board i j) +EMPTY+))
                         (setq answer t)
                         )
                    )
              )
    answer
    )
  )

(defun move-valid? (board delta side)
		      (let ((space (get-cell board (car delta))) ; move's origin.  
			    (end (get-cell board (car (cdr delta)))) ; move's destination.  
			    (delta-x (abs (- (car (car (cdr delta)))
					     (caar delta))
					  )) ; The absolute value of the change in the x-direction 
			    (delta-y (abs (- (cdr (car (cdr delta)))
					     (cdr (car delta)))
					  )) ; The absolute value of the change in the y-direction
			    ) 
			(if (or (eql space +EMPTY+)   ; trying to move an empty space
				(not (eql end +EMPTY+)) ; trying to move to an occupied space (and (> delta-x 0)
				(and (> delta-y 0) ; trying to move diagonally.
				     (> delta-x 0))
				(not (eql side (mod space 2))) ; trying to move an opposing piece.
				(jumped? board delta) 	; jumped a piece.
			      )
			  nil
			  t
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

; Returns a tuple containing the updated player data. Takes into affect both captures and moves.
(defun update-players (captures players)
  (cons (update-player captures (car players)) 
        (update-player captures (cdr players))) 
  )

(defun update-player (captures player)
  (let ((self (deep-copy player)))
    (setf (slot-value self 'pieces) 
          (filter-captured-pieces (slot-value self 'pieces) captures))
    player
    )
  )

(defun filter-captured-pieces (piece-list captures)
  (let ((captured? (lambda (piece)
                     (if (eql (find-if (lambda (pc)
                                         (if (eql pc piece)
                                           t
                                           nil)
                                         ) 
                                       captures)
                              nil)
                       nil
                       t
                       )
                     )
                   ))
    (find-if-not captured? piece-list)
    )
  )
