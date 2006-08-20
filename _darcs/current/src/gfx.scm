; One key idea to remember: the whole goal is to completely 
; separate the internal representation of the game from its display.
; All display functions must be 'read-only', not changing the game state
; in any way. We will write v. 1 to be ASCII only and v. 2 to use OpenGL
; to give a nicer display.

(require 'srfi-1) 
(include "initialize.scm")
; Libraries conditionally loaded.
; ncurses is the ncurses library, gl and glut load the OpenGL and GLUT toolkits, srfi-1 is
; the list library, srfi-4 is for homogenous floating point vectors, and lolevel allows us
; to use pointers (required for some OpenGL functions).

(include "initialize.scm")
(include "move.scm")
(include "ai.scm")
(include "savegame.scm")

(define NCURSES 0)
(define PLAINGL 1)

(define engine PLAINGL)

(define display-init #f)
; whether or not we have initialized the display.

; these define names for the color pairs used by ncurses
(define BLACK-ON-YELLOW 1)
(define WHITE-ON-YELLOW 2)
(define BLUE-ON-YELLOW 3)
(define RED-ON-BLACK 4)

(define initialize-display (lambda ()
		(if (eq? engine NCURSES)
		  (begin
		        (require 'ncurses) 

		        (initscr)
			(nonl)
			(keypad (stdscr) #t)	
			(cbreak)

		  	(start_color)
			(init_pair 1 COLOR_BLACK COLOR_YELLOW)
			; Color pair to be used to paint black's pieces

		        (init_pair 2 COLOR_WHITE COLOR_YELLOW)
			; Color pair to be used to paint white's pieces

			(init_pair 3 COLOR_BLUE COLOR_YELLOW)
			; Color pair to be used for empty spaces

			(init_pair 4 COLOR_RED COLOR_BLACK)
			; Color to be used for row labels.
			)
		)
		(if (eq? engine PLAINGL)
		  (begin
		  (require 'gl 'glut 'glu 'srfi-4 'lolevel 'srfi-18)
		  (include "display-lists.scm")
		  ; load the required libraries.

		  ; Variables for OpenGL.
		  (define CUBE-WIDTH 0.075)
		  (define PYRAMID-HEIGHT 0.15)
		  (define PYRAMID-WIDTH CUBE-WIDTH)
		  (define SPHERE-RADIUS (* 0.5 CUBE-WIDTH))
		  ; IDs for the textures used in the 3D engine.
		  (define PINE-TEXTURE 13)
		  (define GRANITE-TEXTURE 14)
		  (define MARBLE-TEXTURE 15)
		  (define USER 0)
		  (define LOCKED 1)

		  (define next-board '())
		  (define alpha 1.0)
		  (define progress 0.0)
		  (define slide-spc '()) 		   ; will hold a pair of tuples describing how the piece slides from one locale to another.
		  (define fade-out '())            ; define a list of pieces that need to be faded out.
		  (define brd (create-game-board)) ; game board storage for GL. Note that this will refer to the same physical data as the game state.
		  (define move-origin '()) 	 ; from and to data in the current move.
		  (define mode USER)		; if = user, the use can interact with the system. If = locked, then we are doing something
					  ; (AI calculations or animation), and the user cannot interact with the system.
		  (define camera-angle-delta 1) ; the amount a camera angle will change (in degrees) with each key press.
		  (define camera-coor (vector -0.4 0.0 0.0))
		  (define camera-angle 320)
		  (define camera-zoom 1.6875)
		  (define display-list-start 0)

		  (define ai-thinking #f)

		  (glut:InitDisplayMode (+ glut:DOUBLE glut:RGB glut:DEPTH))
		  (glut:CreateWindow "Latrunculi")

		  (create-display-lists)

		  (gl:Enable gl:TEXTURE_2D)
		  (gl:ClearDepth 1.0)
		  (gl:DepthFunc gl:LESS)
		  (gl:Enable gl:DEPTH_TEST)
		  (gl:ShadeModel gl:SMOOTH)
		  (gl:Enable gl:BLEND)
		  (gl:Hint gl:PERSPECTIVE_CORRECTION_HINT gl:NICEST)

		  (gl:BlendFunc gl:SRC_ALPHA gl:ONE_MINUS_SRC_ALPHA)

	          (gl:MatrixMode gl:PROJECTION)
		  (gl:Scalef camera-zoom camera-zoom camera-zoom)
	          (gl:Rotatef camera-angle 1.0 0.0 0.0)
		  ; Set the initial camera zoom and rotation.

		  (gl:ClearColor 0.40 1.0 0.20 0)
		  ; An ugly green for testing.

		  (glut:DisplayFunc render-state)

		  (glut:IdleFunc render-state)

		  (define dec-alpha (lambda ()
			  (set! alpha (- alpha 0.05))
			  (glut:PostRedisplay)

			  (if (> alpha 0)
			    (glut:TimerFunc 100 (lambda (value)
						 (dec-alpha)
						 )
					    1)
			    (begin
			      (set! alpha 1.0)
			      (set! brd next-board)
			      (set! mode USER)
			      (set! fade-out '())
			      )
			    )
		    ))

		  (define slide (lambda ()
				  (set! progress (+ progress 0.10))
				  (glut:PostRedisplay)

				  (if (< progress 1.0)
				    (glut:TimerFunc 100 (lambda (value)
							   (slide)
							   )
						     2)
				    (begin
				      (set! progress 0.0)
				      (set! brd next-board)
				      (set! mode USER)
				      (set! slide-spc '())

				      (define captured-vals (call-with-values (lambda () (affect-captures brd)) list))
				      (set! next-board (car captured-vals))
				      (set! fade-out (car (cdr captured-vals)))

				      (if (not (equal? fade-out '()))
					(begin
					  (set! mode LOCKED)
					  (glut:TimerFunc 100 (lambda (value)
								(dec-alpha)
							       )
							  1)
					  )
					)
				      )
				    )
				  ))

		  (glut:SpecialFunc (lambda (key x y)
				      (if (eq? mode USER)
					(begin

				       (if (eq? key glut:KEY_F7)
					 (begin
					   (save-game "game1.lsg" brd 0 player0 player1)
					   )
					 )

				       (if (eq? key glut:KEY_F8)
					 (begin
					   (define dat (call-with-values (lambda () 
									   (load-game "game1.lsg")
									   ) 
									 vector))

					   (set! brd (vector-ref dat 0))
					   (set! current-turn (vector-ref dat 1))
					   (set! player0 (vector-ref dat 2))
					   (set! player1 (vector-ref dat 3))

					   (glut:PostRedisplay)
					  )
					 )

				       (if (eq? key glut:KEY_RIGHT)
					 (begin
					   (vector-set! camera-coor 0 (+ .01 (vector-ref camera-coor 0)))
					   (glut:PostRedisplay)
					  )
					 )

				       (if (eq? key glut:KEY_LEFT)
					 (begin
					   (vector-set! camera-coor 0 (- (vector-ref camera-coor 0) .01))
					   (glut:PostRedisplay)
					  )
					 )

				       (if (eq? key glut:KEY_UP)
					 (begin
					   (vector-set! camera-coor 1 (+ (vector-ref camera-coor 1) .01))
					   (glut:PostRedisplay)
					  )
					 )

				       (if (eq? key glut:KEY_DOWN)
					 (begin
					   (vector-set! camera-coor 1 (- (vector-ref camera-coor 1) .01))
					   (glut:PostRedisplay)
					  )
					 )

				       (if (eq? key glut:KEY_PAGE_UP)
					 (begin
					   (gl:MatrixMode gl:PROJECTION)
					   (gl:Scalef 1.5 1.5 1.5)
					   (set! camera-zoom (* camera-zoom 1.50))
					   (glut:PostRedisplay)
					  )
					 )

				      (if (eq? key glut:KEY_PAGE_DOWN)
					(begin
					  (gl:MatrixMode gl:PROJECTION)
					  (gl:Scalef 0.75 0.75 0.75)
					  (set! camera-zoom (* camera-zoom 0.75))
					  (glut:PostRedisplay)
					 )
					)

				      (if (eq? key glut:KEY_HOME)
					(begin
					  (gl:MatrixMode gl:PROJECTION)
					  (gl:Rotatef camera-angle-delta 0.0 1.0 0.0)
					  (set! camera-angle (modulo (+ camera-angle 1) 360))
					  ; update the variable holding the camera angle. Used to determine space

					  (glut:PostRedisplay)
					 )
					)

				      (if (eq? key glut:KEY_END)
					(begin
					  (gl:MatrixMode gl:PROJECTION)
					  (gl:Rotatef (* -1 camera-angle-delta) 0.0 1.0 0.0)
					  (set! camera-angle (modulo (- camera-angle camera-angle-delta) 360))
					  (glut:PostRedisplay)
					 )
					)

				      (if (eq? key glut:KEY_INSERT)
					(begin
					  (gl:MatrixMode gl:PROJECTION)
					  (gl:Rotatef camera-angle-delta 1.0 0.0 0.0)
					  (set! camera-angle (modulo (+ camera-angle 1) 360))
					  (glut:PostRedisplay)
					  )
					)
				      )
				     )
				      ))

		  (glut:MouseFunc (lambda (button state x y)
				    (if (and (eq? button glut:LEFT_BUTTON)
					     (eq? state glut:UP)
					     (eq? mode USER))
				      (begin
					(define viewport (make-s32vector 4))
					(define pixel (make-u8vector 3 2))

					(gl:GetIntegerv gl:VIEWPORT viewport)

					(color-render)

				        (gl:ReadPixels x 
						       (- (s32vector-ref viewport 3) y) 
						       1 
						       1 
						       gl:RGB 
						       gl:UNSIGNED_BYTE 
						       (make-locative pixel)
						       )

				       (if (not (eq? (u8vector-ref pixel 0) 255)) 
					 (begin
					   (if (null? move-origin) ; if the first element in move, the origin, is the empty list...
					     (set! move-origin (cons (u8vector-ref pixel 0) (u8vector-ref pixel 1)))
					     (begin
					       (define move (cons move-origin
								  (list (cons (u8vector-ref pixel 0) (u8vector-ref pixel 1)))))

					       (if (move-valid? brd move WHITE) 
						 (begin
						   (set! next-board (move-piece brd move))
						   (set! slide-spc move)
						   (set! mode LOCKED)
						   (glut:TimerFunc 100 (lambda (value)
									 (slide)
									 )
								   2)
						   (glut:PostRedisplay)

						   (set! move-origin '())

						   (define ai-thread (make-thread (lambda ()
										    (if (eq? mode USER)
										      (define ai-mv (find-ai-move brd BLACK))
										      (define ai-mv (find-ai-move next-board BLACK))
										    )

										    (display "I choose: ")
										    (display ai-mv)
										    (newline)

										    (set! ai-thinking #t)

										    (set! mode LOCKED)

										    (set! next-board (move-piece brd ai-mv))
										    (set! slide-spc ai-mv)

										    (glut:TimerFunc 100 (lambda (value)
													  (slide)
													  )
												    2)

										    (set! ai-thinking #f)
										    (glut:PostRedisplay)
										    )
										  ))

						   (thread-quantum-set! ai-thread 300)
						   (thread-start! ai-thread)
						  )
						 (begin
						   (set! move-origin '())
						   (display "illegal move")
						   (newline)
						  )
						 )
					       )
					     )
					   ))

				       (gl:Enable gl:TEXTURE_2D)
				       (gl:Enable gl:DITHER)
				       (gl:Enable gl:BLEND)
				      )
				     )
				    ))

		     (define main-thread (thread-start! (make-thread (lambda ()
								       (glut:MainLoop)
								       ))))
		     (thread-join! main-thread)
		   )
		  )

		(set! display-init #t)
			     ))

(define color-render (lambda ()
			(gl:Disable gl:TEXTURE_2D)
			(gl:Disable gl:DITHER)
			(gl:Disable gl:LIGHTING)
			(gl:Disable gl:BLEND)

			(gl:ClearColor 1 1 1 0)
			(gl:Clear (+ gl:COLOR_BUFFER_BIT gl:DEPTH_BUFFER_BIT))

			(gl:MatrixMode gl:MODELVIEW)
			(gl:LoadIdentity)
			(gl:Translatef (vector-ref camera-coor 0)
				       (vector-ref camera-coor 1)
				       (vector-ref camera-coor 2))

			(vector-for-each (lambda (j row)
					   (vector-for-each (lambda (i col)
					   ; The name of a given space will be the sum of its logical coordinates.

					   (gl:Color3ub i j 0)

					   (gl:Translatef (* -0.5 CUBE-WIDTH) (* -0.5 CUBE-WIDTH) (* -0.5 CUBE-WIDTH))
					   (gl:CallList display-list-start) 
					   (gl:Translatef (* 0.5 CUBE-WIDTH) (* 0.5 CUBE-WIDTH) (* 0.5 CUBE-WIDTH))

					   (if (not (eq? col EMPTY))
					     (begin
					       (gl:Translatef 0.0 ( * 0.5 CUBE-WIDTH) 0.0)

					       (if (eq? col WHITE_KING)
						 (gl:CallList (+ display-list-start 1))
						 )
					       (if (eq? col BLACK_KING)
						 (gl:CallList (+ display-list-start 2))
					       )
					       (if (eq? col WHITE_PAWN)
						 (begin
						   (gl:Translatef 0.0 (* 0.25 CUBE-WIDTH) 0.0)
						   (gl:CallList (+ display-list-start 3))
						   (gl:Translatef 0.0 (* -0.25 CUBE-WIDTH) 0.0)
						 )
						 )
					       (if (eq? col BLACK_PAWN)
						 (begin
						   (gl:Translatef 0.0 (* 0.25 CUBE-WIDTH) 0.0)
						   (gl:CallList (+ display-list-start 4))
						   (gl:Translatef 0.0 (* -0.25 CUBE-WIDTH) 0.0)
						   )
						 )

					       (gl:Translatef 0.0 ( * -0.5 CUBE-WIDTH) 0.0)
					     )

					     )
					   (gl:Translatef CUBE-WIDTH 0.0 0.0)
					   )
					 row)
			(gl:Translatef (* -1 COLS CUBE-WIDTH) 0.0 (* -1 CUBE-WIDTH))
			)
			brd
			      )

			(gl:Flush)

		        (gl:ClearColor 0.40 1.0 0.20 0)
		       ))

(define render-state (lambda ()
		       (gl:MatrixMode gl:MODELVIEW)
		       (gl:LoadIdentity)
		       (gl:Translatef (vector-ref camera-coor 0)
				      (vector-ref camera-coor 1)
				      (vector-ref camera-coor 2))

		       (gl:Clear (+ gl:COLOR_BUFFER_BIT gl:DEPTH_BUFFER_BIT))

		       (vector-for-each (lambda (j row)
					  (vector-for-each (lambda (i col)
							     (define space (cons i j))

							     (gl:Translatef (* -0.5 CUBE-WIDTH) (* -0.5 CUBE-WIDTH) (* -0.5 CUBE-WIDTH))
							     (gl:CallList display-list-start) 
							     (gl:Translatef (* 0.5 CUBE-WIDTH) (* 0.5 CUBE-WIDTH) (* 0.5 CUBE-WIDTH))

							     (if (and (not (equal? mode USER))
								      (not (null? (filter (lambda (k)
											    (if (equal? space k)
											      #t
											      #f))
											  fade-out))))
							        (gl:Color4f 1.0 1.0 1.0 alpha)
							     )

							     (if (and (not (equal? mode USER))
								      (not (null? slide-spc))
								      (equal? (car slide-spc) space))
							       (begin
								 (define from (car slide-spc))
								 (define to (car (cdr slide-spc)))

								 (define delta-x (- (car from) (car to)))
								 (define delta-y (- (cdr from) (cdr to)))

								 (if (not (eq? delta-x 0)) ; if the move is horizontal
								     (gl:Translatef (* -1 progress CUBE-WIDTH delta-x) 0.0 0.0)
								     (gl:Translatef 0.0 0.0 (* progress CUBE-WIDTH delta-y))
								 )
								)
							       )
							       ; Assumes a correct move (i.e. no diagonal)

							     (if (not (eq? col EMPTY))
							       (begin
								 (gl:Translatef 0.0 ( * 0.5 CUBE-WIDTH) 0.0)

								 (if (eq? col WHITE_KING)
								   (gl:CallList (+ display-list-start 1))
								   )

								 (if (eq? col BLACK_KING)
								   (gl:CallList (+ display-list-start 2))
								 )

								 (if (eq? col WHITE_PAWN)
								   (begin
								     (gl:Translatef 0.0 (* 0.25 CUBE-WIDTH) 0.0)
								     (gl:CallList (+ display-list-start 3))
								     (gl:Translatef 0.0 (* -0.25 CUBE-WIDTH) 0.0)
								    )
								   )

								 (if (eq? col BLACK_PAWN)
								   (begin
								     (gl:Translatef 0.0 (* 0.25 CUBE-WIDTH) 0.0)
								     (gl:CallList (+ display-list-start 4))
								     (gl:Translatef 0.0 (* -0.25 CUBE-WIDTH) 0.0)
								     )
								   )

								 (gl:Translatef 0.0 ( * -0.5 CUBE-WIDTH) 0.0)
							       )

							       )
							     (if (and (not (equal? mode USER))
								      (not (null? slide-spc))
								      (equal? (car slide-spc) space))
							       (begin
								 (define from (car slide-spc))
								 (define to (car (cdr slide-spc)))

								 (define delta-x (- (car from) (car to)))
								 (define delta-y (- (cdr from) (cdr to)))

								 (if (not (eq? delta-x 0))
								     (gl:Translatef (* progress CUBE-WIDTH delta-x) 0.0 0.0)
								     (gl:Translatef 0.0 0.0 (* -1 progress CUBE-WIDTH delta-y))
								 )
								)
							       )

							     (gl:Translatef CUBE-WIDTH 0.0 0.0)
							     
							     (gl:Color4f 1.0 1.0 1.0 1.0)
							     )
							   row)
					  (gl:Translatef (* -1 COLS CUBE-WIDTH) 0.0 (* -1 CUBE-WIDTH))
					  )
					  brd
					)

		       (glut:SwapBuffers)
		       ))

(define display-current-state (lambda (board)
		(if (eq? engine NCURSES)
		  (begin
			(wclear (stdscr))

			(if (eq? current-turn AI)
			  (attron A_BOLD))

			(printw (car (cdr player0)))

			(if (eq? current-turn AI)
			  (attroff A_BOLD))

			(attron (COLOR_PAIR RED-ON-BLACK))
			(move 1 3)
			;(map (lambda (num)
			;       (printw (number->string num))
			;       (move 1 (* num 3))
			;       )
			;     (list-tabulate COLS values))
			(attroff (COLOR_PAIR RED-ON-BLACK))

			; Put column labels down
			(printw "\n")

			(draw-board board 2)
			(if (eq? current-turn HUMAN)
			  (attron A_BOLD))

			(printw "\n")
			(printw (car (cdr player1)))

			(if (eq? current-turn HUMAN)
			  (attroff A_BOLD))

			(wrefresh (stdscr))
			(getch)

			(endwin)
		   )
		)
		(if (eq? engine PLAINGL)
		  (begin
		    (if display-init
		      (begin
			    (set! brd board)
			)
		     )
		    )
		  )
	))

(define draw-board (lambda (board y)
		     (vector-for-each (lambda (i row)
			      (attron (COLOR_PAIR RED-ON-BLACK))
			      (printw (number->string i))
			      (move y 1)
			      (attroff (COLOR_PAIR RED-ON-BLACK))

			      (draw-row row 1 y)
			      (printw "\n")
			      (set! y (+ y 1))
			    )
			  board)
		  	))

(define draw-row (lambda (row x y)
		  (define PAWN "[*]")
		  (define KING "[^]")
		  (define BLANK "| |")

		  (vector-for-each (lambda (i curr_piece)
		      ;(define curr_piece c)
		      (move y x)		     

		      (if (eq? EMPTY curr_piece)
			(begin
			 (attron (COLOR_PAIR WHITE-ON-YELLOW))
			 (printw BLANK)
			 (attroff (COLOR_PAIR WHITE-ON-YELLOW))
			))
		      (if (eq? BLACK_PAWN curr_piece)
			(begin
			(attron (COLOR_PAIR BLACK-ON-YELLOW))
			(printw PAWN)
			(attroff (COLOR_PAIR BLACK-ON-YELLOW))
			)
		      )
		      (if (eq? WHITE_PAWN curr_piece)
			(begin
			(attron (COLOR_PAIR BLUE-ON-YELLOW))
			(printw PAWN)
			(attroff (COLOR_PAIR BLUE-ON-YELLOW))
			)
			)
		      (if (eq? BLACK_KING curr_piece)
			(begin
			(attron (COLOR_PAIR BLACK-ON-YELLOW))
			(printw KING)
			(attroff (COLOR_PAIR BLACK-ON-YELLOW))
			)
			)
		      (if (eq? WHITE_KING curr_piece)
			(begin
			(attron (COLOR_PAIR BLUE-ON-YELLOW))
			(printw KING)
			(attroff (COLOR_PAIR BLUE-ON-YELLOW))
			))

		    (set! x (+ x 3))
		   )
		       row)
	))

(define get-player-input (lambda ()
			   ;(nodelay (stdscr) #\t)
			   (move 14 0)
			   (attron A_BOLD)
			   (printw "Input your move in the form: X1 Y1 X2 Y2")
			   (attroff A_BOLD)
			   (move 15 0)
			   (define str "XX YY XX YY")
			   (getstr str)
			   str
			   ))
