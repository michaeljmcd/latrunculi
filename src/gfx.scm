(require 'srfi-1 'gl 'glut 'glu 'srfi-4 'lolevel 'srfi-18 'glf)
; gl and glut load the OpenGL and GLUT toolkits, srfi-1 is
; the list library, srfi-4 is for homogenous floating point vectors, and lolevel allows us
; to use pointers (required for some OpenGL functions).

(include "display-lists.scm")
(include "initialize.scm")
(include "move.scm")
(include "ai.scm")
(include "savegame.scm")
(include "targa.scm")

(define USER 0)
(define LOCKED 1)

(define display-list-start 0)
(define camera-angle-delta 1) ; the amount a camera angle will change (in degrees) with each key press.
(define camera-angle 320)
(define camera-zoom 1.6875)
(define camera-coor (vector -0.4 0.0 0.0))
(define brd (create-game-board)) 
(define mode USER)		; if = user, the user can interact with the system. If = locked, then we are doing something ; (AI calculations or animation), and the user cannot interact with the system.
(define fade-out '())            ; define a list of pieces that need to be faded out (i.e. have been captured)
(define slide-spc '()) 		   ; will hold a pair of tuples describing how the piece slides from one locale to another.
(define alpha 1.0)
(define progress 0.0)
(define next-board '())
(define move-origin '()) 	 ; from and to data in the current move.
(define board-mutex (make-mutex))

(define initialize-display (lambda ()
			     (glut:InitDisplayMode (+ glut:DOUBLE glut:RGB glut:DEPTH))
			     
			     (glut:InitWindowSize 640 480)

		             (glut:CreateWindow "Latrunculi") 

			     (gl:Enable gl:TEXTURE_2D)
			     (gl:ClearDepth 1.0)
			     (gl:Enable gl:DEPTH_TEST)
			     (gl:ShadeModel gl:SMOOTH)
			     (gl:Enable gl:BLEND)
			     (gl:Hint gl:PERSPECTIVE_CORRECTION_HINT gl:NICEST) 
			     (gl:BlendFunc gl:SRC_ALPHA gl:ONE_MINUS_SRC_ALPHA) 
			     ; Initialize OpenGL

			     (glfInit)
			     (glfLoadFont "../fonts/gothic1.glf")
			     (glfEnable (GLF-CONTOURING))
			     (gl:Enable gl:LINE_SMOOTH)
			     ; Initialize GLF
		))

(define show-menu (lambda ()
		    (let* ((menu-img (tga-data "../img/petteia8.tga")) 
			   (menu-pix (list->u8vector (vector->list (vector-ref menu-img 3))))
			   (MENU-TEXTURE 12)
			   (menu-display (lambda ()
				          (gl:Clear (+ gl:COLOR_BUFFER_BIT gl:DEPTH_BUFFER_BIT))
					     (gl:BindTexture gl:TEXTURE_2D MENU-TEXTURE)
					     (gl:Begin gl:QUADS)
					     	(gl:TexCoord2i 0 0)
					     	(gl:Vertex3i -1 -1 -1)

						(gl:TexCoord2i 1 0)
						(gl:Vertex3i 1 -1 -1)

						(gl:TexCoord2i 1 1)
						(gl:Vertex3i 1 1 -1)

						(gl:TexCoord2i 0 1)
						(gl:Vertex3i -1 1 -1)
					     (gl:End)
					  (gl:PushMatrix)

					  (gl:LoadIdentity)
					  (gl:Disable gl:TEXTURE_2D)

					  (gl:Translatef -0.3 0.75 0.0)
					  (gl:Scalef 0.1 0.1 0.1)

					  (gl:Color3f 0.9 0.45 0.19)
					  (glfDrawSolidString "Latrunculi")
					  (gl:Color3f 0.0 0.0 0.0)
					  (glfDrawWiredString "Latrunculi")
					  (gl:Translatef -5.5 -8.5 0.0)

					  (gl:Color3f 1.0 0.0 0.0)
					  (glfDrawSolidString "New Game")
					  (gl:Color3f 0.0 0.0 0.0)
					  (glfDrawWiredString "New Game")
					  (gl:Translatef 0.0 -3.0 0.0)

					  (gl:Color3f 1.0 0.0 0.0)
					  (glfDrawSolidString "Load Game")
					  (gl:Color3f 0.0 0.0 0.0)
					  (glfDrawWiredString "Load Game")
					  (gl:Translatef 0.0 -3.0 0.0)

					  (gl:Color3f 1.0 0.0 0.0)
					  (glfDrawSolidString "Exit")
					  (gl:Color3f 0.0 0.0 0.0)
					  (glfDrawWiredString "Exit")

					  (gl:Enable gl:TEXTURE_2D)
					  (gl:PopMatrix)

					  (gl:Flush)
					  (glut:SwapBuffers)
					  ))
			  (mouse-handler (lambda (button state x y)
					   (if (and (and (>= x 13)
							 (<= x 305))
						    (and (>= y 240)
							 (<= y 280)))
					     (initialize-game))
					   (if (and (and (>= x 11)
							 (<= x 327))
						    (and (>= y 319)
							 (<= y 362)))
					     (display "Load game"))
					   (if (and (and (>= x 13)
							 (<= x 145))
						    (and (>= y 387)
							 (<= y 427)))
					     (exit))
					   ))
			  )
		       (gl:ClearColor 0.80 0.68 0.38 0) 
		       (gl:DepthFunc gl:ALWAYS)

		       (gl:PixelStorei gl:UNPACK_ALIGNMENT 1)

		       (gl:BindTexture gl:TEXTURE_2D MENU-TEXTURE)

		       (gl:TexParameteri gl:TEXTURE_2D
					 gl:TEXTURE_MAG_FILTER
					 gl:LINEAR)

		       (gl:TexParameteri gl:TEXTURE_2D
					 gl:TEXTURE_MIN_FILTER
					 gl:LINEAR)

		       (gl:TexEnvf gl:TEXTURE_ENV gl:TEXTURE_ENV_MODE gl:DECAL)

		       (gl:TexImage2D gl:TEXTURE_2D 
				      0 
				      3 
				      (vector-ref menu-img 1) 
				      (vector-ref menu-img 0)
				      0
				      gl:RGB
				      gl:UNSIGNED_BYTE
				      (make-locative menu-pix))

		       (glut:DisplayFunc menu-display)
		       (glut:IdleFunc menu-display)
		       (glut:MouseFunc mouse-handler)

		       (glut:MainLoop)
		       )
		    ))

(define initialize-game (lambda ()
			    (gl:DepthFunc gl:LESS)
			    (gl:MatrixMode gl:PROJECTION)
			    (gl:Scalef camera-zoom camera-zoom camera-zoom)
			    (gl:Rotatef camera-angle 1.0 0.0 0.0)
			    ; Set the initial camera zoom and rotation.

			    (gl:ClearColor 0.80 0.68 0.38 0) 

			    (create-display-lists)

			    (glut:DisplayFunc render-state)
			    (glut:IdleFunc render-state)

			    (glut:SpecialFunc game-keyboard-handler)
			    (glut:MouseFunc game-mouse-handler) 

			    (glut:MainLoop)
			  ))

(define game-keyboard-handler (lambda (key x y)
				(if (eq? mode USER)
				  (begin
				    (if (eq? key glut:KEY_F7)
				      (begin
					(save-game "game1.lsg" brd 0 player0 player1)
					)
				     )
				    (if (eq? key glut:KEY_F8)
					(let ((dat (call-with-values (lambda () 
								       (load-game "game1.lsg")) 
								     vector)))

					(set! brd (vector-ref dat 0))
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
					(gl:Scalef 1.01 1.01 1.01)
					(set! camera-zoom (* camera-zoom 1.01))
					(glut:PostRedisplay)
				       )
				      )

				   (if (eq? key glut:KEY_PAGE_DOWN)
				     (begin
				       (gl:MatrixMode gl:PROJECTION)
				       (gl:Scalef 0.95 0.95 0.95)
				       (set! camera-zoom (* camera-zoom 0.95))
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


(define game-mouse-handler (lambda (button state x y)
			     (if (and (eq? button glut:LEFT_BUTTON)
				      (eq? state glut:UP)
				      (eq? mode USER))
			       (let ((viewport (make-s32vector 4))
				     (pixel (make-u8vector 3 2)))
				 (gl:GetIntegerv gl:VIEWPORT viewport)
				 
				 (color-render) 
				 
				 (gl:ReadPixels x 
						(- (s32vector-ref viewport 3) y) 
						1 
						1 
						gl:RGB 
						gl:UNSIGNED_BYTE 
						(make-locative pixel)) 
				 
				 (if (not (eq? (u8vector-ref pixel 0) 255)) 
				   (if (null? move-origin) ; if the first element in move, the origin, is the empty list...
				     (set! move-origin (cons (u8vector-ref pixel 0) (u8vector-ref pixel 1))) 
				     (let* ((move (cons move-origin (list (cons (u8vector-ref pixel 0) (u8vector-ref pixel 1)))))
					    (piece (get-cell brd move-origin))
					    (captured-vals (call-with-values (lambda () (affect-captures (move-piece brd move))) list))
					    (update (update-players move piece WHITE captured-vals (cons player0 player1)))
					    (ai-thread (make-thread ai-func))
					    )
				       (if (move-valid? brd move WHITE) 
					 (begin
					   (mutex-lock! board-mutex)
					   
					   (when (eq? (caar update) BLACK) 
					     (set! player0 (car update))
					     (set! player1 (cdr update))) 
					     
					   (when (eq? (caar update) WHITE)
					     (set! player1 (car update))
					     (set! player0 (cdr update)))
					   ; update the players 
					   
					   (set! next-board (move-piece brd move))
					   (define ai-brd (car captured-vals))
					   (set! slide-spc move)
					   (set! mode LOCKED)

					   (set! move-origin '())

					   (glut:TimerFunc 100 (lambda (value)
								 (slide))
							   2)

					   (glut:PostRedisplay)

					   (if (not (game-over? ai-brd (cons player0 player1)))
					     (begin
					       (thread-quantum-set! ai-thread 300) 
					       (thread-start! ai-thread)
					       )
					     (begin
					       (display "Game over.")
					       (set! mode LOCKED)
					       )
					     )
					 )
					 (begin
					   (set! move-origin '())
					   (display "illegal move")
					   (newline)
					  )
				       )
				     )
				   ))
			       (gl:Enable gl:TEXTURE_2D)
			       (gl:Enable gl:DITHER)
			       (gl:Enable gl:BLEND)
			      )
			     )))

(define ai-func (lambda ()
		  (let ((ai-mv (find-ai-move ai-brd BLACK (cons player0 player1))))
		    (newline)
		    (display "I choose: ")(display ai-mv)
		    (newline)

		    (define ai-cond (make-condition-variable))

		   (mutex-lock! board-mutex)
		   (set! mode LOCKED)

		   (set! next-board (move-piece brd ai-mv))
		   (set! slide-spc ai-mv)
		   
		   (let* ((piece (get-cell brd (car ai-mv)))
			  (captured-vals (call-with-values (lambda () (affect-captures brd)) list))
			  (update (update-players ai-mv piece BLACK captured-vals (cons player0 player1)))
			  )
		     (when (eq? (caar update) BLACK) 
		       (set! player0 (car update))
		       (set! player1 (cdr update)))
		     (when (eq? (caar update) WHITE)
		       (set! player1 (car update))
		       (set! player0 (cdr update))
		       )
		     )
		   ; update the players

		   (display "Board value: ") 
		   (display (position-eval next-board BLACK (cons player0 player1)))
		   (newline)

		   (glut:TimerFunc 100 (lambda (value)
					 (slide))
				   2)

		   (glut:PostRedisplay)
		   ) 
		  ))

(define dec-alpha (lambda ()
		    (set! alpha (- alpha 0.05))
		    (glut:PostRedisplay) 

		    (if (> alpha 0)
		      (glut:TimerFunc 100 (lambda (value) (dec-alpha)) 1)
		      (begin
			(set! alpha 1.0)
			(set! brd next-board)
			(set! mode USER)
			(set! fade-out '())

			(mutex-unlock! board-mutex)
		      )
		    )))

(define slide (lambda ()
		(set! progress (+ progress 0.10))
		(glut:PostRedisplay)
		
		(if (< progress 1.0)
		  (glut:TimerFunc 100 (lambda (value) (slide)) 2)
		  (let ((captured-vals (call-with-values (lambda () 
							   (affect-captures next-board)) 
							 list))) 
		    (if (not (eqv? (cadr captured-vals) '()))
		      (begin
			(set! mode LOCKED) 
			(set! next-board (car captured-vals))
			(set! fade-out (cadr captured-vals)) 
			
			(glut:TimerFunc 100 (lambda (value)
					      (dec-alpha)
					      )
					1)
		      )
		    (begin
		      (set! progress 0.0)
		      (set! brd next-board)
		      (set! mode USER)
		      (set! slide-spc '())

		      (mutex-unlock! board-mutex)
		    )
		  )
		    ))
		))

(define color-render (lambda ()
		       (let* ((CUBE-WIDTH 0.075) 
			      (PYRAMID-HEIGHT 0.15) 
			      (PYRAMID-WIDTH CUBE-WIDTH)
			      (SPHERE-RADIUS (* 0.5 CUBE-WIDTH))
			      )

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

			  (gl:ClearColor 0.80 .68 0.38 0) 
			  )
		       ))

(define render-state (lambda ()
		       (let* ((CUBE-WIDTH 0.075) 
			      (PYRAMID-HEIGHT 0.15) 
			      (PYRAMID-WIDTH CUBE-WIDTH)
			      (SPHERE-RADIUS (* 0.5 CUBE-WIDTH))) 

			 (gl:Clear (+ gl:COLOR_BUFFER_BIT gl:DEPTH_BUFFER_BIT))

			 (gl:MatrixMode gl:PROJECTION)
			 (gl:PushMatrix)
			 (gl:LoadIdentity)
			 (gl:Ortho 0 0 0 0 -1 1)
			 (gl:MatrixMode gl:MODELVIEW)
			 (gl:PushMatrix)
			 (gl:LoadIdentity)

			 (gl:Disable gl:TEXTURE_2D)
			 (gl:Color3f 1.0 1.0 0.0)
			 (gl:LoadIdentity)

			 (gl:Translatef -0.85 0.9 0.0)
			 (gl:Scalef 0.1 0.1 0.1)
			 (gl:Color3f 0.0 0.0 0.0)
			 (glfDrawSolidString (cadr player0))
			 ;(glfDrawWiredString (cadr player0))

			 (gl:Translatef 0.0 -18.0 0.0)
			 (gl:Color3f 0.0 0.0 0.0)
			 (glfDrawSolidString (cadr player1))
			 ;(gl:Color3f 0.0 0.0 0.0)
			 ;(glfDrawWiredString (cadr player1))

			 (gl:Enable gl:TEXTURE_2D)

			 (gl:MatrixMode gl:PROJECTION)
			 (gl:PopMatrix)
			 (gl:MatrixMode gl:MODELVIEW)
			 (gl:PopMatrix)
			 
			 (gl:MatrixMode gl:MODELVIEW)
			 (gl:LoadIdentity)
			 (gl:Translatef (vector-ref camera-coor 0)
					(vector-ref camera-coor 1)
					(vector-ref camera-coor 2))

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
			 )
		       ))

