; A seperate file to hold the display lists. This shortens the gfx file, and keeps its contents more readable.

(require-extension gl glu cairo vector-lib)
(include "targa.scm")

; texture 1 will be the board's texture, 2 will be black and 3 will be white.
(define create-display-lists (lambda ()
			       (let* ((PINE-TEXTURE 13)
				      (GRANITE-TEXTURE 14)
				      (MARBLE-TEXTURE 15)
				      (CUBE-WIDTH 0.075) 
				      (PYRAMID-HEIGHT 0.15) 
				      (PYRAMID-WIDTH CUBE-WIDTH)
				      (SPHERE-RADIUS (* 0.5 CUBE-WIDTH))
				      (pine-img (tga-data "../img/pine.tga"))
				      (pine-pix (list->u8vector (vector->list (vector-ref pine-img 3))))
                                      (granite-img (tga-data "../img/granite.tga"))
				      (granite-pix (list->u8vector (vector->list (vector-ref granite-img 3))))
				      (marble-img (tga-data "../img/white_marble.tga"))
				      (marble-pix (list->u8vector (vector->list (vector-ref marble-img 3))))
				      (quad (glu:NewQuadric))) 
				 
				 (glu:QuadricNormals quad glu:SMOOTH)
				 (glu:QuadricTexture quad (integer->char gl:TRUE))
				 (glu:QuadricOrientation quad glu:OUTSIDE)

				 (gl:PixelStorei gl:UNPACK_ALIGNMENT 1)

				 (gl:BindTexture gl:TEXTURE_2D PINE-TEXTURE)

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
						(vector-ref pine-img 1) 
						(vector-ref pine-img 0)
						0
						gl:RGB
						gl:UNSIGNED_BYTE
						(make-locative pine-pix))

				 (gl:BindTexture gl:TEXTURE_2D MARBLE-TEXTURE)
				 (gl:TexImage2D gl:TEXTURE_2D
						0
						3
						(vector-ref marble-img 1)
						(vector-ref marble-img 0)
						0
						gl:RGB
						gl:UNSIGNED_BYTE
						(make-locative marble-pix))

				 (gl:TexParameteri gl:TEXTURE_2D
						   gl:TEXTURE_MAG_FILTER
						   gl:LINEAR)

				 (gl:TexParameteri gl:TEXTURE_2D
						   gl:TEXTURE_MIN_FILTER
						   gl:LINEAR)

				 (gl:TexEnvf gl:TEXTURE_ENV gl:TEXTURE_ENV_MODE gl:DECAL)

                                 ; Set up the granite (the black) texture
				 (gl:BindTexture gl:TEXTURE_2D GRANITE-TEXTURE)

				 (gl:TexParameteri gl:TEXTURE_2D
						   gl:TEXTURE_MAG_FILTER
						   gl:LINEAR)

				 (gl:TexParameteri gl:TEXTURE_2D
						   gl:TEXTURE_MIN_FILTER
						   gl:LINEAR)

				 (gl:TexEnvf gl:TEXTURE_ENV gl:TEXTURE_ENV_MODE gl:DECAL)

                                 (cairo-set-source-surface granite-instance granite-surface 0 0)
                                 (cairo-paint granite-instance)
                                 (cairo-surface-destroy granite-surface)

				 (gl:TexImage2D gl:TEXTURE_2D
						0
						3
						(vector-ref granite-img 1)
						(vector-ref granite-img 0)
						0
						gl:RGB
						gl:UNSIGNED_BYTE
						(make-locative granite-pix))

				 ; We are going to create display lists for each of the following possibilities:
				 ; 1. Empty space
				 ; 2. White king
				 ; 3. Black king
				 ; 4. White pawn
				 ; 5. Black pawn

				 (set! display-list-start (gl:GenLists 5))

				 (gl:NewList display-list-start gl:COMPILE)
				       (gl:BindTexture gl:TEXTURE_2D PINE-TEXTURE)
				       (gl:Begin gl:QUADS)

				       ; Face 1:

				       (gl:TexCoord2f 1.0 0.0)
				       (gl:Vertex3f 0 CUBE-WIDTH CUBE-WIDTH)
				       ; P_1

				       (gl:TexCoord2f 0.0 0.0)
				       (gl:Vertex3f 0 CUBE-WIDTH 0)
				       ; P_2

				       (gl:TexCoord2f 1.0 0.0)
				       (gl:Vertex3f 0 0 0)
				       ; P_6

				       (gl:TexCoord2f 1.0 1.0)
				       (gl:Vertex3f 0 0 CUBE-WIDTH)
				       ; P_5

				       ; Face 2:

				       (gl:TexCoord2f 1.0 0.0)
				       (gl:Vertex3f 0 CUBE-WIDTH 0)
				       ; P_2

				       (gl:TexCoord2f 0.0 0.0)
				       (gl:Vertex3f CUBE-WIDTH CUBE-WIDTH 0)
				       ; P_3

				       (gl:TexCoord2f 0.0 1.0)
				       (gl:Vertex3f CUBE-WIDTH 0 0)
				       ; P_7

				       (gl:TexCoord2f 1.0 1.0)
				       (gl:Vertex3f 0 0 0)
				       ; P_6

				       ; Face 3:
				       (gl:TexCoord2f 0.0 0.0)
				       (gl:Vertex3f CUBE-WIDTH CUBE-WIDTH CUBE-WIDTH)
				       ; P_4

				       (gl:TexCoord2f 1.0 0.0)
				       (gl:Vertex3f CUBE-WIDTH CUBE-WIDTH 0)
				       ; P_3

				       (gl:TexCoord2f 1.0 1.0)
				       (gl:Vertex3f CUBE-WIDTH 0 0)
				       ; P_7

				       (gl:TexCoord2f 0.0 1.0)
				       (gl:Vertex3f CUBE-WIDTH 0 CUBE-WIDTH)
				       ; P_8 

				       ; Face 4:

				       (gl:TexCoord2f 0.0 0.0)
				       (gl:Vertex3f 0 CUBE-WIDTH CUBE-WIDTH)
				       ; P_1

				       (gl:TexCoord2f 1.0 0.0)
				       (gl:Vertex3f CUBE-WIDTH CUBE-WIDTH CUBE-WIDTH)
				       ; P_4

				       (gl:TexCoord2f 1.0 1.0)
				       (gl:Vertex3f CUBE-WIDTH 0 CUBE-WIDTH)
				       ; P_8 

				       (gl:TexCoord2f 0.0 1.0)
				       (gl:Vertex3f 0 0 CUBE-WIDTH)
				       ; P_5

				       ; Face 5:

				       (gl:TexCoord2f 0.0 0.0)
				       (gl:Vertex3f 0 CUBE-WIDTH CUBE-WIDTH)
				       ; P_1
				       
				       (gl:TexCoord2f 1.0 0.0)
				       (gl:Vertex3f 0 CUBE-WIDTH 0)
				       ; P_2

				       (gl:TexCoord2f 1.0 1.0)
				       (gl:Vertex3f CUBE-WIDTH CUBE-WIDTH 0)
				       ; P_3

				       (gl:TexCoord2f 0.0 1.0)
				       (gl:Vertex3f CUBE-WIDTH CUBE-WIDTH CUBE-WIDTH)
				       ; P_4
				       ; Face 6:

				       (gl:TexCoord2f 0.0 1.0)
				       (gl:Vertex3f CUBE-WIDTH 0 CUBE-WIDTH)
				       ; P_8 

				       (gl:TexCoord2f 1.0 1.0)
				       (gl:Vertex3f CUBE-WIDTH 0 0)
				       ; P_7

				       (gl:TexCoord2f 1.0 0.0)
				       (gl:Vertex3f 0 0 0)
				       ; P_6

				       (gl:TexCoord2f 0.0 0.0)
				       (gl:Vertex3f 0 0 CUBE-WIDTH)
				       ; P_5
				       ; white king display list 
				       (gl:End) 
				       (gl:EndList) 
				       
				       (gl:NewList (+ display-list-start 1) gl:COMPILE)
						   (gl:BindTexture gl:TEXTURE_2D MARBLE-TEXTURE)

						   (gl:Begin gl:TRIANGLES)
						    (gl:TexCoord2f -.5 0.5)
						    (gl:Vertex3f (* -0.5 PYRAMID-WIDTH)
								 0
								 (* 0.5 PYRAMID-WIDTH))

						    (gl:TexCoord2f 0.5 1.0)
						    (gl:Vertex3f 0
								 PYRAMID-HEIGHT
								 0)

						    (gl:TexCoord2f 1.0 1.0)
						    (gl:Vertex3f (* 0.5 PYRAMID-WIDTH)
								 0
								 (* 0.5 PYRAMID-WIDTH))
						    ; first face.

						    (gl:TexCoord2f 0.0 0.0)
						    (gl:Vertex3f (* 0.5 PYRAMID-WIDTH)
								 0
								 (* 0.5 PYRAMID-WIDTH))

						    (gl:TexCoord2f 1.0 0.0)
						    (gl:Vertex3f 0.0
								 PYRAMID-HEIGHT
								 0.0)

						    (gl:TexCoord2f 1.0 1.0)
						    (gl:Vertex3f (* 0.5 PYRAMID-WIDTH)
								 0
								 (* -0.5 PYRAMID-WIDTH))
						    ; second.

						    (gl:TexCoord2f 0.0 0.0)
						    (gl:Vertex3f (* 0.5 PYRAMID-WIDTH)
								 0
								 (* -0.5 PYRAMID-WIDTH)) ; pt. 4

						    (gl:TexCoord2f 1.0 0.0)
						    (gl:Vertex3f 0.0
								 PYRAMID-HEIGHT
								 0.0) ; pt. 2

						    (gl:TexCoord2f 1.0 1.0)
						    (gl:Vertex3f (* -0.5 PYRAMID-WIDTH)
								 0
								 (* -0.5 PYRAMID-WIDTH)) ; pt. 5
						    ; third.

						    (gl:TexCoord2f 0.0 1.0)
						    (gl:Vertex3f 0.0
								 PYRAMID-HEIGHT
								 0.0) ; pt. 2

						    (gl:TexCoord2f 1.0 0.0)
						    (gl:Vertex3f (* -0.5 PYRAMID-WIDTH)
								 0
								 (* -0.5 PYRAMID-WIDTH)) ; pt. 5

						    (gl:TexCoord2f 0.0 0.0)
						    (gl:Vertex3f (* -0.5 PYRAMID-WIDTH)
								 0
								 (* 0.5 PYRAMID-WIDTH))
						    ; fourth.
						(gl:End)
				 (gl:EndList)

				 ; The black king
				 (gl:NewList (+ display-list-start 2) gl:COMPILE)
						   (gl:BindTexture gl:TEXTURE_2D GRANITE-TEXTURE)

						   (gl:Begin gl:TRIANGLES)
						    (gl:TexCoord2f -.5 0.5)
						    (gl:Vertex3f (* -0.5 PYRAMID-WIDTH)
								 0
								 (* 0.5 PYRAMID-WIDTH))

						    (gl:TexCoord2f 0.5 1.0)
						    (gl:Vertex3f 0
								 PYRAMID-HEIGHT
								 0)

						    (gl:TexCoord2f 1.0 1.0)
						    (gl:Vertex3f (* 0.5 PYRAMID-WIDTH)
								 0
								 (* 0.5 PYRAMID-WIDTH))
						    ; first face.

						    (gl:TexCoord2f 0.0 0.0)
						    (gl:Vertex3f (* 0.5 PYRAMID-WIDTH)
								 0
								 (* 0.5 PYRAMID-WIDTH))

						    (gl:TexCoord2f 1.0 0.0)
						    (gl:Vertex3f 0.0
								 PYRAMID-HEIGHT
								 0.0)

						    (gl:TexCoord2f 1.0 1.0)
						    (gl:Vertex3f (* 0.5 PYRAMID-WIDTH)
								 0
								 (* -0.5 PYRAMID-WIDTH))
						    ; second.

						    (gl:TexCoord2f 0.0 0.0)
						    (gl:Vertex3f (* 0.5 PYRAMID-WIDTH)
								 0
								 (* -0.5 PYRAMID-WIDTH)) ; pt. 4

						    (gl:TexCoord2f 1.0 0.0)
						    (gl:Vertex3f 0.0
								 PYRAMID-HEIGHT
								 0.0) ; pt. 2

						    (gl:TexCoord2f 1.0 1.0)
						    (gl:Vertex3f (* -0.5 PYRAMID-WIDTH)
								 0
								 (* -0.5 PYRAMID-WIDTH)) ; pt. 5
						    ; third.

						    (gl:TexCoord2f 0.0 1.0)
						    (gl:Vertex3f 0.0
								 PYRAMID-HEIGHT
								 0.0) ; pt. 2

						    (gl:TexCoord2f 1.0 0.0)
						    (gl:Vertex3f (* -0.5 PYRAMID-WIDTH)
								 0
								 (* -0.5 PYRAMID-WIDTH)) ; pt. 5

						    (gl:TexCoord2f 0.0 0.0)
						    (gl:Vertex3f (* -0.5 PYRAMID-WIDTH)
								 0
								 (* 0.5 PYRAMID-WIDTH))
						    ; fourth.
						(gl:End)
				 (gl:EndList)

				 ; White pawn
				 (gl:NewList (+ display-list-start 3) gl:COMPILE)
				       (gl:BindTexture gl:TEXTURE_2D MARBLE-TEXTURE)
				       (glu:Sphere quad SPHERE-RADIUS 10 10)
				 (gl:EndList)

				 ; black pawn.
				 (gl:NewList (+ display-list-start 4) gl:COMPILE)
				       (gl:BindTexture gl:TEXTURE_2D GRANITE-TEXTURE)
				       (glu:Sphere quad SPHERE-RADIUS 10 10)
				 (gl:EndList) 
				 )
			       ))
