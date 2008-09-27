; A seperate file to hold the display lists. This shortens the gfx file, and keeps its contents more readable.
(in-package #:latrunculi)
(defvar *display-list-start* 0)

; texture 1 will be the board's texture, 2 will be black and 3 will be white.
(defun create-display-lists ()
			       (let ((PINE-TEXTURE 13)
				      (GRANITE-TEXTURE 14)
				      (MARBLE-TEXTURE 15)
				      (CUBE-WIDTH 0.075) 
				      (PYRAMID-HEIGHT 0.15) 
				      (PYRAMID-WIDTH 0.075)
				      (SPHERE-RADIUS (* 0.5 0.075))
                      (quad (glu:new-quadric))
                      (pine-surface (sdl:convert-surface :surface (sdl:load-image (sdl:create-path "pine.bmp" "../img/"))
                                                         :free-p t
                                                         :key-color nil
                                                         :surface-alpha 255))
                      (marble-surface (sdl:convert-surface :surface (sdl:load-image (sdl:create-path "white_marble.bmp" "../img/"))
                                                         :free-p t
                                                         :key-color nil
                                                         :surface-alpha 255))
                      (granite-surface (sdl:convert-surface :surface (sdl:load-image (sdl:create-path "granite.bmp" "../img/"))
                                                         :free-p t
                                                         :key-color nil
                                                         :surface-alpha 255))
                                      )
				 (glu:quadric-normals quad :smooth)
				 (glu:quadric-texture quad :true)
				 (glu:quadric-orientation quad :outside)

				 (gl:pixel-store :unpack-alignment 1)

				 (gl:bind-texture :texture-2d PINE-TEXTURE)

				 (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
				 (gl:tex-parameter :texture-2d :texture-min-filter :linear)

				 (gl:tex-env :texture-env :texture-env-mode :decal)
                    (sdl-base::with-pixel (pixels (sdl:fp pine-surface)) 
                                          (gl:tex-image-2d 
                                            :texture-2d 
                                            0 
                                            :rgba
                                            64 
                                            64 
                                            0 
                                            :bgra
                                            :unsigned-byte 
                                            (sdl-base::pixel-data pixels))
                                          )
				 (gl:bind-texture :texture-2d MARBLE-TEXTURE)

                (sdl-base::with-pixel (pixels (sdl:fp marble-surface)) 
                                      (gl:tex-image-2d 
                                        :texture-2d 
                                        0 
                                        :rgba
                                        64 
                                        64 
                                        0 
                                        :bgra
                                        :unsigned-byte 
                                        (sdl-base::pixel-data pixels))
                                      )
                 (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
				 (gl:tex-parameter :texture-2d :texture-min-filter :linear)

				 (gl:tex-env :texture-env :texture-env-mode :decal)

                 ; Set up the granite (the black) texture
				 (gl:bind-texture :texture-2d GRANITE-TEXTURE)

				 (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
				 (gl:tex-parameter :texture-2d :texture-min-filter :linear)

				 (gl:tex-env :texture-env :texture-env-mode :decal)

                 (sdl-base::with-pixel (pixels (sdl:fp granite-surface)) 
                        (gl:tex-image-2d 
                          :texture-2d 
                          0 
                          :rgba
                          64 
                          64 
                          0 
                          :bgra
                          :unsigned-byte 
                          (sdl-base::pixel-data pixels))
                        )

				 ; We are going to create display lists for each of the following possibilities:
				 ; 1. Empty space
				 ; 2. White king
				 ; 3. Black king
				 ; 4. White pawn
				 ; 5. Black pawn

				 (setq *display-list-start* (gl:gen-lists 5))

				 (gl:new-list *display-list-start* :compile)
				       (gl:bind-texture :texture-2d PINE-TEXTURE)
				       (gl:begin :quads)

				       ; Face 1:

				       (gl:tex-coord 1.0 0.0)
				       (gl:Vertex 0 CUBE-WIDTH CUBE-WIDTH)
				       ; P_1

				       (gl:tex-coord 0.0 0.0)
				       (gl:Vertex 0 CUBE-WIDTH 0)
				       ; P_2

				       (gl:tex-coord 1.0 0.0)
				       (gl:Vertex 0 0 0)
				       ; P_6

				       (gl:tex-coord 1.0 1.0)
				       (gl:Vertex 0 0 CUBE-WIDTH)
				       ; P_5

				       ; Face 2:

				       (gl:tex-coord 1.0 0.0)
				       (gl:Vertex 0 CUBE-WIDTH 0)
				       ; P_2

				       (gl:tex-coord 0.0 0.0)
				       (gl:Vertex CUBE-WIDTH CUBE-WIDTH 0)
				       ; P_3

				       (gl:tex-coord 0.0 1.0)
				       (gl:Vertex CUBE-WIDTH 0 0)
				       ; P_7

				       (gl:tex-coord 1.0 1.0)
				       (gl:Vertex 0 0 0)
				       ; P_6

				       ; Face 3:
				       (gl:tex-coord 0.0 0.0)
				       (gl:Vertex CUBE-WIDTH CUBE-WIDTH CUBE-WIDTH)
				       ; P_4

				       (gl:tex-coord 1.0 0.0)
				       (gl:Vertex CUBE-WIDTH CUBE-WIDTH 0)
				       ; P_3

				       (gl:tex-coord 1.0 1.0)
				       (gl:Vertex CUBE-WIDTH 0 0)
				       ; P_7

				       (gl:tex-coord 0.0 1.0)
				       (gl:Vertex CUBE-WIDTH 0 CUBE-WIDTH)
				       ; P_8 

				       ; Face 4:

				       (gl:tex-coord 0.0 0.0)
				       (gl:Vertex 0 CUBE-WIDTH CUBE-WIDTH)
				       ; P_1

				       (gl:tex-coord 1.0 0.0)
				       (gl:Vertex CUBE-WIDTH CUBE-WIDTH CUBE-WIDTH)
				       ; P_4

				       (gl:tex-coord 1.0 1.0)
				       (gl:Vertex CUBE-WIDTH 0 CUBE-WIDTH)
				       ; P_8 

				       (gl:tex-coord 0.0 1.0)
				       (gl:Vertex 0 0 CUBE-WIDTH)
				       ; P_5

				       ; Face 5:

				       (gl:tex-coord 0.0 0.0)
				       (gl:Vertex 0 CUBE-WIDTH CUBE-WIDTH)
				       ; P_1
				       
				       (gl:tex-coord 1.0 0.0)
				       (gl:Vertex 0 CUBE-WIDTH 0)
				       ; P_2

				       (gl:tex-coord 1.0 1.0)
				       (gl:Vertex CUBE-WIDTH CUBE-WIDTH 0)
				       ; P_3

				       (gl:tex-coord 0.0 1.0)
				       (gl:Vertex CUBE-WIDTH CUBE-WIDTH CUBE-WIDTH)
				       ; P_4
				       ; Face 6:

				       (gl:tex-coord 0.0 1.0)
				       (gl:Vertex CUBE-WIDTH 0 CUBE-WIDTH)
				       ; P_8 

				       (gl:tex-coord 1.0 1.0)
				       (gl:Vertex CUBE-WIDTH 0 0)
				       ; P_7

				       (gl:tex-coord 1.0 0.0)
				       (gl:Vertex 0 0 0)
				       ; P_6

				       (gl:tex-coord 0.0 0.0)
				       (gl:Vertex 0 0 CUBE-WIDTH)
				       ; P_5
				       ; white king display list 
				       (gl:End) 
				       (gl:end-list) 
				       
				       (gl:new-list (+ *display-list-start* 1) :compile)
						   (gl:bind-texture :texture-2d MARBLE-TEXTURE)

						   (gl:Begin :triangles)
						    (gl:tex-coord -.5 0.5)
						    (gl:Vertex (* -0.5 PYRAMID-WIDTH)
								 0
								 (* 0.5 PYRAMID-WIDTH))

						    (gl:tex-coord 0.5 1.0)
						    (gl:Vertex 0
								 PYRAMID-HEIGHT
								 0)

						    (gl:tex-coord 1.0 1.0)
						    (gl:Vertex (* 0.5 PYRAMID-WIDTH)
								 0
								 (* 0.5 PYRAMID-WIDTH))
						    ; first face.

						    (gl:tex-coord 0.0 0.0)
						    (gl:Vertex (* 0.5 PYRAMID-WIDTH)
								 0
								 (* 0.5 PYRAMID-WIDTH))

						    (gl:tex-coord 1.0 0.0)
						    (gl:Vertex 0.0
								 PYRAMID-HEIGHT
								 0.0)

						    (gl:tex-coord 1.0 1.0)
						    (gl:Vertex (* 0.5 PYRAMID-WIDTH)
								 0
								 (* -0.5 PYRAMID-WIDTH))
						    ; second.

						    (gl:tex-coord 0.0 0.0)
						    (gl:Vertex (* 0.5 PYRAMID-WIDTH)
								 0
								 (* -0.5 PYRAMID-WIDTH)) ; pt. 4

						    (gl:tex-coord 1.0 0.0)
						    (gl:Vertex 0.0
								 PYRAMID-HEIGHT
								 0.0) ; pt. 2

						    (gl:tex-coord 1.0 1.0)
						    (gl:Vertex (* -0.5 PYRAMID-WIDTH)
								 0
								 (* -0.5 PYRAMID-WIDTH)) ; pt. 5
						    ; third.

						    (gl:tex-coord 0.0 1.0)
						    (gl:Vertex 0.0
								 PYRAMID-HEIGHT
								 0.0) ; pt. 2

						    (gl:tex-coord 1.0 0.0)
						    (gl:Vertex (* -0.5 PYRAMID-WIDTH)
								 0
								 (* -0.5 PYRAMID-WIDTH)) ; pt. 5

						    (gl:tex-coord 0.0 0.0)
						    (gl:Vertex (* -0.5 PYRAMID-WIDTH)
								 0
								 (* 0.5 PYRAMID-WIDTH))
						    ; fourth.
						(gl:End)
				 (gl:end-list)

				 ; The black king
				 (gl:new-list (+ *display-list-start* 2) :compile)
						   (gl:bind-texture :texture-2d GRANITE-TEXTURE)

						   (gl:Begin :triangles)
						    (gl:tex-coord -.5 0.5)
						    (gl:Vertex (* -0.5 PYRAMID-WIDTH)
								 0
								 (* 0.5 PYRAMID-WIDTH))

						    (gl:tex-coord 0.5 1.0)
						    (gl:Vertex 0
								 PYRAMID-HEIGHT
								 0)

						    (gl:tex-coord 1.0 1.0)
						    (gl:Vertex (* 0.5 PYRAMID-WIDTH)
								 0
								 (* 0.5 PYRAMID-WIDTH))
						    ; first face.

						    (gl:tex-coord 0.0 0.0)
						    (gl:Vertex (* 0.5 PYRAMID-WIDTH)
								 0
								 (* 0.5 PYRAMID-WIDTH))

						    (gl:tex-coord 1.0 0.0)
						    (gl:Vertex 0.0
								 PYRAMID-HEIGHT
								 0.0)

						    (gl:tex-coord 1.0 1.0)
						    (gl:Vertex (* 0.5 PYRAMID-WIDTH)
								 0
								 (* -0.5 PYRAMID-WIDTH))
						    ; second.

						    (gl:tex-coord 0.0 0.0)
						    (gl:Vertex (* 0.5 PYRAMID-WIDTH)
								 0
								 (* -0.5 PYRAMID-WIDTH)) ; pt. 4

						    (gl:tex-coord 1.0 0.0)
						    (gl:Vertex 0.0
								 PYRAMID-HEIGHT
								 0.0) ; pt. 2

						    (gl:tex-coord 1.0 1.0)
						    (gl:Vertex (* -0.5 PYRAMID-WIDTH)
								 0
								 (* -0.5 PYRAMID-WIDTH)) ; pt. 5
						    ; third.

						    (gl:tex-coord 0.0 1.0)
						    (gl:Vertex 0.0
								 PYRAMID-HEIGHT
								 0.0) ; pt. 2

						    (gl:tex-coord 1.0 0.0)
						    (gl:Vertex (* -0.5 PYRAMID-WIDTH)
								 0
								 (* -0.5 PYRAMID-WIDTH)) ; pt. 5

						    (gl:tex-coord 0.0 0.0)
						    (gl:Vertex (* -0.5 PYRAMID-WIDTH)
								 0
								 (* 0.5 PYRAMID-WIDTH))
						    ; fourth.
						(gl:End)
				 (gl:end-list)

				 ; White pawn
				 (gl:new-list (+ *display-list-start* 3) :compile)
				       (gl:bind-texture :texture-2d MARBLE-TEXTURE)
				       (glu:Sphere quad SPHERE-RADIUS 10 10)
				 (gl:end-list)

				 ; black pawn.
				 (gl:new-list (+ *display-list-start* 4) :compile)
				       (gl:bind-texture :texture-2d GRANITE-TEXTURE)
				       (glu:Sphere quad SPHERE-RADIUS 10 10)
				 (gl:end-list) 
                 )
                   )
