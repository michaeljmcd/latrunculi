; Latrunculi
; (c) Michael J. McDermott, 2006
; Licensed under the GPL v.2
; Graphics module.

(in-package #:latrunculi)

(defvar *current-state* ':main-menu)
; recognized values are:
; :main-menu - the game is showing the mainmenu
; :active-game - there is an ongoing game
; :animating - a move has been made already, 
;   but the user is not allowed to click yet, as the animation is not done

(defvar *camera-angle* 320)
(defvar *camera-zoom* 1.6875)
(defvar *camera-coordinates* (vector -0.4 0.0 0.0))
(defvar *fade-out* '())
(defvar *slide-space* '())  ; will hold a pair of tuples describing how the piece slides from one locale to another.
(defvar *alpha* 1.0)
(defvar *progress* 0.0)

(defun start () 
  (sdl:with-init () 
    (sdl:window 1024 768 
                :flags (logior sdl:sdl-opengl)
                :title-caption "Latrunculi" 
                :icon-caption "Latrunculi")

     (gl:enable :texture-2d)
     (gl:clear-depth 1.0d0)
     (gl:enable :depth-test)
     (gl:shade-model :smooth)
     (gl:enable :blend)
     (gl:hint :perspective-correction-hint :nicest) 
     (gl:blend-func :src-alpha :one-minus-src-alpha) 
     ; Initialize OpenGL

    (display)
        (sdl:with-events ()
                         (:quit-event () t)
                         (:mouse-button-up-event () 
                                                 (setq *current-state* ':active-game)
                                                 (initialize-game)
                                                 (display)
                                                 )
                         (:key-down-event () (sdl:push-quit-event))
                         (:video-expose-event () (display))
                         )
        )
  )

(defun display ()
  (case *current-state*
    (:main-menu (menu-display))
    (:active-game (game-display))
    )
  )

(defun menu-display ()
  (let ((menu-surface (sdl:convert-surface :surface (sdl:load-image (sdl:create-path "exekias.bmp" "../img/")) 
                                           :free-p t
                                           :key-color nil
                                           :surface-alpha 255
                                           ))
        (menu-texture (first (gl:gen-textures 1)))
        )
    (gl:bind-texture :texture-2d menu-texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)

    (sdl-base::with-pixel (pixels (sdl:fp menu-surface)) 
                          (gl:tex-image-2d 
                            :texture-2d 
                            0 
                            :rgba
                            1024 
                            1024 
                            0 
                            :bgra
                            :unsigned-byte 
                            (sdl-base::pixel-data pixels))
                          )

  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:load-identity)

  (gl:begin :quads)
      (gl:vertex -1 1 0) ; top left
      (gl:tex-coord 1 0)

      (gl:vertex 1 1 0) ; top right
      (gl:tex-coord 1 1)

      (gl:vertex 1 -1 0) ; bottom right
      (gl:tex-coord 0 1)

      (gl:vertex -1 -1 0) ;bottom left
      (gl:tex-coord 0 0)
  (gl:end)

  (sdl:update-display)
  )
  )

(defun initialize-game ()
			    (gl:depth-func :less)
			    (gl:matrix-mode :projection)
			    (gl:scale *camera-zoom* *camera-zoom* *camera-zoom*)
			    (gl:rotate *camera-angle* 1.0 0.0 0.0)
			    ; Set the initial camera zoom and rotation.

			    (gl:clear-color 0.80 0.68 0.38 0) 

			    (create-display-lists)
			  )

(defun game-display ()
		       (let* ((CUBE-WIDTH 0.075) 
			      (PYRAMID-HEIGHT 0.15) 
			      (PYRAMID-WIDTH CUBE-WIDTH)
			      (SPHERE-RADIUS (* 0.5 CUBE-WIDTH))) 
                 (gl:clear :color-buffer-bit :depth-buffer-bit)
                 
                 (gl:matrix-mode :projection)
                 (gl:push-matrix)
                 (gl:load-identity)
                 (gl:ortho 0 0 0 0 -1 1)
                 (gl:matrix-mode :modelview)
                 (gl:push-matrix)
                 (gl:load-identity) 
                 
                 (gl:disable :texture-2d) 
                 (gl:color 255 255 0)
                 (gl:load-identity) 
                 
                 (gl:enable :texture-2d) 
                 (gl:matrix-mode :projection) 
                 (gl:pop-matrix)
                 (gl:matrix-mode :modelview)
                 (gl:pop-matrix) 
                 
                 (gl:matrix-mode :modelview) 
                 (gl:load-identity)
                 (gl:translate (aref *camera-coordinates* 0)
                           (aref *camera-coordinates* 1)
                           (aref *camera-coordinates* 2)) 
                 
                 (loop for x from 0 to (- +ROWS+ 1)
                       do (loop for y from 0 to (- +COLS+ 1)
                                do
                                (let ((current-space (cons x y))
                                      (piece (aref *board* x y)))
                                  
                                  (gl:translate (* -0.5 CUBE-WIDTH) (* -0.5 CUBE-WIDTH) (* -0.5 CUBE-WIDTH)) 
                                  (gl:call-list *display-list-start*)
                                  (gl:translate (* 0.5 CUBE-WIDTH) (* 0.5 CUBE-WIDTH) (* 0.5 CUBE-WIDTH)) 
                                  
                                  (if (and (not (eql *current-state* ':animating))
                                           (not (eql (find current-space *fade-out*) nil))
                                           )
                                    (gl:color 1.0 1.0 1.0 *alpha*)
                                    ) 
                                  
                                  (if (and (not (eql *current-state* ':animating))
                                           (not (eql *slide-space* nil))
                                           (eql (car *slide-space*) current-space))
                                    (progn
                                      (let* ((from (car *slide-space*))
                                             (to (cdr *slide-space*))
                                             (delta-x (- (car from) (car to)))
                                             (delta-y (- (cdr from) (cdr to))))
                                         (if (not (eql delta-x 0)) ; if the move is horizontal
                                           (gl:translate (* -1 *progress* CUBE-WIDTH delta-x) 0.0 0.0)
                                           (gl:translate 0.0 0.0 (* *progress* CUBE-WIDTH delta-y))
                                           )
                                         )
                                       )
                                     )
								 ; Assumes a correct move (i.e. no diagonal)

                                 (cond 
                                   ((eql piece +WHITE_KING+)
                                    (gl:call-list (+ *display-list-start* 1))
                                     )
                                   ((eql piece +BLACK_KING+)
                                    (gl:call-list (+ *display-list-start* 2))
                                     )
                                   ((eql piece +WHITE_PAWN+)
                                    (gl:translate 0.0 (* 0.25 CUBE-WIDTH) 0.0)
                                    (gl:call-list (+ *display-list-start* 3))
                                    (gl:translate 0.0 (* -0.25 CUBE-WIDTH) 0.0)
                                    )
                                   ((eql piece +BLACK_PAWN+)
                                    (gl:translate 0.0 (* 0.25 CUBE-WIDTH) 0.0)
                                    (gl:call-list (+ *display-list-start* 4))
                                    (gl:translate 0.0 (* -0.25 CUBE-WIDTH) 0.0)
                                    )
                                   )

                                 (if (not (eql piece +EMPTY+))
                                   (gl:translate 0.0 ( * -0.5 CUBE-WIDTH) 0.0)
                                   )

                                 (if (and (not (eql *current-state* ':active-game))
                                          (not (eql nil *slide-space*))
                                          (eql (car *slide-space*) space)
                                          )
                                   (progn
                                       (let* ((from (car *slide-space*))
                                              (to (cdr *slide-space*))
                                              (delta-x (- (car from) (car to)))
                                              (delta-y (- (cdr from) (cdr to))))
                                         (if (not (eql delta-x 0)) ; if the move is horizontal
                                           (gl:translate (* *progress* CUBE-WIDTH delta-x) 0.0 0.0)
                                           (gl:translate 0.0 0.0 (* -1 *progress* CUBE-WIDTH delta-y))
                                           )
                                         )
                                       )
                                   )
                                 ) 
                                
                                (gl:translate CUBE-WIDTH 0.0 0.0) 
                                (gl:color 1.0 1.0 1.0 1.0)
                                ) 
                       
                       (gl:translate (* -1 +COLS+ CUBE-WIDTH) 0.0 (* -1 CUBE-WIDTH))
                       )
                 (sdl:update-display)
                 )
               )
