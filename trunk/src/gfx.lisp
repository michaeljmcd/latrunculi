; Latrunculi
; (c) Michael J. McDermott, 2006
; Licensed under the GPL v.2
; Graphics module.

(in-package #:latrunculi)

(defvar *current-state* ':main-menu)
; recognized values are:
; :main-menu - the game is showing the mainmenu
; :active-game - there is an ongoing game
; :game-over - game is over, but still showing the board

(defvar *camera-angle* 320)
(defvar *camera-zoom* 1.6875)
(defvar *camera-coordinates* (vector -0.4 0.0 0.0))
(defvar *camera-angle-delta* 1) ; the amount a camera angle will change (in degrees) with each key press.
(defvar *move-origin* '()) 	 ; from and to data in the current move.

(defmacro at-offset (coordinates &rest operations)
    (let ((x (car coordinates))
          (y (cadr coordinates))
          (z (caddr coordinates)))
    `(progn
        (gl:translate ,x ,y ,z)
        ,@operations
        (gl:translate (* -1 ,x) (* -1 ,y) (* -1 ,z))
    )))

(defun start () 
  (sdl:with-init () 
    (sdl:window 1024 768 
                :flags (logior sdl:sdl-opengl)
                :title-caption "Latrunculi" 
                :icon-caption "Latrunculi")
      (sdl-cffi::sdl-enable-key-repeat sdl-cffi::sdl-default-repeat-delay
                                       sdl-cffi::sdl-default-repeat-interval)
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
        (:mouse-button-up-event (:button button :X x :Y y) 
                                (game-mouse-handler button x y))
        (:key-down-event (:key key) 
                         (if (eql *current-state* ':active-game)
                              (game-keyboard-handler key)
                              (sdl:push-quit-event)))
        (:video-expose-event () (display))
        )))

(defun display (&key (progress 0.0) (slide-space nil) (alpha 1.0) (fade-out nil))
  (case *current-state*
    (:main-menu (menu-display))
    (:active-game (game-display :progress progress :slide-space slide-space :alpha alpha :fade-out fade-out))
    ))

(defun menu-display ()
  (let ((menu-surface (sdl:convert-surface :surface (sdl:load-image (sdl:create-path "exekias.bmp" "../img/"))))
        (menu-texture (first (gl:gen-textures 1))))
    ;(gl:pixel-store :unpack-alignment 3)
    (gl:bind-texture :texture-2d menu-texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)

    ; bmp = bgra? format
    (sdl-base::with-pixel (pixels (sdl:fp menu-surface)) 
                          (gl:tex-image-2d 
                            :texture-2d 
                            0 
                            :rgb ; texture format
                            1024 
                            1024 
                            0 
                            :bgra ; pixel data format
                            :unsigned-byte 
                            (sdl-base::pixel-data pixels)))
    #|
    (sdl-base::with-pixel (pixels (sdl:fp menu-surface)) 
        (glu:build-2d-mipmaps :texture-2d 3 1024 1024 :bgra-ext :unsigned-byte 
            (sdl-base::pixel-data pixels)))
|#

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
  ))

(defun initialize-game ()
  (gl:depth-func :less)
  (gl:matrix-mode :projection)
  (gl:scale *camera-zoom* *camera-zoom* *camera-zoom*)
  (gl:rotate *camera-angle* 1.0 0.0 0.0)
  ; Set the initial camera zoom and rotation.

  (gl:clear-color 0.80 0.68 0.38 0) 

  (create-display-lists))

(defun game-display (&key (progress 0.0) (slide-space nil) (fade-out nil) (alpha 1.0))
  (gl:clear :color-buffer-bit :depth-buffer-bit) 
  
  (gl:matrix-mode :projection)
  (gl:push-matrix)
  (gl:load-identity)
  (gl:ortho 0 0 0 0 -1 1)
  (gl:matrix-mode :modelview)
  (gl:push-matrix)
  (gl:load-identity)

  (gl:disable :texture-2d)
  (gl:color 1.0 1.0 0.0)
  (gl:load-identity)

  (gl:translate -0.85 0.9 0.0)
  (gl:scale 0.1 0.1 0.1) (gl:color 0.0 0.0 0.0)

  ; print text

  (gl:translate 0.0 -18.0 0.0)
  (gl:color 0.0 0.0 0.0) 

  ; print text

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
                 (let ((current-space (cons y x))
                       (piece (aref *board* x y)))
                   
                   (at-offset ((* -0.5 +CUBE-WIDTH+) (* -0.5 +CUBE-WIDTH+) (* -0.5 +CUBE-WIDTH+)) (gl:call-list *display-list-start*))
                  
                   (if (not (eql (find current-space fade-out) nil))
                     (gl:color 1.0 1.0 1.0 alpha)
                     (gl:color 1.0 1.0 1.0 1.0)) 

                   (if (and (not (equal slide-space nil))
                            (equal (car slide-space) current-space))
                       (let ((delta-x (- (caar slide-space) (caadr slide-space)))
                             (delta-y (- (cdar slide-space) (cdadr slide-space))))
                          (gl:translate (* -1 progress +CUBE-WIDTH+ delta-x) 0.0 (* progress +CUBE-WIDTH+ delta-y))))
                   
                  (if (not (eql piece +EMPTY+))
                    (at-offset (0.0 (* 0.5 +CUBE-WIDTH+) 0.0)
                      (cond 
                        ((eql piece +WHITE_KING+)
                         (gl:call-list (+ *display-list-start* 1)))
                        ((eql piece +BLACK_KING+)
                         (gl:call-list (+ *display-list-start* 2)))
                        ((eql piece +WHITE_PAWN+)
                         (gl:translate 0.0 (* 0.25 +CUBE-WIDTH+) 0.0)
                         (gl:call-list (+ *display-list-start* 3))
                         (gl:translate 0.0 (* -0.25 +CUBE-WIDTH+) 0.0))
                        ((eql piece +BLACK_PAWN+)
                         (gl:translate 0.0 (* 0.25 +CUBE-WIDTH+) 0.0)
                         (gl:call-list (+ *display-list-start* 4))
                         (gl:translate 0.0 (* -0.25 +CUBE-WIDTH+) 0.0))))

                   (if (and (not (equal slide-space nil))
                            (equal (car slide-space) current-space))
                       (let ((delta-x (- (caar slide-space) (caadr slide-space)))
                             (delta-y (- (cdar slide-space) (cdadr slide-space))))
                          (gl:translate (* progress +CUBE-WIDTH+ delta-x) 0.0 (* -1 progress +CUBE-WIDTH+ delta-y)))
                          ))
                 
                 (gl:translate +CUBE-WIDTH+ 0.0 0.0) 
                 (gl:color 1.0 1.0 1.0 1.0)))
        (gl:translate (* -1 +COLS+ +CUBE-WIDTH+) 0.0 (* -1 +CUBE-WIDTH+)))
  (sdl:update-display))

(defun game-keyboard-handler (key)
  (if (eql *current-state* ':active-game)
    (progn
      (cond
        ((eql key ':sdl-key-right)
         (setf (aref *camera-coordinates* 0) 
               (+ .01 (aref *camera-coordinates* 0)))
         )
        ((eql key ':sdl-key-left)
         (setf (aref *camera-coordinates* 0) 
               (- (aref *camera-coordinates* 0) .01))
         )
        ((eql key ':sdl-key-up)
         (setf (aref *camera-coordinates* 1) 
               (+ (aref *camera-coordinates* 1) .01))
        )
        ((eql key ':sdl-key-down)
         (setf (aref *camera-coordinates* 1) 
               (- (aref *camera-coordinates* 1) .01))
        )
        ((eql key ':sdl-key-pageup)
         (gl:matrix-mode :projection)
         (gl:scale 1.01 1.01 1.01)
         (setq *camera-zoom* (* *camera-zoom* 1.01))
         )
        ((eql key ':sdl-key-pagedown)
         (gl:matrix-mode :projection)
         (gl:scale 0.95 0.95 0.95)
         (setq *camera-zoom* (* *camera-zoom* 0.95))
         )
        ((eql key ':sdl-key-home)
         (gl:matrix-mode :projection)
         (gl:rotate *camera-angle-delta* 0.0 1.0 0.0)
         (setf *camera-angle* (mod (+ *camera-angle* *camera-angle-delta*) 360))
         )
        ((eql key ':sdl-key-end)
         (gl:matrix-mode :projection)
         (gl:rotate (* -1 *camera-angle-delta*) 0.0 1.0 0.0)
         (setf *camera-angle* (mod (- *camera-angle* *camera-angle-delta*) 360))
         ) 
        ((eql key ':sdl-key-insert)
         (gl:matrix-mode :projection)
         (gl:rotate *camera-angle-delta* 1.0 0.0 0.0)
         (setf *camera-angle* (mod (+ *camera-angle* *camera-angle-delta*) 360))
         )
        )
      (game-display)
      )
    )
  )

(defun animate-slide (progress slide-space)
  (if (< progress 1.0)
    (progn
      (sdl-cffi::sdl-delay 100)
      (display :progress progress :slide-space slide-space)
      (animate-slide (+ progress 0.10) slide-space)
      )
     t
    ))

(defun animate-captures (progress pieces)
  (if (> progress 0.0)
    (progn
      (sdl-cffi::sdl-delay 100)
      (display :alpha progress :fade-out pieces)
      (animate-captures (- progress 0.10) pieces)
      )
    t
    ))

(defun game-mouse-handler (button x y)
  ; 1 button = left
  (when (and (equal button 1)
             (equal *current-state* ':main-menu))
       (setq *current-state* ':active-game) 
       (initialize-game) 
       (display))

  (if (and (equal button 1)
           (equal *current-state* ':active-game))
       (let ((viewport (gl:get-integer :viewport))
             (pixel 0))
        (color-render)

        (setf pixel (gl:read-pixels x
                        (- (aref viewport 3) y)
                        1
                        1
                        :rgb
                        :unsigned-byte))

        (setf (aref pixel 0) (round (* +ROWS+ (/ (aref pixel 0) 255))))
        (setf (aref pixel 1) (round (* +COLS+ (/ (aref pixel 1) 255))))
        ; translate the float back into an integer representation of a cell

        (if (and (not (eql (aref pixel 0) 255))
                 (not (null *move-origin*)))
              (let ((move (cons *move-origin* (list (cons (aref pixel 1)
                                                          (aref pixel 0))))))
                (if (or (move-valid? *board* move +WHITE+) (move-valid? *board* move +BLACK+)) ; for testing
                  (let ((captured-vals (multiple-value-list (affect-captures (move-piece *board* move) (cons *player0* *player1*))))
                        (update (update-players captured-vals (cons *player0* *player1*))))
                    (when (eql (player-color (car update)) +BLACK+)
                      (setq *player0* (car update))
                      (setq *player1* (cdr update)))

                    (when (eql (player-color (car update)) +WHITE+)
                      (setq *player1* (car update))
                      (setq *player0* (cdr update)))

                    (animate-slide 0.0 move)
                    (print captured-vals)

                    (setq *board* (move-piece *board* move))
                    (animate-captures 1.0 (cadr captured-vals))

                    (setq *board* (car captured-vals))
                    (display)

                    (if (not (game-over? *board* (cons *player0* *player1*)))
                      (progn
                        ; find AI mvoe
                        )
                      (progn
                        (print "Game over.")
                        (setq *current-state* ':game-over))
                      ))
                  (progn
                    (setf *move-origin* nil)
                    (print "illegal move")))
                ))

        (if (and (not (eql (aref pixel 0) 255))
                 (null *move-origin*))
          (setf *move-origin* (cons (aref pixel 1) (aref pixel 0))))
        )
       )

        (gl:enable :texture-2d)
        (gl:enable :dither)
        (gl:enable :blend)
    )

(defun color-render ()
  (gl:disable :texture-2d)
  (gl:disable :dither)
  (gl:disable :lighting)
  (gl:disable :blend)

  (gl:clear-color 255 255 255 0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (gl:matrix-mode :modelview)
  (gl:load-identity)

  (gl:translate (aref *camera-coordinates* 0)
                (aref *camera-coordinates* 1)
                (aref *camera-coordinates* 2))
  
  (loop for x from 0 to (- +ROWS+ 1)
        do (loop for y from 0 to (- +COLS+ 1)
                 do
                 (let ((piece (aref *board* x y)))
                   (gl:color (/ x +ROWS+)
                             (/ y +COLS+)
                             0
                             0)
                   
                   (at-offset ((* -0.5 +CUBE-WIDTH+) (* -0.5 +CUBE-WIDTH+) (* -0.5 +CUBE-WIDTH+)) (gl:call-list *display-list-start*))

                   (if (not (eql piece +EMPTY+))
                     (at-offset (0.0 (* 0.5 +CUBE-WIDTH+) 0.0) 
                       (cond
                         ((eql piece +WHITE_KING+)
                          (gl:call-list (+ *display-list-start* 1))
                          )
                         ((eql piece +BLACK_KING+)
                          (gl:call-list (+ *display-list-start* 2))
                          )
                         ((eql piece +WHITE_PAWN+)
                          (gl:translate 0.0 (* 0.25 +CUBE-WIDTH+) 0.0)
                          (gl:call-list (+ *display-list-start* 3))
                          (gl:translate 0.0 (* -0.25 +CUBE-WIDTH+) 0.0)
                          )
                         ((eql piece +BLACK_PAWN+)
                          (gl:translate 0.0 (* 0.25 +CUBE-WIDTH+) 0.0)
                          (gl:call-list (+ *display-list-start* 4))
                          (gl:translate 0.0 (* -0.25 +CUBE-WIDTH+) 0.0)
                          )))
                    )
                   (gl:translate +CUBE-WIDTH+ 0.0 0.0)
                   ))
        (gl:translate (* -1 +COLS+ +CUBE-WIDTH+) 0.0 (* -1 +CUBE-WIDTH+)) )

  (gl:flush) 
  (gl:clear-color 0.80 .68 0.38 0))
