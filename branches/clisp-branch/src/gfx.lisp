; Latrunculi
; (c) Michael J. McDermott, 2006
; Licensed under the GPL v.2
; Graphics module.

(asdf:operate 'asdf:load-op :cffi)
(asdf:operate 'asdf:load-op :lispbuilder-sdl)
(asdf:operate 'asdf:load-op :cl-opengl)

(defvar *current-state* ':main-menu)
; recognized values are:
; :main-menu - the game is showing the mainmenu
; :active-game - there is an ongoing game
; :animating - a move has been made already, 
;   but the user is not allowed to click yet, as the animation is not done

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
                                                 (display))
                         (:key-down-event () (sdl:push-quit-event))
                         (:video-expose-event () (display))
                         )
        )
  )

(defun display ()
  (case *current-state*
    (:main-menu (menu-display))
    (:active-game (menu-display))
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
