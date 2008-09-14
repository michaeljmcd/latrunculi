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
    (:main-menu
        (let ((menu-surface (sdl:load-image (sdl:create-path "exekias.bmp" "../img/")))
              (menu-texture nil))
          (gl:gen-textures 1 (make-pointer menu-texture))
          (gl:bind-texture :texture-2d menu-texture)
          (gl:tex-parameter-i :texture-2d :texture-min-filter :linear)
          (gl:tex-parameter-i :texture-2d :texture-mag-filter :linear)
          (gl:tex-image-2d :texture-2d 0 256 1097 902 0 :rgb :unsigned-byte 989494)
          )
        )
    (:active-game
      )
    )
  )
