; Latrunculi
; (c) Michael J. McDermott, 2006
; Licensed under the GPL v.2

(asdf:operate 'asdf:load-op :uffi)
(asdf:operate 'asdf:load-op :sdl)

(defvar *current-state* ':main-menu)

(defun init-sdl ()
  (sdl:init (logior sdl:+init-video+))
  (let ((surface (sdl:set-video-mode 1024 768 16
				     (logior sdl:+resizable+
					     sdl:+swsurface+))))
    (when (sgum:null-pointer-p surface)
      (error "Unable to set video mode"))
    (sdl:wm-set-caption "Latrunculi" nil)
    surface))

(defun run-sdl-event-loop (surface update-fn)
  (sdl:event-loop
   (:quit ()
	  (return))
   (:idle ()
	  (funcall update-fn))
   ))

(defun generate-display-fun (surface game-state)
  (case game-state
    (:main-menu (lambda () ) )
    (:active-game (lambda ()) )
    )
  ; do nothing
  )

(defun start ()
  (unwind-protect
      (progn
	(let ((surface (init-sdl)))
	  (run-sdl-event-loop surface (generate-display-fun surface *current-state*)))
    (sdl:quit)))
  )
