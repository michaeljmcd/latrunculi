(include "ai.scm")
(include "gfx.scm")

(define board (create-game-board))
(display-current-state board)

;(define moves (list (cons (cons '3 '7) (list (cons '3 '2)))
;		    (cons (cons '2 '0) (list (cons '2 '2)))
;		    (cons (cons '4 '0) (list (cons '4 '2)))
;		    ))

;(define moves (list (cons (cons '3 '7) (list (cons '3 '2)))
;		    (cons (cons '3 '0) (list (cons '3 '1)))
;		    (cons (cons '4 '0) (list (cons '4 '3)))
;		    (cons (cons '4 '3) (list (cons '3 '3)))
;		    ))

;(define moves (list (cons (cons '1 '0) (list (cons '1 '2)))
;		    (cons (cons '1 '2) (list (cons '11 '2)))
;		    (cons (cons '2 '0) (list (cons '2 '2)))
;		    (cons (cons '1 '7) (list (cons '1 '0)))
;		    (cons (cons '0 '7) (list (cons '0 '1)))
;		    ))

(define moves (list (cons (cons '4 '7) (list (cons '4 '1)))
		    (cons (cons '5 '7) (list (cons '5 '2)))
		    (cons (cons '6 '7) (list (cons '6 '1)))
		    ))

(map (lambda (m)
       (set! board (make-move board m))
       (display-current-state board)
       )
     moves)

(win? board BLACK)
(lose? board WHITE)
