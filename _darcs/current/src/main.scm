(include "gfx.scm") ; initialize.scm is included in gfx.scm
(include "ai.scm")

(require 'srfi-13) ; String library.

(define game-state (create-game-board))

(define main (lambda (engine)
	(when (eq? engine NCURSES)
		(display-current-state game-state)
		(define input (get-player-input))

	        (define nums (map (lambda (str)
				    (string->number str)
				    )
				  (string-tokenize input char-set:digit)))

		(define delta (cons (cons (first nums) (second nums)) (list (cons (third nums) (fourth nums)))))

		(if (move-valid? game-state delta WHITE)
		  (begin
		    (set! game-state (make-move game-state delta))
		    (set! current-turn AI)
		    (display-current-state game-state)

		    (define start (current-seconds))

		    (set! game-state (make-move game-state (find-ai-move game-state BLACK)))
		    (define end (current-seconds))
		    (define elapsed (- end start))

		    (display elapsed)
		    (newline)
		    (set! current-turn HUMAN)
		    )
		  )

		(main NCURSES)
	)
	(if (eq? engine PLAINGL)
	  (begin
		(display-current-state game-state)
	    )
	  )
	))

(initialize-display)
(main engine)
