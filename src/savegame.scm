; This file provides the utilities required to load/save a game.

(define save-game (lambda (filename board turn player0 player1)
		    (define out (open-output-file filename))

		    (write board out)
		    (write turn out)
		    (write player0 out)
		    (write player1 out)

		    (close-output-port out)
		    ))

(define load-game (lambda (filename)
		    (define in (open-input-file filename))

		    (define board (read in))
		    (define turn (read in))
		    (define player0 (read in))
		    (define player1 (read in))

		    (close-input-port in)

		    (values board turn player0 player1)
		    ))
