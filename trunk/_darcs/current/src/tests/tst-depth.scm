(include "ai.scm")
(define brd1 (create-game-board))
(define brd2 (make-move brd1 (cons (cons '0 '0) (list (cons '0 '3)))))
