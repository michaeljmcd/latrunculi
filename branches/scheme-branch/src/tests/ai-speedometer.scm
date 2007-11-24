(include "initialize.scm")
(include "ai.scm")

(define start (current-seconds))
(find-ai-move (create-game-board) BLACK)
(define end (current-seconds))

(define delta (- end start))
(newline)
(display delta)
(newline)
