(include "gfx.scm")
(include "ai.scm")

(define board (create-game-board))
(display-current-state board)

(set! board (make-move board (find-ai-move board WHITE)))
(display-current-state board)
