(include "gfx.scm") ; initialize.scm is included in gfx.scm
(include "ai.scm")

;(require 'srfi-13 'tinyclos) ; SRFI-13 is aString library.

(define AI 0)
(define HUMAN 1)

(define BLACK 1)
(define WHITE 0)

(define player0 (list BLACK 
		      "Ajax" 
		      AI 
		      '((3 . (0 . 0)) (3 . (1 . 0)) (3 . (2 . 0))
						(3 . (3 . 0)) (3 . (4 . 0))
						(3 . (5 . 0)) (3 . (6 . 0))
						(3 . (7 . 0)) (3 . (8 . 0))
						(3 . (9 . 0)) (3 . (10 . 0))
						(3 . (11 . 0)) (1 . (5 . 1)))
		      '#(2 1 2 2 2 1 1)))

(define player1 (list WHITE 
		      "Achilles" 
		      HUMAN 
		      '((2 . (0 . 11)) (2 . (1 . 11)) (2 . (2 . 11))
						(2 . (3 . 11)) (2 . (4 . 11))
						(2 . (5 . 11)) (2 . (6 . 11))
						(2 . (7 . 11)) (2 . (8 . 11))
						(2 . (9 . 11)) (2 . (10 . 11))
						(2 . (11 . 11)) (0 . (6 . 6)))
		      '#(40 25 25 0.05 0.05 1 1)))

; Player data is a tuple of the form (C N P M A) where C is the player's color, N
; is the player's name, P is an integer indicating whether the player is human
; controlled or AI, M (for material) is a list of tuples describing all of the pieces
; that the player has and their positions, and A is the player's AI settings (ignored
; if under player control). A is of the form:
; #(search-depth own-pawn-value opponent-pawn-value own-king-mobility-value opponent-king-mobility-value
; overall-mobility-value overall-opponent-mobility-value)

(initialize-display)
;(show-menu)
(initialize-game)
