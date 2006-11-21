(include "gfx.scm") ; initialize.scm is included in gfx.scm
(include "ai.scm")

;(require 'srfi-13 'tinyclos) ; SRFI-13 is aString library.

(define AI 0)
(define HUMAN 1)
(define current-turn HUMAN)

(define BLACK 1)
(define WHITE 0)

(define player0 (list BLACK 
		      "Ajax" 
		      AI 
		      '((BLACK_PAWN . '(0 . 0)) (BLACK_PAWN . '(1 . 0)) (BLACK_PAWN . '(2 . 0))
						(BLACK_PAWN . '(3 . 0)) (BLACK_PAWN . '(4 . 0))
						(BLACK_PAWN . '(5 . 0)) (BLACK_PAWN . '(6 . 0))
						(BLACK_PAWN . '(7 . 0)) (BLACK_PAWN . '(8 . 0))
						(BLACK_PAWN . '(9 . 0)) (BLACK_PAWN . '(10 . 0))
						(BLACK_PAWN . '(11 . 0)) (BLACK_KING . '(5 . 1)))
		      '#(2 1 1 2 2 1 1)))
(define player1 (list WHITE 
		      "Achilles" 
		      HUMAN 
		      '((WHITE_PAWN . '(0 . 11)) (WHITE_PAWN . '(1 . 11)) (WHITE_PAWN . '(2 . 11))
						(WHITE_PAWN . '(3 . 11)) (WHITE_PAWN . '(4 . 11))
						(WHITE_PAWN . '(5 . 11)) (WHITE_PAWN . '(6 . 11))
						(WHITE_PAWN . '(7 . 11)) (WHITE_PAWN . '(8 . 11))
						(WHITE_PAWN . '(9 . 11)) (WHITE_PAWN . '(10 . 11))
						(WHITE_PAWN . '(11 . 11)) (WHITE_KING . '(6 . 10)))
		      '#(2 1 1 2 2 1 1)))

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
