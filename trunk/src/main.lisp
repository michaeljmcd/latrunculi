; Latrunculi
; (c) Michael J. McDermott, 2006
; Licensed under the GPL v.2

(defpackage #:latrunculi
  (:use #:common-lisp)
  (:export #:start)
  (:export #:game-mouse-handler)
  (:export #:game-display)
  (:export #:affect-captures)
  (:export #:at-offset)
  (:export #:move-piece)
  (:export #:make-move)
  (:export #:filter-captured-pieces)
  (:export #:get-cell)
  (:export #:*board*))

(in-package #:latrunculi)

(defconstant +AI+ 0)
(defconstant +HUMAN+ 1)

(defconstant +BLACK+ 1)
(defconstant +WHITE+ 0)

(defconstant +COLS+ 12)
(defconstant +ROWS+ 8)

(defconstant +EMPTY+ -1)
(defconstant +WHITE_KING+ 0)
(defconstant +WHITE_PAWN+ 2)
(defconstant +BLACK_KING+ 1)
(defconstant +BLACK_PAWN+ 3)

(defconstant +INFINITY+ 1e38)
(defconstant +NEGATIVE-INFINITY+ -1e37)

(defclass player ()
  ((color :accessor player-color)
   (name :accessor player-name)
   (controller :accessor player-controlled-by)
   (pieces :accessor piece-list)
   (ai-settings :accessor get-ai-settings))
  )

(defvar ai-settings0 (make-hash-table))
(setf (gethash 'search-depth ai-settings0) 2)
(setf (gethash 'own-pawn-value ai-settings0) 95)
(setf (gethash 'opponent-pawn-value ai-settings0) 65)
(setf (gethash 'own-king-mobility-value ai-settings0) 0.0001)
(setf (gethash 'opponent-king-mobility-value ai-settings0) 0.0001)

(defvar *player0* (make-instance 'player))
(setf (slot-value *player0* 'color) +BLACK+)
(setf (slot-value *player0* 'name) "Ajax")
(setf (slot-value *player0* 'controller) +AI+)
(setf (slot-value *player0* 'pieces) 
      '((3 . (0 . 0)) (3 . (1 . 0)) 
        (3 . (2 . 0)) (3 . (3 . 0))
        (3 . (4 . 0)) (3 . (5 . 0))
        (3 . (6 . 0)) (3 . (7 . 0))
        (3 . (8 . 0)) (3 . (9 . 0))
        (3 . (10 . 0)) (3 . (11 . 0))
        (1 . (5 . 1))
      ))

(setf (slot-value *player0* 'ai-settings) ai-settings0)

(defvar ai-settings1 (make-hash-table))
(setf (gethash 'search-depth ai-settings1) 2)
(setf (gethash 'own-pawn-value ai-settings1) 95)
(setf (gethash 'opponent-pawn-value ai-settings1) 65)
(setf (gethash 'own-king-mobility-value ai-settings1) 0.0001)
(setf (gethash 'opponent-king-mobility-value ai-settings1) 0.0001)

(defvar *player1* (make-instance 'player))
(setf (slot-value *player1* 'color) +WHITE+)
(setf (slot-value *player1* 'name) "Achilles")
(setf (slot-value *player1* 'controller) +AI+)
(setf (slot-value *player1* 'pieces)
      '((2 . (0 . 7)) (2 . (1 . 7))
                      (2 . (2 . 7))
                      (2 . (3 . 7))
                      (2 . (4 . 7))
                      (2 . (5 . 7))
                      (2 . (6 . 7))
                      (2 . (7 . 7))
                      (2 . (8 . 7))
                      (2 . (9 . 7))
                      (2 . (10 . 7))
                      (2 . (11 . 7))
                      (0 . (6 . 6)))
      )
(setf (slot-value *player1* 'ai-settings) ai-settings1)

(defvar players (cons *player0* *player1*))

(defvar *board* 
  (make-array '(8 12) :initial-contents
              '((3 3 3 3 3 3 3 3 3 3 3 3)
                (-1 -1 -1 -1 -1 1 -1 -1 -1 -1 -1 -1)
                (-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                (-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                (-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                (-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                (-1 -1 -1 -1 -1 -1 0 -1 -1 -1 -1 -1)
                (2 2 2 2 2 2 2 2 2 2 2 2))
              :element-type 'integer
          ))

; create-game-board returns a 2D array representing the 12 x 8 board. 
; The board, 
; internally, will have the following representation: a list of lists, where 
; each list is a row. The list elements will be tuples having the 
; following legend:
;-1 - Empty 
; 0 - White King
; 1 - Black King
; 2 - White 'pawn'
; 3 - Black 'pawn'
;
; There is a reason for this. After doing this any white piece mod 2 = 0
; and any black piece mod 2 = 1. This provides a quick way to determine 
; color.
;
; A tuple will be of the form (L . I) where L is a value from the above
; legend and I is the ID for the piece if applicable and -1 if N/A. 

;(latrunculi:start)
