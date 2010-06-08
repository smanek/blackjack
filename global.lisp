(in-package :blackjack)

(define-constant +suites+ '(:spades :diamonds :hearts :clubs))
(define-constant +faces+ '(2 3 4 5 6 7 8 9 10 :jack :queen :king :ace))

(defparameter *starting-chips* 500)
(defparameter *dealer-hit-soft-17* t)
(defparameter *decks-per-shoe* 6)