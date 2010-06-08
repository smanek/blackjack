(in-package :blackjack)

(defclass card ()
  ((suite :initarg :suite
	  :reader get-suite)
   (face  :initarg :face
	  :reader get-face)))

(defmethod %face-values ((face integer))
  (list face))

(defmethod %face-values ((face (eql :ace)))
  (list 11 1))

;;all non-integral and non-ace cards are worth 10
(defmethod %face-values (face)
  (list 10))

(defmethod %card-value ((c card))
  (%face-values (get-face c)))

(defun cards-value (cards)
  (assert (> (length cards) 1))
  (assert (every #'(lambda (card)
		     (typep card 'card))
		 cards))
  (mapcar #'(lambda (vals)
	      (reduce #'+ vals))
	  (apply #'cartesian-product (mapcar #'%card-value cards))))

(defun %get-deck ()
  (mapcar #'(lambda (x)
	      (destructuring-bind (suite face)
		  x
		(make-instance 'card :suite suite :face face)))
	  (cartesian-product +suites+ +faces+)))

(defun get-decks (n)
  (loop for i from 1 upto n appending (%get-deck)))

(defun get-new-shoe ()
  (shuffle (get-decks *decks-per-shoe*)))

(defmethod print-object ((c card) stream)
  (print-unreadable-object (c stream :type t)
    (with-slots (suite face)
	c
      (format stream "~A of ~A" face suite))))

