(in-package :blackjack)

(defclass hand ()
  ((cards :accessor get-cards 
	  :initarg :cards
	  :initform nil)
   (bet   :accessor get-bet   
	  :initarg :bet
	  :initform nil)
   (double-down :initform nil
		:accessor double-down-p)
   (moves-done  :initform nil
		:accessor moves-done-p)))

(let ((player-count 0))
  (defclass player ()
    ((name :initarg :name
	   :initform (format nil "Player ~A" (incf player-count))
	   :reader get-name)
     (hands :initarg nil
	    :initform nil
	    :accessor get-hands))))

(defclass human (player)
  ((chips :initform *starting-chips*
	  :accessor get-chips)))

(defclass dealer (player)
  ((name :initform "The Dealer")))

(defmethod compute-value ((hand hand))  
  (apply #'values
	 (or (remove-duplicates
	      (remove-if #'(lambda (value)
			     (> value 21))
			 (sort (cards-value (get-cards hand)) #'>)))
	     (list 0))))

(defmethod place-bet ((p human) (h hand) (b integer))
  (assert (<= b (get-chips p)))
  (decf (get-chips p) b)
  (setf (get-bet h) (+ b (or (get-bet h) 0))))

(defmethod soft-seventeen-p ((hand hand))
  (let ((values (multiple-value-list (compute-value hand))))
    (and (= 17 (car values))
	 (not (null (cdr values))))))