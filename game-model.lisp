(in-package :blackjack)

(defclass game-state ()
  ((dealer :initform (make-instance 'dealer)
	   :reader get-dealer)
   (players :initarg :players
	    :accessor get-players
	    :initform nil)
   (shoe :initform (get-new-shoe)
	 :accessor get-shoe)
   (hand-counter :initform 0
		 :accessor get-hand-counter)))

(defvar *global-state* (make-instance 'game-state))

(defmethod %get-cards ((n integer))
  (loop for i from 1 upto n
     collect (pop (get-shoe *global-state*))))

(defmethod deal-hand ((player human) &key bet)
  (assert (typep bet 'integer))
  (let ((hand (make-instance 'hand :cards (%get-cards 2))))
    (push hand (get-hands player))
    (place-bet player hand bet)
    hand))

(defmethod deal-hand ((dealer dealer) &key)
  (let ((hand (make-instance 'hand :cards (%get-cards 2))))
    (push hand (get-hands dealer))
    hand))

(defmethod can-hit ((hand hand))
  (and (not (double-down-p hand))
       (not (zerop (compute-value hand)))))

(defmethod hit ((hand hand))
  (assert (can-hit hand))
  (push (car (%get-cards 1)) (get-cards hand)))

(defmethod can-double-down ((hand hand) (human human))
  (and (not (double-down-p hand))
       (= 2 (length (get-cards hand)))
       (>= (get-chips human) (get-bet hand))))

(defmethod double-down ((hand hand) (player human))
  (assert (can-double-down hand player))
  (place-bet player hand (get-bet hand))
  (hit hand)
  (setf (double-down-p hand) t))

(defmethod can-split ((hand hand) (player human))
  (and (= 2 (length (get-cards hand)))
       (eq (get-face (first (get-cards hand)))
	      (get-face (second (get-cards hand))))))

(defmethod split ((hand hand) (player player))
  (assert (can-split hand player))
  (let ((new-hand (deal-hand player :bet (get-bet hand)))
	(split (second (get-cards hand))))
    (setf (second (get-cards hand)) (second (get-cards new-hand)))
    (setf (second (get-cards new-hand)) split)))

(defun end-hand ()
  "Evaluate the outcome of the hands on the table"
  (loop for player in (get-players *global-state*)
     with dealer-value = (compute-value (get-dealer-hand))
     ;;check if each 'hand' won or loss, and update the chip-count appropriately
     do (while (get-hands player)
	  (let* ((hand (pop (get-hands player)))
		 (value (compute-value hand)))
	    (cond 
	      ((> value dealer-value) (incf (get-chips player) (* 2 (get-bet hand))))
	      ((< value dealer-value) t)
	      (t (incf (get-chips player) (get-bet hand)))))) ;;push if it's a tie       
     finally (progn
	       (incf (get-hand-counter *global-state*))
	       (setf (get-shoe *global-state*) (get-new-shoe))
	       (setf (get-hands (get-dealer *global-state*)) nil))))

(defun get-live-players ()
  (remove-if #'(lambda (player)
		 (not (plusp (get-chips player)))) ;;players with non-positive chip counts don't get to play anymore
	     (get-players *global-state*)))

(defun get-dealer-hand ()
  (car (get-hands (get-dealer *global-state*))))

(defmethod dealer-moves ((hand hand))
  (let ((value (compute-value hand))
	(call-again t))
    (cond ((= 0 value) (setf call-again nil)) ;;busted
	  ((< value 17) (hit hand))
	  ((and (= value 17)
		*dealer-hit-soft-17*
		(soft-seventeen-p hand))
	   (hit hand))
	  (t (setf call-again nil)))
    (if call-again
	(dealer-moves hand)
	hand)))