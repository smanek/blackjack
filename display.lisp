(in-package :blackjack)

(defgeneric display (object &key))

(defmethod display ((hand hand) &key (one-up nil))
  ;;showing one card only makes sense when you've just got two cards
  (assert (or (not one-up) 
	      (= 2 (length (get-cards hand)))))
  (let* ((cards-to-display (if one-up
			       (list (car (get-cards hand)) "Face Down")
			       (get-cards hand)))
	 (len (length cards-to-display))
	 (bet (get-bet hand)))
    (loop for card in cards-to-display
       with count = 0
       do (progn 
	    (display card)
	    (incf count)
	    (when (not (= count len)) ;;print | between cards
	      (princ " | ")))
       finally (when bet
		 (print-centered (format nil "(Chips: ~A)" bet) 15)))))

(defmethod %player-display-helper ((player player) one-up)
  (print-repeated "*" 30)
  (print-centered (get-name player) 20)
  (print-repeated "*" 30)
  (newline)
  (mapc #'(lambda (hand)
	    (display hand :one-up one-up))
	(get-hands player))
  (newline 2))

(defmethod display ((d dealer) &key)
  (%player-display-helper d t))

(defmethod display ((h human) &key)
  (%player-display-helper h nil))

(defmethod display ((hand hand) &key (one-up nil))
  ;;showing one card only makes sense when you've just got two cards
  (assert (or (not one-up) 
	      (= 2 (length (get-cards hand)))))
  (let* ((cards-to-display (if one-up
			       (list (car (get-cards hand)) "Face Down")
			       (get-cards hand)))
	 (len (length cards-to-display))
	 (bet (get-bet hand)))
    (loop for card in cards-to-display
       with count = 0
       do (progn 
	    (display card)
	    (incf count)
	    (when (not (= count len)) ;;print | between cards
	      (princ " | ")))
       finally (progn (when bet
			(print-centered (format nil "(Bet: ~A)" bet) 12))
		      (newline)))))

(defmethod %player-display-helper ((player player) one-up)
  (print-repeated "*" 30)
  (let ((name (typecase player
		(dealer (get-name player)) ;;no 'chip count' for dealers
		(human (format nil "~A (~A Chips)" (get-name player) (get-chips player))))))    
    (print-centered name 20))
  (print-repeated "*" 30)
  (newline)
  (mapc #'(lambda (hand)
	    (display hand :one-up one-up))
	(get-hands player))
  (newline 2))

(defmethod display ((d dealer) &key)
  (%player-display-helper d t))

(defmethod display ((h human) &key)
  (%player-display-helper h nil))

(defmethod display ((c card) &key)
  (print-centered (format nil "~@(~A~) of ~@(~A~)" (get-face c) (get-suite c))
		  20))

(defmethod display ((s string) &key (space 20))
  (print-centered s space))

(defmethod display ((state game-state) &key)
  (display (get-dealer state))
  (mapc #'display (get-players state)))

(defmethod prompt ((prompt string) (input (eql :integer)) &key (max nil) (min 1))
  (let ((res nil))
    (while (not res)
      (format t "~A " prompt)
      (handler-case 
	  (progn
	    (setf res (parse-integer (read-line)))
	    (when (and max (> res max))
	      (setf res nil))
	    (when (and min (< res min))
	      (setf res nil)))	
	(t () (setf res nil))))
    res))

(defmethod prompt ((prompt string) (input (eql :char)) &key (min #\a) (max #\z) (lower-case t))
  (let ((res nil))
    (while (not res)
      (format t "~A " prompt)
      (setf res (read-char))
      (when lower-case
	(setf res (char-downcase res)))
      (when (and max (> (char-code res) (char-code max)))
	(setf res nil))
      (when (and min (< (char-code res) (char-code min)))
	(setf res nil)))
    res))

(defmethod prompt ((prompt string) (input (eql :string)) &key (min-length 1) (max-length 10))
  (let ((res nil))
    (while (not res)
      (format t "~A " prompt)
      (setf res (read-line))
      (when (and min-length (> min-length (length res)))
	(setf res nil))
      (when (and max-length (< max-length (length res)))
	(setf res nil)))
    res))

(defun present-menu (&rest options)
  "This fn takes a a series of cons cells as it's argument. 
   The car of each cell is a string prompt, and the cdr of
   each cell is a nullary function that is executed if selected."
  (assert (> (length options) 0))
  (assert (every #'(lambda (option)
		     (typep (car option) 'string)
		     (typep (cdr option) 'function))
		 options))
  (let ((counter 0)
	(fn-hash (make-hash-table)))
    (if (= 1 (length options)) ;;if there's only one option, just do it
	(funcall (cdar options))
	(progn
	  (mapc #'(lambda (option)
		    (let ((number (incf counter)))
		      (format t "~A. ~A~%" number (car option))
		      (setf (gethash number fn-hash) (cdr option))))
		options)
	  (funcall (gethash 
		    (prompt (format nil "Please select an option (1-~A)" counter) :integer :max counter)
		    fn-hash))))))

