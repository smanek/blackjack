(in-package :blackjack)

(defun start-game ()
  ;;initialize the game (setup, get player names)
  (setf *global-state* (make-instance 'game-state))
  (add-player) ;;ensure at least one player is added
  (let ((game-started nil))
    (while (not game-started)
      (present-menu (cons "Add another player" #'add-player)
		    (cons "Start game" #'(lambda ()
					   (setf game-started t))))))

  ;;the main loop
  (while (not (null (get-live-players))) ;;The game's on while at least one player has chips 
    ;;deal the initial hands
    (deal-hand (get-dealer *global-state*))
    (deal-players)
    (display *global-state*)
    
    ;;players make moves, then dealer makes moves, then all is revealed
    (player-moves)
    (dealer-moves (get-dealer-hand))
    (format t "Dealer has: ")
    (display (get-dealer-hand)) ;;dealer can only have one hand (no spliting, etc)
    (end-hand)
    (display-chip-counts))
  (format t "All players are out of chips. Game Over~%"))

(defun add-player ()
  (push (make-instance 'human :name (prompt "Please enter the player's name:" :string :max-length 15))
	(get-players *global-state*)))

(defun deal-players ()
  (mapc #'(lambda (player)
	    (let ((max-bet (min 100 (get-chips player))))
	      (deal-hand player :bet (prompt (format nil "How much would ~A like to bet (1-~A)?"
						     (get-name player) max-bet)
					     :integer
					     :max max-bet))))
	(get-live-players)))

(defun display-chip-counts ()
  (format t "~%Chip Summary:~%")
  (mapc #'(lambda (player)
	    (print-centered (get-name player) 20)
	    (print-centered (get-chips player) 10)
	    (newline))
	(get-players *global-state*))
  (newline))

(defun player-moves ()
  (mapc #'(lambda (player)
	    (while (not (every #'(lambda (hand) ;;repeatadly go over a player's hands to account for splits
				   (moves-done-p hand))
			       (get-hands player)))
	      (mapc #'(lambda (hand)
			(let ((still-moves t))
			  (while still-moves
			    (format t "For ~A's hand: " (get-name player))
			    (display hand)
			    (let ((options (list (cons "Stay" #'(lambda ()
								  (setf still-moves nil)
								  (setf (moves-done-p hand) t))))))
			      (when (can-hit hand) (push (cons "Hit" #'(lambda () (hit hand))) options))			    
			      (when (can-double-down hand player) (push (cons "Double Down" #'(lambda () (double-down hand player))) options))
			      (when (can-split hand player) (push (cons "Split" #'(lambda () (split hand player))) options))
			      
			      (if (> (length options) 1) ;;don't bother prompting if the user can only 'stay'
				  (when (apply #'present-menu options)
				    (format t "~A now has:~%" (get-name player))
				    (mapc #'(lambda (h)
					      (display h))
					  (get-hands player))) 
				  (progn 
				    (setf still-moves nil)
				    (setf (moves-done-p hand) t)))))))
		    (get-hands player))))
	(get-live-players)))
