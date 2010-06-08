(in-package :blackjack)

(defun cartesian-product (&rest rest)
  "Compute the cartesian product of n-lists"
  (assert (not (null rest)))
  (assert (> (length rest) 1))
  (assert (every #'(lambda (x)
		     (typep x 'cons))
		 rest))
  (loop for lst in rest
       with res = (list nil)
       do (let ((new nil))
	    (loop for elt in lst 
	       do (loop for c in res
		     do (push (cons elt c) new)))
	    (setf res new))
     finally (return (mapcar #'reverse res))))

(defun array-exchange (array i j)
  "Exchange the the values at two indices in an array"
  (let ((temp (aref array i)))
    (setf (aref array i) (aref array j))
    (setf (aref array j) temp)))

(defun copy-array (array)
  (let ((dims (array-dimensions array)))
    (adjust-array
     (make-array dims :element-type (array-element-type array) :displaced-to array)
     dims)))

(defun copy-list-to-array (lst)
  (make-array (length lst) :initial-contents lst))

(defun %shuffle-helper (array)
  "Shuffle the array in-place"
  (loop for i from (1- (length array)) downto 1
     do (array-exchange array (random (1+ i)) i)
     finally (return array)))

(defun shuffle (seq)
  "Non-destructive Fisher Yates Shuffle (from Knuth TAOCP 2)"
  (etypecase 
      seq
    (list (coerce (%shuffle-helper (copy-list-to-array seq)) 'list))
    (array (%shuffle-helper (copy-array seq)))))

(defmacro define-constant (name value &optional doc)
  "Define a constant exactly once"
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

;;there's probably a format control string for this, but I didn't see it
(defun print-centered (item width &optional (stream t))
  "Aesthetically print something centered"
  (let* ((string-rep (format nil "~A" item))
	 (total (- width (length string-rep))))
    (print-repeated " " (ceiling total 2) stream)
    (format stream "~A" string-rep)
    (print-repeated " " (floor total 2) stream)))

(defun print-repeated (string n &optional (stream t))
  "Print the string n times."
  (loop for i from 1 upto n do (princ string stream)))

(defun newline (&optional (count 1) (stream t))
  "Print a newline"
  (print-repeated #\Newline count stream))

(defmacro while (expression &body body)
  `(tagbody
    start 
      (if (not ,expression) (go end))
      ,@body
      (go start)
    end))