;;; Learning and Exercises from Practical Commons Lisp
;; CHAPTER 2

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defvar *db* nil)

(defun add-record (cd) (push cd *db*))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
    (prompt-read "Title")
    (prompt-read "Artist")
    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
    (y-or-n-p "Ripped [y/n]")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
	(if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun select-by-artist (artist)
  (remove-if-not
    #'(lambda (cd) (equal (getf cd :artist) artist))
    *db*))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
	(if title	(equal (getf cd :title) title) t)
	(if artist	(equal (getf cd :artist) artist) t)
	(if rating	(equal (getf cd :rating) rating) t)
	(if ripped-p	(equal (getf cd :ripped) ripped) t))))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
	(mapcar
	  #'(lambda (row)
	      (when (funcall selector-fn row)
		(if title	(setf (getf row :title) title))
		(if artist	(setf (getf row :artist) artist))
		(if rating	(setf (getf row :rating) rating))
		(if ripped-p	(setf (getf row :ripped) ripped)))
	      row) *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun new-account (name &optional (balance 0.00)
			 (interest-rate .06))
  "Create a new account that knows the following messages:"
  (let ((name name) (balance balance) (interest-rate interest-rate))
  #'(lambda (message)
      (case message
	(withdraw #'(lambda (amt)
		      (if (<= amt balance)
			(decf balance amt)
			'insufficient-funds)))
	(deposit #'(lambda (amt) (incf balance amt)))
	(balance #'(lambda () balance))
	(name    #'(lambda () name))
	(interest #'(lambda ()
		      (incf balance
			    (* interest-rate balance))))))))

(defun get-method (object message)
  "Return the method that implements the message for this object"
  (funcall object message))

(defun send (object message &rest args)
  "Get the function to implement the message and apply the function to the args"
  (apply (get-method object message) args))

(defmacro define-class (class inst-vars class-vars &body methods)
  "Define a class for OOP"
  ;; Define constructor and generic functions for methods
  `(let ,class-vars
     (mapcar #'ensure-generic-fn ',(mapcar #'first methods))
     (defun ,class ,inst-vars
       #'(lambda (message)
	   (case message
	     ,@(mapcar #'make-clause methods))))))

(defun make-clause (clause)
  "Translate a message from define-class into a case clause."
  `(,(first clause) #'(lambda ,(second clause) .,(rest clause))))


