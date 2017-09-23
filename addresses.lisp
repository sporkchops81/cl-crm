;;; CL-CRM
;;; A basic CRM system implemented in Common Lisp
;; NOTE: use c-w _ to maximize a vim window
;;       use c-b m-left/right/up/down to resize tmux pane

(in-package :cl-crm)

;; General in-memory database of zip-codes
(defvar *zip-codes* (make-hash-table))

;; General in-memory database of counties
(defvar *counties* ())

(defclass address ()
  ((street-number :accessor street-number :initarg :street-number)
   (street-name :accessor street-name :initarg :street-name :initform (error "Must supply a street name"))
   (street-name-2 :accessor street-name-2 :initarg :street-name-2)
   (attention :accessor attention :initarg :attention)
   (city :accessor city :initarg :city :initform (error "Must supply a city"))
   (county :accessor county :initarg :county)
   (state :accessor state :initarg :state :initform (error "Must supply a state"))
   (zip-code :accessor zip-code :initarg :zip-code)))

;; Utility functions
(defun populate-zip-code (zip-code city)
  "Add a zip code:city pair to the zipc-odes database."
  (setf (gethash zip-code *zip-codes*) city))

(defun populate-counties (county state)
  "Add a county:state pair to the counties database. Use two-letter state abbreviation."
  (setf (gethash county *counties*) state))

(defun populate-zip-codes-with-list (zips city)
  "Add a (list zips) to one 'city' and store in the database."
  (dolist (zip zips)
    (populate-zip-code zip city)))

(format t "addresses.lisp loaded...")
