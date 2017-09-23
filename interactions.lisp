;;; interactions.lisp
;;; CL-CRM
;;; Interactions with contacts

(in-package :cl-crm)

;; General in-memory database of interactions
;;
(defvar *interactions* ())

;; Define Interactions Class
;;
(defclass interaction ()
  ((date :accessor date :initarg :date)
   (location :accessor location :initarg :location)
   (contacts :accessor contacts :initarg :contacts :initform (error "Must specify at least one contact for the interaction"))
   (one-clicked :accessor one-clicked :initarg :one-clicked)))

(defun add-interaction (contacts date location)
  "Add an interaction to the database"
  (push (make-instance 'interaction :contacts contacts :date date :location location) *interactions*))

(format t "interactions.lisp loaded...")


