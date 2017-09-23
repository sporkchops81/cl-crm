;;; contacts.lisp
;;; CL-CRM
;;; A basic CRM system implemented in Common Lisp
;; NOTE: use c-w _ to maximize a vim window
;;       use c-b m-left/right/up/down to resize tmux pane

(in-package :cl-crm)

;; General in-memory database of contacts
(defvar *contacts* ())

;; Define Contact Class
;; Slots:
;;   Id
;;   First Name
;;   Middle Name
;;   Last Name
;;   Suffix
;;   Nickname
;;   Gender
;;   Spouse
;;   Children
;;   Relatives
;;   Friends
;;   Office Phone
;;   Cell Phone
;;   Fax Number
;;   Office Address
;;   Home Address
;;   Birthday
;;   Anniversary
;;
(defclass contact ()
  ((id :reader id :initform (gensym))
   (prefix :accessor prefix :initarg :prefix)
   (first-name :accessor first-name :initarg :first-name :initform (error "Must supply a first name."))
   (middle-name :accessor middle-name :initarg :middle-name)
   (last-name :accessor last-name :initarg :last-name :initform (error "Must supply a last name."))
   (suffix :accessor suffix :initarg :suffix)
   (nickname :accessor nickname :initarg :nickname)
   (gender :accessor gender :initarg :gender)
   (spouse :accessor spouse :initarg :spouse)
   (children :accessor children :initarg :children)
   (relatives :accessor relatives :initarg :relatives)
   (office-phone :accessor office-phone :initarg :office-phone)
   (cell-phone :accessor cell-phone :initarg :cell-phone)
   (office-address :accessor office-address :initarg :office-address)
   (home-address :accessor home-address :initarg :home-address)
   (birthday :accessor birthday :initarg :birthday)
   (anniversary :accessor anniversary :initarg :anniversary)))

;; make-simple-contact
(defun make-simple-contact (first-name middle-name last-name suffix nickname)
  "Return a contact made using name parameters only."
  (make-instance 'contact :first-name first-name :middle-name middle-name :last-name last-name :suffix suffix :nickname nickname))

(defun contacts-from-nickname (name)
  "Return contacts from the DB that match the provided nickname string"
  (find name *contacts* :test #'string-equal
			:key #'nickname))

(defmethod print-object ((object contact) stream)
	      (print-unreadable-object (object stream :type t)
		(with-slots (nickname last-name) object
		  (format stream "~a ~a" nickname last-name))))

(format t "contacts.lisp loaded...")
