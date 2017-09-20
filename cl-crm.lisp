;;; CL-CRM
;;; A basic CRM system implemented in Common Lisp

(in-package :cl-user)

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
   (office-phone :accessor :office-phone :initarg :office-phone)
   (cell-phone :accessor :cell-phone :initarg :cell-phone)
   (office-address :accessor :office-address :initarg :office-address)
   (home-address :accessor :home-address :initarg :home-address)
   (birthday :accessor :birthday :initarg :birthday)
   (anniversary :accessor :anniversary :initarg :anniversary)))

;; General in-memory database of contacts
(defvar *contacts* ())

;; A test contact
(setf The-Colonel (make-instance 'contact
			    :prefix "Col."
			    :first-name "Harlan"
			    :last-name "Sanders"
			    :gender "M"
			    :nickname "The Colonel"))

(defun contacts-from-nickname (name)
  (find name *contacts* :test #'string-equal
			:key #'nickname))

