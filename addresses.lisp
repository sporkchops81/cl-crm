;;; CL-CRM
;;; A basic CRM system implemented in Common Lisp
;; NOTE: use c-w _ to maximize a vim window
;;       use c-b m-left/right/up/down to resize tmux pane

(in-package :cl-crm)

;; General in-memory database of zip-codes
(defvar *zip-codes* (make-hash-table))

;; General in-memory database of counties
(defvar *counties* ())

;; General in-memory database of states
(defvar *states* ())

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

;; Create alist of state abbreviations (as symbols
;; and state names
;; Access the cons pairs with (assoc <symbol> <alist>)
;; Return only the state name with (rest (assoc <symbol> <alist>))
(setf states '((AL . "Alabama")
	       (AK . "Alaska")
	       (AZ . "Arizona")
	       (AR . "Arkansas")
	       (CA . "California")
	       (CO . "Colorado")
	       (CT . "Connecticut")
	       (DE . "Delaware")
	       (FL . "Florida")
	       (GA . "Georgia")
	       (HI . "Hawaii")
	       (ID . "Idaho")
	       (IL . "Illinois")
	       (IN . "Indiana")
	       (IA . "Iowa")
	       (KS . "Kansas")
	       (KY . "Kentucky")
	       (LA . "Louisiana")
	       (ME . "Maine")
	       (MD . "Maryland")
	       (MA . "Massachusetts")
	       (MI . "Michigan")
	       (MN . "Minnesota")
	       (MS . "Mississippi")
	       (MO . "Missouri")
	       (MT . "Montana")
	       (NE . "Nebraska")
	       (NV . "Nevada")
	       (NH . "New Hampshire")
	       (NJ . "New Jersey")
	       (NM . "New Mexico")
	       (NY . "New York")
	       (NC . "North Carolina")
	       (ND . "North Dakota")
	       (OH . "Ohio")
	       (OR . "Oregon")
	       (PA . "Pennsylvania")
	       (RI . "Rhode Island")
	       (SC . "South Carolina")
	       (SD . "South Dakota")
	       (TN . "Tennessee")
	       (TX . "Texas")
	       (UT . "Utah")
	       (VT . "Vermont")
	       (VA . "Virgnia")
	       (WA . "Washington")
	       (WV . "West Virgnia")
	       (WY . "Wyoming")
	       (WI . "Wisconsin")
	       (DC . "Washington, D.C.")))
		


