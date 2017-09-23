;;; server.lisp
;;; CL-CRM
;;; Hunchentoot server

(in-package :cl-crm)

(defmacro standard-page ((&key title) &body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
	(:html :xmlns "http://www.w3.org/1999/xhtml"
	       :xml\:lang "en"
	       :lang "en"
	       (:head
		(:meta :http-equiv "Content-Type"
		 		   "text/html;charset=utf-8")
		(:title ,title)
		(:link :type "text/css"
		       :rel "stylesheet"
		       :href "/style.css"))
	       (:body
		 (:div :id "header" ; CL-CRM Header
		       (:span :class "strapline"
			      "CL-CRM Simple CRM written in Common Lisp"))
		 ,@body))))

(start-server :port 8080)

(push (create-prefix-dispatcher "/index.html" 'index) *dispatch-table*)

(defun index ()
  (standard-page (:title "CL-CRM")
		 (:h1 "CL-CRM, the Common Lisp CRM")
		 (:p "More to come...")))

