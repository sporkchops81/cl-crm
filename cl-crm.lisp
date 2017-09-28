;;; CL-CRM
;;; A basic CRM system implemented in Common Lisp
;; NOTE: use c-w _ to maximize a vim window
;;       use c-b m-left/right/up/down to resize tmux pane


(in-package #:cl-crm)

(format t "Starting up cl-crm...~%")
(format t "Loading contacts.lisp...~%")
(load "contacts.lisp")
(format t "Loading addresses.lisp...~%")
(load "addresses.lisp")
(format t "Loading interactions.lisp...~%")
(load "interactions.lisp")
(format t "Loading populate.lisp...~%")
(load "populate.lisp")

(format t "Loading server.lisp...~%")
(load "server.lisp")

