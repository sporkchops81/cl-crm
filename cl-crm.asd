;;;; test.asd

(asdf:defsystem #:cl-crm
 :description "A simple Common Lisp CRM system"
 :author "William R. Cunningham, Jr. <wrcunningham@gmail.com>"
 :license "BSD 3-clause"
 :depends-on (#:cl-who
              #:hunchentoot)
 :serial t
 :components ((:file "package")
              (:file "cl-crm")))
