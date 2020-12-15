;;; -*- package: ("XML-PARSER") -*-

(in-package :xml-parser)

(defVar *lisp-dtd*)

(let ((*default-pathname-defaults*
       #+:mcl (directory-namestring *load-truename*)))
  (declare (special *default-pathname-defaults*))
  (setf *lisp-dtd* (read-dtd-stream #p"lisp.dtd")))

#|
 the definition element classes are generated from the dtd
 in the form such as:

(defClass dc::defClass (dc::definition)
  ()
  (:default-initargs :dtd-element 'defClass)
  (:documentation
   #.(definition-element-documentation 'defclass)))

 |#

:EOF
