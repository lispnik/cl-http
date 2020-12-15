;;; -*- package: ("XML-PARSER")

(in-package :xml-PARSER)


#|
 here is a simple demonstration of XML parser

 |#

(defParameter *xml-doc* (read-xml-stream #4p"xml:email.xml"))
(defParameter *xml-doc* (read-xml-stream #4p"xml:lisp.xml"))

(inspect *xml-doc*)

(let ((*xml-print-readably* t))
  (print (xml-document.element *xml-doc*)))

(let ((*xml-print-readably* t))
  (print *xml-doc*))
