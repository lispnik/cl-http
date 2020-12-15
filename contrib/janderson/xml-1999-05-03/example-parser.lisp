;;; -*- package: ("XML-PARSER")

(in-package :xml-PARSER)


#|
 here is a simple demonstration of XML parser. note that it specializes the
 class of the outer element - thus the defclass form
 

 |#

(with-local-context (pathname (choose-file-default-directory ))
  
  (defclass xml::clos-mail (xmlp::element) ())
  (inspect
   (list 1
         (read-xml-stream "xml/email.xml" :preserve-whitespace t)))
  (inspect
   (list 2
         (read-xml-stream "xml/lisp.xml")))
  
  (let ((*print-readably* t))
    (print (read-xml-stream "xml/email.xml" :preserve-whitespace t)))
  
  (let ((*print-readably* t))
    (print (read-xml-stream "xml/lisp.xml")))
  
  (xml-node.string (read-xml-stream "xml/email.xml" :preserve-whitespace t))
  )
