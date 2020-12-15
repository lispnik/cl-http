;;; -*- package: ("XML-PARSER") -*-
;;;
;;; this version (C) mecom gmbh 24.11.97
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.

#|
<DOCUMENTATION>
 <DESCRIPTION>
 definition for <CODE>XML-TEXT</CODE> provide the DOM-specified wrapper around
 text strings. it's here for conformity, but all of the behaviour except the
 relation to the container can be as well accomplished by incorporating strings
 directly... 
 </DESCRIPTION>
 <CHRONOLOGY>
  <DELTA><DATE>19921202</DATE>
   %xml-element.valid?
  </DELTA>
 <DELTA><DATE>19980102</DATE>
  CHARACTER-DATA, PARSED-CHARACTER-DATA instead of XML-TEXT.
  distinguished PRINT-OBJECT methods.
  </DELTA>
 </CHRONOLOGY>
</DOCUMENTATION>
|#

(in-package :XML-PARSER)


(defMethod node-class
           ((class (eql 'parsed-character-data)) (op t) (context t))
  *xml-pcdata-class*)
(defMethod node-class
           ((class (eql 'character-data)) (op t) (context t))
  *xml-cdata-class*)

(defClass xml-text (xml-node)
  ())
(defClass parsed-character-data (xml-text)
  ())
(defClass character-data (xml-text)
  ())

(defMethod process-markup-element
           ((node xml-text) (stream t))
  (xml-node.append-element *parent-node* node))

(defMethod print-object
           ((datum xml-text) (stream t))
  (print-unreadable-object (datum stream :type t)
    (princ (xml-node.content datum) stream)))

(defMethod print-object
           ((datum parsed-character-data) (stream t))
  (if *xml-print-readably*
    (princ (xml-node.content datum) stream)
    (call-next-method)))

(defMethod print-object
           ((datum character-data) (stream t))
  (cond (*xml-print-readably*
         (write-string "<![CDATA[" stream)
         (princ (xml-node.content datum) stream)
         (write-string "]]>" stream))
        (t
         (call-next-method))))

(defMethod %xml-element.valid?
           ((datum xml-text) (succeed function) fail)
  (funcall succeed datum fail))


#|
(let ((*xml-print-readably* t))
  (inspect (print #!"<TEST>asdf<NESTED att=yes>content

</NESTED></TEST>"))
  (print (make-instance 'xml-node :content "ASDF"))
  (print (make-instance 'xml-named-node :content "QWER" :name 'test)))
#!"<TEST>asdf<NESTED>content</NESTED></TEST>"
(make-instance 'xml-node :content "ASDF")
(make-instance 'xml-named-node :content "QWER" :name 'test)

 |#
:EOF
