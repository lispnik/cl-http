;;; -*- package: "XML-PARSER"; -*-
;;;

"
<DOCUMENTATION>
 <DESCRIPTION>
 definition for <CODE>text</CODE> provide the DOM-specified wrapper around
 text strings. it's here for conformity, but all of the behaviour except the
 relation to the container can be as well accomplished by incorporating strings
 directly... 
 </DESCRIPTION>
 <COPYRIGHT HREF='defsystem.lisp|root().descendant(1,COPYRIGHT)'/>
 <CHRONOLOGY>
  <DELTA><DATE>19921202</DATE>
   %element.valid?
  </DELTA>
 <DELTA><DATE>19980102</DATE>
  CHARACTER-DATA, PARSED-CHARACTER-DATA instead of text.
  distinguished PRINT-OBJECT methods.
  </DELTA>
 <DELTA><DATE>19981114</DATE>
  REC-DOM-19981001:
   hierarchy is now character-data, text, character-data-section
  </DELTA>
 <DELTA DATE='19990428'>default class for serialized-data</DELTA>
 </CHRONOLOGY>
</DOCUMENTATION>
"

(in-package "XML-PARSER")

;;;
;;; metaclass

(defClass character-data-class (qualified-class)
  ()
  (:documentation
   "abstract root class for nodes which contain character data - parsed,
    unparsed, and serialized."))
   

(defMethod make-instance
           ((class character-data-class) &key content)
  ;; if no default class is specified, then use a string instead. this is
  ;; useful if the data is read only and will not be reserialized.
  (with-slots (default-class) class
    (if default-class
      (call-next-method)
      (if (stringp content)
        content
        (xml-form-error *markup-stream* "illegitimate character data: ~s."
                        content)))))

;;;
;;; instance class

(defClass character-data (comment-target content-markup)
  ()
  (:metaclass character-data-class))

(defClass text (character-data)
  ()
  (:metaclass character-data-class))

(defClass character-data-section (text)
  ()
  (:metaclass qualified-class))

(defClass serialized-data (xml-node)
  ()
  (:metaclass character-data-class)
  (:documentation
   "a character-data specialization for the purpose of encoding serialized
    content. when printed, no escaping is performed."))
(setf (slot-value (find-class 'serialized-data) 'default-class)
      'serialized-data)


;;;
;;; parsing 
;;;
;;; only cdata sections need to be 'parsed'

(defMethod reduce-production
           ((production character-data) &key)
  (setf *processed-node* production))

(defMethod reduce-production
           ((production string) &key)
  (setf *processed-node* production))

(defMethod reduce-production
           ((production (eql 'xml-1.0::CharData))
            &key
            ((XML-1.0::char data)))
  (unless *parse-suppress*
    (reduce-production (make-instance 'text :content data))))

(defMethod reduce-production
           ((production (eql 'XML-1.0::CDSect))
            &key
            ((XML-1.0::char data)))
  (unless *parse-suppress*
    (reduce-production (make-instance 'character-data-section :content data))))



(defMethod production-reader-macro
           ((production (eql 'XML-1.0::CDSect)) (stream t)
            &aux data)
  (handler-bind ;; just augment error message for context
    ((error #'(lambda (condition)
                (xml-warn production
                          "data: ~s."
                          data)
                condition)))
    (setf data (read-string-delimited-string "]]>" stream nil))
    (reduce-production production 'xml-1.0::char data)))

;;(production-reader-macro 'XML-1.0::CDSect (markup-stream " asdf>! & ]]> 123"))



(defMethod read-production
           ((production (eql 'xml-1.0::charData)) (stream t))
  (reduce-production production
                     'XML-1.0::char
                     (read-char-string
                      #'(lambda (c) (char= c *tag-open-char*))
                      stream)))

;;;
;;; model operations

(defMethod xml-node.string
           ((datum character-data))
  (xml-node.content datum))

(defMethod %element.valid?
           ((datum character-data) (succeed function) fail)
  (funcall succeed datum fail))



;;;
;;; printing

(defMethod print-object
           ((datum character-data) (stream t))
  (print-unreadable-object (datum stream :type t)
    (prin1 (xml-node.content datum) stream)))

(defMethod print-object
           ((datum character-data-section) (stream t))
  (cond (*print-readably*
         (write-string *cdata-open* stream)
         (write-string (xml-node.content datum) stream)
         (write-string *cdata-close* stream))
        (t
         (call-next-method))))

(defMethod print-object
           ((datum text) (stream t))
  (cond (*print-readably*
         (write-character-data-value (xml-node.content datum) stream))
        (t
         (call-next-method))))

(defMethod print-object
           ((datum serialized-data) (stream t))
  (cond (*print-readably*
         (write-string (xml-node.content datum) stream))
        (t
         (call-next-method))))

#|
(let ((*print-readably* t))
  (inspect (print #!"<TEST>asdf<NESTED att=yes>content

</NESTED></TEST>"))
  (print (make-instance 'xml-node :content "ASDF"))
  (print (make-instance 'xml-named-node :content "QWER" :name 'test)))
#!"<TEST>asdf<NESTED>content</NESTED></TEST>"
(make-instance 'xml-node :content "ASDF")
(make-instance 'xml-named-node :content "QWER" :name 'test)
(make-instance 'xmlp::serialized-data :content "asdf")

 |#

"XMLP"

