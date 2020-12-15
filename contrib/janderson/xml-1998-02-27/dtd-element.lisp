;;; -*- mode: LISP; package ("XML-PARSER")
;;;
;;; this version (C) mecom gmbh 24.11.97
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.

#|
<DOCUMENTATION>
<DESCRIPTION>
common parent class for dtd element declarations and references.
<P>
the elements are read from a stream and appended to the dtd associated with
the stream.
</DESCRIPTION>
<CHRONOLOGY>
<DATE>19971207 jaa</DATE>
<DELTA>
 factored out element declaration and reference to distinct subclasses
 </DELTA>
</CHRONOLOGY> 
</DOCUMENTATION>
|#

(in-package :XML-PARSER)

(defClass dtd-element (dtd-node)
  ((parent
    :accessor dtd-element.dtd
    :type (or null dtd))
   (name
    :accessor dtd-element.name
    :accessor dtd-element.name
    :type symbol)
   (qualified-name
    :initarg :qualified-name
    :accessor dtd-element.qualified-name
    :type symbol)
   (attdefs
    :initarg :attdefs  :initform nil
    :accessor dtd-element.attdefs))
  (:documentation
   "<CODE>DTD-ELEMENT</CODE> is the abstract root class for elements in a
    document type description."))

(defMethod dtd.dtd-element
           ((parent dtd-element) (id symbol))
  "resolve a model id relative to the parent's dtd."
  (dtd.dtd-element (dtd-element.dtd parent) id))

(defMethod element.dtd
           ((element dtd-element))
  (dtd-element.dtd element))



(defMethod process-markup-element
           ((element dtd-element) (stream t))
  ;; append to the current dtd (n.b. not the parent)
  (xml-node.append-element *dtd* element))

(defmethod xml-node.append-element
           ((parent null) (element dtd-element))
  (xml-validity-error nil
                      "no dtd present to contain element declaration: ~s."
                      element))



:EOF
