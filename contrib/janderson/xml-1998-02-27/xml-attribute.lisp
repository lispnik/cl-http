;;; -*- Package: ("XML-PARSER") -*-
;;;
;;; this version (C) mecom gmbh 24.11.97
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.

(in-package :xml-parser)

(defMethod node-class
           ((node (eql 'attribute)) (name t) (value t))
  *attribute-class*)

(defClass xml-attribute (xml-named-node)
  ((name
    :accessor xml-attribute.name)
   (content
    :accessor xml-attribute.value
    :type string)))

(defMethod initialize-instance
           ((self xml-attribute) &key value)
  (setf (xml-attribute.value self) value)
  (call-next-method))  


(defMethod print-object
           ((datum xml-attribute) (stream t))
  (if *xml-print-readably*
    (if *parent-node*
      (format stream "~s=~s" (xml-attribute.name datum)
              (xml-attribute.value datum))
      (format stream "#i(~s :name ~s :content ~s)"
              (type-of datum)
              (xml-attribute.name datum)
              (xml-attribute.value datum)))
    (print-unreadable-object (datum stream :type t :identity t)
      (format stream "~s=~s" (xml-attribute.name datum)
              (xml-attribute.value datum)))))

#|
(let ((*xml-print-readably* t))
  (print (make-instance 'xml-attribute :name 'test :content "ASDF")))
 |#

:EOF
