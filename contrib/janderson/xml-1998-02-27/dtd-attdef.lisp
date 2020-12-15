;;; -*- mode: LISP; package ("XML-PARSER")
;;;
;;; this version (C) mecom gmbh 24.11.97
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.

#|
<DOCUMENTATION>
<DESCRIPTION>
 parsing and construction of attribute declarations
 </DESCRIPTION>
<CHRONOLOGY>
<DATE>19971217</DATE>
<DELTA>read-typed-processing-instruction reads parameters rather than
 attributes
 </DELTA>
<DATE>19971218</DATE>
<DELTA>support for NOTATION clause in attribute definitions
 </DELTA>
 </CHRONOLOGY>
</DOCUMENTATION>
 |#


(in-package :XML-PARSER)

(defMethod node-class
           ((node (eql 'attribute-definition)) (op t) (context t))
  *attribute-definition-class*)

(defclass dtd-attdef (xml-named-node)
  ((name
    :accessor dtd-attdef.name)
   (parent
    :initarg :context :initform nil 
    :accessor dtd-attdef.context
    :type (or null dtd-element))
   (type
    :initarg :type  :initform 'xml::cdata
    :accessor dtd-attdef.type)
   (mode
    :initarg :mode :initform nil
    :accessor dtd-attdef.mode)
   (content
     :initarg :default :initform nil
     :accessor dtd-attdef.default)))

(defClass dtd-attlist (xml-reference-node)
  ((referent
    :initarg :dtd-element :initform nil
    :type (or symbol dtd-element))
   (content
    :initarg :attdefs :initform nil
    :accessor dtd-attlist.attdefs)))


(defmethod print-object ((object dtd-attdef) stream)
  (handler-case
    (progn (format stream "~S " (dtd-attdef.name object))
           (let ((type (dtd-attdef.type object)))
             (when (and (consp type) (typep (first type) 'dtd-notation))
               (format stream "NOTATION "))
             (format stream "~S " type))
           (case (dtd-attdef.mode object)
             (XML::\#REQUIRED (write-string "#REQUIRED" stream))
             (XML::\#IMPLIED (write-string "#IMPLIED" stream))
             (XML::\#FIXED
              (format stream "#FIXED ~s" (dtd-attdef.default object)))
             (t (format stream "~s" (dtd-attdef.default object)))))
    (error (condition) (format stream "printing error: ~s." condition) nil)))

(defMethod print-object
           ((datum dtd-attlist) stream)
  (print-unreadable-object (datum stream :type t)
    (format stream "~s"
            (mapcar #'(lambda (att) (xml-node.name att))
                    (dtd-attlist.attdefs datum)))))

(defMethod copy-instance :after
           ((datum dtd-attlist))
  (setf (dtd-attlist.attdefs datum)
        (mapcar #'copy-instance (dtd-attlist.attdefs datum))))

(defMethod xml-node.get-attdef
           ((node dtd-element) indicator)
  (find indicator (dtd-element.attdefs node) :key #'dtd-attdef.name))

(defMethod xml-node.get-attdef
           ((node dtd-element-reference) indicator)
  (or (call-next-method)
      (xml-node.get-attdef (dtd-element-reference.dtd-element node)
                           indicator)))

(defMethod dtd-element.id
           ((datum dtd-element) &optional (key :ID) &aux attdef)
  (when (setf attdef (xml-node.get-attdef datum key))
    (dtd-attdef.default attdef)))

(defMethod (setf dtd-element.id)
           (value (datum dtd-element)  &optional (key :ID) &aux attdef)
  (when (setf attdef (xml-node.get-attdef datum key))
    (setf (dtd-attdef.default attdef) value)))


(defMethod read-typed-markup-declaration
           ((element-decl (eql 'xml::attlist)) (stream t)
            &aux attributes)
  ;; the attlist is of the form:
  ;;   dtd-name {att-name type (value | (keyword value) | keyword)}* {comment}?
  (let ((*parent-node* (make-instance 'dtd-attlist
                         :parent *parent-node*
                         :dtd-element (read-markup-tag-type stream)
                         :attdefs nil)))
    (setf attributes (read-markup-tag-parameters element-decl stream))
    (do ((name (pop attributes) (pop attributes)))
        ((null name))
      (cond ((eq name 'xml::--)
             (xml-node.append-element
              *parent-node*
              (make-instance (node-class 'comment name stream)
                :data attributes))
             (return))
            ((typep name 'xml-comment)
             (xml-node.append-element *parent-node* name))
            (t
             (let ((type (pop attributes))
                   (dcl (pop attributes)))
               (multiple-value-bind (attdef new-attributes)
                                    (make-attdef name type dcl attributes)
                 (setf attributes new-attributes)
                 (xml-node.append-element *parent-node* attdef))))))
    *parent-node*))

(defMethod make-attdef
           (name (type (eql 'xml::NOTATION)) notations &optional rest-attributes)
  (unless (consp notations)
    (xml-form-error *markup-stream* "illegitimate attribute notations: ~s"
                    notations))
  (make-attdef name (mapcar #'(lambda (notation)
                                (dtd.dtd-notation *dtd* notation))
                            notations)
               (first rest-attributes)
               (rest rest-attributes)))

(defMethod make-attdef
           (name (type cons) dcl &optional rest-attributes)
  (make-attdef name (make-model type) dcl rest-attributes))

(defMethod make-attdef
           (name type dcl &optional rest-attributes)
  (values (make-instance (node-class 'attribute-definition name type)
            :name name
            :type type
            :mode nil
            :default dcl)
          rest-attributes))

(defMethod make-attdef
           (name type (dcl (eql 'xml::\#REQUIRED)) &optional rest-attributes)
  (values (make-instance (node-class 'attribute-definition name type)
            :name name
            :type type
            :mode dcl
            :default nil)
          rest-attributes))

(defMethod make-attdef
           (name type (dcl (eql 'xml::\#IMPLIED)) &optional rest-attributes)
  (values (make-instance (node-class 'attribute-definition name type)
            :name name
            :type type
            :mode dcl
            :default nil)
          rest-attributes))

(defMethod make-attdef
           (name type (dcl (eql 'xml::\#FIXED)) &optional rest-attributes)
  (values (make-instance (node-class 'attribute-definition name type)
            :name name
            :type type
            :mode dcl
            :default (first rest-attributes))
          (rest rest-attributes)))


(defMethod process-markup-element
           ((element dtd-attlist) (context t))
  (xml-node.append-element (dtd-element (xml-node.referent element)) element))

(defMethod xml-node.append-element
           ((node dtd-element-declaration) (element dtd-attlist))
  ;; this is not the official definition, but the effect is the same:
  ;; prior definitions have precedence
  (dolist (attdef (DTD-attlist.attdefs element))
    (setf (xml-node.parent attdef) node))
  (setf (dtd-element-declaration.attdefs node)
        (append (dtd-element-declaration.attdefs node)
                (DTD-attlist.attdefs element)))
  element)


:EOF
