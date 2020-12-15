;;; -*- mode: LISP; package ("XML-PARSER")
;;;
;;; this version (C) mecom gmbh 24.11.97
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.

#|
<DOCUMENTATION>
 <DESCRIPTION>
  definitions for DTD declarations. (ELEMENT, ATTLIST)
  <P>
  each element appears to consume ca.300-500 bytes for its definition and
  another 300-500 for its predicate (if validation is enabled).
  this form examining the memory requirements of the KBV-LDT definition.
  there were 626 elements (384 definitions and 242 references).
  maximum number of elements was 39, but hte average was only 1.5.
  rough 
  </DESCRIPTION>
 <CHRONOLOGY>
  <DELTA><DATE>19971125 jaa</DATE>
   <LI>wrap unary element models in parens when printing an ELEMENT form</LI>
   <LI>factored out dtd-add-element, dtd-remove-element.</LI>
  </DELTA>
  <DELTA><DATE>19971125</DATE>
   <UL><LI>added bindings for the reservered elements
    <LI>dtd-add-element appends new element rather than  pushing it
    <LI>dtd-element-reference (dtd-element-declaration)
    </UL>
   </DELTA>
  <DELTA><DATE>19971126</DATE>
   ANY and EMPTY elements</DELTA>
  <DELTA><DATE>19971127</DATE>
   dtd-element-declaration.predicate</DELTA>
  <DELTA><DATE>19971208</DATE>
   factored out declaration and reference to distinct subclasses
   </DELTA>
  <DELTA><DATE>19971228</DATE>
   XML-NODE.ATTRIBUTES</DELTA>
  <DELTA><DATE>19971230</DATE>
   node-class invocation passes the element name, as it should in order
   to elect an element-specific class for the declaration instance
   </DELTA>
  </CHRONOLOGY> 
 </DOCUMENTATION>
|#

(in-package :XML-PARSER)


(defMethod node-class
           ((node (eql 'element-decl)) (name t) (context t))
  (if (ignore-errors (subtypep name *dtd-element-class*))
    name
    *dtd-element-class*))

(defClass dtd-element-declaration (dtd-element)
  ((parent :accessor dtd-element-declaration.dtd
    :type (or null dtd))
   (name :accessor dtd-element-declaration.name
    :type symbol)
   (qualified-name :accessor dtd-element-declaration.qualified-name
    :type symbol)
   (content
    :initarg :model :initform nil
    :accessor dtd-element-declaration.model
    :reader dtd-element-declaration.get-model
    :type (or null dtd-model))
   (attdefs :accessor dtd-element-declaration.attdefs)
   (comments
    :initarg :comments  :initform nil
    :accessor dtd-element-declaration.comments)
   (predicate
    :initform nil
    :accessor dtd-element-declaration.predicate))
  (:documentation
   "the class of element definitions for document type definitions.
    binds a document-specific name, a global name qualified by document structure,
    and definitions for structure (sub elements) and attributes."))

(defMethod dtd-element-declaration.model
           ((decl null))
  nil)

(defMethod xml-node.attributes
           ((node dtd-element-declaration))
  (dtd-element.attdefs node))


(defMethod dtd-add-element :after
           ((dtd dtd) (element dtd-element-declaration))
  (when (string-equal (string (dtd-element-declaration.name element))
                      (string (dtd.name dtd)))
    (setf (dtd.root dtd) element)))

(defMethod dtd-remove-element :after
           ((dtd dtd) (element dtd-element-declaration))
  (when (eq element (dtd.root dtd))
    (setf (dtd.root dtd) nil)))

(defMethod dtd.dtd-element-reference
           ((dtd t) (element dtd-element-declaration))
  (dtd.dtd-element-reference dtd (dtd-element-declaration.name element)))

(defMethod dtd-element-reference
           ((element dtd-element-declaration))
  (dtd.dtd-element-reference *dtd* element))

(defMethod dtd.dtd-qualified-element
           ((dtd dtd) (element dtd-element-declaration))
  (dtd.dtd-qualified-element
   dtd
   (build-qualified-dtd-element-name
    dtd (dtd-element-declaration.name element))))

(defClass dtd-reserved-declaration (dtd-element)
  ((name
    :accessor dtd-element-declaration.name
    :accessor dtd-element.name
    :type symbol))
  (:metaclass xml-token-class)
  (:documentation
   "the class of universal document type definition elements.
    it includes elements such as PCDATA, CDATA, etc."))

(defMethod dtd-reserved-declaration
           ((id symbol))
  (or (gethash id (xml-token-class.map (find-class 'dtd-reserved-declaration)))
      (error "reserved element not defined: ~s." id)))

(defVar *cdata-element*
  (make-instance 'dtd-reserved-declaration :name 'xml::cdata))
(defVar *rcdata-element*
  (make-instance 'dtd-reserved-declaration :name 'xml::\#rcdata))
(defVar *pcdata-element*
  (make-instance 'dtd-reserved-declaration :name 'xml::\#pcdata))
(defVar *notype-element*
  (make-instance 'dtd-reserved-declaration :name 'xml::\#notype))
(defVar *any-element*
  (make-instance 'dtd-reserved-declaration :name 'xml::ANY))
(defVar *empty-element*
  (make-instance 'dtd-reserved-declaration :name 'xml::EMPTY))


(defMethod write-attlist
           (name attlist stream)
  (when attlist
    (format stream "~%<!ATTLIST ~A ~{~%          ~A~}>"
            name attlist)))

(defMethod print-object ((datum dtd-element-declaration) stream)
  (flet ((get-print-model (&aux (model (dtd-element-declaration.get-model datum)))
           (typecase model
             (dtd-reserved-model
              (case (dtd-model.name model)
                ((xml::any xml::empty) model)
                (t (list model))))
             (dtd-element-model (list model))
             (t (if (eql (dtd-model.occurrence model) 1)
                  model
                  (list model))))))
    (if *xml-print-readably*
      (typecase *parent-node*
        ;; if it's part of an element's model, print just the name
        (dtd-element (princ (dtd-element-declaration.name datum) stream))
        ;; otherwise, it's the outermose element and prints an element definition
        (t
         (let ((*parent-node* datum))
           (format stream "<!ELEMENT ~a ~a~@[ -- ~{~a~^ ~}~]>"
                   (dtd-element-declaration.name datum)
                   (get-print-model)
                   (dtd-element-declaration.comments datum))
           (when *xml-print-dtd-attlists*
             (write-attlist (dtd-element-declaration.name datum)
                            (dtd-element-declaration.attdefs datum)
                            stream)))))
      (print-unreadable-object (datum stream :type t)
        (with-accessors ((name dtd-element-declaration.name)
                         (q-name dtd-element-declaration.qualified-name))
                        datum
          (format stream "~:[~a/~a~;~a~]" (eq name q-name) name q-name))))))
      
(defMethod print-object
           ((datum dtd-reserved-declaration) (stream t))
  "the base method for the most primitive nodes just continues recursively"
  (cond (*xml-print-readably*
         (princ (xml-node.name datum) stream))
        (t
         (print-unreadable-object (datum stream)
           (princ (xml-node.name datum) stream)))))

(defMethod shared-initialize :after
           ((*parent-node* dtd-element-declaration) (slots t) &key
            &aux (model (dtd-element-declaration.get-model *parent-node*)))
  (typecase model
    (null t)
    (cons (when (and (> (length model) 1)
                     (not (find-if #'connector-name-p model))
                     (not (find-if #'occurrence-name-p model)))
            (warn "connector missing in element model: ~s." model)))
    (dtd-model t)
    (t (setf model (list model))))
  
  (setf model
        (handler-case (make-model model)
          (error (condition &aux (*xml-print-readably* nil))
                 (warn "raised error parsing model: ~s: ~a."
                       *parent-node* condition)
                 nil)))
  (setf (dtd-element-declaration.model *parent-node*) model))

(defMethod (setf dtd-element-declaration.model) :after
           ((model null) (decl dtd-element-declaration))
  (setf (dtd-element-declaration.predicate decl) nil))


(defMethod read-typed-markup-declaration
       ((element-decl (eql 'xml::element)) (stream t)
        &aux (name (read-markup-tag-type stream))
             (model (read-markup-model stream))
             (args (read-markup-tag-parameters element-decl stream)))
  (setf element-decl
        (make-instance (node-class 'element-decl name stream)
          :name name
          :qualified-name (build-qualified-element-name name)
          :model (or model 'XML::EMPTY)))
  (do ((arg (pop args) (pop args)))
      ((null arg))
    (cond ((eq arg 'xml::--)
           (xml-node.append-element element-decl
                                    (make-instance (node-class 'comment
                                                               element-decl
                                                               args)
                                      :content args))
           (setf args nil))
          ((typep arg 'xml-comment)
           (xml-node.append-element element-decl arg))
          (t
           (xml-form-error element-decl
                           "illegitimate DTD-declaration ~A ~{~A~^ ~}."
                           arg args))))
  element-decl)




:EOF
