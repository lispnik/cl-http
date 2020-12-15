;;; -*- mode: LISP; package: "XML-PARSER"; -*-
;;;

"
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
 <COPYRIGHT>
  this version (C) mecom gmbh 19981024
  available only from the cl-http repository and NOT to be REdistributed
  in ANY form. see cl-xml.html.
  </COPYRIGHT>
 <CHRONOLOGY>
  <DELTA><DATE>19971125 jaa</DATE>
   <LI>wrap unary element models in parens when printing an ELEMENT form</LI>
   <LI>factored out dtd-add-element, dtd-remove-element.</LI>
  </DELTA>
  <DELTA><DATE>19971125</DATE>
   <UL><LI>added bindings for the reservered elements
    <LI>dtd-add-element appends new element rather than  pushing it
    <LI>dtd-element-reference (element-declaration)
    </UL>
   </DELTA>
  <DELTA><DATE>19971126</DATE>
   ANY and EMPTY elements</DELTA>
  <DELTA><DATE>19971127</DATE>
   element-declaration.predicate</DELTA>
  <DELTA><DATE>19971208</DATE>
   factored out declaration and reference to distinct subclasses
   </DELTA>
  <DELTA><DATE>19971228</DATE>
   XML-NODE.ATTRIBUTES</DELTA>
  <DELTA><DATE>19971230</DATE>
   node-class invocation passes the element name, as it should in order
   to elect an element-specific class for the declaration instance
   </DELTA>
  <DELTA><DATE>19980814</DATE>
   with universal names, the element references can now be simple symbols, so
   the distinction between element declaration and element reference is no
   longer useful
   </DELTA>
  <DELTA><DATE>19981218</DATE>
   finally eliminated all references to qualified names for element
   declarations;
   eliminated the check for a context when setting a declaration's model, since
   the sepc precludes multiple names in an element declaration form.
   </DELTA>
  </CHRONOLOGY> 
 </DOCUMENTATION>
 "

(in-package "XML-PARSER")

;;;
;;; class definitions

(defClass element-declaration (dtd-node)
  ((context
    :accessor element-declaration.document-type
    :type (or null document-type))
   (name
    :accessor element-declaration.name
    :accessor dtd-element.name
    :type symbol)
   (content
    :initarg :model :initform nil
    :accessor element-declaration.model
    :type (or null element-model))
   (attdefs
    :initform nil :initarg :attdefs
    :accessor element-declaration.attdefs)
   (predicate
    :initform nil
    :writer (setf element-declaration.predicate)
    :reader element-declaration.get-predicate))
  (:documentation
   "the class of element definitions for document type definitions.
    binds a document-specific name, a global name qualified by document structure,
    and definitions for structure (sub elements) and attributes.")
  (:metaclass keyword-qualified-class))

(defMethod element-declaration.model
           ((decl null))
  nil)

(defMethod element-declaration.attdefs
           ((decl symbol))
  (when (setf decl (element-declaration decl *document* nil))
    (element-declaration.attdefs decl)))

(defMethod xml-node.attributes
           ((node element-declaration))
  (element-declaration.attdefs node))

(defMethod element.package
           ((node element-declaration))
  (element.package (element-declaration.name node)))

(defMethod element-declaration.predicate
           ((name symbol) &aux (decl (element-declaration name t)))
  (when decl
    (element-declaration.predicate decl)))

(defMethod element-declaration.predicate
           ((decl element-declaration))
  (with-slots (predicate) decl
    (or predicate (compile-model-predicate decl))))


(defClass reserved-element-declaration (element-declaration)
  ((name
    :accessor element-declaration.name
    :type symbol))
  (:metaclass xml-token-class)
  (:documentation
   "the class of universal document type definition elements.
    it includes elements such as PCDATA, CDATA, etc."))

(defVar *cdata-element*
  (make-instance 'reserved-element-declaration :name 'xml::cdata))
(defVar *rcdata-element*
  (make-instance 'reserved-element-declaration :name 'xml::\#rcdata))
(defVar *pcdata-element*
  (make-instance 'reserved-element-declaration :name 'xml::\#pcdata))
(defVar *notype-element*
  (make-instance 'reserved-element-declaration :name 'xml::\#notype))
(defVar *any-element*
  (make-instance 'reserved-element-declaration :name 'xml::ANY))
(defVar *empty-element*
  (make-instance 'reserved-element-declaration :name 'xml::EMPTY))



;;;
;;; model operations

(defMethod element-declaration
           ((element element-declaration) (context t) &optional error-p)
  (declare (ignore error-p))
  element)


(defMethod (setf element-declaration)
           ((entity element-declaration) (id symbol) (context t)
            &aux (old (get id 'element-declaration)))
  (when (and old (element-model-equalp (element-declaration.model old)
                                       (element-declaration.model entity)))
    (setf (element-declaration.predicate entity)
          (element-declaration.get-predicate old)))
  (setf (get id 'element-declaration) entity))

(defMethod attribute-declaration
           ((decl element-declaration) (attribute-name symbol) (context t)
            &optional (error-p *undefined-attribute-condition*)
            &aux entity)
  (cond ((setf entity (find attribute-name
                            (element-declaration.attdefs decl)
                            :key #'attribute.name))
         (attribute-declaration decl entity context error-p))
         (error-p
          (error error-p :format-string "undeclared attribute: ~s, ~s."
                 :format-arguments (list  decl attribute-name)))))



;;;
;;; printing

(defMethod print-object
           ((datum reserved-element-declaration) (stream t))
  "the base method for the most primitive nodes just continues recursively"
  (cond (*print-readably*
         (princ (xml-node.name datum) stream))
        (t
         (print-unreadable-object (datum stream)
           (princ (xml-node.name datum) stream)))))

(defMethod shared-initialize :after
           ((*parent-node* element-declaration) (slots t) &key)
  (with-slots ((model content)) *parent-node*
    (typecase model
      (null t)
      (cons (when (and (> (length model) 1)
                       (not (find-if #'connector-name-p model))
                       (not (find-if #'occurrence-name-p model)))
              (warn "connector missing in element model: ~s." model)))
      (element-model t)
      (symbol t)
      (t (xml-warn *parent-node* "questionable element model: ~s." model)))
  
    (setf model
          (handler-case (make-model model)
            (error (condition &aux (*print-readably* nil))
                   (warn "raised error parsing model: ~s: ~a."
                         *parent-node* condition)
                   nil)))
    (setf (element-declaration.model *parent-node*) model)))

(defMethod (setf element-declaration.model) :after
           ((model null) (decl element-declaration))
  (setf (element-declaration.predicate decl) nil))



;;;
;;; parsing

(defMethod reduce-production
           ((decl element-declaration) &key)
  (setf (element-declaration (xml-node.name decl) *document*) decl)
  (call-next-method))

(defMethod reduce-production
           ((production (eql 'xml::elementdecl))
            &key
            ((xml-1.0::Name name))
            ((xml-1.0::ContentModel model)))
  (unless *parse-suppress*
    (reduce-production (make-instance 'element-declaration
                         :name name
                         :model model))))

(defMethod production-reader-macro
           ((production (eql 'xml::elementdecl)) (stream t)
            &aux name model)
  (handler-bind ;; just augment error message for context
    ((error #'(lambda (condition)
                (xml-warn production
                          "name: ~s, model: ~s."
                          name model)
                condition)))
    (multiple-value-setq (name model)
      (read-entity-attributes production stream))
    (reduce-production production
                       'xml-1.0::Name name
                       'xml-1.0::ContentModel model)))

;;;
;;; printing

(defMethod write-attlist
           (name attlist stream)
  (when attlist
    (format stream "~%<!ATTLIST ~A ~{~%          ~A~}>"
            name attlist)))

(defMethod print-object ((datum element-declaration) stream)
  (flet ((get-print-model
             (&aux (model (element-declaration.model datum)))
           (typecase model
             (reserved-model
              (case (element-model.name model)
                ((xml::any xml::empty) model)
                (t (list model))))
             (t model)))
         (print-model (model stream
                       &aux (*print-readably* t))
           (typecase model
             (symbol (write-identifier model stream))
             (list (write-identifier-list model stream))
             (abstract-element-model (print-object model stream))
             (t
              (write-string "#ILLEGAL-MODEL" stream)))))
    (if *print-readably*
      (typecase *parent-node*
        ;; if it's part of an element's model, print just the name
        (element-declaration (princ (element-declaration.name datum) stream))
        ;; otherwise, it's the outermose element and prints an element definition
        (t
         (let ((*parent-node* datum)
               (model (get-print-model)))
           (write-string *markup-declaration-open* stream)
           (write-string "ELEMENT " stream)
           (write-identifier (element-declaration.name datum) stream)
           (write-char #\space stream)
           (print-model model stream)
           (write-string *tag-close* stream)
           (when *print-attlist-declarations*
             (write-attlist (element-declaration.name datum)
                            (element-declaration.attdefs datum)
                            stream)))))
      (let ((*parent-node* datum)
            (model (get-print-model)))
        (print-unreadable-object (datum stream :type t)
          (write-identifier (element-declaration.name datum) stream)
          (write-char #\space stream)
          (print-model model stream))))))

#|
;; ok
(production-reader-macro
 'xml::elementdecl
 (markup-stream "asdf ( a, b) >"))


|#

"XMLP"
