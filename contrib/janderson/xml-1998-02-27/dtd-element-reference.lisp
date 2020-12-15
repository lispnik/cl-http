;;; -*- mode: LISP; package ("XML-PARSER")
;;;
;;; this version (C) mecom gmbh 24.11.97
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.

#|
<CHRONOLOGY>
<DATE>19971128</DATE>
<DELTA>
 <LI>dtd-element-declaration.model now caches the model
 <LI>dtd-element-declaration.predicate
 </DELTA>
<DATE>19971208</DATE>
 <DELTA>factored out declaration and reference to distinct subclasses</DELTA>
<DATE>19971228</DATE>
 <DELTA>XML-NODE.ATTRIBUTES</DELTA>
</CHRONOLOGY>
 |#

(in-package :XML-PARSER)

(defMethod node-class
           ((node (eql 'element-decl-ref)) (op t) (context t))
  *dtd-element-reference-class*)



(defClass dtd-element-reference (dtd-element)
  ((parent :accessor dtd-element-reference.parent
    :type (or null dtd-element))
   (name :accessor dtd-element-reference.name
    :type symbol)
   (qualified-name :accessor dtd-element-reference.qualified-name
    :type symbol)
   (content
    :initarg :dtd-element
    :reader dtd-element-reference.get-dtd-element
    :writer dtd-element-reference.set-dtd-element
    :type (or symbol dtd-element))
   (attdefs :accessor dtd-element-reference.attdefs))
  (:documentation
   "the class of references to element definitions for document type
    definitions.
    as a subclass of <CODE>DTD-ELEMENT</CODE> it can bind the same information
    as a dtd element. it can also override the definition of the referenced
    element. in particular the attribute definitions can include aspects, such
    as the role, which are specific to the referenced instance and thus to the
    position in the structure hierarchy.
    the referenced element is bound first as a symbol and resolved at the
    time it is first referenced. this makes it possible to forward reference
    elements in a dtd at the same time as legitimate elements are present
    in the referencing models to bind attribute definitions, etc."))

(defMethod dtd-element-reference.dtd-element
           ((element dtd-element-reference)
            &aux (ref (dtd-element-reference.get-dtd-element element)))
  (unless (typep ref 'dtd-element)
    (if (setf ref (dtd.dtd-element (dtd-element-reference.parent element) ref))
      (if (eq ref element)
        (warn "circular element reference: ~s." element)
        (dtd-element-reference.set-dtd-element ref element))
      (error "dtd reference element not found: ~s."
             (dtd-element-reference.get-dtd-element element))))
  ref)

(defMethod dtd-element-declaration.model
           ((element dtd-element-reference))
  (dtd-element-declaration.model
   (dtd-element-reference.dtd-element element)))

(defMethod dtd-element-declaration.predicate
           ((element dtd-element-reference))
  (dtd-element-declaration.predicate
   (dtd-element-reference.dtd-element element)))

(defMethod xml-node.attributes
           ((node dtd-element-reference))
  (append (dtd-element-reference.attdefs node)
          (xml-node.attributes (dtd-element-reference.dtd-element node))))

(defMethod xml-node.attributes
           ((node t))
  nil)


(defMethod print-object ((datum dtd-element-reference) stream)
  "an element reference need print only the reference-specific attributes.
   the original element is printed as part of the global dtd"
  (if *xml-print-readably*
    (typecase *parent-node*
      (dtd-element (princ (dtd-element.name datum) stream))
      (t
       (let ((*parent-node* datum))
         (format stream "<!ELEMENT-REFERENCE ~a >"
                 (dtd-element-reference.qualified-name datum))
         (when *xml-print-dtd-attlists*
           (write-attlist (dtd-element-reference.qualified-name datum)
                          (dtd-element-reference.attdefs datum)
                          stream)))))
    (print-unreadable-object (datum stream :type t)
      (format stream "~a" (dtd-element-reference.get-dtd-element datum)))))

(defMethod shared-initialize
           ((self dtd-element-reference) (slots t) &rest initargs
            &key name (dtd-element name) (referent dtd-element))
  (prog1 (apply #'call-next-method self slots
                :referent referent
                initargs)
    (dtd-element-reference.set-dtd-element referent self)))

(defMethod read-typed-markup-declaration
           ((element-decl (eql 'xml::element-reference)) (stream t)
            &aux (name (read-markup-tag-type stream)))
  (read-delimited-list #\> stream t)
  (make-instance (node-class 'element-decl-ref element-decl stream)
    :name name
    :qualified-name (build-element-reference-name name)))

:EOF
