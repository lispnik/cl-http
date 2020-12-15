;;; -*- package: ("XML-PARSER") -*-
;;;
;;; this version (C) mecom gmbh 24.11.97
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.
;;;
;;; this file defines many of the interface functions. it is not, at present,
;;; complete

(in-package :xml-parser)

(defGeneric compile-model-predicate
  (model)
  (:documentation
   "compiles an element model description into a predicate which tests the
    element content.
    if applied to a (NOT DTD-MODEL) datum, the generic function
    NULL-MODEL-PREDICATE if used."))


(defGeneric define-field-accessor
  (class field-descriptor)
  (:documentation
   "generates a field node to serve as accessor to the given record field."))

(defGeneric dtd-element
  (id)
  (:documentation
   "maps an id to an element definition. there are three namespaces:
    reserved names, element-reference names, and element-definition names. 
    reserved names are either members of a fixed set or identified lexically.
    the distinction between definition names and reference names is that the
    element appears both as a definition and as a reference in some other
    element's model."))

(defGeneric dtd-element-reference
  (id))

(defGeneric dtd-entity
  (name))

(defgeneric dtd-entity.value
  (entity))

(defGeneric dtd.dtd-element
  (dtd id)
  (:documentation
   "maps an id to an element wrt a specific dtd.
    this permits the dtd to enforce its own relative naming protocol."))

(defGeneric dtd.dtd-entity
  (dtd id))

(defGeneric dtd.dtd-notation
  (dtd id))

(defGeneric load-dtd
  (name source)
  (:documentation
   "check for a preloaded dtd or load it from the specified source.
    when loaded, coerce result to a dtd."))

(defGeneric make-field-reader
  (type offset length)
  (:documentation
   "generate a record slot reader function for the specified parameters."))

(defGeneric make-field-writer
  (type offset length)
  (:documentation
   "generate a record slot writer function for the specified parameters."))

(defGeneric make-processing-instruction
  (pi-name &rest args)
  (:documentation
   "constructs a processing instruction node given the pi-name and the
    tag content."))

(defGeneric match-node
  (pattern node &key environment continuation))

(defGeneric match-pattern-to-node
  (pattern node continuation environment))

(defGeneric match-pattern-to-node/attributes
  (pattern node cont env))

(defGeneric match-pattern-to-node/names
  (pattern node)
  (:documentation
   "perform the match between the name/type of a pattern and a datum node.
    supports boolean expressions for type specifications."))

(defGeneric match-pattern-to-node/content
  (pattern node continuation environment))

(defGeneric match-succeed
 (pattern node continuation environment))

(defGeneric match-fail
  (pattern node continuation environment))

(defGeneric node-class
  (node-class type context)
  (:documentation
   "provides a mechansim to tailor the instantiated node class to the
    node <CODE>type</CODE> and the <CODE>context</CODE>.
    the class reflects the intended standard class, the type the operator
    which appears in the element, and the context the stream from which it
    is being read."))

(defGeneric process-markup-element
  (element context)
  (:documentation
   "performs side-effects specified by the element in the context.
    specialized or qualified methods on the function serve as notifiers
    that a given node has been 'processed'."))

(defGeneric read-markup-stream
  (stream &optional type)
  (:documentation
   "the interface function intenden to read entire markup streams to EOF.
    it saves and returns a list of the elements of the given type"))

(defGeneric read-markup-element
  (stream)
  (:documentation
   "the interface function intended to read a single element from a
    markup stream"))

(defGeneric read-markup-tag-parameters
  (tag-name stream)
  (:documentation
   "reads a the parameters for a tag from a stream given the tag name"))

(defGeneric read-typed-markup-declaration
  (name stream)
  (:documentation
   "this function is triggered by the '!' element reader.
    it reads a markup declaration from the stream given the tag name.
    the 1.0 implementation supplies ATTLIST, DOCTYPE, ENTITY, and
    NOTATION methods."))

(defGeneric read-typed-section-element
  (name stream)
  (:documentation
   "this function is triggered by the '![' element reader.
    it reads a markup declaration from the stream given the tag name."))

(defGeneric read-typed-processing-instruction
  (name stream)
  (:documentation
   "this function is triggered by the '?' element reader.
    it reads a processing instruction from a stream given the target name."))

(defGeneric read-typed-markup-element
  (name stream)
  (:documentation
   "this, the second dispatch step in the reader, dispatches on the type
    specified in a tag. the 1.0 implementation includes methods for
    '!' (element declarations, pcdata, etc), and '?' (processing instructions),
    as well as the content method specialized to any symbol."))


(defGeneric xml-document.element
  (document))

(defGeneric xml-document.validate?
  (document))

(defGeneric xml-element.context
  (node))

(defGeneric xml-element.declaration
  (node))

(defGeneric xml-element.content
  (node))

(defGeneric xml-element.name
  (node))

(defGeneric xml-element.get
  (node indicator))

(defGeneric xml-element.set
  (node indicator value))


(defGeneric xml-validity-error
  (context message &rest args)
  (:documentation
   "generates a validity error if the present document stipulates validation."))

(defGeneric xml-form-error
  (context message &rest args)
  (:documentation
   "generates a form error. this is independant of document constraints"))

(defGeneric xml-cell-error
  (cell message &rest args)
  (:documentation
   "generates a cell error. this is independant of document constraints"))

;;;
;;; generic interface to patterns:
;;; matches are according to type, attributes and relations. in addition, the
;;; pattern side specifies an occurrence and a binding variable.

(defGeneric xml-pattern.attributes (datum))
(defGeneric xml-pattern.occurrence (datum))
(defGeneric xml-pattern.relations (datum))
(defGeneric xml-pattern.type (datum))
(defGeneric xml-pattern.variable (datum))


(defGeneric xml-datum.attributes (datum))
(defGeneric xml-datum.relations (datum))
(defGeneric xml-datum.type (datum))

:EOF
