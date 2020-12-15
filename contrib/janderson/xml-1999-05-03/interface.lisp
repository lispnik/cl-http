;;; -*- package: "XML-PARSER"; -*-
;;;

"
<DOCUMENTATION>
 <COPYRIGHT HREF='defsystem.lisp|root().descendant(1,COPYRIGHT)'/>
 <CHRONOLOGY>
  <DELTA><DATE>19981217</DATE>
   changed dangling element.get/set to xmlnode.*
   </DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
"

(in-package "XML-PARSER")

(defGeneric attribute.name (attribute))
(defGeneric (setf attribute.name) (value attribute))
(defGeneric attribute.mode (attribute))
(defGeneric (setf attribute.mode) (mode attribute))
(defGeneric attribute.value (attribute))
(defGeneric (setf attribute.value) (value attribute))

(defGeneric attribute-declaration
  (element-name name context &optional error-p))

(defGeneric compile-model-predicate
  (model)
  (:documentation
   "compiles an element model description into a predicate which tests the
    element content.
    if applied to a (NOT element-model) datum, the generic function
    NULL-MODEL-PREDICATE if used."))


(defGeneric define-field-accessor
  (class field-descriptor)
  (:documentation
   "generates a field node to serve as accessor to the given record field."))

(defGeneric document.element (document))
(defGeneric document.validate? (document)
  (:method ((node t)) nil))
(defGeneric document.standalone? (document))
(defGeneric document.encoding (document))
(defGeneric document.dtd (document)
  (:method ((node t)) nil))

(defGeneric dtd-element
  (id)
  (:documentation
   "maps an id to an element definition.
    there are two namespaces: reserved names and element-definition names. 
    reserved names are either members of a fixed set or identified lexically.
    the distinction between definition names and reference names is that the
    element appears both as a definition and as a reference in some other
    element's model."))

(defGeneric element.context (node))
(defGeneric element.content (node))
(defGeneric element.name (node))
(defGeneric xml-node.name (node)
  (:method ((node null)) nil)
  (:method ((name list)) (first name))
  (:method ((name symbol)) name))

(defGeneric element-declaration
  (name context &optional error-p)
  (:documentation
   "maps an id to an element."))

(defGeneric element-declaration.attdefs (element-declaration))
(defGeneric element-declaration.document-type (element-declaration))
(defGeneric element-declaration.name (element-declaration))
(defGeneric element-declaration.predicate (element-declaration))

(defgeneric entity-declaration.value
  (entity))

(defGeneric parameter-entity-declaration
  (name context &optional error-p))

(defGeneric general-entity-declaration
  (name context &optional error-p))

(defGeneric notation-declaration
  (name context &optional error-p))

(defGeneric location.name
  (location)
  (:documentation "returns the simple name from a location.")
  (:method ((location null)) nil)
  (:method ((location pathname)) (pathname-name location)))


(defGeneric load-dtd
  (name source)
  (:documentation
   "check for a preloaded dtd or load it from the specified source.
    when loaded, coerce result to a dtd."))

(defGeneric load-xml
  (source)
  (:documentation
   "decode the source: iterate over the result until either an unrecognized
    type results, or an element results, for which no specialized method
    exists. document nodes map to their element node."))

(defGeneric make-field-reader
  (type offset length)
  (:documentation
   "generate a record slot reader function for the specified parameters."))

(defGeneric make-field-writer
  (type offset length)
  (:documentation
   "generate a record slot writer function for the specified parameters."))

(defGeneric make-model
  (content)
  (:documentation
   "translates a parsed content model into the internal representation."))
  

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

(defGeneric normalize-content-value
  (xml-node value)
  (:documentation
   "transforms a specified value according to the constriants placed by the
    specified node. used for attributes, text nodes, comments..."))


(defGeneric raise-xml-condition
  (context condition message-or-keyword &rest args)
  (:documentation
   "generates and raises the specified condition. determines whether to signal
    an error or a warning based on condition type."))

(defGeneric read-dtd-stream
  (stream &rest initargs))


(defGeneric read-markup-element
  (stream)
  (:documentation
   "the interface function intended to read a single element from a
    markup stream"))

(defGeneric read-markup-stream
  (stream &optional type)
  (:documentation
   "the interface function intenden to read entire markup streams to EOF.
    it saves and returns a list of the elements of the given type"))

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

(defGeneric read-typed-processing-instruction
  (name stream)
  (:documentation
   "this function is triggered by the '?' element reader.
    it reads a processing instruction from a stream given the target name."))

(defGeneric production-reader-macro
  (name stream)
  (:documentation
   "this, the second dispatch step in the reader, dispatches on the production
    name which is expected based on the entities initial character(s)."))

(defGeneric pi-target-reader-macro
  (name stream)
  (:documentation
   "specialized for the respective processing-instruction forms baed on the
    'target' name"))

(defGeneric read-production
  (production stream)
  (:documentation
   "parses input from the <CODE><I>stream</I></CODE> according to the syntax
    of specified <CODE><I>production</I></CODE>. "))

(defGeneric reduce-production
  (production &key &allow-other-keys)
  (:method ((production symbol) &key)
           (declare (ftype (function (symbol string &rest list) nil) xml-warn))
           (when (eq (symbol-package production) *xml-package*)
             (xml-warn 'internal "missing production: ~s." production))
           production)
  (:documentation
   "transforms a property list with elements appropriate to the syntax
    of <CODE><I>production</I></CODE> into an instance which represents the
    nonterminal in the DOM.
    nb. it recurses on the initial resutl to permit instance-specific
    side-effects. specialized or qualified methods on the function can serve
    as notifiers that a given node has been 'processed'.
    the result value must be acceptable as content in the respective
    context entity."))

(defGeneric stream-pathname
  (stream)
  (:documentation
   "determines pathname from a stream. specialized to support concatenated
    streams."))

(defGeneric translate-public-id (public-id)
  (:documentation
   "should eventually look up the public id in a system-specific map and
    return the public id.")
  (:method ((id t)) nil))

(defGeneric translate-system-id (system-id)
  (:documentation
   "merges the system id with the currently active context and generates
    the absolute resource identifier.")
  (:method ((id t)) nil))

(defGeneric xml-decl-pi.encoding (xml-decl-pi))
(defGeneric (setf xml-decl-pi.encoding) (encoding xml-decl-pi))
(defGeneric xml-decl-pi.standalone? (xml-decl-pi))
(defGeneric (setf xml-decl-pi.standalone?) (boolean xml-decl-pi))
(defGeneric xml-decl-pi.validate? (xml-decl-pi))
(defGeneric (setf xml-decl-pi.validate?) (encoding xml-decl-pi))
(defGeneric xml-decl-pi.version (xml-decl-pi))
(defGeneric (setf xml-decl-pi.version) (version xml-decl-pi))

(defGeneric xml-node.get (node indicator &optional default))
(defGeneric xml-node.set (node indicator value))

(defGeneric xml-form-error
  (context message &rest args)
  (:documentation
   "raises the condition *form-error-condition* with message and args"))
   

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

(defGeneric xml-node.append-element (parent child)
  #|(:argument-precedence-order child parent)|#)
(defGeneric xml-node.delete-element (parent child))
(defGeneric xml-node.content (node))
(defGeneric xml-node.context (node))
(defGeneric xml-node.reference (node))
(defGeneric xml-node.system-id (node))
(defGeneric xml-node.public-id (node))
(defGeneric xml-node.string (node)
  (:method ((content list))
           (if (rest content)
             (with-output-to-string (stream)
               (dolist (e content)
                 (write-string (xml-node.string e) stream)))
             (xml-node.string (first content))))
  (:method ((node string)) node)
  (:method ((node symbol)) (string node))
  (:method ((node null)) ""))

(defGeneric document.node-comments (document node)
  (:method ((document t) (node t)) nil))

(declaim (ftype (function (symbol) symbol) inherit-attribute?))
(declaim (ftype (function (symbol stream) symbol) write-identifier))
(declaim (ftype (function (&key name) document-type) default-document-type))
(declaim (ftype (function () symbol) in-dtd?))
(declaim (ftype (function (package) string) namespace.prefix))
(declaim (ftype (function (package) string) namespace.uri))

;;;
;;; miscellaneous compatibility functions

#+:ALLEGRO
(defun whitespacep (x) (excl::whitespace-char-p x))

;;;
;;; macros

(defMacro with-parser-bindings
          (arglist &rest body &aux (block (gensym "PARSE-")))
  `(block ,block
     (destructuring-bind (&key
                          ((:redefined-element *redefined-element-condition*)
                           *redefined-element-condition*)
                          ((:undefined-element *undefined-element-condition*)
                           *undefined-element-condition*)
                          ((:redefined-entity *redefined-entity-condition*)
                           *redefined-entity-condition*)
                          ((:undefined-entity *undefined-entity-condition*)
                           *undefined-entity-condition*)
                          ((:redefined-attribute *redefined-attribute-condition*)
                           *redefined-attribute-condition*)
                          ((:undefined-attribute *undefined-attribute-condition*)
                           *undefined-attribute-condition*)
                          ((:redefined-notation *redefined-notation-condition*)
                           *redefined-notation-condition*)
                          ((:undefined-notation *undefined-notation-condition*)
                           *undefined-notation-condition*)
                          ((:validity-error *validity-error-condition*)
                           *validity-error-condition*)
                          ((:verbose *parse-verbose*) *parse-verbose*)
                          ((:suppress *parse-suppress*) *parse-suppress*)
                          ((:validate *validate*) *validate*)
                          ((:preserve-whitespace *preserve-whitespace-default*)
                            *preserve-whitespace-default*)
                          ((:error-handler *error-handler*)
                           #'(lambda (condition)
                               (return-from ,block
                                 (values nil condition)))))
                      ,arglist
       ,@body)))

(defMacro with-default-document-type
          ((&rest args) &rest body &aux (temp (gensym)))
  `(let* ((,temp (default-document-type ,@args))
          (*document-type* ,temp))
     (unwind-protect (progn ,@body)
       (setf (document-type.name ,temp) nil))))

"XMLP"

