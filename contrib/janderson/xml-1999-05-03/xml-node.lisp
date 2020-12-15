;;; -*- package: "XML-PARSER"; -*-
;;;

(in-package "XML-PARSER")

"
<DOCUMENTATION>
 <COPYRIGHT HREF='defsystem.lisp|root().descendant(COPYRIGHT)'/>
 <DESCRIPTION>
  definitions for
  <UL>
   <LI><CODE>XML-NODE</CODE>,
   <LI><CODE>XML-ATTRIBUTE-CONTEXT</CODE>
   <LI><CODE>XML-NAMED-NODE</CODE>
   <LI><CODE>XML-REFERENCE-NODE</CODE> and
   <LI><CODE>XML-EXTERNAL-NODE</CODE>
   </UL>
  provide the base structure and behaviour for XML-form data structures and
  for the inclusion of external data by reference.
  </DESCRIPTION>
 <CHRONOLOGY>
  <DELTA><DATE>19971218</DATE>
   xml-external-node slots for public and system distinguished hard-coded.
   </DELTA>
  <DELTA><DATE>19980621</DATE>
   <CODE>XML-LINK-ORIGIN</CODE> added to distinguish nodes which can
   serve as the origin of a link.
   see <A HREF='/doc/CLASS?NAME=XMLP::XML-LINK-ELEMENT><CODE>XML-LINK-ELEMENT</CODE></A>
   </DELTA>
  <DELTA><DATE>19981114</DATE>
   added metaclass for default class specification.
   the previous <CODE>node-class</CODE> mechanism, which predated namespaces and
   accepted a thrid argument to permit dtream qualification, is eliminated
   in favor of specializing <CODE>make-instance</CODE> for the meta-class
   <CODE>qualified-class</CODE> which binds a default class name or, in the case
   of <code>element</code> and <code>attribute</code>the metaclass
   <CODE>keyword-qualified-class</CODE>, which takes one of the initialization
   parameters as a possible class name.</DELTA>
  <DELTA><DATE>19981218</DATE>
   clear separation between pathnames and strings as system-id values. when a
   pathname, then in pathname syntax; when a string then as a url.</DATE>
  <DELTA><DATE>19981218</DATE>
   xml-external-node.system-id now uniformly stores as url;
   element-relative attribute attribute names are note <attribute>+<element>
   rather than <element>:<attribute> since it does not require escaping.
   <BR/>
   once again collecting the declarations by side-effect prior to
   returning them as a parsed attribute.</DELTA>
  <DELTA DATE='19990420'>get-precedence-list</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
"

;;;
;;; metaclasses for qualified instantiation

(defClass qualified-class (standard-class)
  ((default-class
     :initform nil :initarg :default-class
     :accessor class.default-class)))

(defMethod validate-superclass
           ((class qualified-class) (superclass standard-class))
  t)

(defClass keyword-qualified-class (qualified-class)
  ((class-initarg
    :initform :name :initarg :class-initarg
     :accessor class.class-initarg)))

(defMethod validate-superclass
           ((class keyword-qualified-class) (superclass qualified-class))
  t)


(defMethod class.default-class
           ((class symbol))
  (class.default-class (find-class class)))

(defMethod (setf class.default-class)
           ((default t) (class symbol))
  (setf (class.default-class (find-class class)) default))

(defMethod (setf class.default-class)
           ((default symbol) (class t))
  (setf (class.default-class class) (find-class default)))

(defMethod make-instance
           ((class qualified-class) &rest initargs)
  ;; if a default class is specified, use it instead of the abstract class
  (declare (dynamic-extent initargs))
  (with-slots (default-class) class
    (if (and default-class (not (eq default-class class))
             (not (eq default-class (class-name class))))
      ;; note that the subclasses SHOULD NOT be of the same metaclass unless
      ;; continued specialization is desired...
      (apply #'make-instance default-class initargs)
      (call-next-method))))

(defMethod class.class-initarg
           ((class symbol))
  (class.class-initarg (find-class class)))

(defMethod (setf class.class-initarg)
           ((initarg t) (class symbol))
  (setf (class.class-initarg (find-class class)) initarg))

(defun get-precedence-list (class)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (class-precedence-list class))

(defMethod class-initarg-class
           ((class keyword-qualified-class) (initargs list) &aux initarg-class)
  (with-slots (class-initarg) class
    (when (and class-initarg
               (setf initarg-class (getf initargs class-initarg))
               (setf initarg-class (find-class initarg-class nil))
               (find class (get-precedence-list initarg-class)))
      initarg-class)))

(defMethod make-instance
           ((class keyword-qualified-class) &rest initargs
            &aux (initarg-class (class-initarg-class class initargs)))
  ;; use the class-specific keyword to extract an initarg to use as the
  ;; effective class. require that the abstract class appear among the
  ;; precedence classes.
  (declare (dynamic-extent initargs))
  (cond (initarg-class 
         (apply #'make-instance initarg-class initargs))
        (t
         (apply #'call-next-method class initargs))))
           

(unless (find-class 'abstract-class nil)
  (defClass abstract-class (standard-class) ())
  (defMethod validate-superclass
           ((class abstract-class) (superclass standard-class))
  t)
  (defMethod make-instance ((class abstract-class) &key)
    (error "abstract class permits no instances: ~s." (class-name class))))

;;;
;;; abstract node classes

(defClass xml-node ()
  ((context
    :initarg :context :initform nil
    :accessor xml-node.context
    :type (or null xml-node)
    :documentation
    "binds a link back to the containing node.")
   (content
    :initarg :content :initform nil
    :accessor xml-node.children :accessor xml-node.content
    :type element-content-list
    :documentation
    "binds the node's content. this list can include primitive data and nodes
     among its members."))
  (:documentation
   "<CODE>XML-NODE</CODE> is the abstract root class for all objects which
    can appear in an xml document."))

(defmethod xml-datum.type
           ((node xml-node))
  nil)

(defMethod link-context
           ((child xml-node) (parent xml-node))
  (setf (xml-node.context child) parent))

(defMethod link-context
           ((child t) (parent xml-node))
  )

(defMethod xml-node-link-content
           ((self xml-node) &aux content)
  (typecase (setf content (xml-node.content self))
    (cons  (dolist (c content) (link-context c self)))
    (t nil)))

(defMethod initialize-instance :after
           ((self xml-node) &key)
  (xml-node-link-content self))

(defMethod xml-node.append-element
           ((parent xml-node) (child xml-node))
  (setf (xml-node.children parent)
        (nconc (xml-node.children parent) (list child)))
  (link-context child parent)
  child)

(defMethod xml-node.delete-element
           ((parent xml-node) (child xml-node))
  (setf (xml-node.children parent) (delete child (xml-node.children parent)))
  (setf (xml-node.context child) nil)
  child)


(defMethod xml-node.append-element
           ((parent null) (child t))
  (when (xml-verbose 'xml-node.append-element)
    (format *trace-output* "~%no parent: ~s." child))
  child)

;; specialize a version for a list of parents - but then copy the children since
;; each child can have at most one parent

(defMethod xml-node.append-element
           ((parent-list list) (child standard-object))
  (dolist (parent parent-list)
    (xml-node.append-element parent (copy-instance child))))

(defMethod xml-node.append-element
           ((parent-list list) (child t))
  (dolist (parent parent-list)
    (xml-node.append-element parent child)))

(defMethod xml-node.append-element
           ((parent t) (child t))
  child)


(defMethod xml-node.append-element :before
           ((parent t) (child t))
  (when (xml-verbose 'xml-node.append-element)
    (format t "~%append-element: ~s: ~s." parent child)))

(defMethod reduce-production :before
           ((what t) &rest args)
  (when (xml-verbose 'reduce-production)
    (format t "~%reduce-production: ~s: ~s." what args)))
  

;;;
;;;

(defClass xml-datum ()
  ((name
    :initarg :name :initform nil
    :accessor xml-datum.name
    :type symbol
    :documentation
    "binds a name for a node. in a concrete class this bind a symbol or
     a reference to a dtd element.")
   (content
    :initarg :content :initform nil
    :accessor xml-datum.content :accessor xml-datum.relations
    :type t
    :documentation
    "binds the node's content. this can bind atomic data or data objects."))
  (:metaclass abstract-class)
  (:documentation
   "<CODE>XML-DATUM</CODE> binds a name an a data-object.
    it also serves as an abstract specializer which stipulates the data
    side of the pattern-matching interface:
    <UL>
    <LI><CODE>XML-DATUM.ATTRIBUTES</CODE>
    <LI><CODE>XML-DATUM.RELATIONS</CODE>
    <LI><CODE>XML-DATUM.TYPE</CODE>
    </UL>
    see also <CODE>XML-PATTERN</CODE>."))


(defClass xml-named-node (xml-datum xml-node)
  ((name
    :accessor xml-node.name))
  (:documentation
   "<CODE>XML-NAMED-NODE</CODE> is the abstract root class nodes which
    bind a name."))


;;;
;;;

(defClass xml-attribute-context (xml-named-node)
  ((attributes
    :initform nil
    :accessor xml-node.attributes))
  (:metaclass abstract-class)
  (:documentation
   "generalized node which binds attribute instances. at least ELEMENT and
    NAMESPACE-PI specialize this class."))

(defMethod initialize-instance :after
           ((instance xml-attribute-context) &key)
  (dolist (attribute (slot-value instance 'attributes))
    (link-context attribute instance)))

(defMethod xml-node.get-attribute-if
           ((node t) (predicate t) (next t))
  nil)

(defMethod xml-node.get-attribute-if
           ((node xml-attribute-context) predicate next)
  (cond ((find-if predicate (slot-value node 'attributes)))
        (next
         (xml-node.get-attribute-if (funcall next node) predicate next))
        (t nil)))

(defMethod xml-node.get-attribute
           ((node xml-attribute-context) (name t))
  (xml-node.get-attribute-if node
                             #'(lambda (a) (eq (attribute.name a) name))
                             (when (inherit-attribute? name)
                               #'xml-node.context)))

(defMethod xml-node.get-attribute
           ((node xml-attribute-context) (name string))
  (xml-node.get-attribute-if node
                             #'(lambda (a)
                                 (string= (attribute.name a) name))
                             (when (inherit-attribute? name)
                               #'xml-node.context)))

(defMethod xml-node.get-attribute-by-type
           ((node xml-attribute-context) type)
  (xml-node.get-attribute-if node
                             #'(lambda (a) (typep a type))
                             (when (inherit-attribute? type)
                               #'xml-node.context)))

(defMethod xml-node.get
           ((node xml-attribute-context) (indicator symbol) &optional default
            &aux att)
  (if (setf att (xml-node.get-attribute node indicator))
    (attribute.value att)
    default))
            
(defMethod xml-node.set
           ((node xml-attribute-context) (name symbol) (value t)
            &aux att)
  ;; the value type is non-specific here: the respective attribute class
  ;; specifies any transformations.
  (with-slots (attributes) node
    (cond ((setf att (find-if #'(lambda (a) (eq (attribute.name a) name))
                              attributes))
           (case (attribute.mode att)
             (XMLNS.0:\#FIXED
              (unless (equalp (xml-node.string value) (xml-node.string att))
                (raise-xml-condition node
                                     *attribute-value-error-condition*
                                     "erroneous attribute value: ~s: ~s /= ~s."
                                     name (xml-node.string value)
                                     (xml-node.string att))))
             (t ; the others set no constraints
              ))
           (setf (attribute.value att) value))
          (t
           (setf att (make-instance *attribute-class*
                       :name name :value value :context node))
           (setf attributes (nconc attributes (list att)))))
    
    value))

(defMethod (setf xml-node.get)
           (datum (node xml-attribute-context) indicator &optional default)
  (declare (ignore default))
  ;; the default is supported in order to support a setf form, which allows
  ;; a symbol-macrolet on the attribute name to depend on a call to xml-node.get
  (xml-node.set node indicator datum))


;;;
;;;

(defClass xml-reference-node (xml-node)
  ((reference
    :initarg :referent :initform nil
    :accessor xml-node.reference
    :type xml-node))
  (:metaclass abstract-class)
  (:documentation
   "<CODE>XML-REFERENCE-NODE</CODE> is the abstract class of nodes which refer
    to another instance."))

;;;
;;;

(defClass xml-temporal-node (xml-node)
  ((assertion-time
    :initform 0
    :accessor xml-node.assertion-time)
   (modification-time
    :initform 0
    :accessor xml-node.modification-time))
  (:metaclass abstract-class)
  (:documentation
   "<CODE>XML-TEMPORAL-NODE</CODE> is the abstract class of nodes which track
    some aspect of their behaviour. XML-EXTERNAL-NODES use this to determine
    if they're up to date."))

(defMethod xml-node.up-to-date?
           ((node xml-temporal-node))
  (xml-node.assertion-time node))

;;;
;;;

(defClass xml-external-node (xml-temporal-node)
  ((system-id
    :initform nil
    :reader xml-node.system-id
    :writer xml-node.set-system-id
    :type (or null string))
   (public-id
    :initform nil
    :reader xml-node.public-id
    :writer xml-node.set-public-id
    :type (or null string)))
  (:metaclass abstract-class)
  (:documentation
   "<CODE>XML-EXTERNAL-NODE</CODE> is the abstract root class for
    dtd elements which may incorporate an external definition by
    reference.<BR>
    <CODE>XML-NODE.SYSTEM-ID</CODE> binds a location where the resource is
    stored. (also known as the 'SYSTEM' attribute.)
    <CODE>XML-NODE.PUBLIC-ID</CODE> binds the public id, should one be
    provided.
    the timestamps are used to track currency by caching the external
    modification time as such and noting the point when the check was performed
    as 'assertion time'."))

;; control access through the exported functions. retain the writers in order
;; to permit specialization

(defMethod (setf xml-node.system-id)
           ((id t) (node xml-external-node))
  (xml-form-error (type-of node) "erroneous system identifier: ~s." id))

(defMethod (setf xml-node.system-id)
           ((id string) (node xml-external-node))
  (xml-node.set-system-id id node))

(defMethod (setf xml-node.system-id)
           ((id pathname) (node xml-external-node))
  (xml-node.set-system-id (name-string id) node))

(defMethod (setf xml-node.system-id)
           ((id null) (node xml-external-node))
  (xml-node.set-system-id id node))

(defMethod (setf xml-node.public-id)
           ((id t) (node xml-external-node))
  (xml-form-error (type-of node) "erroneous public identifier: ~s." id))

(defMethod (setf xml-node.public-id)
           ((id string) (node xml-external-node))
  (xml-node.set-public-id id node))

(defMethod (setf xml-node.public-id)
           ((id null) (node xml-external-node))
  (xml-node.set-public-id id node))

(defMethod shared-initialize :after
           ((node xml-external-node) (slots t) &key system-id public-id)
  (setf (xml-node.system-id node) system-id)
  (setf (xml-node.public-id node) public-id))

(defMethod xml-node.up-to-date?
           ((node xml-external-node)
            &aux external-time)
  ;; external nodes are 'up to date' relative to two things: the external
  ;; reference and the document. if the node has been referenced after the
  ;; document was created, then it counts as up to date. this wil be the case
  ;; for multiply referenced external entities. otherwise the access time is
  ;; checked against an external modification time to determine the status.
  (with-slots (assertion-time modification-time system-id) node
    (or (null system-id)
        ; either there must have been a check performed, the temporal position
        ; of which postdates the respective document creation or the mod-time
        ; is retrieved and the time of the last check show the external resource
        ; to have remained unmodified.
        (prog1
          (or (and assertion-time
                   *document*
                   (>= assertion-time (xml-node.assertion-time *document*)))
              (and modification-time
                   (setf external-time (system-id.timestamp system-id))
                   (>= modification-time external-time)))
          (setf assertion-time (get-universal-time))))))


;;;
;;;

(defClass xml-link-origin (xml-external-node
                           xml-reference-node)
  ()
  (:metaclass abstract-class)
  (:documentation
   "a link origin combines a location for the external resource with an
    instance for the internal value."))

;;;
;;;

(defClass comment-target (xml-node)
  ()
  (:metaclass abstract-class))

(defMethod reduce-production :after
           ((node comment-target) &key)
  (setf *comment-target* node))

;;;
;;;

(defClass declaration-markup (xml-node)
  ()
  (:metaclass abstract-class)
  (:documentation
   "a marker class to indicate that presence in a declaration context is
    permitted."))

(defClass content-markup (xml-node)
  ()
  (:metaclass abstract-class)
  (:documentation
   "a marker class to indicate that presence in a element context is
    permitted."))

(defClass document-markup (xml-node)
  ()
  (:metaclass abstract-class)
  (:documentation
   "a marker class to indicate that presence at document top-level is
    permitted."))

;;;
;;; abstract class for elements/nodes in the document type definition
;;; the general reduction method simply augments the dtd

(defClass dtd-node (xml-named-node comment-target declaration-markup)
  ()
  (:metaclass abstract-class)
  (:documentation
   "<CODE>DTD-NODE</CODE> is the abstract root class for all components of a
    document type description."))

(defMethod reduce-production
           ((node dtd-node) &key)
  (xml-node.append-element *document-type* node)
  (setf *processed-node* node))


;;;
;;; abstract interface class for patterns

(defClass xml-pattern ()
  ()
  (:documentation
   "the <CODE>XML-PATTERN</CODE> class serves as an abstract specializer
    which stipulates the pattern side of the pattern-matching interface:
    <UL>
    <LI><CODE>XML-PATTERN.ATTRIBUTES</CODE>
    <LI><CODE>XML-PATTERN.RELATIONS</CODE>
    <LI><CODE>XML-PATTERN.OCCURRENCE</CODE>
    <LI><CODE>XML-PATTERN.TYPE</CODE>
    <LI><CODE>XML-PATTERN.VARIABLE</CODE>
    </UL>
    see also <CODE>XML-DATUM</CODE>."))


;;;
;;;

(defMethod map-node
           ((node t) function &optional predicate)
  (when (or (null predicate) (funcall predicate node))
    (funcall function node)))

(defMethod map-node
           ((nodes list) function &optional predicate)
  (dolist (node nodes)
    (map-node node function predicate)))

(defMethod map-node :after
           ((node xml-node) function &optional predicate)
  (map-node (xml-node.content node) function predicate))

(defMethod map-node :after
           ((node xml-attribute-context) function &optional predicate)
  (map-node (xml-node.attributes node) function predicate))

(defMethod copy-node
           ((node xml-node)
            &aux 
            (class (class-of node))
            (slots (#+CCL class-instance-slots #+ALLEGRO class-slots
                    class))
            (new (allocate-instance class))
            name)
  (dolist (s slots)
    (cond #+ALLEGRO((eq :class (slot-definition-allocation s)))
          (t (setf name (slot-definition-name s))
             (setf (slot-value new name) (slot-value node name)))))
  new)
;(copy-node (make-instance 'element :name 'asdf))
;(copy-node (make-instance 'attribute :name 'asdf))

;;;
;;; print functions

(defMethod print-object :around
           ((node xml-node) (stream t))
  ;; handler for errors during node printing
  (handler-case (call-next-method)
    (error (condition)
           (ignore-errors
            (xml-warn (type-of node) "printing error: ~s." condition))
           nil)))

(defMethod print-object :after
           ((node comment-target) (stream t))
  ;; print comments
  (when (and *print-readably* *preserve-comments*)
    (dolist (comment (document.node-comments *document* node))
      (when *print-pretty* 
        (format stream "~&~v@t" *form-indent*))
      (print-object comment stream))))

(defMethod print-object
           ((datum xml-node) (stream t))
  "the base method for the most primitive nodes just continues recursively"
  (with-slots (context content) datum
    (if *print-readably*
      (when content (princ content stream))
      (print-unreadable-object (datum stream :type t :identity t)
        (format stream "(~s/~s)" (type-of context) (type-of content))))))

(defMethod print-object
           ((datum xml-named-node) (stream t))
  "the base method for the most primitive nodes just continues recursively"
  (cond (*print-readably*
         (print-start-tag datum stream)
         (princ (xml-node.content datum) stream)
         (print-end-tag datum stream))
        (t
         (print-unreadable-object (datum stream :type t :identity t)
           (with-slots (context content) datum
             (format stream "[~s](~s/~s)"
                     (xml-node.name datum)
                     (type-of context)
                     (type-of content)))))))


(defMethod print-start-tag
           ((datum xml-named-node) (stream t))
  (write-string *start-tag-open* stream)
  (write-identifier (xml-node.name datum) stream)
  (write-string *tag-close* stream))

(defMethod print-end-tag
           ((datum xml-named-node) (stream t))
  (write-string *end-tag-open* stream)
  (write-identifier (xml-node.name datum) stream)
  (write-string *tag-close* stream))



#|
(let ((*print-readably* t))
  (print (make-instance 'xml-node :content "ASDF"))
  (print (make-instance 'xml-named-node :content "QWER" :name 'test)))

(namespace.prefix (find-package "html"))

 |#


"XMLP"


   
