;;; -*- Package: "XML-PARSER"; -*-

"
<DOCUMENTATION>
 <DESCRIPTION>
  note the hierarchy: XML-SERIALIZABLE, XML-INTERFACE, element.
  </DESCRIPTION>
 <COPYRIGHT HREF='defsystem.lisp|root().descendant(1,COPYRIGHT)'/>
 <CHRONOLOGY>
  <DELTA><DATE>19980701</DATE>
   recognized attribute subclasses </DELTA>
  <DELTA><DATE>19981114</DATE>
   class parameterization as metadata;
   eliminated trimming in normalization - leave it to the strict parsing</DELTA>
  <DELTA><DATE>19981218</DATE>
   print-object modified to support class specific content;
   added entity internalzation for @ENTITY/IES attributes.</DELTA>
  </CHRONOLOGY>
 <DISCUSSION>
 <H3>attribute instances</H3>
 <P>
 attribute instances model the name / value pairs which may be associated with
 XML/DOM elements. if the document incorporates a DTD, then the elements attributes
 will have been declared. such a declaration comprises specifications for
 type, domain constraints, default values, occurrence:
 <UL>
 <LI> the type specifications is reified as a class hierarchy, the the root
      of which is the abstract class of all attributes.
      <FOOTNOTE>
      a better implementation would allow anonymous classes and bind them
      into the respective dtd, but the mcl mop does not appear to allow it.
      </FOOTNOTE>
      the first specialization is for CDATA / NAME / NMTOKEN value domains
      the NAME domain is further specialized for id and entity values.
      note
      that the respective class is further qualified by the attribute's name</LI>
 <LI> the domain constraints are cached and used to check assigned values.</LI>
 <LI> the (optional) default value becomes the cotent of the delcaration prototype
      and thereby carried over to the instantial value.
 <LI> the occurrence constraint is held in the prototype and checked after
      instantiation.
 </UL>
 </P>
 <P>
 note that the attribute's content value is not necessarily a single string,
 but a list of elements. in the simplest form this is a list of one string
 element, but a parsed attribute may contain entity references which would be
 interleaved with string content if not resolved during parsing or if manipulated
 by the application.
 such a content structure is, in particular, required in order to generate
 documents with entity references.
 </P>
 thos attribute specializations which inter their value differ from this
 standard: since they bind specific value types in any case, there's no need
 to store entities in the content, so a list in unnecessary. this applies to
 @NAME (a symbol), @NAMES (list symbol), and @NAMESPACE (a package).
 note also that the class is specified by the name's package in addition to the
 presence of a respective class definition. the intent is:
 1. 'attribute is provided to make instance
 2. if the sttribute names a class that is used
 3. otherwise @CDATA is used, in which case
 4. if the attribute name is a namespace prefix, a @NAMESPACE attribute instance
    is created
 5. otherwise a cdata instance is created.
 6. if an @NAMESPACE instance is created, the class-wide specification is
    observed 
  </DISCUSSION>
 </DOCUMENTATION>
 "

(in-package "XML-PARSER")


(defClass attribute-class (keyword-qualified-class) ())

(defMethod class-initarg-class
           ((class attribute-class) (initargs list) &aux initarg-class)
  (with-slots (class-initarg) class
    (when (and class-initarg
               (setf initarg-class (getf initargs class-initarg)))
      (if (eq (symbol-package initarg-class) *namespace-name-package*)
        'xml-1.0::@NAMESPACE
        (call-next-method)))))

(defMethod make-instance
           ((class attribute-class) &rest initargs &key (type nil type-p))
  (cond (type-p
          (setf type (attribute-type.class type))
          (remf initargs :type)
          (apply #'make-instance type initargs))
        (t
         (call-next-method))))

(defClass attribute (xml-named-node)
  ((name
    :accessor attribute.name)
   (content
    :reader attribute.value
    :type t)
   (type
    :allocation :class
    :reader attribute.type)
   (mode
    :initarg :mode :initform nil
    :accessor attribute.mode
    :type symbol)
   (domain
    :initarg :domain :initform nil
    :accessor attribute.domain
    :type list))
  (:documentation
   "the abstract class of element attribute bindings.")
  (:metaclass attribute-class))

(defClass xml-1.0::@CDATA (attribute)
  ((type :allocation :class :initform "CDATA"))
  (:metaclass attribute-class))
(setf (class.default-class 'attribute) 'xml-1.0::@CDATA)

(defClass xml-1.0::@NAMESPACE (xml-1.0::@CDATA) ()
  (:metaclass qualified-class))
(defClass xml::@NAME (attribute) ()
  (:metaclass keyword-qualified-class))
(defClass xml::@NAMES (attribute) ()
  (:metaclass keyword-qualified-class))
(defClass xml::@ID (xml::@NAME)
  ((type :allocation :class :initform "ID"))
  (:metaclass keyword-qualified-class))
(defClass xml::@IDREF (xml::@NAME)
  ((type :allocation :class :initform "IDREF"))
  (:metaclass keyword-qualified-class))
(defClass xml::@IDREFS (xml::@NAMES)
  ((type :allocation :class :initform "IDREFS"))
  (:metaclass keyword-qualified-class))
(defClass xml::@ENTITY (xml::@NAME)
  ((type :allocation :class :initform "ENTITY"))
  (:metaclass keyword-qualified-class))
(defClass xml::@ENTITIES (xml::@NAMES)
  ((type :allocation :class :initform "ENTITIES"))
  (:metaclass keyword-qualified-class))
(defClass xml::@NMTOKEN (attribute)
  ((type :allocation :class :initform "NMTOKEN"))
  (:metaclass keyword-qualified-class))
(defClass xml::@NMTOKENS (attribute)
  ((type :allocation :class :initform "CDATA"))
  (:metaclass keyword-qualified-class))
(defClass xml::@NOTATION (xml::@NAME)
  ((type :allocation :class :initform "NOTATION"))
  (:metaclass keyword-qualified-class))
(defClass xml::@ENUMERATION (xml::@NMTOKENS)
  ((type :allocation :class :initform nil))
  (:metaclass keyword-qualified-class))


;;; the canonical content form is a list of text nodes. we allow lists of
;;; strings and other arbitrary objects. a single string is transformed
;;; according to attribute type into a content list.

(defMethod initialize-instance :after
           ((self attribute) &key value)
  (with-slots (content) self
    (unless content
      (setf (attribute.value self) value))))

(defMethod initialize-instance :after
           ((self XML-1.0::@NAMESPACE) &key)
  (with-slots (content) self
    (setf content (make-namespace content))))

(defMethod (setf attribute.value)
           ((value string) (node attribute))
  (setf (slot-value node 'content)
        (normalize-content-value node (list value))))

(defMethod (setf attribute.value)
           ((value string) (node xml::@NAMES))
  (setf (slot-value node 'content)
        (normalize-content-value node value)))

(defMethod (setf attribute.value)
           ((value list) (node attribute))
  (setf (slot-value node 'content) value))

(defMethod xml-node.string
           ((node attribute))
  (xml-node.string (attribute.value node)))

(defMethod xml-node.string
           ((node xml-1.0::@NAMES))
  (format nil "~{~s~^ ~}" (attribute.value node)))

(defMethod xml-node.string
           ((node xml-1.0::@NAMESPACE))
  (namespace.uri (attribute.value node)))

(defMethod normalize-content-value
           ((attribute attribute) (value string))
  ;; the xml-1.0 document specifies that attribute values are to be normalized
  ;; by trimming whitespace and replacing contained whitespace with a single
  ;; space. the dom document specifies that assignment binds an unparsed
  ;; text node. we just leave the value as a simple string, as transformations
  ;; have already been performed in the parsing stage.
  value)

(defMethod normalize-content-value
           ((attribute xml::@NAME) (value string))
  (setf value (call-next-method))
  (intern-name value))

(defMethod normalize-content-value
           ((node xml::@NAMES) (value string))
  (setf value (call-next-method))
  (setf value (read-name-list #\)
                              (encoded-stream (concatenate 'string value ")"))))
  (normalize-content-value node value))
;(normalize-content-value (make-instance 'xml::@NAMES) "asdf qwer")

(defMethod normalize-content-value
           ((attribute xml::@ENTITY) (value symbol))
  (unparsed-entity-reference value))
;(normalize-content-value (make-instance 'xml::@ENTITIES) "qwer")

(defMethod normalize-content-value
           ((attribute xml::@ENTITIES) (value symbol))
  (unparsed-entity-reference value))
;(normalize-content-value (make-instance 'xml::@ENTITIES) "asdf qwer")

(defMethod normalize-content-value
           ((attribute attribute) (value list))
  (mapl #'(lambda (list)
            (setf (first list)
                  (normalize-content-value attribute (first list))))
        value))

(defMethod normalize-content-value
           ((attribute t) (value t))
  value)


;;;
;;; initializing an attribute node includes binding the attributes
;;; this includes asserting any required side-effects.


(defMethod initialize-instance-attributes
           ((node xml-attribute-context) (null null) (attlist t))
  node)

(defMethod initialize-instance-attributes
           ((node xml-attribute-context) (attribute attribute) (attlist list)
            &aux old)
  (with-slots (attributes) node
    (if (setf old (find (attribute.name attribute) attributes
                        :key #'attribute.name))
      (setf attributes (nsubstitute attribute old attributes))
      (push attribute attributes))
    (with-slots (name content) attribute
      (when (whitespace-attribute-name? name)
        (assert-whitespace-handling content)))
    (initialize-instance-attributes node (first attlist) (rest attlist))))

(defMethod initialize-instance-attributes
           ((node xml-attribute-context) (name symbol) (attlist list)
            &aux (value (pop attlist)))
  (xml-node.set node name value)

  (when (whitespace-attribute-name? name)
    (assert-whitespace-handling value))

  (initialize-instance-attributes node (first attlist) (rest attlist)))


(defMethod initialize-instance-attributes
           ((node xml-attribute-context) (name string) (attlist list))
  (setf name (cond ((string-equal name *ns-prefix*)
                    *default-namespace-prefix*)
                   ((qualified-name? name)
                    (intern-name name))
                   (t
                    (intern (if *attribute-namespace-strict?*
                              (concatenate 'string name
                                           *attribute-namespace-punctuation-string*
                                           (string (xml-node.name node)))
                              name)
                            (symbol-package (xml-node.name node))))))

  (initialize-instance-attributes node name attlist))


(defMethod (setf attribute-declaration)
           ((attdef attribute) (decl element-declaration) (attribute-name symbol)
            (context t))
  (pushnew attdef (element-declaration.attdefs decl) :key #'attribute.name))

(defMethod attribute-declaration
           ((gi t) (attribute-definition attribute) (context t)
            &optional error-p)
  (declare (ignore error-p))
  attribute-definition)


;;;
;;; whitespace handling
;;; permit a non-case-sensistive attribute name.

(defMethod whitespace-attribute-name?
           ((name symbol))
  (and (eq (symbol-package name) *xml-package*)
       (string-equal name "whitespace")))

(defMethod whitespace-attribute-name?
           ((name string))
  (string-equal "xml:whitespace" name))

(defMethod assert-whitespace-handling
           ((spec string))
  (when *document*                      ; do this when parsing only
    (cond  ((string-equal spec "preserve")
            (setf *preserve-whitespace* t))
           ((string-equal spec "default")
            (setf *preserve-whitespace* *preserve-whitespace-default*))
           (t
            (raise-xml-condition *validity-error-condition*
                                 'xml-1.0::Attribute
                                 "erroneous whitespace handling specifier: ~s."
                                 spec)))))

(defMethod assert-whitespace-handling
           ((spec t))
  (assert-whitespace-handling (xml-node.string spec)))
    


(defMethod print-object
           ((attribute attribute) (stream t))
  (if *print-readably* 
    (typecase *parent-node*
      (xml-attribute-context
       (write-attribute-name (attribute.name attribute) stream)
       (write-char *attribute-equals-char* stream)
       (print-attribute-value attribute stream))
      (dtd-node
       (write-attribute-name (attribute.name attribute) stream)
       (write-char #\space stream)
       (print-attribute-type attribute stream)
       (write-char #\space stream)
       (print-attribute-default attribute stream))
      (t
       (format stream "#i(~s :name ~s :content ~s)"
               (type-of attribute)
               (attribute.name attribute)
               (attribute.value attribute))))
    (print-unreadable-object (attribute stream :type t :identity t)
      (format stream "~s=~@[~c~]~s"
              (attribute.name attribute)
              (case (attribute.mode attribute)
                (xml-1.0::\#required #\!)
                (xml-1.0::\#fixed #\=)
                (xml-1.0::\#implied #\?))
              (attribute.value attribute)))))

(defun print-attribute-type
       (attribute stream &aux tokens)
  ;; first print the type identifier, then the noation names or the
  ;; token constraints
  (with-slots (type domain) attribute
    (write-identifier type stream)
    (typecase (setf tokens domain)
      (list (write-char #\space stream)
            (write-char #\( stream)
            (do ((token (pop tokens) (pop tokens)))
                ((null token))
              (write-identifier token stream)
              (when tokens (write-char #\space stream)))
            (write-char #\( stream))
      (t ; just in case
       (print-object tokens stream)))))

(defun print-attribute-default
       (attribute stream)
  (with-slots (default mode) attribute
    (case mode
      (:REQUIRED (write-string "#REQUIRED" stream))
      (:IMPLIED (write-string "#IMPLIED" stream))
      (:FIXED (write-string "#FIXED" stream)))
    (when default
      (write-attribute-string default stream))))

(defMethod print-attribute-value
           ((attribute attribute) stream)
  (write-char *attribute-quote-char* stream)
  (print-attribute-value (attribute.value attribute) stream)
  (write-char *attribute-quote-char* stream))

(defMethod print-attribute-value
           ((attribute xml-1.0::@NAMESPACE) stream)
  (write-char *attribute-quote-char* stream)
  (write-string (namespace.uri (attribute.value attribute)) stream)
  (write-char *attribute-quote-char* stream))

(defMethod print-attribute-value
           ((value string) stream)
  (write-attribute-data-value value stream))

(defMethod print-attribute-value
           ((value entity-reference) stream)
  (print-object value stream))

(defMethod print-attribute-value
           ((value list) stream)
  (dolist (element value) (print-attribute-value element stream)))
       

(push (make-instance 'attribute :name 'xsl::type
                     :type 'xml:id
                     :content nil)
      *global-attributes*)

(defun inherit-attribute?
       (name)
  (find name *inherited-attributes*))

#|
(let ((*print-readably* t))
  (print (make-instance 'attribute :name 'test :content "ASDF")))
 |#

"XMLP"
