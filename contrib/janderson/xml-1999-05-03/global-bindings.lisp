;;; -*- package: "XML-PARSER"; -*-
;;;

"
<DOCUMENTATION>
 <DESCRIPTION>
 global bindings
 </DESCRIPTION>
 <COPYRIGHT HREF='defsystem.lisp|root().descendant(1,COPYRIGHT)'/>
 <CHRONOLOGY>
  <DELTA><DATE>19971124</DATE>*warn-if-undefined*</DELTA>
  <DELTA><DATE>19971204</DATE>
   version 0.30:<BR>
         model compiler; element matcher, xml pi according to PR-19921208
   </DELTA>
  <DELTA><DATE>19980102</DATE>
   cdata and pcdata classes</DELTA>
  <DELTA><DATE>19980320</DATE>
   whitespace predicate for allegro</DELTA>
  <DELTA><DATE>19980405</DATE>
   namespace support according to wd-xml-names-19980327</DELTA>
  <DELTA><DATE>19980621</DATE>
   pi, namespace punctuation</DELTA>
  <DELTA><DATE>19980702</DATE>
   recognized atribute subclasses</DELTA>
  <DELTA><DATE>19981024</DATE>
    previous-node -> processed-node to indicate the circumstances
    </DELTA>
  <DELTA><DATE>19981222</DATE>
   removed logical hosts in favour of local-context-based defaults;
   model compiler errors with PCDATA;
   document type caching.</DELTA>
  <DELTA DATE='19990429'>*attribute-namespace-strict?*</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
"

(in-package "XML-PARSER")


;;;
;;; parser state variables (see also namespaces)

(defVar *document* nil
  "binds the currently processed document.")

(defVar *document-type* nil
  "binds the document-type instance while the dtd is being read in order to
   collect them.
   the presence of a binding also specifies which kind of entity reference
   is permitted.
   nb. this has no bearing on declaration resolution, since element definitions
   use qualified names, which are then resolved with respect to a global
   namespace.")

(defVar *default-document-type* nil
  "bound to a document-typ instance while reading dtd content which does
   not belong to any document.")

(defVar *in-external-subset* nil
  "determines the permissibility of conditional sections and specifies whether
   a declaration belongs in the internal or the external subset of the 
   document type.")

(defVar *in-attribute-literal* nil
  "determines the permissibility fo entity references.")

(defvar *parent-node* nil
  "binds the current node when working with a dtd or reading a document.
   the name of the 'current' node is used to generate qualified names.
   component nodes are added to the current node after creation.")

(defVar *processed-node* nil
  "binds the last processed node for error reporting.")

(defVar *comment-target* nil
  "binds the last processed node which is eligible as a comment target.")

(defParameter *markup-stream* nil
  "binds the stream from which a document definition or element definitions
   is/are being read.")

(defVar *pattern-environment* nil
  "binds the dynamic state of a given pattern function application.
   since it is rebound for each application, the global binding is never used.")

(defVar *compiler-trace* nil
  "a logical value which determines whether tracing code is compiled into
   element validity predicates. if non-null, then code is included which
   exmamins *parse-verbose* to control trace messages. if so compiled, then
   (setf (xml-verbose :validity) <boolean>) then controls trace messages
   at runtime.")

(defVar *expand-parameter-entities* t)
(defVar *expand-general-entities* t)

(defParameter *error-handler* nil
  "binds a function which the reader will invoke to try to handle those errors
   which would otherwise cause it to terminate. if the value is null, then
   those functions which read entire streams will bind a handler which simply
   reports the error and returns NIL as the result.")

(defVar *global-attributes* nil
  "binds attributes which serve as universal defaults.")


(defVar *form-indent* 0)

(defVar *inherited-attributes* nil
  "a list of attribute names <em>and or types</em> which are inherited from
   contianing elements")

(defParameter *permit-abbreviated-close-tags* nil
  "non-conforming close-tag abbreviation")

(defParameter *preserve-whitespace-default* nil
  "specifies the 'default' whitespace handling. this is observed when the
   asserted at the start of a parse and reasserted when the whitespace attribute
   specifies 'default'.")

(defParameter *preserve-whitespace* *preserve-whitespace-default*
  "determines whether element content preserves or suppresses whitespace.")

(defParameter *preserve-comments* t
  "determines whether comments are preserved and printed.")

(defparameter *print-attlist-declarations* nil
  "controls whether the list of attributes is printed together with the element.
   the static binding should remain NIL.")

(defParameter *print-empty-close-tags* nil
  "determines if empty elements print with or without close tags")

(defParameter *print-readably* nil
  "determines if elements print readably.
   distinct from cl binding as that is rebound out of our control, and,
   if set, it can cause problems with error messages.")

(defVar *redefined-element-condition* 'redefined-element-error
  "specifies whether to enforce the standard validation rules should a dtd
   element be redefined.")
(defVar *undefined-element-condition* 'undefined-element-error
  "specifies whether to enforce the standard validation rules should a dtd
   element be undefined.")
(defVar *undefined-entity-condition* 'undefined-entity-error)
(defVar *undefined-notation-condition* 'undefined-notation-error)
(defVar *undefined-attribute-condition* 'undefined-attribute-error)
(defVar *redefined-entity-condition* 'redefined-entity-error)
(defVar *redefined-notation-condition* 'redefined-notation-error)
(defVar *redefined-attribute-condition* 'redefined-attribute-error)
(defVar *root-element-error-condition* 'root-element-error)
(defVar *attribute-value-error-condition* 'attribute-value-error)
(defVar *character-category-condition* 'character-category-error)
(defVar *entity-reference-context-condition* 'entity-reference-context-error)
(defVar *form-error-condition* 'form-error)
(defVar *validity-error-condition* 'validity-error)
(defVar *element-content-condition* 'element-content-error)

(defVar *parse-suppress* nil
  "specifies whether the elements are to be instantiated when read or that
   the streams is merely to be parsed. 
   specialized methods for reduce-production might well modify it.")

(defVar *parse-verbose* nil)

(defVar *xml-version* 1.0)
(defVar *processor-version* 0.432)
(defVar *encoding* nil)
(defVar *standalone* nil)
(defVar *validate* nil)

(defConstant *namespace-name-package* (find-package "xmlns")
  "package for name prefixes. note that this inherits from no package, since
   in order for attributes to be distinguished by name, they must have
   the xmlns prefix. xml as prefix is not permitted.")

(defConstant *xml-package* (find-package "xml")
  "the base package for symbols in xml-encoded documents.
   it comprises those symbols which are standard to XML.
   they are interned in and exported from this package.
   all namespace-specific packages use this one in order to ensure that the
   standard symbols are always unambiguously present.")

(defParameter *html-package* (find-package "html"))

(defParameter *user-package* (find-package "xml-user")
  "the default markup reader package. it is asserted if the current package
   would otherwise not import from the XML base package.")

(defParameter *default-package* *user-package*
  "the namespace used when no namespace prefix is provided for a symbol.")

(defConstant *default-namespace-prefix*  (intern "" "xmlns")
  "the symbol |xmlns|:|| designates a binding to the 'defualt namespace'.")

(defVar *ns-prefix* (intern "xmlns" "xmlns"))
(defVar *ns-prefix-length* (length (string *ns-prefix*)))
(defVar *xml-prefix* (intern "xml" "xmlns"))

(defVar *rebind-default-content-namespace* nil
  "in accordinace with rec-names-19980916.")

(defParameter *pattern-package* (find-package "xsl"))

(defVar *default-namespaces*
  ;; note that the first two both inherit from the base package only.
  (acons *default-namespace-prefix* *default-package*
         (acons *ns-prefix* *namespace-name-package*
                (acons *xml-prefix* *xml-package*
                       nil))))

(defParameter *namespaces* *default-namespaces*
  "an a-list of prefix/namespace bindings known in a given dynamic context.")


(defvar *start-tag* nil
  "an ersatz element which holds the last start tag name when *parse-suppress*
   is true.")

(defvar *end-tag* nil)
(defVar *permit-etag-attributes* nil)

;;; readtables for stream syntax

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defVar *declaration-tag-marker* (marker-tag "MDO"))
  (defVar *processing-instruction-tag-marker* (marker-tag "PIO"))
  (defVar *section-tag-marker* (marker-tag "DTGO"))
  (defVar *close-tag-marker* (marker-tag "NET"))
  (defVar *open-tag-section-marker* (marker-tag "DSO"))
  (defVar *close-tag-section-marker* (marker-tag "DTGC"))
  (defVar *marked-section-close-marker* (marker-tag "MSC"))
  (defVar *entity-marker* (marker-tag "ERO"))
  (defVar *parameter-entity-marker* (marker-tag "PERO"))
  (defVar *tag-end-marker* (marker-tag "TAGC"))
  (defVar *opt-marker* (marker-oc "opt"))
  (defVar *rep-marker* (marker-oc "rep"))
  (defVar *plus-marker* (marker-oc "plus"))
  (defVar *and-marker* (marker-sep "and"))
  (defVar *or-marker* (marker-sep "or"))
  (defVar *seq-marker* (marker-sep "seq")))


(defVar *attribute-quote-char* #\')
(defVar *declaration-open-char* #\<)
(defVar *declaration-section-open* "[")
(defVar *declaration-section-close* "]")
(defVar *markup-declaration-open* "<!")
(defVar *markup-declaration-close* ">")
(defVar *start-tag-open* "<")
(defVar *end-tag-open* "</")
(defVar *tag-close* ">")
(defVar *empty-tag-close* "/>")
(defVar *tag-open-char* #\<)
(defVar *tag-close-char* #\>)
(defVar *empty-tag-close-char* #\/)
(defVar *start-section-char* #\[)
(defVar *end-section-char* #\])
(defVar *cdata-close-char*)
(defVar *cdata-close* "]]>")
(defVar *cdata-open* "<![CDATA[")
(defVar *parameter-entity-marker-char* #\%)
(defVar *parameter-entity-reference-open-char* #\%)
(defVar *general-entity-reference-open-char* #\&)
(defVar *character-reference-open-char* #\#)
(defVar *entity-reference-close-char* #\;)
(defVar *namespace-punctuation-char* #\:)
(defVar *attribute-namespace-punctuation-char* #\+)
(defVar *attribute-namespace-punctuation-string* "+")
(defVar *attribute-namespace-strict?* nil
  "specifies whether unqualified attribute names are inflected with
   the element name when interned.") 
(defVar *pi-open* "<?")
(defVar *pi-close* "?>")
(defVar *pi-close-char* #\?)
(defVar *attribute-equals-char* #\=)



#|
;; 19981218: jaa eliminated in favour of the *local-context*-based defaults
;; in the http server

(defparameter *dtd-pathname-defaults*
  (merge-pathnames (make-pathname :directory '(:RELATIVE "DTD")
                                  :name :wild :type "dtd")
                   cl-user::*xml-source-directory*)
  "binds the pathname for the directory in which document type definitions
   and element definitions are stored.")

(defparameter *xml-pathname-defaults*
  (merge-pathnames (make-pathname :directory '(:RELATIVE "XML")
                                            :name :wild :type "xml")
                 cl-user::*xml-source-directory*))


(setf (logical-pathname-translations "dtd")
      `(("**;*.*" ,(merge-pathnames
                    (make-pathname :directory '(:RELATIVE :wild-inferiors)
                                   :name :wild :type :wild)
                    *dtd-pathname-defaults*))))

(setf (logical-pathname-translations "xml")
      `(("**;*.*" ,(merge-pathnames
                    (make-pathname :directory '(:RELATIVE :wild-inferiors)
                                   :name :wild :type :wild)
                    *xml-pathname-defaults*))
        ("*.*" ,(merge-pathnames
                 (make-pathname :name :wild :type :wild)
                 *xml-pathname-defaults*))
        ("*" ,(merge-pathnames
               (make-pathname :name :wild)
               *xml-pathname-defaults*))))
|#


;;;
;;; instantiation parameters.
;;; in addition to the class specification which is bound statically in the
;;; respective concrete class, the element and attribute classes are available
;;; as dynamic bindings. they are rebound to keywords in the dtd and document
;;; parse functions and could well be modified through processing instructions.

(defParameter *element-class-attribute* "TYPE")
(defParameter *element-class* 'element)
(defParameter *attribute-class* 'attribute)


;;;
;;; XSL parameters

(defParameter *xsl-type-attribute* 'XSL::TYPE)
(defParameter *xsl-attributes-attribute* 'XSL::ATTRIBUTES)
(defParameter *xsl-occurrence-attribute* 'XSL::OCCURRENCE)
(defParameter *xsl-variable-attribute* 'XSL::VARIABLE)
(defParameter *pattern-element-class* 'xml-pattern-element)
(defParameter *xsl-excluded-content-types*
  '(xsl::attribute))


"XMLP"

