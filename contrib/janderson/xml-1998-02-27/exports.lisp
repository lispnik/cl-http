;;; -*- package: ("XML-PARSER") -*-
;;;
;;; this version (C) mecom gmbh 24.11.97
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.

#|
<DOCUMENTATION>
 <DESCRIPTION>
package definitions and published symbols for the xml parser.
<P>
in addition to their role in program structure,
packages are used as the basis for namespace support (despite its 
unofficial status in xml-1.0).
this is why the program source originates from the "XML-PARSER" package and
not the "XML" package. the latter is well kept unencumbered for that namesapce
of xml documents which is reserved for symbols specified by the standard.
<P>
at present the reader ensures that the active package uses the "XML" package in
order to locate these symbols. it may well be possible to relax this constraint
once the behaviour of namespaces is defined. it seems unlikely that reserved
symbols will appear only in certain locations AND be required in those locations.
were that the case, the reader could selectively install the reserved package
appropriately or the names could be parsed and interened by hand. it is more
likely, however, that numerous forms - for example processing instructions - will
leave open the possibility that the application chose the namespace for the
operator freely.
<P>
in keeping with the lisp restriction that internal symbols must contain two
colons, the namespace syntax is a bit cumbersome. the single colon from published
examples will not succeed.
eventually a special symbol reader will be necessary to manage namespace syntax
properly.
 </DESCRIPTION>

 <CHRONOLOGY>
 <DELTA><DATE>19971124</DATE>*xml-warn-if-undefined*</DELTA>
 <DELTA><DATE>19971125</DATE>
  XML-USER package to keep base xml package clean</DELTA>
 <DELTA><DATE>19971210</DATE>
  standalone instead of RMD</DELTA>
 <DELTA><DATE>19971217</DATE>
  nickname "XMLP" for parser application package.
   </DELTA>
  <DELTA><DATE>19980102</DATE>
   *XML-CDATA-CLASS*, *XML-PCDATA-CLASS* instead of *XML-TEXT-CLASS*</DELTA>
 </CHRONOLOGY>
</DOCUMENTATION>
|#

(defPackage "XML-PARSER" (:use #+:ccl "CCL"
                               #+:allegro "CLOS"
                               "CL") (:nicknames "XMLP"))

(in-package "XML-PARSER")

(eval-when (:load-toplevel :compile-toplevel :execute)
  ;; this must be present at compile time since token constants will be
  ;; generated and require it to canonicalize their names
  (defConstant *markup-package*
    (defPackage "XML-1.0" (:use) (:nicknames "XML"))
    "the XML base package.
   it comprises those symbols which are standard to XML. they are interned in
   and exported from this package. all dtd-specific packages use this one to
   ensure that the symbols are always unambiguously present."))

(defVar *markup-pattern-package*
  (defPackage "XSL-1.0" (:use "XML") (:nicknames "XSL"))
  "adds the symbols from the XSL draf ca 00.08.97")
  
(defVar *markup-user-package*
  (defPackage "XML-USER" (:use "XML"))
  "the default markup reader package. it is asserted if the current package
   would otherwise not import from the XML base package.")

#|(shadowing-import 'clos::defClass :xml-parser)
#-:aclpc
(defclass xml-parser::mop-identity-class
  (clos:identity-class mcl-mop::mop-standard-class)
  ())|#


;;; the standard symbols from the XML package
;;;
(export '(XML-1.0::--
          XML-1.0::|#FIXED|
          XML-1.0::|#IMPLIED|
          XML-1.0::|#PCDATA|
          XML-1.0::|#RCDATA|
          XML-1.0::|#REQUIRED|
          XML-1.0::ALL
          XML-1.0::ANY
          XML-1.0::AS
          XML-1.0::ATTLIST
          XML-1.0::CDATA
          XML-1.0::CLASS
          XML-1.0::DEFAULT
          XML-1.0::DOCTYPE
          XML-1.0::ELEMENT
          XML-1.0::ELEMENT-REFERENCE
          XML-1.0::EMPTY
          XML-1.0::ENCODING
          XML-1.0::ENTITY
          XML-1.0::ENTITIES
          XML-1.0::HREF
          XML-1.0::ID
          XML-1.0::IDREF
          XML-1.0::IDREFS
          XML-1.0::IGNORE
          XML-1.0::INCLUDE
          XML-1.0::INTERNAL
          XML-1.0::NAMESPACE
          XML-1.0::NDATA
          XML-1.0::NMTOKEN
          XML-1.0::NMTOKENS
          XML-1.0::NONE
          XML-1.0::NOTATION
          XML-1.0::PRESERVE
          XML-1.0::PUBLIC
          XML-1.0::STANDALONE
          XML-1.0::|standalone|
          XML-1.0::SYSTEM
          XML-1.0::XML
          XML-1.0::XML-SPACE
          XML-1.0::XML-STYLESHEET
          )
        "XML-1.0")

;;; some XSL symbols
;;;
(export '(XSL-1.0::APPLY
          XSL-1.0::ARG
          XSL-1.0::ATTRIBUTE
          XSL-1.0::CHILDREN
          XSL-1.0::DEFINE-MACRO
          XSL-1.0::DEFINE-STYLE
          XSL-1.0::EVAL
          XSL-1.0::FROM
          XSL-1.0::HAS-VALUE
          XSL-1.0::IMPORT
          XSL-1.0::IMPORTANCE
          XSL-1.0::INVOKE
          XSL-1.0::MODE
          XSL-1.0::NAME
          XSL-1.0::ONLY
          XSL-1.0::POSITION
          XSL-1.0::PRIORITY
          XSL-1.0::ROOT
          XSL-1.0::RULE
          XSL-1.0::SELECT
          XSL-1.0::STYLE-RULE
          XSL-1.0::TARGET-ELEMENT
          XSL-1.0::TYPE
          XSL-1.0::VALUE
          XSL-1.0::XSL)
        "XSL-1.0")

(in-package :XML-PARSER)

;;; minimal node content and structure
;;;
(export '(xml-node
          xml-named-node
          xml-reference-node
          xml-external-node
          xml-node.parent
          xml-node.content
          xml-node.append-element
          xml-node.delete-element
          xml-node.name
          xml-node.referent
          xml-node.locations
          
          xml-pattern
          xml-pattern.attributes
          xml-pattern.content
          xml-pattern.occurrence
          xml-pattern.type
          xml-pattern.variable))
          
;;; parameters
;;;
(export '(*xml-pathname-defaults*
          *xml-preserve-comments* 
          *xml-preserve-whitespace* 
          *xml-print-readably*
          *xml-print-dtd-attlists*
          *xml-print-empty-close-tags*
          *xml-warn-if-redefine*
          *xml-warn-if-undefined*

          ;; CLOS
          *ATTRIBUTE-CLASS*
          *COMMENT-CLASS*
          *DOCUMENT-CLASS*
          *DOCUMENT-TYPE-DEFINITION-CLASS*
          *DTD-ELEMENT-CLASS*
          *DTD-ELEMENT-REFERENCE-CLASS*
          *NAMED-CHARACTER-REFERENCE-CLASS*
          *NUMERIC-CHARACTER-REFERENCE-CLASS*
          *XML-ELEMENT-CLASS*
          *XML-NAMESPACE-CLASS*
          *XML-XML-CLASS*
          *XML-PCDATA-CLASS*
          *XML-CDATA-CLASS*

          ;; XapiJ
          *XML-PROCESSOR-CLASS*

          ;; parser tokens
          *declaration-tag-marker*
          *processing-instruction-tag-marker* 
          *section-tag-marker*
          *close-tag-marker*
          *open-tag-section-marker*
          *close-tag-section-marker*
          *entity-marker*
          *parameter-entity-marker*
          *tag-end-marker*
          *opt-marker*
          *rep-marker*
          *plus-marker*
          *and-marker*
          *or-marker*
          *seq-marker*
          *cdata-element*
          *rcdata-element*
          *pcdata-element*
          ))

;;; generic reader symbols
;;;
(export '(*markup-readtable*
          *parent-node*
          *dtd*
          *document*
          markup-reader-macro
          node-class
          process-markup-element
          read-typed-markup-element
          read-markup-element
          read-markup-stream
          read-markup-tag-parameters
          read-xml-stream
          read-dtd-stream
          ))

;;; dtd-parser.lisp
;;; reader extensions for dtd elements

(export '(dtd
          dtd.name
          dtd.elements
          dtd.root
          dtd.file
          dtd.dtd-element
          dtd.dtd-qualified-element
          dtd-element
          dtd-element.name
          dtd-element.model
          dtd-element.attdefs
          dtd-element-declaration
          dtd-element-declaration.dtd
          dtd-element-declaration.name
          dtd-element-declaration.qualified-name
          dtd-element-declaration.model
          dtd-element-declaration.attdefs
          dtd-element-declaration.comments
          dtd-add-element
          dtd-delete-element
          reserved-dtd-name
          dtd-reserved-model
          dtd-element-reference
          dtd-attdef
          dtd-attdef.name
          dtd-attdef.context
          dtd-attdef.type
          dtd-attdef.mode
          dtd-model
          dtd-model-group
          dtd-element-model
          dtd-model.content
          dtd-model.occurrence
          dtd-model.connector
          xml-comment
          xml-comment.data
          ))


;;; pattern-function.lisp
;;; uses the pattern matcher to implement pattern-directed functions

(export '(defPattern-selector
          defPattern-method
          pattern.property
          ))



;;; pi-node.lisp
;;; reader extensions for processing instructions

(export '(pi-node
          xml-pi
          xml-pi.version
          xml-pi.encoding
          xml-pi.standalone
          ))

;;; xml-element-reader.lisp
;;;

(export '(xml-element
          xml-element.context
          xml-element.declaration
          xml-element.attributes
          xml-element.content
          xml-element.name
          xml-element.get
          xml-element.set
          make-xml-element
          XML-NUMERIC-ENTITY-REFERENCE
          xml-named-entity-reference
          dtd-entity.content
          xml-node.value
          ))

;;; xml-element-node.lisp
;;;

(export '(xml-node
          xml-node.name
          xml-node.parent
          xml-node.children

          xml-external-definition
          xml-node.locations
          xml-node.load-external-definition)
        )

;;; xml-record-node.lisp
;;;

(export '(xml-record-class
          xml-record-field
          xml-record
          element.field-content
          define-field-accessor
          make-field-reader
          make-field-writer
          dtd-element.record-description))

;;;
;;; xml-error
(export '(xml-cell-error
          xml-form-error
          xml-validity-error))
:EOF
