;;; -*- package: "XML-PARSER"; -*-
;;;

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
 <DELTA><DATE>19971125</DATE>
  XML-USER package to keep base xml package clean</DELTA>
 <DELTA><DATE>19971210</DATE>
  standalone instead of RMD</DELTA>
 <DELTA><DATE>19971217</DATE>
  nickname "XMLP" for parser application package.
   </DELTA>
  <DELTA><DATE>19980408</DATE>
   additional dom exports</DELTA>
 </CHRONOLOGY>
</DOCUMENTATION>
|#

(defPackage "XML-PARSER"
  (:use #+:ccl "CCL" #+:allegro "CLOS" #+:allegro "EXCL" "CL")
  (:nicknames "XMLP")
  ;; shadow several cl symbols as the purpose is respectively identical
  (:shadow "*PRINT-READABLY*" "PPRINT")
  #+CCL
  (:import-from "CCL" "NAME" "ERROR-TYPE" "STREAM-POSITION")
  #+LISPWORKS
  (import "HCL" "VALIDATE-SUPERCLASS"))

(in-package "XML-PARSER")

;;;
;;; intrinsic package definitions for XML namespaces
;;; the packages are named such that the proper name is short, but is NOT
;;; the colloquial term. the colloquial name(s) is(are) appear only in the list
;;; of namespace bindings.
;;; the long name - the one identified with the URI - appears as the first
;;; nickname, so that it doesn't print when the nodes are printed through a
;;; mechanism other than serialization.

(eval-when (:load-toplevel :compile-toplevel :execute)
  ;; this must be present at compile time since token constants will be
  ;; generated and require it to canonicalize their names
  (defPackage "xmlns"
      (:use)
      (:nicknames "XMLNS")
      (:intern "XMLNS.0" "XMLNS.1" "XMLNS.2" "XMLNS.3"))

  (defPackage "XMLNS.0"
      (:use)
      (:nicknames "http://www.w3.org/XML/1998/namespace"
                  "xml" "XML" "XML-1.0" )))


;;; the xml-user package is bound initially to the default namespace prefix

(defPackage "XMLNS.1" (:use "XML") (:nicknames "xml-user"))


;;; these package are prepared in order to make them available to code

(defPackage "XMLNS.2"
  (:use "XML")
  (:nicknames "xsl" "XSL" "XSL-1.0" "-//www.w3.org//BNF xsl//EN" ))

(defPackage "XMLNS.3"
  (:use "XML")
  (:nicknames "html" "-//www.w3.org//DTD html//EN"))


;;;
;;; the standard symbols from the XML package are predefined in order to ensure
;;; that they are found from every package and are not interned locally

(export '(XML-1.0::--
          XML-1.0::|#FIXED|
          XML-1.0::|#IMPLIED|
          XML-1.0::|#PCDATA|
          XML-1.0::|#RCDATA|
          XML-1.0::|#REQUIRED|
          XML-1.0::ALL
          XML-1.0::|amp|
          XML-1.0::ANY
          XML-1.0::|apos|
          XML-1.0::AS
          XML-1.0::ATTLIST
          XML-1.0::CDATA
          XML-1.0::CLASS
          XML-1.0::DEFAULT
          XML-1.0::DOCTYPE
          XML-1.0::ELEMENT
          XML-1.0::EMPTY
          XML-1.0::|encoding|
          XML-1.0::ENTITY
          XML-1.0::ENTITIES
          XML-1.0::|gt|
          XML-1.0::HREF
          XML-1.0::ID
          XML-1.0::IDREF
          XML-1.0::IDREFS
          XML-1.0::IGNORE
          XML-1.0::INCLUDE
          XML-1.0::INTERNAL
          XML-1.0::|lt|
          XML-1.0::|namespace|
          XML-1.0::NDATA
          XML-1.0::NMTOKEN
          XML-1.0::NMTOKENS
          XML-1.0::NONE
          XML-1.0::NOTATION
          XML-1.0::PRESERVE
          XML-1.0::PUBLIC
          XML-1.0::|quot|
          XML-1.0::|standalone|
          XML-1.0::|version|
          XML-1.0::SYSTEM
          XML-1.0::|xml|
          XML-1.0::XML-SPACE
          XML-1.0::XML-STYLESHEET
          )
        "XML-1.0")

;;;
;;; generator macro symbols
;;; these are an exception to those above, as they are used programmatically
;;; rather than appearing in encoded streams.

(export '(xml-1.0::DECLARE-XML-ENCODING
          xml-1.0::DECLARE-XML-VERSION
          xml-1.0::WITH-XML-DOCUMENT 
          xml-1.0::WITH-ELEMENT
          xml-1.0::WITH-EXTERNAL-ENTITY)
        "XML-1.0")

;;;
;;; some XSL symbols

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


;;;
;;; XML-PARSER symbols

(export '(*ATTRIBUTE-CLASS*
          *DEFAULT-DOCUMENT-TYPE*
          *DOCUMENT* 
          *DOCUMENT-TYPE* 
          *ERROR-HANDLER* 
          *ELEMENT-CLASS* 
          *ELEMENT-CLASS-ATTRIBUTE* 
          *END-TAG*
          *FORM-INDENT*
          *GLOBAL-ATTRIBUTES* 
          *PARENT-NODE* 
          *PRESERVE-COMMENTS* 
          *PRESERVE-WHITESPACE* 
          *PRINT-ATTLIST-DECLARATIONS* 
          *PRINT-EMPTY-CLOSE-TAGS* 
          *REDEFINED-ATTRIBUTE-CONDITION* 
          *REDEFINED-ELEMENT-CONDITION* 
          *REDEFINED-ENTITY-CONDITION* 
          *REDEFINED-NOTATION-CONDITION* 
          *START-TAG*
          *UNDEFINED-ATTRIBUTE-CONDITION* 
          *UNDEFINED-ELEMENT-CONDITION* 
          *UNDEFINED-ENTITY-CONDITION* 
          *UNDEFINED-NOTATION-CONDITION* 
          *XML-PATHNAME-DEFAULTS* 
          ATTRIBUTE 
          ATTRIBUTE.NAME 
          ATTRIBUTE.MODE 
          ATTRIBUTE.VALUE 
          COMMENT 
          COMMENT.DATA 
          DEFINE-FIELD-ACCESSOR 
          DEFAULT-DOCUMENT-TYPE
          DOCUMENT 
          DOCUMENT-TYPE 
          DOCUMENT.ELEMENT 
          DOCUMENT.ENCODING 
          DOCUMENT.STANDALONE? 
          DOCUMENT.VERSION 
          DOM 
          DOM* 
          DOM-ELEMENT 
          DOM-ELEMENT-STYLE 
          DOM-NAMESTRING 
          DOM-TARGET 
          DTD 
          DTD-ADD-ELEMENT 
          DTD-DELETE-ELEMENT 
          DTD-ELEMENT 
          DTD-ELEMENT.ATTDEFS 
          DTD-ELEMENT.MODEL 
          DTD-ELEMENT.NAME 
          DTD-ELEMENT.RECORD-DESCRIPTION 
          DTD-RESERVED-MODEL 
          DTD.ELEMENTS 
          DTD.FILE 
          DTD.NAME 
          DTD.ROOT 
          ELEMENT 
          ELEMENT-DECLARATION 
          ELEMENT-DECLARATION.ATTDEFS 
          ELEMENT-DECLARATION.DOCUMENT-TYPE 
          ELEMENT-DECLARATION.MODEL 
          ELEMENT-DECLARATION.NAME 
          ELEMENT-DECLARATION.PREDICATE 
          ELEMENT-MODEL 
          ELEMENT-MODEL-ELEMENT 
          ELEMENT-MODEL-GROUP 
          ELEMENT-MODEL.CONNECTOR 
          ELEMENT-MODEL.CONTENT 
          ELEMENT-MODEL.NAME 
          ELEMENT-MODEL.OCCURRENCE 
          ELEMENT.ATTRIBUTES 
          ELEMENT.CONTENT 
          ELEMENT.CONTEXT 
          ELEMENT.DECLARATION 
          ELEMENT.NAME 
          ELEMENT.PACKAGE 
          ELEMENT.SET 
          ENTITY-DECLARATION.CONTENT 
          INTERN-NAME
          INTERN-SYMBOL
          LET-DUPLICATE-ELEMENTS 
          LET-DUPLICATE-ELEMENTS-CONTENT 
          LET-ELEMENT-ATTRIBUTES 
          LET-ELEMENTS 
          LET-ELEMENTS-CONTENT 
          LET-ELEMENTS-CONTENT-LIST 
          LOAD-XML 
          MAKE-ELEMENT 
          MAKE-ELEMENT* 
          MAKE-FIELD-READER 
          MAKE-FIELD-WRITER 
          MAKE-MODEL 
          MAP-ELEMENT-CONTENT 
          NAMED-ENTITY-REFERENCE
          NOTATION-DECLARATION
          NOTATION-DECLARATION.NAME
          NUMERIC-ENTITY-REFERENCE 
          PROCESSING-INSTRUCTION 
          PROCESSING-INSTRUCTION.TARGET 
          PROCESSING-INSTRUCTION.DATA
          READ-DTD-STREAM 
          READ-MARKUP-ELEMENT 
          READ-MARKUP-STREAM 
          READ-MARKUP-TAG-PARAMETERS 
          READ-XML-ELEMENT
          READ-XML-STREAM 
          RESERVED-DTD-NAME 
          TRANSLATE-PUBLIC-ID 
          WHEN-DUPLICATE-ELEMENTS 
          WHEN-ELEMENT-CONTENT-LIST 
          WHEN-ELEMENTS 
          WHEN-ELEMENTS-CONTENT
          WRITE-IDENTIFIER
          XML-CELL-ERROR 
          XML-DECL-PI 
          XML-DECL-PI.ENCODING 
          XML-DECL-PI.STANDALONE 
          XML-DECL-PI.VERSION 
          XML-EXTERNAL-DEFINITION 
          XML-EXTERNAL-NODE 
          XML-FILE-P 
          XML-FORM-ERROR 
          XML-LINK-ELEMENT 
          XML-NAMED-NODE 
          XML-NODE 
          XML-NODE 
          XML-NODE.APPEND-ELEMENT 
          XML-NODE.COMMENTS 
          XML-NODE.CONTENT 
          XML-NODE.CONTENT 
          XML-NODE.CONTEXT 
          XML-NODE.CONTEXT 
          XML-NODE.DELETE-ELEMENT 
          XML-NODE.GET
          XML-NODE.LOAD-EXTERNAL-DEFINITION 
          XML-NODE.NAME 
          XML-NODE.NAME 
          XML-NODE.PUBLIC-ID 
          XML-NODE.REFERENCE 
          XML-NODE.REFERENT 
          XML-NODE.SET
          XML-NODE.STRING 
          XML-NODE.SYSTEM-ID 
          XML-NODE.VALUE 
          XML-PATTERN 
          XML-PATTERN.ATTRIBUTES 
          XML-PATTERN.CONTENT 
          XML-PATTERN.OCCURRENCE 
          XML-PATTERN.TYPE 
          XML-PATTERN.VARIABLE 
          XML-RECORD
          XML-RECORD-CLASS 
          XML-RECORD-FIELD 
          XML-WARN
          XML-VALIDITY-ERROR 
          XPTR
          XPTR.TERM
          XPTR.REST
          XPTR-ACCESSOR
          XPTR-ACCESSOR.LOCATIONS
          XPTR-ACCESSOR.TERM
          XPTR-ACCESSOR.XPTR
          XPTR-TERM
          XPTR-TERM.ATTRIBUTES
          XPTR-TERM.QUANTIFIER
          XPTR-TERM.TYPE
          XPTR-TERM.STRING
          XPTR-NODE-VALUE
          XPTR-NODE-SET-VALUE
          XPTR-VALUE
          XPTR-SET-VALUE
          WITH-XPTRS
          WITH-XPTR-STRINGS
          WITH-XPTR-ELEMENTS
          WITH-NAMESPACE-BINDINGS
          WITH-NAMESPACE-BOUND
          WITH-DEFAULT-NAMESPACE
          WITH-ELEMENT-NAMESPACES
          ))


;;; if the common lisp types are not yet present, create the package in
;;; preparations for "clos-dom.lisp". note that the file "cl-types.lisp"
;;; can reify the cl type hierarchy, but it takes 150K from the heap to do it.

(let ((package (find-package "CL-TYPES")))
  (cond (package
         (do-symbols (symbol package)
           (export symbol package)))
        (t
         (defPackage "CL-TYPES"
           (:use)
           (:export "ARRAY"
                    "CLASS"
                    "CLASS-NAME"
                    "CONS"
                    "FUNCTION"
                    "GENERIC-FUNCTION"
                    "LIST"
                    "METHOD"
                    "PACKAGE"
                    "SEQUENCE"
                    "STANDARD-OBJECT"
                    "STRING"
                    "SYMBOL"
                    "T"
                    "VECTOR")))))



(import '(url::name-string url::url url::file-url url::http-url
          url::translated-pathname url::parse-url))
(import '(http::*local-context* http::local-context))


"XMLP"

