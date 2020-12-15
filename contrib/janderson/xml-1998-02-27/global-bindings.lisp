;;; -*- package: ("XML-PARSER") -*-
;;;
;;; this version (C) mecom gmbh 24.11.97
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.

#|
<DOCUMENTATION>
 <DESCRIPTION>
 global bindings
 </DESCRIPTION>
 <CHRONOLOGY>
  <DELTA><DATE>19971124</DATE>*xml-warn-if-undefined*</DELTA>
 <DELTA><DATE>19971204</DATE>
  version 0.30:<BR>
        model compiler; element matcher, xml pi according to PR-19921208
  </DELTA>
 <DELTA><DATE>19980102</DATE>
  cdata and pcdata classes</DELTA>
</CHRONOLOGY>
</DOCUMENTATION>
|#
(in-package :xml-parser)



(defVar *document* nil
  "binds the currently processed document.")

(defVar *document-context* nil
  "binds the context of the currently processed document.")

(defvar *parent-node* nil
  "binds the current node when working with a dtd or reading a document.
   the name of the 'current' node is used to generate qualified names.
   component nodes are added to the current node after creation.")

(defVar *processed-node* nil
  "binds the last processed node for error reporting.")

(defParameter *markup-stream* nil
  "binds the stream from which a document definition or element definitions
   is/are being read.")

(defVar *dtd* nil
  "binds the currently active dtd.
   the name of the 'current' dtd serves to generate qualify element names.
   when NULL, a default dtd is always installed
   when parsing a stream. should no dtd be present, then reader side-effects
   which would modify the dtd have no effect and element definitions are
   resolved with respect to a global namespace.")

(defVar *pattern-environment* nil
  "binds the dynamic state of a given pattern function application.
   since it is rebound for each application, the global binding is never used.")

(defParameter *xml-handle-errors* nil
  "specifies whether the reader is to try to handle errors, or pass them on.")

(defParameter *xml-preserve-whitespace* nil
  "determines whether element content preserves or suppresses whitespace.")

(defParameter *xml-preserve-comments* t
  "determines whether comments are preserved and printed.")

(defParameter *xml-print-readably* nil
  "determines if the elements print completely or as summaries.")

(defparameter *xml-print-dtd-attlists* nil
  "controls whether the list of attributes is printed together with the element.
   the static binding should remain NIL.")

(defParameter *xml-print-empty-close-tags* nil
  "determines if empty elements print with or without close tags")

(defParameter *xml-verbose* nil)

(defParameter *xml-warn-if-redefine* t
  "specifies whether to enforce the standard validation rules should a dtd
   element be redefined.")

(defParameter *xml-warn-if-undefined* t
  "specifies whether to enforce the standard validation rules should a dtd
   element be undefined.")

(defVar *xml-markup-ignore* nil
  "specifies whether the elements are to be processed as read.")

(defVar *xml-version* 1.0)
(defVar *xml-processor-version* 0.30)

;;; parameters for stream syntax

(defVar *markup-type-readtable* nil
  "a readtable specialized for markup tag type syntax")
(defVar *markup-entity-readtable* nil
  "a readtable specialized for markup entity syntax")
(defVar *markup-model-readtable* nil
  "a readtable specialized for markup model syntax (*, +, |, etc)")
(defVar *markup-attribute-readtable* nil
  "a readtable specialized for markup tag attribute syntax")
(defVar *markup-pcdata-readtable* nil
  "a readtable specialized for markup content in PCDATA syntax.
   it enforces special syntax for '<', '&' only. CDATA is handled with
   a direct string reader.")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defVar *declaration-tag-marker* (marker-tag "MDO"))
  (defVar *processing-instruction-tag-marker* (marker-tag "PIO"))
  (defVar *section-tag-marker* (marker-tag "DTGO"))
  (defVar *close-tag-marker* (marker-tag "NET"))
  (defVar *open-tag-section-marker* (marker-tag "DSO"))
  (defVar *close-tag-section-marker* (marker-tag "DTGC"))
  (defVar *entity-marker* (marker-tag "ERO"))
  (defVar *parameter-entity-marker* (marker-tag "PERO"))
  (defVar *tag-end-marker* (marker-tag "TAGC"))
  (defVar *opt-marker* (marker-oc "opt"))
  (defVar *rep-marker* (marker-oc "rep"))
  (defVar *plus-marker* (marker-oc "plus"))
  (defVar *and-marker* (marker-sep "and"))
  (defVar *or-marker* (marker-sep "or"))
  (defVar *seq-marker* (marker-sep "seq")))

(defVar *start-tag-open*)
(defVar *end-tag-open*)
(defVar *tag-close*)
(defVar *tag-open-char*)
(defVar *tag-close-char*)
(defVar *cdata-close-char*)
(defVar *entity-reference-open-char*)
(defVar *p-entity-reference-open-char*)

(setf (logical-pathname-translations "dtd")
      `(("**;*.*" "Source:XML;dtd;**;*.*")))

(setf (logical-pathname-translations "xml")
      `(("**;*.*" "Source:XML;xml;**;*.*")))

(defparameter *dtd-pathname-defaults*
  (translate-logical-pathname "Source:XML;dtd;*.dtd")
  "binds the pathname for the directory in which document type definitions
   and element definitions are stored.")

(defparameter *xml-pathname-defaults*
  (translate-logical-pathname "Source:XML;xml;*.xml"))


;;;
;;; parameters for node classes

(defParameter *attribute-class* 'xml-attribute)
(defParameter *attribute-definition-class* 'dtd-attdef)
(defParameter *comment-class* 'xml-comment)
(defparameter *concatenated-stream-class* 'concatenated-stream)
(defParameter *document-type-definition-class* 'dtd)
(defParameter *document-type-declaration-class* 'doctype)
(defParameter *dtd-document-class* 'dtd-document)
(defParameter *dtd-element-class* 'dtd-element-declaration)
(defParameter *dtd-element-reference-class* 'dtd-element-reference)
(defParameter *entity-class* 'dtd-named-entity)
(defParameter *entity-class* 'dtd-named-entity)
(defParameter *named-character-reference-class* 'xml-named-entity-reference)
(defParameter *notation-class* 'dtd-notation)
(defParameter *numeric-character-reference-class* 'xml-numeric-entity-reference)
(defParameter *parameter-entity-class* 'dtd-parameter-entity)
(defParameter *xml-document-class* 'xml-document)
(defParameter *xml-element-class* 'xml-element)
(defParameter *xml-xml-class* 'xml-xml)
(defParameter *xml-namespace-class* 'xml-namespace)
(defParameter *xml-pcdata-class* 'parsed-character-data)
(defParameter *xml-cdata-class* 'character-data)

;;;
;;; XSL parameters

(defParameter *xsl-type-attribute* 'XSL::TYPE)
(defParameter *xsl-attributes-attribute* 'XSL::ATTRIBUTES)
(defParameter *xsl-occurrence-attribute* 'XSL::OCCURRENCE)
(defParameter *xsl-variable-attribute* 'XSL::VARIABLE)
(defParameter *pattern-element-class* 'xml-pattern-element)
(defParameter *xsl-excluded-content-types*
  '(xsl::attribute))

(defParameter *xml-validate?* nil)

(defParameter *xml-encoding-element-type* 'character)

:EOF
