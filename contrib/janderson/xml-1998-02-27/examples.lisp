;;; -*- package: ("XML-PARSER")

(in-package :xml-PARSER)

#|first, the control parameters: for readable but extensive output v/s
  brief but unreadable; for content whitespace according to standard or not;
  for no close tag if there is no content; to prevent or allow redefinitions |#
(setf *xml-print-readably* nil)         ; (setf *xml-print-readably* t)
(setf *xml-preserve-whitespace* t)      ; (setf *xml-preserve-whitespace* nil)
(setf *xml-print-empty-close-tags* nil) ; (setf *xml-print-empty-close-tags* t)
(setf *xml-warn-if-redefine* t)         ; (setf *xml-warn-if-redefine* nil)

#|next, the simplest possible examples of xml data - note the effect of the
  setting for whitespace.

#!"<TEST>this is a test</TEST>"
#!"<TEST>
 this
 is
 another
 test
 </TEST>"

;;; now with attributes
#!"<TEST att1='asdf' OCCURRENCE=* ATTRIBUTES=<ATTLIST v1=1 v2=2/>>content here</TEST>"

;;; empty, without a close tag
#!"<TEST att1='asdf' att2=qwert/>"

;; first of all there are document type definitions.
;; these may appear in files (named by pathname), in strings, or in streams
;(inspect *)
;(inspect *dtd*)
;(setf *dtd* nil)
;(setf (dtd.elements *dtd*) nil)
#!"<!DOCTYPE test>"
#!"<!ELEMENT test (e1*) >"
#!"<!element e1 (a+ | b*) -- foo>"

(inspect
 (read-markup-stream
  "<!DOCTYPE test>
   <!ELEMENT test (e1*) >
   <!element e1 (a+ | b*) -- foo>
   <!element a #PCDATA -- foo>
   <!element b #PCDATA -- foo>
   <!ATTLIST e1 :SET-NO #CDATA #FIXED 5>"))



;; note that the standard read table is augmented with an entry for
;; marked up strings.

#!"
<!DOCTYPE LAB-DATA>
<!ELEMENT LAB-DATA (DATA-SOURCE*, ) >
<!ELEMENT DATA-SOURCE (NAME, DESCRIPTION, PROPERTIES)>
<!ELEMENT ABREVIATED-SOURCE (ID, DESCRIPTION)>
<!ATTLIST ABREVIATED-SOURCE
         :NAME #CDATA #FIXED DATA-SOURCE>
<!ELEMENT NAME #CDATA>
<!ELEMENT ID #CDATA>
<!ELEMENT DESCRIPTION #CDATA>
<!ELEMENT PROPERTIES (PROTOCOL?, DIRECTION?, ENCRYPTION?) >
<!ATTLIST PROPERTIES :NAME #CDATA #FIXED TEST>
<!ATTLIST ID :NAME #CDATA #FIXED NAME>
<!ELEMENT PROTOCOL #CDATA>
<!ELEMENT DIRECTION #CDATA>
<!ELEMENT ENCRYPTION #CDATA>
"

#!"
<!DOCTYPE LIBRARY>
<!ELEMENT LIBRARY (lit*) >
<!ELEMENT lit (autor, titel) >
<!ELEMENT autor #CDATA >
<!ELEMENT titel #CDATA >
"


;; in addition to the data-access primitives, there are tools for
;; pattern-directed access to the markup data-trees.
;; these include both primitive pattern matching, and pattern-directed
;; generic functions.

(defParameter *e1*
  #!"<LAB-DATA::DATA-SOURCE>
      <NAME>HCG</NAME>
      <NAME>a hcg test</NAME>
      <DESCRIPTION>this element describes a laboratory test</DESCRIPTION>
      <PROPERTIES>
       <PROTOCOL>ASTM</PROTOCOL>
       <ENCRYPTION>KBV</ENCRYPTION>
       </PROPERTIES>
      </DATA-SOURCE>")
;(inspect *e1*)

(defParameter *p1*
  (xml-node "<LAB-DATA::DATA-SOURCE?>
              <NAME!/>
              <DESCRIPTION!/>
              <PROPERTIES!/>
              </DATA-SOURCE>"))
;(inspect *p1*)
(defParameter *p2*
  (xml-node "<LAB-DATA::ABREVIATED-SOURCE>
              <ID?/><DESCRIPTION?/>
              </ABREVIATED-SOURCE>"))
(defParameter *result1* (match-node *p1* *e1*))
(defParameter *result2* (match-node *p2* *e1*))
;(inspect *result1*)
;(inspect *result2*)


;; the functions comprise two kinds of method: selection patterns and action
;; methods. the former are named patterns. the latter are functions which
;; are applied only if the parameters can be bound.
;; thay are packaged together in a single function object which can be
;; applied directly to a markup instance

(defPattern-selector (labortest from-ldt)
  "<LAB-DATA::DATA-SOURCE!><NAME!/></DATA-SOURCE>")
(defPattern-selector (labortest from-kum)
  "<LAB-DATA::DATA-SOURCE!><NAME!/><DESCRIPTION!/></DATA-SOURCE>")
(defPattern-method lab-data
  (data-source name description)
  (inspect (list :name name :description description))
  (call-next-method))
(defPattern-method labortest :before
  (data-source name &rest arguments)
  (inspect (list :arguments arguments))
  (setf (pattern.property :args) arguments))
(defPattern-method labortest :after
  (data-source name)
  (inspect (list :after (pattern.property :args))))
(defPattern-method labortest
  (data-source name &rest arguments)
  (inspect (list arguments data-source name)))
;(inspect #'labortest)
;(inspect #'?::labortest)

;(trace match-fail)
;(trace match-node)
(labortest *e1*)


;;;
;;; tests for record access

(defrecord test (field1 :word) (field2 :long) (field3 (:string 7)))
(defGeneric test.field1 (test))
(defGeneric (setf test.field1) (datum test))

(defClass test (xml-record)
  ()
  (:record-definition (field1 :word)
                      (field2 :long)
                      (field3 (:string 7))
                      (field4 (:array :character 8))
                      (field5 (:array :byte 8)))
  (:metaclass xml-record-class))

(rlet ((test :test :field1 1 :field2 2 :field3 "asdf"))
  (%put-fstring test "12345678" 14 8)
  (%put-vector test #(1 2 3 4 5 6 7 8) 22 8)
  (let ((i (make-instance 'test :record test)))
    (inspect i)
    (break)
    (incf (test.field1 i))
    (list (xml-node.field-content i :field1)
          (xml-node.field-content i :field2)
          (xml-node.field-content i :field3)
          (xml-node.field-content i :field4)
          (xml-node.field-content i :field5))))


:EOF

