;;; -*- package: ("XML-PARSER")

(in-package :xml-PARSER)

;; models are combinations of element names with +,*,?,&,|, and ','

(with-input-from-string (stream "((a+, b)? & (c | d)*)")
  (read-markup-model stream))
(make-model (with-input-from-string (stream "((a+, b)? & (c | d)*)")
               (read-markup-model stream)))

(inspect
 (make-model (with-input-from-string (stream "((a+, b)? & (c | d)*)")
               (read-markup-model stream))))

;; reading without processing just to see the possible elements
;; note that the standard read table is augmented with an entry for
;; marked up strings.

(inspect '(#!<!ELEMENT asdf (a, s)>
           #!<!ENTITY asdf 'testing'>
           #!<!NOTATION xyz>
           #!<!ATTLIST asdf att1 #FIXED '123'>
           #!<?XML VERSION='1.0'?>
           #!<?NAMESPACE HREF='xyz/123' AS='ASDF' ?>
           #!<TEST>this is a test</TEST>))


;; note, the control parameters: for readable but extensive output v/s
;; brief but unreadable; for content whitespace according to standard or not;
;; for no close tag if there is no content; to prevent or allow redefinitions

(setf *xml-print-readably* nil)         ; (setf *xml-print-readably* t)
(setf *xml-preserve-whitespace* t)      ; (setf *xml-preserve-whitespace* nil)
(setf *xml-print-empty-close-tags* nil) ; (setf *xml-print-empty-close-tags* t)
(setf *xml-warn-if-redefine* t)         ; (setf *xml-warn-if-redefine* nil)


;; also, the simplest possible examples of xml data - note the effect of the
;; setting for whitespace.
;; note that for simple reading, the active dtd must be null, or the there is
;; a namespace mismatch since the package is important

(setf *dtd* nil)
#!"<TEST>this is a test</TEST>"
#!"<TEST>
 this
 is
 another
 test
 </TEST>"

;;; now with attributes
(inspect (list #!"<TEST att1='asdf' OCCURRENCE=* ATTRIBUTES=<ATTLIST v1=1 v2=2/>>
    content here
    </TEST>"))

;;; empty, without a close tag
#!"<TEST att1='asdf' att2=qwert/>"


;; now reading with a dtd
;; note that this can't use read macros, since the point is to do the
;; reading in the presence of rebound specials, which the specific functions
;; for dtd and xml streams provide
;;
;; first read with an explicitly bound dtd

(setf (dtd t) nil)

(read-dtd-stream
 "<!DOCTYPE TEST>
       <!ELEMENT TEST (a, s)>
       <!ELEMENT a #PCDATA>
       <!ELEMENT s #PCDATA>
       <!ENTITY asdf '#FIXED testing'>
       <!NOTATION xyz>
       <!ATTLIST TEST att1 CDATA %asdf; >")
(inspect (dtd "TEST"))
(inspect
(read-xml-stream
 "<?XML VERSION 1.0 ?>
  <!DOCTYPE TEST>
  <TEST><a>1234</a> <s>qwert</s></TEST>"))
(xml-element.get (xml-document.element (top-inspect-form)) 'TEST::|att1|)


;; now expunge that dtd and read with an external dtd, either loaded
;; explicitly, or by referenceing it from a document

(setf (dtd t) nil)
(read-dtd-stream #p"home:source;xml;documenttypedefinitions;lisp.dtd")
(load-dtd #p"home:source;xml;documenttypedefinitions;lisp.dtd")
;(inspect (dtd "LISP"))
;(setf (dtd "LISP") nil)

(dtd *dtd*)
;; then a document
(inspect
 (read-xml-stream
  "<?XML VERSION 1.0 RMD=ALL ?>
  <!DOCTYPE LISP SYSTEM 'home:source;xml;documenttypedefinitions;lisp.dtd'>
  <LISP>
   <DEFPARAMETER> <NAME>testing</NAME><!-- a comment -->
    <VALUE>one two three</VALUE> </DEFPARAMETER>
   </LISP>"))

(inspect
(read-xml-stream
 "<?XML VERSION 1.0 ?>
  <!DOCTYPE LISP SYSTEM 'home:source;xml;documenttypedefinitions;lisp.dtd'>
  <LISP>
   <DEFPARAMETER> <NAME>testing</NAME><!-- a comment -->
    <VALUE>one two three</VALUE> </DEFPARAMETER>
   </LISP>"))

(let ((*xml-print-readably* t)) (print (top-inspect-form)))



;; more document type definitions

#!"<!DOCTYPE test>"
#!"<!ELEMENT test (e1*) >"
#!"<!element e1 (a+ | b*) -- foo>"

(inspect
 (read-dtd-stream
  "<!DOCTYPE test>
   <!ELEMENT test (e1*) >
   <!element e1 (a+ | b*) -- foo>
   <!element a #PCDATA -- foo>
   <!element b #PCDATA -- foo>
   <!ATTLIST e1 :SET-NO #CDATA #FIXED 5>"))

(read-dtd-stream
"
<!DOCTYPE LAB-DATA>
<!ELEMENT LAB-DATA (DATA-SOURCE*) >
<!ELEMENT DATA-SOURCE (NAME, DESCRIPTION, PROPERTIES)>
<!ELEMENT ABREVIATED-SOURCE (ID, DESCRIPTION)>
<!ATTLIST ABREVIATED-SOURCE
         :NAME CDATA #FIXED DATA-SOURCE>
<!ELEMENT NAME CDATA>
<!ELEMENT ID CDATA>
<!ELEMENT DESCRIPTION CDATA>
<!ELEMENT PROPERTIES (PROTOCOL?, DIRECTION?, ENCRYPTION?) >
<!ATTLIST PROPERTIES :NAME CDATA #FIXED TEST>
<!ATTLIST ID :NAME CDATA #FIXED NAME>
<!ELEMENT PROTOCOL CDATA>
<!ELEMENT DIRECTION CDATA>
<!ELEMENT ENCRYPTION CDATA>
")

(inspect (dtd "LAB-DATA"))
(setf (dtd "LAB-DATA") nil)

(read-xml-stream
"<?XML VERSION=1.0 RMD=INTERNAL ?>
<!DOCTYPE LIBRARY [ <!ELEMENT LIBRARY (lit*) >
                    <!ELEMENT lit (autor, titel) >
                    <!ELEMENT autor (vor, nach) >
                    <!ELEMENT titel #PCDATA >
                    <!ELEMENT vor #PCDATA >
                    <!ELEMENT nach #PCDATA > ]>
<LIBRARY>
 <lit><autor><VOR>an</VOR><NACH>author</NACH></autor>
      <titel>ein buch</titel></lit>
 <lit><autor><VOR>another</VOR><NACH>author</NACH></autor>
      <titel>noch ein buch</titel></lit>
 <lit><autor><VOR>the last</VOR><NACH>author</NACH></autor>
      <titel>das letzte buch</titel></lit>
 </LIBRARY>
")

(inspect (dtd "LIBRARY"))
(setf (dtd "LIBRARY") nil)


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
(let ((*xml-print-readably* t)) (print *e1*))
(inspect *e1*)

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

(setf (dtd "KUM") nil)
(read-dtd-stream "entwicklung:source:lisp:xml:data:waldherr.dtd")


:EOF

