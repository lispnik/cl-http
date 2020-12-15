;;; -*- package: ("XML-PARSER")

(in-package :xml-PARSER)


#|
 here is a simple demonstration of XSL-type pattern matching
 for reference the xsl patterns are themselves described with a
 dtd
 |#

(let ((*xml-print-readably* t))
  (print (dtd "XSL")))

#|
 for the purpose of demonstration, it would be possible to generate elements
 procedurally, but the printform is conciser...

(make-instance 'xml-pattern-element
    :declaration (dtd-element 'xsl::element)
    :content
    (list (make-instance 'xml-pattern-element
            :declaration (dtd-element 'xsl::target-element)
            :attributes
            (list (make-instance 'xml-attribute
                    :name 'xsl::variable
                    :value 'RECHNUNGSADRESSE)
                  (make-instance 'xml-attribute
                    :name 'xsl::type
                    :value 'RECHNUNG)))))

 a simple, XSL-conforming pattern specifies a single target element in some
 context. here we pick out either the occupation or the address

 note that, in order to benefit from the dtd, the element must specify the
 correct namespace.
|#

(defParameter *personnel-dtd*
  (document.dtd
   (read-dtd-stream
    "<!DOCTYPE PERSONNEL>
     <!ELEMENT COMPANY (PERSON)* >
     <!ELEMENT PERSON (JOB, ADDRESS+, SALARY?)>
     <!ELEMENT JOB #PCDATA >
     <!ELEMENT ADDRESS (NAME, STREET?) >
     <!ELEMENT SALARY #PCDATA > >
     <!ELEMENT NAME #PCDATA > >
     <!ELEMENT STREET #PCDATA >")))
;(mapc #'print (dtd-instances))

(defparameter *e1*
  #!"<PERSONNEL::PERSON>
     <JOB>Rechtsanwalt</JOB>
     <ADDRESS XSL:TYPE=STATEMENTS><NAME>the address for statements</NAME></ADDRESS>
     <ADDRESS><NAME>the home address</NAME></ADDRESS>
     <SALARY>too high</SALARY>
     </PERSONNEL::PERSON>")
(defparameter *e2*
  #!"<PERSONNEL::PERSON>
     <JOB>mail clerk</JOB>
     <ADDRESS><NAME>the home address</NAME></ADDRESS>
     <SALARY>too low</SALARY>
     </PERSONNEL::PERSON>")
(defparameter *e3*
  #!"<PERSONNEL::PERSON>
     <JOB>mail clerk</JOB>
     <NAME>the home address</NAME>
     <SALARY>too low</SALARY>
     </PERSONNEL::PERSON>")

(let ((*xml-print-readably* t)
      (*print-pretty* t))
  (print *e1*))

(inspect *e1*)
(inspect *e2*)

(xml-element.valid? *e1*)
(xml-element.valid? *e2*)
(xml-element.valid? *e3*)


#|
 there are two ways to test a given element:
 <OL>
 <LI>for validity and</LI>
 <LI>as a match to some pattern.</LI>
 </OL>
 <CODE>XML-ELEMENT.VALID?</CODE> peforms the validity test against a model.
 <CODE>PATTERN&DATUM</CODE> performs a match against a pattern.
 |#

(defparameter *?1*
  #!?<element><target-element VARIABLE=STATEMENT-ADDRESS
                              TYPE=PERSONNEL::STATEMENTS/>
       </element>)
(pattern-variables *?1*)
(let ((*xml-print-readably* t)
      (*print-pretty* t))
  (print *?1*))
(xml-element.valid? *?1*)
(pattern&datum *?1* *e1*)


#|
 the single target element in *?1* is all that the proposed XSL standard
 permits.
 it in addition to the single target proposed by XSL, it 

(defparameter *?2*
  #!?<element><target-element VARIABLE=STATEMENT-ADDRESS
                              TYPE=PERSONNEL::STATEMENTS/>
              <target-element VARIABLE=OCCUPATION
                              TYPE=PERSONNEL::JOB/>
       </element>)
(pattern-variables *?2*)
(let ((*xml-print-readably* t)
      (*print-pretty* t))
  (print *?2*))

(xml-element.valid? *?2*)
(pattern&datum *?2* *e1*)
(pattern&datum *?1* *e1*)

(defparameter *?bad*
  #!?<element><target-element VARIABLE=ADDRESS
                              TYPE=XML-USER::RECHNUNG/>
              <rule VARIABLE=OCCUPATION
                              TYPE=XML-USER::BERUF/>
       </element>)
(xml-element.valid? *?bad*)

#|
(trace pattern&datum pattern-fail pattern-succeed
       PATTERN&DATUM->CONTINUE
       PATTERN&DATUM-EXPRESSION
       pattern&datum/attributes->continue
       PATTERN-ATTRIBUTE&DATUM->CONTINUE
       PATTERN&DATUM/RELATIONS->CONTINUE
       PATTERN&DATUM/RELATIONS->CONTINUE-OCCURRENCE
       PATTERN&DATUM/TYPES?
       bind-pattern-variable)
(untrace)
|#
