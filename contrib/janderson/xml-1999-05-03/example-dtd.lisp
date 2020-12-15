;;; -*- package: "XML-PARSER"; -*-

(in-package "XML-PARSER")
(unless (get-dispatch-macro-character #\# #\!)
  (patch-readtable-for-xml))

;; models are combinations of element names with +,*,?,&,|, and ','
;; see the end of "element-model.lisp" for a set of further examples

(inspect
 (list 1
       (make-model (with-input-from-string (stream "((a+, b)? & (c | d)*)")
                     (read-markup-model (encoded-stream stream))))))

;; or as shorthand - careful here: select this explicitly, as
;; the editor may otherwise not parse it correctly when making a selection stream

(inspect (list 2 #!M((a+, b)? & (c | d)*)  ))


;; reading without processing just to see the possible elements
;; note that the standard read table is augmented with an entry for
;; marked up strings.

(inspect '(3
           (#!!<!ELEMENT asdf (a, s)>
            #!!<!ELEMENT asdf ANY>
            #!!<!ELEMENT asdf EMPTY>
            #!!<!ENTITY asdf 'testing'>
            #!!<!NOTATION xyz>
            #!!<!ATTLIST asdf att1 CDATA #FIXED '123'>
            #!!<?XML VERSION='1.0'?> ; returns no value
            #!!<?NAMESPACE HREF='xyz/123' AS='ASDF' ?> ; returns no value
            #!"<TEST>this is a test</TEST>")))


;; note, the control parameters: for readable but extensive output v/s
;; brief but unreadable; for content whitespace according to standard or not;
;; for no close tag if there is no content; to prevent or allow redefinitions

(setf *print-readably* nil)             ; (setf *print-readably* t)
(setf *preserve-whitespace* t)          ; (setf *preserve-whitespace* nil)
(setf *print-empty-close-tags* nil)     ; (setf *print-empty-close-tags* t)
(setf *redefined-element-condition* 'redefined-element-error)
                                        ; (setf *warn-if-redefine* nil)


;; also, the simplest possible examples of xml data - note the effect of the
;; setting for whitespace.
;; note that for simple reading, the active dtd must be null, or the there is
;; a namespace mismatch since the package is important


#!"<TEST>this is a test</TEST>"
; #!"<+TEST>this is a test</TEST>" ; should error
; #!"<(TEST TOO)>this is a test</TEST>" ; should error

(inspect '(4
           #!"<TEST>
 this
 is
 another
 test
 </TEST>"))

;;; now with attributes
(inspect '(5
           #!"<TEST att1='asdf' OCCURRENCE='*' ATTRIBUTES='<ATTLIST v1=1 v2=2/>'>
    content here
    </TEST>"))

;;; empty, without a close tag
(inspect '(6
           #!"<TEST att1='asdf' att2='qwert'/>"))


;; now reading with a dtd
;; note the result of reading the dtd stream is the last declaration only.

;(%dtd-clear (dtd "TEST"))

(inspect
 (list 7
       (with-default-document-type ()
         (read-dtd-stream
        (encoded-stream "
<!ELEMENT TEST (a, s)>
       <!ELEMENT a #PCDATA>
       <!ELEMENT s #PCDATA>
       <!ENTITY asdf '#FIXED testing'>
       <!NOTATION xyz>
       <!ATTLIST TEST att1 CDATA '%asdf;' >
")))))

(inspect
 (list 8
       (with-default-document-type ()
       (read-dtd-stream "
<!ELEMENT test (e1*) >
<!ELEMENT e1 (a+ | b*) >
<!ELEMENT a #PCDATA >
<!ELEMENT b #PCDATA >
<!ATTLIST e1 SET-NO CDATA #FIXED '5'>
"))))

(inspect
 (list 9
       (with-default-document-type ()
       (read-dtd-stream
"
<!ELEMENT LAB-DATA (DATA-SOURCE*) >
<!ELEMENT DATA-SOURCE (NAME, DESCRIPTION, PROPERTIES)>
<!ELEMENT ABREVIATED-SOURCE (ID, DESCRIPTION)>
<!ATTLIST ABREVIATED-SOURCE
         :NAME CDATA #FIXED DATA-SOURCE>
<!ELEMENT NAME CDATA>
<!ELEMENT ID CDATA>
<!ELEMENT DESCRIPTION CDATA>
<!ELEMENT PROPERTIES (PROTOCOL?, DIRECTION?, ENCRYPTION?) >
<!ATTLIST PROPERTIES NAME CDATA #FIXED TEST>
<!ATTLIST ID NAME CDATA #FIXED NAME>
<!ELEMENT PROTOCOL CDATA>
<!ELEMENT DIRECTION CDATA>
<!ELEMENT ENCRYPTION CDATA>
"))))


;; these document type definitions are read in the context of a temporary instance.
;; this means that although the results are returned, they will not be reflected
;; in a persistent dtd unless the stream read is itself a document - that is
;; incorperates its own nams doctype

;;
(with-default-document-type () 
  (inspect
   (list 10
         (read-dtd-stream (merge-pathnames (make-pathname :directory '(:relative "dtd") :name "lisp" :type "dtd")
                                           cl-user::*xml-source-directory*)))))

;; now document streams, which on the otherhand, returns the document
;; not the syntax for system id values

(inspect
 (list 11
       (read-xml-stream  ;; nb, the xml decl must sart at the first byte...
        (encoded-stream "<?XML version='1.0' ?>
  <!DOCTYPE TEST>
  <TEST><a>1234</a> <s>qwert</s></TEST>"))))


#+ALLEGRO
(defun choose-file-default-directory ()
  (format *terminal-io* "default-directory: ")
  (setf *default-pathname-defaults* (pathname (string-trim #(#\newline #\space #\return) (read-line *terminal-io*)))))

(with-local-context (pathname (choose-file-default-directory ))
  (inspect
   (list 12
         (read-xml-stream
          (encoded-stream "<?XML version='1.0' standalone='no' ?>
  <!DOCTYPE LISP SYSTEM 'dtd/lisp.dtd'>
  <LISP xmlns:='-//mecomnet.de//DTD Lisp (sort of)//en'>
   <DEFPARAMETER> <NAME>testing</NAME><!-- a comment -->
    <VALUE>one two three</VALUE> </DEFPARAMETER>
   </LISP>")))))

(with-local-context (choose-file-default-directory )
  (inspect
   (list 13
         (read-xml-stream
          (encoded-stream "<?XML VERSION='1.0' ?>
  <!DOCTYPE LISP SYSTEM 'dtd/lisp.dtd'>
  <LISP xmlns:='-//mecomnet.de//DTD Lisp (sort of)//en'>
   <DEFPARAMETER> <NAME>testing</NAME><!-- a comment -->
    <VALUE>one two three</VALUE> </DEFPARAMETER>
   </LISP>")))))


;; more document type definitions

(inspect
(list 14
      (read-xml-stream
       (encoded-stream
"<?xml version='1.0' ?>
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
"))))



"XMLP"


