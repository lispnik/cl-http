;;; -*- mode: lisp; package "HTTP-USER" -*-

#|
 <FILE-DOCUMENTATION>
 <DESCRIPTION>
 this file contains the bare-minimum support for generating xml documents
 within the cl-http server.
 this includes:
 <DL>
 <DT>mime types</DT>
 <DD>an additional mime type for xml data added to the known url types
  </DD>
 <DT>document generation</DT>
 <DD>the <CODE>WITH-document</CODE> macro is provided analogous to
  the html macro.
  <BR>
  the <CODE>WITH-element</CODE> is just a generic tag-wrapping function
  with support for attribute printing.
  <BR>
  note that these are really here for illustration purposes only.
  the parser adopts o-o interface. the instances which it constructs perform
  all necessary syntactic rearranging and formatting and my be printed to
  a directly a stream in the place of procedural generation through
  WITH-document etal.
  </DD>
 <DT>determining client capabilities</DT>
 <DD><CODE>HEADER-ACCEPT-P</CODE> provides a means to determine if the
  client can accept a particular mime type. 
  </DD>
 </DL>
 <P>
 see "example-server.lisp" for examples.
 </DESCRIPTION>
 <CHRONOLOGY>
  <DELTA><DATE>19980312</DATE>
         <AUTHOR MAILTO="Hallvard.Traetteberg@idi.ntnu.no">HT</AUTHOR>
   back-quote problems in WITH-document
   </DELTA>
  <DELTA><DATE>19980317</DATE>
   added header-cond to permit accept-specific forms as is a cond
   </DELTA>
  <DELTA DATE='19980325' >
   exported with-* macros</DELTA>
  <DELTA DATE='19990208' >
   WITH-DOCUMENT -> WITH-XML-DOCUMENT;
   fixed case in WITH-XML-DOCUMENT</DELTA>
  <DELTA DATE='19990302'>
   with-xml-document, with-external-entity, declare-xml* to xml-1.0 package.
   </DELTA>
  </CHRONOLOGY>
 </FILE-DOCUMENTATION>
 |#

(in-package :HTTP)

(import '(xml-parser::*xml-version*))
(export '(HEADER-ACCEPT-P
          DECLARE-XML-VERSION))

(http::define-url-export-types
  (:xml-file :xml (:text :plain :charset :iso-8859-1)
             :data-type :text))

(defMacro header-accept-p (mime-type &optional exact-p)
  `(let* ((accept-header (http::get-header-object :accept))
          (mime-types nil)
          (test-type ',(if (consp mime-type) (first mime-type) mime-type))
          (sub-type ',(if (consp mime-type) (second mime-type) :*)))
     (when accept-header
       (dolist (raw-value (http::header-raw-value accept-header))
         (dolist (value
                  (http::parse-mime-content-type-sequence-header raw-value))
           (push value mime-types))))
     (setf mime-types (nreverse mime-types))
     
     (find-if #'(lambda (accept-type)
                  (and (or ,(unless exact-p '(eq (first accept-type) :*))
                           (eq (first accept-type) test-type))
                       (or ,(unless exact-p '(eq (second accept-type) :*))
                           (eq sub-type :*)
                           (eq (second accept-type) sub-type))))
              mime-types)))


(defMacro header-cond (&rest mime-clauses)
  (let ((t-clause (find t mime-clauses :key #'first)))
    (setf mime-clauses (remove t-clause mime-clauses))
    `(block mime-loop
       (let* ((accept-header (http::get-header-object :accept)))
         (flet ((test-mime-type (mime-type &aux (test-type (first mime-type))
                                                (sub-type (second mime-type)))
                  ,@(mapcar
                     #'(lambda (clause &aux (accept-type (first clause)))
                         `(when (and (or ;; ,(unless exact-p '(eq ,(first accept-type) :*))
                                         (eq ,(first accept-type) test-type))
                                     (or ;; ,(unless exact-p '(eq ,(second accept-type) :*))
                                         (eq sub-type :*)
                                         (eq ,(second accept-type) sub-type)))
                            (return-from mime-loop (progn ,@(rest clause)))))
                     mime-clauses)))
           (when accept-header
             (dolist (raw-value (http::header-raw-value accept-header))
               (dolist (value
                        (http::parse-mime-content-type-sequence-header raw-value))
                 (test-mime-type value))))
           ,@(rest t-clause))))))



(defun xml-1.0::declare-xml-version (&key (stream *output-stream*) version encoding standalone)
  "Declares the document form as the current XML generation."
  (html2::%issue-command ("?xml" stream :fresh-line t :trailing-line t)
    (when version
      (http::fast-format stream " version='~a'" version))
    (when encoding
      (http::fast-format stream " encoding='~a'" encoding))
    (when standalone
      (http::fast-format stream " standalone='~a'" standalone))
    (write-string " ?" stream)))

(defun xml-1.0:declare-xml-encoding (&key (stream *output-stream*) version encoding)
  "Declares the document form as the current XML generation."
  (html2::%issue-command ("?xml" stream :fresh-line t :trailing-line t)
    (when version
      (http::fast-format stream " version='~a'" version))
    (when encoding
      (http::fast-format stream " encoding='~a'" encoding))
    (write-string " ?" stream)))

(defMacro xml-1.0:with-element ((type &key (stream '*output-stream*) attributes)
                             &body body
                             &aux (type-sym (gensym))
                                  (attribute-sym (gensym)))
  `(let ((,type-sym (string ,type))
         (,attribute-sym ,attributes))
     (html2::issue-command* ,type-sym ,attribute-sym ,stream)
     ,@body
     (html2::issue-command ,type-sym ,stream nil t)))

(defmacro xml-1.0:with-xml-document ((doctype &key (stream '*output-stream*)
                                           system public
                                           attributes
                                           (version *xml-version*)
                                           encoding
                                           standalone
                                           (root-element t)
                                           internal-subset)
                             &body body
                             &aux (doctype-sym (gensym)))
  "Asserts the contents of BODY the content of the root element in an XML document."
  `(let ((,doctype-sym ,doctype))
     (xml-1.0:declare-xml-version :stream ,stream :encoding ,encoding :version ,version
                          :standalone ,standalone)
     (html2::%issue-command ("!DOCTYPE" ,stream
                             :fresh-line t :trailing-line t)
       (http::fast-format ,stream " ~a" ,doctype-sym)
       ,@(when (or public system)
          `((xmlp::write-external-id-parameters ,system ,public ,stream)))
       ,@(cond ((or (stringp internal-subset)
                    (and (symbolp internal-subset) internal-subset))
                `((write-string ,internal-subset ,stream)))
               (internal-subset
                `((funcall ,internal-subset ,stream))))
       )
     ,(if (or root-element attributes)
        `(xml-1.0:with-element (,(if (and root-element (not (eq root-element t)))
                          root-element doctype)
                        :stream ,stream :attributes ,attributes)
          ,@body)
        `(progn ,@body))
     (fresh-line ,stream)))

(defmacro xml-1.0:with-external-entity (( &key (stream '*output-stream*)
                                             (version *xml-version*)
                                             encoding)
                             &body body)
  `(progn
     (xml-1.0:declare-xml-encoding :stream ,stream :encoding ,encoding :version ,version)
     ,@body
     (fresh-line ,stream)))


:EOF
