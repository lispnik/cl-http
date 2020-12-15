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
 <DD>the <CODE>WITH-XML-DOCUMENT</CODE> macro is provided analogous to
  the html macro.
  <BR>
  the <CODE>WITH-XML-ELEMENT</CODE> is just a generic tag-wrapping function
  with support for attribute printing.
  <BR>
  note that these are really here for illustration purposes only.
  the parser adopts o-o interface. the instances which it constructs perform
  all necessary syntactic rearranging and formatting and my be printed to
  a directly a stream in the place of procedural generation through
  WITH-XML-DOCUMENT etal.
  </DD>
 <DT>determining client capabilities</DT>
 <DD><CODE>HEADER-ACCEPT-P</CODE> provides a means to determine if the
  client can accept a particular mime type. 
  </DD>
 </DL>
 <P>
 see "example-server.lisp" for examples.
 </DESCRIPTION>
 </FILE-DOCUMENTATION>
 |#

(in-package :HTTP)

(import '(xml-parser::*xml-version*))
(export '(HEADER-ACCEPT-P
         DECLARE-XML-VERSION
         WITH-XML-DOCUMENT))

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

(defun declare-xml-version (&optional (stream html:*output-stream*))
  "Declares the document type as the current XML generation.
   The DTD is handled separately, since it depends on the data."
  (html2::%issue-command ("?XML" stream :fresh-line t :trailing-line t)
    (http::fast-format stream " VERSION=~S ?" *xml-version*)))

(defMacro with-xml-element ((type &key (stream '*output-stream*) attributes)
                             &body body
                             &aux (type-sym (gensym))
                                  (attribute-sym (gensym)))
  `(let ((,type-sym (string ,type))
         (,attribute-sym ,attributes))
     (html2::issue-command* ,type-sym ,attribute-sym ,stream)
     ,@body
     (html2::issue-command ,type-sym ,stream nil t)))

(defmacro with-xml-document ((doctype &key (stream '*output-stream*)
                                           system public
                                           attributes)
                             &body body
                             &aux (doctype-sym (gensym)))
  "Asserts the contents of BODY is an XML document."
  `(let ((,doctype-sym ,doctype))
     (declare-xml-version ,stream)
     (html2::%issue-command ("!DOCTYPE" ,stream
                             :fresh-line t :trailing-line t)
       (http::fast-format ,stream " ~a" ,doctype-sym)
       ,@(if public
           `((let ((public ,public)
                   (system ,system))
               (format stream " PUBLIC \"~a\" ~[\"\";\"~a\"~]" public system)))
           (if system
             ((let ((system ,system))
                (format stream " SYSTEM \"~a\"~]" system))))))
     (with-xml-element (,doctype-sym :stream stream :attributes ,attributes)
       ,@body)
     (fresh-line ,stream)))


:EOF
