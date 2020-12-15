;;; -*- mode: LISP; package ("XML-PARSER")
;;;
;;; this version (C) mecom gmbh 24.11.97
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.

#|
 <FILE-DOCUMENTATION>
 <KEYWORDS> dtd </KEYWORDS>
 <DESCRIPTION>
 <P>
 a stream parser for document type definitions requires only a sequence of
 declarations. these can be elements or entities.
 if a document type <em>declaration</em> is present, then it asserts the
 document type, otherwise (particularly for external subsets) the type
 declaration will already have been provided in the refering document.
 </DESCRIPTION>
 <CHRONOLOGY>
 <DATE>19971210</DATE>
  <DELTA>changed the interface to permit the dtd to be specified as a parameter
   </DELTA>
 <DATE>19971218</DATE>
  <DELTA>eliminated the DTD-DOCUMENT. it serves no purpose, since a dtd
   cannot stand alone. it is read (normally) as a dependant on an xml document
   where the dtd name and the effective root element is determined. as such,
   in order to read the dtd stream, the dtd itself must have already been
   created in order to accept the internal subset and the name. so it's now
   passed as a parameter as the standard call. a name is supported in addition
   as a convenience interface.
   </DELTA>
 </CHRONOLOGY>
 </FILE-DOCUMENTATION>
 |#
(in-package :XML-PARSER)

#|
[79]  ExternalPE ::= TextDecl extSubset
[31]  extSubset  ::=  ( markupdecl | conditionalSect | PEReference | S )*
 |#

(defMethod read-dtd-stream
           ((*markup-stream* concatenated-stream) (*dtd* dtd))
  (let* ((*document* *dtd*)
         (*parent-node* *dtd*)
         (*package* (dtd.package *dtd*))
         (*xml-preserve-whitespace* nil)
         (eof '#.(gensym)))
    (unless (peek-char *tag-open-char* *markup-stream* nil)
      (xml-form-error *markup-stream* "document contains no elements."))

    (handler-bind ;; handle errors other than file errors
      ((stream-error #'(lambda (condition) condition))
       (error #'(lambda (condition)
                  (warn "error while reading dtd from ~s:~%~a"
                        (parse-position *markup-stream*) condition)
                  (when *xml-handle-errors*
                    (return-from read-dtd-stream nil))
                  condition)))
      (loop (when (eq eof
                      (read-process-entity-data *markup-stream* nil nil eof))
              (return))))
      

    (cond ((not (typep *dtd* *document-type-definition-class*))
           (xml-form-error *document* "illegitimate dtd: ~s." *dtd*)))
    *dtd*))

(defMethod read-dtd-stream
           ((stream stream) (dtd dtd))
  (read-dtd-stream (markup-stream stream) dtd))

(defMethod read-dtd-stream
           ((source pathname) (dtd t))
  (with-open-file (stream source :direction :input)
    (read-dtd-stream stream dtd)))

(defMethod read-dtd-stream
           ((source string) (dtd t))
  (with-input-from-string (stream source)
    (read-dtd-stream stream dtd)))

(defMethod read-dtd-stream
           ((source stream) (dtd t) &aux read-dtd)
  ;; if no dtd instance is provided, create or locate it and then read the
  ;; content. if the read fails, then delete the dtd since the content is
  ;; likely inconsistent
  (setf dtd
        (make-instance (node-class 'document-type-definition source nil)
          :name (string dtd)
          :url source
          ;; since we're reading the dtd here, reset existing content
          :elements nil
          :entities nil
          :notations nil
          :reinitialize t))
  (unwind-protect
    ;; try to load the dtd. use the binding in the document to check if the
    ;; load succeeded.
    ;; if it succeeds, return the result otherwise delete the result.
    (setf read-dtd (read-dtd-stream source dtd))
    (unless (typep read-dtd 'dtd)
      (warn "deleting incomplete DTD: ~s." dtd)
      (delete-dtd dtd))))

(defMethod read-dtd-stream
           ((source t) (dtd null))
  (read-dtd-stream source (gentemp "DTD-")))
  

(set-markup-dispatch-macro #\!
                           #'(lambda (stream &aux name)
                               (when (alpha-char-p (peek-char t stream))
                                 (setf name (read stream)))
                               (read-dtd-stream 
                                (if (eql #\" (peek-char nil stream))
                                  (read stream t t t) stream)
                                name))
                           t)



(defMethod load-dtd
           (name (source t))
  (read-dtd-stream source name))

(defMethod load-dtd
           (name (sources list) &aux result)
  (flet ((%load-dtd (source)
           (handler-case (load-dtd name source)
             (error (condition)
                    (warn "unable to load the dtd: ~s: ~a"
                          source condition)
                    nil))))
    (dolist (source sources)
      (when (typep (setf result (%load-dtd source)) 'dtd)
        (return result)))))



:EOF


