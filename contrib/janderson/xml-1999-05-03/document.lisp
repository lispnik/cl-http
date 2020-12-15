;;; -*- :mode: lisp; package: ("XML-PARSER")
;;;

"
<DOCUMENTATION>
 <DESCRIPTION>
  the 'document' container for xml data streams
  </DESCRIPTION>
 <COPYRIGHT HREF='defsystem.lisp|root().descendant(1,COPYRIGHT)'/>
 <CHRONOLOGY>
  <DELTA><DATE>19971210</DATE>documents now accept comments.
   </DELTA>
  <DELTA><DATE>19980312</DATE>
         <AUTHOR MAILTO='Hallvard.Traetteberg@idi.ntnu.no'>HT</AUTHOR>
   princ in print-object (document).
   </DELTA>
  <DELTA><DATE>19980622</DATE>
   added cache for document namespaces and included in print-object</DELTA>
  <DELTA><DATE>19980906</DATE>
   simplified comments to cache them in the document node only. this makes
   them document-specific, rather than globally associated with a node.
   </DELTA>
  <DELTA><DATE>19981024</DATE>
   removed direct binding for encoding, standalone, version and delegated them
   to the declaration node</DELTA>
  <DELTA><DATE>19981214</DATE>
   added document name to unreadble print form</DATE>
  <DELTA><DATE>19981223</DATE>
   generalized document.comments to document.dictionary, as it is also now used
   to store ID->node bindings.
   </DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
"

(in-package "XML-PARSER")


;;;
;;; node class definition

(defClass document (xml-external-node)
  ((xml-decl-pi
    :initarg :xml-decl-pi :initform nil
    :reader document.get-xml-decl-pi
    :writer (setf document.xml-decl-pi)
    :type (or null xml-decl-pi))
   (doctype
    :initarg :doctype :initform nil
    :writer (setf document.doctype) :reader document.get-doctype
    :documentation
    "bind the doctype element, including the content of the internal and,
     if present, the external dtd subsets.")
   (content
    :initarg :element :initform nil
    :accessor document.element
    :type (or null element)
    :documentation
    "bind the root element")
   (namespaces
    :initarg :namespaces :initform nil
    :accessor document.namespaces
    :type list)
   (dictionary
    :initform (make-hash-table)
    :accessor document.dictionary))
  (:documentation
   "an <CODE>document</CODE> instance binds the pi declaration from a
    document, the dtd, and the content. a system identifier may also be
    included.")
  (:metaclass qualified-class))

(defMethod document.xml-decl-pi
           ((document document))
  (with-slots (xml-decl-pi) document
    (cond (xml-decl-pi)
          (t
           (setf xml-decl-pi (make-instance 'xml-decl-pi)
                 (xml-node.context xml-decl-pi) document)
           xml-decl-pi))))
         

(defMethod document.doctype
           ((document document))
  (with-slots (doctype) document
    (cond (doctype doctype)
          (t
           (setf doctype (make-instance 'document-type
                           :name (gentemp "DOC-" *user-package*))
                 (xml-node.context doctype) document)
           doctype))))

;; delegated attributes

(defMethod document.standalone?
           ((instance document))
  (xml-decl-pi.standalone? (document.xml-decl-pi instance)))
(defMethod (setf document.standalone?)
           (datum (instance document))
  (setf (xml-decl-pi.standalone? (document.xml-decl-pi instance)) datum))

(defMethod document.encoding
           ((instance document))
  (xml-decl-pi.encoding (document.xml-decl-pi instance)))
(defMethod (setf document.encoding)
           (datum (instance document))
  (setf (xml-decl-pi.encoding (document.xml-decl-pi instance)) datum))

(defMethod document.validate?
           ((instance document))
  (xml-decl-pi.validate? (document.xml-decl-pi instance)))
(defMethod (setf document.validate?)
           (datum (instance document))
  (setf (xml-decl-pi.validate? (document.xml-decl-pi instance)) datum))

(defMethod document.version
           ((instance document))
  (xml-decl-pi.version (document.xml-decl-pi instance)))
(defMethod (setf document.version)
           (datum (instance document))
  (setf (xml-decl-pi.version (document.xml-decl-pi instance)) datum))

(defMethod document.valid?
           ((instance document) &aux (element (document.element instance)))
  (when element (element.valid? element)))


(defMethod initialize-instance :after
           ((self document)
            &key (validate? nil validate-p)
                 (version nil version-p)
                 (encoding nil encoding-p)
                 (standalone? nil standalone-p))
  (when validate-p (setf (document.validate? self) validate?))
  (when version-p (setf (document.version self) version))
  (when standalone-p (setf (document.standalone? self) standalone?))
  (when encoding-p (setf (document.encoding self) encoding))
  (setf (xml-node.assertion-time self) (get-universal-time)))

;;;
;;; parsing

(defMethod reduce-production
           ((document document)
            &key
            ((xml-1.0::xmldecl xml-decl))
            ((xml-1.0::doctypedecl doctype-decl))
            ((xml-1.0::element element)))
  (when xml-decl
    (unless (typep xml-decl 'xml-decl-pi)
      (xml-form-error document "erroneous xml decl: ~s." xml-decl))
    (xml-node.append-element document xml-decl))

  (cond (doctype-decl
         (unless (typep doctype-decl 'document-type)
           (xml-form-error document "erroneous doctype: ~s." doctype-decl))
         (xml-node.append-element document doctype-decl))
        (t
         (when (document.validate? document)
           (raise-xml-condition document
                                *validity-error-condition*
                                "no dtd present for validation"))))

  (cond ((typep element 'element)
         (xml-node.append-element document element))
        ((null element)
         (raise-xml-condition document
                              *root-element-error-condition*
                              "no root element."))
        (t
         (raise-xml-condition document
                              *root-element-error-condition*
                              "erroneous root element: ~s."
                              (document.element document))))

  document)


(defMethod read-production
           ((production (eql 'xml-1.0::document)) (stream t)
            &aux 
            *document*
            (*parent-node* *parent-node*)
            (*document-type* *document-type*)
            (*namespaces* *namespaces*)
            (*preserve-whitespace* *preserve-whitespace-default*)
            (*markup-stream* stream)
            (*processed-node* nil)
            (*comment-target* *comment-target*)
            next-node
            element xml-decl-pi doctype-decl)
  ;; a few special issues:
  ;; - in order to have the document context available during the parse
  ;; it is generated and bound "ahead of time".
  ;; - the body is simple read until EOF; content constraints are applied
  ;; when the form is reduced.
  ;; - a separate handler is asserted for other errors - to augment the
  ;; message for the document context. it runs in the dynamic context and, as
  ;; such requires its own form.
  (handler-bind
    ((error #'(lambda (condition)
                (xml-warn production
                          "xml-decl: ~s, doctype-decl: ~s, element: ~s."
                          xml-decl-pi doctype-decl element)
                condition)))
    (setf *document* (make-instance 'document
                       :validate? *validate*))
    (setf *parent-node* *document*)

    (handler-case
      (loop
        (setf next-node (read-production 'xml-1.0::documentMarkup stream))
        (typecase next-node
          (xml-decl-pi
           (when xml-decl-pi
             (raise-xml-condition production
                                  *form-error-condition*
                                  "duplicate xml declaration: ~s."
                                  next-node))
           (setf xml-decl-pi next-node)
           ;; carry over the standalone value immediately. this is necessary
           ;; in order the the document-type can observe it
           ;; in order to ensure that the external subset is read
           (setf (document.standalone? *document*)
                 (xml-decl-pi.standalone? next-node)))

          (document-type
           (xml-node.append-element *document* next-node))

          (element
           (when element
             (raise-xml-condition production
                                  *form-error-condition*
                                  "duplicate root element: ~s."
                                  next-node))
           (setf element next-node))

          (comment
           ;; comments are cached and otherwise ignored
           (xml-node.append-element *document* next-node))

          (processing-instruction
           ;; processing instructions are ignored - they don't appear in the
           ;; document tree
           )

          (t
           (xml-form-error production
                             "erroneous document content: ~s."
                             next-node))))
      (end-of-file (condition) (declare (ignore condition))))
    (reduce-production *document*
                       'xml-1.0::xmldecl xml-decl-pi
                       'xml-1.0::doctypedecl doctype-decl
                       'xml-1.0::element element)))


;;;
;;; printing

(defMethod xml-node.string
           ((document document))
  (xml-node.string (document.element document)))

(defMethod print-object
           ((datum document) (stream t))
  (with-slots (xml-decl-pi doctype namespaces system-id public-id) datum
    (cond (*print-readably*
           (format stream "~a~%" xml-decl-pi)
           (format stream "~@[~a~%~]" doctype)
           (when *parse-verbose*
             (format stream "<!-- generated by cl-xml (v.~a) -->~%"
                     *processor-version*))
           (write-string  "<!-- " stream)
           (write-external-id-parameters system-id public-id stream) 
           (format stream "~% namespaces:~%")
           (format stream "~{ ~a~%~}" namespaces)
           (write-string  " -->" stream)
           (write-char #\newline stream)
           (princ (document.element datum) stream))
          (t
           (print-unreadable-object (datum stream :identity *parse-verbose* :type t)
             (format stream "~a ~s"
                     (when doctype (document-type.name doctype))
                     xml-decl-pi))))))

"<H3>DOM operations</H3>
<P>
the document tree is constructed as the final step in reading an xml document.
the datum instance which was constructed for each given encoded form is passed
as an argument to PROCESS-ELEMENT. the respective methods for grove entities
begin with the most recent entity, and search up through the document tree to
locate the correct context entity, and incorporate the parsed entity node as
appropriate. in most instances, this means appending the new node to an
existing collection of child nodes..
</P>
<P>
comments are cached <EM>w.r.t. a document</EM> for specified nodes.
this is done during parsing whenever a comment is read. when the document is
serialized, the comments are wrtten after the respective element.
</P>
<P>
ID -> node bindings are also document-relative. since the domain is distinct
from the key for comment -> node pairing, it is stored in the same place.
"


(defMethod document.node-comments
           ((document document) (node t))
  (gethash node (document.dictionary document)))

(defMethod (setf document.node-comments)
           (comments (document document) (node t))
  (setf (gethash node (document.dictionary document)) comments))

(defMethod document.id-element
           ((document document) (id t))
  (gethash id (document.dictionary document)))

(defMethod (setf document.id-element)
           (comments (document document) (id t))
  (setf (gethash id (document.dictionary document)) comments))

(defMethod document.element
           ((node t))
  nil)


#|
(inspect (make-instance 'document))
(inspect (make-instance 'document
           :version "1.1" :encoding "ISO123" :standalone? t :validate? t
           :system-id "//asdf.qwer/one/two"))
           
|#

"XMLP"
