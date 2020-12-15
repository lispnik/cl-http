;;; -*- mode: LISP; package ("XML-PARSER")
;;;
;;; this version (C) mecom gmbh 24.11.97
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.

#|
 <FILE-DOCUMENTATION>
 <KEYWORDS> DOCTYPE element implementation </KEYWORDS>
 <DESCRIPTION>
 <P>
 a <CODE>DOCTYPE</CODE> instance binds parameters for the document type
 declaration (nb. NOT its definition - this is not the dtd)
 </DESCRIPTION>
 <CHRONOLOGY>
 </CHRONOLOGY>
 </FILE-DOCUMENTATION>
 |#
(in-package :XML-PARSER)

(defMethod node-class
           ((node (eql 'document-type-declaration)) (op t) (context t))
  *document-type-declaration-class*)

(defClass doctype (xml-named-node xml-external-node xml-comment-node)
  ((name
    :initarg :name
    :accessor doctype.name
    :type string)
   (url
    :initarg :url :initform nil
    :accessor doctype.url
    :type string
    :documentation
    "binds the location of the declaration itself.")
   (content
    :initarg :elements  :initform nil
    :accessor doctype.internal))
  (:documentation
   "a <CODE>DOCTYPE</CODE> instance collects a document type
    <EM>declaration</EM>, which in turn is used to locate the definition."))

(defMethod initialize-instance
           ((self doctype) &rest initargs &key name system (url system))
  (unless name
    (error "illegitimate doctype name: ~s." name))
  (unless url
    (setf url (format nil "internal://DTD/~a" name)))
  (apply #'call-next-method self
         :url url
         initargs))

(defMethod read-typed-markup-declaration
           ((name (eql 'xml::doctype)) (stream t)
            &aux (*parent-node* *parent-node*)
                 name args)
  (setf name (read-markup-tag-type stream))
  (setf *parent-node*
        (make-instance (node-class 'document-type-declaration name stream)
          :name name :url stream))
  (let ((*package* (make-markup-package name)))
    (setf args (read-markup-tag-parameters name stream)))

  (do ((arg (pop args) (pop args)))
      ((or (null arg) (eq arg *close-tag-section-marker*)))
    (cond
     ((eq arg 'xml::system)
      (setf (xml-node.system *parent-node*) (url (pop args))))
     ((eq arg 'xml::public)
      (setf (xml-node.system *parent-node*) (url (pop args))
            (xml-node.public-id *parent-node*) (pop args)))
     ((eq arg *open-tag-section-marker*)
      (setf (doctype.internal *parent-node*)
            (butlast args (length (member *close-tag-section-marker* args))))
      (setf args (member *close-tag-section-marker* args)))
     (t
      (xml-form-error *parent-node*
                      "illegitimate doctype elements: ~a." args)
      (setf args nil))))
  *parent-node*)


(defmethod print-object ((datum doctype) stream)
  (flet ((print-it (&aux (public-id (xml-node.public-id datum))
                         (system (xml-node.system datum))
                         (internal (doctype.internal datum)))
           (format stream "<!DOCTYPE ~a" (doctype.name datum))
           (if public-id
             (format stream " PUBLIC ~s ~s"
                     (url-namestring public-id datum)
                     (url-namestring system datum))
             (if system
               (format stream " SYSTEM ~s"
                       (url-namestring system datum))))
           (when internal
             (format stream " [~{~% ~a~} ]" internal))
           (format stream " >")))
    (if *xml-print-readably*
      (print-it)
      (print-unreadable-object (datum stream :identity *xml-verbose* :type t)
        (format stream "~A {~d}"
                (doctype.name datum)
                (length (doctype.internal datum)))))))


(defMethod document.dtd
           ((document t))
  nil)

(defMethod process-markup-element
           ((datum doctype) (stream t) &aux dtd)
  "when reading an XML-stream a DOCTYPE form is read at the outset.
   the parsed document declaration is merged into the document.
   the effect is that the specified internal DTD is processed and if an external
   is specified that is loaded as well"
  (xml-node.append-element *parent-node* datum)
  (when (typep (setf dtd (document.dtd *parent-node*)) 'dtd)
    (setf *dtd* dtd)
    (setf *package* (dtd.package dtd)))
  datum)


:EOF
