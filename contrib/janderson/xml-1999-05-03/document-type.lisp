;;; -*- mode: LISP; package: "XML-PARSER"; -*-
;;;

"
<DOCUMENTATION>
 <KEYWORDS> document-type element implementation </KEYWORDS>
 <DESCRIPTION>
 <P>
 a <CODE>document-type</CODE> instance binds parameters for the document type
 declaration (nb. NOT its definition - this is not the dtd)</P>
 </DESCRIPTION>
 <COPYRIGHT HREF='defsystem.lisp|root().descendant(1,COPYRIGHT)'/>
 <CHRONOLOGY>
  <DELTA><DATE>19980808</DATE>
   a DTD does not bind a single namespace. the internal and externa subsets
   are read in the combined scope of one of several possible processing
   instructions:
   <UL>
    <LI>a namespace pi which appears in the prologue of the document entity
     extends over both the external and the internal subsets</LI>
    <LI>a pi which appears within the internal subset affects the internal
     subset and the external subset, but tot the remainder of the document
     entity</LI>
    <LI>a pi which appears in the external subset extends over that external
     entity only.</LI>
    </UL>
   </DELTA>
  <DELTA><DATE>19981218</DATE>
   supprt reinitialization in the document type cache. in particular, if the
   system id changes, then the cached external subset is to be discarded.
   </DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
 "

(in-package "XML-PARSER")


;;;
;;; a metaclass which runs the instance cache

(defClass document-type-class (keyword-qualified-class)
  ((instances
    :initform nil
    :reader document-type.instances)))

(defMethod validate-superclass
           ((class document-type-class) (superclass keyword-qualified-class))
  t)

(defMethod make-instance
           ((class document-type-class) &rest initargs &key name &aux instance)
  (with-slots (instances) class
    (cond ((and name
                (setf instance (find name instances :key #'document-type.name)))
           (apply #'reinitialize-instance instance initargs)
           instance)
          (t
           (setf instance (call-next-method))
           (when name (push instance instances))
           instance))))

;;;
;;; the document type class itself

(defClass document-type (xml-named-node
                         xml-external-node
                         comment-target
                         document-markup)
  ((name
    :initarg :name
    :accessor document-type.name
    :type string)
   (content
    :initarg :internal-subset  :initform nil
    :accessor document-type.internal-subset)
   (external
    :initarg :external-subset  :initform nil
    :reader document-type.external-subset))
  (:documentation
   "a <CODE>document-type</CODE> instance collects a the components of a
    document type <EM>declaration</EM>.
    note that specializations should observe the metaclass if they are
    to be cached.")
  (:metaclass document-type-class))

(defMethod document-type-augment-subset
           ((document-type document-type) (node xml-node))
  (with-slots (content external) document-type
    (if *in-external-subset*
      (setf external (nconc external (list node)))
      (setf content (nconc content (list node)))))
  (setf (xml-node.context node) document-type)
  node)


  
(defMethod shared-initialize :before
           ((self document-type) (slots t)
            &key ((:system-id new-system-id)))
  (with-slots (system-id external) self
    (with-accessors ((assertion-time xml-node.assertion-time)
                     (modification-time xml-node.modification-time)) self
    (when (and (slot-boundp self 'system-id)
               (not (string-equal system-id new-system-id)))
      (setf external nil
            modification-time nil
            assertion-time nil)))))

(defMethod (setf document-type.external-subset)
           (subset (document-type document-type))
  (with-slots (external assertion-time modification-time) document-type
    (setf external subset)
    (setf assertion-time (get-universal-time))
    (unless subset (setf modification-time nil))))

(defMethod document.validate?
           ((datum document-type))
  (document.validate? (xml-node.context datum)))


;;;
;;; maintain a global instance for use when reading declarations for
;;; test purposes.
(setf *default-document-type*
      (make-instance 'document-type :name nil))

(defun default-document-type
       (&key (name (document-type.name *default-document-type*)))
  (setf (document-type.name *default-document-type*) name
        (document-type.internal-subset *default-document-type*) nil
        (document-type.external-subset *default-document-type*) nil)
  *default-document-type*)

(defun in-dtd?
       ()
  *document-type*)
  
;;;
;;; parse implementation

(defMethod reduce-production
           ((node document-type) &key)
  (setf *processed-node* node))

(defMethod reduce-production
           ((production (eql 'xml-1.0::doctypedecl))
            &key
            ((xml-1.0::Name name))
            ((xml-1.0::PubidLiteral public-id))
            ((xml-1.0::SystemLiteral system-id))
            ((xml-1.0::markupdecl declarations)))
  (reduce-production
   (make-instance 'document-type
       :name name :internal-subset declarations
       :public-id public-id :system-id system-id)))

(defMethod production-reader-macro
           ((production (eql 'xml-1.0::doctypedecl)) (stream t)
            &aux name system-id public-id markupdecl)
  (handler-bind ;; augment error message for context
    ((error #'(lambda (condition)
                (xml-warn production
                          "Name: ~s, SystemLiteral: ~s, SystemPubid: ~s, markupdecl: ~s."
                          name system-id public-id markupdecl)
                condition)))
    (with-namespace-frame
      (multiple-value-setq (name system-id public-id markupdecl)
        (read-entity-attributes production stream))
      (reduce-production production
                         'xml-1.0::Name name
                         'xml-1.0::PubidLiteral public-id
                         'xml-1.0::SystemLiteral system-id
                         'xml-1.0::markupdecl markupdecl))))



;;; model operations
;;;

(defun document-type-instances
       ()
  (document-type.instances (find-class 'document-type)))

(defMethod document-type.instances
           ((instance document-type))
  (document-type.instances (class-of instance)))

(defun element-declaration-instances
       (&aux list)
  (dolist (document-type (document-type-instances))
    (setf list (union list (document-type.element-declarations document-type))))
  list)

(defun document-type-declaration-collector
       (document-type type &aux list)
  (with-slots (content external) document-type
    (flet ((get-decl (datum) (when (typep datum type) (push datum list))))
      (map nil #'get-decl content)
      (map nil #'get-decl external)
      (nreverse list))))

(defMethod document-type.element-declarations
           ((self document-type))
  (document-type-declaration-collector self 'element-declaration))

(defMethod document-type.notation-declarations
           ((self document-type))
  (document-type-declaration-collector self 'notation-declaration))

(defMethod document-type.general-entity-declarations
           ((self document-type))
  (document-type-declaration-collector self 'general-entity-declaration))

(defMethod document-type.parameter-entity-declarations
           ((self document-type))
  (document-type-declaration-collector self 'parameter-entity-declaration))


;;;
;;; retrieving external content


(defMethod get-external-subset-content
           ((*document-type* document-type)
            &aux location duplicate-content)
  (declare (ignorable duplicate-content))
  (with-accessors ((modification-time xml-node.modification-time)
                   (public-id xml-node.public-id)
                   (system-id xml-node.system-id)
                   (external document-type.external-subset))
                  *document-type*
    (when (setf location (or (translate-public-id public-id)
                             (translate-system-id system-id)))
      (setf external nil)
      (multiple-value-setq (duplicate-content modification-time)
        (read-production 'xml-1.0::ExtSubset location))
      external)))



;;;
;;; printing

(defmethod print-object ((datum document-type) stream)
  (with-accessors ((public-id xml-node.public-id)
                   (system-id xml-node.system-id)
                   (internal document-type.internal-subset)
                   (external document-type.external-subset)
                   (name document-type.name))
                  datum
    (flet ((print-it ()
             (write-string *markup-declaration-open* stream)
             (write-string "DOCTYPE " stream)
             (write-identifier name stream)
             (write-char #\space stream)
             (write-external-id-parameters system-id public-id stream)
             (when internal
               (format stream " ~a~{~% ~a~}~% ~a"
                       *declaration-section-open* internal
                       *declaration-section-close*))
             (write-string *tag-close* stream)))
      (if *print-readably*
        (with-namespace-frame (print-it))
        (print-unreadable-object (datum stream :identity *parse-verbose* :type t)
          (format stream "~A {~d/~d}"
                  (document-type.name datum)
                  (length internal) (length external)))))))



#|
(read-production
 'XML-1.0::doctypedecl
 (markup-stream "asdf PUBLIC '-/ssss/ssss'  SYSTEM 'ww/ss/xx'>"))

|#

"XMLP"
