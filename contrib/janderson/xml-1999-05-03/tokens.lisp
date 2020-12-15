;;; -*- mode: LISP; package: "XML-PARSER"; -*-
;;;
;;; this version (C) mecom gmbh 24.10.98
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.

"
<DOCUMENTATION>
 <COPYRIGHT HREF='defsystem.lisp|root().descendant(1,COPYRIGHT)'/>
 <DESCRIPTION>
 </DESCRIPTION>
 <CHRONOLOGY>
  <DELTA><DATE>19971205</DATE>
   added markers. they are instances, and thus unique, which the reader and parser
   uses as special tags. they are now objects rather than uninterned symbols to
   distinguish them for dispatching - which begins to matter in malformed data.
   </DELTA>
  <DELTA><DATE>19981206</DATE>
   added type for model markers</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
"


(in-package "XML-PARSER")

(eval-when (:execute :load-toplevel :compile-toplevel)

(defClass xml-token-class (standard-class)
  ((name-map
    :initform (make-hash-table :test #'eq)
    :initarg :name-map
    :allocation :class
    :accessor xml-token-class.map
    :documentation "eq hashtable name -> map")
   (name-package
    :initform (find-package "xml")
    :initarg :name-package
    :allocation :class
    :accessor xml-token-class.package
    :documentation "the package for names of class instances"))
  (:documentation
   "a metaclass for tokens which maps a name -> a unique element.
     the name may be a symbol or a string. if a string it is mapped to an
     exported symbol in the package (xml-token-class.package <class>).
     symbols are coerced into that package."))

(defMethod xml-token-class-token-name
           ((class xml-token-class) (name string))
  (with-accessors ((package xml-token-class.package)) class
    (multiple-value-bind (new-name new?)
                         (intern name package)
      (when new? (export new-name package))
      new-name)))

(defMethod validate-superclass
           ((class xml-token-class) (superclass standard-class))
  t)

(defMethod xml-token-class-token-name
           ((class xml-token-class) (name symbol))
  (with-accessors ((package xml-token-class.package)) class
    (if (eq (symbol-package name) package)
      name
      (multiple-value-bind (new-name new?)
                           (intern (string name) package)
        (when new? (export new-name package))
        new-name))))

(defMethod xml-token-class-token-name
           ((class xml-token-class) (name null))
  (error "a name must be supplied for a token."))

(defMethod make-instance
           ((class xml-token-class) &rest initargs
            &key name 
            &allow-other-keys
            &aux instance)
  (setf name (xml-token-class-token-name class name))
  
  (with-accessors ((name-map xml-token-class.map)) class
    (cond ((gethash name name-map))
          (t
           (setf instance (apply #'call-next-method class
                                 :name name
                                 initargs))
           (setf (gethash name name-map) instance)
           instance))))

(defClass xml-marker ()
  ((name :initarg :name :reader xml-marker.name
         :type string))
  (:metaclass xml-token-class))
(defClass marker-tag (xml-marker)
  () 
  (:metaclass xml-token-class))
(defClass model-marker (xml-marker)
  ())
(defClass marker-sep (model-marker)
  ()
  (:metaclass xml-token-class))
(defClass marker-oc (model-marker)
  ()
  (:metaclass xml-token-class))

(defMethod print-object
           ((self xml-marker) stream)
  (declare (special *print-readably*))        ; this 'cause of load order
  (if *print-readably*
    (princ (xml-marker.name self) stream)
    (print-unreadable-object (self stream)
      (princ (xml-marker.name self) stream))))

(defun marker-tag (x) (make-instance 'marker-tag :name x))
(defun marker-sep (x) (make-instance 'marker-sep :name x))
(defun marker-oc (x) (make-instance 'marker-oc :name x))

) ; end eval-when

"XMLP"

