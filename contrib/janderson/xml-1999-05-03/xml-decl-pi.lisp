;;; -*- package: "XML-PARSER"; -*-

"
<DOCUMENTATION>
 <DESCRIPTION>
  document-specific attributes are specified in the document declaration. this
  is included as a processing instruction with the operator 'xml'
  
  the only processing is to bind it to the document instance
  </DESCRIPTION>
 <COPYRIGHT HREF='defsystem.lisp|root().descendant(1,COPYRIGHT)'/>
 <CHRONOLOGY>
  <DELTA><DATE>19971210</DATE>
   <CODE>xml-decl-pi</CODE>modified for 'standalone' rather
   than 'required markup' - value range (:yes :no nil)
  <DELTA><DATE>19981024</DATE>
   separated general pi definitions from those for xml declaration</DELTA>
  <DELTA><DATE>19981114</DATE>
   split read function into parse/reduce phases;
   augmented error reporting</DELTA>
  <DELTA><DATE>19981218</DATE>
   xml-decl-pi.standalone? now (member t nil)</DATE>
  </CHRONOLOGY>
 </DOCUMENTATION>
"

(in-package "XML-PARSER")

;;;
;;;

(defClass xml-decl-pi (processing-instruction
                       comment-target)
  ((version
    :initarg :version :initform *xml-version*
    :reader xml-decl-pi.version
    :type number)
   (encoding
    :initarg :encoding :initform *encoding*
    :accessor xml-decl-pi.encoding
    :type (or null string symbol))
   (standalone
    :initarg :standalone? :initform *standalone*
    :reader xml-decl-pi.standalone?
    :type (member t nil))
  (validate
    :initarg :validate? :initform *validate*
    :accessor xml-decl-pi.validate?
    :type (member t nil)))
  (:metaclass qualified-class))

(defMethod (setf xml-decl-pi.standalone?)
           ((value (eql t)) (node xml-decl-pi))
  (setf (slot-value node 'standalone) value))

(defMethod (setf xml-decl-pi.standalone?)
           ((value null) (node xml-decl-pi))
  (setf (slot-value node 'standalone) value))

(defMethod (setf xml-decl-pi.standalone?)
           ((value t) (node xml-decl-pi))
  (flet ((standalone-error ()
           (raise-xml-condition 'xml-1.0::standalone 'form-error
                                "erroneous standalone value: ~s."
                                value)))
    (typecase value
      ((or string symbol)
       (cond ((string-equal "yes" value)
              (setf (xml-decl-pi.standalone? node) t))
             ((string-equal "no" value)
              (setf (xml-decl-pi.standalone? node) nil))
             (t
              (standalone-error))))
      ((standalone-error))))
  value)

(defMethod (setf xml-decl-pi.version)
           (version (node xml-decl-pi))
  (flet ((version-error ()
           (raise-xml-condition 'xml-1.0::version 'form-error
                                "erroneous version: ~s."
                                version)
           *xml-version*))
    (setf (slot-value node 'version)
          (typecase version
            (number version)
            ((or string symbol)
             (let ((parsed-version (read-from-string (string version))))
               (if (numberp parsed-version)
                 parsed-version
                 (version-error))))
            (t
             (version-error))))))


;;;
;;; parsing

(defMethod reduce-production
           ((xml-decl xml-decl-pi) &key)
  (call-next-method)
  (setf *processed-node* xml-decl))

(defMethod reduce-production
           ((production (eql 'XML-1.0::XMLDecl))
            &key
            ((XML-1.0::EncodingDecl encoding))
            ((XML-1.0::VersionInfo version))
            ((XML-1.0::SDDecl standalone))
            &aux node)
  (handler-bind ;; just augment error message for context
    ((error #'(lambda (condition)
                (xml-warn production
                          "encoding: ~s, version: ~s, standalone: ~s."
                          encoding version standalone)
                condition)))
    (setf node (make-instance 'xml-decl-pi))
    (when version
      (setf (xml-decl-pi.version node) version))
    (when standalone
      (setf (xml-decl-pi.standalone? node) standalone))
    (when encoding
      (setf (xml-decl-pi.encoding node) encoding)))
  (reduce-production node))
  

(defMethod production-reader-macro
           ((production (eql 'XML-1.0::XMLDecl)) (stream t)
            &aux version encoding standalone)
  (handler-bind ;; just augment error message for context
    ((error #'(lambda (condition)
                (xml-warn production
                          "encoding: ~s, standalone: ~s, version: ~s."
                          encoding standalone version)
                condition)))
    (multiple-value-setq (encoding version standalone)
      (read-entity-attributes production stream))
    (reduce-production production
                       'XML-1.0::EncodingDecl encoding
                       'XML-1.0::VersionInfo version
                       'XML-1.0::SDDecl standalone)))


;;;
;;; printing

(defMethod print-object
           ((datum xml-decl-pi) (stream t))
  (flet ((format-content (&aux (standalone (xml-decl-pi.standalone? datum)))
           (unless (typep (xml-node.context datum) 'document-type)
             (format stream
                     " ~@[version='~a' ~]~@[standalone='~a' ~]"
                     (xml-decl-pi.version datum)
                     (if (or (eq standalone nil) (string-equal standalone "no"))
                       "no" "yes")))
           (format stream
                   " ~@[encoding='~a' ~]"
                   (xml-decl-pi.encoding datum))))
    (cond (*print-readably*
            (format stream "~A?XML" *start-tag-open*)
            (format-content)
            (format stream " ?~A" *tag-close*))
          (t
           (print-unreadable-object (datum stream :type t)
             (format-content))))))




#|
(inspect (make-instance 'xml-decl-pi))
(inspect (make-instance 'xml-decl-pi
           :encoding "ISO1234" :version "1.1" :standalone? t :validate? t))


(read-production 'xml-1.0::markupdecl
                 (encoded-stream "<?xml version='1.0'  standalone='yes'?>"))

(read-production 'xml-1.0::markupdecl
                 (encoded-stream "<?xml version='1.0' standalone='no' encoding='qwer-qwe'?>"))
|#

"XMLP"


