;;; -*- package: ("XML-PARSER"); -*-
;;;
;;; this version (C) mecom gmbh 19981024
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.

"
<DOCUMENTATION>
 <DESCRIPTION>
  implements encoded streams and entity input streams
  </DESCRIPTION>
 <CHRONOLOGY>
  <DELTA><DATE>19981024</DATE>
   extracted from markup-reader</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
 "

(in-package "XML-PARSER")

(defClass stream-state ()
  ((source :initarg :source :initform "?"
           :reader stream-state.source
           :type (or named-entity-declaration stream))
   (element :initarg :element :initform *processed-node*
            :reader stream-state.element
            :type (or null xml-node))
   (offset :initarg :offset :initform 0
           :reader stream-state.offset
           :type integer)))
#+:ccl
(defClass entity-string-stream (ccl::string-input-stream)
  ((entity :initarg :entity :initform nil
           :reader stream.entity))
  (:metaclass qualified-class))
#+:allegro
(defClass entity-string-stream (excl::string-input-stream)
  ((entity :initarg :entity :initform nil
           :reader stream.entity))
  (:metaclass qualified-class))
#-(or ccl allegro)
(error "no definition for entity-string-stream is possible")

(defMethod initialize-instance
           ((instance entity-string-stream)
            &rest initargs
            &key entity (string (xml-node.string entity)))
  (apply #'call-next-method instance
         :string string
         initargs))

(defMethod print-object
           ((datum stream-state) stream)
  (print-unreadable-object (datum stream)
    (format stream "~s @ ~s (after ~s)"
            (stream-state.source datum) (stream-state.offset datum)
            (stream-state.element datum))))

;;; see also "entity-declaration.lisp" for the method which prepends an entity to
;;; the stream


(defGeneric stream-position (stream &optional where)
  (:method ((stream string-stream) &optional where)
           (declare (ignore where))
           #+:CCL (slot-value stream 'ccl::index)
           #+:ALLEGRO (slot-value stream 'excl::charpos)
           #-(or CCL ALLEGRO) 0)
  (:method ((source function) &optional where)
           (declare (ignore where))
           nil)
  (:method ((stream t) &optional where)
           (if where
             #+:CCL (ccl::stream-position stream where)
             #+:ALLEGRO (excl::stream-file-position stream where)
             #+:CCL (ccl::stream-position stream)
             #+:ALLEGRO (excl::stream-file-position stream)))
  (:method ((stream null) &optional where)
           (declare (ignore where))
           -1)
  #+:ALLEGRO
  (:method ((stream file-stream) &optional where)
           (declare (ignore where))
           (file-position stream)))



(defGeneric stream-state
  (stream)
  (:method ((stream t)) (make-instance 'stream-state))
  (:method ((stream file-stream))
            (make-instance 'stream-state
              :source (namestring stream) :offset (stream-position stream)))
  (:method ((stream string-stream) &aux (string (string-stream-string stream)))
           (when (> (length string) 16)
             (setf string (concatenate 'string (subseq string 0 12) "<..>")))
           (make-instance 'stream-state
             :source string :offset (stream-position stream)))
  (:method ((stream entity-string-stream))
           (make-instance 'stream-state 
             :source (stream.entity stream) :offset (stream-position stream))))

"XMLP"
