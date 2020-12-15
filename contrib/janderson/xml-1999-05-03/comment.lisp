;;; -*- mode: lisp; package: "XML-PARSER"; -*-

"
<DOCUMENTATION>
 <COPYRIGHT HREF='defsystem.lisp|root().descendant(1,COPYRIGHT)'/>
 <CHRONOLOGY>
  <DELTA><DATE>19981114</DATE>
   split read function into parse/reduce phases;
   augmented error reporting</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
 "

(in-package "XML-PARSER")

;;;
;;;

(defClass comment (declaration-markup content-markup document-markup)
  ((content
    :reader comment.data))
  (:metaclass qualified-class))

(defMethod (setf comment.data)
           ((value t) (instance comment))
  (setf (slot-value instance 'content)
        (normalize-content-value instance value)))

(defMethod normalize-content-value
           ((comment comment) (data string))
  data)

(defMethod normalize-content-value
           ((comment comment) (data list))
  (format nil "~{~a~^ ~}" data))

(defMethod normalize-content-value
           ((comment comment) (data t))
  (write-to-string data))

(defMethod normalize-comment-data
           ((comment comment))
  (normalize-content-value comment (comment.data comment)))


(defmethod initialize-instance :after
           ((instance comment) &key data)
  (setf (comment.data instance) data))


;;;
;;; parsing

(defMethod reduce-production
           ((comment comment) &key)
  (xml-node.append-element *document* comment)
  comment)

(defMethod reduce-production
           ((production (eql 'XML-1.0::Comment))
            &key
            ((XML-1.0::char data)))
  (unless *parse-suppress*
    (reduce-production (make-instance 'comment :data data))))

(defMethod production-reader-macro
           ((production (eql 'XML-1.0::Comment)) (stream t)
            &aux data)
  (handler-bind ;; just augment error message for context
    ((error #'(lambda (condition)
                (xml-warn production
                          "data: ~s."
                          data)
                condition)))
    (setf data (read-string-delimited-string "-->" stream nil))
    (reduce-production production 'xml-1.0::char data)))

;;;
;;; printing

(defMethod print-object
           ((datum comment) stream)
  (if *print-readably*
    (let ((*parent-node* datum))
        (format stream "<!-- ~a -->" (comment.data datum)))
    (print-unreadable-object (datum stream :type t)
      (format stream "-- ~a" (comment.data datum)))))



#|
;; ok
(production-reader-macro
 'XML-1.0::comment
 (markup-stream "comment asasas -->"))

;; should fail
(production-reader-macro
 'XML-1.0::comment
 (markup-stream "comment asasas --"))

|#

"XMLP"

