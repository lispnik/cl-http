;;; -*- mode: lisp; package "XML-PARSER"; -*-

"
<DOCUMENTATION>
 <COPYRIGHT HREF='defsystem.lisp|root().descendant(1,COPYRIGHT)'/>
 <CHRONOLOGY>
  <DELTA><DATE>19981114</DATE>
   new</DELTA>
  <DELTA><DATE>19981214</DATE>
   added missing reduce-production-method - these are not actually dtd-node's
   and thus are not handled there.</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
 "

(in-package "XML-PARSER")

;;;
;;; class definitions

(defClass conditional-section (declaration-markup)
  ()
  (:metaclass abstract-class))

(defClass include-section (conditional-section)
  ()
  (:metaclass qualified-class))

(defClass ignore-section (conditional-section)
  ()
  (:metaclass qualified-class))


;;;
;;; parsing
;;;
;;; both include and ignore sections note the last node and instantiate a
;;; respective instance. ignore sections bind *parse-suppress* in order to
;;; suppres instantiation within their scope.

(defMethod reduce-production
           ((production conditional-section) &key)
  production)

(defMethod reduce-production
           ((production (eql 'xml-1.0::includeSect)) &key)
  (reduce-production (make-instance 'include-section)))

(defMethod reduce-production
           ((production (eql 'xml-1.0::ignoreSect)) &key)
 (reduce-production (make-instance 'ignore-section)))

(defMethod production-reader-macro
           ((production (eql 'xml-1.0::includeSect)) (stream t))
  (handler-bind
    ((section-end-condition
      #'(lambda (condition)
          (declare (ignore condition))
          (return-from production-reader-macro
            (reduce-production production)))))
    (loop (setf *processed-node*
                (read-production 'xml-1.0::markupdecl stream))))
  ;; shouldn't happen
  (reduce-production production))

(defMethod production-reader-macro
           ((production (eql 'xml-1.0::ignoreSect)) (stream t)
            &aux
            (*parse-suppress* t))
  (handler-bind
    ((section-end-condition
      #'(lambda (condition)
          (declare (ignore condition))
          (return-from production-reader-macro
            (reduce-production production)))))
    (loop (setf *processed-node*
                (read-production 'xml-1.0::markupdecl stream))))
  ;; shouldn't happen
  (reduce-production production))
  

"XMLP"

