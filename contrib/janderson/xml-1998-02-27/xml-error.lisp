;;; -*- package: ("XML-PARSER") -*-

#|
<DOCUMENTATION>
<DESCRIPTION>
a framework for validation and form constraints.
</DESCRIPTION>
</DOCUMENTATION>
|#
(in-package :XML-PARSER)

(define-condition xml-error (simple-error) ())
(define-condition xml-validity-error (xml-error) ())
(define-condition xml-form-error (xml-error) ())
(define-condition xml-cell-error (xml-error cell-error) ())


(defMethod xml-validity-error
           ((context t) message &rest args)
  (if (xml-document.validate? *document*)
    (cerror "ignore the validity error"
            'xml-validity-error
           :format-string "validity error in context: ~s:~%~?"
           :format-arguments (list context message args))
    (warn "validity error in context: ~s:~%~s"
          context (apply #'format nil message args))))

(defMethod xml-form-error
           ((context t) message &rest args)
  (error 'xml-form-error
         :format-string "form error in context: ~s:~%~?"
         :format-arguments (list context message args)))
;(form-error 'test "this is to test ~a and ~a." 'form 'errors)


(defMethod xml-cell-error
           ((context t) message &rest args)
  (error 'xml-cell-error
         :format-string "xml cell error in context: ~s:~%~?"
         :format-arguments (list context message args)))

(defMethod xml-verbose
           ((what t))
  (find-xml-verbose *xml-verbose* what))

(defMethod find-xml-verbose
           ((settings cons) (what t))
  (find what settings))

(defMethod find-xml-verbose
           ((settings null) (what t))
  nil)

(defMethod find-xml-verbose
           ((settings (eql t)) (what t))
  t)

(defMethod find-xml-verbose
           ((settings t) (what t))
  (equalp settings t))


:EOF
