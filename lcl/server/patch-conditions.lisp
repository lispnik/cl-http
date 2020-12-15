;;;; PATCH-CONDITIONS.LISP
;;;
;;; in the file `server/http-conditions.lisp', in the method
;;; `http-url-string' the construct `with-slots' is used.
;;; In Lucid this fails. In this file for each applicable
;;; condition a seperate method is defined.

(in-package "HTTP")

(defmethod http-url-string ((condition reportable-condition))
  (let ((url (reportable-condition-url condition)))
  (etypecase url
    (null nil)
    (string url)
    (url:url (url:name-string url)))))
