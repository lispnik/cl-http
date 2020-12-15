;;;   -*- Mode: LISP; Package: http-user; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-

;;;
;;; (c) Copyright  1996, John C. Mallery
;;;     All Rights Reserved.
;;;


;;;------------------------------------------------------------------- 
;;;
;;; CLIENT EXAMPLES
;;;

;; In order to run these examples, you need to first load the client for your
;; platform. See the client directory under your platform directory.

;; Please report any interesting ideas or examples of using the functional
;; client to WWW-CL@ai.mit.edu.  We need to discuss what program interfaces
;; to the web would be useful in the distributed system.

(in-package :http-user)

;;;------------------------------------------------------------------- 
;;;
;;; LINKING LISP OVER THE WEB
;;;

;; Define a server-side handler
(defmethod handle-remote-procedure-call (url stream query-alist)
  (declare (ignore url))
  (let (values error fctn args)
    (handler-case
      ;; Bind the alist values coming across the wire after applying the
      ;; appropriate readers.
      (http::bind-query-values* ((function :package (find-package :http-user))
                                 (arguments :reader read-from-string))
                                (url query-alist)
        (setq fctn function
              args arguments
              values (multiple-value-list (apply function arguments))))
      (error (err) (setq error err)))
    (cond ;; Report the error back to the caller using text/plain
      (error 
       (http:with-successful-response (stream :text)
         (format stream "Error applying ~S to ~S:~2%~A" fctn args (report-string error))))
      ;; return lisp data that will use the Lisp reader on client using application/lisp-sexp
      (t (http:with-successful-response (stream :sexp)
           (return-form-values stream :values values))))))

(defparameter *rpc-url* #u"/cl-http/remote-procedure-call.html")

(http:export-url *rpc-url*
                 :html-computed-form
                 :form-function #'identity
                 :response-function #'handle-remote-procedure-call
                 :description "Simple remote procedure call applies a function to arguments and returns values."
                 :keywords '(:cl-http :demo)
                 :secure-subnets `(,(local-host-ip-address)))

;; Define the client-side caller.
(defun remote-procedure-call (function &rest arguments)
  (let ((return-alist (post-form-values *rpc-url* :function function :arguments arguments)))
    (bind-query-values (values)
                       (nil return-alist)
      (values-list values))))

#|

;; do some arithmetic
(remote-procedure-call 'truncate 23 4)

;; whoopsie, get an error
(remote-procedure-call 'trunc 23 4)

|#
