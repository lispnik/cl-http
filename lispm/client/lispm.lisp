;;; -*- Syntax: ansi-common-lisp; Base: 10; Package: http; Mode: LISP -*-

;;; (C) Copyright 1994, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; LISPM CLIENT NETWORK INTERFACE
;;;

(net:define-protocol :http (:http :tcp)
  (:invoke (access-path)
    (net:get-connection-for-service access-path :translation :modal :characters t)))

(declaim (inline deallocate-client-http-stream))

;; a no-op on the lisp machine but required for platforms such as the mac.
;; 8/13/96 -- JCMa.
(defun deallocate-client-http-stream (stream)
  (declare (ignore stream)))

(declaim (special *client-timeout*))

(defun open-http-stream-to-host (host port)
  (declare (values stream))
  (www-utils::with-tcp-port-for-protocol 
    (:http port)
    (handler-bind ((net:host-does-not-support-service
                     #'(lambda (condition)
                         ;; so why doesn't this work? 
                         #|(scl:send condition :special-command :specify-path)|#
                         ;; dunno, but the heavy hammer here does.
                         (push '(:http :tcp :http) (scl:send host :get :service))
                         (throw 'neti::find-path-retry t))))
      (let ((tcp:*tcp-connect-timeout* *client-timeout*))
	(neti::invoke-service-on-host :http host)))))
