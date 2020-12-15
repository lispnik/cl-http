;;; -*- Syntax: ansi-common-lisp; Base: 10; Package: http; Mode: LISP -*-

;;; (C) Copyright 1995, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; MAC HTTP CLIENT NETWORK INTERFACE
;;;

(in-package :http)

(declaim (inline deallocate-client-http-stream))

;; noop present for implementations that resource their TCP streams, e.g. mac.
(defun deallocate-client-http-stream (stream)
  (declare (ignore stream)))

(define-parameter *client-timeout* (* 60 2)          ; give up after 2 minutes
   "Second before the client times out trying to make an http connection.")

(declaim (type function client-manage-server-response))

(defun open-http-stream-to-host (host port)
  (declare (values stream))
  (let ((stream (if *proxy-service*
                    (find-proxyhost-connection host :port port :default-timeout *client-timeout*)
                  (find-direct-connection host :port port))))
     (unless stream
        (error 'host-not-responding :host host :port port))
     stream))

;; this is the function used by the portable client code.
(defun %get-user-name+pw (url-string &optional (stream *terminal-io*))
  (declare (values user-id pw abort-p))
  (let* ((user-name (default-client-user-id))
	 (password (default-client-pw)))
    (multiple-value-bind (uid pw abort-p)
	(ask-user-name+pw url-string user-name password stream)
      (unless abort-p
	(cond-every
	 (uid (set-default-client-user-id uid))
	 (pw (set-default-client-pw pw))))
      (values uid pw abort-p))))

(defun ask-user-name+pw (url-string name pwd stream)
  (let (nname npwd)
    (format stream "~&Access to URL, ~a, is restricted.~%" url-string)
    (if name
	(format stream "~&Login (default: ~a): " name)
      (format stream "~&Login: "))
    (setq nname (read-line stream nil nil))
    (if (equal nname "")
	(setq nname name))
    (if pwd
	(format stream "~&Password (default: ~a): " (make-string (length pwd) :initial-element #\*) pwd)
      (format stream "~&Password: "))
    (setq npwd (read-line stream nil nil))
    (if (equal npwd "")
	(setq npwd pwd))
    (values (or nname name) (or npwd pwd) (not (and nname npwd)))))
  

(defparameter *default-client-user-id* nil)

(define default-client-user-id ()
   (or *default-client-user-id* ""))

(defun set-default-client-user-id (user-id)
   (when user-id
       (setq *default-client-user-id* user-id)))

(defparameter *default-client-pw* nil)

(define default-client-pw ()
   (or *default-client-pw* ""))

(defun set-default-client-pw (pw)
   (when pw
       (setq *default-client-pw* pw)))
