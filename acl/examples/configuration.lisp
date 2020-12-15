;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: HTTP; Base: 10 -*-

(in-package "HTTP")

;;; Make the CL-HTTP configuration work under ACL unix.
;;; This file must be loaded after the default configuration file
;;; and before the default exports file. It sets parameters specific
;;; to the ACL unix version.
;;; Reason: Under UNIX, port 80 requires special privilege.
;;; - OBC

;;; "1" is also fine across UNIX client servers. Tune this number to serve
;;; other types of clients. The default of 5 now works well with ACL
;;; and maybe it will be nicer for MAC clients as well.
;;;
(setq *number-of-listening-processes* #-ACLPC 5 #+ACLPC 1)

;;; Q4: Choose 80, 8000 or an integer in range to define the standard port
;;;     used for http service. From a web client you can then access the
;;;     service using for example the URL:
;;; http://machine.site.location.class:8000/cl-http/
;;;
(eval-when (load eval)
(let ((answer (user::with-input-argument ("port=" (or fixnum symbol))
		(user::y-n-or-type '(integer 8000 9000) "HTTP service on port 80(y) or 8000(n) (y/n/A Number)? "))))
  (case answer
    ((t) (set-standard-http-port 80))
    ((nil) (set-standard-http-port 8000))
    (t (set-standard-http-port answer))))
)

;; Initialize all server variables for current host
(reset-server-local-host-variables)

;;; This is required by acl;server;tcp-interface
;;;
(setq  *http-ports* (list *standard-http-port*))

;;; Set the range to hunt for a server socket beyond the standard http port
;;; value. This is very useful while UNIX GC does not reclaim its sockets.
;;;
(standard-port-range 10)

;; Controls whether the file log stream remains open all the time, or it is
;; reopenned for each log transaction. Production servers should keep the log
;; file stream open.
(log-file-stream-stays-open #-ACLPC t #+ACLPC nil)

#+ACLPC
(setq *persistent-connection-maximum-requests* 1)
#+ACLPC
(setq *persistent-connection-timeout* 0)

#||
;;; Bogus example defining and using a proxy
;;; service as provided with the ACL version:

(define-proxy-entry "poxy1.ai.mit.edu" :ports 8000)
(define-proxy-entry "proxy2.ai.mit.edu" :ports 8000)
(define-proxy-entry "mainproxy.mit.edu" :ports 8000)

(define-proxy-directs "mit.edu")

(enable-proxy-service)
||#
