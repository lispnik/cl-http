;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: WWW-UTILS; Base: 10 -*-

(in-package :www-utils)

;; Until we have chunk encoding decoding for 1.1
(setq *http-version* "HTTP/1.1")
(setq http::*persistent-connection-maximum-requests* 1)
(setq http::*number-of-listening-processes* 1)
(setq http::*persistent-connection-timeout* 0)
(setq http1:log-file-stream-stays-open t)

(defvar *buffer-chunk* (make-string 512))

;;; original is http:acl;aclpc;patched.lisp
;;; do something meaningful for the stream type on acl5
(defmethod transfer-buffer-streams ((from-stream stream)
                                    (to-stream stream)
                                    &optional (size (length *buffer-chunk*)))
   (loop with buffer = (make-string size)
       with buffer-size = (min size (length buffer))
       as end = (read-sequence buffer from-stream :end buffer-size)
       do (write-sequence buffer to-stream :end end)
	  (clim-sys:process-yield)
       when (< end buffer-size)
       do (finish-output to-stream)
       until (zerop end)))






