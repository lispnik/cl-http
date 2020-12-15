;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: HTTP; Base: 10 -*-

(in-package "HTTP")

;;; Patches for ACLPC.
;;; Copyright (C) 1996 Olivier (OBC) all rights reserved.
;;; See copyright notice in CLIM;CLIM-SYS;MINIPROC file.

;; Until we have chunk encoding decoding for 1.1
(setq *http-version* "HTTP/1.0")
;(setq http::*persistent-connection-maximum-requests* 1)
;(setq http::*number-of-listening-processes* 1)
;(setq http::*persistent-connection-timeout* 0)
;;(log-file-stream-stays-open t)


(defun map-errorn (errorn)
  (case (- errorn 10000)
    (49 'UNKNOWN-ADDRESS)
    ((53 101) 'CONNECTION-LOST)
    (54 'CONNECTION-CLOSED)
    (55 'NETWORK-RESOURCES-EXHAUSTED)
    ((57 58) 'CONNECTION-ERROR)
    (59 'LOCAL-NETWORK-ERROR)
    (60 'PROTOCOL-TIMEOUT)
    (61 'CONNECTION-REFUSED)
    (63 'NETWORK-PARSE-ERROR)
    (64 'HOST-STOPPED-RESPONDING)
    (65 'HOST-NOT-RESPONDING)
    (68 'UNKNOWN-HOST-NAME)
    (70 'BAD-CONNECTION-STATE)
    (71 'REMOTE-NETWORK-ERROR)
    ((91 92 93) 'DOMAIN-RESOLVER-ERROR)
    (t 'NETWORK-ERROR)))

(defmethod simple-condition-format-control ((error error))
  (slot-value error 'acl::format-control))

(defmethod simple-condition-format-arguments ((error error))
  (slot-value error 'acl::format-arguments))

(defmacro handle-socket-errors (form &optional ignore-errors)
  `(handler-case ,form
     (error (error)
       (destructuring-bind (&optional stream errorn)
           (simple-condition-format-arguments error)
         (if (and (streamp stream)
                  (numberp errorn))
             (let ((control (simple-condition-format-control error)))
               (if (equal control "Error in using socket ~s, error code ~d")
		   (if (not ,ignore-errors)
		       (signal (map-errorn errorn)
			       :format-arguments (list stream errorn))))))))))

;;; From server;http-conditions

;; Catch any errors reporting errors.
(user::handling-redefinition
(defmethod report-status :around ((condition http-condition) stream)
  (handler-case-if (not *debug-server*)
     (call-next-method)
     (error (error)
	    (if (live-connection-p stream)
		(handle-socket-errors (report-status-unhandled-error error stream (server-request *server*)) t)
	      (warn "Unable to report error ~a to client." error)))))
)

;;; From server;utils

(user::handling-redefinition
(defmethod http-input-data-available-p (stream &optional timeout-seconds)
  "Returns non-null when input data is available on the HTTP STREAM within TIMEOUT-SECONDS.
When timeout-seconds is null, data must be immediately available. A dead HTTP connection
means no data is available."
  (labels ((data-available-p (stream)
             (loop while (ipc::stream-listen stream)        ;some data available
                   for char = (peek-char nil stream nil)
                   while char                   ;clear any dangling White Space due to buggy clients.
                   when (member char '(#\return #\linefeed #\space #\tab) :test #'eql)
                     do (read-char stream t)
                   return t                     ;something still there.
                   finally (return nil)))
           (continue-p (stream)
             (or (not (www-utils:live-connection-p stream))     ;connection went dead
                 (data-available-p stream))))   ;data available
    (declare (inline data-available-p))
    (cond ((not (www-utils:live-connection-p stream)) nil)
          ((data-available-p stream) t)
          ((and timeout-seconds (not (zerop timeout-seconds)))
           ;; Block until there is reason to take action
           (process-wait-with-timeout "HTTP Request Wait" timeout-seconds #'continue-p stream)
           ;; Determine whether input data was available without consing.
           (and (www-utils:live-connection-p stream)
                (ipc::stream-listen stream)))
          (t nil))))
)

;;; From server;server
;;;

(defun process-request%% (server stream catch-errors-p)
  (flet ((read-request (stream)
	   (multiple-value-bind (line eof delim length)
	       (read-delimited-line stream '(#\Linefeed #\Return) t *server-line-buffer*)
	     (declare (ignore delim))
	     ;;(format t "~&Line: ~S~&EOF: ~S~&Delim: ~S~&Length: ~D" line eof delim length)
	     (if eof
		 (error 'bad-syntax-provided :format-string "Connection dropped while reading request line.")
	       (subseq line 0 length))))
	 (handle-connection-closure (cond)
	   (when (http-close-connection-p cond)
	     (setf (server-close-connection-p server) t))))
    (declare (inline read-request handle-connection-closure))
    (handler-case-if catch-errors-p
      (handle-socket-errors 
       (let ((request (read-request stream)))
	 (setf (server-request server) request);; capture immediately in case of error
	 (%execute-request server request stream)) t)
      (reportable-condition
       (cond)
       (report-status cond stream)
       (handle-connection-closure cond))
      (url:parsing-error
       (error)
       (report-status error stream)
       (handle-connection-closure error)))
    ;; these may well need to be handled by individual methods due to the possible
    ;; fencepost error where the output buffer has already been sent before we hit
    ;; the force-output below.   4/19/96 -- JCMa.
    (ignore-errors (clear-input stream))		;clear input in preparation for next transaction
    (ignore-errors (force-output stream))		;force output while deciding what to do next
    (incf (server-requests-completed server)))) ;count completed requests

(user::handling-redefinition
(defmethod provide-service ((server basic-server-mixin))
    (with-connection-noted                      ;first increment connection counter
      (with-server-line-buffer ()
        (with-standard-server-io-syntax ()
          (let ((stream (server-stream server)))
            (with-local-port-context ((local-port stream))
              (clim-sys:process-loop
	       :declare (declare (dynamic *server-line-buffer* *server* *standard-http-port* *local-context*))
	       :with ((catch-errors-p (not *debug-server*)) end requested)
	       :until end
	       :do (progn
		     (setq requested (ipc::stream-listen stream))
		     (when requested
		       (process-request%% server stream catch-errors-p)
		     (setq end (null (persistent-connection-p server nil)))
		     (if (and requested (null end))
			 (log-access server))
		     (unless (http-input-data-available-p stream *persistent-connection-timeout*)
                         ;; Close with abort because we have timed out. There doesn't seem to
                         ;; be a reliable close here. At least, the client has all the data.   12/1/95 -- JCMa.
		       (close stream :abort (not (live-connection-p stream)))
		       (setq end t))
		     ;; Reset the server instance to avoid confusion accross
		     ;; transactions in a persistent connection
		     (unless end
		       (reset-transaction-state server))))
	       ;;primary close frees the client ASAP without using abort.
	       :finally (progn (close stream :abort (not (live-connection-p stream)))
			       (log-access server)))))))))
)

(user::handling-redefinition
(defun %process-request (server stream)
  (flet ((read-request (stream)
	   (multiple-value-bind (line eof delim length)
	       (read-delimited-line stream '(#\Linefeed #\Return) t *server-line-buffer*)
	     (declare (ignore delim))
	     ;;(format t "~&Line: ~S~&EOF: ~S~&Delim: ~S~&Length: ~D" line eof delim length)
	     (if eof
		 (error 'bad-syntax-provided :format-string "Connection dropped while reading request line.")
		 (subseq line 0 length))))
	 (handle-connection-closure (cond)
	   (when (http-close-connection-p cond)
	     (setf (server-close-connection-p server) t))))
    (declare (inline read-request handle-connection-closure))
    (handler-case
     (handle-socket-errors
      (let ((request (read-request stream)))
	(setf (server-request server) request);; capture immediately in case of error
	(%execute-request server request stream))
      t)
      (reportable-condition
	(cond)
	(setf (server-status server) (status-code cond))
	(report-status cond stream)
	(handle-connection-closure cond))
      (url:parsing-error
	(error)
	(setf (server-status server) 400)
	(report-status error stream)
	(handle-connection-closure error)))
    (force-output stream)			;force output while deciding what to do next
    (incf (server-requests-completed server))))	;count completed requests
)

#+future-version
(user::handling-redefinition
(defmethod provide-service ((server basic-server-mixin))
  (with-connection-noted                        ;first increment connection counter
    (with-server-line-buffer ()
      (with-standard-server-io-syntax ()
        (let ((stream (server-stream server)))
          (with-local-port-context ((local-port stream))
            (let-if (and *debug-server* (null *break-on-signals*))
                    ((*break-on-signals* t))
	      (clear-white-space stream) ;tolerate clients with leading CR-LF or whitespace.
              (clim-sys:process-loop
	       :declare (declare (dynamic *server-line-buffer* *server* *standard-http-port* *local-context* *break-on-signals*))
	       :with (end requested)
	       :until end
	       :do (progn
		     (setq requested (ipc::stream-listen stream))
		     (when requested
		       (%process-request server stream))
		     (setq end (null (persistent-connection-p server nil)))
		     (if (and requested (null end))
			 (log-access server))
		     (unless (http-input-data-available-p stream *persistent-connection-timeout*)
                         ;; Close with abort because we have timed out. There doesn't seem to
                         ;; be a reliable close here. At least, the client has all the data.   12/1/95 -- JCMa.
		       (close stream :abort (not (live-connection-p stream)))
		       ;; (return-from provide-service)
		       (setq end t))
		     ;; Reset the server instance to avoid confusion accross
		     ;; transactions in a persistent connection
		     (unless end
		       (reset-transaction-state server)))
	       ;;primary close frees the client ASAP without using abort.
	       :finally (progn (close stream :abort (not (live-connection-p stream)))
			       (log-access server)))
	      )))))))
)

;;;server;log
;;;log-queue-main-loop has to be rewritten
(user::handling-redefinition
(defmethod write-access-log-entry ((log common-file-format-mixin) (server server-logging-mixin) log-stream gmt-p)
  (declare (ignore log-stream gmt-p)))
 )

(user::handling-redefinition
(defmethod write-access-log-entry ((log extended-common-file-format-mixin) (server server-logging-mixin) log-stream gmt-p)
  (declare (ignore log-stream gmt-p)))
)

(user::handling-redefinition
(defmethod log-access ((server server-logging-mixin)) nil)
)

;;; From acl;server;unix
;;;

(defvar *buffer-chunk* (make-string 128))

(defmethod www-utils::transfer-buffer-streams ((from-stream stream) (to-stream stream) &optional (size (length *buffer-chunk*)))
  (loop with buffer = *buffer-chunk*
      with buffer-size = (min size (length buffer))
      as end = (read-sequence buffer from-stream :end buffer-size)
      do (write-sequence buffer to-stream :end end)
	 (clim-sys:process-yield)
      when (< end buffer-size)
      do (finish-output to-stream)
      until (zerop end)))
    
;;; Server;server

(user::handling-redefinition
(defun %write-document-from-pathname (url pathname content-type stream &optional charset)
  (minp:with-open-file (file-stream pathname :direction :input)
    (with-conditional-get-response
      (stream content-type
              :last-modification (file-stream-creation-date file-stream)
              :character-set charset
              :entity-tag (file-stream-version file-stream)
              :bytes (file-stream-length-in-bytes file-stream)
              :expires (expiration-universal-time url)
              :content-location url
              :cache-control (url:response-cache-control-directives url)
              :content-language (languages url))
      ;; send the contents
      (stream-copy-until-eof file-stream stream))))
)

(user::handling-redefinition
(defun %%write-binary-file (pathname url content-type stream &optional charset)
  (minp:with-open-file (file-stream pathname :direction :input :element-type  #+ACLPC 'character #-ACLPC '(unsigned-byte 8))
    (let ((resource-length (file-stream-length-in-bytes file-stream))
          (last-modification (file-stream-creation-date file-stream))
          (version (file-stream-version file-stream))
          (expires (expiration-universal-time url))
	  (cache-control (url:repsonse-cache-control-directives url))
          (languages (languages url)))
      (handling-conditional-get (stream :last-modification last-modification
                                        :character-set charset
                                        :entity-tag version :expires expires
                                        :termination-line-p t)
        ;; send the contents
        (let ((range (get-header :range)))
          (cond ;; if more than one range, send whole resource for now.  6/25/96 -- JCMa.
            ((and range (null (cddr range)))
             ;; Send a byte range
             (destructuring-bind (start last) (second range)
               (multiple-value-bind (start-byte last-byte content-length) 
                   (byte-range-parameters start last resource-length)
                 (let ((headers `(:content-range (:bytes ,start-byte ,last-byte ,resource-length))))
                   (declare (dynamic-extent headers))
                   (with-successful-response (stream content-type :status :partial-content :bytes content-length
                                                     :last-modification last-modification
                                                     :character-set charset
                                                     :entity-tag version :expires expires :cache-control cache-control
                                                     :content-location url
                                                     :content-language languages
                                                     :additional-mime-headers headers
                                                     :termination-line-p t)
                     (with-binary-stream (stream :output)
                       (stream-copy-byte-range file-stream stream start-byte last-byte)))))))
            ;; Send the full content
            (t (with-successful-response (stream content-type :status :success :bytes resource-length
                                                 :last-modification last-modification
                                                 :character-set charset
                                                 :entity-tag version :expires expires :cache-control cache-control
                                                 :content-location url
                                                 :content-language languages
                                                 :termination-line-p t)
                 (with-binary-stream (stream :output)
                   (stream-copy-until-eof file-stream stream))))))))))
)
