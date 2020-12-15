;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-

;;; (C) Copyright 1994-99, John C. Mallery
;;;     All Rights Reserved.
;;;

(in-package :http)

;;;------------------------------------------------------------------- 
;;;
;;; REPORTING RELATED TO HTTP CONDITIONS
;;;

(defgeneric report-status-line (http-condition stream)
  (:documentation "Primitive that sends a status line over an HTTP connection."))

(defmethod report-status-line ((condition http-condition) stream)
  (declare (ignore stream))
  (error "The condition, ~S, cannot be reported to the client." condition))

(defmethod report-status-line ((condition reportable-condition) stream)
  (let* ((status-code (status-code condition))
         (reason (or (http-reason condition)
                     (get-string-for-status-code status-code))))
    (send-status-line stream status-code reason)))

(defgeneric report-status-message (http-condition stream &optional format-string format-args)
  (:documentation "Primitive that sends a status body message over an HTTP connection."))

(defmethod report-status-message ((condition http-condition) stream &optional format-string format-args)
  (declare (ignore stream format-string format-args))
  (error "The condition, ~S, cannot be reported to the client." condition))

(define cl-http-signature (&optional (stream *output-stream*))
  "Writes the server signature as an address on STREAM."
  (with-emphasis (:address :stream stream)
    (note-anchor *server-version* :reference *cl-http-home-page-url-string* :stream stream)))

(defun %report-status-message (url reason stream &optional report)
  (with-html-document (:declare-dtd-version-p t :stream stream)
    (with-document-preamble (:stream stream)
      (declare-title reason :stream stream))
    (with-standard-document-body (:stream stream)
      (with-section-heading (reason :stream stream)
        (horizontal-line :stream stream)
        (with-paragraph (:stream stream)
          (cond (report
                 (html:with-verbatim-text (:fresh-line nil :stream stream)
                   (html:write-string-quoting-specials report stream)))
                (url (fast-format stream "~A for URI ~A"
                                  reason (typecase url
                                           (url (url:name-string url))
                                           (t url))))
                (t (let ((mail-url (concatenate 'string "mailto:" (server-mail-address))))
                     (declare (dynamic-extent mail-url))
                     (fast-format stream "~&Error ~D not properly reported.~&Please advise the server maintainer at: "
                                  (server-status *server*))
                     (note-anchor (server-mail-address) :reference mail-url :stream stream)))))
        (horizontal-line :stream stream)
        (cl-http-signature stream)))))

(defmethod report-status-message ((condition reportable-condition) stream &optional format-string format-args)
  (let* ((status-code (status-code condition))
         (reason (or (http-reason condition)
                     (get-string-for-status-code status-code)))
         (url (http-url condition)))
    (unless format-string
      (setq format-string (format-string condition)))
    (unless format-args
      (setq format-args (format-args condition)))
    (if format-string
        (let ((report (apply #'format nil format-string format-args)))
          (declare (dynamic-extent report))
          (%report-status-message url reason stream report))
        (%report-status-message url reason stream))))

(defmethod report-status-message ((condition document-moved) stream &optional format-string format-args)
  (let ((new-urls (new-urls condition))
        (reason (http-reason condition)))
    (with-document-preamble (:stream stream)
      (declare-title reason :stream stream))
    (with-document-body (:stream stream)
      (with-section-heading (reason :stream stream)
        (horizontal-line :stream stream)
        format-string format-args               ;ignore
        (with-paragraph (:stream stream)
          (write-string "This document has moved " stream)
          (typecase new-urls
            (cons
              (cond ((cdr new-urls)
                     (loop for urls = new-urls then (cdr urls)
                           for num upfrom 1
                           while urls
                           for label = (with-output-to-string (string)
                                         (write-string "here " string)
                                         (write num :stream string :escape nil :base 10))
                           do (note-anchor label :reference (car urls) :stream stream)
                              (when (cdr urls) (write ", "))))
                    (t (note-anchor "here" :reference (car new-urls) :stream stream))))
            (t (note-anchor "here" :reference new-urls :stream stream)))
          (write-char #\. stream))
        (horizontal-line :stream stream)
        (cl-http-signature stream)))))

(define-generic http-url-string (reportable-condition)
  (:documentation
    "Returns the URL string for REPORTABLE-CONDITION, or null is a URL is unavailable."))

(defmethod http-url-string ((condition reportable-condition))
  (let ((url (http-url condition)))
    (etypecase url
      (null nil)
      (string url)
      (url:url (url:name-string url)))))

;;;------------------------------------------------------------------- 
;;;
;;; REPORT STATUS
;;;

(define-generic report-http-headers (http-condition stream &optional termination-line-p header-plist content-type)
  (:documentation "Top-level methods for sending HTTP headers accompanying REPORT-STATUS."))

#+ignore
(progn
  (defparameter count 0)

  (defmethod report-http-headers ((condition null) stream &optional (termination-line-p t) header-plist content-type)
    (when (< count 1)
      (incf count)
      (tv:notify nil "REPORT-HTTP-HEADERS error is in a break. Go debug it.")
      (break "Lost with null condition.")))
  )

(defmethod report-http-headers ((condition condition) stream
                                &optional (termination-line-p t) header-plist (content-type :html))
  (let ((headers `(:date ,(server-request-time *server*)
                   :server ,*server-version*
                   :connection (:close)
                   ,.(when content-type
                       `(:content-type ,(%mime-content-type-spec content-type)))
                   ,@header-plist)))
    (declare (dynamic-extent headers))
    (write-headers stream headers termination-line-p)))

(defmethod report-http-headers ((condition reportable-condition) stream &optional (termination-line-p t)
                                header-plist (content-type :html)
                                &aux (server *server*))
  (when (http-close-connection-p condition)
    (setf (server-close-connection-p server) t))
  (%write-document-mime-headers stream content-type nil nil nil nil nil nil nil nil nil nil nil
                                termination-line-p nil header-plist))

(defmethod report-http-headers :around ((condition method-not-allowed) stream
                                        &optional (termination-line-p t) header-plist content-type)
  (let* ((url (http-url condition))
         (server *server*)
         ;; If the url is an uninterned search URL, we just default to the
         ;; standard methods on the server  8/7/96 -- JCMa.
         (more-headers `(:allow ,(http-methods (or (and url (intern-url url :if-does-not-exist :soft))
                                                   server)
                                               (server-http-version server))
                         ,@header-plist)))
    (declare (dynamic-extent more-headers))
    (case (http-method condition)
      (:put ;; close connection because the server will try to blast data at us. barf.
        (unless (client-http-version-meets-p server :http/1.1)
          (setf (server-close-connection-p server) t))))
    (call-next-method condition stream termination-line-p more-headers content-type)))

(define-generic report-status (http-condition stream)
  (:documentation "Top-level function for reporting a condition over an HTTP connection."))

(defmethod report-status ((condition http-condition) stream)
  (declare (ignore stream))
  (error "The condition, ~S, cannot be reported to the client." condition))

(defmethod report-status ((condition reportable-condition) stream)
  (let* ((server *server*)
         (method (or (server-method server) (http-method condition))))
    (case method
      (:head ;; head never sends a body
        (report-status-line condition stream)
       (report-http-headers condition stream t))
      (t (cond ((client-http-version-meets-p server :http/1.1)
                (%with-chunked-transfer-encoding
                  (stream)
                  (progn (report-status-line condition stream)
                         (report-http-headers condition stream nil))
                  (report-status-message condition stream)))
               (t (report-status-line condition stream)
                  (report-http-headers condition stream t)
                  (report-status-message condition stream)))))))

(defmethod report-status ((error url:parsing-error) stream)
  (let* ((url (url:url-string error))
         (reason (get-string-for-status-code 400))
         (server *server*)
         (method (server-method server)))
    (case method
      (:head ;; head never sends a body
        (send-status-line stream 400 reason)
       (report-http-headers error stream t nil nil))
      (t (cond ((client-http-version-meets-p server :http/1.1)
                (%with-chunked-transfer-encoding
                  (stream)
                  (progn (send-status-line stream 400 reason)
                         (report-http-headers error stream nil))
                  (%report-status-message url reason stream)))
               (t (send-status-line stream 400 reason)
                  (report-http-headers error stream t)
                  (%report-status-message url reason stream)))))))

(defvar *report-status-condition* nil
  "Current condition being reported by REPORT-STATUS.")

;; Catch any errors reporting errors.  Errors that re-enter this code
;; are sure losers attempting to resignal within the process of
;; signalling the original error.  These cases are detected and handled
;; in a safe manner. 9/22/99 -- JCMa.
(defmethod report-status :around ((condition http-condition) stream)
  (flet ((report-rentrant-error (primary-error secondary-error)
           (let ((secondary-error-type (type-of secondary-error)))
             (report-bug *bug-http-server*
                         (format nil "REPORT-STATUS Re-entrant Error: ~S" secondary-error-type)
                         "~:[~;~&Log: ~:*~S~]~&Primary Error: ~S~:[~;~&Primary Error Report: ~:*~A~]~
                          ~&Secondary Error: ~S~:[~;~&Secondary Error Report: ~:*~A~]~:[~;~&Backtrace: ~:*~A~]"
                         (when http:*server* (write-extended-common-logfile-entry http:*server* nil))
                         (type-of primary-error) (report-string primary-error) (type-of secondary-error) (report-string secondary-error)
                         (when *stack-backtraces-in-bug-reports* (stack-backtrace-string condition))))))
    (cond (*report-status-condition*
           (report-rentrant-error *report-status-condition* condition))
          (t (let ((*report-status-condition* condition))
               (handler-case-if (not *debug-server*)
                  (call-next-method condition stream)
                 (network-error () nil)         ;no need to report errors telling the user he lost
                 (error (error) (bug-report-error error))))))))

;; Don't bother reporting to dead TCP streams.
(defmethod report-status :around ((condition reportable-condition) stream)
  (when (www-utils:live-connection-p stream)
    (call-next-method)))

;; No client present.
(defmethod report-status :around ((condition request-timeout) stream)
  (when (www-utils:live-connection-p stream)
    (call-next-method)))

;; Handles any residual errors not covered by other condition types.
(defmethod report-status-unhandled-error ((error condition) stream request)
  (flet ((report-the-secondary-error (error stream reason)
           (let* ((report-string (report-string error))
                  (report (with-output-to-string (string)
                            (with-paragraph (:stream string)
                              (with-rendition (:bold :stream string)
                                (write-string "Request: " string))
                              (if request (write-string request string) "{Not available}"))
                            (with-paragraph (:stream string)
                              (with-rendition (:bold :stream string)
                                (write-string "Error: " string))
                              (with-verbatim-text (:fresh-line nil :stream stream)
                                (write-string-quoting-specials report-string string))))))
             (declare (dynamic-extent report-string report))
             (%report-status-message nil reason stream report))))
    (let ((reason (get-string-for-status-code 500)))
      (cond ((handler-case (client-http-version-meets-p *server* :http/1.1) (error () nil))
             (%with-chunked-transfer-encoding
               (stream)
               (progn (send-status-line stream 500 reason)
                      (report-http-headers error stream nil))
               (report-the-secondary-error error stream reason)))
            (t (send-status-line stream 500 reason)
               (report-http-headers error stream t)
               (report-the-secondary-error error stream reason))))))

(defmethod report-status-unhandled-error :around ((error condition) stream request)
  (declare (ignore request))
  (when (www-utils:live-connection-p stream)
    (call-next-method))
  (bug-report-error error))

(defmethod report-status ((condition server-internal-error) stream)
  (let* ((error (server-error condition))
         (error-report (and error (report-string error)))
         (server *server*)
         (method (or (server-method server) (http-method condition))))
    (case method
      (:head ;; head redirects never send a body
        (report-status-line condition stream)
       (report-http-headers error stream t))
      (t (cond ((client-http-version-meets-p server :http/1.1)
                (%with-chunked-transfer-encoding
                  (stream)
                  (progn (report-status-line condition stream)
                         (report-http-headers error stream nil))
                  (report-status-message condition stream error-report)))
               (t (report-status-line condition stream)
                  (report-http-headers error stream t)
                  (report-status-message condition stream error-report)))))))

(defmethod report-status :around ((condition server-internal-error) stream)
  stream                                        ;ignore
  (when (www-utils:live-connection-p stream)
    (call-next-method))
  (bug-report-error condition))

;; This is quite inline so that it doesn't waste time when the server is on the edge.
(defmethod report-status ((condition server-overloaded) stream)
  (flet ((report-the-message (stream reason)
           (with-html-document (:declare-dtd-version-p t :stream stream)
             (with-document-preamble (:stream stream)
               (declare-title reason :stream stream))
             (with-standard-document-body (:stream stream)
               (with-section-heading (reason :stream stream)
                 (horizontal-line :stream stream)
                 (with-paragraph (:stream stream)
                   (etypecase *overload-message*
                     (null
                       (write-string "This server is currently operating at capacity and cannot accept your request. Please try again later."
                                     stream))
                     (string (write-string *overload-message* stream))
                     (function (funcall *overload-message* condition stream))
                     (symbol (funcall (fdefinition *overload-message*) condition stream))))
                 (horizontal-line :stream stream)
                 (cl-http-signature stream))))))
    (let* ((status-code (status-code condition))
           (reason (or (http-reason condition)
                       (get-string-for-status-code status-code)))
           (server *server*)
           (method (or (server-method server) (http-method condition))))
      (case method
        (:head ;; head redirects never send a body
          (send-status-line stream status-code reason)
         (report-http-headers condition stream t))
        (t (cond ((client-http-version-meets-p server :http/1.1)
                  (%with-chunked-transfer-encoding
                    (stream)
                    (progn (send-status-line stream status-code reason)
                           (report-http-headers condition stream nil))
                    (report-the-message stream reason)))
                 (t (case method    ;close connection for 1.0 puts
                      (:put (setf (server-close-connection-p server) t)))
                    (send-status-line stream status-code reason)
                    (report-http-headers condition stream t)
                    (report-the-message stream reason))))))))

(defmethod report-status ((condition access-control-condition) stream)
  (let* ((server *server*)
         (method (or (server-method server) (http-method condition))))
    (case method
      (:head ;; head redirects never send a body
        (report-status-line condition stream)
       (report-http-headers condition stream t nil nil))
      (t (cond ((client-http-version-meets-p server :http/1.1)
                (%with-chunked-transfer-encoding
                  (stream)
                  (progn (report-status-line condition stream)
                         (report-http-headers condition stream nil))
                  (report-status-message condition stream)))
               (t (case method ;close connection for 1.0 puts
                    (:put (setf (server-close-connection-p server) t)))
                  (report-status-line condition stream)
                  (report-http-headers condition stream t)
                  (report-status-message condition stream)))))))

(defmethod authentication-header-spec ((condition recoverable-unauthorized-client-access))
  (www-authenticate-header-spec (http-authentication-realm condition) (http-authentication-method condition)))

(defmethod authentication-header-spec ((condition client-access-with-stale-nonce))
  (www-authenticate-header-spec (http-authentication-realm condition) (http-authentication-method condition) :stale t))

(defmethod report-status ((condition recoverable-unauthorized-client-access) stream)
  (let* ((more-headers (authentication-header-spec condition))
         (server *server*)
         (method (or (server-method server) (http-method condition))))
    (declare (dynamic-extent more-headers))
    (case method
      (:head ;; head redirects never send a body
        (report-status-line condition stream)
       (report-http-headers condition stream t more-headers nil))
      (t (cond ((client-http-version-meets-p server :http/1.1)
                (%with-chunked-transfer-encoding
                  (stream)
                  (progn (report-status-line condition stream)
                         (report-http-headers condition stream nil more-headers))
                  (report-status-message condition stream)))
               (t (case method                  ;close connection for 1.0 puts
                    (:put (setf (server-close-connection-p server) t)))
                  (report-status-line condition stream)
                  (report-http-headers condition stream t more-headers)
                  (report-status-message condition stream)))))))

(defmethod report-status ((condition unsupported-method) stream)
  (let* ((method (http-method condition))
         (url (http-url-string condition))
         (args (list method url)))
    (declare (dynamic-extent args))
    (cond ((client-http-version-meets-p *server* :http/1.1)
           (%with-chunked-transfer-encoding
             (stream)
             (progn (report-status-line condition stream)
                    (report-http-headers condition stream nil))
             (report-status-message condition stream "The ~A method is not supported: ~S" args)))
          (t (report-status-line condition stream)
             (report-http-headers condition stream t)
             (report-status-message condition stream "The ~A method is not supported: ~S" args)))))

;; body section can be sent with MIME multi-part message.
(defmethod report-status ((condition document-moved) stream)
  (let* ((target-window (http-target-window condition))
         (more-headers `(,.(loop for url in (ensure-list (new-urls condition))
                                 collect :location
                                 collect (url:name-string url))
                         ,.(when target-window
                             `(:window-target ,target-window))))
         (method (http-method condition)))
    (declare (dynamic-extent more-headers))
    (case method
      (:head ;; head redirects never send a body
        (report-status-line condition stream)
       (report-http-headers condition stream t more-headers nil))
      (t (cond ((client-http-version-meets-p *server* :http/1.1)
                (%with-chunked-transfer-encoding
                  (stream)
                  (progn (report-status-line condition stream)
                         (report-http-headers condition stream nil more-headers))
                  (report-status-message condition stream)))
               (t (case method ;; close connection for 1.0 PUT
                    (:put (setf (server-close-connection-p *server*) t)))
                  (report-status-line condition stream)
                  (report-http-headers condition stream t more-headers)
                  (report-status-message condition stream)))))))

(declaim (inline handle-url-standard-redirection))

(defun handle-url-standard-redirection (url temporary-p method &optional target-window)
  (check-type target-window (or null string))
  (let ((alternate-urls (url:alternate-urls url)))
    (cond
      ((null alternate-urls)
       (signal 'document-not-found :url url))
      (temporary-p
       (signal 'document-moved-temporarily :url url :method method :new-urls alternate-urls :target-window target-window))
      (t (signal 'document-moved-permanently :url url :method method :new-urls alternate-urls :target-window target-window)))))

(defmethod report-status ((condition precondition-failed) stream)
  (report-status-line condition stream)
  (report-http-headers condition stream t nil nil))

;;;------------------------------------------------------------------- 
;;;
;;; AUTOMATIC BUG REPORTING BY ERROR CLASS
;;;

;; report the bugs in general
(defmethod bug-report-error ((error error))
  (let ((error-type (type-of error)))
    (report-bug *bug-http-server*
                (format nil "HTTP Error: ~S" error-type)
                "~&Log: ~S~&Error: ~S~:[~;~&Error Report: ~:*~A~]~:[~;~&Backtrace: ~:*~A~]"
                (if http:*server*
                    (write-extended-common-logfile-entry http:*server* nil)
                    "NA")
                error-type (report-string error)
                (when *stack-backtraces-in-bug-reports*
                  (stack-backtrace-string error)))))

(defmethod bug-report-error ((condition condition))
  (let ((condition-type (type-of condition)))
    (report-bug *bug-http-server*
                (format nil "HTTP Condition: ~S" condition-type)
                "~&Log: ~S~&Condition: ~S~:[~;~&Condition Report: ~:*~A~]~:[~;~&Backtrace: ~:*~A~]"
                (if http:*server*
                    (write-extended-common-logfile-entry http:*server* nil)
                    "NA")
                condition-type (report-string condition)
                (when *stack-backtraces-in-bug-reports*
                  (stack-backtrace-string condition)))))

(defmethod bug-report-error ((error server-internal-error))
  (let* ((err (or (server-error error) error))
         (error-type (type-of err)))
    (report-bug *bug-http-server*
                (format nil "HTTP Error: ~S" error-type)
                "~&Log: ~S~&Error: ~A~:[~;~&Error Report: ~:*~A~]~:[~;~&Backtrace: ~:*~A~]"
                (if http:*server*
                    (write-extended-common-logfile-entry http:*server* nil)
                    "NA")
                error-type (report-string err)
                (when *stack-backtraces-in-bug-reports*
                  (stack-backtrace-string error)))))

(defmethod bug-report-error ((error error-computing-response))
  (let* ((headers (http-headers error))
         (err (or (server-error error) error))
         (error-type (type-of err))
         (backtrace (http-stack-backtrace error)))
    (report-bug *bug-http-server*
                (format nil "HTTP Error: ~S" error-type)
                "~&Log: ~S~&Error: ~A~:[~;~&Error Report: ~:*~A~]~:[~;~&Headers: ~:*~S~]~:[~;~&Backtrace: ~:*~A~]"
                (if http:*server*
                    (write-extended-common-logfile-entry http:*server* nil)
                    "NA")
                error-type (report-string err) headers backtrace)))

(defmethod bug-report-error ((error error-handling-post-method))
  (let* ((err (or (server-error error) error))
         (error-type (type-of err))
         (backtrace (http-stack-backtrace error))
         (headers (http-headers error))
         (form-alist (http-form-alist error)))
    (report-bug *bug-http-server*
                (format nil "HTTP Error: ~S" error-type)
                "~&Log: ~S~&Error: ~S~:[~;~&Error Report: ~:*~A~]~:[~;~&Headers: ~:*~S~]~
                 ~:[~;~&Form-Alist: ~:*~S~]~:[~;~&Backtrace: ~:*~A~]"
                (if http:*server*
                    (write-extended-common-logfile-entry http:*server* nil)
                    "NA")
                error-type (report-string err)
                headers form-alist backtrace)))
