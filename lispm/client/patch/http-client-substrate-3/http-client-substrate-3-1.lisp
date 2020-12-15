;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for HTTP-CLIENT-SUBSTRATE version 3.1
;;; Reason: Function HTTP::SEND-REQUEST:  No longer repackage header-writer; just call function directly, when appropriate.
;;; Function HTTP::WITH-HTTP-REQUEST: Eliminate functional repackaging.
;;; Function HTTP:INVOKE-HTTP-SERVICE:  Update argument names and documentation.
;;; Function (CLOS:METHOD HTTP:INVOKE-HTTP-SERVICE (HTTP::BASIC-CLIENT-MIXIN T T T)):  -
;;; Function (CLOS:METHOD HTTP:INVOKE-HTTP-SERVICE (HTTP::BASIC-CLIENT-MIXIN T T T)):  -
;;; Function HTTP:WITH-HTTP-REQUEST:  -
;;; Function (CLOS:METHOD HTTP:INVOKE-HTTP-SERVICE (URL:HTTP-URL T T T)):  -
;;; Remove function (CLOS:METHOD HTTP:INVOKE-HTTP-SERVICE (T T LIST T)): no longer needed.
;;; Written by JCMa, 11/16/99 23:14:13
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.8, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.19,
;;; W3 Presentation System 8.0, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 41, HTTP Proxy Server 4.0,
;;; HTTP Client Substrate 3.0, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.11, DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x994 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189585,
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.24),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:CLIENT;CLIENT.LISP.223")


(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL)
  (SCT:REQUIRE-PATCH-LEVEL-FOR-PATCH '(CL-HTTP 70. 20.)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CLIENT.LISP.223")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;;;------------------------------------------------------------------- 
;;;
;;; REQUEST
;;;

(defun send-request (stream method url http-version request-headers &optional proxy-p)
  ;; write the request
  (fast-format stream "~A " method)
  ;; write request url
  (when proxy-p
    (write-url-context url stream))
  (write-string (relative-name-string url) stream)	;avoids escaping problems for broken urls
  ;; write version
  (fast-format stream " ~A" http-version)
  ;; end the request line
  (send-cr-line-feed stream)
  ;; send the headers
  (etypecase request-headers
    (function (funcall request-headers stream method http-version))
    (list (write-request-header-plist stream request-headers method http-version)))
  ;; end the headers
  (send-cr-line-feed stream)
  (force-output stream))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CLIENT.LISP.223")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;; formerly invoke-http-service-on-host
(define-generic invoke-http-service (client method header-writer response-handler &optional request-entity-generator http-version)
  (:documentation "Invokes HTTP service for client on the remote host.
header-writer is a function called with (stream method http-version) 
to write the request headers to stream or it is a header plist.
RESPONSE-HANDLER is called with (CLIENT STREAM HTTP-VERSION)
REQUEST-ENTITY-GENERATOR is a function that transmits the body of an HTTP request.
It is called with (CLIENT STREAM HTTP-VERSION). Output is automatically forced on
STREAM.")) 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CLIENT.LISP.223")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod invoke-http-service ((client basic-client-mixin) method header-writer response-handler
                                &optional request-entity-generator (http-version *client-http-version*) &aux request-rejected-p)
  (flet ((trace-request (url method version &aux (trace-stream *trace-output*))
           (format trace-stream "~&Host: ~A~%Request: ~A " (host-string url) method)
           (url:write-local-name url trace-stream)
           (format trace-stream " ~A" version)))
    (with-current-connection (client :http-version http-version :connection-var connection)
      (let* ((url (client-url client))
             (request-version (connection-version connection))
             (stream (connection-stream connection)))
        ;; remember transmitted headers
        (setf (client-method client) method)
	;; send a request to the remote host
	(send-request stream method url request-version header-writer (client-proxy client))
        ;; Trace the request
        (when *trace-client* (trace-request url method request-version))
        ;; send the request body when provided
        (when request-entity-generator
          (case request-version
            ((:http/1.0 :http/0.9)  ;; 1.0 remote server, just send the data.
             (funcall request-entity-generator client stream request-version)
             (force-output stream))
            (t (with-client-line-buffer ()
                 (let ((reply (read-reply-line stream *client-line-buffer*)))
                   (multiple-value-bind (status response-reason)
                       (parse-reply reply)
                     (setf (client-status client) status
                           (client-reason client) response-reason)
                     (read-delimited-line stream '(#\Linefeed #\Return) nil *client-line-buffer*) ;; no headers
                     (case status
                       (100
                         (funcall request-entity-generator client stream request-version)
                         (force-output stream))
                       (t (setq request-rejected-p t)))))))))
        ;; handle the server response.
        (cond (request-rejected-p   ;; failed at continue checkpoint
               (funcall response-handler client stream request-version))
              (t (with-client-line-buffer ()
                   (let ((reply (read-reply-line stream)))
                     (multiple-value-bind (status response-reason server-version)
                         (parse-reply reply)
                       (setf (client-status client) status
                             (client-reason client) response-reason)
                       (with-headers-for-client (client stream server-version)
                         ;; the values returned by the handler must be the values of the form.
                         (funcall response-handler client stream request-version)))))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CLIENT.LISP.223")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;; formerly with-remote-resource
(define-macro with-http-request ((url-or-client method &key request-headers request-body) &body response-body)
  "Invokes HTTP service for URL-OR-CLIENT using METHOD.
REQUEST-HEADERS is a function which is called with (STREAM METHOD HTTP-VERSION)
to write the request headers or a header plist.
RESPONSE-BODY is the response-handler for the request.  It can access the
lexical variables: (CLIENT REMOTE-STREAM HTTP-VERSION).
REQUEST-BODY is an advanced option that accepts a s-expression to transmit a
request body. It can access the lexical variables: (CLIENT REMOTE-STREAM
HTTP-VERSION). Output is automatically forced on REMOTE-STREAM."
  `(flet ((response-handler (client remote-stream http-version)
            client http-version remote-stream   ; possibly ignore
            ,@response-body)
          ,@(when request-body
              `((request-handler (client remote-stream http-version)
                                 client http-version    ; possibly ignore
                                 ,request-body))))
     (declare (dynamic-extent #'response-handler))
     (invoke-http-service ,url-or-client ,method
                          ,request-headers
                          #'response-handler
                          ,(when request-body `(function request-handler)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CLIENT.LISP.223")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod invoke-http-service ((url http-url) method header-writer response-handler &optional request-entity-generator
				(http-version *client-http-version*))
  (with-client (url :client-var *client*)
    (invoke-http-service *client* method header-writer response-handler request-entity-generator http-version)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CLIENT.LISP.223")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod invoke-http-service (url method (header-writer list) response-handler &optional request-entity-generator
				    (http-version *client-http-version*))
  (flet ((request-header-writer (http-version)
           (declare (ignore http-version))
           header-writer))
    (invoke-http-service url method #'request-header-writer response-handler request-entity-generator http-version)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;CLIENT.LISP.223")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(SCL:FUNDEFINE '(METHOD INVOKE-HTTP-SERVICE (T T LIST T)))
