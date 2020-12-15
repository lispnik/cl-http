;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for W4 version 41.1
;;; Reason: Function (CLOS:METHOD W4::PERFORM-ACTION (URL:HTTP-URL W4::OPEN-HTTP-ACTION W4::ACTIVITY)):  update for with-http-request.
;;; Function HTTP::%GET-URL-HEADERS:  -
;;; Function HTTP::%GET-URL-HEADERS-AND-BODY:  -
;;; Function (CLOS:METHOD W4::PERFORM-ACTION (URL:HTTP-URL W4::OPEN-HTTP-ACTION W4::ACTIVITY)):  -
;;; Written by JCMa, 11/18/99 19:36:23
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.8, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.20,
;;; W3 Presentation System 8.0, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 41, HTTP Proxy Server 4.1,
;;; HTTP Client Substrate 3.1, Color 427.1, Graphics Support 431.0,
;;; Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Color System Documentation 10.0, SGD Book Design 10.0, HTTP Client 49.1,
;;; Image Substrate 440.4, W4 Constraint-Guide Web Walker 41.0, W4 Examples 13.0,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.11,
;;; DEC OSF/1 V4.0 (Rev. 110),
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
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48),
;;; Add clos to mx list methods (from W:>Reti>add-clos-to-mx-list-methods.lisp.1),
;;; Its end of line patch (from W:>Reti>its-end-of-line-patch.lisp.3),
;;; Unix inbox from spoofing patch (from W:>Reti>unix-inbox-from-spoofing-patch.lisp.13),
;;; hack to treat namespace as a partial cache of domain (from W:>hes>fixes>partial-namespace-domain.lisp.5),
;;; Popup patch (from W:>Reti>popup-patch.lisp.1),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:CLIENT;W4-CLIENT.LISP.166"
  "HTTP:W4;WALKER.LISP.50")


(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL)
  (SCT:REQUIRE-PATCH-LEVEL-FOR-PATCH '(HTTP-CLIENT-SUBSTRATE 3. 1.)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W4;WALKER.LISP.50")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-")

(defmethod perform-action ((url url:http-url) (action open-http-action) (activity activity))
  (with-slots (function arguments follow-redirects-p method) action
    (let ((outgoing-headers `(:host (,(url:host-string url) ,(url:host-port url))
			      ,.(activity-outgoing-headers action))))
      (declare (dynamic-extent outgoing-headers))
      (handler-case-if 
	  *debug-walker*
	 (if follow-redirects-p
	     (handling-redirects (url)
	       (with-http-request (url method)
		 outgoing-headers
		 (apply function action activity url http::remote-stream arguments)))
	     (with-http-request (url method)
	       outgoing-headers
	       (apply function action activity url http::remote-stream arguments)))
	(client-condition (cond) (activity-report-condition activity url cond))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;W4-CLIENT.LISP.166")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;;;------------------------------------------------------------------- 
;;;
;;; METHODS FOR WEB WALKER
;;;

(defun %get-url-headers (url headers report-stream authorization)
  (handling-redirects (url)
    (with-http-request (url :head 
                            :request-headers (compute-standard-request-headers 
                                               url :authorization authorization :header-plist headers
					       :user-agent (if (getf headers :user-agent) nil *server-version*)))
      (let ((status (client-status client))
            (http-version (client-connection-version client))
            (response-headers *headers*)
            redirection)
        (case status
          ((200 203 204 205 206))
          ((301 302)
           (let ((alternate-urls (mapcar #'url:intern-url (ensure-list (or (get-header :location response-headers) 
                                                                           (get-header :content-location response-headers))))))
             (push alternate-urls redirection)
             (signal (ecase status
                       (301 'document-moved-permanently)
                       (302 'document-moved-temporarily))
                     :new-urls alternate-urls :version http-version)))
          ((402 403 405 406 407))
          (404
            (when *debug-client*
              (fresh-line report-stream)
              (%write-common-logfile-entry (host-string url) (concatenate 'string (url:name-string url) " HEAD")
                                           status  0 "-"  *log-times-in-gmt* report-stream)))
          ;; do something about authentication -- JCMa 12/10/1996.
          (401 (destructuring-bind (&optional authentication-method . realm) (get-header :WWW-Authenticate response-headers)
                 (declare (ignore authentication-method realm))
                 nil))
          ((nil) (setq status 408))             ; didn't return a status code
          ((408 500 501 502 503 504 505))
          (t (client-signal-http-code url status :head :headers response-headers :reason (client-reason client) :version http-version)))
        ;; return values for walker
        (values (durable-response-headers client) status redirection http-version)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;W4-CLIENT.LISP.166")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun %get-url-headers-and-body (url headers report-stream authorization)
  (flet ((standard-capture (stream copy-mode length)
	   (ecase copy-mode
	     ((:text :crlf)
	      (crlf-stream-copy-into-string stream length))
	     (:binary
	       (with-binary-stream (stream :input)
		 (binary-stream-copy-into-8-bit-array stream length))))))
    (declare (inline standard-capture))
    (handling-redirects (url)
      (with-http-request (url :get 
			      :request-headers (compute-standard-request-headers
						 url :authorization authorization :header-plist headers
						 :user-agent (if (getf headers :user-agent) nil *server-version*)))
	(let ((status (client-status client))
	      (http-version (client-connection-version client))
	      (response-headers (client-response-headers client))
	      response-body redirection)
	  (case status
	    ((200 205 206)
	     (let* ((content-type (get-header :content-type response-headers))
		    (copy-mode (mime-content-type-copy-mode content-type))
		    (content-length (get-header :content-length response-headers)))
	       (setq response-body (cond ((or content-length (member http-version '(:http/1.0 :http/0.9)))
					  (standard-capture remote-stream copy-mode content-length))
					 (t (let ((transfer-encoding (get-header :transfer-encoding response-headers)))
					      (case transfer-encoding
						(:chunked (chunked-input-capture remote-stream copy-mode response-headers))
						((nil) (error 'bad-syntax-provided "No content length header was provided."))
						(t (error 'server-not-implemented :close-connection t :url url
							  :format-string "The HTTP transfer decoding, ~A, is not implemented."
							  :format-args (list transfer-encoding))))))))))
	    ((201 202 203 204))
	    ((300 402 403 405 406 407 415))
	    ((301 302)
	     (let ((alternate-urls (mapcar #'url:intern-url (ensure-list (or (get-header :location response-headers) 
									     (get-header :content-location response-headers))))))
	       (flush-input-entity remote-stream response-headers http-version)
	       (push alternate-urls redirection)
	       (signal (ecase status
			 (301 'document-moved-permanently)
			 (302 'document-moved-temporarily))
		       :new-urls alternate-urls :version http-version)))
	    ;; do something about authentication -- JCMa 12/10/1996.
	    (401 (destructuring-bind (&optional authentication-method . realm) (get-header :WWW-Authenticate response-headers)
		   (declare (ignore authentication-method realm))
		   nil))
	    (404
	      (when *debug-client*
		(fresh-line report-stream)
		(%write-common-logfile-entry (host-string url) (concatenate 'string (url:name-string url) " GET")
					     status 0 "-" *log-times-in-gmt* report-stream)))
	    ((nil) (setq status 408))		; didn't return a status code
	    ((408 411 414 500 501 502 503 504 505))
	    (t (client-signal-http-code url status :get :headers response-headers :reason (client-reason client) :version http-version)))
	  (values response-body (durable-response-headers client) status redirection http-version))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W4;WALKER.LISP.50")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-")

(defmethod perform-action ((url url:http-url) (action open-http-action) (activity activity))
  (with-slots (function arguments follow-redirects-p method) action
    (let ((outgoing-headers `(:host (,(url:host-string url) ,(url:host-port url))
			      ,.(activity-outgoing-headers action))))
      (declare (dynamic-extent outgoing-headers))
      (handler-case-if 
	  *debug-walker*
	 (if follow-redirects-p
	     (handling-redirects (url)
	       (with-http-request (url method)
		 outgoing-headers
		 (apply function action activity url http::remote-stream arguments)))
	     (with-http-request (url method)
	       outgoing-headers
	       (apply function action activity url http::remote-stream arguments)))
	(client-condition (cond) (activity-report-condition activity url cond))))))

