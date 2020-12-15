;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for HTTP-BASE-CLIENT version 49.2
;;; Reason: Update callers of HTTP::WITH-HTTP-REQUEST to eliminate functional repackaging
;;; of header-writer.
;;; 
;;; Function HTTP::%DELETE-URL: -
;;; Function HTTP::%POST-URL:  -
;;; Function HTTP::%PUT-URL:  -
;;; Function HTTP::%SHOW-URL:  -
;;; Function HTTP::%SHOW-URL-HEADERS:  -
;;; Function HTTP::%SHOW-URL-OPTIONS:  -
;;; Function HTTP::%SHOW-URL-TRACE:  -
;;; Function (CLOS:METHOD HTTP::ACCESS-URL (HTTP::POST-TRANSACTION-CONTROL (EQL :POST) T T T)):  -
;;; Function (CLOS:METHOD HTTP::ACCESS-URL (T (EQL :GET) T T T)):  -
;;; Function (CLOS:METHOD HTTP::ACCESS-URL (T (EQL :HEAD) T T T)):  -
;;; Function (CLOS:METHOD HTTP:CAPTURE-RAW-URL (URL:HTTP-URL)):  -
;;; Written by JCMa, 11/17/99 20:54:05
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
;;; HTTP Client Substrate 3.0, Color 427.1, Graphics Support 431.0,
;;; Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Color System Documentation 10.0, SGD Book Design 10.0, HTTP Client 49.1,
;;; Image Substrate 440.4, Ivory Revision 5, VLM Debugger 329, Genera program 8.11,
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
;;; Popup patch (from W:>Reti>popup-patch.lisp.1).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:CLIENT;FLOGGER.LISP.50"
  "HTTP:CLIENT;SEXP-BROWSER.LISP.57")


(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL)
  (SCT:REQUIRE-PATCH-LEVEL-FOR-PATCH '(HTTP-CLIENT-SUBSTRATE 3. 1.)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.57")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;;;------------------------------------------------------------------- 
;;;
;;;  DELETE METHOD
;;;

(defun %delete-url (url headers stream)
  (handler-case
    (handling-redirects (url)
      (handling-authentication (authorization)
	(with-http-request
	  (url :delete 
	       :request-headers (compute-standard-request-headers
				  url :authorization authorization :header-plist headers))
	  (when *debug-client*
	    (fresh-line stream)
	    (print-headers stream)
	    (terpri stream))
	  (case (client-status client)
	    (200
	      (with-transfer-decoding* (remote-stream url http-version :headers *headers*)
		(destructuring-bind (major-type minor-type) (get-header :content-type)
		  (display url major-type minor-type remote-stream stream)))
	      (values url :deleted))
	    (202
	      (values url :accepted))
	    (204
	      (values url :deleted))
	    (t (client-signal-http-code url (client-status client) :delete 
					:reason (client-reason client) :version http-version))))))
    (http-condition (cond) (www-utils:report-condition cond stream))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.57")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun %post-url (url vector headers stream)
  (flet ((send-data (remote-stream vector content-length)
           (with-binary-stream (remote-stream :output)
	     (write-vector remote-stream vector 0 content-length)
	     (force-output remote-stream))))
    (handler-case
      (let* ((content-length (fill-pointer vector))
	     (outgoing-headers `(,@headers :content-type (:application :x-www-form-urlencoded)
				 :content-length ,content-length)))
	(handling-redirects (url)
	  (handling-authentication (authorization)
	    (with-http-request
	      (url :post
		   :request-headers (compute-standard-request-headers
				      url :authorization authorization :header-plist outgoing-headers)
		   :request-body (send-data remote-stream vector content-length))
	      (case (client-status client)
		(200
		  (with-transfer-decoding* (remote-stream url http-version :headers *headers*)
		    (destructuring-bind (major-type minor-type &key &allow-other-keys)
			(get-header :content-type)
		      (display url major-type minor-type remote-stream stream))))
		(204
		  (values url nil))
		(t (client-signal-http-code url (client-status client) :post :reason (client-reason client)
					    :version http-version)))))))
      (http-condition (cond) (www-utils:report-condition cond stream)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.57")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;;;------------------------------------------------------------------- 
;;;
;;; PUT METHOD
;;; 

;; Handles the 1.1 PUT and the 1.0 cretenous PUT
(defun %put-url (url resource-writer content-length content-type headers stream version)
  (flet ((send-data (writer remote-stream url content-length)
           (with-transfer-encoding (remote-stream (if content-length
						      :fixed-length :chunked))
	     (funcall writer url remote-stream))
           (force-output remote-stream)))
    (handler-case
      (let ((outgoing-headers `(,@headers
                                ,.(cond ((eq version :overwrite) nil)
                                        ((numberp version) `(:derived-from ,version))
                                        (t nil))
                                :content-type ,content-type
                                ,@(if content-length
				      `(:content-length ,content-length)
				      `(:transfer-encoding :chunked)))))
        (declare (dynamic-extent outgoing-headers))
        (handling-redirects (url)
	  (handling-authentication (authorization)
	    (with-http-request
	      (url :put 
		   :request-headers (compute-standard-request-headers
				      url :authorization authorization :header-plist outgoing-headers)
		   :request-body (send-data resource-writer remote-stream url content-length))
	      (with-slots (status reason) client
		(case status
		  ((200 201 204)
		   (let ((content-location (get-header :content-location))
			 (content-version (get-header :content-version))
			 (last-modified (get-header :last-modified))
			 (keyword (if (eql status 201) :created :modified)))
		     (when content-location
		       (setq content-location (intern-url content-location :if-does-not-exist :create)))
		     (case status
		       (200 (with-transfer-decoding* (remote-stream url http-version :headers *headers*)
			      (destructuring-bind (major-type minor-type)
				  (get-header :content-type)
				(display (or content-location url) major-type minor-type remote-stream stream)))))
		     (values (or content-location url) keyword content-version last-modified)))
		  (t (client-signal-http-code url status :put :reason reason :version http-version))))))))
      (http-condition (cond) (www-utils:report-condition cond stream)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.57")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;;;------------------------------------------------------------------- 
;;;
;;;  STANDARD GET METHOD
;;; 

(defun %show-url (url headers stream &key raw-output-p start end)
  (let ((range (when (and start end)
		 `(,start ,end))))
    (declare (dynamic-extent range))
    (handler-case
      (handling-redirects (url)
	(handling-authentication (authorization)
	  (with-http-request
	    (url :get 
		 :request-headers (compute-standard-request-headers
				    url :authorization authorization :range range :header-plist headers))
	    (if raw-output-p
		(display-raw-output client *headers* stream)
		(with-status-code-dispatch (:client client :url url :status (client-status client)
						    :success-status-codes (200 203 205 206)
						    :exceptions-flush-entities t) 
		  (with-transfer-decoding* (remote-stream url http-version :headers *headers*)
		    (destructuring-bind (major-type minor-type &key &allow-other-keys)
			(get-header :content-type *headers*)
		      (display url major-type minor-type remote-stream stream))))))))
      (http-condition (cond) (www-utils:report-condition cond stream)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.57")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;;;------------------------------------------------------------------- 
;;;
;;; STANDARD HEAD METHOD
;;;

(defun %show-url-headers (url headers stream)
  (handler-case
    (handling-redirects (url)
      (handling-authentication (authorization)
	(with-http-request
	  (url :head 
	       :request-headers (compute-standard-request-headers url :authorization authorization :header-plist headers))
	  remote-stream				;ignore 
	  (with-status-code-dispatch (:client client :url url :status (client-status client) :exceptions-flush-entities nil)
	    (fresh-line stream)
	    (print-headers stream *headers*)
	    (terpri stream)))))
    (http-condition (cond) (www-utils:report-condition cond stream))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.57")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;;;------------------------------------------------------------------- 
;;;
;;; OPTIONS METHOD
;;;

(defun %show-url-options (url headers stream)
  (handler-case
    (handling-redirects (url)
      (handling-authentication (authorization)
	(with-http-request
	  (url :options 
	       :request-headers (compute-standard-request-headers
				  url :authorization authorization :header-plist headers))
	  remote-stream				;ignore
	  (with-status-code-dispatch (:client client :url url :status (client-status client) :exceptions-flush-entities t)
	    (fresh-line stream)
	    (print-headers stream *headers*)
	    (terpri stream)))))
    (http-condition (cond) (www-utils:report-condition cond stream))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.57")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;;;------------------------------------------------------------------- 
;;;
;;; TRACE METHOD
;;;

(defun %show-url-trace (url headers stream &key (max-forwards 5.))
  (handler-case
    (handling-redirects (url)
      (handling-authentication (authorization)
	(with-http-request
	  (url :trace
	       :request-headers (compute-standard-request-headers
				  url :authorization authorization 
				  :header-plist `(,@(when max-forwards `(:max-forwards ,max-forwards)) ,@headers)))
	  (with-status-code-dispatch (:client client :url url :status (client-status client) :exceptions-flush-entities t)
	    (fresh-line stream)
	    (print-headers stream *headers*)
	    (terpri stream)
	    (with-transfer-decoding* (remote-stream url http-version :headers *headers*)
	      (stream-decode-crlf-until-eof remote-stream stream))))))
    (http-condition (cond) (www-utils:report-condition cond stream))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;FLOGGER.LISP.50")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod access-url ((transaction-control post-transaction-control) (http-method (eql :post)) http-version persistent-connections-p stream)
  (declare (ignore http-version persistent-connections-p report-stream))
  (flet ((send-data (remote-stream vector content-length)
           (with-binary-stream (remote-stream :output)
	     (write-vector remote-stream vector 0 content-length)
	     (force-output remote-stream))))
    (let* ((url (transaction-control-url transaction-control))
	   (headers (transaction-control-post-headers transaction-control))
	   (vector (transaction-control-form-data-vector transaction-control)))
      (handler-case
	(handling-redirects (url)
	  (handling-authentication (authorization)
	    (with-http-request
	      (url :post
		   :request-headers (compute-standard-request-headers url :authorization authorization :header-plist headers)
		   :request-body (send-data remote-stream vector (fill-pointer vector)))
	      (with-status-code-dispatch (:client client :url url :status (client-status client)
						  :success-status-codes (200)
						  :exceptions-flush-entities t
						  :http-version http-version) 
		(with-transfer-decoding* (remote-stream url http-version :headers *headers*)
		  (advance-input-buffer remote-stream))))))
	(http-condition (cond) (www-utils:report-condition cond stream))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;FLOGGER.LISP.50")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod access-url (transaction-control (http-method (eql :get)) http-version persistent-connections-p report-stream)
  (declare (ignore http-version persistent-connections-p report-stream))
  (let ((url (transaction-control-url transaction-control))
	(headers (transaction-control-headers transaction-control)))
    (handling-redirects (url)
      (handling-authentication (authorization)
	(with-http-request
	  (url :get :request-headers (compute-standard-request-headers url :authorization authorization :header-plist headers))
	  (with-status-code-dispatch (:client client :url url :status (client-status client)
					      :success-status-codes (200 203 205 206)
					      :exceptions-flush-entities t
					      :http-version http-version) 
	    (with-transfer-decoding* (remote-stream url http-version :headers *headers*)
	      (advance-input-buffer remote-stream))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;FLOGGER.LISP.50")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod access-url (transaction-control (http-method (eql :head)) http-version persistent-connections-p report-stream)
  (declare (ignore http-version persistent-connections-p report-stream))
  (let ((url (transaction-control-url transaction-control))
	(headers (transaction-control-headers transaction-control)))
    (handling-redirects (url)
      (handling-authentication (authorization)
	(with-http-request
	  (url :head 
	       :request-headers (compute-standard-request-headers url :authorization authorization :header-plist headers))
	  remote-stream				;ignore 
	  (with-status-code-dispatch (:client client :url url :status (client-status client)
					      :exceptions-flush-entities nil :http-version http-version)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;SEXP-BROWSER.LISP.57")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod capture-raw-url ((url url:http-url) &key headers (display-stream *standard-output*)  start end)
  (let ((range (when (and start end)
		 `(,start ,end))))
    (declare (dynamic-extent range))
    (handler-case
      (handling-redirects (url)
	(handling-authentication (authorization)
	  (with-http-request
	    (url :get :request-headers (compute-standard-request-headers url :authorization authorization :range range :header-plist headers)) 
	    (let ((remote-stream (client-stream client))
		  (status (client-status client))
		  (http-version (client-connection-version client)))
	      (format display-stream "~&Status Code: ~D (~A)~%Server Version: ~(~A~)" status (get-string-for-status-code status) http-version)
	      (fresh-line display-stream) 
	      (print-headers display-stream *headers*)
	      (terpri display-stream)
	      (flet ((stream-capture-until-eof (from-stream ignore copy-mode)
		       (declare (ignore ignore))
		       (let ((content-length (get-header :content-length)))
			 (ecase copy-mode
			   ((:text :crlf)
			    (crlf-stream-copy-into-string from-stream content-length))
			   (:binary
			     (with-binary-stream (from-stream :input)
			       (binary-stream-copy-into-8-bit-array from-stream content-length)))))))
		(let ((copy-mode (mime-content-type-copy-mode (get-header :content-type *headers*))))
		  (with-transfer-decoding* (remote-stream (client-url client) http-version :headers *headers* :copy-mode copy-mode
							  :stream-functions '(stream-capture-until-eof))
		    (stream-capture-until-eof remote-stream nil copy-mode))))))))
      (http-condition (cond) (www-utils:report-condition cond display-stream))))) 

