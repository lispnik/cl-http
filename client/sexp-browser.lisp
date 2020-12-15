;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-

;;; (C) Copyright 1996-99, John C.Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CL-HTTP S-EXPRESSION BROWSER
;;; 

(in-package :http)

;; Export here to preserve modularity in the primary package definitions.
(mapc #'(lambda (x) (export (intern x :http) :http))
      '("CAPTURE-RAW-URL"
	"DELETE-URL"
	"POST-URL"
	"PUT-URL"
	"SHOW-RAW-URL" 
	"SHOW-RAW-URL-TRACE"
	"SHOW-RAW-URL-OPTIONS"             
	"SHOW-URL"
	"SHOW-URL-HEADERS"))

;;;------------------------------------------------------------------- 
;;;
;;; GENERIC METHOD TO DISPLAY CONTENT ON A STREAM
;;; 

(defgeneric display (url major-type minor-type remote-stream local-stream)
  (:documentation "The primary method for displaying URL on LOCAL-STREAM¬
according to the MIME content type MAJOR-TYPE/MINOR-TYPE, where the data is
read from REMOTE-STREAM.")) 

(defmethod display ((url url:url) major-type minor-type remote-stream local-stream)
  (declare (ignore remote-stream local-stream))
  (error 'unsupported-media-type :url url
         :format-string "No display method defined for the MIME content type ~A/~A."
         :format-args (list major-type minor-type)))

(defmethod display ((http-url url:http-url) (major-type (eql :text)) (minor-type (eql :plain)) remote-stream stream)
  (fresh-line stream)
  (stream-decode-crlf-until-eof remote-stream stream))

(defmethod display ((http-url url:http-url) (major-type (eql :text)) (minor-type (eql :html)) remote-stream stream)
  (fresh-line stream)
  (stream-decode-crlf-until-eof remote-stream stream))

(defmethod display ((http-url url:http-url) (major-type (eql :text)) (minor-type (eql :uri-list)) remote-stream stream)
  (fresh-line stream)
  (stream-decode-crlf-until-eof remote-stream stream))

(defmethod display ((url url:http-url) (major-type (eql :image)) minor-type remote-stream local-stream)
  (declare (ignore remote-stream minor-type))
  (html:note-anchor "Image Not Shown" :reference url :stream local-stream))

(defmethod display ((url url:http-url) (major-type (eql :audio)) minor-type remote-stream local-stream)
  (declare (ignore remote-stream minor-type))
  (html:note-anchor "Audio Not Shown" :reference url :stream local-stream)) 

(defmethod display ((url url:http-url) (major-type (eql :video)) minor-type remote-stream local-stream)
  (declare (ignore remote-stream minor-type))
  (html:note-anchor "Video Not Shown" :reference url :stream local-stream))

(defmethod display ((url url:http-url) (major-type (eql :application)) minor-type remote-stream local-stream)
  (declare (ignore remote-stream minor-type))
  (html:note-anchor "Application Not launched" :reference url :stream local-stream))

;; Used to return values to an application from the POST Method.
(defmethod display ((url url:http-url) (major-type (eql :application)) (minor-type (eql :lisp-sexp)) remote-stream local-stream)
  (declare (ignore local-stream))
  (let ((*read-eval* nil))
    (read remote-stream nil nil))) 

;;;------------------------------------------------------------------- 
;;;
;;; AUTHENTICATION
;;; 

(defvar *realm-username-table* (make-hash-table :test #'equalp))

(defgeneric encode-realm-username (realm user-name password))

(defmethod encode-realm-username (realm user-name password)
  (declare (ignore realm))
  `(,user-name ,password))

(defgeneric decode-realm-username (realm entry)
  (declare (values user-name pw)))

(defmethod decode-realm-username (realm entry)
  (declare (ignore realm))
  (values-list entry))

(defgeneric pw-data (url realm method)
  (declare (values user-name pw)))

(defmethod pw-data (url (realm string) (method (eql :basic)))
  (declare (ignore url))
  (let (entry)
    (when (setq entry (gethash realm *realm-username-table*))
      (decode-realm-username realm entry))))

(defmethod pw-data (url (realm cons) (method (eql :basic)))
  (destructuring-bind (realm-name &key &allow-other-keys) realm
    (pw-data url realm-name method)))

(defmethod pw-data ((url url:http-url) (realm cons) (method (eql :digest)))
  (destructuring-bind (realm-name &key &allow-other-keys) realm
    (let ((table *realm-username-table*)
	  entry)
      (when (setq entry (or (gethash (url:name-string url) table)
			    (gethash (url:local-context-string url) table)
			    (gethash (url::root-context-string url) table)
			    (gethash realm table)))
	(decode-realm-username realm-name entry)))))

(defgeneric cache-pw-data (url realm method user-name password))

(defmethod cache-pw-data ((url string) realm method user-name password)
  (cache-pw-data (url:intern-url url :if-does-not-exist :create) realm method user-name password))

(defmethod cache-pw-data (url (realm realm) method user-name password)
  (cache-pw-data url (realm-name realm) method user-name password))

(defmethod cache-pw-data (url (realm string) (method (eql :basic)) user-name password)
  (declare (ignore url))
  (let ((entry (encode-realm-username realm user-name password)))
    (setf (gethash realm *realm-username-table*) entry)))

(defmethod cache-pw-data (url (realm cons) (method (eql :basic)) user-name password)
  (destructuring-bind (realm-name &key &allow-other-keys) realm
    (cache-pw-data url realm-name method user-name password)))

(defmethod cache-pw-data (url realm (method (eql :digest)) user-name password)
  (destructuring-bind (realm-name &key domain &allow-other-keys) realm
    (let ((entry (encode-realm-username realm-name user-name password))
	  (table *realm-username-table*))
      (dolist (uri domain)
	(setf (gethash uri table) entry))
      (setf (gethash (url:name-string url) table) entry
	    (gethash (url:local-context-string url) table) entry
	    (gethash (url::root-context-string url) table) entry
	    (gethash realm-name table) entry))))

(defparameter *prompt-user-for-passwords* t
  "Controls whether the user is prompted for passwords.")

(defmacro without-password-prompting (() &body body)
  `(let ((*prompt-user-for-passwords* nil)) . ,body))

(defgeneric get-user-name+pw (url realm method reprompt-p &optional stream)
  (declare (values user-name password))
  (:documentation "Returns the USER-ID and PASSWORD for URL given REALM
and authentication method, METHOD, prompting for input on STREAM."))

(defmethod get-user-name+pw ((url url:http-url) (realm string) method reprompt-p &optional (stream *terminal-io*))
  (declare (ignore reprompt-p))
  (cond (*prompt-user-for-passwords*
	 (let ((string (url:name-string url)))
	   #-(OR Genera MCL) 
	   (%get-user-name+pw string stream)
	   #+(OR Genera MCL)
	   (%get-user-name+pw string realm method stream)))
	(t (let ((client *client*))
	     (error 'recoverable-unauthorized-client-access
		    :url url
		    :method (client-method client)
		    :authentication-method method
		    :authentication-realm realm
		    :headers *headers*
		    :reason "No user to prompt for password."
		    :version (client-connection-version client))))))

(defmethod get-user-name+pw ((url url:http-url) (realm cons) (method (eql :basic)) reprompt-p &optional (stream *terminal-io*))
  (destructuring-bind (realm-name) realm
    (get-user-name+pw url realm-name method reprompt-p stream)))

(defmethod get-user-name+pw ((url url:http-url) (realm cons) (method (eql :digest)) reprompt-p &optional (stream *terminal-io*))
  (destructuring-bind (realm-name &rest plist) realm
    (declare (ignore plist))
    (get-user-name+pw url realm-name method reprompt-p stream)))

(defmethod get-user-name+pw :around ((url url:http-url) (realm cons) method reprompt-p &optional (stream *terminal-io*))
  (unless reprompt-p
    (multiple-value-bind (user-name password)
	(pw-data url realm method)
      (when (and user-name password)
	(return-from get-user-name+pw (values user-name password)))))
  ;; if no cache hit, then prompt the user.
  (multiple-value-bind (user-name password)
      (call-next-method url realm method stream)
    (cache-pw-data url realm method user-name password)
    (values user-name password)))

(define-variable *basic-authorization-header-cache* (make-hash-table :test #'equal))

(define clear-authentication-caches ()
  "Clears all authentication caches."
  (clrhash *basic-authorization-header-cache*)
  (clrhash *realm-username-table*)
  (map-url-table #'(lambda (key url)
		     (declare (ignore key))
		     (clear-authorization-header-cache url))))

(defgeneric cache-authorization-header-for-realm (url method authentication-method realm authorization-header)
  (declare (values authorization-header))
  (:documentation "Caches AUTHORIZATION-HEADER for future use with URL, HTTP-METHOD, AUTHENTICATION-METHOD, or REALM."))

(defmethod cache-authorization-header-for-realm ((url url:http-url) method (authentication-method (eql :basic))
						 (realm string) authorization-header)
  (declare (ignore method))
  (setf (gethash realm *basic-authorization-header-cache*) authorization-header)
  authorization-header)

(defmethod cache-authorization-header-for-realm ((url url:http-url) method (authentication-method (eql :digest)) realm authorization-header)
  (declare (ignore realm))
  (let ((entry (get-value url :authorization-header-cache-plist)))
    (cond (entry
	   (setf (getf entry method) authorization-header))
	  (t (setf (get-value url :authorization-header-cache-plist) `(,method ,authorization-header)))))
  authorization-header)

(defgeneric recall-authorization-header-for-realm (url method authentication-method realm)
  (declare (values authorization-header found-p)))

(defmethod recall-authorization-header-for-realm ((url url:http-url) method (authentication-method (eql :basic)) realm)
  (declare (ignore method))
  (destructuring-bind (realm-name) realm
    (gethash realm-name *basic-authorization-header-cache*)))

(defmethod recall-authorization-header-for-realm ((url url:http-url) method (authentication-method (eql :digest)) realm)
  (declare (ignore realm))
  (let ((entry (get-value url :authorization-header-cache-plist)))
    (when entry
      (getf entry method))))

(defgeneric invalidate-authorization-header-cache (url method authentication-method realm))

(defmethod invalidate-authorization-header-cache ((url url:http-url) method (authentication-method (eql :basic)) realm)
  (declare (ignore method))
  (destructuring-bind (realm-name) realm
    (remhash realm-name *basic-authorization-header-cache*)
    (remhash realm-name *realm-username-table*)))

(defmethod invalidate-authorization-header-cache ((url url:http-url) method (authentication-method (eql :digest)) realm)
  (declare (ignore realm))
  (let ((entry (get-value url :authorization-header-cache-plist)))
    (when entry
      (remf entry method)
      (unless entry
	(remove-value url :authorization-header-cache-plist)))))

(defgeneric clear-authorization-header-cache (url)
  (:documentation "Clears the authorization header cache on URL."))

(defmethod clear-authorization-header-cache ((url url:http-url))
  (remove-value url :authorization-header-cache-plist))

(define-generic get-authorization-header (url http-method authentication-method realm &optional recompute-p reprompt-p stream)
  (declare (values authorization-header found-in-cache-p))
  (:documentation "Returns a header plist suitable for returning with the authorization retry."))

(defmethod get-authorization-header ((url url:http-url) http-method method realm &optional recompute-p reprompt-p stream)
  (declare (ignore http-method realm stream recompute-p reprompt-p))
  (error "~S is an unknown authorization method for an HTTP URL." method))

(defmethod get-authorization-header ((url string) http-method method realm &optional recompute-p reprompt-p (stream *terminal-io*))
  (get-authorization-header (url:intern-url url :if-does-not-exist :error) http-method method realm recompute-p reprompt-p stream))

(defmethod get-authorization-header :around ((url url:http-url) http-method method realm 
					     &optional recompute-p reprompt-p (stream *terminal-io*))
  (cond (recompute-p
	 (invalidate-authorization-header-cache url http-method method realm)
	 (call-next-method url http-method method realm recompute-p reprompt-p stream))
	;; returns the header and found-in-cache-p
	((recall-authorization-header-for-realm url http-method method realm))
	(t (call-next-method url http-method method realm recompute-p reprompt-p stream))))

(defmethod get-authorization-header ((url url:http-url) http-method (method (eql :basic)) realm
				     &optional recompute-p reprompt-p (stream *terminal-io*))
  (declare (ignore recompute-p))
  (multiple-value-bind (user-id pw)
      (get-user-name+pw url realm method reprompt-p stream)
    (when (and user-id pw)
      (destructuring-bind (realm-name) realm
	(let ((hash (concatenate 'string user-id ":" pw)))
	  (declare (dynamic-extent hash))
	  (cache-authorization-header-for-realm
	    url http-method method realm-name `(:authorization (:basic ,(base64:base64-encode-vector hash :characters-p t)))))))))

(defmethod get-authorization-header ((url url:http-url) http-method (method (eql :digest)) realm
				     &optional recompute-p reprompt-p (stream *terminal-io*))
  (declare (ignore recompute-p))
  (multiple-value-bind (user-id pw)
      (get-user-name+pw url realm method reprompt-p stream)
    (when (and user-id pw)
      (destructuring-bind (realm-name &key algorithm nonce opaque &allow-other-keys) realm
	(let* ((digest-fctn (algorithm-digest-function algorithm))
	       (user-realm-pw (concatenate 'string user-id ":" realm-name ":" pw))
	       (user-realm-pw-digest (funcall digest-fctn user-realm-pw))
	       (uri-method (concatenate 'string (symbol-name http-method) ":" (relative-name-string url)))
	       (uri-method-digest (funcall digest-fctn uri-method))
	       (response (concatenate 'string user-realm-pw-digest ":" nonce ":" uri-method-digest))
	       (response-digest (funcall digest-fctn response)))
	  (declare (dynamic-extent user-realm-pw user-realm-pw-digest uri-method uri-method-digest response))
	  (cache-authorization-header-for-realm
	    url http-method method realm-name
	    `(:authorization
	       (:digest :username ,user-id :realm ,realm-name :nonce ,nonce
			:uri ,(name-string url) :response ,response-digest
			:opaque ,opaque :algorithmm ,algorithm))))))))

(defgeneric client-authenticate-user (condition &key recompute-authorization-header-p prompt-for-password-p stream)
  (declare (values authorization-header found-in-cache-p)))

(defmethod client-authenticate-user ((condition unauthorized-access) &key recompute-authorization-header-p
				     prompt-for-password-p (stream *terminal-io*))
  (let ((url (http-url condition))
        (http-method (http-method condition))
        (authentication-method (http-authentication-method condition))
        (realm (http-authentication-realm condition)))
    (get-authorization-header url http-method authentication-method realm
			      recompute-authorization-header-p prompt-for-password-p stream)))

(define-parameter *number-of-authentication-retries* 5
		  "The number of times to reprompt the user for bad passwords.")

;; Used to throw into a loop when user supplied wrong password, fixed.  -cvince 8/30/96
(define-macro handling-authentication ((authorization-var) &body body)
  "Handles authentication by rerunning body with AUTHENTICATION-VAR bound
to a user-specified authentication."
  `(loop with ,authorization-var
         with found-in-cache-p and recompute-header-p and reprompt-p
	 for tries upfrom 0
	 doing (handler-case-if (< tries *number-of-authentication-retries*)
		  (return (progn . ,body))
		 (unauthorized-client-access
		   (cond)
		   (multiple-value-setq (,authorization-var found-in-cache-p)
		     (client-authenticate-user
		       cond :recompute-authorization-header-p recompute-header-p
		       :prompt-for-password-p reprompt-p :stream *query-io*))
		   (unless ,authorization-var (return nil))
		   (when (or recompute-header-p (not found-in-cache-p))
		     (setq reprompt-p t))
		   (unless recompute-header-p
		     (setq recompute-header-p t))))))

;;;------------------------------------------------------------------- 
;;;
;;; UTILITIES
;;;

(defgeneric display-raw-output (client headers &optional display-stream))

(defmethod display-raw-output ((client client) headers &optional (display-stream *standard-output*))
  (let ((remote-stream (client-stream client))
	(status (client-status client))
	(http-version (client-connection-version client)))
    (format display-stream "~&Status Code: ~D (~A)~%Server Version: ~(~A~)" status (get-string-for-status-code status) http-version)
    (fresh-line display-stream) 
    (print-headers display-stream headers)
    (terpri display-stream)
    (with-transfer-decoding* (remote-stream (client-url client) http-version :headers headers)
      (destructuring-bind (major-type minor-type &key &allow-other-keys)
	  (get-header :content-type *headers*)
	(display (client-url client) major-type minor-type remote-stream display-stream)))))

(define-macro with-status-code-dispatch ((&key (client '*client*)
					       (headers '*headers*)
					       (status '(client-status client))
					       (url '(client-url client))
					       (success-status-codes '(200))
					       exceptions-flush-entities
					       (http-version '(client-connection-version client)))
					 &body body)
  `(let ((client ,client)
	 (status ,status)
	 (headers ,headers))
     (flet ((do-it () ,@body))
       (declare (dynamic-extent #'do-it))
       (case status
	 (,success-status-codes
	  (do-it))
	 ((301 302 303)
	  ,@(when exceptions-flush-entities
	      `((flush-input-entity (client-stream client) headers ,http-version)))
	  (signal (ecase status
		    (301 'document-moved-permanently)
		    (302 'document-moved-temporarily)
		    (303 'document-forwarded))
		  :new-urls (mapcar #'url:intern-url
				    (ensure-list (or (get-header :location headers)
						     (get-header :content-location headers))))
		  :version ,http-version))
	 (401
	   (destructuring-bind (&optional authentication-method . realm)
	       (get-header :WWW-Authenticate headers)
	     (case authentication-method
	       ((:basic :digest)
		,@(when exceptions-flush-entities
		    `((flush-input-entity (client-stream client) headers ,http-version)))
		(error 'recoverable-unauthorized-client-access
		       :url ,url
		       :method (client-method client)
		       :authentication-method authentication-method
		       :authentication-realm realm))
	       (t (client-signal-http-code ,url status (client-method client) 
					   :headers headers 
					   :reason (client-reason client)
					   :version ,http-version)))))
	 (t (client-signal-http-code ,url status (client-method client) 
				     :headers headers 
				     :reason (client-reason client)
				     :version ,http-version))))))

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

(defgeneric show-url-headers (url &key headers stream)
  (:documentation "Access headers URL and display it on STREAM."))

(defmethod show-url-headers ((string string) &key headers (stream *standard-output*))
  (show-url-headers (url:intern-url string) :headers headers :stream stream))

(defmethod show-url-headers ((url url:http-url) &key headers (stream *standard-output*))
  (%show-url-headers url headers stream))

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

(define-generic show-raw-url (url &key headers stream start end)
  (:documentation "Access URL and display it on STREAM without display processing.
HEADERS are a plist of outgoing headers to accompany the request."))

(defmethod show-raw-url ((url url:http-url) &key headers (stream *standard-output*) start end)
  (%show-url url headers stream :raw-output-p t :start start :end end))

(defmethod show-raw-url ((string string) &key headers (stream *standard-output*) start end)
  (show-raw-url (url:intern-url string) :headers headers :stream stream :start start :end end))

(define-generic show-url (url &key headers stream)
  (:documentation "Access URL and display it on STREAM.
HEADERS are a plist of outgoing headers to accompany the request."))

(defmethod show-url ((url url:http-url) &key headers (stream *standard-output*))
  (%show-url url headers stream :raw-output-p nil))

(defmethod show-url ((string string) &key headers (stream *standard-output*))
  (show-url (url:intern-url string) :headers headers :stream stream))

(defmethod show-url ((url url:ftp-pathname) &key headers (stream *standard-output*))
  (declare (ignore headers))
  (multiple-value-bind (user-id pw)
      (url:user-id-and-password url)
    (www-utils:ftp-copy-file (url::ftp-url-pathname url)
			     stream :element-type 'character
			     :user-id (or user-id "anonymous")
			     :user-pw (or pw (www-utils::user-mail-address)))))

(defmethod show-url ((url url:ftp-directory) &key headers (stream *standard-output*))
  (declare (ignore headers))
  (flet ((directory-info (url)
			 (multiple-value-bind (user-id pw)
			     (url:user-id-and-password url)
			   (www-utils::ftp-directory-info
			     (url::ftp-url-pathname url)
			     (or user-id "anonymous")
			     (or pw (www-utils::user-mail-address))))))
    (declare (inline directory-info))
    ;; get down to business
    (loop for (path . plist) in (directory-info url)
	  for ftp-url = (pathname-ftp-url-string path)
	  for length = (getf plist :length-in-bytes)
	  for creation-date = (getf plist :creation-date)
	  for reference-date = (getf plist :reference-date)
	  do (format stream
		     #-(or CLIM-SYS CMU) "~&~A~60T~:[~;~:*~\\time\\~]~:[~; (~:*~\\time\\)~]~:[~;~:* [~D Bytes]~] "
		     ;; Where \\time\\ escape is not recognized here - OBC
		     #+(or CLIM-SYS CMU) "~&~A~60T~:[~;~:*~a~]~:[~; (~:*~a)~]~:[~;~:* [~D Bytes]~] "
		     ftp-url creation-date reference-date length) 
	     #+Genera (format stream "~&~A~60T~:[~;~:*~\\time\\~]~:[~; (~:*~\\time\\)~]~:[~;~:* [~D Bytes]~] "
			      ftp-url creation-date reference-date length)
	     #-Genera (format stream "~&~A~60T~:[~;~:*~A~]~:[~; (~:*~A)~]~:[~;~:* [~D Bytes]~] "
			      ftp-url (when creation-date (write-standard-time creation-date nil))
			      (when reference-date (write-standard-time reference-date nil))
			      length))))  

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
                              
(define-generic show-url-trace (url &key max-forwards headers stream )
  (:documentation "Execute the TRACE method URL and display the result on STREAM."))
      
(defmethod show-url-trace ((url url:http-url) &key (max-forwards 5) headers (stream *standard-output*))
  (%show-url-trace url headers stream :max-forwards max-forwards)) 

(defmethod show-url-trace ((string string) &key (max-forwards 5) headers (stream *standard-output*))
  (show-url-trace (url:intern-url string) :max-forwards max-forwards :headers headers :stream stream)) 

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

(define-generic show-url-options (url &key headers stream)
  (:documentation "Execute the options method URL and display the result on STREAM."))

(defmethod show-url-options ((url url:http-url) &key headers (stream *standard-output*))
  (%show-url-options url headers stream))

(defmethod show-url-options ((string string) &key headers (stream *standard-output*))
  (show-url-options (url:intern-url string) :headers headers :stream stream)) 

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

(defgeneric delete-url (url &key headers stream)
  (:documentation "Deletes URL from the origin server."))

(defmethod delete-url ((url url:http-url) &key headers  (stream *standard-output*))
  (%delete-url url headers stream))

(defmethod delete-url ((string string) &key headers (stream *standard-output*))
  (delete-url (url:intern-url string) :headers headers :stream stream))

;;;------------------------------------------------------------------- 
;;;
;;; POST URL 
;;; 

(defun %push-entry-url-encoded-vector (vector key value)
  (flet ((push-key (key vector)
	   (loop for idx upfrom 0 below (length key)
		 for char = (aref key idx)
		 for code = (char-code char)
		 do (vector-push-extend code vector)
		 finally (vector-push-extend #.(char-code #\=) vector)))
	 (push-value (value vector)
	   (loop for idx upfrom 0 below (length value)
		 for char = (aref value idx)
		 for code = (char-code char)
		 do (vector-push-extend code vector)
		 when (eql code #.(char-code #\Return))
		   do (vector-push-extend #.(char-code #\Linefeed) vector)
		 finally (vector-push-extend #.(char-code #\&) vector))))
    (declare (inline push-key push-value))
    ;; push the key
    (push-key (etypecase key
		(symbol (symbol-name key))
		(string key))
	      vector)
    ;; push the value
    (let ((val-string (typecase value
			(string value)
			(number (write-to-string value :base 10. :escape nil))
			(t (write-to-string value :base 10. :escape nil)))))
      (declare (dynamic-extent val-string))
      (push-value val-string vector))))

(defmacro %url-encoded-vector (source type vector-size)
  `(loop with vector = (make-array ,vector-size :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)
	   ,@(ecase type
	       (:plist `(for (key value) on ,source by (function cddr)))
	       (:alist `(for (key value) in ,source)))
	 do (%push-entry-url-encoded-vector vector key value)
	 finally (return vector)))

(defun url-encoded-vector-from-alist (alist &optional (size 100))
  (%url-encoded-vector alist :alist size))

(defun url-encoded-vector-from-plist (plist &optional (size 100))
  (%url-encoded-vector plist :plist size))

#|
(defun print-url-encoded-vector (vector &optional (stream *standard-output*))
  (loop for idx upfrom 0 below (length vector)
        do (write-char (code-char (aref vector idx)) stream))
  vector)
|#

(define-generic post-url (url form-values &key headers stream)
  (:documentation "Posts FORM-VALUES to URL using the HTTP method.
URL is either an interned URL or a string. FORM-VALUES is either a
URL encoded vector, an alist of (keyword values), or a property list of
keyword values.  This method returns the values returned by the application
of the DISPLAY method to the MIME content type of the server reply.
Application/Lisp-SExp can return lisp values."))

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

(defmethod post-url ((url url:http-url) (vector vector) &key headers (stream *standard-output*))
  (%post-url url vector headers stream))

(defmethod post-url ((string string) vector &key headers (stream *standard-output*))
  (post-url (url:intern-url string) vector :headers headers :stream stream))

(defmethod post-url (url (list cons) &key headers (stream *standard-output*))
  (let ((vector (etypecase (car list)
		  (atom (url-encoded-vector-from-plist list))
		  (cons (url-encoded-vector-from-alist list)))))
    (declare (dynamic-extent vector))
    (post-url url vector :headers headers :stream stream)))

(define-generic post-form-values (url &rest args &key headers stream &allow-other-keys)
  (declare (values returned-form-values))
  (:documentation "Posts form values URL. 
Call this with the destination URL and an alternating set of keyword and values pairs.
values are readable lisp objects. HEADERS and STREAM are priveledged arguments
that are passed directly to POST-URL.  All other keywords and values are passed through
to the server via the HTTP POST method.  Values are transfered using write-to-string with
escape t and base 10. They are written to strings, and so, BIND-QUERY-VALUES*
should be used to restore them in the server response function."))

(defmethod post-form-values (url &rest args &key headers (stream *standard-output*) &allow-other-keys)
  (declare (dynamic-extent args))
  (let* ((plist (loop for (key val) on args by #'cddr
		      unless (member key '(:headers :stream))
			collect key
			and collect val))
	 (vector (url-encoded-vector-from-plist plist)))
    (declare (dynamic-extent plist vector))
    (post-url url vector :headers headers :stream stream)))

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

(defun %put-url-from-pathname (url pathname headers stream &optional content-type content-length version)
  (let* ((copy-mode (url:copy-mode url))
	 (content-type-spec (or content-type (url::mime-content-type-spec url)))
	 (path (ecase copy-mode
		 ((:binary :text) pathname)
		 (:crlf (ensure-crlf-canonical-file pathname))))
	 (file-length (or content-length (www-utils:file-length-in-bytes path)))
	 (file-version (or version (file-version pathname))))
    (flet ((send-data (url stream)
	     (declare (ignore url))
	     (stream-copy-bytes path stream file-length copy-mode)))
      (declare (dynamic-extent #'send-data))
      (%put-url url #'send-data file-length content-type-spec headers stream file-version))))

(defun %put-url-from-vector (url vector headers stream start end &optional content-type version )
  (let ((content-type-spec (or content-type (url::mime-content-type-spec url)))
	(active-length (unless (stringp vector) (- end start))))
    (flet ((send-data (url stream)
	     (declare (ignore url))
	     (write-vector stream vector start end)))
      (declare (dynamic-extent #'send-data))
      (%put-url url #'send-data active-length content-type-spec headers stream version))))

(define-generic put-url (url resource &key headers stream content-type version &allow-other-keys)
  (declare (values remote-uri status version last-modified))
  (:documentation "Puts RESOURCE in URL reporting returns on STREAM.
RESOURCE is a pathname, a vector, or a writer function that receives the arguments: (URL REMOTE-STREAM).
VERSION defaults to the universal time when RESOURCE was created.
When VERSION is :OVERWRITE no conflict checking is done.
VERSION may also be passed in.
CONTENT-TYPE defaults to the content-type associated with the URL, but may be passed in.
CONTENT-LENGTH is optional for computed puts and not recommended when resource is a pathname.
START and END are available to specify subsequences when RESOURCE is a vector."))

(defmethod put-url ((string string) resource &key headers (stream *standard-output*)
		    content-type content-length version)
  (put-url (url:intern-url string) resource :headers headers
	   :content-type content-type :content-length content-length
	   :version version :stream stream))

(defmethod put-url ((url url:http-url) (pathname pathname) &key headers (stream *standard-output*)
		    content-type content-length version)
  (%put-url-from-pathname url pathname headers stream content-type content-length version))

(defmethod put-url ((url url:http-url) (writer function) &key headers (stream *standard-output*)
		    (content-type (url::mime-content-type-spec url)) content-length (version (get-universal-time)))
  (%put-url url writer content-length content-type headers stream version))

(defmethod put-url ((url url:http-url) (vector vector) &key headers (stream *standard-output*) (start 0) (end (length vector))
		    content-type (version (get-universal-time)))
  (%put-url-from-vector url vector headers stream start end content-type version))


;;;------------------------------------------------------------------- 
;;;
;;; TEST VECTORS FOR FIXED LENGTH AND CHUNKED PUT
;;;


#|

(defun file-equal (path1 path2)
  (with-open-file (file1 path1 :direction :input :element-type '(unsigned-byte 8))
    (with-open-file (file2 path2 :direction :input :element-type '(unsigned-byte 8))
      (loop for idx upfrom 0
	    for byte1 = (read-byte file1 nil nil)
	    for byte2 = (read-byte file2 nil nil)
	    while (and byte1 byte2)
	    unless (eql byte1 byte2)
	      return (values nil idx)
	    finally (return (values (eql byte1 byte2) idx))))))

(defun test (url n)
  (flet ((fctn (url stream)
	   (declare (ignore url))
	   (loop with vector = "abcdefghijklmnopqrstuvwxyz1234567890 
"
		 with radix = (length vector)
		 for count upfrom 0 below n
		 doing (multiple-value-bind (item idx)
			   (truncate count radix)
			 (write-char (aref vector idx) stream)
			 item))))
    (put-url url #'fctn)))

(defun test2 (url)
  (flet ((fctn (url stream)
	   (declare (ignore url))
	   (stream-copy-until-eof #p"j:>jcma>hot.gif" stream :binary)))
    (put-url url #'fctn)))

(put-url "http://jcma.ai.mit.edu/jcma/launch.lisp" #p"j:>jcma>launch-demo.lisp" :version :overwrite)
(file-equal #p"j:>jcma>launch-demo.lisp.newest" #p"j:>jcma>html>launch.lisp.newest")

(put-url "http://jcma.ai.mit.edu/jcma/hot.gif" #p"j:>jcma>hot.gif" :version :overwrite)
(file-equal #p"j:>jcma>hot.gif.newest" #p"j:>jcma>html>hot.gif.newest")

(test #u"http://jcma.ai.mit.edu/jcma/test.text" 100)

(test2 #u"http://jcma.ai.mit.edu/jcma/test.gif")

(file-equal #p"j:>jcma>hot.gif.newest" #p"j:>jcma>html>test.gif.newest") 

|#


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(define-generic capture-raw-url (url &key headers display-stream start end)
  (declare (values vector header-alist))
  (:documentation "Access URL and a vector containing it.
HEADERS are a plist of outgoing headers to accompany the request."))

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

(defmethod capture-raw-url ((string string) &key headers (display-stream *standard-output*)  start end)
  (capture-raw-url (url:intern-url string) :headers headers :display-stream display-stream :start start :end end)) 
