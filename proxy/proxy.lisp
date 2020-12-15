;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-

;;; (C) Copyright 1996-1997, Christopher R. Vincent & John C. Mallery
;;;     All Rights Reserved.
;;; (C) Enhancements Copyright 1998-1999, John C. Mallery, All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CL-HTTP PROXY SERVER
;;;
;;; JCMa Questions
;;; 
;;; 1. How are response codes logged? Proxy logging would be desirable. Check netscape docs.
;;; 2. Are client conditions passed through correctly?
;;; 3. How are non-continuable client conditions handled?
;;; 4. What happens when a server error occurs upstream?
;;; 5. Why don't we handle http 1.1 post and put?
;;; 6. The previous proxy version seemed rather slow on my powerbook serving netscape.
;;;      We should meter the proxy service and see where the time is going.

(in-package :http)

;;------------------------------------------------------------------- 
;;;
;;; COMPUTING HEADERS
;;; 

(define proxy-version ()
  "Returns the proxy version, for example \"(CL-HTTP/41.4)\""
  (destructuring-bind (major minor &rest port-version-numbers)
                      (www-utils:%server-version-info)
    (declare (ignore port-version-numbers))
    ; shorter than entire server info string
    (format nil "(cl-http/~D.~D)" major minor)))

(defun proxy-via-header (&optional (comments-p t))
  "Returns a value that can be appended to a Via header."
  (if comments-p
    (or *proxy-via-header*
        (setf *proxy-via-header* `(,(%tokenize-header-keyword *http-version*) ,*local-host-domain-name* ,(proxy-version))))
    ; short version, no comments
    (or *proxy-via-header-no-comments*
        (setf *proxy-via-header-no-comments* `(,(intern-keyword *http-version*) ,*local-host-domain-name* nil)))))

(defconstant *response-status-codes-with-no-entity* '(100 204 304)
  "Response status codes that do return entity bodies.")

(declaim (inline status-code-implies-entity-p))

(defun response-status-code-implies-entity-p (code)
  "Returns whether a status code implies a message body."
  (not (member code *response-status-codes-with-no-entity*)))

(defun %compute-via-header (header)
  "Returns a new Via header list including the local host." 
  (let ((new (proxy-via-header *preserve-via-header-comments-p*)))
    ;; Destructively strip comments off existing entries
    (unless *preserve-via-header-comments-p* 
      (dolist (item header)
        (setf (third item) nil)))
    `(,new ,. header)))

(defgeneric compute-via-header (header-set)
  (:documentation "Returns a new VIA header value based on HEADER-SET that includes the local host."))

(defmethod compute-via-header ((header-set header-set))
  (let ((via (get-header :via header-set)))
    (%compute-via-header via)))

(defmethod compute-via-header ((header-plist cons))
  (let ((via (getf header-plist :via)))
    (%compute-via-header via)))

(defmethod compute-via-header ((header-plist null))
  (%compute-via-header nil))

;; All other headers are end-to-end by default. (:proxy-connection is Netscape only)
(defconstant *hop-by-hop-headers* 
  '(:connection :keep-alive :public :proxy-authenticate :te :trailer :transfer-encoding :upgrade :proxy-connection))

(defun proxy-header-plist (header-set)
  "Converts HEADER-SET into a property list of (keyword value).
Removes connection level headers"
  (%with-header-set-index (header-set)
    index					;ignore
    (with-fast-array-references ((headers headers vector))
      (loop with key
	    for idx fixnum upfrom 0 below (fill-pointer headers)
	    for header = (aref headers idx)
	    unless (or (%header-suppress-p header)
		       (member (setq key (header-keyword header)) *hop-by-hop-headers*))
	      collect key
	      and collect (header-value header)))))

(defgeneric write-proxy-response-headers (server response-headers response-http-version stream &optional header-plist))

;; Should check (server-http-version server) and (client-connection-version client) to perform appropriate modifications.
(defmethod write-proxy-response-headers ((server proxy-server-mixin) (response-headers header-set) response-http-version stream &optional header-plist)
  (declare (ignore response-http-version))
  (let* ((new-via (compute-via-header response-headers))
	 (modification-plist (if (get-header-object :date response-headers)
				 (list :via new-via)
				 (list :date (server-request-time server) :via new-via))))
    (declare (dynamic-extent new-via modification-plist))
    (write-modified-headers response-headers stream modification-plist *hop-by-hop-headers* t header-plist)))

(defmethod write-proxy-response-headers ((server proxy-server-mixin) (response-headers cons) response-http-version stream &optional header-plist)
  (declare (ignore response-http-version))
  (let* ((new-via (compute-via-header response-headers))
	 (modification-plist (if (getf response-headers :date)
				 (list :via new-via)
				 (list :date (server-request-time server) :via new-via))))
    (declare (dynamic-extent new-via modification-plist))
    (write-modified-headers response-headers stream modification-plist *hop-by-hop-headers* t header-plist)))

(defgeneric write-proxy-request-headers (server request-headers request-http-version stream &optional header-plist))

;; Should check (server-http-version server) to perform appropriate modifications.
(defmethod write-proxy-request-headers ((server proxy-server-mixin) request-headers request-http-version stream &optional header-plist)
  (declare (ignore request-http-version))
  (let* ((new-via (compute-via-header request-headers))
	 (modification-plist (list :via new-via)))
    (declare (dynamic-extent new-via modification-plist))
    (write-modified-headers request-headers stream modification-plist *hop-by-hop-headers* t header-plist)))

;;;------------------------------------------------------------------- 
;;;
;;; SIMPLE PROXY RELAY
;;; 

(declaim (inline proxy-relay-transfer-encoding))

;; obsolete 11/16/99 -- JCMa.
(defun proxy-relay-transfer-encoding (content-length http-version)
  (if (or content-length (member http-version '(:http/0.9 :http/1.0))) :fixed-length :chunked))

(defun get-transfer-encoding-header-plist (content-length http-version)
  (declare (values header-plist transfer-encoding))
  (if (or content-length (member http-version '(:http/0.9 :http/1.0)))
      (values nil :fixed-length)
      (values '(:transfer-encoding :chunked) :chunked)))

(defun proxy-relay-simple-request (server method request-http-version)
  (flet ((write-request-headers (stream method http-version)
	   (declare (ignore method))
	   (write-proxy-request-headers server (server-headers server) http-version stream)))
    (declare (dynamic-extent #'write-request-headers))
    (let ((request-url (server-url server))
	  (request-stream (server-stream server)))
      (when *trace-proxy*
	(format *trace-output*  "~&;Proxying a ~A ~A request for ~S." request-http-version method (name-string request-url))
	(format *trace-output* "~&;~2TClient Request Headers:")
	(write-header-buffer (server-headers server) *trace-output*))
      (with-http-request (request-url method :request-headers #'write-request-headers)
	(let* ((client-status (client-status client))
	       (client-http-version (client-connection-version client))
	       (client-response-headers (client-response-headers client)))
	  (with-transfer-decoding* (remote-stream request-url http-version :headers client-response-headers :copy-mode :binary)
	    (send-status-line request-stream client-status (client-reason client))
	    (cond ((response-status-code-implies-entity-p client-status)
		   (multiple-value-bind (additional-headers transfer-encoding)
		       (get-transfer-encoding-header-plist
			 (get-header :content-length client-response-headers) request-http-version)
		     (write-proxy-response-headers server client-response-headers client-http-version request-stream additional-headers)
		     ;; Send the entity body
		     (with-transfer-encoding (request-stream transfer-encoding)
		       (stream-copy-until-eof remote-stream request-stream :binary))))
		  (t (write-proxy-response-headers server client-response-headers client-http-version request-stream)))))))))

(defun proxy-relay-request-with-entity (server method request-http-version)
  (let* ((request-url (server-url server))
	 (request-headers (server-headers server))
	 (request-content-length (get-header :content-length request-headers))
	 (request-stream (server-stream server)))
    (flet ((send-data (from-stream to-stream url headers content-length from-http-version to-http-version)
	     ;; Send 100 code to continue as appropriate
	     (case from-http-version
	       ((:http/1.0 :http/0.9))
	       (t (report-status-continue from-stream)
		  (send-cr-line-feed from-stream) 
		  (force-output from-stream)))
	     ;; transfer the body
	     (with-binary-stream (from-stream :input)
	       (with-binary-stream (to-stream :output)
		 (with-transfer-decoding* (from-stream url from-http-version :headers headers :copy-mode :binary)
		   (with-transfer-encoding (to-stream (proxy-relay-transfer-encoding content-length to-http-version))
		     (stream-copy-until-eof from-stream to-stream :binary))))))
	   (write-request-headers (stream method http-version)
	     (declare (ignore method))
	     (let ((additional-headers (get-transfer-encoding-header-plist (get-header :content-length request-headers) http-version)))
	       (write-proxy-request-headers server request-headers http-version stream additional-headers))))
      (declare (dynamic-extent #'write-request-headers))
      (with-http-request
        (request-url method 
                     :request-headers #'write-request-headers
                     :request-body (send-data request-stream remote-stream request-url request-headers 
                                              request-content-length request-http-version http-version)) 
        (let* ((client-status (client-status client))
	       (client-http-version (client-connection-version client))
               (client-response-headers (client-response-headers client)))
          (with-transfer-decoding* (remote-stream request-url http-version :headers client-response-headers :copy-mode :binary)
            (send-status-line request-stream client-status (client-reason client))
            (cond ((response-status-code-implies-entity-p client-status)
		   (multiple-value-bind (additional-headers transfer-encoding)
		       (get-transfer-encoding-header-plist
			 (get-header :content-length client-response-headers) request-http-version)
		     (write-proxy-response-headers server client-response-headers client-http-version request-stream additional-headers)
                     (with-transfer-encoding (request-stream transfer-encoding)
                       (stream-copy-until-eof remote-stream request-stream :binary))))
                  (t (write-proxy-response-headers server client-response-headers client-http-version request-stream)))))))))

;;;------------------------------------------------------------------- 
;;;
;;; PROXY METHODS 
;;; 

(defmethod invoke-proxy-service :around ((server proxy-server-mixin) url method http-version
					 &aux (catch-error-p (not *debug-proxy*)))
  (flet ((ensure-live-upstream-connection (condition)
	   (declare (ignore condition))
	   (abort-if-connection-inactive *server*)
	   nil))
    (with-subnet-access-control
      ((server-address server) (or *proxy-subnets* *secure-subnets*) 
       :deny-subnets *disallowed-subnets*
       :rejection-form (error 'access-forbidden :method method :url url))
      (handler-case-if 
	  catch-error-p
	 ;; Nasty signalling ensues if the client has dropped the connection, 
	 ;; so intercept errors here and abort the connection if the client is gone. -- JCMa 5/24/1999.
	 (handler-bind-if catch-error-p
	    ((condition #'ensure-live-upstream-connection))
	   (call-next-method server url method http-version))
	(protocol-timeout (err) (error 'gateway-timeout :format-string (report-string err) :method method :url url))
	(connection-refused (err) (error 'service-unavailable :format-string (report-string err) :method method :url url))
	(remote-network-error (err) (error 'bad-gateway :format-string (report-string err) :method method :url url :close-connection t))))))

(defmethod invoke-proxy-service ((server proxy-server-mixin) (uri http-url) (method (eql :get)) request-http-version)
  (if *proxy-caching-p*
      (invoke-proxy-cache-service server method request-http-version)
      (proxy-relay-simple-request server method request-http-version)))

(defmethod invoke-proxy-service ((server proxy-server-mixin) (uri url) (method (eql :head)) request-http-version)
  (declare (ignore request-http-version))
  (flet ((write-request-headers (stream method http-version)
	   (declare (ignore method))
	   (write-proxy-request-headers server (server-headers server) http-version stream)))
    (declare (dynamic-extent #'write-request-headers))
    (let ((request-stream (server-stream server)))
      (with-http-request (uri :head :request-headers #'write-request-headers)
	remote-stream				;ignore
	(send-status-line request-stream (client-status client) (client-reason client))
	(write-proxy-response-headers server (client-response-headers client) (client-connection-version client) request-stream)))))

(defmethod invoke-proxy-service ((server proxy-server-mixin) (uri http-url) (method (eql :delete)) request-http-version)
  (proxy-relay-simple-request server method request-http-version))

;; Can't handle * correctly until intern-url handles hosts w/out paths (e.g. http://wilson.ai.mit.edu)
;; Should relay error response entities.
(defmethod invoke-proxy-service ((server proxy-server-mixin) (uri http-url) (method (eql :options)) request-http-version)
  (declare (ignore request-http-version))
  (flet ((message-allow-headers (headers)	; edit allow header for access through proxy
           (let ((allow (get-header-object :allow headers)))
             (when allow
               (setf (%header-value allow) (intersection *proxy-methods* (header-value allow) :test #'eql)))))
	 (write-request-headers (stream method http-version)
	   (declare (ignore method))
	   (write-proxy-request-headers server (server-headers server) http-version stream)))
    (declare (inline message-allow-headers)
	     (dynamic-extent #'write-request-headers))
    (let ((request-stream (server-stream server)))
      (with-http-request (uri :options :request-headers #'write-request-headers)
        remote-stream				;ignore
        (send-status-line request-stream (client-status client) (client-reason client))
        ;; must precede computing response headers
        (message-allow-headers *headers*)
	(write-proxy-response-headers server (client-response-headers client) (client-connection-version client) request-stream)))))

;; For now just use 1.0 unless we already have a 1.1 connection to the server.
(defmethod invoke-proxy-service ((server proxy-server-mixin) (uri http-url) (method (eql :post)) request-http-version)
  (let ((*client-http-version* :HTTP/1.0))
    (proxy-relay-request-with-entity server method request-http-version)))

;; For now just use 1.0 unless we already have a 1.1 connection to the server.
(defmethod invoke-proxy-service ((server proxy-server-mixin) (uri http-url) (method (eql :put)) request-http-version)
  (let ((*client-http-version* :HTTP/1.0))
    (proxy-relay-request-with-entity server method request-http-version)))

(defmethod invoke-proxy-service ((server proxy-server-mixin) (uri http-url) (method (eql :trace)) request-http-version)
  (declare (ignore request-http-version))
  (flet ((set-max-forwards (value headers)
           (let ((maxf (get-header-object :max-forwards headers)))
             (setf (%header-value maxf) value)))
	 (write-request-headers (stream method http-version)
	   (declare (ignore method))
	   (write-proxy-request-headers server (server-headers server) http-version stream)))
    (declare (dynamic-extent #'write-request-headers))
    (let ((request-url uri)
          (request-headers (server-headers server))
          (request-stream (server-stream server)))
      (with-header-values (max-forwards) request-headers
        (cond ;; bounce the trace back
	  ((and max-forwards (<= max-forwards 0))
	   (with-chunked-transfer-encoding
	     (request-stream '(:message :http) :status :success :location request-url :cache-control '(:no-cache t))
	     (write-modified-headers request-headers request-stream nil *hop-by-hop-headers* t)))
	  ;; Pass the request through
	  (t (set-max-forwards (1- max-forwards) request-headers)	; decrement max-forwards
	     (with-http-request (request-url :trace :request-headers #'write-request-headers)
	       (let* ((client-status (client-status client))
		      (client-http-version (client-connection-version client))
		      (client-response-headers (client-response-headers client)))
		 (with-transfer-decoding* (remote-stream request-url http-version :headers client-response-headers :copy-mode :binary)
		   (send-status-line request-stream client-status (client-reason client))
		   (cond ((response-status-code-implies-entity-p client-status)
			  (let* ((response-content-length (get-header :content-length client-response-headers))
				 (transfer-encoding (proxy-relay-transfer-encoding response-content-length http-version))
				 (additional-headers (case transfer-encoding
						       (:chunked '(:transfer-encoding :chunked)))))
			    (write-proxy-response-headers server client-response-headers client-http-version request-stream additional-headers)
			    (with-transfer-encoding (request-stream transfer-encoding)
			      (stream-copy-until-eof remote-stream request-stream :binary))))
			 (t (write-proxy-response-headers server client-response-headers client-http-version request-stream))))))))))))

(pushnew :cl-http-proxy *features*)


;;;------------------------------------------------------------------- 
;;;
;;; FTP PROXY METHODS
;;;

(defmethod invoke-proxy-service ((server proxy-server-mixin) (url url:ftp-directory) (method (eql :get)) request-http-version
				 &aux directory-listing)
  request-http-version
  (labels ((ftp-directory-info (url)
	     (multiple-value-bind (user-id pw)
		 (url:user-id-and-password url)
	       (www-utils::ftp-directory-info
		 (url::ftp-url-pathname url)
		 (or user-id "anonymous")
		 (or pw (www-utils::user-mail-address)))))
	   (write-item (path plist stream)
	     (destructuring-bind (&key length-in-bytes creation-date directory
				       #+cl-http-file-author author &allow-other-keys)
		 plist
	       (let ((ftp-url (pathname-ftp-url-string path directory))
		     (name (pathname-name path))
		     (type (pathname-type path))
		     (version (pathname-version path)))
		 (html:with-table-row (:stream stream)
		   (html:with-table-cell (:stream stream)
		     (html:with-rendition (:bold :stream stream)
		       (html:with-anchor-noted (:reference ftp-url :stream stream)
			 (fast-format stream "~A" name)
			 (when (and type (not (eql type :unspecific)))
			   (fast-format stream ".~A" type)
			   (when (and version (integerp version))
			     (fast-format stream ".~D" version))))))
		   (if creation-date
		       (html:with-table-cell (:horizontal-alignment :right :stream stream)
			 (write-standard-time creation-date stream))
		       (html:with-table-cell (:horizontal-alignment :center :stream stream)
			 (write-string "--" stream)))
		   (cond ((www-utils:pathname-directory-p path)
			  (html:with-table-cell (:horizontal-alignment :right :stream stream)))
			 (length-in-bytes
			  (html:with-table-cell (:horizontal-alignment :right :stream stream)
			    (write length-in-bytes :stream stream :escape nil :base 10.)))
			 (t (html:with-table-cell (:horizontal-alignment :center :stream stream)
			      (write-string "--" stream))))
		   #+cl-http-file-author
		   (if author
		       (html:with-table-cell (:horizontal-alignment :right :stream stream)
			 (write-string author stream))
		       (html:with-table-cell (:horizontal-alignment :center :stream stream))))))))
    (declare (inline directory-info))
    (cond ((setq directory-listing (ftp-directory-info url))
	   (let* ((title (relative-name-string url))
		  (stream (server-stream server))
		  (proxy-response-headers `(:via ,(compute-via-header nil))))
	     (declare (dynamic-extent title proxy-response-headers))
	     (with-successful-response (stream :html :status :success  :location url :additional-headers proxy-response-headers)
	       (html:with-html-document (:declare-dtd-version-p t :stream stream)
		 (html:with-document-preamble (:stream stream)
		   (html:declare-title title :stream stream))
		 (html:with-standard-document-body (:stream stream)
		   (html:with-section-heading (title :stream stream)
		     (html:horizontal-line  :stream stream)
		     (html:with-table (:cell-spacing 4 :stream stream)
		       (html:with-table-row (:stream stream)
			 (html:with-table-cell (:header-p t :horizontal-alignment :center :stream stream)
			   (write-string "URL" stream))
			 (html:with-table-cell (:header-p t :horizontal-alignment :center :stream stream)
			   (write-string "Creation Date" stream))
			 (html:with-table-cell (:header-p t :horizontal-alignment :center :stream stream)
			   (write-string "Bytes" stream))
			 #+cl-http-file-author
			 (html:with-table-cell (:header-p t :horizontal-alignment :center :stream stream)
			   (write-string "Author" stream)))
		       (loop for (path . plist) in directory-listing
			     for translated = (translated-pathname path)
			     do (write-item translated plist stream)))
		     (html:horizontal-line :stream stream)
		     (cl-http-signature stream)))))))
	  (t (error 'document-not-found :url url :method :get)))))

(defmethod invoke-proxy-service ((server proxy-server-mixin) (url url:ftp-pathname) (method (eql :get)) request-http-version)
  request-http-version
  (let* ((url (server-url server))
	 (pathname (url::ftp-url-pathname url))
	 (type (pathname-primary-extension pathname nil))
	 (copy-mode (or (url::%content-type-copy-mode type nil) :binary))
	 (element-type (ecase copy-mode
			 (:text 'character)
			 ((:binary :crlf) '(unsigned-byte 8))))
         (stream (server-stream server))
	 (proxy-response-headers `(:via ,(compute-via-header nil))))
    (declare (dynamic-extent proxy-response-headers))
    (multiple-value-bind (user-id pw)
	(url:user-id-and-password url)
      (setf (server-close-connection-p server) t)	;close connection for time being
      (with-successful-response (stream (case type ((:unknown nil) :text) (t type)) :location url :additional-headers proxy-response-headers)
	(case copy-mode
	  (:text
	    (with-text-stream (stream :output)
	      (www-utils:ftp-copy-file pathname stream :element-type element-type
				       :user-id (or user-id "anonymous")
				       :user-pw (or pw (www-utils::user-mail-address)))))
	  ((:binary :crlf)
	   (with-binary-stream (stream :output)
	     (www-utils:ftp-copy-file pathname stream :element-type element-type
				      :user-id (or user-id "anonymous")
				      :user-pw (or pw (www-utils::user-mail-address))))))))))

#|

;; An example of tightening up FTP proxy security.

(defparameter *ai-lab-basic-realm* (intern-realm "ai-lab-basic"))

(defparameter *cl-http-access-controls* (intern-access-control *ai-lab-basic-realm* :cl-http :if-does-not-exist :error))

(defmethod url:authentication-realm ((url url:ftp-url))
  *ai-lab-basic-realm*)

(defmethod url:capabilities ((url url:ftp-url))
  *cl-http-access-controls*)

(defmethod invoke-proxy-service :around (server (url url:ftp-url) method  request-http-version)
  server request-http-version
  (cond ((and (neti:ns-eq (host-object url) (parse-host "beet-chex.ai.mit.edu")))
	 (let ((realm (url:authentication-realm url)))
	   (with-authentication-access-control
	     (url method (get-header :authorization) realm
		  :require-capabilities (url:capabilities url)
		  :rejection-form (error 'recoverable-unauthorized-client-access :method method :url url
					 :authentication-method (realm-scheme realm) 
					 :authentication-realm realm))
	     (call-next-method))))
	(t (error 'access-forbidden :method method :url url))))

(define-proxy-subnets
  #|"128.52.0.0"|#				; MIT AI Lab
  )

|#
