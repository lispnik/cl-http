;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-

;;; (C) Copyright 1996-1997, Christopher R. Vincent
;;;     All Rights Reserved.
;;; (C) Enhancements Copyright 1998-99, John C. Mallery
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CL-HTTP PROXY SERVER
;;;

(in-package :http)

;;;------------------------------------------------------------------- 
;;;
;;; PROXY-CACHE
;;; 

(define check-only-if-cached-precondition (server &optional (headers *headers*))
  (with-header-values (cache-control) headers
    (and cache-control
	 (getf cache-control :only-if-cached)
	 (error 'gateway-timeout :url (server-url-string server) :method (server-method server)))))

(defun proxy-respond-with-representation (server rep request-http-version)
  (proxy-trace "~&;Responding with ~S" rep)
  (let ((stream (server-stream server)))
    (report-status-success stream)
    (write-proxy-response-headers server (cached-representation-response-headers rep) request-http-version stream)
    (write-representation-entity rep stream)))

(defun proxy-respond-for-conditional-get (server rep request-http-version)
  (proxy-trace "~&;Responding with Not Modified on ~S" rep)
  (let ((request-stream (server-stream server)))
    (send-status-line request-stream 304 "Not Modified")
    (write-proxy-response-headers server (cached-representation-response-headers rep) request-http-version request-stream)))

(defun proxy-respond-handling-conditional-get (server representation request-http-version)
  (let ((last-modification (or (cached-representation-last-modification representation)
			       (cached-representation-verified-date representation))))
    (unless (http-version-less-p request-http-version :http/1.1)
      (check-if-unmodified-since-precondition last-modification (server-method server) (server-headers server)))
    (if (if-modified-since-p last-modification (server-headers server))
	(proxy-respond-for-conditional-get server representation request-http-version)
	(proxy-respond-with-representation server representation request-http-version))))

(defun proxy-respond-while-caching-entity (representation http-version time remote-stream request-stream transfer-encoding request-headers response-headers)
  (proxy-trace "~&;Caching data for ~S" representation)
  (setf (cached-representation-response-headers representation) (proxy-header-plist response-headers))	;set response headers
  (cached-representation-note-transaction representation http-version time request-headers)
  (setf (cached-representation-last-reference representation) time)
  (with-transfer-encoding (request-stream transfer-encoding)
    (with-open-representation (entity-stream representation :output)
      (with-binary-stream (request-stream :output)
	(let ((broadcast (make-broadcast-stream request-stream entity-stream)))
	  (declare (dynamic-extent broadcast))
	  (stream-copy-until-eof remote-stream broadcast :binary))))))

; Abstract this further when incorporating additional HTTP/1.1 functionality.
(defun proxy-respond-with-remote-access (server res rep request-http-version)
  (let* ((request-url (server-url server))
	 (request-headers (server-headers server))
	 (request-stream (server-stream server)))
    (flet ((write-request-headers (stream method http-version)
	     (declare (ignore method))
	     (let ((additional-headers (when rep
					 (proxy-trace "~&;Sending conditional request to origin server for ~S." rep)
					 `(:if-modified-since ,(cached-representation-verified-date rep)))))
	       (declare (dynamic-extent additional-headers))
	       (write-proxy-request-headers server request-headers http-version stream additional-headers))))
      (declare (dynamic-extent #'write-request-headers))
      (when *trace-proxy*
	(format *trace-output* "~&;~2TProxy Request Headers:~&")
	(write-request-headers *trace-output* (server-method server) request-http-version))
      (with-http-request (request-url :get :request-headers #'write-request-headers)
	(let* ((client-status (client-status client))
	       (client-http-version (client-connection-version client))
	       (client-response-headers (client-response-headers client)))
	  (with-transfer-decoding* (remote-stream request-url http-version :headers client-response-headers :copy-mode :binary)
	    (when *trace-proxy*
	      (format *trace-output* "~&;Origin Server ~A Status: ~D" client-http-version client-status)
	      (format *trace-output* "~&;Origin Server Headers:~&")
	      (write-header-buffer client-response-headers *trace-output*))
	    (cond ((= 200 client-status)
		   (let* ((response-content-length (get-header :content-length client-response-headers))
			  (transfer-encoding (proxy-relay-transfer-encoding response-content-length request-http-version))
			  (additional-headers (case transfer-encoding
						(:chunked '(:transfer-encoding :chunked)))))
		     (declare (dynamic-extent additional-headers))
		     (send-status-line request-stream client-status (client-reason client))
		     (write-proxy-response-headers server client-response-headers client-http-version request-stream additional-headers)
		     (cond ((and (proxy-cacheable-client-request-p http-version request-headers)
				 (proxy-cacheable-server-response-p http-version client-response-headers))
			    (let* ((time (get-universal-time))
				   (resource (or res (intern-cached-resource (name-string request-url) *http-proxy-cache* :if-does-not-exist :create)))
				   (representation (intern-cached-representation resource request-headers :if-does-not-exist :create :creation-time time)))
			      (proxy-respond-while-caching-entity representation http-version time remote-stream request-stream
								  transfer-encoding request-headers client-response-headers)))
			   (t (with-transfer-encoding (request-stream transfer-encoding)
				(stream-copy-until-eof remote-stream request-stream :binary))))))
		  ((and rep (= client-status 304))
		   (let ((time (server-request-time server)))
		     (cached-representation-update-response-headers rep client-response-headers)	;update response headers
		     (cached-representation-note-transaction rep http-version time request-headers)
		     (setf (cached-representation-last-reference rep) time))
		   (proxy-respond-handling-conditional-get server rep request-http-version))
		  ((response-status-code-implies-entity-p client-status)	; don't cache other entities
		   (proxy-trace "~&;Relaying entity for status ~S response." client-status)
		   (send-status-line request-stream client-status (client-reason client))
		   (let* ((response-content-length (get-header :content-length client-response-headers))
			  (transfer-encoding (proxy-relay-transfer-encoding response-content-length request-http-version))
			  (additional-headers (case transfer-encoding
						(:chunked '(:transfer-encoding :chunked)))))
		     (declare (dynamic-extent additional-headers))
		     (write-proxy-response-headers server client-response-headers client-http-version request-stream additional-headers)
		     (with-transfer-encoding (request-stream transfer-encoding)
		       (stream-copy-until-eof remote-stream request-stream :binary))))
		  (t (send-status-line request-stream client-status (client-reason client))
		     (write-proxy-response-headers server client-response-headers client-http-version request-stream)
		     (proxy-trace "~&;Relaying headers for status ~S response." client-status)))))))))

(define-generic invoke-proxy-cache-service (server method request-http-version)
  (:documentation "Service a proxy request with caching operations."))

;; HTTP 1.1 cache-control implemented except for the fields on the directives
;; :private & :no-cache and the :no-transform directive.   2/4/99 -- JCMa.
(defmethod invoke-proxy-cache-service ((server proxy-server-mixin) (method (eql :get)) request-http-version)
  (when *trace-proxy*
    (format *trace-output*  "~&;Proxying a ~A ~A request for ~S." request-http-version method (server-url-string server))
    (format *trace-output* "~&;~2TClient Request Headers:~&")
    (write-header-buffer (server-headers server) *trace-output*))
  (with-slots (stream url url-string headers) server
    (let* ((resource (intern-cached-resource url-string *http-proxy-cache* :if-does-not-exist :soft))
           (representation (and resource (intern-cached-representation resource headers :if-does-not-exist :soft))))
      (cond (representation
	     (proxy-trace "~&;Matching valid representation found for ~S" representation)
	     (cond ((or (cached-representation-must-revalidate-p representation)
			(proxy-revalidate-cache-for-client-p representation request-http-version (server-request-time server)))
		    (proxy-trace "~&;Revalidating representation for ~S" representation)
		    (unless (http-version-less-p request-http-version :http/1.1)
		      (check-only-if-cached-precondition server (server-headers server)))
		    (proxy-respond-with-remote-access server resource representation request-http-version))
		   (t (setf (cached-representation-last-reference representation) (get-universal-time))
		      (proxy-respond-handling-conditional-get server representation request-http-version))))
	    (t (proxy-trace "~&;No matching representation found]." representation)
	       (unless (http-version-less-p request-http-version :http/1.1)
		 (check-only-if-cached-precondition server (server-headers server)))
	       (proxy-respond-with-remote-access server resource representation request-http-version))))))
