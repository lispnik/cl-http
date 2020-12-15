;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for HTTP-PROXY version 4.1
;;; Reason: Overhaul Proxy to use HEADER-SETS directly in request and response.
;;; The cached header representation remains a header property list.
;;; This should reduce proxy latency and consing noticeably as well as
;;; eliminate format mismatch errors when fetching from the cache.
;;; 
;;; Function HTTP::REPRESENTATION-FRESHNESS-LIFETIME:  new header regime.
;;; Function (CLOS:METHOD HTTP::COMPUTE-EXPIRATION-TIME (HTTP::CACHED-REPRESENTATION)):  -
;;; Function HTTP::CACHED-REPRESENTATION-UPDATE-RESPONSE-HEADERS:  -
;;; Function HTTP::VARIANT-MATCHES-REQUEST-P:  -
;;; Function HTTP:INTERN-CACHED-REPRESENTATION:  -
;;; Function (CLOS:METHOD HTTP:INTERN-CACHED-REPRESENTATION (HTTP::CACHED-RESOURCE T)):  -
;;; Remove function HTTP::COMPUTE-VIA-HEADER: undefine.
;;; Function HTTP::PROXY-RESPOND-FOR-CONDITIONAL-GET:  -
;;; Function HTTP::PROXY-RESPOND-WITH-REPRESENTATION:  -
;;; Function HTTP::PROXY-RELAY-REQUEST-WITH-ENTITY:  -
;;; Function HTTP::PROXY-RELAY-SIMPLE-REQUEST:  -
;;; Function HTTP::PROXY-RESPOND-WITH-REMOTE-ACCESS:  -
;;; Function (CLOS:METHOD HTTP::INVOKE-PROXY-SERVICE (HTTP::PROXY-SERVER-MIXIN URL:HTTP-URL (EQL :OPTIONS) T)):  -
;;; Function (CLOS:METHOD HTTP::INVOKE-PROXY-SERVICE (HTTP::PROXY-SERVER-MIXIN URL:HTTP-URL (EQL :TRACE) T)):  -
;;; Function (CLOS:METHOD HTTP::INVOKE-PROXY-SERVICE (HTTP::PROXY-SERVER-MIXIN URL:URL (EQL :HEAD) T)):  -
;;; Remove function HTTP::PROXY-RESPONSE-HEADERS: undefine, now obsolete.
;;; Remove function HTTP::COMPUTE-PROXY-RESPONSE-HEADERS: undefine, now obsolete.
;;; Function (CLOS:METHOD HTTP::WRITE-PROXY-REQUEST-HEADERS (HTTP::PROXY-SERVER-MIXIN T T T)):  -
;;; Function HTTP::WRITE-PROXY-REQUEST-HEADERS:  -
;;; Function HTTP::WRITE-PROXY-RESPONSE-HEADERS:  -
;;; Function HTTP::GET-TRANSFER-ENCODING-HEADER-PLIST:  -
;;; Function HTTP::PROXY-RELAY-REQUEST-WITH-ENTITY:  -
;;; Function HTTP::PROXY-RELAY-SIMPLE-REQUEST:  -
;;; Remove function HTTP::COMPUTE-PROXY-REQUEST-HEADERS: undefine.
;;; Remove function HTTP::PROXY-REQUEST-HEADERS: undefine.
;;; Function (CLOS:METHOD HTTP::WRITE-RESPONSE-HEADERS (HTTP::CACHED-REPRESENTATION T)):  -
;;; Function (CLOS:METHOD HTTP::WRITE-REQUEST-HEADERS (HTTP::CACHED-REPRESENTATION T)):  -
;;; Function HTTP::GET-HEADER-VALUE:  -
;;; Function (CLOS:METHOD HTTP::GET-HEADER-VALUE (HTTP::HEADER-SET T)):  -
;;; Function (CLOS:METHOD HTTP::GET-HEADER-VALUE (CONS T)):  -
;;; Function HTTP::GET-REQUEST-HEADER-VALUE:  -
;;; Written by JCMa, 11/12/99 18:44:42
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.7, Statice Runtime 466.1,
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
  "HTTP:PROXY;CACHE.LISP.23"
  "HTTP:PROXY;PROXY-CACHE.LISP.23"
  "HTTP:PROXY;PROXY.LISP.23"
  "HTTP:PROXY;PROXY.LISP.24"
  "HTTP:PROXY;PROXY.LISP.25"
  "HTTP:PROXY;PROXY-CACHE.LISP.26"
  "HTTP:PROXY;CACHE.LISP.26"
  "HTTP:PROXY;CACHE.LISP.27"
  "HTTP:PROXY;DOCUMENTATION.LISP.7")


(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL)
  (SCT:REQUIRE-PATCH-LEVEL-FOR-PATCH '(CL-HTTP 70. 20.) '(HTTP-CLIENT-SUBSTRATE 3. 1.)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.23")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun cached-representation-update-response-headers (representation response-headers)
   "Merges any missing headers from old-headers into new-headers when recording server response headers for the proxy cache."
   (let ((old-headers (cached-representation-response-headers representation))) 
      (flet ((update-header (header value)
                  (setf (getf old-headers header) value)))
         (declare (dynamic-extent #'update-header))
         (map-headers #'update-header response-headers)
         (setf (cached-representation-response-headers representation) old-headers))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.23")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(define-generic intern-cached-representation (resource header-plist &key if-does-not-exist creation-time)
  (declare (values cached-representation newly-created-p))
  (:documentation "Intern a cached HTTP representation of a resource from the resource and request headers"))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.23")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod intern-cached-representation ((resource cached-resource) header-plist &key (if-does-not-exist :error) creation-time)
  (flet ((%get-cached-representation (resource header-plist)
	   "Find a representation of resource that is the same variant implied by request headers in header-plist"
	   (let ((representations (cached-resource-representations resource)))
	     (when representations
	       (let ((vary (cached-resource-vary resource)))
		 (cond ((eq vary :*) nil)
		       (vary
			(loop for variant in representations
			      when (variant-matches-request-p vary variant header-plist)
				return variant))      
		       (t (car representations))))))))
    (declare (inline %get-cached-representation))
    (or (%get-cached-representation resource header-plist)
	(ecase if-does-not-exist
	  (:soft nil)
	  (:create
	    (let* ((cache (cache-object-cache resource))
		   (database (cache-database cache))
		   (object (make-cached-representation resource)))
	      (setf (cached-representation-identifier object) (make-representation-identifier object)
		    (cached-representation-creation-date object) (or creation-time (get-universal-time))
		    (cached-representation-request-headers object) header-plist
		    (cached-representation-entity object) (make-http-cache-database-identifier database))
	      (push object (cached-resource-representations resource))
	      (values object t)))
	  (:error (error (format nil "There is no matching representation cached for ~S." 
				 (cached-resource-uri-string resource))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.23")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun cached-representation-note-transaction (representation http-version verified-date request-headers)
  "Called to update the data for cached representation whenever a new request is cached or validated."
  (flet ((must-revalidate-p (http-version request-headers response-headers)
	   (case http-version
	     ((:http/0.9 :http/1.0)
	      (get-header :authorization request-headers))
	     (t (multiple-value-bind (directive found-p)
		    (getf response-headers :cache-control)
		  (cond (found-p
			 (loop for (key value) on directive by #'cddr
			       when (and (member key '(:must-revalidate :proxy-revalidate)) value)
				 return t
			       finally (return (and (get-header :authorization request-headers)
						    (not (or (get-header :must-revalidate request-headers)
							     (get-header :public request-headers)
							     (let ((s-maxage (get-header :s-maxage request-headers)))
							       (and s-maxage (not (zerop s-maxage))))))))))
			(t (get-header :authorization request-headers))))))))
    (declare (inline must-revalidate-p))
    (let ((response-headers (cached-representation-response-headers representation)))
      (setf (cached-representation-request-headers representation) (proxy-header-plist request-headers)
	    (cached-representation-verified-date representation) verified-date
	    (cached-representation-etag representation) (getf response-headers :etag)
	    (cached-representation-last-modification representation) (getf response-headers :last-modified)
	    (cached-representation-must-revalidate-p representation) (must-revalidate-p http-version request-headers response-headers)
	    (cached-representation-http-version representation) http-version
	    (%cached-representation-expiration-time representation) nil))))	;reset expiration time

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.23")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun variant-matches-request-p (vary variant header-plist)
  "Determine if request headers satisfy the vary requirements for a variant."
  (flet ((header-values-equal (a b)
	   (equalp a b)))
    (declare (inline header-values-equal)) 
    (loop for field in vary
	  with variant-request = (cached-representation-request-headers variant)
	  unless (header-values-equal (getf header-plist field)
				      (getf variant-request field))
	    return nil
	  finally (return t))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.23")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;; figure out what happens when there is no expires
(defun representation-freshness-lifetime (representation)
  "Return the freshness lifetime of a cached representation."
  (let* ((headers (cached-representation-response-headers representation))
	 (expires (getf headers :expires)))
    (when expires
      (- expires (cached-representation-verified-date representation)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.23")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod compute-expiration-time ((rep cached-representation))
  (let ((headers (cached-representation-response-headers rep)))
    (or ;; cache control directives override the expires header in HTTP 1.1
      (unless (member (cached-representation-http-version rep) '(:http/0.9 :http/1.0))
	(multiple-value-bind (directive found-p)
	    (getf headers :cache-control)
	  (and found-p
	       (let ((max-age (or (getf directive :s-maxage)	;S-MAXAGE overrides MAXAGE
				  (getf directive :maxage))))
		 (and max-age (+ max-age (cached-representation-verified-date rep)))))))
      ;; Try for an expires time and try to relativize it to avoid clock synchronization problems.
      (let ((expires (getf headers :expires))
	    date)
	(when expires
	  (if (setq date (getf headers :date))
	      (+ (- expires date) (cached-representation-verified-date rep))
	      expires)))
      ;; default to our standard stalness interval.
      (+ (cached-representation-verified-date rep) *proxy-cache-default-expiration-interval*))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.23")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(SCL:FUNDEFINE 'COMPUTE-VIA-HEADER)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.23")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun %compute-via-header (header)
  "Returns a new Via header list including the local host." 
  (let ((new (proxy-via-header *preserve-via-header-comments-p*)))
    ;; Destructively strip comments off existing entries
    (unless *preserve-via-header-comments-p* 
      (dolist (item header)
        (setf (third item) nil)))
    `(,new ,. header)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.23")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defgeneric compute-via-header (header-set)
  (:documentation "Returns a new VIA header value based on HEADER-SET that includes the local host."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.23")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod compute-via-header ((header-set header-set))
  (let ((via (get-header :via header-set)))
    (%compute-via-header via)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.23")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod compute-via-header ((header-plist cons))
  (let ((via (getf header-plist :via)))
    (%compute-via-header via)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY-CACHE.LISP.23")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun proxy-respond-for-conditional-get (server rep request-http-version)
  (proxy-trace "~&;Responding with Not Modified on ~S" rep)
  (let ((request-stream (server-stream server)))
    (send-status-line request-stream 304 "Not Modified")
    (write-proxy-response-headers server (cached-representation-response-headers rep) request-http-version request-stream)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY-CACHE.LISP.23")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun proxy-respond-with-representation (server rep request-http-version)
  (proxy-trace "~&;Responding with ~S" rep)
  (let ((stream (server-stream server)))
    (report-status-success stream)
    (write-proxy-response-headers server (cached-representation-response-headers rep) request-http-version stream)
    (write-representation-entity rep stream)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.23")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(SCL:FUNDEFINE 'PROXY-RESPONSE-HEADERS)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.23")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(SCL:FUNDEFINE 'COMPUTE-PROXY-RESPONSE-HEADERS)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.24")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defgeneric write-proxy-request-headers (server request-headers request-http-version stream &optional header-plist))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.24")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defgeneric write-proxy-response-headers (server response-headers response-http-version stream &optional header-plist))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.24")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun proxy-relay-request-with-entity (server method request-http-version)
  (let* ((request-url (server-url server))
	 (request-headers (server-headers server))
	 (request-content-length (get-header :content-length request-headers))
	 (request-stream (server-stream server)))
    (declare (dynamic-extent client-request-headers))
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
          (declare (dynamic-extent response-headers))
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


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.24")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun proxy-relay-simple-request (server method request-http-version)
  (flet ((write-request-headers (stream method http-version)
	   (declare (ignore method))
	   (write-proxy-request-headers server (server-headers server) http-version stream)))
    (declare (dynamic-extent #'write-request-headers))
    (let ((request-url (server-url server))
	  (request-stream (server-stream server)))
      (with-http-request (request-url method :request-headers #'write-request-headers)
	(let* ((client-status (client-status client))
	       (client-http-version (client-connection-version client))
	       (client-response-headers (client-response-headers client)))
	  (declare (dynamic-extent response-headers))
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


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.24")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

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
      (declare (dynamic-extent client-request-headers))
      (with-http-request (uri :options :request-headers #'write-request-headers)
        remote-stream				;ignore
        (send-status-line request-stream (client-status client) (client-reason client))
        ;; must precede computing response headers
        (message-allow-headers *headers*)
	(write-proxy-response-headers server (client-response-headers client) (client-connection-version client) request-stream)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.24")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

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


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.24")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

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

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.25")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;; Should check (server-http-version server) to perform appropriate modifications.
(defmethod write-proxy-request-headers ((server proxy-server-mixin) request-headers request-http-version stream &optional header-plist)
  (declare (ignore request-http-version))
  (let* ((new-via (compute-via-header request-headers))
	 (modification-plist (list :via new-via)))
    (declare (dynamic-extent new-via modification-plist))
    (write-modified-headers request-headers stream modification-plist *hop-by-hop-headers* t header-plist)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.25")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;; Should check (server-http-version server) and (client-connection-version client) to perform appropriate modifications.
(defmethod write-proxy-response-headers ((server proxy-server-mixin) response-headers response-http-version stream &optional header-plist)
  (declare (ignore response-http-version))
  (let* ((new-via (compute-via-header response-headers))
	 (modification-plist (if (get-header-object :date response-headers)
				 (list :via new-via)
				 (list :date (server-request-time server) :via new-via))))
    (declare (dynamic-extent new-via modification-plist))
    (write-modified-headers response-headers stream modification-plist *hop-by-hop-headers* t header-plist)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.25")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;; Should check (server-http-version server) and (client-connection-version client) to perform appropriate modifications.
(defmethod write-proxy-response-headers ((server proxy-server-mixin) (response-headers header-set) response-http-version stream &optional header-plist)
  (declare (ignore response-http-version))
  (let* ((new-via (compute-via-header response-headers))
	 (modification-plist (if (get-header-object :date response-headers)
				 (list :via new-via)
				 (list :date (server-request-time server) :via new-via))))
    (declare (dynamic-extent new-via modification-plist))
    (write-modified-headers response-headers stream modification-plist *hop-by-hop-headers* t header-plist)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.25")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun get-transfer-encoding-header-plist (content-length http-version)
  (declare (values header-plist transfer-encoding))
  (if (or content-length (member http-version '(:http/0.9 :http/1.0)))
      (values nil :fixed-length)
      (values '(:transfer-encoding :chunked) :chunked)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.25")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(SCL:FUNDEFINE 'COMPUTE-PROXY-REQUEST-HEADERS)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.25")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(SCL:FUNDEFINE 'PROXY-REQUEST-HEADERS)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY-CACHE.LISP.26")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

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


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY-CACHE.LISP.26")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

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


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY-CACHE.LISP.26")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

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

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.26")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defgeneric get-header-value (headers header-keyword)
  (declare (values parsed-value found-p)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.26")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod get-header-value ((header-set header-set) header-keyword)
  (get-header header-keyword header-set))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.26")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod get-header-value ((header-plist cons) header-keyword)
  (let ((value (getf header-plist header-keyword :+not-found+)))
    (case value
      (:+not-found+ (values nil nil))
      (t (values value t)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.26")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun get-request-header-value (representation header-keyword)
  (get-header-value (cached-representation-request-headers representation) header-keyword))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.26")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defun get-response-header-value (representation header-keyword)
  (get-header-value (cached-representation-response-headers representation) header-keyword))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.26")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(declaim (inline get-request-header-value))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.26")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(declaim (inline get-response-header-value))

;========================
(SCT:BEGIN-PATCH-SECTION)
; From buffer http-proxy-4-1.lisp >http>lispm>proxy>patch>http-proxy-4 W: (4)
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.27")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;;;------------------------------------------------------------------- 
;;;
;;; OPERATIONS ON REPRESENTATIONS
;;;

(defmethod write-request-headers ((representation cached-representation) stream &optional termination-line-p
				  modification-plist excluded-headers additional-headers)
  (write-modified-headers (cached-representation-request-headers representation)
			  stream modification-plist excluded-headers termination-line-p additional-headers))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;CACHE.LISP.27")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod write-response-headers ((representation cached-representation) stream &optional termination-line-p
				   modification-plist excluded-headers additional-headers)
  (write-modified-headers (cached-representation-response-headers representation)
			  stream modification-plist excluded-headers termination-line-p additional-headers))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;DOCUMENTATION.LISP.7")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(w3p:define-presentation-method w3p:present (representation (type cached-representation) stream (view w3p:html-view) &key)
  (macrolet ((writing-entry ((name stream) &body body)
	       `(progn
		  (html:with-rendition (:bold :stream ,stream)
		    (html:fast-format ,stream "~A: " ,name))
		  ,@body
		  (html:break-line :stream ,stream)))
	     (writing-headers ((name stream) &body body)
	       `(writing-entry (,name ,stream)
			       (html:with-paragraph-style (:quotation :fresh-line nil :stream ,stream)
				 (html:with-verbatim-text (:fresh-line nil :width 120 :stream ,stream)
				   ,@body)))))
    (cond (verbose-p
	   (with-slots (resource identifier etag entity creation-date verified-date last-reference must-revalidate-p
				 last-modification) representation
	     (html:with-paragraph (:stream stream)
	       (writing-entry ("Resource" stream)
			      (w3p:present resource '((cached-resource) :verbose-p nil) :view w3p:+html-view+ :stream stream))
	       (writing-entry ("Representation Identifier" stream) (write identifier :stream stream))
	       (when etag
		 (writing-entry ("Etag" stream) (print-entity-tag-header etag stream)))
	       (writing-entry ("Size" stream) (fast-format stream "~D Bytes" (cache-object-size representation)))
	       (writing-entry ("Creation Date" stream) (write-time creation-date stream))
	       (when verified-date
		 (writing-entry ("Last Verified" stream) (write-time verified-date stream)))
	       (when last-modification
		 (writing-entry ("Last Modified" stream) (write-time last-modification stream)))
	       (writing-entry ("Expiration Date" stream)
			      (write-time (cached-representation-expiration-time representation) stream))
	       (when last-reference
		 (writing-entry ("Last Reference" stream)(write-time last-reference stream)))
	       (when must-revalidate-p 
		 (writing-entry ("Must Revalidate" stream) (fast-format stream "yes")))
	       (writing-entry ("Server HTTP Version" stream) (fast-format stream "~A" (cached-representation-http-version representation)))
	       (writing-entry ("Cached Entity" stream)
			      (flet ((write-ref (stream)
				       (fast-format stream "/cl-http/proxy/cached-representation?~A+~A+entity"
						    (cached-resource-uri-string resource) identifier)))
				(declare (dynamic-extent #'write-ref))
				(html:with-anchor-noted (:reference #'write-ref :stream stream)
				  (w3p:present entity '((cache-identifier) :representation representation) 
					       :view w3p:+html-view+ :stream stream)))))
	     (html:with-paragraph (:stream stream)
	       (writing-headers ("Response Headers" stream)
			      (write-response-headers representation stream))
	       (writing-headers ("Request Headers" stream)
			      (write-request-headers representation stream)))))
	  (t (with-slots (resource identifier) representation
	       (fast-format stream "Representation: ")
	       (flet ((write-ref (stream)
			(fast-format stream "/cl-http/proxy/cached-representation?~A+~A"
				     (cached-resource-uri-string resource) identifier)))
		 (declare (dynamic-extent #'write-ref))
		 (html:with-anchor-noted (:reference #'write-ref :stream stream)
		   (fast-format stream "ID#~D" identifier))))))))

