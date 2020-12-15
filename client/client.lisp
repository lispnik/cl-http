;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-

;;; (C) Copyright 1996-99, John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CL-HTTP CLIENT SUBSTRATE
;;;

(in-package :http)

;; Export here to preserve modularity in the primary package definitions.
(mapc #'(lambda (x) (export (intern x :http) :http))
      '("*CLIENT-HTTP-VERSION*"
        "*CLIENT-TIMEOUT*"
        "INVOKE-HTTP-SERVICE"
        "WITH-HTTP-REQUEST"))



;;;------------------------------------------------------------------- 
;;;
;;; PROXY CONFIGURATION
;;;

(defclass basic-proxy-mixin
          (property-list-mixin)
    ((host :initform nil :initarg :host :accessor proxy-host)
     (port :initform nil :initarg :port :accessor proxy-port)
     (domain-name :initform nil :initarg :domain-name :accessor proxy-domain-name))) 

(defclass proxy
          (basic-proxy-mixin)
    () 
  (:documentation "HTTP Proxy object."))

(defmethod print-object ((proxy basic-proxy-mixin) stream)
  (print-unreadable-object (proxy stream :type t :identity t)
    (let ((domain-name (proxy-domain-name proxy)))
      (when domain-name
        (format stream "~A:~D" domain-name (proxy-port proxy)))))
  proxy) 

(defmethod proxy-domain-name :before ((proxy basic-proxy-mixin))
  (with-slots (host domain-name) proxy
    (when (and (null domain-name) host)
      (setf domain-name (domain-name-for-parsed-ip-address host)))))

(define-generic proxy-host-and-port (proxy)
  (declare (values proxy-host proxy-port))
  (:documentation "Returns the parsed proxy host and port."))

(defmethod proxy-host-and-port ((proxy basic-proxy-mixin))
  (values (proxy-host proxy) (proxy-port proxy)))

(define make-proxy (host port &optional (domain-name (domain-name-for-parsed-ip-address host)))
  (case *proxy-class*
    (proxy (make-instance 'proxy :host host :port port :domain-name domain-name))
    (t (make-instance *proxy-class* :host host :port port :domain-name domain-name))))

(define standard-proxy ()
  "Returns the standard client proxy."
  *standard-proxy*)

;; figure out how to add a setf method on default-proxy.-- JCMa 12/9/1996.
(defun set-standard-proxy (domain-name port)
  (check-type port (integer 0))
  (setq *standard-proxy* (etypecase domain-name
			   (null nil)
			   (string
			     (let* ((host (parse-host domain-name))
				    (name (host-domain-name host)))
			       (make-proxy host port name))))))

(define-generic proxy-local-loop-p (server proxy)
  (:documentation "Returns non-null when PROXY creates a local loop back to sever."))

;; Handle virtual hosts when it is a problem.   2/5/99 -- JCMa.
(defmethod proxy-local-loop-p ((server proxy-server-mixin) (proxy basic-proxy-mixin))
  (and (equalp (proxy-host proxy) (server-host server))
       (= (proxy-port proxy) (server-host-local-port server))))

(defmethod proxy-local-loop-p ((server null) (proxy basic-proxy-mixin))
  nil)

(define choose-proxy (host-string port)
  "Determine the proxy for a HOST-STRING and PORT. 
Returns a proxy object."
  (declare (values proxy))
  (check-type host-string string)
  (check-type port integer)
  (let ((proxy *standard-proxy*))
    (and proxy
	 (not (proxy-local-loop-p *server* proxy))
	 proxy)))

;;;------------------------------------------------------------------- 
;;;
;;; CLIENT CLASSES
;;;

(defclass client-proxy-mixin
          ()
    ((proxy :initform nil :initarg :proxy :accessor client-proxy))
  (:documentation "A mixin that allows a client to invoke http service via a proxy.")) 

(defclass client-transaction-info-mixin 
          ()
    ((reason :initarg :reason :accessor client-reason)
     (request-headers :initform nil :initarg :headers :accessor client-request-headers)
     (response-headers :initform nil :initarg :response-headers :accessor client-response-headers)
     (status :initform nil :initarg :status :accessor client-status))
  (:documentation "Information about the current http transaction."))

(defclass basic-client-mixin
          (client-transaction-info-mixin client-proxy-mixin property-list-mixin)
    ((connection :initform nil :initarg :connection :accessor client-connection)
     (method :initform nil :initarg :method :accessor client-method)
     (url :initform nil :initarg :url :accessor client-url))
  (:documentation "The essential components of a HTTP client.
HEADER-FUNCTION should evaluate to a header-plist when applied to the connection HTTP version."))

(defclass client 
          (basic-client-mixin)
    ()
  (:documentation "The HTTP client class.")) 

(defmethod print-object ((client basic-client-mixin) stream)
  (print-unreadable-object (client stream :type t :identity t)
    (when (client-url client)
      (let ((host-domain-name (client-host-domain-name client))
            (port (client-host-port client)))
        (when host-domain-name
          (format stream "~A: ~D" host-domain-name port)))))
  client)

;;;------------------------------------------------------------------- 
;;;
;;; MACROS
;;;

(define-macro with-client-line-buffer (() &body body)
  `(using-resource (*client-line-buffer* line-buffer *line-buffer-size*)
     ,@body))

(define-macro with-headers-for-client ((client stream version) &body body)
  "Reads headers from STREAM for the client and makes them available via GET-HEADER and MAP-HEADERS."
  `(let ((*headers* (resourced-read-headers (client-response-headers ,client) ,stream)))
     (update-connection-status-from-headers ,version *headers*)
     ,@body))

(define-macro with-client-connection ((client &key (connection-var 'connection)
                                              (require-connection-p t)) &body body)
  `(let ((,connection-var (client-connection ,client)))
     ,(if require-connection-p  
          `(cond (,connection-var ,@body)
                 (t (error "No connection has been established for the client, ~S." ,client)))
          `(progn . ,body))))

(defmacro with-client-http-stream ((client &key (stream-var 'stream)) &body body)
  `(with-client-connection (,client)
     (let ((,stream-var (connection-stream connection)))
       ,@body)))

(define-generic client-stream (client)
  (:documentation "Returns the HTTP stream to the remote URL associated with CLIENT."))

(defmethod client-stream ((client basic-client-mixin))
  (with-client-http-stream (client)
    stream))

(defmethod url:name-string ((client client) &optional compute-p)
  (url:name-string (client-url client) compute-p)) 

;;;------------------------------------------------------------------- 
;;;
;;;  REMOTE HOST ACCESSORS
;;; 

(defmacro with-client-url ((client) &body body)
  `(let ((url (client-url ,client)))
     (cond (url ,@body)
           (t (error "No URL has been specified for the client, ~S." ,client)))))

(define-generic client-host (client)
  (:documentation "Returns the remote host object for CLIENT."))

(defmethod client-host ((client basic-client-mixin)) 
  (with-client-url (client)
    (host-object url)))

(define-generic client-host-port (client)
  (:documentation "Returns the remote host object for CLIENT."))

(defmethod client-host-port ((client basic-client-mixin)) 
  (with-client-url (client)
    (host-port url)))

(define-generic client-host-domain-name (client)
  (:documentation "Returns the remote host object for CLIENT."))

(defmethod client-host-domain-name ((client basic-client-mixin)) 
  (with-client-url (client)
    (host-string url))) 

(defmacro with-client-proxy ((client &key (proxy-var 'proxy)) &body body)
  `(let ((,proxy-var (client-proxy ,client)))
     (when ,proxy-var
       ,@body)))

(define-generic proxy-host (client-or-proxy)
  (:documentation "Returns the proxy host for client-or-proxy."))

(defmethod proxy-host ((client client-proxy-mixin))
  (with-client-proxy (client)
    (proxy-host proxy)))

(define-generic proxy-port (client-or-proxy)
  (:documentation "Returns the proxy port for client-or-proxy."))

(defmethod proxy-port ((client client-proxy-mixin))
  (with-client-proxy (client)
    (proxy-port proxy)))

(define-generic proxy-domain-name (client-or-proxy)
  (:documentation "Returns the proxy domain-name for client-or-proxy."))

(defmethod proxy-domain-name ((client client-proxy-mixin))
  (with-client-proxy (client)
    (proxy-domain-name proxy))) 

(define-generic client-choose-proxy (client)
  (declare (values proxy))
  (:documentation "Select a proxy host and port based on the URL associated with CLIENT."))

(defmethod client-choose-proxy ((client client-proxy-mixin))
  (setf (client-proxy client) (choose-proxy (client-host-domain-name client) (client-host-port client))))

(defmethod return-connection ((client basic-client-mixin))
  (with-client-connection (client :require-connection-p nil)
    (when connection
      (return-connection connection)
      (setf (client-connection client) nil)
      t)))

(define-generic client-connection-version (client)
  (:documentation "Returns the HTTP version keyword for the client's current connection."))

(defmethod client-connection-version ((client basic-client-mixin))
  (with-client-connection (client :require-connection-p nil)
    (if connection
        (connection-version connection)
        (%tokenize-header-keyword  *client-http-version*))))

(defmethod deallocate-connection ((client basic-client-mixin))
  (with-slots (connection) client
    (deallocate-connection connection)
    (setf connection nil)))

;;;------------------------------------------------------------------- 
;;;
;;; RESOURCING CLIENT OBJECTS
;;; 

;; Only resourcing in genera and MCL pass in the resource to the initializer.
(define-generic initialize-resourced-client (resource client url proxy))

(defmethod initialize-resourced-client (resource (client basic-client-mixin) url proxy)
  (declare (ignore resource)) 
  ;; Initialize the URL first
  (setf (client-url client) url
	(client-response-headers client) (allocate-resource 'header-set))
  ;; Initialize proxy host and port
  (if proxy
      (setf  (client-proxy client) proxy)
      (client-choose-proxy client))
  (client-trace "~&Allocate Client: ~S" client)
  client)

(defun make-client (&optional (class *client-class*))
  "Create a new client object."
  (case class
    (client (make-instance 'client))
    (t (make-instance class)))) 

(defun make-resourced-client (resource url proxy)
  (declare (ignore resource url proxy))
  (make-client)) 

(define-generic deinitialize-resourced-client (resource client)
  (:documentation "Deinitializes a client object, deallocating any substructure and resetting instance variable values."))

(defmethod deinitialize-resourced-client (resource (client basic-client-mixin) &aux (connection (client-connection client)))
  (declare (ignore resource)) 
  ;; critically important to return the connection for a number of platforms.
  (when connection
    (return-connection connection))
  ;; reinitialize instance variables
  (setf (client-connection client) nil
        (client-method client) nil
        (client-url client) nil)
  (client-trace "~&Deallocate Client: ~S" client)
  client)

(defmethod deinitialize-resourced-client :after (resource (client property-list-mixin))
  (declare (ignore resource)) 
  (setf (property-list client) nil)
  client)

(defmethod deinitialize-resourced-client :after (resource (client client-proxy-mixin))
  (declare (ignore resource)) 
  (setf (client-proxy client) nil)
  client) 

(defmethod deinitialize-resourced-client :after (resource (client client-transaction-info-mixin))
  (declare (ignore resource)) 
  ;; deallocate resourced headers
  (let ((header-set (client-response-headers client)))
    (when header-set
      (deallocate-resource 'header-set header-set)))
  ;;reset instance variables
  (setf (client-reason client) nil
        (client-request-headers client) nil
        (client-response-headers client) nil
        (client-status client) nil)
  client) 

(defun match-http-client-p (resource client url proxy)
  (declare (ignore resource client url proxy))
  t)

(defresource http-client (url proxy)
  :constructor make-resourced-client
  :matcher match-http-client-p
  :initializer initialize-resourced-client
  :deinitializer deinitialize-resourced-client
  :initial-copies 0)

(define clear-client-resource ()
  "Clears the resource of HTTP client objects."
  (clear-resource 'http-client))

(define-generic durable-response-headers (client)
  (:documentation "Returns the response headers for CLIENT after disabling automatic deallocation."))

(defmethod durable-response-headers ((client client-transaction-info-mixin))
  (let ((header-set (client-response-headers client)))
    (cond (header-set
	   (setf (get-value client :durable-response-headers) header-set
		 (client-response-headers client) nil)
	   header-set)
	  ((get-value client :returned-response-headers))
	  (t nil))))

; formerly called with-client-allocated
(define-macro with-client ((url &key proxy-host (proxy-port 80)
                                (client-var '*client*)) &body body)
  `(let ((proxy (when ,proxy-host
                  (make-proxy ,proxy-host ,proxy-port))))
     (using-resource (,client-var http-client ,url proxy)
       ,@body)))

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

;;;------------------------------------------------------------------- 
;;;
;;; PARSE REPLY LINE
;;; 

(defun read-reply-line (stream &optional (buffer *client-line-buffer*))
  (multiple-value-bind (line eof delim length)
      (read-delimited-line stream '(#\Linefeed #\Return) t buffer)
    (declare (ignore delim))
    ;;(format t "~&Line: ~S~&EOF: ~S~&Delim: ~S~&Length: ~D" line eof delim length)
    (cond (eof
           (let ((host (foreign-host stream))
                 (port (foreign-port stream)))
             (error 'http-host-stopped-responding :host host :port port)))
          (t (subseq line 0 length)))))

(defun parse-reply (reply-line &aux (len (length reply-line)) pos1 pos2 pos3 pos4)
  (declare (values status-code reason http-version))
  (flet ((space-p (char) (char= char #\space)))
    (cond
      ((and (setq pos1 (position #\space reply-line :start 0 :end len :test #'char=))
            (setq pos2 (position-if-not #'space-p reply-line :start pos1 :end len))
            (setq pos3 (position #\space reply-line :start pos2 :end len :test #'char=)))
       (let ((status-code (parse-integer reply-line :start pos2 :end pos3 :junk-allowed t))
             (reason (and (setq pos4 (position-if-not #'space-p reply-line :start pos3 :end len))
			  (subseq reply-line pos4 len)))
             (http-version (tokenize-header-keyword reply-line 0 pos1)))
         (client-trace "~&Response: ~S~&~%" reply-line)
         #|(client-trace"~&Code: ~D~&Reason: ~S~&Version: ~S~&~%" status-code reason http-version)|#
         (values status-code reason http-version)))
      (t (error 'bad-server-response :response reply-line
                :format-string "Ill-formed reply from HTTP server, ~S." :format-args (list reply-line))))))

;;;------------------------------------------------------------------- 
;;;
;;; NETWORK FAILURE RETRIES 
;;;

(define-macro with-network-failure-retries ((client &key (retries '*client-retry-times-for-network-errors*)
                                                    (sleep-interval '*client-retry-sleep-seconds*)
                                                    (report-failures '*client-persistent-connection-report-failures*))
                                            &body body)
  "Execute BODY with retries on network errors.  Releases the client connection on error."
  `(prog ((n-tries 1)
          (retry-limit ,retries))
      try-again
         (handler-case-if 
             (and (not *debug-client*) (< n-tries retry-limit))
            (progn ,@body)
           (host-stopped-responding () 
                                    (incf n-tries) 
                                    (return-connection ,client)
                                    (go try-again))
           (network-error (err)
                          (when ,report-failures
                            (report-bug (email-address-for-bug-reports) "Connection Error"
                                        "~&Error: ~S~&Description: ~A" (type-of err) (report-string err)))
                          (incf n-tries)
                          (process-wait-with-timeout "HTTP Connection Retry" ,sleep-interval #'(lambda () nil))
                          (return-connection ,client)
                          (go try-again))))) 

(define-generic ensure-client-connection (client &optional http-version)
  (declare (values connection new-p))
  (:documentation "Ensure that a client has an open connection to the origin server or a proxy."))

(defmethod ensure-client-connection ((client basic-client-mixin) &optional (http-version *client-http-version*))
  (let ((connection (client-connection client)))
    (cond ((and connection (eq :open (connection-state connection)))
           (values connection nil))
          ;; Allocate a new HTTP connection
          (t ;; Release any existing connection
           (when connection
             ;; make sure to return a closed connection 
             (return-connection connection))
           (let ((proxy (client-proxy client))
                 host port domain-name)
             (if proxy
                 ;; use proxy host if present
                 (multiple-value-setq (host port) (proxy-host-and-port proxy))
                 ;; Otherwise use the URL host & port
                 (setq host (client-host client)
                       port (client-host-port client)
                       domain-name (client-host-domain-name client)))
             ;; Get a fresh connection 
             (setq connection (get-connection host port domain-name))
             ;; Set initial HTTP version for connection
	     (check-type http-version keyword)
             (setf (connection-version connection) http-version
                   ;; remember the client connection
                   (client-connection client) connection)
             ;; Return the connection
             (values connection t))))))

(define transfer-encoding-request-headers (method request-headers)
  "Returns request headers with appropriate TE headers for METHOD."
  (let ((encodings (case method
                     (:get *acceptable-transfer-encodings*)
                     (t nil))))
    (if encodings
        `(:connection (:te) :te (:chunked .,encodings) .,request-headers)
        request-headers)))

(define write-request-header-plist (stream header-plist http-method http-version)
  "Writes request headers when they appear as a header plist."
  (let ((transmitted-headers (case http-version
			       (:http/0.9 header-plist)
			       (:http/1.0 (if (or (not *client-persistent-connections*)
						  (getf header-plist :connection))
					      header-plist
					      `(:connection (:keep-alive) . ,header-plist)))
			       (t (transfer-encoding-request-headers http-method header-plist)))))
    (declare (dynamic-extent transmitted-headers))
    (write-headers stream transmitted-headers nil)))

;; formerly invoke-http-service-on-host
(define-generic invoke-http-service (client method header-writer response-handler &optional request-entity-generator http-version)
  (:documentation "Invokes HTTP service for client on the remote host.
HEADER-WRITER is a function called with (stream method http-version) 
to write the request headers to stream or it is a header plist.
RESPONSE-HANDLER is called with (CLIENT STREAM HTTP-VERSION)
REQUEST-ENTITY-GENERATOR is a function that transmits the body of an HTTP request.
It is called with (CLIENT STREAM HTTP-VERSION). Output is automatically forced on
STREAM.")) 

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

(defmethod invoke-http-service ((url http-url) method header-writer response-handler &optional request-entity-generator
				(http-version *client-http-version*))
  (with-client (url :client-var *client*)
    (invoke-http-service *client* method header-writer response-handler request-entity-generator http-version)))

#+ignore
(defmethod invoke-http-service :around ((client basic-client-mixin) method header-generator response-handler
                                        &optional request-entity-generator (http-version *client-http-version*))
  (with-network-failure-retries (client)
    (call-next-method client method header-generator response-handler request-entity-generator http-version))) 

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

(define-macro handling-redirects ((url) &body body)
  "Handles redirects by rerunning BODY with URL bound to the redirected url.
Returns multiple values returned by body."
  `(flet ((do-it (,url) ,@body))
     (declare (dynamic-extent #'do-it))
     (loop named done
           with urls = (ensure-list ,url)
           for count upfrom 0 below 5
           for url = (pop urls)
           doing (handler-case
                   (let ((.values. (multiple-value-list (do-it url))))
                     (declare (dynamic-extent .values.))
                     (unless urls
                       (return-from done (values-list .values.))))
                   (document-moved
                     (cond)
                     (unless (setq urls (new-urls cond))
                       (error "Redirect returned no redirection URLS."))))
           finally (error 'too-many-redirects :n-redirects count :url ,url))))

;; this should be converted into a header writer for greater efficiency 11/17/99 -- JCMa.
(define-generic compute-standard-request-headers (url &key authorization user-agent range header-plist)
  (:documentation "Add the standard line-browser headers to a header-plist.
RANGE is a list of start end positions indicating one or more byte ranges."))

(defmethod compute-standard-request-headers ((url http-url) &key authorization (user-agent *server-version*) 
                                             range header-plist)
  (macrolet ((push-entry (header value)
               `(setq headers (list* ,header ,value headers))))
    (let ((headers header-plist))
      (cond-every
        ((not (getf header-plist :accept))
         (push-entry :accept '((:* :*))))
        (range
          (loop for (start end) on range by #'cddr 
                do (check-type start integer)
                   (check-type end integer)
                collect `(,start ,(1- end)) into spec
                finally (push-entry :range `(:bytes ., spec))))
        (authorization
          (destructuring-bind (header value) authorization
            (push-entry header value)))
        (user-agent
          (push-entry :user-agent user-agent)))
      (push-entry :host `(,(host-string url) ,(host-port url)))
      headers)))

(define-generic flush-input-entity (stream headers http-version)
  (:documentation "Flushes the entity body from input STREAM.")) 

(defmethod flush-input-entity (stream headers (http-version (eql :http/0.9)))
  (declare (ignore headers stream))
  (let ((conn (client-connection *client*)))
    (setf (connection-close-p conn) t)))

(defmethod flush-input-entity (stream headers (http-version (eql :http/1.0)))
  (let ((conn (client-connection *client*))
        content-length)
    (unless (connection-close-p conn)
      (if (setq content-length (get-header :content-length headers))
          (advance-input-buffer stream content-length)
          (setf (connection-close-p conn) t)))))

(defmethod flush-input-entity (stream headers http-version &aux content-length)
  (declare (ignore http-version))
  (cond ((setq content-length (get-header :content-length headers))
         (advance-input-buffer stream content-length))
        (t (let ((transfer-encoding (get-header :transfer-encoding headers)))
             (case transfer-encoding
               (:chunked
                 (with-chunked-transfer-decoding (stream :headers headers)
                   (advance-input-buffer stream)))
               ((nil) (error 'bad-syntax-provided :url (client-url *client*)
                             :format-string "No content length header was provided."))
               (t (error 'server-not-implemented :close-connection t :url (client-url *client*)
                         :format-string "The HTTP transfer decoding, ~A, is not implemented."
                         :format-args (list transfer-encoding))))))))
