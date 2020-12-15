;;;   -*- Mode: lisp; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-
;;;
;;; (c) Copyright  1999, John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; SERVER FLOGGER (version 0.9)
;;;
;;; Provides a facility to replay requests against a server for performance tuning
;;; Requires client-substrate to be loaded. 

;;;------------------------------------------------------------------- 
;;;
;;; MAIN CODE 
;;;

(in-package :http)

(defclass flogger
	  (property-list-mixin)
    ((host :initform :get :initarg :host :accessor flogger-host)
     (port :initform :get :initarg :port :accessor flogger-port)
     (http-method :initform :get :initarg :http-method :accessor flogger-http-method)
     (transaction-controllers :initform nil :initarg :transaction-controllers :accessor flogger-transaction-controllers)
     (next :initform nil :accessor flogger-next))
  )

(defclass transaction-control
          (property-list-mixin)
    ((url :initform nil :initarg :url :accessor transaction-control-url)
     (headers :initform nil :initarg :headers :accessor transaction-control-headers)))

(defclass post-transaction-control
          (transaction-control)
    ((vector :initform nil :initarg :vector :accessor transaction-control-vector)
     (request-headers :initform nil :initarg :request-headers :accessor transaction-control-request-headers)
     (user :initform nil :initarg :user :accessor transaction-control-user)
     (form-alist :initform nil :initarg :form-alist :accessor transaction-control-form-alist)))

(defmethod print-object ((flogger flogger) stream)
  (let ((host (flogger-host flogger))
	(port (flogger-port flogger))
	(method (flogger-http-method flogger)))
    (print-unreadable-object (flogger stream :type t :identity t)
      (when host
	(format stream "~A:~D ~A" host port method)))))

(defmethod print-object ((transaction-control transaction-control) stream)
  (let ((url (transaction-control-url transaction-control)))
    (print-unreadable-object (transaction-control stream :type t :identity t)
      (when url
	(write-string (relative-name-string url) stream)))))


;;;------------------------------------------------------------------- 
;;;
;;; ALLOCATING CONTROLLERS
;;;

(defun make-transaction-controllers-from-spec (host port default-request-headers control-specs)
  (let ((default-context (%make-context host port)))
    (loop for (url headers) in control-specs
	  for ctlr = (let ((url-string (merge-url url default-context)))
		       (handler-case 
			 (make-instance 'transaction-control
					:url (intern-url url-string)
					:headers (or headers default-request-headers))
			 (parsing-error () (format *error-output* "~&Bad URL Syntax: ~S" url)
					nil)))
	  when ctlr
	    collect ctlr)))

(defun make-transaction-controllers-from-logfile (pathname http-method host port &key (log-format *default-log-line-parser*)
                                                           status-codes default-request-headers (stream *standard-output*))
  (with-open-file (file pathname :direction :input)
    (using-resource (line-buffer line-buffer *line-buffer-size*)
      (loop with parser = (log-line-parser log-format)
            with default-context = (%make-context host port)
            for line = (read-delimited-line file '(#\return #\Linefeed) nil line-buffer)
            while line
            for ctlr = (handler-case-if (not *debug-client*)
                          (multiple-value-bind (url host method date server-version status)
                              (funcall parser line)
                            host date server-version	;ignore
                            (when (and (eql method http-method)
                                       (or (null status-codes) (member status status-codes)))
                              (let ((url-string (merge-url url default-context)))
                                (handler-case 
                                  (make-instance 'transaction-control
                                                 :url (intern-url url-string)
                                                 :headers default-request-headers)
                                  (parsing-error () (format stream "~&Bad URL Syntax: ~S" line) nil)))))
                         (error () nil))
            when ctlr collect ctlr))))

(defun make-transaction-controllers-from-post-logfile (pathname host port &key (log-format :post-log-format)
                                                                status-codes default-request-headers (stream *standard-output*))
  (with-open-file (file pathname :direction :input)
    (using-resource (line-buffer line-buffer *line-buffer-size*)
      (loop with parser = (log-line-parser log-format)
            with default-context = (%make-context host port)
            for line = (read-delimited-line file '(#\return #\Linefeed) nil line-buffer)
            while line
            for ctlr = (handler-case-if nil ;;(not *debug-client*)
                          (multiple-value-bind (url host method date http-version status bytes-received bytes-transmitted user referer
                                                form-alist user-agent ua-version ua-comment)
                              (funcall parser line)
                            method http-version status bytes-received bytes-transmitted
                            (case method
                              (:bad-record (format stream "Bad Log Record"))
                              (:post
                                (when (and form-alist
                                           (or (null status-codes) (member status status-codes)))
                                  (let ((url-string (merge-url url default-context)))
                                    (handler-case 
                                      (make-instance 'post-transaction-control
                                                     :url (intern-url url-string)
                                                     :user user
                                                     :form-alist form-alist
                                                     :property-list (list :referer referer :date date :host host 
                                                                          :user-agent (list user-agent ua-version ua-comment))
                                                     :headers default-request-headers)
                                      (parsing-error () (format stream "~&Bad URL Syntax: ~S" line) nil)))))))
                         (error () nil))
            when ctlr collect ctlr))))

(defgeneric allocate-transaction-controllers (host port method control-specs &key log-format status-codes default-request-headers))

(defmethod allocate-transaction-controllers (host port method (control-specs cons) &key log-format status-codes default-request-headers)
  (declare (ignore host port default-request-headers log-format status-codes))
  (error "Unsupported allocation method, ~S." method))

(defmethod allocate-transaction-controllers (host port (method (eql :get)) (control-specs cons) &key log-format status-codes default-request-headers)
   (declare (ignore log-format status-codes))
  (make-transaction-controllers-from-spec host port default-request-headers control-specs))

(defmethod allocate-transaction-controllers (host port (method (eql :head)) (control-specs cons) &key log-format status-codes default-request-headers)
   (declare (ignore log-format status-codes))
  (make-transaction-controllers-from-spec host port default-request-headers control-specs))

(defmethod allocate-transaction-controllers (host port (method (eql :get)) (logfile pathname) &key (log-format *default-log-line-parser*)
                                                  status-codes default-request-headers)
  (make-transaction-controllers-from-logfile logfile method host port
					     :log-format log-format :status-codes status-codes :default-request-headers default-request-headers))

(defmethod allocate-transaction-controllers (host port (method (eql :head)) (logfile pathname) &key (log-format *default-log-line-parser*)
                                                  status-codes default-request-headers)
  (make-transaction-controllers-from-logfile logfile method host port
					     :log-format log-format) :status-codes status-codes :default-request-headers default-request-headers)

(defmethod allocate-transaction-controllers (host port (method (eql :post)) (logfile pathname) &key (log-format *default-log-line-parser*)
                                                  status-codes default-request-headers)
  (make-transaction-controllers-from-post-logfile logfile host port :log-format log-format :status-codes status-codes :default-request-headers default-request-headers))

(defun allocate-flogger (host port method control-specs &key  (log-format *default-log-line-parser*) status-codes
                              default-request-headers (class 'flogger))
  "Returns a Web flogger that will make requests using the HTTP method, METHOD, to HOST on PORT.

  CONTROL-SPECS is either a list of transaction specs suitable for METHOD, or for the

  HEAD and GET methods, a pathname containing a log file.

  LOG-FORMAT controls the kind of parser applied to the log file. It defaults to *default-log-line-parser*.

  STATUS-CODES is either null or a list of acceptable status code to use when creating transaction controllers 
  from a log file.

  DEFAULT-REQUEST-HEADERS can be used to control the behavior of the flogger.
  For example, by providing '(:connection :close), neither HTTP 1.0 nor HTTP 1.1
  transactions will attempt to keep the connection between the server and client open."
   (make-instance class
       :host host
       :port port
       :http-method method
       :transaction-controllers (allocate-transaction-controllers host port method control-specs
                                                                  :log-format (ecase method 
                                                                                (:post :post-log-format)
                                                                                ((:get :head) log-format))
                                                                  :status-codes status-codes
                                                                  :default-request-headers default-request-headers)))


;;;------------------------------------------------------------------- 
;;;
;;; OPERATIONS ON TRANSACTION CONTROLS
;;;

(defmethod transaction-control-url-string ((transaction-control transaction-control))
  (name-string (transaction-control-url transaction-control))) 

(defmethod get-transaction-control ((flogger flogger))
  (let ((next (or (flogger-next flogger)
		  (flogger-transaction-controllers flogger))))
    (cond (next
	   (setf (flogger-next flogger) (cdr next))
	   (car next))
	  (t (error "No transaction controllers are available from ~S." flogger)))))

(defmethod transaction-control-form-data-vector ((transaction-control post-transaction-control))
  (or (transaction-control-vector transaction-control)
      (setf (transaction-control-vector transaction-control) (url-encoded-vector-from-alist (transaction-control-form-alist transaction-control)))))

(defmethod transaction-control-post-headers ((transaction-control post-transaction-control))
  (flet ((compute-post-headers (transaction-control)
	   (let* ((vector (transaction-control-form-data-vector transaction-control))
		  (headers (transaction-control-headers transaction-control))
		  (content-length (fill-pointer vector)))
	     `(,@headers :content-type (:application :x-www-form-urlencoded) :content-length ,content-length))))
    (or (transaction-control-request-headers transaction-control)
	(setf (transaction-control-request-headers transaction-control) (compute-post-headers transaction-control)))))

(defmethod transaction-control-clear-caches ((transaction-control post-transaction-control))
  (setf (transaction-control-vector transaction-control) nil
	(transaction-control-request-headers transaction-control) nil))


;;;------------------------------------------------------------------- 
;;;
;;; ACCESSING URLS
;;;

(defgeneric access-url (transaction-control http-method http-version persistent-connections-p report-stream)
  (:documentation "Performs HTTP METHOD according to TRANSACTION-CONTROL using protocol version HTTP-VERSION.
Higher volumes of accesses are possible than with normal client or web walker
methods because entity bodies received from the server are discarded at the
TCP level.  For HTTP 1.0, PERSISTENT-CONNECTIONS-P controls whether
connections persistent if supported by the server.  Reporting and errors are
logged on STREAM."))

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

(defmethod access-url ((transaction-control post-transaction-control) (http-method (eql :post)) http-version persistent-connections-p report-stream)
  (declare (ignore http-version persistent-connections-p))
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
	(http-condition (cond) (www-utils:report-condition cond report-stream))))))

(defvar *active-floggers* 0
   "Holds the number of currently actvie floggers.")

(defmethod access-url :around (transaction-control method http-version persistent-connections-p stream)
  (flet ((report-error (transaction-control method error status-code stream)
	   (let ((line "--------------------------------------------------------------------------------"))
	     (fast-format stream "~&~A~&Error: ~S~&Method: ~A~&URL: ~A~&Error Report: ~I"
			  line  (or status-code (type-of error)) method
			  (transaction-control-url-string transaction-control)
			  (www-utils:report-condition error stream)))))
    (atomic-incf *active-floggers*)
    (unwind-protect
	(handler-case-if (not *debug-client*)
	   (let ((*client-http-version* http-version)
		 (*client-persistent-connections* persistent-connections-p))
	     (call-next-method transaction-control method http-version persistent-connections-p stream))
	  (http-condition (err) (report-error transaction-control method err (status-code err) stream))
	  (error (err) (report-error transaction-control method err nil stream)))
      (atomic-decf *active-floggers*)))) 

;; Ports may specialize this method for more efficient resource utilization.
(defgeneric asynchronous-access-url (transaction-control method http-version persistent-connections-p stream)
  (:documentation "An asynchronous version of ACCESS-URL. See ACCESS-URL for documentation."))

(defmethod asynchronous-access-url (transaction-control method http-version persistent-connections-p stream)
  (let* ((url (transaction-control-url transaction-control))
	 (host-string (host-string url))
	 (port (or (port url) 80)))
    (process-run-function
      (format nil "HTTP Client ~A (~D)" host-string port)
      #'access-url transaction-control method http-version persistent-connections-p stream)))

#+MCL
(defmethod asynchronous-access-url ((transaction-control transaction-control) method http-version persistent-connections-p stream)
  (let ((connection-spec `(:client ,(transaction-control-url-string transaction-control) nil)))
    (declare (dynamic-extent connection-spec))
    (launch-process connection-spec `(:priority 0)
		    #'access-url transaction-control method http-version persistent-connections-p stream)))


;;;------------------------------------------------------------------- 
;;;
;;; FLOGGING LOOP
;;;


(defgeneric flog-server (flogger &key delay max-hits max-connections http-version persistent-connections-p print-p stream)
  (:documentation "Flogs a server according to the parameters contained in FLOGGER.

You can create a flogger using ALLOCATE-FLOGGER.

DELAY is the number of milliseconds between requests. If DELAY is zero,
the server is hit as fast as possible.
MAX-CONNECTIONS is maximum number of simultaneous requests allowed.
HTTP-VERSION is one of (:HTTP/1.0, :http/1.1, or :http/0.9).
PERSISTENT-CONNECTIONS-P controls whether persistent connections are used.
MAX-HITS is the number of times to hit the server before stopping.
If MAX-HITS is NIL, the server is flogged endlessly.
PRINT-P controls the operations are recorded on STREAM"))

;; see INTERNAL-TIME-UNITS-PER-SECOND for internal time units per second 

(defmethod flog-server ((flogger flogger) &key (delay 0) (max-connections 1) (max-hits 1000) (http-version :http/1.0)
			print-p  (persistent-connections-p *client-persistent-connections*) (stream *error-output*))
  (declare (fixnum delay))
  (let ((method (flogger-http-method flogger)))
    (flet ((resume-p (resume-time)
	     (< resume-time (get-internal-real-time)))
	   (available-connections-p (max-connections)
	     (< *active-floggers* max-connections)))
      (loop for transaction-control = (get-transaction-control flogger)
	    for count upfrom 1
	    until (and max-hits (> count max-hits))
	    do (if (< 1 max-connections)
		   (asynchronous-access-url transaction-control method http-version persistent-connections-p stream)
		   (access-url transaction-control method http-version persistent-connections-p stream))
	       (when print-p
		 (fast-format stream "~&[~I] ~A (~D): ~A" 
			      (write-standard-time (get-universal-time) stream)
			      method count (transaction-control-url-string transaction-control)))
	    unless (zerop delay)
	      do (process-wait "HTTP Flog Wait" #'resume-p (+ (get-internal-real-time) delay))
	    unless (available-connections-p max-connections)
	      do (process-wait "HTTP Connection Wait" #'available-connections-p max-connections)))))

(export '(allocate-flogger flog-server flogger) :http)


;;;------------------------------------------------------------------- 
;;;
;;; TESTING CODE
;;;

#|

;; setting down the persistent connection time speeds up turn around time.
(setq *persistent-connection-timeout* 0)

(setq f (allocate-flogger "gnoscere.ai.mit.edu" 80 :get #p"FUJI:/usr/cl-http.sct/log/Common-Log-80.text"))

(setq f (allocate-flogger "jefferson.ai.mit.edu" 80 :get #p"FUJI:/usr/cl-http.sct/log/1999/04/1999-04-01-Common-Log-80.text"))

(setq f (allocate-flogger "gnoscere.ai.mit.edu" 80 :get #p"http:log;Common-Log-80.text"))

(setq f (allocate-flogger "gnoscere.ai.mit.edu" 80 :get #p"http:log;Common-Log-80.text"
                                     :default-request-headers '(:connection :close )))

(setq f (allocate-flogger "gnoscere.ai.mit.edu" 80 :get #p"http:log;Common-Log-80.text")) 

(flog-server f :delay 1000 :max-hits 20 :print-p t :http-version :http/1.0)

(flog-server f :delay 1000 :max-hits 20 :max-connections 4 :print-p nil :http-version :http/1.0 :persistent-connections-p nil)

(flog-server f :delay 1000 :max-hits 100 :max-connections 4 :print-p t :http-version :http/1.0
	     :persistent-connections-p nil :stream tv:initial-lisp-listener)

(setq f (allocate-flogger "gnoscere.ai.mit.edu" 80 :post #p"FUJI:/usr/cl-http.sct/log/Common-Log-80.text"))

(flog-server a :delay 1000 :max-hits 100 :max-connections 4 :print-p t :http-version :http/1.0
	     :persistent-connections-p nil :stream tv:initial-lisp-listener)

(setq f (allocate-flogger "fuji-vlm.ai.mit.edu" 80 :post #p"FUJI:/usr/cl-http.sct/log/Post-Log-80.text"))


(flog-server f :delay 1000 :max-hits 120 :max-connections 1 :print-p t :http-version :http/1.0
	     :persistent-connections-p nil :stream tv:initial-lisp-listener)

|#
