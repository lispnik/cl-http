;;; -*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-
;;;
;;; (C) Copyright 1997-98, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; REMOTE SERVER LOG WINDOW
;;;

(in-package :http)

;;;------------------------------------------------------------------- 
;;;
;;; UTILITIES
;;;

(defparameter *log-window-color-scheme* (create-color-scheme :background "#141496"
							     :foreground :yellow
							     :link :red
							     :visited-link :orange-mandarian
							     :active-link :aquamarine))

(defmacro with-log-document-body ((&key (stream '*output-stream*)) &body body)
  `(with-standard-document-body (:color-scheme *log-window-color-scheme* :stream ,stream)
     ,@body))

(defparameter *log-window-statistics-refresh-rate* 10)

(defparameter *log-window-history-size* 20
  "The number of log entries to display on the remote client before updating the display.")

(defparameter *log-window-display-window* "CL-HTTP-Window"
  "The client window to use for display.")

(defparameter *log-window-connection-timeout* (* 60. 60. 60. 1.)	;timeout after one hour
  "The amount of idle time before closing down server push connections.")

(defparameter *cl-http-icon-small* "/cl-http/icons/power-small.gif")

(defparameter *cl-http-title* "/cl-http/icons/cl-http.gif")

(defparameter *log-window-logo-url* "/cl-http/maintenance/log-window-logo.html")

(defparameter *log-window-title-url* "/cl-http/maintenance/log-window-title.html")

(defparameter *log-window-statistics-url* "/cl-http/maintenance/log-window-statistics.html")

(defparameter *log-window-form-url* "/cl-http/maintenance/log-window-form.html")

(defparameter *log-window-notification-url* "/cl-http/maintenance/log-window-notification.html")

(defparameter *log-window-url* "/cl-http/maintenance/log-window.html")

(defparameter *log-window-logs-url* "/cl-http/maintenance/logs/")

(defparameter *log-window-activities* nil
  "A the choices of activities available from the log window.")

(defparameter *log-window-activity-presentation* nil
  "A presentation that returns the URL to display or NIL.")

(defun log-window-activity-presentation (&optional recache-p)
  (cond ((or recache-p (null *log-window-activity-presentation*))
	 (setq *log-window-activities* (stable-sort *log-window-activities* #'string< :key #'car))
	 (setq *log-window-activity-presentation* `((w3p:member-alist (("None" . nil) . ,*log-window-activities*)
									   :test equalp :value-key cdr)
							 :name-key first)))
	(t *log-window-activity-presentation*)))

(defun %define-log-window-activity (menu-string url)
  (let ((entry (assoc menu-string *log-window-activities* :test #'string-equal)))
    (cond (entry (setf (cdr entry) url))
	  (t (push `(,menu-string . ,url) *log-window-activities*)))
    (setq *log-window-activity-presentation* nil)
    *log-window-activities*))

(defmacro define-log-window-activities (&body activity-specs)
  `(progn
     (setq *log-window-activities* nil)
     . ,(loop for (menu-string url) in activity-specs
	      collect `(%define-log-window-activity ,menu-string ,url))))

(define-log-window-activities
  ("Configure Server" "/cl-http/maintenance/configure-server.html")
  ("Documentation" "/cl-http/docs.html")
  ("Edit User ACL" "/cl-http/edit-user.html")
  ("View Server Logs" "/cl-http/maintenance/logs/"))



;;;------------------------------------------------------------------- 
;;;
;;; LOG STATISTICS
;;;

(defun get-statistics-log (&optional (port *standard-http-port*))
  (find-if #'(lambda (l) (typep l 'log-counters-mixin)) (standard-access-logs port)))

(define-generic log-statistics-snapshot (log)
  (declare (values n-access-denials n-client-errors n-insufficient-resource-denials
		   n-redirects n-requests n-requests-served n-server-errors
		   n-deletes n-gets n-heads n-options n-posts n-puts n-traces n-extension-methods
		   bytes-transmitted bytes-received elapsed-seconds cpu-time connections 
		   uptime launch-time current-time)))

(defmethod log-statistics-snapshot ((log log-counters-mixin))
  (let ((launch-time (log-creation-time log))
	current-time uptime)
    (with-lock-held ((log-lock log) :write "Log Snapshot")
      (setq current-time (get-universal-time)
	    uptime (- current-time launch-time))
      (values (log-number-of-access-denials log)
	      (log-number-of-client-errors log)
	      (log-number-of-insufficient-resource-denials log)
	      (log-number-of-redirects log)
	      (log-total-number-of-requests log)
	      (log-number-of-requests-served log)
	      (log-number-of-server-errors log)
	      (log-number-of-deletes log)
	      (log-number-of-gets log)
	      (log-number-of-heads log)
	      (log-number-of-options log)
	      (log-number-of-posts log)
	      (log-number-of-puts log)
	      (log-number-of-traces log)
	      (log-number-of-extension-methods log)
	      (log-bytes-transmitted log)
	      (log-bytes-received log)
	      (elapsed-seconds log)
	      (cpu-time log)
	      (log-http-connections log)
	      uptime launch-time current-time))))

(defun useful-interval-ratio  (n seconds)
  (declare (values rate units ))
  (flet ((rate (numerator denominator class)
	   (cond ((zerop denominator)
		  (return-from useful-interval-ratio (values 0 class)))
		 (t (let ((rate (round numerator denominator)))
		      (unless (zerop rate)
			(return-from useful-interval-ratio (values rate class))))))))
    (cond ((zerop n)
	   (values 0 "(na)"))
	  (t (rate n seconds "sec")
	     (rate n (floor seconds 60) "min")
	     (rate n (floor seconds #.(* 60 60)) "hr")
	     (rate n (floor seconds #.(* 60 60 24)) "day")
	     (rate n (floor seconds #.(* 60 60 24 7)) "wk")))))

(defmethod log-display-statistics-as-html-table ((log log-counters-mixin) stream)
  (macrolet ((with-entry ((label stream &optional col-span) &body value-writer)
	       (let ((form (etypecase label
			     ((or string symbol) `(write-string ,label ,stream))
			     (cons label))))
		 `(with-table-row (:stream ,stream)
		    (with-table-cell (:stream ,stream)
		      (with-rendition (:italic :stream ,stream)
			,form))
		    (with-table-cell (:horizontal-alignment :right :stream ,stream
							    ,.(when col-span `(:column-span ,col-span)))
		      ,@value-writer))))
	     (write-rate-entry (label value uptime stream)
	       `(multiple-value-bind (rate unit-string)
		    (useful-interval-ratio ,value ,uptime)
		  (with-entry ((fast-format ,stream "~A / ~A" ,label unit-string) ,stream)
			      (fast-format ,stream "~D" rate)))))
    (flet ((write-entry (label value stream)
	     (with-entry (label stream)
			 (fast-format stream "~D" value)))
	   (write-heading (label stream)
	     (with-table-cell (:header-p t :column-span 2 :stream stream)
	       (write-string label stream))))
      (multiple-value-bind (n-access-denials n-client-errors n-insufficient-resource-denials
			    n-redirects n-requests n-requests-served n-server-errors
			    n-deletes n-gets n-heads n-options n-posts n-puts n-traces n-extension-methods
			    bytes-transmitted bytes-received elapsed-seconds cpu-time connections uptime launch-time current-time)
	  (log-statistics-snapshot log)
	(let ((total-cpu-time (truncate cpu-time 1000000.)))
	  (with-table (:stream stream)
	    (with-table-row (:stream stream)
	      (with-table-cell (:vertical-alignment :top :horizontal-alignment :center :column-span 3 :stream stream)
		(with-table (:border 1 :stream stream)
		  (with-entry ("Uptime Time" stream)
			      (write-interval uptime stream))
		  (with-entry ("Launch Time" stream)
			      (write-standard-time launch-time stream t))
		  (with-entry ("Current Time" stream)
			      (write-standard-time current-time stream t))
		  (with-entry ("Elapsed Time" stream)
			      (write-interval elapsed-seconds stream))
		  (with-entry ("CPU Time" stream)
			      (write-interval total-cpu-time stream)))))
	    (with-table-row (:stream stream)
	      (with-table-cell (:vertical-alignment :top :stream stream)
		(with-table (:stream stream)
		  (write-heading "Request Status" stream)
		  (write-entry "Request Served" n-requests-served stream)
		  (unless-every
		    ((zerop n-redirects) (write-entry "Redirects" n-redirects stream))
		    (nil (write-entry "Total Delivered" (+ n-requests-served n-redirects) stream))
		    ((zerop n-access-denials) (write-entry "Access Denials" n-access-denials stream))
		    ((zerop n-insufficient-resource-denials) (write-entry "Overload Denials" n-insufficient-resource-denials stream))
		    ((zerop n-server-errors) (write-entry "Server Errors" n-server-errors stream))
		    ((zerop n-client-errors) (write-entry "Client Errors" n-client-errors stream)))
		  (write-entry "Total Requests" n-requests stream)))
	      (with-table-cell (:vertical-alignment :top :stream stream)
		(with-table (:stream stream)
		  (write-heading "HTTP Methods" stream)
		  (unless-every
		    ((zerop n-deletes) (write-entry "Delete" n-deletes stream))
		    ((zerop n-gets) (write-entry "Get" n-gets stream))
		    ((zerop n-heads) (write-entry "Head" n-heads stream))
		    ((zerop n-options) (write-entry "Options" n-options stream))
		    ((zerop n-posts) (write-entry "Post" n-posts stream))
		    ((zerop n-puts) (write-entry "Put" n-puts stream))
		    ((zerop n-traces) (write-entry "Trace" n-traces stream))
		    ((zerop n-extension-methods) (write-entry "Extension" n-extension-methods stream)))))
	      (with-table-cell (:vertical-alignment :top :stream stream)
		(with-table (:stream stream)
		  (write-heading "Performance" stream)
		  (write-rate-entry "Requests" n-requests uptime stream)
		  (write-entry "Requests / Conn" (round n-requests connections) stream)
		  (write-entry "CPU Msecs / Request" (truncate (round cpu-time n-requests) 1000.) stream)
		  (write-entry "Msecs / Request" (truncate (* elapsed-seconds 1000.)  n-requests) stream)
		  (write-rate-entry "Bytes Sent" bytes-transmitted uptime stream)
		  (write-rate-entry "Bytes Received" bytes-received uptime stream)
		  (unless-every
		    ((zerop n-insufficient-resource-denials) (write-rate-entry "Overloads" n-insufficient-resource-denials uptime stream))
		    ((zerop n-server-errors) (write-rate-entry "Errors" n-server-errors uptime stream)))
		  (write-entry "Bytes Received" bytes-received stream)
		  (write-entry "Bytes Sent" bytes-transmitted stream))))))))))

(defmethod write-log-statistics-pane (url stream)
  (flet ((refresh-p (log ticks stream)
	   (handler-case
	     (or (>= (log-total-number-of-requests log) ticks)
		 (not (live-connection-p stream)))
	     (error () t))))
    (let ((log (get-statistics-log (local-port stream))))
      (if log
	  (ns4.0:with-server-push-response (stream)
	    (setf (server-timeout *server*) *log-window-connection-timeout*)
	    (loop doing (ns4.0:with-block
			  (stream :content-type :html :content-location url)
			  (with-html-document (:stream stream)
			    (with-log-document-body (:stream stream)
			      (with-centering (:stream stream)
				(log-display-statistics-as-html-table log stream)))))
			(force-output stream)
			(process-wait "Log Counter Refresh Wait" #'refresh-p
				      log (+ (log-total-number-of-requests log) *log-window-statistics-refresh-rate*) stream)
			(unless (live-connection-p stream)
			  (return))))
	  (error 'document-not-found :url url
		 :format-string "There is no log with counters on the port ~D."
		 :format-args (list (local-port stream)))))))


;;;------------------------------------------------------------------- 
;;;
;;; NOTIFICATION PANE
;;;

(defclass process-queued-remote-notification-log-mixin
	  (process-queued-logging-mixin)
    ((notification-active-p :initform nil :initarg :notification-active-p :accessor remote-notification-active-p))
  (:documentation "Notifies remote clients asynchronously from the active HTTP connection to be logged."))

(defclass remote-notification-log
          (access-log process-queued-remote-notification-log-mixin log-locking-mixin property-list-mixin)
    ((notification :initform :remote-client :initarg :notification :accessor log-notification)
     (predicate :initform nil :initarg predicate :accessor notification-log-predicate)
     (notifier :initarg notifier :accessor notification-log-notifier)
     (streams :initform nil :accessor log-remote-streams)
     (ticks :initform 0 :accessor notification-log-ticks))
  (:documentation "This log class notifies remote log streams about HTTP activity."))

;;(pushnew 'remote-notification-log *log-classes*)

(defmethod log-notifications-on ((log remote-notification-log) &optional on-p)
  (setf (log-notification log) (if on-p :remote-client nil)))

(defun create-remote-notification-log (&optional (port *standard-http-port*))
  (create-notification-access-log #'log-remote-streams #'notify-remote-client
				  :name "Remote Log Window"
				  :port port
				  :class 'remote-notification-log))

(defun remote-notification-log-p (log)
  (typep log 'remote-notification-log))

(defun get-remote-notification-log (&optional (port *standard-http-port*))
  (find-if #'remote-notification-log-p (standard-access-logs port)))

(defun ensure-remote-notification-log (&optional (port *standard-http-port*))
  (or (get-remote-notification-log port)
      (create-remote-notification-log port)))

;; Ensure that task queue is active.
(defmethod add-access-log :before ((log process-queued-remote-notification-log-mixin) port)
  (declare (ignore port))
  (setf (remote-notification-active-p log) t)
    (tq:ensure-active-task-queue log))

(defmethod remove-access-log :around ((log process-queued-remote-notification-log-mixin) port)
  (setf (remote-notification-active-p log) nil)
  (call-next-method log port)
  (tq:clear-task-queue log))

;; Deactivate the task queue and free its process.
(defmethod unregister-log :before ((log process-queued-remote-notification-log-mixin))
  (tq:task-queue-process-kill log))

(defmethod maybe-remove-idle-remote-notification-log ((log process-queued-remote-notification-log-mixin))
  (declare (values remote-notification-log-removed-p))
  (unless (log-remote-streams log)
    (remove-access-log log (log-port log))
    t))

(defmethod log-server-access and ((log remote-notification-log) (server server-logging-mixin))
  (with-slots (notification) log
    (when notification
      (incf (notification-log-ticks log))
      (and (funcall (notification-log-predicate log) log)
	   (funcall (notification-log-notifier log) log server)))
    t))

(defmethod log-install-remote-stream ((log remote-notification-log) stream)
  (with-log-write-lock (log)
    (pushnew stream (log-remote-streams log))))

(defun %log-deinstall-remote-stream (log stream)
  (setf (log-remote-streams log) (delete stream (log-remote-streams log))))

(defmethod log-deinstall-remote-stream ((log remote-notification-log) stream)
  (with-log-write-lock (log)
    (%log-deinstall-remote-stream log stream)))

;; Synchronous version   9/18/98 -- JCMa.
;(defmethod notify-remote-client ((log remote-notification-log) (server server-logging-mixin))
;  (let ((streams (log-remote-streams log))
;	(requests (server-requests-completed server)))
;    (handler-case
;      (loop for stream in streams
;	    do (handler-case
;		 (progn
;		   (fast-format stream "{~D} " requests)
;		   (write-extended-common-logfile-entry server stream)
;		   (terpri stream)
;		   (force-output stream))
;		 (network-error () (%log-deinstall-remote-stream log stream))))
;      (error (err) (bug-report-error-logging-access server log err 'notify-remote-client)))
;    t))

;(defmethod log-entry-writer ((log process-queued-remote-notification-log-mixin) (server server-logging-mixin))
;  (let* ((requests (server-requests-completed server))
;	 (host-name (host-log-name server))
;	 (user-name (%server-user-qualified-name server))
;	 (request (server-request server))
;	 (request-time (server-request-time server))
;	 (status (server-status server))
;	 (bytes (www-utils:bytes-transmitted (server-stream server)))	;total bytes (not number of bytes in a document)
;	 (headers (server-headers server))
;	 (user-agent (get-header :user-agent headers))
;	 (referrer (massage-referrer (get-header :referer headers))))
;    (flet ((write-log-entry (log-stream)
;	     (fast-format log-stream "{~D} " requests)
;	     (%write-extended-common-logfile-entry
;	       host-name request request-time status bytes user-name user-agent referrer nil log-stream #\space)
;	     ;; Trailing CR makes this consistently parsable.
;	     (terpri log-stream)))
;      #'write-log-entry)))

(declaim (inline %write-log-window-notification))

(defun %write-log-window-notification (host-name requests-per-connection request request-time status bytes user-name user-agent referer
						 cpu-time elapsed-time stream)
  (flet ((write-milliseconds (milliseconds stream) ;; "Writes milliseconds in 4 characters."
	   (cond ((< milliseconds 1000)
		  (prin1 (float (/ (round milliseconds 10) 100)) stream))
		 ((< milliseconds 10000)
		  (prin1 (float (/ (round milliseconds 10) 100)) stream))
		 ((< milliseconds 100000)
		  (prin1 (float (/ (round milliseconds 100) 10)) stream))
		 (t (prin1 (round milliseconds 1000) stream))))
	 (write-microseconds (microseconds stream) ;; "Writes microseconds in 4 characters."
	   (let* ((milliseconds (round microseconds 1000)))
	     (cond ((> 10 milliseconds)
		    (write-string "   " stream))
		   ((> 100 milliseconds)
		    (write-string "  " stream))
		   ((> 1000 milliseconds)
		    (write-char #\space stream)))
	     (prin1 milliseconds stream)))
         (get-request-indices (request)
           (when request
             (let* ((end (length request))
                    (pos1 (and (not (zerop end)) (%fast-position-if white-space-char-p request :start 0 :end end)))
                    (pos2 (and pos1 (%fast-position-if white-space-char-p request :start (1+ pos1) :end end))))
               (values pos2 (and pos1 end))))))
    (declare (inline write-microseconds write-milliseconds get-request-indices))
    (multiple-value-bind (http-version-pos request-length)
        (get-request-indices request)
      ;; date and time
      (write-char #\[ stream)
      (multiple-value-bind (seconds minutes hours)
	  (decode-universal-time request-time)
	(write-24-hour-time hours minutes seconds stream))
      ;; milliseconds of elapsed time
      (write-char #\space stream)
      (write-milliseconds elapsed-time stream)
      ;; microsecond CPU time
      (write-char #\space stream)
      (write-microseconds cpu-time stream)
      (write-char #\] stream)
      ;; host domain name or IP address.
      (write-char #\space stream)
      (write-string host-name stream)
      (write-char #\tab stream)
      ;; Status code returned to the client.
      (prin1 status stream)
      ;; HTTP version
      (write-char #\space stream)
      (cond (http-version-pos
             (write-string request stream :start (1+ http-version-pos) :end request-length))
            (request-length
             (write-string "HTTP/0.9" stream))
            (t (write-string "HTTP/?.?" stream)))
      (write-char #\space stream)
      ;; number of requests per connection
      (fast-format stream "{~D} " requests-per-connection)
      ;; Number of bytes transfered to the client.
      (write-char #\space stream)
      (prin1 bytes stream)
      ;; Authenticated User Name
      (write-char #\tab stream)
      (if user-name (write user-name :escape t :stream stream) (write-char #\- stream))
      ;; Exact request received from client.
      (write-char #\space stream)
      (write-char #\" stream)
      (cond (http-version-pos
             (write-string request stream :start 0 :end http-version-pos))
            (request-length
             (write-string request stream :start 0 :end request-length)))
      (write-char #\" stream)
      (write-char #\tab stream)
      (if user-agent (write user-agent :stream stream :escape t) (write-char #\- stream))
      (write-char #\space stream)
      (if referer (write referer :stream stream :escape t) (write-char #\- stream))
      ;; Trailing CR makes this consistently parsable.
      (terpri stream))))

(defmethod log-entry-writer ((log process-queued-remote-notification-log-mixin) (server server-logging-mixin))
  (let* ((requests (server-requests-completed server))
	 (host-name (host-log-name server))
	 (user-name (%server-user-qualified-name server))
	 (request (server-request server t))
	 (request-time (server-request-time server))
	 (status (server-status server))
	 (bytes (www-utils:bytes-transmitted (server-stream server)))	;total bytes (not number of bytes in a document)
	 (header-set (server-headers server))
	 cpu-time elapsed-time user-agent-val referer-val)
    (when header-set
      (with-header-values (user-agent referer) header-set
	(setq user-agent-val user-agent
	      referer-val referer)))
    (psetq cpu-time (cpu-time server)
	   elapsed-time (elapsed-time server))
    (flet ((write-log-entry (log-stream)
	     (%write-log-window-notification
	       host-name requests request request-time status bytes user-name user-agent-val referer-val
	       cpu-time elapsed-time log-stream)))
      #'write-log-entry))) 

(defmethod notify-remote-client ((log process-queued-remote-notification-log-mixin) (server server-logging-mixin))
  (when (remote-notification-active-p log)
    (tq:push-task-queue log (log-entry-writer log server)))
  t)

(defparameter *log-window-notification-timeout* 1
  "The number of seconds to wait for a log window notification pane to accept the update.")

(defparameter *log-window-notification-interval* 5
  "The number of entries to write before forcing output on a log window notification stream.")

(defmethod tq:task-queue-execute-pending-tasks ((log process-queued-remote-notification-log-mixin))
  (labels ((report-logging-error (log error fatal-p)
             (let ((error-type (type-of error)))
               (report-bug *bug-http-server* (format nil "HTTP~:[~; Fatal~] Remote Log Notification Error: ~S" fatal-p error-type)
			   "~:[~;~&Remote Log notification has been suspended.~]~
                            ~&Log: ~S~&Error: ~S~:[~;~&Error Report: ~:*~A~]~:[~;~&Backtrace: ~:*~A~]"
			   fatal-p log error-type (report-string error)
			   (when *stack-backtraces-in-bug-reports*
			     (stack-backtrace-string error)))))
	   (%handle-logging-error (error)
	     (typecase error
	       ((or network-error file-error)
		(report-logging-error log error nil)
		(sleep 5)
		t)
	       (t (report-logging-error log error t)
		  (stop-log-queue log)
		  t))))
    (declare (dynamic-extent #'%handle-logging-error))
    (macrolet ((apply-log-streams ((log-streams timeout &key (stream-var 'stream)) &body body)
		 `(loop for ,stream-var in ,log-streams
			do (handler-case
			     (cond ((live-connection-p ,stream-var)	;don't write to dead streams
				    (with-timeout (,timeout :error-p nil)
				      ,@body))
				   (t (%log-deinstall-remote-stream log ,stream-var)))
			     (network-error () (%log-deinstall-remote-stream log ,stream-var))))))
      (handler-bind-if (not *debug-server*)
	 ((error #'%handle-logging-error))
	(loop with write-timeout = *log-window-notification-timeout*
	      with output-interval = (1- *log-window-notification-interval*)
	      for closure = (tq:pop-task-queue log)
	      while closure
	      for count downfrom output-interval
	      for log-streams = (log-remote-streams log)
	      do (cond (log-streams
			(let ((force-output-p (zerop count)))
			  (apply-log-streams (log-streams write-timeout)
					     (funcall closure stream)
					     (when force-output-p (force-output stream)))
			  (when force-output-p (setq count output-interval))))
		       ;; no clients means deactivate log.
		       (t (remove-access-log log (log-port log))))
	      while (tq:task-queue-run-p log)
	      finally (apply-log-streams ((log-remote-streams log) write-timeout)
					 (force-output stream)))))))

(defmethod write-log-window-notification-pane (url stream)
  (flet ((refresh-p (log ticks stream)
	   (handler-case
	     (or (>= (notification-log-ticks log) ticks)
		 (not (live-connection-p stream)))
	     (error () t))))
    (let ((log nil)
	  (port (local-port stream)))
      (unwind-protect
	  (ns4.0:with-server-push-response (stream)
	    (setf (server-timeout *server*) *log-window-connection-timeout*)
	    (loop doing (ns1.1:with-block
			  (stream :content-type :html :content-location url)
			  (with-html-document (:stream stream)
			    (with-log-document-body (:stream stream)
			      (with-font (:size 2 :stream stream)
				(with-verbatim-text (:fresh-line nil :width 256 :stream stream)
				  (unwind-protect
				      ;; make sure it is not deinstalled by another log window.
				      (log-install-remote-stream (setq log (ensure-remote-notification-log port)) stream)
				    (process-wait "Log Window Refresh Wait" #'refresh-p
						  log (+ (notification-log-ticks log) *log-window-history-size*) stream)
				    (log-deinstall-remote-stream log stream)))))))
			(unless (live-connection-p stream)
			  (return))
			(force-output stream)))
	(when log
	  (maybe-remove-idle-remote-notification-log log))))))


;;;------------------------------------------------------------------- 
;;;
;;; CONTROL FORM PANE
;;;


(defgeneric write-log-window-form (url stream))

(defmethod write-log-window-form (url stream)
  (flet ((accept-activities (label query-id stream)
	   (with-table-row (:stream stream)
	     (with-table-cell (:stream stream)
	       (with-rendition (:bold :stream stream)
		 (write-string label stream)))
	     (with-table-cell (:horizontal-alignment :left :stream stream)
	       (w3p:accept (log-window-activity-presentation) :stream stream :view w3p:+html-view+ 
			   :present-p t :default nil :prompt nil :prompt-mode :raw
			   :query-identifier (string query-id) :insert-default t :active-p t))))
	 (accept-parameter (label query-id value stream)
	   (with-table-row (:stream stream)
	     (with-table-cell (:stream stream)
	       (with-rendition (:bold :stream stream)
		 (write-string label stream)))
	     (with-table-cell (:horizontal-alignment :left :stream stream)
	       (w3p:accept `(w3p:integer 0 999) :stream stream :view w3p:+html-view+ 
			   :present-p t :default value :prompt nil :prompt-mode :raw
			   :query-identifier (string query-id) :insert-default t :active-p t)))))
    (http:with-successful-response (stream :html
					   :expires (url:expiration-universal-time url)
					   :cache-control (url:response-cache-control-directives url)
					   :content-location url
					   :content-language (url:languages url))
      (with-html-document (:stream stream)
	(with-log-document-body (:stream stream)
	  (with-fillout-form (:post url :stream stream)
	    (with-centering (:stream stream)
	    (with-table (:stream stream :border nil :cell-padding 2 :cell-spacing 2)
	       (with-table-row (:stream stream)
		 (with-table-cell (:header-p t :column-span 2 :stream stream)
		   (with-font (:size 5 :stream stream)
		     (fast-format stream "~A" (local-context)))))
	      (accept-parameter "Statistics Refresh Rate" :statistics-refresh-interval *log-window-statistics-refresh-rate* stream)
	      (accept-parameter "Log History Size" :history-size *log-window-history-size* stream)
	      (accept-activities "Select Activity" :activity stream)
	      (with-table-row (:stream stream)
		(with-table-cell (:horizontal-alignment :center :stream stream)
		  (accept-input 'reset-button "Reset" :stream stream))
		(with-table-cell (:horizontal-alignment :left :stream stream)
		  (accept-input 'submit-button "Submit" :stream stream))))))
	  ;; sign the document
	  (with-emphasis (:address :stream stream)
	    (ns4.0:note-anchor *server-version* :reference *cl-http-home-page-url-string*
			       :target *log-window-display-window* :stream stream)))))))

(defmethod respond-log-window-form ((url url:http-form) stream query-alist)
  (labels ((jump-to-activity (query-id presentation-type query-alist)
	     (let ((activity (accept-value query-id presentation-type query-alist)))
	       (when activity
		 (redirect-request *server* (merge-url activity) *log-window-display-window*))))
	   (accept-value (query-id presentation-type query-alist)
	     (let ((raw-value (second (assoc query-id query-alist :test #'eq))))
	       (if raw-value
		   (handler-case 
		     (w3p:accept-from-string presentation-type raw-value :view w3p:+html-view+)
		     (w3p:input-not-of-required-type () (values nil nil)))
		   (values nil nil)))))
    (macrolet ((update-value (query-id reference presentation-type)
		 `(multiple-value-bind (new-value p-type)
		      (accept-value ,query-id ,presentation-type query-alist)
		    (when p-type
		      (setf ,reference new-value)))))
      (update-value :statistics-refresh-interval *log-window-statistics-refresh-rate* '(w3p:integer 0 999))
      (update-value :history-size *log-window-history-size* '(w3p:integer 0 999))
      ;; local control exited here when a jump is present
      (jump-to-activity :activity (log-window-activity-presentation) query-alist)))
  ;; no need to update the form pane
  (http:with-successful-response (stream :html :status :no-content
					 :cache-control (url:response-cache-control-directives url)
					 :content-location url))
  ;; Replace the form with any new values
  #+foo(write-log-window-form url stream))


;;;------------------------------------------------------------------- 
;;;
;;; FRAME
;;;

(defmethod write-log-window-title-pane (url stream)
  (http:with-conditional-get-response (stream :html :expires (url:expiration-universal-time url)
					      :cache-control (url:response-cache-control-directives url)
					      :content-language (url:languages url))
    (with-html-document (:stream stream)
      (with-log-document-body (:stream stream)
	(with-centering (:stream stream)
	  (ns4.0:with-table (:background :grey :stream stream)
	    (with-font (:size 6 :stream stream)
	      (with-rendition (:bold :stream stream)
		(image *cl-http-title* "Common Lisp Hypermedia Server" :stream stream
			     :vertical-space 0 :horizontal-space 0
			     :width 425 :height 26 :alignment :texttop)))))))))

(defmethod write-log-window-logo (url stream)
  (http:with-conditional-get-response (stream :html :expires (url:expiration-universal-time url)
					      :cache-control (url:response-cache-control-directives url)
					      :content-language (url:languages url))
    (with-html-document (:stream stream)
      (with-log-document-body (:stream stream)
	(with-centering (:stream stream)
	  (with-font (:size 6 :stream stream)
	    (with-rendition (:bold :stream stream)
	      (ns4.0:with-anchor-noted (:reference *cl-http-home-page-url-string* :target *log-window-display-window* :stream stream)
		(image *cl-http-icon-small* "CL-HTTP" :stream stream :alignment :texttop
			     :vertical-space 0 :horizontal-space 0 :width 67 :height 30)))))))))

(defmethod write-log-frame-set (url stream)
  (http:with-conditional-get-response (stream :html
                                              :expires (url:expiration-universal-time url)
                                              :cache-control (url:response-cache-control-directives url)
                                              :content-location url
                                              :content-language (url:languages url))
    (with-html-document (:stream stream)
      (with-document-preamble (:stream stream)
        (declare-title "CL-HTTP Log Window" :stream stream))
      (ns4.0:with-document-frameset (:rows '((:pixel 55) :wild) :stream stream)
	(ns4.0:with-document-frameset (:columns '((:pixel 100) :wild) :stream stream)
	  (ns4.0:note-document-frame :name "logo-pane" :reference *log-window-logo-url*
				     :target *log-window-display-window* :resizable-p t :scrolling nil :stream stream)
	  (ns4.0:note-document-frame :name "title-pane" :reference *log-window-title-url*
				     :target *log-window-display-window* :resizable-p t :scrolling nil :stream stream))
	(ns4.0:with-document-frameset (:rows '((:percentage 50) :wild) :stream stream)
	  (ns4.0:with-document-frameset (:columns '((:percentage 50) :wild) :stream stream)
	    (ns4.0:note-document-frame :name "statistics-pane" :reference *log-window-statistics-url*
				       :target "display-pane" :resizable-p t :stream stream)
	    (ns4.0:note-document-frame :name "form-pane" :reference *log-window-form-url* 
				       :target *log-window-display-window* :resizable-p t :stream stream))
	  (ns4.0:note-document-frame :name "notification-pane" :reference *log-window-notification-url* 
				     :target *log-window-display-window* :resizable-p t :stream stream))))))


;;;------------------------------------------------------------------- 
;;;
;;; EXPORTS
;;;

(defun export-log-window (&rest args)
  (let ((export-args `(:private t
		       :language :en
		       :keywords (:cl-http :maintenance :log-window)
		       ,@args)))
    (apply #'export-url #u*log-window-logo-url*
	   :computed
	   :response-function #'write-log-window-logo
	   :expiration `(:interval ,(* 15. 60.))
	   export-args)
    (apply #'export-url #u*log-window-title-url*
	   :computed
	   :response-function #'write-log-window-title-pane
	   :expiration `(:interval ,(* 15. 60.))
	   export-args)
    (apply #'export-url #u*log-window-statistics-url*
	   :computed
	   :response-function #'write-log-statistics-pane
	   :must-revalidate t
	   :documentation "Displays a table of log statistics."
	   export-args)
    (apply #'export-url #u*log-window-form-url*
	   :computed-form
	   :form-function #'write-log-window-form
	   :response-function #'respond-log-window-form
	   :must-revalidate t
	   :documentation "Displays the log window control form."
	   export-args)
    (apply #'export-url #u*log-window-notification-url*
	   :computed
	   :response-function #'write-log-window-notification-pane
	   :must-revalidate t
	   :documentation "Displays a the remote log window."
	   export-args)
    (apply #'export-url #u*log-window-url*
	   :computed
	   :response-function #'write-log-frame-set
	   :expiration `(:interval ,(* 15. 60.))
	   export-args)
    (apply #'export-url #u*log-window-logs-url*
	   :directory
	   :pathname *standard-log-directory*
	   :recursive-p t
	   export-args)
    (destructuring-bind (&key (secure-subnets (list (local-host-ip-address)))
			      (authentication-realm :server)
			      (capabilities :webmasters) &allow-other-keys)
	export-args
      (export-web-configuration-interface :secure-subnets secure-subnets
					  :authentication-realm authentication-realm
					  :capabilities capabilities))))

(export '(*cl-http-icon-small*
	  *cl-http-title*
	  *log-window-color-scheme*
	  *log-window-connection-timeout*
	  *log-window-form-url*
	  *log-window-history-size*
	  *log-window-logo-url*
	  *log-window-logs-url*
	  *log-window-notification-url*
	  *log-window-statistics-refresh-rate*
	  *log-window-statistics-url*
	  *log-window-title-url*
	  *log-window-url*
	  define-log-window-activities
	  export-log-window)
	:http)

;; Use this form to export CL-HTTP Log Window.

#+Ignore
(export-log-window
  ;; Webmaster access only on server
  :authentication-realm :server
  :capabilities :webmasters
  :secure-subnets `(,(local-host-ip-address)))

#+Ignore
(export-log-window
  ;; Webmaster access only on server
  :authentication-realm nil
  :capabilities nil
  :secure-subnets nil)
