;;; -*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-
;;;
;;; (c) Copyright 1994-99, John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; LOGS
;;;

(in-package :http)

;; should use a without interrupts for safety.  7/27/95 -- JCMa.
(define log-file-stream-stays-open (&optional on-p)
  "Controls whether the file log stream remains open all the time,
or it is reopenned for each log transaction. Production servers
should keep the log file stream open."
  (when (and *log-file-stream-stays-open* (not on-p))
    (close-all-logs))
  (setq *log-file-stream-stays-open* (not (null on-p))))

(define-macro with-log-write-lock ((log) &body body)
  `(with-slots (lock) ,log
     (www-utils:with-lock-held (lock :write "HTTP Log Wait") ,@body)))

(defmacro %with-log-stream ((log &key open-file-forces-output-p) &body body)
  `(cond (*log-file-stream-stays-open*
          (let ((log-stream (%log-stream log)))
            ,@body
            ,@(when open-file-forces-output-p
                '((force-output log-stream)))))
         (t (with-slots (filename) ,log
              ;; standard character or base character must be used to
              ;; prevent the Lispm from adding escapes into the file.
              ;; standard-char is better because it ensures
              ;; interoperability across lisp implementations. 9/29/95 -- JCMa.
              (with-open-file (log-stream filename :direction :output :if-exists :append
                                          :if-does-not-exist :create
                                          :element-type #+Genera 'standard-char
                                          #-Genera *standard-character-type*)
                ,@body)))))

(define-macro with-log-stream ((log) &body body)
  `(with-log-write-lock (,log)
     (%with-log-stream (,log) . ,body)))

(define map-all-logs (function)
  "Maps FUNCTION over all logs."
  (declare (dynamic-extent function))
  (mapc function *all-logs*))

(defparameter *log-classes* '(basic-common-file-log basic-extended-common-file-log common-file-log
                                                    extended-common-file-log http-log basic-url-metering
                                                    extended-http-log notification-log custom-notification-log)
  "The available log classes.")

(setf (documentation '*log-access-log-class* 'variable)
      (format nil "This variable controls the class of access log used to record HTTP transactions.
If you change this variable other than in the configuration file read on start up, 
use CLEAR-ACCESS-LOGS to clear server logging datastructures. Although you can define or customize
your own log classes (see http:server;log.lisp), a number of predefined log classes are available.

Currently Defined Log Classes~2%
~:{~S~&~A~:^~2%~}" (mapcar #'(lambda (x) (list x (documentation x 'type)))
                           (sort *log-classes* #'string<))))

(defmethod print-object ((log basic-log-mixin) stream)
  (with-slots (name port) log
    (print-unreadable-object (log stream :type t :identity t)
      (format stream "~:[~;~:*~A~]~:[~;~:* (port: ~D)~]" name port))))


;;;------------------------------------------------------------------- 
;;;
;;; TRANSACTIONS
;;;

(defmethod print-object ((transaction transaction) stream)
  (with-slots (url) transaction
    (print-unreadable-object (transaction stream :type t :identity t)
      (when url
        (write-string (url:name-string url) stream)))))

(defmethod print-object ((log http-log) stream)
  (with-slots (name port n-requests) log
    (print-unreadable-object (log stream :type t :identity t)
      (format stream "~A (~D) [~D transactions]" name port n-requests)))) 

(define-generic access-log-p (thing)
  (:documentation "Returns non-null when THING is an access log."))

(defmethod access-log-p ((log access-log)) t)

(defmethod access-log-p (thing) 
  (declare (ignore thing))
  nil) 


;;;------------------------------------------------------------------- 
;;;
;;; CUSTOM NOTIFICATIONS
;;;

(define ensure-extended-access-log (&key (port *standard-http-port*)
                                         (name "Extended-CL-HTTP-Log")
                                         (directory "HTTP:LOGS;Extended;")
                                         (class 'basic-extended-common-file-log))
  "Add a extended common logfile for PORT."
  (multiple-value-bind (log newly-created-p)
      (intern-access-log (or name (default-log-file-name port class))
                         :port port
                         :if-does-not-exist :create
                         :directory directory
                         :class class)
    (log-file-logging-on log t)
    (log-notifications-on log nil)
    (unless newly-created-p
      (start-log-queue log))
    (values log newly-created-p)))

(define create-notification-access-log (predicate notifier &key (name "Notification-Log")
                                                  (port *standard-http-port*)
                                                  (class 'custom-notification-log))
  (flet ((equal-log (x)
           (equalp name (log-name x))))
    (declare (dynamic-extent #'equal-log))
    (check-type predicate function)
    (check-type notifier function)
    (let ((log (find-access-log-if #'equal-log port)))
      (unless log
        (multiple-value-setq (log)
          (intern-access-log (or name (default-log-file-name port class))
                             :port port
                             :directory *standard-log-directory*
                             :class class
                             :if-does-not-exist :create)))
      (setf (notification-log-notifier log) notifier
            (notification-log-predicate log) predicate)
      (log-notifications-on log t)
      (add-access-log log port)
      log)))

;;;------------------------------------------------------------------- 
;;;
;;; ALLOCATING LOGS
;;;

(define default-log-file-name (port &optional (name *log-access-log-class*))
  (format nil "~:(~A-~D~)" name port))

(defvar *standard-access-logs* nil
  "An alist of all the standard access logs indexed by port number.")

(define standard-access-logs (port)
  "Returns the standard access logs for the port, PORT."
  (declare (values logs))
  (check-type port integer)
  (let ((entry (assoc port *standard-access-logs* :test #'=)))
    (cond (entry (cdr entry))
          (t (intern-access-log (default-log-file-name port)
                                :port port
                                :directory *standard-log-directory*
                                :class *log-access-log-class*
                                :if-does-not-exist :create)
             (or (cdr (assoc port *standard-access-logs* :test #'eql))
                 (error "Should never happen: Access log not created!"))))))

(defun %set-standard-logs (port logs)
  (check-type port integer)
  (check-type logs cons)
  (unless (every #'access-log-p logs)
    (error "Bad argument to LOGS, ~S" logs))
  (let ((entry (assoc port *standard-access-logs* :test #'eql)))
    (cond (entry (setf (cdr entry) logs))
          (t (push (list* port logs) *standard-access-logs*)))
    *standard-access-logs*))

(defsetf standard-access-logs %set-standard-logs)

(define-generic find-access-log-if (predicate port &optional start end)
  (:documentation "Finds all access logs for PORT satisfying PREDICATE.
PORT is either an integer denoting a port number or NIL, 
which case all logs are checked."))

(defmethod find-access-log-if (predicate (port integer) &optional (start 0) end)
  (let ((entry (assoc port *standard-access-logs* :test #'eql)))
    (and entry
         (find-if predicate (cdr entry) :start start :end end))))

(defmethod find-access-log-if (predicate (port null) &optional (start 0) end)
  (find-if predicate *all-logs* :start start :end end))

(define-generic add-access-log (access-log port)
  (:documentation "Adds an access log, ACCESS-LOG, for PORT."))

(defmethod add-access-log ((log access-log) (port integer))
  (let ((entry (assoc port *standard-access-logs* :test #'eql)))
    (cond (entry
           (unless (member log (cdr entry))
             (nconc entry (list log))))
          (t (push (list port log) *standard-access-logs*)))))

(define-generic remove-access-log (access-log port)
  (:documentation "Removes an access log, ACCESS-LOG, for PORT.
PORT is either an integer or :ALL"))

(defmethod remove-access-log ((log access-log) (port integer))
  (let ((entry (assoc port *standard-access-logs* :test #'eql)))
    (when (member log (cdr entry))
      (delete log entry)
      (unless (cdr entry)
        (setq *standard-access-logs* (delete entry *standard-access-logs*))))))

(defmethod remove-access-log ((log access-log) (port (eql :all)))
  (loop for entry in *standard-access-logs*
        do (when (member log (cdr entry))
             (delete log entry)
             (unless (cdr entry)
               (setq *standard-access-logs* (delete entry *standard-access-logs*))))))

;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(define ensure-current-log (&optional (port *standard-http-port*))
  "Ensures that a log has been created and is current."
  (declare (values logs-for-port))
  (let ((logs (standard-access-logs port)))
    (mapc #'start-log-queue logs)
    logs))

(define current-access-logs ()
  "Returns the current access log."
  (standard-access-logs *standard-http-port*))

(define clear-access-logs (&aux logs)
  "Clears all access logs so that none will be known in the environment."
  (setq logs *all-logs*
        *all-logs* nil
        *standard-access-logs* nil)
  (mapc #'log-queue-process-kill logs)
  logs)

(define-generic register-log (log)
  (:documentation "Primitive that registers LOG so that it is found by intern-access-log, but does not make the log active."))

(defmethod register-log ((log basic-log-mixin))
  (pushnew log *all-logs* :test #'equalp :key #'log-name))

(define-generic unregister-log (log)
  (:documentation "Primitive that unregisters LOG. LOC should be removed from active ports with remove log."))

(defmethod unregister-log ((log basic-log-mixin))
  (setq *all-logs* (delete log *all-logs*)))

(defmethod unregister-log :after ((log process-queued-logging-mixin))
  (tq:task-queue-process-kill log))

(define intern-access-log (name &key (port *standard-http-port*)
                                (if-does-not-exist :error)
                                (directory *standard-log-directory*)
                                host
                                (class *log-access-log-class*))
  (declare (values log newly-created-p))
  (flet ((equal-log-p (x)
           (and (equalp name (log-name x))
                (typep x class))))
    (declare (dynamic-extent #'equal-log-p))
    (etypecase name
      (string
        (cond
          ((find-access-log-if #'equal-log-p nil))
          (t (ecase if-does-not-exist
               (:soft nil)
               (:create
                 (let* ((*standard-log-directory* directory)
                        (log (allocate-log :name name :port port :class class :local-host (or host (local-host)))))
                   (when port (add-access-log log port))
                   (values log t)))
               (:error (error "Unknown HTTP log, ~A." name))))))
      (basic-log-mixin name))))

(define-generic unintern-access-log (log)
  (:documentation "Removes LOG from any ports and uninterns it."))

(defmethod unintern-access-log ((log basic-log-mixin))
  (remove-access-log log :all)
  (unregister-log log))

(defmethod initialize-instance :after ((log basic-log-mixin) &key &allow-other-keys)
  (with-slots (creation-time) log
    (register-log log)
    (setq creation-time (get-universal-time))
    log))

(define-generic initialize-log-filename (file-logging-mixin)
  (declare (values log))
  (:documentation "Initializes the log file and directory for FILE-LOGGING-MIXIN."))

;; default method
(defmethod initialize-log-filename (log) log)

(defmethod initialize-log-filename ((logs cons))
  (mapcar #'initialize-log-filename logs))

(defmethod initialize-log-filename ((log file-logging-mixin))
  (with-slots (name port filename log-file-name) log
    (let* ((pathname (translated-pathname *standard-log-directory*))
           (name-for-file (concatenate 'string log-file-name "-" (write-to-string port :base 10.))))
      (unless (probe-directory pathname)
        (www-utils:create-directories-recursively pathname))
      (setf filename (www-utils:%make-log-pathname
                       (pathname-device pathname)
                       (pathname-directory pathname) name-for-file (pathname-host pathname)))
      log)))                                    ;must return log
 
(defmethod initialize-instance :after ((log file-logging-mixin) &key &allow-other-keys)
  (initialize-log-filename log))

(defmethod initialize-instance :after ((log log-locking-mixin) &key &allow-other-keys)
  (with-slots (lock) log
    (setq lock (www-utils:make-lock "HTTP Log Lock" :type :simple))
    log))

(defmethod initialize-instance :after ((log dynamic-loggin-mixin) &key (size *default-log-size*) &allow-other-keys)
  (macrolet ((make-table (table size)
               `(if ,table
                    (clrhash ,table )
                    (setf ,table (make-hash-table :size ,size)))))
    (with-slots (name url-table creation-time local-host filename lock) log
      (make-table url-table size)
      (initialize-host-name-space log (floor size 10.))
      log)))

(define allocate-log (&key (name "CL-HTTP-Log")
                           (local-host (local-host))
                           (port *standard-http-port*)
                           (class *log-access-log-class*))
  (make-instance class :name name :local-host local-host :port port))

;;;------------------------------------------------------------------- 
;;;
;;;  ACCESSING THE FILE STREAM FOR A LOG
;;;

(define-generic open-log-stream-p (log)
  (:documentation "Returns non-null when the log stream is open."))

(defmethod open-log-stream-p ((log file-logging-mixin))
  (with-slots (file-stream) log
    (and file-stream (open-stream-p file-stream))))

(define-generic %log-stream (log)
  (:documentation "Return the log-stream, openning it if necessary.
Does not treat concurency issues."))

(defmethod %log-stream ((log file-logging-mixin))
  (with-slots (filename file-stream) log
    (unless (and file-stream (open-stream-p file-stream))
      ;; the idea here is to maintain a visible record of the log file in the
      ;; file system.  If the file does to exist, create it.
      (unless (probe-file filename)
        (with-open-file (fstream filename :direction :output :if-does-not-exist :create)
          #-(OR Genera Allegro ACLNT CMU) (declare (ignore fstream))))
      ;; then reopen and remember it is open
      (setq file-stream (open filename :direction :output :if-exists :append)))
    file-stream))

(define-generic log-stream (log)
  (:documentation "Return the log-stream, openning it if necessary.
Does treat concurency issues."))

(defmethod log-stream ((log file-logging-mixin))
  (with-slots (filename file-stream) log
    (unless (and file-stream (open-stream-p file-stream))
      (with-log-write-lock (log)
        (unless (open-log-stream-p log)
          (%log-stream log))))
    file-stream))

(define-generic %close-log-stream (log)
  (:documentation "Closes the file log stream if it is open."))

(defmethod %close-log-stream ((log file-logging-mixin))
  (with-slots (file-stream) log
    (when (open-log-stream-p log)
      (force-output file-stream)
      (close file-stream))))

(define-generic close-log-stream (log)
  (:documentation "Carefully closes the file log stream if it is open."))

(defmethod close-log-stream ((log file-logging-mixin))
  (with-log-write-lock (log)
    (%close-log-stream log)))

(defmethod close-log-stream (log)
  (declare (ignore log))
  nil)

(define close-all-logs ()
  "Closes any open file streams for all HTTP logs."
  (mapc #'close-log-stream *all-logs*))

;;;------------------------------------------------------------------- 
;;;
;;; SETTING SWITCHES
;;;

;; Provide default method
(defmethod log-notification (log)
  (declare (ignore log))
  nil)

(defmethod log-notifications ((logs cons))
  (loop for log in logs
        when (log-notification log)
          return t
        finally (return nil)))

(define-generic log-notifications-on (log-or-logs &optional on-p)
  (:documentation "Turns notifications on and off according to on-p."))

(defmethod log-notifications-on (log &optional on-p)
  (declare (ignore log on-p)))

(defmethod log-notifications-on ((log log-notification-mixin) &optional on-p)
  (setf (log-notification log) (if on-p :tv nil)))

(defmethod log-notifications-on ((logs cons) &optional on-p)
  (dolist (log logs)
    (log-notifications-on log on-p))
  logs)

(define-generic log-dynamic-logging-on (log-or-logs &optional on-p)
  (:documentation "Turns dynamic logging on and off according to ON-P."))

(defmethod log-dynamic-logging-on (log &optional on-p)
  (declare (ignore log on-p)))

(defmethod log-dynamic-logging-on ((log dynamic-loggin-mixin) &optional on-p)
  (setf (log-dynamic-logging log) (if on-p t nil)))

(defmethod log-dynamic-logging-on ((logs cons) &optional on-p)
  (dolist (log logs)
    (log-dynamic-logging-on log on-p))
  logs)

(define-generic log-file-logging-on (log-or-logs &optional on-p)
  (:documentation "Turns file logging on and off according to on-p."))

(defmethod log-file-logging-on (log &optional on-p)
  (declare (ignore log on-p)))

(defmethod log-file-logging-on ((log file-logging-mixin) &optional on-p)
  (setf (log-file-logging log) (if on-p t nil)))

(defmethod log-file-logging-on ((logs cons) &optional on-p)
  (dolist (log logs)
    (log-file-logging-on log on-p))
  logs)

;;;------------------------------------------------------------------- 
;;;
;;; COUNTING
;;;

;; see *status-code-alist* for the current list of status codes.
(declaim (inline %note-access-status-code))

(defun %note-access-status-code (log status-code)
  (check-type status-code integer)
  ;; with-slots required for locatives to work in Genera.
  (with-slots (n-requests n-requests-served n-server-errors n-client-errors
                          n-insufficient-resource-denials
                          n-redirects n-access-denials) log

    (atomic-incf n-requests)
    (cond
      ((< 199 status-code 300) (atomic-incf n-requests-served))
      ((member status-code '(500 501 505)) (atomic-incf n-server-errors))
      ((= 503 status-code) (atomic-incf n-insufficient-resource-denials))
      ((< 299 status-code 400) (atomic-incf n-redirects))
      ((and (< 400 status-code 416) (/= status-code 408))
       (atomic-incf n-access-denials))
      ((member status-code '(400 408 502) :test #'=)
       (atomic-incf n-client-errors)))
    log))

(define-generic note-access-status-code (log status-code)
  (:documentation "Updates counters in LOG according to status-code."))

(defmethod note-access-status-code ((log log-counters-mixin) status-code)
  (%note-access-status-code log status-code))

(declaim (inline %note-http-method))

(defun %note-http-method (log method)
  ;; with-slots required for locatives to work in Genera.
  (with-slots (n-gets n-heads n-posts n-puts n-deletes
                      n-options n-traces n-extension-methods) log
    (case method
      (:get (atomic-incf n-gets))
      (:head (atomic-incf n-heads))
      (:post (atomic-incf n-posts))
      (:put (atomic-incf n-puts))
      (:delete (atomic-incf n-deletes))
      (:options (atomic-incf n-options))
      (:trace (atomic-incf n-traces))
      (t (typecase method
           (keyword (atomic-incf n-extension-methods)))))))

(define-generic note-http-method (log method)
  (:documentation "Updates counters in LOG according to HTTP method."))

(defmethod note-http-method ((log log-counters-mixin) method)
  (%note-http-method log method))

(declaim (inline %note-http-connections))

(defun %note-http-connections (log requests-completed)
  (with-slots (n-connections) log
    (when (< requests-completed 2)
      (atomic-incf n-connections))))

(define-generic note-http-connections (log requests-completed)
  (:documentation "Updates counters in LOG according to REQUESTS-COMPLETED."))

(defmethod note-http-connections ((log log-counters-mixin) requests-completed)
  (%note-http-connections log requests-completed))

(defmethod elapsed-seconds ((log log-counters-mixin))
  (truncate (elapsed-time log) internal-time-units-per-second))


;;;------------------------------------------------------------------- 
;;;
;;; METERING LOGS
;;;

(define enable-url-metering (&key (port *standard-http-port*) (class 'basic-url-metering))
  "Enable URL metering on port with metering class, CLASS."
  (multiple-value-bind (log newly-created-p)
      (intern-access-log "URL-Metering-Log"
                         :port port
                         :if-does-not-exist :create
                         :class class)
    (unless newly-created-p
      (add-access-log log port))
    (values log newly-created-p)))

(define disable-url-metering (&key (port *standard-http-port*) (class 'basic-url-metering))
  "Disables URL metering on PORT for metering class, CLASS."
  (let ((log (intern-access-log "URL-Metering-Log" :port port
                                :if-does-not-exist :soft
                                :class class)))
    (when log
      (remove-access-log log port))))

;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defmethod log-host-transaction ((host host) (transaction transaction))
  (with-slots (http-transactions http-n-transactions) host
    (push transaction http-transactions)
    (incf http-n-transactions)
    transaction))

(defmethod register-transaction ((transaction transaction) (log dynamic-loggin-mixin))
  (with-slots (url-table n-transactions) log
    (with-slots (url method host) transaction
      (let* ((url (transaction-url transaction))
             (entry (gethash url url-table))
             (bucket (and entry (assoc method entry))))
        (cond (bucket
               (push transaction (cddr bucket))
               (incf (second bucket)))
              (entry
               (push `(,method 1 ,transaction) (cdr entry)))
              (t (setf (gethash url url-table) `((,method 1 ,transaction))))))
      (incf n-transactions)
      ;; tack the transaction onto the host
      (log-host-transaction (transaction-host transaction) transaction)
      transaction)))

(define-generic log-transaction (log url host method time http-version &rest parameters))

(defmethod log-transaction ((log dynamic-loggin-mixin) (url url) (host host) method time http-version &rest parameters)
  (check-type method keyword)
  (check-type time integer)
  (destructuring-bind (&key user status bytes) parameters
    (make-instance 'transaction
                   :url url
                   :host host
                   :user user
                   :method method
                   :time time
                   :server-version http-version
                   :status status 
                   :bytes bytes)))

(defmethod log-transaction :around ((log dynamic-loggin-mixin) (url url) (host host) method time http-version &rest parameters)
  (let ((transaction (apply #'call-next-method log url host method time http-version parameters)))
    (unless (host-http-version host)
      (setf (host-http-version host) http-version))
    (register-transaction transaction log)))

(defmethod log-transaction ((log extended-dynamic-loggin-mixin) (url url) (host host) method time http-version &rest parameters)
  (check-type method keyword)
  (check-type time integer)
  (destructuring-bind (&key user status bytes user-agent referrer) parameters
    (make-instance 'extended-common-log-transaction
                   :url url
                   :host host
                   :user user
                   :method method
                   :time time
                   :server-version http-version
                   :status status 
                   :bytes bytes
                   :user-agent user-agent
                   :referrer referrer)))

;;;------------------------------------------------------------------- 
;;;
;;; RENAME LOG FILES
;;;

(define-parameter *log-pathname-universal-time-offset* 0
                  "The offset in seconds used when creating pathnames for log files.
Use this to ensure that when log files are switched each day, the name reflects
the right day rather than the next day.")

(define-generic log-date-pathname (log &optional ut-offset)
  (declare (values pathname))
  (:documentation "Returns an ISO date prefixed log name.
UT-OFFSET is an offset in seconds to use when creating the pathname.
This defaults to *log-pathname-universal-time-offset*."))

(defmethod log-date-pathname ((log file-logging-mixin) &optional (ut-offset *log-pathname-universal-time-offset*))
  (with-slots (filename) log
    (flet ((date-name (log-name)
             (multiple-value-bind (sec min hour day month year)
                 (decode-universal-time (+ (get-universal-time) ut-offset))
               (declare (ignore sec min hour))
               (values (write-to-string year :base 10.)
                       (format nil "~2,'0D" month)
                       (concatenate 'string (write-iso-date year month day nil) "-"
                                    (pathname-external-name-string log-name))))))
      (declare (inline date-name))
      (multiple-value-bind (year month name)
          (date-name (pathname-name filename))
        (make-pathname :host (pathname-host filename)
                       :directory `(:absolute ,@(cdr (pathname-directory filename)) ,year ,month)
                       :name name
                       :type (pathname-type filename)
                       :version (pathname-version filename))))))

(define-generic switch-log-file (log)
  (:documentation "Switch log files by renaming the current log file to the current date."))

(defmethod switch-log-file ((log file-logging-mixin))
  (with-slots (filename lock) log
    (www-utils:with-lock-held (lock :write)
      (%close-log-stream log)
      (when (probe-file filename)
        (let ((new-name (log-date-pathname log)))
          (pathname-create-directory-if-needed new-name)
          (rename-file filename new-name))))))

(defmethod switch-log-file (log)
  (declare (ignore log))
  nil)

(define switch-all-log-files ()
  "Switches all open log files."
  (map-all-logs #'switch-log-file))

;; add the daily task to swicth the log files.
(www-utils:add-periodic-task "Switch Log Files" :daily '(switch-all-log-files))


;;;------------------------------------------------------------------- 
;;;
;;; PROCESS QUEUED LOGGING
;;;

(defmethod initialize-instance :after ((log process-queued-logging-mixin) &key &allow-other-keys)
  ;; intialization some strings
  (setf (tq:task-queue-wait-whostate log) "Log Wait"
        (tq:task-queue-process-name log) (concatenate 'string "HTTP-" (substitute #\- #\space (log-name log)) "-Daemon"))
  ;; start up the process
  (start-log-queue log)
  log)


(defmethod tq:task-queue-execute-pending-tasks ((log basic-process-queued-file-logging-mixin))
  (labels ((report-logging-error (log error fatal-p)
             (let ((error-type (type-of error)))
               (report-bug *bug-http-server* (format nil "HTTP~:[~; Fatal~] Logging Error: ~S" fatal-p error-type)
                           "~:[~;~&Logging has been suspended. Attend to the error at once and resume logging.~]~
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
    (handler-bind-if (not *debug-server*)
       ((error #'%handle-logging-error))
      (%with-log-stream (log :open-file-forces-output-p t)
        (loop for closure = (tq:pop-task-queue log)
              while closure
              do (funcall closure log-stream)
              while (tq:task-queue-run-p log))))))

(define-generic start-log-queue (log)
  (:documentation "Starts LOG-QUEUE writing log entries by activing its process."))

(defmethod start-log-queue (log)
  (declare (ignore log))
  nil)

(defmethod start-log-queue ((log process-queued-logging-mixin))
  (tq:start-task-queue log))

(define start-all-log-queues ()
  "Starts all processes associated with logging."
  (map-all-logs #'start-log-queue))

(define-generic log-queue-process-kill (log)
  (:documentation "Stops the log queue process and kills it."))

(defmethod log-queue-process-kill (log)
  (declare (ignore log)))

(defmethod log-queue-process-kill ((log process-queued-logging-mixin))
  (tq:task-queue-process-kill log))

(define-generic stop-log-queue (log)
  (:documentation "Stops LOG-QUEUE queue from writing log entries by stopping its process."))

(defmethod stop-log-queue (log)
  (declare (ignore log))
  nil)

(defmethod stop-log-queue ((log process-queued-logging-mixin))
  (tq:stop-task-queue log))

;; Specialize the primary method to run clean up activity.
(defmethod tq:stop-task-queue ((log process-queued-logging-mixin))
  ;; Close the log stream if it is open
  (close-log-stream log))

(define stop-all-log-queues ()
  "Stops all processes associated with logging."
  (map-all-logs #'stop-log-queue))

(define-generic ensure-active-log-queue (log)
  (:documentation "Ensures that log-queue is active and writing log entries."))

(defmethod ensure-active-log-queue ((log process-queued-logging-mixin))
  (tq:ensure-active-task-queue log))

;; stop the log process because it runs without looking at the lock
(defmethod switch-log-file :around ((log basic-file-logging-mixin))
  (unwind-protect
      (prog2 (stop-log-queue log)               ;stop the log queue before switching log files
             (call-next-method))                ;do it
    (start-log-queue log)))                     ;restart

;;;------------------------------------------------------------------- 
;;;
;;; WRITING COMMON LOG ENTRIES
;;;

;; Common Logfile Format (see
;; http://www.w3.org/hypertext/WWW/Daemon/User/Config/Logging.html)

(declaim (inline %write-common-logfile-entry))

(defun %write-common-logfile-entry (host-name request request-time status bytes user-name
                                              &optional (gmt-p *log-times-in-gmt*) (stream *standard-output*)
                                              (delimiter #\space))
  (macrolet ((write-delimiter (stream)
               `(write-char delimiter ,stream)))
    ;; host domain name or IP address.
    (write-string host-name stream)
    ;; RFC932 logname
    (write-delimiter stream)
    (write-char #\- stream)                     ;copy 85% UNIX mentality of NCSA server
    #|(write-rfc-931-logname server stream)|#
    ;; Authenticated User Name
    (write-delimiter stream)
    (if user-name (write user-name :escape t :stream stream) (write-char #\- stream))
    ;; date and time
    (write-delimiter stream)
    (write-char #\[ stream)
    ;; Canonical times in GMT to adhere to the standard and enhance
    ;; portability/comparability.  9/29/95 -- JCMa.
    (if gmt-p
        (write-standard-time request-time stream nil 0)
        (write-standard-time request-time stream t))
    (write-char #\] stream)
    ;; Exact request received from client.
    (write-delimiter stream)
    ;; What should really be done when there is no request string, probably because the
    ;; client got a 408 -- request timeout.  7/20/95 -- JCMa.
    (write (or request "") :stream stream :escape t)
    ;; Status code returned to the client.
    (write-delimiter stream)
    (write status :stream stream :base 10. :escape nil)
    ;; Number of bytes transfered to the client.
    (write-delimiter stream)
    (write bytes :stream stream :base 10. :escape nil)))

(declaim (inline %write-extended-common-logfile-entry))

(defun %write-extended-common-logfile-entry (host-name request request-time status bytes user-name user-agent referer
                                                       &optional (gmt-p *log-times-in-gmt*) (stream *standard-output*)
                                                       (delimiter #\space))
  (macrolet ((write-delimiter (stream)
               `(write-char delimiter ,stream)))
    (%write-common-logfile-entry host-name request request-time status bytes user-name gmt-p stream delimiter)
    (write-delimiter stream)
    (if user-agent (write user-agent :stream stream :escape t) (write-char #\- stream))
    (write-delimiter stream)
    (if referer (write referer :stream stream :escape t) (write-char #\- stream))))


;;;------------------------------------------------------------------- 
;;;
;;; LOGGING POST METHOD
;;;

(defconstant *log-form-special-characters* '(#\tab #\return #\= #\&)
  "Special characters used for encoding forms POST logs.")

(defun print-form-alist-log-entry (form-alist stream)
  (loop for (keyword . values) in form-alist
	do (loop for value in values
		 do (write-escaped-string (symbol-name keyword) *log-form-special-characters* stream)
		    (fast-format stream "~I=~I&"
				 (write-escaped-string (symbol-name keyword) *log-form-special-characters* stream)
				 (write-escaped-string value *log-form-special-characters* stream)))))

(defun parse-form-alist-log-entry (string &optional (start 0) (end (length string)))
  (flet ((get-keyword (string &optional (start 0) (end (length string)))
	   (declare (fixnum start end))
	   (multiple-value-bind (string unescaped-p new-string-p)
	       (string-unescape-special-chars string start end)
	     unescaped-p
	     (if new-string-p
		 (%tokenize-form-query-keyword string)
		 (%tokenize-form-query-keyword string start end)))))
    (declare (inline get-keyword))
    (unless (= start end)
      (with-fast-array-references ((string string string))
        (loop for s1 = start then (1+ (the fixnum e2))
              while (< s1 end)
              for e1 = (or (char-position #\= string s1 end)
                           (error "ill-formed query alist encoding in ~s" (subseq string start end)))
              for s2 = (1+ (the fixnum e1))
              for e2 = (or (char-position #\& string s2 end) end)
              for keyword = (get-keyword string s1 e1)
              for value = (unless (= s2 e2)
                            (string-unescape-special-chars string s2 e2))
              collect `(,keyword ,value))))))

(declaim (inline %write-http-post-logfile-entry))

(defun %write-http-post-logfile-entry (host-name user-name request-time request status bytes-received bytes-transmitted
						 user-agent referer form-alist
						 &optional (gmt-p *log-times-in-gmt*) (stream *standard-output*)
						 (delimiter #\tab))
  (macrolet ((write-delimiter (stream)
               `(write-char delimiter ,stream)))
    ;; host domain name or IP address.
    (write-string host-name stream)
    ;; Authenticated User Name
    (write-delimiter stream)
    (if user-name (write user-name :escape t :stream stream) (write-char #\- stream))
    ;; date and time
    (write-delimiter stream)
    (write-char #\[ stream)
    ;; Canonical times in GMT to adhere to the standard and enhance
    ;; portability/comparability.  9/29/95 -- JCMa.
    (if gmt-p
        (write-standard-time request-time stream nil 0)
        (write-standard-time request-time stream t))
    (write-char #\] stream)
    ;; Exact request received from client.
    (write-delimiter stream)
    ;; What should really be done when there is no request string, probably because the
    ;; client got a 408 -- request timeout.  7/20/95 -- JCMa.
    (write (or request "") :stream stream :escape t)
    ;; Status code returned to the client.
    (write-delimiter stream)
    (write status :stream stream :base 10. :escape nil)
    ;; Number of bytes received from the client.
    (write-delimiter stream)
    (write bytes-received :stream stream :base 10. :escape nil)
    ;; Number of bytes transfered to the client.
    (write-delimiter stream)
    (write bytes-transmitted :stream stream :base 10. :escape nil)
    ;; User Agent
    (write-delimiter stream)
    (if user-agent (write user-agent :stream stream :escape t) (write-char #\- stream))
    (write-delimiter stream)
    ;; Referrer
    (if referer (write referer :stream stream :escape t) (write-char #\- stream))
    ;; Write form alist
    (write-delimiter stream)
    (print-form-alist-log-entry form-alist stream)))

(defun %server-write-post-logfile-entry (server log-stream gmt-p delimiter)
  (let* ((host-name (host-log-name server))
	 (user-name (%server-user-qualified-name server))
	 (request-time (server-request-time server))
	 (request (server-request server t))
	 (status (server-status server))
	 (bytes-transmitted (server-bytes-transmitted server))
	 (bytes-received (server-bytes-received server))
	 (form-alist (server-form-alist server))
	 (headers (server-headers server))
	 (user-agent (get-header :user-agent headers))
	 (referer (get-header :referer headers)))
    (%write-http-post-logfile-entry host-name user-name request-time request status bytes-received bytes-transmitted
				    user-agent referer form-alist gmt-p log-stream delimiter)))

(define enable-post-logging (&key (port *standard-http-port*) (class 'post-log))
  "Enable HTTP POST logging on port with log class, CLASS."
  (multiple-value-bind (log newly-created-p)
      (intern-access-log "Post-Log" :port port :if-does-not-exist :create :class class)
    (unless newly-created-p
      (add-access-log log port))
    (values log newly-created-p)))

(define disable-post-logging (&key (port *standard-http-port*) (class 'post-log))
  "Disables HTTP POST logging on port with log class, CLASS."
  (let ((log (intern-access-log "Post-Log" :port port :if-does-not-exist :soft :class class)))
    (when log
      (remove-access-log log port))))

;;;------------------------------------------------------------------- 
;;;
;;; PARSE LOG FILES
;;;

;;eis.calstate.edu [Tue May 17 03:43:00 1994] GET /stocks.html HTTP/1.0

;(%parse-ncsa-http-1-2-log-entry "eis.calstate.edu [Tue May 17 03:43:00 1994] GET /stocks.html HTTP/1.0")

(defun %parse-ncsa-http-1-2-log-entry (line &aux (l (length line)))
  (declare (values url host method date server-version status byte-content))
  (flet ((white-space-p (x)
           (char= x #\space)))
    (declare (inline white-space-p))
    (let (p1 p2 p3 p4 p5 p6 p7)
      (cond
        ((and (setq p1 (position-if #'white-space-p line :start 0 :end l))
              (setq p2 (char-position #\[ line p1 l))
              (setq p3 (char-position #\] line p2 l))
              (setq p4 (position-if-not #'white-space-p line :start (the fixnum (1+ p3)) :end l))
              (setq p5 (position-if #'white-space-p line :start p4 :end l))
              (setq p6 (position-if-not #'white-space-p line :start p5 :end l)))
         (setq p7 (position-if #'white-space-p line :start p6 :end l))
         (values (subseq line p6 (or p7 l))
                 (subseq line 0 p1)
                 (intern (nstring-upcase (subseq line p4 p5)) *keyword-package*)
                 (parse-gmt-time line (the fixnum (1+ p2)) p3)
                 (when p7
                   (let ((p8 (position-if-not #'white-space-p line :start p7 :end l)))
                     (subseq line p8 l))) nil nil)) ;; no status or byte-content info for NCSA.
        (t (values nil nil :bad-record))))))

; (%parse-common-log-format-log-entry "GATOR-MAC-9.AI.MIT.EDU - - [1994-09-28 00:51:21] \"GET /homepage HTTP/1.0\" 200 3856")

(defun %parse-common-log-format-log-entry (line &aux (end (length line)))
  (declare (values url host method date server-version status bytes user-name)
	   (fixnum end))
  (labels ((white-space-p (x)
	     (char= x #\space))
	   (parse-exact-request (exact-request start end)
	     (declare (fixnum end))
	     (unless (eql start end)
	       (let* ((r1 (%fast-position-if white-space-p exact-request :start start :end end))
		      (r2 (%fast-position-if white-space-p exact-request :start (1+ r1) :end end))
		      (method (%tokenize-header-keyword exact-request start r1))
		      (url (subseq exact-request (1+ r1) (or r2 end)))
		      (server-version (if r2
					  (%tokenize-header-keyword exact-request (the fixnum (1+ r2)) end)
					  :HTTP/0.9)))	; http 0.9 or telnet hackers don't send version
		 (declare (fixnum r1)
			  (type (or null fixnum) r2))
		 (values method url server-version))))
	   (parse-user (line start end)
	     (declare (fixnum start))
	     (if (and (eql #\- (aref line start)) (eql #\space (aref line (1+ start))))
		 nil
		 (subseq line start (1+ (the fixnum (%fast-position-if-not white-space-p line :start start :end end :from-end t)))))))
    (declare (inline white-space-p parse-exact-request parse-user))
    (let (p1 p2 p3 p4 p5 p6 p7 p8)
      p3
      (cond
	((and (setq p1 (%fast-position-if white-space-p line :start 0 :end end))
	      (setq p2 (%fast-position-if white-space-p line :start (1+ (the fixnum p1)) :end end))
	      (setq p3 (%fast-position-if white-space-p line :start (1+ (the fixnum p2)) :end end))
	      (setq p4 (char-position #\[ line (1+ (the fixnum p3)) end))
	      (setq p5 (char-position #\] line (1+ (the fixnum p4)) end))
	      (setq p6 (char-position #\" line (1+ (the fixnum p5)) end))
	      (setq p7 (char-position #\" line (1+ (the fixnum p6)) end))
	      (< (1+ (the fixnum p6)) p7)       ;empty request string is a bad entry  7/20/95 -- JCMa.
	      (setq p8 (%fast-position-if white-space-p line :start (+ 2 (the fixnum p7)) :end end)))
	 (locally
	   (declare (fixnum p1 p2 p3 p4 p5 p6 p7 p8))
	   (multiple-value-bind (method url server-version)
	       (parse-exact-request line (1+ p6) p7)
	     (let ((host (subseq line 0 p1))
		   (date (parse-gmt-time line (1+ p4) p5))
		   (status (parse-integer line :start (+ 2 p7) :end p8))
		   (bytes (parse-integer line :start (1+ p8) :end end))
		   (user (parse-user line (1+ p2) p4)))
	       (values url host method date server-version status bytes user)))))
	(t (values nil nil :bad-record))))))

;; Consider tokenizing BROWSER, REFERER and USER.
(defun %parse-extended-common-log-format-log-entry (line &optional (delimiter #\tab) &aux (end (length line)))
  (declare (values url host method date server-version status bytes user-name browser referer)
           (fixnum end))
  (labels ((field-delimiter-char-p (x)
             (char= x delimiter))
           (white-space-p (x)
             (char= x #\space))
           (parse-exact-request (exact-request start end)
             (declare (fixnum end))
             (unless (eql start end)
               (let* ((r1 (%fast-position-if white-space-p exact-request :start start :end end))
                      (r2 (%fast-position-if white-space-p exact-request :start (1+ r1) :end end))
                      (method (%tokenize-header-keyword exact-request start r1))
                      (url (subseq exact-request (1+ r1) (or r2 end)))
                      (server-version (if r2
                                          (%tokenize-header-keyword exact-request (the fixnum (1+ r2)) end)
                                          :HTTP/0.9)))	; http 0.9 or telnet hackers don't send version
                 (declare (fixnum r1)
                          (type (or null fixnum) r2))
                 (values method url server-version))))
           (parse-field (line start end bounding-delimiter)
             (declare (fixnum start))
             (if (eql #\- (aref line start))
                 nil
                 (let ((s1 (char-position bounding-delimiter line start end))
                       (s2 (char-position bounding-delimiter line start end t)))
                   (when (and s1 s2)
                     (subseq line (1+ (the fixnum s1)) s2))))))
    (declare (inline field-delimiter-char-p parse-exact-request parse-field))
    (let (p1 p2 p3 p4 p5 p6 p7 p8 p9 p10)
      p3
      (cond
        ((and (setq p1 (%fast-position-if field-delimiter-char-p line :start 0 :end end))
              (setq p2 (%fast-position-if field-delimiter-char-p line :start (1+ (the fixnum p1)) :end end))
              (setq p3 (%fast-position-if field-delimiter-char-p line :start (1+ (the fixnum p2)) :end end))
              (setq p4 (char-position #\[ line (1+ (the fixnum p3)) end))
              (setq p5 (char-position #\] line (1+ (the fixnum p4)) end))
              (setq p6 (char-position #\" line (1+ (the fixnum p5)) end))
              (setq p7 (char-position #\" line (1+ (the fixnum p6)) end))
              (< (1+ (the fixnum p6)) p7)       ;empty request string is a bad entry  7/20/95 -- JCMa.
              (setq p8 (%fast-position-if field-delimiter-char-p line :start (+ 2 (the fixnum p7)) :end end))
              (setq p9 (%fast-position-if field-delimiter-char-p line :start (1+ (the fixnum p8)) :end end))
              (setq p10 (%fast-position-if field-delimiter-char-p line :start (1+ (the fixnum p9)) :end end)))
         (locally
           (declare (fixnum p1 p2 p3 p4 p5 p6 p7 p8))
           (multiple-value-bind (method url server-version)
               (parse-exact-request line (1+ p6) p7) 
             (let ((host (subseq line 0 p1))
                   (date (parse-gmt-time line (1+ p4) p5))
                   (status (parse-integer line :start (+ 2 p7) :end p8))
                   (bytes (parse-integer line :start (1+ p8) :end p9))
                   (user (parse-field line (1+ p2) p4 #\"))
                   (browser (parse-field line p9 p10 #\"))
                   (referer (parse-field line p10 end #\")))
               (values url host method date server-version status bytes user browser referer)))))
        (t (values nil nil :bad-record))))))

#|gnoscere.ai.mit.edu  - [1999-07-12 20:37:28] "POST /cl-http/find-documentation.html HTTP/1.1" 200 560 3577 "Mozilla/4.0 (compatible; MSIE 4.5; Mac_PowerPC)" "http://fuji-vlm.ai.mit.edu/cl-http/find-documentation.html" ((:SUBSTRING "flog") (:MODULE "HTTP") (:LISP-TYPE "ALL") (:DOCUMENTATION "YES") (:EXTERNAL "NO") (:SUBMIT "Submit"))|#

(defun %parse-http-post-log-entry (line &optional (start 0) (end (length line)))
  (declare (values url host method date http-version status bytes-received bytes-transmitted user referer
		   form-alist user-agent ua-version ua-comment)
	   (fixnum start end))
  (labels ((delimiter-char-p (x)
	     (char= x #\tab))
	   (white-space-p (x)
	     (char= x #\space))
	   (null-entry-p (string start end)
	     (declare (fixnum start end))
	     (and (= (1+ start) end) (eql #\- (aref string start))))
	   (parse-exact-request (exact-request start end)
	     (declare (fixnum start end))
	     (unless (eql start end)
	       (let* ((r1 (%fast-position-if white-space-p exact-request :start start :end end))
		      (r2 (%fast-position-if white-space-p exact-request :start (1+ r1) :end end))
		      (method (%tokenize-header-keyword exact-request start r1))
		      (url (subseq exact-request (1+ r1) (or r2 end)))
		      (server-version (if r2
					  (%tokenize-header-keyword exact-request (the fixnum (1+ r2)) end)
					  :HTTP/0.9)))	; http 0.9 or telnet hackers don't send version
		 (declare (fixnum r1)
			  (type (or null fixnum) r2))
		 (values method url server-version))))
	   (parse-user (line start end)
	     (declare (fixnum start end))
	     (subseq line start (1+ (the fixnum (%fast-position-if-not delimiter-char-p line :start start :end end :from-end t))))))
    (declare (inline delimiter-char-p parse-exact-request parse-user))
    (let (p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13)
      p3
      (cond
	((and (setq p1 (%fast-position-if-not delimiter-char-p line :start start :end end))
	      (setq p2 (%fast-position-if delimiter-char-p line :start (1+ (the fixnum p1)) :end end))
	      (setq p3 (%fast-position-if delimiter-char-p line :start (1+ (the fixnum p2)) :end end))
	      (setq p4 (char-position #\[ line (1+ (the fixnum p3)) end))
	      (setq p5 (char-position #\] line (1+ (the fixnum p4)) end))
	      (setq p6 (char-position #\" line (1+ (the fixnum p5)) end))
	      (setq p7 (char-position #\" line (1+ (the fixnum p6)) end))
	      (< (1+ (the fixnum p6)) p7)	;empty request string is a bad entry  7/20/95 -- JCMa.
	      (setq p8 (%fast-position-if delimiter-char-p line :start (1+ (the fixnum p7)) :end end))
	      (setq p9 (%fast-position-if delimiter-char-p line :start (1+ (the fixnum p8)) :end end))
	      (setq p10 (%fast-position-if delimiter-char-p line :start (1+ (the fixnum p9)) :end end))
	      (setq p11 (%fast-position-if delimiter-char-p line :start (1+ (the fixnum p10)) :end end))
	      (setq p12 (%fast-position-if delimiter-char-p line :start (1+ (the fixnum p11)) :end end))
	      (setq p13 (%fast-position-if delimiter-char-p line :start (1+ (the fixnum p12)) :end end)))
	 (locally
	   (declare (fixnum p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13))
	   (multiple-value-bind (method url http-version)
	       (parse-exact-request line (1+ p6) p7)
	     method
	     (let ((host (subseq line p1 p2))
		   (user (unless (null-entry-p line (1+ p2) p3)
			   (parse-user line (1+ p2) p3)))
		   (date (parse-gmt-time line (1+ p4) p5))
		   (status (parse-integer line :start (1+ p8) :end p9))
		   (bytes-received (parse-integer line :start (1+ p9) :end p10))
		   (bytes-transmitted (parse-integer line :start (1+ p10) :end p11))
		   (referer (unless (null-entry-p line (1+ p12) p13)
			      (subseq line (+ 2 p12) (1- p13))))
		   (form-alist (parse-form-alist-log-entry line (1+ p13) end)))
	       (multiple-value-bind (user-agent ua-version ua-comment)
		   (unless (null-entry-p line (1+ p11) p12)
		     (parse-user-agent line (+ 2 p11) (1- p12)))
		 (values url host method date http-version status bytes-received bytes-transmitted user referer
			 form-alist user-agent ua-version ua-comment))))))
	(t (values nil nil :bad-record))))))

(define-parameter *log-line-parser-alist* '((:ncsa-1-2 . %parse-ncsa-http-1-2-log-entry)
                                            (:common-log-format . %parse-common-log-format-log-entry)
                                            (:extended-common-log-format . %parse-extended-common-log-format-log-entry)
                                            (:post-log-format . %parse-http-post-log-entry)))

(define-parameter *default-log-line-parser* :common-log-format
                  "The default log format for parsing log files.")

(defun log-line-parser (log-type)
  (or (cdr (assoc log-type *log-line-parser-alist*))
      (error "Unknown log type, ~S." log-type)))

(defgeneric parse-log-file (log pathname &key log-format stream)
   (:documentation "Parses the log PATHNAME according to LOG-FORMAT.
LOG-FORMAT defaults to the value of *default-log-line-parser*.
Errors are reported on STREAM."))

(defmethod parse-log-file ((log dynamic-loggin-mixin) pathname &key (log-format *default-log-line-parser*) (stream *standard-output*))
  (with-slots (local-host) log
    (with-open-file (file pathname :direction :input)
      (using-resource (line-buffer line-buffer *line-buffer-size*)
	(loop with parser = (log-line-parser log-format)
	      with context = (concatenate 'string "HTTP://" (host-domain-name local-host))
	      for line = (read-delimited-line file '(#\return #\Linefeed) nil line-buffer)
	      while line
	      do (multiple-value-bind (url host method date server-version)
		     (funcall parser line)
		   (cond
		     ((member method '(:get :head :post))
		      (handler-case
			(let ((url-string (merge-url url context)))
			  (declare (dynamic-extent url-string))
			  (log-transaction log
					   (intern-url url-string :if-does-not-exist :create)
					   (intern-host log :domain-name host :object host)
					   method
					   date
					   server-version))
			(parsing-error () (format stream "~&Bad URL Syntax: ~S" line))))
		     (t (format stream "~&Not Parsed by ~A: ~S" log-format line)))))))))

(defmethod url-transactions ((log dynamic-loggin-mixin) (url url))
  (with-slots (url-table) log
    (values (gethash url url-table) url)))

(defmethod url-transactions ((log dynamic-loggin-mixin) (url-string string))
  (multiple-value-bind (url)
      (url:intern-url url-string :if-does-not-exist :soft)
    (when url
      (url-transactions log url))))

(defmethod client-host-name ((transaction transaction))
  (with-slots (host) transaction
    (host-name host)))

(defgeneric show-url-transactions (log url-string &key stream))

(defmethod show-url-transactions ((log dynamic-loggin-mixin) (url-string string) &key (stream *standard-output*))
  (multiple-value-bind (transactions url)
      (url-transactions log url-string)
    (loop for (method n . trns) in transactions
          do (format stream "~&~D ~A transaction~P on ~A" n method n (name-string url))
             (when (y-or-n-p "Describe them? ")
               (loop for tr in trns ;;(sort trns #'string< :key #'client-host-name)
                     do (describe tr))))))
