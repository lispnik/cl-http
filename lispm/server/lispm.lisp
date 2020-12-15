;;;   -*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-
;;;
;;; (c) Copyright  1994-99, John C. Mallery
;;;     All Rights Reserved.
;;;


;;;------------------------------------------------------------------- 
;;;
;;; GENERAL
;;;

(in-package :www-utils)

;; when this feature is present, authors are included in directory listing under HTML3
;; UNIX file directories don't seem to bright so remove this feature for the VLM until
;; someone has a better idea.   9/27/96 -- JCMa.
#-VLM
(pushnew :cl-http-file-author *features*)

(pushnew :genera-cl-http *features*)

(pushnew :multi-threaded *features*)

;; Genera 8.3 gets confused at load time unless the mapped symbols are
;; interned via explicit calls to intern! Event though package prefixed
;; symbols works for the compile time version it lost for the load time
;; version. - JCMa -- 9/15/1995
(eval-when (load eval compile)
  (mapc #'(lambda (x)
            (shadowing-import x :www-utils)
            (export x :www-utils))
        (list (intern "FIXNUM-MICROSECOND-TIME" :time)
              (intern "FILE-NOT-FOUND" :fs)
              (intern "PARSE-PATHNAME-ERROR" :fs)
              (intern "BYTES-TRANSMITTED" :tcp)
              (intern "BYTES-RECEIVED" :tcp)
              (intern "DEFAULT-PATHNAME" :fs)
              (intern "DEFRESOURCE" :scl)
              (intern "CLEAR-RESOURCE" :scl)
              (intern "ALLOCATE-RESOURCE" :scl)
              (intern "DEALLOCATE-RESOURCE" :scl)
              (intern "USING-RESOURCE" :scl)
              (intern "GENERIC-FUNCTION-METHODS" :clos)
              (intern "METHOD-SPECIALIZERS" :clos)
              (intern "CLASS-DIRECT-SUPERCLASSES" :clos)
              (intern "PROCESS-ACTIVE-P" :process)
              (intern "PROCESS-IDLE-TIME" :process)
              (intern "PROCESS-KILL" :process)
              (intern "PROCESS-PRESET" :process)
	      (intern "PROCESS-PRIORITY" :si)
              (intern "PROCESS-RESET" :process)
              (intern "PROCESS-RUN-FUNCTION" :process)
              (intern "PROCESS-RUN-TIME" :process)
              (intern "PROCESS-WAIT" :process)
              (intern "PROCESS-WHOSTATE" :process)
              (intern "PROCESS-WAIT-WITH-TIMEOUT" :process)
	      (intern "WITH-TIMEOUT" :process)
              (intern "CHUNK-TRANSFER-ENCODING-MODE" :tcp)
              (intern "NOTE-FIRST-CHUNK" :tcp)
              (intern "NOTE-LAST-CHUNK" :tcp)
              (intern "CHUNK-TRANSFER-DECODING-MODE" :tcp)
              (intern "CHUNK-TRANSFER-DECODING-MODE-END" :tcp)
              (intern "END-OF-CHUNK-TRANSFER-DECODING" :tcp)
              (intern "CHUNK-TRANSFER-CONTENT-LENGTH" :tcp)
              (intern "CHUNK-TRANSFER-CONTENT-LENGTH-HEADER" :tcp)
	      (intern "INPUT-CHUNK-SIZE" :tcp)
	      (intern "MODIFY-HASH" :scl)
              )))

;; a series of network conditions that we would like to be able to within
;; portable code.
(eval-when (load eval compile)
  (mapc #'(lambda (x)
            (destructuring-bind (sym pkg) x
              (let ((sym (intern sym pkg)))
                (import sym :www-utils)
                (export sym :www-utils))))
        '(("BAD-CONNECTION-STATE" :sys)
          ("CONNECTION-CLOSED" :sys)
          ("CONNECTION-ERROR" :sys)
          ("CONNECTION-LOST" :sys)
          ("CONNECTION-REFUSED" :sys)
          ("DOMAIN-RESOLVER-ERROR" :neti)
          ("HOST-NOT-RESPONDING" :sys)
          ("HOST-STOPPED-RESPONDING" :sys)
          ("LOCAL-NETWORK-ERROR" :sys)
          ("NETWORK-ERROR" :sys)
          ("NETWORK-RESOURCES-EXHAUSTED" :sys)
          ("PROTOCOL-TIMEOUT" :neti)
          ("REMOTE-NETWORK-ERROR" :sys)
          ("UNKNOWN-ADDRESS" :sys)
          ("UNKNOWN-HOST-NAME" :sys)))

;; Remap the name of parse error to avoid collisions
  (deftype network-parse-error () 
    "Specialization of Common Lisp File-error in which the file was not found on open."
    '(and condition sys:parse-error))

  (export 'network-parse-error :www-utils))

(define-condition network-error-mixin
                  (network-error)
  ()
  (:documentation "Mixin to allow ports to inherit instance variables and methods to network conditions
defined at the portable code level."))

(scl:defmethod (:network network-error-mixin) ()
  (neti:find-network-named :internet))

(scl:defmethod (:network-type network-error-mixin) ()
  :tcp)

(define-macro with-fast-array-references (variable-bindings &body body)
  "Establishes local variables that are array registers.
Use just like let in the places fast array references are desired."
  (loop for (var val) in variable-bindings
        collect var into array-registers
        collect `(,var ,val) into bindings
        finally (return `(let ,bindings
                           (declare (sys:array-register ,@array-registers))
                           ,@body))))

(define-macro with-tcp-port-for-protocol ((protocol port) &body body)
  "Makes PROTOCOL use port PORT with the scope of BODY."
  `(scl:let-if (/= ,port 80.)
               ((tcp::*tcp-protocol-alist* (list* (list* ,port ,protocol) tcp::*tcp-protocol-alist*)))
     ,@body))

(declaim (inline report-condition))

(define report-condition (condition stream)
  "Prints the report string for CONDITION onto STREAM."
  (dbg:report condition stream))

(declaim (inline report-string))

(define report-string (condition)
  "Returns the report string for CONDITION."
  (dbg:report-string  condition))

(define-macro with-null-stream ((&rest streams) &body body)
  "Binds streams to the null-stream within BODY."
  (loop for stream in streams
        collect `(,stream #'sys:null-stream) into bindings
        finally (return `(let ,bindings ,@body))))

(define-macro atomic-incf (reference &optional (delta 1))
  `(process:atomic-incf ,reference ,delta))

(define-macro atomic-decf (reference &optional (delta 1))
  `(process:atomic-decf ,reference ,delta))

(define-macro atomic-push (item reference)
  `(process:atomic-push ,item ,reference))

(define-macro atomic-pop (reference)
  `(process:atomic-pop ,reference))

(define-macro atomic-replacef (reference new-value)
  `(process:atomic-replacef ,reference ,new-value))

(define-macro atomic-conditional-replacef (reference predicate new-value)
  "When PREDICATE returns non-null, this setfs REFERENCE to NEW-VALUE.
Predicate is called (OLD-VALUE NEW-VALUE). The operation 
assures that precicate applicaiton ande swap are atomic."
  (declare (values old-value value-replaced-p))
  (let ((location (gensym))
	(old-value (gensym))
	(pred (gensym)))
    `(let ((,location (scl:locf ,reference))
	   (,pred ,predicate))
       (loop for ,old-value = (scl:location-contents ,location)
	     while (funcall ,pred ,old-value ,new-value)
	     when (scl:store-conditional ,location ,old-value ,new-value)
	       return (values ,old-value t)
	     finally (return nil)))))

(declaim (inline arglist))

(defun arglist (function)
  (declare (values (arglist values)))
  (scl:arglist function))

(declaim (inline special-form-p))

(defun special-form-p (function)
  (lisp:special-form-p function))

(export 'special-form-p :www-utils)

(setf (fdefinition '%CHAR-EQUAL) (fdefinition 'si:char-equal-internal)
      (symbol-plist '%CHAR-EQUAL) (symbol-plist 'si:char-equal-internal))

(define-macro let-if (condition bindings &body forms)
  `(scl:let-if ,condition ,bindings . ,forms))


;;;------------------------------------------------------------------- 
;;;
;;; SYSTEM-RELATED
;;;

(defparameter *system* (sct:find-system-named "CL-HTTP")) 

(define %server-version-info()
  "Returns the version numbers for the server and the port."
  (declare (values major minor))
  (multiple-value-bind (major minor)
      (sct:get-system-version *system*)
    (values (list major minor))))


;;;------------------------------------------------------------------- 
;;;
;;; HOST DOMAIN NAME HACKING
;;;

(defvar *internet-network* (neti:find-object-named :network "INTERNET"))

(declaim (inline host-internet-address))

(defun host-internet-address (host)
  (check-type host NET:HOST)
  (second (assoc *internet-network* (scl:send host :address) :test #'neti:ns-eq)))

(declaim (inline host-parsed-internet-address))

(defun host-parsed-internet-address (host)
  (check-type host NET:HOST)
  (second (assoc *internet-network* (scl:send host :network-addresses) :test #'neti:ns-eq)))

(declaim (inline host-eq))

(define host-eq (host1 host2)
  "Returns non-null if host1 is the same host as host2."
  (neti:ns-eq host1 host2) ;;Broken by nickname breakage in namespace  10/2/94 -- JCMa.
  #| (= (host-parsed-internet-address host1)
     (host-parsed-internet-address host2))|#)

(declaim (inline ip-address-for-parsed-ip-address))

(define ip-address-for-parsed-ip-address (ip-number)
  "Returns an IP address as a string from, IP-NUMBER, the parsed address."
  (neti:unparse-internet-address ip-number))

(define ip-address-for-host-domain-name (domain-name)
  "Returns the IP address string for domain-name."
  (host-internet-address (neti:parse-host domain-name)))

(define domain-name-for-parsed-ip-address (ip-number &optional (no-error-p t))
  "Given the parsed IP address (an integer), IP-NUMBER, this returns the domain name or NIL."
  (%domain-name-for-ip-address (neti:unparse-internet-address ip-number) no-error-p))

(define local-host ()
  "The host object for the local host on which we are running."
  net:*local-host*)

(define local-host-ip-address (&optional recache-p)
  "Returns the IP address of the local host."
  (cond ((and (not recache-p) http:*local-host-ip-address*))
        (t (setq http:*local-host-ip-address* (host-internet-address neti:*local-host*)))))

(define local-host-parsed-ip-address (&optional recache-p)
  "Returns the parsed IP address of the local host."
  (cond ((and (not recache-p) http:*local-host-address*))
        (t (setq http:*local-host-address* (host-parsed-internet-address neti:*local-host*)))))

(define local-host-domain-name (&optional recache-p)
  "Returns the local host domain name."
  (cond ((and (not recache-p) http:*local-host-domain-name*))
        (t (setq http:*local-host-domain-name*
                 (or http:*http-host-name* (host-mail-name neti:*local-host*))))))


;;;------------------------------------------------------------------- 
;;;
;;; OBTAINING THE DOMAIN NAME FOR AN IP ADDRESS
;;;

(define %domain-name-for-ip-address (address &optional (no-error-p t))
  "Given the IP address, ADDRESS, this returns the domain name or NIL."
  (declare (values domain-name successfully-resolved-p))
  (check-type address string)
  (labels ((unparse-domain-style-name (domain-name-list)
	     (apply #'concatenate 'string
		    (loop with length = (length domain-name-list)
			  for item in domain-name-list
			  for idx upfrom 1
			  collect (nstring-upcase item)
			  while (< idx length)
			  collect ".")))
	   (do-it (address)
	     ;; Special magic to get reverse parse out of the Lispm DNS system.
	     (let* ((name-list `("ARPA" "IN-ADDR" ,@(neti:parse-domain-style-name address)))
		    (domain-spec (neti:domain-query (nreverse name-list)))
		    (domain-list (fifth (car domain-spec))))
	       (if domain-list
		   (values (unparse-domain-style-name domain-list) t)
		   (values address nil)))))
    (declare (dynamic-extent #'unparse-domain-style-name))
    (if no-error-p
	(handler-case
	  (do-it address)
	  (neti:domain-resolver-error () (values address nil)))
	(do-it address))))

(declaim (special http:*resolve-ip-addresses*))

(define domain-name-for-ip-address (address &optional update-object-p)
  (declare (values domain-name successfully-resolved-p))
  (check-type address string)
  (flet ((ip-string (host)
           (second (assoc *internet-network* (scl:send host :address))))
         (resolved-domain-name-maybe (name host)
           (multiple-value-bind (domain-name success-p)
               (%domain-name-for-ip-address (subseq name 9))
             (when (and update-object-p success-p)
               (scl:send host :set-names (list domain-name))
               (scl:send host :change-of-attributes))
             domain-name)))
    (declare (inline ip-string resolved-domain-name-maybe))
    (cond ((string= "INTERNET|" address  :start1 0 :end1 9 :start2 0 :end2 9)
           (multiple-value-bind (host)
               (net:parse-host address nil)
             (if http:*resolve-ip-addresses*
                 (resolved-domain-name-maybe address host)
                 (ip-string host))))
          (t address))))

;; these hosts are created for each connection.  Why not cach the host
;; object so that repeated contact recycle the same host?  6/10/94 --
;; Mail-Server.

(declaim (inline %host-domain-name))

(defun %host-domain-name (host &optional update-object-p (resolve-p http:*resolve-ip-addresses*))
  (flet ((ip-string (host)
           (second (assoc *internet-network* (scl:send host :address))))
         (resolved-domain-name-maybe (name host)
           (multiple-value-bind (domain-name success-p)
               (%domain-name-for-ip-address (subseq name 9))
             (when (and update-object-p success-p)
               (scl:send host :set-names (list domain-name))
               (scl:send host :change-of-attributes))
             domain-name)))
    (declare (inline ip-string resolved-domain-name-maybe))
    (let ((name (scl:send host :primary-name)))
      (etypecase name
        (string
          (cond ((string= "INTERNET|" name  :start1 0 :end1 9 :start2 0 :end2 9)
                 (if resolve-p
                     (resolved-domain-name-maybe name host)
                     (ip-string host)))
                (t name)))
        (neti:name (scl:send host :mail-name))))))

(defmethod http::host-domain-name ((host NET:HOST) &optional update-object-p)
  "Returns the domain name of HOST as a string."
  (%host-domain-name host update-object-p http:*resolve-ip-addresses*))

(declaim (special http:*resolve-ip-addresses-for-log*))

(defmethod http::host-log-name ((host NET:HOST))
  "Returns the domain name of HOST as a string."
  (%host-domain-name host nil http:*resolve-ip-addresses-for-log*))

(declaim (inline %host-log-name))

(define %host-log-name (address host &optional resolve-ip-address-p)
  "Returns a string for use in logging server access."
  (declare (ignore address))
  (%host-domain-name host resolve-ip-address-p resolve-ip-address-p)) 

(define parse-host (host &optional no-error-p (inhibit-validity-checking-p neti:*inhibit-validity-checking*))
  "Parses HOST and returns a host object."
  (typecase host
    (net:host host)
    (string 
      (cond ((equalp host "localhost") (local-host))    ; RFC 1738 says defines this string   5/3/96 -- JCMa.
            ((url:numeric-hostname-p host)
             (let ((host-spec (concatenate 'string "INTERNET|" host)))
               (declare (dynamic-extent host-spec))
               (net:parse-host host-spec no-error-p)))
            (t (net:parse-host host no-error-p))))
    ;; feed it to parse-host to signal the error as necessary
    (t (unless no-error-p
         (net:parse-host host no-error-p (not inhibit-validity-checking-p))))))

;(define host-http-name (host)
;  "Returns the internet host name for HOST."
;  (let ((h (parse-host host)))
;    (cond ((host-eq h neti:*local-host*)
;           (or http:*http-host-name*
;               (scl:send h :mail-name)))
;          (t (scl:send h :mail-name)))))

;; lispm-specific function.  2/24/96 -- JCMa.
(defun debracket-host-name (string &optional (start 0) (end (length string)))
  (flet ((ip-address-p (string s e)
           (loop for start = s then (and dot-pos (1+ dot-pos))
                 for dot-pos = (http::char-position-2-case #\. string start e)
                 for count from 1
                 for (number next-pos) = (multiple-value-list
                                           (parse-integer string :start start :end dot-pos :radix 10 :junk-allowed t))
                 unless (and (<= 0 number 255)
                             (eql next-pos dot-pos))
                   return nil
                 while dot-pos
                 finally (return (eql 4 count)))))
    (let (s e)
      (cond ((and (> end 2)
		  (char-equal #\[ (aref string 0))
		  (char-equal #\] (aref string (setq e (1- end))))
		  (ip-address-p string (setq s (1+ start)) e))
	     (subseq string s e))
	    ((and (not e) (not s) (zerop start) (= end (length string)))
	     string)
	    (t (subseq string (or s start) (or e end)))))))

(define host-http-name (host)
  "Returns the internet host name for HOST."
  (flet ((www-name (host)
           (or (scl:send host :user-get :http-name)
               (scl:send host :mail-name))))
    (declare (inline host-http-name))
    (let ((h (parse-host host)))
      (cond ((host-eq h neti:*local-host*)
             (or http:*http-host-name*
                 (debracket-host-name (www-name h))))
            (t (debracket-host-name (www-name h)))))))

(define host-mail-name (host)
  "The internet mail name for HOST."
  (declare (notinline))
  (debracket-host-name (scl:send (parse-host host) :mail-name)))

(scl:defmethod (host-name fs::lispm-host) ()
  (debracket-host-name (scl:send scl:self :mail-name)))

(scl:defmethod (host-short-name fs::lispm-host) ()
  (scl:send scl:self :name))

;(define server-mail-address ()
;  "Returns the mail address for the current user."
;  (destructuring-bind (uid host)
;      (scl:send si:*user* :mail-address)
;    (concatenate 'string uid "@" (host-mail-name host))))

(declaim (inline local-port))

(define local-port (http-stream)
  "Returns the local host port for the remote connection via http-stream."
  (scl:send http-stream :local-port))

(declaim (inline foreign-port))

(define foreign-port (http-stream)
  "Returns the foreign host port for the remote connection via http-stream."
  (scl:send http-stream :foreign-port))

(declaim (inline foreign-host))

(define foreign-host (http-stream)
  "Returns the foreign host for the remote connection via http-stream."
  (scl:send http-stream :foreign-host))


;;;------------------------------------------------------------------- 
;;;
;;; DAILY TASKS
;;;

(defvar *daily-server-timer* nil)

(defun daily-server-timer ()
  (or *daily-server-timer*
      (setq *daily-server-timer* (process:create-timer-call 'run-daily-server-tasks '()
                                                            :name "Daily Server Tasks"))))

(defvar *daily-server-tasks* nil)

;; A better time to run night time jobs, because it is midnight on the west coast.
(define next-3am-universal-time (&optional (offset 0) (reference-time (get-universal-time)))
  "Returns the universal time for the next 3am relative to REFERENCE-TIME.
OFFSET is a positive or negative number of seconds relative to 3am."
  (multiple-value-bind (seconds minutes hours date month year day-of-the-week)
      (decode-universal-time reference-time)
    (declare (ignore day-of-the-week))
    (+ (if (plusp (- (+ seconds (* 60 minutes) (* #.(* 60 60) hours))
		     #.(* 3 60 60)		;3am
		     offset))
	   #.(* 60. 60. 24.)			;plus 24 hours
	   0)
       offset					;offset
       (encode-universal-time 0 0 3. date month year (time-zone)))))

;; set up a timer for mighnight to start the 24 interval timer
(defun synchronize-daily-server-tasks ()
  (process:reset-timer-absolute (daily-server-timer) (next-3am-universal-time)))

(defun weekday ()
  (declare (values weekday-keyword))
  (multiple-value-bind (seconds minutes hours date month year day-of-the-week)
      (decode-universal-time (get-universal-time))
    seconds minutes hours date month year       ;ignore
    (intern (string-upcase (time:day-of-the-week-string day-of-the-week :long)) :keyword)))

;; toplevel routines for getting work done everyday at midnight PST.
(defun run-daily-server-tasks ()
  (www-utils:with-null-stream (*standard-output* *query-io*)
    (loop with week-day = (weekday)
          for (name periodicity form) in *daily-server-tasks*
          when (case periodicity
                 (:daily t)
                 (:weekly name (eq periodicity :sunday))
                 (t (eq periodicity week-day)))
            do (handler-case
                 (apply (car form) (cdr form))
                 (error
                   (err)
                   (log-http-server-error
                     "Error in daily server task, ~S:~&~A"
                     name (report-string err))))))
  ;; Reset so we run again.
  (synchronize-daily-server-tasks))

(defvar *hourly-server-timer* nil)

(defun hourly-server-timer ()
  (or *hourly-server-timer*
      (setq *hourly-server-timer* (process:create-timer-call 'run-hourly-server-tasks '()
                                                            :name "Hourly Server Tasks"))))
(defvar *hourly-server-tasks* nil)

(defun next-hourly-universal-time (&optional (offset 0))
  (check-type offset (integer 0 59))
  (multiple-value-bind (seconds minutes hours date month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore seconds minutes))
    (+ (encode-universal-time 0 offset hours date month year) (* 60. 60.))))

;; Set up a timer to run every hour on the half hour.
(defun synchronize-hourly-server-tasks ()
  (process:reset-timer-absolute (hourly-server-timer) (next-hourly-universal-time 30)))

;; toplevel routines for getting work done everyday at midnight PST.
(defun run-hourly-server-tasks ()
  (www-utils:with-null-stream (*standard-output* *query-io*)
    (loop with week-day = (weekday)
          for (name periodicity form) in *hourly-server-tasks*
          when (case periodicity
                 (:hourly t)
                 (:weekly name (eq periodicity :sunday))
                 (t (eq periodicity week-day)))
            do (handler-case
                 (apply (car form) (cdr form))
                 (error
                   (err)
                   (log-http-server-error "Error in hourly server task, ~S:~&~A" name (report-string err))))))
  ;; Reset so we run again.
  (synchronize-hourly-server-tasks))

(define add-periodic-task (name periodicity form)
  "Add a periodic server task.
NAME is a unique string naming the task.
PERIODICITY is a weekday keyword or :daily, or :weekly or :hourly.
FORM is the form that executes the desired task."
  (check-type name string)
  (check-type form cons)
  (ecase periodicity
    ((:daily :sunday :monday :tuesday :wednesday :thursday :friday :saturday :weekly)
     (let ((entry (find name *daily-server-tasks* :test #'string-equal :key #'first)))
       (cond (entry
              (setf (second entry) periodicity
                    (third entry) form))
             (t (setq *daily-server-tasks* `(,.*daily-server-tasks* (,name ,periodicity ,form)))))))
    (:hourly
      (let ((entry (find name *hourly-server-tasks* :test #'string-equal :key #'first)))
        (cond (entry
               (setf (second entry) periodicity
                     (third entry) form))
              (t (setq *hourly-server-tasks* `(,.*hourly-server-tasks* (,name ,periodicity ,form)))))))))

(define delete-periodic-task (name)
  "Delete a periodic server task named NAME."
  (setq *daily-server-tasks* (delete name *daily-server-tasks* :test #'string-equal :key #'first)
        *hourly-server-tasks* (delete name *hourly-server-tasks* :test #'string-equal :key #'first)))

(define show-periodic-tasks (&key (stream *standard-output*))
  "Describes the periodic server tasks on STREAM."
  (flet ((format-tasks (title tasks)
           (loop initially (format stream "~&~A~2%" title)
                 for (name periodicity form) in (sort tasks #'scl:alphalessp :key #'first)
                 for count upfrom 1
                 do (format stream "~&~D  ~A [~:(~A~)]: ~~S~" count name periodicity form))))
    (format stream "~&--------------------------------------------------------------------------------")
    (format-tasks "Daily HTTP Server Tasks" *daily-server-tasks*)
    (format stream "~&--------------------------------------------------------------------------------")
    (format-tasks "Hourly HTTP Server Tasks" *hourly-server-tasks*)
    (format stream "~&--------------------------------------------------------------------------------")))

(add-periodic-task "Turn Off Server Debugging" :hourly '(http::debug-server nil))


;;;------------------------------------------------------------------- 
;;;
;;; TIME RELATED
;;;

(declaim (inline month-string))

(define month-string (month &optional (mode :long))
  "Returns the month string for MONTH
mode can be any of: :SHORT, :LONG, :MEDIUM, :FRENCH, :ROMAN, :GERMAN, :ITALIAN."
  (time:month-string month mode))

(declaim (inline day-of-the-week-string))

(define day-of-the-week-string (weekday &optional (mode :long))
  "Returns the weekday string for weekday
mode can be any of: :SHORT, :LONG, :MEDIUM, :FRENCH, :ROMAN, :GERMAN, :ITALIAN."
  (time:day-of-the-week-string weekday mode))

(declaim (inline time-zone))

(define time-zone (&optional update-p)
  "Returns the timezone as an offset from GMT."
  (declare (ignore update-p))
  time:*timezone*)

(declaim (inline write-positive-fixnum))

;; Optimizes writing of date in headers
(define write-positive-fixnum (fixnum &optional (base 10.) (stream *standard-output*))
  (si:print-fixnum-1 fixnum base stream))

(declaim (inline parse-gmt-time))

(define parse-gmt-time (string &optional (start 0) (end (length string)))
  (zwei::parse-rfc822-date-time string start end))

;(define gmt-time (&optional (universal-time (get-universal-time)))
;  (print-gmt-time nil universal-time))

(si:advise-permanently zwei::parse-rfc822-date-time :around |Handle Error| 0
  (scl:condition-case-if (and (not http:*debug-server*)
                              (scl:variable-boundp http:*server*)
                              http:*server*)
                         (err)
       :do-it 
     (error
       (www-utils:report-bug http:*server-mail-address*
                             "Time Parsing Error"
                             "Error in (zwei::parse-rfc822-date-time ~S)~&Type: ~S~&Report: ~A"
                             (first scl:arglist) (type-of err)
                             (www-utils:report-string err))
       (get-universal-time))))

(si:compile-advice 'ZWEI::PARSE-RFC822-DATE-TIME)

;; removed   10/1/96 -- JCMa.
;(si:advise-permanently time:parse-universal-time :around |Handle Error| 0
;  (scl:condition-case-if (not http:*debug-server*)
;                         (err)
;       :do-it 
;     (time:parse-error
;       (www-utils:report-bug http:*server-mail-address*
;                             "Time Parsing Error"
;                             "Error in (time:parse-universal-time ~S)~&Type: ~S~&Report: ~A"
;                             (first scl:arglist) (type-of err)
;                             (www-utils:report-string err))
;       (get-universal-time))))
;
;(si:unadvise-permanent time:parse-universal-time :around 0)


;;;------------------------------------------------------------------- 
;;;
;;; FILE RELATED OPERATIONS
;;;

(eval-when (compile load eval)
  (unless (fboundp 'fcli::set-pathname-directory)
    (defun fcli::set-pathname-directory (pathname new-directory)
      (declare (notinline))
      (scl:send pathname :new-directory new-directory))
    (defsetf pathname-directory fcli::set-pathname-directory))

  (unless (fboundp 'fcli::set-pathname-name)
    (defun fcli::set-pathname-name (pathname new-name)
      (declare (notinline))
      (scl:send pathname :new-name new-name))
    (defsetf pathname-name fcli::set-pathname-name))

  (unless (fboundp 'fcli::set-pathname-type)
    (defun fcli::set-pathname-type (pathname new-type)
      (declare (notinline))
      (scl:send pathname :new-type new-type))
    (defsetf pathname-type fcli::set-pathname-type))

  (unless (fboundp 'fcli::set-pathname-version)
    (defun fcli::set-pathname-version (pathname new-version)
      (declare (notinline))
      (scl:send pathname :new-version new-version))
    (defsetf pathname-version fcli::set-pathname-version)))

(defmethod pathname-truename ((pathname pathname))
  (scl:send pathname :truename))

(declaim (inline %pathname-directory-file-p))

(defun %pathname-directory-file-p (pathname)
  "Returns non-null if PATHNAME is a directory masquerading as a file.
only comes up on the lisp machine"
  #+VLM (scl:send (pathname pathname) :get :directory)
  #-VLM (equalp "directory" (pathname-type (pathname pathname))))

(declaim (inline %pathname-directory-p))

(defun %pathname-directory-p (pathname)
  (null (pathname-name (pathname pathname))))

(defun pathname-directory-p (pathname)
  (let ((path (pathname pathname)))
    (or (%pathname-directory-p path)
        (%pathname-directory-file-p path))))

(defmethod pathname-directory-most-specific-name ((pathname pathname))
  (or (pathname-name pathname)
      (car (last (pathname-directory pathname)))))

(defmethod pathname-as-directory ((pathname pathname) &aux name)
  (cond ((and (setq name (pathname-name pathname))
              (%pathname-directory-file-p pathname))
         (make-pathname :host (pathname-host pathname)
                        :device (pathname-device pathname)
                        :directory `(,@(pathname-directory pathname) ,name)))
        (t pathname)))

(declaim (inline file-stream-pathname))

(define file-stream-pathname (file-stream)
  "Returns the pathname associated with file-stream."
  (scl:send file-stream :pathname))

(declaim (inline file-stream-creation-date))

(define file-stream-creation-date (file-stream)
  "Returns the creation date in universal time for FILE-STREAM's source file."
  (scl:send file-stream :creation-date))

(defmethod file-creation-date ((pathname pathname))
  (cond ((%pathname-directory-p pathname)
         (with-open-file (file-stream pathname :direction :probe-directory)
           (scl:send file-stream :creation-date)))
        (t (with-open-file (file-stream pathname)
             (scl:send file-stream :creation-date)))))

(declaim (inline file-stream-modification-date))

;; does not handle directories   8/29/97 -- JCMa.
(define file-stream-modification-date (file-stream)
  "Returns the modification date in universal time for FILE-STREAM's source file."
  (scl:send file-stream :creation-date))

(defmethod file-modification-date ((pathname pathname))
  (cond ((%pathname-directory-p pathname)
         (with-open-file (file-stream pathname :direction :probe-directory)
           (let ((properties (cdr (scl:send file-stream :properties))))
	     (or (getf properties :modification-date)
		 (getf properties :creation-date)))))	;VLM was coming up with NIL   10/12/99 -- JCMa.
        ;; creation date is the same as modification date for versioned file systems.
        (t (with-open-file (file-stream pathname)
             (scl:send file-stream :creation-date)))))

(declaim (inline file-stream-length-in-bytes))

(define file-stream-length-in-bytes (file-stream)
  "Returns the length in bytes for FILE-STREAM's source file."
  (scl:send file-stream :length))

(defmethod file-length-in-bytes ((pathname pathname) &optional new-length)
  (declare (ignore new-length))
  (with-open-file (file-stream pathname)
    (file-length file-stream))) 

(declaim (inline file-stream-version))

(defun file-stream-version (file-stream)
  (scl:send file-stream :creation-date))

(defun file-version  (pathname)
  "Returns the pathname version or NIL if pathname does not exist."
  (with-open-file (file-stream pathname :direction :probe :if-does-not-exist nil)
    (when file-stream
      (file-stream-version file-stream))))

(define file-properties (pathname)
  "Returns the length in bytes  and the creation in in universal time 
for FILE-STREAM's source file."
  (declare (values length-in-bytes last-modification-date version))
  (let ((plist (cdr (fs:file-properties pathname))))
    (values (getf plist :length-in-bytes)
            (getf plist :creation-date)
            (getf plist :creation-date))))

(define directory-info (pathname &key (name :wild) (type :wild) (version :newest) (sort-pathnames t))
  "Returns a poperty list of information for every file in the directory PATHNAME
that matches pathnames wildcards."
  (declare (notinline))
  (flet ((get-directory-listing (p &optional (sort-p sort-pathnames))
           (cdr (if sort-p
                    (fs:directory-list p :sorted :no-extra-info)
                    (fs:directory-list p :no-extra-info))))
         (pattern (path type)
           (make-pathname :host (pathname-host path)
                          :directory (pathname-directory path)
                          :name (etypecase name
                                  (keyword
                                    (ecase name
                                      (:wild "*")))
                                  (string name))
                          :type (etypecase type
                                  (keyword
                                    (case type
                                      (:wild "*")
                                      (t (symbol-name type))))
                                  (string type))
                          :version (etypecase version
                                     (keyword
                                       (ecase version
                                         (:wild nil)
                                         (:newest :newest))))))
         (sorter (e1 e2)
           (let ((p1 (car e1))
                 (p2 (car e2)))
             (and (scl:alphalessp (pathname-name p1) (pathname-name p2))
                  (let ((t1 (pathname-type p1))
                        (t2 (pathname-type p2)))
                    (cond ((and t1 t2)
                           (scl:alphalessp t1 t2))
                          (t1 nil)
                          (t t)))))))
    (declare (dynamic-extent #'get-directory-listing #'pattern #'sorter))
    (let ((p (pathname pathname)))
      (typecase type
        (keyword
          (ecase type
            (:wild (get-directory-listing (pattern p "*")))))
        (string
          (get-directory-listing (pattern p type)))
        (cons
          (loop for type in type
                nconc (get-directory-listing (pattern p type)) into paths
                finally (return (if sort-pathnames
                                    (sort paths #'sorter)
                                    paths))))))))

(defun directory-list* (pathname predicate &rest options &aux files directories)
  "Accepts the options :FILES :DIRECTORIES :SORTED :PROPERTIES."
  (flet ((collect-p (entry)
           (flet ((test-predicate (path)
                    (or (null predicate) (funcall predicate path))))
             (declare (dynamic-extent #'test-predicate))
             (when entry
               (destructuring-bind (path . plist) entry
                 (cond
                   ((null path) nil)
                   ((and files directories)
                    #+VLM (when (getf plist :directory)
                            (scl:send path :putprop t :directory))
                    (test-predicate path))
                   (files
                    (cond ((getf plist :directory) nil)
                          (t (test-predicate path))))
                   ((and directories (getf plist :directory))
                    #+VLM (scl:send path :putprop t :directory)
                    (test-predicate path))
                   (t nil))))))
         (coerce-pathname (entry)
           (when (and directories (getf (cdr entry) :directory))
             (setf (first entry) (pathname-as-directory (first entry))))
           entry))
    (declare (inline collect-p coerce-pathname))
    (let* ((path (pathname pathname))
           (dir (make-pathname :host (pathname-host path)
                               :device (pathname-device path)
                               :directory (pathname-directory path)
                               :name :wild
                               :type :wild
                               :version :newest))
           (entries (apply #'fs:directory-list dir
                           (if (member :sorted options) '(:sorted :no-extra-info) '(:no-extra-info)))))
      (setq files (member :files options)
            directories (member :directories options))
      (cond ((member :properties options)
             (loop for entry in entries
                   when (collect-p entry)
                     collect (coerce-pathname entry)))
            (t (loop for entry in entries
                     when (collect-p entry)
                       collect (car (coerce-pathname entry))))))))

(declaim (inline create-directories-recursively))

(define create-directories-recursively (pathname)
  (with-null-stream (*standard-output*)
    (fs:create-directories-recursively pathname)))

;(define probe-directory (pathname)
;  "Returns non-null if the directory pathname exists."
;  (handler-case 
;    (not (null (fs:directory-list (merge-pathnames "bar.foo.newest" pathname))))
;    (fs:directory-not-found () nil)))

;(define probe-directory (pathname)
;  "Returns non-null if the directory pathname exists."
;  (loop with path = (pathname pathname)
;        with host = (pathname-host path)
;        with device = (pathname-device path)
;        for dir in (pathname-directory path :case :local)
;        collect dir into directories
;        ;; lispm fails to return nil and signals directory not found for host
;        ;; mac file system 2/14/95 -- JCMa.
;        when (and (cdr directories)
;                  (not (open (make-pathname :host host
;                                            :device device
;                                            :directory directories)
;                             :direction :probe-directory :if-does-not-exist nil)))
;          do (return nil)
;        finally (return t)))

(define probe-directory (pathname)
  "Returns non-null if the directory pathname exists."
  (flet ((probe-it (pathname)
           (loop with path = (pathname pathname)
                 with host = (pathname-host path)
                 with device = (pathname-device path)
                 for dir in (pathname-directory path :case :local)
                 collect dir into directories
                 ;; lispm fails to return nil and signals directory not found for host
                 ;; mac file system 2/14/95 -- jcma.
                 when (and (cdr directories)
                           (not (open (make-pathname :host host
                                                     :device device
                                                     :directory directories)
                                      :direction :probe-directory :if-does-not-exist nil)))
                   do (return nil)
                 finally (return t))))
    (declare (inline probe-it))
    #-VLM
    (probe-it pathname)
    ;; the VLM was losing with directory probes. Remove once OPEN Fixed  9/18/96 -- JCMa.
    #+VLM
    (handler-case 
      (probe-it pathname)
      (fs:directory-not-found () nil))))

(defmethod url::pathname-ftp-url-string ((pathname string) &optional force-directory-p)
  (url::pathname-ftp-url-string (pathname pathname) force-directory-p))

(defmethod url::pathname-ftp-url-string ((pathname pathname) &optional force-directory-p)
  (let ((host (pathname-host pathname))
        (dir (pathname-directory pathname))
        (name (pathname-external-name-string (pathname-name pathname)))
        (extension (pathname-type pathname)))
    (cond ((or force-directory-p (equalp extension "directory"))
	   (if name
	       (url::make-ftp-url-string (host-http-name host) `(,@(cdr dir) ,name))
	       (url::make-ftp-url-string (host-http-name host) (cdr dir))))
	  (t (url::make-ftp-url-string (host-http-name host) (cdr dir) name (unless (eql extension :unspecific)extension))))))

(defun host-access-path-for-host-p (access-path host)
  (let ((ahost (scl:send access-path :host))
        (rhost (parse-host host t)))
    (host-eq ahost rhost)))

(defvar *standard-get-user-id-and-password* #'fs:get-user-id-and-password)
(defvar *nfs-authentication-function* #'rpc:authentication-initialize)

(define-macro with-automatic-login ((host user-id user-pw) &body body)
  "Supplies userid and PW to ensure successul FTP login to host with BODY."
  `(labels ((standard-get-user-id-and-password (access-path host-user-id host-password condition)
              (funcall *standard-get-user-id-and-password* access-path host-user-id host-password condition))
            (nfs-authenticate (authentication-mixin host-user-id host-password)
              (funcall *nfs-authentication-function* authentication-mixin
                       (or host-user-id ,user-id)
                       (or host-password ,user-pw)))
            (automatic-get-user-id-and-password
               (access-path host-user-id host-password condition)
              (cond ((and (null host-user-id)
                          (null host-password)
                          (host-access-path-for-host-p access-path ,host))
                     (values ,user-id ,user-pw))
                    (t (standard-get-user-id-and-password access-path host-user-id host-password condition)))))
     (scl:letf ((#'fs:get-user-id-and-password #'automatic-get-user-id-and-password)
                (#'rpc:authentication-initialize #'nfs-authenticate))
       ,@body)))

;(define ftp-directory-info (directory &optional (user-id "anonymous") (user-pw (server-mail-address)))
;  "Returns a list of pathname spec for directory just like DIRECTORY-INFO.
;If a network error is encountered, this returns NIL."
;  (let* ((path (pathname directory))
;         (host (pathname-host path)))
;    (http::handler-case-if (not http::*debug-client*) 
;       (with-automatic-login (host user-id user-pw)
;	 ;; ansi CL directory fails due to :fast option  3/13/94 -- JCMa.
;	 (directory-info directory))
;      ;; handle remote connection problems, including dead host, refused connection.
;      (sys:remote-network-error () nil))))

(define ftp-directory-info (directory &optional (user-id "anonymous") (user-pw (server-mail-address)))
  "Returns a list of pathname spec for directory just like DIRECTORY-INFO.
If a network error is encountered, this returns NIL."
  (let* ((path (pathname directory))
         (host (pathname-host path)))
    (with-automatic-login (host user-id user-pw)
      ;; ansi CL directory fails due to :fast option  3/13/94 -- JCMa.
      (directory-info directory))))

;(define ftp-copy-file (from-pathname to-stream &key (element-type 'character)
;                                     (user-id "anonymous") (user-pw (server-mail-address)))
;  "Copies the content of FROM-PATHNAME to TO-STREAM. 
;If a network error is encountered, this returns NIL, otherwise T.
;ELEMENT-TYPE is the ANSI file openning argument."
;  (declare (values success-p))
;  (let ((host (pathname-host from-pathname)))
;    (http::handler-case-if (not http::*debug-client*) 
;      (with-automatic-login (host user-id user-pw)
;        (with-open-file (ftp-stream from-pathname :direction :input :element-type element-type)
;          (http::stream-copy-until-eof ftp-stream to-stream (case element-type
;							      (character :text)
;							      (t :binary)))
;          (values t)))
;      ;; handle remote connection problems, including dead host, refused connection.
;      (sys:remote-network-error () nil))))

(define ftp-copy-file (from-pathname to-stream &key (element-type 'character)
                                     (user-id "anonymous") (user-pw (server-mail-address)))
  "Copies the content of FROM-PATHNAME to TO-STREAM. 
If a network error is encountered, this returns NIL, otherwise T.
ELEMENT-TYPE is the ANSI file openning argument."
  (declare (values success-p))
  (let ((host (pathname-host from-pathname)))
      (with-automatic-login (host user-id user-pw)
        (with-open-file (ftp-stream from-pathname :direction :input :element-type element-type)
          (http::stream-copy-until-eof ftp-stream to-stream (case element-type
							      (character :text)
							      (t :binary)))
          (values t)))))


;;;------------------------------------------------------------------- 
;;;
;;; READING DELIMITED LINES
;;;

(declaim (special http::*server-line-buffer*))

;; Generic function in http:server;utils.lisp   12/18/95 -- JCMa.
;; use the EOF handling to prevent running off the end and failing to detect
;; that the server is no longer handing out the bits.  7/17/95 -- JCMa.
;; Don't catch connection-lost so the client or web walker can handle it.   7/16/96 -- JCMa.
(defun %read-delimited-line (stream delimiters eof buffer)
  (let* ((size (array-total-size buffer))
         (index -1)
         error-p delimiter)
    (block read-loop
      (flet ((handle-read-error (cond)
               (typecase cond
                 (connection-lost nil)
                 (t (setq error-p t)
                    (return-from read-loop)
                    t))))
        (declare (dynamic-extent handle-read-error))
        (handler-bind ((end-of-file #'handle-read-error))
          (with-fast-array-references ((buffer buffer string))
            (loop initially (setf (fill-pointer buffer) 0)
                  for char = (read-char stream t eof t)
                  until (or (null char) (member char delimiters :test #'char=))
                  for idx upfrom 0
                  unless (< idx size)
                    do (setq size (floor (* (the fixnum size) 1.2))
                             buffer (adjust-array buffer size :element-type 'character))
                  do (setf (aref buffer idx) char)
                     (setq index idx)
                  finally (setq delimiter char))))))
    (if (= -1 index)
        (values (if error-p eof buffer) error-p delimiter 0)
        (values buffer error-p delimiter (setf (fill-pointer buffer) (1+ (the fixnum index)))))))

;; assumes that the fill pointer indicates where to start filling
(defun %buffered-stream-read-delimited-line (stream delimiters eof buffer)
  (let* ((size (array-total-size buffer))
         (buffer buffer)
         (index -1)
         error-p delimiter)
    (declare (sys:array-register buffer))
    (block read-loop
      (flet ((handle-read-error (cond)
               (typecase cond
                 (connection-lost nil)
                 (t (setq error-p t)
                    (return-from read-loop)
                    t))))
        (declare (dynamic-extent handle-read-error))
        (handler-bind ((end-of-file #'handle-read-error))
          (loop with idx1 = (fill-pointer buffer)
                doing (multiple-value-bind (input-buffer offset limit)
                          (scl:send stream :read-input-buffer t)
                        #+testing(break "Read-Headers (~D ~D): ~S" offset limit (subseq input-buffer offset limit))
                        ;; if this is reached signal EOF. Shouldn't
                        ;; happen to EOF T arg to :read-input-buffer  7/16/96 -- JCMa.
                        (unless input-buffer (error 'sys:end-of-file))
                        (let ((input-buffer input-buffer))
                          (declare (sys:array-register input-buffer))
                          (loop for idx2 upfrom offset below limit
                                for char = (aref input-buffer idx2)
                                until (member char delimiters :test #'eql)
                                unless (< idx1 size)
                                  do (setq size (floor (* (the fixnum size) 1.2))
                                           buffer (adjust-array buffer size :element-type 'character))
                                do (setf (aref buffer idx1) char)
                                   (setq index idx1)
                                   (incf idx1)
                                finally (setq delimiter char)
                                        (cond ((= idx2 limit)
                                               (scl:send stream :advance-input-buffer))
                                              (t (scl:send stream :advance-input-buffer (1+ idx2))
                                                 (return-from read-loop))))))))))
    (if (= -1 index)
        (values (if error-p eof buffer) error-p delimiter (fill-pointer buffer))
        (values buffer error-p delimiter (setf (fill-pointer buffer) (1+ index))))))

(defmethod read-delimited-line (stream &optional (delimiters '(#\Return #\Linefeed)) eof buffer)
  "Reads a line from stream which is delimited by DELIMITERS."
  (declare (values line eof delimiter length))
  (if buffer
      (%read-delimited-line stream delimiters eof buffer)
      (using-resource (line-buffer http::line-buffer http::*line-buffer-size*)
                      (multiple-value-bind (buf error-p delim length)
                          (%read-delimited-line stream delimiters eof line-buffer)
                        (values (if error-p eof (subseq buf 0 length)) error-p delim length)))))

(defmethod read-delimited-line ((stream si:buffered-input-stream) &optional (delimiters '(#\Return #\Linefeed))
                                eof buffer)
  "Reads a line from stream which is delimited by DELIMITERS."
  (declare (values line eof delimiter length))
  (cond (buffer
	 (setf (fill-pointer buffer) 0)		;know where to start inserting
	 (%buffered-stream-read-delimited-line stream delimiters eof buffer))
	(t (using-resource (line-buffer http::line-buffer http::*line-buffer-size*)
	     (setf (fill-pointer line-buffer) 0)	;know where to start inserting
	     (multiple-value-bind (buf error-p delim length)
		 (%buffered-stream-read-delimited-line stream delimiters eof line-buffer)
	       (values (if error-p eof (subseq buf 0 length)) error-p delim length))))))

(declaim (inline char-bits))

(define char-bits (char)
  (scl:char-bits char))

(declaim (inline string-thin))

(define string-thin (string)
  (scl:string-thin string))


;;;------------------------------------------------------------------- 
;;;
;;; SECURE SUBNETS
;;;

(define parse-internet-address (ip-address)
  "Parses IP-ADDRESS specification."
  (check-type ip-address string)
  (let ((trimmed (string-trim '(#\space) ip-address)))
    (declare (dynamic-extent trimmed))
    (tcp::parse-internet-address trimmed)))

(define parse-internet-addresses (ip-addresses)
  "Parses IP-ADDRESSES into a list of ip-address specifications."
  (loop for ip-address in ip-addresses
        for parsed-address = (parse-internet-address ip-address)
        when parsed-address
          collect parsed-address))

;; poor name as the code looks counter intuitive in the deny access case.
;; This should be called ip-subnet-match-p   10/10/95 -- JCMa.
(define ip-host-trusted-p (address secure-subnets &optional (network (net:local-network-of-type :internet)))
  "Returns non-null if IP-address address is trusted given secure-subnets."
  (flet ((address-on-subnet-p (address subnet subnet-number official-subnet-number)
           (or ;; the same address 128.81.41.7
             (tcp:compare-internet-address subnet address)
             ;; the same subnetted network 128.81.41.0
             (tcp:compare-internet-address subnet subnet-number)
             ;; the same offical network 128.81.0.0
             (tcp:compare-internet-address subnet official-subnet-number))))
    (declare (inline address-on-subnet-p))
    (cond (secure-subnets
           (loop with subnet-number = (tcp::internet-subnet-number address 0 network)
                 with official-subnet-number = (tcp::official-internet-subnet-number address 0)
                 for subnet in secure-subnets
                 when (address-on-subnet-p address subnet subnet-number official-subnet-number)
                   do (return t)
                 finally (return nil)))
          (t t))))


;;;------------------------------------------------------------------- 
;;;
;;; STREAM HACKING
;;;

(define-macro with-binary-stream ((stream direction) &body body)
  "Turns STREAM into a binary stream within the scope of BODY.
DIRECTION can be :OUTPUT, :INPUT, or :BOTH."
  `(unwind-protect
       (progn ,(ecase direction
                 (:output `(tcp::binary-output-mode ,stream))
                 (:input `(tcp::binary-input-mode ,stream))
                 (:both `(progn (tcp::binary-output-mode ,stream)
                                (tcp::binary-input-mode ,stream))))
              ,@body)
     ,(ecase direction
        (:output `(tcp::ascii-output-mode ,stream))
        (:input `(tcp::ascii-input-mode ,stream))
        (:both `(progn (tcp::ascii-output-mode ,stream)
                       (tcp::ascii-input-mode ,stream))))))


(define-macro with-text-stream ((stream direction) &body body)
  "Turns STREAM into an TEXT stream within the scope of BODY.
DIRECTION can be :OUTPUT, :INPUT, or :BOTH."
  `(unwind-protect
       (progn ,(ecase direction
                 (:output `(tcp::ascii-output-mode ,stream))
                 (:input `(tcp::ascii-input-mode ,stream))
                 (:both `(progn (tcp::ascii-output-mode ,stream)
                                (tcp::ascii-input-mode ,stream))))
              ,@body)
     ,(ecase direction
        (:output `(tcp::ascii-output-mode ,stream))
        (:input `(tcp::ascii-input-mode ,stream))
        (:both `(progn (tcp::ascii-output-mode ,stream)
                       (tcp::ascii-input-mode ,stream))))))

(declaim (inline live-connection-p))

(define live-connection-p (http-stream)
  "Returns non-null if the TCP/IP connection over HTTP-STREAM remains alive
in that the remote host continue to respond at the TCP/IP level."
  (scl:send http-stream :connected-p))

;; Provide an answer for windows.   4/2/99 -- JCMa.
(scl:defmethod (:connected-p tv:minimum-window) () t)

(define abort-current-connection ()
  "Aborts the computation associated with the current HTTP connection."
  (scl:signal 'sys:abort))

(declaim (inline abort-if-connection-dead))

(define abort-if-connection-dead (http-stream)
  "Aborts the HTTP connection if the TCP/IP connection over HTTP-STREAM
has died, i.e. the remote host is no longer connected."
  (unless (live-connection-p http-stream)
    (scl:signal 'tcp::tcp-connection-closed
                :connection (scl:send http-stream :tcb)
                :foreign-host (scl:send http-stream :foreign-host))))


;;;------------------------------------------------------------------- 
;;;
;;; TRACKING AND ACCESSING SERVER PROCESS
;;;

(declaim (inline current-process))

(defun current-process ()
  "Returns the current process."
  scl:*current-process*)

(defun map-all-processes (function)
  "Maps function over all running processes."
  (loop for process in process:*all-processes*
	do (funcall function process)))


;;;------------------------------------------------------------------- 
;;;
;;; SYMBOLICS NETWORK INTERFACE
;;;

(defun match-http-server-p (resource object stream host address)
  (declare (ignore resource object stream host address))
  t)

(scl:defresource http-server (stream host address)
  :constructor http:make-server
  :matcher match-http-server-p
  :initializer http:initialize-resourced-server
  :deinitializer http:deinitialize-resourced-server)

(define http:clear-server-resource ()
  "Clears the resource of HTTP server objects."
  (scl:clear-resource 'http-server))

(define describe-server-resource (&optional (stream *standard-output*))
  "Describes the internal datastructure of the server resource on STREAM."
  (let ((*standard-output* stream))
    (si:describe-resource 'http-server)))

(declaim (inline accept-connection))

(defun accept-connection (stream)
  (scl:send stream :accept))

(declaim (inline reject-connection))

(defun reject-connection (stream rejection-string)
  (scl:send stream :reject rejection-string))

(tcp:add-tcp-port-for-protocol :http 80) 

(declaim (type function http:provide-service http:set-server-status http:log-access))
(declaim (special http:*server*))
(declaim (special http:*server-timeout*))
(declaim (special http:*number-of-connections*))
(declaim (special http:*reject-connection-threshold*))
(declaim (special http:*reject-connection-message*))

(net:define-server :http (:medium :byte-stream
                          :error-disposition :debugger
                          :stream (stream :accept-p nil
                                          :characters t
                                          :translation :modal)
                          :host client
                          :address client-address
                          :reject-unless-trusted nil
			  :process-name "HTTP Server")
   (flet ((log-dropped-connection (server)
            (http:set-server-status server 408) ;client timeout status code changed from 504 -- JCMa 5/29/1995.
            (http:log-access server)
            (close stream :abort t))
          (handle-unhandled-error (server error)
            (handler-case
              (progn
                (http:set-server-status server 500)
                (http:log-access server)
                (http::report-status-unhandled-error error stream (http::server-request server nil))
                (finish-output stream)          ; push the output
                (close stream :abort (not (live-connection-p stream))))
              (sys:bad-connection-state () (close stream :abort t))
              (neti:protocol-timeout () (close stream :abort t))
              (error (error)
                     (http::bug-report-error error)
                     (close stream :abort t)))
            ;; log the access after the output has been forced.
            (http:log-access server))
          (accept-connection-p ()
            (< http:*number-of-connections* http:*reject-connection-threshold*)))
     (declare (inline accept-connection-p)
              (dynamic-extent #'log-dropped-connection #'handle-unhandled-error))

     (handler-case
       (cond ;; when below the rejection threshold, accept the connection.
	 ((accept-connection-p)
	  (accept-connection stream)		;accept the connection
	  ;; main body
	  (scl:using-resource (server http-server stream client client-address)
	    (let ((http:*server* server))
	      (declare (special http:*server*))
	      (flet ((handle-condition (condition)
		       (typecase condition
			 (sys:abort () (close stream :abort (not (live-connection-p stream))))
			 ;; Catch errors where the connection is dropped on the other end
			 (neti:protocol-timeout (log-dropped-connection server))
			 (sys:bad-connection-state (log-dropped-connection server))
			 (sys:network-stream-closed (log-dropped-connection server))
			 (error (handle-unhandled-error server condition))
			 (condition (return-from handle-condition nil))
			 (t (handle-unhandled-error server condition)))	;This should never execute
		       (throw 'exit-http-server nil)))
		(declare (dynamic-extent #'handle-condition))
		(scl:condition-bind-if (not http:*debug-server*)
				       ((condition #'handle-condition))
		  (catch 'exit-http-server
		    (http:provide-service server)))))))
	 ;; Server operating at critical, reject the connection summarily.
	 (t (reject-connection stream http:*reject-connection-message*)))
       ;; Can get errors accepting or rejecting connections which are typically dropped connections.
       (sys:network-error () nil)
       (error (error) (http::bug-report-error error)))))


;;;------------------------------------------------------------------- 
;;;
;;; ENABLE AND DISABLE SERVICES
;;;

(define enable-http-service (&optional on-ports)
  "Top-level method for starting up HTTP servers."
  (declare (ignore on-ports))
  (sys:enable-services :http nil))

(define disable-http-service (&optional on-ports)
  "Top-level method for shutting down HTTP servers."
  (declare (ignore on-ports))
  (sys:disable-services :http nil))

(define map-http-servers (function)
  "Maps FUNCTION over all active HTTP servers."
  (flet ((fctn (server in-use-p resource)
           (declare (ignore resource))
           (when in-use-p
             (funcall function server))))
    (declare (dynamic-extent #'fctn))
    (scl:map-resource 'http-server #'fctn)))

(define all-servers (&aux servers)
  "Returns all active servers."
  (flet ((collect (server)
           (push server servers)))
    (declare (dynamic-extent #'collect))
    (map-http-servers #'collect)
    servers))

;;;------------------------------------------------------------------- 
;;;
;;; LOGGING EVENTS
;;;

(define-macro log-http-access (host format-string &rest format-args)
  `(fs:log-server-event :www :http ,host ,format-string ,@format-args))

(defun log-http-server-error (format-string &rest format-args)
  (apply #'tv:notify nil format-string format-args))

(define http::tv-notify-http-access (client-host method url-string case)
  (let ((host-name (http::host-domain-name client-host)))
    (ecase case
      (:accepted
        (tv:notify nil "HTTP Serving ~A: Method ~S: ~S" host-name method url-string))
      (:rejected
        (tv:notify nil "HTTP Rejected ~A: Method ~S: ~S" host-name method url-string))
      (:timeout
        (tv:notify nil "HTTP Timed Out Serving ~A: Method ~S: ~S" host-name method url-string)))))

(declaim (inline notify-log-window))

(define notify-log-window (format-string &rest format-args)
  "Top-level method for writing to the HTTP log window."
  (apply #'tv:notify nil format-string format-args))

(declaim (notinline common-logfile-notify))

(define common-logfile-notify (server)
  "Issues a notification of server activity on a window."
  (let ((string (with-output-to-string (stream)
                  (http:write-common-logfile-entry server stream))))
    (declare (dynamic-extent string))
    (if (find #\~ string :test #'eql)
        (notify-log-window "~A" string)
        (notify-log-window string))))

;;;------------------------------------------------------------------- 
;;;
;;; INITIALIZATIONS
;;;

(declaim (inline add-initialization))

(define add-initialization (name form &optional keywords list-name)
  "Adds the initialization FORM with name to LIST-NAME."
  (si:add-initialization name form keywords list-name))

(declaim (inline run-initializations))

(define run-initializations (list-name &optional redo-flag)
  "Runs the initializations on LIST-NAME."
  (si:initializations list-name redo-flag))

(declaim (inline reset-initializations))

(define reset-initializations (list-name)
  "Resets the initializations on LIST-NAME."
  (si:reset-initializations list-name))

(declaim (inline delete-initialization))

(define delete-initialization (name &optional keywords list-name)
  "Deletes the initialization named NAME from LIST-NAME."
  (si:delete-initialization name keywords list-name))

;; Main initialization runs on cold boot and warm boot.
(add-initialization
  "Run CL-HTTP Server Cold Initializations."
  '(http:run-server-initializations t)
  '(:normal :warm))


;;;------------------------------------------------------------------- 
;;;
;;; BUG REPORTING
;;;

(define user-mail-address ()
  "Returns the mail address for the current user."
  (destructuring-bind (user host)
      (scl:send si:*user* :mail-address)
    (concatenate 'string user "@" (host-mail-name host))))

(defun allocate-email-message (to subject message &key from reply-to keywords comments
                                  file-references additional-headers)
  (cl:macrolet ((default-from ()
                  `(destructuring-bind (user host) (scl:send si:*user* :mail-address)
                     `((:name ,user :host (:object ,host)))))
                (ensure-list (item)
                  `(etypecase ,item
                     (list ,item)
                     (atom (list ,item)))))
    (make-instance
      'zwei:send-message-string-draft-msg
      :headers `(:to ,to :subject ,subject                 
                     :from ,(or from (default-from))
                     ,.(when reply-to `(:reply-to ,reply-to))
                     ,.(when keywords `(:keywords ,keywords))
                     ,.(when comments `(:comments ,comments))
                     ,.(when file-references
                         `(:file-references ,(ensure-list file-references)))
                     ,.additional-headers)
      :text message
      :report-stream nil)))

(define send-mail-from (from to subject message-writer &key keywords comments file-references reply-to
                             additional-headers)
  "Send an email message from FROM to TO with subject SUBJECT.
MESSAGE-WRITER is either a string or a function accepting a stream argument
that is used to generate the message body. KEYWORDS is a list of keywords for a keyword
header. COMMENTS is a string for a comment header. FILE-REFERENCES is a list of pathnames.
REPLY-TO is automatically added unless supplied here."
  (labels ((message-string (message)
             (etypecase message
               (string message)
               (function (with-output-to-string (string)
                           (funcall message string)))))
           (parse-address (address)
             (handler-case
               (etypecase address
                 (cons
                   (loop for addr in address
                         append (zwei:parse-addresses addr)))
                 (string (zwei:parse-addresses address)))
               (zwei:parse-error (error) (zwei:error "~A" (dbg:report-string error)))))
           (send-it ()
             (scl:send (allocate-email-message to subject (message-string message-writer)
                                               :from from :reply-to reply-to
                                               :keywords keywords
                                               :comments comments
                                               :file-references file-references
                                               :additional-headers additional-headers)
                       :transmit))
           (last-resort (case from to subject message)
             (tv:notify tv:selected-window "~A~&From: ~A~&To: ~A~&Subject: ~A~2%~A"
                        case from to subject (message-string message)))
           (handle-mailer-error ()
             (loop for path in (net:find-paths-to-service :store-and-forward-mail)
                   for host = (neti::service-access-path-host path)
                   unless (host-eq host (local-host))
                     do (scl:condition-case-if (not http::*debug-server*) ()
                             (let ((zwei:*mail-network-host* host))
                               (send-it)
                               (return))
                           (mailer:mailer-error nil))
                   finally (last-resort "No Mailer found for Error Report:" from to subject message-writer))))
    (declare (inline send-it handle-mailer-error))
    ;; dont reroute the address as necessary
    ;; parse it up
    (scl:condition-case-if (not http::*debug-server*) ()
         (progn
           (setq from (parse-address from)
                 to (parse-address to)
                 reply-to (if reply-to
                              (parse-address reply-to)
                              `((:name ,(getf (car from) :name) :host ,(getf (car from) :host)))))
           (handler-case 
             (send-it)
             ;;formerly mailer:mailer-not-running
             (mailer:mailer-error () (handle-mailer-error))))
       (error (last-resort "Error Mailing Error Report:" from to subject message-writer)))))

(define report-bug  (to subject format-string &rest format-args)
  "Reports a bug to TO with SUBJECT and message body
produced by applying format to format-string and format-args.
The from field is http:*server-mail-address*."
  (process:process-run-function "Report HTTP Server Bug"
                                #'send-mail-from http:*server-mail-address* to subject
                                (apply #'format nil format-string format-args)))

;;;------------------------------------------------------------------- 
;;;
;;; CLEAN UP HANGING HTTP PROCESSES
;;;

(define find-idle-http-processes (&optional (idle-time http:*server-timeout*))
  "Returns all HTTP server processes that have been idle for more than IDLE-TIME (sixtieths of a second)."
  (loop for process in process:*all-processes*
        when (and (http-server-process-p process)
                  (< idle-time (process:process-idle-time process)))
          collect process))

(declaim (inline http-server-process-p))

(define http-server-process-p (process)
  "Returns non-null when process is an HTTP server process."
  (string-equal "HTTP Server" (process:process-name process) :start1 0 :start2 0 :end1 11 :end2 11))

(define map-idle-http-processes (function &optional (predicate #'identity))
  "Returns all HTTP server processes that have been idle for more than IDLE-TIME (sixtieths of a second)."
  (loop for process in process:*all-processes*
        when (and (http-server-process-p process)
                  (funcall predicate process))
          do (funcall function process)))

;; Could be improved by maintaining a backpointer from process to HTTP server 8/25/99 -- JCMa.
(defun process-owned-by-http-server-p (process)
  "Returns non-null when process is owned by a running HTTP server."
  (flet ((uses-process-p (server)
	   (when (http::server-process-p server process)
	     (throw 'server-owns-process t))))
    (declare (dynamic-extent #'uses-process-p))
    (catch 'server-owns-process
      (map-http-servers #'uses-process-p)
      nil)))					;return NIL

(defun uninitialized-idle-http-server-process-p (process &aux idle-time)
  (and (http-server-process-p process)
       (numberp (setq idle-time (process:process-idle-time process)))
       ;use server time out because it controls how long to wait before dropping a connection
       (> idle-time http::*server-timeout*)
       ;; hanging states
       (member (process:process-whostate process) '("TCP Accept") :test #'equalp)
       (not (process-owned-by-http-server-p process))))

(defvar *idle-http-process-scavenger-timer* nil)

(defun idle-http-process-scavenger-timer ()
  (or *idle-http-process-scavenger-timer*
      (setq *idle-http-process-scavenger-timer* (process:create-timer-call 'scavenge-idle-http-processes '()
                                                                           :name "Scavenge Idle HTTP Server Processes"))))

;; set up a timer to run the scavenger every server timout interval
(defun synchronize-idle-http-process-scavenger ()
  (process:reset-timer-absolute (idle-http-process-scavenger-timer)
                                (+ (get-universal-time) (min (* http:*server-life-time* 1000.)
                                                             (ceiling http:*server-timeout* 60.)))))

(defparameter *safe-server-abort-states* '("TCP In" "TCP Out" "TCP Finish" "TCP Closing" "TCP Accept")
  "Process whostates from which is is safe to abort HTTP connnections.")

(defun server-safe-to-abort-p (server)
  "Returns non-null when it is safe to abort the HTTP connection for SERVER."
  (let ((process (http::server-process server)))
    (and process
	 (or (null *safe-server-abort-states*)
	     (member (process:process-whostate process) *safe-server-abort-states* :test #'equalp)))))

(define-variable *idle-connections-scavenged*  0
		 "The number of idle connections scavenged since boot time.")

;; Don't just kill active HTTP connections. Useful for web walker, log window
;; and other lengthly computations.   3/18/97 -- JCMa.
(define scavenge-idle-http-processes ()
  "Finds and kills any HTTP servers that have been idle for more than IDLE-TIME."
  (labels ((reason-to-die (server)
             (cond ((http::server-timeout-p server) "it has been idle too long")
                   ((http::server-life-time-expired-p server) "its lifetime expired")
                   (t nil)))
           (idle-time (server)
             (let ((idle (http::server-idle-time server)))
               (when idle
                 (ceiling idle 60.))))
           (report-connection-abort (server process reason-to-die)
	     (format nil "~&Forcibly Aborting ~A
                          ~&~5TState: ~A~&Reason: ~A
                          ~:[~;~&~10TIdle Time: ~:*~\\time-interval\\~]~
                          ~:[~;~&~10TURL: ~:*~A~]"
		     server (process:process-whostate process) reason-to-die
		     (idle-time server) (http::server-url-string server)))
           (kill-http-server (server reason-to-die)
             (let ((process (http::server-process server)))
               (when process                    ;beware of fencepost error
		 (prog1 (report-connection-abort server process reason-to-die)
			(scl:process-interrupt process #'http::abort-connection server)))))
	   (report-connection-scavenges (report)
	     (when http::*report-idle-connection-scavenges*
	       (http:report-bug http:*bug-http-server* "HTTP Connection Forcibly Aborted" report)))
           (maybe-kill-http-server (server &aux reason-to-die report)
	     (process:with-no-other-processes	;Don't allow process to change state
	       (when (and (setq reason-to-die (reason-to-die server))
			  (server-safe-to-abort-p server))
		 (setq report (kill-http-server server reason-to-die))))
	     (when report
	       (atomic-incf *idle-connections-scavenged*)
	       (report-connection-scavenges report)))
	   (maybe-kill-unitialized-http-server-process (process)
	     (when (uninitialized-idle-http-server-process-p process)
	       (process:process-kill process)
	       (atomic-incf *idle-connections-scavenged*)
	       (report-connection-scavenges
		 (format nil "~&Forcibly Aborting Unitialized HTTP Server Process
                              ~&State: ~A~&Reason: ~A~:[~;~&Idle Time: ~:*~\\time-interval\\~]"
			 (process:process-whostate process) "it has been idle too long" (process:process-idle-time process))))))
    (declare (inline reason-to-die))
    (unless http:*debug-server*
      (map-http-servers #'maybe-kill-http-server)
      (map-all-processes #'maybe-kill-unitialized-http-server-process))
    (synchronize-idle-http-process-scavenger)))

;; Remove all timers from the system at disk save time to avoid losing worlds
(si:add-initialization "Clear HTTP Timers"
                       '(mapc #'(lambda (x)
                                  (let ((timer (scl:symbol-value-globally x)))
                                    (when timer
                                      (process:clear-timer timer)
                                      (setf (scl:symbol-value-globally x) nil))))
                              '(*hourly-server-timer* *daily-server-timer* *idle-http-process-scavenger-timer*))
                       '(:before-cold))

;; start the timer running.
(si:add-initialization "Synchronize HTTP Timers"
                       '(progn
                          (synchronize-hourly-server-tasks)
                          (synchronize-daily-server-tasks)
                          (synchronize-idle-http-process-scavenger))
                       '(:now :login))

;;;------------------------------------------------------------------- 
;;;
;;; LOG-RELATED PORTABILITY CODE
;;;

(define make-lock (name &key (type :simple))
  "Returns a lock named name that is suitable for use with with-lock-held.
TYPE can be either :SIMPLE (the default) or :MULTIPLE-READER-SINGLE-WRITER.
A simple lock queues all operations whereas a :MULTIPLE-READER-SINGLE-WRITER lock
allows multiple read threads but only a single writer thread."
  (process:make-lock name :type type))

;(define-macro with-lock-held ((lock &optional (mode :write) whostate) &body body)
;  "Executes BODY with LOCK held in MODE, which is one of :READ or :WRITE."
;  `(loop with lock = ,lock
;         doing (cond ((process:lock-idle-p lock)
;                      (process:with-lock (lock ,mode) . ,body)
;                      (return))
;                     (t (process:process-wait ,whostate #'process:lock-idle-p lock)))))

(define-macro with-lock-held ((lock &optional (mode :write) whostate) &body body)
  "Executes BODY with LOCK held in MODE, which is one of :READ or :WRITE."
  (declare (ignore whostate))
  `(process:with-lock (,lock :mode ,mode)
     . ,body))

(define %make-log-pathname (device directory name &optional host)
  "Returns the pathname to which current log entries are written."
  (make-pathname
    :host (or host (http::host-domain-name (local-host)))
    :device device
    :directory directory
    :name name
    :type "text"))

(declaim (inline process-enable))

(define process-enable (process)
  "Enables PROCESS so that it can run."
  (process:enable process))

(declaim (inline process-disable))

(define process-disable (process)
  "Disables PROCESS so that it does not run."
  (process:disable process))

(define make-process (process-name &key priority quantum run-reasons background-p 
                                   restart-after-reset warm-boot-action &allow-other-keys)
  "Creates a process using a portable set of options."
  (declare (ignore background-p))
  (process:make-process process-name
                        :priority (if priority (process:make-process-priority :foreground priority) process:*default-process-priority*)
                        :quantum (or quantum si:default-quantum)
                        :run-reasons run-reasons
                        :restart-after-reset restart-after-reset
                        :warm-boot-action (ecase warm-boot-action
                                            (:delayed-restart 'process:process-warm-boot-delayed-restart)
                                            (:kill 'process:process-warm-boot-reset)
                                            (:restart 'process:process-warm-boot-restart))))

(mapc #'(lambda (sym) (export sym :www-utils)) '(process-disable process-enable make-process))


(define http:enable-http-service (&key (on-ports '(80)) listeners)
  "Top-level method for starting up HTTP servers."
  (declare (ignore listeners))
  (sys:enable-services '(:http) nil)
  on-ports)

(define http:disable-http-service (&optional (on-ports '(80)))
  "Top-level method for shutting down HTTP servers."
  (sys:disable-services '(:http) nil)
  on-ports)

(define http:http-service-enabled-p (&optional (on-ports '(80)))
  "Returns the ports on which HTTP services is enabled or NIL if HTTP is enabled on no ports."
  (if (neti:service-enabled-p :http)
      on-ports
      nil))


;;;------------------------------------------------------------------- 
;;;
;;; AN INTERFACE CONSISTENT WITH PROPERTY LIST MIXIN FOR PATHNAMES
;;;

(defmethod http:get-value ((pathname pathname) indicator &optional default)
  (let ((value (getf (scl:send pathname :plist) indicator :+not-found+)))
    (case value
      (:+not-found+
        (values default nil))
      (t (values value t)))))

(defmethod http::%put-value ((pathname pathname) indicator value)
  (scl:send pathname :putprop value indicator))

(defmethod (setf http:get-value) (value (pathname pathname) indicator &optional default)
  (declare (ignore default))
  (scl:send pathname :putprop value indicator))

(defmethod http:remove-value ((pathname pathname) indicator)
  (scl:send pathname :remprop indicator))

(defmethod http:property-list ((pathname pathname))
  (scl:send pathname :plist))

(defmethod http:map-indicators ((pathname pathname) function)
  (loop for item in (scl:send pathname :plist) by #'cddr
        do (funcall function item)))

(defmethod http:map-values ((pathname pathname) function)
  (loop for item in (cdr (scl:send pathname :plist)) by #'cddr
        do (funcall function item)))

(defmethod property-list-location ((pathname pathname))
  (scl:send pathname :property-list-location))

(export 'property-list-location :www-utils)

(defmethod property-list-value-location ((pathname pathname) indicator &optional default)
  (si:without-interrupts
    (multiple-value-bind (val found-p)
        (http:get-value pathname indicator default)
      (unless found-p
        (scl:send pathname :putprop val indicator))
      (scl:locf (scl:getf (cdr (property-list-location pathname)) indicator)))))

(export 'property-list-value-location :www-utils)

(scl:deflocf http:get-value (pathname indicator &optional default)
  `(property-list-value-location ,pathname ,indicator ,default))


;;;------------------------------------------------------------------- 
;;;
;;; CRLF CANONICALIZATION
;;;

(defun pathname-crlf-lock-idle-p (pathname)
  "Returns non-null of pathname is NOT CRLF locked."
  (declare (inline pathname-crlf-locked-p))
  (not (http:get-value pathname :crlf-locked-p)))

(defun pathname-crlf-locked-p (pathname)
  "Returns non-null of pathname is CRLF locked."
  (declare (inline pathname-crlf-locked-p))
  (http:get-value pathname :crlf-locked-p))

(defun %atomic-pathname-crlf-lock (pathname)
  (loop with location = (scl:locf (http:get-value pathname :crlf-locked-p))
        with process = scl:*current-process*
        for current-value = (scl:location-contents location)
        do (when (scl:store-conditional location current-value process)
             (return process))
           (process-wait "Grab CRLF Lock" #'pathname-crlf-lock-idle-p pathname)))

(defun %atomic-pathname-crlf-unlock (pathname)
  (loop with location = (scl:locf (http:get-value pathname :crlf-locked-p))
        for current-value = (scl:location-contents location)
        while current-value
        do (when (scl:store-conditional location current-value nil)
             (return nil))))

(defmacro with-pathname-crlf-locked ((pathname) &body body)
  "Grabs a lock on PATHNAME to prevent multiple threads from CRLF encoding the same pathname simultaneously."
  `(let ((pathname ,pathname))
     (unwind-protect
         (progn (%atomic-pathname-crlf-lock pathname)
                . ,body)
       (%atomic-pathname-crlf-unlock pathname))))

(declaim (inline %stream-encode-crlf-until-eof))

;; Find a faster translating stream if possible.   6/24/96 -- JCMa.
(defun %stream-encode-crlf-until-eof (from-stream to-stream)
  (using-resource (line-buffer http::line-buffer http::*line-buffer-size*)
    (loop with line and eof and delimiter and length
	  do (multiple-value-setq (line eof delimiter length)
	       (read-delimited-line from-stream '(#\Return #\Linefeed) nil line-buffer))
	  unless (zerop length)
	    do (with-fast-array-references ((array line vector))
		 delimiter			;ignore
		 (loop for idx upfrom 0 below length
		       do (scl:send to-stream :tyo (scl:char-to-ascii (aref array idx)))))
	  when (eql delimiter #\Return)
	    do (scl:send to-stream :tyo #.(scl:char-to-ascii #\Return))
	       (scl:send to-stream :tyo #.(scl:char-to-ascii #\Linefeed))
	  until eof)))

(declaim (inline %stream-decode-crlf-until-eof))

(defun %stream-decode-crlf-until-eof (from-stream to-stream)
  (using-resource (line-buffer http::line-buffer http::*line-buffer-size*)
    (with-fast-array-references ((output line-buffer vector))
      (unless (and (scl:operation-handled-p from-stream :read-input-buffer)
		   (scl:operation-handled-p to-stream :string-out))
	(error "Copying methods not supported on streams."))
      (loop with at-cr-flag and idx2 = 0 and input-limit = (array-total-size line-buffer)
	    doing (multiple-value-bind (buffer offset limit)
		      (scl:send from-stream :read-input-buffer)
		    (cond ((null buffer)
			   (unless (zerop idx2)
			     (scl:send to-stream :string-out output 0 idx2))
			   (return nil))
			  (t (with-fast-array-references ((input buffer array))
			       (loop for idx1 upfrom offset below limit
				     for ch = (aref input idx1)
				     do (cond ((and at-cr-flag (= ch #.(si:ascii-code #\Linefeed)))
					       (setq at-cr-flag nil))
					      (t (setf (aref output idx2) (si:ascii-to-char ch))
						 (setq at-cr-flag (= ch #.(si:ascii-code #\Return)))
						 (unless (< (incf idx2) input-limit)
						   (scl:send to-stream :string-out output 0 idx2)
						   (setq idx2 0))))
				     finally (scl:send from-stream :advance-input-buffer))))))))))

(defmethod http:stream-encode-crlf-until-eof ((from-stream si:stream) (to-stream si:stream))
  (error "This operation has not been defined from ~S to ~S." (type-of from-stream) (type-of to-stream)))

(defmethod http:stream-encode-crlf-until-eof ((from-stream si:character-stream) (to-stream si:unsigned-byte-8-stream))
  (%stream-encode-crlf-until-eof from-stream to-stream))

;; allows mac-fs:mac-rpc-file-binary-output-stream to work
(defmethod http:stream-encode-crlf-until-eof ((from-stream si:character-stream) (to-stream si:binary-stream))
  (%stream-encode-crlf-until-eof from-stream to-stream))

(defmethod http:stream-decode-crlf-until-eof ((from-stream si:stream) (to-stream si:stream))
  (error "This operation has not been defined from ~S to ~S." (type-of from-stream) (type-of to-stream)))

(defmethod http:stream-decode-crlf-until-eof ((from-stream si:unsigned-byte-8-stream) (to-stream si:character-stream))
  (%stream-decode-crlf-until-eof from-stream to-stream))

;; allows mac-fs:mac-rpc-file-binary-output-stream to work
(defmethod http:stream-decode-crlf-until-eof ((from-stream si:binary-stream) (to-stream si:character-stream))
  (%stream-decode-crlf-until-eof from-stream to-stream))

(defmethod http:stream-decode-crlf-until-eof ((from-stream si:character-stream) (to-stream si:character-stream))
  (si:stream-copy-until-eof from-stream to-stream))

;; the symbol is going to be a #:TERMINAL-IO-SYN-STREAM or similar   2/8/99 -- JCMa.
(defmethod http:stream-decode-crlf-until-eof ((from-stream si:character-stream) (to-stream symbol))
  (si:stream-copy-until-eof from-stream to-stream))

(defmethod http:valid-crlf-cache-file-p ((pathname fs:pathname))
  (declare (values valid-crlf-cache-p source-pathname crlf-pathname cache-pathname))
  (let (source-probe cache cache-probe)
    (with-open-file (source-file pathname :direction :probe :if-does-not-exist :error)
      (unless (setq source-probe (scl:send source-file :truename))
        (return-from http:valid-crlf-cache-file-p (values nil)))
      ;; get the cache counter part
      (setq cache (http:crlf-pathname source-probe))
      ;; check the cache counter part
      (with-open-file (crlf-file cache :direction :probe :if-does-not-exist nil)
        ;; cache exists
        (unless (setq cache-probe (and crlf-file (scl:send crlf-file :truename)))
          (return-from http:valid-crlf-cache-file-p (values nil source-probe nil cache)))
        ;; source-probe is more recent
        (unless (< (scl:send source-file :creation-date)
                   (scl:send crlf-file :creation-date))
          (return-from http:valid-crlf-cache-file-p (values nil source-probe cache-probe cache)))))
    ;; if we get here the cache is valid
    (return-from http:valid-crlf-cache-file-p (values t source-probe cache-probe cache))))

;; This has locking to avoid race conditions. 7/3/96 -- JCMa.
(defmethod http:ensure-crlf-canonical-file ((pathname fs:pathname))
  (declare (values crlf-canonicalized-pathname newly-updated-p))
  (flet ((clean-up-crlf-files (pathname)
           (loop with wild-pathname = (scl:send pathname :new-pathname :version :wild)
                 and files-deleted-p
                 for (file) in (cdr (fs:directory-list wild-pathname ':fast ':sorted))
                 unless (eql file pathname)
                   do (scl:send (http::crlf-origin-pathname file) :remprop :crlf-pathname)      ;remove pointer
                      (scl:send file :delete)   ;delete old cr-lf pathname
                      (setq files-deleted-p t)
                 finally (when files-deleted-p
                           (scl:send wild-pathname :expunge)))))
    (declare (inline clean-up-crlf-files))
    (loop
      (multiple-value-bind (valid-crlf-cache-p source-pathname crlf-pathname canonical-pathname)
          (http:valid-crlf-cache-file-p pathname)
        (cond (valid-crlf-cache-p (return-from http:ensure-crlf-canonical-file crlf-pathname))
              ((pathname-crlf-lock-idle-p source-pathname)
               ;; small window here where two threads could update the same file twice.   7/4/96 -- JCMa.
               (let ((new-crlf-pathname nil))
                 (with-pathname-crlf-locked (source-pathname)
                   (setq new-crlf-pathname (http:crlf-canonicalize-file source-pathname canonical-pathname)))
                 (when (and new-crlf-pathname
                            (not (eq new-crlf-pathname crlf-pathname)))
                   (clean-up-crlf-files new-crlf-pathname))
                 (return-from http:ensure-crlf-canonical-file (values new-crlf-pathname t))))
              (t (process-wait "CRLF Wait" #'pathname-crlf-lock-idle-p source-pathname)))))))

(defmethod http:crlf-pathname ((pathname pathname))
  (cond ((http::get-value pathname :crlf-pathname))
        (t (let ((path (make-pathname :host (pathname-host pathname)
                                      :device (pathname-device pathname)
                                      :directory (pathname-directory pathname)
                                      :name (pathname-name pathname)
                                      :type (http::crlf-pathname-type (pathname-type pathname))
                                      :version (pathname-version pathname))))
             (http::%put-value pathname :crlf-pathname path)
             path))))

(defmethod http:copy-file ((from-pathname pathname) (to-pathname pathname)
                           &key (element-type :default)
                           (copy-creation-date t) (copy-author t) (report-stream nil)
                           (create-directories :query) (if-exists nil) )
  (scl:copy-file from-pathname to-pathname
                 :element-type element-type
                 :copy-creation-date copy-creation-date
                 :copy-author copy-author
                 :report-stream report-stream
                 :create-directories create-directories
                 :if-exists if-exists))

(eval-when (compile eval load)
  (export (intern "PATHNAME-EXTERNAL-NAME-STRING" :www-utils) :www-utils))

(declaim (inline pathname-external-name-string))

(defun pathname-external-name-string (string &optional start end)
  "Removes any operating system specific characters from STRING,
returning a copy of the string."
  (declare (ignore start end))
  string)

(defmethod set-file-author ((pathname pathname) (author string) &optional (error-p t))
  (fs:change-file-properties pathname error-p :author author))

(defun load-http-logical-host ()
  "Loads the HTTP logical host to ensure correct directory mappings."
  (load "sys:site;http.translations" :verbose nil))

(declaim (inline generate-message-id))

(defun generate-message-id (&optional time uniquizer)
  "Generates a unique message ID based on TIME, universal time, 
and UNIQUIZER, a factor assuring two threads on the same machine cannot collide."
  (zwei:generate-standard-message-id time uniquizer))

(declaim (inline copy-vector-portion))

(defun copy-vector-portion (from-vector from-start from-end to-vector to-start to-end)
  (scl:copy-array-portion from-vector from-start from-end to-vector to-start to-end))

(defmethod http::write-stack-backtrace ((error error) stream &optional (n-frames http::*stack-backtrace-number-of-frames*))
  (dbg:with-erring-frame (dbg:*current-frame* error)
    (dbg:bug-report-description error stream n-frames)))


;;;------------------------------------------------------------------- 
;;;
;;; FAST INTEGER BIT VECTOR HACKING
;;;

;; No function calls and no consing.
(define decode-bitmask (mask &aux integer-list)
  "Decode an integer used as a binary interpretation for non-negative integers."
  (declare (values integer-list))
  (macrolet ((collect-mask-indices (mask function &optional base)
               `(progn
                  ,.(loop for idx downfrom 31 to 0
                          collect `(and (ldb-test ,(byte 1 idx) ,mask)
                                        (push ,(cond ((null base) idx)
                                                     ((zerop idx) `(* 32. ,base))
                                                     (t `(+ (* 32. ,base) ,idx)))
                                              integer-list))))))
    (etypecase mask
      (bignum
        (loop for base downfrom (1- (si:bignum-length mask)) to 0
              for word  = (si:bignum-ref mask base)
              unless (zerop word)
                do (collect-mask-indices word function base)))
      (fixnum
        (collect-mask-indices mask function)))
    integer-list))

(define map-bitmask (mask function)
  "Maps FUNCTION over every index position in MASK where the bit is 1.
This maps up from the lowest bit, which is position 1."
  (check-type mask integer)
  (macrolet ((call-function-on-mask-indices (mask function &optional base)
               `(progn
                  ,.(loop for idx upfrom 0 below 32
                          collect `(and (ldb-test ,(byte 1 idx) ,mask)
                                        (funcall ,function ,(cond ((null base) idx)
                                                                  ((zerop idx) `(* 32. ,base))
                                                                  (t `(+ (* 32. ,base) ,idx)))))))))
    (etypecase mask
      (bignum
        (loop for base from 0 below (si:bignum-length mask)
              for word  = (si:bignum-ref mask base)
              unless (zerop word)
                do (call-function-on-mask-indices word function base)))
      (fixnum
        (call-function-on-mask-indices mask function)))))

(define reverse-map-bitmask (mask function)
  "Maps FUNCTION over every index position in MASK where the bit is 1.
This maps down from the highest bit."
  (macrolet ((call-function-on-mask-indices (mask function &optional base)
               `(progn
                  ,.(loop for idx downfrom 31 to 0
                          collect `(and (ldb-test ,(byte 1 idx) ,mask)
                                        (funcall ,function ,(cond ((null base) idx)
                                                                  ((zerop idx) `(* 32. ,base))
                                                                  (t `(+ (* 32. ,base) ,idx)))))))))
    (etypecase mask
      (bignum
        (loop for base downfrom (1- (si:bignum-length mask)) to 0
              for word  = (si:bignum-ref mask base)
              unless (zerop word)
                do (call-function-on-mask-indices word function base)))
      (fixnum
        (call-function-on-mask-indices mask function)))))


;;;------------------------------------------------------------------- 
;;;
;;; SHTML TEMPLATE PARSING
;;;

(defconstant +shtml-tag-start+ #.(tcp::string-to-8-bit-vector "<!--#"))
(defconstant +shtml-tag-start-length+ #.(length "<!--#"))
(defconstant +shtml-tag-end+ #.(tcp::string-to-8-bit-vector "-->"))
(defconstant +shtml-tag-end-length+ #.(length "-->"))

(declaim (special http::*server-line-buffer* http::*line-buffer-size*))

(defun vector-to-string (vector &optional (start 0) (end (length vector)) buffer)
  (let* ((length (- end start))
	 (string (or buffer (make-array length :element-type 'scl:string-char :fill-pointer 0))))
    (with-fast-array-references ((vector vector vector) (string string string))
      (loop for idx1 upfrom start below end
	    for idx2 upfrom (fill-pointer buffer)
	    for byte = (aref vector idx1)
	    for char = (if (member byte '#.(mapcar #'si:char-to-ascii  '(#\return #\linefeed))) #\space (si:ascii-to-char byte))
	    do (setf (aref string idx2) char)
	    finally (setf (fill-pointer string) (1+ idx2))
		    (return (values string length))))))

#+Trace-SHTML-Parser
(defparameter *idx* 0)

#+trace-shtml-parse
(defun trace-find-pattern (idx partial pattern p-start p-end buffer start window)
  (format t "~&~'b~D Wrap Tag:~ [~D ~D] pat= ~S (~S) buffer= ~S ~D ~D"
	  *idx* idx partial
	  (tcp::8-bit-vector-to-string pattern) (tcp::8-bit-vector-to-string pattern p-start p-end)
	  (tcp::8-bit-vector-to-string buffer start (+ start window))
	  start (+ start window)))

(defun %search-for-pattern-in-vector (pattern vector &optional (s1 0) (e1 (length pattern)) (s2 0) (e2 (length vector))
					      &aux match-idx match-item window-start)
  "Matches PATTERN in VECTOR returning the first index where PATTERN appears in VECTOR.
If a partial pattern appear at the end of vector, a PARTIAL-MATCH-WINDOW is returned as
a second value to indicate the number of elements matched within VECTOR."
  (declare (values index partial-match-window))
  (macrolet ((initialize-window (pattern pattern-start)
	       `(setq match-idx ,pattern-start
		      match-item (aref ,pattern match-idx))))
    (with-fast-array-references ((v1 pattern) (v2 vector))
      (loop initially (initialize-window v1 s1)
	    for idx upfrom s2 below e2
	    for item = (aref v2 idx)
	    do #+ignore(format t "~&~D. (eql ~S ~S) => ~S" idx item match-item (eql match-item item))
	       (cond ((eql match-item item)
		      (when (= match-idx s1) (setq window-start idx))
		      (incf match-idx)
		      (when (= match-idx e1)
			#+Trace-SHTML-Parser
			(unless (zerop s1) (format t "~&~'b~D Complete Partial Match (~D) at ~D~" *idx* (- e1 s1) window-start))
			(return-from %search-for-pattern-in-vector window-start))
		      (setq match-item (aref pattern match-idx)))
		     ((= match-idx s1))
		     (t (initialize-window v1 s1)))
	    finally (cond ((= match-idx s1) (return-from %search-for-pattern-in-vector nil))
			  (t #+Trace-SHTML-Parser(format t "~&~'b~D Partial Match (~D) at ~D~" *idx* match-idx window-start)
			     (return-from %search-for-pattern-in-vector (values window-start match-idx))))))))

(defmethod http:parse-shtml-template ((pathname pathname))
  (declare (values template-parameters))
  (with-open-file (file-stream pathname :direction :input :element-type '(unsigned-byte 8))
    (using-resource (elt-buf-resource http::line-buffer http::*line-buffer-size*)
      (flet ((new-element-buffer () (prog1 elt-buf-resource (setf (fill-pointer elt-buf-resource) 0)))
	     (make-entry (read-start read-end buffer s-idx e-idx element-buffer)
	       (vector-to-string buffer s-idx e-idx element-buffer)
	       (multiple-value-bind (function plist)
		   (http::parse-shtml-element element-buffer 0 (fill-pointer element-buffer))
		 (list* read-start read-end function plist)))
	     (find-pattern (pattern buffer &optional (p-start 0) (p-end (length pattern)) (start 0) (end (length buffer)))
	       (declare (values start-index partial-offset complete-offset))
	       (if (zerop p-start)
		   (%search-for-pattern-in-vector pattern buffer 0 p-end start end)
		   (let* ((window (- p-end p-start))) 	;partial match in previous buffer
		     (multiple-value-bind (idx partial)
			 (%search-for-pattern-in-vector pattern buffer p-start p-end start (+ start window))
		       #-trace-shtml-parse (declare (ignore partial))
		       #+trace-shtml-parse (trace-find-pattern idx partial pattern p-start p-end buffer start window)
		       (if (and idx (= idx start))	;force
			   (return-from find-pattern (values idx nil window))
			   (%search-for-pattern-in-vector pattern buffer 0 p-end start end)))))))
	(declare (inline new-element-buffer))
	#+trace-shtml-parse (setq *idx* 0)	  
	(loop with buffer and offset and limit and buffer-pos and element-buffer
	      and read-start = 0 and read-end = 0 and read-offset = 0 and wrap-idx
	      do (multiple-value-setq (buffer offset limit) (scl:send file-stream :read-input-buffer))
	      unless buffer
		unless (zerop read-offset)
		  collect (list read-start (+ read-start read-offset))
		    end
		  and do (loop-finish)
	      while buffer
	      do (setq buffer-pos offset)
		 #+trace-shtml-parse(format t "~&~'bNew Buffer:~ ~S ~D ~D"  buffer offset limit)
	      when element-buffer
		collect (multiple-value-bind (end partial-idx completion-size)
			    (find-pattern +shtml-tag-end+ buffer (or wrap-idx 0) +shtml-tag-end-length+ buffer-pos limit)
			  (when (or (null end) partial-idx (and completion-size (null wrap-idx)))
			    (error "Malformed element (byte ~D): ~S" read-end element-buffer))
			  (setq buffer-pos (+ end (or completion-size +shtml-tag-end-length+)))
			  (prog1 (make-entry read-start read-end buffer offset buffer-pos element-buffer)
				 (setq read-start (+ read-end (fill-pointer element-buffer))
				       element-buffer nil wrap-idx nil)))
	      nconc (loop with start and end and completion-size and sv1 = (or wrap-idx 0)
			  do (multiple-value-setq (start wrap-idx completion-size)
			       (find-pattern +shtml-tag-start+ buffer sv1 +shtml-tag-start-length+ buffer-pos limit))
			  while start
			  until wrap-idx
			  do #+trace-shtml-parse (incf *idx*)
			     (setq element-buffer (new-element-buffer))	;set up new element buffer
			     (cond (completion-size	;wrapped on start tag
				    (vector-to-string +shtml-tag-start+ 0 sv1 (new-element-buffer))
				    (setq read-end (+ read-start (- read-offset sv1))
					  sv1 0))	; reset pattern offset
				   (t (unless (zerop sv1) (setq sv1 0))	; reset pattern offset if completion of partial fails
				      (setq read-end (+ read-start read-offset (- start buffer-pos)))))
			     (setq read-offset 0)
			     (multiple-value-setq (end wrap-idx)	;get end tag
			       (find-pattern +shtml-tag-end+ buffer
					    0 +shtml-tag-end-length+ (+ start (or completion-size +shtml-tag-start-length+)) limit))
				 
			     (when wrap-idx	;wrapped on end tag
			       (vector-to-string buffer start limit element-buffer)
			       (loop-finish))
			  when end		;standard case -- no wrap
			    collect (prog2 (setq buffer-pos (+ end +shtml-tag-end-length+))
					   (make-entry read-start read-end buffer start buffer-pos element-buffer)
					   (setq read-start (+ read-end (fill-pointer element-buffer))
						 element-buffer nil))	;reset element buffer
			  else do (vector-to-string buffer start limit element-buffer)	;wrapped on element body
				  (loop-finish))
	      do (unless (or element-buffer (= read-start limit))	;keep track of bytes to read.
		   (incf read-offset (- limit buffer-pos)))
		 (scl:send file-stream :advance-input-buffer))))))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defmacro define-pathname-canonical-type (canonical-type (default &key byte-size) &body specs)
  (let ((keyword (intern (string-upcase canonical-type) :keyword)))
    `(prog1
       (fs:define-canonical-type ,keyword ,default ,@specs)
       ,@(when byte-size
	   (check-type byte-size (mod 256))
	   `((setf (get ,keyword :binary-file-byte-size) ,byte-size))))))


(define-pathname-canonical-type :class ("CLASS" :byte-size 8)
  (:unix "CLASS" "CLASS")
  (:unix42 "CLASS" "CLASS")
  ((:vms4 :vms4.4) "CLASS")
  ((:vms :msdos :os/2) "CLS"))

(define-pathname-canonical-type :java ("JAVA")
  (:unix "JAVA" "JAVA")
  (:unix42 "JAVA" "JAVA")
  ((:vms4 :vms4.4) "JAVA")
  ((:vms :msdos :os/2) "jva"))

(sct:define-module-type :java :java :class
  sct:no-load-or-compile-module)

(sct:define-module-type :class :class nil
  sct:binary-data-module)

;;:png :au :aiff :aif :wav :ram :mpeg mpg :qt mov :sit :macbin :zip :z :gz :tgz :dir :dcr

(define http-user-email-address ()
  "Returns the email address of the user running the http lisp image."
  (destructuring-bind (user-id mail-host)
      (scl:send si:*user* :mail-address)
    (concatenate 'string user-id "@" (scl:send mail-host :mail-name))))

(define tcp-service-port-number (protocol &optional error-p)
  "Returns the service port number for the TCP protocol denoted by protocol.
PROTOCOL is a keyword,, but integer and string are also supported."
  (etypecase protocol
    (integer protocol)
    (keyword (tcp:protocol-name-tcp-port protocol error-p))
    (string (tcp:protocol-name-tcp-port (http::symbolize protocol http::*keyword-package*) error-p))))

;; Specialize some printing methods for fast-format
(defmethod http::%princ-item ((string string) (stream si:output-stream))
  (scl:send stream :string-out string 0 (zl:array-active-length string)))

(defmethod http::%princ-item ((integer integer) (stream si:output-stream))
  (si:print-fixnum-or-bignum integer stream))

(defmethod http::%princ-item ((symbol symbol) (stream si:output-stream))
  (si:print-pname-string symbol nil stream))

(defmethod http::%princ-item ((list list) (stream si:output-stream))
  (si:print-list list *print-level* nil stream nil)) 
