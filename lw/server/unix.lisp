;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: www-utils -*-

;;; (C) Copyright 1994-1995, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;; (C) Allegro enhancements Copyright 1995, OBC. All Rights Reserved.
;;;
;;; LispWorks enhancements Copyright (C) 1995-1998 Harlequin Group plc  All rights reserved.
;;;



;;;------------------------------------------------------------------- 
;;;
;;; MAC AND LISPM FILES COMPATABILITY CODE ADAPTED FOR LISPWORKS
;;;

(in-package :www-utils)

(define report-condition (condition stream)
  "Prints the report string for CONDITION onto STREAM."
  (handler-case (format stream "~A" condition)
    ;; Cannot guaranty all errors are printable.
    (error ()
	   (format stream "~&~A~%" condition)
	   (describe condition stream))))

(define report-string (condition)
  "Returns the report string for CONDITION."
  (with-output-to-string (stream)
    (report-condition condition stream)))

(defmethod http::write-stack-backtrace ((error condition) stream &optional n-frames)
  (dbg::output-backtrace :bug-form stream))

;; Define equivalence mapping to the MCL case.
(deftype file-not-found () 
  "Specialization of Common Lisp File-error in which the file was not found on open."
  '(and condition file-error))

#+lispworks3.2
(defun special-operator-p (symbol)
   (special-form-p symbol))

#+lispworks3.2
(export 'special-operator-p :www-utils)

; with-tcp-port-for-protocol not used !

(defmacro with-array-registers (bindings &body body)
  `(let ,bindings ,@body))

(defmacro with-fast-array-references (bindings &body body)
  "Declares the arrays in bindings (var value &optional type)
as type and sets speed to 3 with safety 0 within its scope."
  (loop for (var val type) in bindings
	collect `(,var ,val) into n-bindings
	when type
	  collect `(type ,type ,var) into type-dcls
	finally (return `(let ,n-bindings
			   (declare (optimize (speed 3) (safety 0)) . ,type-dcls)
			   ,@body))))

#+clim-sys
(define-macro atomic-incf (reference &optional (delta 1))
  "Atomically increments REFERENCE by DELTA."
  `(clim-sys:atomic-incf ,reference ,delta))

#+clim-sys
(define-macro atomic-decf (reference &optional (delta 1))
  "Atomically decrements REFERENCE by DELTA."
  `(clim-sys:atomic-decf ,reference ,delta))

#+clim-sys
(define-macro atomic-push (item reference)
  "Atomically pushes ITEM onto REFERENCE."
  `(clim-sys:without-scheduling
     (push ,item ,reference)))

#+clim-sys
(define-macro atomic-pop (reference)
  "Atomically pops an item off REFERENCE."
  `(clim-sys:without-scheduling
     (pop ,reference)))

#+clim-sys
(define-macro atomic-conditional-replacef (reference predicate new-value)
  "When PREDICATE returns non-null, this setfs REFERENCE to NEW-VALUE.
Predicate is called (OLD-VALUE NEW-VALUE). The operation 
assures that precicate applicaiton ande swap are atomic."
  (declare (values old-value))
  (let ((old-value (gensym))
          (new-value-var (gensym))
	  (pred (gensym)))
     `(clim-sys:without-scheduling
          (let ((,old-value ,reference)
                  (,new-value-var ,new-value)
                  (,pred ,predicate))
             (when (funcall ,pred ,old-value ,new-value-var)
                 (prog1 ,old-value
                    (setf ,reference ,new-value-var)))))))

(declaim (inline arglist))

(defun arglist (function)
  "Returns the arglist for FUNCTION."
  (declare (values (arglist values type arglist-types value-types)))
  #-lispworks4.0   ; 4.0 is an anomaly: LW package in bother newer and older versions!
  (lw:function-lambda-list function)
  #+lispworks4.0
  (hcl:function-lambda-list function))

;;;------------------------------------------------------------------- 
;;;
;;; OBTAINING THE DOMAIN NAME FOR AN IP ADDRESS
;;;

(declaim (inline %parse-host-address))

(defun %parse-host-address (address)
  "Returns an IP-NUMBER which is integer denoting the address of host."
  (declare (values ip-number))
  (etypecase address
    (integer address)
    (string (or (ipc:string-ip-address address)
		(ipc:internet-address address)))))

(declaim (inline ip-address-for-parsed-ip-address))

(define ip-address-for-parsed-ip-address (ip-number)
  "Returns an IP address as a string from, IP-NUMBER, the parsed address."
  (ipc:ip-address-string ip-number))

(defvar *last-domain-name* nil)

;;; There are surely better strategies to get your domain name.
;;; But they depend on your OS. Can someone shine light on
;;; getting DNS domain names portably on UNIX?
;;;
(defun default-domain-name (&optional (where "HTTP:lw;defaultdomain"))
  (flet ((dnp (dn)
           (and (stringp dn)
		;; Need at least one of #\. or this may not be DNS!
		;; Number may vary with contries?
                (find #\. dn))))
    (let ((dn0 (ipc:getdomainname))
          dn)
      (cond ((dnp dn0)
             dn0)
            ((and
              (setq dn (if (probe-file where)
                           (with-open-file (stream where :direction :input)
                             (read-line stream))))
              (dnp dn))
                (setq *last-domain-name* dn))
            (t
             (warn "Unexpected DNS domain name ~s." (or dn dn0))
             (format t "~&Enter you local DNS domain name 'local.institution.type': ")
             (peek-char t)
             (setq dn (read-line))
             (cond ((dnp dn)
                    (setq *last-domain-name* dn)
                    (with-open-file (stream where :direction :output
                                     :if-does-not-exist :create
                                     :if-exists :supersede)
                      (write-string dn stream)
                      (terpri stream))
                    dn)
                   (t
                    "the.unknown.domain")))))))

(defvar *domain-name-lookup* (make-hash-table :test #'eql))

(define domain-name-for-parsed-ip-address (ip-number &optional (no-error-p t))
  (or (gethash ip-number *domain-name-lookup*)
      (setf (gethash ip-number *domain-name-lookup*)
            (let ((host-name (if no-error-p
                                 (ignore-errors (ipc:get-host-name-by-address ip-number))
                               (ipc:get-host-name-by-address ip-number))))
              (if host-name
                  (if (find #\. host-name) ; some OSes return fully qualified names
                      host-name
                    (concatenate 'string host-name "." (default-domain-name)))
                ;; No error fall-back
                (ip-address-for-parsed-ip-address ip-number))))))

(declaim (inline domain-name-for-ip-address))

(define domain-name-for-ip-address (address &optional (no-error-p t))
  "Given the IP address, ADDRESS, this returns the domain name or NIL."
  (domain-name-for-parsed-ip-address (%parse-host-address address) no-error-p))

(declaim (inline ip-address-for-host-domain-name))

(define ip-address-for-host-domain-name (domain-name)
  "Returns the IP address string for domain-name."
  (ip-address-for-parsed-ip-address (%parse-host-address domain-name))) 

(declaim (inline %local-host-parsed-ip-number))

(defun %local-host-parsed-ip-number ()
  (ipc:internet-address (machine-instance)))

(define local-host (&optional recache-p)
  "The host object for the local host on which we are running.  For this port, we use the IP address."
  (local-host-parsed-ip-address recache-p))

(define local-host-ip-address (&optional recache-p)
  "Returns the IP address of the local host."
  (cond ((and (not recache-p) http::*local-host-ip-address*))
        (t (setq http::*local-host-ip-address* (ip-address-for-parsed-ip-address 
                                                 (%local-host-parsed-ip-number))))))

(define local-host-parsed-ip-address (&optional recache-p)
  "Returns the parsed IP address of the local host."
  (cond ((and (not recache-p) http:*local-host-address*))
        (t (setq http:*local-host-address* (%local-host-parsed-ip-number)))))

(define %local-host-domain-name ()
  (let ((ip-number (%local-host-parsed-ip-number)))
    (if (zerop ip-number)
        ;; zerop means no network connection and no DNS
        (ip-address-for-parsed-ip-address ip-number)
        ;; normal case wth network and DNS
        (domain-name-for-parsed-ip-address ip-number))))

(defun local-host-domain-name (&optional recache-p)
  "Returns the local host domain name."
  (cond ((and (not recache-p) http::*local-host-domain-name*))
        (t (setq http::*local-host-domain-name* (%local-host-domain-name)))))


;;;------------------------------------------------------------------- 
;;;
;;; HOST RELATED
;;;

(define parse-host (address &optional no-error-p)
  "Top-level method for parsing a host ADDRESS."
  (declare (values ip-number))
  (cond (no-error-p
         (handler-case
           (%parse-host-address address)
           (network-error () nil)))
        (t (%parse-host-address address))))

(declaim (inline host-mail-name))

(define host-mail-name (host)
  "The internet mail name for HOST."
  (domain-name-for-ip-address host t))

(define host-eq (host1 host2)
  "Returns non-null if HOST1 is equal to HOST2."
  (cond ((or (null host1) (null host2))
         nil)
        ((and (integerp host1) (integerp host2))
         (= host1 host2))
        (t (= (%parse-host-address host1)
              (%parse-host-address host2)))))

(define host-http-name (host)
  "Returns the internet host name for HOST."
  (host-mail-name host))

(declaim (inline %host-log-name))

(define %host-log-name (address host &optional resolve-ip-address-p)
  "Returns a string for use in logging server access."
  (declare (ignore host))
  (if resolve-ip-address-p
      (domain-name-for-parsed-ip-address address t)
      (ip-address-for-parsed-ip-address address))) 


;;;------------------------------------------------------------------- 
;;;
;;; FILE RELATED OPERATIONS
;;;

(define file-stream-creation-date (file-stream)
  "Returns the creation date in universal time for FILE-STREAM's source file."
  (file-write-date file-stream))

(declaim (inline file-stream-length-in-bytes))

(define file-stream-length-in-bytes (file-stream)
  "Returns the length in bytes for FILE-STREAM's source file."
  (file-length file-stream))

(defgeneric file-length-in-bytes (pathname-or-url &optional new-length)
   (:documentation "Returns the number of bytes in PATHNAME-OR-URL or NIL."))

(declaim (inline %file-length-in-bytes))

(defun %file-length-in-bytes (pathname)
  #+lispworks3.2
  (file-length pathname)
  #-lispworks3.2
  (sys:file-size pathname))

(defmethod file-length-in-bytes ((pathname pathname) &optional new-length)
  (declare (ignore new-length))
  (%file-length-in-bytes pathname))

(defmethod file-creation-date ((pathname pathname))
  (file-write-date pathname))

(define file-stream-modification-date (file-stream)
  "Returns the modification date in universal time for FILE-STREAM's source file."
  (file-write-date file-stream))

(defmethod file-modification-date ((pathname pathname))
  (file-write-date pathname))

(declaim (inline file-stream-version))

(defun file-stream-version (file-stream)
  (file-stream-creation-date file-stream))

(declaim (inline file-version))

(defun file-version  (pathname)
  (when (probe-file pathname)
    (file-creation-date pathname)))

(define file-properties (pathname)
  "Returns the length in bytes  and the creation in in universal time 
 for FILE-STREAM's source file."
  (declare (values length-in-bytes creation-date version))
  (let ((creation-date (file-creation-date pathname)))
    (values (%file-length-in-bytes pathname)
	    creation-date
	    creation-date)))

(declaim (inline pathname-directory-p))

(defun pathname-directory-p (pathname)
  "Returns non-null if PATHNAME¬denotes a directory."
  #+lispworks3.2
  (lw:directoryp pathname)
  #+lispworks4
  (lw:file-directory-p pathname))

;;; When it's unclear what shell is used by CL, check the shell argument SHELL
;;; -- OBC
#-LispWorks4
(defun system (arg)
  (sys::call-system arg))

;;; OBC added
#-LispWorks4
(defun unix-sh-test (cond path &aux (strpath (cond ((stringp path) path)
                                                   ((pathnamep path)
                                                    (namestring path)))))
  (if strpath
      (= (system (format nil "test ~a \"~a\" || exit 1"
			 cond strpath))	; This is the fix: "strpath"!!
	 0)
    nil))

(defun directory-list (pathname &rest options)
  "Returns a lisp Machine style directory listing."
  (let ((pathnames (apply #'unix-directory-list* pathname nil options)))
    (when (member :sorted options)
      (setq pathnames (sort pathnames #'(lambda (x y)
                                          (and (string< (pathname-name x)
                                                        (pathname-name y))
                                               (string< (pathname-type x)
                                                        (pathname-type y)))))))
    (loop with length and creation-date
          for path in pathnames
          do (multiple-value-setq (length creation-date)
               (file-properties path))
          collect `(,path 
                    ,.(when length `(:length-in-bytes ,length))
                    ,.(when creation-date `(:creation-date ,creation-date))))))

(declaim (inline alphalessp))

(defun alphalessp (a b)
  (string< a b))

(define directory-info (pathname &key (name :wild) (type :wild) (version :newest) (sort-pathnames t)
                                 directories)
  "Returns a property list of information for every file in the directory PATHNAME
that matches pathnames wildcards. Directories are included when directories is non-null."
  (declare (notinline))
  (flet ((get-directory-listing (p &optional (sort-p sort-pathnames))
           (let ((args nil))
             (declare (dynamic-extent args))
             (when sort-p (push :sorted args))
             (when directories (push :directories args))
             (apply #'directory-list p :no-extra-info args)))
         (pattern (path type)
           (make-pathname :name (etypecase name
                                  (keyword
                                    (ecase name
                                      (:wild :wild)))
                                  (string name))
                          :type (etypecase type
                                  (keyword
                                    (case type
                                      (:wild :wild)
                                      (t (symbol-name type))))
                                  (string type))
                          :version (etypecase version
                                     (keyword
                                       (ecase version
                                         (:wild nil)
                                         (:newest :newest))))
			  :defaults path))
         (sorter (e1 e2)
           (let ((p1 (car e1))
                 (p2 (car e2)))
             (and (alphalessp  (pathname-name p1) (pathname-name p2))
                  (let ((t1 (pathname-type p1))
                        (t2 (pathname-type p2)))
                    (cond ((and t1 t2)
                           (alphalessp t1 t2))
                          (t1 nil)
                          (t t)))))))
    (let ((p (pathname pathname)))
      (typecase type
        (keyword
          (ecase type
            (:wild (get-directory-listing (pattern p :wild)))))
        (string
          (get-directory-listing (pattern p type)))
        (cons
          (loop for type in type
                nconc (get-directory-listing (pattern p type)) into paths
                finally (return (if sort-pathnames
                                    (sort paths #'sorter)
                                    paths))))))))

(defun unix-directory-list* (pathname predicate &rest options)
  (let ((dirs (directory pathname
                         ;; This fixes directory problem on UNIX for Allegro
                         ;; LispWorks does this by default anyway
                         #-lispworks :directories-are-files #-lispworks nil)))
    (unless (member :directories options)
      (setq dirs (loop for file in dirs
		       unless (pathname-directory-p file)
		       collect file)))
    (when predicate
      (setq dirs (loop for file in dirs
                       when (funcall predicate file)
                       collect file)))
    dirs))

(defun directory-list* (pathname predicate &rest options)
  "Accepts the options :FILES :DIRECTORIES :SORTED :PROPERTIES."
  (let ((pathnames (apply #'unix-directory-list* pathname predicate options)))
    (when (member :sorted options)
      (setq pathnames (sort pathnames #'(lambda (x y)
                                          (and (string< (pathname-name x)
                                                        (pathname-name y))
                                               (string< (pathname-type x)
                                                        (pathname-type y)))))))
    (cond ((member :properties options)
           (loop for path in pathnames
                 collect  (multiple-value-bind (length creation-date)
                              (file-properties path) 
                            `(,path 
                              ,.(when length `(:length-in-bytes ,length))
                              ,.(when creation-date `(:creation-date ,creation-date))))))
          (t pathnames))))

(define create-directories-recursively (pathname)
  "Recursively create directories according to the directories present in PATHNAME."
  #-LispWorks4
  (create-directory-recursively1 pathname)
  #+LispWorks4
  (ensure-directories-exist pathname))

;;; For implementations where pathname-directory does
;;; not return NIL when there is no directory in the pathname.
;;; -- OBC
#-LispWorks4
(defun pathname-dirs (pathname)
  (let ((dirs (pathname-directory pathname)))
    (and (consp dirs) dirs)))

;;; -- OBC
#-LispWorks4
(defun create-a-directory (path &optional (error-p t))
  (let ((str (namestring path))) ;;(directorystring path)
    (case (system (format nil "mkdir ~S" str))
      (0 path)
      (t (if error-p
             (if (probe-directory path)
                 (error "create-a-directory: file or directory already exists: ~a" path)
               (error "create-a-directory: failed on: ~a" path))
           path)))))

;;; Return path if you can write in it or over it.
;;; -- OBC
#-LispWorks4
(defun file-permit-p (path &optional (permission "w"))
  (and (unix-sh-test (concatenate 'string "-" permission) path) path))

;;; -- OBC
#-LispWorks4
(defun create-directory-recursively1 (path &optional (error-p t))
  (ctypecase path
    (string (setq path (translate-logical-pathname (pathname path))))
    (pathname))
  ;; most system cannot create a whole directory from scratch so
  ;; recursively create directories for path to be valid
  (let ((host (pathname-host path))
        (order-dirs (nreverse (maplist #'reverse (reverse (pathname-dirs path)))))
        lastpath result)
    (dolist (dirs order-dirs)
      (setq lastpath (make-pathname :host host
                                    :directory dirs))
      (cond ((probe-directory lastpath)
             (setq result lastpath))
            (t
             (if error-p
                 (setq result (create-a-directory lastpath error-p))
               (if (and result (file-permit-p result))
                   (setq result (create-a-directory lastpath error-p))
                 ;; quiet and early termination, don't want to bother
                 ;; user with any low system messages since ERROR-P is NIL
                 (return))))))))

(defgeneric probe-directory (pathname)
  (:documentation "Returns non-null if the directory pathname exists."))

(defmethod probe-directory ((pathname pathname))
  (probe-file
   (lw:pathname-location pathname) ;unlike make-pathname, this preserves the type of pathname
   ))

(defmethod probe-directory ((pathname string))
  (probe-directory (pathname pathname)))
   
(define-macro with-automatic-login ((host user-id user-pw) &body body)
  "Supplies userid and PW to ensure successul FTP login to host with BODY."
  `(progn (notify-log-window "~&(WITH-AUTOMATIC-LOGIN (~S ~S ~S) - Not available on MAC"
                             ,host ,user-id ,user-pw)
          ,@body))

(define ftp-directory-info (directory &optional (user-id "anonymous") (user-pw (user-mail-address)))
  "Returns a list of pathname spec for directory just like DIRECTORY-INFO.
If a network error is encountered, this returns NIL."
  (let* ((path (pathname directory))
         (host (pathname-host path)))
    (handler-case 
      (with-automatic-login (host user-id user-pw)
        ;; ansi CL directory fails due to :fast option  3/13/94 -- JCMa.
        (directory-info directory))
      ;; handle remote connection problems, including dead host, refused connection.
      (remote-network-error () nil))))

(define ftp-copy-file (from-pathname to-stream &key (element-type 'character)
                                     (user-id "anonymous") (user-pw (user-mail-address)))
  "Copies the content of FROM-PATHNAME to TO-STREAM. 
If a network error is encountered, this returns NIL, otherwise T.
ELEMENT-TYPE is the ANSI file openning argument."
  (let ((host (pathname-host from-pathname)))
    (handler-case 
      (with-automatic-login (host user-id user-pw)
        (with-open-file (ftp-stream from-pathname :direction :input :element-type element-type)
          (http::stream-copy-until-eof ftp-stream to-stream)
          (values t)))
      ;; handle remote connection problems, including dead host, refused connection.
      (remote-network-error () nil))))

;; MJS 02Oct97: This is www-utils:char-bits, which doesn't have to be
;; cltl1:char-bits becase it was probably only needed to fix some bugs on the
;; LispM version.
(declaim (inline char-bits))

;; returns font or shift bits
;; not an issue if they are not stored in characters.
(defun char-bits (char)
  (declare (ignore char))
  0)

(declaim (inline string-thin))

;; removes fonts from Lispm Fat strings.-- JCMa 12/30/1994.
(defun string-thin (string)
  "Strips font description"
  string)

;;;------------------------------------------------------------------- 
;;;
;;; SECURE SUBNETS
;;;

;; move into portable code.-- JCMa 12/30/1994.
(define parse-internet-addresses (ip-addresses)
  "Parses IP-ADDRESSES into a list of ip-address specifications."
  (loop for ip-address in ip-addresses
        for parsed-address = (%parse-host-address ip-address)
        when parsed-address
          collect parsed-address)) 

(define ip-host-trusted-p (address secure-subnets &optional network)
  "Returns non-null if IP-address address is trusted given secure-subnets."
  (declare (ignore network))
  (flet ((address-match-p (addr1 addr2)
           (let ((diff (logxor addr1 addr2)))
             (macrolet ((masked-test (mask)
                          `(or (not (logtest ,mask addr2))
                               (not (logtest ,mask diff)))))
               (and (masked-test #xFF000000)
                    (masked-test #x00FF0000)
                    (masked-test #x0000FF00)
                    (masked-test #x000000FF))))))
    (declare (inline address-match-p))
    (cond (secure-subnets
           (member (etypecase address
                     (integer address)
                     (string (%parse-host-address address)))
                   secure-subnets
                   :test #'address-match-p))
          (t t))))

;;;------------------------------------------------------------------- 
;;;
;;; LOGGING EVENTS
;;; 

(define notify-log-window (format-string &rest format-args)
  "Top-level method for writing to the HTTP log window."
  (let ((stream *trace-output*))
    (fresh-line stream)
    (write-char #\[ stream)
    (http::write-standard-time (get-universal-time) stream)
    (write-string "]  " stream)
    (apply #'format stream format-string format-args)))

(define expose-log-window ()
  "Exposes the Log window. Does nothing."
  nil)

(define common-logfile-notify (server)
  "Issues a notification of server activity on a window."
  (http::write-common-logfile-entry server *trace-output*)
  (terpri *trace-output*)
  (finish-output *trace-output*))

(define log-http-server-error (format-string &rest format-args)
  (apply #'notify-log-window  format-string format-args))

(define http::log-http-request (client-host method url-string case)
  #|(log-http-access client-host
                   (if accepted-p "Serving ~A ~S" "Rejected ~A ~S") method url-string)|#
  (let ((host-name (http::host-domain-name client-host)))
    (ecase case
      (:accepted
        (notify-log-window "HTTP Serving ~A: Method ~S: ~S" host-name method url-string))
      (:rejected
        (notify-log-window "HTTP Rejected ~A: Method ~S: ~S" host-name method url-string))
      (:timeout
        (notify-log-window "HTTP Timed Out Serving ~A: Method ~S: ~S" host-name method url-string)))))

;; export the logging symbols
#+ignore
(export '(log-window notify-log-window expose-log-window common-logfile-notify) :www-utils)

;;;------------------------------------------------------------------- 
;;;
;;; MAIL SENDING  
;;;

;; need to interface to a mail sending program, e.g.eudora. -- JCMa 1/1/1995.
#+LispWorks3.2
(define report-bug  (to subject format-string &rest format-args)
  (notify-log-window (with-output-to-string
                       (stream)
                       (format  stream "~&Report Bug:  To:~A~
                                                       ~&Subject: ~A"
                                to subject)
                       (fresh-line stream)
                       (apply #'format stream format-string format-args))))


;;;------------------------------------------------------------------- 
;;;
;;; LOG RELATED PORTABILITY
;;;

(defun make-lock (name &key (type :simple))
  "Returns a lock named name that is suitable for use with with-lock-held.
TYPE can be either :SIMPLE (the default) or :MULTIPLE-READER-SINGLE-WRITER.
A simple lock queues all operations whereas a :MULTIPLE-READER-SINGLE-WRITER lock
allows multiple read threads but only a single writer thread."
  (clim-sys:make-lock name))

(defmacro with-lock-held ((lock &optional (mode :write) (whostate "Wait for Lock")) &body body)
  "Executes BODY with LOCK held in MODE, which is one of :READ or :WRITE."
  (declare (ignore mode))
  `(clim-sys:with-lock-held (,lock ,whostate)
     ,@body))

(defun %make-log-pathname (device directory name host)
  "Returns the pathname to which current log entries are written."
  (make-pathname
    :host host
    :device device
    :directory directory
    :name name
    :type "text"))


;;;------------------------------------------------------------------- 
;;;
;;; PERIODIC TIMERS
;;;

(defun create-timer-call (function arguments &key name)
  "Make a timer which will asynchronously call the given function with
arguments when the time expires."
  (mp:make-named-timer name
		       'make-process-and-run-function
		       name function arguments))

;; Portable?
#+clim-sys
(defun make-process-and-run-function (name function args)
  (clim-sys:make-process #'(lambda ()
			     (apply function args))
			 :name name))

(defun reset-timer-absolute (timer absolute-universal-time)
  "Reset the timer to expire at the given absolute-universal-time."
  (mp:schedule-timer-relative
   timer
   ;; Convert to seconds from time now
   (- absolute-universal-time
      (get-universal-time))))

(defvar *time-zone* nil
   "Time zone offset from GMT in hours.")

(define time-zone (&optional update-p)
  "Returns the timezone as an offset from GMT."
  (if (or update-p
	  (not *time-zone*))
      (setq *time-zone*
            (nth-value 8 (get-decoded-time)))
    *time-zone*))

;; Portable?
(defvar *daily-server-timer* nil)

;; Portable?
(defun daily-server-timer ()
  (or *daily-server-timer*
      (setq *daily-server-timer*
	    (create-timer-call 'run-daily-server-tasks '()
			       :name "Daily Server Tasks"))))

;; Portable?
(defun synchronize-daily-server-tasks ()
  (reset-timer-absolute (daily-server-timer)
			(next-3am-universal-time)))




;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

#|
genera process functions
  CLIM-SYS:PROCESS-WAIT-WITH-TIMEOUT - Function (WAIT-REASON TIMEOUT PREDICATE)
  CLIM-SYS:PROCESS-NAME - Function (PROCESS)
  CLIM-SYS:PROCESS-YIELD - Function ()
  CLIM-SYS:PROCESS-STATE - Function (PROCESS)
  CLIM-SYS:PROCESS-WAIT - Function (WAIT-REASON PREDICATE)
  CLIM-SYS:PROCESS-WHOSTATE - Function (PROCESS)
  CLIM-SYS:PROCESS-INTERRUPT - Function (PROCESS FUNCTION)
  CLIM-SYS:ENABLE-PROCESS - Function (PROCESS)
  CLIM-SYS:CURRENT-PROCESS - Function ()
  CLIM-SYS:DISABLE-PROCESS - Function (PROCESS)
  CLIM-SYS:ALL-PROCESSES - Function ()
  CLIM-SYS:MAKE-PROCESS - Function (FUNCTION &key :NAME)
  CLIM-SYS:RESTART-PROCESS - Function (PROCESS)
  CLIM-SYS:DESTROY-PROCESS - Function (PROCESS)

|#

(defun process-run-function (name function &rest args)
  (apply 'mp:process-run-function name nil function args))


(defun process-disable (process)
  (clim-sys:disable-process process))

(defun process-enable (process)
  (clim-sys:enable-process process))

(defun process-kill (process)
  (clim-sys:destroy-process process))

(defun process-reset (process)
  (clim-sys:restart-process process))

(defun current-process ()
  (clim-sys:current-process))

(export 'current-process :www-utils)

;; MJS 09Aug99: Don't use CLIM-SYS:MAKE-PROCESS because the one that
;; is shipped with CLIM doesn't take the extra args.
(define make-process (process-name &key (priority 0) quantum
				   run-reasons background-p 
				   restart-after-reset warm-boot-action &allow-other-keys)
  "Creates a process using a portable set of options."
  (declare (ignore priority quantum run-reasons background-p
                   warm-boot-action restart-after-reset))
  (mp:process-run-function process-name
                           ;; MJS 09Aug99: could pass priority is we knew what it meant.
                           (list)
                           (constantly nil)))

;; updated from acl/server/unix.lisp   7/14/96 -- JCMa.
(defun process-wait (wait-reason predicate &rest args)
  (declare (dynamic-extent args))
  (flet ((wait-function ()
	   (apply predicate args)))
    (declare (dynamic-extent #'wait-function))
    (clim-sys:process-wait wait-reason (if args #'wait-function predicate))))

(defun process-wait-with-timeout (wait-reason timeout predicate &rest args)
  (declare (dynamic-extent args))
  (flet ((wait-function ()
	   (apply predicate args)))
    (declare (dynamic-extent #'wait-function))
    (clim-sys:process-wait-with-timeout wait-reason timeout (if args #'wait-function predicate))))


(defun process-run-time (process)
  "Returns the amount of run time the process has accumulated, in microseconds."
  (* 1000000 (mp::process-time-slice process)) ;MJS 03Apr97: rather a lie, but better than nothing
  )

(defun process-idle-time (process)
  "Returns the amount of time since the process ran last, in sixtieths of a second."
  (declare (ignore process))
  60					;MJS 03Apr97: a complete lie
  )

;; MJS 13Oct99: could do process-priority is we knew what it meant.
(defun process-priority (process)
  (declare (ignore process))
  0)

(defun (setf process-priority) (value process)
  (declare (ignore process))
  value)



;;;------------------------------------------------------------------- 
;;;
;;; TIMERS AND TIME-BOUNDED EXECUTION
;;;

(defun make-timer (resource function)
  (declare (ignore resource function))
  (mp:make-named-timer 'cl-http-timer nil))

(defun initialize-timer (resource timer function)
  (declare (ignore resource))
  (setf (slot-value timer 'mp::function) function)
  timer)

(defun deinitialize-timer (resource timer)
  (declare (ignore resource))
  (setf (slot-value timer 'mp::function) nil)
  timer)

(defresource
  timer (function)
  :constructor make-timer
  :initializer initialize-timer
  :deinitializer deinitialize-timer
  :initial-copies 0)

(defun with-timeout-internal (timeout function error-p)
  (declare (dynamic-extent function))
  (let ((catch-tag nil)
        (process mp:*current-process*))
    (labels ((timeout-throw ()
               (throw catch-tag nil))
             (timeout-action ()
               (mp:process-interrupt process #'timeout-throw)))
      (declare (dynamic-extent #'timeout-throw #'timeout-action))
      (using-resource (timer timer #'timeout-action)
        (setq catch-tag timer)
        (catch timer
          (unwind-protect
              (progn
                (mp:schedule-timer-relative timer timeout)
                (return-from with-timeout-internal
                  (funcall function)))
            (mp:unschedule-timer timer)))
        (when error-p
          (error "Timeout: body took longer than ~d second(s) to complete."
		 timeout))))))

(defmacro with-timeout ((timeout &key error-p) &body body)
  "Executes BODY and returns the values of the last form in BODY. However, if
the execution takes longer than TIMEOUT seconds, abort it. If :ERROR-P is
unsupplied or false, just return nil. If :ERROR-P is true, signal an error."
  (let ((function (gensym "function")))
    `(flet ((,function () . ,body))
       (declare (dynamic-extent #',function))
       (with-timeout-internal ,timeout #',function ,error-p))))



;;;------------------------------------------------------------------- 
;;;
;;; ATOMIC CREATION OF CRLF FILES
;;;

(defmethod http:valid-crlf-cache-file-p ((pathname pathname))
  ;; Since creation uses an atomic rename-file operation, the crlf is valid if
  ;; it exists and is newer than the source.
  (let* ((source (probe-file pathname))
         (canonical (http:crlf-pathname source))
         (c-probe-date (file-write-date canonical)))
    (cond ((and source c-probe-date (< (file-write-date source) c-probe-date))
           (values t source (truename canonical)))
          (t (values nil source (probe-file canonical) canonical)))))

(defmethod http:ensure-crlf-canonical-file ((pathname pathname))
  (multiple-value-bind (valid-crlf-cache-p source-pathname crlf-pathname canonical-pathname)
      (http:valid-crlf-cache-file-p pathname)
    (cond (valid-crlf-cache-p crlf-pathname)
          ((atomic-crlf-canonicalize-file source-pathname canonical-pathname)
	   (values (http:crlf-pathname pathname) t))
	  (t (http:crlf-pathname pathname)))))

(defun atomic-crlf-canonicalize-file (source-pathname canonical-pathname)
  (let ((lock-file (make-pathname :type (concatenate 'simple-string
						     (pathname-type canonical-pathname)
						     "-lock")
				  :defaults canonical-pathname)))
    ;; Our caller saw an out-of-date crlf file, so repeatedly try to create it.
    (loop (let ((lock-stream nil))
	    (unwind-protect
		(progn
		  ;; Atomically probe and create the lock file if it doesn't
		  ;; exist.
		  (clim-sys:without-scheduling
		   (setq lock-stream
			 (open lock-file :direction :output
			       :if-exists nil
			       :if-does-not-exist :create)))
		  (when lock-stream
		    ;; We have the lock, so noone else can create the file now.
		    ;; If the file is valid then it has just been create by another process.
		    (when (http:valid-crlf-cache-file-p source-pathname)
		      (return))
		    (let* ((temp-file (make-pathname
				       :type (concatenate 'simple-string
							  (pathname-type canonical-pathname)
							  "-temp")
				       :defaults canonical-pathname)))
		      ;; Make the file in a temporary place.
		      (http:crlf-canonicalize-file source-pathname temp-file)
		      ;; Unix rename is atomic, so other processes will see either
		      ;; the old out-of-date file or the new one.
		      (rename-file temp-file canonical-pathname)
		      (return t))))
	      (when lock-stream
		(close lock-stream)
		(delete-file lock-file))))
	  (clim-sys:process-wait-with-timeout
	   "CRLF Wait"
	   1
	   #'(lambda () (not (probe-file lock-file)))))))

