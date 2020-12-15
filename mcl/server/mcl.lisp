;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: www-utils -*-

;;; (C) Copyright 1994-1998, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; MAC MCL COMPATABILITY CODE 
;;;
;;; Largely corresponds to http:lispm;server;lispm.lisp 
;;;
;;;------------------------------------------------------------------- 

(in-package :www-utils)

;; Don't call ccl::idle-update to avoid crashing.  This provides a workaround for
;; an MCL bug that can crash powerbooks.  If you find your powerbook falling asleep,
;; you will need to adjust the powerbook control panels.  -- JCMa 6/7/1995.
(setq ccl::*cpu-idle-p* nil)

; Advise the lisp environment that we have MAC-CL-HTTP loaded.
(pushnew :mac-cl-http *features*)
(pushnew :mcl-cl-http *features*)

#+:ccl-3 ;; Advise MAC-CL-HTTP  that the Lisp Environment is multithreaded
(pushnew :multi-threaded *features*) 

(eval-when (load eval compile)
   (mapc #'(lambda (x)
                   (shadowing-import x :www-utils)
                   (export x :www-utils))
             (list (intern "DEFRESOURCE" :resources)
                     (intern "CLEAR-RESOURCE" :resources)
                     (intern "ALLOCATE-RESOURCE" :resources)
                     (intern "DEALLOCATE-RESOURCE" :resources)
                     (intern "USING-RESOURCE" :resources)
                     (intern "MAP-RESOURCE" :resources)
                     (intern "GENERIC-FUNCTION-METHODS" :ccl)
                     (intern "METHOD-SPECIALIZERS" :ccl)
                     (intern "PROCESS-ACTIVE-P" :ccl)
                     (intern "PROCESS-DISABLE" :ccl)
                     (intern "PROCESS-ENABLE" :ccl)
                     (intern "PROCESS-KILL" :ccl)
                     (intern "PROCESS-PRIORITY" :ccl)
                     (intern "PROCESS-PRESET" :ccl)
                     (intern "PROCESS-RESET" :ccl)
                     (intern "PROCESS-RUN-FUNCTION" :ccl)
                     (intern "PROCESS-WAIT" :ccl)
                     (intern "PROCESS-WHOSTATE" :ccl)
                     (intern "*CURRENT-PROCESS*" :ccl)
                     (intern "CREATE-TIMER-CALL" :ccl)
           (intern "CLEAR-TIMER" :ccl)
           (intern "RESET-TIMER-ABSOLUTE" :ccl)
                      (intern "RESET-TIMER-RELATIVE" :ccl)
                      (intern "WITH-TIMEOUT" :ccl)
                      (intern "CHUNK-TRANSFER-ENCODING-MODE" :ccl)
                      (intern "NOTE-FIRST-CHUNK" :ccl)
                      (intern "NOTE-LAST-CHUNK" :ccl)
                      (intern "CHUNK-TRANSFER-DECODING-MODE" :ccl)
                      (intern "CHUNK-TRANSFER-DECODING-MODE-END" :ccl) 
                      (intern "CHUNK-TRANSFER-CONTENT-LENGTH" :ccl)
                      (intern "CHUNK-TRANSFER-CONTENT-LENGTH-HEADER" :ccl)
                      (intern "END-OF-CHUNK-TRANSFER-DECODING" :ccl)
                      (intern "CLASS-DIRECT-SUPERCLASSES" :ccl))))

;; a series of network conditions that we would like to be able to within
;; portable code.
(eval-when (load eval compile)
  (mapc #'(lambda (sym)
            (let ((sym (intern sym :ccl)))
              (import sym :www-utils)
              (export sym :www-utils)))
        '("BAD-CONNECTION-STATE"
          "CONNECTION-CLOSED"
          "CONNECTION-ERROR"
          "CONNECTION-LOST"
          "CONNECTION-REFUSED"
          "DOMAIN-RESOLVER-ERROR"
          "HOST-NOT-RESPONDING"
          "HOST-STOPPED-RESPONDING"
          "LOCAL-NETWORK-ERROR"
          "NETWORK-ERROR"
          "NETWORK-PARSE-ERROR"
          "NETWORK-RESOURCES-EXHAUSTED"
          "PROTOCOL-TIMEOUT"
          "REMOTE-NETWORK-ERROR"
          "UNKNOWN-ADDRESS"
          "UNKNOWN-HOST-NAME"))) 

;;;------------------------------------------------------------------- 
;;;
;;;  ANSI COMPLIANCE ISSUES
;;; 

#-ccl-4.3
(declaim (inline special-operator-p))

#-ccl-4.3
(defun special-operator-p (symbol)
  (ccl::special-form-p symbol)) 

#-ccl-4.3
(defun array-displacement (array)
   (ccl::displaced-array-p array))
#-ccl-4.3
(export '(special-operator-p array-displacement) :www-utils) 

;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(define-condition network-error-mixin
                  (network-error)
  ()
  (:documentation "Mixin to allow ports to inherit instance variables and methods to network conditions
defined at the portable code level."))


(declaim (inline report-condition))

(define report-condition (condition stream)
  "Prints the report string for CONDITION onto STREAM."
  (ccl::report-condition condition stream))

(declaim (inline report-string))

(define report-string (condition)
  "Returns the report string for CONDITION."
  (with-output-to-string (stream)
    (report-condition condition stream)))

;; Define equivalence mapping to the MCL case.
(deftype file-not-found () 
  "Specialization of Common Lisp File-error in which the file was not found on open."
  '(and condition file-error))

(export 'file-not-found :www-utils) 

(define-macro with-atomic-execution (&body body)
  `(ccl:without-interrupts ,@body))

(define-macro atomic-incf (reference &optional (delta 1))
  "Atomically increments REFERENCE by DELTA."
  `(with-atomic-execution
     (incf ,reference ,delta)))

(define-macro atomic-decf (reference &optional (delta 1))
  "Atomically decrements REFERENCE by DELTA."
  `(with-atomic-execution
     (decf ,reference ,delta)))

(define-macro atomic-push (item reference)
  "Atomically pushes ITEM onto REFERENCE."
  `(with-atomic-execution
     (push ,item ,reference)))

(define-macro atomic-pop (reference)
  "Atomically pops an item off REFERENCE."
  `(with-atomic-execution
     (pop ,reference)))

(define-macro atomic-setf (accessor &rest values)
  "Like SETF except performs the action atomically."
  `(with-atomic-execution
     (setf ,accessor ,@values)))

(define-macro atomic-replacef (reference new-value)
  `(with-atomic-execution
     (setf ,reference ,new-value)))

(define-macro atomic-conditional-replacef (reference predicate new-value)
  "When PREDICATE returns non-null, this setfs REFERENCE to NEW-VALUE.
Predicate is called (OLD-VALUE NEW-VALUE). The operation 
assures that precicate applicaiton and swap are atomic."
  (declare (values old-value value-replaced-p))
  (let ((old-value (gensym))
        (new-value-var (gensym))
        (pred (gensym)))
    `(with-atomic-execution
       (let ((,old-value ,reference)
             (,new-value-var ,new-value)
             (,pred ,predicate))
         (when (funcall ,pred ,old-value ,new-value-var)
           (setf ,reference ,new-value-var)
           (values ,old-value t))))))

(declaim (inline arglist))

(defun arglist (function)
  "Returns the arglist for FUNCTION."
  (declare (values (arglist values type arglist-types value-types)))
  (ccl:arglist function)) 

;;;------------------------------------------------------------------- 
;;;
;;; OBTAINING THE DOMAIN NAME FOR AN IP ADDRESS
;;;

#||
(defun ip-address-string-p (string &aux (count 0))
  "Returns non-null if string is a well-form IP address string."
  (flet ((good-char-p (char)
           (or (digit-char-p char)
               (and (char= char #\.) (incf count)))))
    (declare (inline good-char-p))
    (and (stringp string)
         (loop for idx upfrom 0 below (length string)
               unless (good-char-p (aref string idx))
                 do (return-from ip-address-string-p nil)
               finally (return-from ip-address-string-p (eql count 3))))))

(defun %parse-host-address (address)
  (cond (http:*resolve-ip-addresses* (ccl::tcp-host-address address))
        ((integerp address) address)
        ((ip-address-string-p address)
         )
        ( (error  'ccl:tcp-unknown-domain-name 
                  "Can't resolve unknown domain names with domain resolution turned off.~
                            Please use IP addresses only or turn domain resolution on."))))||#

(declaim (inline %parse-host-address))

(defun %parse-host-address (address)
  "Returns an IP-NUMBER which is integer denoting the address of host."
  (declare (values ip-number))
  (typecase address
    (integer address)
    (string
      (cond ((equalp address "localhost") (local-host)) ; RFC 1738 says defines this string   5/3/96 -- JCMa.
            (t (ccl::tcp-host-address address))))))

(declaim (inline ip-address-for-parsed-ip-address))

(define ip-address-for-parsed-ip-address (ip-number)
  "Returns an IP address as a string from, IP-NUMBER, the parsed address."
  (ccl::tcp-addr-to-str ip-number)) 

(define domain-name-for-parsed-ip-address (ip-number &optional (no-error-p t))
  "Given the parsed IP address (an integer), IP-NUMBER, this returns the domain name or NIL.
When no-error-p is T, this returns the IP Address string when a domain error occurs."
  (check-type ip-number integer)
  (cond ;; Return cached value when available. 
    ;; Faster and used for standalone operation.
    ((and (eql ip-number (local-host-parsed-ip-address))
          http:*local-host-domain-name*)) 
    ;; DNS off case returns IP addresses only
    ((or (null http:*resolve-ip-addresses*) (zerop ip-number)) 
     (ip-address-for-parsed-ip-address ip-number))
    (t (%domain-name-for-parsed-ip-address ip-number no-error-p))))
        
(declaim (inline domain-name-for-ip-address))

(define domain-name-for-ip-address (address &optional (no-error-p t))
  "Given the IP address, ADDRESS, this returns the domain name or NIL."
  (domain-name-for-parsed-ip-address (%parse-host-address address) no-error-p))

(declaim (inline ip-address-for-host-domain-name))

(define ip-address-for-host-domain-name (domain-name)
  "Returns the IP address string for domain-name."
  (ip-address-for-parsed-ip-address (%parse-host-address domain-name))) 

(declaim (notinline %local-host-parsed-ip-number))

(defun %local-host-parsed-ip-number ()
  (handler-case
    (ccl::%tcp-getaddr)
    (ccl:domain-error () 0)))

(defparameter *local-host*  nil
  "The local host object, which is a parsed IP number on the MAC." )

(define local-host (&optional recache-p)
  "The host object for the local host on which we are running."
  (cond ((and (not recache-p) *local-host*))
        (t (setq *local-host* (%local-host-parsed-ip-number)))))

(define local-host-ip-address (&optional recache-p)
   "Returns the IP address of the local host."
   (cond ((and (not recache-p) http::*local-host-ip-address*))
            (t (setq http:*local-host-ip-address* (ip-address-for-parsed-ip-address 
                                                                        (%local-host-parsed-ip-number))))))

(define local-host-parsed-ip-address (&optional recache-p)
  "Returns the parsed IP address of the local host."
  (cond ((and (not recache-p) http:*local-host-address*))
        (t (setq http:*local-host-address* (%local-host-parsed-ip-number)))))

(defun %domain-name-for-parsed-ip-address (ip-number &optional (no-error-p t))
  (flet ((tcp-host-cname (ip-number)
           (let* ((name (ccl::tcp-host-cname ip-number))
                  (last-elt (1- (the fixnum (length name)))))
             (if (eql (aref name last-elt) #\.) ; strip trailing . to clean up Mac bug.
                 (subseq name 0 last-elt)
                 name))))
    (declare (inline tcp-host-cname)) 
    (if no-error-p
        ;; handle domain errors
        (handler-case
          (tcp-host-cname ip-number)
          ;; formerly tcp-domain-server-not-found
          (ccl:domain-error () (ip-address-for-parsed-ip-address ip-number)))
        ;; no local condition handling
        (tcp-host-cname ip-number))))

(define %local-host-domain-name ()
  (let ((ip-number (%local-host-parsed-ip-number)))
    (if (zerop ip-number)
        ;; zerop means no network connection and no DNS
        (ip-address-for-parsed-ip-address ip-number)
        ;; normal case
        ;; When a mac is running with only an IP address and no DNS entry,
        ;; it will get an error here.  Using the no error argument allows it
        ;; to continue operating with just it's IP address. -- JCMa 8/16/96
        (%domain-name-for-parsed-ip-address ip-number t))))

(defun local-host-domain-name (&optional recache-p)
  "Returns the local host domain name."
  (cond ((and (not recache-p) http::*local-host-domain-name*))
        (t (setq http::*local-host-domain-name* (or http:*http-host-name* (%local-host-domain-name)))))) 

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
           (ccl:network-error () nil)))
        (t (%parse-host-address address))))

(declaim (inline host-mail-name))

(define host-mail-name (host)
  "The internet mail name for HOST."
  (domain-name-for-ip-address host t))

(define host-eq (host1 host2)
   "Returns non-null if HOST1 is equal to HOST2."
   (cond ((and (integerp host1) (integerp host2))
              (= host1 host2))
            ((or (null host1) (null host2)) nil)
            (t (eql (%parse-host-address host1)
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

#-:ccl-3
(define local-port (http-stream)
  "Returns the local host port for the remote connection via http-stream."
  (let* ((conn (slot-value http-stream 'ccl::conn))
         (pb (when conn (ccl::conn-pb conn))))
    (when pb
      (ccl:rref pb tcpiopb.status.localport))))

#+:ccl-3
(declaim (inline local-port))

#+:ccl-3
(define local-port (http-stream)
  "Returns the local host port for the remote connection via http-stream."
  (ccl::stream-local-port http-stream))

#-:ccl-3
(define foreign-port (http-stream)
  "Returns the foreign host port for the remote connection via http-stream."
  (let* ((conn (slot-value http-stream 'ccl::conn))
         (pb (when conn (ccl::conn-pb conn))))
    (when pb
      (ccl:rref pb tcpiopb.status.remoteport))))

#+:ccl-3
(declaim (inline foreign-port))

#+:ccl-3
(define foreign-port (http-stream)
  "Returns the foreign host port for the remote connection via http-stream."
  (ccl::stream-remote-port http-stream))

#-:ccl-3
(define foreign-host (http-stream)
  "Returns the foreign host for the remote connection via http-stream."
  (let* ((conn (slot-value http-stream 'ccl::conn))
         (pb (when conn (ccl::conn-pb conn))))
    (when pb
      (ccl:rref pb tcpiopb.status.remotehost))))

#+:ccl-3
(declaim (inline foreign-host))

#+:ccl-3
(define foreign-host (http-stream)
  "Returns the foreign host for the remote connection via http-stream."
  (ccl::stream-remote-host http-stream)) 

;;;------------------------------------------------------------------- 
;;;
;;; FILE RELATED OPERATIONS
;;; 

(declaim (inline file-stream-creation-date))

(define file-stream-creation-date (file-stream)
  "Returns the creation date in universal time for FILE-STREAM's source file."
  (file-write-date file-stream))

(declaim (inline file-stream-modification-date))

(define file-stream-modification-date (file-stream)
  "Returns the modification date in universal time for FILE-STREAM's source file."
  (file-write-date file-stream))

(declaim (inline file-stream-pathname))

(define file-stream-pathname (file-stream)
  "Returns the pathname associated with file-stream."
  (ccl::stream-filename file-stream))

(proclaim '(inline file-stream-length-in-bytes))

(define file-stream-length-in-bytes (file-stream)
  "Returns the length in bytes for FILE-STREAM's source file."
  (file-length file-stream))

(defgeneric file-length-in-bytes (pathname-or-url &optional new-length)
   (:documentation "Returns the number of bytes in PATHNAME-OR-URL or NIL."))

#-:CCL-3
(defmethod file-length-in-bytes ((pathname pathname) &optional new-length)
  (declare (ignore new-length))
  (with-open-file (file-stream pathname)
    (file-length file-stream)))

#+:CCL-3
(defmethod file-length-in-bytes ((pathname pathname) &optional new-length)
   (declare (ignore new-length))
  (ccl::file-data-size pathname))

; ; These are used to provide the last modification date.  It would be better to use
;; FILE-MODIFICATION-DATE as it is more intuitive. -- JCMa 7/3/1997.
#-:CCL-3
(defmethod file-creation-date ((pathname pathname))
  (with-open-file (file-stream pathname)
    (file-create-date file-stream)))

#+:CCL-3
(defmethod file-creation-date ((pathname pathname))
  (ccl::file-create-date pathname))

#-:CCL-3
(defmethod file-modification-date ((pathname pathname))
  (with-open-file (file-stream pathname)
    (file-write-date file-stream))) 

#+:CCL-3
(defmethod file-modification-date ((pathname pathname))
  (ccl::file-write-date pathname))

(declaim (inline file-stream-version))

(defun file-stream-version (file-stream)
  (file-stream-creation-date file-stream))

(declaim (inline file-version))

(defun file-version  (pathname)
  (when (probe-file pathname)
    (file-modification-date pathname)))

#-:CCC-3
(define file-properties (pathname)
  "Returns the length in bytes  and the creation in in universal time 
for FILE-STREAM's source file."
  (declare (values length-in-bytes creation-date version))
  (with-open-file (file-stream pathname)
    (values (file-stream-length-in-bytes file-stream)
            (file-stream-creation-date file-stream)
            (file-stream-version file-stream))))

#+:CCL-3
(declaim (inline file-properties))

#+:CCL-3
(defun file-properties (pathname)
   "Returns the length in bytes  and the creation in in universal time 
for FILE-STREAM's source file."
   (declare (values length-in-bytes creation-date version))
   (values (if (ccl:directoryp pathname)
                  0                                     ;alternatively, might want to sum the file lengths
                  (ccl::file-data-size pathname))
               (ccl::file-write-date pathname)          ; last modification date
               (file-version pathname)))

(declaim (inline pathname-directory-p))

(defun pathname-directory-p (pathname)
  "Returns non-null if PATHNAME¬denotes a directory."
  (ccl::directoryp pathname)) 

(defun directory-list (pathname &rest options)
   "Returns a lisp Machine style directory listing."
   (let ((pathnames (directory (merge-pathnames pathname "*.*")
                                              :files t
                                              :resolve-aliases t
                                              :directories t)))
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

(define directory-info (pathname &key (name :wild) (type :wild) (version :newest) (sort-pathnames t)
                                 directories)
  "Returns a poperty list of information for every file in the directory PATHNAME
that matches pathnames wildcards. Directories are included when directories is non-null."
  (declare (notinline))
  (flet ((get-directory-listing (p &optional (sort-p sort-pathnames))
           (let ((args nil))
             (declare (dynamic-extent args))
             (when sort-p (push :sorted args))
             (when directories (push :directories args))
             (apply #'directory-list p :no-extra-info args)))
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
             (and (string<  (pathname-name p1) (pathname-name p2))
                  (let ((t1 (pathname-type p1))
                        (t2 (pathname-type p2)))
                    (cond ((and t1 t2)
                           (string< t1 t2))
                          (t1 nil)
                          (t t)))))))
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

(defun directory-list* (pathname predicate &rest options)
  "Accepts the options :FILES :DIRECTORIES :SORTED :PROPERTIES."
  (let ((pathnames (directory (merge-pathnames pathname "*.*")
                              :files (member :files options)
                              :test predicate
                              :resolve-aliases t
                              :directories (member :directories options))))
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
  (let ((p (http::translated-pathname pathname)))
    (ccl:create-directory (make-pathname 
                            :device (pathname-device p)
                            :directory (pathname-directory p))
                          :if-exists :error)))

(defgeneric probe-directory (pathname)
  (:documentation "Returns non-null if the directory pathname exists."))

(defmethod probe-directory ((pathname pathname))
  (ccl::probe-file (make-pathname :host (pathname-host pathname)
                                  :device (pathname-device pathname)
                                  :directory (pathname-directory pathname))))

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
      (ccl:remote-network-error () nil))))

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
      (ccl:remote-network-error () nil)))) 

(declaim (inline char-bits))

;; returns font or shift bits
;; not an issue in MCL as they are not stored in characters.
(defun char-bits (char)
  (declare (ignore char))
  0)

;; removes styles from strings
(defun string-thin (string)
   "Strips font description"
   (case (array-element-type string)
      (character
        (let* ((len (length string))
                  (new (make-string len :element-type 'base-character)))
           (ccl::move-string-bytes string new 0 0 len)
           new))
      (t string))) 

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

(define-macro %ensure-parsed-address (addr)
   `(etypecase ,addr
       (integer ,addr)
       (string (%parse-host-address ,addr))))

; recognize wildcards in secure-subnets (e.g., 128.52.0.0)   10/22/96 -- slh. 
(define ip-address-match-p (ip-address1 ip-address2 &optional subnet-mask)
  "Returns non-null if IP-ADDRESS1 is the same as IP-ADDRESS2 or is on the same subnet as IP-ADDRESS2.
IP-ADDRESS1 and IP-ADDRESS2 can be either IP address strings or parsed IP numbers."
  (declare (ignore subnet-mask))
  (macrolet ((field-mask (mask number)
               `(if (zerop ,(if (typep mask 'fixnum)
                                `(the fixnum (logand ,mask ,number))
                                `(logand ,mask ,number)))
                    ,mask 0)))
    (let ((addr1 (%ensure-parsed-address ip-address1))
          (addr2 (%ensure-parsed-address ip-address2))) 
      (declare (dynamic-extent addr1 addr2))
      (zerop (logand (logxor addr1 addr2)
                     (lognot (logior (field-mask #xFF000000 addr2)
                                     (field-mask #x00FF0000 addr2)
                                     (field-mask #x0000FF00 addr2)
                                     (field-mask #x000000FF addr2))))))))

(define ip-host-trusted-p (address secure-subnets &optional network)
   "Returns non-null if IP-address address is trusted given secure-subnets."
   (declare (ignore network))
   (if secure-subnets
      (let ((addr (%ensure-parsed-address address)))
         (declare (dynamic-extent addr))
         (member addr secure-subnets :test #'ip-address-match-p))
      t))

;;;------------------------------------------------------------------- 
;;;
;;; STREAM HACKING
;;;

(define-macro with-binary-stream ((stream direction) &body body)
  "Turns text STREAM into a binary stream within the scope of BODY.
direction can be :OUTPUT, :INPUT, or :BOTH."
  `(unwind-protect
       (progn ,(ecase direction
                 (:output `(ccl:binary-output-mode ,stream))
                 (:input `(ccl:binary-input-mode ,stream))
                 (:both `(progn (ccl:binary-output-mode ,stream)
                                (ccl:binary-input-mode ,stream))))
              ,@body)
     ,(ecase direction
        (:output `(ccl:ascii-output-mode ,stream))
        (:input `(ccl:ascii-input-mode ,stream))
        (:both `(progn (ccl:ascii-output-mode ,stream)
                       (ccl:ascii-input-mode ,stream))))))

(define-macro with-text-stream ((stream direction) &body body)
   "Turns a binary STREAM into a text stream within the scope of BODY.
direction can be :OUTPUT, :INPUT, or :BOTH."
   `(unwind-protect
       (progn ,(ecase direction
                      (:output `(ccl:ascii-output-mode ,stream))
                      (:input `(ccl:ascii-input-mode ,stream))
                      (:both `(progn (ccl:ascii-output-mode ,stream)
                                            (ccl:ascii-input-mode ,stream))))
                  ,@body)
       ,(ecase direction
            (:output `(ccl:binary-output-mode ,stream))
            (:input `(ccl:binary-input-mode ,stream))
            (:both `(progn (ccl:binary-output-mode ,stream)
                                  (ccl:binary-input-mode ,stream))))))

;;;------------------------------------------------------------------- 
;;;
;;; ABORTING CONNECTIONS
;;; 

(declaim (inline live-connection-p))

#-:ccl-3
(define live-connection-p (http-stream)
  "Returns non-null if the TCP/IP connection over HTTP-STREAM remains alive
in that the remote host continue to respond at the TCP/IP level."
  (if (slot-value http-stream 'ccl::conn) t nil))

#+:ccl-3
(define live-connection-p (http-stream)
  "Returns non-null if the TCP/IP connection over HTTP-STREAM remains alive
in that the remote host continue to respond at the TCP/IP level."
  (eql :established (ccl::stream-connection-state-name http-stream)))

(declaim (inline abort-http-stream))

(define abort-http-stream (http-stream)
  "Closes http-stream in abort mode.  
This will push any output in the transmit buffer and catch any network errors.
Takes care to clean up any dangling pointers."
  (handler-case 
    (close http-stream :abort t)
    (ccl:network-error ())))

(export 'abort-http-stream :www-utils)

;; these definitions should be moved into the shared code -- JCMa 12/30/1994.
(define abort-current-connection ()
  "Aborts the computation associated with the current HTTP connection."
  (signal 'http-abort))

(declaim (inline abort-if-connection-dead))

(define abort-if-connection-dead (http-stream)
   "Aborts the HTTP connection if the TCP/IP connection over HTTP-STREAM
has died, i.e. the remote host is no longer connected."
   (unless (live-connection-p http-stream)
      (abort-current-connection))) 

;;;------------------------------------------------------------------- 
;;;
;;; 
;;; 

(declaim (inline bytes-transmitted))

#-ccl-3
(define bytes-transmitted (stream)
  "Returns the number of bytes transmitted over STREAM."
  (ccl:bytes-transmitted stream))

#-ccl-3
(defun set-bytes-transmitted (stream value)
  (declare (ignore stream value)))

#+ccl-3
(define bytes-transmitted (stream)
  "Returns the number of bytes transmitted over STREAM."
  (ccl::stream-bytes-transmitted stream))

#+ccl-3
(declaim (inline set-bytes-transmitted))

#+ccl-3
(defun set-bytes-transmitted (stream value)
  (setf (ccl::stream-bytes-transmitted stream) value)) 

(defsetf bytes-transmitted set-bytes-transmitted) 

(export 'bytes-transmitted :www-utils) 

(declaim (inline bytes-received))

#-ccl-3
(define bytes-received (stream)
  "Returns the number of bytes received over STREAM."
  (declare (ignore stream))
  0)

#-ccl-3
(defun set-bytes-received (stream value)
  (declare (ignore stream value)))

#+ccl-3
(define bytes-received (stream)
  "Returns the number of bytes received over STREAM."
  (ccl::stream-bytes-received stream))

#+ccl-3
(declaim (inline set-bytes-received))

#+ccl-3
(defun set-bytes-received (stream value)
  (setf (ccl::stream-bytes-received stream) value)) 

(defsetf bytes-received set-bytes-received) 

(export 'bytes-received :www-utils)


;;;------------------------------------------------------------------- 
;;;
;;; MAPPING SERVERS
;;;

(defun map-http-servers (function)
  "Maps FUNCTION over all active HTTP servers."
  (flet ((fctn (server in-use-p resource)
           (declare (ignore resource))
           (when in-use-p
             (funcall function server))))
    (declare (dynamic-extent #'fctn))
    (map-resource 'http::http-server #'fctn)))

(defun all-servers (&aux servers)
  "Returns all active servers."
  (flet ((collect (server)
           (push server servers)))
    (declare (dynamic-extent #'collect))
    (map-http-servers #'collect)
    servers)) 

;;;------------------------------------------------------------------- 
;;;
;;; TIME RELATED 
;;;

(in-package :ccl)
(defun www-utils::fixnum-microsecond-time ()
  (without-interrupts
    (rlet ((time :unsignedwide))
          (#_Microseconds time)
          (+ (%get-unsigned-word time 6)
             (the fixnum (ash (the fixnum (%get-unsigned-byte time 5)) 16))))))
(in-package :www-utils)

(export 'fixnum-microsecond-time :www-utils)

(declaim (fixnum *time-zone*))

(define-parameter *time-zone* (ccl::get-time-zone)
   "Time zone offset from GMT in hours.")

(define time-zone (&optional update-p)
  "Returns the timezone as an offset from GMT."
  (cond (update-p (setq *time-zone* (ccl::get-time-zone)))
        (*time-zone*)
        (t (setq *time-zone* (ccl::get-time-zone)))))

(defvar *daily-server-timer* nil)

(defun daily-server-timer ()
  (or *daily-server-timer*
      (setq *daily-server-timer* (create-timer-call 'www-utils:run-daily-server-tasks '()
                                                        :name "Daily Server Tasks"))))
;; set up a timer for mighnight to start the 24 interval timer
(defun synchronize-daily-server-tasks ()
  (reset-timer-absolute (daily-server-timer) (next-3am-universal-time)))

(defun clear-daily-server-timer ()
  (when *daily-server-timer*
    (clear-timer *daily-server-timer*)
    ;; explicit call required to clear timer in MCL 3.0
    ;; #-ppc-target remove conditional so processes are killed in 4.0 too   1/18/97 -- JCMa.
    (ccl::terminate *daily-server-timer*)
    (setq *daily-server-timer* nil)))

(pushnew 'clear-daily-server-timer ccl::*lisp-cleanup-functions*) 

(pushnew 'synchronize-daily-server-tasks ccl:*lisp-startup-functions*) 

;;;------------------------------------------------------------------- 
;;;
;;; CLEAN UP HANGING HTTP PROCESSES
;;;

(define http-server-process-p (process)
  "Returns non-null when process is an HTTP server process."
  (string-equal "HTTP server" (ccl:process-name process) :start1 0 :start2 0 :end1 11 :end2 11))

(define find-idle-http-processes (&optional (idle-time http:*server-timeout*))
  "Returns all HTTP server processes that have been idle for more than IDLE-TIME (sixtieths of a second)."
  (loop for process in ccl:*all-processes*
        when (and (http-server-process-p process)
                  (< idle-time (process-idle-time process)))
          collect process))

(define map-idle-http-processes (function &optional (predicate #'identity))
  "Returns all HTTP server processes that have been idle for more than IDLE-TIME (sixtieths of a second)."
  (loop for process in ccl:*all-processes*
        when (and (http-server-process-p process)
                  (funcall predicate process))
          do (funcall function process)))

(defvar *idle-http-process-scavenger-timer* nil)

(defun idle-http-process-scavenger-timer ()
  (or *idle-http-process-scavenger-timer*
      (setq *idle-http-process-scavenger-timer* (create-timer-call 'scavenge-idle-http-processes '()
                                                                           :name "Scavenge Idle HTTP Server Processes"))))

(defun clear-idle-http-process-scavenger-timer ()
   (when *idle-http-process-scavenger-timer*
       (clear-timer *idle-http-process-scavenger-timer*)
       ;; explicit call required to clear timer in MCL 3.0
       ;; #-ppc-target remove conditional so processes are killed in 4.0 too   1/18/97 -- JCMa.
       (ccl::terminate *idle-http-process-scavenger-timer*)
       (setq *idle-http-process-scavenger-timer* nil)))

;; set up a timer to run the scavenger every server timout interval
(defun synchronize-idle-http-process-scavenger ()
  (reset-timer-absolute (idle-http-process-scavenger-timer)
			(+ (get-universal-time) (min (ceiling http:*server-life-time* 1000.)
						     (ceiling http:*server-timeout* 60.)))))

(defparameter *safe-server-abort-states* '("TCP In" "TCP Out" "TCP Finish" "TCP Closing" "TCP Accept")
  "Process whostates from which is is safe to abort HTTP connnections.")

(defun server-safe-to-abort-p (server)
  "Returns non-null when it is safe to abort the HTTP connection for SERVER."
  (let ((process (http::server-process server)))
    (and process
	 (or (null *safe-server-abort-states*)
	     (member (ccl:process-whostate process) *safe-server-abort-states* :test #'equalp))))) 

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
                   (let ((idle-time (idle-time server)))
                      (format nil "~&Forcibly Aborting ~A
                          ~&~5TState: ~A~&Reason: ~A
                          ~:[~;~&~10TIdle Time: ~:*~A~]~
                                         ~:[~;~&~10TURL: ~:*~A~]"
                                   server (ccl:process-whostate process) reason-to-die
		                   (and idle-time (write-time-interval idle-time nil))  (http::server-url-string server))))
                (kill-http-server (server reason-to-die)
                   (let ((process (http::server-process server)))
                      (when process                    ;beware of fencepost error
		          (prog1 (report-connection-abort server process reason-to-die)
			     (ccl:process-interrupt process #'http::abort-connection server)))))
                (maybe-kill-http-server (server &aux reason-to-die report)
	            (ccl::without-interrupts	;Don't allow process to change state lispm uses PROCESS::WITH-NO-OTHER-PROCESSES
                      (when (and (setq reason-to-die (reason-to-die server))
                                        (server-safe-to-abort-p server))
                          (setq report (kill-http-server server reason-to-die))))
	            (when report
	                (atomic-incf *idle-connections-scavenged*)
	                (when http::*report-idle-connection-scavenges*
		            ;; (tv:notify nil report)                ;lose notifications as they just cause trouble
		            (http:report-bug http:*bug-http-server* "HTTP Connection Forcibly Aborted" report)))))
      (declare (inline reason-to-die))
      (unless http:*debug-server*
         (map-http-servers #'maybe-kill-http-server))
      (synchronize-idle-http-process-scavenger))) 

(pushnew 'clear-idle-http-process-scavenger-timer ccl::*lisp-cleanup-functions*) 

(pushnew 'synchronize-idle-http-process-scavenger ccl:*lisp-startup-functions*) 

;; Main initialization runs on cold boot.
#+ignore
(pushnew 'http::run-server-initializations ccl:*lisp-startup-functions*) 

;;;------------------------------------------------------------------- 
;;;
;;; MAIL SENDING  
;;;

;;; In MCL 3.0, http:mac;server;mail.lisp implements these for real
;;; based on the simple mailer in http:mac;server;smtp.lisp

#-:ccl-3
(defun send-mail-from (from to subject mail-writer &key reply-to keywords comments
                            file-references)
  (declare (ignore reply-to keywords comments file-references))
  (notify-log-window
    (with-output-to-string (stream)
      (format stream "~&~A~&From: ~A~&To: ~A~&Subject: ~A~2%"
              case from to subject)
      (write-message mail-writer stream))))

#-:ccl-3
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
;;; MCL specific initializations 
;;;

(add-initialization "Maybe Enable EGC" '(ccl:egc  (ccl::egc-mmu-support-available-p))) 

;;;------------------------------------------------------------------- 
;;;
;;; LOG RELATED PORTABILITY
;;;

#-:ccl-3
(defun make-lock (name &key type)
  (declare (ignore name type))
  nil)

#+ccl-3
(defun make-lock (name &key type)
  "Returns a lock named NAME that is suitable for use with WITH-LOCK-HELD.
TYPE is one of :SIMPLE or :MULTIPLE-READER-SINGLE-WRITER.
In the later case, multiple readers can access lock protected data simultaneously."
  #-:multiple-reader-locks (declare (ignore name type))
  #-:multiple-reader-locks (ccl:make-lock)
  #+:multiple-reader-locks (ccl:make-lock name type))

#-ccl-3
(defmacro with-lock-held ((lock &optional mode whostate) &body body)
  "Executes BODY with LOCK held in MODE, which is one of :READ or :WRITE."
  (declare (ignore lock mode whostate))
  `(ccl:without-interrupts . ,body))

#+:ccl-3
(defmacro with-lock-held ((lock &optional (mode :write) (whostate "Wait for Lock")) &body body)
   "Executes BODY with LOCK held in MODE, which is one of :READ or :WRITE."
   #-:multiple-reader-locks (declare (ignore mode))
   #-:multiple-reader-locks
   `(ccl:with-lock-grabbed 
       (,lock ccl::*current-process* ,whostate)
       . ,body)
   #+:multiple-reader-locks
   `(ccl::with-lock (,lock :mode ,mode :lock-value ccl::*current-process* :whostate ,whostate)
       . ,body)) 

(defun %make-log-pathname (device directory name &optional machine-name)
  "Returns the pathname to which current log entries are written."
  (declare (ignore machine-name))
  (make-pathname
    :device device
    :directory directory
    :name name
    :type "text")) 

(define make-process (process-name &key (type :simple) (priority 0) (quantum ccl:*default-quantum*)
                                   run-reasons background-p 
                                   restart-after-reset warm-boot-action &allow-other-keys)
  "Creates a process using a portable set of options.
TYPE can be either :SIMPLE (the default) or :MULTIPLE-READER-SINGLE-WRITER.
A simple lock queues all operations whereas a :MULTIPLE-READER-SINGLE-WRITER lock
allows multiple read threads but only a single writer thread.
MCL does not implement :MULTIPLE-READER-SINGLE-WRITER as of version 4.2."
  (declare (ignore warm-boot-action restart-after-reset type))
  (ccl::make-process process-name
                     :priority priority
                     :quantum quantum
                     :run-reasons run-reasons
                     :background-p background-p))

(export 'make-process :www-utils)

(declaim (inline process-wait-with-timeout))

;; MCL uses 60ths of a second rather than seconds for TIME
(define process-wait-with-timeout (whostate time function &rest args)
  "Returns when FUNCTION with ARGS returns non-null or after TIME seconds.
TIME must be a fixnum."
  (apply #'ccl::process-wait-with-timeout whostate (round time 1/60) function args))

(export 'process-wait-with-timeout :www-utils)

(declaim (inline process-poll-with-timeout))

;; MCL uses 60ths of a second rather than seconds for TIME
(define process-poll-with-timeout (whostate time function &rest args)
  "Returns when FUNCTION with ARGS returns non-null or after TIME seconds.
TIME must be a fixnum."
  (apply #'ccl::process-poll-with-timeout whostate (round time 1/60) function args))

(export 'process-poll-with-timeout :www-utils) 

#|
;; Use mcl process-poll because process-wait is too slow.
(defun process-wait (whostate function &rest args)
  (declare (dynamic-extent args))
  (apply #'ccl::process-poll whostate function args))

(export 'process-wait :www-utils)

|#

(declaim (inline current-process))

(defun current-process ()
  "Returns the current process."
  ccl::*current-process*)

(export 'current-process :www-utils)

#+CCL-3
;; It would be nice if MCL were to provide real micro second time. 3/24/97 -- JCMa.
(defun process-run-time (process)
  "Returns the amount of run time the process has accumulated, in microseconds."
  (* (the fixnum #.(truncate 1000000 60.))
     (ccl::process-total-run-time process)))    ;sixtieths of a second

(export 'process-run-time :www-utils)

#+CCL-3
(defun process-idle-time (process)
  "Returns the amount of time since the process ran last, in sixtieths of a second."
  (* 60. (ccl::process-last-run-time process))) ;seconds

(export 'process-idle-time :www-utils)

;;;------------------------------------------------------------------- 
;;;
;;;  PROPERTY LISTS FOR PROCESSES
;;; 

(declaim (inline %process-property-list))
    
;; hijack the process slot WARM-BOOT-ACTION to store the process property list
;; WARM-BOOT-ACTION is unused in MCL 4.2 -- JCMa 4/3/1999.
(defun %process-property-list (process)
   (ccl::process.warm-boot-action (ccl:require-type process 'ccl::process)))
    
(declaim (inline %set-process-property-list))
    
(defun %set-process-property-list (process plist)
   (setf (ccl::process.warm-boot-action process) plist))
    
(defsetf %process-property-list %set-process-property-list)
    
(declaim (inline %remove-property-list))
    
(defun %remove-property-list (process)
   (setf (%process-property-list process) nil)) 

(defun clear-process-property-lists ()
   (dolist (p ccl:*all-processes*)
      (%remove-property-list p)))
    
(defmethod http:get-value ((process ccl::process) indicator &optional default)
   (let ((value (getf (%process-property-list process) indicator :+not-found+)))
      (case value
         (:+not-found+
           (values default nil))
         (t (values value t)))))
    
(defmethod http::%put-value ((process ccl::process) indicator value)
   (let ((plist (%process-property-list process)))
      (prog1 (setf (getf plist indicator) value)
         (setf (%process-property-list process) plist))))

(defmethod (setf http:get-value) (value (process ccl::process) indicator &optional default)
   (declare (ignore default))
   (let ((plist (%process-property-list process)))
      (prog1 (setf (getf plist indicator) value)
         (setf (%process-property-list process) plist))))
    
(defmethod http:remove-value ((process ccl::process) indicator)
   (let ((plist (%process-property-list process)))
      (when plist
          (prog1 (remf plist indicator)
             (if plist
                (setf (%process-property-list process) plist)
                (%remove-property-list process))))))
    
(defmethod http:property-list ((process ccl::process))
   (%process-property-list process))
    
(defmethod http:map-indicators ((process ccl::process) function)
   (loop for item in (%process-property-list process) by #'cddr
            do (funcall function item)))
    
(defmethod http:map-values ((process ccl::process) function)
   (loop for item in (cdr (%process-property-list process)) by #'cddr
            do (funcall function item))) 

;;;------------------------------------------------------------------- 
;;;
;;;  BASIC READ-DELIMITED-LINE FOR MCL 
;;;

(defmethod read-delimited-line (stream &optional (delimiters '(#\Linefeed #\Return)) eof buffer)
   "Reads a line from stream which is delimited by DELIMITERS."
   (declare (values line eof delimiter length)
                 (special http::*line-buffer-size*)
                 (special http::line-buffer))
   (labels ((clear-delimiter (prev-char stream reader reader-arg)
                   (let ((char (funcall reader reader-arg)))
                      (when (and char
                                        (or (eql char prev-char)
                                              (not (member char delimiters))))
                          (ccl:stream-untyi stream char))))
                (do-it (stream delimiters buffer)
                   (declare (type string buffer))
                   (let* ((size (array-total-size buffer))
                             (index -1)
                             error-p delimiter)
                      (if (ccl:stream-eofp stream)
                         (setq error-p t)
                         (multiple-value-bind (reader reader-arg) 
                                                         (ccl:stream-reader stream)
                             (with-fast-array-references ((buffer buffer string))
                                 (loop initially (setf (fill-pointer buffer) 0)
                                          for char = (funcall reader reader-arg)
                                          until (or (null char) (member char delimiters))
                                          for idx upfrom 0
                                          unless (< idx size)
                                          do (setq size (floor (* (the fixnum size) 1.2))
                                                       buffer (adjust-array buffer size :element-type '#.ccl::*default-character-type*))
                                          do (setf (aref buffer idx) char)
                                          (setq index idx)
                                          ;; (format t "~:C|" char)
                                          finally (cond (char
                                                               (setq delimiter char)
                                                               (clear-delimiter delimiter stream reader reader-arg))
                                                              (t (setq error-p t)))))))
                      (if (= -1 index)
                         (values (if error-p eof buffer) error-p delimiter 0)
                         (values buffer error-p delimiter (setf (fill-pointer buffer) (1+ (the fixnum index))))))))
      (declare (inline clear-delimiter))
      (if buffer
         (do-it stream delimiters buffer)
         (using-resource (line-buffer http::line-buffer http::*line-buffer-size*)
            (multiple-value-bind (buf error-p delim length)
                                            (do-it stream delimiters line-buffer)
                (values (if error-p eof (subseq buf 0 length)) error-p delim length)))))) 

;;;------------------------------------------------------------------- 
;;; 
;;; BUFFER-ORIENTED READ-DELIMITED-LINE 
;;; 

#+Open-Transport
(defmethod read-delimited-line ((stream ccl::modal-ascii-or-binary-tcp-stream-mixin) &optional (delimiters '(#\Linefeed #\Return)) eof buffer)
   "Reads a line from stream which is delimited by DELIMITERS."
   (declare (values line eof delimiter length) (special http::*line-buffer-size*) (special http::line-buffer))
   (cond (buffer
              (setf (fill-pointer buffer) 0)
              (%buffered-stream-read-delimited-line stream delimiters eof buffer)) 
            (t (using-resource (line-buffer http::line-buffer http::*line-buffer-size*)
                   (setf (fill-pointer line-buffer) 0)
                   (multiple-value-bind (buf error-p delim length)
                                                   (%buffered-stream-read-delimited-line stream delimiters eof line-buffer)
                       (values (if error-p eof (subseq buf 0 length)) error-p delim length)))))) 

;;;------------------------------------------------------------------- 
;;;
;;;   PARAMETERS ESSENTIAL FOR APPLETALK OPERATION
;;; 
(define-variable *host-name-for-apple-talk-operation* "Local-Host.AppleTalk.Net"
                 "The domain name for the local host during AppleTalk Operation.")

(export '*host-name-for-apple-talk-operation* :www-utils)
(export '*host-name-for-apple-talk-operation* :http)

(declaim (special url:*url-host-name-resolution*))

(define initialize-apple-talk-configuration (&optional (host-name (local-host-ip-address t)) domain-name-resolution-available-p)
   "Initializes the server to operate over AppleTalk, either in standalone mode or on an AppleTalk network. 
DOMAIN-NAME-RESOLUTION-AVAILABLE-P indicates whether doman name resolution service is accessible on the local host or
over the AppleTalk network. When DNS is unavailable, HOST-NAME must be an IP address (e.g., 127.0.0.3) unless the host file
in the system directory is edited to inform the TCP implementation of the mappings between host names and IP addresses.
When DNS is available, HOST-NAME should be the domain name for the server.  If the Hosts file in the system folder
contains the mappings from domain names to IP addresses, DOMAIN-NAME-RESOLUTION-AVAILABLE-P should be non-null
and host name should be the mapped domain name (e.g.,  Local-Host.AppleTalk.Net)."
   (let ((dns-available-p (not (null domain-name-resolution-available-p)))) 
      ;; Turn off domain name resolution in the TCP subtrate
      (setf (ccl::tcp-use-name-resolver) dns-available-p)
      ;; Controls whether IP addresses are resolved when writing log entries.
      ;; Production servers should turn this off to avoid the overhead of DNS lookup during logging.
      (when (and (not dns-available-p) http:*resolve-ip-addresses*)
          ;; Don't take any chances
          (setq http:*log-resolve-ip-addresses* nil)
          ;; Controls whether IP addresses are resolved in all contexts other than logging.
          (setq http:*resolve-ip-addresses* dns-available-p))
      ;;  Perform server initialization
      (when host-name
          (check-type host-name string)
          (unless dns-available-p
             (unless (ccl::%tcp-ip-string-p host-name)
                (error "HOST-NAME is ~A, which is not an IP address.~
                             ~&You need to either specify an IP address such as 127.0.0.3 as the domain ~
                             name or create a hosts file in the system directory that maps ~A to the correct IP address." host-name host-name))
             ;; Tell the MAC its domain name for local talk operation.
             (setq http:*local-host-domain-name* host-name)
             ;; force the logical name of the local host for local talk operation.
             (setq http:*http-host-name* http:*local-host-domain-name*)
             ;; Don't resolve host names while parsing URLs.
             (setq url:*url-host-name-resolution* :never))
          ;; Remember the host name in use for AppleTalk Operation,
          (setq *host-name-for-apple-talk-operation* host-name) 
          ;; Tell the MAC its IP Address for local talk operation.
          (local-host-ip-address t)
          host-name)))

(export 'initialize-apple-talk-configuration :www-utils) 

;;;------------------------------------------------------------------- 
;;;
;;; AN INTERFACE CONSISTENT WITH PROPERTY LIST MIXIN FOR PATHNAMES
;;;

(defvar *pathname-property-list-table* nil
  "Holds the property lists for pathnames in the file system.")

(declaim (inline pathname-property-list-table))

;; Pathnames are not EQ in MCL 4.2. Change hashtable test to EQ when and if it is fixed. -- JCMa 5/11/1999.
(defun pathname-property-list-table ()
  (or *pathname-property-list-table*
      (setf *pathname-property-list-table* (make-hash-table :test #'equal))))

(defun clear-pathname-property-list-table ()
   (when *pathname-property-list-table*
       (clrhash *pathname-property-list-table*)))

(declaim (inline %pathname-property-list))

(defun %pathname-property-list (pathname)
  (gethash pathname (pathname-property-list-table)))

(declaim (inline %set-pathname-property-list))

(defun %set-pathname-property-list (pathname plist)
  (setf (gethash pathname (pathname-property-list-table)) plist))

(defsetf %pathname-property-list %set-pathname-property-list)

(declaim (inline %pathname-property-list-put-value))

(defun %pathname-property-list-put-value (pathname indicator value)
   (let ((plist (%pathname-property-list pathname)))
      (prog1 (setf (getf plist indicator) value)
         (setf (%pathname-property-list pathname) plist))))

(declaim (inline %remove-property-list))

(defun %remove-property-list (pathname)
  (remhash pathname (pathname-property-list-table))) 

(defmethod http:get-value ((pathname pathname) indicator &optional default)
  (let ((value (getf (%pathname-property-list pathname) indicator :+not-found+)))
    (case value
      (:+not-found+
        (values default nil))
      (t (values value t))))) 

(defmethod http::%put-value ((pathname pathname) indicator value)
   (%pathname-property-list-put-value pathname indicator value))

(defmethod (setf http:get-value) (value (pathname pathname) indicator &optional default)
   (declare (ignore default))
   (%pathname-property-list-put-value pathname indicator value)) 

(defmethod http:remove-value ((pathname pathname) indicator)
  (let ((plist (%pathname-property-list pathname)))
    (when plist
      (prog1 (remf plist indicator)
             (if plist
                 (setf (%pathname-property-list pathname) plist)
                 (%remove-property-list pathname))))))

(defmethod http:property-list ((pathname pathname))
  (%pathname-property-list pathname))

(defmethod http:map-indicators ((pathname pathname) function)
  (loop for item in (%pathname-property-list pathname) by #'cddr
        do (funcall function item)))

(defmethod http:map-values ((pathname pathname) function)
  (loop for item in (cdr (%pathname-property-list pathname)) by #'cddr
        do (funcall function item))) 

;;;------------------------------------------------------------------- 
;;;
;;; CRLF CANONICALIZATION
;;;

(declaim (notinline %pathname-lock))

(defun %pathname-lock (pathname)
  (http:get-value pathname :crlf-locked-p))

(declaim (notinline %set-pathname-lock))

(defun %set-pathname-lock (pathname value)
  (http::%put-value pathname :crlf-locked-p value))

(defsetf %pathname-lock %set-pathname-lock)

(declaim (notinline pathname-crlf-locked-p))

(defun pathname-crlf-lock-idle-p (pathname)
  "Returns non-null of pathname is NOT CRLF locked."
  (not (%pathname-lock pathname)))

(declaim (notinline pathname-crlf-locked-p))

(defun pathname-crlf-locked-p (pathname)
  "Returns non-null of pathname is CRLF locked."
  (%pathname-lock pathname)) 

; This will need a without-interrupts if MCL goes to a preemptive scheduler
(defmacro store-conditional* (place old new)
  `(when (eq ,place ,old)
     (setf ,place ,new)
     t))

(defun %atomic-pathname-crlf-lock (pathname)
  (loop with process = ccl::*current-process*
        for current-value = (%pathname-lock pathname)
        do (when (store-conditional* (%pathname-lock pathname) current-value process)
             (return process))
           (process-wait "Grab CRLF Lock" #'pathname-crlf-lock-idle-p pathname)))

(defun %atomic-pathname-crlf-unlock (pathname)
  (loop for current-value = (%pathname-lock pathname)
        while current-value
        do (when (store-conditional* (%pathname-lock pathname) current-value nil)
             (return nil))))

(defmacro with-pathname-crlf-locked ((pathname) &body body)
  "Grabs a lock on PATHNAME to prevent multiple threads from CRLF encoding the same pathname simultaneously."
  `(let ((pathname ,pathname))
     (unwind-protect
         (progn (%atomic-pathname-crlf-lock pathname)
                . ,body)
       (%atomic-pathname-crlf-unlock pathname))))

(defmethod http:stream-encode-crlf-until-eof ((from-stream ccl::stream) (to-stream ccl::stream))
  (error "This operation has not been defined from ~S to ~S." (type-of from-stream) (type-of to-stream)))

;; Find a faster translating stream if possible.   6/24/96 -- JCMa.
;; From-stream should be a character stream and to-stream should be an unsigned 8-bit byte stream.
;; Since MCL does not have streams corresponding precisely to these, we wing it. -- JCMa 7/22/1996.
(defmethod http:stream-encode-crlf-until-eof ((from-stream ccl::input-stream) (to-stream ccl::output-binary-stream))
  (using-resource (line-buffer http::line-buffer http::*line-buffer-size*)
    (multiple-value-bind (writer to-sink)
        (ccl:stream-writer to-stream)
      (loop with line and eof and delimiter and length
            do (multiple-value-setq (line eof delimiter length)
                 (read-delimited-line from-stream '(#\Return #\Linefeed) nil line-buffer))
            do (unless (zerop length)
                 (with-fast-array-references ((array line vector))
                   delimiter                    ;ignore
                   (loop for idx upfrom 0 below length
                         for byte = (char-code (aref array idx))
                         do (funcall writer to-sink byte))))
               (funcall writer to-sink #.(char-code #\Return))
               (funcall writer to-sink #.(char-code #\Linefeed))
            until eof)))) 

(defmethod http:valid-crlf-cache-file-p ((pathname pathname))
  (let (source-probe cache cache-probe)
    (with-open-file (source-file pathname :direction :probe :if-does-not-exist :error)
      (unless (and source-file
                   (setq source-probe (file-stream-pathname source-file)))
        (return-from http:valid-crlf-cache-file-p (values nil)))
      ;; get the cache counter part
      (setq cache (http:crlf-pathname source-probe))
      ;; check the cache counter part
      (with-open-file (crlf-file cache :direction :probe :if-does-not-exist nil)
        ;; cache exists
        (unless (setq cache-probe (and crlf-file (file-stream-pathname crlf-file)))
          (return-from http:valid-crlf-cache-file-p (values nil source-probe nil cache)))
        ;; source-probe is more recent
        (unless (< (ccl::file-write-date source-file)
                   (ccl::file-write-date crlf-file))
          (return-from http:valid-crlf-cache-file-p (values nil source-probe cache-probe cache)))))
    ;; if we get here the cache is valid
    (return-from http:valid-crlf-cache-file-p (values t source-probe cache-probe cache))))

(defmethod http:valid-crlf-cache-file-p :around ((pathname logical-pathname))
  (http:valid-crlf-cache-file-p (translate-logical-pathname pathname))) 

;; This has locking to avoid race conditions. 7/3/96 -- JCMa.
(defmethod http:ensure-crlf-canonical-file ((pathname pathname))
   (declare (values crlf-canonicalized-pathname newly-updated-p))
   (loop
      (multiple-value-bind (valid-crlf-cache-p source-pathname crlf-pathname canonical-pathname)
                                      (http:valid-crlf-cache-file-p pathname)
          (cond (valid-crlf-cache-p (return-from http:ensure-crlf-canonical-file crlf-pathname))
                   ((null source-pathname) (signal 'file-error :pathname source-pathname))
                   ((pathname-crlf-lock-idle-p source-pathname)
                     ;; small window here where two threads could update the same file twice.   7/4/96 -- JCMa.
                     (let ((new-crlf-pathname nil))
                        (with-pathname-crlf-locked (source-pathname)
                            (setq new-crlf-pathname (http:crlf-canonicalize-file source-pathname canonical-pathname)))
                        (return-from http:ensure-crlf-canonical-file (values new-crlf-pathname t))))
                   (t (process-wait "CRLF Wait" #'pathname-crlf-lock-idle-p source-pathname)))))) 

;; Specialized to ensure that the name + type of pathnames never exceed 31 characters.
;; Achieves this by truncating from the end of pathname.  If it collides, we'll worry about it later!
(defmethod http:crlf-pathname ((pathname pathname))
  (cond ((http::get-value pathname :crlf-pathname))
        (t (let* ((crlf-type (http::crlf-pathname-type (pathname-type pathname)))
                  (name (pathname-name pathname))
                  (type-len (length crlf-type))
                  (name-len (length name))
                  (name-limit (when (< 31 (+ (the fixnum type-len) (the fixnum name-len) 1))
                                (- 31 (the fixnum type-len) 1)))
                  (npath (make-pathname :host (pathname-host pathname)
                                        :device (pathname-device pathname)
                                        :directory (pathname-directory pathname)
                                        :name (if name-limit (subseq name 0 name-limit) name)
                                        :type crlf-type)))
             (http::%put-value pathname :crlf-pathname npath)
             npath))))

(defmethod http:crlf-pathname :around ((pathname logical-pathname))
  (http:crlf-pathname (translate-logical-pathname pathname))) 

;;;------------------------------------------------------------------- 
;;;
;;;  SPECIALIZATIONS OF STREAM COPYING METHODS FOR PERFORMANCE
;;; 

#+(and CCL-3 (not Open-Transport))
(defmethod http::stream-copy-byte-range ((from-stream ccl::input-file-stream) (to-stream ccl::basic-tcp-stream)
                                         start end &aux (conn (ccl::tcp-stream-conn to-stream)))
  (ccl::tcp-with-connection-grabbed (conn ccl::*current-process* "TCP Out")
                                    (ccl::tcp-write-file-bytes conn (ccl::stream-filename from-stream) (- end start) start)))

#+(and CCL-3 (not Open-Transport))
(defmethod http::stream-copy-bytes ((from-stream ccl::basic-tcp-stream) (pathname pathname) bytes &optional copy-mode
                                    &aux (conn (ccl::tcp-stream-conn from-stream)))
  (declare (ignore copy-mode))                  ; copy-mode is binary :binary
  (ccl::tcp-with-connection-grabbed (conn ccl::*current-process* "TCP In")
                                    (ccl::tcp-read-bytes-to-file conn pathname bytes 0)))

#+(and CCL-3 Open-Transport)
(defmethod http::stream-copy-byte-range ((from-stream ccl::input-file-stream) (to-stream ccl::basic-tcp-stream)
                                         start end &aux (buffer (ccl::stream-io-buffer to-stream)))
  (ccl::io-buffer-write-file buffer (ccl::stream-filename from-stream) start end))

 #+(and CCL-3 Open-Transport)
(defmethod http::stream-copy-bytes ((from-stream ccl::basic-tcp-stream) (pathname pathname) bytes &optional copy-mode
                                    &aux (buffer (ccl::stream-io-buffer from-stream)))
  (declare (ignore copy-mode))                  ; copy-mode is binary :binary
  (ccl::io-buffer-read-bytes-to-file buffer pathname bytes 0))

#+ignore
(defmethod http::stream-decode-crlf-until-eof (from-stream(to-stream ccl::output-file-stream))
  (resources:using-resource (line-buffer http::line-buffer http::*line-buffer-size*)
                            (ccl::with-fsopen-file (pb (ccl::stream-filename to-stream))
                                                   (loop with line and eof and delimiter and length
                                                         do (multiple-value-setq (line eof delimiter length)
                                                              (read-delimited-line from-stream '(#\Return #\Linefeed) nil line-buffer))
                                                         do (identity delimiter)        ; ignore
                                                         unless (zerop length)
                                                           do (vector-push-extend  #.(char-code #\Return) line-buffer 1)
                                                              (ccl::fswrite pb (+ 2 (the fixnum length)) line)
                                                         until eof))))

#+ignore
(defmethod http::stream-encode-crlf-until-eof (from-stream (to-stream ccl::output-file-stream))
  (resources:using-resource (line-buffer http::line-buffer http::*line-buffer-size*)
                            (ccl::with-fsopen-file (pb (ccl::stream-filename to-stream))
                                                   (loop with line and eof and delimiter and length
                                                         do (multiple-value-setq (line eof delimiter length)
                                                              (read-delimited-line from-stream '(#\Return #\Linefeed) nil line-buffer))
                                                         do (identity delimiter)        ; ignore
                                                         unless (zerop length)
                                                           do (vector-push-extend  #.(char-code #\Return) line-buffer 2)
                                                              (vector-push-extend  #.(char-code #\Linefeed) line-buffer 1)
                                                              (ccl::fswrite pb (+ 2 (the fixnum length)) line)
                                                         until eof))))


;;;------------------------------------------------------------------- 
;;;
;;; SPECIALIZED HTTP STREAM POLLING
;;;

#+ccl-3
(defmethod http::http-input-data-available-p ((stream ccl::basic-tcp-stream) &optional timeout-seconds)
   (labels ((data-available-p (stream)
                   (loop while (listen stream)        ;some data available
                            for char = (peek-char nil stream nil)
                            while char                   ;clear any dangling White Space due to buggy clients.
                            when (member char '(#\return #\linefeed #\space #\tab) :test #'eql)
                            do (read-char stream t)
                            return t                     ;something still there.
                            finally (return nil)))
                (continue-p (stream)
                   (or (not (www-utils:live-connection-p stream))     ;connection went dead
                         (data-available-p stream))))   ;data available
      (declare (inline data-available-p))
      (cond ((not (www-utils:live-connection-p stream)) nil)
               ((data-available-p stream) t)
               ((and timeout-seconds (not (zerop timeout-seconds)))
                 ;; Block until there is reason to take action
                 (process-poll-with-timeout
                   "HTTP Request Wait" timeout-seconds #'continue-p stream)
                 ;; Determine whether input data was available without consing.
                 (and (www-utils:live-connection-p stream)
                         (listen stream)))
               (t nil)))) 

;;;------------------------------------------------------------------- 
;;;
;;; EXTERNAL PATHNAMES
;;;

(declaim (inline pathname-external-name-string))

(defun pathname-external-name-string (string &optional (start 0) (end (length string)))
  "Removes any operating system specific characters from STRING,
returning a copy of the string."
  (remove ccl::*pathname-escape-character* string :start start :end end)) 

(define tcp-service-port-number (protocol &optional error-p)
   "Returns the service port number for the TCP protocol denoted by protocol.
PROTOCOL is a keyword, but integer and string are also supported."
   (if error-p
      (ccl::tcp-service-port-number protocol)
      (handler-case
         (ccl::tcp-service-port-number protocol)
         (error () nil)))) 

;;;------------------------------------------------------------------- 
;;;
;;; 
;;;
(defconstant +shtml-tag-start+ (ccl::string-to-8-bit-vector "<!--#"))
(defconstant +shtml-tag-start-length+ (length +shtml-tag-start+))
(defconstant +shtml-tag-end+ (ccl::string-to-8-bit-vector "-->"))
(defconstant +shtml-tag-end-length+ (length +shtml-tag-end+)) 

;; this should be smarter about the data and use the data-cache facility as appropriate. -- JCMa 5/11/1999.
(defmethod http::parse-shtml-template ((pathname pathname))
   (declare (values template-parameters))
   (labels ((copy-bytes-to-string (vector string start end)
                   (declare (type vector vector) (type string string) (fixnum start end) (optimize (speed 3) (safety 0)))
	           (loop for idx1 upfrom start below end
                            for idx2 upfrom 0
		            for byte = (aref vector idx1)
		            do (setf (aref string idx2) (if (member byte '#.(mapcar #'char-code '(#\Return #\Linefeed)) :test #'=)
                                                                       #\space
						                       (code-char byte))))
	           string)
                (extract-parameters (vector start end buffer)
	           (let* ((length (- end start))
		             (string (if (< length (array-total-size buffer)) buffer (make-array length :fill-pointer t :element-type ccl::*default-character-type*))))
                      (declare (dynamic-extent string))
	              (copy-bytes-to-string vector string start end)	; copy the chars into the string 
	              (setf (fill-pointer string) length)
	              (http::parse-shtml-element string 0 length))))
      (with-open-file (file-stream pathname :direction :input :element-type '(unsigned-byte 8))
          (let* ((read-end (file-length file-stream))
	            (data (make-array read-end :fill-pointer t :adjustable t :element-type '(unsigned-byte 8))))
             (declare (dynamic-extent data)
                           (type vector data))
	     (setq data (http::binary-stream-copy-into-8-bit-array file-stream read-end 0 data))
	     (using-resource (buffer http::line-buffer http::*line-buffer-size*)
	        (loop with read-start = 0 and end
                         for start = (search +shtml-tag-start+ data :start1 0 :end1 +shtml-tag-start-length+ :start2 read-start :end2 read-end :test #'eql)
		         while start
		         do (unless (setq end (search +shtml-tag-end+ data :start1 0 :end1 +shtml-tag-end-length+ :start2 (+ start +shtml-tag-start-length+) :end2 read-end :test #'eql))
                                 (error "Unbalanced SHTML element at byte ~D" start))
		         collect (multiple-value-bind (function parameter-plist)
                                                                    (extract-parameters data start (setq end (+ end +shtml-tag-end-length+)) buffer)
			                `(,read-start ,start ,function ,.parameter-plist)) into template-parameters
		         do (setq read-start end)
		         finally (return (if (= read-start read-end)
				                  template-parameters
				                  (nconc template-parameters `((,read-start ,read-end)))))))))))

#| (defun parse-shtml (url)
    (let ((pathname (url::cached-pathname url)))
       (multiple-value-bind (crlf-pathname)
                                       (http::ensure-crlf-canonical-file pathname)
           (url:parse-template url :shtml crlf-pathname))
       url))|# 

;;;------------------------------------------------------------------- 
;;;
;;;  BETTER BUG REPORTING IN AUTOMATICALLY GENERATED SERVER BUG MAIL
;;;

;; MCL doesn't provide a means to specify the number of frames to print. -- JCMa 5/14/1999.
(defmethod http::write-stack-backtrace ((error condition) stream &optional n-frames)
   (declare (ignore error n-frames))
   (let ((*debug-io* stream))
   (ccl::print-call-history :stack-group ccl::*current-stack-group* :start-frame 0 :detailed-p t))) 

