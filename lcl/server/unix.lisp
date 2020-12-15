;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: www-utils -*-

;;; (C) Copyright 1994-1995, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;; (C) Allegro enhancements Copyright 1995, OBC. All Rights Reserved.
;;;
;;; LispWorks enhancements Copyright (c) The Harlequin Group Limited, 1995.
;;;


;;;------------------------------------------------------------------- 
;;;
;;; MAC AND LISPM FILES COMPATABILITY CODE ADAPTED FOR ALLEGRO CL
;;;

(in-package :www-utils)

;; MJS 25Sep95: currently this cannot be trapped because the underlying error
;; code just generates file-error.
#+LispWorks
(deftype bad-connection-state () nil)

; Advise the lisp environment that we have MAC-CL-HTTP loaded.
#+CCL
(pushnew :mac-cl-http *features*)

#+CCL
(declaim (inline report-condition))

(define report-condition (condition stream)
  "Prints the report string for CONDITION onto STREAM."
  #+CCL
  (ccl::report-condition condition stream)
  #-CCL
  (describe condition stream))

#+CCL
(declaim (inline report-string))

(define report-string (condition)
  "Returns the report string for CONDITION."
  (with-output-to-string (stream)
    (report-condition condition stream)))

;; Define equivalence mapping to the MCL case.
(deftype file-not-found () 
  "Specialization of Common Lisp File-error in which the file was not found on open."
  '(and condition file-error))

#+ignore
(export 'file-not-found :www-utils)

#+(or LispWorks3.2 lcl4.2)
(defun special-operator-p (symbol)
   (#-lcl4.2 special-form-p #+lcl4.2 lisp:special-form-p symbol))

#+(or LispWorks3.2 lcl4.2)
(export 'special-operator-p :www-utils)

; with-tcp-port-for-protocol not used !

(defmacro with-array-registers (bindings &body body)
  `(let ,bindings ,@body))

#+CCL
(define-macro atomic-incf (reference &optional (delta 1))
  "Atomically increments REFERENCE by DELTA."
  `(#+CCL ccl:without-interrupts #+CLIM-SYS clim-sys:without-scheduling
     (incf ,reference ,delta)))

#+CLIM-SYS
(define-macro atomic-incf (reference &optional (delta 1))
  "Atomically increments REFERENCE by DELTA."
  `(clim-sys:atomic-incf ,reference ,delta))

#+CCL
(define-macro atomic-decf (reference &optional (delta 1))
  "Atomically decrements REFERENCE by DELTA."
  `(#+CCL ccl:without-interrupts #+CLIM-SYS clim-sys:without-scheduling
     (decf ,reference ,delta)))

#+CLIM-SYS
(define-macro atomic-decf (reference &optional (delta 1))
  "Atomically decrements REFERENCE by DELTA."
  `(clim-sys:atomic-decf ,reference ,delta))

(define-macro atomic-push (item reference)
  "Atomically pushes ITEM onto REFERENCE."
  `(#+CCL ccl:without-interrupts #+CLIM-SYS clim-sys:without-scheduling
          (push ,item ,reference)))

(define-macro atomic-pop (reference)
  "Atomically pops an item off REFERENCE."
  `(#+CCL ccl:without-interrupts #+CLIM-SYS clim-sys:without-scheduling
          (pop ,reference)))

(declaim (inline arglist))

(defun arglist (function)
  "Returns the arglist for FUNCTION."
  (declare (values (arglist values type arglist-types value-types)))
  (#+CCL ccl:arglist #+Allegro excl:arglist #+LispWorks lw:function-lambda-list #+lcl4.2 lcl::arglist function))

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
  #+CCL (ccl::tcp-host-address  address)
  #-CCL
  (etypecase address
    (integer address)
    (string #+(or Allegro LispWorks lcl4.2)
            (ipc:internet-address address)
            #-(or Allegro LispWorks lcl4.2)
            (nyi-internet-address address))))

(declaim (inline ip-address-for-parsed-ip-address))

(define ip-address-for-parsed-ip-address (ip-number)
  "Returns an IP address as a string from, IP-NUMBER, the parsed address."
  (#+CCL ccl::tcp-addr-to-str #+(or Allegro LispWorks lcl4.2) ipc:ip-address-string
   #-(or CCL Allegro LispWorks lcl4.2) nyi-ip-address-string
   ip-number))

#+CCL
(define domain-name-for-parsed-ip-address (ip-number &optional (no-error-p t))
  "Given the parsed IP address (an integer), IP-NUMBER, this returns the domain name or NIL.
When no-error-p is T, this returns the IP Address string when a domain error occurs."
  (check-type ip-number integer)
  (flet ((kludge-around-mcl201-bug (ip-number)
           (let* ((name (ccl::tcp-host-cname ip-number))
                  (last-elt (1- (the fixnum (length name)))))
             (if (eql (aref name last-elt) #\.)
                 (subseq name 0 last-elt)
                 name))))
    (declare (inline kludge-around-mcl201-bug))
    (cond 
      ((or (null http:*resolve-ip-addresses*) (zerop ip-number)) 
       (ip-address-for-parsed-ip-address ip-number))
      (no-error-p
       (handler-case
         (kludge-around-mcl201-bug ip-number)
         ;; formerly tcp-domain-server-not-found
         (ccl:domain-error () (ip-address-for-parsed-ip-address ip-number))))
      (t (kludge-around-mcl201-bug ip-number)))))

#+(or Allegro LispWorks lcl4.2)
(defvar *last-domain-name* nil)

;;; There are surely better strategies to get your domain name.
;;; But they depend on your OS. Can someone shine light on
;;; getting DNS domain names portably on UNIX?
;;;
#+(or Allegro LispWorks lcl4.2)
(defun default-domain-name (&optional (where
                                       #+Allegro "HTTP:acl;defaultdomain"
                                       #+lcl4.2 "HTTP:lcl;defaultdomain"
                                       #+LispWorks "HTTP:lw;defaultdomain"))
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

#+(or Allegro LispWorks lcl4.2)
(defvar *domain-name-lookup* (make-hash-table :test #'equal
                                              ;; ACL bug reported 28/6/95
                                              #+ignore #'eql))

;;; Need to figure out how to do this portably for non CCL (e.g. UNIX)
;;; This returns one of "host.site.entity.type" or an IP address - OBC
#+(or Allegro LispWorks lcl4.2)
(define domain-name-for-parsed-ip-address (ip-number &optional (no-error-p t))
  (or (gethash ip-number *domain-name-lookup*)
      (let ((domain (default-domain-name)))
        (setf (gethash ip-number *domain-name-lookup*)
              (let ((host-name (if no-error-p
                                   (ignore-errors (ipc:get-host-name-by-address ip-number))
                                 (ipc:get-host-name-by-address ip-number))))
                (if host-name
                    (if (stringp host-name)
                        (if (not (find #\. host-name)) ;Solaris 1.x or 2.x with NIS
                            (concatenate 'string host-name "." domain)
                          host-name)    ;Solaris 2.x with DNS
                      (if no-error-p
                          host-name
                        (error "Unexpected host name ~s." host-name)))
                  ;; No error fall-back
                  (ip-address-for-parsed-ip-address ip-number)))))))

(declaim (inline domain-name-for-ip-address))

(define domain-name-for-ip-address (address &optional (no-error-p t))
  "Given the IP address, ADDRESS, this returns the domain name or NIL."
  (domain-name-for-parsed-ip-address (%parse-host-address address) no-error-p))

(declaim (inline ip-address-for-host-domain-name))

(define ip-address-for-host-domain-name (domain-name)
  "Returns the IP address string for domain-name."
  (ip-address-for-parsed-ip-address (%parse-host-address domain-name))) 

(declaim (notinline %local-host-parsed-ip-number))

#+CCL
(defun %local-host-parsed-ip-number ()
  (handler-case
    (ccl::%tcp-getaddr)
    (ccl:domain-error () 0)
    ;; when a powerbook is disconnected, an untyped error is signalled
    ;; within CCL::TCP-DRIVER-REFNUM when it tries to open TCP.
    ;; MACTCP should signal a typed error here. -- JCMa 1/20/1995.
    (error () 0)))

#+FRANZ-INC
(defun %local-host-parsed-ip-number ()
  (ipc:internet-address (ipc::franz-hostname)))

#+(or LispWorks lcl4.2)
(defun %local-host-parsed-ip-number ()
  (ipc:internet-address (machine-instance)))

(define local-host ()
  "The host object for the local host on which we are running."
  (or http::*local-host-address*
      (setq http::*local-host-address* (%local-host-parsed-ip-number))))

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

;; pass this function back to tcp-stream.lisp where it is used in ccl::tcp-host-cname
#+CCL
(setf (symbol-function 'ccl::local-host-domain-name) #'local-host-domain-name)

;;;------------------------------------------------------------------- 
;;;
;;; HOST RELATED
;;;

;;#+CCL
(define parse-host (address &optional no-error-p)
  "Top-level method for parsing a host ADDRESS."
  (declare (values ip-number))
  (cond (no-error-p
         (handler-case
           (%parse-host-address address)
           (#+CCL ccl:network-error #-CCL network-error () nil)))
        (t (%parse-host-address address))))

(declaim (inline host-mail-name))

(define host-mail-name (host)
  "The internet mail name for HOST."
  (domain-name-for-ip-address host t))

(define host-eq (host1 host2)
  "Returns non-null if HOST1 is equal to HOST2."
  (cond ((and (integerp host1) (integerp host2))
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

;;; These need definitions in non CCL case!

#+CCL
(define local-port (http-stream)
  "Returns the local host port for the remote connection via http-stream."
  (let* ((conn (slot-value http-stream 'ccl::conn))
         (pb (when conn (ccl::conn-pb conn))))
    (when pb
      (ccl:rref pb tcpiopb.status.localport))))

#+CCL
(define foreign-port (http-stream)
  "Returns the foreign host port for the remote connection via http-stream."
  (let* ((conn (slot-value http-stream 'ccl::conn))
         (pb (when conn (ccl::conn-pb conn))))
    (when pb
      (ccl:rref pb tcpiopb.status.remoteport))))

#+CCL
(define foreign-host (http-stream)
  "Returns the foreign host for the remote connection via http-stream."
  (let* ((conn (slot-value http-stream 'ccl::conn))
         (pb (when conn (ccl::conn-pb conn))))
    (when pb
      (ccl:rref pb tcpiopb.status.remotehost))))

#+Allegro
(defmethod local-port ((http-stream ipc::tcp-client-stream))
  (slot-value http-stream 'ipc::server-port))

#+Allegro
(defmethod local-port ((http-stream ipc::tcp-server-stream))
  (slot-value http-stream 'ipc::port))

#+Allegro
(defmethod foreign-host ((http-stream ipc::tcp-client-stream))
  (or (slot-value http-stream 'ipc::host-address) ;What seems wanted
      (slot-value http-stream 'ipc::host)))

#+Allegro
(defmethod foreign-port ((http-stream ipc::tcp-client-stream))
  (slot-value http-stream 'ipc::port))

#+(or LispWorks lcl4.2)
(defun local-port (http-stream)
  (ipc:find-detail-from-stream http-stream :local-port))

#+(or LispWorks lcl4.2)
(defun foreign-host (http-stream)
  (ipc:find-detail-from-stream http-stream :remote-host))

#+(or LispWorks lcl4.2)
(defun foreign-port (http-stream)
  (ipc:find-detail-from-stream http-stream :remote-port))


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

#+(and CCL (not :CCL-3))
(defmethod file-length ((pathname pathname) &optional new-length)
  (declare (ignore new-length))
  (with-open-file (file-stream pathname)
    (file-length file-stream)))

#+:CCL-3
(defmethod file-length ((pathname pathname) &optional new-length)
  (declare (ignore new-length))
  (ccl::file-data-size pathname))

#+(and CCL (not :CCL-3))
(defmethod file-creation-date ((pathname pathname))
  (with-open-file (file-stream pathname)
    (file-write-date file-stream)))

#+:CCL-3
(defmethod file-creation-date ((pathname pathname))
  (ccl::file-write-date pathname))

#-(or Genera CCL)
(defmethod file-creation-date ((pathname pathname))
  (file-write-date pathname))

(declaim (inline file-stream-version))

(defun file-stream-version (file-stream)
  (file-stream-creation-date file-stream))

(declaim (inline file-version))

(defun file-version  (pathname)
  (when (probe-file pathname)
    (file-creation-date pathname)))

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
  (values (ccl::file-data-size pathname)
          (ccl::file-write-date pathname)
          (file-version pathname)))

(declaim (inline pathname-directory-p))

(defun pathname-directory-p (pathname)
  "Returns non-null if PATHNAME¬denotes a directory."
  #+CCL
  (ccl::directoryp pathname)
  #+LispWorks
  (lw:directoryp pathname)
  #+(and UNIX ;OBC
         (not LispWorks))
  (and (unix-sh-test "-d " pathname) t))

;;; When it's unclear what shell is used by CL, check the shell argument SHELL
;;; -- OBC
(defun system (arg)
  #+LispWorks
  (sys::call-system arg)
  #+Allegro
  (excl:shell arg)
  #+KCL
  (sys::system arg)
  #+lcl4.2
  (nth 2 
       (multiple-value-list 
           (lcl::run-program "/bin/sh" :arguments (list "-c" arg) :wait t))))

;;; OBC added
(defun unix-sh-test (cond path &aux (strpath (cond ((stringp path) path)
                                                   ((pathnamep path)
                                                    (namestring path)))))
    #+UNIX
    (if strpath
        (= (system (format nil "test ~a \"~a\" || exit 1"
                           cond strpath))       ; This is the fix: "strpath"!!
           0)
        nil)
    #-UNIX
    (and cond strpath t))

(defun directory-list (pathname &rest options)
  "Returns a lisp Machine style directory listing."
  (let ((pathnames #+UNIX
                   (apply #'unix-directory-list* (merge-pathnames pathname "*.*") nil options)
                   #+MCL
                   (directory (merge-pathnames pathname "*.*")
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

(declaim (inline alphalessp))

(defun alphalessp (a b)
  (string< a b))

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
            (:wild (get-directory-listing (pattern p "*")))))
        (string
          (get-directory-listing (pattern p type)))
        (cons
          (loop for type in type
                nconc (get-directory-listing (pattern p type)) into paths
                finally (return (if sort-pathnames
                                    (sort paths #'sorter)
                                    paths))))))))

#+UNIX ;OBC
(defun unix-directory-pathname (pathname)
  (let ((lastdir (pathname-name pathname)))
    (if lastdir
        (make-pathname :directory (append (pathname-directory pathname)
                                          (list lastdir))

                       :defaults pathname))))

#+UNIX ;OBC
(defun unix-directory-list* (pathname predicate &rest options)
  (let ((dirs (directory pathname
                         ;; This fixes directory problem on UNIX for Allegro
                         ;; LispWorks does this by default anyway
                         #-(or LispWorks lcl4.2) :directories-are-files 
                         #-(or LispWorks lcl4.2) nil)))
    #+lcl4.2
    (setf dirs
      (remove-if #'(lambda (e) (member (pathname-name e) '(() ".")
                                       :test #'equal))
                 dirs))
    (if (not (member :directories options))
        (setq dirs (loop for file in dirs
                       unless (pathname-directory-p file)
                       collect file))
      #-Allegro ;Fix for non Allegro case
      (setq dirs (loop for file in dirs
                     when (pathname-directory-p file)
                     collect (unix-directory-pathname file)
                     else collect file)))
    (if predicate
        (setq dirs (loop for file in dirs
                       when (funcall predicate file)
                       collect file)))
    dirs))

(defun directory-list* (pathname predicate &rest options)
  "Accepts the options :FILES :DIRECTORIES :SORTED :PROPERTIES."
  (let ((pathnames #+UNIX ;OBC
                   (apply #'unix-directory-list* (merge-pathnames pathname "*.*") predicate options)
                   #+MCL
                   (directory (merge-pathnames pathname "*.*")
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
  #+CCL
  (ccl:create-directory pathname :if-exists :error)
  #-CCL
  (create-directory-recursively1 pathname))

;;; For implementations where pathname-directory does
;;; not return NIL when there is no directory in the pathname.
;;; -- OBC
(defun pathname-dirs (pathname)
  (let ((dirs (pathname-directory pathname)))
    (and (consp dirs) dirs)))

;;; -- OBC
(defun create-a-directory (path &optional (error-p t))
  (let ((str (namestring path))) ;;(directorystring path)
    (case #+UNIX (system (format nil "mkdir ~S" str))
          #+MCL (ccl:create-directory path)
          #+ACLPC (win:mkdir str)
          #-(or UNIX MCL ACLPC)
          (progn (warn "Create a directory not implemented for this system.")
                 1)
      (0 path)
      (t (if error-p
             (if (probe-directory path)
                 (error "create-a-directory: file or directory already exists: ~a" path)
               (error "create-a-directory: failed on: ~a" path))
           path)))))

;;; Return path if you can write in it or over it.
;;; -- OBC
(defun file-permit-p (path &optional (permission "w"))
  #-UNIX
  (declare (ignore permission))
  #+UNIX
  (and (unix-sh-test (concatenate 'string "-" permission) path) path)
  #-UNIX
  path)

;;; -- OBC
(defun create-directory-recursively1 (path &optional (error-p t))
  (ctypecase path
    (string (setq path (pathname path)))
    (pathname))
  ;; most system cannot create a whole directory from scratch so
  ;; recursively create directories for path to be valid
  (let ((host    (pathname-host path))
        (pn-dirs (pathname-dirs path)))
    (case (car pn-dirs)
      (:absolute (setf (car pn-dirs) :root)))
    (let ((order-dirs (nreverse (maplist #'reverse (reverse pn-dirs))))
          lastpath result)
      (dolist (dirs #-(or ACLPC lucid) order-dirs
                    #+lucid (cdr order-dirs)
                    #+ACLPC (cddr order-dirs))
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
                   (return)))))))))

(defgeneric probe-directory (pathname)
  (:documentation "Returns non-null if the directory pathname exists."))

(defmethod probe-directory ((pathname pathname))
  (#+CCL ccl::probe-file #-CCL probe-file ;Why call CCL::PROBE-FILE?
         (make-pathname :host (pathname-host pathname)
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
      (#+CCL ccl:remote-network-error #-CCL remote-network-error () nil))))

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
      (#+CCL ccl:remote-network-error #-CCL remote-network-error () nil))))

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

;; Presently only does exact ip number matches.
; Needs to match partial addresses (e.g., 128.52.0.0) -- JCMa 12/30/1994.
(define ip-host-trusted-p (address secure-subnets &optional network)
  "Returns non-null if IP-address address is trusted given secure-subnets."
  (declare (ignore network))
  (flet ((address-match-p (addr1 addr2)
           (= addr1 addr2)))
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
;;; STREAM HACKING
;;;

#+CCL
(define-macro with-binary-stream ((stream direction) &body body)
  "Turns STREAM into a binary stream within the scope of BODY.
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

#+(or Allegro LispWorks)
(define-macro with-binary-stream ((stream direction) &body body)
  (declare (ignore stream direction))
  `(progn ,@body))

;;; Allegro provides a real stream implementation from X3J13
;;; STREAM-DEFINITION-BY-USER, Version 1, 22-Mar-89 by David N. Gray
;;;
#+Allegro
(defmethod http:stream-copy-until-eof ((from-stream stream:fundamental-input-stream) (to-stream stream:fundamental-output-stream))
  (loop with buffer-size = 512
      with buffer = (make-sequence '(vector character) buffer-size)
      as end = (read-sequence buffer from-stream)
      do (write-sequence buffer to-stream :end end)
      until (zerop end)))

;;; The only thing required is for the output stream to support
;;; stream-write-byte. This is for output to TCP-CLIENT-STREAM
;;; supporting a character-or-binary io patch...
;;;
#+Allegro
(defmethod http:stream-copy-until-eof ((from-stream stream:fundamental-binary-input-stream) (to-stream stream:fundamental-output-stream))
  (loop with buffer-size = 512
      with buffer = (make-sequence '(vector (unsigned-byte 8)) buffer-size)
      as end = (read-sequence buffer from-stream)
      do (write-sequence buffer to-stream :end end)
      until (zerop end)))

#+lcl4.2
(define-macro with-binary-stream ((stream direction) &body body)
  (declare (ignore stream direction))
  `(labels ((b-to-b (from-stream to-stream size)
              (dotimes (i size)
                (write-byte (read-byte from-stream) 
                            to-stream)))
            (c-to-c (from-stream to-stream size)
              (dotimes (i size)
                (write-char (read-char from-stream) 
                            to-stream)))
            (c-to-b (from-stream to-stream size)
              (dotimes (i size)
                (write-byte (char-code (read-char from-stream))
                            to-stream)))
            (b-to-c (from-stream to-stream size)
              (dotimes (i size)
                (write-char (code-char (read-byte from-stream))
                            to-stream)))
            (http:stream-copy-until-eof (from-stream to-stream)
              (let ((size (file-stream-length-in-bytes from-stream))
                    (itype (stream-element-type from-stream))
                    (otype (stream-element-type to-stream)))
                (cond ((and (subtypep itype 'character)
                            (not (subtypep otype 'character)))
                       (c-to-b from-stream to-stream size))
                      ((and (subtypep otype 'character)
                            (not (subtypep itype 'character)))
                       (b-to-c from-stream to-stream size))
                      ((and (subtypep itype 'character)
                            (subtypep otype 'character))
                       (c-to-c from-stream to-stream size))
                      (t 
                       (b-to-b from-stream to-stream size))))))
     ,@body))     

#-(or CCL Allegro LispWorks lcl4.2)
(define-macro with-binary-stream ((stream direction) &body body)
  `(progn
     (warn "WITH-BINARY-STREAM may not be implemeted yet.")
     ,@body))

;;;------------------------------------------------------------------- 
;;;
;;; ABORTING CONNECTIONS
;;; 

#+CCL
(define live-connection-p (http-stream)
  "Returns non-null if the TCP/IP connection over HTTP-STREAM remains alive
in that the remote host continue to respond at the TCP/IP level."
  (if (slot-value http-stream 'ccl::conn) t nil))

#+ignore ;;(from Genera)
(define live-connection-p (http-stream)
  "Returns non-null if the TCP/IP connection over HTTP-STREAM remains alive
in that the remote host continue to respond at the TCP/IP level."
  (scl:send http-stream :connected-p))

#+lcl4.2
(defmacro open-stream-p (stream)
  `(lucid::stream-open-p ,stream))

#-CCL
(define live-connection-p (http-stream)
  "Returns non-null if the TCP/IP connection over HTTP-STREAM remains alive
in that the remote host continue to respond at the TCP/IP level."
  (open-stream-p http-stream))

(declaim (inline abort-http-stream))

(define abort-http-stream (http-stream)
  "Closes http-stream in abort mode.  
This will push any output in the transmit buffer and catch any network errors.
Takes care to clean up any dangling pointers."
  (handler-case 
    (close http-stream :abort t)
    (#+CCL ccl::network-error #-CCL error ())))

#+ignore
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
;;; LOGGING EVENTS
;;; 

;;Bound to the HTTP server log window when one exists.
(defvar *log-window* nil)

;;Returns the active log window
#+CCL
(defun log-window ()
  (flet ((make-log-window (host-name)
           (make-instance 'ccl::fred-window
                          :scratch-p t
                          :color-p t
                          :window-show t
                          :window-title (format nil "MAC Common Lisp HTTP Log (~:(~A~))" host-name)
                          :window-layer 0)))
    (ccl:without-interrupts
      (let ((window *log-window*))
        (cond 
          ((and window (ccl::wptr window))
           window)
          (t (setq *log-window*  (make-log-window (local-host-domain-name)))))))))

#+(and CCL (not ccl-3))
(defmacro with-log-window-stream ((stream) &body body)
  `(let* ((window (log-window))
              ;;;Karsten 5/17/95 A Fred-Window is already the right stream in 2.01, 
          ;; window-key-handler is undefined
          (,stream window))
     (ccl::set-mark (ccl::fred-buffer window) t)
     (fresh-line ,stream)
     (ccl::window-show-cursor window)
     (prog1 (progn . ,body)
            (ccl::fred-update window))))

;; Bound to the process queue for the HTTP server log window when one exists.
#+:ccl-3
(defvar *log-window-process-queue* nil)

#+:ccl-3
(define log-window-process-queue ()
  "Returns the process queue for the CL-HTTP log window."
  (ccl:without-interrupts
    (cond (*log-window-process-queue*)
          (t (setq *log-window-process-queue* (ccl::make-process-queue 
                                                "HTTP Log Window Process Queue"))))))

#+:ccl-3
(define-macro with-log-window-stream ((stream) &body body)
  "Use this macro to write to the log window, which is bound to STREAM.
A process queue prevents forms executed within BODY from colliding with
other processes trying to write the log simultaneously."
  `(ccl::with-process-enqueued 
     ((log-window-process-queue) ccl::*current-process* "HTTP Log Window Wait")
     (let* ((window (log-window))
            (,stream (ccl::window-key-handler window)))
                                                ;how to make sure we're inserting at the end of the buffer????
       (ccl::set-mark (ccl::fred-buffer window) t)
       (fresh-line ,stream)
       (ccl::window-show-cursor window)
       (prog1 (progn . ,body)
              (ccl::fred-update window)))))

#-lucid
(declaim (ftype http::write-standard-time))

#+CCL
(define notify-log-window (format-string &rest format-args)
  "Top-level method for writing to the HTTP log window."
  (with-log-window-stream (stream)
                          (fresh-line stream)
                          (write-char #\[ stream)
                          (http::write-standard-time (get-universal-time) stream)
                          (write-string "]  " stream)
                          (apply #'format stream format-string format-args)))

#-CCL
(define notify-log-window (format-string &rest format-args)
  "Top-level method for writing to the HTTP log window."
  (let ((stream *trace-output*))
    (fresh-line stream)
    (write-char #\[ stream)
    (http::write-standard-time (get-universal-time) stream)
    (write-string "]  " stream)
    (apply #'format stream format-string format-args)))

#+CCL
(define expose-log-window ()
  "Exposes the window."
  (let ((selected-window ccl::*selected-window*))
    ;; bring the log window to the front
    ;; (ccl::window-bring-to-front (log-window)) ;; causes confusion with selection
    (ccl::window-select (log-window))
    ;; but don't lose selection of the current window
    (ccl::window-select selected-window)))

#-CCL
(define expose-log-window ()
  "Exposes the Log window. Does nothing."
  nil)

#+CCL
(define common-logfile-notify (server)
  "Issues a notification of server activity on a window."
  (with-log-window-stream (stream)
                          (http::write-common-logfile-entry server stream)))

#-CCL
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
(define report-bug  (to subject format-string &rest format-args)
  (notify-log-window (with-output-to-string
                       (stream)
                       (format  stream "~&Report Bug:  To:~A~
                                                       ~&Subject: ~A"
                                to subject)
                       (fresh-line stream)
                       (apply #'format stream format-string format-args))))

(declaim (inline bytes-transmitted))

#-lcl4.2
(define bytes-transmitted (stream)
  "Returns the number of bytes transmitted over STREAM."
  #+CCL (ccl:bytes-transmitted stream)
  #-CCL (declare (ignore stream))
  #-CCL   
  0)

#+lcl4.2
(defun bytes-received (stream)
  (file-position (lucid::split-stream-input-stream stream)))

#+lcl4.2
(defun set-bytes-received (stream n)
  (file-position (lucid::split-stream-input-stream stream)) n)

#+lcl4.2
(defsetf bytes-received set-bytes-received)

#+lcl4.2
(defun bytes-transmitted (stream)
  (file-position (lucid::split-stream-output-stream stream)))

#+lcl4.2
(defun set-bytes-transmitted (stream n)
  (file-position (lucid::split-stream-output-stream stream)) n)

#+lcl4.2
(defsetf bytes-transmitted set-bytes-transmitted)

#+ignore
(export 'bytes-transmitted :www-utils)

#+lcl4.2
(export 'bytes-received :www-utils)


;;;------------------------------------------------------------------- 
;;;
;;; MCL specific initializations 
;;;

#+CCL
(add-initialization "Maybe Enable EGC" '(ccl:egc  (ccl::egc-mmu-support-available-p)))


;;;------------------------------------------------------------------- 
;;;
;;; LOG RELATED PORTABILITY
;;;

(defun make-lock (name)
  "Returns a lock named name that is suitable for use with with-lock-held."
  (clim-sys:make-lock name))

(defmacro with-lock-held ((lock &optional (mode :write) (whostate "Wait for Lock")) &body body)
  "Executes BODY with LOCK held in MODE, which is one of :READ or :WRITE."
  (declare (ignore mode))
  `(clim-sys:with-lock-held (,lock ,whostate)
     ,@body))

(defun %make-log-pathname (directory name &optional machine-name)
  "Returns the pathname to which current log entries are written."
  (declare (ignore machine-name))
  (make-pathname 
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
  #+LispWorks (mp:make-named-timer name
                                   'make-process-and-run-function
                                   name function arguments)
  #+lcl4.2(apply #'make-named-timer name function arguments)
  #-(or LispWorks lcl4.2) (nyi-create-timer-call))

(defun make-named-timer (name func &rest args)
  (lcl::make-process 
   :name (format () "Timer: ~a" name)
   :function #'timer-toplevel
   :args (list func args)))

(defun timer-toplevel (func args)
  (loop
    (cond ((not (lcl::symbol-process-boundp '*sleep-time*
                                            lcl::*current-process*))
           (setf (lcl::symbol-process-value '*sleep-time* 
                                            lcl::*current-process*)
             ())
           (lcl::deactivate-process lcl::*current-process*))
          ((not (numberp (lcl::symbol-process-value '*sleep-time*
                                                    lcl::*current-process*)))
           (lcl::deactivate-process lcl::*current-process*))
          (t
           (format t "~%Activating ~s in ~d seconds"
                   func (lcl::symbol-process-value '*sleep-time*
                                                   lcl::*current-process*))
           (sleep (shiftf (lcl::symbol-process-value '*sleep-time*
                                                     lcl::*current-process*)
                          ()))
           (apply func args)))))

;; Portable?
#+CLIM-SYS
(defun make-process-and-run-function (name function args)
  (clim-sys:make-process #'(lambda ()
                             (apply function args))
                         :name name))

(defun reset-timer-absolute (timer absolute-universal-time)
  "Reset the timer to expire at the given absolute-universal-time."
  #+LispWorks (mp:schedule-timer-relative
               timer
               ;; Convert to seconds from time now
               (- absolute-universal-time
                  (get-universal-time)))
  #+lcl4.2 (progn
             (setf (lcl::symbol-process-value '*sleep-time* timer)
               (- absolute-universal-time
                  (get-universal-time)))
             (lcl::activate-process timer))
  #-(or LispWorks lcl4.2)(nyi-reset-timer-absolute))

(defvar *time-zone* nil
   "Time zone offset from GMT in hours.")

(define time-zone (&optional update-p)
  "Returns the timezone as an offset from GMT."
  (if (or update-p
          (not *time-zone*))
      (setq *time-zone*
        #+LispWorks (system::timezone)
        #+lcl4.2 (round (lucid::osi_timezone) 60)
        #-(or LispWorks lcl4.2) (nyi-time-zone))
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


#+lucid
(defmethod read-delimited-line (stream &optional (delimiters '(#\Return #\Linefeed)) eof buffer)
  "Reads a line from stream which is delimited by DELIMITERS."
  (declare (values line eof delimiter length)
           (special http::*line-buffer-size*)
           (special http::line-buffer))
  (flet ((do-it (stream delimiters buffer)
           (flet ((clear-delimiter (prev-char stream)
                    ;; Inline compiling causes lucid to combine
                    ;; vars with the same name. Therefore use lchar
                    ;; instead of char.
                    (let ((lchar (read-char stream nil nil)))
                      (when (and lchar
                                 (not (eql lchar :would-hang))
                                 (or (eql lchar prev-char)
                                     (not (member lchar delimiters 
                                                  :test #'char=))))
                        (unread-char lchar stream)))))
             (declare (inline clear-delimiter))
             (let* ((size (array-total-size buffer))
                    (index -1)
                    error-p delimiter)
               (handler-case
                   (with-fast-array-references ((buffer buffer string))
                     (loop initially (setf (fill-pointer buffer) 0)
                         for char = (read-char stream t eof t)
                         until (or (eql char eof)
                                   (member char delimiters :test #'char=))
                         for idx upfrom 0
                         unless (< idx size)
                         do (setq size (floor (* (the fixnum size) 1.2))
                                  buffer (adjust-array buffer size :element-type 'character))
                         if (eql char :would-hang)
                         do (decf idx)
                            (lcl::process-allow-schedule)
                         else
                         do (setf (aref buffer idx) char)
                         do (setq index idx)
                            ;; (format t "~:C|" char)
                         finally (if (and (eql char eof) (< 0 idx))
                                     (setq error-p t)
                                   (setq delimiter char))
                                 (clear-delimiter char stream)))
                 (end-of-file  () (setq error-p t)))
               (if (= -1 index)
                   (values (if error-p eof buffer) error-p delimiter 0)
                   (values buffer error-p delimiter (setf (fill-pointer buffer) (1+ (the fixnum index)))))))))
    (if buffer
        (do-it stream delimiters buffer)
        (using-resource (line-buffer http::line-buffer http::*line-buffer-size*)
          (multiple-value-bind (buf error-p delim length)
              (do-it stream delimiters line-buffer)
            (values (if error-p eof (subseq buf 0 length)) error-p delim length))))))


#+clim-sys
(defun process-wait (wait-reason predicate &rest args)
  (declare (dynamic-extent args))
  (flet ((wait-function ()
	   (apply predicate args)))
    (declare (dynamic-extent wait-function))
    (clim-sys:process-wait wait-reason (if args #'wait-function predicate))))
