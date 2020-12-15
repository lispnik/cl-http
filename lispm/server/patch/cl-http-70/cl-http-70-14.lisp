;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.14
;;; Reason: Function (CLOS:METHOD CLOS:INITIALIZE-INSTANCE (HTTP::BASIC-DATA-UNIVERSE) :AFTER):  make multiple reader lock.
;;; Function WWW-UTILS:WITH-LOCK-HELD:  call PROCESS:WITH-LOCK using the keyword argument MODE properly.
;;; Function HTTP::WITH-LOG-WRITE-LOCK:  update.
;;; Function HTTP::WITH-REALM-WRITE-LOCK:  -
;;; Function (CLOS:METHOD HTTP:INTERN-ACCESS-CONTROL (HTTP::STANDARD-REALM-ACCESS-CONTROL-MIXIN STRING)):  -
;;; Function (CLOS:METHOD HTTP:INTERN-GROUP (HTTP::STANDARD-REALM-GROUP-MIXIN STRING)):  -
;;; Function (CLOS:METHOD HTTP:INTERN-USER (HTTP::STANDARD-REALM-USER-MIXIN STRING)):  -
;;; Function (CLOS:METHOD HTTP::WRITE-LISP-SOURCE (HTTP::STANDARD-REALM NULL T)):  -
;;; Function HTTP::WITH-DATA-CACHE-LOCK:  -
;;; Function (CLOS:METHOD HTTP::RECACHE-DATA (HTTP::BASIC-DATA-CACHE)):  -
;;; Function (CLOS:METHOD HTTP::WRITE-CACHE-DATA (HTTP::BASIC-DATA-CACHE T)):  -
;;; Function HTTP::WITH-DATA-UNIVERSE-LOCK:  -
;;; Function (CLOS:METHOD HTTP:CLEAR-DATA-UNIVERSE (HTTP::BASIC-DATA-UNIVERSE)):  -
;;; Function (CLOS:METHOD HTTP:ENSURE-CURRENT-DATA (HTTP::BASIC-DATA-UNIVERSE)):  -
;;; Function (CLOS:METHOD HTTP:RECACHE-DATA-UNIVERSE (HTTP::FILE-DATA-UNIVERSE)):  -
;;; Function (CLOS:METHOD HTTP::RECACHE-DATA-UNIVERSE-AS-NECESSARY (HTTP::FILE-DATA-UNIVERSE)):  -
;;; Function HTTP::WITH-LOG-WRITE-LOCK:  -
;;; Function (CLOS:METHOD HTTP::CLOSE-LOG-STREAM (HTTP::FILE-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::LOG-STREAM (HTTP::FILE-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::SWITCH-LOG-FILE (HTTP::FILE-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD HTTP::WRITE-ACCESS-LOG-ENTRY-TO-FILE (HTTP::FILE-LOGGING-MIXIN HTTP::SERVER-LOGGING-MIXIN)):  -
;;; Function (CLOS:METHOD TQ:CLEAR-TASK-QUEUE (TQ:TASK-QUEUE)):  -
;;; Function (CLOS:METHOD TQ:POP-TASK-QUEUE (TQ:TASK-QUEUE)):  -
;;; Function (CLOS:METHOD TQ:PUSH-TASK-QUEUE (TQ:TASK-QUEUE T)):  -
;;; Written by JCMa, 10/01/99 17:10:03
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.6, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.13,
;;; W3 Presentation System 8.0, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.0, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.24, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 41,
;;; White House Publication System 25.32, WH Automatic Categorization System 15.16,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.11,
;;; DEC OSF/1 V4.0 (Rev. 110),
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
  "HTTP:SERVER;DATA-CACHE.LISP.99"
  "HTTP:LISPM;SERVER;LISPM.LISP.442"
  "HTTP:SERVER;AUTHENTICATION.LISP.147"
  "HTTP:SERVER;DATA-CACHE.LISP.100"
  "HTTP:SERVER;LOG.LISP.179"
  "HTTP:SERVER;SERVER.LISP.803"
  "HTTP:SERVER;TASK-QUEUE.LISP.24")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;DATA-CACHE.LISP.99")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: HTTP -*-")

(defmethod initialize-instance :after ((universe basic-data-universe) &key &allow-other-keys)
  (let ((size (data-universe-cache-table-size universe))
	(audit-size (data-universe-auditor-table-size universe)))
    (setf (data-universe-cache-table universe) (make-hash-table :test #'equal :size size)
	  (data-universe-auditor-table universe) (make-hash-table :test #'equal :size audit-size)
	  (data-universe-lock universe) (make-lock (data-universe-name universe)
						   :type :multiple-reader-single-writer))
    universe))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.442")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

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

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.179")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(define-macro with-log-write-lock ((log) &body body)
  `(with-slots (lock) ,log
     (www-utils:with-lock-held (lock :write "HTTP Log Wait") ,@body)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.147")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(define-macro with-realm-write-lock ((realm) &body body)
  "Provides concurrency control for shared resources associated with realms."
  `(www-utils:with-lock-held ((realm-lock ,realm) :write "Realm Wait") ,@body))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;DATA-CACHE.LISP.100")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: HTTP -*-")

(define-macro with-data-cache-lock ((data-cache &optional (mode :write)) &body body)
  "Grabs the lock for DATA-CACHE with mode MODE within the scope of BODY."
  `(let ((lock (data-cache-lock ,data-cache)))
     (with-lock-held (lock ,mode "Data Wait")
       . ,body)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;DATA-CACHE.LISP.100")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: HTTP -*-")

;;;------------------------------------------------------------------- 
;;;
;;; UTILS
;;;

(define-macro with-data-universe-lock ((data-universe &optional (mode :write)) &body body)
  "Grabs the lock for data-universe with mode MODE within the scope of BODY."
  `(let ((lock (data-universe-lock ,data-universe)))
     (with-lock-held (lock ,mode "Data Universe Wait")
       . ,body)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.147")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod write-lisp-source ((realm standard-realm) (ignore null) stream)
  (flet ((write-item (key value)
           (declare (ignore key))
           (write-lisp-source realm value stream)))
    (declare (dynamic-extent #'write-item))
    (with-realm-write-lock (realm) 
      ;; Create the realm.
      (format stream ";;;-------------------------------------------------------------------~&;;;
~&;;; ~:(~A~) Realm~&;;;~2%" (realm-name realm))
      (write (intern-realm-form realm) :stream stream)
      (terpri stream)
      ;; create all the groups within the realm.
      (map-groups realm #'write-item)
      ;; Create all users and link them to groups.
      (map-users realm #'write-item)
      ;; Create all access control objects and link them to groups and users.
      (map-access-controls realm #'write-item)
      (terpri stream)
      realm))) 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.147")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod intern-access-control ((realm standard-realm-access-control-mixin) (name string) 
                                  &key (if-does-not-exist :error)
                                  (capability-alist nil capability-alist-supplied-p)
                                  &aux access-control-object)
  (declare (values interned-access-control newly-created-p))
  (with-slots (access-control-table) realm
    (cond ((setq access-control-object (gethash name access-control-table))
           (when capability-alist-supplied-p
             (update-access-control access-control-object capability-alist))
           access-control-object)
          (t (ecase if-does-not-exist
               (:soft nil)
               ((:create :url-create)
                (setq access-control-object (ecase if-does-not-exist
                                              (:create (make-access-control realm name))
                                              (:url-create (make-url-access-control realm name))))
                (update-access-control access-control-object capability-alist)
                (with-realm-write-lock (realm)
                  (setf (gethash name access-control-table) access-control-object))
                (values access-control-object t))
               (:error (error "There is no access-control named, ~A, in the realm, ~A."
                              name (realm-name realm))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.147")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod intern-group ((realm standard-realm-group-mixin) (name string) &key (if-does-not-exist :error)
                         (members nil members-supplied-p) &aux group-object)
  (declare (values interned-group newly-created-p))
  (with-slots (group-table) realm
    (cond ((setq group-object (gethash name group-table))
           (when members-supplied-p
             (udpate-users-and-groups group-object members if-does-not-exist))
           group-object)
          (t (ecase if-does-not-exist
               (:soft nil)
               (:create
                 (setq group-object (make-group realm name))
                 (with-realm-write-lock (realm)
                   (setf (gethash name group-table) group-object))
                 (when members-supplied-p
                   (udpate-users-and-groups group-object members if-does-not-exist))
                 (values group-object t))
               (:error (error "There is no group named, ~A, in the realm, ~A."
                              name (realm-name realm))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;AUTHENTICATION.LISP.147")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod intern-user ((realm standard-realm-user-mixin) (user string) 
                        &key (if-does-not-exist :error) password groups personal-name email-address 
                        &allow-other-keys
                        &aux user-object)
  (declare (values interned-user newly-created-p))
  (with-slots (user-table) realm
    (cond ((setq user-object (gethash user user-table))
           (cond-every
             (password (setf (user-password-digest user-object) (make-user-password-digest realm user password)))
             (groups
               (dolist (group groups)
                 (group-add-user (intern-group realm group :if-does-not-exist :create) user-object)))
             (email-address (setf (user-email-address user-object) email-address))
             (personal-name (setf (user-personal-name user-object) personal-name)))
           user-object)
          (t (ecase if-does-not-exist
               (:soft nil)
               (:create 
                 (setq user-object (make-user realm user password email-address personal-name))
                 (dolist (group groups)
                   (group-add-user (intern-group realm group :if-does-not-exist :create) user-object))
                 (with-realm-write-lock (realm)
                   (setf (gethash user user-table) user-object))
                 (values user-object t))
               (:error (error "There is no user, ~S, in the realm, ~S." user (realm-name realm))))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;DATA-CACHE.LISP.100")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: HTTP -*-")

(defmethod recache-data ((data-cache basic-data-cache) &optional stream length)
  (declare (values data-array))
  (unless (and stream length)
    (error "No data stream and data size were provided."))
  (let ((new-array (allocate-resource 'data-cache-array length))
	(ut (get-universal-time))
	old-array old-size offset)
    ;; obtain new data
    (setq new-array (binary-stream-copy-into-8-bit-array stream length 0 new-array))
    ;; Use lock only around the actual update to minimize latency
    (with-data-cache-lock (data-cache :write)
      (setf old-array (data-cache-array data-cache)
	    old-size (data-cache-size data-cache)
            (data-cache-array data-cache) new-array
	    (data-cache-size data-cache) length
	    (data-cache-update-time data-cache) ut)
      (data-cache-reset-revalidation-time data-cache ut))
    (cond-every
      (old-array
	(deallocate-resource 'data-cache-array old-array))
      ;; Update cache indices
      ((and old-size (not (zerop (setq offset (- length old-size)))))
       (data-universe-increment-total-cache-size (data-cache-universe data-cache) offset)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;DATA-CACHE.LISP.100")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: HTTP -*-")

(defmethod recache-data-universe-as-necessary ((universe file-data-universe) &optional (universal-time (get-universal-time))
					       &aux next-revalidation)
  (flet ((maybe-recache-item (pathname data-cache)
           (unless (crlf-pathname-p pathname) 
	     (recache-if-necessary data-cache nil universal-time)
	     (setq next-revalidation (if next-revalidation
					 (min next-revalidation (data-cache-revalidation-time data-cache))
					 (data-cache-revalidation-time data-cache))))))
    (declare (dynamic-extent #'maybe-recache-item))
    (with-data-universe-lock (universe :write) 
      (map-data-caches universe #'maybe-recache-item))
    next-revalidation))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;DATA-CACHE.LISP.100")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: HTTP -*-")

;; Make sure we have a data array, but don't get caught in an infinite
;; loop if there is an error caching the data. This is designed to
;; minimize latency on the front end in recache-date by allowing it to
;; deallocate the old array without waiting for any users to finish.
;; The read lock here assures that the resetter waits his turn before
;; updating the instance variable.
(defmethod write-cache-data ((data-cache basic-data-cache) stream &optional (start 0) end)
  (loop with array
        repeat 3
        doing (with-data-cache-lock (data-cache :read)	;protects an reader from an update
                (when (setq array (data-cache-array data-cache))
                  (binary-stream-copy-from-8-bit-array array stream start end)
                  (return-from write-cache-data)))
              (with-data-cache-lock (data-cache :write)
                (unless (data-cache-array data-cache)
                  (recache-data data-cache)))
        finally (error "Failed to recache missing data for ~S." data-cache)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;DATA-CACHE.LISP.100")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: HTTP -*-")

(defmethod clear-data-universe ((universe basic-data-universe))
  (flet ((uncache-item (key data-cache)
           (declare (ignore key))
           (unregister-data universe data-cache)))
    (declare (dynamic-extent #'uncache-item))
    (with-data-universe-lock (universe :write)
      (map-data-caches universe #'uncache-item)
      (map-data-auditors universe #'uncache-item))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;DATA-CACHE.LISP.100")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: HTTP -*-")

(defmethod ensure-current-data ((universe basic-data-universe))
  (flet ((ensure-fresh-item (key data-cache)
			    (declare (ignore key))
			    (ensure-current-data data-cache)))
    (with-data-universe-lock (universe :write)
      (map-data-caches universe #'ensure-fresh-item))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;DATA-CACHE.LISP.100")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: HTTP -*-")

(defmethod recache-data-universe ((universe file-data-universe))
  (flet ((recache-item (pathname data-cache)
           (unless (crlf-pathname-p pathname)
	     (recache-data data-cache))))
    (with-data-universe-lock (universe :write)
      (map-data-caches universe #'recache-item))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.179")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod close-log-stream ((log file-logging-mixin))
  (with-log-write-lock (log)
    (%close-log-stream log)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.179")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod log-stream ((log file-logging-mixin))
  (with-slots (filename file-stream) log
    (unless (and file-stream (open-stream-p file-stream))
      (with-log-write-lock (log)
        (unless (open-log-stream-p log)
          (%log-stream log))))
    file-stream))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.179")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod switch-log-file ((log file-logging-mixin))
  (with-slots (filename lock) log
    (www-utils:with-lock-held (lock :write)
      (%close-log-stream log)
      (when (probe-file filename)
        (let ((new-name (log-date-pathname log)))
          (pathname-create-directory-if-needed new-name)
          (rename-file filename new-name))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.803")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-access-log-entry-to-file ((log file-logging-mixin) (server server-logging-mixin))
  ;; This form is executed atomically with interprocess locking.
  ;; When log-file-stream-stays-open is turned on, entries are written to 
  ;; disk only after the stream buffer is filled.
  ;; While this reduces disk access, it can lose up to a buffer's worth of logs
  ;; in the event of a machine crash. FORCE-OUTPUT here would handle this
  (with-slots (log-times-in-gmt-p) log
    (with-log-stream (log)
      (write-access-log-entry log server log-stream log-times-in-gmt-p)
      ;; Trailing CR makes this consistently parsable.
      (terpri log-stream))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.24")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod clear-task-queue ((task-queue task-queue))
  (with-slots (lock queue pointer n-pending-tasks) task-queue
    (with-lock-held (lock :write "Task Queue Clear")
      (prog1 queue
             (setq queue nil
                   pointer nil
                   n-pending-tasks 0)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.24")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod pop-task-queue ((task-queue task-queue) &aux entry)
  (with-slots (lock queue pointer n-pending-tasks) task-queue
    (with-lock-held (lock :write "Task Queue Pop")
      (when (setq entry (pop queue))
        (decf (the fixnum n-pending-tasks))
        (unless queue
          (setq pointer nil))))
    entry))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.24")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod push-task-queue ((task-queue task-queue) task)
  (with-slots (lock queue pointer n-pending-tasks) task-queue 
    (let ((entry (list task)))
      (with-lock-held (lock :write "Task Queue Push")
        (if queue
            (setf (cdr pointer) entry)
            (setq queue entry))
        (setq pointer entry)
        (incf (the fixnum n-pending-tasks))))))

