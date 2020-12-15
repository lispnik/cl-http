;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-

;;; (C) Copyright 1995-97, Christopher R. Vincent and John C. Mallery
;;;     All Rights Reserved.
;;;
;;;
;;;------------------------------------------------------------------- 
;;;
;;; SERVER PREFERENCE CONFIGURATION TOOL
;;;

(in-package :http)


;;;------------------------------------------------------------------- 
;;;
;;; PREFERENCE TYPE
;;;

(defmethod print-object ((preference preference) stream)
  (with-slots (keyword) preference
    (print-unreadable-object (preference stream :type t :identity t)
      (when (slot-boundp preference 'keyword)
        (write (symbol-name keyword) :escape nil :stream stream)))))

(defmethod print-object ((preference-type preference-type) stream)
  (with-slots (name) preference-type
    (print-unreadable-object (preference-type stream :type t :identity t)
      (when (slot-boundp preference-type 'name)
        (write name :stream stream :escape nil)))))

(defvar *preference-type-table* (make-hash-table)
  "preference-type objects stored in hash table by keyword")

(defvar *preference-type-tables* nil
  "All known preference-type tables.")

(defun intern-preference-type-table (keyword &optional (if-does-not-exist :error))
  (flet ((%create-preference-type-table (keyword)
           (let ((table (make-hash-table :test #'equal)))
             (push `(,(intern-keyword (string keyword)) . ,table) *preference-type-tables*)
             (values table t))))
    (let ((entry (assoc keyword *preference-type-tables* :test #'eql)))
      (cond (entry (cdr entry))
            (t (ecase if-does-not-exist
                 (:create
                   (%create-preference-type-table keyword))
                 (:soft nil)
                 (:error (error "there is no preference-type table named, ~S." keyword))))))))

(defun unintern-preference-type-table (keyword)
  (setq *preference-type-tables* (delete keyword *preference-type-tables* :key #'car)))

(define-macro with-preference-type-table ((keyword) &body body)
  "Binds the current preference-types table to KEYWORD within the scope of BODY."
  `(let ((*preference-type-table* (intern-preference-type-table ,keyword)))
     (declare (special *preference-type-table*))
     ,@body))

(defun find-preference-type (keyword &optional (errorp t))
  (let ((object (etypecase keyword
                  (keyword (gethash keyword *preference-type-table*))
                  (preference-type
                    (when (gethash (preference-type-keyword keyword) *preference-type-table*)
                      keyword)))))
    (cond (object)
          (errorp (error "There is no Preference-Type named ~S." keyword))
          (t nil))))

(defmethod %register-preference-type ((preference-type preference-type))
  (unless (find-preference-type preference-type nil)
    (setf (gethash (preference-type-keyword preference-type) *preference-type-table*) preference-type))
  preference-type)

(defmethod initialize ((preference-type preference-type))
  (%register-preference-type preference-type)
  preference-type)

(defmethod intern-preference-type ((keyword symbol) &key (if-does-not-exist :error))
  (or (find-preference-type keyword nil)
      (ecase if-does-not-exist
        (:soft nil)
        (:create (initialize (make-instance 'preference-type :keyword keyword)))
        (:error (error "There is no preference-type named, ~S." (symbol-name keyword))))))

(defmethod intern-preference-type ((preference-type preference-type) &key (if-does-not-exist :error))
  (with-slots (keyword) preference-type
    (cond ((find-preference-type keyword nil)
           preference-type)
          (t (ecase if-does-not-exist
               (:soft nil)
               (:error (error "There is no preference-type named, ~S." (symbol-name keyword))))))))

(defmethod unintern-preference-type (preference-type)
  (let ((obj (find-preference-type preference-type nil)))
    (when obj
      (dolist (inf (preference-type-inferiors obj))
        (remove-inferior obj inf))
      (remhash (preference-type-keyword preference-type) *preference-type-table*))))

(eval-when (:load-toplevel :execute :compile-toplevel)

(defun %define-preference-type (keyword name display-string description &optional inferiors)
  (let ((preference-type (intern-preference-type keyword :if-does-not-exist :create)))
    (setf (preference-type-keyword preference-type) keyword
          (preference-type-name preference-type) name
          (preference-type-inferiors preference-type) (mapcar #'(lambda (x)
                                                                  (intern-preference x :if-does-not-exist :create))
                                                              inferiors)
          (preference-type-display-string preference-type) display-string
          (preference-type-description preference-type) description)
      
    (%register-preference-type preference-type)))

(defmacro define-preference-type (keyword &key name display-string description inferiors)
  `(%define-preference-type ',keyword ',name ',display-string ',description ',inferiors)))

(defmethod add-inferior ((preference-type preference-type) (preference preference))
  (with-slots (inferiors) preference-type
    (with-slots (superiors) preference
      (unless-every
        ((member preference inferiors)
         (setq inferiors (nconc inferiors (list preference))))
        ((member preference-type superiors)
         (setq superiors (nconc superiors (list preference-type)))))
      preference)))

(defmethod remove-inferior ((preference-type preference-type) (preference preference))
  (with-slots (inferiors) preference-type
    (with-slots (superiors) preference
      (setf inferiors (delete preference inferiors)
            superiors (delete preference-type superiors))))
  preference)


;;;------------------------------------------------------------------- 
;;;
;;; PREFERENCE CLASS
;;;

(defvar *preference-table* (make-hash-table)
  "Preference objects stored in hash table by keyword")

(defvar *preference-tables* nil
  "All known preference tables.")

(defun intern-preference-table (keyword &optional (if-does-not-exist :error))
  (flet ((%create-preference-table (keyword)
           (let ((table (make-hash-table :test #'equal)))
             (push `(,(intern-keyword (string keyword)) . ,table) *preference-tables*)
             (values table t))))
    (let ((entry (assoc keyword *preference-tables* :test #'eql)))
      (cond (entry (cdr entry))
            (t (ecase if-does-not-exist
                 (:create
                   (%create-preference-table keyword))
                 (:soft nil)
                 (:error (error "there is no preference table named, ~S." keyword))))))))

(defun unintern-preference-table (keyword)
  (setq *preference-tables* (delete keyword *preference-tables* :key #'car)))

(define-macro with-preference-table ((keyword) &body body)
  "Binds the current preferences table to KEYWORD within the scope of BODY."
  `(let ((*preference-table* (intern-preference-table ,keyword)))
     (declare (special *preference-table*))
     ,@body))

(defun find-preference (keyword &optional (errorp t))
  (let ((object (etypecase keyword
                  (keyword (gethash keyword *preference-table*))
                  (preference
                    (when (gethash (preference-keyword keyword) *preference-table*)
                      keyword)))))
    (cond (object)
          (errorp (error "There is no Preference named ~S." keyword))
          (t nil))))

(defmethod %register-preference ((preference preference))
  (unless (find-preference preference nil)
    (setf (gethash (preference-keyword preference) *preference-table*) preference))
  preference)

(defmethod initialize ((preference preference))
  (%register-preference preference)
  preference)

(defmethod intern-preference ((keyword symbol) &key (if-does-not-exist :error))
  (or (find-preference keyword nil)
      (ecase if-does-not-exist
        (:soft nil)
        (:create (initialize (make-instance 'preference :keyword keyword)))
        (:error (error "There is no preference named, ~S." (symbol-name keyword))))))

(defmethod intern-preference ((preference preference) &key (if-does-not-exist :error))
  (with-slots (keyword) preference
    (cond ((find-preference keyword nil)
           preference)
          (t (ecase if-does-not-exist
               (:soft nil)
               (:error (error "There is no preference named, ~S." (symbol-name keyword))))))))

(defmethod unintern-preference (preference)
  (let ((obj (find-preference preference nil)))
    (when obj
      (dolist (sup (preference-superiors obj))
        (remove-inferior sup obj))
      (remhash (preference-keyword preference) *preference-table*))))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(eval-when (:load-toplevel :execute :compile-toplevel)
  (defun %define-preference (keyword name presentation-type prompt default-value-function
                                     get-value-function set-value-function set-value-form
                                     description superiors)
    (let ((preference (intern-preference keyword :if-does-not-exist :create)))
      (setf (preference-name preference) keyword
            (preference-name preference) name
            (preference-prompt preference) prompt
            (preference-default-value-getter preference) default-value-function 
            (preference-value-getter preference) get-value-function
            (preference-value-setter preference) set-value-function
            (preference-value-setter-source preference) set-value-form
            (preference-presentation-type preference) presentation-type
            (preference-description preference) description)
      (dolist (sup superiors)
        (add-inferior (intern-preference-type sup) preference))
      (%register-preference preference)))

  (defmacro define-preference (keyword &key name presentation-type prompt
                               default-value-form get-value-form set-value-form
                               superiors
                               description)
    `(%define-preference ,keyword ,name ,presentation-type ,prompt
                         #'(lambda () ,default-value-form)
                         #'(lambda () ,get-value-form)
                         #'(lambda (value) ,set-value-form)
                         ',set-value-form
                         ,description ',superiors)))

(define-generic set-active-preference (preference value)
  (:documentation "Sets the active value for PREFERENCE to VALUE in the running server."))

(defmethod set-active-preference ((preference preference) value)
  (funcall (preference-value-setter preference) value))

(define-generic preference-query-identifier (preference)
  (:documentation "Returns a string suitable for use as a query identifier with W3P."))

(defmethod preference-query-identifier ((preference preference))
  (symbol-name (preference-keyword preference)))

(define-generic write-preference (preference stream)
  (:documentation "Writes a form to STREAM that will initialize PREFERENCE to its current value."))

(defmethod write-preference ((preference preference) stream)
  (let* ((value (funcall (preference-value-getter preference)))
         (form `(let ((value ,(typecase value
                                ((or symbol cons) `(quote ,value))
                                (t value))))
                  ,(preference-value-setter-source preference))))
    (declare (dynamic-extent))
    (write form :pretty t :readably t :base 10. :stream stream)))
                                                
;;;------------------------------------------------------------------- 
;;;
;;; DEFINE THE CONFIGURATION PARAMETERS FOR THE SERVER
;;;

(define-preference :accept-write-methods
                   :name "Accept Write Methods"
                   :presentation-type `(w3p:member-sequence (:access-controlled :authenticated-users
                                                             :authenticated-users-on-secure-subnets
                                                             :local-host :none :remote-host :secure-subnets))
                   :prompt "Write Methods Accepted From Hosts:"
                   :default-value-form '(:local-host)
                   :get-value-form *accept-write-methods*
                   :set-value-form (setq *accept-write-methods* value)
                   :description "Controls the security policy for side-effecting methods such
as PUT or DELETE.  Each security policy imposes minimum requirements to invoke these methods. 
The following security policies are available.
ACCESS-CONTROLLED: Requires URLs to restrict users via either user authentication or subnet security.
AUTHENTICATED-USERS: Requires URLs to restrict users only via user authentication.
AUTHENTICATED-USERS-ON-SECURE-SUBNETS: Requires URLs to restrict users via both user authentication 
and subnet security.
LOCAL-HOST: Requires users to be on the local host running the server.
NONE: No users can invoke side-effecting methods.
REMOTE-HOST: Does require URLs to control access, but respects any global or URL level access controls.
SECURE-SUBNETS: Requires URLs to restrict access to trusted hosts.")

(define-preference :authentication-data-pathname
                   :name "Authentication Data Pathname"
                   :presentation-type 'w3p:pathname
                   :prompt "Authentication Data Pathname:"
                   :default-value-form (pathname "http:pw;authentication-data.lisp")
                   :get-value-form *authentication-data-pathname*
                   :set-value-form (setq *authentication-data-pathname* value)
                   :description "The pathname holding the disk representation of server authentication 
information as lisp source code.")

(define-preference :auto-export
                   :name "Auto-Export"
                   :presentation-type '(w3p:member-sequence (:on-demand :export))
                   :prompt "Export URLs on demand or all at export time?"
                   :default-value-form :on-demand
                   :get-value-form *auto-export*
                   :set-value-form (setq *auto-export* value)
                   :description "When :ON-DEMAND, static URLs are exported the first time they are accessed.  
When :EXPORT, all of this work is done when a superior directory is exported.")

(define-preference :bug-list
                   :name "Bug Mailing List"
                   :presentation-type '(w3p:bounded-string 32)
                   :prompt "Bug Mailing List:"
                   :default-value-form "Bug-CL-HTTP"
                   :get-value-form *server-bug-list*
                   :set-value-form (setq *server-bug-list* value)
                   :description "The standard mailing list for server bug reports.")

(define-preference :config-pathname
                   :name "Configuration File Pathname"
                   :presentation-type 'w3p:basic-string
                   :prompt "Configuration file pathname:"
                   :default-value-form "configuration.lisp"
                   :get-value-form *standard-configuration-pathname*
                   :set-value-form (setq *standard-configuration-pathname* value)
                   :description "When configurations are saved they are written to this file.")

#+MCL
(define-preference :host-name-for-apple-talk-operation
                   :name "Host Name for AppleTalk"
                   :presentation-type '(or null string) ; should accept a domain here -- JCMa 6/30/1996.
                   :prompt "Local Host Domain Name for AppleTalk Operation"
                   :default-value-form nil
                   :get-value-form *host-name-for-apple-talk-operation*
                   :set-value-form (www-utils:initialize-apple-talk-configuration value *resolve-ip-addresses*)
                   :description "This set the domain name for the local host during AppleTalk Operation.
This field should be None when the server operates normally over a TCP/IP network with DNS service.")
  
(define-preference :http-port
                   :name "Standard HTTP Port"
                   :presentation-type '(integer 0 9999)
                   :prompt "Standard HTTP port:"
                   :default-value-form 80
                   :get-value-form *standard-http-port*
                   :set-value-form (set-standard-http-port value)
                   :description "The standard port for HTTP connections.")

#+ccl-3
(declaim (special *number-of-listening-processes*))

#+ccl-3
(define-preference :listening-processes
                   :name "Number of Listening Processes"
                   :presentation-type '(integer 1 50)
                   :prompt "Number of Listening Processes:"
                   :default-value-form 5
                   :get-value-form *number-of-listening-processes*
                   :set-value-form (setq *number-of-listening-processes* value)
                   :description "The number of threads simultaneously listening for HTTP connections.")

(define-preference :log-class
                   :name "Log Access Log Class"
                   :presentation-type '(w3p:member-sequence (common-file-log  extended-common-file-log http-log extended-http-log))
                   :prompt "Which  kind of log should  access logging use?"
                   :default-value-form 'common-file-log
                   :get-value-form *log-access-log-class*
                   :set-value-form (setq *log-access-log-class* value)
                   :description "Controls the class of log used for logging.")

(define-preference :log-directory
                   :name "Standard Log Directory"
                   :presentation-type 'w3p:bounded-string
                   :prompt "Standard Log Directory:"
                   :default-value-form "LOG;"
                   :get-value-form *standard-log-directory*
                   :set-value-form (setq *standard-log-directory* value)
                   :description "Common logs are written to the merge of this directory and the local host.")

(define-preference :log-file-stream-stays-open
                   :name "Log File Stream Stays Open"
                   :presentation-type 'w3p:boolean
                   :prompt "Keep stream to log open?"
                   :default-value-form nil
                   :get-value-form *log-file-stream-stays-open*
                   :set-value-form (log-file-stream-stays-open value)
                   :description "Controls whether the file log stream remains open all the time, or if it is 
reopenned for each log transaction. Production servers may wish to keep the log file stream open.")

(define-preference :log-notifications
                   :name "Log Notifications On"
                   :presentation-type 'w3p:boolean
                   :prompt "Show transactions in a log window?"
                   :default-value-form t
                   :get-value-form (loop for item in (current-access-logs)
                                         when (and item (log-notification item))
                                           return t
                                         finally (return nil))
                   :set-value-form (mapc #'(lambda (x) (log-notifications-on x value)) (current-access-logs))
                   :description "Show transactions in a log window.  Turn this off in production servers 
to save time writing log entries to the notification window.")

(define-preference :log-resolve-ip
                   :name "Resolve Log IP Addresses"
                   :presentation-type 'w3p:boolean
                   :prompt "Resolve IP addresses when writing log entries?"
                   :default-value-form nil
                   :get-value-form *log-resolve-ip-addresses* 
                   :set-value-form (setq *log-resolve-ip-addresses*  value)
                   :description "Controls whether IP addresses are resolved when writing log entries.
Production servers should turn this off to avoid the overhead of DNS lookup during logging.")

(define-preference :log-times-in-gmt
                   :name "Log Times in GMT"
                   :presentation-type 'w3p:boolean
                   :prompt "Write log entries in Greenwich Mean Time?"
                   :default-value-form t
                   :get-value-form *log-times-in-gmt*
                   :set-value-form (setq *log-times-in-gmt* value)
                   :description "Controls whetherthe times in log file entries are written in Greenwich 
Mean Time or not.")

(define-preference :mail-host
                   :name "Mail Host"
                   :presentation-type '(w3p:bounded-string 60)
                   :prompt "Mail Host:"
                   :default-value-form *default-mailer-host*
                   :get-value-form *default-mailer-host*
                   :set-value-form (setq *default-mailer-host* value)
                   :description "The default mail host to use in return address when sending email.")

(define-preference :maintainer 
                   :name "Server Maintainer"
                   :presentation-type '(w3p:bounded-string 32)
                   :prompt "Maintainer User Name:"
                   :default-value-form "WebMaster"
                   :get-value-form *server-maintainer*
                   :set-value-form (setq *server-maintainer* value)
                   :description "A user name or mailing list of people maintaining the server.")

(define-preference :max-connections
                   :name "Maximum Number of Connections"
                   :presentation-type '(integer 1 50)
                   :prompt "Maximum number of simultaneous connections:"
                   :default-value-form 20
                   :get-value-form *maximum-number-of-connections*
                   :set-value-form (set-maximum-number-of-connections value)
                   :description "This controls the maxium number of HTTP connections allowed simultaneously.")

#+CCL-3
(define-preference :network-mail-host 
                   :name "Newtork Mail Host"
                   :presentation-type '(w3p:null-or-type (w3p:bounded-string 60))
                   :prompt "Primary store and forward mail host:"
                   :default-value-form nil
                   :get-value-form smtp:*network-mail-host*
                   :set-value-form (setq smtp:*network-mail-host*  value)
                   :description "The primary store and forward mail host at the local site.This is the domain name of the mail host. 
It may also by the IP address.  If None is entered, no mail will be sent by functions REPORT-BUG and SEND-MAIL-FROM.")

(define-preference :resolve-ip 
                   :name "Resolve IP Addresses"
                   :presentation-type 'w3p:boolean
                   :prompt "Resolve IP addresses in all contexts other than logging?"
                   :default-value-form t
                   :get-value-form *resolve-ip-addresses*
                   :set-value-form (setq *resolve-ip-addresses* value)
                   :description "Controls whether IP addresses are resolved in all contexts other than logging.")

;; needs a subnet presentation type.   -cvince 1/6/97
(define-preference :secure-subnets
                   :name "Secure Subnets"
                   :presentation-type '(w3p:null-or-type (w3p:sequence (w3p:bounded-string 15)))
                   :prompt "Secure Subnets:"
                   :default-value-form nil
                   :get-value-form (mapcar #'ip-address-for-parsed-ip-address *secure-subnets*)
                   :set-value-form (parse-secure-subnets value)
                   :description "Defines default subnet security for all URLs exported by this server.  Should be a comma-separated
list of IP addresses or None.")

(define-preference :standard-export-pathnames
                   :name "Standard Export Pathnames"
                   :presentation-type '(w3p:null-or-type (w3p:sequence w3p:existing-pathname)) 
                   :prompt "Export Pathnames to Load at Server Initialization:"
                   :default-value-form nil
                   :get-value-form *standard-export-pathnames*
                   :set-value-form (setq *standard-export-pathnames* value)
                   :description "The standard pathnames to load on server initialization that export URLs.
The value should be either None or a comma-separated list of pathnames.  This interface will be 
exported automatically.")

#+CCL-3
(define-preference :store-and-forward-mail-hosts
                   :name "Accessible Store and Forward Mailers"
                   :presentation-type '(w3p:null-or-type (w3p:sequence (w3p:bounded-string 60)))
                   :prompt "Other store and forward mail hosts:"
                   :default-value-form nil
                   :get-value-form smtp:*store-and-forward-mail-hosts*
                   :set-value-form (setq smtp:*store-and-forward-mail-hosts*  value)
                   :description "This is a list of all store and forward mail hosts accessible from the site.  
These mail hosts will be used whenever the primary mail host is inaccessible.  
Mail hosts should be listed in decreasing order of priority.
Should be either None or a comma-separated list of hosts.")

(define-preference :url-host-name-resolution
                   :name "URL Host Name Resolution"
                   :presentation-type '(w3p:member-sequence (:always :preferred :never))
                   :prompt "When should host names be resolved on the primary DNS name when interning a URL?"
                   :default-value-form :always
                   :get-value-form url:*url-host-name-resolution*
                   :set-value-form (setq url:*url-host-name-resolution* value)
                   :description "Controls whether host names are DNS resolved and connonicalized on
the primary DNS name for the host when the URL is interned. Choices
are :always, :preferred, :never. Relevant when *resolve-ip-addresses* is non-null.")

(define-preference :write-config-file
                   :name "Write Configuration File"
                   :presentation-type 'w3p:boolean
                   :prompt "Write this configuration to a file?"
                   :default-value-form nil
                   :get-value-form *standard-configuration-write-file-p*
                   :set-value-form (setq *standard-configuration-write-file-p* value)
                   :description "When non-null, the Lisp code for a remote configuration is saved to a file.")


;;;------------------------------------------------------------------- 
;;;
;;; DEFINE THE KINDS OF CONFIGURATION PARAMETERS USED BY THE SERVER
;;;
;;; Note that each preference can have only a single superior or else
;;; duplicate questions will be generated, resulting in errors.   3/15/97 -- JCMa.

(define-preference-type :configuration-preferences
                        :name "Configuration"
                        :display-string "Configuration"
                        :inferiors (:config-pathname)
                        :description "Parameters related to server configuration.")

(define-preference-type :dns-preferences
                        :name "DNS"
                        :display-string "Domain Name Service"
                        :inferiors (#+MCL :host-name-for-apple-talk-operation
                                    :log-resolve-ip :resolve-ip :url-host-name-resolution)
                        :description "Parameters related to Domain Name Service.")

(define-preference-type :http-preferences
                        :name "HTTP"
                        :display-string "HTTP Service"
                        :inferiors (:http-port #+ccl-3 :listening-processes :max-connections)
                        :description "Parameters related to HTTP service.")

(define-preference-type :logging-preferences
                        :name "Logging"
                        :display-string "Server Logging"
                        :inferiors (:log-class :log-directory :log-file-stream-stays-open
                                               :log-notifications :log-times-in-gmt)
                        :description "Parameters related to server logging.")

(define-preference-type :mail-preferences
                        :name "Mail"
                        :display-string "Mail"
                        :inferiors (:mail-host :bug-list :maintainer
                                               #+CCL-3 :network-mail-host
                                               #+CCL-3 :store-and-forward-mail-hosts)
                        :description "Parameters related to electronic mail.")

(define-preference-type :security-preferences
                        :name "Security"
                        :display-string "Security & Authentication"
                        :inferiors (:accept-write-methods :authentication-data-pathname :secure-subnets)
                        :description "Parameters related to user authentication, access control, and security.")

(define-preference-type :uri-preferences
                        :name "URI"
                        :display-string "Uniform Resource Indicators"
                        :inferiors (:auto-export :standard-export-pathnames)
                        :description "Parameters related to uniform resource indicators.")
