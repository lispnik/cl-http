;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-

;;; Copyright John C. Mallery,  1994-1999.
;;; All rights reserved.
;;;
;;; CL-HTTP CLASSES
;;; 
;;; Contains all the class definitions for the HTTP package.
;;; Differing copyrights are indicated in some code blocks.

(in-package :http)

;;;------------------------------------------------------------------- 
;;;
;;;  PROPERTY LIST MIXIN FOR OBJECTS
;;;

(defclass property-list-mixin
          ()
    ((plist :initform nil :initarg :property-list :accessor property-list))
  (:documentation "A property list mixin for classes."))

;;;------------------------------------------------------------------- 
;;;
;;; HTTP VERSION
;;;

(defclass http-protocol
          ()
    ((keyword :allocation :class :reader protocol-version )
     (protocol :initform :http :reader protocol)
     (major-version :allocation :class :reader major-version)
     (minor-version :allocation :class :reader minor-version)))

(defclass http/0.9
          (http-protocol)
    ((keyword :initform :http/0.9 )
     (major-version :initform 0)
     (minor-version :initform 9)))

(defclass http/1.0
          (http/0.9)
    ((keyword :initform :http/1.0)
     (major-version :initform 1)
     (minor-version :initform 0)))

(defclass http/1.1
          (http/1.0)
    ((keyword :initform :http/1.1)
     (major-version :initform 1)
     (minor-version :initform 1)))


;;;------------------------------------------------------------------- 
;;;
;;; HEADERS
;;;

(defclass entity-tag
          ()
    ((value :initarg :value :accessor entity-tag-value)))

(defclass wild-entity-tag
          (entity-tag)
    ()
  (:documentation "A wild entity tag that matches any entity tag." ))

(defclass weak-entity-tag
          (entity-tag)
    ()
  (:documentation "A weak entity tag changes only when there is semantic change in the entity." ))

(defclass strong-entity-tag
          (entity-tag)
    ()
  (:documentation "A strong entity tag changes whenever any the entity changes in any way." ))

;; Track buffered header positions with structure
(defstruct (header-position)
  (start 0 :type fixnum)
  (end 0 :type fixnum)
  (next nil))

;; Enhance the data structure for lazy header evaluation.
(defclass header
          ()
    ((keyword :initform nil :initarg :keyword :accessor header-keyword)
     (raw-value :initform nil :initarg :raw-value :accessor %header-raw-value)
     (value :initarg :value :accessor %header-value)))

;; Header specialization that uses header buffers
(defclass buffered-header 
          (header)
    ((buffer :initarg :buffer :accessor %header-buffer)	;buffer where raw data stored
     (raw-value-position :initarg :raw-value-position :accessor %header-raw-value-position)	;positional pointers into buffer
     (suppress-p :initarg :suppress-p :accessor %header-suppress-p)))	;suppress printing?

;; Data structure resourced header buffers.
(defclass header-set
	  ()
    ((buffer :initarg :buffer :accessor %header-set-buffer)	;buffer storing raw headers
     (line-ends :initarg :line-ends :accessor %header-set-line-ends)	;vector of line limits
     (index :initarg :index :accessor %header-set-index)))	; (index-vector . header-vector)

;;;------------------------------------------------------------------- 
;;;
;;; HOST NAMESPACE
;;;
(in-package :http)

(defclass host
          ()
    ((domain-name :initform nil :initarg :domain-name)
     (ip-address :initform nil :initarg :ip-address :reader host-ip-address)
     (object :initform nil :initarg :object)
     (http-version :initform nil :initarg :http-version :accessor host-http-version)
     (http-transactions :initform nil :reader host-http-transactions)
     (http-n-transactions :initform 0 :reader http-n-transactions)))

(defclass host-name-space
          ()
    ((local-host :initform nil :initarg :local-host :reader host-name-space-local-host)
     (host-table :initform nil)))

;;;------------------------------------------------------------------- 
;;;
;;; IMAGE-MAPS
;;;
;;; (C) Copyright 1995, Christopher R. Vincent
;;;     All Rights Reserved.

(defclass image-map-data
          ()
    ((url-default :initarg :url-default
                  :initform nil
                  :accessor im-url-default)
     (region-list :initarg :region-list
                  :initform nil
                  :accessor im-region-list))
  (:documentation "General image-map information."))

(defclass proximity-list-mixin
          ()
    ((proximity-list :initarg :proximity-list
                     :initform nil
                     :accessor im-proximity-list))
  (:documentation "Mixin for using points."))

(defclass cern-image-map
          (image-map-data) ()
  (:documentation "The CERN format uses only shapes."))

(defclass ncsa-image-map
          (image-map-data proximity-list-mixin) ()
  (:documentation "The NCSA format considers proximity to points if no
shape is selected."))

(defclass computed-image-map
          (image-map-data proximity-list-mixin) ()
  (:documentation "Allows image-map regions to call functions or URLs."))

(defclass point
          ()
    ((x :initarg :x
        :type integer
        :accessor x-of)
     (y :initarg :y
        :type integer
        :accessor y-of))
  (:documentation "Integer coordinates."))

(defclass shape
          ()
    ((point1 :initarg :point1
             :type point
             :accessor point1)
     (point2 :initarg :point2
             :type point
             :accessor point2))
  (:documentation "Defines the physical boundaries of a region."))

(defclass region
          ()
    ((destination :initarg :destination
                  :initform nil
                  :accessor destination)
     (bounding-shape :initarg :bounding-shape
                     :type shape
                     :accessor bounding-shape))
  (:documentation "Records information for a clickable region of an
image-map."))

(defclass rectangle
          (shape) ()
  (:documentation "Defined by a bounding rectangle."))

(defclass circle
          (shape)
    ((center :initarg :center
             :type point
             :accessor center)
     (radius :initarg :radius
             :type integer
             :accessor radius))
  (:documentation "Circle defined by center point and radius."))

(defclass polygon
          (shape)
    ((point-list :initarg :point-list
                 :initform nil
                 :accessor point-list))
  (:documentation "A polygon is defined by a list of its vertices."))

(defclass oval
          (shape) ()
  (:documentation "An oval is defined by its bounding rectangle."))


;;;------------------------------------------------------------------- 
;;;
;;; LOG CLASSES
;;;

(defclass basic-log-mixin
          ()
    ((name :initform "CL-HTTP-Log" :initarg :name :accessor log-name)
     (port :initform *standard-http-port* :initarg :port :accessor log-port)
     (creation-time :initform nil :reader log-creation-time)))

(defclass access-log (basic-log-mixin) ())

(defclass log-counters-mixin
          ()
    ((n-access-denials :initform 0 :accessor log-number-of-access-denials)
     (n-client-errors :initform 0 :accessor log-number-of-client-errors)
     (n-insufficient-resource-denials :initform 0 :accessor log-number-of-insufficient-resource-denials)
     (n-redirects :initform 0 :accessor log-number-of-redirects)
     (n-requests :initform 0 :accessor log-total-number-of-requests)
     (n-requests-served :initform 0 :accessor log-number-of-requests-served)
     (n-server-errors :initform 0 :accessor log-number-of-server-errors)
     (bytes-transmitted :initform 0 :accessor log-bytes-transmitted)
     (bytes-received :initform 0 :accessor log-bytes-received)
     (elapsed-time :initform 0 :accessor elapsed-time)  ;elapsed time in INTERNAL-TIME-UNITS-PER-SECOND
     (cpu-time :initform 0 :accessor cpu-time)  ;microseconds of compute time
     (n-connections :initform 0 :accessor log-http-connections)
     (n-deletes :initform 0 :accessor log-number-of-deletes)
     (n-gets :initform 0 :accessor log-number-of-gets)
     (n-heads :initform 0 :accessor log-number-of-heads)
     (n-options :initform 0 :accessor log-number-of-options)
     (n-extension-methods :initform 0 :accessor log-number-of-extension-methods)
     (n-posts :initform 0 :accessor log-number-of-posts)
     (n-puts :initform 0 :accessor log-number-of-puts)
     (n-traces :initform 0 :accessor log-number-of-traces)))

(defclass log-locking-mixin
          ()
    ((lock :initform nil :initarg :lock :accessor log-lock)))

(defclass file-logging-mixin
          (log-locking-mixin)
    ((log-file-name :initform "Log" :initarg :log-file-name :allocation :class)
     (file-logging :initform t :initarg :file-logging :accessor log-file-logging)
     (file-stream :initform nil)
     (filename :initform nil :initarg :filename :accessor log-filename)
     (log-times-in-gmt-p :initform *log-times-in-gmt* :initarg :log-times-in-gmt-p :accessor log-times-in-gmt-p)))

(defclass process-queued-logging-mixin
          (tq:task-queue)
    ()
  (:documentation "A mixin that writes log entries with a separate process."))

(defclass basic-process-queued-file-logging-mixin
          (process-queued-logging-mixin file-logging-mixin)
    ())

(defclass basic-file-logging-mixin
          (#+multi-threaded basic-process-queued-file-logging-mixin
           #-multi-threaded file-logging-mixin)
    ()
  (:documentation "Mixes in the appropriate file logging method
depending on whether the Lisp implementation is multi-threaded or not."))

(defclass dynamic-loggin-mixin
          (host-name-space)
    ((dynamic-logging :initform nil :initarg :dynamic-logging :accessor log-dynamic-logging)
     (n-transactions :initform 0 :reader log-n-transactions)
     (url-table :initform nil)))

(defclass extended-dynamic-loggin-mixin
          (dynamic-loggin-mixin)
    ()
  (:documentation "This dynamic logging class records accesses according to the Common File Format
but also records the referrer field and the user agent when they are present."))

(defclass log-notification-mixin
          ()
    ((notification :initform nil :initarg :notification :accessor log-notification)))

(defclass common-file-format-mixin
          ()
    ()
  (:documentation "Mixes in Common Log File Format logging."))

(defclass extended-common-file-format-mixin
          ()
    ()
  (:documentation "Mixes in the Extended Common Log File Format logging,
which includes the referrer and user agent fields.."))

(defclass basic-common-file-log
          (common-file-format-mixin access-log basic-file-logging-mixin)
    ()
  (:documentation "This log class records accesses according to the Common File Format,
but does not support counters or notifications. Useful as an ancillary log."))

(defclass basic-extended-common-file-log
          (extended-common-file-format-mixin access-log basic-file-logging-mixin)
    ()
  (:documentation "This log class records accesses according to the Common File Format
but also records the referrer field and the user agent when they are present,
but does not support counters or notifications. Useful as an ancillary log."))

(defclass common-file-log
          (common-file-format-mixin
            log-counters-mixin
            log-notification-mixin
            access-log
            basic-file-logging-mixin)
    ((log-file-name :initform "Common-Log" :initarg :log-file-name :allocation :class))
  (:documentation "This log class records accesses according to the Common File Format."))

(defclass extended-common-file-log
          (extended-common-file-format-mixin
            log-counters-mixin
            log-notification-mixin
            access-log
            basic-file-logging-mixin)
    ((log-file-name :initform "Ext-Common-Log" :initarg :log-file-name :allocation :class))
  (:documentation "This log class records accesses according to the Common File Format
but also records the referrer field and the user agent when they are present.
This will cons more than common-file-log because it must copy two header values."))

(defclass http-log
          (common-file-log dynamic-loggin-mixin)
    ((log-file-name :initform "Ext-HTTP-Log" :initarg :log-file-name :allocation :class))
  (:documentation "Combines standard common logfile format with window notifications and dynamic logging."))

(defclass extended-http-log
          (extended-common-file-log extended-dynamic-loggin-mixin)
    ()
  (:documentation "Combines extended common logfile format with window notifications and dynamic logging."))

(defclass http-post-file-format-mixin
          ()
    ()
  (:documentation "Mixes in HTTP POST Log File Format logging."))

(defclass post-log
          (http-post-file-format-mixin access-log basic-file-logging-mixin)
    ((log-file-name :initform "Post-Log" :initarg :log-file-name :allocation :class))
  (:documentation "This log class records HTTP POST values. 
Used as an ancillary log."))

(defclass notification-log
          (log-notification-mixin access-log)
    ()
  (:documentation "This log class notifies the console concerning HTTP activity."))

(defclass basic-url-metering
          (access-log)
    ()
  (:documentation "This log class meters every the response functions for every url.
It stores CPU time, elapsed time, and number of hits on the url property list."))

(defclass custom-notification-log
          (notification-log)
    ((predicate :initarg predicate :accessor notification-log-predicate)
     (notifier :initarg notifier :accessor notification-log-notifier))
  (:documentation "This log class notifies the console concerning HTTP activity when a predicate is true."))

(defclass transaction
          ()
    ((host :initarg :host :reader transaction-host)
     (user :initarg :user :reader transaction-user)
     (time-stamp :initarg :time :reader transaction-time-stamp)
     (method :initarg :method :reader transaction-method)
     (url :initarg :url :reader transaction-url)
     (server-version :initarg :server-version :reader transaction-server-version)
     (status :initarg :status :reader transaction-status)
     (bytes :initarg :bytes :reader transaction-bytes))
  (:documentation "An HTTP transaction with slots to hold all information obtainable with Common Log Format"))

(defclass extended-common-log-transaction
          (transaction)
    ((user-agent :initarg :user-agent :reader transaction-user-agent)
     (referrer :initarg :referrer :reader transaction-referrer))
  (:documentation "An HTTP transaction with slots to hold all information obtainable with Common Log Format
as well as the user agent and referrer URL.") )

;;;------------------------------------------------------------------- 
;;;
;;; CL-HTTP AUTHENTICATION
;;; 
;;; (C) Copyright 1995-97, John C. Mallery and Christopher R. Vincent.
;;;     All Rights Reserved.

(defclass realm
          ()
    ((user-class :initarg :user-class :allocation :class :reader realm-user-class)
     (group-class :initarg :group-class :allocation :class :reader realm-group-class)
     (access-control-class :initarg :access-control-class
                           :allocation :class :reader realm-access-control-class)
     (url-access-control-class :initarg :url-access-control-class
                               :allocation :class :reader realm-url-access-control-class)))

(defclass realm-name-scheme-mixin
          ()
    ((name :initform "No Name" :initarg :name :accessor realm-name)
     (scheme :allocation :class :reader realm-scheme)))

(defclass standard-lock-mixin
          ()
    ((lock :initarg :lock :accessor realm-lock)))

(defclass standard-realm-user-mixin
          ()
    ((user-table :initarg :user-table :accessor realm-user-table
                 :initform (make-hash-table :test #'equal :size *realm-user-table-size*))))

(defclass standard-realm-group-mixin
          ()
    ((group-table :initarg :group-table :accessor realm-group-table
                  :initform (make-hash-table :test #'equalp :size *realm-group-table-size*))))

(defclass standard-realm-access-control-mixin
          ()
    ((access-control-table :initarg :url-group-table :accessor realm-url-group-table
                           :initform (make-hash-table :test #'equalp :size *realm-access-control-table-size*))))
(defclass standard-realm 
          (standard-lock-mixin
            standard-realm-user-mixin
            standard-realm-group-mixin
            standard-realm-access-control-mixin
            realm-name-scheme-mixin realm)
    ()
  (:documentation "The standard authentication realm object."))

(defclass user (property-list-mixin) ()
  (:documentation "Basic class for a user in an authentication realm."))

(defclass user-email-address-mixin
          ()
    ((personal-name :initform nil :initarg :personal-name :accessor user-personal-name)
     (email-address :initform nil :initarg :email-address :accessor user-email-address))) 

(defclass authenticated-user
          ()
    ((realm :initarg :realm :accessor user-realm)
     (groups :initform nil :initarg :groups :accessor user-groups)))

(defclass user-password-mixin
          ()
    ;; user names are artifacts of particular authentication schemes.
    ((username :initarg :username :accessor user-name)
     ;; Store the username:password cookie as an MD5 digest
     (password-digest :initform nil :initarg :password-digest :accessor user-password-digest)))

(defclass standard-user
          (authenticated-user user-password-mixin user-email-address-mixin user)
    ())

(defclass group (property-list-mixin) ())

(defclass standard-group
          (group)
    ((name :initform "No Name" :initarg :name :accessor group-name)
     (realm :initarg :realm :accessor group-realm)
     (users :initform nil :initarg :users :accessor group-users)
     (superiors :initform nil :initarg :superiors :accessor group-superiors)
     (inferiors :initform nil :initarg :inferiors :accessor group-inferiors))
  (:documentation "Basic properties of a group of users in an authentication realm"))

(defclass access-control () ())

(defclass standard-access-control
          (access-control)
    ((name :initform "No Name" :initarg :name :accessor access-control-name)
     (realm :initarg :realm :accessor access-control-realm)
     (alist :initarg :alist :accessor access-control-alist)
     (default-groups :initform nil :initarg :default-groups :accessor access-control-default-groups)
     (default-users :initform nil :initarg :default-users :accessor access-control-default-users)))

(defclass url-access-control
          (access-control)
    ()
  (:documentation "An access control object created for a specific URL."))

(defclass standard-url-access-control
          (url-access-control standard-access-control)
    ()) 

(defclass basic-user-mixin () ())

(defclass basic-user (basic-user-mixin standard-user) ()) 

(defclass basic-group-mixin () ())

(defclass basic-group (basic-group-mixin standard-group) ())

(defclass basic-access-control-mixin () ())

(defclass basic-access-control (basic-access-control-mixin standard-access-control) ())

(defclass basic-url-access-control-mixin () ())

(defclass basic-url-access-control (basic-url-access-control-mixin standard-url-access-control) ())

(defclass basic-realm-mixin 
          ()
    ((scheme :initform :basic)
     (user-class :initform 'basic-user)
     (group-class :initform 'basic-group)
     (access-control-class :initform 'basic-access-control)
     (url-access-control-class :initform 'basic-url-access-control))
  (:documentation "Defines a realm using the HTTP basic authentication scheme."))

(defclass basic-realm 
          (basic-realm-mixin standard-realm)
    ()
  (:documentation "Defines a realm using the HTTP basic authentication scheme.")) 

(defclass digest-user-mixin () ())

(defclass digest-user (digest-user-mixin standard-user) ())

(defclass digest-group-mixin () ())

(defclass digest-group (digest-group-mixin standard-group) ())

(defclass digest-access-control-mixin () ())

(defclass digest-access-control (digest-access-control-mixin standard-access-control) ())

(defclass digest-url-access-control-mixin () ())

(defclass digest-url-access-control (digest-url-access-control-mixin standard-url-access-control) ())

(defclass digest-realm-mixin
          ()
    ((scheme :initform :digest)
     (user-class :initform 'digest-user)
     (group-class :initform 'digest-group)
     (access-control-class :initform 'digest-access-control)
     (url-access-control-class :initform 'digest-url-access-control))
  (:documentation "Defines a realm using the HTTP digest authentication scheme."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define algorithm-digest-function (algorithm)
    "Returns the digest function associated with ALGORITHM,
which is one of :MD5 or :SHA."
    (ecase algorithm
      (:md5 #'md5:md5-digest-hexadecimal-string)
      (:sha #'sha:sha-digest-hexadecimal-string)))
  )

(defclass digest-realm-algorithm-mixin
          ()
    ((algorithm :reader digest-realm-algorithm :allocation :class)
     (digest-function :reader digest-realm-digest-function :allocation :class))
  (:documentation "Mixes in the digest-function for digest realms."))

(defclass md5-algorithm-mixin
          (digest-realm-algorithm-mixin)
    ((algorithm :initform :md5 :allocation :class)
     (digest-function :initform  (algorithm-digest-function :md5) :allocation :class))
  (:documentation "Mixes in the MD5 digest digest-function for digest realms."))

(defclass sha-algorithm-mixin
          (digest-realm-algorithm-mixin)
    ((algorithm :initform :sha :allocation :class)
     (digest-function :initform  (algorithm-digest-function :sha) :allocation :class))
  (:documentation "Mixes in the SHA digest digest-function for digest realms."))

(defclass digest-realm
          (md5-algorithm-mixin digest-realm-mixin standard-realm)
    ()
  (:documentation "Defines a realm using the HTTP digest authentication scheme."))

(defclass digest-sha-realm
          (sha-algorithm-mixin digest-realm-mixin standard-realm)
    ()
  (:documentation "Defines a realm using the HTTP digest authentication scheme."))

;;;------------------------------------------------------------------- 
;;;
;;; SERVER CLASS
;;;

(defclass basic-server-mixin
          (property-list-mixin)
    ((stream :initarg :stream :accessor server-stream)
     (address :initarg :address :accessor server-address)
     (host :initarg :host :accessor server-host)
     (request :initform nil :accessor %server-request)
     (request-copy :initform nil :accessor server-request-copy)	;need for logging purposes
     (method :initform nil :accessor server-method)
     (url-buffer :initform nil :accessor server-url-buffer)
     (url-string :initform nil :accessor server-url-string)
     (url :initform nil :accessor server-url)
     (status :initform nil :reader server-status)       ;special accessor defined elsewhere
     (headers :initform nil :accessor server-headers)
     (form-data :initform nil :accessor server-form-data)
     (http-version :initform nil)
     (requests-completed :initform 0 :accessor server-requests-completed :type fixnum)
     (close-connection-p :initform nil :accessor server-close-connection-p)
     (persistent-connection-p :initform nil :accessor server-persistent-connection-p)
     (request-time :initform nil :accessor %server-request-time)        ;time of current request in universal time
     (timeout :initform 0 :accessor server-timeout)     ;maximum process idle time
     (life-time :initform 0 :accessor server-life-time) ;total life time
     (start-time :initform 0 :accessor server-start-time)       ;clock time in INTERNAL-TIME-UNITS-PER-SECOND
     (process :initform nil :accessor server-process)   ;process we running in
     (process-start-time :initform 0 :accessor server-process-start-time))      ;microsecond start for process.
  (:documentation "The essential components for an HTTP server."))

(defclass proxy-server-mixin () ()
  (:documentation "Provides proxy service capabilities."))

(defclass uri-resolver-mixin () ()
  (:documentation "Provides URI resolution service capabilities."))

(defclass server-logging-mixin
          ()
    ()
  (:documentation "Records information information necessary for logging server activity."))

(defclass server-authentication-mixin
          ()
    ((user-object :initform nil :accessor server-user-object)
     (rfc-931-response-type :initform nil :accessor server-rfc-931-response-type)
     (rfc-931-response :initform nil :accessor server-rfc-931-response))
  (:documentation "Mixin for authenticating users."))

(defclass server
          (uri-resolver-mixin proxy-server-mixin server-authentication-mixin server-logging-mixin basic-server-mixin)
    ()
  (:documentation "The HTTP server class."))


;;;------------------------------------------------------------------- 
;;;
;;; PREFERENCE TYPE
;;;
;;; (C) Copyright 1995-97, Christopher R. Vincent and John C. Mallery
;;;     All Rights Reserved.

(defclass preference ()
    ((keyword :initarg :keyword :accessor preference-keyword)
     (name :initarg :name :accessor preference-name)
     (superiors :initform nil :initarg :superiors :accessor preference-superiors)
     (presentation-type :initarg :presentation-type :accessor preference-presentation-type)
     (default-value-getter :initarg :default-value-getter :accessor preference-default-value-getter)
     (value-getter :initarg :value-getter :accessor preference-value-getter)
     (value-setter :initarg :value-setter :accessor preference-value-setter)
     (value-setter-source :initarg :value-setter-source :accessor preference-value-setter-source)
     (prompt :initarg :prompt :accessor preference-prompt)
     (description :initarg :description :accessor preference-description)))

(defclass preference-type ()
    ((keyword :initarg :keyword :accessor preference-type-keyword)
     (name :initarg :name :accessor preference-type-name)
     (inferiors :initarg :inferiors :accessor preference-type-inferiors)
     (display-string :initarg :display-string :accessor preference-type-display-string)
     (description :initarg :description :accessor preference-type-description)))
