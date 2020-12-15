;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-
;;;
;;; (C) Copyright 1995-99, John C. Mallery and Christopher R. Vincent.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CL-HTTP AUTHENTICATION
;;; 
;;; Branched off from authentication.lisp.79 and modularized for database storage.   1/24/96 -- JCMa.
;;;

(in-package :http)

;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defmethod print-object ((realm realm) stream)
  (print-unreadable-object (realm stream :type t :identity t)
    (write-string (realm-name realm) stream)))

(defmethod print-object ((user user) stream)
  (print-unreadable-object (user stream :type t :identity t)
    (write-string (user-name user) stream)))

(defmethod print-object ((group group) stream)
  (print-unreadable-object (group stream :type t :identity t)
    (write-string (group-name group) stream)))

(defmethod print-object ((access-control access-control) stream)
  (print-unreadable-object (access-control stream :type t :identity t)
    (write-string (access-control-name access-control) stream)))

(define-macro with-realm-write-lock ((realm) &body body)
  "Provides concurrency control for shared resources associated with realms."
  `(www-utils:with-lock-held ((realm-lock ,realm) :write "Realm Wait") ,@body))


;;;------------------------------------------------------------------- 
;;;
;;; REALM OPERATIONS
;;;

(define-generic realm-scheme (realm)
  (:documentation "Returns the token for a realm's authentication scheme."))

(define-generic realm-name (realm-or-string)
  (:documentation "Returns the name string for REALM-OR-STRING."))

(defmethod realm-name ((realm string))
  realm)

(define-variable *realm-table* nil
                 "The hash table pointing to authentication realms on the server.")

(declaim (inline realm-table))

(defun realm-table ()
  "Returns a hash table pointing to authentication realms on the server."
  (or *realm-table* (setq *realm-table* (make-hash-table :test #'equalp :size 10))))

(defun clear-realm-table ()
  "Clears all known realms."
  (if *realm-table*
      (clrhash *realm-table*)
      (realm-table)))

(declaim (inline %get-realm))

(defun %get-realm (realm-keyword &optional (realm-table (realm-table)))
  (gethash realm-keyword realm-table)) 

(defparameter *realm-scheme-class-alist* '((:basic . basic-realm)
                                           (:digest . digest-realm)
                                           (:digest-sha . digest-sha-realm))
  "Maps the realm authentication scheme to the realm class.
When applications specialize the realm class for a security scheme,
substitute the specialized realm class  for the default.")

(defun %get-realm-class (scheme)
  (or (cdr (assoc scheme *realm-scheme-class-alist* :test #'eq))
      (error "The authentication scheme, ~S, is unknown." scheme)))

(define add-realm-scheme (scheme realm-class)
  "Adds or replaces the realm class used for the authentication scheme, SCHEME.
REALM-CLASS must be a realm. SCHEME must be a keyword.
If you replace a default authentication schemes, you must ensure that the proper
super classes are inherited."
  (declare (values realm-class new-scheme-p))
  (check-type scheme keyword)
  (unless (and (symbolp realm-class)
               (subtypep realm-class 'realm))
    (error "~S is not a realm." realm-class))
  (let ((entry (assoc scheme *realm-scheme-class-alist*)))
    (cond (entry
           (setf (cdr entry) realm-class)
           (values realm-class nil))
          (t (push `(,scheme . ,realm-class) *realm-scheme-class-alist*)
             (values realm-class t)))))

(define remove-realm-scheme (scheme)
  "Removes knowledge of a realm scheme.
Dangerous.  Use with caution."
  (setq *realm-scheme-class-alist* (delete scheme *realm-scheme-class-alist* :key #'car)))

(define map-realms (function &optional (realm-table (realm-table)))
  "Maps FUNCTION over the groups in REALM.
FUNCTION is called with the arguments (NAME-STRING GROUP-OBJECT)."
  (maphash function realm-table))

(defgeneric make-realm (name class))

(defmethod make-realm ((name string) (class symbol))
  (make-instance class :name name))

(define intern-realm (realm &key (if-does-not-exist :error) (scheme :basic))
  (declare (values interned-realm newly-created-p))
  (flet ((do-it (realm)
           (let ((realm-table (realm-table)))
             (cond ((%get-realm realm realm-table))
                   (t (ecase if-does-not-exist
                        (:soft nil)
                        (:create
                          (let ((object (make-realm realm (%get-realm-class scheme))))
                            (setf (realm-lock object) (make-lock (format nil "~A Realm Lock" realm)
                                                                 :type :multiple-reader-single-writer))
                            (setf (gethash realm realm-table) object)
                            (values object t)))
                        (:error (error "~S is not a known realm." realm))))))))
    (declare (inline do-it))
    (etypecase realm
      (string (do-it realm))
      (symbol (do-it (symbol-name realm)))
      (realm realm))))

(define-generic unintern-realm (realm)
  (:documentation "Uninterns a realm."))

(defmethod unintern-realm ((realm realm))
  (remhash (realm-name realm) (realm-table)))

(defmethod unintern-realm ((realm string))
  (remhash realm (realm-table)))

(declaim (inline add-realm))

(define add-realm (realm-name scheme)
  "Adds a realm object of type scheme to realm-table."
  (intern-realm realm-name :if-does-not-exist :create :scheme scheme))

(declaim (inline add-group))

(define add-group (realm group)
  "Adds GROUP to realm, creating if necessary."
  (intern-group (intern-realm realm :if-does-not-exist :error)
                group
                :if-does-not-exist :create))

(declaim (inline add-groups))

(define add-groups (realm &rest groups)
  "Adds GROUPs to realm, creating if necessary."
  (loop with realm-object = (intern-realm realm :if-does-not-exist :error)
        for group in groups
        do (intern-group realm-object group :if-does-not-exist :create)))

(defmethod  authentication-scheme ((url http-url))
  (realm-scheme (intern-realm (url:authentication-realm url) :if-does-not-exist :error)))

(define-generic make-user-password-digest (realm username password)
  (declare (values user-password-digest))
  (:documentation "Returns a digest of USERNAME and PASSWORD specific to REALM."))

(defmethod make-user-password-digest (realm (user user) password)
  (make-user-password-digest realm (user-name user) password))

(define-generic update-user (user &key personal-name email-address password groups &allow-other-keys)
  (declare (values user))
  (:documentation "Updates USER according to the arguments."))

(defmethod update-user ((user user) &key (personal-name nil personal-name-supplied-p)
                        (email-address nil email-address-supplied-p) (password nil password-supplied-p)
                        groups &allow-other-keys
                        &aux realm)
  (flet ((get-user-realm ()
           (or realm (setq realm (user-realm user)))))
    (declare (inline get-user-realm))
    (cond-every
      (email-address-supplied-p ;; change the users's email address
        (setf (user-email-address user) email-address))
      (personal-name-supplied-p ;; Change the user's personal name.
        (setf (user-personal-name user) (or personal-name "")))
      (password-supplied-p ;;change the user's password
        (setf (user-password-digest user) (make-user-password-digest (get-user-realm) user password))))
    (let* ((current-groups (user-groups user))
           (n-groups (loop for g in groups
                           collect (intern-group (get-user-realm) g)))
           (add-groups (set-difference n-groups current-groups))
           (del-groups (set-difference current-groups n-groups)))
      (declare (dynamic-extent n-groups add-groups del-groups))
      (dolist (g add-groups)
        (group-add-user g user))
      (dolist (g del-groups)
        (group-remove-user g user)))
    user))

(defgeneric %realm-get-user (realm user-name)
  (:documentation "Returns the user object for USER-NAME in REALM or null."))

(defmethod %realm-get-user ((realm standard-realm-user-mixin) user-name)
  (with-slots (user-table) realm
    (gethash user-name user-table)))

(defmethod %realm-set-get-user ((realm standard-realm-user-mixin) user-name user-object)
  (with-slots (user-table) realm
    (setf (gethash user-name user-table) user-object)))

(defmethod (setf %realm-get-user) ((user-object user)  (realm standard-realm-user-mixin) user-name)
  (with-slots (user-table) realm
    (setf (gethash user-name user-table) user-object)))

(defgeneric make-user (realm user-name &optional password email-address personal-name))

(defmethod make-user ((realm standard-realm-user-mixin) user-name &optional password email-address personal-name)
  (make-instance (realm-user-class realm)
                 :username user-name
                 :realm realm
                 :password-digest (and password (make-user-password-digest realm user-name password))
                 :personal-name personal-name
                 :email-address email-address))

(define-generic intern-user (realm user &key if-does-not-exist password groups
                                   personal-name email-address &allow-other-keys)
  (declare (values user-object newly-created-p))
  (:documentation "Primary method for defining a REALM."))

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

(defmethod intern-user ((realm symbol) user &key (if-does-not-exist :error) password groups personal-name
                        email-address &allow-other-keys)
  (intern-user (intern-realm realm :if-does-not-exist :error) user
               :if-does-not-exist if-does-not-exist
               :password password
               :groups groups
               :personal-name personal-name
               :email-address email-address))

(defmethod intern-user ((realm string) user &key (if-does-not-exist :error) password groups personal-name
                        email-address &allow-other-keys)
  (intern-user (intern-realm realm :if-does-not-exist :error) user
               :if-does-not-exist if-does-not-exist
               :password password
               :groups groups
               :personal-name personal-name
               :email-address email-address))

(define-generic unintern-user (realm user)
  (:documentation "Uninterns USER from REALM.
REALM can be an interned realm or a realm keyword."))

(defmethod unintern-user ((realm standard-realm-user-mixin) (user user))
  (remhash (user-name user) (realm-user-table realm)))

(defmethod unintern-user :before ((realm realm) (user user))
  (dolist (group (user-groups user))
    (group-remove-user group user)))

(defmethod unintern-user ((realm realm) user)
  (let ((object (intern-user realm user :if-does-not-exist :soft)))
    (when object
      (unintern-user realm object))))

(defmethod unintern-user ((realm symbol) user) 
  (unintern-user (intern-realm realm :if-does-not-exist :error) user))

(define parse-authentication-object (string intern-function &key (if-does-not-exist :soft))
  (declare (values authentication-object newly-created-p))
  (let* ((len (1+ (position-if-not #'http:white-space-char-p string :start 0 :end (length string) :from-end t)))
         (pos1 (position-if-not #'http:white-space-char-p string :start 0 :end len))
         (pos2 (or (position #\| string :start pos1 :end len) len))
         (realm-name (subseq string pos1 pos2))
         (object-name (subseq string (1+ pos2) len))
         (realm (intern-realm realm-name :if-does-not-exist (case if-does-not-exist
                                                              (:error if-does-not-exist)
                                                              (t :soft)))))
    (when realm
      (funcall intern-function realm-name object-name :if-does-not-exist if-does-not-exist))))

(define-generic map-users (realm function)
  (:documentation "Maps FUNCTION over the users in REALM.
FUNCTION is called with the arguments (NAME-STRING USER-OBJECT)."))

(defmethod map-users ((realm standard-realm-user-mixin) function)
  (with-slots (user-table) realm
    (maphash function user-table)))

(define-generic map-groups (realm function)
  (:documentation "Maps FUNCTION over the groups in REALM.
FUNCTION is called with the arguments (NAME-STRING GROUP-OBJECT)."))

(defmethod map-groups ((realm standard-realm-group-mixin) function)
  (with-slots (group-table) realm
    (maphash function group-table)))

(define-generic map-access-controls (realm function)
  (:documentation "Maps FUNCTION over the access-controls in REALM.
FUNCTION is called with the arguments (NAME-STRING ACCESS-CONTROL-OBJECT)."))

(defmethod map-access-controls ((realm standard-realm-access-control-mixin) function)
  (with-slots (access-control-table) realm
    (maphash function access-control-table)))

(define sorted-realms (&key (predicate #'string<) (key #'realm-name))
  "Returns all known realm sorted by PREDICATE on the value of KEY."
  (let ((realms nil))
    (declare (dynamic-extent realms))
    (flet ((collect (name realm)
             (declare (ignore name))
             (push realm realms)))
      (map-realms #'collect (realm-table))
      (sort realms predicate :key key))))

(define-generic sorted-users (realm &key predicate key)
  (:documentation "Returns all users in REALM sorted according to PREDICATE on the value of KEY."))

(defmethod sorted-users ((realm realm) &key (predicate #'string<) (key #'user-name))
  (let ((users nil))
    (declare (dynamic-extent users))
    (flet ((collect (name user)
             (declare (ignore name))
             (push user users)))
      (declare (dynamic-extent #'collect))
      (map-users realm #'collect)
      (sort users predicate :key key))))

(define-generic sorted-groups (realm &key predicate key)
  (:documentation "Returns all groups in REALM sorted according to PREDICATE on the value of KEY."))

(defmethod sorted-groups ((realm realm) &key (predicate #'string<) (key #'group-name))
  (let ((groups nil))
    (declare (dynamic-extent groups))
    (flet ((collect (name group)
             (declare (ignore name))
             (push group groups)))
      (declare (dynamic-extent #'collect))
      (map-groups realm #'collect)
      (sort groups predicate :key key))))

(define-generic sorted-access-controls (realm &key predicate key)
  (:documentation "Returns all access-controlss in REALM sorted according to PREDICATE on the value of KEY."))

(defmethod sorted-access-controls ((realm realm) &key (predicate #'string<) (key #'access-control-name))
  (let ((access-controls nil))
    (declare (dynamic-extent access-controls))
    (flet ((collect (name access-control)
             (declare (ignore name))
             (push access-control access-controls)))
      (map-access-controls realm #'collect)
      (sort access-controls predicate :key key))))

;; Cliches for the user accessibility.
(declaim (inline add-user))

(define add-user (user realm &rest args)
  "Add a user object with its appropriate slots to a realm."
  (apply #'intern-user (intern-realm realm :if-does-not-exist :error) user
         :if-does-not-exist :create args))

(declaim (inline delete-user))

(defun delete-user (user realm )
  (unintern-user realm user))

(defgeneric %authenticate-user (realm authorization method)
  (declare (values user-object-or-null))
  (:documentation "Verify a user in a realm and returns the user object or null."))

(defmethod %authenticate-user (realm authorization method)
  (declare (ignore realm authorization method))
  nil)

(defun authenticate-user (realm authorization &optional http-method)
  "Authenticate user in a REALM based on authorization.
Returns an authenicated user object or NIL if authentication not possible."
  (declare (values user-object-or-null))
  (let ((realm (intern-realm realm :if-does-not-exist :soft)))
    (and realm
         (%authenticate-user realm authorization http-method))))

(define-generic authenticate-user-p (user digest http-method)
  (declare (values user-object-or-null))
  (:documentation "Returns the user object if the DIGEST matches the USER for HTTP-METHOD, otherwise returns NIL."))

(define-generic www-authenticate-header-spec (realm authentication-method &rest keyword-value-pairs)
  (:documentation "Returns a WWW-AUTHENTICATE plist suitable for writing the header over HTTP."))

;;;------------------------------------------------------------------- 
;;;
;;; GROUPS
;;;

(defun %intern-group-list (realm list &optional (if-does-not-exist :soft))
  (loop for name in list
        for user = (and (stringp name)
                        (intern-user realm name :if-does-not-exist if-does-not-exist))
        for group = (intern-group realm name :if-does-not-exist if-does-not-exist)
        when user
          collect user into users
        else when group
               collect group into groups
        else do (error "Unknown user or group for the realm, ~S." realm)
        finally (return (values users groups))))

(declaim (inline %group-add-user))

(defun %group-add-user (group user)
  (with-slots (users) group
    (with-slots (groups) user
      (unless (member user users :test #'eq)
        (atomic-push user users)
        (atomic-push group groups)))))

(declaim (inline %group-delete-user))

(defun %group-remove-user (group user)
  (with-slots (users) group
    (when (member user users :test #'eq)
      (setf users (delete user users)
            (user-groups user) (delete group (user-groups user))))))

(declaim (inline %group-add-inferior))

(defun %group-add-inferior (group inferior)
  (with-slots (inferiors) group
    (with-slots (superiors) inferior
      (unless (member inferior inferiors :test #'eq)
        (atomic-push inferior inferiors)
        (atomic-push group superiors)))))

(declaim (inline %group-delete-inferior))

(defun %group-remove-inferior (group inferior)
  (with-slots (inferiors) group
    (when (member inferior inferiors :test #'eq)
      (setf inferiors (delete inferior inferiors)
            (group-superiors inferior) (delete group (group-superiors inferior))))))

(define-generic group-add-user (group user)
  (:documentation "Adds USER to GROUP."))

(defmethod group-add-user ((group standard-group) (user authenticated-user))
  (%group-add-user group user))

(define-generic user-add-group (user group)
  (:documentation "Adds GROUP to USER."))

(defmethod user-add-group ((user user) (group group))
  (group-add-user group user))

(define-generic group-remove-user (group user)
  (:documentation "Remove USER from GROUP."))

(defmethod group-remove-user ((group standard-group) (user authenticated-user))
  (%group-remove-user group user))

(define-generic user-remove-group (user group)
  (:documentation "Removes GROUP from USER."))

(defmethod user-remove-group ((user user) (group group))
  (group-remove-user group user))

(define-generic group-add-inferior (superior inferior)
  (:documentation "Adds an inferior group, INFERIOR, to a superior group, SUPERIOR."))

(defmethod group-add-inferior ((superior standard-group) (inferior standard-group))
  (%group-add-inferior superior inferior))

(define-generic group-remove-inferior (superior inferior)
  (:documentation "Removes an inferior group, INFERIOR, from a superior group, SUPERIOR."))

(defmethod group-remove-inferior ((superior standard-group) (inferior standard-group))
  (%group-remove-inferior superior inferior))

(defgeneric udpate-users-and-groups (group users-and-groups &optional if-does-not-exist))

(defmethod udpate-users-and-groups ((group group) users-and-groups &optional (if-does-not-exist :soft))
  (destructuring-bind (users groups) users-and-groups
    (%intern-group-list (group-realm group) users-and-groups if-does-not-exist)
    (cond-every
      (users
        (dolist (user users)
          (group-add-user group user)))
      (groups
        (dolist (inferior groups)
          (group-add-inferior group inferior))))))

(defgeneric make-group (realm group-name))

(defmethod make-group ((realm standard-realm-group-mixin) group-name)
  (make-instance (realm-group-class realm)
                 :name group-name
                 :realm realm))

(define-generic intern-group (realm group &key if-does-not-exist members)
  (declare (values group-object newly-create-p))
  (:documentation "Interns the group named, NAME, in the realm, REALM.
When supplied, members are set to MEMBERS.
Interning of MEMBERS respects the value of IF-DOES-NOT-EXIST."))

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

(defmethod intern-group ((realm realm) (name symbol) &key (if-does-not-exist :error)
                         (members nil members-supplied-p))
  (cond (members-supplied-p
         (intern-group realm (symbol-name name) :if-does-not-exist if-does-not-exist
                       :members members))
        (t (intern-group realm (symbol-name name) :if-does-not-exist if-does-not-exist))))

(defmethod intern-group ((realm string) name &key (if-does-not-exist :error)
                         (members nil members-supplied-p))
  (cond (members-supplied-p
         (intern-group (intern-realm realm :if-does-not-exist :error) name
                       :if-does-not-exist if-does-not-exist
                       :members members))
        (t (intern-group (intern-realm realm :if-does-not-exist :error) name
                         :if-does-not-exist if-does-not-exist))))

(defmethod intern-group ((realm realm) (group group) &key if-does-not-exist
                         (members nil members-supplied-p))
  (declare (ignore if-does-not-exist))
  (unless (typep group (realm-group-class realm))
    (error "Group, ~S, is the wrong class for REALM, ~S." group realm))
  (when members-supplied-p
    (udpate-users-and-groups group members))
  group)

(defmethod unintern-group ((realm standard-realm-group-mixin) (group group))
  (remhash (group-name group) (realm-group-table realm)))

(defmethod unintern-group :before ((realm realm) (group group))
  (dolist (user (group-users group))
    (user-remove-group user group))
  (dolist (g (group-inferiors group))
    (group-remove-inferior group g))
  (dolist (g (group-superiors group))
    (group-remove-inferior g group)))

(defmethod unintern-group ((realm realm) (group string))
  (let ((object (intern-group realm group :if-does-not-exist :soft)))
    (when object
      (unintern-group realm object))))

(defmethod unintern-group ((realm string) group) 
  (unintern-group (intern-realm realm :if-does-not-exist :error) group)) 

(define-generic group-keyword (group)
  (:documentation "Returns a keyword denoting the group."))

(defmethod group-keyword ((group group))
  (intern (group-name group) *keyword-package*))

(define-generic qualified-name (authentication-object &optional recompute-p)
  (:documentation "Returns the name of authentication-object qualified by realm [e.g., \"my-realm|my-name|\"]"))

(defmethod qualified-name ((group group) &optional recompute-p)
  (with-value-cached (group :qualified-name :recompute-p recompute-p)
    (concatenate 'string (realm-name (group-realm group)) "|" (group-name group))))

;;;------------------------------------------------------------------- 
;;;
;;; URL ACCESS CONTROL
;;; 

;; add new keywords here as new methods are desired.
(defparameter *url-capability-methods* '(:default :get :head :post :put :delete)
  "The set of allowed URL capabilities.")

(declaim (inline check-url-capability-method))

(defun check-url-capability-methods (alist)
  (loop for (method) in alist
        unless (member method *url-capability-methods* :test #'eq)
          do (error "The URL capability method, ~S, is not one of the known set, ~S.
Perhaps you need to update http:*url-capability-methods*." method *url-capability-methods*)))

;; returns an alist of (method groups users)
(defun %intern-access-control-alist (realm alist)
  (declare (values access-alist default-groups default-users))
  (flet ((parse-entry (entry)
           (destructuring-bind (method &rest capabilities) entry
             (loop for name in capabilities
                   for user = (and (stringp name)
                                   (intern-user realm name :if-does-not-exist :soft))
                   for group = (and (null user)
                                    (intern-group realm name :if-does-not-exist :soft))
                   when user
                     collect user into users
                   else when group
                          collect group into groups
                   else do (error "Unknown user or group, ~S for the realm, ~S." name realm)
                   finally (return (when method
                                     (list method groups users)))))))
    (check-url-capability-methods alist)
    (let ((default (assoc :default alist :test #'eq)))
      (destructuring-bind (&optional default-groups default-users)
          (and default (cdr (parse-entry default)))
        (loop for method in *url-capability-methods*
              for entry = (unless (member method '(:default) :test #'eq)
                            (assoc method alist :test #'eq))
              when entry 
                collect (parse-entry entry) into access-alist
              finally (return (values access-alist default-groups default-users)))))))

(defmethod update-access-control ((access-control standard-access-control) capability-alist)
  (with-slots (realm) access-control
    (multiple-value-bind (access-alist default-groups default-users)
        (%intern-access-control-alist realm capability-alist)
      (setf (access-control-alist access-control) access-alist
            (access-control-default-groups access-control) default-groups
            (access-control-default-users access-control) default-users))))

(define-generic capability-alist (access-control)
  (:documentation "Returns a capability alist for ACCESS-CONTROL suitable for intern-access-control."))

(defmethod capability-alist ((access-control standard-access-control))
  (with-slots (alist default-groups default-users) access-control
    (flet ((make-entry (method groups users)
             `(,method ,.(mapcar #'group-keyword groups)
               ,.(mapcar #'user-name users))))
      (loop for (method groups users) in alist
            collect (make-entry method groups users)
              into capability-alist
            finally (return (cond ((or default-groups default-users)
                                   `(,.capability-alist
                                     ,(make-entry :default default-groups default-users)))
                                  (t capability-alist))))))) 

(defgeneric make-access-control (realm name))

(defmethod make-access-control ((realm standard-realm-access-control-mixin) name)
  (make-instance (realm-access-control-class realm)
                 :name name
                 :realm realm))

(defgeneric make-url-access-control (realm name))

(defmethod make-url-access-control ((realm standard-realm-access-control-mixin) name)
  (make-instance (realm-url-access-control-class realm)
                 :name name
                 :realm realm))

(define-generic intern-access-control (realm name &key if-does-not-exist capability-alist)
  (declare (values access-control-object newly-interned-p))
  (:documentation "Interns a URL capabilities object named, NAME, in the realm, REALM.
capabilities-alist can be an alist of (METHOD &rest users-and-groups)."))

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

(declaim (inline %intern-access-control-aux))

(defun %intern-access-control-aux (realm name if-does-not-exist capability-alist capability-alist-supplied-p)
  (cond (capability-alist-supplied-p
         (intern-access-control realm name
                                :if-does-not-exist if-does-not-exist
                                :capability-alist capability-alist))
        (t (intern-access-control realm name :if-does-not-exist if-does-not-exist))))

(defmethod intern-access-control (realm (name symbol) &key (if-does-not-exist :error)
                                        (capability-alist nil capability-alist-supplied-p))
  (%intern-access-control-aux realm (symbol-name name) if-does-not-exist capability-alist capability-alist-supplied-p))

(defmethod intern-access-control ((realm string) name &key (if-does-not-exist :error)
                                  (capability-alist nil capability-alist-supplied-p))
  (%intern-access-control-aux (intern-realm realm :if-does-not-exist :error)
                              name if-does-not-exist capability-alist capability-alist-supplied-p))

(defmethod intern-access-control ((realm symbol) name &key (if-does-not-exist :error)
                                  (capability-alist nil capability-alist-supplied-p))
  (%intern-access-control-aux (intern-realm realm :if-does-not-exist :error)
                              name if-does-not-exist capability-alist capability-alist-supplied-p))

(defmethod intern-access-control (realm (url url:http-url) &key (if-does-not-exist :error)
                                        (capability-alist nil capability-alist-supplied-p))
  (%intern-access-control-aux realm (url:name-string url) if-does-not-exist capability-alist capability-alist-supplied-p))

(defmethod intern-access-control ((realm realm) (access-control access-control) &key if-does-not-exist
                                  (capability-alist nil capability-alist-supplied-p))
  (declare (ignore if-does-not-exist))
  (unless (typep access-control (realm-access-control-class realm))
    (error "ACCESS-CONTROL, ~S, is the wrong class for REALM, ~S." access-control realm))
  (when capability-alist-supplied-p
    (update-access-control access-control capability-alist))
  access-control)

(define-generic unintern-access-control (realm access-control)
  (:documentation "Unintern access-control from REALM."))

(defmethod unintern-access-control ((realm standard-realm-access-control-mixin) (access-control string))
  (with-slots (access-control-table) realm
    (remhash access-control access-control-table)))

(defmethod unintern-access-control ((realm realm) (access-control access-control))
  (unintern-access-control realm (access-control-name access-control)))

(defmethod unintern-access-control ((realm symbol) access-control) 
  (unintern-access-control (intern-realm realm :if-does-not-exist :error) access-control))

(define-generic add-access-control-group (name-or-url realm &key capabilities)
  (declare (values access-control newly-created-p))
  (:documentation "Interns access-control, named NAME-OR-URL, in REALM with method capabilities."))

(defmethod add-access-control-group (name (realm realm) &key capabilities)
  (intern-access-control realm name :if-does-not-exist :create :capability-alist capabilities))

(defmethod add-access-control-group (name (realm symbol) &key capabilities)
  (intern-access-control (intern-realm realm :if-does-not-exist :error)
                         name :capability-alist capabilities
                         :if-does-not-exist :create))

(defmethod add-access-control-group ((url url:http-url) (realm realm) &key capabilities)
  (intern-access-control realm (url:name-string url)
                         :capability-alist capabilities
                         :if-does-not-exist :url-create))

(defmethod url:initialize-authentication ((url url:authentication-mixin) realm capabilities
                                          &aux realm-obj access-control)
  (macrolet ((require-realm (realm-obj)
               `(unless ,realm-obj
                  (error "Authentication cannot be initialized without a realm."))))
    ;; intern the objects
    (setq realm-obj (typecase realm
                      (null nil)
                      (t (intern-realm realm :if-does-not-exist :error))))
    (setq access-control (typecase capabilities
                           (null nil)
                           (cons
                             (require-realm realm-obj)
                             (add-access-control-group url realm-obj :capabilities capabilities))
                           (access-control
                             (cond
                               (realm-obj
                                (unless (eq realm-obj (access-control-realm capabilities))
                                  (error "REALM, ~S, for the URL, ~S, is not the same as the authentication realm for CAPABILITIES, ~S.
They must be the same." realm-obj capabilities)))
                               (t (setq realm-obj (access-control-realm capabilities))
                                  (require-realm realm-obj)))
                             capabilities)
                           (t (require-realm realm-obj)
                              (intern-access-control realm-obj capabilities :if-does-not-exist :error))))
    ;; set the instance variables.
    (with-slots (url:authentication-realm url:capabilities) url
      (setq url:authentication-realm realm-obj
            url:capabilities access-control))))

(define-generic allow-user-access-p (url-or-access-control user-or-server http-method)
  (:documentation "Decides whether USER-OR-SERVER should get access to URL-OR-ACCESS-CONTROL via HTTP-METHOD."))

(defmethod allow-user-access-p ((access-control standard-access-control) (user user) (http-method symbol))
  (with-slots (alist default-groups default-users) access-control
    (flet ((any-intersection (l1 l2)
             (and l1 l2
                  (loop for item in l1
                        when (member item l2 :test #'eq)
                          return t
                        finally (return nil))))
           (any-member (item l2)
             (and l2 (member item l2 :test #'eq))))
      (declare (inline any-intersection any-member))
      (let ((entry (assoc http-method alist :test #'eq)))
        (cond (entry
               (destructuring-bind (&optional groups users) (cdr entry)
                 (or (any-intersection (user-groups user) groups)
                     (any-member user users))))
              ((or (any-intersection (user-groups user) default-groups)
                   (any-member user default-users)))
              ((not (or alist default-groups default-users))
               t)
              (t nil))))))

(defmethod allow-user-access-p ((url url) (user user) http-method)
  (let ((access-control (url:capabilities url)))
    (if access-control
        (allow-user-access-p access-control user http-method)
        t)))

(defmethod allow-user-access-p ((url url) user http-method)
  (declare (ignore user http-method))
  (null (url:capabilities url)))

;; These are not cached because plist mixin was not added to them.
(defmethod qualified-name ((access-control access-control) &optional recompute-p)
  (declare (ignore recompute-p))
  (concatenate 'string (realm-name (access-control-realm access-control)) "|" (access-control-name access-control)))

;;;------------------------------------------------------------------- 
;;;
;;; AUTHENTICATION UTILITIES
;;; 

(define define-realm-table (token-scheme-a-list)
  "Initializes and defines realms authenticated the server from an a-list of name tokens and schemes."
  (loop initially (clear-realm-table)
        for (name . scheme) in token-scheme-a-list
        collect (intern-realm name :if-does-not-exist :create :scheme scheme)))

(define-macro with-authentication-access-control ((url method authorization realm &key rejection-form
                                                       require-capabilities) &body body)
  "Executes REJECTION-FORM whenever AUTHORIZATION does not qualify for CAPABILITIES under REALM,
Otherwise executes BODY. If REQUIRE-CAPABILITIES is non-null, all accesses are
rejected whenever CAPABILITIES is null"
  `(cond (,realm
          (handler-case
            (let ((user (authenticate-user ,realm ,authorization ,method)))
              (cond ((and user
                          (let ((capabilities (url:capabilities ,url)))
                            ,(if require-capabilities
                                 `(and capabilities (allow-user-access-p capabilities user ,method))
                                 `(or (null capabilities) (allow-user-access-p capabilities user ,method)))))
                     (setf (server-user-object *server*) user)
                     ,@body)
                    (t ,rejection-form)))
            (unknown-authentication-method () ,rejection-form)))
         (t ,@body)))

(define-macro with-access-control ((url method server secure-subnets &key require-secure-subnets require-capabilities
                                        deny-subnets write-method-p) &body body)
  (let ((code `(with-subnet-access-control ((server-address ,server)
                                            ,secure-subnets
                                            :deny-subnets ,deny-subnets
                                            :require-secure-subnets ,require-secure-subnets
                                            :rejection-form (error 'access-forbidden :method ,method :url ,url))
                 (let ((.realm. (url:authentication-realm ,url)))
                   (with-authentication-access-control
                     (,url ,method
                      (get-header :authorization)
                      .realm.
                      :require-capabilities ,require-capabilities
                      :rejection-form (error 'recoverable-unauthorized-client-access :method ,method :url ,url
                                             :authentication-method (authentication-scheme ,url) 
                                             :authentication-realm .realm.))
                     ,@body)))))
    (if write-method-p
        `(with-remote-write-control (,url :rejection-form (error 'access-forbidden :method ,method :url ,url))
           ,code)
        code)))


;;;------------------------------------------------------------------- 
;;;
;;; SAVING AUTHENTICATION DATA
;;;

(define-generic write-lisp-source (realm authentication-object stream)
  (declare (values authenication-object))
  (:documentation "Writes Lisp source code on STREAM for reloading AUTHENTICATION-OBJECT in REALM.
AUTHENTICATION-OBJECT can be a user, group, access-control."))

(defmethod write-lisp-source ((realm standard-realm) (group standard-group) stream)
  (with-slots (inferiors) group
    (let ((infs (mapcar #'group-keyword inferiors)))
      (fresh-line stream)
      (write `(intern-group ,(realm-name realm) ,(group-name group) 
                            ,.(when infs `(:members ',infs))
                            :if-does-not-exist :create)
             :stream stream)
      (fresh-line stream)
      group)))

(defmethod write-lisp-source :around ((realm standard-realm) (user standard-user) stream)
  (with-slots (groups) user
    (let ((user-code `(intern-user ,(realm-name realm) ,(user-name user)
                                   :if-does-not-exist :create
                                   :email-address ,(user-email-address user)
                                   :personal-name ,(user-personal-name user)
                                   :groups ',(mapcar #'group-name groups))))
      (declare (dynamic-extent user-code))
      (fresh-line stream)
      (cond ((%write-lisp-source-specialized-info-p user)
             (write-string "(LET " stream)
             (write `((user ,user-code)) :stream stream) 
             (call-next-method)
             (write-char #\) stream))           ; close let
            (t (write user-code :stream stream)))
      (fresh-line stream)
      user)))

(defgeneric %write-lisp-source-specialized-info-p (object)
  (:documentation "Returns non-null when additional slots will be set on OBJECT
by combined methods for write-lisp-source.")
  (:method-combination or))

(defmethod %write-lisp-source-specialized-info-p or (object)
  (declare (ignore object)) nil)

(defmethod %write-lisp-source-specialized-info-p or ((user user-password-mixin))
  (with-slots (password-digest) user
    password-digest)) 

;; each type of authenticated user may need to define this method.
;; This one covers any user classes employing user-password-mixin
(defmethod write-lisp-source ((realm standard-realm) (user user-password-mixin) stream)
  (with-slots (username password-digest) user
    (when password-digest
      (fresh-line stream)
      (let ((*print-base* 8))
        (write `(setf (user-password-digest user) ',password-digest) :stream stream)))
    user)) 

(defmethod write-lisp-source ((realm standard-realm-access-control-mixin) (access-control standard-access-control) stream)
  (fresh-line stream)
  (write `(intern-access-control ,(realm-name realm) ,(access-control-name access-control)
                                 :capability-alist ',(capability-alist access-control)
                                 :if-does-not-exist :create)
         :stream stream)
  (fresh-line stream)
  access-control)

;; Access controls linked to URLs are not written out because they are created when the URL is exported.
(defmethod write-lisp-source ((realm standard-realm) (access-control url-access-control) stream) 
  (declare (ignore stream)) 
  access-control)

(defgeneric intern-realm-form (realm)
  (:documentation "Returns a form that interns and instantiates REALM."))

(defmethod intern-realm-form ((realm standard-realm))
  `(intern-realm ,(realm-name realm)
                 :if-does-not-exist :create
                 :scheme ,(realm-scheme realm)))

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

(define save-authentication-data (&key (pathname *authentication-data-pathname*) if-exists)
  "Top-level function to write a LISP file containing all current realm, group, and user password data.
IF-EXISTS can be either :NEW-VERSION, :SUPERSEDE, :OVERWRITE."
  ;; Make sure the directory exists
  (pathname-create-directory-if-needed pathname)
  (with-open-file (stream pathname :direction :output :if-does-not-exist :create
                          :if-exists (or if-exists #+Genera :new-version #-Genera :supersede))
    (format stream ";;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-
~&;;;~&;;; AUTHENTICATION DATA -- ")
(print-gmt-time stream)
(format stream "~&;;;~2%(in-package :http)~2%")
(flet ((write-realm (name realm)
         (declare (ignore name))
         (write-lisp-source realm nil stream)))
  (declare (dynamic-extent #'write-realm))
  (let ((*print-pretty* t)
        (*print-readably* t))
    (map-realms #'write-realm (realm-table)))
  (format stream "~&;;; End Complete Authentication Data Save
~&;;; Begin Incremental Authentication Data Saves~2%")
  pathname)))

(define-generic save-authentication-object (authentication-object &key pathname &allow-other-keys)
  (declare (values authentication-object))
  (:documentation "Writes AUTHENTICATION-OBJECT to an existing authentication data PATHNAME.
Use this method for incrementally updating this file, and periodically dump a new version of
the pathname when you want to eliminate duplicate entries."))

(defmethod save-authentication-object ((user standard-user) &key (pathname *authentication-data-pathname*))
  (with-open-file (stream pathname :direction :output :if-does-not-exist :error
                          :if-exists :append)
    (write-lisp-source (user-realm user) user stream)))

(defmethod save-authentication-object ((group standard-group) &key (pathname *authentication-data-pathname*))
  (with-open-file (stream pathname :direction :output :if-does-not-exist :error
                          :if-exists :append)
    (write-lisp-source (group-realm group) group stream)))

(defmethod save-authentication-object ((access-control standard-access-control)
                                       &key (pathname *authentication-data-pathname*))
  (with-open-file (stream pathname :direction :output :if-does-not-exist :error
                          :if-exists :append)
    (write-lisp-source (access-control-realm access-control) access-control stream)))

(define restore-authentication-data (&key (pathname *authentication-data-pathname*)
                                          (if-does-not-exist :error) clear-stale-data)
  "Top-level function for restoring authentication information from PATHNAME.
IF-DOES-NOT-EXIST can be :error or NIL. When CLEAR-STALE-DATA is non-null,
all authentication data in dynamic memory is purged."
  (cond ((or (eq if-does-not-exist :error)
             (and (probe-directory pathname)
                  (probe-file pathname)))
         (let ((*read-base* 8))
           ;; clear all previsous data
           (when clear-stale-data (clear-realm-table))
           (load pathname :verbose nil :if-does-not-exist (ecase if-does-not-exist
                                                            ((:error nil) if-does-not-exist)
                                                            (:soft nil))))
         t)
        (t nil)))

(define initialize-server-authentication (&optional (pathname *authentication-data-pathname*))
  "Ensures that password data is loaded.
If no data exists, this creates the SERVER realm using digest scheme and
creates a Webmaster user in the :webmasters group and the Webmasters access control."
  (cond ((restore-authentication-data :pathname pathname :if-does-not-exist :soft))
        ;; define the webmaster realm
        (t (add-realm :server :digest)
           ;; define the group of webmasters in the
           (add-groups :server :webmasters)
           ;; Define a set of capabilities giving the :elite-members group basic access
           (add-access-control-group "Webmasters"
                                     :server
                                     :capabilities '((:default :webmasters)))
           ;; Set up two users, assigning realms and groups.
           (intern-user :server "Webmaster"
                        :groups '(:webmasters)
                        :personal-name "Webmaster"
                        :email-address *bug-http-server*
                        :if-does-not-exist :create)
           (pathname-create-directory-if-needed pathname)
           (save-authentication-data :pathname pathname))))

(add-initialization
  "Initialize Server Authentication"
  '(initialize-server-authentication)
  '(:normal)
  '*server-launch-initialization-list*)


;;;------------------------------------------------------------------- 
;;;
;;; USER METHODS
;;;

(define-generic user-name (user-or-server)
  (:documentation "Returns the unqualified user name."))

(define-generic user-personal-name (user-or-server)
  (:documentation "Returns the unqualified user name."))

(define-generic user-email-address (user-or-server)
  (:documentation "Returns the unqualified user name."))

(define-generic user-realm (user-or-server)
  (:documentation "Returns the unqualified user name."))

(define-generic user-groups (user-or-server)
  (:documentation "Returns the unqualified user name."))

(define-generic user-qualified-name (user-or-server &optional recompute-p)
  (:documentation "Returns the username for user qualified by realm [e.g., \"my-realm|my-username|\"]"))

(defmacro %user-qualified-name (user recompute-p)
  `(with-value-cached (,user :qualified-name :recompute-p ,recompute-p)
     (concatenate 'string (realm-name (user-realm ,user)) "|" (user-name ,user))))

(defmethod user-qualified-name ((user user) &optional recompute-p)
  (%user-qualified-name user recompute-p))

(defmethod qualified-name ((user user) &optional recompute-p)
  (%user-qualified-name user recompute-p))

(define-generic send-mail-to-user (user-or-server subject message-writer &key from keywords
                                                  comments file-references reply-to)
  (declare (values authenticated-user))
  (:documentation "Sends an email message to AUTHENTICATED-USER with SUBJECT from FROM.
MESSAGE-WRITER is either a string or a function accepting a single stream argument."))

(defmethod send-mail-to-user ((user user) subject message-writer
                              &key (from http:*server-mail-address*) keywords comments file-references reply-to)
  (send-mail-from from (user-email-address user) subject message-writer
                  :reply-to reply-to
                  :keywords keywords
                  :comments comments
                  :file-references file-references)
  user)

;;;------------------------------------------------------------------- 
;;;
;;; HTTP/1.0 BASIC AUTHENTICATION SCHEME
;;; 

;; MD5 digesting of passwords could be upgraded to SHA sometime. However, that
;; would break everyone's passwords. Given the lack of security on basic, it
;; is hardly worth it. 3/15/97 -- JCMa.
(defmethod make-user-password-digest ((realm basic-realm-mixin) (username string) (password string))
  (let ((string (concatenate 'string username ":" password)))
    (declare (dynamic-extent string))
    (md5:md5-digest-hexadecimal-string string)))

(defmethod authenticate-user-p ((user basic-user-mixin) digest method)
  (declare (values user-or-null)
           (ignore method))
  (if (md5:md5-with-temporary-digest-string
	(equalp (md5:md5-digest-hexadecimal-string digest) (user-password-digest user)))
      user
      nil))

(defmethod %authenticate-user ((realm basic-realm-mixin) authorization method)
  "Checks the basic authentication authorization and returns the user object if valid."
  (declare (values user-object-or-null))
  (when authorization
    (destructuring-bind (authentication-method cookie &rest args) authorization
      (declare (ignore args))
      (case authentication-method
        (:basic
          (let* ((decoded-authorization (base64:base64-decode-vector cookie :decoded-byte-type *standard-character-type*))
                 (username (subseq decoded-authorization 0 (position #\: decoded-authorization :test #'char-equal)))
                 (user (%realm-get-user realm username)))
            (declare (dynamic-extent decoded-authorization username))
            (when user
              (authenticate-user-p user decoded-authorization method))))
        (t nil)))))

;; backwards compatibility to digest lists in the first implementation  11/15/95 -- JCMa.
(defmethod (setf user-password-digest) :around ((password-digest cons) (user basic-user-mixin))
  (call-next-method (apply #'md5::md5-hexadecimal-encode password-digest) user))

(defmethod www-authenticate-header-spec ((realm basic-realm-mixin) (method symbol) &rest args)
  (declare (ignore args))
  `(:www-authenticate (,method ,(realm-name realm))))


;;;-------------------------------------------------------------------
;;;
;;; DIGEST AUTHENTICATION SCHEME
;;;

(defvar *digest-authentication-random-seed* nil)

;; This could use more sources of ergativity.  One good start would be to
;; capture 60 characters from the user at start up.
(defun make-random-seed (&optional old-seed)
  (sha:sha-digest-hexadecimal-string
    (with-output-to-string (stream)
      (write (get-internal-real-time) :stream stream)
      (cond (old-seed (write old-seed :stream stream))
            (t (write (machine-instance) :stream stream)
               (write (get-internal-real-time) :stream stream)
               (write (random 99999999999999999999999999999999999) :stream stream)
               (write (get-internal-run-time) :stream stream)
               (write (software-version) :stream stream)
               ;; toss in a bunch of environment variables
               (dolist (sym '(*gensym-counter* *features* * ** *** + ++ +++ *error-output* *standard-output* *print-level*
                                               *package* *load-pathname* *terminal-io* *query-io* *readtable*
                                               *default-pathname-defaults*))
                 (write (symbol-value sym) :stream stream :escape nil :base 10 :readably nil))))
      (write (get-universal-time) :stream stream)
      (write (get-internal-run-time) :stream stream))))

(defun save-random-seed (&key (pathname *random-seed-pathname*))
  (pathname-create-directory-if-needed pathname)
  (with-open-file (file pathname :if-does-not-exist :create
                        :if-exists #+Genera :overwrite #-Genera :supersede :direction :output)
    (write-string *digest-authentication-random-seed* file)))

;; Capture some new randomness every time the random seed is restored from
;; network and disk latency involved in reading the seed.
(defun restore-random-seed (&key (pathname *random-seed-pathname*))
  (let* ((start (fixnum-microsecond-time))
         (file-p (probe-file pathname))
         (end (fixnum-microsecond-time)))
    (when file-p
      (with-open-file (file pathname :direction :input)
        (let* ((old (read-line file))
               (new-randomness (write-to-string (- end start)))
               (new (concatenate 'string old new-randomness)))
          (declare (dynamic-extent old new-randomness new))
          (sha:sha-digest-hexadecimal-string new))))))

(define digest-authentication-random-seed (&optional force-new-p)
  "Returns the random seed for Digest authentication.
This gets more randomness each time the first digest authentication is
performed for each cold boot of the server.  If you're worried about the
randomness of the seed, you can invoke this function manually with
force-new-p sixteen times."
  (flet ((initialize-random-seed ()
           (let ((old-seed (restore-random-seed)))
             (prog1 (setq *digest-authentication-random-seed* (make-random-seed old-seed))
                    (save-random-seed)))))
    (cond (force-new-p (initialize-random-seed))
          (*digest-authentication-random-seed*)
          (t (initialize-random-seed)))))

;; Must follow server authentication initialization.
(add-initialization
  "Initialize Random Seed"
  '(digest-authentication-random-seed t)
  '(:normal)
  '*server-launch-initialization-list*)

(defun generate-digest-nonce (opaque random-seed &optional (life-time *digest-authentication-nonce-life-time*))
  (flet ((time-window-for-nconce ()
           (write-to-string (truncate (server-request-time *server*) life-time))))
    (declare (inline time-window-for-nconce))
    (let* ((window (time-window-for-nconce))    ;life time for nonces
           (client-address (write-to-string (server-address *server*) :base 8.))        ;prevent replay attack
           (hash (concatenate 'string opaque window client-address random-seed))
           (digest (sha:sha-digest-vector hash))
           (integer-chars (with-fast-array-references ((array digest vector))
                            (loop for i fixnum from 0 to 15 by 2
                                  collect (write-to-string (aref array i))))))
      (declare (dynamic-extent window client-address hash digest integer-chars))
      (apply #'concatenate 'string integer-chars))))

(define digest-nonce-opaque-pair ()
  "Returns a dotted list of the nonce and opaque component."
  (let* ((time-factor (write-to-string (get-internal-run-time) :base 10.))
         (opaque (sha:sha-digest-hexadecimal-string time-factor))
         (nonce (generate-digest-nonce opaque (digest-authentication-random-seed))))
    (declare (dynamic-extent time-factor))
    `(,nonce . ,opaque)))

(define generate-user-password (user-id &optional (n-chars 12) (life-time *digest-authentication-nonce-life-time*))
  "Generates a password for USER-ID that is N-CHARS long.
This is the recommended way to generate new passwords.
Do not specify LIFE-TIME unless you understand what you are doing.
LIFE-TIME is the time window during which the same arguments for 
USER-ID and N-CHARS will yield the same password."
  (labels ((vowel-p (char)
             (find (char-downcase char) "aeiouy" :test #'eql :start 0 :end 6))
           (consonant-p (char)
             (let ((ch (char-downcase char)))
               (unless (find ch "aeiouy" :test #'eql :start 0 :end 6)
                 ch)))
           (next-consonant (seq start end)
             (with-fast-array-references ((seq seq string))
               (loop for idx upfrom start below end
                     for char = (consonant-p (aref seq idx))
                     when char
                       return (values (char-downcase char) (1+ idx))
                     finally (return (next-consonant seq 0 start)))))
           (next-vowel (seq start end)
             (with-fast-array-references ((seq seq string))
               (loop for idx upfrom start below end
                     for char = (vowel-p (aref seq idx)) 
                     when char
                       return (values (char-downcase char) (1+ idx))
                     finally (return (next-vowel seq 0 start))))))
    (declare (inline consonant-p vowel-p))
    (let* ((nonce (generate-digest-nonce user-id (digest-authentication-random-seed) life-time))
           (chars (base64:base64-encode-vector nonce))
           (l (length chars)))
      (declare (dynamic-extent nonce chars)
               (fixnum l n-chars))
      (loop with pw-l = (min n-chars l)
            with pw = (make-array pw-l :element-type *standard-character-type* :fill-pointer t)
            with pos = 0 and vowel-index = 0 and consonant-index = 0 and char
            for idx upfrom 0 below l
            when (evenp idx)
              do  (multiple-value-setq (char consonant-index)
                    (next-consonant chars consonant-index l))
            else
              do (multiple-value-setq (char vowel-index)
                   (next-vowel chars vowel-index l))
            do (setf (aref pw pos) char
                     pos (1+ pos))
            while (< pos n-chars)
            finally (setf (fill-pointer pw) pos)
                    (return pw)))))

(defmethod make-user-password-digest ((realm digest-realm-mixin) (username string) (password string))
  (let ((string (concatenate 'string username ":" (realm-name realm) ":" password)))
    (declare (dynamic-extent string))
    (funcall (digest-realm-digest-function realm) string)))

(declaim (inline accept-nonce-value-p))

(define accept-nonce-value-p (nonce opaque)
  "Returns non-null unless NONCE is stale or invalid, given OPAQUE."
  (let ((local-nonce (generate-digest-nonce opaque (digest-authentication-random-seed))))
    (declare (dynamic-extent local-nonce))
    (equalp nonce local-nonce)))

(defmethod authenticate-user-p ((user digest-user-mixin) authorization http-method)
  (destructuring-bind (&key nonce opaque response uri &allow-other-keys) (cdr authorization)
    (cond 
      ((accept-nonce-value-p nonce opaque)
       ;; use the method and url from the http request rather than from the authorization header
       (let* ((realm (user-realm user))
              (digest-fctn (digest-realm-digest-function realm))
              (uri-method (concatenate 'string (symbol-name http-method) ":" (server-relative-url-string *server*)))
              (uri-method-digest (funcall digest-fctn uri-method))
              (user-nonce-uri-method (concatenate 'string (user-password-digest user) ":" nonce ":" uri-method-digest))
              (user-nonce-uri-method-digest (funcall digest-fctn user-nonce-uri-method)))
         (declare (dynamic-extent uri-method uri-method-digest user-nonce-uri-method user-nonce-uri-method-digest))
         (if (equalp response user-nonce-uri-method-digest)
             user
             nil)))
      (t (let ((realm (user-realm user)))
           (error 'client-access-with-stale-nonce :method http-method :url uri
                  :authentication-method  (realm-scheme realm)
                  :authentication-realm realm))))))

(defmethod %authenticate-user ((realm digest-realm-mixin) authorization http-method)
  "Checks the digest authentication authorization and returns the user object if valid."
  (declare (values user-object-or-null))
  (and authorization
       (destructuring-bind (authentication-method . plist) authorization
         (case authentication-method
           (:digest
             (let* ((username (getf plist :username))
                    (user (%realm-get-user realm username)))
               (and user
                    (authenticate-user-p user authorization http-method))))
           (t nil)))))

(defmethod www-authenticate-header-spec ((realm digest-realm-mixin) (method symbol) &rest args)
  `(:www-authenticate (,method ,(realm-name realm) :algorithm ,(digest-realm-algorithm realm) ,.args)))
