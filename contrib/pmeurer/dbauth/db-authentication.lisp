;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-
;;;
;;; (C) Copyright 1999, John C. Mallery and Paul Meurer.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; DATABASE INTERFACE FOR CL-HTTP AUTHENTICATION
;;;
;;;

#|

Database Interface for Authentication Objects

The authentication system in CL-HTTP can be specialized to use a database
rather than a file system for persistent storage. 

Given these objects and methods, everything works as usual except that some
persistent data is now stored in the database rather than the file system.

This file contains the genereal API. An implementation for SQL databases (using 
P. Meurer's SQL/ODBC package) is contained in the file sql-authentication.lisp.  

|#

(in-package :http)

(defmacro define-entity-type (name superclasses slots &rest db-class-options)
  "Macro DEFINE-ENTITY-TYPE
Defines a persistent class (an entity).
Syntax:
define-entity-type name ({superclass-name}*) ({slot-specifier}*) [[db-class-option]]
=> new-entity-type

slot-specifier         ::= (persistent-slot-name slot-type {persistent-slot-option}*)
persistent-slot-option ::= {:no-nulls boolean} |
                   {:read-only boolean} |
                   {:inverse inverse-accessor-name}
db-class-option := (:multiple-index {persistent-slot-name-list}*) |
                   (:instance-variables {(transient-slot-name [[slot-option]])}*) |
                   (:conc-name internal-accessor-prefix) |
                   (:documentation documentation-string)

slot-type   := type | (set-of type)
type        := Lisp-type | entity-type 
slot-option := a standard class slot option
"
  `(%define-entity-type ,name ,superclasses ,slots ,@db-class-options))

(defmacro with-db-transaction (&body body)
  "If the forms in body execute without error, all active transactions are 
committed on exit. If an error is occurs, the active transactions are rolled back."
  `(with-transaction ,@body))

(defgeneric make-entity (entity-type &rest initargs &key &allow-other-keys)
  (:documentation 
   "Creates an object of ENTITY-TYPE and stores persistent slots or creates a transient
copy of a persistent object."))

(defmacro %map-entities (function entity-type &key constraints)
  "Maps FUNCTION (of one argument) over all objects of ENTITY-TYPE (or a subtype)
 that satisfy CONSTRAINTS."
  `(for-each ((entity ,entity-type) ,(car constraints))
     (funcall ,function entity)))

(defun make-basic-realm (&key (scheme :basic) name)
  (make-entity 'db-basic-realm :name name :scheme scheme))

(defun make-digest-realm (&key (scheme :basic) name)
  (make-entity 'digest-db-realm :name name :scheme scheme))

(define-entity-type db-basic-realm
		    (basic-realm-mixin db-realm)
  ()
  (:instance-variables
    (user-class :initform 'db-basic-user)
    (group-class :initform 'db-basic-group))
  (:documentation "Realm class that supports persistent user objects."))

(define-entity-type db-digest-realm
		    (digest-realm-mixin db-realm)
  ()
  (:instance-variables
    (user-class :initform 'db-digest-user)
    (group-class :initform 'db-digest-group))
  (:documentation "Realm class that supports persistent user objects."))

(define-entity-type db-basic-group (basic-group-mixin db-group) () )

(define-entity-type db-digest-group (digest-group-mixin db-group) () )

;;;------------------------------------------------------------------- 
;;;
;;; AUTHENTICATION DATABASE INTERFACE
;;;

;;;------------------------------------------------------------------- 
;;;
;;; AUTHENTICATION ENTITIES
;;;

(define-entity-type db-realm
		    (standard-lock-mixin
                     standard-realm-user-mixin
                     standard-realm-group-mixin
                     standard-realm-access-control-mixin
                     realm)
  ((name string :no-nulls t :inverse-index t :read-only t)
   (scheme keyword :no-nulls t :read-only t))
  (:multiple-index (name scheme))
  (:instance-variables (r-name))
  (:conc-name %realm-)
  (:documentation "A persistent realm object."))

(define-entity-type db-basic-realm
		    (basic-realm-mixin db-realm)
  ()
  (:instance-variables
    (user-class :initform 'db-basic-user)
    (group-class :initform 'db-basic-group))
  (:documentation "Realm class that supports persistent user objects."))

(define-entity-type digest-db-realm
		    (digest-realm-mixin db-realm)
  ()
  (:instance-variables
    (user-class :initform 'digest-user)
    (group-class :initform 'db-digest-group))
  (:documentation "Realm class that supports persistent user objects."))

;; new
(define-entity-type db-user
		    (standard-user)
  ((realm db-realm :no-nulls t :inverse-index t :read-only t :inverse %realm-users)
   (name string :inverse-index t :read-only t)
   (password string)
   (email-address string)
   (personal-name string))
  (:multiple-index (realm name))
  (:instance-variables
   (u-realm) (u-name) (u-password) (u-contact) (u-groups))
  (:conc-name %user-)
  (:documentation "A persistent http user-id."))

;(describe (make-instance 'db-user))

(define-entity-type db-basic-user (basic-user-mixin db-user) () )
(define-entity-type db-digest-user (digest-user-mixin db-user) () )

(define-entity-type db-group
		    (group)
  ((realm db-realm :no-nulls t :inverse-index t :read-only t :inverse %realm-groups)
   (name string :no-nulls t :inverse-index t :read-only t)
   (users (set-of db-user #+obsolete db-user-id) :index t :inverse-index t :inverse %user-groups #+obsolete %user-id-groups)
   (inferiors (set-of db-group) :index t :inverse-index t :inverse %group-superiors))
  (:multiple-index (realm name))
  (:instance-variables (g-realm) (g-name) (g-users) (g-inferiors) (g-superiors))
  (:conc-name %group-)
  (:documentation "Persistent http group."))

(define-entity-type db-basic-group (basic-group-mixin db-group) () )

(define-entity-type db-digest-group (digest-group-mixin db-group) () )

(defmacro add-to-set ((accessor object) value)
  "Adds an object to a persistent set slot."
  `(pushnew ,value (,accessor ,object)))

(defmacro delete-from-set ((accessor object) value)
  "Deletes an object from a persistent set slot."
  (let ((old-values (gensym)))
    `(let ((,old-values (,accessor ,object)))
       (setf (,accessor ,object) 
             (delete ,value ,old-values)))))

(defun %find-realm-named (scheme realm-name)
  (for-each ((realm db-realm)
             (:where (:and
                      (eq scheme (%realm-scheme realm))
                      (string-equal realm-name (%realm-name realm)))))
    (return-from %find-realm-named realm))
  nil)

(defmethod %find-user-named ((realm db-realm) user-name)
  (for-each ((user db-user)
             (:where (:and
                      (eq realm (%user-realm user))
                      ;; User-names are case-sensitive in HTTP
                      (string= user-name (%user-name user)))))
    (return-from %find-user-named user))
  nil)

(defmethod %find-group-named ((realm db-realm) group-name)
  (for-each ((group db-group)
		 (:where (:and
                          (eq realm (%group-realm group))
                          (string-equal group-name (%group-name group)))))
    (return-from %find-group-named group))
  nil)

;;;------------------------------------------------------------------- 
;;;
;;; SUPPORT CODE
;;;

(define-macro define-entity-accessors (external-accessor 
                                       internal-accessor slot-name 
                                       entity-class &key provide-setter)
  "Defines accessor methods, EXTERNAL-ACCESSOR, to access and set SLOT-NAME on
ENTITY-CLASS using INTERNAL-ACCESSOR for database refresh. When provide-setter
is non-null, a setter is also defined."
  (let ((object (gentemp (concatenate 'string (string-upcase entity-class) "-"))))
    `(progn
       (defmethod ,external-accessor ((,object ,entity-class))
	 (declare #+ignore(sys:function-parent ,slot-name define-entity-accessors)
		  (values ,slot-name newly-refreshed-p))
	 (with-slots (,slot-name) ,object
	   (let ((refresh-p (not (slot-boundp ,object ',slot-name))))
	     (when refresh-p
	       (with-db-transaction
		 (setf ,slot-name (,internal-accessor ,object))))
	     ;; report whether the slot was updated or not.
	     (values ,slot-name refresh-p))))
       ,.(when provide-setter
	  `((defmethod (setf ,external-accessor) (new-value (,object ,entity-class))
	      #+ingore
              (declare (sys:function-parent ,slot-name define-entity-accessors))
	      (with-db-transaction
		(setf (,internal-accessor ,object) new-value)
	        (with-slots (,slot-name) ,object
                  (setf ,slot-name new-value))))))
       ',external-accessor)))

(define-macro with-databased-realms (() &body body)
  "Enables databased realm creation within body."
  `(let ((*realm-scheme-class-alist* '((:basic . db-basic-realm)
				       (:digest . db-digest-realm))))
     ,@body))

(define enable-databased-realms (&optional (on-p t))
  "Globally turns on and off the creation of databased realms."
  (setq *realm-scheme-class-alist* (if on-p
                                     '((:basic . db-basic-realm)
                                       (:digest . db-digest-realm))
                                     '((:basic . basic-realm)
                                       (:digest . digest-realm)))))


;;;------------------------------------------------------------------- 
;;;
;;; REALM API
;;;

(define-entity-accessors realm-name %realm-name r-name db-realm)
(define-entity-accessors realm-scheme %realm-scheme scheme db-realm)

;; shouldn't be needed. --jcma 1/25/96
#+ignore
(defmethod realm-scheme ((realm db-realm))
  (with-slots (scheme) realm
    scheme))

(define intern-databased-realm (realm &key (if-does-not-exist :error) (scheme :basic))
  "Used to create databased realms with databased users."
  (declare (values interned-realm newly-created-p))
  (case if-does-not-exist
    (:create
      (with-databased-realms ()
	(intern-realm realm :if-does-not-exist if-does-not-exist :scheme scheme)))
    (t (intern-realm realm :if-does-not-exist if-does-not-exist))))

(defgeneric initialize-realm (realm)
  (:documentation "Initializes all the dynamic caches for a databased realm."))

(defmethod initialize-realm ((realm db-realm))
  (let ((user-table (realm-user-table realm))
	(group-table (realm-group-table realm))
	(groups (%realm-groups realm))
	(users  (%realm-users realm)))
    (dolist (group groups)
      (setf (gethash (group-name group) group-table) group))
    (dolist (user users)
      (setf (gethash (user-name user) user-table) user)))
  realm)

(defmethod intern-realm-form ((realm db-realm))
  `(intern-databased-realm ,(realm-name realm)
			   :if-does-not-exist :create
			   :scheme ,(realm-scheme realm)))

(defmethod make-realm ((name string) (class (eql 'db-basic-realm)))
  (with-db-transaction
    (let ((realm (%find-realm-named :basic name)))
      (cond (realm (initialize-realm realm))
	    (t (make-basic-realm :scheme :basic :name name))))))

(defmethod make-realm ((name string) (class (eql 'db-digest-realm)))
  (with-db-transaction
    (let ((realm (%find-realm-named :digest name)))
      (cond (realm (initialize-realm realm))
	    (t (make-digest-realm :scheme :digest :name name))))))

(defmethod unintern-realm :after ((realm db-realm))
  (%delete-object realm))

(defmethod unintern-realm :around ((realm db-realm))
  (with-db-transaction
    (call-next-method realm)))

(defmethod make-user ((realm db-realm) user-name &optional password 
                      email-address personal-name)
  (with-db-transaction
    (let ((user (%find-user-named realm user-name)))
      (cond (user
	     (update-user user :personal-name personal-name :email-address email-address
			  :password password)
	     user)
	    (t (let ((password-digest
                      (and password
                           (make-user-password-digest realm user-name password))))
		 (make-entity (realm-user-class realm)
                              :realm realm
                              :name user-name
                              :password password-digest
                              :email-address email-address
                              :personal-name personal-name)))))))

(defmethod unintern-user :after ((realm db-realm) (user db-user))
  (%delete-object user))

(defmethod unintern-user :around ((realm db-realm) (user db-user))
  (with-db-transaction
    (call-next-method realm user)))

(defmethod make-group ((realm db-realm) group-name)
  (with-db-transaction
    (or (%find-group-named realm group-name)
	(make-entity (realm-group-class realm)
                     :realm realm
                     :name group-name))))

(defmethod unintern-group :after ((realm db-realm) (group db-group))
  (%delete-object group))

(defmethod unintern-group :around ((realm db-realm) (group db-group))
  (with-db-transaction
    (call-next-method realm group)))

(defmethod write-lisp-source ((realm db-realm) (ignore null) stream)
  (flet ((write-item (key value)
           (declare (ignore key))
           (write-lisp-source realm value stream)))
    (declare (dynamic-extent #'write-item))
    ;; Create the realm.
    (format stream ";;;-------------------------------------------------------------------~&;;; ~
                                    ~&;;; ~:(~A~) Realm (Databased Groups & Users)~&;;;~2%" (realm-name realm))
    (write (intern-realm-form realm) :stream stream)
    (terpri stream)
    ;; Create all access control objects and link them to groups and users.
    (map-access-controls realm #'write-item)
    (terpri stream)
    realm))


;;;------------------------------------------------------------------- 
;;;
;;; DATABASE USER API
;;;


(define-entity-accessors user-groups %user-groups u-groups db-user)

(define-entity-accessors user-name %user-name u-name db-user)

(define-entity-accessors user-password-digest %user-password u-password 
                         db-user :provide-setter t)

(define-entity-accessors user-realm %user-realm u-realm db-user)

(define-entity-accessors user-personal-name %user-personal-name
                         personal-name db-user :provide-setter t)

(define-entity-accessors user-email-address %user-email-address
                         email-address db-user :provide-setter t)

(defmethod map-users ((realm db-realm) function)
  (flet ((call-function (object)
	   (funcall function (user-name object) object)))
    (declare (dynamic-extent #'call-function))
    (%map-entities #'call-function db-user
                   :constraints ((:where (eq realm (%user-realm entity)))))))

(defmethod save-authentication-object ((user db-user)
                                       &key (pathname *authentication-data-pathname*))
  (declare (ignore pathname))
  user)

(defmethod write-lisp-source ((realm db-realm) (user db-user) stream)
  (declare (ignore stream))
  user)


;;;------------------------------------------------------------------- 
;;;
;;; DATABASED GROUP API
;;;

(define-entity-accessors group-inferiors %group-inferiors g-inferiors db-group)

(define-entity-accessors group-name %group-name g-name db-group)

(define-entity-accessors group-realm %group-realm g-realm db-group)

(define-entity-accessors group-superiors %group-superiors g-superiors db-group)

(define-entity-accessors group-users %group-users g-users db-group)

(defmethod group-add-inferior ((superior db-group) (inferior db-group))
  (unless (member inferior (group-inferiors superior))
    (with-db-transaction
      (add-to-set (%group-inferiors superior) inferior)
      ;; trigger transient slot refresh on next access
      (slot-makunbound superior 'g-superiors)
      (slot-makunbound inferior 'g-inferiors))))

(defmethod group-add-user ((group db-group) (user db-user #+obsolete db-user-id))
  (unless (member user (group-users group))
    (with-db-transaction
      (add-to-set (%group-users group) user)
      (%invalidate-users-slot group))))

(defmethod group-remove-inferior ((superior db-group) (inferior db-group))
  (when (member inferior (group-inferiors superior))
    (with-db-transaction
      (delete-from-set (%group-inferiors superior) inferior)
      (slot-makunbound superior 'g-superiors)
      (slot-makunbound inferior 'g-inferiors))))

(defmethod group-remove-user ((group db-group) (user db-user #+obsolete db-user-id))
  (when (member user (group-users group))
    (with-db-transaction
      (delete-from-set (%group-users group) user)
      ;; trigger refresh of transient slot
      (%invalidate-users-slot group))))

;; not very elegant
(defmethod %invalidate-users-slot ((group db-group))
  (mapc (lambda (user) 
          (slot-makunbound user 'u-groups))
        (group-users group))
  (slot-makunbound group 'g-users)
  (mapc (lambda (user) 
          (slot-makunbound user 'u-groups))
        (group-users group))
  ;users
  )

(defmethod map-groups ((realm db-realm) function)
  (flet ((call-function (object)
	   (funcall function (group-name object) object)))
    (declare (dynamic-extent #'call-function))
    (%map-entities #'call-function db-group
                   :constraints 
                   ((:where (eq realm (%group-realm entity)))))))

(defmethod save-authentication-object ((group db-group)
                                       &key (pathname *authentication-data-pathname*))
  (declare (ignore pathname))
  group)

(defmethod write-lisp-source ((realm db-realm) (group db-group) stream)
  (declare (ignore stream))
  group)

; EOF
