;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: www-utils -*-
;;;
;;; (C) Copyright 1999, John C. Mallery and Paul Meurer.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; SQL/ODBC BACKEND TO CL-HTTP AUTHENTICATION
;;;
;;;

(in-package :www-utils)

(eval-when (:load-toplevel :compile-toplevel :execute)
  #+(or :mcl :lispworks)
  (shadowing-import 'sql::in) ; is used in a couple of loops in www-utils.lisp
  (use-package :sql))

(initialize-database-type) ;; defaults to :odbc

(export '(for-each make-entity sql:with-transaction %realm-scheme %realm-name
           %define-entity-type %delete-object))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (enable-sql-reader-syntax))

;; set this to T to initially create the necessary database objects
(defparameter *initialize-db-auth* nil)

(defparameter *db-name* "OracleMac")
(defparameter *auth-user* "system")
;; set the password here
(defparameter *auth-password* "gvprckvnis")

;; for maximal flexibility, we don't restrict ourself to *default-database*
;; but can use an other db connection
(defvar *auth-db* 
  (or *default-database*
      (connect *db-name* :user-id *auth-user* :password *auth-password*
               :if-exist :old)))

(defvar *schema* 'authentication)

;; First time initializations. Creates necessary database objects.
(eval-when (:load-toplevel)
  (when *initialize-db-auth*
    (with-database (*auth-db*)
      (create-user [?*schema*] :password *auth-password*)
      (execute :grant '(connect resource) :to [?*schema*])
      ;; create sequence for object ids
      (execute :create-sequence [?*schema* entity-seq] :start-with 1)
      (create-table [?*schema* id-table-mappings]
                    '((object-id integer :primary-key)
                      (table-name (varchar 64) :not-null))))))

;; to do: add some integrity constraints to table definitions

(defmacro concat (&rest strings)
  `(concatenate 'string ,@strings))

(defclass entity-mixin ()
  ())

;; A buffer for the transient representations of persistent objects
(defparameter *entity-table* (make-hash-table))

(defun entity-class-p (cl)
  (subtypep cl 'entity-mixin))

;; verify as many conditions as possible in the server
(defmacro for-each (((entity-var entity-class) (&key where)) &body body)
  (let ((class (gensym))
        (external-where ())
        (internal-where ())
        (where (cond ((null where) ())
                     ((eq :and (car where))
                      (cdr where))
                     (t (list where)))))
    (dolist (criterion where)
      (if (eq (car criterion) 'member)
        (push criterion external-where)
        (push criterion internal-where)))
    `(let ((,entity-var ',entity-class)) ; this trick makes the table column accessible
       (with-database (*auth-db*)
         (do-query ((object-id class-name)
                    [select [object-id] [class]
                            :from (entity-db-table ',entity-class)
                            ,@(when internal-where
                                `(:where
                                  ,(if (cdr internal-where)
                                     `[and ,@(mapcar (lambda (criterion)
                                                       (destructuring-bind (op att val) criterion
                                                         `(build-where-criterion ',op ,att ,val)))
                                                     internal-where)]
                                     (destructuring-bind (op att val) (car internal-where)
                                       `(build-where-criterion ',op ,att ,val)))))])
                   (let ((,class (intern class-name "HTTP")))
                     (when (subtypep ,class ',entity-class)
                       (let ((,entity-var (make-entity ,class :id object-id)))
                         (when (and ,@external-where)
                           ,@body)))))))))

;; under construction
;(defun symbol-concat (symbol-or-string &rest symbols-or-strings)
;  )

; Monster macro. Should be broken down.
;; Documentation see http::define-entity-type
(defmacro %define-entity-type (name superclasses slots &rest db-class-options)
  (let ((standard-class-slots 
         (cons '(%object-id)
               (cdr (find-if (lambda (option) (eq (car option) :instance-variables))
                             db-class-options))))
        (conc-name (cadr (find-if (lambda (option) (eq (car option) :conc-name))
                                  db-class-options)))
        (multiple-index (cdr (find-if (lambda (option)
                                        (eq (car option) :multiple-index))
                                      db-class-options)))
        (object (gentemp (concat (symbol-name name) "-")))
        (value (gentemp "VALUE-"))
        (index-count 0)
        (table name))
    `(progn
       (defclass ,name ,(append superclasses (list 'entity-mixin))
         ,standard-class-slots)
       ;; Avoids an error on wrong initargs in mcl (through using &allow-other-keys)
       ;; and solves a subtle problem:
       ;; In the case that a persistent slot is a transient slot in a superclass and has a
       ;; default initvalue, it must be left unbound when the object is lazily recreated from the db.
       (defmethod initialize-instance :after ((,object ,name) &key %from-db-p &allow-other-keys)
         (when %from-db-p
           (mapc (lambda (slot-desc) 
                   (when (slot-exists-p ,object (car slot-desc))
                     (slot-makunbound ,object (car slot-desc))))
                 ',slots)))
       (when ',slots
         (with-database (*auth-db*)
           (when *initialize-db-auth*
             (create-table (print [?*schema* ',table])
                           ',(list* 
                              ;; internal field for object identity
                              '([object-id] integer :primary-key)
                              ;; entity-class
                              '([class] (varchar 32))
                              (mapcar (lambda (db-slot-description)
                                        (list* [(car db-slot-description)]
                                               (let ((slot-type (cadr db-slot-description)))
                                                 (cond ((eq slot-type 'string)
                                                        '(varchar 64))
                                                       ((eq slot-type 'text)
                                                        '(varchar 2000))
                                                       ((eq slot-type 'keyword)
                                                        '(varchar 32))
                                                       ((eq slot-type 'integer)
                                                        'integer)
                                                       ((listp slot-type) ;; column not needed
                                                        '(varchar 2000))
                                                       (t 'integer)))
                                               (loop for (option value)
                                                     on (cddr db-slot-description)
                                                     by #'cddr
                                                     when (and (eq option :no-nulls) value)
                                                     collect :not-null)))
                                      slots)))))
         ;; create indices
         ,@(mapcar (lambda (multiple-index-list)
                     `(with-database (*auth-db*)
                        (when *initialize-db-auth*
                          (create-index [?*schema* ',(intern (format nil
                                                                     "~a-INDEX~d"
                                                                     (symbol-name table)
                                                                     (incf index-count)))]
                                        :on [?*schema* ',table]
                                        :unique-p t
                                        :attributes ',multiple-index-list))))
                   multiple-index)
         ;; build internal slot accessors
         ,@(mapcar 
            (lambda (db-slot-description)
              (destructuring-bind (db-slot-name db-slot-type &key no-nulls index
                                                inverse-index read-only inverse)
                                  db-slot-description
                (declare (ignore inverse-index index no-nulls))
                (let* ((getter-name (intern (concat (or (symbol-name conc-name) "")
                                                    (symbol-name (car db-slot-description))) "HTTP"))
                       (list-slot-p (and (consp db-slot-type)
                                         (eq (car db-slot-type) 'http::set-of)))
                       (link-table (when list-slot-p
                                     (intern (concat (symbol-name table) "-"
                                                     (symbol-name db-slot-name)
                                                     "-LINK"))))
                       (inverse-object (gentemp (concat (symbol-name
                                                         (if list-slot-p
                                                           (cadr db-slot-type)
                                                           db-slot-type))
                                                        "-"))))
                  `(progn
                     ,(when list-slot-p
                        `(with-database (*auth-db*)
                           (when *initialize-db-auth*
                             (create-table (print [?*schema* ',link-table])
                                           ',(list 
                                              ;; link field for object
                                              '([link-id] integer)
                                              ;; value field
                                              `([value]
                                                ,(let ((slot-type (cadr db-slot-type)))
                                                   ;; factor this out!
                                                   (cond ((eq slot-type 'string)
                                                          '(varchar 64))
                                                         ((eq slot-type 'text)
                                                          '(varchar 2000))
                                                         ((eq slot-type 'keyword)
                                                          '(varchar 32))
                                                         ((eq slot-type 'integer)
                                                          'integer)
                                                         (t 'integer)))))))))
                     
                     ;; getter methods
                     (defmethod ,getter-name ((,object (eql ',name)))
                       [',db-slot-name])
                     (defmethod ,getter-name ((,object ,name))
                       (with-database (*auth-db*)
                         ,(if list-slot-p
                            `(let ((internal-rep
                                    (select [value] :flatp t
                                            :from [?*schema* ',link-table]
                                            :where [= [link-id] 
                                                      (slot-value ,object '%object-id)])))
                               ,(case (cadr db-slot-type)
                                  ((string text integer) 'internal-rep)
                                  (keyword `(mapcar (lambda (v) (intern v 'keyword))
                                                    internal-rep))
                                  (otherwise `(if (entity-class-p (cadr ',db-slot-type))
                                                (mapcar #'%make-entity internal-rep)
                                                internal-rep))))
                            `(let ((val ;; not needed for list-slot-p = t
                                    (car (select
                                          [',db-slot-name] :flatp t 
                                          :from [?*schema* ',table]
                                          :where [= [object-id]
                                                    (slot-value ,object '%object-id)]))))
                               ,(case db-slot-type
                                  ((string text integer) 'val)
                                  (keyword `(intern val 'keyword))
                                  (otherwise ; this can't be determined at compile time
                                   `(if (entity-class-p ',db-slot-type)
                                      (when val (%make-entity val))
                                      val)))))))
                     ;; setter methods
                     ,(unless read-only
                        `(defmethod (setf ,getter-name) (,value (,object ,name))
                           (with-database (*auth-db*)
                             ,(when list-slot-p
                                `(let ((link-id (slot-value ,object '%object-id)))
                                   (delete-records :from [?*schema* ',link-table]
                                                   :where [= [link-id] ?link-id])
                                   (mapcar (lambda (v)
                                             (insert-records 
                                              :into [?*schema* ',link-table]
                                              :values `(,link-id
                                                        ,,(case (cadr db-slot-type)
                                                            ((string text integer) 'v)
                                                            (keyword '(intern v 'keyword))
                                                            (otherwise `(if (entity-class-p
                                                                             (cadr ',db-slot-type))
                                                                          (slot-value v '%object-id)
                                                                          v))))))
                                           ,value)))
                             ,(unless list-slot-p
                                `(update-records 
                                  [?*schema* ',table]
                                  :attributes `(,[',db-slot-name])
                                  :values 
                                  (list
                                   ,(cond ((eq db-slot-type 'string) value)
                                          ((eq db-slot-type 'text) value)
                                          ((eq db-slot-type 'keyword) `(symbol-name ,value))
                                          (t `(if (entity-class-p ',db-slot-type)
                                                (slot-value ,value '%object-id)
                                                ,value))))
                                  :where [= [object-id]
                                            (slot-value ,object '%object-id)])))))
                     ,(when inverse
                        `(defmethod ,inverse ((,inverse-object ,(if list-slot-p
                                                                  (cadr db-slot-type)
                                                                  db-slot-type)))
                           (let ((inverse-list ()))
                             (for-each ((object ,name)
                                        (:where (,(if list-slot-p 'member 'eq)
                                                 ,inverse-object
                                                 (,getter-name object))))
                               (pushnew object inverse-list))
                             inverse-list)))))))
            slots)
         ;; need access to table and table name at runtime
         (defmethod entity-db-table ((,object ,name)) (values [?*schema* ',table] ',table))
         (defmethod entity-db-table ((,object (eql ',name))) (values [?*schema* ',table] ',table)))
       (unless ',slots
         (defmethod entity-db-table ((,object (eql ',name)))
           (dolist (class ',superclasses)
             (let ((table (entity-db-table class)))
               (when table (return table)))))))))

(defgeneric entity-db-table (entity))
(defmethod entity-db-table ((object t)) nil)

(defgeneric new-entity-id (entity))

(defmethod new-entity-id ((entity t))
  (with-database (*auth-db*)
    (floor (car (select [?*schema* entity-seq.nextval]
                        :from [sys dual] :flatp t)))))

(defun %id-table (object-id)
  (with-database (*auth-db*)
    (let ((table-name
           (car (select [table-name] :flatp t
                        :from [?*schema* id-table-mappings]
                        :where [= [object-id] ?object-id]))))
      (when table-name
        (sql::make-sql-expression table-name)))))

(defmethod %make-entity ((object-id integer))
  (print object-id)
  (or (gethash object-id *entity-table*)
      (with-database (*auth-db*)
        (let* ((table (%id-table object-id))
               (class (intern (car (select [class] :flatp t 
                                           :from table
                                           :where [= [object-id] ?object-id])) "HTTP"))
               (entity (make-instance class :%from-db-p t)))
          (setf (slot-value entity '%object-id) object-id)
          (setf (gethash object-id *entity-table*) entity)))))

(defgeneric db-representation (obj))

(defmethod db-representation ((obj t))
  obj)

(defmethod db-representation ((obj (eql nil)))
  obj)
  
(defmethod db-representation ((obj list))
  (mapcar #'db-representation obj))

(defmethod db-representation ((obj entity-mixin))
  (slot-value obj '%object-id))

(defmethod db-representation ((obj symbol))
  (symbol-name obj))

(defmethod make-entity ((class symbol) &rest initargs &key id &allow-other-keys)
  (or (and id (gethash id *entity-table*))
      (let* ((entity (apply #'make-instance class :%from-db-p id initargs))
             ;; get new object-id
             (object-id (or id (new-entity-id entity))))
        (unless id
          (with-database (*auth-db*)
            (multiple-value-bind (table table-name)
                                 (entity-db-table entity)
              (with-transaction
                (insert-records
                 :into table
                 :attributes
                 (list* [object-id] [class]
                        (loop for att in initargs by #'cddr
                              collect [?att]))
                 ;; *** what about saving slots that have been initialized
                 ;; through initialize-instance? No problem here, but in general.
                 :values
                 (list* object-id (symbol-name class)
                        (loop for (att val) on initargs by #'cddr
                              collect
                              (cond
                               ((consp val)
                                (mapcar (lambda (v)
                                          (insert-records 
                                           :into [?*schema* 
                                                  (intern 
                                                   (concat
                                                    (symbol-name table-name)
                                                    ;;(symbol-name class) ; ?? or some subclass!
                                                    "-" (symbol-name att) "-LINK"))] 
                                           :values (list object-id (db-representation v))))
                                        val))
                               (t (db-representation val))))))
                (insert-records
                 :into [?*schema* id-table-mappings]
                 :attributes (list [object-id] [table-name])
                 :values (list object-id (sql::build-sql-string table)))))))
        (setf (slot-value entity '%object-id) object-id
              (gethash object-id *entity-table*) entity)
        entity)))

(defmethod case-insensitive-representation ((obj string))
  (string-downcase obj))

(defmethod case-insensitive-representation ((obj symbol))
  (string-downcase (string obj)))

;; is LOWER Oracle specific?
(defmethod case-insensitive-representation ((obj sql::sql-expression))
  [lower ?obj])

(defun build-where-criterion (op att val)
  (case op
    (string-equal
     [= (case-insensitive-representation att)
        (case-insensitive-representation val)])
    (member
     [in (db-representation att) (db-representation val)])
    (otherwise
     [= (db-representation att) (db-representation val)])))

(defun %delete-object (object)
  "Deletes object from persistent store."
  (with-database (*auth-db*)
    (with-transaction
      (let ((db-table (entity-db-table object)))
        (with-slots (%object-id) object
          (delete-records :from db-table
                          :where [= [object-id] ?%object-id])
          (delete-records :from [?*schema* id-table-mappings]
                          :where [= [object-id] ?%object-id])
          (remhash %object-id *entity-table*))))))

; EOF
