;;;   -*- Mode: LISP; Package: resources; BASE: 10; Syntax: ANSI-Common-Lisp;-*-

;;;
;;; (c) Copyright  1995, 1997, 1999 John C. Mallery
;;;     All Rights Reserved.
;;;

;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modification History
;;;
;;; 11/28/96 bill  %deallocate-resource doesn't push the object on
;;;                the free list if it's already there.
;;;


;;;------------------------------------------------------------------- 
;;;
;;; RESOURCE MANAGEMENT
;;;
(eval-when (eval compile load)
  (defpackage resources
    (:use #-Genera"COMMON-LISP" #+Genera"FUTURE-COMMON-LISP")
    (:export 
      "*DEFAULT-RESOURCE-SIZE*"
      "ALLOCATE-RESOURCE"
      "CLEAR-RESOURCE"
      "DEALLOCATE-RESOURCE"
      "DEFRESOURCE"
      "DESCRIBE-RESOURCE"
      "MAP-RESOURCE"
      "MATCHING-RESOURCE"
      "RESOURCE"
      "USING-RESOURCE"))
  )

(in-package :resources)

(defclass resource
          ()
    ((constructor :initform nil :initarg :constructor :reader resource-constructor)
     (initializer :initform nil :initarg :initializer :reader resource-initializer)
     (deinitializer :initform nil :initarg :deinitializer :reader resource-deinitializer)
     (free-list :initform nil :initarg :free-list :reader resource-free-list)
     (pool :initform nil :initarg :pool :reader resource-pool)
     (name :initform nil :initarg :name :reader resource-name)
     (parameters :initform nil :initarg :parameters :reader resource-parameters)
     (initial-copies :initform 0 :initarg :initial-copies :reader resource-initial-copies)))

(defclass matching-resource
          (resource)
    ((matcher :initform nil :initarg :matcher :reader resource-matcher)
     (description :initform nil :initarg :description :reader resource-description)))

(defmethod print-object ((resource resource) stream)
  (with-slots (name) resource
    (print-unreadable-object (resource stream :type t :identity t)
      (when name (princ name stream)))))

(defmethod describe-resource ((resource symbol) &optional (stream *standard-output*))
  (describe-resource (symbol-value resource) stream)) 

(defmethod describe-resource ((resource resource) &optional (stream *standard-output*))
  (with-slots (name free-list pool initial-copies constructor initializer deinitializer) resource
    (format stream "~&~A is a resource of class ~S~&Resource Pool: ~D~&Free Objects: ~D~&Initial Copies: ~D~&Constructor: ~S~&Initializer: ~S~&Deinitializer: ~S"
            name (type-of resource) (length pool) (length free-list) initial-copies constructor initializer deinitializer)
    resource))

(defmethod describe-resource :after ((resource matching-resource) &optional (stream *standard-output*))
  (with-slots (matcher description) resource
    (format stream "~&Matcher: ~S~:[~;~&Description: ~:*~A~]" matcher description)
    resource)) 

;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

#+MCL
(defmacro with-atomic-execution (&body body)
  `(ccl:without-interrupts . ,body)) 

#+Lucid
(defmacro with-atomic-execution (&body body)
  `(ccl:without-interrupts . ,body))

#+LispWorks
(defmacro with-atomic-execution (&body body)
  `(lw:without-interrupts . ,body))

#+(and CLIM-SYS (not (or Lucid LispWorks CMU)))
(defmacro with-atomic-execution (&body body)
  `(clim-sys:without-scheduling . ,body))

#+CMU
(defmacro with-atomic-execution (&body body)
  `(system:without-interrupts . ,body)) 

#-(or MCL CLIM-SYS LispWorks Lucid CMU)
(defmacro with-atomic-execution (&body body)
  `(progn . ,body))

(defmacro %get-resource (name &optional error-p)
  (if error-p
      `(or (and (boundp ,name)
                (symbol-value ,name))
           (error "There is no resource named, ~S
Somebody might have clobbered the symbol value." ,name))
      `(and (boundp ,name)
            (symbol-value ,name))))

(defvar *all-resources* nil)

(defun %create-resource (name &key (class 'resource))
  (let ((resource (make-instance class :name name)))
    (set name resource)
    (push resource *all-resources*)
    resource))

(declaim (inline intern-resource))

(defun intern-resource (resource &key (if-does-not-exist :error) (class 'resource))
  (etypecase resource
    (symbol
      (or (%get-resource resource nil)
          (ecase if-does-not-exist
            (:soft nil)
            (:error (error "There is no resource named, ~S."))
            (:create (%create-resource resource :class class)))))
    (resource resource)))

(eval-when (load eval compile)
  (defun extract-parameters (args)
    (loop for arg in args
          unless (find arg lambda-list-keywords)
            collect (etypecase arg
                      (symbol arg)
                      (cons (first arg))))))

(defmacro define-resource-method (name operation lambda-list documentation &key declarations) 
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list
       ,documentation
       ,.(when declarations `(,declarations))
       (,operation (%get-resource ,(car lambda-list) t) ,@(cdr (extract-parameters lambda-list))))))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defmethod %deallocate-resource ((resource resource) object)
  "Return object to pool name. It's a bad idea to free an object to the wrong
   pool. Name is evaluated."
  (with-slots (free-list) resource
    (let (deinitializer)
      (when (setq deinitializer (resource-deinitializer resource))
        (funcall deinitializer resource object))
      (with-atomic-execution
        (unless (member object free-list :test 'eq)
          (push object free-list))))))

(defmethod %allocate-resource ((resource resource) args &aux newly-created-p)
  (with-slots (free-list) resource
    (flet ((create-object ()
             (let* ((constructor (resource-constructor resource))
                    (object (apply constructor resource args)))
               (vector-push-extend object (resource-pool resource))
               (setq newly-created-p t)
               object)))
      (declare (inline create-object))
      ;; find an object
      (let ((object (with-atomic-execution
                      (or (pop free-list) (create-object))))
            (initializer (resource-initializer resource)))
        (when initializer
          (apply initializer resource object args))
        (values object newly-created-p)))))

(defmethod %allocate-resource ((resource matching-resource) args &aux newly-created-p)
  (with-slots (free-list) resource
    (flet ((get-matching-object ()
             (let ((match-fctn (resource-matcher resource)))
               (with-atomic-execution
                 (loop for prev = nil then l
                       for l = free-list then (cdr l)
                       while l
                       when (or (null match-fctn)
                                (apply match-fctn resource (first l) args))
                         do (return (prog1 (first l)
                                           (if prev
                                               (setf (cdr prev) (cdr l))
                                               (setq free-list (rest l)))))
                       finally (return nil)))))
           (create-object ()
             (let* ((constructor (resource-constructor resource))
                    (object (apply constructor resource args)))
               (vector-push-extend object (resource-pool resource))
               (vector-push-extend args (resource-description resource))
               (setq newly-created-p t)
               object)))
      (declare (inline get-matching-object create-object))
      (let ((object (with-atomic-execution
                      (or (get-matching-object) (create-object))))
            (initializer (resource-initializer resource)))
        (when initializer
          (apply initializer resource object args))
        (values object newly-created-p)))))

(define-resource-method deallocate-resource %deallocate-resource (resource object)
                        "Return object to pool name. It's a bad idea to free an object to the wrong
pool. Name is evaluated.")

(define-resource-method allocate-resource %allocate-resource (resource &rest args)
                        "Get a copy of the NAMEd resource, given the args (for the initializer, 
   matcher and constructor). Name is evaluated.")

(defmacro using-resource ((variable resource &rest args) &body body)
  "VARIABLE is bound to an object from RESOURCE which is initialized with ARGS."
  `(let ((,variable (allocate-resource ',resource ,@args)))
     (unwind-protect
         (progn ,@body)
       (when ,variable
         (deallocate-resource ',resource ,variable))))) 

;;;------------------------------------------------------------------- 
;;;
;;; UTILITY OPERATIONS
;;;

(defmethod clear-resource ((resource symbol))
  (clear-resource (%get-resource resource t)))

(defmethod clear-resource ((resource resource))
  "Zaps Name's pool, and starts from empty. Normally only used within 
   DefResource when recompiled, or user should call if you change the
   constructor s.t. the old objects in the pool are obsolete. 

   Not a good idea to call this if active objects are in use."
  (with-slots (free-list pool description) resource
    (setq free-list nil)
    (fill pool nil :end (fill-pointer pool))
    (setf (fill-pointer pool) 0)
    resource))

(defmethod clear-resource :after ((resource matching-resource))
  (with-slots (description) resource
    (fill description nil :end (fill-pointer description))
    (setf (fill-pointer description) 0))
  resource)

(defmethod clear-resource :around ((resource resource))
  (with-atomic-execution
    (call-next-method)))

(defmethod map-resource ((resource symbol) function &rest args)
  (declare (dynamic-extent args))
  (apply #'map-resource (%get-resource resource t) function args))

(defmethod map-resource ((resource resource) function &rest args)
  "Calls function once for every object in the resource specified by re-
       source-name. function is called with the following arguments: 

       o  The object

       o  t if the object is allocated, or nil if it is free

       o  resource

       o  Any additional arguments specified by args"
  (declare (dynamic-extent args))
  (with-slots (pool free-list name) resource
    (loop for idx upfrom 0 to (1- (fill-pointer  pool))
          for object = (aref pool idx)
          for allocated-p = (not (member object free-list))
          do (apply function object allocated-p resource args))))

(defmethod number-of-resourced-objects ((resource resource))
  (with-slots (pool) resource
    (fill-pointer pool)))

(defmethod fill-resource ((resource resource) &optional number-of-copies)
  (declare (values n-copies))
  (with-slots (initial-copies parameters pool constructor description matcher) resource
    (let ((n-copies (- (or number-of-copies initial-copies)
                       (number-of-resourced-objects resource))))
      (when (plusp n-copies)
        (dotimes (i n-copies)
          (vector-push-extend (funcall constructor parameters) pool)))
      (values n-copies))))

(defmethod fill-resource :around ((resource matching-resource) &optional number-of-copies)
  (declare (values n-copies)
           (ignore number-of-copies))
  (with-slots (parameters description) resource
    (let ((n-copies (call-next-method)))
      (dotimes (i n-copies)
        (vector-push-extend parameters description))
      n-copies)))

(defmethod fill-resource :around ((resource resource) &optional number-of-copies)
  (declare (values n-copies))
  (with-atomic-execution
    (call-next-method resource number-of-copies)))

(defparameter *default-resource-size* 10.)

(defmethod initialize-resource ((resource resource) &rest args)
  (macrolet ((compiled-definition (fspec)
               `(typecase ,fspec
                  (null nil)
                  (compiled-function ,fspec)
                  (t (fdefinition ,fspec)))))
    (with-slots (constructor deinitializer initial-copies initializer name parameters pool) resource
      (destructuring-bind (&key set-name set-parameters set-constructor
                                set-initializer set-deinitializer set-initial-copies
                                (initial-resource-size *default-resource-size*)
                                &allow-other-keys)
          args
        (setq name set-name
              parameters (extract-parameters set-parameters)
              constructor (compiled-definition set-constructor)
              initializer (compiled-definition set-initializer)
              deinitializer (compiled-definition set-deinitializer)
              initial-copies set-initial-copies)
        (unless pool
          (setq pool (make-array initial-resource-size :adjustable t :fill-pointer t)))
        (clear-resource resource)
        (fill-resource resource initial-copies)
        resource))))

(defmethod initialize-resource :before ((resource matching-resource) &rest args)
  (macrolet ((compiled-definition (fspec)
               `(typecase ,fspec
                  (compiled-function ,fspec)
                  (t (fdefinition ,fspec)))))
    (with-slots (matcher description) resource
      (destructuring-bind (&key set-matcher (initial-resource-size *default-resource-size*)
                                &allow-other-keys) args
        (setq matcher (and set-matcher (compiled-definition set-matcher)))
        (unless description
          (setq description (make-array initial-resource-size :adjustable t :fill-pointer t)))))))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defmacro defresource (name parameters &key constructor initializer deinitializer
                       (initial-copies 0) matcher)
  "Name, an unevaluated symbol, will become the name of the new resource.
   PARAMETERS, a lambda list, are used to initialize (create) instances of the 
   resource, and come from allocate-resource (so it can be used to supply, 
   e.g. default arguments)

   CONSTRUCTOR is a function to call to make a new object when the resource
   is empty, and accepts the PARAMETERS as arguments. Note this is required.

   Options are:

        :INITIAL-COPIES (used to set up the pool to begin with).

        :INITIALIZER (called on a newly allocated object, and the other 
        parameters). Note the constructor isn't called on objects that
        are already in the pool.

        :DEINITIALIZER (called on a newly freed object) Useful to allow gc
        of objects the resource refers to.

        :MATCHER Args are like initializer, but is expected to be a predicate
        that succeeds if the unallocated pool object is appropriate for the 
        call. The default one assumes only pool objects created with the same
        parameters are appropriate.
        This is useful if you are going to put different size objects in the
        pool, but don't want to have to create a new object when a (bigger)
        one already exists."
  `(initialize-resource (intern-resource ',name :if-does-not-exist :create
                                         :class ',(if matcher 'matching-resource 'resource))
                        :set-name ',name
                        :set-parameters ',parameters
                        :set-constructor ',constructor
                        :set-initializer ',initializer
                        :set-deinitializer ',deinitializer
                        :set-matcher ',matcher
                        :set-initial-copies ,initial-copies))

#|
(defun make-test (resource foo bar baz)
  (print (list 'make-test resource foo bar baz))
  (list 'make-test foo bar baz))

(defun initializer (resource object foo bar baz)
  (print (list 'initializer resource object foo bar baz))
  object)

(defun deinitializer (resource object)
  (print (list 'deinitialize resource object))
  object)

(defresource test
             (foo bar baz)
  :constructor 'make-test
  :initializer 'initializer
  :deinitializer 'deinitializer)
|#
