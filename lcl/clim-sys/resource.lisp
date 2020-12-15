;;;   -*- Mode: LISP; Package: MINP; BASE: 10; Syntax: ANSI-Common-Lisp;-*-

;;;
;;; (c) Copyright  1995, John C. Mallery
;;;     All Rights Reserved.
;;;
;;; Minor edits by OBC to be compatible with CLIM-SYS implementations
;;;


;;;------------------------------------------------------------------- 
;;;
;;; RESOURCE MANAGEMENT
;;;

(in-package "MINP")

;;; Moved package definition to CLIM-SYS/PACKAGE
;;; See also CLIM-SYS/PROCESS file.

#+ignore
(defpackage "MINP"
  (:use)
  (:export
   "ALLOCATE-RESOURCE" "CLEAR-RESOURCE" "DEALLOCATE-RESOURCE" "DEFRESOURCE"
   "MAP-RESOURCE" "USING-RESOURCE"))

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


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

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

;;; Declaim story continues
;;;
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

(defmacro define-resource-method (name operation lambda-list documentation)
  `(progn
     (declaim (inline name))
     (defun ,name ,lambda-list
       ,documentation
       (,operation (%get-resource ,(car lambda-list) t)
		   ;; minor change - OBC
		   ,@(extract-parameters (rest lambda-list))))))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defmethod %deallocate-resource ((resource resource) object)
  "Return object to pool. Don't try and free an object to the wrong pool."
  (with-slots (deinitializer) resource
    (when deinitializer
      ;; Remove resource argument
      (funcall deinitializer #+ignore resource object))
    (clim-sys:without-scheduling
      (with-slots (free-list) resource
	(push object free-list)))
    object))

(defmethod %allocate-resource ((resource resource) args)
  (with-slots (initializer constructor free-list pool name) resource
    (flet ((create-object ()
             (let ((object (apply constructor resource args)))
               (vector-push-extend object pool)
               object)))
      (declare (inline create-object))
      ;; find an object
      (let ((object (clim-sys:without-scheduling
                      (cond ((pop free-list))
                            (t (create-object))))))
        (when initializer
	  ;; Remove resource argument
          (apply initializer #+ignore resource object args))
        object))))

(defmethod %allocate-resource :around ((resource matching-resource) args)
  (with-slots (matcher initializer free-list description) resource
    (flet ((get-matching-object ()
             (clim-sys:without-scheduling
               (loop for l = free-list then (cdr l)
                     for prev = nil then l
                     when (apply matcher (first l) args)
                       do (return (prog1 (first l)
                                         (if prev
                                             (setf (cdr prev) (cdr l))
                                             (setq free-list (rest l)))))
                     finally (return nil)))))
      (declare (inline get-matching-object))
      (clim-sys:without-scheduling
        (let ((object (get-matching-object)))
          (cond (object
                 (when initializer
		   ;; Remove resource argument
                   (apply initializer #+ignore resource object args)))
                (t (multiple-value-bind (object newly-created-p)
                       (call-next-method)
                     (when newly-created-p
		       ;; This one does not seem necessary since see above
                       (clim-sys:without-scheduling
                         (vector-push-extend (copy-list args) description)))
                     (values object newly-created-p)))))))))

(define-resource-method
  deallocate-resource %deallocate-resource (resource object)
  "Return object to pool name. It's a bad idea to free an object to the wrong
pool. Name is evaluated.")

(define-resource-method
  allocate-resource %allocate-resource (resource &rest args)
  "Get a copy of the NAMEd resource, given the args (for the initializer, 
   matcher and constructor). Name is evaluated.")

(defmacro using-resource ((variable resource &rest args) &body body)
  "VARIABLE is bound to an object from RESOURCE which is initialized with ARGS."
  `(let* ((,variable (allocate-resource ',resource ,@args))) ;Resource now quoted to behave like CLIM-SYS resources - OBC
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
  (clim-sys:without-scheduling
    (call-next-method)))

;;; Notice FUNCTION is first argument in CLIM-SYS specfication - OBC
;;;
(defmethod map-resource (function (resource symbol) &rest args)
  (apply #'map-resource function (%get-resource resource t) args))

(defmethod map-resource (function (resource resource) &rest args)
  "Calls function once for every object in the resource specified by re-
       source-name. function is called with the following arguments: 

       o  The object

       o  t if the object is allocated, or nil if it is free

       o  resource

       o  Any additional arguments specified by args"
  (if args
      (warn "Extra arguments ~s are not supported by the CLIM-SYS resource specification"))
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
    (let ((n-copies (- (number-of-resourced-objects resource)
                       (or number-of-copies initial-copies))))
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
  (clim-sys:without-scheduling
    (call-next-method resource number-of-copies)))

(defparameter *default-resource-size* 10.)

(defmethod initialize-resource ((resource resource) &rest args)
  (macrolet ((compiled-definition (fspec)
               `(typecase ,fspec
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
          (setq description (make-array initial-resource-size :adjustable t :fill-pointer t)))
        (call-next-method)))))


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
  ;; Change to insure compilation of lambda forms - OBC
  (flet ((compquotify (x)
	   (if (symbolp x)
	       (list 'quote x)
	     x)))
    `(initialize-resource (intern-resource ',name :if-does-not-exist :create
					   :class ',(if matcher 'matching-resource 'resource))
			  :set-name ',name
			  :set-parameters ',parameters
			  :set-constructor ,(compquotify constructor)
			  :set-initializer ,(compquotify initializer)
			  :set-deinitializer ,(compquotify deinitializer)
			  :set-matcher ,matcher
			  :set-initial-copies ,initial-copies)))

;;; Great idea to include some tests.
;;; Keeping them up to date - OBC
#||
(defun make-test (resource foo bar baz)
  (print (list 'make-test resource foo bar baz))
  (list 'make-test foo bar baz))

;;; Notice the resource argument is not needed anymore
(defun test-initializer (#+ignore resource object foo bar baz)
  (print (list 'initializer 'test object foo bar baz))
  object)

(defun test-deinitializer (#+ignore resource object)
  (print (list 'deinitialize 'test object))
  object)

(defresource test
  (foo bar baz)
  ;; Unquote these symbols
  :constructor make-test
  :initializer test-initializer
  :deinitializer test-deinitializer)

(defresource test1
  (a b c)
  :constructor #'(lambda (resource a b c)
		   (print (list 'construct resource a b c)))
  :initializer #'(lambda (object a b c)
		   (print (list 'initialize object a b c))
		   object)
  :deinitializer #'(lambda (object)
		     (print (list 'deinitialize object))
		     object))
||#

;;; Trace
#||
;;; First test
MINP(64): (setq x (allocate-resource 'test 1 2 3))

(MAKE-TEST #<RESOURCE TEST @ #x145de02> 1 2 3) 
(INITIALIZER TEST (MAKE-TEST 1 2 3) 1 2 3) 
(MAKE-TEST 1 2 3)
MINP(65): (map-resource #'(lambda (&rest xx) (print xx)) 'test)

((MAKE-TEST 1 2 3) T #<RESOURCE TEST @ #x145de02>) 
NIL
MINP(66): (describe x)
(MAKE-TEST 1 2 3) is a CONS
MINP(67): (deallocate-resource 'test x)

(DEINITIALIZE TEST (MAKE-TEST 1 2 3)) 
((MAKE-TEST 1 2 3))
MINP(68): (map-resource #'(lambda (&rest xx) (print xx)) 'test)

((MAKE-TEST 1 2 3) NIL #<RESOURCE TEST @ #x145de02>) 
NIL
MINP(69):
;;; Second test - works here when defresource expr is compiled.
;;; Actually this extension to support lambdas does not seem
;;; portable so don't use it until CLIM-SYS implementations fix it...
MINP(77): (setq x (allocate-resource 'test1 1 2 3))

(CONSTRUCT #<RESOURCE TEST1 @ #xe106e2> 1 2 3) 
(INITIALIZE (CONSTRUCT #<RESOURCE TEST1 @ #xe106e2> 1 2 3) 1 2 3) 
(CONSTRUCT #<RESOURCE TEST1 @ #xe106e2> 1 2 3)
MINP(78): (map-resource #'(lambda (&rest xx) (print xx)) 'test1)

((CONSTRUCT #<RESOURCE TEST1 @ #xe106e2> 1 2 3) T #<RESOURCE TEST1 @ #xe106e2>) 
NIL
MINP(79): (deallocate-resource 'test1 x)

(DEINITIALIZE (CONSTRUCT #<RESOURCE TEST1 @ #xe106e2> 1 2 3)) 
((CONSTRUCT #<RESOURCE TEST1 @ #xe106e2> 1 2 3))
MINP(80): (map-resource #'(lambda (&rest xx) (print xx)) 'test1)

((CONSTRUCT #<RESOURCE TEST1 @ #xe106e2> 1 2 3) NIL
 #<RESOURCE TEST1 @ #xe106e2>) 
NIL
MINP(81): 
||#
