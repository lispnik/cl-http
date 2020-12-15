;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: www-present -*-

;;; (C) Copyright 1996, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;
;;; Christopher R. Vincent
;;; cvince@ai.mit.edu
;;;
;;; (C) Enhancements Copyright 1997, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;;
;;;------------------------------------------------------------------- 
;;;
;;; BASIC PRESENTATION SYSTEM FOR THE WORLD-WIDE WEB
;;;

#|

system.lisp contains the basic system for defining presentation-types and
presentation-generic-functions, etc. the interface to this module should stay
the same when an application uses a subset or superset of the standard
presentation-generic-functions. all the other files are basically
configurations.

|#


(in-package :www-present)

;;;------------------------------------------------------------------- 
;;;
;;; CONDITION HANDLING
;;;

(define-condition presentation-condition
                  (condition)
  ()
  (:report (lambda (condition stream)
             (format stream "~A was signalled." (type-of condition)))))

(define-condition presentation-type-class-not-found (presentation-condition) ())

(define-condition presentation-type-not-found (presentation-condition) 
  ((name :initarg :name :reader presentation-type-not-found-name))
  (:report (lambda (condition stream)
             (let* ((name (presentation-type-not-found-name condition))
                    (string (typecase name
                              (string name)
                              (t (write-to-string name)))))
               (format stream "The presentation type ~A could not be found." string)))))

(define-condition presentation-generic-function-not-found (presentation-condition) ())

(define-condition presentation-input-condition (presentation-condition) ())

(define-condition presentation-parse-error (presentation-input-condition) ())

(define-condition handle-input-error (presentation-parse-error) 
  ((object :initarg :object :reader handle-input-error-object)
   (type :initarg :type :reader handle-input-error-type)
   (stream :initarg :stream :reader handle-input-error-stream)
   (view :initarg :view :reader handle-input-error-view)
   (args :initarg :args :reader handle-input-error-args))
  (:report (lambda (condition stream)
             (let ((string (write-to-string (handle-input-error-object condition)))
                   (type (handle-input-error-type condition)))
               (format stream "The input ~A is not of the required type, ~S" string type)))))
   
(define-condition input-not-of-required-type (presentation-parse-error)
  ((string :initarg :string :reader input-not-of-required-type-string)
   (type :initarg :type :reader input-not-of-required-type-type))
  (:report (lambda (condition stream)
             (let ((string (input-not-of-required-type-string condition))
                   (type (input-not-of-required-type-type condition)))
               (format stream "The input ~A is not of the required type, ~S" string type)))))
               
(defun input-not-of-required-type (object type)
  "this function only signals an error, does not return."
  (error 'input-not-of-required-type
         :string (write-to-string object)
         :type type))


;;;------------------------------------------------------------------- 
;;;
;;; UTILITIES
;;;

;; this conditionalization should be moved out of portable code.  12/15/96 --
;; JCMa.
(defmacro get-class-precedence-list (class)
  `(#+CMU pcl:class-precedence-list
    #+MCL CCL:class-precedence-list
    #-(or MCL CMU) clos:class-precedence-list
    ,class))


(defun declare-lambda-list-ignorable-form (lambda-list)
  "build a declare ignorable form for a lambda-list."
  (flet ((arg-names (item)
	   (etypecase item
	     (list (cond ((eq (length item) 3)
			  (list (first item) (third item)))
			 (t (list (first item)))))
	     (atom (unless (member item '(&optional &key &allow-other-keys &rest &aux))
		     (list item))))))
    (declare (inline arg-names))
    (loop for item in lambda-list
	  append (arg-names item) into result  
	  finally (return `(declare (ignorable ,@result))))))

(defun member-lambda-list (symbol lambda-list)
  "determine if a symbol is an argument of a lambda list"
  (loop for item in lambda-list
        when (cond ((atom item) (eql symbol item))
                   (t (eql symbol (first item))))
          return t
        finally (return nil)))


;;;------------------------------------------------------------------- 
;;;
;;; PRESENTATION TYPE CLASSES
;;;

(defmethod make-load-form ((presentation-type-class presentation-type-class) 
                           #+(or ANSI-CL DRAFT-ANSI-CL-2) &optional
                           #+(or ANSI-CL DRAFT-ANSI-CL-2) environment)
  #+(or ANSI-CL DRAFT-ANSI-CL-2)
  (declare (ignore environment))
  (make-load-form-saving-slots presentation-type-class))

(defvar *presentation-type-class-table* (make-hash-table)
  "an internal presentation-type-class structure is used for method dispatch")

(defun get-presentation-type-class (name &optional (errorp t))
  "get an exact match for a presentation-type-class, accepts a presentation-type name"
  (check-type name symbol)
  (let ((match (gethash name *presentation-type-class-table*)))
    (unless (or match (not errorp))
      (error 'presentation-type-class-not-found))
    match))

(defun generate-presentation-type-class-prototype (name)
  "generate a class prototype for method dispatch on a presentation-type-class, accepts a presentation-type name"
  (check-type name symbol)
  (let ((class (get-presentation-type-class name t)))
    (make-instance (class-name class))))

(defmacro define-presentation-type-class (name &key superiors)
  "returns a presentation-class name for a presentation-type, accepts presentation-type name and inheritance"
  (flet ((get-class-name (name)
           (let ((class (get-presentation-type-class name t)))
             (when class (class-name class)))))
    (check-type name symbol)
    (let ((superior-names (or (mapcar #'get-class-name superiors) '(presentation-type-class)))
          (class-name (intern (string-upcase (concatenate 'string (symbol-name name) 
                                                          "-presentation-class")))))
      `(progn 
         (defclass ,class-name ,superior-names ()
           (:documentation ,(format nil "class for presentation-type ~S" name)))
         (setf (gethash ',name *presentation-type-class-table*)
               (find-class ',class-name))
         ',class-name))))


;;;------------------------------------------------------------------- 
;;;
;;; PRESENTATION TYPES
;;;

(defmethod print-object ((presentation-type presentation-type) stream)
  (with-slots (name) presentation-type
    (print-unreadable-object (presentation-type stream :type t :identity t)
      (when (slot-boundp presentation-type 'name)
        (write-string (symbol-name name) stream)))))

(defmethod make-load-form ((presentation-type presentation-type)
                           #+(or ANSI-CL DRAFT-ANSI-CL-2) &optional
                           #+(or ANSI-CL DRAFT-ANSI-CL-2) environment)
  #+(or ANSI-CL DRAFT-ANSI-CL-2)
  (declare (ignore environment))
  (let ((name (presentation-type-name presentation-type))
        (superior-symbols (mapcar #'presentation-type-name
                                  (presentation-type-superiors presentation-type))))
    (values
      `(make-instance ',(class-name (find-class 'presentation-type))
                      :name ',name
                      :class ',(slot-value presentation-type 'class)
                      :parameters ',(slot-value presentation-type 'parameters)
                      :options ',(slot-value presentation-type 'options)
                      :option-accept-args ',(slot-value presentation-type 'option-accept-args)
                      :superiors nil
                      :inherit-from ',(slot-value presentation-type 'inherit-from)
                      :description ,(slot-value presentation-type 'description)
                      :class-key ,(slot-value presentation-type 'class-prototype))
      `(setf (presentation-type-superiors (get-presentation-type ',name))
             (mapcar #'get-presentation-type ',superior-symbols)))))

(defvar *presentation-type-table* (make-hash-table)
  "presentation-types stored in hash table are keyed on symbols")

(defmethod get-presentation-type ((name symbol) &optional (errorp t))
  "get an exact match for a presentation-type from a symbol"
  (let ((match (gethash name *presentation-type-table*)))
    (unless (or match (not errorp))
      (error 'presentation-type-not-found :name name))
    match))

(defmethod get-presentation-type ((class standard-class) &optional (errorp t))
  "get an exact match for a presentation-type from a class"
  (get-presentation-type (class-name class) errorp))

(defmethod get-presentation-type ((class built-in-class) &optional (errorp t))
  "get an exact match for a presentation-type from a class"
  (get-presentation-type (class-name class) errorp))

(defun %find-presentation-type (class &optional (errorp t))
  "find the most specific defined presentation-type from a class"
  (let* ((precedence-list (get-class-precedence-list class))
         (match (loop for class in precedence-list
                      for presentation-type = (get-presentation-type class nil)
                      while (not presentation-type)
                      finally (return presentation-type))))
    (unless (or match (not errorp))
      (error 'presentation-type-not-found :name class))
    match))

(defmethod find-presentation-type ((class standard-class) &optional (errorp t) environment)
  "find the most specific defined presentation-type from a standard-class"
  (declare (ignore environment))
  (%find-presentation-type class errorp))

(defmethod find-presentation-type ((class built-in-class) &optional (errorp t) environment)
  "find the most specific defined presentation-type from a built-in-class"
  (declare (ignore environment))
  (%find-presentation-type class errorp))

(defmethod find-presentation-type ((name symbol) &optional (errorp t) environment)
  "find the most specific defined presentation-type from a class name"
  (find-presentation-type (find-class name t environment) errorp environment))

(defmethod get-presentation-type-parameters ((presentation-type presentation-type) &optional (errorp t))
  "return the presentation type parameters for a presentation-type"
  (declare (ignore errorp))
  (presentation-type-parameters presentation-type))

(defmethod get-presentation-type-parameters ((name symbol) &optional (errorp t))
  "return the presentation type parameters for presentation-type name"
  (let ((match (get-presentation-type name errorp)))
    (when match (presentation-type-parameters match))))

(defmethod get-presentation-type-options ((presentation-type presentation-type) &optional (errorp t))
  "return the presentation type options for a presentation-type"
  (declare (ignore errorp))
  (presentation-type-options presentation-type))

(defmethod get-presentation-type-options ((name symbol) &optional (errorp t))
  "return the presentation type options for presentation-type name"
  (let ((match (get-presentation-type name errorp)))
    (when match (presentation-type-options match))))

(defmethod presentation-type-precedence-list ((presentation-type presentation-type) &optional (errorp t))
  "find the presentation-type precedence list according to the slot values of superiors"
  (declare (ignore errorp))
  (labels ((build-precedence-list (type-list)
             (let ((superiors 
                     (loop with result-list = nil
                           for type in type-list
                           for type-superiors = (presentation-type-superiors type)
                           when type-superiors
                             do (loop for superior in type-superiors
                                      unless (or (member superior result-list) (null superior))
                                        do (setq result-list (nconc result-list (list superior))))
                           finally (return result-list))))
               (cond ((null superiors) nil)
                     (t (concatenate 'list superiors (build-precedence-list superiors)))))))
    (nconc (list presentation-type) (build-precedence-list (list presentation-type)))))

(defmethod presentation-type-precedence-list ((name symbol) &optional (errorp t))
  "find the presentation-type precedence list according to the slot values of superiors"
  (let ((match (get-presentation-type name errorp)))
    (when match (presentation-type-precedence-list match errorp))))

(defun presentation-type-of (object &optional (errorp t) environment)
  "find the most specific defined presentation-type for an object by class name.
used by present as a best guess, but only works for types with no required parameters."
  (let* ((class (class-of object))
         (match (find-presentation-type class errorp environment)))
    (unless (or match (not errorp))
      (error 'presentation-type-not-found))
    (presentation-type-name match)))

(defmethod get-presentation-type-class-prototype ((presentation-type presentation-type) &optional (errorp t))
  "get the class-prototype for a presentation-type, used for method dispatch"
  (declare (ignore errorp))
  (presentation-type-class-prototype presentation-type))

(defmethod get-presentation-type-class-prototype ((name symbol) &optional (errorp t))
  "get the class-prototype for a presentation-type name, used for method dispatch"
  (let ((match (get-presentation-type name errorp)))
    (when match (get-presentation-type-class-prototype match errorp))))

(defun get-superiors-from-inherit-from (inherit-from)
  "get the names of superior presentation types from the define-presentation-type inherit-from syntax"
  ;; kludge to get around meta-evaluating inherit-from, just grab what we need
  ;; second style known to work for Allegro, should really make this portable sometime
  (flet ((extract-name (backquote)
           #+(or MCL Genera Lispworks Lucid CMU)
           (cond ((atom backquote) backquote)
                 ((atom (cadadr backquote)) (cadadr backquote))
                 (t (cadadr (second backquote))))
           #-(or MCL Genera Lispworks Lucid CMU)
           (cond ((atom backquote) backquote)
                 ((atom (caaadr backquote)) (caaadr backquote))
                 (t (caaaar (second backquote))))))
    (declare (inline extract-name))
    (when (and (consp inherit-from) (eql 'quote (first inherit-from)))
      (setq inherit-from (second inherit-from)))
    (cond ((atom inherit-from) (list inherit-from))
          ((eql 'and (car inherit-from)) (mapcar #'extract-name (cdr
                                                                  inherit-from)))
          (t (list (extract-name inherit-from))))))

(defun decode-parameters-list (parameters-list)
  "return a lambda-list from the parameters arg to define-presentation-type, ensures &rest arg"
  (cond ((member '&rest parameters-list) parameters-list)
        (t (let ((insert-pos (position-if #'(lambda (item) (member item '(&key &aux))) parameters-list)))
             (concatenate 'list 
                          (subseq parameters-list 0 insert-pos)
                          '(&rest rest-args)
                          (when insert-pos (subseq parameters-list insert-pos)))))))

(defun decode-options-list (options-list)
  "return a lambda-list and an accept-args alist from the options arg to define-presentation-type"
  (loop for item in options-list
        when (atom item) collect item into lambda-list
        else when (<= (length item) 3) collect item into lambda-list
        else collect (subseq item 0 2) into lambda-list
             and collect (cons (intern (symbol-name (car item)) :keyword) (subseq item 3)) into options-list
        finally (return (values (concatenate 'list 
                                             '(&key) 
                                             lambda-list 
                                             (unless (member-lambda-list 'description lambda-list)
                                               '(description))
                                             '(&allow-other-keys))
                                options-list))))

(defmacro define-presentation-type (name parameters
                                    &key options inherit-from description
                                    &environment environment)
  "define a presentation-type, optionally for a clos class."
  (check-type name symbol)
  (let ((class (find-class name nil environment))
        (parameters-lambda-list (decode-parameters-list parameters))
        (superiors (cond (inherit-from (get-superiors-from-inherit-from inherit-from))
                         ((eql name t) nil)
                         (t '(t)))))
    (multiple-value-bind (options-lambda-list option-accept-args)
        (decode-options-list options)
      (unless description
        (setq description (format nil "presentation-type ~[for class~;named~] ~S." (cond (class 0) (t 1)) name)))
      `(eval-when (compile load eval)
         (progn
           (define-presentation-type-class ,name :superiors ,superiors)
           (setf (gethash ',name *presentation-type-table*)
                 (make-instance 'presentation-type
                                :name ',name
                                :class (find-class ',name nil)
                                :parameters ',parameters-lambda-list
                                :options ',options-lambda-list
                                :option-accept-args ',option-accept-args
                                :superiors (mapcar #'get-presentation-type ',superiors)
                                :inherit-from ',inherit-from
                                :description ,description
                                :class-prototype (generate-presentation-type-class-prototype ',name))))))))
 
(defmacro define-presentation-types (arg-lists)
  "invoke multiple calls of define-presentation-type on a list of arg-lists"
  `(progn ,.(loop for list in arg-lists
                  collect `(define-presentation-type ,@list) into result
                  finally (return result))))

(defmacro remove-presentation-type (name)
  "remove a presentation-type"
  (check-type name symbol)
  (get-presentation-type name)
  `(remhash ',name *presentation-type-table*))

(defmacro remove-presentation-types (name-list)
  "invoke multiple calls of remove-presentation-type on a list of symbols"
  `(progn ,.(loop for name in name-list
                  collect `(remove-presentation-type ,name) into result
                  finally (return result))))


;;;------------------------------------------------------------------- 
;;;
;;; PRESENTATION TYPE SPECIFIERS
;;;

(defgeneric acceptable-presentation-type-specifier (specifier)
  (:documentation "Determines if specifier is an acceptable presentation-type-specifier."))
(defmethod acceptable-presentation-type-specifier ((specifier symbol))
  (when specifier t))
(defmethod acceptable-presentation-type-specifier ((specifier cons)) 
  (or (symbolp (car specifier))
      (symbolp (caar specifier))))
(defmethod acceptable-presentation-type-specifier (specifier) 
  (declare (ignore specifier))
  nil)

(deftype presentation-type-specifier ()
  `(satisfies acceptable-presentation-type-specifier))

(defmacro with-presentation-type-decoded ((name-var &optional parameters-var options-var)
                                          type &body body)
  "bind the parts of a presentation-type-specifier"
  `(progn 
     (check-type ,type presentation-type-specifier)
     (multiple-value-bind (,.(when name-var `(,name-var))
                           ,.(when parameters-var `(,parameters-var))
                           ,.(when options-var `(,options-var)))
         (cond ((atom ,type)
                (values ,.(when name-var `(,type))
                        ,@(when parameters-var `(nil))
                        ,@(when options-var `(nil))))
               ((atom (car ,type))
                (values ,.(when name-var `((car ,type)))
                        ,.(when parameters-var `((cdr ,type)))
                        ,@(when options-var `(nil))))
               (t (values ,.(when name-var `((caar ,type)))
                          ,.(when parameters-var `((cdar ,type)))
                          ,.(when options-var `((cdr ,type))))))
       ,@body)))

(defmacro with-presentation-type-parameters ((name type) &body body)
  "bind the presentation-type parameters for a presentation-type-specifier"
  (let ((parameter-lambda-list (get-presentation-type-parameters name t)))
    `(with-presentation-type-decoded (nil parameter-args) ,type
       (destructuring-bind ,parameter-lambda-list parameter-args
         ,(declare-lambda-list-ignorable-form parameter-lambda-list)
         ,@body))))

(defmacro with-presentation-type-options ((name type) &body body)
  "bind the presentation-type options for a presentation-type-specifier"
  (let ((option-lambda-list (get-presentation-type-options name t)))
    `(with-presentation-type-decoded (nil nil option-args) ,type
       (destructuring-bind ,option-lambda-list option-args
         ,(declare-lambda-list-ignorable-form option-lambda-list)
         ,@body))))

         
;;;------------------------------------------------------------------- 
;;;
;;; PRESENTATION VIEWS
;;;

(defmacro define-presentation-view-class (name &key inherits-from (description (format nil "the view class ~S" name)))
  "define a view class"
  `(defclass ,name ,inherits-from () (:documentation ,description)))

(defmacro define-presentation-view-classes (arg-lists)
  "invoke multiple calls on define-view-class on a list of arg-lists"
  `(progn ,.(loop for list in arg-lists
                  collect `(define-presentation-view-class ,@list) into result
                  finally (return result))))

(defmacro define-presentation-view (name class &optional (errorp t) &environment environment)
  "define an instance of a view class"
  (let ((found-class (find-class class errorp environment)))
    (when found-class
      `(defparameter ,name (make-instance ',class)))))

(defvar *stream-default-views* (make-hash-table)
  "store default presentation-views for streams")

(defmacro stream-default-view (stream)
  "returns default view, use setf to set default view"
  `(gethash ,stream *stream-default-views*))
 

;;;------------------------------------------------------------------- 
;;;
;;; PRESENTATION GENERIC FUNCTIONS
;;;

(defvar *presentation-generic-function-table* (make-hash-table)
  "hash table keyed on function name that user sees, stores actual generic function name with lambda list")

(defun get-presentation-generic-function-info (presentation-function-name &optional (errorp t))
  "retrieve the generic function name and lambda-list for presentation-function-name"
  (check-type presentation-function-name symbol)
  (let ((info (gethash presentation-function-name *presentation-generic-function-table*)))
    (unless (or info (not errorp))
      (error 'presentation-generic-function-not-found))
    info))

(defmacro call-presentation-generic-function (presentation-function-name args)
  "call a presentation-generic-function on a list of arguments"
  (check-type presentation-function-name symbol)
  (check-type args list)
  (let ((generic-function-name (first (get-presentation-generic-function-info presentation-function-name t)))
        (type-name (car args)))
    `(funcall ',generic-function-name (get-presentation-type-class-prototype ,type-name) ,@(cdr args))))

(defmacro funcall-presentation-generic-function (presentation-function-name &rest args)
  "call a presentation-generic-function on multiple arguments"
  `(call-presentation-generic-function ,presentation-function-name ,args))

(defmacro define-presentation-generic-function (generic-function-name 
                                                presentation-function-name lambda-list &rest options)
  "define a presentation-generic-function, with user-visible and internal names"
  (let ((generic-info (cons generic-function-name lambda-list)))
    `(progn (defgeneric ,generic-function-name ,lambda-list ,@options)
            (setf (gethash ',presentation-function-name *presentation-generic-function-table*) ',generic-info))))
    
(defmacro remove-presentation-generic-function (presentation-function-name)
  "remove a presentation-generic-function"
  (check-type presentation-function-name symbol)
  (let ((generic-function-name (first (get-presentation-generic-function-info presentation-function-name t))))
    `(progn (fmakunbound ',generic-function-name)
            (remhash ',presentation-function-name *presentation-generic-function-table*))))


;;;------------------------------------------------------------------- 
;;;
;;; PRESENTATION METHODS
;;;

(defmacro define-presentation-method (presentation-function-name &rest body)
  "add a method to the presentation-generic-function with the visible name presentation-function-name"
  #+Genera
  (declare (scl:arglist presentation-function-name [qualifiers]* specialized-lambda-list &body body)
	   (zwei:indentation 0 1 1 2 2 1))
  (let* ((generic-function-info (get-presentation-generic-function-info presentation-function-name t))
         (generic-function-name (first generic-function-info))
         (generic-lambda-list (cdr generic-function-info))
         (qualifiers (loop for item in body
                           while (atom item)
                           collect item into result
                           do (pop body)
                           finally (return result)))
         (specialized-lambda-list (pop body))
         (documentation (when (stringp (first body)) (list (pop body))))
         (declarations (loop for item in body
                             while (consp item) 
                             while (eq 'declare (first item))
                             collect item into result
                             do (pop body)
                             finally (return result)))
         (options-var (when (member 'options generic-lambda-list) '(options)))
         (parameters-var (when (member 'parameters generic-lambda-list) '(parameters)))
         (method-lambda-list `(,@parameters-var ,@options-var ,@specialized-lambda-list))
         (type-pos (1- (position 'type generic-lambda-list)))
         (method-type (nth type-pos method-lambda-list))
         (type-var (typecase method-type 
                     (cons (first method-type))
                     (t method-type)))
         (type-name (when (consp method-type) (second method-type)))
         (type-key (typecase method-type
                     (cons `(,(first generic-lambda-list) ,(class-name (get-presentation-type-class type-name t))))
                     (t (first generic-lambda-list)))))
    (setf (nth type-pos method-lambda-list) type-var)
    (push type-key method-lambda-list)
    (unless (consp type-key)
      (push type-key body))
    (when parameters-var
      (cond (type-name (let ((parameters-lambda-list (get-presentation-type-parameters type-name)))
                         (setq body `((destructuring-bind ,parameters-lambda-list ,@parameters-var
                                        ,(declare-lambda-list-ignorable-form parameters-lambda-list)
                                        ,@body)))))
            (t (push (car parameters-var) body))))
    (when options-var
      (cond (type-name (let ((options-lambda-list (get-presentation-type-options type-name)))
                         (setq body `((destructuring-bind ,options-lambda-list
                                          ,@options-var
                                        ,(declare-lambda-list-ignorable-form options-lambda-list)
                                        ,@body)))))
            (t (push (car options-var) body))))
    `(defmethod ,generic-function-name ,@qualifiers ,method-lambda-list
                ,@documentation
                ,@declarations
                ,type-var
                ,@body)))

(defmacro define-default-presentation-method (presentation-function-name &body body)  
  "add a default method to the presentation-generic-function, type supplied should be an atom"
  #+Genera(declare (zwei:indentation 0 1 1 2 2 1))
  `(define-presentation-method ,presentation-function-name ,@body))

;;;------------------------------------------------------------------- 
;;;
;;; OTHER FUNCTIONS
;;;

(defmethod presentation-type-superior-p ((presentation-type presentation-type) (putative-superior presentation-type))
  "determines if one presentation-type is a superior another."
  (member putative-superior (presentation-type-precedence-list presentation-type)))

(defmethod presentation-type-superior-p ((name symbol) (putative-superior-name symbol))
  "determines if one presentation-type name is a superior another."
  (presentation-type-superior-p (get-presentation-type name t)
                                (get-presentation-type putative-superior-name t)))

(defun read-token (&optional (stream *standard-input*) &aux (delimiters '(#\space #\tab #\Return #\Linefeed)))
  "reads token from stream, preserving final delimeter, returns an upcase string or nil"
  (flet ((delimiter-p (stream &aux (next (peek-char nil stream nil nil nil)))
           (when next (member next delimiters :test #'char-equal))))
    (declare (inline delimiter-p))
    (loop for char = (read-char stream nil nil t)
          while char
          unless (member char delimiters :test #'char-equal)
            collect (char-upcase char) into chars 
          until (and chars (delimiter-p stream))
          finally (return (when chars (coerce chars 'string))))))

(defun read-token-from-string (string)
  (with-input-from-string (stream string)
    (read-token stream)))

;; Useful abstraction for writing presentation parsers.   3/9/97 -- JCMa.
(defun read-delimited-string (delimiters &optional (stream *standard-input*) eof-error-p eof-value)
  "Reads a string from stream, stopping at the first delimiter."
  (declare (values string delimiter))
  (loop with delimiter
	for char = (read-char stream eof-error-p eof-value)
	until (or (null char)
		  (and (member char delimiters :test #'eql)
		       (setq delimiter char)))
	collect char into result
	finally (return (values (when result (coerce result 'string)) delimiter))))
				  
