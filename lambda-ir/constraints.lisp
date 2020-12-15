;;;   -*- Mode: LISP; Package: lambda-ir; BASE: 10; SYNTAX: ansi-common-lisp -*-

;;; (C) Copyright 1997, Andrew J. Blumberg (blumberg@ai.mit.edu).
;;;     All Rights Reserved.
;;;
;;;
;;;------------------------------------------------------------------- 


(in-package :lambda-ir)

(defmacro evaluate-and (term-list stat-lookup)
  `(let ((storage (make-bit-vector (bit-vector-length) 1)))
     (loop for term in ,term-list
	   do (cond ((listp term) (sparse-bit-and (evaluate term) storage storage))
		    (t (sparse-bit-and (,stat-lookup term) storage storage))))
     storage))

(defmacro evaluate-or (term-list stat-lookup)
  `(let ((storage (make-bit-vector (bit-vector-length) 0)))
     (loop for term in ,term-list
	   do (cond ((listp term) (sparse-bit-ior (evaluate term) storage storage))
		    (t (sparse-bit-ior (,stat-lookup term) storage storage))))
     storage))

(defmacro evaluate-andc2 (term-list stat-lookup)
  `(let ((storage (make-bit-vector (bit-vector-length) 1)))
     (let* ((first (first ,term-list))
	    (second (second ,term-list))
	    (use-first (if (listp first) 
			   (evaluate first)
			   (,stat-lookup first)))
	    (use-second (if (listp second)
			    (evaluate second)
			    (,stat-lookup second))))
       (sparse-bit-andc2 use-first use-second storage))
     storage))

(defmacro evaluate-not (term stat-lookup)
  `(let ((use-term (car ,term)))
     (typecase use-term
       (list (sparse-bit-not (evaluate use-term)))
       (t (sparse-bit-not (,stat-lookup use-term))))))

(defmethod %basic-logical-search ((token-cluster token-cluster) search-expression &key mask)
  (labels ((stat-lookup (word)
             (let ((token (%token-intern word token-cluster)))
               (if token
                   (get-token-stats token)
                   (make-bit-vector (bit-vector-length)))))
           (evaluate (term)
		     (destructuring-bind (branch . expression) term
		       (case branch
                 (:and (evaluate-and expression stat-lookup))
                 (:or (evaluate-or expression stat-lookup))
                 (:not (evaluate-not expression stat-lookup))
                 (:andc2 (evaluate-andc2 expression stat-lookup))
                 (t (make-bit-vector (bit-vector-length) 1))))))
    (declare (dynamic-extent #'stat-lookup))
    (if mask
        (bit-and (evaluate search-expression) mask)
        (evaluate search-expression))))

(defmethod %basic-logical-search ((token-cluster mapping-token-cluster) search-expression &key mask)
  (labels ((stat-lookup (word)
             (let ((token-stats (if (char-equal (aref word 0) #\")
				    (stemmed-lookup token-cluster (subseq word 1 (1- (length word))))
				    (get-token-stats (%token-intern word token-cluster)))))
               (or token-stats (make-bit-vector (bit-vector-length)))))
           (evaluate (term)
             (destructuring-bind (branch . expression) term
               (ecase branch
                 (:and (evaluate-and expression stat-lookup))
                 (:or (evaluate-or expression stat-lookup))
                 (:not (evaluate-not expression stat-lookup))
                 (:andc2 (evaluate-andc2 expression stat-lookup))))))
    (declare (dynamic-extent #'stat-lookup))
    (if mask
        (bit-and (evaluate search-expression) mask)
        (evaluate search-expression))))

(defgeneric perform-search-constraint (context constraint specifier-args limiter-args &key mask)
  (:documentation "Performs the search specified by CONSTRAINT as parameterized by SPECIFIER-ARGS and LIMITER-ARGS."))

(defmethod perform-search-constraint ((context document-context) (constraint search-constraint) specifier-args limiter-args &key mask)
  (let ((token-cluster (obtain-token-cluster context)))
    (with-slots (args-to-search-specifier-function limiting-function) constraint
      (let ((arg-transform (funcall args-to-search-specifier-function specifier-args)))
        (unless arg-transform
          (return-from perform-search-constraint :EMPTY-ARGS))
	(let ((basic-results (%basic-logical-search token-cluster arg-transform :mask mask)))
	  (if limiting-function
	      (values
		(funcall limiting-function context
			 basic-results
			 limiter-args)
		(bit-vector-cardinality basic-results))
	      (values
		basic-results
		(bit-vector-cardinality basic-results))))))))

(defmethod document-mask-from-logical-search ((context document-context) (constraint search-constraint) specifier-args &optional mask)
  (with-slots (args-to-search-specifier-function) constraint
    (let ((token-cluster (obtain-token-cluster context))
          (arg-transform (funcall args-to-search-specifier-function specifier-args)))
      (cond (arg-transform
             (%basic-logical-search token-cluster arg-transform :mask mask))
            (t nil)))))

(defgeneric number-satisfying-search-constraint (context constraint specifier-args limiter-args &key mask)
  (:documentation "Counts the number of documents satisfying a given request."))

(defmethod number-satisfying-search-constraint ((context document-context) (constraint search-constraint)
                                                specifier-args limiter-args &key mask)
  (bit-vector-cardinality (perform-search-constraint context constraint specifier-args limiter-args :mask mask)))

(defun make-search-constraint (&key in-search-arguments in-arg-function in-limiting-args in-limiting-function)
  (let ((constraint (%create-unnamed-object 'search-constraint)))
    (with-slots (search-arguments args-to-search-specifier-function limiting-arguments limiting-function) constraint
      (setf search-arguments in-search-arguments
            args-to-search-specifier-function in-arg-function
            limiting-arguments in-limiting-args
            limiting-function in-limiting-function))
    constraint))

(defgeneric perform-search-constraint-set (context constraint-set specifier-args limiter-args &key masks)
  (:documentation "Performs the constraint-set."))

(defmethod perform-search-constraint-set ((context document-context) (constraint-set search-constraint-set)
                                          specifier-args limiter-args &key masks)
  (with-slots (token-clusters) context
    (with-slots (search-constraints constraint-function) constraint-set
      (funcall
        constraint-function
        (loop for constraint in search-constraints
              for specifier-arg in specifier-args
              for limiter-arg in limiter-args
              for mask in masks
              collect (perform-search-constraint context constraint specifier-arg limiter-arg :mask mask))))))

(defun make-search-constraint-set (&key in-constraints in-function)
  (let ((constraint-set (%create-unnamed-object 'search-constraint-set)))
    (with-slots (search-constraints constraint-function) constraint-set
      (setf search-constraints in-constraints
            constraint-function in-function))))
