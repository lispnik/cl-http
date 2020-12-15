;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: (w4 :use (future-common-lisp www-utils url http)); -*-

;;; Copyright John C. Mallery,  1995-96.
;;; All rights reserved.

;;;------------------------------------------------------------------- 
;;;
;;; CLASS DEFINITIONS
;;;
(in-package :w4)

(defclass walker-documentation-mixin
          ()
    ((documentation :initform nil :initarg doc-string :accessor documentation-string)))

(defclass constraint-type
          (walker-documentation-mixin)
    ((name :initform nil :initarg :name :accessor constraint-type-name)
     (function :initarg :function :accessor constraint-type-function)
     (lambda-list :initarg :lambda-list :accessor constraint-type-lambda-list)
     (instance-class :initform 'constraint :initarg :instance-class :accessor constraint-type-instance-class))
  (:documentation "Constraint type is a template that governs constraints of the type."))

(defclass circumstance-constraint-type
          (constraint-type)
    ((allocator :initarg :allocator :accessor constraint-type-allocator)
     (instance-class :initform 'circumstance-constraint :initarg :instance-class :accessor constraint-type-instance-class))
  (:documentation "Circumstance constraint type is a template that governs constraints
that can embed other constraints."))

(defclass constraint
          ()
    ((type :initform nil :initarg :constraint-type :accessor constraint-type)
     (arguments :initform nil :initarg :arguments :accessor constraint-arguments))
  (:documentation "A constraint instance."))

(defclass constraint-set
          ()
    ((context-constraints :initform nil :initarg :context-constraints :accessor context-constraints)
     (url-constraints :initform nil :initarg :url-constraints :accessor url-constraints)
     (dns-constraints :initform nil :initarg :dns-constraints :accessor dns-constraints)
     (header-constraints :initform nil :initarg :header-constraints :accessor header-constraints)
     (resource-constraints :initform nil :initarg :resource-constraints :accessor resource-constraints)
     (sorted-p :initform nil :initarg sorted-p :accessor sorted-p)
     (constraints :initform nil :initarg :constraints :accessor constraints))
  (:documentation "A set of constraints."))

(defclass action-type
          (walker-documentation-mixin)
    ((name :initform nil :initarg :name :accessor action-type-name)
     (function :initform nil :initarg :function :accessor action-type-function)
     (lambda-list :initform nil :initarg :lambda-list :accessor action-type-lambda-list)
     (instance-class :initform 'action :initarg :instance-class :accessor action-type-instance-class))
  (:documentation "Action type is a template that governs action of the type."))

(defclass generator-type 
	  (action-type)
    ((instance-class :initform 'generator :initarg :instance-class :accessor action-type-instance-class))
  (:documentation "Generator is an action type that generaters URLs for exploration."))

(defclass encapsulating-action-type
          (action-type)
    ()
  (:documentation "An encapulating action type is a template that governs action of the type
  and whose instances contain component actions within them."))

(defclass action
          ()
    ((type :initform nil :initarg :action-type :accessor action-type)
     (arguments :initform nil :initarg :arguments :accessor action-arguments))
  (:documentation "Basic action class."))

(defclass generator
	  (action)
    ()
  (:documentation "Basic generator class."))

(defclass encapsulating-action
          (action)
    ((inferiors :initform nil :initarg :inferiors :accessor action-inferiors))
  (:documentation "Basic around action class."))

(defclass queue-locking-mixin
	  ()
    ((lock :initform (make-lock "Queue") :initarg :lock :accessor queue-lock)))

(defclass queue-entry
          (property-list-mixin)
    ((url :initarg :url :accessor qe-url)
     (depth :initarg :depth :accessor qe-depth)
     (parent-stack :initarg :parent-stack :accessor qe-parent-stack)
     (satisfies-constraints-p :initarg :satisfies-constraints-p :accessor qe-satisfies-constraints-p)
     (state :initform :pending :initarg :state :accessor qe-state)
     (retries :initform 0 :initarg :retries :accessor qe-retries)))

(defclass queue
          (queue-locking-mixin property-list-mixin)
    ((activity :initarg :activity :accessor queue-activity)
     (queue :initform nil :initarg :queue :accessor queue)
     (retry-queue :initform nil :initarg :retry-queue :accessor retry-queue)))

(defclass depth-first-queue (queue) ())

(defclass breadth-first-queue
	  (queue)
    ((pointer :initform nil :initarg :pointer :accessor pointer)))

(defclass best-first-queue
	  (queue)
    ((predicate :initform nil :initarg :predicate :accessor predicate)))

(defclass activity-parameters-mixin
          ()
    ((retries-on-network-error :initform *retries-on-network-error* :initarg :retries-on-network-error
                               :accessor activity-retries-on-network-error)
     (wait-interval-before-retry :initform *wait-interval-before-retry* :initarg :wait-interval-before-retry
                                 :accessor activity-wait-interval-before-retry)
     (url-host-name-resolution :initform url:*url-host-name-resolution*
                               :initarg :url-host-name-resolution
                               :accessor activity-url-host-name-resolution)))

(defclass activity
          (activity-parameters-mixin walker-documentation-mixin property-list-mixin)
    ((queue :initform nil :initarg queue :accessor activity-queue)
     (queue-type :initform :depth-first :initarg queue-type :accessor activity-queue-type)
     (name :initform nil :initarg :name :accessor activity-name)
     (constraint-set :initarg :constraint-set :accessor activity-constraint-set)
     (actions :initarg actions :accessor activity-actions)
     (unsatisfied-actions :initarg unsatisfied-actions :accessor activity-unsatisfied-actions)
     (report-stream :initform nil :initarg :report-stream :accessor activity-report-stream)
     (user-agent :initform (robot-version) :initarg :user-agent :accessor activity-user-agent)
     (operator :initform nil :initarg :operator :accessor activity-operator)
     (connection-timeout :initform http:*client-timeout* :initarg :connection-timeout :accessor activity-connection-timeout)
     (life-time :initform http:*server-timeout* :initarg :life-time :accessor activity-life-time))
  (:documentation "Web walking activity."))


;;;------------------------------------------------------------------- 
;;;
;;; CONSTRAINT CLASSES
;;;

(defclass context-constraint
          (constraint)
    ()
  (:documentation "A constraint instance referring to contextual constraints."))

(defclass url-constraint
          (constraint)
    ()
  (:documentation "A constraint instance referring to syntactic properties of a URL."))

(defclass dns-constraint
          (constraint)
    ()
  (:documentation "A constraint instance requiring DNS resolution."))

(defclass dns-url-constraint
          (dns-constraint url-constraint)
    ()
  (:documentation "A constraint instance referring to syntactic properties of a URL
but potentially requiring DNS resolution."))

(defclass header-constraint
          (dns-constraint)
    ()
  (:documentation "A constraint instance referring to the headers of a resource."))

(defclass resource-constraint
          (dns-constraint)
    ()
  (:documentation "A constraint instance referring to the content of a resource."))

(defclass html-constraint
          (constraint)
    ()
  (:documentation "A constraint instance referring to the content of an HTML document."))

(defclass html-head-constraint (html-constraint) ())

(defclass html-body-constraint (html-constraint) ())

(defclass circumstance-constraint (constraint) ())


;;;------------------------------------------------------------------- 
;;;
;;; ACTION CLASSES
;;;

(defclass open-http-action
          (action)
    ((method :initform :get :initarg :method :accessor activity-method)
     (follow-redirects-p :initform t :initarg :follow-redirects-p :accessor activity-follow-redirects-p)
     (outgoing-headers :initform nil :initarg :outgoing-headers :accessor activity-outgoing-headers))
  (:documentation "An action with an open HTTP connection."))


;;;------------------------------------------------------------------- 
;;;
;;; PRINT METHODS 
;;;

(defmethod write-name ((constraint-type constraint-type) &optional stream)
  (with-slots (name) constraint-type
    (write-string (or name "No Name Yet") stream)))

(defmethod write-name ((constraint constraint) &optional stream)
  (with-slots (type) constraint
    (cond (type
           (write-name type stream))
          (t (write-string "No Name Yet" stream)))))

(defmethod print-object ((constraint-type constraint-type) stream)
  (print-unreadable-object (constraint-type stream :type t :identity t)
    (with-slots (name) constraint-type
      (write-string (or name "No name yet") stream))))

(defmethod print-object ((constraint constraint) stream)
  (print-unreadable-object (constraint stream :type t :identity t)
    (with-slots (type) constraint
      (write-string (or (constraint-type-name type) "No constraint type yet") stream))))

(defmethod print-object ((constraint-set constraint-set) stream)
  (print-unreadable-object (constraint-set stream :type t :identity t)
    (with-slots (constraints) constraint-set
      (cond (constraints
             (dolist (c constraints)
               (write-char #\( stream)
                           (write-name c stream)
                           (write-char #\) stream)))
            (t (write-string "No constraints yet" stream))))))

(defmethod print-object ((action-type action-type) stream)
  (print-unreadable-object (action-type stream :type t :identity t)
    (with-slots (name) action-type
      (write-string (or name "No name yet") stream))))

(defmethod print-object ((action action) stream)
  (print-unreadable-object (action stream :type t :identity t)
    (with-slots (type) action
      (write-string (or (action-type-name type) "No action type yet") stream))))

(defmethod print-object ((activity activity) stream)
  (print-unreadable-object (activity stream :type t :identity t)
    (with-slots (name) activity
      (write-string (or name "No name yet") stream))))

(defmethod print-object ((q queue) stream)
  (with-slots (activity) q
    (print-unreadable-object (q stream :type t :identity t)
      (when activity
	(format stream "{~D: ~D} ~A"
		(number-of-queue-entries q)
		(number-of-queue-retry-entries q)
		(activity-name activity))))))

(defmethod print-object ((queue-entry queue-entry) stream)
  (print-unreadable-object (queue-entry stream :type t :identity t)
    (with-slots (url depth) queue-entry
      (if url
	  (format stream "{~D} ~A" depth (url:name-string url))
	  (write-string "No url yet" stream)))))
