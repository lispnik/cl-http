;;; -*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: HTTP -*-

;;; (C) Copyright 1998, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; DATA CACHING IN DYNAMIC MEMORY
;;;

(in-package :http)

;;;------------------------------------------------------------------- 
;;;
;;; CLASS DEFINITIONS
;;;

(defclass data-universe-cache-mixin
          ()
    ((minimum-frequency-data-cache :initform nil :initarg :minimum-frequency-data-cache :accessor minimum-frequency-data-cache)
     (current-window :initform 0 :initarg :current-window :reader current-cache-window)
     (window-frequency :initform *data-cache-window-frequency* :initarg :window-frequency :accessor cache-window-frequency)))

(defclass basic-data-universe
          (data-universe-cache-mixin)
    ((name :initform nil :initarg :name :accessor data-universe-name)
     (lock :initform nil :initarg :lock :accessor data-universe-lock)
     (cache :initform nil :initarg :cache :accessor data-universe-cache-table)
     (cache-size :initform *data-universe-maximum-cache-elements* :initarg :cache-size :accessor data-universe-cache-table-size)
     (total-cache-size :initform 0 :initarg :total-cache-size :accessor data-universe-total-cache-size)
     (cached-elements :initform 0 :initarg :cached-elements :accessor data-universe-cached-elements)
     (revalidation-interval :initform *data-universe-revalidation-interval* :initarg :revalidation-interval
                            :accessor data-universe-revalidation-interval)
     (maximum-cache-size :initform *data-universe-maximum-cache-size* :initarg :maximum-cache-size
                         :accessor data-universe-maximum-cache-size)
     (maximum-cache-elements :initform *data-universe-maximum-cache-elements* :initarg :maximum-cache-elements
			     :accessor data-universe-maximum-cache-elements)
     (audit-access-p :initform *data-universe-audit-accesses* :initarg audit-access-p :accessor data-universe-audit-access-p)
     (auditor-table :initform nil :initarg auditor-table :accessor data-universe-auditor-table)
     (auditor-table-size :initform *data-universe-audited-elements* :initarg :auditor-table-size
			 :accessor data-universe-auditor-table-size)))

(defclass data-universe-revalidation-mixin
          ()
    ((revalidator-process :initform nil :accessor data-universe-revalidator-process)	;the revalidator process
     (revalidator-run-p :initform nil :accessor data-universe-revalidator-run-p)	;whether it is allowed to run
     (revalidator-run-immediately-p :initform nil :accessor data-universe-revalidator-run-immediately-p)	;whether to run immediately
     (revalidator-process-priority :initform 0 :initarg :revalidator-process-priority
				   :accessor data-universe-revalidator-process-priority)
     (revalidator-process-name :initform "Data Universe Revalidator" :initarg :revalidator-process-name
			       :accessor data-universe-revalidator-process-name)
     (revalidator-wait-whostate :initform "Revalidate Wait" :accessor data-universe-revalidator-wait-whostate :allocation :class))
  (:documentation "A mixin that provides asynchronous revalidation of cached data."))

(defclass file-data-universe
          (data-universe-revalidation-mixin basic-data-universe)
    ())

(defmethod print-object ((universe basic-data-universe) stream)
  (with-slots (name) universe
    (print-unreadable-object (universe stream :type t :identity t)
      (when name
        (princ name stream)))))

(defclass data-auditor-cache--mixin
          ()
    ((current-window :initform 0 :initarg :window :accessor current-cache-window)))

(defclass data-auditor
          (data-auditor-cache--mixin)
    ((reference-count :initform 0 :initarg :reference-count :accessor %data-auditor-reference-count)
     (last-reference :initform 0 :initarg :last-reference :accessor %data-auditor-last-reference)
     (universe :initform 0 :initarg :universe :accessor data-auditor-universe)
     (datum :initform nil :initarg :datum :accessor data-auditor-datum))
  (:documentation "Class that records access statistics for data."))

(defmethod print-object ((auditor data-auditor) stream)
  (let ((datum (data-auditor-datum auditor)))
    (print-unreadable-object (auditor stream :type t :identity t)
      (when datum
        (princ datum stream)))))

(defclass data-cache-cache--mixin
          ()
    ((current-window :initform 0 :initarg :window :accessor current-cache-window)
     (previous-reference-count :initform 0 :initarg :previous-reference-count :accessor data-cache-previous-reference-count)))

(defclass basic-data-cache
          (data-cache-cache--mixin)
    ((array :initform nil :initarg :array :accessor data-cache-array)
     (size :initform 0 :initarg :size :accessor data-cache-size)
     (universe :initform nil :initarg :universe :accessor data-cache-universe)
     (update-time :initform nil :initarg :update-time :accessor data-cache-update-time)
     (revalidation-time :initform nil :initarg :revalidation-time :accessor data-cache-revalidation-time)
     (revalidation-interval :initarg :revalidation-interval :accessor data-cache-revalidation-interval)
     (lock :initform nil :initarg :lock :accessor data-cache-lock)
     (reference-count :initform 0 :initarg :reference-count :accessor data-cache-reference-count)
     (last-reference :initform nil :initarg :last-reference :accessor data-cache-last-reference)
     (wired-p :initform nil :initarg :wired-p :accessor data-cache-wired-p))
  (:documentation "The basic data cache class from which all instances are built."))

(defclass file-data-cache-mixin
          ()
    ((pathname :initform nil :initarg :pathname :accessor data-cache-pathname)
     (version :initform 0 :initarg :version :accessor data-cache-version)
     (last-modification :initform 0 :initarg :last-modification :accessor data-cache-last-modification))
  (:documentation "A mixin class that enables data caches to keep the contents of files in memory."))

(defclass binary-file-data-cache
          (file-data-cache-mixin basic-data-cache)
    ()
  (:documentation "A mixin class for caching binary file data in memory."))

(defclass crlf-file-data-cache
          (binary-file-data-cache)
    ((crlf-pathname :initform nil :initarg :crlf-pathname :accessor data-cache-crlf-pathname))
  (:documentation "A mixin class for caching CFLF canonicalizable file data in memory."))

;; Print method for file data caches.
(defmethod print-object ((data-cache file-data-cache-mixin) stream)
  (let ((pathname (data-cache-pathname data-cache)))
    (print-unreadable-object (data-cache stream :type t :identity t)
      (when pathname
        (princ pathname stream)))))


;;;------------------------------------------------------------------- 
;;;
;;; UTILS
;;;

(define-macro with-data-universe-lock ((data-universe &optional (mode :write)) &body body)
  "Grabs the lock for data-universe with mode MODE within the scope of BODY."
  `(let ((lock (data-universe-lock ,data-universe)))
     (with-lock-held (lock ,mode "Data Universe Wait")
       . ,body)))

(define-macro with-data-cache-lock ((data-cache &optional (mode :write)) &body body)
  "Grabs the lock for DATA-CACHE with mode MODE within the scope of BODY."
  `(let ((lock (data-cache-lock ,data-cache)))
     (with-lock-held (lock ,mode "Data Wait")
       . ,body)))

(declaim (inline get-pathname-data-cache))

(defun get-pathname-data-cache (pathname)
  (get-value pathname :data-cache))

(defun set-pathname-data-cache (pathname data-cache)
  (setf (get-value pathname :data-cache) data-cache))

(defun remove-pathname-data-cache (pathname)
  (remove-value pathname :data-cache))

(declaim (inline get-pathname-data-auditor))

(defun get-pathname-data-auditor (pathname)
  (get-value pathname :data-auditor))

(defun set-pathname-data-auditor (pathname data-auditor)
  (setf (get-value pathname :data-auditor) data-auditor))

(defsetf get-pathname-data-auditor set-pathname-data-auditor)

(defun remove-pathname-data-auditor (pathname)
  (remove-value pathname :data-auditor))


;;;------------------------------------------------------------------- 
;;;
;;; OPERATIONS ON DATA-UNIVERSES
;;;

(defparameter *standard-data-universe* nil
  "Holds the current data universe object.")

(define-macro with-data-universe ((universe) &body body)
  "Binds data-universe as the standard data universe within body."
  `(let ((*standard-data-universe* ,universe)) . ,body))

(declaim (inline standard-data-universe))

(defun standard-data-universe ()
  "Returns the current data universe object."
  *standard-data-universe*)

(defparameter *data-universe-class* 'file-data-universe
  "The standard class for new data universes.")

(defvar *data-universes* nil
  "All known data universes.")

(define intern-data-universe (universe &key (if-does-not-exist :error) (class *data-universe-class*))
  "Interns UNIVERSE and returns the data universe object.
IF-DOES-NOT-EXIST indicates the action to take if data-universe 
does not already exist and can be any of :SOFT, :CREATE, :ERROR.
CLASS is the class of universe to create and NAME is the name for
a newly created universe."
  (declare (values data-universe newly-created-p))
  (etypecase universe
    (string
      (or (find universe *data-universes* :test #'equalp :key #'data-universe-name)
          (ecase if-does-not-exist
            (:soft nil)
            (:create (values (register-data-universe (make-instance class :name universe)) t))
            (:error (error "No data universe named, ~S, exists." universe)))))
    (basic-data-universe universe)))

(defmethod register-data-universe ((universe basic-data-universe))
  (pushnew universe *data-universes*)
  (unless *standard-data-universe*
    (setq *standard-data-universe* universe))
  universe)

(define-generic unintern-data-universe (universe)
  (declare (values uninterned-p universe)) 
  (:documentation "Uninterns data-universe."))

(defmethod unintern-data-universe ((universe basic-data-universe))
  (when (member universe *data-universes*)
    (setq *data-universes* (delete universe *data-universes*))
    (when (eq universe *standard-data-universe*)
      (setq *standard-data-universe* nil))
    (values t universe)))

(defmethod unintern-data-universe ((universe string))
  (let ((dc (intern-data-universe universe :if-does-not-exist :soft)))
    (when dc
      (unintern-data-universe dc))))

(defmethod initialize-instance :after ((data-cache basic-data-cache) &key &allow-other-keys)
  (setf (data-cache-lock data-cache) (make-lock (data-cache-name data-cache)
						:type :multiple-reader-single-writer))
  data-cache)

(defmethod initialize-instance :after ((universe basic-data-universe) &key &allow-other-keys)
  (let ((size (data-universe-cache-table-size universe))
	(audit-size (data-universe-auditor-table-size universe)))
    (setf (data-universe-cache-table universe) (make-hash-table :test #'equal :size size)
	  (data-universe-auditor-table universe) (make-hash-table :test #'equal :size audit-size)
	  (data-universe-lock universe) (make-lock (data-universe-name universe)
						   :type :multiple-reader-single-writer))
    universe))

(define-generic data-universe-increment-total-cache-size (universe &optional delta)
  (:documentation "Increments the total cache size of UNIVERSE  by DELTA,
which defaults to 1. The operation is thread safe."))

(defmethod data-universe-increment-total-cache-size ((universe basic-data-universe) &optional (delta 1))
  (with-slots (total-cache-size) universe
    (atomic-incf total-cache-size delta)))

(define-generic cache-data (universe data-source &key revalidation-interval wired-p &allow-other-keys)
  (declare (values data-cache))
  (:documentation "Primary method for caching DATA-SOURCE in the data universe, UNIVERSE."))

(defmethod cache-data ((universe file-data-universe) (data-source pathname) &key revalidation-interval wired-p &allow-other-keys)
  (let ((path (data-universe-cache-key universe data-source))
        (interval (or revalidation-interval (data-universe-revalidation-interval universe)))
        cache auditor)
    (setq cache (cond ((get-pathname-data-cache path))
		      ((crlf-canonicalizable-pathname-p path)
		       (make-instance 'crlf-file-data-cache
				      :pathname path
				      :crlf-pathname (crlf-pathname path)
				      :revalidation-interval interval
				      :wired-p wired-p
				      :universe universe))
		      (t (make-instance 'binary-file-data-cache
					:pathname path
					:revalidation-interval interval
					:wired-p wired-p
					:universe universe))))
    (when (setq auditor (get-pathname-data-auditor path))
      (setf (data-cache-last-reference cache) (%data-auditor-last-reference auditor)
	    (current-cache-window cache) (current-cache-window auditor)))
    ;; register the data-cache
    (register-data universe cache)
    (when auditor
      (with-slots (reference-count) cache 
	(atomic-incf reference-count (%data-auditor-reference-count auditor)))
      (unregister-data universe auditor))
    ;;return it
    cache))

(defmethod cache-data ((universe file-data-universe) (data-source string) &key revalidation-interval wired-p &allow-other-keys)
  (cache-data universe (pathname data-source) :revalidation-interval revalidation-interval :wired-p wired-p))

(defmethod cache-data ((universe basic-data-universe) (data-sources cons) &rest args)
  (declare (dynamic-extent args))
  (loop for entry in data-sources
        do (typecase entry
             (atom (apply #'cache-data universe entry args))
             (cons (destructuring-bind (data-source . args) entry
                     (let ((nargs `(,@args ,.args)))
                       (declare (dynamic-extent nargs))
                       (apply #'cache-data universe data-source nargs))))))) 

(defmethod cache-data ((universe string) data-sources &rest args)
  (declare (dynamic-extent args))
  (apply #'cache-data (intern-data-universe universe) data-sources args))

(defmethod cache-data ((universe basic-data-universe) (data-sources null) &rest args)
  (declare (dynamic-extent args)
	   (ignore args))
  (intern-data-universe universe))

(define-macro define-cached-data ((data-universe &key (clear-data-universe-p t)
						 (audit-access-p '*data-universe-audit-accesses*)
						 (data-universe-class ''file-data-universe)) &rest data-sources)
  "Defines DATA-SOURCES as the initial data cached in DATA-UNIVERSE.
CLEAR-DATA-UNIVERSE-P controls whether existing universes are cleared
before the new data is cached. AUDIT-ACCESS-P controls whether
the universe tracks accesses of uncached data."

  `(multiple-value-bind (universe new-p)
       (intern-data-universe ,(string data-universe) :if-does-not-exist :create :class ,data-universe-class)
     ,(if clear-data-universe-p
          `(unless new-p
             (clear-data-universe universe))
          'new-p)
     (setf (data-universe-audit-access-p universe) ,audit-access-p)
     (cache-data universe ',data-sources)
     universe))

#|
(eval `(define-cached-data ("Standard-Pathname-Cache" :audit-access-p t)
			   ("w.ai.mit.edu:>http>www>cl-http>cl-http.html" :wired-p t)
	 ,.(loop for path in (directory "http:www;cl-http;icons;*.gif.newest")
		 collect `(,(make-pathname :host (pathname-host path)
					   :device (pathname-device path)
					   :directory (pathname-directory path)
					   :name (pathname-name path)
					   :type (pathname-type path))
			   :wired-p t))))

|#

(define-generic decache-data (universe data-source))

(defmethod decache-data ((universe basic-data-universe) data-source)
  (let ((data-cache (find-data-cache universe data-source nil)))
    (when data-cache
      (decache-data universe data-cache))))

(defmethod decache-data ((universe basic-data-universe) (data-cache basic-data-cache))
  (unregister-data universe data-cache))

(defmethod decache-data :around ((universe basic-data-universe) (data-cache file-data-cache-mixin))
  (when (and (call-next-method)
	     (data-universe-audit-access-p universe))
    (let* ((pathname (data-cache-pathname data-cache))
           (auditor (get-pathname-data-auditor pathname))
           (count (data-cache-reference-count data-cache))
           (access-time (data-cache-last-reference data-cache))
           (window (current-cache-window data-cache))
           (u (data-cache-universe data-cache)))
      (cond (auditor
	     (setf (%data-auditor-reference-count auditor) count
		   (%data-auditor-last-reference auditor) access-time
		   (current-cache-window auditor) window
		   (data-auditor-universe auditor) auditor))
	    (t (setq auditor (make-instance 'data-auditor :universe u :datum pathname
					    :window window :reference-count count
					    :last-reference access-time))
	       (register-data u auditor)))
      t)))

(defmethod decache-data ((universe null) (data-cache basic-data-cache))
  (decache-data (data-cache-universe data-cache) data-cache))

(define-generic find-data-cache (universe data-source &optional error-p)
  (declare (values data-cache))
  (:documentation "Finds the data cache for DATA-SOURCE in UNIVERSE."))

(defmethod find-data-cache ((universe basic-data-universe) data-source &optional (error-p t))
  (cond ((gethash (data-universe-cache-key universe data-source) (data-universe-cache-table universe)))
        (error-p (error "DATA-SOURCE, ~S, is not cached in ~S." data-source universe))
        (t nil)))

(define-generic register-pathname-data-cache (pathname data-cache)
  (:documentation "Registers data-cache with pathname."))

(defmethod register-pathname-data-cache ((pathname pathname) (data-cache file-data-cache-mixin))
  (let* ((old-data-cache (get-pathname-data-cache pathname))
	 (version        (pathname-version pathname))
	 (other-pathname (make-pathname :version (cond ((null version) :newest)
						       ((eq version :newest) nil)
						       (t version))
					:defaults pathname)))
    (when (and old-data-cache (not (eq old-data-cache data-cache)))
      (cerror "A data cache, ~S, already exists for ~A." old-data-cache (data-cache-pathname data-cache)))
    (set-pathname-data-cache pathname data-cache)
    (unless (eq pathname other-pathname)
      (set-pathname-data-cache other-pathname data-cache) )
    data-cache ))

(define-generic register-data (universe data-cache))

(defmethod register-data ((universe basic-data-universe) (data-cache basic-data-cache))
  (with-slots (cached-elements total-cache-size) universe
    (setf (gethash (data-universe-cache-key universe data-cache) (data-universe-cache-table universe)) data-cache)
    (atomic-incf cached-elements)
    (atomic-incf total-cache-size (data-cache-size data-cache))
    data-cache))

(defmethod register-data :after ((universe file-data-universe) (data-cache file-data-cache-mixin))
  (register-pathname-data-cache (data-cache-pathname data-cache) data-cache)
  data-cache)

(defmethod register-data :after ((universe file-data-universe) (data-cache crlf-file-data-cache))
  (register-pathname-data-cache (data-cache-crlf-pathname data-cache) data-cache)
  (setf (gethash (data-cache-crlf-pathname data-cache) (data-universe-cache-table universe)) data-cache)
  data-cache)

(defmethod register-data ((universe basic-data-universe) (data-auditor data-auditor))
  (setf (gethash (data-universe-cache-key universe data-auditor) (data-universe-auditor-table universe)) data-auditor)
  data-auditor)

(defmethod register-data :after ((universe file-data-universe) (data-auditor data-auditor))
  (let ((pathname (data-auditor-datum data-auditor)))
    (setf (get-pathname-data-auditor pathname) data-auditor)
    data-auditor))

(define-generic unregister-data (universe data-cache-or-auditor)
  (declare (values removed-p old-data-cache-or-auditor)))

(defmethod unregister-data ((universe basic-data-universe) (data-cache basic-data-cache))
  (with-slots (cached-elements total-cache-size) universe
    (let ((cache-table (data-universe-cache-table universe)))
      (when (remhash (data-universe-cache-key universe data-cache) cache-table)
        (atomic-decf cached-elements)
        (atomic-decf total-cache-size (data-cache-size data-cache))
        t))))

(defmethod unregister-data :after ((universe file-data-universe) (data-cache file-data-cache-mixin))
  (remove-pathname-data-cache (data-cache-pathname data-cache)))

(defmethod unregister-data :after ((universe basic-data-universe) (data-cache crlf-file-data-cache))
  (let ((crlf-pathname (data-cache-crlf-pathname data-cache))
	(cache-table (data-universe-cache-table universe)))
    (remove-pathname-data-cache crlf-pathname)
    (remhash crlf-pathname cache-table)))

(defmethod unregister-data :around ((universe basic-data-universe) data-cache)
  (let ((removed-p (call-next-method)))
    (values (not (null removed-p)) data-cache)))

(defmethod unregister-data ((universe basic-data-universe) (data-auditor data-auditor))
  (when (remhash (data-universe-cache-key universe data-auditor) (data-universe-auditor-table universe))
    t))

(defmethod unregister-data :after ((universe file-data-universe) (data-auditor data-auditor))
  (let ((pathname (data-auditor-datum data-auditor)))
    (remove-pathname-data-auditor pathname)
    data-auditor))

(define-generic data-universe-cache-key (universe data-source)
  (:documentation "Returns a canonicalization of DATA-SOURCE suitable for use as a cache key."))

(defmethod data-universe-cache-key (universe data-source)
  (declare (ignore universe))
  (error "No specialized method for DATA-UNIVERSE-CACHE-KEY has been defined for ~S." (type-of data-source)))

(defmethod data-universe-cache-key ((universe file-data-universe) (data-source file-data-cache-mixin))
  (data-cache-pathname data-source))

(defmethod data-universe-cache-key ((universe file-data-universe) (data-source string))
  (data-universe-cache-key universe (pathname data-source)))

(defmethod data-universe-cache-key ((universe file-data-universe) (data-source pathname))
  (make-pathname :version nil :defaults data-source))

(defmethod data-universe-cache-key ((universe file-data-universe) (data-source logical-pathname))
  (data-universe-cache-key universe (translate-logical-pathname data-source)))

(defmethod data-universe-cache-key ((universe basic-data-universe) (data-source data-auditor))
  (data-auditor-datum data-source))

(define-generic get-data-cache (universe data-source)
  (declare (values data-cache))
  (:documentation "Returns the data-cache object associated with DATA-SOURCE in the data universe, UNIVERSE."))

(defmethod get-data-cache ((universe basic-data-universe) data-source)
  (gethash (data-universe-cache-key universe data-source) (data-universe-cache-table universe)))

(define-generic remove-data-cache (universe data-source)
  (declare (values removed-p old-data-cache))
  (:documentation "Removes the data cache for DATA-SOURCE from the data universe, UNIVERSE."))

(defmethod remove-data-cache ((universe basic-data-universe) data-source)
  (unregister-data universe (get-data-cache universe data-source)))

(define-generic recache-data-cache (universe data-source)
  (:documentation "Recaches the data associated with DATA-SOURCE in the data universe UNIVERSE."))

(defmethod recache-data-cache ((universe basic-data-universe) data-source)
  (let ((cache (get-data-cache universe data-source)))
    (when cache 
      (recache-data cache))))

(define-generic map-data-caches (universe function)
  (:documentation "Maps FUNCTION over all the data caches associated with the data universe, UNIVERSE.
FUNCTION is called with the arguments (DATA-SOURCE-KEY DATA-CACHE-OBJECT)."))

(defmethod map-data-caches ((universe basic-data-universe) function)
  (declare (dynamic-extent function))
  (maphash function (data-universe-cache-table universe)))

(define-generic map-data-auditors (universe function)
  (:documentation "Maps FUNCTION over all the data auditors associated with the data universe, UNIVERSE.
FUNCTION is called with the arguments (DATA-SOURCE-KEY DATA-AUDITOR-OBJECT)."))

(defmethod map-data-auditors ((universe basic-data-universe) function)
  (declare (dynamic-extent function))
  (maphash function (data-universe-auditor-table universe)))

(define-generic recache-data-universe (universe)
  (:documentation "Recaches all cached data in UNIVERSE."))

(defmethod recache-data-universe ((universe file-data-universe))
  (flet ((recache-item (pathname data-cache)
           (unless (crlf-pathname-p pathname)
	     (recache-data data-cache))))
    (with-data-universe-lock (universe :write)
      (map-data-caches universe #'recache-item))))

(defmethod recache-data-universe ((universe string))
  (recache-data-universe (intern-data-universe universe)))

(define-generic recache-data-universe-as-necessary (universe &optional universal-time)
  (declare (values next-revalidation-universal-time))
  (:documentation "Recaches all stale cached data in UNIVERSE relative to UNIVERSAL-TIME."))

(defmethod recache-data-universe-as-necessary ((universe file-data-universe) &optional (universal-time (get-universal-time))
					       &aux next-revalidation)
  (flet ((maybe-recache-item (pathname data-cache)
           (unless (crlf-pathname-p pathname) 
	     (recache-if-necessary data-cache nil universal-time)
	     (setq next-revalidation (if next-revalidation
					 (min next-revalidation (data-cache-revalidation-time data-cache))
					 (data-cache-revalidation-time data-cache))))))
    (declare (dynamic-extent #'maybe-recache-item))
    (with-data-universe-lock (universe :write) 
      (map-data-caches universe #'maybe-recache-item))
    next-revalidation))

(defmethod recache-data-universe-as-necessary ((universe string) &optional (universal-time (get-universal-time)))
  (recache-data-universe-as-necessary (intern-data-universe universe) universal-time))

(define-generic clear-data-universe (universe)
  (:documentation "Clears all cached data in UNIVERSE."))

(defmethod clear-data-universe ((universe basic-data-universe))
  (flet ((uncache-item (key data-cache)
           (declare (ignore key))
           (unregister-data universe data-cache)))
    (declare (dynamic-extent #'uncache-item))
    (with-data-universe-lock (universe :write)
      (map-data-caches universe #'uncache-item)
      (map-data-auditors universe #'uncache-item))))

(defmethod clear-data-universe ((universe string))
  (clear-data-universe (intern-data-universe universe)))

(define-generic ensure-current-data (data-universe-or-data-cache)
  (:documentation "Ensures that data-universe-or-data-cache holds the most recent information
from the associated data source."))

(defmethod ensure-current-data ((universe basic-data-universe))
  (flet ((ensure-fresh-item (key data-cache)
			    (declare (ignore key))
			    (ensure-current-data data-cache)))
    (with-data-universe-lock (universe :write)
      (map-data-caches universe #'ensure-fresh-item))))

(define-generic set-minimum-frequency-data-cache (universe)
  (:documentation "Sets the minimum frequency data cache of the data universe, universe,
to the lowest-frequency volatile data cache in the current cache window."))

(defmethod set-minimum-frequency-data-cache ((universe data-universe-cache-mixin))
  (setf (minimum-frequency-data-cache universe) (data-universe-sweep-for-data-cache
						  universe :minimum-frequency :current-window-volatile)))


;;;------------------------------------------------------------------- 
;;;
;;; SWEEPING DATA UNIVERSES
;;;

(defmacro collect-data-cache (universe accessor predicate &optional constraint)
  `(loop with best-data-cache and best-value and data-cache-value
         for data-cache being the hash-values in (data-universe-cache-table ,universe)
         ,@(when constraint
             (destructuring-bind (condition filter) constraint
                 `(,condition (,filter data-cache))))
         do (cond ((null best-data-cache)
                   (psetq best-data-cache data-cache
                          best-value (,accessor data-cache)))
                  ((,predicate (setq data-cache-value (,accessor data-cache)) best-value)
                   (psetq best-data-cache data-cache
                          best-value data-cache-value)))
         finally (return (values best-data-cache best-value))))

(define-generic data-universe-sweep-for-data-cache (universe case context)
  (:documentation "Finds the data cache satifying CASE in the data universe, UNIVERSE,
according to CONTEXT. The table below enumerates possible values.

              CASE                CONTEXT

        :MINIMUM-FREQUENCY      :CURRENT-WINDOW
        :MINIMUM-FREQUENCY      :CURRENT-WINDOW-WIRED
        :MINIMUM-FREQUENCY      :CURRENT-WINDOW-VOLATILE
        :MAXIMUM-FREQUENCY      :CURRENT-WINDOW
        :MAXIMUM-FREQUENCY      :CURRENT-WINDOW-WIRED
        :MAXIMUM-FREQUENCY      :CURRENT-WINDOW-VOLATILE
        :MINIMUM-FREQUENCY      :PREVIOUS-WINDOW
        :MINIMUM-FREQUENCY      :PREVIOUS-WINDOW-WIRED
        :MINIMUM-FREQUENCY      :PREVIOUS-WINDOW-VOLATILE
        :MAXIMUM-FREQUENCY      :PREVIOUS-WINDOW
        :MAXIMUM-FREQUENCY      :PREVIOUS-WINDOW-WIRED
        :MAXIMUM-FREQUENCY      :PREVIOUS-WINDOW-VOLATILE
        :LEAST-RECENT-ACCESS    :ALL
        :LEAST-RECENT-ACCESS    :WIRED
        :LEAST-RECENT-ACCESS    :VOLATILE
        :MOST-RECENT-ACCESS     :ALL
        :MOST-RECENT-ACCESS     :WIRED
        :MOST-RECENT-ACCESS     :VOLATILE
        :MINIMUM-SIZE           :ALL
        :MINIMUM-SIZE           :WIRED
        :MINIMUM-SIZE           :VOLATILE
        :MAXIMUM-SIZE           :ALL
        :MAXIMUM-SIZE           :WIRED
        :MAXIMUM-SIZE           :VOLATILE"))

(defmethod data-universe-sweep-for-data-cache ((universe data-universe-cache-mixin)
                                               (case (eql :minimum-frequency)) (context (eql :current-window)))
  (collect-data-cache universe data-cache-reference-count <))

(defmethod data-universe-sweep-for-data-cache ((universe data-universe-cache-mixin)
                                               (case (eql :minimum-frequency)) (context (eql :current-window-wired)))
  (collect-data-cache universe data-cache-reference-count < (when data-cache-wired-p)))

(defmethod data-universe-sweep-for-data-cache ((universe data-universe-cache-mixin)
                                               (case (eql :minimum-frequency)) (context (eql :current-window-volatile)))
  (collect-data-cache universe data-cache-reference-count < (unless data-cache-wired-p)))

(defmethod data-universe-sweep-for-data-cache ((universe data-universe-cache-mixin)
                                               (case (eql :maximum-frequency)) (context (eql :current-window)))
  (collect-data-cache universe data-cache-reference-count >))

(defmethod data-universe-sweep-for-data-cache ((universe data-universe-cache-mixin)
                                               (case (eql :maximum-frequency)) (context (eql :current-window-wired)))
 (collect-data-cache universe data-cache-reference-count > (when data-cache-wired-p)))

(defmethod data-universe-sweep-for-data-cache ((universe data-universe-cache-mixin)
                                               (case (eql :maximum-frequency)) (context (eql :current-window-volatile)))
  (collect-data-cache universe data-cache-reference-count > (unless data-cache-wired-p)))

(defmethod data-universe-sweep-for-data-cache ((universe data-universe-cache-mixin)
                                               (case (eql :minimum-frequency)) (context (eql :previous-window)))
  (collect-data-cache universe data-cache-previous-reference-count <))

(defmethod data-universe-sweep-for-data-cache ((universe data-universe-cache-mixin)
                                               (case (eql :minimum-frequency)) (context (eql :previous-window-wired)))
  (collect-data-cache universe data-cache-previous-reference-count < (when data-cache-wired-p)))

(defmethod data-universe-sweep-for-data-cache ((universe data-universe-cache-mixin)
                                               (case (eql :minimum-frequency)) (context (eql :previous-window-volatile)))
  (collect-data-cache universe data-cache-previous-reference-count < (unless data-cache-wired-p)))

(defmethod data-universe-sweep-for-data-cache ((universe data-universe-cache-mixin)
                                               (case (eql :maximum-frequency)) (context (eql :previous-window)))
  (collect-data-cache universe data-cache-previous-reference-count >))

(defmethod data-universe-sweep-for-data-cache ((universe data-universe-cache-mixin)
                                               (case (eql :maximum-frequency)) (context (eql :previous-window-wired)))
(collect-data-cache universe data-cache-previous-reference-count > (when data-cache-wired-p)))

(defmethod data-universe-sweep-for-data-cache ((universe data-universe-cache-mixin)
                                               (case (eql :maximum-frequency)) (context (eql :previous-window-volatile)))
 (collect-data-cache universe data-cache-previous-reference-count > (unless data-cache-wired-p)))

(defmethod data-universe-sweep-for-data-cache ((universe basic-data-universe)
                                               (case (eql :least-recent-access)) (context (eql :all)))
  (collect-data-cache universe data-cache-last-reference <))

(defmethod data-universe-sweep-for-data-cache ((universe basic-data-universe)
                                               (case (eql :least-recent-access)) (context (eql :wired)))
  (collect-data-cache universe data-cache-last-reference < (when data-cache-wired-p)))

(defmethod data-universe-sweep-for-data-cache ((universe basic-data-universe)
                                               (case (eql :least-recent-access)) (context (eql :volatile)))
  (collect-data-cache universe data-cache-last-reference < (unless data-cache-wired-p)))

(defmethod data-universe-sweep-for-data-cache ((universe basic-data-universe)
                                               (case (eql :most-recent-access)) (context (eql :all)))
  (collect-data-cache universe data-cache-last-reference >))

(defmethod data-universe-sweep-for-data-cache ((universe basic-data-universe)
                                               (case (eql :most-recent-access)) (context (eql :wired)))
 (collect-data-cache universe data-cache-last-reference > (when data-cache-wired-p)))

(defmethod data-universe-sweep-for-data-cache ((universe basic-data-universe)
                                               (case (eql :most-recent-access)) (context (eql :volatile)))
(collect-data-cache universe data-cache-last-reference > (unless data-cache-wired-p)))

(defmethod data-universe-sweep-for-data-cache ((universe basic-data-universe)
                                               (case (eql :minimum-size)) (context (eql :all)))
  (collect-data-cache universe data-cache-size <))

(defmethod data-universe-sweep-for-data-cache ((universe basic-data-universe)
                                               (case (eql :minimum-size)) (context (eql :wired)))
  (collect-data-cache universe data-cache-size < (when data-cache-wired-p)))

(defmethod data-universe-sweep-for-data-cache ((universe basic-data-universe)
                                               (case (eql :minimum-size)) (context (eql :volatile)))
 (collect-data-cache universe data-cache-size < (unless data-cache-wired-p)))

(defmethod data-universe-sweep-for-data-cache ((universe basic-data-universe)
                                               (case (eql :maximum-size)) (context (eql :all)))
 (collect-data-cache universe data-cache-size >))

(defmethod data-universe-sweep-for-data-cache ((universe basic-data-universe)
                                               (case (eql :maximum-size)) (context (eql :wired)))
 (collect-data-cache universe data-cache-size > (when data-cache-wired-p)))

(defmethod data-universe-sweep-for-data-cache ((universe basic-data-universe)
                                               (case (eql :maximum-size)) (context (eql :volatile)))
(collect-data-cache universe data-cache-size > (unless data-cache-wired-p)))


;;;------------------------------------------------------------------- 
;;;
;;; CACHE MIGRATION POLICY
;;;

(define-generic data-universe-advance-current-window (universe)
  (:documentation "Advances the window within which data source hit frequencies are compared."))

(defmethod data-universe-advance-current-window ((universe data-universe-cache-mixin))
  (with-slots (current-window window-frequency) universe
    (atomic-incf current-window window-frequency)))

(defmethod initialize-instance :after ((universe data-universe-cache-mixin) &key &allow-other-keys)
  (data-universe-advance-current-window universe)
  universe)


;;;------------------------------------------------------------------- 
;;;
;;; CACHING DATA AND WRITING FROM CACHE
;;;

#+Genera
(defvar *data-cache-area* (si:make-area :name 'data-cache-area :representation :structure)
  "The area in which all arrays used to cache HTTP data are stored.")

#+Genera
(defun make-data-cache-array (resource size)
  (declare (ignore resource))
  (scl:make-array size :element-type '(unsigned-byte 8) :fill-pointer t :area *data-cache-area*))

#-Genera
(defun make-data-cache-array (resource size)
  (declare (ignore resource))
  (make-array size :element-type '(unsigned-byte 8) :fill-pointer t))

(defun match-data-cache-array-p (resource array size)
  (declare (ignore resource))
  (let ((array-size (array-total-size array)))
    (declare (fixnum size array-size))
    (and (<= size array-size)			;fit inside array?
	 (< (float (/ size array-size)) .10))))	;within hysterisis -- maintain resource distribution & frequency

;; this must allocate and deallocate rapidly
(defresource data-cache-array (size)
  :constructor make-data-cache-array
  :matcher match-data-cache-array-p)

(define clear-data-cache-array-resource ()
  (clear-resource 'data-cache-array))

(defmethod initialize-instance :before ((data-cache file-data-cache-mixin) &key pathname &allow-other-keys)
  (setf (data-cache-pathname data-cache) pathname)
  data-cache)

(define-generic data-cache-reset-revalidation-time (data-cache &optional universal-time)
  (:documentation "Resets the time when the cache should be revalidated."))

(defmethod data-cache-reset-revalidation-time ((data-cache basic-data-cache) &optional (universal-time (get-universal-time)))
  (setf (data-cache-revalidation-time data-cache) (+ universal-time (data-cache-revalidation-interval data-cache))))

(defmethod data-cache-name ((data-cache basic-data-cache))
  (symbol-name (type-of data-cache)))

(defmethod data-cache-name ((data-cache file-data-cache-mixin))
  (format nil "~A" (data-cache-pathname data-cache)))

(define-generic recache-data (data-cache &optional stream length)
  (declare (values data-array))
  (:documentation "Recaches the data in DATA-CACHE.
STREAM is an input stream to the data source.
LENGTH is number of bytes to read into the cache."))

(defmethod recache-data ((data-cache basic-data-cache) &optional stream length)
  (declare (values data-array))
  (unless (and stream length)
    (error "No data stream and data size were provided."))
  (let ((new-array (allocate-resource 'data-cache-array length))
	(ut (get-universal-time))
	old-array old-size offset)
    ;; obtain new data
    (setq new-array (binary-stream-copy-into-8-bit-array stream length 0 new-array))
    ;; Use lock only around the actual update to minimize latency
    (with-data-cache-lock (data-cache :write)
      (setf old-array (data-cache-array data-cache)
	    old-size (data-cache-size data-cache)
            (data-cache-array data-cache) new-array
	    (data-cache-size data-cache) length
	    (data-cache-update-time data-cache) ut)
      (data-cache-reset-revalidation-time data-cache ut))
    (cond-every
      (old-array
	(deallocate-resource 'data-cache-array old-array))
      ;; Update cache indices
      ((and old-size (not (zerop (setq offset (- length old-size)))))
       (data-universe-increment-total-cache-size (data-cache-universe data-cache) offset)))))

(defmethod recache-data :around ((data-cache file-data-cache-mixin) &optional file-stream length)
  (multiple-value-prog1 (call-next-method data-cache file-stream (or length (file-length-in-bytes file-stream)))
    (setf (data-cache-last-modification data-cache) (file-stream-modification-date file-stream)
          (data-cache-version data-cache) (file-stream-version file-stream))))

(defmethod recache-data :around ((data-cache binary-file-data-cache) &optional source length)
  (with-open-file (file-stream (or source (data-cache-pathname data-cache))
                               :direction :input :element-type '(unsigned-byte 8))
    (call-next-method data-cache file-stream (or length (file-length file-stream)))))

(defmethod recache-data :around ((data-cache crlf-file-data-cache) &optional source length)
  (declare (ignore source))
  (ensure-crlf-canonical-file (data-cache-pathname data-cache))
  (call-next-method data-cache (data-cache-crlf-pathname data-cache) length))

(define-generic write-cache-data (data-cache stream &optional start end)
  (:documentation "Writes the contents of DATA-CACHE to STREAM
starting from START upto but not including END. This operation is
protected from other threads corrupting the cache by recaching during a write."))

;; Make sure we have a data array, but don't get caught in an infinite
;; loop if there is an error caching the data. This is designed to
;; minimize latency on the front end in recache-date by allowing it to
;; deallocate the old array without waiting for any users to finish.
;; The read lock here assures that the resetter waits his turn before
;; updating the instance variable.
(defmethod write-cache-data ((data-cache basic-data-cache) stream &optional (start 0) end)
  (loop with array
        repeat 3
        doing (with-data-cache-lock (data-cache :read)	;protects an reader from an update
                (when (setq array (data-cache-array data-cache))
                  (binary-stream-copy-from-8-bit-array array stream start end)
                  (return-from write-cache-data)))
              (with-data-cache-lock (data-cache :write)
                (unless (data-cache-array data-cache)
                  (recache-data data-cache)))
        finally (error "Failed to recache missing data for ~S." data-cache)))

(declaim (inline %valid-data-cache-p))

(defun %valid-data-cache-p (data-cache &optional last-modification)
  (and (data-cache-array data-cache)
       (if last-modification (< last-modification (data-cache-update-time data-cache)) t)))

(define-generic valid-data-cache-p (data-cache &optional last-modification)
  (:documentation "Returns non-null when the data in DATA-CACHE is valid.
If last-modification is provided, this returns non-null only if
the cache has been refreshed before last-modification. last-modification
is in universal time."))

(defmethod valid-data-cache-p ((data-cache basic-data-cache) &optional last-modification)
  (%valid-data-cache-p data-cache last-modification))

(define-generic recache-if-necessary (data-cache &optional last-modification access-time)
  (:documentation "When the cache becomes stale, this automatically recaches the data in DATA-CACHE.
It assumes a single thread of control."))

(defmethod recache-if-necessary ((data-cache basic-data-cache) &optional last-modification (access-time (get-universal-time))
                                 &aux revalidation-time)
  (unless last-modification
    ;; Check the last modification of the resource every revalidation interval.
    (when (and (setq revalidation-time (data-cache-revalidation-time data-cache))
               (< revalidation-time access-time))
      (data-cache-reset-revalidation-time data-cache access-time)       ;reset the revalidation time
      (setq last-modification (data-source-last-modification data-cache))))
  ;; Quick check for cache validity without locking
  (unless (valid-data-cache-p data-cache last-modification)
    (recache-data data-cache)))

(define-generic data-source-last-modification (data-cache)
  (:documentation "Returns the universal time when the data source was last modified."))

(defmethod data-source-last-modification ((data-cache file-data-cache-mixin))
  (file-modification-date (data-cache-pathname data-cache)))

(defmethod ensure-current-data ((data-cache basic-data-cache))
  (recache-if-necessary data-cache))

;; This could open the file three times to recache, but if no change
;; dominates, it only touches the the disk once.   3/19/98 -- JCMa.
(defmethod ensure-current-data ((data-cache file-data-cache-mixin))
  (recache-if-necessary data-cache (data-source-last-modification data-cache)))

(define-generic note-data-access (universe auditor &optional access-time)
  (:documentation "Records the data access."))

(defmethod note-data-access (universe auditor &optional access-time)
  (declare (ignore universe access-time))
  (error "No specialized method for note-data-access has been defined for when AUDITOR is a ~S." (type-of auditor)))

(defmethod note-data-access ((universe basic-data-universe) (auditor data-auditor) &optional (access-time (get-universal-time)))
  (with-slots (reference-count last-reference) auditor
    (let ((cache-window (current-cache-window universe)))
      (setf last-reference access-time)
      (if (eql cache-window (current-cache-window auditor))
          (atomic-incf reference-count)
          (setf (current-cache-window auditor) cache-window
                (%data-auditor-reference-count auditor) 1))
      auditor)))

(defmethod note-data-access ((universe basic-data-universe) (data-cache basic-data-cache) &optional (access-time (get-universal-time)))
  (with-slots (reference-count last-reference) data-cache
    (let ((cache-window (current-cache-window universe)))
      (setf last-reference access-time)
      (if (eql cache-window (current-cache-window data-cache))
          (atomic-incf reference-count)
          (setf (current-cache-window data-cache) cache-window
                (data-cache-previous-reference-count data-cache) (data-cache-reference-count data-cache)
                (data-cache-reference-count data-cache) 1))
      data-cache)))

(defmethod note-data-access :after ((universe data-universe-cache-mixin) (data-cache data-cache-cache--mixin)
                                    &optional access-time)
  (declare (ignore access-time))
  (with-slots (minimum-frequency-data-cache) universe
    (flet ((swap-data-cache-p (data-cache1 data-cache2)
             (or (null data-cache1)
                 (< (data-cache-reference-count data-cache1) (data-cache-reference-count data-cache2)))))
      (unless (data-cache-wired-p data-cache)
        (www-utils:atomic-conditional-replacef minimum-frequency-data-cache #'swap-data-cache-p data-cache))
      data-cache)))

(defmethod note-data-access ((universe file-data-universe) (datum pathname) &optional (access-time (get-universal-time)))
  (let ((auditor (get-pathname-data-auditor datum)))
    (cond (auditor
           (note-data-access universe auditor access-time))
          (t (setq auditor (make-instance 'data-auditor
					  :universe universe :datum datum
					  :window (current-cache-window universe)
					  :reference-count 1 :last-reference access-time))
	     (register-data universe auditor)))))

(defun access-pathname-data (pathname &optional (access-time (get-universal-time)) &aux universe)
  "Returns the data-cache object or null if pathname is not in the cache."
  (declare (values data-cache-or-null))
  (when (setq universe *standard-data-universe*)
    (let ((data-cache (get-pathname-data-cache pathname)))
      (cond (data-cache
	     (note-data-access (data-cache-universe data-cache) data-cache access-time)
	     data-cache)
	    ((data-universe-audit-access-p universe)
	     (note-data-access universe pathname access-time)
	     nil)
	    (t nil)))))

(define-macro with-data-cache ((pathname &key (variable 'data-cache)) form &body body)
  "When PATHNAME has an in-memory data cache, FORM is evaluated instead of BODY,
which handles the normal uncached case."
  `(let ((,variable (access-pathname-data ,pathname (server-request-time *server*))))
     (cond (,variable ,form)
	   (t . ,body))))

(define-generic data-cache-note-reference (data-cache)
  (:documentation "Atomically notes each reference to DATA-CACHE."))

(defmethod data-cache-note-reference ((data-cache basic-data-cache))
  (with-slots (reference-count) data-cache
    (atomic-incf reference-count)))

(define-generic data-cache-reset-reference-count (data-cache))

(defmethod data-cache-reset-reference-count ((data-cache basic-data-cache))
  (with-slots (reference-count) data-cache
    (setf reference-count 0)))


;;;------------------------------------------------------------------- 
;;;
;;; ASYNCHRONOUS REVALIDATION OF CACHED DATA
;;;

(defmethod initialize-instance :after ((data-universe data-universe-revalidation-mixin) &key &allow-other-keys)
  (setf (data-universe-revalidator-process-name data-universe) (concatenate 'string (data-universe-name data-universe) " Revalidator")))

(defmethod register-data-universe :after ((data-universe data-universe-revalidation-mixin))
  (data-universe-ensure-active-revalidator data-universe))

(defmethod unintern-data-universe :after ((data-universe data-universe-revalidation-mixin))
  (data-universe-revalidator-process-kill data-universe)
  data-universe)

;; Allow the process priority to be adjusted while running
(defmethod (setf data-universe-revalidator-process-priority) :after ((process-priority integer) (data-universe data-universe-revalidation-mixin))
  (let ((process (data-universe-revalidator-process data-universe)))
    (when process
      (setf (process-priority process) process-priority))))

(defgeneric data-universe-revalidator-waiting-p (data-universe)
  (:documentation "Returns non-null if DATA-UNIVERSE's revalidator is in a wait state rather than revalidating cached data."))

(defmethod data-universe-revalidator-waiting-p ((data-universe data-universe-revalidation-mixin))
  (let ((process (data-universe-revalidator-process data-universe))
	(wait-whostate (data-universe-revalidator-wait-whostate data-universe)))
    (equalp (process-whostate process) wait-whostate)))

(defgeneric data-universe-immediate-revalidation (data-universe)
  (:documentation "Forces DATA-UNIVERSE's revalidator to run immediately unless it is already running."))

(defmethod data-universe-immediate-revalidation ((data-universe data-universe-revalidation-mixin))
  (when (data-universe-revalidator-waiting-p data-universe)
    (setf (data-universe-revalidator-run-immediately-p data-universe) t)))

(defgeneric data-universe-revalidator-work-p (data-universe)
  (:documentation "Returns non-null when data-universe's revalidator is active and there is work for it."))

(defmethod data-universe-revalidator-work-p ((data-universe data-universe-revalidation-mixin))
  (and (data-universe-revalidator-run-p data-universe)
       (< 0 (data-universe-cached-elements data-universe))))

(defparameter *data-cache-revalidation-hysterisis* 10
  "The number of revalidation buckets per data-universe revalidation-interval.")

(defgeneric data-universe-revalidator-main-loop (task-queue)
  (:documentation "The main loop for revalidating cached data."))

(defmethod data-universe-revalidator-main-loop ((data-universe data-universe-revalidation-mixin))
  (flet ((run-immediately-p (data-universe)
	   (when (data-universe-revalidator-run-immediately-p data-universe)
	     (setf (data-universe-revalidator-run-immediately-p data-universe) nil)
	     t)))
    (loop with wait-whostate = (data-universe-revalidator-wait-whostate data-universe)
	  with revalidation-interval fixnum = (data-universe-revalidation-interval data-universe)
	  with hysterisis fixnum = (floor revalidation-interval (the fixnum *data-cache-revalidation-hysterisis*))
	  and elapsed-time = 0
	  ;; wait until there is work to do
	  doing (unless (data-universe-revalidator-work-p data-universe)
		  (process-wait wait-whostate #'data-universe-revalidator-work-p data-universe))
		;; Map cacheable resources, check their validity, and possibly
		;; recache, returning the earliest time for revalidation
		#+ignore(notify-log-window "Revalidating ~A" (data-universe-name data-universe))
		(let* ((start-time (get-universal-time))
		       (cache-time (+ start-time (min revalidation-interval (max hysterisis elapsed-time))))
		       (next-revalidation (recache-data-universe-as-necessary data-universe cache-time))
		       (finish-time (get-universal-time))
		       (wait-seconds (- next-revalidation finish-time)))
		  (declare (bignum start-time finish-time next-revalidation))
		  #+ignore(notify-log-window "Waiting ~\\time-interval\\ seconds before Revalidating ~A"
					     wait-seconds (data-universe-name data-universe))
		  (setq elapsed-time (- finish-time start-time))
		  (when (plusp wait-seconds)
		    ;; wait until the next revalidation or restart on command.
		    (process-wait-with-timeout wait-whostate wait-seconds #'run-immediately-p data-universe))))))

(defgeneric data-universe-start-revalidator (data-universe)
  (:documentation "Starts data-universe revalidating cached data asynchronously."))

(defmethod data-universe-start-revalidator ((data-universe data-universe-revalidation-mixin))
  (let ((process (data-universe-revalidator-process data-universe)))
    (cond (process
           (process-preset process #'data-universe-revalidator-main-loop data-universe)
           (process-enable process))
          (t (setq process (make-process (data-universe-revalidator-process-name data-universe)
					 :background-p t
					 :priority (data-universe-revalidator-process-priority data-universe)
                                         :restart-after-reset t
                                         :warm-boot-action :delayed-restart))
	     (setf (data-universe-revalidator-process data-universe) process)
             (process-preset process #'data-universe-revalidator-main-loop data-universe)
             (process-enable process)))
    (setf (data-universe-revalidator-run-p data-universe) t)
    process))

(defmethod data-universe-start-revalidator ((data-universe string))
  (data-universe-start-revalidator (intern-data-universe data-universe)))

(defgeneric data-universe-stop-revalidator (data-universe)
  (:documentation "Stops DATA-UNIVERSE's revalidator from revalidating cached resources."))

;; specialize this method to perform clean up activity.
(defmethod data-universe-stop-revalidator ((data-universe data-universe-revalidation-mixin))
  (declare (values data-universe)))

(defmethod data-universe-stop-revalidator :around ((data-universe data-universe-revalidation-mixin))
 (let ((process ( data-universe-revalidator-process data-universe)))
    (when process
      ;; set a flag tell the process to shutdown
      (setf (data-universe-revalidator-run-p data-universe) nil)
      ;; wait until the shutdown is complete
      (process-wait "Revalidator Shutdown" #'data-universe-revalidator-waiting-p data-universe)
      (call-next-method)
      ;; disable the processes run reasons
      (process-disable process)
      process)))

(defmethod data-universe-stop-revalidator ((data-universe string))
  (data-universe-stop-revalidator (intern-data-universe data-universe)))

(defgeneric data-universe-revalidator-process-kill (data-universe)
  (:documentation "Stops DATA-UNIVERSE's revalidation process and kills it."))

(defmethod data-universe-revalidator-process-kill ((data-universe data-universe-revalidation-mixin))
  (let ((process (data-universe-revalidator-process data-universe)))
    (when process
      (data-universe-stop-revalidator data-universe)
      (prog1 (process-kill process)
             (setf (data-universe-revalidator-process data-universe) nil)))))

(defgeneric data-universe-revalidator-active-p (data-universe)
  (:documentation "Returns non-null if data-universe's revalidator is active."))

(defmethod data-universe-revalidator-active-p ((data-universe data-universe-revalidation-mixin))
  (let ((process (data-universe-revalidator-process data-universe)))
    (and process (process-active-p process))))

(defgeneric data-universe-ensure-active-revalidator (data-universe)
  (declare (values task-queue))
  (:documentation "Ensures that data-universe's revalidator is active and refreshing cached data."))

(defmethod data-universe-ensure-active-revalidator ((data-universe data-universe-revalidation-mixin))
  (or (data-universe-revalidator-active-p data-universe)
      (data-universe-start-revalidator data-universe))
  data-universe)

(defmethod data-universe-ensure-active-revalidator (data-universe)	;provide a noop for other classes
  data-universe)
