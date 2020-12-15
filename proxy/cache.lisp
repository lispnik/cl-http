;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-

;;; (C) Copyright 1996-97, Christopher R. Vincent
;;;     All Rights Reserved.
;;; (C) Enhancements 1998-99, John C. Mallery
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CL-HTTP PROXY SERVER
;;;


(in-package :http)

;;;------------------------------------------------------------------- 
;;;
;;; CACHE SUBSTRATE
;;;

(defclass cache ()
  ((name :initarg :name :accessor cache-name)
   (description :initarg :description :accessor cache-description)
   (database :initarg :database :accessor cache-database)
   (max-size :initarg :max-size :accessor cache-max-size))
  (:documentation "Basic cache."))

(defmethod print-object ((cache cache) stream)
  (with-slots (name) cache
    (print-unreadable-object (cache stream :type t :identity t)
      (when (slot-boundp cache 'name)
        (write-string (string name) stream)))))

(defclass cache-object ()
  ((cache :initarg :cache :accessor cache-object-cache)
   (creation-date :initarg :creation-date :accessor cache-object-creation-date)
   (size :initform 0 :initarg :size :accessor cache-object-size))
  (:documentation "Basic cache object."))

(defgeneric remove-cache-object (cache-object cache)
  (:documentation "Remove an object from a cache."))

(defgeneric map-cache (function cache)
  (:documentation "Call FUNCTION on all the cache-objects in CACHE."))

(defgeneric cache-size (cache)
  (:documentation "Returns the total size of a cache."))

; Specialize this for efficiency.
(defmethod cache-size ((cache cache) &aux (total 0))
  (flet ((accum (object)
           (incf total (cache-object-size object))))
    (map-cache cache #'accum)
    total))

(defgeneric clear-cache (cache)
  (:documentation "Remove all objects from a cache."))

(defmethod clear-cache ((cache cache))
  (map-cache cache #'remove-cache-object))

(defgeneric expunge-cache (cache space)
  (:documentation "Clear SPACE bytes of data from the cache."))

; Specialize this for more intelligent replacement.
(defmethod expunge-cache ((cache cache) (space integer) &aux (removed nil))
  (flet ((do-it (cache-object)
           (decf space (cache-object-size cache-object))
           (remove-cache-object cache-object cache)
           (push cache-object removed)
           (if (<= space 0)
             (return-from expunge-cache removed))))
    (declare (inline do-it))
    (map-cache cache #'do-it)
    removed))

;;;------------------------------------------------------------------- 
;;;
;;; HTTP CACHE
;;;

(defclass http-cache (cache)
  ((resource-table :initarg :resource-table :accessor cache-resource-table)
   (resource-table-lock :initarg :resource-table-lock :accessor cache-resource-table-lock))
  (:documentation "HTTP resource cache."))

(defun make-http-cache (name &key database description (max-size 10240))
  "Create an instance of an HTTP cache."
  (let ((cache (make-instance 'http-cache
                 :name name 
                 :database (or database (make-http-cache-database))
                 :max-size max-size
                 :description (or description "An HTTP resource cache.")
                 :resource-table (make-hash-table :test #'equalp)
                 :resource-table-lock (make-lock "Cache Resource Table"))))
    cache))

;;;------------------------------------------------------------------- 
;;;
;;; RESOURCES
;;;

(defclass cached-resource (cache-object)
  ((uri-string :initarg :uri-string :accessor cached-resource-uri-string)
   (representations  :initform nil :initarg :representations :accessor cached-resource-representations)
   (vary :initform nil :initarg :vary :accessor cached-resource-vary))
  (:documentation "A cached HTTP resource."))

(defmethod print-object ((cached-resource cached-resource) stream)
  (with-slots (uri-string) cached-resource
    (print-unreadable-object (cached-resource stream :type t :identity t)
      (when (slot-boundp cached-resource 'uri-string)
        (write-string uri-string stream)))))

(defun make-cached-resource (cache uri)
  "Create an instance of a cached-resource."
  (make-instance 'cached-resource
    :cache cache
    :uri-string uri))

(defun %get-cached-resource (uri cache)
  "Retrieve a cached resource from its uri-string."
  (check-type uri string)
  (with-lock-held ((cache-resource-table-lock cache) :read)
    (gethash uri (cache-resource-table cache))))

(defun intern-cached-resource (resource cache &key (if-does-not-exist :error))
  "Intern a uri as a cached HTTP resource."
  (declare (values cached-resource newly-interned-p))
  (flet ((intern-uri (uri)
           (or (%get-cached-resource uri cache)
	       (ecase if-does-not-exist
		 (:soft nil)
		 (:create
		   (let ((object (make-cached-resource cache uri)))
		     (with-lock-held ((cache-resource-table-lock cache) :write)
		       (setf (gethash uri (cache-resource-table cache)) object))
		     (setf (cache-object-creation-date object) (get-universal-time))
		     (values object t)))
		 (:error (error "~S is not a cached HTTP resource." uri))))))
    (declare (inline intern-uri))
    (etypecase resource
      (string (intern-uri resource))
      (url (intern-uri (coerce-url-string resource)))
      (cached-resource resource))))

(defmethod unintern-cached-resource ((resource cached-resource))
  "Unintern a cached resource."
  (let* ((cache (cache-object-cache resource))
         (cache-table (cache-resource-table cache)))
    (remhash (cached-resource-uri-string resource) cache-table)))

(defmethod remove-cache-object ((cache-object cached-resource) (cache http-cache))
  (loop for rep in (cached-resource-representations cache-object)
        doing (unintern-cached-representation rep))
  (unintern-cached-resource cache-object))

(defmethod cached-resource-size (resource)
  "Total size, including all the representations."
  (+ (cache-object-size resource)
     (loop for rep in (cached-resource-representations resource)
           sum (cache-object-size rep))))

(defun map-cached-resources (function cache)
  "FUNCTION accepts two args, URI-string and resource."
  (maphash function (cache-resource-table cache)))

(defun cached-resources-count (cache)
  "Returns the number of HTTP resources in a cache."
  (hash-table-count (cache-resource-table cache)))

;;;------------------------------------------------------------------- 
;;;
;;; REPRESENTATIONS
;;;

(defclass cached-representation (cache-object)
    ((identifier :initarg :identifier :accessor cached-representation-identifier)
     (etag :initarg :etag :accessor cached-representation-etag)	;entity tag http1.1
     (resource :initarg :resource :accessor cached-representation-resource)
     (entity :initarg :entity :accessor cached-representation-entity)	;entity bytes
     (creation-date :initarg :creation-date :accessor cached-representation-creation-date)	;cache creation time
     (verified-date :initform nil :initarg :verified-date :accessor cached-representation-verified-date)	;latest contact w/ origin server
     (last-modification :initform nil :initarg :last-modification :accessor cached-representation-last-modification)
     (expiration-time :initform nil :initarg :expiration-time :accessor %cached-representation-expiration-time)
     (must-revalidate-p :initform nil :initarg :must-revalidate-p :accessor cached-representation-must-revalidate-p)
     (last-reference :initform nil :initarg :last-reference :accessor cached-representation-last-reference)	;most recent cache reference
     (http-version :initarg :http-version :accessor cached-representation-http-version)
     (request-headers :initform nil :initarg :request-headers :accessor cached-representation-request-headers)
     (response-headers :initform nil :initarg :response-headers :accessor cached-representation-response-headers))
  (:documentation "A cached representations of an HTTP resource."))

(defmethod print-object ((cached-representation cached-representation) stream)
  (print-unreadable-object (cached-representation stream :type t :identity t)
    (when (slot-boundp cached-representation 'resource)
      (with-slots (uri-string) (cached-representation-resource cached-representation)
        (write-string uri-string stream)))))

(defun make-cached-representation (resource)
  "Create an instance of a resource representation."
  (make-instance 'cached-representation
    :cache (cache-object-cache resource)
    :resource resource))

(defparameter *representation-identifier-counter* 1000
  "Used for generating cached-representation identifiers.")

(defun make-representation-identifier (representation)
  "Generate a unique identifier that can be used to refer to a resource representation."
  (declare (ignore representation))
  (atomic-incf *representation-identifier-counter* 1))

(defmethod clear-cache ((cache http-cache))
  (flet ((%clear-resource (uri res)
           (declare (ignore uri))
           (remove-cache-object res cache)))
    (map-cached-resources #'%clear-resource cache)))

(defun variant-matches-request-p (vary variant header-plist)
  "Determine if request headers satisfy the vary requirements for a variant."
  (flet ((header-values-equal (a b)
	   (equalp a b)))
    (declare (inline header-values-equal)) 
    (loop for field in vary
	  with variant-request = (cached-representation-request-headers variant)
	  unless (header-values-equal (getf header-plist field)
				      (getf variant-request field))
	    return nil
	  finally (return t))))

(define-generic intern-cached-representation (resource header-plist &key if-does-not-exist creation-time)
  (declare (values cached-representation newly-created-p))
  (:documentation "Intern a cached HTTP representation of a resource from the resource and request headers"))

(defmethod intern-cached-representation ((resource cached-resource) header-plist &key (if-does-not-exist :error) creation-time)
  (flet ((%get-cached-representation (resource header-plist)
	   "Find a representation of resource that is the same variant implied by request headers in header-plist"
	   (let ((representations (cached-resource-representations resource)))
	     (when representations
	       (let ((vary (cached-resource-vary resource)))
		 (cond ((eq vary :*) nil)
		       (vary
			(loop for variant in representations
			      when (variant-matches-request-p vary variant header-plist)
				return variant))      
		       (t (car representations))))))))
    (declare (inline %get-cached-representation))
    (or (%get-cached-representation resource header-plist)
	(ecase if-does-not-exist
	  (:soft nil)
	  (:create
	    (let* ((cache (cache-object-cache resource))
		   (database (cache-database cache))
		   (object (make-cached-representation resource)))
	      (setf (cached-representation-identifier object) (make-representation-identifier object)
		    (cached-representation-creation-date object) (or creation-time (get-universal-time))
		    (cached-representation-request-headers object) header-plist
		    (cached-representation-entity object) (make-http-cache-database-identifier database))
	      (push object (cached-resource-representations resource))
	      (values object t)))
	  (:error (error (format nil "There is no matching representation cached for ~S." 
				 (cached-resource-uri-string resource))))))))

(defun get-cached-representation (resource identifier &optional (errorp t))
  "Find a representation using its unique identifier, use for documentation, gc, etc."
  (check-type resource cached-resource)
  (let ((match (loop for variant in (cached-resource-representations resource)
                     when (equal identifier (cached-representation-identifier variant))
                     return variant)))
    (unless (or match (not errorp))
      (error (format nil "There is no matching representation cached for ~S."
                     (cached-resource-uri-string resource))))
    match))
  
(defun unintern-cached-representation (representation)
  "Unintern a representation of a cached resource."
  (check-type representation cached-representation)
  (let ((resource (cached-representation-resource representation)))
    (remove-database-object (cached-representation-entity representation))
    (setf (cached-resource-representations resource)
          (remove representation (cached-resource-representations resource) :test #'eq))))

(defmethod remove-cache-object ((cache-object cached-representation) (cache http-cache))
  (unintern-cached-representation cache-object))

(defmethod map-cache ((cache http-cache) (function function))
  (flet ((do-it (uri res)
           (declare (ignore uri))
           (loop for rep in (cached-resource-representations res)
                 doing (funcall function rep))
           (funcall function res)))
    (declare (inline do-it))
    (map-cached-resources #'do-it cache)))

;;;------------------------------------------------------------------- 
;;;
;;; OPERATIONS ON REPRESENTATIONS
;;;

(defmethod write-request-headers ((representation cached-representation) stream &optional termination-line-p
				  modification-plist excluded-headers additional-headers)
  (write-modified-headers (cached-representation-request-headers representation)
			  stream modification-plist excluded-headers termination-line-p additional-headers))

(defmethod write-response-headers ((representation cached-representation) stream &optional termination-line-p
				   modification-plist excluded-headers additional-headers)
  (write-modified-headers (cached-representation-response-headers representation)
			  stream modification-plist excluded-headers termination-line-p additional-headers))

(defgeneric get-header-value (headers header-keyword)
  (declare (values parsed-value found-p)))

(defmethod get-header-value ((header-set header-set) header-keyword)
  (get-header header-keyword header-set))

(defmethod get-header-value ((header-plist cons) header-keyword)
  (let ((value (getf header-plist header-keyword :+not-found+)))
    (case value
      (:+not-found+ (values nil nil))
      (t (values value t)))))

(declaim (inline get-request-header-value))
(defun get-request-header-value (representation header-keyword)
  (get-header-value (cached-representation-request-headers representation) header-keyword))

(declaim (inline get-response-header-value))
(defun get-response-header-value (representation header-keyword)
  (get-header-value (cached-representation-response-headers representation) header-keyword))

(defun representation-age (representation &optional (now (get-universal-time)))
  "return the current age of a cached representation"
  (- now (cached-representation-verified-date representation)))

;;maybe return negative for remaining fresh?
(defun representation-expired (representation &optional (now (get-universal-time)))
  "if expired returns staleness in seconds, otherwise nil"
  (- now (cached-representation-expiration-time representation)))

;; figure out what happens when there is no expires
(defun representation-freshness-lifetime (representation)
  "Return the freshness lifetime of a cached representation."
  (let* ((headers (cached-representation-response-headers representation))
	 (expires (getf headers :expires)))
    (when expires
      (- expires (cached-representation-verified-date representation)))))

(defparameter *proxy-cache-default-expiration-interval* (* 15 60)	;15 minutes
  "The default expiration interval for cached representations that provide no headers indicating when they expire.")

(define-generic compute-expiration-time (cached-representation)
  (declare (values expiration-universal-time))
  (:documentation "Computes the universal time when CACHED-REPRESENTATION becomes invalid and requires revalidation."))

(defmethod compute-expiration-time ((rep cached-representation))
  (let ((headers (cached-representation-response-headers rep)))
    (or ;; cache control directives override the expires header in HTTP 1.1
      (unless (member (cached-representation-http-version rep) '(:http/0.9 :http/1.0))
	(multiple-value-bind (directive found-p)
	    (getf headers :cache-control)
	  (and found-p
	       (let ((max-age (or (getf directive :s-maxage)	;S-MAXAGE overrides MAXAGE
				  (getf directive :maxage))))
		 (and max-age (+ max-age (cached-representation-verified-date rep)))))))
      ;; Try for an expires time and try to relativize it to avoid clock synchronization problems.
      (let ((expires (getf headers :expires))
	    date)
	(when expires
	  (if (setq date (getf headers :date))
	      (+ (- expires date) (cached-representation-verified-date rep))
	      expires)))
      ;; default to our standard stalness interval.
      (+ (cached-representation-verified-date rep) *proxy-cache-default-expiration-interval*))))

(defmethod cached-representation-expiration-time ((rep cached-representation))
  (or (%cached-representation-expiration-time rep)
      (setf (%cached-representation-expiration-time rep) (compute-expiration-time rep))))

(defmethod stale-cached-representation-p ((rep cached-representation))
  (let ((expiration (cached-representation-expiration-time rep))
	(time (or (and (boundp '*server*) (server-request-time *server*))
		  (get-universal-time))))
    (<= expiration time)))

(define-generic proxy-cached-representation-p (cached-representation request-headers http-version))

(defmethod proxy-cached-representation-p ((rep cached-representation) request-headers http-version)
  http-version
  (with-header-values (pragma) request-headers
    (cond (pragma (not (eql pragma :no-cache)))
	  (t t))))

(defun proxy-cacheable-server-response-p (http-version headers)
  "Returns non-null when the origin server HEADERS for HTTP-VERSION indicate that the response is cacheable by a proxy."
  (case http-version
    ((:http/0.9 :http/1.0)
     (with-header-values (expires date) headers
       (not (and expires date (< expires date)))))	;http 1.1 spec 14.9.3 backward compatibility feature for http 1.0
    (t (multiple-value-bind (directive found-p)
	   (get-header :cache-control headers)
	 (cond ((or (not found-p) (getf directive :public))
		t)
	       (t (loop for (key value) on directive by #'cddr
			do (case key
			     ((:private :no-cache :no-store)
			      (when value (return nil))))
			finally (return t))))))))

(defun proxy-cacheable-client-request-p (http-version headers)
  "Returns non-null when the client HEADERS for HTTP-VERSION indicate that the response is cacheable by a proxy."
  (case http-version
    ((:http/0.9 :http/1.0) t)
    (t (multiple-value-bind (directive found-p)
	   (get-header :cache-control headers)
	 (cond ((and found-p (getf directive :no-store)) nil)
	       (t t))))))

(define proxy-revalidate-cache-for-client-p (representation client-version &optional (time (get-universal-time)))
  "Returns non-null if a CACHE-CONTROL header contains the no cache directive."
  (or (case client-version
	((:http/0.9 :http/1.0)
	 (< (cached-representation-expiration-time representation) time))	; Standard freshness check.
	;; equivalent to previous netscape lossage for http 1.1
	(t (multiple-value-bind (directive found-p)
	       (get-header :cache-control)
	     (if found-p
		 ;; check cache control parameters
		 (cond ;; forced revalidation by client. :NO-CACHE can force a partial revalidation but we don't implement it.
		   ((getf directive :no-cache) t)
		   ;; Cache expired?
		   ((< (cached-representation-expiration-time representation) time)
		    (let ((max-stale (getf directive :max-stale)))
		      ;; Cache cannot be any staler than MAX-STALE.
		      (if (and max-stale (< (- time (cached-representation-expiration-time representation)) max-stale))
			  nil
			  t)))
		   ;; Cache must be more recent than MAX-AGE
		   ((let ((max-age (getf directive :max-age)))
		      (and max-age
			   (or (zerop max-age)
			       (> max-age (- time (cached-representation-verified-date representation))))))
		    t)
		   ;; Cache must retain at least as much freshness as MIN-FRESH.
		   ((let ((min-fresh (getf directive :min-fresh)))
		      (and min-fresh
			   (< min-fresh (- (cached-representation-expiration-time representation) time))))
		    t)
		   (t nil))
		 ;; Standard freshness check.
		 (< (cached-representation-expiration-time representation) time)))))
      ;; HTTP 1.1 clients can send pragma too.
      (multiple-value-bind (directive found-p) 
	  (get-header :pragma)
	(and found-p
	     (etypecase directive
	       (atom (eq :no-cache directive))
	       (cons (member :no-cache directive)))))))

(defmethod cached-representation-update-response-headers ((representation cached-representation) (response-headers header-set))
  "Merges any missing headers from old-headers into new-headers when recording server response headers for the proxy cache."
  (let ((old-headers (cached-representation-response-headers representation))) 
    (flet ((update-header (header header-object)
	     (setf (getf old-headers header) (header-value header-object))))
      (declare (dynamic-extent #'update-header))
      (map-headers #'update-header response-headers)
      (setf (cached-representation-response-headers representation) old-headers))))

(defun cached-representation-note-transaction (representation http-version verified-date request-headers)
  "Called to update the data for cached representation whenever a new request is cached or validated."
  (flet ((must-revalidate-p (http-version request-headers response-headers)
	   (case http-version
	     ((:http/0.9 :http/1.0)
	      (get-header :authorization request-headers))
	     (t (multiple-value-bind (directive found-p)
		    (getf response-headers :cache-control)
		  (cond (found-p
			 (loop for (key value) on directive by #'cddr
			       when (and (member key '(:must-revalidate :proxy-revalidate)) value)
				 return t
			       finally (return (and (get-header :authorization request-headers)
						    (not (or (get-header :must-revalidate request-headers)
							     (get-header :public request-headers)
							     (let ((s-maxage (get-header :s-maxage request-headers)))
							       (and s-maxage (not (zerop s-maxage))))))))))
			(t (get-header :authorization request-headers))))))))
    (declare (inline must-revalidate-p))
    (let ((response-headers (cached-representation-response-headers representation)))
      (setf (cached-representation-request-headers representation) (proxy-header-plist request-headers)
	    (cached-representation-verified-date representation) verified-date
	    (cached-representation-etag representation) (getf response-headers :etag)
	    (cached-representation-last-modification representation) (getf response-headers :last-modified)
	    (cached-representation-must-revalidate-p representation) (must-revalidate-p http-version request-headers response-headers)
	    (cached-representation-http-version representation) http-version
	    (%cached-representation-expiration-time representation) nil))))	;reset expiration time

(defmacro with-open-representation ((stream representation direction) &body body)
  "Execute BODY with STREAM bound to a stream into REPRESENTATION.
DIRECTION is either :INPUT or :OUTPUT and is evaluated at compile time."
  `(let ((identifier (cached-representation-entity ,representation)))
     ,(ecase direction
	(:output
	  `(progn 
	     (setf (cache-object-size ,representation) 0)
	     (multiple-value-prog1
	       (with-open-resource (,stream identifier :output)
		 ,@body)
	       (setf (cache-object-size ,representation) (database-object-size identifier)))))
	(:input
	  `(with-open-resource (,stream identifier :input)
	     ,@body)))))

(defmacro with-input-from-representation-entity ((stream representation) &body body)
  "Execute BODY with STREAM bound to an input stream from a representation's entity."
  `(let ((identifier (cached-representation-entity ,representation)))
     (with-open-resource (,stream identifier :input)
       ,@body)))

(defun write-representation-entity (representation output-stream)
  "write a representation of a resource to a stream"
  (with-open-representation (data-stream representation :input)
    (stream-copy-until-eof data-stream output-stream :binary)))

(defun write-representation-entity-byte-range (representation output-stream start end)
  "write a representation of a resource to a stream"
  (with-open-representation (data-stream representation :input)
    (stream-copy-byte-range data-stream output-stream start end)))
