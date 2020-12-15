;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-

;;; (C) Copyright 1996-97, Christopher R. Vincent
;;;     All Rights Reserved.h
;;; (C) Enhancements 1998, John C. Mallery
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CL-HTTP PROXY SERVER
;;;

(in-package :http)

;; Obviously, still needs a way to expunge items.

;;;------------------------------------------------------------------- 
;;;
;;; GENERIC DATABASE INTERFACE
;;;

(defclass database ()
  ((identifier-class :initarg :identifier-class :accessor database-identifier-class))
  (:documentation "A generic database."))

(defgeneric initialize-database (database)
  (:documentation "Initialize an instance of a database."))

(defmethod initialize-instance :after ((database database) &key)
  (initialize-database database))

(defun make-database (class &rest args)
  "Return a new database."
  (declare (dynamic-extent args))
  (apply #'make-instance class args))

(defclass database-identifier ()
  ((database :initarg :database :accessor database-identifier-database)
   (value :initarg :value :accessor database-identifier-value))
  (:documentation "A unique identifier that can be used to access a database entry."))

(defgeneric initialize-database-identifier (database identifier)
  (:documentation "Initialize an instance of a database-identifier."))

(defgeneric make-database-identifier (database)
  (:documentation "Return a new database identifier."))

(defmethod make-database-identifier ((database database))
  (let ((object (make-instance (database-identifier-class database) :database database)))
    (initialize-database-identifier object database)
    object))

(defgeneric write-database-identifier (identifier stream)
  (:documentation "Write a rendition of a database identifier over a stream."))

(defgeneric database-object-size (database-identifier)
  (:documentation "Returns the number of bytes in a database object."))

(defgeneric remove-database-object (database-identifier)
  (:documentation "Remove an object from a database."))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defgeneric open-resource (database identifier direction)
  (declare (values stream))
  (:documentation "Opens a stream to the resource denoted by IDENTIFIER in DATABASE.
DIRECTIONN is either :INPUT or :OUTPUT."))

(defmacro with-open-resource ((stream identifier direction) &body body)
  "Execute body with STREAM bound to stream into the resource denoted by IDENTIFIER.
DIRECTION is either :INPUT or :OUTPUT."
  `(let ((abort-p t)
	 (,stream))
     (unwind-protect
	 (multiple-value-prog1
	   (progn
	     (setq ,stream (open-resource (database-identifier-database ,identifier) ,identifier ,direction))
	     ,@body)
	   (setq abort-p nil))
       (when ,stream
	 (close ,stream :abort abort-p)))))

;;;------------------------------------------------------------------- 
;;;
;;; FILESYSTEM DATABASE
;;;

(defclass filesystem-database (database)
  ((identifier-class :initform 'filesystem-database-identifier)
   (default-pathname :initform (pathname "http:proxy-cache;") :initarg :default-pathname :accessor database-default-pathname))
  (:documentation "A database implemented on the filesystem."))

(defmethod initialize-instance :after ((database filesystem-database) &key)
  (let ((pathname (database-default-pathname database)))
    (when pathname
      (pathname-create-directory-if-needed (pathname pathname)))))

(defmethod initialize-database ((database filesystem-database))
  database)

(defclass filesystem-database-identifier
	  (database-identifier)
    ()
  (:documentation "Identifier for a filesystem-database."))

(defparameter *filesystem-database-identifier-counter* 1000
  "Used for generating filesystem-database-identifiers.")

(defmethod initialize-database-identifier ((identifier filesystem-database-identifier)
                                           (database filesystem-database))
  (setf (database-identifier-value identifier)
        (merge-pathnames (database-default-pathname database)
                         (write-to-string (atomic-incf *filesystem-database-identifier-counter*)))))

(defmethod write-database-identifier ((id filesystem-database-identifier) stream)
  (with-slots (value) id
    (write-string (namestring value) stream)))

(defmethod database-object-size ((id filesystem-database-identifier))
  (file-length-in-bytes (database-identifier-value id)))

(defmethod remove-database-object ((id filesystem-database-identifier))
  (let ((file (database-identifier-value id)))
    (when (probe-file file)
      (delete-file file))))

(defmethod open-resource ((database filesystem-database) (identifier filesystem-database-identifier) (direction (eql :input)))
  (open (database-identifier-value identifier) :direction :input :element-type '(unsigned-byte 8)
	:if-does-not-exist :error))

(defmethod open-resource ((database filesystem-database) (identifier filesystem-database-identifier) (direction (eql :output)))
  (open (database-identifier-value identifier) :direction :output :element-type '(unsigned-byte 8)
	:if-does-not-exist :create :if-exists :supersede))

;;;------------------------------------------------------------------- 
;;;
;;; HTTP CACHE DATABASE
;;;

;;; This section of code must be modified and the system recompiled when 
;;; switching to a new type of database.

(defclass http-cache-database (filesystem-database)
  ((identifier-class :initform 'http-cache-database-identifier))
  (:documentation "A database for an HTTP cache."))

(defun make-http-cache-database (&optional (pathname (proxy-cache-default-directory)))
  (make-database 'http-cache-database :default-pathname pathname))

(defclass http-cache-database-identifier (filesystem-database-identifier) () 
  (:documentation "An identifier for an HTTP cache database."))

(defun make-http-cache-database-identifier (database)
  "Accepts request headers, returns a new http-cache-identifier."
  (make-database-identifier database))
