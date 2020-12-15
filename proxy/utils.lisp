;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTTP; Base: 10 -*-
;;;
;;; (C) Copyright 1996-1997, Christopher R. Vincent and John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; VARIABLES
;;; 

(in-package :http)


;;;------------------------------------------------------------------- 
;;;
;;; EXPORTS
;;;

(mapc #'(lambda (x)
	        (export (intern x :http) :http))
          '("*HTTP-PROXY-CACHE*"
	     "CACHE-OBJECT-SIZE"
	     "CACHED-REPRESENTATION-RESPONSE-HEADERS"
	     "CACHED-REPRESENTATION-VERIFIED-DATE"
             "DISABLE-PROXY-SERVICE" 
             "ENABLE-PROXY-SERVICE"
             "EXPORT-PROXY-INTERFACES"
	     "HTTP-CACHE"
	     "INTERN-CACHED-REPRESENTATION"
	     "INTERN-CACHED-RESOURCE"
             "PROXY-SERVICE-ENABLED-P"
	     "UNINTERN-CACHED-REPRESENTATION"
	     "UNINTERN-CACHED-RESOURCE"
	     "WITH-INPUT-FROM-REPRESENTATION-ENTITY"
	     "WITH-OUTPUT-TO-REPRESENTATION-ENTITY"))

;;;------------------------------------------------------------------- 
;;;
;;; VARIABLES
;;;

(define-parameter *debug-proxy* nil
  "Controls proxy debugging.")

(define-parameter *trace-proxy* nil
  "Controls proxy tracing.")

(defparameter *http-proxy-cache* nil
  "The primary HTTP proxy cache.")

(defparameter *http-proxy-database* nil
  "The primary HTTP proxy database.")

(define-parameter *preserve-via-header-comments-p* t
  "When NIL, strips the optional comments from Via header entries.") 

(define-parameter *proxy-caching-p*
  #+(OR MCL Genera) t
  #-(OR MCL Genera) nil
  "Allow the proxy to use caching operations.")

(define-parameter *proxy-methods* '(:get :head :options :trace :post :put :delete)
  "The methods generally supported through the proxy.  Used for OPTIONS.")

(define-parameter *proxy-via-header* nil
  "The header value to be used when appending the Via header.")

(define-parameter *proxy-via-header-no-comments* nil
  "Short version of the header value to be used when appending the Via header.")


;;;------------------------------------------------------------------- 
;;;
;;;  UTILITIES
;;;

(defun debug-proxy (&optional (on-p (not *debug-proxy*)))
  "Toggle proxy debugging.
Turns off condition handling in proxy-specific code"
  (setf *debug-proxy* on-p))

(defun trace-proxy (&optional (on-p (not *trace-proxy*)))
  "Toggle proxy tracing."
  (setf *trace-proxy* on-p))

(defmacro proxy-trace (format-string &rest format-args)
  `(when *trace-proxy*
     (format *trace-output* ,format-string ,@format-args)))

#+MCL
(defun get-ramdisk-pathname ()
  "Returns the RAM disk pathame when a RAM disk exists on a MAC."
  (probe-file (pathname "RAM Disk:")))

(defun proxy-cache-default-directory ()
   "Returns the default pathname to use as the cache directory."
   #+MCL
   (let ((ramdisk (get-ramdisk-pathname)))
      (if ramdisk
	 (merge-pathnames ";proxy-cache;" ramdisk)
	 (pathname "http:proxy-cache;")))
   #-MCL
   (pathname "http:proxy-cache;"))

(declaim (ftype (function (&optional pathname) *) make-http-cache-database))

(defun initialize-proxy-cache-database (&optional redo-p)
  (cond ((or redo-p (null *http-proxy-database*))
	 (setq *http-proxy-database* (make-http-cache-database (proxy-cache-default-directory))))
	(t *http-proxy-database*)))

(declaim (ftype (function * *) make-http-cache))

(defun initialize-proxy-cache (&optional redo-p)
  (cond ((or redo-p (null *http-proxy-cache*))
	 (initialize-proxy-cache-database redo-p)
	 (setq *http-proxy-cache* (make-http-cache "HTTP Proxy Cache"
						   :database *http-proxy-database*
						   :description "The primary cache for the HTTP proxy server.")))
	(t *http-proxy-cache*)))

(define enable-proxy-service ()
   "Enables proxy service on a running server."
   (setq *proxy-service* t))

(define disable-proxy-service ()
   "Disables proxy service on a running server."
   (setq *proxy-service* nil))

(declaim (inline proxy-service-enabled-p))

(define proxy-service-enabled-p ()
   "Returns non-null if proxy service is enabled."
   *proxy-service*)

(define initialize-proxy (&optional redo-p)
   "Initializes and enables proxy service.
Called on server initialization list."
  (initialize-proxy-cache redo-p)
  (enable-proxy-service))

(add-initialization
  "Initialize Proxy."
  '(initialize-proxy)
  '(:normal)
  '*server-initialization-list*)
