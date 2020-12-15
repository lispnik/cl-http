;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: URL; Base: 10 -*-

(in-package "URL")

;;; Prototype URLs for UNIX CGI access from CL-HTTP.
;;; Copyright (C) 1995 Olivier (OBC).
;;;	All Rights Reserved.

(defpackage "URL"
  (:use)
  (:export "HTTP-UNIXCGI-PATH" "HTTP-UNIXCGI-OBJECT" "HTTP-UNIXCGI-SEARCHABLE-OBJECT"
	   "MAKE-OBJECT-PATHNAME-NAME" "UNIXCGI-URL" "URL-SEARCH-STRING-P" "QUICK-SEARCH-PARENT"))

;;;
;;; URL definitions for UNIX CGI
;;;

(defclass http-unixcgi-path
          (http-path)
    ()
  (:documentation "URL path to a UNIX CGI script."))

(defclass http-unixcgi-object
          (http-object http-unixcgi-path)
    ()
  (:documentation "Root class for standard http objects on the server."))

;;; Bypass the general search mechanism of CL-HTTP
;;;
(defclass http-unixcgi-searchable-object
          (http-unixcgi-object)
    ((search-string :initform nil :initarg :search-string))
    (:documentation "Root class for standard http objects on the server."))

(defmethod search-parser ((url http-unixcgi-searchable-object))
  #'(lambda (string &optional (start 0) end)
      (declare (ignore string start end))
      nil))

(defmethod write-local-name ((url http-unixcgi-searchable-object) &optional stream)
  (write-path url stream)
  (write-char #\/ stream)
  (write-object-name-string url stream)
  (write-search url stream))

(defmethod write-search ((url http-unixcgi-searchable-object) &optional (stream *standard-output*))
  (write-char *search-url-delimiter* stream)
  (with-slots (search-string) url
    (if search-string
	(write-string search-string stream))))

(defmethod make-object-pathname-name ((url http-unixcgi-object))
  (with-slots (object extension) url
    (namestring (make-pathname :name object
			       :type extension))))

(defvar *search-url-string* (make-string 1 :initial-element *search-url-delimiter*))

(defun url-search-string-p (string)
  (and (position *search-url-delimiter* string) t))

(defmethod nsubseq ((sequence array) (start fixnum) &optional (end (length sequence)))
  (locally (declare (optimize (speed 3) (safety 0)))
    (if (and (= start 0)
	     (eql end (length sequence)))
	sequence
      (make-array (- end start)
		  :element-type (array-element-type sequence)
		  :displaced-to sequence
		  :displaced-index-offset start))))

(defun url-search-nsubseq (string)
  (let ((pos (position *search-url-delimiter* string)))
    (if pos
	(values (nsubseq string 0 (incf pos)) pos))))

(defun unixcgi-url (url &key ((:host host-string)) port pathname (start 0) (end (length url)) &allow-other-keys)
  (let (object urlo extension path-index)
    ;; extract the host parameters
    (unless (and host-string port)
      (multiple-value-setq (host-string port path-index)
        (get-host-port-info url start end)))
    ;; extract the path components
    (multiple-value-bind (path object-index next-index search-p)
	(get-path-info url path-index end)
      ;; get the object components when present
      (when object-index
	(multiple-value-setq (object extension)
	  (get-object-info url object-index next-index)))
      (cond 
       (object
	(if search-p
	    (setq urlo
	      (make-instance 'http-unixcgi-searchable-object
		:host-string host-string
		:port port
		:path path
		:object object
		#+ignore (if extension
			     object
			   (concatenate 'string object *search-url-string*))
		:search-string (subseq url (the fixnum (1+ next-index)) end)
		:extension extension
		#+ignore   (if object
			       extension
			     (concatenate 'string extension *search-url-string*))))
	  (setq urlo
	    (make-instance 'http-unixcgi-object
	      :host-string host-string
	      :port port
	      :path path
	      :object object
	      :extension extension))))
       (t
	(setq urlo
	  (make-instance 'http-unixcgi-path
	    :host-string host-string
	    :port port
	    :path path)))))
    (if pathname
	(setf (translated-pathname urlo) pathname))
    urlo))

(defmethod quick-search-parent ((url string))
  (multiple-value-bind (basearch position)
      (url-search-nsubseq url)
    (if (not (eq basearch url))
	(values (gethash basearch *url-table*) position))))

(defmethod quick-search-parent ((url http-unixcgi-searchable-object))
  (quick-search-parent (name-string url)))

(defmethod quick-search-parent ((url http-unixcgi-object))
  nil)
