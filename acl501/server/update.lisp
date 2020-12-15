(in-package "URL")

;;; Prototype Fast Dynamic Updates to registered URLs.
;;; Copyright (C) 1995 Olivier (OBC) all rights reserved.
;;;
;;; This provides fast and cheap alternatives to reloading all exports
;;; to update the server when anything signification changes in the
;;; server location such as new name, new address, new port or new
;;; physical location for contents.
;;;
;;; Provide bare updating the url table for local context changes
;;; and for translated pathname location changes. Make a server
;;; in one place and move it to a new site where it will start running
;;; just as before.
;;;

(defpackage "URL"
  (:use) (:export "UPDATE-URLS" "UPDATE-URLS-PATHNAME" "SHOW-URLS"))

(defvar *url-table2* (make-hash-table :test #'equalp))

(defun context-host-port (context)
  (if (string= "http://" context :end1 7 :end2 7)
      (get-host-port-info context 7
			  (or (position #\/ context :start 7) (length context))
			  t)
    (error "Not a valid local context: ~s" context)))

;;;
;;; This will have to be called multiple times if the server is listening
;;; on multiple ports  since this only switches from one known context
;;; (port) to a new one at a time. This is currently limited to http
;;; schema.
;;;
;;; Example:
;;; (update-urls (http::local-context) "http://foo.bar:1000" (www-utils:loca-host))
;;;

;;; Find all URLs matching an old context "http://asdfsdf:34324" and update
;;; them to match a new local context.
;;;
(defun update-urls (newcontext oldcontext &key (local-host-ip-number (www-utils:local-host)) fast-remap-p)
  (if (not fast-remap-p)
      ;; This now uses John's host change approach. Will work
      ;; for all host objects we thing. But is a bit slower ;-)
      (multiple-value-bind (old-host old-port)
	  (context-host-port oldcontext)
	(multiple-value-bind (new-host new-port)
	    (context-host-port newcontext)
	  (url::remap-url-host old-host new-host :old-port old-port :new-port new-port)))
    (let (changed)
      (clrhash *url-table2*)
      (multiple-value-bind (old-host old-port)
	  (context-host-port oldcontext)
	(multiple-value-bind (new-host new-port)
	    (context-host-port newcontext)
	  (with-hash-table-iterator (iterate *url-table*)
	    (loop 
	      (multiple-value-bind (found url-string url) (iterate)
		(if (null found)
		    (return-from update-urls (cond (changed
						    (rotatef *url-table* *url-table2*)
						    *url-table*)
						   (t
						    :unchanged)))
		  (multiple-value-bind (host port pos)
		      (get-host-port-info url-string)
		    (when (and (equal host old-host)
			       (eql port old-port))
		      (setq changed t)
		      (setq url-string (concatenate 'string newcontext "/" (nsubseq url-string pos)))
		      (setf (slot-value url 'name-string) url-string
			    (slot-value url 'host-string) new-host
			    (slot-value url 'port) new-port)
		      (if local-host-ip-number
			  (setf (slot-value url 'host-object) local-host-ip-number)))
		    (setf (gethash url-string *url-table2*) url)))))))))))

(defun make-local-context (hostdomain port)
  (case port
    ((nil 80) (concatenate 'string "http://" (string-downcase hostdomain)))
    (t (concatenate 'string "http://" (string-downcase hostdomain) ":" (write-to-string port :base 10)))))

(defun update-urls-host-port (newhost newport oldhost oldport &optional (local-host-ip-number (www-utils:local-host)))
  (newer-update-urls oldhost newhost :old-port oldport :new-port new-port)
  (update-urls (make-local-context newhost newport) (make-local-context oldhost oldport) local-host-ip-number))

;;; Surgically replace a portion of pathnames matching oldpath
;;; by newpath in all urls. This is only useful to patch a server
;;; delivered with preloaded urls. After a directory structure
;;; has been moved around. Otherwise reloading the exports will do fine.
;;;
#-(or ACLPC KCL)
(defun update-urls-pathname (newpath oldpath)
  (let (match changed)
    (setq newpath (namestring newpath)
	  oldpath (namestring oldpath))
    (with-hash-table-iterator (iterate *url-table*)
      (loop (multiple-value-bind (found url-string url)
		(iterate)
	      (declare (ignore url-string))
	      (if (null found)
		  (return-from update-urls-pathname
		    (cond (changed
			   *url-table*)
			  (t
			   :unchanged)))
		(let ((pathname (and (slot-exists-p url 'pathname)
				     (slot-value url 'pathname))))
		  (when (and pathname
			     (setq pathname (namestring pathname))
			     (setq match (search oldpath pathname)))
		    (setq changed t)
		    (setq pathname (concatenate 'string
				     (nsubseq pathname 0 match)
				     newpath
				     (nsubseq pathname (min (+ match (length oldpath))
							    (length pathname)))))
		    (setf (slot-value url 'pathname) (pathname pathname))))))))))

#+(or ACLPC KCL)
(defun update-urls-pathname (newpath oldpath)
  (let (match changed)
    (setq newpath (namestring newpath)
	  oldpath (namestring oldpath))
    (maphash #'(lambda (url-string url)
		 (declare (ignore url-string))
		 (let ((pathname (and (slot-exists-p url 'pathname)
				      (slot-value url 'pathname))))
		   (when (and pathname
			      (setq pathname (namestring pathname))
			      (setq match (search oldpath pathname)))
		     (setq changed t)
		     (setq pathname (concatenate 'string
				      (nsubseq pathname 0 match)
				      newpath
				      (nsubseq pathname (min (+ match (length oldpath))
							     (length pathname)))))
		     (setf (slot-value url 'pathname) (pathname pathname)))))
	     *url-table*)
    (cond (changed
	   *url-table*)
	  (t
	   :unchanged))))

#-(or ACLPC KCL)
(defun show-urls (&optional showpath)
  (with-hash-table-iterator (iterate *url-table*)
    (loop as l = (multiple-value-list (iterate))
	while (first l)
	do (write-string (second l))
	   (write-char #\ )
	   (let ((url (third l)))
	     (if showpath
		 (write (and (slot-exists-p url 'pathname)
			     (slot-value url 'pathname)))
	       (write url)))
	   (terpri))))

#+(or ACLPC KCL)
(defun show-urls (&optional showpath)
  (maphash #'(lambda (key url)
	       (write-string key)
	       (write-char #\ )
	       (if showpath
		   (write (and (slot-exists-p url 'pathname)
			       (slot-value url 'pathname)))
		 (write url))
	       (terpri))
	   *url-table*))
