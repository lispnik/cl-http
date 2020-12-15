;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: HTTP; Base: 10 -*-

(in-package "HTTP")

;;; Prototype UNIX CGI access from CL-HTTP.
;;; Copyright (C) 1995 Olivier (OBC).
;;;	All Rights Reserved.
;;;
;;; We hit our head against CL-HTTP's assumption that all
;;; url must be pre-exported (even in *auto-export* mode) this
;;; conflicts with what we expect on UNIX, but it's much more secure?
;;;
;;; For now we expect the contents of the CGI-BIN and subdirectories
;;; to be exported once. We don't expect it to change later, this
;;; can be argued as a security measure. And we don't expect
;;; *auto-export* to work for CGI-BIN objects. Since we don't need
;;; the extra search processing provided by HTTP-SEARCH, we define
;;; a set of raw search objects for UNIXCGI and response methods
;;; that escape to UNIX shell.
;;;
;;; Examples: 
;;;
;;; Note that you can define multiple cgi-bin places...
;;;
;;; (define-unix-cgi-bin "/cl-http/cgi-bin/" (pathname "http:acl;cgi-bin;"))
;;;
;;; or the equivalent:
;;; (export-url "/cl-http/cgi-bin/" :unixcgi :pathname (pathname "http:acl;cgi-bin;") :redefine t)
;;;
;;; accessing the following URLs will provide minimum testing:
;;; http://you.machine.site.edu/cl-http/cgi-bin/date
;;; http://you.machine.site.edu/cl-http/cgi-bin/test
;;; http://you.machine.site.edu/cl-http/cgi-bin/test?
;;; http://you.machine.site.edu/cl-http/cgi-bin/test?foo=1&bar=1
;;;
;;; If you add cgi scripts in "http:acl;cgi-bin;" you need to reevaluate
;;; the above definition to test them.
;;;

(defmethod export-type ((url url:http-unixcgi-path))
  :unixcgi)

(defmethod translation-method ((url url:http-unixcgi-path))
  :unixcgi)

(defmethod export-url ((url string) (translation (eql :unixcgi)) &rest args)
  (let (mainurl
	(pathname (getf args :pathname)))
    (setq pathname (translate-logical-pathname pathname))
    (setf (getf args :pathname) pathname)
    (setq mainurl 
      (apply #'url:unixcgi-url (merge-url url (local-context)) args))
    (cond ((getf args :redefine)
	   (url::unregister (url::register mainurl t))
	   (setq mainurl (url::register mainurl nil)))
	  (t
	   (setq mainurl (url::register mainurl t))))
    (when (pathname-directory-p pathname)
      (loop with fullname 
	  for path in (directory-list* pathname nil :directories)
	  as name = (pathname-name path)
	  as type = (pathname-type path)
	  when (or type name)
	  do (setq fullname (namestring (make-pathname :name name :type type :directory (cons :absolute (slot-value mainurl 'url::path)))))
	     (apply #'export-url fullname
		    :unixcgi :pathname path args)
	     (unless (or (url:url-search-string-p fullname)
			 (pathname-directory-p path))
	       (apply #'export-url (concatenate 'string fullname url::*search-url-string*)
		      :unixcgi :pathname path
		      args))))
    mainurl))

;;; Alternate UNIX CGI interface
;;;
(defmacro define-unix-cgi-bin (url pathname &key (redefine t))
  `(export-url ,url
	       :unixcgi
	       :pathname ,pathname
	       :redefine ,redefine
	       ;; :expiration `(:interval 0)
	       :keywords '(:cl-http :unix-cgi)))

;;;
;;; UNIX CGI PARENT SEARCH AND MUTATOR
;;;

(defmethod reuse-registered-search-object (url url-string position)
  (declare (ignore url url-string position)))

(defmethod reuse-registered-search-object ((url url:http-unixcgi-searchable-object) url-string position)
  (declare (ignore position))
  (url:unixcgi-url (merge-url url-string (local-context))))

(defmethod mutate-pre-exported-search-url ((url-string string))
  (multiple-value-bind (parent position)
      (url:quick-search-parent url-string)
    (if parent
	(reuse-registered-search-object parent url-string position))))

;;;
;;; UTILITIES ADAPTED FOR UNIX RESPONSES:
;;;

(defvar *cgi-bindings* (get-cgi-variable-bindings (all-cgi-variables)))

;;; If you send too much parameters, use post or you're out of luck here
;;;
(defvar *unix-cgi-buffer* (make-string 4096 :element-type 'character))

(defmacro with-write-buffer ((&rest ignore) . body)
  (declare (ignore ignore))
  `(macrolet ((write-buffer (string buf pos)
		`(let ((#1=#:string ,string)
		       #2=#:length)
		   (unless (stringp #1#)
		     (setq #1# (princ-to-string #1#)))
		   (setq #2# (length #1#))
		   (setf (subseq ,buf ,pos (incf ,pos #2#)) #1#)))
	      (write-char-buffer (char buf pos)
		`(progn (setf (aref ,buf ,pos) ,char)
			(incf ,pos))))
     ,@body))

(defmacro with-unix-cgi-buffer ((buffer &key (protect t)) . body)
  `(let ((,buffer *unix-cgi-buffer*))
     (with-write-buffer ()
       (if ,protect
	   (clim-sys:without-scheduling ,@body)
	 (progn ,@body)))))

(defun write-buffer-unix-cgi-vars (server buffer pos &rest keyvals)
  (with-write-buffer ()
    (loop for (var def) in *cgi-bindings*
	as val = (getf keyvals var)
	do (write-buffer (symbol-name var) buffer pos)
	   (write-buffer "=" buffer pos)
	   (unless val
	     (typecase def
	       ((and symbol (not null))
		(setq val (symbol-value def)))
	       (cons
		(setq val (apply (first def)
				 (subst server '*server* (rest def)))))
	       (t (setq val def))))
	   (write-char-buffer #\" buffer pos)
	   (write-buffer (or val "") buffer pos)
	   (write-char-buffer #\" buffer pos)
	   (write-char-buffer #\Space buffer pos)))
  (values pos buffer))

;;; OVERLOADING FOR UNIXCGI

;;; Note that the default HTTP scheme parser can easily
;;; be extended. So the parser will construct and HTTP-SEARCH
;;; object, however we tricked the translation to be :UNIXCGI
;;; we can now patch the scheme parse final effect here.
;;;
(defmethod write-document ((url url:http-search) (translation (eql :unixcgi)) stream)
  (let ((originalsearch (server-url-string *server*)))
    (write-document (mutate-pre-exported-search-url originalsearch) translation stream)))

(defvar *unix-shell-illegals* ";& ()|%?*<>	
{}$@![]'\"`#")

(defun unix-shell-validation (name url &optional (url-string (url:name-string url)))
  (if (find-if #'(lambda (c) (find c *unix-shell-illegals*)) name)
      (error 'bad-syntax-provided :url url
	     :format-string "No form values were returned for UNIX CGI ~A."
	     :format-args (list url-string))))

;;;
;;; UNIX CGI METHODS SUPPORTED
;;;

(defmethod write-document ((urlo url:http-unixcgi-object) translation stream)
  (declare (ignore translation))
  (let ((url (server-url-string *server*))
	(method :get))
    (let ((pos? (position url::*search-url-delimiter* url))
	      name (shargs "") cmd (args ""))
      (setq name (url:make-object-pathname-name urlo))
      (unix-shell-validation name urlo url)
      (if pos?
	  (setq args (subseq url (1+ pos?))))
      (unless (find #\= args)
	(setq shargs args
	      args ""))
      (setq cmd (namestring (url:translated-pathname (or (url:quick-search-parent urlo) urlo))))
      (let (getcmd (pos 0))
	(with-unix-cgi-buffer (buffer)
	  (setq pos
	    (write-buffer-unix-cgi-vars *server* buffer pos
					:script_name name
					:request_method method
					:query_string args
					:path nil))
	  (write-buffer cmd buffer pos)
	  (write-char-buffer #\Space buffer pos)
	  (write-buffer shargs buffer pos)
	  (setq getcmd (subseq buffer 0 pos)))
	;;(format t "~&~a~%" getcmd)
	(let ((ustream (excl:run-shell-command 
			getcmd :input :stream :output :stream
			:wait nil)))
	  (report-status-success stream)
	  (stream-copy-until-eof ustream stream)
	  ;; Pre-close stream
	  (setf (ipc::tcp-client-alive stream) nil)
	  (close ustream)
	  t)))))

(defmethod post-document ((urlo url:http-unixcgi-object) type subtype stream)
  (declare (ignore type subtype))
  (let ((url (server-url-string *server*))
	(method :post))
    (let (name cmd)
      (setq name (url:make-object-pathname-name urlo))
      (unix-shell-validation name urlo url)
      (setq cmd (namestring (url:translated-pathname urlo)))
      (let (postcmd (pos 0))
	(with-unix-cgi-buffer (buffer)
	  (setq pos
	    (write-buffer-unix-cgi-vars *server* buffer pos
					:script_name name
					:request_method method
					:content_length (get-header :context-length)
					:path nil))
	  (write-buffer cmd buffer pos)
	  (setq postcmd (subseq buffer 0 pos)))
	(format t "~&~a~%" postcmd)
	(let ((ustream (excl:run-shell-command 
			postcmd :input :stream :output :stream
			:wait nil)))
	  (stream-copy-until-eof stream ustream)
	  (report-status-success stream)
	  (stream-copy-until-eof ustream stream)
	  ;; Pre-close stream
	  (setf (ipc::tcp-client-alive stream) nil)
	  (close ustream)
	  t)))))
