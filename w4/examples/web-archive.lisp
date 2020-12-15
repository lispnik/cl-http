;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: w4; Base: 10 -*-

;;; (C) Copyright 1996, Christopher R. Vincent
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; WEB WALKER ARCHIVING APPLICATION
;;;

(in-package :w4)

#|
issues remaining:   -cvince 8/19/96
captures headers, doesn't do anything with them yet
the web interface for defining/building archives is not fully developed
strip or insert base tag
intern should do right thing, sometimes inconsistencies with hosts that have aliases (www.ai.mit.edu)
parser blows out on cl-http homepage(?)
need to use gensym in some macros
develop error handling/reporting
there is no good way to know when to stop translating uris during walk (except depth)
need to break up the code that save-web-archive produces to facilitate incremental dumps
|#


;;;------------------------------------------------------------------- 
;;;
;;; ARCHIVE INFRASTRUCTURE
;;;

(define-parameter *web-archive-base-uri-path* "/cl-http/archives/"
  "The base uri that archives are exported under.")

(define-parameter *web-archive-default-directory* #p"http:web-archive;"
  "Default directory for storing archived resources.")

(define-variable *web-archive-table* (make-hash-table :test #'equalp)
  "Maps archive names to web-archive objects.")

(define-variable *web-archive-resource-version* :newest
  "Controls which version of a resource is returned, integer or :newest.")

(defclass web-archive ()
  ((name :initarg :name :accessor web-archive-name)
   (display-string :initarg :display-string :accessor web-archive-display-string)
   (resource-table :initarg :resource-table :accessor web-archive-resource-table)
   (directory :initarg :directory :accessor web-archive-directory)
   (creation-date :initarg :creation-date :accessor web-archive-creation-date))
  (:documentation "A web archive."))

(defmethod print-object ((web-archive web-archive) stream)
  (with-slots (display-string) web-archive
    (print-unreadable-object (web-archive stream :type t :identity t)
      (when (slot-boundp web-archive 'display-string)
        (write-string display-string stream)))))

(defclass archive-resource ()
  ((uri-string :initarg :uri-string :accessor archive-resource-uri-string)
   (version-table :initarg :version-table :accessor archive-resource-version-table)
   (creation-date :initarg :creation-date :accessor archive-resource-creation-date))
  (:documentation "A resource in a web archive."))

(defmethod print-object ((archive-resource archive-resource) stream)
  (with-slots (uri-string) archive-resource
    (print-unreadable-object (archive-resource stream :type t :identity t)
      (when (slot-boundp archive-resource 'uri-string)
        (write-string uri-string stream)))))

(defun %initialize-archive (archive)
  "perform the appropriate exports for an archive."
  (with-slots (name display-string) archive
    (let* ((name-string (string-downcase (symbol-name name)))
           (search-path (concatenate 'string *web-archive-base-uri-path* name-string "/uri?"))
           (version-path (concatenate 'string *web-archive-base-uri-path* name-string "/versions?"))
           (archive-path (concatenate 'string *web-archive-base-uri-path* name-string "/"))
           (overview-path (concatenate 'string archive-path "top.html")))
      (export-url (intern-url (merge-url search-path (local-context)))
                  :search
                  :response-function #'respond-to-archive-request
                  :keyword '(:cl-http :web-walker :archive)
                  :doumentation (format nil "Access web resource for web archive ~A." (or display-string name-string)))
      (export-url (intern-url (merge-url version-path (local-context)))
                  :search
                  :response-function #'respond-to-archive-request
                  :keyword '(:cl-http :web-walker :archive)
                  :doumentation (format nil "Access resource versions for archive ~A." (or display-string name-string)))
      (export-url (intern-url (merge-url overview-path (local-context)))
                  :computed
                  :response-function #'respond-to-archive-overview
                  :keyword '(:cl-http :web-walker :archive)
                  :doumentation (format nil "Overview of web archive ~A." (or display-string name-string)))
      (export-url (intern-url (merge-url archive-path (local-context)))
                  :redirect
                  :alternate-urls (intern-url (merge-url overview-path (local-context)))
                  :keywords '(:cl-http :web-walker :archive)))))

(defmacro define-web-archive (name &key display-string (archive-table '*web-archive-table*) directory)
  "define a new web archive."
  `(let* ((name-string (string-downcase (symbol-name ,name)))
          (object (make-instance 'web-archive
                    :name ,name
                    :display-string (or ,display-string name-string)
                    :resource-table (make-hash-table :test #'equalp)
                    :directory (or ,directory (merge-pathnames (concatenate 'string ";" name-string ";")
                                                               *web-archive-default-directory*))
                    :creation-date (get-universal-time))))
     (%initialize-archive object)
     (setf (gethash ,name ,archive-table) object)
     object))

(define-generic get-web-archive (name &optional errorp archive-table)
  (:documentation "Get an exact match for a web archive."))

(defmethod get-web-archive ((name string) &optional (errorp t) (archive-table *web-archive-table*))
  (get-web-archive (symbolize name :w4) errorp archive-table))

(defmethod get-web-archive ((name symbol) &optional (errorp t) (archive-table *web-archive-table*))
  (let ((match (gethash name archive-table)))
    (unless (or match (not errorp))
      (error (format nil "Web archive ~S not found." name)))
    match))

(defun get-web-archive-names (&optional (table *web-archive-table*) &aux result)
  "get a list of all web archive names"
  (maphash #'(lambda (name arch) (declare (ignore arch)) (push name result))
           table)
  result)

(define-generic get-archive-resource (archive resource &optional errorp archive-table)
  (:documentation "Get an exact match for a resource in a web archive"))

(defmethod get-archive-resource (archive (resource string) &optional (errorp t) (archive-table *web-archive-table*))
  (get-archive-resource (get-web-archive archive errorp archive-table) errorp archive-table))

(defmethod get-archive-resource ((archive web-archive) (resource string) &optional (errorp t) (archive-table *web-archive-table*))
  (declare (ignore archive-table))
  (let ((match (gethash resource (web-archive-resource-table archive))))
    (unless (or match (not errorp))
      (error (format nil "Web resource ~A not found in archive ~A." resource (web-archive-display-string archive))))
    match))

(define-macro with-archive-resource-content ((stream-var archive-var &key uri-string content-type version date) &body body)
  "open a version of a resource for archive ARCHIVE-VAR, accepting input over STREAM-VAR."
  `(let* ((now (get-universal-time))
          (uri-md5 (md5:md5-encode-string ,uri-string))
          (resource (gethash ,uri-string (web-archive-resource-table ,archive-var)))
          (version-pathname (merge-pathnames (concatenate 'string ":" (write-to-string ,version) ":" 
                                                          (write-to-string uri-md5) "." 
                                                          (string-downcase (symbol-name ,content-type)))
                                             (web-archive-directory ,archive-var))))
     (unless resource 
       (let ((object (make-instance 'archive-resource
                        :uri-string ,uri-string
                        :version-table (make-hash-table :test #'equalp)
                        :creation-date now)))
         (setf (gethash ,uri-string (web-archive-resource-table ,archive-var)) object)
         (setq resource object)))
     (setf (gethash ,version (archive-resource-version-table resource)) (cons version-pathname (or ,date now)))
     (setf (gethash :newest (archive-resource-version-table resource)) (cons version-pathname (or ,date now)))
     (with-open-file (,stream-var version-pathname
		      :direction :output
		      :element-type (cond 
                                     ((and ,content-type 
                                           (member (mime-content-type-copy-mode (mime-content-type-spec ,content-type)) '(:text :crlf)))
                                      http::*standard-character-type*)
                                     (t '(unsigned-byte 8)))
                      :if-does-not-exist :create
		      :if-exists http::*file-exists-supersede-action*)
       ,@body)))

(defmacro with-archive-resource-headers ((stream-var archive-var &key uri-string version) &body body)
  "write the headers for a resource to the filesystem."
  `(let ((header-pathname (merge-pathnames (concatenate 'string ":" (write-to-string ,version) ":" 
                                                        (write-to-string (md5:md5-encode-string ,uri-string)) ".headers")
                                           (web-archive-directory ,archive-var))))
     (with-open-file (,stream-var header-pathname
                                  :direction :output
                                  :element-type http::*standard-character-type*
                                  :if-does-not-exist :create
                                  :if-exists http::*file-exists-supersede-action*)
       ,@body)))

(define-generic save-web-archive (archive)
  (:documentation "Write the structure of an archive to the filesystem for later use."))

(defmethod save-web-archive ((archive symbol))
  (save-web-archive (get-web-archive archive t)))

(defmethod save-web-archive ((archive web-archive))
  (flet ((make-res (res)
           (with-slots (uri-string version-table creation-date) res
             `(let ((obj (make-instance 'archive-resource 
                           :uri-string ,uri-string
                           :creation-date ,creation-date)))
                (let ((vers-table (make-hash-table :test #'equalp)))
                  (setf ,@(loop for version being the hash-key of version-table using (hash-value pair)
                                collect `(gethash ,version vers-table)
                                collect `(cons (pathname ,(namestring (car pair))) ,(cdr pair)))
                        (slot-value obj 'version-table) vers-table))
                obj))))
    (with-slots (name display-string resource-table directory creation-date) archive
      (with-open-file (stream (merge-pathnames "archive.lisp" directory)
                              :if-does-not-exist :create
                              :if-exists :supersede
                              :direction :output)
        (write-line ";;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-" stream)
        (write-string ";;; Lisp code for restoring web archive "stream)
        (write-line display-string stream)
        (write-line "" stream)
        (write-line "(in-package :w4)" stream)
        (write-line "" stream)
        (write
         `(let ((object (make-instance ',(class-name (find-class 'web-archive))
                          :name ',name
                          :display-string ,display-string
                          :directory (pathname ,(namestring directory))
                          :creation-date ,creation-date)))
            (let ((res-table (make-hash-table :test #'equalp)))
              (setf ,@(loop for uri being the hash-key of resource-table using (hash-value resource)
                            collect `(gethash ,uri res-table)
                            collect (make-res resource))
                    (slot-value object 'resource-table) res-table))
            (setf (gethash ',name *web-archive-table*) object)
            (%initialize-archive object)
            object)
         :stream stream :escape t :pretty t :case :downcase :readably t))))
  archive)

(define-generic restore-web-archive (archive &optional pathname)
  (:documentation "Restore a web archive that has been saved to the file system.
the PATHNAME of the archive.lisp file must be supplied if the archive is not in the standard archive directory."))

(defmethod restore-web-archive ((archive symbol) &optional pathname)
  (unless pathname
    (setq pathname (merge-pathnames (concatenate 'string ";" (string-downcase (symbol-name archive)) ";" "archive.lisp")
                                     *web-archive-default-directory*)))
  (load pathname)
  (let ((match (get-web-archive archive nil)))
    (unless match
      (error (format nil "Archive ~S could not be restored from file ~A." archive pathname)))
    match))

(defun save-all-web-archives (&aux result)
  "save all the web archives in the archive table."
  (maphash #'(lambda (key val)
               (declare (ignore key))
               (push (save-web-archive val) result))
           *web-archive-table*)
  result)

(defun restore-all-web-archives (&optional (archive-directory (merge-pathnames "*.*" *web-archive-default-directory*)) &aux result)
  "search the toplevel archive directory for archives and attempt to restore them."
  (let ((dirs #+MCL(directory archive-directory :directories t)
	      #-MCL (directory archive-directory)))
    (loop for item in dirs
      for file = (merge-pathnames "archive.lisp" item)
          when (and (pathname-directory-p item) (probe-file file))
          do (let ((name (symbolize (string-upcase (car (last (pathname-directory file)))) :w4)))
               (push (restore-web-archive name file) result)))
    result))
  

;;;------------------------------------------------------------------- 
;;;
;;; HTML GENERATION
;;;

(defmethod display-resource-versions ((url url) (archive web-archive) (resource archive-resource) stream 
                                         &aux (versions (make-array 0 :fill-pointer t :adjustable t)))
  "display the versions of a resource."
  (with-slots (uri-string) resource
      (let ((title (format nil "Versions of ~A" uri-string)))
        (html:with-html-document (:stream stream)
          (html:with-document-preamble (:stream stream)
            (html:declare-base-reference url :stream stream)
            (html:declare-title title :stream stream))
          (html:with-standard-document-body (:stream stream)
            (html:with-section-heading (title :level 2 :stream stream)
              (write-string "Archive: " stream)
              (write-string (web-archive-display-string archive) stream)
              (html:with-paragraph (:stream stream)
                (maphash #'(lambda (vers pair)
                            (unless (eq :newest vers) (vector-push-extend (cons vers (cdr pair)) versions)))
                            (archive-resource-version-table resource)))
                (setq versions (stable-sort versions #'(lambda (x y) (< (car x) (car y)))))
                (html2:with-enumeration (stream :definition)
                  (map nil
                       #'(lambda (pair &aux (string (write-to-string (car pair))))
                           (html:enumerating-item (stream)
                             (html:note-anchor string 
                                               :reference (concatenate 'string "uri?" uri-string "+" string)
                                               :stream stream)
                             (write-string ", created " stream)
                             (http::print-gmt-time stream (cdr pair))))
                       versions)))
            (html:horizontal-line :stream stream)
            (html:note-anchor http::*server-version* :reference *cl-http-home-page-url-string* 
                              :stream stream))))))
        
(defmethod respond-to-archive-request ((url url:http-search) stream)
  "respond to a request from a web archive."
  (with-slots (url:search-keys url:path) url
    (flet ((decode-command-args (path)
             (let* ((len (length path))
                    (archive-name (when (> len 1) (symbolize (string-upcase (elt url:path (- len 2))) :w4)))
                    (archive (get-web-archive archive-name t))
                    (resource (get-archive-resource archive (car url:search-keys) t)))
               (values archive resource))))
      (let ((command (symbolize (car (last url:path)) *keyword-package*)))
        (cond ((eql :uri command)
               (multiple-value-bind (archive resource) (decode-command-args url:path)
                 (let* ((version (or (when (second url:search-keys) (parse-integer (second url:search-keys)))
                                     *web-archive-resource-version*))
                        (version-pathname (car (gethash version (archive-resource-version-table resource)))))
                   (unless (and version-pathname (probe-file version-pathname))
                     (error (format nil "Could not locate the resource ~A version ~D in archive ~S." 
                                    (car url:search-keys) version (web-archive-name archive))))
                   (let ((content-type (symbolize (pathname-type version-pathname) *keyword-package*)))
                     (with-open-file (file-stream version-pathname :direction :input)
                       (with-conditional-get-response
                         (stream content-type
                                 :location url
                                 :last-modification (file-stream-creation-date file-stream)
                                 :version (file-stream-version file-stream)
                                 :bytes (file-stream-length-in-bytes file-stream)
                                 :expires (expiration-universal-time url)
                                 :cache-control '(:public t))
                         (stream-copy-until-eof file-stream stream)))))))
              ((eql :versions command)
               (multiple-value-bind (archive resource) (decode-command-args url:path)
                 (with-successful-response (stream :html :expires (url:expiration-universal-time url)
                                                   :cache-control (url:response-cache-control-directives url)
                                                   :content-location url :content-language (languages url))
                   (display-resource-versions url archive resource stream))))
              (t (error "Unknown archive command.")))))))

(defmethod respond-to-archive-overview ((url url:http-computed-url) stream 
                                        &aux (uris (make-array 0 :fill-pointer t :adjustable t)))
  "displays information about a web archive."
  (with-slots (url:path) url
    (let* ((len (length url:path))
           (archive-name (when (> len 1) (symbolize (string-upcase (elt url:path (1- len))) :w4)))
           (archive (get-web-archive archive-name t)))
      (flet ((write-link (resource)
               (with-slots (uri-string version-table) resource
                 (html:with-table-row (:stream stream)
                   (html:with-table-cell (:stream stream)
                     (html:note-anchor (write-to-string (1- (hash-table-count version-table)))
                                       :reference (concatenate 'string "versions?" uri-string) :stream stream))
                   (html:with-table-cell (:stream stream)
                     (html:note-anchor uri-string :reference (concatenate 'string "uri?" uri-string) :stream stream)))
                   )))
        (declare (inline write-link))
        (with-successful-response (stream :html :expires (url:expiration-universal-time url)
                                          :cache-control (url:response-cache-control-directives url)
                                          :content-location url :content-language (languages url))
          (with-slots (name display-string resource-table directory creation-date) archive
            (let ((title (format nil "Archive Overview: ~A" display-string)))
              (html:with-html-document (:stream stream)
                (html:with-document-preamble (:stream stream)
                  (html:declare-base-reference url :stream stream)
                  (html:declare-title title :stream stream))
                (html:with-standard-document-body (:stream stream)
                  (html:with-section-heading (title :level 2 :stream stream)
                    (html:with-paragraph (:stream stream)
                      (html:with-rendition (:bold :stream stream)
                        (write-string "Archive Name: " stream))
                      (write-string (string-downcase (symbol-name name)) stream)
                      (html:break-line :stream stream)
                      (html:with-rendition (:bold :stream stream)
                        (write-string "Display String: " stream))
                      (write-string display-string stream)
                      (html:break-line :stream stream)
                      (html:with-rendition (:bold :stream stream)
                        (write-string "Creation Date: " stream))
                      (http::print-gmt-time stream creation-date)
                      (html:break-line :stream stream)
                      (html:with-rendition (:bold :stream stream)
                        (write-string "Archive Location: " stream))
                      (write-string (namestring directory) stream))
                    (html:with-paragraph (:stream stream)
                      (maphash #'(lambda (uri res) (declare (ignore uri)) (vector-push-extend res uris)) resource-table)
                      (setq uris (stable-sort uris #'(lambda (x y) (string< (archive-resource-uri-string x)
                                                                            (archive-resource-uri-string y)))))
                      (cond ((zerop (fill-pointer uris)) (write-string "No archived resources." stream))
                            (t (html:with-rendition (:bold :stream stream)
                                 (write-string "Archived Resources:" stream))
                               (html:with-table (:stream stream)
                                 (map nil #'write-link uris)))))
                    (html:with-paragraph (:stream stream)
                      (write-string "Back to " stream)
                      (html:note-anchor "web archives" :reference (concatenate 'string *web-archive-base-uri-path* "top.html")
                                        :stream stream)
                      (write-char #\. stream))
                    (html:horizontal-line :stream stream)
                    (html:note-anchor http::*server-version* :reference *cl-http-home-page-url-string* 
                                      :stream stream)))))))))))

(defmethod respond-to-show-archives ((url url:http-computed-url) stream
                                     &aux (archives (make-array 0 :fill-pointer t :adjustable t)))
  "displays all current archives."
  (flet ((write-link (arch)
           (with-slots (name display-string creation-date) arch
             (html:with-paragraph (:stream stream)
               (html:note-anchor display-string :reference (concatenate 'string (string-downcase (symbol-name name)) "/top.html") 
                                 :stream stream)
               (write-string ", created " stream)
               (http::print-gmt-time stream creation-date)))))
    (declare (inline write-link))
  (with-successful-response (stream :html :expires (url:expiration-universal-time url)
                                    :cache-control (url:response-cache-control-directives url)
                                    :content-location url :content-language (languages url))
      (let ((title (format nil "Web Archives on ~A:" (string-downcase (local-host-domain-name)))))
        (html:with-html-document (:stream stream)
          (html:with-document-preamble (:stream stream)
            (html:declare-base-reference url :stream stream)
            (html:declare-title title :stream stream))
          (html:with-standard-document-body (:stream stream)
            (html:with-section-heading (title :level 2 :stream stream)
              (html:with-paragraph (:stream stream)
                (write-string "Define a " stream)
                (html:note-anchor "new archive" 
                                  :reference (concatenate 'string *web-archive-base-uri-path* "define-archive.html")
                                  :stream stream)
                (write-string " on host " stream)
                (write-string (string-downcase (local-host-domain-name)) stream)
                (write-char #\. stream)
                (html:break-line :stream stream)
                (html:note-anchor "Add resources" 
                                  :reference (concatenate 'string *web-archive-base-uri-path* "build-archive.html")
                                  :stream stream)
                (write-string " to an existing archive." stream))
              (html:with-paragraph (:stream stream)
                (maphash #'(lambda (name arch) (declare (ignore name)) (vector-push-extend arch archives))
                         *web-archive-table*)
                (setq archives (stable-sort archives #'(lambda (x y) (string< (web-archive-display-string x) 
                                                                              (web-archive-display-string y)))))
                (map nil #'write-link archives))
              (html:horizontal-line :stream stream)
              (html:note-anchor http::*server-version* :reference *cl-http-home-page-url-string* 
                                :stream stream))))))))




;;;------------------------------------------------------------------- 
;;;
;;; CONTENT TRANSLATION UTILITIES
;;;

;;this could use work...
(define find-stream-match-with-copy (in out pattern &key (match :exact) (delimiter *close-element-delimiter*))
  (flet ((copy (in out)
           (write-char (read-char in nil nil nil) out)))
    (declare (inline copy))
    (loop for char = (peek-char nil in nil nil nil)
          with pred = (ecase match
                        (:exact 'char=)
                        (:case-insensitive 'char-equal))
          with chars = (coerce pattern 'list)
          with last = (1- (length chars))
          with now = 0
          until  (null char)
          ;
          when (char-equal char delimiter)
          do (copy in out)
          and return nil
          ;
          else when (funcall pred char (elt chars now))
          when (= now last)
          do (copy in out)
          and return t
          else
          do (incf now)
          and do (copy in out)
          end
          ;
          else
          do (setq now 0)
          and do (copy in out))))
       
(defmethod write-translated-html-content ((archive-name string) (remote-host string) (content string) stream)
  "parse html CONTENT string, translate uris and write the result to stream."
  (with-input-from-string (s content)
    (write-translated-html-content archive-name remote-host s stream)))

(defmethod write-translated-html-content ((archive-name string) (remote-host string) content stream)
  "parse html CONTENT stream, translate uris and write the result to stream."
  (let* ((buffsize 64)
         (buffer (make-array buffsize :element-type http::*standard-character-type* :adjustable t :fill-pointer t)))
    (labels ((dump-buffer (buffer stream)
               (write-string buffer stream)
               (setf (fill-pointer buffer) 0))
             (translate (uri &aux (string (name-string uri)))
               (cond ((local-url-p uri)
                      (let* ((len (length string))
                             (pos (search "//" string :start1 0 :end1 1 :start2 0 :end2 len))
                             (pos2 (position #\/ string :start (+ 2 pos) :end len)))
                        (concatenate 'string *web-archive-base-uri-path* archive-name "/uri?http://"
                                     remote-host (subseq string pos2 len))))
                     (t (concatenate 'string *web-archive-base-uri-path* archive-name "/uri?" string))))
             (parse-tag (in out &aux c tok)
               (clear-white-space in)
               (setq tok (read-token in *standard-delimiters*))
               (write-char *open-element-delimiter* out)
               (write-string (symbol-name tok) out)
               (cond ((member tok '(:a :img))
                      (when (find-stream-match-with-copy in out 
                                                         (case tok 
                                                           (:a "href") 
                                                           (:img "src") 
                                                           (t (error "no defined hyperlink attribute."))) 
                                                         :match :case-insensitive 
                                                         :delimiter *close-element-delimiter*)
                        (clear-white-space in)
                        (setq c (read-char in nil nil nil))
                        (unless (and c (char-equal c #\=))
                          (error "error translating html, no #\= found"))
                        (clear-white-space in)
                        (when (char-equal #\" (peek-char nil in nil nil nil))
                          (read-char in))
                        (clear-white-space in)
                        (write-string "= \"" out)
                        (unless (char-equal #\# (peek-char nil in nil nil nil))
			  (let* ((uri-string (get-url-string in))
                                 (uri (intern-url-referenced-url uri-string)))
                            (cond (uri (write-string (translate uri) out))
                                  (t (write-string uri-string out))))
                          (write-char #\" out))))
                     (t nil))))
      (declare (inline dump-buffer translate parse-tag))
      (loop for char = (read-char content nil nil t)
	    while char
	    when (>= (fill-pointer buffer) buffsize) do (dump-buffer buffer stream)
	    when (char= *open-element-delimiter* char) 
            do (dump-buffer buffer stream)
            and do (parse-tag content stream)
	    else do (vector-push-extend char buffer)
	    finally (dump-buffer buffer stream)))))
      
  
;;;------------------------------------------------------------------- 
;;;
;;; WEB WALKER DEFINITIONS
;;;

;;replace write-string?
(define-action-type
  archive-resource
  (:standard
   :documentation "An action that archives web resources.")
  (action activity url archive version archive-method &key (headers-p t) (content-p t))
  (declare (ignore action))
  (let* ((report-stream (report-stream activity))
         (archive-name (string-downcase (symbol-name (web-archive-name archive))))
         (headers (get-resource-headers activity url))
         (content (get-resource-content activity url))
         (content-type (second (get-header :content-type headers)))
         (date (get-header :date headers))
         (remote-host (host-string url)))
    (with-slots (name-string) url
      (when headers-p
        (cond (headers 
               (format report-stream "~&;Adding headers for ~A to archive ~A." name-string (web-archive-display-string archive))
               (with-archive-resource-headers (dump-stream archive 
                                                           :uri-string name-string 
                                                           :version version)
                 (http::write-headers dump-stream (http::header-plist headers))))
              (t (format report-stream "~&;No headers returned for ~A." name-string))))
      (cond (content
             (when content-p
               (format report-stream "~&;Adding content for ~A to archive ~A." name-string (web-archive-display-string archive))
               (with-archive-resource-content (dump-stream archive 
                                                           :uri-string name-string 
                                                           :content-type content-type 
                                                           :version version 
                                                           :date date)
                 (case content-type
                   (:html (case archive-method
                            (:translated (write-translated-html-content archive-name remote-host content dump-stream))
                            (t (write-string content dump-stream))))
                   (t (http::binary-stream-copy-from-8-bit-array content dump-stream 0 (length content)))))))
            (t (format report-stream "~&;No content returned for ~A." name-string)))
      (force-output report-stream))))
 

;;;------------------------------------------------------------------- 
;;;
;;; LISP INTERFACE
;;;

(define-generic archive-resources (archive root-url archive-method &key 
                                           headers-p
                                           content-p
                                           save-structure-p
                                           version
                                           depth
                                           operator
                                           hosts
                                           constraints
                                           minimize-dns-p
                                           report-stream
                                           respect-no-robots-p)
  (:documentation "Archives web resources in ARCHIVE, starting at ROOT-URL.
ARCHIVE-METHOD is :verbatim or :translated.  :translated remaps hyperlinks in HTML content.
When HEADERS-P is non-null a headers file is created for each resource.
When CONTENT-P is non-null a content file is created for each resource.
When SAVE-STRUCTURE-P is non-null, the archive structure is automatically saved to the filesystem.
VERSION can be specified to add to an existing walk.  otherwise, a new version of the archive is created.

Archive resources are accessible through the following web interface:
/web-archive/top.html                     display all archives currently loaded.
/web-archive/                             redirect -> /web-archive/top.html
/web-archive/my-archive/top.html          display information about archive my-archive.
/web-archive/my-archive/                  redirect -> /web-archive/my-archive/top.html
/web-archive/my-archive/uri?              display a version of a resource. version may be specified as a second 
                                             search argument, defaulting to *web-archive-resource-version*
/web-archive/my-archive/versions?         display all the versions of a resource"))

(defmethod archive-resources ((archive symbol) root-url (archive-method symbol) &key
                              (headers-p t)
                              (content-p t)
                              save-structure-p
                              (version (get-universal-time)) 
                              (depth 0)  
                              operator 
                              hosts
                              constraints 
                              minimize-dns-p
                              (report-stream *standard-output*) 
                              (respect-no-robots-p t))
  (archive-resources (get-web-archive archive t) root-url archive-method
                     :headers-p headers-p
                     :content-p content-p
                     :save-structure-p save-structure-p
                     :depth depth 
                     :version version
                     :operator operator
                     :hosts hosts
                     :constraints constraints
                     :minimize-dns-p minimize-dns-p
                     :report-stream report-stream
                     :respect-no-robots-p respect-no-robots-p))

;;fix report-stream!!
;;fix hosts
(defmethod archive-resources ((archive web-archive) root-url (archive-method symbol) &key
                              (headers-p t)
                              (content-p t)
                              save-structure-p
                              (version (get-universal-time)) 
                              (depth 0)  
                              operator 
                              hosts
                              constraints 
                              minimize-dns-p
                              (report-stream *standard-output*) 
                              (respect-no-robots-p t))

  "archive web resources in ARCHIVE. ARCHIVE-METHOD is :verbatim or :translated."
  (declare (ignore report-stream))
  (let ((host (host-string (intern-url root-url :if-does-not-exist :uninterned))))
    (unless operator
      (setq operator (concatenate 'string http::*server-maintainer* "@" (or http::*default-mailer-host* (local-host-domain-name)))))
    (with-activity
      ("archive-resources"
       (:url-host-name-resolution (if minimize-dns-p :never :preferred) :if-does-not-exist :uninterned :report-stream '*standard-output*)
       :constraints `((depth ,depth)
                      (no-cycles)
                      ,.(when respect-no-robots-p
                          (list '(header-robots-allowed)))
                      ,.(when hosts
                          `((url-referrer-host ,host)))
                      ,.(when constraints constraints))
       :actions `((archive-resource ,archive ,version ,archive-method :headers-p ,headers-p :content-p ,content-p)
		  (generate-inferiors)))
      (walk root-url activity))
    (when save-structure-p (save-web-archive (web-archive-name archive)))
    archive))


;;;------------------------------------------------------------------- 
;;;
;;; WEB INTERFACE FOR DEFINING AND BUILDING ARCHIVES
;;;


(defmethod compute-define-archive ((url url:http-form) stream)
  "define a new web archive."
  (flet ((accept (qid ptype prompt &optional def) 
           (html:with-rendition (:bold :stream stream)
             (w3p:accept ptype
                         :stream stream 
                         :view w3p:+html-view+ 
                         :present-p t
                         :default def
                         :prompt prompt 
                         :prompt-mode :raw
                         :display-default nil
                         :query-identifier qid
                         :insert-default t
                         :active-p t))))
    (declare (inline accept))
    (with-successful-response (stream :html :expires (url:expiration-universal-time url)
                                      :cache-control (url:response-cache-control-directives url)
                                      :content-location url :content-language (languages url))
      (let ((title (format nil "Define Archive on ~A" (string-downcase (local-host-domain-name)))))
        (html:with-html-document (:stream stream)
          (html:with-document-preamble (:stream stream)
            (html:declare-base-reference url :stream stream)
            (html:declare-title title :stream stream))
          (html:with-standard-document-body (:stream stream)
            (html:with-section-heading (title :level 2 :stream stream)
              (html:with-fillout-form (:post url :stream stream)
                (html:with-paragraph (:stream stream)
                  (accept "NAME" 'string "Symbol Name:" "new-archive")
                  (html:break-line :stream stream)
                  (write-string "This symbol name servers as a unique identifier for the archive." stream))
                (html:with-paragraph (:stream stream)
                  (accept "DISPLAY" '(w3p:null-or-type (w3p:bounded-string 50)) "Display String:")
                  (html:break-line :stream stream)
                  (write-string "Defaults to symbol name when None." stream))
                (html:with-paragraph (:stream stream)
                  (accept "DIRECTORY" '(w3p:null-or-type w3p:pathname) "File System Location:")
                  (html:break-line :stream stream)
                  (write-string "This pathname defaults to \"" stream)
                  (w3p:present (merge-pathnames (concatenate 'string ";symbol-name;") *web-archive-default-directory*)
                               'w3p:pathname
                               :view w3p:+textual-view+
                               :stream stream)
                  (write-char #\" stream))
                (html:with-paragraph (:stream stream)
                  (html:accept-input 'html:submit-button "SUBMIT" :display-string "Define Archive" :stream stream)))
              (html:with-paragraph (:stream stream)
                (write-string "Back to " stream)
                (html:note-anchor "web archives" :reference (concatenate 'string *web-archive-base-uri-path* "top.html")
                                  :stream stream)
                (write-char #\. stream))
              (html:horizontal-line :stream stream)
              (html:note-anchor http::*server-version* :reference *cl-http-home-page-url-string* 
                                :stream stream))))))))
					 
(defmethod respond-to-define-archive ((url url:http-form) stream query-alist)
  "define a new web archive."
  (flet ((accept (qid ptype)
           (handler-case 
             (w3p:accept-from-string ptype (second (assoc qid query-alist)))
             (w3p:input-not-of-required-type 
              () (error (format nil "Invalid data entered for form value ~S" qid))))))
  (let ((name (let ((*package* (find-package :w4))) (accept :name 'symbol)))
        (string (accept :display '(w3p:null-or-type w3p:basic-string)))
        (path (accept :directory '(w3p:null-or-type  w3p:pathname))))
    (define-web-archive name :display-string string :directory path)
    (with-successful-response (stream :html :expires (url:expiration-universal-time url)
                                      :cache-control (url:response-cache-control-directives url)
                                      :content-location url :content-language (languages url))
      (let ((title (format nil "Define Archive on ~A" (string-downcase (local-host-domain-name)))))
        (html:with-html-document (:stream stream)
          (html:with-document-preamble (:stream stream)
            (html:declare-base-reference url :stream stream)
            (html:declare-title title :stream stream))
          (ns1.1:with-document-body (:background :white :stream stream)
            (html:with-section-heading (title :level 2 :stream stream)
              (html:with-paragraph (:stream stream)
                (write-string "Web archive " stream)
                (write-string (web-archive-display-string (get-web-archive name t)) stream)
                (write-string " created." stream))
              (html:with-paragraph (:stream stream)
                (write-string "Back to " stream)
                (html:note-anchor "web archives" :reference (concatenate 'string *web-archive-base-uri-path* "top.html")
                                  :stream stream)
                (write-char #\. stream))
              (html:horizontal-line :stream stream)
              (html:note-anchor http::*server-version* :reference *cl-http-home-page-url-string* 
                                :stream stream)))))))))

(defmethod compute-build-archive ((url url:http-form) stream)
  (flet ((accept (qid ptype prompt &optional def) 
           (html:with-paragraph (:stream stream)
             (html:with-rendition (:bold :stream stream)
               (w3p:accept ptype
                           :stream stream 
                           :view w3p:+html-view+ 
                           :present-p t
                           :default def
                           :prompt prompt 
                           :prompt-mode :raw
                           :display-default nil
                           :query-identifier qid
                           :insert-default t
                           :active-p t)))))
    (declare (inline accept))
    (with-successful-response (stream :html :expires (url:expiration-universal-time url)
                                      :cache-control (url:response-cache-control-directives url)
                                      :content-location url :content-language (languages url))
      (let ((title (format nil "Build Archive on ~A" (string-downcase (local-host-domain-name)))))
        (html:with-html-document (:stream stream)
          (html:with-document-preamble (:stream stream)
            (html:declare-base-reference url :stream stream)
            (html:declare-title title :stream stream))
          (html:with-standard-document-body (:stream stream)
            (html:with-section-heading (title :level 2 :stream stream)
              (write-string "This form initiates a walk that adds to an existing archive." stream)
              (html:with-paragraph (:stream stream)
                (cond ((zerop (hash-table-count *web-archive-table*))
                       (write-string "There are no web archives defined." stream))
                      (t (html:with-fillout-form (:post url :stream stream)
                           (accept "ARCHIVE" `(w3p:member-sequence ,(get-web-archive-names)) "Web Archive:")
                           (accept "METHOD" '(w3p:member-sequence (:verbatim :translated)) "Archive Method:" :verbatim)
                           (accept "ROOT" 'w3p:bounded-string "URL [starting point]:")
                           (accept "DEPTH" '(w3p:integer 0 1000) "Depth [maximum steps explored]:" 3)
                           (accept "HOST-ONLY" 'w3p:boolean "Origin Host Only [explore no other servers]:" nil)
                           (accept "HOSTS" '(w3p:sequence w3p:bounded-string 50) "Hosts [limit walk to servers]:")
                           (accept "ROBOTS" 'w3p:boolean "Respect No Robots:" t)
                           (accept "DNS" 'w3p:boolean "Minimize DNS [non-canonical host names]:" t)
                           (accept "OPERATOR" '(w3p:bounded-string 50) "Operator:")
                           (html:with-rendition (:bold :stream stream)
                             (write-string "Constraints [specify a list of <a href=\"/cl-http/find-constraints?\">constraints</a>]:" stream))
                           (html:accept-input 'html:multi-line-text "CONSTRAINTS" :stream stream)
                           (html:with-paragraph (:stream stream)
                             (html:accept-input 'html:submit-button "SUBMIT" :display-string "Build Archive" :stream stream))))))
              (html:with-paragraph (:stream stream)
                (write-string "Back to " stream)
                (html:note-anchor "web archives" :reference (concatenate 'string *web-archive-base-uri-path* "top.html")
                                  :stream stream)
                (write-char #\. stream))
              (html:horizontal-line :stream stream)
              (html:note-anchor http::*server-version* :reference *cl-http-home-page-url-string* 
                                :stream stream))))))))
   
(defmethod respond-to-build-archive ((url url:http-form) stream query-alist)
  (flet ((accept (qid ptype)
           (handler-case 
             (w3p:accept-from-string ptype (second (assoc qid query-alist)))
             (w3p:input-not-of-required-type 
              () (error (format nil "Invalid data entered for form value ~S" qid))))))
    (let ((archive (let* ((*package* (find-package :w4)) 
                          (name (accept :archive 'symbol)))
                     (get-web-archive name t)))
          (method (accept :method '(w3p:member-sequence (:verbatim :translated))))
          (root (accept :root 'w3p:basic-string))
          (depth (accept :depth '(w3p:integer 0 1000)))
          (robots (accept :robots 'w3p:boolean))
          (operator (accept :operator 'w3p:basic-string))
          (dns (accept :dns 'w3p:boolean))
          (host-only (accept :host-only 'w3p:boolean))
          (host-list (accept :hosts '(w3p:null-or-type (w3p:sequence w3p:bounded-string 50))))
          (constraints (read-constraints-from-string (second (assoc :constraints query-alist)))))
      (when host-only
        (pushnew root host-list :test #'equalp))
      (with-successful-response (stream :html :expires (url:expiration-universal-time url)
                                        :cache-control (url:response-cache-control-directives url)
                                        :content-location url :content-language (languages url))
        (let ((title "Walk Results"))
          (html:with-html-document (:stream stream)
            (html:with-document-preamble (:stream stream)
              (html:declare-base-reference url :stream stream)
              (html:declare-title title :stream stream))
            (ns1.1:with-document-body (:background :white :stream stream)
              (html:with-section-heading (title :level 2 :stream stream)
                (force-output stream)
                (html:with-paragraph (:stream stream)
                  (let ((*standard-output* stream))   ;;fix this kludge
                    (html:with-verbatim-text (:stream stream)
                      (archive-resources archive root method
                                         :content-p t
                                         :headers-p t
                                         :save-structure-p t
                                         :depth depth
                                         :operator operator 
                                         :respect-no-robots-p robots
                                         :minimize-dns-p dns
                                         :hosts host-list
                                         :constraints constraints))))
                (html:with-paragraph (:stream stream)
                     (write-string "Back to " stream)
                     (html:note-anchor "web archives" :reference (concatenate 'string *web-archive-base-uri-path* "top.html")
                                       :stream stream)
                     (write-char #\. stream))
                (html:horizontal-line :stream stream)
                (html:note-anchor http::*server-version* :reference *cl-http-home-page-url-string* 
                                  :stream stream)))))))))


;;;------------------------------------------------------------------- 
;;;
;;; TEST CASES
;;;

#|

(clrhash *web-archive-table*)

;;attempt to reload any archives in the standard directory
(restore-all-web-archives)

(define-web-archive 'wilson 
  :display-string "Wilson August 1996" 
  :directory (pathname "Ford HD1:Desktop Folder:wilson-archive:"))

(archive-resources 'wilson "http://wilson.ai.mit.edu/cl-http/" :verbatim
                   :content-p t
                   :headers-p t
                   :save-structure-p t
                   :depth 1
                   :operator ""
                   :respect-no-robots-p nil)

(save-web-archive 'wilson)

;;overwrite the structure
(define-web-archive 'wilson 
  :display-string "Wilson August 1996" 
  :directory (pathname "Ford HD1:Desktop Folder:wilson-archive:"))

;;restore it
(restore-web-archive 'wilson (pathname "Ford HD1:Desktop Folder:wilson-archive:archive.lisp"))

(define-web-archive 'dole96 :display-string "Dole '96 Online Campaign")

(archive-resources 'dole96 "http://www.dole96.com/" :verbatim 
                   :headers-p t
                   :save-structure-p t
                   :depth 3
                    :operator ""
                   :respect-no-robots-p t)

(define-web-archive cg96 :display-string "Clinton/Gore '96")

|#

;;; end
