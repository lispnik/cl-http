;;;   -*- mode: lisp; package: http; base: 10; syntax: ansi-common-lisp; default-character-style: (:fix :roman :normal);-*-

;;;
;;; (c) Copyright  1995-97, John C. Mallery
;;;     All rights reserved.
;;;
;;;  EXPORTING MAIL ARCHIVES WITH CL-HTTP
;;;
;;;  In so far as it presents mail messages as a hypertext archive this
;;;  facility is much like Tom Gruber's Hypermail.   It is different
;;;  because it works only from the original mail file, and is therefore,
;;;  always up to date. No periodic batch processing is required to write
;;;  HTML files and copy them around.  HTML overviews are synthesized on  the
;;;  fly from in-core datastructures. Some precomputation is done in order to
;;;  minimize runtime consing.   Datastructures for mail archives and messages
;;;  make possible any variety of applications that can be layered  on top of
;;;  this storage model. Property list mixins for MAIL-ARCHIVE and MESSAGE
;;;  classes provide flexible storage slots for user applications.  These
;;;  classes may be specialized as well.
;;;    
;;;
;;; Issues:
;;;          1. Spot HTML message bodies & and render them as html, arranging
;;;             for bad html in one message not to corrupt the next messag
;;;          2. Handle MIME Multipart Messages intelligently
;;;          3. Cache the summary with the date to avoid recomputing.
;;;          4. More efficient, and incremental sorting of subject & author views.
;;;          5. Adaptive Page Layout: Break up views onto multiple pages automatically
;;;          6. Full-text search (constraint-based)
;;;          7. Slicker look and feel.
;;;          8. More and better views.
;;;          9. Integrate with POP or IMAP4 server.
;;;         10. Automatic undigestification of digest messages.
;;;         11. Handle more message content-types.
;;;         12. Allow user defined message display based on content type.
;;;         13. Handle various encoding formats.         

":MAIL-ARCHIVE exports a file containing an archive of email messages. 

It presents a summary of the mail file via URL. It also creates a search URL
by stripping any extension from URL and appending a question mark. The search
URL allows access to individual messages by number, ranges of messages by
number, and alternate summary views by view name. It automatically maintains
current conversation threads and presents views based on these. Color schemes
can be specified at export time for each mail archive.

The following keyword arguments are accepted:

        PATHNAME            -- (required) The pathname holding the mail archive.
        MAIL-FILE-FORMAT    -- (required) The format of the mail file. Choices are :LISPM, :EUDORA.

        MAILING-LIST        -- (optional) The mailing list for which this is an archive. When provided
                               buttons are generated for replying to messages. The port must implement
                               WWW-UTILS:SEND-MAIL-FROM (see: http:mcl;server;smtp.lisp) and SMTP
                               must be properly configured.

        DEFAULT-VIEW        -- (optional) The view shown as a default. Choices are: :AUTHOR, :BACKWARD, 
                               :CONVERSATION, :DATE, :FORWARD, :SUBJECT
        MESSAGE-FORMAT      -- (optional) The format in which messages are defaulted displayed. Choices are:
                               :CONTENT, :CONTENT-FULL-HEADERS, :CONTENT-FULL-HEADERS-AFTER
        DOCUMENTATION       -- (optional) A description that is printed as HTML in the section heading
                                of summary views.
        MESSAGE-HEADING     -- (optional) The string used to make message headings. Default is \"Message\".
        TITLE               -- (optional) The title to use for summary views. Default is the exported URL.

        REPLY               -- (optional) A keyword or hexadecimal RGB spec to use for coloring replies.
        BOX                 -- (optional) A keyword or hexadecimal RGB spec to use for the highlight box
                               background color, which defaults to :GREY-LIGHT-VERY.
	BACKGROUND-URL      -- (optional) An image URL to use as the background pattern.
	BACKGROUND          -- (optional) A keyword or hexadecimal RGB spec to use for the background color.
	FOREGROUND          -- (optional) A keyword or hexadecimal RGB spec to use for the foreground color.
	LINK                -- (optional) A keyword or hexadecimal RGB spec to use for the link color.
	VISITED-LINK        -- (optional) A keyword or hexadecimal RGB spec to use for the visited link color.
	ACTIVE-LINK         -- (optional) A keyword or hexadecimal RGB spec to use for the active link color.

        SEARCH-URL          -- (optional) A search URL to use. Default is one built from the URL.
        MAIL-ARCHIVE-CLASS  -- (optional) The class to use for mail archive instances.
        MESSAGE-CLASS       -- (optional) The class to use for message instances.
        THREAD-CLASS        -- (optional) The class to use for thread instances.
        THREAD-DEFAULT-VIEW -- (optional) The view shown as a default. Choices are: :FORWARD, :MESSAGES.
        RECACHE-P           -- (optional) When non-null, recaches the mail archive at export time. 
" 


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(in-package :http)

(defparameter *mail-archive-default-view* :backward
  "The default for DEFAULT-VIEW when exporting mail archives.")

(defparameter *mail-archive-thread-default-view* :messages
  "The default for THREAD-DEFAULT-VIEW when exporting mail archives.")

(defparameter *message-format* :content
  "The default message format used when exporting mail files.")

(defparameter *mail-archive-class* 'mail-archive
  "The default class for mail archive instances.")

(defparameter *message-class* 'message
  "The default class for message instances.")

(defparameter *thread-class* 'thread
  "The default class for thread instances.")

(defparameter *mail-archive-initialization-life-time* (* *server-life-time* 2)
  "The default maximum time allowed to update a mail archive in milliseconds.")

(defparameter *mail-archive-incremental-update-life-time* *server-life-time*
  "The default maximum time allowed to incremental a mail archive in milliseconds.")

;;;------------------------------------------------------------------- 
;;;
;;; CLASS DEFINITIONS
;;;

(defclass mail-archive-url
          (url:http-form)
    ((mail-archive :initform nil :initarg :mail-archive))
  (:documentation "A specialization of http-object to hold some data for mail archives."))

(defclass msg-threading-mixin
	  ()
    ((thread :initform nil :initarg :thread :accessor msg-thread)))

(defclass message
          (msg-threading-mixin property-list-mixin)
    ((mail-archive :initform nil :initarg :mail-archive :accessor msg-mail-archive)
     (time :initform nil :initarg :time :accessor msg-time)
     (date :initform nil :initarg :date :accessor msg-date)
     (from :initform nil :initarg :from :accessor msg-from)
     (subject :initform nil :initarg :subject :accessor msg-subject)
     (content-type :initform nil :initarg :content-type :accessor msg-content-type)
     (message-id :initform nil :initarg :message-id :accessor msg-message-id)
     (in-reply-to :initform nil :initarg :in-reply-to :accessor msg-in-reply-to)
     (references :initform nil :initarg :references :accessor msg-references)
     (heading :initform nil :initarg :heading :accessor msg-heading)
     (url-string :initform nil :initarg :url-string :accessor msg-url-string)
     (reply-url-string :initform nil :initarg :reply-url-string :accessor msg-reply-url-string)
     (index :initform nil :initarg :index :accessor msg-index)
     (header-position :initform nil :initarg :header-position :accessor msg-header-position)
     (body-position :initform nil :initarg :body-position :accessor msg-body-position)
     (end-position :initform nil :initarg :end-position :accessor msg-end-position)))

(defclass thread
	  (property-list-mixin)
    ((mail-archive :initarg :mail-archive :initform nil :reader thread-mail-archive)
     (index :initform nil :initarg :index :accessor thread-index)
     (url-string :initform nil :initarg :url-string :accessor thread-url-string)
     (heading :initform nil :initarg :heading :accessor thread-heading)
     (unresolved-message-ids :initform nil :reader thread-unresolved-message-ids)
     (message-ids :initform nil :reader thread-message-ids)
     (messages :initform nil :accessor %thread-messages)))

(defclass ma-threading-mixin ()
    ((thread-class :initform 'thread :initarg :thread-class :accessor ma-thread-class)
     (thread-table :initform (make-hash-table :test #'equalp) :initarg :thread-table :accessor ma-thread-table)
     (thread-number :initform 0 :initarg thread-number :accessor ma-thread-number)
     (thread-heading :initform "Conversation" :initarg :thread-heading :accessor ma-thread-heading)
     (thread-default-view :initform :forward :initarg :thread-default-view :accessor ma-thread-default-view)
     (reverse-threads :initform nil :initarg reverse-threads :accessor ma-reverse-threads))
  (:documentation "A mixin that provides threading for mail archives."))

(defclass ma-server-life-time-mixin
	  ()
    ((initialization-life-time :initform *mail-archive-initialization-life-time*
			       :initarg :initialization-life-time :accessor ma-initialization-life-time)
     (incremental-initialization-life-time :initform *mail-archive-incremental-update-life-time*
					   :initarg :incremental-initialization-life-time
				   :accessor ma-incremental-initialization-life-time))
  (:documentation "Requests appropriate resources for update operations."))

(defclass mail-archive
          (ma-threading-mixin ma-server-life-time-mixin property-list-mixin)
    ((url :initform nil :initarg :url :accessor ma-url)
     (search-url :initform nil :initarg :search-url :accessor ma-search-url)
     (pathname :initform nil :initarg :pathname :accessor ma-pathname)
     (length-in-bytes :initform nil :initarg :length-in-bytes :accessor ma-length-in-bytes)
     (size :initform nil :initarg :size :accessor ma-size)
     (lock :initform nil :initarg :lock :accessor ma-lock)
     (cache-time :initform nil :initarg :cache-time :accessor ma-cache-time)
     (prefixed-messages-p :initform nil :initarg :prefixed-messages-p :accessor ma-prefixed-messages-p)
     (message-delimiter :initform nil :initarg :message-delimiter :accessor ma-message-delimiter)
     (message-class :initform 'message :initarg :message-class :accessor ma-message-class)
     (start-time :initform nil :initarg :start-time :accessor ma-start-time)
     (end-time :initform nil :initarg :end-time :accessor ma-end-time)
     (default-view :initform :conversation :initarg :default-view :accessor ma-default-view)
     (message-format :initform :content :initarg :message-format :accessor ma-message-format)
     (mailing-list :initform nil :initarg :mailing-list :accessor ma-mailing-list)
     (views :initarg :views :accessor ma-views)
     (color-scheme :initform nil :initarg color-scheme :accessor %ma-color-scheme)
     (message-table :initform (make-hash-table :test #'equalp) :initarg :message-table :accessor ma-message-table)
     (message-heading :initform "Message" :initarg :message-heading :accessor ma-message-heading)
     (messages :initform nil :initarg :messages :accessor ma-messages)
     (reverse-messages :initform nil :initarg reverse-messages :accessor ma-reverse-messages)
     (author-sorted-messages :initform nil :initarg :author-sorted-messages :accessor ma-author-sorted-messages)
     (subject-sorted-messages :initform nil :initarg :subject-sorted-messages :accessor ma-subject-sorted-messages)))

(defmethod print-object ((message message) stream)
  (with-slots (index) message
    (print-unreadable-object (message stream :type t :identity t)
      (when index
        (write index :stream stream :base 10)))
    message))

(defmethod print-object ((mail-archive mail-archive) stream)
  (with-slots (message-table url) mail-archive
    (print-unreadable-object (mail-archive stream :type t :identity t)
      (when (and url message-table)
        (write (hash-table-count message-table) :stream stream :base 10)
        (write-string ": " stream)
        (write-string (name-string url) stream)))
    mail-archive))

(defmethod print-object ((thread thread) stream)
  (print-unreadable-object (thread stream :type t :identity t)
    (let ((msgs (thread-messages thread)))
      (when msgs
	(format stream "{~{~D~^, ~}}" (mapcar #'msg-index msgs)))))
  thread)

;;;------------------------------------------------------------------- 
;;;
;;; PLATFORM COMPATIBILITY CODE
;;; 

#+Genera
(defmacro handling-character-style-mapping-exception (&body body)
  `(flet ((automatically-resume (error)		;automatically proceed from character style mapping errors
	    (let ((proceed-types (dbg:proceed-types error)))	;these occur when Lispm-styled text appears
	      (when (member :use-type-zero proceed-types)	;in a mail message.
		(sys:proceed error :use-type-zero)))))
     (handler-bind ((si:escape-loading-stream-decoding-error #'automatically-resume))
       ,@body)))

#+Genera
(defmacro with-direct-access-open-file ((file-stream pathname &key (direction :input)) &body body) 
  `(scl:with-open-file (,file-stream ,pathname :direction ,direction :direct t :element-type 'scl:string-char)
     (handling-character-style-mapping-exception ,@body)))

#-Genera
(defmacro with-direct-access-open-file ((file-stream pathname &key (direction :input)) &body body) 
  `(with-open-file (,file-stream ,pathname :direction ,direction :element-type *standard-character-type*)
     ,@body))

#+Genera
(defmacro with-standard-access-open-file ((file-stream pathname &key (direction :input)) &body body) 
  `(with-open-file (,file-stream ,pathname :direction ,direction :element-type *standard-character-type*)
     (handling-character-style-mapping-exception ,@body)))

#-Genera
(defmacro with-standard-access-open-file ((file-stream pathname &key (direction :input)) &body body) 
  `(with-open-file (,file-stream ,pathname :direction ,direction :element-type *standard-character-type*)
     ,@body))

(declaim (inline set-up-input-buffer)) 

#+Genera
(defun set-up-input-buffer (stream start end)
  (scl:send stream :read-bytes (- end start) start))

#-Genera
(defun set-up-input-buffer (stream start end)
  (declare (ignore end))
  (file-position stream start))

#+(or Genera MCL LispWorks)
(defun get-eql-specializer-object (specializer)
  (when (and (consp specializer) (eq (first specializer) 'eql))
    (second specializer)))

#+Allegro
(defun get-eql-specializer-object (specializer)
  (typecase specializer
    (clos:eql-specializer (clos:eql-specializer-object specializer))
    (t nil)))

#+CMU
(defun get-eql-specializer-object (specializer)
  (and (pcl::eql-specializer-p specializer)
       (pcl::eql-specializer-object specializer)))

;;;------------------------------------------------------------------- 
;;;
;;; MESSAGE DELIMITER MANAGEMENT 
;;; 

(defmacro define-message-delimiter (name (&key prefix-delimitation-p documentation) &body body)
  "Defines message delimitation named, NAME.
PREFIX-DELIMITATION-P indicates whether delimiters appear before or after messages.
BODY is lisp code that returns non-null for delimiter lines.
BODY can access the lexical variables LINE and LENGTH."
  #+Genera (declare (zwei:indentation 0 2 1 4 2 2))
  (let ((keyword (symbolize (string name) *keyword-package*))
	(predicate-name (symbolize (format nil "~A-~:[post~;pre~]-message-delimiter-p" (string name) prefix-delimitation-p))))
    `(progn
       (defun ,predicate-name (line &optional (length (length line)))
	 ,@(when documentation (list documentation))
	 #+CMU(declare (type base-string line) (type (mod 1024) length))
	 #+Genera(declare (sys:function-parent ,predicate-name define-message-delimiter))
	 ,@body)
       (set-mail-file-format-delimiter ,keyword ',predicate-name ,prefix-delimitation-p)
       ',predicate-name)))

(eval-when (compile eval load)
  
(defvar *mail-format-delimiters* nil)

(defun set-mail-file-format-delimiter (format predicate prefix-delimitation-p)
  (let ((entry (assoc format *mail-format-delimiters*)))
    (cond (entry
	   (setf (second entry) predicate
		 (third entry) prefix-delimitation-p)
	   entry)
	  (t (push `(,format ,predicate ,prefix-delimitation-p) *mail-format-delimiters*))))))

(defun get-mail-file-format-delimiters (format)
  (declare (values message-delimiter prefixed-delimitation-p))
  (let ((entry (assoc format *mail-format-delimiters*)))
    (cond (entry (values-list (cdr entry)))
          (t (error "Unknown mail file format, ~s." format)))))

(declaim (inline clear-prefix-message-delimiter))

(defun clear-prefix-message-delimiter (stream)
  (read-delimited-line stream '(#\linefeed #\return) nil *server-line-buffer*))


;;;------------------------------------------------------------------- 
;;;
;;; PARAMETERS
;;;

(define-message-delimiter lispm
     (:documentation "Delimits messages in archives written by the Symbolics Mailer.")
   (and (not (zerop length))
	(eql (aref line 0) #\)))

(define-message-delimiter eudora
     (:prefix-delimitation-p t
      :documentation "Delimits messages in mail files written by the Eudora Mail Reader.")
   (and (> length 11)
	(string-equal "From ???@???" line :start1 0 :end1 12 :start2 0 :end2 12)))

;; provided by Jong-won Choi
(define-message-delimiter emacs-vm-digest
     (:documentation "Delimits messages in digest files written by ???.")
   (and (= length 30)
	(string-equal "------------------------------" line :start1 0 :end1 29 :start2 0 :end2 29)))

;; provided by Paul Werkowski
(define-message-delimiter mh-packf
     (:documentation "Delimits messages in digest files written by MH.")
   (and (= length 4)
	(string-equal #.(make-string 4 :initial-element (code-char 1)) line)))

;; Provided by Olivier Clarisse
#+Ignore
(define-message-delimiter rmail
     (:prefix-delimitation-p t
      :documentation "Delimits messages in mail files written in Rmail format.")
   (and (> length 6)
	(string-equal "From " line :start1 0 :end1 5 :start2 0 :end2 5)
	(or (eql (elt line 5) #\-)
	    ;; Anybody with CLIM-SYS enabled should have this function defined
	    ;; in the new release (first file loaded from
	    ;; http:clim;clim-sys;sysdcl !
	    #+CLIM-SYS
	    (eql (cl-user::search-wild "From *@*" line) 0))))

#+Ignore
(define-message-delimiter rmail
     (:prefix-delimitation-p t
      :documentation "Delimits messages in mail files written in Rmail format.")
   (macrolet ((when-let (bindings &body body)
		(loop for binding in bindings
		      for (var) = binding
		      collect var into vars
		      collect `(setq . ,binding) into clauses
		      finally (return `(let ,vars (when (and . ,clauses) . ,body))))))
     (flet ((match-month (line start)
	      (when (>= length (+ start 4))
		(dolist (month '("Jan " "Feb " "Mar " "Apr " "May " "Jun " "Jul " "Aug " "Sep " "Oct " "Nov " "Dec "))
		  (when (string= month line :start1 0 :end1 4 :start2 start :end2 (+ start 4))
		    (return (+ start 4))))))
	    (match-day-num (line start)
	      (when (and (>= length (+ start 3))
			 (or (char= (aref line start) #\Space)
			     (digit-char-p (aref line start)))
			 (digit-char-p (aref line (1+ start)))
			 (char= (aref line (+ start 2)) #\Space))
		(+ start 3)))
	    (match-time (line start)
	      (do ((pos start (1+ pos)))
		  ((>= pos length) nil)
		(let ((char (aref line pos)))
		  (cond ((char= char #\Space)
			 (return (when (> pos start) (1+ pos))))
			((char= char #\:))
			((digit-char-p char))
			(t (return nil))))))
	    (match-zone (line start)
	      (when (>= length (+ start 1))
		(when-let ((pos (position #\Space line :start start)))
			  (1+ pos))))
	    (match-year (line start)
	      (and (>= length (+ start 4))
		   (digit-char-p (aref line start))
		   (digit-char-p (aref line (1+ start)))
		   (digit-char-p (aref line (+ start 2)))
		   (digit-char-p (aref line (+ start 3)))
		   (or (= length (+ start 4))
		       (char= (aref line (+ start 4)) #\Space)))))
       (declare (inline match-month match-day-num match-time match-zone match-year))
       (and (> length #.(+ 6 4 4 4))
	    (string= "From " line :start1 0 :end1 5 :start2 0 :end2 5)
	    ;; Time
	    (dolist (day '(" Mon " " Tue " " Wed " " Thu " " Fri " " Sat " " Sun ") nil)
	      (when (do ((day-start 5 (1+ day-start)))
			((> (+ day-start 5) length) nil)
		      (when-let ((day-start (search day line :start2 day-start)))
				(when-let ((day-num (match-month line (+ day-start 5)))
					   (time (match-day-num line day-num))
					   (zone (match-time line time)))
					  (and (or (when-let ((year (match-zone line zone)))
							     (match-year line year))
						   (match-year line zone))
					       (return day-start))))
		      (return nil))
		(return t)))))))

;;; RMail format; matches the "From " prefix and the time.
;;; Provided by Douglas Crosher.
(define-message-delimiter rmail
     (:prefix-delimitation-p t
      :documentation "Delimits messages in mail files written in Rmail format.")
   (flet ((match-month (start)
	    (declare (type (mod 1024) start))
	    (when (>= length (+ start 4))
	      (dolist (month '("Jan " "Feb " "Mar " "Apr " "May " "Jun "
			       "Jul " "Aug " "Sep " "Oct " "Nov " "Dec "))
		(declare (type simple-base-string month))
		(when (string= month line :start1 0 :end1 4
			       :start2 start :end2 (+ start 4))
		  (return (+ start 4))))))
	  (match-day-num (start)
	    (declare (type (mod 1024) start))
	    (when (and (>= length (+ start 3))
		       (or (char= (aref line start) #\Space)
			   (digit-char-p (aref line start)))
		       (digit-char-p (aref line (1+ start)))
		       (char= (aref line (+ start 2)) #\Space))
	      (+ start 3)))
	  (match-time (start)
	    (declare (type (mod 1024) start))
	    (do ((pos start (1+ pos)))
		((>= pos length) nil)
	      (declare (type (mod 1024) pos))
	      (let ((char (aref line pos)))
		(cond ((char= char #\Space)
		       (return (when (> pos start) (1+ pos))))
		      ((char= char #\:))
		      ((digit-char-p char))
		      (t
		       (return nil))))))
	  (match-zone (start)
	    (declare (type (mod 1024) start))
	    (when (>= length (+ start 1))
	      (let ((pos (position #\Space line :start start)))
		(when pos (1+ pos)))))
	  (match-year (start)
	    (declare (type (mod 1024) start))
	    (and (>= length (+ start 4))
		 (digit-char-p (aref line start))
		 (digit-char-p (aref line (1+ start)))
		 (digit-char-p (aref line (+ start 2)))
		 (digit-char-p (aref line (+ start 3)))
		 (or (= length (+ start 4))
		     (char= (aref line (+ start 4)) #\Space)))))
     (declare (inline match-month match-day-num match-time match-zone match-year))
     (and (> length #.(+ 6 4 4 4))
	  (string= "From " line :start1 0 :end1 5 :start2 0 :end2 5)
	  ;; Time
	  (dolist (day '(" Mon " " Tue " " Wed " " Thu " " Fri " " Sat " " Sun ") nil)
	    (declare (type simple-base-string day))
	    (when (do ((day-start 5 (1+ day-start)))
		      ((> (+ day-start 5) length) nil)
		    (declare (type (or null (mod 1024)) day-start))
		    (setq day-start (search day line :start2 day-start))
		    (if day-start
			(when (let ((day-num (match-month (+ day-start 5))))
				(declare (type (or null (mod 1024)) day-num))
				(when day-num
				  (let ((time (match-day-num day-num)))
				    (declare (type (or null (mod 1024)) time))
				    (when time
				      (let ((zone (match-time time)))
					(declare (type (or null (mod 1024)) zone))
					(when zone
					  (or (let ((year (match-zone zone)))
						(declare (type (or null (mod 1024)) year))
						(when year
						  (match-year year)))
					      (match-year zone))))))))
			  (return day-start))
			(return nil)))
	      (return t))))))

;; This should be set to the standard format on the platform
(defparameter *mail-file-format*
	      #+Genera :lispm
	      #+MCL :eudora
	      #+(or Franz-Inc CMU) :rmail
              #-(or Genera MCL CMU Franz-Inc) nil 
              "The default format used when exporting mail files.")


;;;------------------------------------------------------------------- 
;;;
;;; COLOR SCHEMES
;;;

(defgeneric %ma-color-scheme (mail-archive-or-url))

(defmethod %ma-color-scheme ((url mail-archive-url))
  (get-value url :color-scheme))

(defgeneric ma-color-scheme (mail-archive-or-url)
  (declare (values background-url background foreground link visited-link active-link reply)))

(defmethod ma-color-scheme (mail-archive-or-url)
  (values-list (%ma-color-scheme mail-archive-or-url)))

(defgeneric ma-set-document-color-scheme (mail-archive-url &key background-url background foreground reply box
							   link visited-link active-link))

(defmethod ma-set-document-color-scheme ((url mail-archive-url) &key background-url background foreground
					 reply box link visited-link active-link )
  (with-slots (mail-archive) url
    (macrolet ((default-color (color)
		 `(when *standard-color-scheme*
		    (,(symbolize (concatenate 'string "COLOR-SCHEME-" (string color)) :html) *standard-color-scheme*)))
	       (get-hex-color (color)
		 `(and ,color
		       (or (color-mapping ,color nil)
			   (error "The ~A color could not be mapped into a hexadecimal color." ',color ,color)))))
      (if (or background-url background foreground link visited-link active-link *standard-color-scheme*)
	  (setf (get-value url :color-scheme)
		`(,(etypecase background-url
		     (null (default-color background-url))
		     (string background-url)
		     (url (name-string background-url)))
		  ,(or (get-hex-color background) (default-color background))
		  ,(or (get-hex-color foreground) (default-color foreground))
		  ,(or (get-hex-color link) (default-color link))
		  ,(or (get-hex-color visited-link) (default-color link))
		  ,(or (get-hex-color active-link) (default-color link))
		  ,(get-hex-color reply)
		  ,(get-hex-color box)))
	  (remove-value url :color-scheme))
      ;; initialize the mail archive in case of reexport.
      (when mail-archive
	(initialize-mail-archive-color-scheme mail-archive)))))

(defgeneric initialize-mail-archive-color-scheme (mail-archive))

(defmethod initialize-mail-archive-color-scheme ((mail-archive mail-archive))
  (with-slots (url) mail-archive
    (setf (%ma-color-scheme mail-archive) (%ma-color-scheme url))
    (ma-highlight-color mail-archive t)))

(defmacro with-mail-archive-document-body ((mail-archive stream) &body body)
  `(multiple-value-bind (background-url background foreground link visited-link active-link reply-color)
       (ma-color-scheme ,mail-archive)
     (declare (ignore reply-color))
     (with-document-body
       (:background-url background-url :background background :foreground foreground
			:link link :visited-link visited-link :active-link active-link
			:stream ,stream)
       ,@body)))

(defgeneric ma-background-url (mail-archive-or-url))

(defmethod ma-background-url (mail-archive-or-url)
  (first (%ma-color-scheme mail-archive-or-url)))

(defgeneric ma-background-color (mail-archive-or-url))

(defmethod ma-background-color (mail-archive-or-url)
  (second (%ma-color-scheme mail-archive-or-url)))

(defgeneric ma-foreground-color (mail-archive-or-url))

(defmethod ma-foreground-color (mail-archive-or-url)
  (third (%ma-color-scheme mail-archive-or-url)))

(defgeneric ma-link-color (mail-archive-or-url))

(defmethod ma-link-color (mail-archive-or-url)
  (fourth (%ma-color-scheme mail-archive-or-url)))

(defgeneric ma-visited-link-color (mail-archive-or-url))

(defmethod ma-visited-link-color (mail-archive-or-url)
  (fifth (%ma-color-scheme mail-archive-or-url)))

(defgeneric ma-active-link-color (mail-archive-or-url))

(defmethod ma-active-link-color (mail-archive-or-url)
  (sixth (%ma-color-scheme mail-archive-or-url)))

(defgeneric ma-reply-color (mail-archive-or-url))

(defmethod ma-reply-color (mail-archive-or-url)
  (seventh (%ma-color-scheme mail-archive-or-url)))

(defgeneric ma-box-color (mail-archive-or-url))

(defmethod ma-box-color (mail-archive-or-url)
  (eighth (%ma-color-scheme mail-archive-or-url)))

(defgeneric ma-highlight-color (mail-archive-or-url &optional recompute-p))

(defmethod ma-highlight-color (mail-archive-or-url &optional recompute-p)
  (with-value-cached (mail-archive-or-url :highlight-color :recompute-p recompute-p)
    (inverse-color (or (ma-background-color mail-archive-or-url)
			     :white))))

(defconstant +hyperarchive-url-string+ "http://wilson.ai.mit.edu/cl-http/features.html#hyperarchive")

(defgeneric ma-write-signature (mail-archive stream))

(defmethod ma-write-signature ((mail-archive mail-archive) stream)
  (with-rendition (:italic :stream stream)
    (with-rendition (:bold :stream stream)
      (note-anchor "HyperArchive" :reference +hyperarchive-url-string+ :stream stream))
    (fast-format stream " powered by ")
    (note-anchor *server-version* :reference *cl-http-home-page-url-string* :stream stream)))

;;;------------------------------------------------------------------- 
;;;
;;; MESSAGE OPERATIONS 
;;;

(defconstant +message-no-subject-string+ "(No Subject)")

(defun useless-subject-p (string)
  (or (null string)
      (equalp string +message-no-subject-string+)))

(defun message-id-header-p (line &optional (start 0) (end (length line)))
  (declare (values crossreference-header-p first-header-line-p))
  (macrolet ((first-line-p (string)
               `(not (and ,string (white-space-char-p (aref ,string 0)))))
             (header-p (header line start end)
               (let ((len (length header)))
                 `(let ((e (+ ,start ,len)))
                    (and (< e ,end)
                         (string-equal ,header ,line :start1 ,start :end1 e :start2 ,start :end2 e))))))
    (if (first-line-p line)
        (values (or (header-p "In-Reply-To:" line start end)
                    (header-p "References:" line start end))
                t)
        (values nil nil))))

(defgeneric write-message-id-hyperlinks (archive string stream &optional start end))

(defmethod write-message-id-hyperlinks ((archive mail-archive) string stream &optional (start 0) (end (length string)))
  (flet ((message-id-position (string start end)
           (let* ((s (position #\< string :start start :end end :test #'eql))
                  (e (and s (position #\> string :start (1+ s) :end end :test #'eql :from-end t))))
             (values s e))))
    (loop with pos = start
          while (< pos end)
          do (multiple-value-bind (s e)
                 (message-id-position string pos end)
               (cond ((and s e)
                      (let* ((xref (subseq string s (1+ e)))
                             (xref-msg (ma-get-message archive xref nil)))
                        (declare (dynamic-extent xref))
                        (cond (xref-msg
                               (with-anchor-noted (:reference (msg-url-string xref-msg) :stream stream)
                                 (write-string-quoting-specials string stream s (1+ e)))
                               (setq pos (1+ e)))
                              (t (write-string-quoting-specials string stream s (1+ e))
                                 (setq pos (1+ e))))))
                     (t (let ((next (or (and s (position #\< string :start (1+ s) :end end :test #'eql))
                                        end)))
                          (write-string-quoting-specials string stream pos next)
                          (setq pos next))))))))

(defun msg-write-full-verbatim-headers (archive stream file-stream start end)
  (with-verbatim-text (:stream stream)
    (loop initially (set-up-input-buffer file-stream start end)
          with buffer = *server-line-buffer*
          with hyperlink-msg-id
          do (multiple-value-bind (line eof-p delim length)
                 (read-delimited-line file-stream '(#\return #\linefeed) nil buffer)
               (declare (ignore delim))
               (unless (zerop length)
                 (multiple-value-bind (cross-reference-p header-first-line-p)
                     (message-id-header-p line 0 length)
                   (when header-first-line-p
                     (setq hyperlink-msg-id cross-reference-p)))
                 (cond (hyperlink-msg-id
                        (let ((e (position #\< line :start 0 :end length :test #'eql)))
                          (write-string-quoting-specials line stream 0 e)
			  (when e
			    (write-message-id-hyperlinks archive line stream e length))))
                       (t (write-string-quoting-specials line stream 0 length)))
                 (write-char #\Newline stream)
                 (when eof-p (return))))
          while (< (file-position file-stream) end))))

(declaim (inline reply-line-p))

(defun reply-line-p (line &optional (end (length line)))
  (declare (values index-or-null))
  (with-fast-array-references ((line line string))
    (loop for idx upfrom 0 below end
	  do (case (aref line idx)
	       ((#\> #\|) (return idx))
	       ((#\space #\tab))
	       (t (return nil)))
	  finally (return nil))))

(defun msg-write-body-as-verbatim-html (stream file-stream start end reply-color)
  ;; Use internal primitives to generate tag environments on a
  ;; region basis rather than less efficient line by line basis.
  (flet ((open-reply-environment (stream reply-color)
	   (html::issue-command "BLOCKQUOTE" stream nil nil)
	   (when reply-color
	     (html::%issue-command ("FONT" stream :fresh-line nil)
	       (ns3.0::%write-make-font-args stream nil nil reply-color)))
	   (html::issue-command "I" stream nil nil))
	 (close-reply-environment (stream reply-color)
	   (html::issue-command "I" stream nil t)
	   (when reply-color
	     (html::issue-command "FONT" stream nil t))
	   (html::issue-command "BLOCKQUOTE" stream nil t)))
    (declare (inline open-reply-environment close-reply-environment))
    (with-verbatim-text (:stream stream)
      (loop initially (set-up-input-buffer file-stream start end)
	    with buffer = *server-line-buffer*
	    with reply-line-p
	    do (multiple-value-bind (line eof-p delim length)
		   (read-delimited-line file-stream '(#\return #\linefeed) nil buffer)
		 (declare (ignore delim))
		 (unless (zerop length)
		   (let ((r-idx (reply-line-p line length)))
		     (cond (r-idx
			    (unless reply-line-p
			      (open-reply-environment stream reply-color)
			      (setq reply-line-p t)))
			   (reply-line-p
			    (close-reply-environment stream reply-color)
			    (setq reply-line-p nil)))
		     ;; write the line
		     (if r-idx
			 (write-string-anchoring-urls line stream (1+ (the fixnum r-idx)) length)
			 (write-string-anchoring-urls line stream 0 length))))
		 (write-char #\Newline stream)
		 (when eof-p (return)))
	    while (< (file-position file-stream) end)
	    finally (when reply-line-p
		      (close-reply-environment stream reply-color))))))

(defun msg-write-body-as-reply-text (stream file-stream start end)
  (loop initially (set-up-input-buffer file-stream start end)
	with buffer = *server-line-buffer*
	do (multiple-value-bind (line eof-p delim length)
	       (read-delimited-line file-stream '(#\return #\linefeed) nil buffer)
	     (declare (ignore delim))
	     (write-char #\> stream)
	     (unless (zerop length)
	       (write-string line stream))
	     (write-char #\Newline stream)
	     (when eof-p (return)))
	while (< (file-position file-stream) end)))

(defgeneric msg-before-p (message ref-time))

(defmethod msg-before-p ((message message) ref-time)
  (with-slots (time) message
    (< (or time -1) ref-time)))

(defgeneric msg-after-p (message ref-time))

(defmethod msg-after-p ((message message) ref-time)
  (with-slots (time) message
    (< ref-time (or time -1))))

(defgeneric msg-write-headers (message stream))

(defmethod msg-write-headers ((message message) stream)
  (flet ((%write-a-header (stream header raw-value &optional break-line-p quote-p)
           (when raw-value
             (when break-line-p (break-line :stream stream))
             (write-string (symbol-name (or (%header-print-name header) header)) stream)
             (write-string ": " stream)
	     (if quote-p
		 (write-string-quoting-specials raw-value stream)
		 (write-string raw-value stream)))))
    (with-slots (date from subject) message
      (%write-a-header stream :date date)
      (%write-a-header stream :from from date)
      (%write-a-header stream :subject subject t t))))

(defgeneric msg-write-xref-headers (message stream))

(defmethod msg-write-xref-headers ((message message) stream)
  (flet ((%write-a-header (mail-archive stream header raw-value &optional break-line-p)
           (when raw-value
             (when break-line-p (break-line :stream stream))
             (write-string (symbol-name (or (%header-print-name header) header)) stream)
             (write-string ": " stream)
	     (with-font (:size 3 :stream stream)
	       (loop for (msg-id . more) = raw-value then more
		     do (write-message-id-hyperlinks mail-archive msg-id stream)
		     while more
		     do (write-string ", " stream))))))
    (with-slots (mail-archive in-reply-to references) message
      (%write-a-header mail-archive stream :in-reply-to in-reply-to in-reply-to)
      (%write-a-header mail-archive stream :references references references))))

(defgeneric msg-write-summary (message stream view))

(defmethod msg-write-summary ((message message) stream (view (eql :header)))
  (with-slots (heading) message
    (enumerating-item (stream :head heading)
      (msg-write-headers message stream))))

(defmethod msg-write-summary ((message message) stream (view (eql :date-author)))
  (flet ((%write-a-header (stream header raw-value &optional break-line-p)
           (when raw-value
             (when break-line-p (break-line :stream stream))
             (write-string (symbol-name (or (%header-print-name header) header)) stream)
             (write-string ": " stream)
             (write-string raw-value stream))))
    (with-slots (date from heading) message
      (enumerating-item (stream :head heading)
	(%write-a-header stream :date date)
	(%write-a-header stream :from from t)))))

(defmethod msg-write-summary ((message message) stream (view (eql :date-author-subject)))
  (flet ((%write-a-header (stream header raw-value &optional break-line-p)
           (when raw-value
             (when break-line-p (break-line :stream stream))
             (write-string (symbol-name (or (%header-print-name header) header)) stream)
             (write-string ": " stream)
             (write-string raw-value stream))))
    (with-slots (date from heading subject) message
      (enumerating-item (stream :head heading)
	(%write-a-header stream :date date)
	(%write-a-header stream :from from t)
	(%write-a-header stream :subject subject t)))))

(defmethod msg-write-summary ((message message) stream (view (eql :author)))
  (with-slots (date from subject url-string) message
    (write-string from stream)
    (write-string " -- " stream)
    (note-anchor subject :reference url-string :stream stream)))

(defmethod msg-write-summary ((message message) stream (view (eql :date)))
  (with-slots (date from subject url-string) message 
    (write-string (if date date " -- ") stream)
    (with-font (:color :red :stream stream)
      (write-string "  -- " stream)
      (write-string from stream))
    (write-string " -- " stream)
    (note-anchor subject :reference url-string :stream stream))) 

(defmethod msg-write-summary ((message message) stream (view (eql :subject)))
  (with-slots (date from subject url-string) message
    (note-anchor subject :reference url-string :stream stream) 
    (write-string " -- " stream)
    (write-string from stream)))

(defmethod msg-write-summary ((message message) stream (view (eql :author-date)))
  (with-slots (date from) message
    (write-string from stream)
    (when date
      (fast-format stream " on ~A" date))))


;;;------------------------------------------------------------------- 
;;;
;;; WRITING MESSAGE BODYS BY CONTENT TYPE
;;;

(defgeneric msg-write-message-content (message major-type minor-type parameters stream &optional file-stream)
  (:documentation "Writes MESSAGE content from disk onto STREAM
according to MIME content type (MAJOR-TYPE MINOR-TYPE . PARAMETERS).
FILE-STREAM is bound to a file stream into the message data on disk."))

;; Default method is to show plain text
(defmethod msg-write-message-content ((message message) major-type minor-type parameters stream &optional file-stream)
  (declare (ignore major-type minor-type parameters))
  (with-slots (mail-archive subject header-position body-position end-position) message
    (msg-write-body-as-verbatim-html stream file-stream body-position end-position (ma-reply-color mail-archive))))

(defmethod msg-write-message-content ((message message) (major-type (eql :text)) (minor-type (eql :plain))
				      parameters stream &optional file-stream)
  (declare (ignore parameters))
  (with-slots (mail-archive subject header-position body-position end-position) message
    (msg-write-body-as-verbatim-html stream file-stream body-position end-position (ma-reply-color mail-archive))))

(defmethod msg-write-message-content ((message message) (major-type (eql :text)) (minor-type (eql :html))
				      parameters stream &optional file-stream)
  (declare (ignore parameters))
  (with-slots (mail-archive subject header-position body-position end-position) message
    (loop initially (set-up-input-buffer file-stream body-position end-position)
	  with buffer = *server-line-buffer*
	  do (multiple-value-bind (line eof-p delim length)
		 (read-delimited-line file-stream '(#\return #\linefeed) nil buffer)
	       (declare (ignore delim))
	       (unless (zerop length)
		 (write-string line stream))
	       (write-char #\Newline stream)
	       (when eof-p (return)))
	  while (< (file-position file-stream) end-position))))


;;;------------------------------------------------------------------- 
;;;
;;; WRITING MESSAGE DATA FROM DISK
;;;

(defgeneric msg-write-message-data (message view stream &optional file-stream)
  (:documentation "Writes MESSAGE data from disk onto STREAM according to VIEW.
FILE-STREAM is bound to a file stream into the message data on disk.")) 

(defmethod msg-write-message-data ((message message) (view (eql :message-content)) stream &optional file-stream)
  (destructuring-bind (major-type minor-type &rest parameters)
      (msg-content-type message)
    (msg-write-message-content message major-type minor-type parameters stream file-stream)))

(defmethod msg-write-message-data ((message message) (view cons) stream &optional file-stream)
  (loop for component in view
	do (msg-write-message-data message component stream file-stream)))

(defmethod msg-write-message-data ((message message) (view (eql :horizontal-line)) stream &optional fstream)
  (declare (ignore fstream))
  (horizontal-line :stream stream :size 2.))

(defmethod msg-write-message-data ((message message) (view (eql :headers-full)) stream &optional file-stream)
  (with-slots (mail-archive subject header-position body-position end-position) message
    (msg-write-full-verbatim-headers mail-archive stream file-stream header-position body-position)))

(defmethod msg-write-message-data ((message message) (view (eql :reply-text)) stream &optional file-stream)
  (with-slots (body-position end-position mail-archive) message
    (msg-write-body-as-reply-text stream file-stream body-position end-position)))

(defmethod msg-write-message-data :around ((message message) format stream &optional fstream)
  (declare (ignore fstream))
  (let ((mail-archive (msg-mail-archive message)))
    (handler-case
      (with-lock-held ((ma-lock mail-archive) :read)
	(with-direct-access-open-file (file-stream (ma-pathname mail-archive) :direction :input)
	  (call-next-method message format stream file-stream)))
      (file-not-found () (error 'document-not-found :url (ma-url mail-archive))))))


;;;------------------------------------------------------------------- 
;;;
;;; WRITING MESSAGE COMPONENTS
;;;

(defgeneric msg-write-message (message format stream))

(defmethod msg-write-message ((message message) (view string) stream)
  (write-string view stream))

(defmethod msg-write-message ((message message) (view cons) stream)
  ;; Main loop that puts up message components
  (loop for component in view
	do (msg-write-message message component stream)))

(defmethod msg-write-message (message (format (eql :horizontal-line)) stream)
  (declare (ignore message))
  (horizontal-line :stream stream :size 2.))

(defmethod msg-write-message ((mail-archive mail-archive) (view (eql :archive-info)) stream)
  (ma-write-archive-info mail-archive stream))

(defmethod msg-write-message ((message message) (view (eql :archive-info)) stream)
  (ma-write-archive-info (msg-mail-archive message) stream))

(defmethod msg-write-message ((message message) (view (eql :thread-enumeration)) stream)
  (when (msg-write-summary message stream :thread-enumeration)
    (horizontal-line :stream stream :size 2.)))

(defmethod msg-write-message ((message message) (view (eql :thread-enumeration)) stream)
  (when (msg-write-summary message stream :thread-enumeration)
    (horizontal-line :stream stream :size 2.)))

(defmethod msg-write-message ((message message) (view (eql :headers)) stream)
  (with-rendition (:bold :stream stream) 
    (msg-write-headers message stream)
    (msg-write-xref-headers message stream)))

(defmethod msg-write-message ((message message) (view (eql :headers-full)) stream)
  (with-rendition (:bold :stream stream) 
    (msg-write-message-data message :headers-full stream)))

(defmethod msg-write-message ((message message) (view (eql :message-content)) stream)
  (msg-write-message-data message :message-content stream))

(defmethod msg-write-message ((message message) (format (eql :reply-form)) stream)
  (let* ((mail-archive (msg-mail-archive message))
	 (message-id `(,(msg-message-id message)))
	 (in-reply-to (msg-in-reply-to message))
	 (references (if in-reply-to
			 `(,@in-reply-to ,@(msg-references message))
			 (msg-references message))))
    (declare (dynamic-extent message-id references))
    (with-cookie-values ((user-email-address user-personal-name))
      (ma-accept-message mail-archive
			 (ma-mailing-list mail-archive)
			 stream
			 :email-address (and user-email-address (read-from-armor-plated-string user-email-address))
			 :personal-name (and user-personal-name (read-from-armor-plated-string user-personal-name))
			 :subject (msg-subject message)
			 :message message
			 :in-reply-to message-id
			 :references references))))

(defmethod msg-write-message ((message message) (view (eql :reply-anchor)) stream)
  (let ((url (msg-reply-url-string message)))
    (when url 
      (with-anchor-noted (:reference url :stream stream)
	(with-rendition (:bold :stream stream)
	  (fast-format stream "Reply to Message ~D" (msg-index message)))))))

(defmethod write-message-button ((mail-archive mail-archive) (message-index integer) view display-string stream)
  (with-fillout-form (:post (ma-url mail-archive) :stream stream)
    (accept-input 'hidden "OPERATION" :default "SHOW-MESSAGE" :stream stream)
    (accept-input 'hidden "VIEW" :default (symbol-name view) :stream stream)
    (accept-input 'hidden "INDEX" :default message-index :stream stream)
    (accept-input 'submit-button "SUBMIT" :display-string display-string :stream stream)))

(defmethod msg-write-message ((message message) (view (eql :reply-button)) stream)
  (when (msg-reply-url-string message)
    (write-message-button (msg-mail-archive message) (msg-index message) :reply "Reply" stream)))

(defmethod msg-write-message ((message message) (view (eql :previous-button)) stream)
  (let ((index (1- (the fixnum (msg-index message)))))
    (unless (zerop index)
      (let ((mail-archive (msg-mail-archive message)))
	(write-message-button mail-archive index (ma-message-format mail-archive) "Previous" stream)))))

(defmethod msg-write-message ((message message) (view (eql :next-button)) stream)
  (let ((mail-archive (msg-mail-archive message))
	(index (1+ (the fixnum (msg-index message)))))
    (unless (< (ma-size mail-archive) index)
      (write-message-button mail-archive index (ma-message-format mail-archive) "Next" stream))))

(defmethod msg-write-message ((message message) (view (eql :backward-in-thread-button)) stream &aux thread previous-msg)
  (when (and (setq thread (msg-thread message))
	     (setq previous-msg (thread-previous-message thread message)))
    (let ((mail-archive (msg-mail-archive message)))
      (write-message-button mail-archive (msg-index previous-msg) (ma-message-format mail-archive) "Back in Conversation" stream))))

(defmethod msg-write-message ((message message) (view (eql :forward-in-thread-button)) stream &aux thread next-msg)
  (when (and (setq thread (msg-thread message))
	     (setq next-msg (thread-next-message thread message)))
    (let ((mail-archive (msg-mail-archive message)))
      (write-message-button mail-archive (msg-index next-msg) (ma-message-format mail-archive) "Forward in Conversation" stream))))


;;;------------------------------------------------------------------- 
;;;
;;; PREDEFINED MESSAGE FORMATS
;;;

(defmacro define-message-view ((view) &body components)
  `(defmethod msg-write-message (message (format (eql ,(symbolize (symbol-name view) *keyword-package*))) stream)
     #+Genera(declare (sys:function-parent ,view define-message-view))
     (msg-write-message message (quote ,(loop for keyword in components
					      do (check-type keyword keyword)
					      collect keyword))
			stream)))

(define-message-view (content)
  :thread-enumeration
  :headers
  :horizontal-line
  :message-content
  :reply-anchor
  :horizontal-line)

(define-message-view (content-full-headers)
  :thread-enumeration
  :headers-full
  :horizontal-line
  :message-content
  :reply-anchor
  :horizontal-line)

(define-message-view (content-full-headers-after)
  :thread-enumeration
  :message-content
  :reply-anchor
  :horizontal-line
  :headers-full
  :horizontal-line)

(define-message-view (:reply)
  :reply-form
  :horizontal-line)

;; obsolete
(define-message-view (verbatim)
  :thread-enumeration
  :headers
  :horizontal-line
  :message-content
  :reply-anchor
  :horizontal-line)
;; obsolete
(define-message-view (verbatim-full-headers)
  :thread-enumeration
  :headers-full
  :horizontal-line
  :message-content
  :reply-anchor
  :horizontal-line)
;; obsolete
(define-message-view (verbatim-full-headers-after)
  :thread-enumeration
  :message-content
  :reply-anchor
  :horizontal-line
  :headers-full
  :horizontal-line)


;;;------------------------------------------------------------------- 
;;;
;;; THREAD OPERATIONS
;;;

(defgeneric decache-thread (thread))

(defmethod decache-thread ((thread thread))
  (with-slots (messages unresolved-message-ids) thread
    (remove-value thread :thread-subject)
    (setq messages nil
          unresolved-message-ids nil)))

(defgeneric msg-cronological-sort-key (message))

(defmethod msg-cronological-sort-key ((message msg-threading-mixin))
  (with-slots (time) message
    (or time -1)))

(defgeneric push-thread (message-id-message-thread thread)
  (:documentation "Merges messages of THREAD1 into THREAD2."))

(defmethod push-thread ((message-id string) (thread thread))
  (with-slots (mail-archive message-ids messages unresolved-message-ids) thread
    (unless (member message-id message-ids :test #'equalp)
      (push message-id message-ids)
      (let ((msg (ma-get-message mail-archive message-id nil)))
        (cond (msg
	       (push-ordered msg messages #'< :key #'msg-cronological-sort-key)
	       (setf (msg-thread msg) thread))
	      (t (setq unresolved-message-ids `(,.unresolved-message-ids ,message-id)))))
      (note-message-id-in-thread message-id thread mail-archive))))

(defmethod push-thread ((message msg-threading-mixin) (thread thread))
  (push-thread (msg-message-id message) thread))

(defmethod push-thread ((thread1 thread) (thread2 thread))
  "Merges messages of THREAD1 into THREAD2."
  (with-slots (message-ids) thread1
    (dolist (message message-ids)
      (push-thread message thread2))))

(defgeneric %sort-thread-messages (thread))

(defmethod %sort-thread-messages ((thread thread))
  (with-slots (mail-archive message-ids messages unresolved-message-ids) thread
    (loop for msg-id in message-ids
          for msg = (ma-get-message mail-archive msg-id nil)
          when msg
            do (push-ordered msg messages #'< :key #'msg-cronological-sort-key)
	       (setf (msg-thread msg) thread)
          else collect msg-id into unresolved-msg-ids
          finally (setq unresolved-message-ids unresolved-msg-ids))))

(defgeneric %intern-unresolved-message-ids (thread))

(defmethod %intern-unresolved-message-ids ((thread thread))
  (with-slots (mail-archive messages unresolved-message-ids) thread
    (loop for msg-id in unresolved-message-ids
	  for msg = (ma-get-message mail-archive msg-id nil)
	  when msg
	    do (push-ordered msg messages #'< :key #'msg-cronological-sort-key)
	       (setf (msg-thread msg) thread)
	       (setq unresolved-message-ids (delete msg-id unresolved-message-ids :test #'equalp)))))

(defgeneric thread-messages (thread)
  (declare (values messages unresolved-message-ids)))

(defmethod thread-messages ((thread thread))
  (with-slots (message-ids messages unresolved-message-ids) thread
    (cond ((and message-ids (not (or messages unresolved-message-ids)))
	   (%sort-thread-messages thread))
	  (unresolved-message-ids
	   (%intern-unresolved-message-ids thread)))
    (values messages unresolved-message-ids)))

(defgeneric message-thread (message archive))

(defmethod message-thread ((message-id string) (archive ma-threading-mixin))
  (with-slots (thread-table) archive
    (gethash message-id thread-table nil)))

(defmethod message-thread ((message msg-threading-mixin) archive)
  (declare (ignore archive))
  (message-thread (msg-message-id message) (msg-mail-archive message)))

(defgeneric thread-previous-message (thread message))

(defmethod thread-previous-message ((thread thread) (message msg-threading-mixin))
  (multiple-value-bind (messages)
      (thread-messages thread)
    (unless (eql message (first messages))
      (loop with prev = (pop messages)
	    for msg in messages
	    when (eql msg message)
	      return prev
	    do (setq prev msg)
	    finally (error "The message, ~S, does not appear in the thread, ~S." message thread)))))

(defgeneric thread-next-message (thread message))

(defmethod thread-next-message ((thread thread) (message msg-threading-mixin))
  (multiple-value-bind (messages)
      (thread-messages thread)
    (loop for (msg . more) = messages then more
	  while more
	  when (eql msg message)
	    return (first more)
	  finally (error "The message, ~S, does not appear in the thread, ~S." message thread))))

(defgeneric note-message-id-in-thread (message-or-message-id thread mail-archive)
  (declare (values thread new-p merge-thread-p)))

(defmethod note-message-id-in-thread ((message-id string) (thread thread) (mail-archive ma-threading-mixin))
  (with-slots (thread-table) mail-archive
    (flet ((update-subject-key (mail-archive message-id)
	     (let ((msg (ma-get-message mail-archive message-id nil)))
	       (when msg
		 (setf (gethash (msg-subject msg) (ma-thread-table mail-archive)) thread)))))
      (declare (inline update-subject-key))
      (let ((existing-thread (gethash message-id thread-table)))
	(cond ((null existing-thread)
	       (setf (gethash message-id thread-table) thread)
	       (update-subject-key mail-archive message-id)
	       (values thread t))
	      ((eq existing-thread thread)
	       (values thread nil))
	      ;; merge threads because the message ID is linking them
	      (t (let ((msg (ma-get-message mail-archive message-id nil)))
		   (push-thread existing-thread thread)
		   (setf (gethash message-id thread-table) thread)
		   (when msg
		     (setf (msg-thread msg) thread))
		   (values thread nil t))))))))

(defmethod note-message-id-in-thread ((message msg-threading-mixin) (thread thread) mail-archive)
  (declare (ignore mail-archive))
  (note-message-id-in-thread (msg-message-id message) thread (msg-mail-archive message)))

(defconstant *thread-reference-string* "Thread-")

(defun thread-reference-p (string)
  (and (< 7 (length string))			;Allegro's string= blows out on shorter strings.
       (string= *thread-reference-string* string :start1 0 :end1 7 :start2 0 :end2 7)))

(defun thread-make-url-string (search-url number)
  (concatenate 'string (coerce-url-string search-url) *thread-reference-string* (write-to-string number :base 10)))

(defun thread-make-heading (search-url number &optional (text "Conversation"))
  (let* ((url (thread-make-url-string search-url number))
         (text (concatenate 'string text " " (write-to-string number :base 10.))))
    (declare (dynamic-extent url text))
    (note-anchor text :stream nil :reference url)))

(defgeneric create-thread (mail-archive))

(defmethod create-thread ((mail-archive ma-threading-mixin))
  (with-slots (reverse-threads search-url thread-class thread-heading thread-number thread-table) mail-archive
    (macrolet ((%make-thread-instance (class &rest args)
                 `(case ,class
                    (thread (make-instance 'thread . ,args))	; optimize default case.
                    (t (make-instance ,class . ,args)))))
      (let* ((index (1+ (the fixnum thread-number)))
	     (url-string (thread-make-url-string search-url index))
	     (heading (thread-make-heading search-url index thread-heading))
	     (thread (%make-thread-instance thread-class :mail-archive mail-archive
					    :index index :url-string url-string :heading heading)))
	(setf (gethash index thread-table) thread)
	(push thread reverse-threads)
	(setf (ma-thread-number mail-archive) index)
	thread))))

(defgeneric ma-get-thread (mail-archive key &optional error-p))

(defmethod ma-get-thread ((mail-archive ma-threading-mixin) key &optional error-p)
  (with-slots (thread-table) mail-archive
    (cond ((gethash key thread-table))
          (error-p (error "~S does not denote a thread in ~S." key mail-archive))
          (t nil))))

(defgeneric note-message-reply (mail-archive message referenced-message))

(defmethod note-message-reply ((mail-archive ma-threading-mixin) (message-id string) (referenced-message-id string))
  (let ((thread1 (message-thread message-id mail-archive))
        (thread2 (message-thread referenced-message-id mail-archive)))
    (cond ((and thread1 thread2)
           (push-thread thread1 thread2))
          (thread1
           (push-thread referenced-message-id thread1))
          (thread2
           (push-thread message-id thread2))
          (t (let ((thread (create-thread mail-archive)))
               (push-thread message-id thread)
               (push-thread referenced-message-id thread))))))

(defmethod note-message-reply (mail-archive (message msg-threading-mixin) referenced-message)
  (note-message-reply mail-archive (msg-message-id message) referenced-message))

(defmethod note-message-reply (mail-archive message (referenced-message msg-threading-mixin))
  (note-message-reply mail-archive message (msg-message-id referenced-message)))

(defgeneric note-message-subject (mail-archive message subject))

;; Arrange for the subject string to be shared across subject threaded messages.   2/1/97 -- JCMa.
(defmethod note-message-subject ((mail-archive ma-threading-mixin) (message msg-threading-mixin) (subject string))
  (with-slots (thread-table) mail-archive
    (let ((entry (gethash subject thread-table)))
      (etypecase entry
	(null
	  (setf (gethash subject thread-table) (or (message-thread message mail-archive) message)))
	(thread
	  (push-thread (msg-message-id message) entry))
	(msg-threading-mixin
	  (let ((thread (create-thread mail-archive)))
	    (push-thread (msg-message-id entry) thread)
	    (push-thread (msg-message-id message) thread)
	    (setf (gethash subject thread-table) thread)))))))

(defgeneric note-message-references (message))

(defmethod note-message-references ((message msg-threading-mixin))
  (with-slots (in-reply-to references subject) message
    (flet ((note-references (mail-archive message-id xrefs)
	     (etypecase message-id
	       (atom
		 (loop for xref in xrefs
		       do (note-message-reply mail-archive message-id xref)))
	       (cons
		 (loop for msg-id in message-id
		       do (loop for xref in xrefs
				do (note-message-reply mail-archive msg-id xref)))))))
      (let ((archive (msg-mail-archive message)))
	(cond-every
	  (in-reply-to (note-references archive (msg-message-id message) in-reply-to))
	  (references (note-references archive (msg-message-id message) references))
	  ((not (useless-subject-p subject))
	   (note-message-subject archive message subject)))))))


;;;------------------------------------------------------------------- 
;;;
;;; WRITING THREAD SUMMARIES
;;;

(defgeneric thread-subject (thread &optional recompute-p))

(defmethod thread-subject ((thread thread) &optional recompute-p)
  (with-slots (messages) thread
    (with-value-cached (thread :thread-subject :recompute-p recompute-p)
      (loop for idx downfrom (1- (the fixnum (length messages))) to 0
	    for subject = (msg-subject (nth idx messages))
	    do (unless (useless-subject-p subject)
		 (return subject))
	    finally (return-from thread-subject +message-no-subject-string+)))))

(defgeneric thread-write-subject (thread stream))

(defmethod thread-write-subject ((thread thread) stream)
  (with-rendition (:bold :stream stream)
    (write-string "Subject: " stream))
  (write-string (thread-subject thread) stream))

(defmethod msg-write-summary ((message msg-threading-mixin) stream (view (eql :thread-enumeration))
			      &aux (thread (message-thread message nil)))
  (with-slots (mail-archive) message
    (when thread
      (multiple-value-bind (messages unresolved-msg-ids)
	  (thread-messages thread)
	(when (or messages unresolved-msg-ids)
	  (with-centering (:stream stream)
	    (ns4.0:with-table (:border nil :background :Grey-Light-Very :cell-spacing 0 :cell-padding 4 :stream stream)
	      (with-table-row (:vertical-alignment :top :stream stream)
		(with-table-cell (:stream stream)
		  (with-font (:size 3 :stream stream)
		    (with-rendition (:bold :stream stream)
		      (write-string (thread-heading thread) stream)))
		  (with-font (:color (ma-highlight-color mail-archive) :size 2 :stream stream)
		    (cond-every
		      (unresolved-msg-ids
			(break-line :stream stream)
			(with-font (:size 2 :stream stream)
			  (with-rendition (:bold :stream stream)
			    (write-string "Unresolved Message-IDs: " stream))
			  (with-font (:size 1 :stream stream)
			    (loop for (msg-id . more) = unresolved-msg-ids then more
				  do (write-string-quoting-specials msg-id stream)
				  while more
				  do (write-string ", " stream)))))
		      (messages
			(let ((cols (if (< (length messages) 7) 1 2)))
			  (with-enumeration (stream :enumerate)
			    (ns4.0:with-multiple-columns (cols :stream stream)
			      (loop for msg in messages
				    do (enumerating-item (stream)
					 (if (eq msg message)
					     (with-rendition (:bold :stream stream)
					       (write-string "=> " stream)
					       (msg-write-summary msg stream :author-date))
					     (with-anchor-noted (:reference (msg-url-string msg) :stream stream)
					       (msg-write-summary msg stream :author-date))))))))))
		    ;; must return non-null whenever displays are generated.
		    t))))))))))

(defgeneric thread-write-summary (thread stream view))

(defmethod thread-write-summary ((thread thread) stream (view (eql :forward)))
  (flet ((write-heading (stream)
	   (let ((subject (thread-subject thread)))
	     (with-font (:size 4 :stream stream)
	       (with-rendition (:bold :stream stream)
		 (write-string (thread-heading thread) stream)
		 (when subject
		   (write-string ": " stream))))
	     (when subject
	       (with-font (:size 3 :stream stream)
		(write-string (thread-subject thread) stream)))))
	 (write-unresolved-msg-ids (stream unresolved-message-ids)
	   (with-font (:size 3 :stream stream)
	     (with-rendition (:bold :stream stream)
	       (write-string "Unresolved Message-IDs: " stream)))
	   (with-font (:size 2 :stream stream)
	     (loop for (msg-id . more) = unresolved-message-ids then more
		   do (write-string-quoting-specials msg-id stream)
		   while more
		   do (write-string ", " stream)))))
    (declare (dynamic-extent #'write-heading))
    (multiple-value-bind (messages unresolved-message-ids)
	(thread-messages thread)
      (enumerating-item (stream :head #'write-heading)
	(when unresolved-message-ids
	  (break-line :stream stream)
	  (write-unresolved-msg-ids stream unresolved-message-ids)))
      (when messages
	(with-emphasis (:quotation :stream stream)
	  (with-enumeration (stream :definition)
	    (with-rendition (:bold :stream stream)
	      (loop for msg in messages
		    do (msg-write-summary msg stream :date-author)))))))))

(defmethod thread-write-summary ((thread thread) stream (view (eql :messages)))
  (multiple-value-bind (messages)
      (thread-messages thread)
    (when messages
      (write-message-sequence (thread-mail-archive thread) messages stream))))

(defmethod write-message-sequence ((mail-archive mail-archive) messages stream &optional format)
  (with-slots (message-format pathname url) mail-archive
    (loop with format = (or format message-format)
	  for (msg . more) = messages then more
	  do (with-section-heading ((msg-heading msg) :stream stream)
	       (msg-write-message msg format stream))
	  while more
	  do  (horizontal-line :stream stream :size 2.))))

;;;------------------------------------------------------------------- 
;;;
;;; INITIALIZING AND UPDATING MAIL ARCHIVES
;;; 

(defmethod file-length-in-bytes ((mail-archive mail-archive) &optional new-length)
  (declare (ignore new-length))
  (with-slots (length-in-bytes lock) mail-archive
    (with-lock-held (lock :read)
      (return-from file-length-in-bytes (or length-in-bytes 0)))))

(defgeneric ma-mailing-list (mail-archive)
  (:documentation "Returns the mailing list associated with MAIL-ARCHIVE."))

(defmethod ma-mailing-list ((url mail-archive-url))
  (get-value url :mailing-list))

(defun msg-make-url-string (search-url number)
  (concatenate 'string (coerce-url-string search-url) (write-to-string number :base 10)))

(defun msg-make-reply-url-string (search-url number)
  (concatenate 'string (coerce-url-string search-url) (write-to-string number :base 10) "+reply"))

(defun msg-make-heading (search-url number &optional (text "Message"))
  (let* ((url (msg-make-url-string search-url number))
         (text (concatenate 'string text " " (write-to-string number :base 10.))))
    (declare (dynamic-extent url text))
    (note-anchor text :stream nil :reference url)))

(defun msg-quote-string (from)
 (if from
     (with-output-to-string (stream)
       (etypecase from
	 (string (write-string-quoting-specials from stream))
	 (cons (loop for (line . more) = line then more
		     while line
		     do (write-string-quoting-specials line stream)
		     when more
		       do (write-char #\space stream)))))
     "Unknown"))

(defun homogenize-subject (string &optional (start 0) end &aux (len (length string)))
  (typecase string
    (cons (mapcar #'homogenize-subject string start end))
    (t (labels ((trim-char-p (char)
		  (member char '(#\space #\return #\tab #\. #\: #\, #\;) :test #'eql))
		(useless-p (string start end len)
		  (with-fast-array-references ((string string string))
		    (or (= start end)
			(zerop len)
			(loop for idx upfrom start below end
			      unless (trim-char-p (aref string idx))
				return nil
			      finally (return t))))))
	 (declare (inline trim-char-p useless-p))
	 (unless end (setq end len))
	 (cond ((useless-p string start end len)
		+message-no-subject-string+)
	       (t (loop for pos = (string-search "Re:" string 0 3 start end)
			while pos
			do (setq start (+ (the fixnum pos) 3)))
		  (let* ((s (position-if-not #'trim-char-p string :start start :end end))
			 (e (and s (1+ (the fixnum (position-if-not #'trim-char-p string :start s :end end :from-end t))))))
		    (cond ((null s) +message-no-subject-string+)
			  ((and (= start s)
				(= end e)
				(= len (- (the fixnum e) (the fixnum s) )))
			   string)
			  (t (subseq string s e))))))))))


(defgeneric scan-message-line (mail-archive stream delimiters eof buffer)
  (declare (values line error-p delimiter length))
  (:documentation "Reads a message line for mail-archive from stream with delimiters.
Buffer is the line buffer to use. Extensions may specialize or advise this method."))

(defmethod scan-message-line (mail-archive stream delimiters eof buffer)
  (declare (ignore mail-archive))
  (read-delimited-line stream delimiters eof buffer))

(defgeneric scan-message-body (mail-archive file-stream message-delimiter plist)
  (declare (values end-position more-messages-p))
  (:documentation "Scans the mesage body to find the start of the next message.
   PLIST is (message-number body-position header-position message-headers).
Specialize this method or add an after method in order to operate on message body during the scan.")) 

(defmethod scan-message-body (mail-archive file-stream message-delimiter plist)
  (declare (ignore plist))
  (loop with buffer = *server-line-buffer*
        for position = (file-position file-stream)
        doing (multiple-value-bind (line error-p delimiter length)
                  (scan-message-line mail-archive file-stream '(#\Return #\Linefeed) nil buffer)
                delimiter                       ; ignore
                (cond ((or error-p (null line))
                       (return (values position nil)))
                      ((funcall message-delimiter line length)
                       (return (values position t)))))))

(defgeneric create-message (mail-archive file-stream number)
  (declare (values message more-messages-p)))

(defmethod create-message ((mail-archive mail-archive) file-stream number)
  (declare (values message more-messages-p))
  (with-slots (search-url message-table message-delimiter message-heading  
			  message-class) mail-archive
    (macrolet ((%make-message-instance (class &rest args)
		 `(case ,class
		    (message (make-instance 'message . ,args))  ; optimize default case.
		    (t (make-instance ,class . ,args)))))
      (flet ((process-subject (subject)
	       (etypecase subject
		 (null +message-no-subject-string+)
		 (string (if (useless-subject-p subject) +message-no-subject-string+ (homogenize-subject subject)))
		 (cons (if (cdr subject)
			   (let ((args (loop for (subj . more) = subject then more
					     collect (homogenize-subject subj)
					     while more
					     collect (coerce '(#\return) 'string))))
			     (declare (dynamic-extent args))
			     (apply #'concatenate 'string args))
			   (homogenize-subject (car subject)))))))
	(unwind-protect
	    (let* ((header-position (file-position file-stream))
		   (headers (resourced-read-headers *headers* file-stream))
		   (body-position (file-position file-stream))
		   (plist `(,number ,body-position ,header-position ,headers)))
	      (declare (dynamic-extent plist))
	      (cond ((null-header-set-p headers)
		     (values nil nil))
		    (t (multiple-value-bind (end-position more-messages-p)
			   (scan-message-body mail-archive file-stream message-delimiter plist)
			 (let* ((from (get-header :from headers))
				(quoted-from (msg-quote-string from))
				(date (car (get-raw-header :date headers t)))
				(subject (get-header :subject headers))
				(time (handler-case (get-header :date headers) (error () nil)))
				(url-string (msg-make-url-string search-url number))
				(msg-id (get-header :message-id headers))
				(in-reply-to (get-header :in-reply-to headers))
				(references (get-header :references headers))
				(reply-url (when (ma-mailing-list mail-archive)
					     (msg-make-reply-url-string search-url number)))
				msg)
			   (cond (msg-id
				  (when (consp msg-id)
				    (setq msg-id (delete-duplicates msg-id :test #'equalp))
				    (unless (cdr msg-id)
				      (setq msg-id (car msg-id)))))
				 (t (setq msg-id (generate-message-id (get-universal-time) time))))
			   (setq msg (%make-message-instance 
				       message-class
				       :mail-archive mail-archive
				       :index number
				       :url-string url-string
				       :reply-url-string reply-url
				       :heading (msg-make-heading search-url number message-heading)
				       :time time
				       :date date
				       :from quoted-from
				       :subject (process-subject subject)
				       :content-type (or (get-header :content-type) '(:text :plain))
				       :message-id msg-id
				       :in-reply-to in-reply-to
				       :references references
				       :header-position header-position
				       :body-position body-position
				       :end-position  end-position))
			   (setf (gethash number message-table) msg)
			   (when (msg-message-id msg)
			     (setf (gethash (msg-message-id msg) message-table) msg))
			   ;; update thread data structures -- better as an after method for modularity
			   (note-message-references msg)
			   ;; return values
			   (values msg more-messages-p))))))
	  (clear-header-set *headers*))))))

(defgeneric initialize-mail-archive (mail-archive file-stream))

(defmethod initialize-mail-archive ((mail-archive mail-archive) file-stream)
  (with-slots (url search-url message-table pathname prefixed-messages-p) mail-archive
    (using-resource (*headers* header-set)
      (using-resource (*server-line-buffer* line-buffer *line-buffer-size*)
	(clrhash message-table)
	(when prefixed-messages-p
	  (clear-prefix-message-delimiter file-stream))
	;; Initialize Color Schemes
	(initialize-mail-archive-color-scheme mail-archive)
	(setf (ma-mailing-list mail-archive) (ma-mailing-list url))
	;; main loop
	(loop with more-messages-p = t and msg and count = 1
	      and start and end and reverse-messages
	      do (multiple-value-setq (msg more-messages-p)
		   (create-message mail-archive file-stream count))
	      when msg
		collect msg into messages
		and do (push msg reverse-messages)
		       (incf count)
		       (cond-every
			 ((or (null start) (msg-before-p msg start))
			  (setq start (or (msg-time msg) start)))
			 ((or (null end) (msg-after-p msg end))
			  (setq end (or (msg-time msg) end))))
	      while more-messages-p
	      finally (setf (ma-messages mail-archive) messages
			    (ma-reverse-messages mail-archive) reverse-messages
			    (ma-size mail-archive) (1- count)
			    (ma-length-in-bytes mail-archive) (file-length file-stream)
			    (ma-start-time mail-archive) start
			    (ma-end-time mail-archive) end
			    (ma-cache-time mail-archive) (get-universal-time)))
	mail-archive))))

(defmethod initialize-mail-archive :before ((mail-archive mail-archive) file-stream)
  (declare (ignore file-stream))
  (when *server*
    (setf (server-life-time *server*) (ma-initialization-life-time mail-archive))))

(defgeneric create-mail-archive (url))

(defmethod create-mail-archive ((url mail-archive-url))
  (with-slots (pathname) url
    (let ((search-url (get-value url :search-url))
          (default-view (get-value url :default-view))
          (views (get-value url :views))
          (message-heading (get-value url :message-heading))
          (mail-archive-class (get-value url :mail-archive-class))
          (message-format (get-value url :message-format))
          (message-class (get-value url :message-class))
          archive)
      (multiple-value-bind (message-delimiter prefixed-messages-p)
          (get-mail-file-format-delimiters (get-value url :mail-file-format))
        (setq archive (make-instance mail-archive-class :url url
                                     :search-url search-url 
                                     :pathname pathname
                                     :prefixed-messages-p prefixed-messages-p
                                     :message-delimiter (fdefinition message-delimiter)
                                     :default-view default-view
                                     :views views
                                     :message-heading message-heading
                                     :message-format message-format
                                     :message-class message-class)))
      (setf (slot-value url 'mail-archive) archive
            (get-value search-url :mail-archive) archive
            (ma-lock archive) (make-lock (name-string url))) 
      (with-lock-held ((ma-lock archive) :write "Create Mail Archive")
        (flet ((init-archive (url file-stream)
                 (declare (ignore url))
                 (initialize-mail-archive archive file-stream)))
          (declare (dynamic-extent #'init-archive))
          (apply-mail-archive url pathname #'init-archive)))
      archive)))

(defgeneric reinitialize-mail-archive (mail-archive)
  (:documentation "Primitive for reinitializing the mail archive for UPDATE-MAIL-ARCHIVE.
Should not be called directly."))

(defmethod reinitialize-mail-archive ((mail-archive mail-archive))
  (with-slots (url pathname) mail-archive
    (flet ((init-archive (url file-stream)
	     (declare (ignore url))
	     (initialize-mail-archive mail-archive file-stream)))
      (declare (dynamic-extent #'init-archive))
      (apply-mail-archive url pathname #'init-archive))))

(defgeneric incremental-update-mail-archive (mail-archive  old-size old-length new-length)
  (:documentation "Primitive to incrementally add new messages to a mail archive.
Called by update-mail-archive when new messages appear in the mail file.
Should not be called directly."))

(defmethod incremental-update-mail-archive ((mail-archive mail-archive) old-size old-length new-length)
  (with-slots (pathname) mail-archive
    (using-resource (*headers* header-set)
      (using-resource (*server-line-buffer* line-buffer *line-buffer-size*)
	(with-direct-access-open-file (file-stream pathname :direction :input)
	  (loop initially (set-up-input-buffer file-stream old-length new-length)
		with more-messages-p = t and msg and count = (1+ old-size) and reverse-messages
		with start = (ma-start-time mail-archive)
		with end = (ma-end-time mail-archive)
		do (multiple-value-setq (msg more-messages-p)
		     (create-message mail-archive file-stream count))
		when msg
		  collect msg into messages
		  and do (push msg reverse-messages)
			 (incf count)
			 (cond-every
			   ((or (null start) (msg-before-p msg start))
			    (setq start (or (msg-time msg) start)))
			   ((or (null end) (msg-after-p msg end))
			    (setq end (or (msg-time msg) end))))
		while more-messages-p
		finally (setf (ma-messages mail-archive) (nconc (ma-messages mail-archive) messages)
			      (ma-reverse-messages mail-archive) (nconc reverse-messages (ma-reverse-messages mail-archive))
			      (ma-size mail-archive) (1- count)
			      (ma-length-in-bytes mail-archive) new-length
			      (ma-start-time mail-archive) start
			      (ma-end-time mail-archive) end
			      (ma-cache-time mail-archive) (get-universal-time))))))))

(defmethod incremental-update-mail-archive :before ((mail-archive ma-server-life-time-mixin) old-size old-length new-length)
  (declare (ignore old-size old-length new-length))
 (when *server*
    (setf (server-life-time *server*) (ma-incremental-initialization-life-time mail-archive))))

(defgeneric update-mail-archive (mail-archive &optional recache-p)
  (:documentation "Updates a mail archive when the length changes.
Performs thread locking."))

(defmethod update-mail-archive ((mail-archive mail-archive) &optional recache-p)
  (with-slots (lock pathname length-in-bytes size) mail-archive
    (with-lock-held (lock :write "Update Mail Archive")
      (let ((new-length (file-length-in-bytes pathname))
            (old-length (or length-in-bytes 0))
            (old-size (or size 0)))
        (if (or recache-p (< new-length old-length))
	    ;; Lost bits better fully recompute
	    (reinitialize-mail-archive mail-archive)
	    ;; Otherwise add parse and add the new messages,
	    (incremental-update-mail-archive mail-archive old-size old-length new-length))
	mail-archive))))

(defgeneric subject-sorted-messages (mail-archive))

(defmethod subject-sorted-messages ((mail-archive mail-archive))
  (with-slots (subject-sorted-messages messages) mail-archive
    (or subject-sorted-messages
        (setq subject-sorted-messages (stable-sort (copy-list messages) #'string< :key #'msg-subject)))))

(defgeneric author-sorted-messages (mail-archive))

(defmethod author-sorted-messages ((mail-archive mail-archive))
  (with-slots (author-sorted-messages messages) mail-archive
    (or author-sorted-messages
        (setq author-sorted-messages (stable-sort (copy-list messages) #'string< :key #'msg-from)))))

(defgeneric invalidate-caches (mail-archive))

(defmethod invalidate-caches ((mail-archive mail-archive))
  (with-slots (author-sorted-messages subject-sorted-messages) mail-archive
    (setq subject-sorted-messages nil
          author-sorted-messages nil)))

(defgeneric maybe-update-mail-archive (mail-archive))

(defmethod maybe-update-mail-archive ((mail-archive mail-archive))
  (with-slots (pathname) mail-archive
    (unless (= (file-length-in-bytes mail-archive) (file-length-in-bytes pathname))
      (invalidate-caches mail-archive)
      (update-mail-archive mail-archive))))

(defgeneric mail-archive (url &optional recache-p)
  (declare (values archive newly-created-p)))

(defmethod mail-archive ((url mail-archive-url) &optional recache-p)
  (declare (values archive newly-created-p))
  (with-slots (mail-archive) url
    (cond ((and (not recache-p) mail-archive))
          (t (values (setq mail-archive (create-mail-archive url)) t)))))

(defun apply-mail-archive (url pathname function)
  (declare (dynamic-extent function))
  (cond (pathname
         (handler-case 
           (with-standard-access-open-file (file-stream pathname :direction :input)
             (funcall function url file-stream))
           (file-not-found () (error 'document-not-found :url url))))
        (t (error 'document-not-found :url url))))


;;;------------------------------------------------------------------- 
;;;
;;; WRITE ARCHIVE VIEW BANNER
;;;

(defgeneric ma-write-summary-view-choices (mail-archive stream &optional current-view))

(defmethod ma-write-summary-view-choices ((mail-archive mail-archive) stream &optional current-view)
  (with-slots (search-url views) mail-archive
    (loop initially (write-string "[ " stream)
          with search-url-string = (coerce-url-string search-url)
          for (view . more) = views then more
	  do (let ((text (string-capitalize (symbol-name view))))
	       (declare (dynamic-extent text))
	       (if (eq view current-view)
		   (write-string text stream)
		   (let ((url-string (concatenate 'string search-url-string (symbol-name view))))
		     (declare (dynamic-extent url-string))
		     (note-anchor text :reference url-string :stream stream))))
	  while more
	  do (write-string " | " stream)
	  finally (write-string " ]" stream))))

(defgeneric ma-write-description (mail-archive stream))

(defmethod ma-write-description ((mail-archive mail-archive) stream)
  (with-slots (url) mail-archive
    (let ((description (get-value url :description)))
      (when description
	(with-paragraph (:stream stream)
	  (with-centering (:stream stream)
	    (ns4.0:with-table (:border nil :width .85 :background :Grey-Light-Very :cell-spacing 0 :cell-padding 6 :stream stream)
	      (with-table-row (:stream stream)
		(with-table-cell (:stream stream)
		  (with-font (:size 2 :stream stream)
		    (etypecase description
		      (string (write-string description stream))
		      (function (funcall description stream)))))))))))))

(defgeneric ma-write-archive-info (mail-archive stream &optional view start end))

(defmethod ma-write-archive-info ((mail-archive mail-archive) stream &optional view start end)
  (with-slots (size start-time end-time) mail-archive
    (flet ((write-a-date (time string stream)
             (when time
               (break-line :stream stream)
               (write-string string stream)
               (write-time time stream))))
      (with-paragraph (:stream stream)
        (with-font (:size 2 :stream stream)
          (with-rendition (:bold :stream stream)
            (cond (end
		   (fast-format stream "Messages: ~D - ~D [~D]" start end size))
                  (t (fast-format stream "Messages: ~D" size)
		     (break-line :stream stream)	;better as a mixin for modularity   2/1/97 -- JCMa.
		     (fast-format stream "Conversations: ~D" (ma-thread-number mail-archive))))
            (write-a-date (if start (msg-time (ma-get-message mail-archive start t)) start-time)
                          "Start Date: " stream)
            (write-a-date (if end (msg-time (ma-get-message mail-archive end t)) end-time) 
                          "End Date: " stream)
            (break-line :stream stream)
            (write-string "Views: " stream)
            (ma-write-summary-view-choices mail-archive stream view)))))))

#+ignore
(defmethod ma-write-archive-info ((mail-archive mail-archive) stream &optional view start end)
  (with-slots (size start-time end-time) mail-archive
    (macrolet ((with-cell ((stream &optional horizontal-alignment rendition) &body body)
		 `(with-table-cell (:stream ,stream
					  :vertical-alignment :bottom
					  ,@(when horizontal-alignment `(:horizontal-alignment ,horizontal-alignment)))
		    (with-font (:size 2 :stream ,stream)
		      ,@(case rendition
			  ((nil) body)
			  (t `((with-rendition (:bold :stream ,stream)
				 . ,body))))))))
      (flet ((write-a-date (time string stream)
	       (when time
		 (with-table-row (:vertical-alignment :top :stream stream)
		   (with-cell (stream :left :bold)
			      (write-string string stream))
		   (with-cell (stream :left)
			      (write-time time stream))))))
	(declare (inline write-a-date))
	  (ns4.0:with-table (:border nil :cell-spacing 1 :cell-padding 0 :stream stream)
	    (with-table-row (:vertical-alignment :top :stream stream)
	      (with-cell (stream :left :bold)
			 (write-string "Messages" stream))
	      (with-cell (stream :left)
			 (if end
			     (fast-format stream "~D - ~D [~D]" start end size)
			     (fast-format stream "~D" size))))
	    (unless end				;better as a mixin for modularity   2/1/97 -- JCMa.
	      (with-table-row (:vertical-alignment :top :stream stream)
		(with-cell (stream :left :bold)
			   (write-string "Conversations" stream))
		(with-cell (stream :left)
			   (fast-format stream "~D" (ma-thread-number mail-archive)))))
	    (write-a-date (if start (msg-time (ma-get-message mail-archive start t)) start-time)
			  "Start Date" stream)
	    (write-a-date (if end (msg-time (ma-get-message mail-archive end t)) end-time) 
			  "End Date" stream)
	    (with-table-row (:vertical-alignment :top :stream stream)
	      (with-cell (stream :left :bold)
			 (write-string "Views" stream))
	      (with-cell (stream :left :bold)
			 (ma-write-summary-view-choices mail-archive stream view))))))))


;;;------------------------------------------------------------------- 
;;;
;;; WRITE ARCHIVE VIEWS
;;; 

(defgeneric ma-write-summary (mail-archive stream view)
  (:documentation "Writes an overview of MAIL-ARCHIVE on STREAM.
VIEW is a keyword denoting the display mode for the overview.
To extend the view, merely add new keyword specializations of VIEW."))

(defmethod ma-write-summary (mail-archive stream view)
  (declare (ignore stream))
  (error "Unknown view, ~S, on the mail-archive, ~S." view mail-archive))

(defmethod ma-write-summary ((mail-archive mail-archive) stream (view (eql :forward)))
  (with-slots (messages) mail-archive
    (with-enumeration (stream :definition)
      (with-rendition (:bold :stream stream)
        (loop for msg in messages
              do (msg-write-summary msg stream :header))))))

(defmethod ma-write-summary ((mail-archive mail-archive) stream (view (eql :backward)))
  (with-slots (reverse-messages) mail-archive
    (with-enumeration (stream :definition)
      (with-rendition (:bold :stream stream)
        (loop for msg in reverse-messages
              do (msg-write-summary msg stream :header))))))

(defmethod ma-write-summary ((mail-archive mail-archive) stream (view (eql :date)))
  (with-slots (messages) mail-archive
    (with-enumeration (stream :enumerate)
      (loop for msg in messages
            do (enumerating-item (stream)
                 (msg-write-summary msg stream :date)))))) 

(defmethod ma-write-summary ((mail-archive mail-archive) stream (view (eql :subject)))
  (with-enumeration (stream :enumerate)
    (loop for msg in (subject-sorted-messages mail-archive)
          do (enumerating-item (stream)
               (msg-write-summary msg stream :subject)))))

(defmethod ma-write-summary ((mail-archive mail-archive) stream (view (eql :author)))
  (with-enumeration (stream :enumerate)
    (loop for msg in (author-sorted-messages mail-archive)
          do (enumerating-item (stream)
               (msg-write-summary msg stream :author)))))

(defmethod ma-write-summary ((mail-archive mail-archive) stream (thread thread))
  (with-slots (thread-default-view) mail-archive
    (with-enumeration (stream :definition)
      (thread-write-summary thread stream thread-default-view))))

(defmethod ma-write-summary ((mail-archive ma-threading-mixin) stream (view (eql :conversation)))
  (with-enumeration (stream :definition)
    (loop for thread in (ma-reverse-threads mail-archive)
	  do (thread-write-summary thread stream :forward))))

(defmethod ma-write-summary ((mail-archive mail-archive) stream (view (eql :single)))
  (with-slots (reverse-messages) mail-archive
    (with-enumeration (stream :definition)
      (with-rendition (:bold :stream stream)
        (loop for msg in reverse-messages
	      unless (msg-thread msg)
		do (msg-write-summary msg stream :header))))))

(defmethod ma-write-summary :around ((mail-archive mail-archive) stream view)
  (with-slots (url) mail-archive
    (let ((title (get-value url :title)))
      (flet ((write-title (stream)
	       (with-centering (:stream stream)
		 (write-string title stream))))
	(declare (dynamic-extent #'write-title))
	(with-html-document (:stream stream)
	  (with-document-preamble (:stream stream)
	    (declare-title title :stream stream))
	  (with-mail-archive-document-body (mail-archive stream)
	    (with-section-heading (#'write-title :stream stream)
	      (ma-write-description mail-archive stream)
	      (ma-write-archive-info mail-archive stream view)
	      (horizontal-line :stream stream :size 5.)
	      ;; main loop
	      (call-next-method mail-archive stream (or view (ma-default-view mail-archive)))
	      (horizontal-line :stream stream :size 5.)
	      (ma-write-signature mail-archive stream))))))))

(defconstant *ma-write-summary-view-argument-position* 2)

(defun ma-map-summary-view-keywords (function)
  "Function is called on each keyword symbol that names a valid view"
  (loop for method in (generic-function-methods #'ma-write-summary)
	for method-specializers = (method-specializers method)
        for view-specializer = (when method-specializers
				 (get-eql-specializer-object
				   (elt method-specializers *ma-write-summary-view-argument-position*)))
        when view-specializer
          do (funcall function view-specializer)))

(defun mail-archive-view-keyword-p (keyword)
  (flet ((view-keyword-p (view)
           (when (eql keyword view)
             (return-from mail-archive-view-keyword-p t)))
         (view-string-p (view)
           (when (equalp keyword (symbol-name view))
             (return-from mail-archive-view-keyword-p t))))
    (declare (dynamic-extent #'view-keyword-p #'view-string-p))
    (ma-map-summary-view-keywords (etypecase keyword
                                       (keyword #'view-keyword-p)
                                       (string #'view-string-p)))
    nil))

;; maintain alphabetical order
(define-parameter *mail-archive-summary-views* nil
                  "The summary views available for mail archives.")

(defun mail-archive-summary-views (&optional recache-p)
  (cond ((and (null recache-p) *mail-archive-summary-views*))
        (t (let ((views))
             (flet ((collect (view)
                      (push-ordered view views #'string< :key #'symbol-name)))
               (declare (dynamic-extent #'collect))
               (ma-map-summary-view-keywords #'collect)
               (setq *mail-archive-summary-views* views))))))

(defun class-superclasses (class)
  (flet ((name (class)
	   (typecase class
	     (symbol class)
	     (t (class-name class)))))
    (loop for super-class in (class-direct-superclasses class)
	  for name = (name super-class)
	  as sc = (class-superclasses super-class)
	  unless (member name classes)
	    collect name into classes
	  when sc 
	    nconc (loop for item in sc
			for n = (name item)
			unless (member n classes)
			  collect n)
	      into classes
	  finally (return classes))))

#+old
(defun mail-archive-valid-summary-views (mail-archive-class &optional recache-p)
   (loop with class = (find-class mail-archive-class)
	    with super-classes = `(,mail-archive-class ,.(class-superclasses class))
	    for view in (mail-archive-summary-views recache-p)
	    when (loop for class in super-classes
		             ;; Kind of unsightly. Got a better way?
		             when (handler-case (fdefinition `(method ma-write-summary (,class t (eql ,view)))) (error () nil))
		             return t
		             finally (return nil))
	    collect view))

(defun mail-archive-valid-summary-views (mail-archive-class &optional recache-p)
  (flet ((method-exists-p (class top-class view)
	   (let ((spec `(,(find-class class) ,top-class
			 #-Franz-Inc (eql ,view)
			 #+Franz-Inc ,(#+ACLPC allegro:intern-eql-specializer #-ACLPC clos:intern-eql-specializer)
			 )))
	     (declare (dynamic-extent spec))
	     (find-method #'ma-write-summary () spec nil))))
    (declare (inline method-exists-p))
    (loop with class = (find-class mail-archive-class)
	  with super-classes = `(,mail-archive-class . ,(class-superclasses class))
	  with top-class = (find-class t)
	  for view in (mail-archive-summary-views recache-p)
	  when (loop for class in super-classes
		     when (method-exists-p class top-class view)
		       return t
		     finally (return nil))
	    collect view)))

(defgeneric ma-get-message (mail-archive key &optional error-p))

(defmethod ma-get-message ((mail-archive mail-archive) key &optional error-p)
  (with-slots (message-table) mail-archive
    (cond ((gethash key message-table))
          (error-p (error "~S does not denote a message in ~S." key mail-archive))
          (t nil))))

(defgeneric ma-write-message (mail-archive url key view stream))

(defmethod ma-write-message ((mail-archive mail-archive) url key view stream &aux msg)
  (with-slots (message-table lock size message-format) mail-archive
    (setq msg (ma-get-message mail-archive key nil))
    (unless msg
      (maybe-update-mail-archive mail-archive)
      (setq msg (ma-get-message mail-archive key nil)))
    (cond (msg
	   (let* ((format (or view message-format))
		  (ma-url (ma-url mail-archive))
		  (title (or (get-value ma-url :title) (name-string ma-url))))
	     (flet ((write-title (stream)
		      (with-centering (:stream stream)
			(write-string title stream)
			(break-line :stream stream)
			(write-string (msg-heading msg) stream))))
	       (declare (dynamic-extent #'write-title))
	       (with-conditional-get-response (stream :html
						      :last-modification (or (msg-time msg) (ma-cache-time mail-archive))
						      :expires (expiration-universal-time ma-url)
						      :cache-control (url:response-cache-control-directives ma-url)
						      :content-language (languages ma-url))
		 (with-html-document (:stream stream)
		   (with-document-preamble (:stream stream)
		     (declare-title title :stream stream))
		   (with-mail-archive-document-body (mail-archive stream)
		     (with-section-heading (#'write-title :stream stream)
		       (msg-write-message mail-archive :archive-info stream)
		       (msg-write-message nil :horizontal-line stream)
		       (msg-write-message msg format stream)
		       ;; Put up the archive signature
		       (ma-write-signature mail-archive stream))))))))
	  (t (error 'document-not-found :url url)))))

(defgeneric ma-write-message-interval (mail-archive view stream start end))

(defmethod ma-write-message-interval ((mail-archive mail-archive) view stream start end)
  (maybe-update-mail-archive mail-archive)
  (let* ((ma-url (ma-url mail-archive))
	 (title (or (get-value ma-url :title) (name-string ma-url))))
    (flet ((write-title (stream)
	     (with-centering (:stream stream)
	       (write-string title stream))))
      (declare (dynamic-extent  #'write-title))
      (with-conditional-get-response (stream :html
					     :last-modification (ma-cache-time mail-archive)
					     :expires (expiration-universal-time ma-url)
					     :cache-control (url:response-cache-control-directives ma-url)
					     :content-language (languages ma-url))

	(with-html-document (:stream stream)
	  (with-document-preamble (:stream stream)
	    (declare-title title :stream stream))
	  (with-mail-archive-document-body (mail-archive stream)
	    (with-section-heading (#'write-title :stream stream)
	      (msg-write-message mail-archive :archive-info stream)
	      (msg-write-message nil :horizontal-line stream)
	      ;; Main loop that puts up messages
	      (loop for idx upfrom start to end
		    for msg = (ma-get-message mail-archive idx t)
		    do (with-section-heading ((msg-heading msg) :stream stream)
			 (msg-write-message msg view stream)))
	      ;; Put up the archive signature
	      (ma-write-signature mail-archive stream))))))))

;;;------------------------------------------------------------------- 
;;;
;;; DISPATCH METHODS FOR :HEAD AND :GET
;;; 

(defmethod write-document-headers ((url mail-archive-url) (translation (eql :mail-archive)) stream)
  (%write-document-headers url :html stream))

(defmethod write-document-headers ((url url:http-search) (translation (eql :mail-archive)) stream)
  (let ((pathname (url:search-database url)))
    (cond (pathname
           (handler-case
             (multiple-value-bind (length last-modification version)
                 (file-properties pathname)
               (declare (ignore length))        ;we are computing the actual resources so don't send length.
               (%write-document-headers-no-pathname url :html stream nil last-modification version))
             (file-not-found () (error 'document-not-found :url url))))
          (t (error 'document-not-found :url url))))) 

(defmethod write-document ((url mail-archive-url) (translation (eql :mail-archive)) stream)
  (multiple-value-bind (archive newly-created-p)
      (mail-archive url)
    (unless newly-created-p
      (maybe-update-mail-archive archive))
    (with-conditional-get-response (stream :html
                                           :last-modification (ma-cache-time archive)
                                           :expires (expiration-universal-time url)
                                           :cache-control (url:response-cache-control-directives url)
                                           :content-language (languages url))
      (ma-write-summary archive stream (ma-default-view archive)))))

(defgeneric write-mail-archive-message (url stream))

(defmethod write-mail-archive-message ((url url:http-search) stream)
  (flet ((get-message (archive key)
           (let ((designator (if (every #'digit-char-p key)
                                 (parse-integer key :junk-allowed t)
                                 key)))
             ;; If we don't find it, try updating and looking again.  If the
             ;; search key is a message-id, this will currently loose if the
             ;; message-id contains characters that are magic to HTTP
             (when (or (ma-get-message archive designator nil)
                       (prog2 (maybe-update-mail-archive archive)
                              (ma-get-message archive designator nil)))
               designator)))
	 (ma-write-summary-response (url archive stream view)
	   (with-conditional-get-response (stream :html
						  :last-modification (ma-cache-time archive)
						  :expires (expiration-universal-time url)
						  :cache-control (response-cache-control-directives url)
						  :content-language (languages url))
	     (ma-write-summary archive stream view))))
    (declare (inline get-message))
    (let* ((keys (search-keys url))
           (parent (search-parent url))
           key)
      (multiple-value-bind (archive newly-created-p)
          (or (get-value parent :mail-archive)
              (mail-archive (get-value parent :summary-url)))
        (unless newly-created-p
          (maybe-update-mail-archive archive))
        (cond ((null keys)
               (signal 'document-moved-temporarily :url url :new-urls (list (ma-url archive))))
              ((and (cdr keys) (every #'digit-char-p (second keys)))
               (ma-write-message-interval archive (ma-message-format archive) stream
					  (parse-integer (first keys) :junk-allowed t)
					  (parse-integer (second keys) :junk-allowed t)))
              ;; If the search key is the name of a view exported by the archive, show it.
              ((and (stringp (setq key (first keys)))
		    (member key (ma-views archive) :test #'(lambda (x y) (equalp x (symbol-name y)))))
               (maybe-update-mail-archive archive)
               (ma-write-summary-response url archive stream (symbolize key *keyword-package*)))
	      ;; Check for thread references
	      ((and (stringp (setq key (first keys)))
		    (thread-reference-p key)
		    (setq key (ma-get-thread archive (parse-integer key :start 7 :junk-allowed t) nil)))
               (maybe-update-mail-archive archive)
               (ma-write-summary-response url archive stream key))
              ;; Look for the message.
              ((setq key (get-message archive (first keys)))
	       (let ((view (second keys)))
		 (when view
		   (setq view (symbolize view *keyword-package*)))
		 (ma-write-message archive url key view stream)))
              ;; Invoke default method for default view
              (t (maybe-update-mail-archive archive)
		 (ma-write-summary-response url archive stream (ma-default-view archive))))))))


;;;------------------------------------------------------------------- 
;;;
;;; REPLYING TO MESSAGES
;;;

(defparameter *discourse-semantics* '(("Question" . :question) ("Answer" . :answer)
				      ("Agree" . :agree) ("Qualify" . :qualify)
				      ("Alternative" . :alternative) ("Disagree" . :disaggree) ))

(defgeneric ma-discourse-semantics (mail-archive))

(defmethod ma-discourse-semantics ((mail-archive mail-archive))
  (or (get-value mail-archive :discourse-semantics)
      *discourse-semantics*))

(defparameter *default-message-text-box-size* 30)

(defgeneric ma-accept-message (mail-archive to stream &key message personal-name email-address in-reply-to
                                            subject references discourse message-text text-box-rows))

(defmethod ma-accept-message ((mail-archive mail-archive) to stream &key message personal-name email-address
			      in-reply-to subject references discourse message-text (text-box-rows *default-message-text-box-size*))
  (macrolet ((accept-field ((name stream) &body body)
	       `(with-table-row (:stream ,stream)
		  (with-table-cell (:horizontal-alignment :right :stream ,stream)
		    (with-rendition (:bold :stream ,stream)
		      (fast-format stream "~A:" ,name)))
		  (with-table-cell (:horizontal-alignment :left :stream ,stream)
		    ,@body)))
	     (note-hidden-field (field stream)
	       `(when ,field
		  (let ((value (write-to-armor-plated-string ,field)))
		    (declare (dynamic-extent value))
		    (accept-input 'hidden ,(symbol-name field) :default value :stream ,stream)))))
    (flet ((write-heading (stream)
	     (if message
		 (fast-format stream "Post Reply to Message ~D" (msg-index message))
		 (fast-format stream "Post Message to Discussion"))))
      (declare (dynamic-extent #'write-heading))
      (with-section-heading (#'write-heading :stream stream)
	(with-fillout-form (:post (ma-url mail-archive) :stream stream)
	  (accept-input 'hidden "OPERATION" :default "POST-MESSAGE" :stream stream)	;record operation
	  (note-hidden-field in-reply-to stream)	;track state info
	  (note-hidden-field references stream)
	  (note-hidden-field subject stream)
	  (with-centering (:stream stream)
	    (ns4.0:with-table (:border nil :background (ma-box-color mail-archive) :cell-spacing 0 :cell-padding 6 :stream stream)
	      (with-table-row (:stream stream)
		(with-table-cell (:stream stream)
		  (with-table (:border nil :stream stream)
		    (accept-field ("Email Address" stream)
				  (accept-input 'string+ "EMAIL-ADDRESS" :default email-address :size 60 :max-length 240 :stream stream))
		    (accept-field ("Personal Name" stream)
				  (accept-input 'string+ "PERSONAL-NAME" :default personal-name :size 60 :max-length 240 :stream stream))
		    (accept-field ("To" stream)
				  (fast-format stream "~A" to))
		    (accept-field ("Subject" stream)
				  (accept-input 'string+ "NEW-SUBJECT" :default subject :size 60 :max-length 240 :stream stream))
		    (when in-reply-to
		      (accept-field ("In Reply To" stream)
				    (if message
					(with-anchor-noted (:reference (msg-url-string message) :stream stream)
					  (fast-format stream "Message ~D" (msg-index message)))
					(loop for (msg-id . more) = in-reply-to then more
					      do (write-string-quoting-specials msg-id stream)
						 while more
						   do (write-string ", " stream))))
		      (accept-field ("Discourse" stream)
				    (accept-input 'radio-button "RELATION" :choices (ma-discourse-semantics mail-archive)
						  :default discourse :linebreaks nil :stream stream)))
		    (accept-field ("Action" stream)	;Put up send and clear buttons.
				  (accept-input 'submit-button "SUBMIT" :display-string "Send" :stream stream))
		    (with-table-row (:stream stream)	;accept the message body
		      (with-table-cell (:column-span 2 :stream stream)
			(flet ((write-msg (stream)
				 (cond (message (msg-write-message-data message :reply-text stream))
				       (message-text (write-string message-text stream)))))
			  (declare (dynamic-extent #'write-msg))
			  (accept-input 'multi-line-text "MESSAGE" :colunms 72 :rows text-box-rows :stream stream
					:default (cond (message-text)
						       (message #'write-msg)
						       (t nil))))))))))))))))

(defconstant *ma-user-email-address-cookie-persistence* (* 60 60 24 90))	;save for 90 days

(defgeneric ma-user-email-address-cookie-headers (mail-archive email-address personal-name))

(defmethod ma-user-email-address-cookie-headers ((mail-archive mail-archive) email-address personal-name)
  (when (or email-address personal-name)
    (multiple-value-bind (current-user-agent current-user-agent-version)
	(current-user-agent)
      (when (user-agent-capability-p :cookies current-user-agent current-user-agent-version)
	(let* ((expiration (+ (get-universal-time) *ma-user-email-address-cookie-persistence*))
	       (url (ma-url mail-archive))
	       (domain (host-string url))
	       (path (relative-path-string url))
	       (headers nil))
	  (cond-every
	    (personal-name
	      (setq headers `(:set-cookie ,(make-set-cookie-header-value
					     :user-personal-name (write-to-armor-plated-string personal-name)
					     :expires expiration :domain domain :path path))))
	    (email-address
	      (setq headers `(:set-cookie ,(make-set-cookie-header-value
					     :user-email-address (write-to-armor-plated-string email-address)
					     :expires expiration :domain domain :path path)
			      ,. headers))))
	  (return-from ma-user-email-address-cookie-headers headers))))))

(defgeneric ma-post-message-report-success (mail-archive to stream email-address personal-name))

(defmethod ma-post-message-report-success ((mail-archive mail-archive) to stream email-address personal-name)
  (let ((headers (ma-user-email-address-cookie-headers mail-archive email-address personal-name)))
    (declare (dynamic-extent headers))
    (with-successful-response (stream :html :status :success :cache-control :no-cache :additional-headers headers)
      (with-html-document (:stream stream)
	(with-document-preamble (:stream stream)
	  (declare-title "Message Sent Successfully" :stream stream))
	(with-mail-archive-document-body (mail-archive stream)
	  (with-section-heading ("Message Sent Successfully" :stream stream)
	    (horizontal-line :stream stream)
	    (with-emphasis (:quotation :stream stream)
	      (fast-format stream "Thank you for your submission.")
	      (fast-format stream "Your message was sent successfully to ~A on " to)
	      (print-gmt-time stream)
	      (write-char #\. stream))
	    (horizontal-line :stream stream)
	    (ma-write-signature mail-archive stream)))))))

(defgeneric ma-post-message-report-failure (mail-archive stream &optional error))

(defmethod ma-post-message-report-failure ((mail-archive mail-archive) stream &optional error)
  (with-successful-response (stream :html :status :success :cache-control :no-cache)
    (with-html-document (:stream stream)
      (with-document-preamble (:stream stream)
	(declare-title "Message Not Sent" :stream stream))
      (with-mail-archive-document-body (mail-archive stream)
	(with-section-heading ("Message Not Sent" :stream stream)
	  (horizontal-line :stream stream)
	  (cond (error
		 (with-emphasis (:quotation :stream stream)
		   (fast-format stream "Your message was not sent because of errors sending email. Please try again later.")
		   (with-paragraph (:stream stream)
		     (with-rendition (:bold :stream stream)
		       (fast-format stream "Error: "))
		     (fast-format stream "~S" (type-of error))
		     (with-verbatim-text (:fresh-line nil :stream stream)
		       (write-string-quoting-specials (report-string error) stream)))))
		(t (with-emphasis (:quotation :stream stream)
		     (fast-format stream "Your message was not sent because it had incorrect or incomplete fields.
                                               All posting must carry a valid email address so people can reply to you.
                                               Please use the back button on your browser and try again."))))
	  (horizontal-line :stream stream)
	  (ma-write-signature mail-archive stream))))))

(defgeneric ma-post-message (mail-archive stream form-alist))

(defmethod ma-post-message ((mail-archive mail-archive) stream form-alist)
  (flet ((unsafe-chars-p (string)
	   (find-if #'(lambda (ch) (member ch '(#\. #\, #\; #\: #\" #\' #\`) :test #'eql)) string)))
    (declare (inline unsafe-chars-p))
    (bind-query-values (email-address personal-name subject new-subject in-reply-to references discourse message)
		       (url form-alist)
      (let* ((personal-name (when personal-name (string-trim `(#\space #\tab) personal-name)))
	     (email-address (when email-address (string-trim `(#\space #\tab) email-address)))
	     (from (if personal-name
		       (format nil "~:[~A~;~S~] <~A>" (unsafe-chars-p personal-name) personal-name email-address)
		       email-address))
	     (to (ma-mailing-list mail-archive))
	     (subj (cond ((useless-subject-p new-subject)
			  (if subject
			      (read-from-armor-plated-string subject)
			      +message-no-subject-string+))
			 (t new-subject)))
	     (text message)
	     (reply-to (when in-reply-to (read-from-armor-plated-string in-reply-to)))
	     (references (when references (read-from-armor-plated-string references)))
	     (disc (when discourse (list discourse)))
	     (comment (fast-format nil "Posted from ~A via CL-HTTP HyperArchive at ~A"
				   (server-host-ip-address *server*) (name-string (ma-url mail-archive)))))
	(cond ((and (valid-internet-mail-address-p email-address)
		    (valid-internet-mail-address-p to)
		    text)
	       (handler-case-if (not *debug-server*)
		  (let ((*debug-server* t))
		    (send-mail-from from to subj text :keywords disc :comments comment
				    :additional-headers `(,.(when reply-to `(:in-reply-to ,reply-to))
							  ,.(when references `(:references ,references))))
		    (ma-post-message-report-success mail-archive to stream email-address personal-name))
		 (error (err) (ma-post-message-report-failure mail-archive stream err))))
	      (t (ma-post-message-report-failure mail-archive stream)))))))

(defgeneric ma-execute-form-operation (operation url stream form-alist)
  (:documentation "Standard form dispatch for mail archives.
Specialize new operations on OPERATION."))

(defmethod ma-execute-form-operation ((operation (eql :show-message)) url stream form-alist)
  (bind-query-values (view index) (url form-alist)
    (let ((view (symbolize view *keyword-package*))
	  (key (parse-integer index :junk-allowed t)))
      (ma-write-message (mail-archive url) url key view stream))))

(defmethod ma-execute-form-operation ((operation (eql :post-message)) url stream form-alist)
  (ma-post-message (mail-archive url) stream form-alist))

(defmethod ma-execute-form-operation (operation url stream form-alist)
  (declare (ignore url stream form-alist))
  (error "The operation, ~A, is not defined on this URL." operation))

(defgeneric ma-dispatch-form-operation (url stream form-alist)
  (:documentation "Standard response function for values posted to URL."))

(defmethod ma-dispatch-form-operation ((url mail-archive-url) stream form-alist)
  (bind-query-values (operation) (url form-alist)
    (ma-execute-form-operation (symbolize operation *keyword-package*) url stream form-alist)))


;;;------------------------------------------------------------------- 
;;;
;;; EXPORTS
;;; 

(defmethod url:initialize-specialization ((url mail-archive-url) class init-args)
  (macrolet ((set-param (url param)
               (let ((key (symbolize param *keyword-package*)))
                 `(if ,param
                      (setf (get-value ,url ,key) ,param)
                      (remove-value ,url ,key)))))
    (url::with-class-change-for-initialize-specialization (url class init-args)
      (destructuring-bind (&key search-url mailing-list
				(title (name-string url)) (message-heading "Message")
				(default-view *mail-archive-default-view*)
				views 
				(mail-file-format *mail-file-format*)
				(mail-archive-class *mail-archive-class*)
				(message-format *message-format*)
				(message-class *message-class*)
				(thread-class *thread-class*)
				(thread-default-view *mail-archive-thread-default-view*)
				(response-function #'ma-dispatch-form-operation)
				background-url background foreground reply (box :Grey-Light-Very)
				link visited-link active-link
				&allow-other-keys) init-args
	(let ((valid-views (mail-archive-valid-summary-views mail-archive-class t)))	;update vie cache on export
	  (unless-every
	    ((member default-view valid-views)	;update vie cache on export
	     (error "DEFAULT-VIEW is ~S, is not one of the known summary views ~S."
		    default-view valid-views))
	    ((and response-function (good-response-function-p response-function))
	     (error "RESPONSE-FUNCTION, ~S, must be a defined function when exporting the URL, ~S, with translation, ~S"
		    response-function url :mail-archive)))
	  (if views
	      (loop for view in views
		    unless (member view valid-views)
		      do (error "The view, ~S, is not one of the known summary views ~S."
				view valid-views))
	      (setq views valid-views))
	  (ma-set-document-color-scheme url :background-url background-url
					:background background :foreground foreground :reply reply :box box
					:link link :visited-link visited-link :active-link active-link)
          ;; Add other init arguments afterwards.
          (set-param url search-url)
	  (set-param url mailing-list)
          (set-param url title)
          (set-param url mail-file-format)
          (set-param url default-view)
          (set-param url views)
          (set-param url message-heading)
          (set-param url message-format)
          (set-param url mail-archive-class)
          (set-param url message-class)
	  (set-param url thread-class)
	  (set-param url thread-default-view)
	  (setf (response-function url) response-function)
          url)))))

;; Exports a summary and lets you select individual messages.
(defmethod export-url ((url url:http-object) (export-type (eql :mail-archive)) &rest args)
  (destructuring-bind (&key search-url recache-p &allow-other-keys) args
    (let* ((search-url-string (etypecase search-url
                                (null (concatenate 'string (url::name-string-without-extension url) "?"))
                                (string (merge-url search-url (local-context)))
                                (http-search (url:name-string search-url))))
           (search-url (typecase search-url
                         (http-search search-url)
                         (t (intern-url search-url-string :if-does-not-exist :create))))
           (n-args `(,.args :search-url ,search-url)))
      (declare (dynamic-extent search-url))
      (%export-url-object url export-type args)
      (prog1
        (url:initialize-specialization url 'mail-archive-url n-args)
        (apply #'export-url search-url :mail-archive :summary-url url args)
        ;; create the mail archive cache
        (when recache-p
          (using-resource (*server-line-buffer* line-buffer *line-buffer-size*)
            (mail-archive url t)))))))

(defmethod unintern-url :before ((url mail-archive-url))
  (let ((search-url (or (get-value url :search-url)
                        (url:intern-url (concatenate 'string (url::name-string-without-extension url) "?")
                                        :if-does-not-exist :error))))
    (unintern-url search-url)))

(defmethod export-url ((url url:http-search) (translation (eql :mail-archive)) &rest args)
  (macrolet ((set-param (url param)
               (let ((key (symbolize param *keyword-package*)))
                 `(if ,param
                      (setf (get-value ,url ,key) ,param)
                      (remove-value ,url ,key)))))
    (destructuring-bind (&key pathname  summary-url &allow-other-keys) args
      (unless pathname
        (error "No pathname was provided while exporting the url, ~S, with method, ~S"
               url translation))
      (set-param url summary-url)
      (%export-url-search url translation #'write-mail-archive-message
                          #'url:standard-parse-search-info #'url:standard-write-search-info
                          (translated-pathname pathname)))))

(defvar *mail-archive-urls* nil)

(defun mail-archive-urls (&optional recache-p &aux result)
  (flet ((collect (key value)
	   (declare (ignore key))
	   (typecase value
	     (mail-archive-url
	       (push-ordered value result #'string< :key #'(lambda (url)
							     (get-value url :title)))))))
    (declare (dynamic-extent #'collect))
    (cond ((or (null *mail-archive-urls*) recache-p)
	   (map-url-table #'collect)
	   (setq *mail-archive-urls* result))
	  (t *mail-archive-urls*))))

;; Use this function to provide an index of mail archives.
(defun write-mail-archive-directory (url stream)
  (declare (ignore url))
  (with-successful-response (stream :html)
    (with-html-document (:stream stream)
      (with-document-preamble (:stream stream)
	(declare-title "Mail HyperArchives" :stream stream))
      (With-standard-document-body (:stream stream)
	(with-section-heading ("Mail HyperArchives" :stream stream)
	  (horizontal-line :stream stream)
	  (with-enumeration (stream :definition)
	    (loop for url in (mail-archive-urls)
		  for title = (get-value url :title)
		  for documentation = (get-value url :description)
		  do (flet ((write-heading (stream)
			      (note-anchor title :reference url :stream stream)))
		       (enumerating-item (stream :head #'write-heading)
			 (when documentation
			   (write-string documentation stream))))))
	  (horizontal-line :stream stream)
	  (cl-http-signature stream))))))


;;;------------------------------------------------------------------- 
;;;
;;; EXAMPLES
;;;

#|
 
(defparameter *www-cl-documentation* "<p>The public discussion at <a
href=\"mailto:www-cl@ai.mit.edu\">WWW-CL@ai.mit.edu</a> covers all aspects of
the <a
href=\"http://www.ai.mit.edu/projects/iiip/doc/cl-http/home-page.html\">Common
Lisp Hypermedia Server</a>, the <a
href=\"http://www.ai.mit.edu/people/cvince/papers/w3p-paper.html\">W3P Presentation
System</a>, the Basic HTTP Client, the <a
href=\"http://www.ai.mit.edu/projects/iiip/doc/cl-http/w4/w4-abstract.html\">W4 Web Walker</a>, and
related facilities. It also includes general issues related to programming
World Wide Web applications in Lisp with special emphasis on Artificial Intelligence</p>")

(defparameter *www-cl-current-archive* "<p>The current archive is <a
href=\"http://wilson.ai.mit.edu/cl-http/archives/www-cl.html\">
http://wilson.ai.mit.edu/cl-http/archives/www-cl.html</a>.</p>")

(defparameter *www-cl-other-archives* "<b>Browse Earlier Archives</b>

<ul>
<li><a href=\"http://wilson.ai.mit.edu/cl-http/archives/www-cl-1996-12-30.html\">June through December, 1996</a>
<li><a href=\"http://wilson.ai.mit.edu/cl-http/archives/www-cl-1996-06-01.html\">January to June, 1996</a>
</ul>")

(defparameter *www-cl-subscribe* "<b>Join the Discussion</b>
<ul>

<li><b>Subscribe</b> by sending email to the listserve:
	<dl>
		<dt><i>To:</i> <a href=\"mailto:www-cl-request@ai.mit.edu?subject=subscribe\">
		<b>www-cl-request@ai.mit.edu</b></a>
		<dt><i>Subject:</i> <b>subscribe</b>
	</dl>

<li><b>Unsubscribe</b> by sending email to the listserve:
	<dl>
		<dt><i>To:</i> <a href=\"mailto:www-cl-request@ai.mit.edu?subject=unsubscribe\">
		<b>www-cl-request@ai.mit.edu</b></a>
		<dt><i>Subject:</i> <b>unsubscribe</b>
	</dl>
</ul>")

(http:export-url "http://wilson.ai.mit.edu/cl-http/archives/www-cl.html"
		 :mail-archive
		 :mailing-list "WWW-CL@ai.mit.edu"
		 :pathname "wilson:>http>mail>WWW-CL.text"
		 :title "World Wide Web and Common Lisp"
		 :documentation (format nil "~A<center><table border=0 cellspacing=0 cellpadding=4>~
                                             <tr valign=top><td width=50%><font size=2>~A</font></td>~
                                                            <td width=50%><font size=2>~A</font></td>~
                                              </tr></table></center>"
					*www-cl-documentation* *www-cl-other-archives* *www-cl-subscribe*)
		 :mail-file-format :lispm
		 :recache-p t
		 :default-view :conversation
		 :reply :maroon
		 :background :white
		 :foreground :black
		 :link :blue
		 :visited-link :red
		 :active-link :pink-neon)

|#
