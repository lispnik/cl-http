;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.20
;;; Reason: Function URL:URL-POSITION:  fix bug.
;;; Remove function HTTP::WRITE-HEADER-BUFFER: undefine.
;;; Function HTTP::WRITE-HEADER-BUFFER:  add optional argument termination-line-p.
;;; Function (CLOS:METHOD HTTP::WRITE-HEADER-BUFFER (HTTP::HEADER-SET T)):  implement.
;;; Function (CLOS:METHOD HTTP::INVOKE-SERVER-METHOD (HTTP::BASIC-SERVER-MIXIN (EQL :TRACE) (EQL :HTTP/1.1))):  use non-consing method to write headers.
;;; Function HTTP::%GET-MODIFICATION:  -
;;; Function HTTP::WRITE-MODIFIED-HEADERS:  new.
;;; Written by JCMa, 11/12/99 18:32:49
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.7, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.19,
;;; W3 Presentation System 8.0, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 41, HTTP Proxy Server 4.0,
;;; HTTP Client Substrate 3.0, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.11, DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x994 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189585,
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.24),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;URL.LISP.374"
  "HTTP:SERVER;HEADERS.LISP.435"
  "HTTP:SERVER;SERVER.LISP.810"
  "HTTP:SERVER;HEADERS.LISP.437"
  "HTTP:SERVER;HEADERS.LISP.438")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.374")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defun url-position (string &optional (start 0) (end (length string)))
  "Locates the first URL in STRING between START and END.
Returns the START-URL and END-URL indices or nil when no URL is found."
  (declare (values start-url end-url))
  (flet ((scheme-p (string start end)
	   ;; this could be compiled out for more speed.  2/13/97 -- JCMa.
	   (loop for (scheme) in *scheme-parsers*
		 for end1 = (length scheme)
		 for start2 = (- (the fixnum end) (the fixnum end1))
		 when (and (<= start start2)
			   (string-equal scheme string :start1 0 :end1 end1
					 :start2 start2 :end2 end))
		   do (return (values scheme start2))
		 finally (return nil)))
	 (url-delimiter-p (char)
	   (member char '(#\space #\newline #\< #\>) :test #'eql))
	 (trim-p (char)
	   (member char '(#\. #\! #\: #\; #\, #\+ #\= #\@ #\$ #\% #\* ) :test #'eql))
	 (good-scheme-p (scheme string start end)
	   (cond ((member scheme '("mailto" "news"  "telnet") :test #'equalp) 
		  t)
		 (t (let ((pos (1+ (the fixnum start))))
		      (and (< pos end) (eql #\/ (aref string pos))))))))
    (declare (inline scheme-p url-delimiter-p trim-p good-scheme-p))
    (let ((pos (char-position #\: string start end)))
      (when pos
	(multiple-value-bind (scheme url-start)
	    (scheme-p string start pos)         ;Ensure that we have a defined URL scheme
	  (cond
	    (scheme
	     (with-fast-array-references ((string string string))
	       ;; don't get confused by package prefixes
	       (cond ((good-scheme-p scheme string pos end) 
		      ;; back down any number of trimmable characters
		      (loop with url-end = (loop for idx upfrom url-start below end
						 when (url-delimiter-p (aref string idx))
						   return idx
						 finally (return end))
			    for e1 downfrom url-end above pos
			    unless (trim-p (aref string (setq e1 (1- (the fixnum url-end)))))
			      do (return-from url-position (values url-start url-end))
			    else do (setq url-end e1)
			    while (< pos url-end)
			    finally (return-from url-position nil)))
		     (t (url-position string (1+ (the fixnum pos)) end)))))
	    (t (url-position string (1+ (the fixnum pos)) end))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.435")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(SCL:FUNDEFINE 'WRITE-HEADER-BUFFER)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.435")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-generic write-header-buffer (header-set stream &optional termination-line-p)
  (:documentation "Writes the raw header buffer associated with header-set to stream.
When termination-line-p is non-null, a blank line is transmitted to terminate the headers."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.435")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod write-header-buffer ((header-set header-set) stream &optional termination-line-p)
  (with-fast-array-references ((buffer (%header-set-buffer header-set) string)
			       (line-ends (%header-set-line-ends header-set) vector))
    (loop for start = 0 then end
	  for idx upfrom 0 below (fill-pointer line-ends)
	  for end = (aref line-ends idx)
	  do (write-line buffer stream :start start :end end))
    (when termination-line-p
      (send-cr-line-feed stream))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.810")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod invoke-server-method ((server basic-server-mixin) (method (eql :trace)) (http-version (eql :http/1.1)))
  (with-slots (stream url-string headers) server
    (let ((url (url:intern-url url-string :if-does-not-exist :soft)))
      (cond ((or (and url (translation-method url))
		 (and *auto-export* (auto-export-pathname-url url-string)))
	     (setf (server-url server) url) 
	     (with-access-control
	       (url method server (or (url:secure-subnets url) *secure-subnets*)
		    :deny-subnets *disallowed-subnets*)
	       ;; this is the meat of the trace response
	       (with-chunked-transfer-encoding
		 (stream '(:message :http) :status :success :content-location url :cache-control '(:no-cache t))
		 (write-header-buffer headers stream t))))
	    (t (error 'document-not-found :url url-string :method :trace))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.437")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod write-modified-headers :around (header-set stream &optional modification-plist excluded-headers termination-line-p additional-headers)
  (declare (ignore header-set modification-plist excluded-headers))
  (call-next-method)
  (loop for (header-name header-value) on additional-headers by #'cddr
	do (%write-header header-name header-value stream))
  (when termination-line-p
    (send-cr-line-feed stream)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.438")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmacro %get-modification (modification-plist header)
  `(loop with reset-plist-p = t
	 for ptr = ,modification-plist then (cddr ptr)
	 while ptr
	 do (if (eq ,header (car ptr))
		(return (values (prog1 (second ptr)
				       (if reset-plist-p
					   (setf ,modification-plist (cddr ptr))
					   (setf ptr (cddr ptr))))
				,header))
		(and reset-plist-p (setq reset-plist-p nil)))
	 finally (return nil)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.438")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-generic write-modified-headers (header-set stream &optional modification-plist excluded-headers termination-line-p additional-headers)
  (:documentation "Writes the raw header buffer associated with HEADER-SET to STREAM.
MODIFICATION-PLIST is a property list of HEADER-KEYWORD HEADER-VALUE that
supplies new values for any existing headers. modification-plist is
destructively modified and should be consed on the stack by callers.  When
header-set does not contain these headers, they are appended to the headers
written. Note that MODIFICATION-PLIST is destructively modified by this
operation.  EXCLUDED-HEADERS is a list of headers that should not be
transmitted.  When TERMINATION-LINE-P is non-null, a blank line is transmitted
to terminate the headers.  ADDITIONAL-HEADERS is a list of ancillary headers
that are appended to the transmitted headers."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.438")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod write-modified-headers ((header-set header-set) stream &optional modification-plist excluded-headers termination-line-p additional-headers)
  (declare (ignore termination-line-p additional-headers))
    (%with-header-set-index (header-set)
      (with-fast-array-references ((index index vector)
				   (headers headers vector))
	(loop with header-object
	      for idx fixnum upfrom 0 below (fill-pointer headers)
	      for header-name = (aref index idx)
	      unless (member header-name excluded-headers)
		do (multiple-value-bind (new-value found-p)
		       (%get-modification modification-plist header-name)
		     (cond (found-p
			    (%write-header header-name new-value stream))
			   ((%header-suppress-p (setq header-object (aref headers idx))))
			   (t (write-header header-name header-object stream)))))))
    ;; send any remaining headers
    (loop for (header-name header-value) on modification-plist by #'cddr
	  do (%write-header header-name header-value stream)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.438")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod write-modified-headers ((header-plist cons) stream &optional modification-plist excluded-headers termination-line-p additional-headers)
  (declare (ignore termination-line-p additional-headers))
    (loop for (header-name header-value) on header-plist by #'cddr
	  unless (member header-name excluded-headers)
	    do (multiple-value-bind (new-value found-p)
		   (%get-modification modification-plist header-name)
		 (%write-header header-name (if found-p new-value header-value) stream)))
    ;; send any remaining headers
    (loop for (header-name header-value) on modification-plist by #'cddr
	  do (%write-header header-name header-value stream)))
