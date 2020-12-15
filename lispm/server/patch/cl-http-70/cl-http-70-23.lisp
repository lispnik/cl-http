;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Base: 10; Package: http; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.23
;;; Reason: Function HTTP::PARSE-COOKIE-HEADER:  robustify.
;;; Function TK1::%GET-OR-PUT-TOKEN:  no tokenization when no data.
;;; Function TQ::TASK-QUEUE-NEXT:  new
;;; Function (CLOS:METHOD TQ::TASK-QUEUE-NEXT (TQ:TASK-QUEUE)):  implement.
;;; DEFINE-CONDITION HTTP::REQUEST-MISSING-CONTENT-TYPE-HEADER:  new condition.
;;; Function (CLOS:METHOD HTTP:POST-DOCUMENT (URL:FORM-PROCESSING-MIXIN NULL NULL T)):  handle null content-type header in post.
;;; Function (CLOS:METHOD HTTP:POST-DOCUMENT (URL:FORM-PROCESSING-MIXIN T T T)):  use unsupported media condition.
;;; Function (CLOS:METHOD HTTP::INVOKE-SERVER-METHOD (HTTP::BASIC-SERVER-MIXIN (EQL :POST) SYMBOL)):  -
;;; Written by JCMa, 12/14/99 19:32:41
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.8, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.22,
;;; W3 Presentation System 8.0, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 41, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.11, DEC OSF/1 V4.0 (Rev. 110),
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
  "HTTP:SERVER;HEADERS.LISP.443"
  "HTTP:SERVER;TOKENIZER.LISP.37"
  "HTTP:SERVER;TASK-QUEUE.LISP.25"
  "HTTP:SERVER;HTTP-CONDITIONS.LISP.171"
  "HTTP:SERVER;SERVER.LISP.812")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.443")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun parse-cookie-header (string &optional (start 0) (end (length string)) error-p)
  (flet ((get-entry-indices (string start end)
	   (with-fast-array-references ((string string string))
	     (loop with key-delimiter
		   for idx upfrom start below end
		   for char = (aref  string idx)
		   do (case char
			(#\= (setq key-delimiter idx))
			(#\; (return (values key-delimiter idx))))
		   finally (return (values key-delimiter end))))))
    (declare (inline get-entry-indices))
    (with-string-trim-bounds (*white-space-chars* string start end)
      (loop with e1 and e2 and s2
	    for s = start then (1+ (the fixnum e2))
	    while (< s end)
	    for s1 = (position-if-not* #'white-space-char-p string :start s :end end)
	    while s1
	    do (multiple-value-setq (e1 e2) 
		 (get-entry-indices string s1 end))
	    when (and e1 (< s1 e1) (setq s2 (1+ (the fixnum e1))) (< s2 e2))
	      collect (%intern-header-keyword-value string s1 e1)
	      and collect (subseq string s2 e2)
	    else do (when error-p
		      (error 'bad-cookie-header-value
			     :format-string "No value for Cookie header: ~S"
			     :format-args (list (subseq string s1 e2))))))))

(define-header :cookie (:header :request)
  :print-string "Cookie"
  :parse-function 'parse-cookie-header
  :print-function 'print-cookie-header) 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TOKENIZER.LISP.37")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (tk1 use future-common-lisp :colon-mode :external); BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun %get-or-put-token (tokenizer string start end)
  (let ((*tokenizer* tokenizer)
	(top-level (tokenizer-top-level tokenizer)))
    (if top-level
	(unless (= start end)
	  (with-fast-array-references ((string string string))
	    (loop with previous-level = top-level
		  and level = top-level
		  and test = (tokenizer-test tokenizer)
		  for idx upfrom start below end
		  while level
		  for char = (aref string idx)
		  for entry = (find-entry char level test)
		  when entry
		    do (psetq previous-level level
			      level (entry-level entry))
		  else
		    do (return-from %get-or-put-token (values (%put-token level string test idx start end) t))
		  finally (return-from %get-or-put-token
			    (cond ((= idx end)
				   (etypecase entry
				     (value-entry
				       (value-entry-value entry))
				     (entry
				       (values (%set-value-entry entry (tokenize-value string start end) previous-level test)
					       t))))
				  (entry
				   (multiple-value-bind (new-entry token)
				       (%make-level-entry string idx start end)
				     (setf (entry-level entry) (make-level :entries (list new-entry)))
				     (values token t)))
				  (t (error "Unreachable clause.")))))))
	(multiple-value-bind (new-entry token)
	    (%make-level-entry string start start end)
	  (setf (tokenizer-top-level tokenizer) (make-level :entries (list new-entry)))
	  (values token t)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.25")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defgeneric task-queue-next (task-queue)
   (declare (values first-entry))
   (:documentation "Gets the first without removing it from the queue  with inter-process locking."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.25")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

(defmethod task-queue-next ((task-queue task-queue))
  (with-lock-held ((task-queue-lock task-queue) :read "Task Queue Next")
    (car (task-queue-queue task-queue))))

(eval-when (load eval) (export (intern "TASK-QUEUE-NEXT" :tq) :tq))
;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.171")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition request-missing-content-type-header
                  (bad-syntax-provided)
  ((reason :initform "Bad Request: Missing Content-Type Header")))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.812")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod post-document ((url url:form-processing-mixin) type subtype stream)
  (declare (ignore stream))
  (error 'unsupported-media-type :url url :close-connection t
         :format-string "Unsupported Media Type: POST method with the MIME type ~A/~A is not implemented."
         :format-args (list type subtype)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.812")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod post-document ((url url:form-processing-mixin) (type null) (subtype null) stream)
  (declare (ignore stream))
  (error 'request-missing-content-type-header :url url :close-connection t))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.812")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod invoke-server-method ((server basic-server-mixin) (method (eql :post)) (http-version symbol) &aux translation-method)
  (macrolet ((handle-redirect (condition tag)
               `(destructuring-bind (target-url &rest other-urls) (new-urls ,condition)
                  (cond ;; optimize redirect by reinvoking for singleton local url.
                    ((and (null other-urls) (local-redirect-p target-url))
                     (setf (server-url-string server) (url:name-string target-url))
                     (go ,tag))
                    (t (report-status ,condition stream))))))
    (with-slots (stream url-string) server
      (tagbody
        retry1
           (handler-case
             (destructuring-bind (&optional doc-type doc-subtype &rest args) (get-header :content-type)
               (declare (ignore args))
	       (multiple-value-bind (url)
		   (url:intern-url url-string :if-does-not-exist :soft)
		 (tagbody
		   retry1
		      (cond ((and url (setq translation-method (translation-method url)))
			     (setf (server-url server) url)
			     (with-access-control (url method server (or (url:secure-subnets url) *secure-subnets*)
						       :deny-subnets *disallowed-subnets*)
			       (case translation-method
				 ((:redirect :temporary-redirect)	; redirect when there is forwarding.
				  (handle-url-standard-redirection
				    url (eql translation-method :temporary-redirect) :post))
				 (t (case http-version
				      ((:http/0.9 :http/1.0))
				      ;; alert HTTP 1.1 or greater clients that we are ready
				      (t (report-status-continue stream)
					 (send-cr-line-feed stream)
					 (force-output stream)
					 (setf (server-status server) 100.)))
				    ;; Upgrade this when reading chunked encodings is available. 7/24/96 -- JCMa.
				    (let ((transfer-encoding (get-header :transfer-encoding)))
				      (when transfer-encoding
					(error 'server-not-implemented :close-connection t :url url :method :post
					       :format-string "The HTTP transfer encoding, ~A, is not implemented."
					       :format-args (list transfer-encoding)))
				      (post-document url doc-type doc-subtype stream))))))
			    ((setq url (locate-controlling-url url-string method (url:valid-search-url-p url-string)))
			     (go retry1))
			    (t (error 'document-not-found :url url-string :method :post :close-connection t))))))
             (redirection (cond) (handle-redirect cond retry1)))))))

