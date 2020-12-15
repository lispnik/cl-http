;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-

;;; (C) Copyright 1994-99, John C Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; PARSING UNIVERSAL RESOURCE LOCATORS (URLS)
;;;

(in-package :url) 

(define-constant *search-url-delimiter* #\?)

(define-constant *search-url-search-item-delimiter* #\+)

;; I'm afraid to set this to NIL due to the danager of browser lossage.
(define-parameter *downcase-url-strings* nil
                  "Defaultedly downcases URL string when non-null.")

(define-parameter *escape-search-urls* t
                  "Controls whether the suffix of a search URLs is escaped on intern and on generation.") 

(define-parameter *retain-original-name-string* t
                  "Controls whether the original URL namestring is retained when creating new URLs
or computed from the parsed representation.") 

;;;------------------------------------------------------------------- 
;;;
;;; TOP LEVEL CONDITIONS
;;;

(define-condition url-condition ()())

(define-condition url-error (url-condition) ())

(define-condition parsing-error
                  (url-condition)
  ((url-string :initform nil :initarg :url-string :reader url-string))
  (:report (lambda (condition stream)
             (format stream "Error parsing the URL, ~S."
                     (url-string condition)))))

(define-condition data-type-error
                  (url-error)
  ((url-object :initarg :url :reader url-object)
   (format-string :initarg :format-string :reader format-string)
   (format-args :initarg :format-args :reader format-args))
  (:report (lambda (condition stream)
             (apply #'format stream (format-string condition)
                    (format-args condition)))))


;;;------------------------------------------------------------------- 
;;;
;;; URL PARSING CONDITIONS
;;;

(define-condition unknown-uri (url-error) ())

(define-condition no-interned-url-found
                  (parsing-error unknown-uri)
  ((url-string :initarg :url-string :reader url-string))
  (:report (lambda (condition stream)
             (format stream "No interned object found for the URL, ~S."
                     (url-string condition)))))

(define-condition no-search-parent-found
                  (parsing-error)
  ((url :initarg :url :reader url-object))
  (:report (lambda (condition stream)
             (format stream "No search parent found for the URL, ~S."
                     (coerce-url-string (url-object condition))))))

(define-condition interned-url-already-using-name
                  (parsing-error)
  ((interned-url :initarg :interned-url :reader interned-url)
   (uninterned-url :initarg :uninterned-url :reader uninterned-url))
  (:report (lambda (condition stream)
             (format stream "An interned object, ~S, is already registered under the same name as, ~S."
                     (interned-url condition) (uninterned-url condition)))))

(define-condition scheme-parsing-error (parsing-error) ())

(define-condition no-scheme-found
                  (scheme-parsing-error)
  ()
  (:report (lambda (condition stream)
             (format stream "No protocol scheme found in the URL, ~S."
                     (url-string condition)))))

(define-condition no-parser-for-scheme
                  (scheme-parsing-error)
  ()
  (:report (lambda (condition stream)
             (format stream "No parser defined for protocol scheme in the URL, ~S."
                     (url-string condition)))))

(define-condition bad-data-type-for-scheme
                  (scheme-parsing-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Bad data type for datum, ~S, in this parser scheme."
                     (url-string condition)))))

(define-condition host-parsing-error (parsing-error) ())

(define-condition no-host-found
                  (host-parsing-error)
  ()
  (:report (lambda (condition stream)
             (format stream "No host specification found in the URL, ~S."
                     (url-string condition)))))

(define-condition bad-host-port-specification
                  (host-parsing-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Host port was improperly specified in the URL, ~S."
                     (url-string condition)))))

(define-condition superfluous-port-specification
                  (host-parsing-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Host port specification not allowed in the URL, ~S."
                     (url-string condition)))))

(define-condition bad-host-name-specification
                  (host-parsing-error)
  ((host :initarg :host :reader host))
  (:report (lambda (condition stream)
             (format stream "The host name, ~S, was improperly specified."
                     (host condition)))))

(define-condition wais-parsing-error (parsing-error) ())

(define-condition bad-wais-database (wais-parsing-error) ())

(define-condition no-wais-database
                  (bad-wais-database)
  ()
  (:report (lambda (condition stream)
             (format stream "No database specified in WAIS URL, ~S."
                     (url-string condition)))))

(define-condition path-components-in-wais-database
                  (bad-wais-database)
  ()
  (:report (lambda (condition stream)
             (format stream "WAIS database contains path components in WAIS URL, ~S."
                     (url-string condition)))))

(define-condition no-wais-type
                  (wais-parsing-error)
  ()
  (:report (lambda (condition stream)
             (format stream "No WAIS type specified in WAIS URL, ~S."
                     (url-string condition)))))

(define-condition bad-wais-size (wais-parsing-error) ())

(define-condition no-wais-size
                  (bad-wais-size)
  ()
  (:report (lambda (condition stream)
             (format stream "No WAIS version specified in WAIS URL, ~S."
                     (url-string condition)))))

(define-condition non-numeric-wais-size
                  (bad-wais-size)
  ()
  (:report (lambda (condition stream)
             (format stream "Non-numeric WAIS version specified in WAIS URL, ~S."
                     (url-string condition)))))


;;;------------------------------------------------------------------- 
;;;
;;; DATA CONVERSION CONDITIONS
;;;

(define-condition data-type-operation-undefined (data-type-error) ())


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defvar *scheme-parsers* nil)

(defvar *scheme-maximum-length* 5
  "The maximum length of a URL scheme, not including the delimiting colon.")

(declaim (fixnum *scheme-maximum-length*))

(defun update-scheme-maximum-length ()
  (setq *scheme-maximum-length* (loop for (scheme) in *scheme-parsers*
                                      maximize (length scheme))))

(defun register-scheme (scheme parser)
  (setq scheme (string-downcase scheme))
  (let ((entry (assoc scheme *scheme-parsers* :test #'string-equal)))
    (cond (entry
           (setf (cdr entry) parser))
          (t (setq *scheme-parsers* (nconc *scheme-parsers* `((,scheme . ,parser))))))
    (update-scheme-maximum-length)
    *scheme-parsers*))

(defun unregister-scheme (scheme)
  (prog1
    (setq *scheme-parsers* (delete-if #'(lambda (x) (string-equal (car x) scheme)) *scheme-parsers*))
    (update-scheme-maximum-length)))

(defun get-scheme-parser (url-string &optional (start 0) (end (length url-string)) no-error-p &aux pos pos2)
  (declare (values parser start-index)
           (fixnum start))
  (cond ((setq pos (char-position #\: url-string start (min (the fixnum (setq pos2 (+ start *scheme-maximum-length* 1)))
							    end)))
         (loop for (scheme . parser) in *scheme-parsers*
               when (string-equal scheme url-string :start2 start :end2 pos)
                 do (return (values parser (the fixnum (1+ pos))))
               finally (if no-error-p
                           (return nil)
                           (error 'no-parser-for-scheme :url-string url-string))))
        (no-error-p nil)
	;; check for unknown schemes longer than *scheme-maximum-length*
	((and (< pos2 end) (char-position #\: url-string pos2 end))
	 (error 'no-parser-for-scheme :url-string url-string))
        (t (error 'no-scheme-found :url-string url-string))))

(declaim (inline scheme-prefixed-url-p))

(define scheme-prefixed-url-p (url-string &optional (start 0) (end (length url-string)))
  "Returns non-null if URL carries scheme prefix."
  (get-scheme-parser url-string start end t))

(defvar *http-scheme-parser* nil)

(defun initialize-http-scheme-parser ()
  (setq *http-scheme-parser* (get-scheme-parser "http:")))

(define http-url-string-p (url-string &optional (start 0) (end (length url-string)))
  (declare (values http-scheme-p relative-url-p))
  (cond ((eq *http-scheme-parser* (get-scheme-parser url-string start end t))
	 (values t nil))
	;; Some unknown scheme is present.
	((char-position #\: url-string start end) (values nil nil))
	;; it's potentially a relative HTTP URL if it starts with / or a legal url character.
	((< start end) (values nil t))
	(t (values nil nil))))

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

(defun numeric-hostname-p (hostname)
  (flet ((numeric-host-char-p (char)
           (or (digit-char-p char)
               (char= char #\.))))
    (declare (inline numeric-host-char-p))
    (every #'numeric-host-char-p hostname)))

(defun maybe-trim-url-string (url-string start end)
  (if (and (or (null start) (zerop start))
           (or (null end) (= end (length url-string))))
      url-string
      (subseq url-string start end)))

(defconstant *bad-internet-hostname-characters* '(#\space))

(define-variable *qualify-unqualified-domain-names* nil
                 "Controls whether unqualified domain names are automatically qualified.
It should not be turned on when running the Web walker.")

(defun check-host-string (host url-string start end)
  (declare (values check-host-string numeric-p))
  (flet ((bad-chars-in-host-p (host)
           (loop for idx upto (the fixnum (1- (length host)))
                 for char = (aref host idx)
                 when (member char *bad-internet-hostname-characters* :test #'char-equal)
                   do (return t)
                 finally (return nil))))
    (declare (inline bad-chars-in-host-p))
    (cond ((numeric-hostname-p host)
           (values host t))
          ((bad-chars-in-host-p host)
           (error 'bad-host-name-specification :host host
                  :url-string (maybe-trim-url-string url-string start end)))
	  ((zerop (count #\. host))
           (cond (*qualify-unqualified-domain-names*
		  (values (http:qualify-domain-name host) nil))
		 #+Allegro
		 ((equalp host "localhost") (values host nil))
		 (t (error 'bad-host-name-specification :host host
			   :url-string (maybe-trim-url-string url-string start end)))))
          (t (values host nil)))))

(defun get-host-port-indices (url-string &optional (start 0) (end (length url-string)))
  (declare (fixnum start end))
  (let* ((pos1 (string-search= "://" url-string 0 3 start (+ start 3 (the fixnum *scheme-maximum-length*))))
         (pos2 (and pos1 (+ 3 (the fixnum pos1))))
         (pos3 (and pos2 (char-position #\/ url-string pos2 end))))
    (when pos3
      (values pos2 pos3))))

(defun url-equal (url-string1 url-string2 &key (start1 0) (end1 (length url-string1)) (start2 0) (end2 (length url-string2))
                              &aux host2-end)
  "Returns non-null when URL-STRING1 is lexically equal to URL-STRING2."
  (and (= (- end1 start1) (- end2 start2))
       (multiple-value-bind (host1-start host1-end)
           (get-host-port-indices url-string1 start1 end1)
         host1-start                            ;ignore
         (setq host2-end (- host1-end (- start1 start2)))
         (and (string-equal url-string1 url-string2 :start1 start1 :end1 host1-end :start2 start2 :end2 host2-end)
              (string= url-string1 url-string2 :start1 host1-end :end1 end1 :start2 host2-end :end2 end2)))))

(defun get-port-info (url-string &optional (start 0) (end (length url-string)))
  (declare (values port-number host-end)
           (fixnum start end))
  (let ((p (and (> (- end start) 1)
                (char-position #\: url-string (the fixnum (+ 1 start)) end)))
        (port nil))
    (cond
      ((null p) (values nil end))
      ((and p (locally (declare (fixnum p))
                       (< (the fixnum (1+ p)) end)))
       (loop for idx from (the fixnum (1+ p)) upto (the fixnum (1- end))
             unless (digit-char-p (aref url-string idx))
               do (error 'bad-host-port-specification :url-string url-string))
       ;; parse the port integer
       (setq port (parse-integer url-string :start (the fixnum (1+ p)) :end end))
       ;; return the values
       (values port p))
      ;; null port specification
      (t (values nil p)))))

#+ignore
(defun get-host-port-info (url-string &optional (start 0) end no-delimiter-search-p &aux s e)
  "When no-delimiter-search-p is non-null, parse assumes START and END delimit host-port specification."
  (declare (values host-string port-number start-index)
           (fixnum start))
  (cond
    ((if no-delimiter-search-p
         ;; pregiven, so use them
         (and (setq s start)
              (setq e end))
         ;; find hostport delimiters
         (and (setq s (string-search= "//" url-string 0 2 start end))
              (setq e (or (char-position #\/ url-string (setq s (the fixnum (+ 2 s))) end) end))))
     (multiple-value-bind (port host-end)
         (get-port-info url-string s e)
       (let ((host (subseq url-string s host-end)))
         ;; return values
         ;; check the hostname for syntactic well-formedness and qualify unqualified domain names.
         (values (check-host-string host url-string start end)
                 port (the fixnum (1+ e))))))
    (t (error 'no-host-found :url-string (maybe-trim-url-string url-string start end)))))

(defun get-host-port-info (url-string &optional (start 0) end no-delimiter-search-p &aux s e)
   "When no-delimiter-search-p is non-null, parse assumes START and END delimit host-port specification."
   (declare (values host-string port-number start-index)
                 (fixnum start))
   (cond
     ((if no-delimiter-search-p
         ;; pregiven, so use them
         (and (setq s start)
                 (setq e end))
         ;; find hostport delimiters
         (and (setq s (string-search= "//" url-string 0 2 start end))
                 (setq e (or (char-position #\/ url-string (setq s (the fixnum (+ 2 s))) end) end))))
       (multiple-value-bind (port host-end)
                                       (get-port-info url-string s e)
           (let ((host (subseq url-string s host-end)))
              ;; return values
              ;; check the hostname for syntactic well-formedness and qualify unqualified domain names.
              (values (check-host-string host url-string start end)
                          port (the fixnum (1+ e))))))
     (t (error 'no-host-found :url-string (maybe-trim-url-string url-string start end)))))

(defun get-password-info (url-string start end)
  (declare (values password userid-end)
           (fixnum start end))
  (let ((p (char-position #\: url-string (the fixnum (+ 2 start)) end))
        (password nil))
    (declare (type (or null fixnum) p))
    (cond
      ((null p) (values nil end))
      ((and p (< (the fixnum (1+ p)) end))
       (setq password (subseq url-string (the fixnum (1+ p)) end))
       ;; return the values
       (values password p))
      ;; null password specification
      (t (values nil p)))))

(defun get-user-id-and-password (url-string &optional (start 0) end &aux s e at-sign)
  (declare (values user-id password host-start-index host-end-index)
           (fixnum start))
  (cond
    ((and (setq s (string-search= "//" url-string 0 2 start end))
          (setq e (char-position #\/ url-string (setq s (+ 2 s)) end))
          (setq at-sign (char-position #\@ url-string s e)))
     (multiple-value-bind (password userid-end)
         (get-password-info url-string s at-sign)
       (let ((host (subseq url-string s userid-end)))
         ;; perhaps check the userid for syntactic well-formedness, if possible.
         ;; return values
         (values host password (the fixnum (1+ at-sign)) e))))
    ((and s e)
     (values nil nil s (the fixnum e)))
    (t (error 'no-host-found :url-string url-string))))

(defun get-telnet-info (url-string &optional (start 0) end)
  (declare (values userid host-string start-index port-number)
           (fixnum start))
  (let ((s (string-search= "//" url-string 0 2 start end)))
    (if s
        (let* ((s1 (the fixnum (+ 2 (the fixnum s))))
               (pos (char-position #\@ url-string s1 end))
               user-id host)
          ;; get the optional user-id
          (when pos
            (setq user-id (subseq url-string s1 pos))
            (setq s1 (the fixnum (1+ pos))))
          ;; get the optional port info
          (multiple-value-bind (port host-end)
              (get-port-info url-string s1 end)
            ;; get the host string
            (setq host (subseq url-string s1 host-end))
            ;; check the hostname for syntactic well-formedness
            (check-host-string host url-string start end)
            ;; return values
            (values user-id host port)))
        (error 'no-host-found :url-string (maybe-trim-url-string url-string start end)))))

(defun %merge-relative-url (relative-url default-url &optional (start1 0) (end1 (length relative-url)) (start2 0) (end2 (length default-url)) &aux url-pos)
  "Merges RELATIVE-URL against DEFAULT-URL and returns a new fully specified url.
DEFAULT-URL must be fully-specified."
  (declare (values merged-url))
  (cond ((eql (aref relative-url start1) #\/)
	 (multiple-value-bind (s1 e1)
	     (get-host-port-indices default-url start2 end2)
	   s1
	   (setq url-pos e1)))
	(t (let ((last-slash (char-position #\/ default-url start2 end2 t)))
	     (unless last-slash
	       (error 'parsing-error :url-string (subseq default-url start2 end2)))
	     (setq url-pos (1+ last-slash)))))
  (let* ((url-copy-size (- url-pos start2))
	 (merged-size (+ (- end1 start1) url-copy-size))
	 (merged-url (make-array merged-size :element-type http::*standard-character-type*)))
    (copy-vector-portion default-url start2 url-pos merged-url 0 url-copy-size)
    (copy-vector-portion relative-url start1 end1 merged-url url-copy-size merged-size)
    merged-url))

(declaim (notinline parse-host))

#-(or genera mcl lispworks Franz-Inc)
(defun parse-host (host &optional no-error-p)
  "Parse a host for URL parsing.
Signal a URL condition if a bad host specification is provided."
  (cond ((www-utils:parse-host host t))
        (no-error-p nil)
        (t (error 'bad-host-name-specification :host host))))

#+genera
(define-parameter *inhibit-host-validity-checking* nil
                  "Controls whether the domain name system is consulted to check the validity of host objects.")

#+(or genera mcl lispworks Franz-Inc)
(defun parse-host (host &optional no-error-p #+genera (inhibit-validity-checking-p *inhibit-host-validity-checking*))
  "Parse a host for URL parsing.
Signal a URL condition if a bad host specification is provided."
  (flet ((resignal-bad-host (err)
           (cond (http:*debug-server* nil)
                 (t (typecase err
                      (unknown-host-name nil)
                      (t (error 'bad-host-name-specification :host host)))))))
    (declare (dynamic-extent #'resignal-bad-host))
    (handler-bind ((unknown-host-name #'resignal-bad-host))
      (www-utils:parse-host host no-error-p #+genera inhibit-validity-checking-p))))

(defun get-path-info (url-string &optional (start 0) (end (length url-string)))
  (declare (values path object-index next-index search-p)
           (fixnum start end))
  (flet ((make-path-component (start end)
           (subseq url-string start end)))
    (declare (inline make-path-component))
    (loop with p-idx = start
          for idx from start upto (the fixnum (1- end))
          for char = (aref url-string idx)
          for search-p = (char= char *search-url-delimiter*)
          when (char= char #\/)
            collect (make-path-component p-idx idx) into path
            and do (setq p-idx (the fixnum (1+ idx)))
          until search-p
          finally (return (values path (if (= p-idx idx) nil p-idx) idx search-p)))))

(defun get-object-info (url-string &optional (start 0) (end (length url-string)))
  (declare (values name extension))
  (let ((pos (char-position #\. url-string start end t)))
    (cond (pos
           (values (subseq url-string start pos)
                   (subseq url-string (the fixnum (1+ pos)) end)))
          (t (subseq url-string start end)))))

(defun parse-search-info (url-string &optional (start 0) (end (length url-string)))
  "Parses the search component of a search URL according to the standard search rules."
  (declare (values search-keys))
  (unless (= start end)                         ;no search keys provided
    (with-fast-array-references ((url-string url-string string))
      (loop with p-idx = start
            with e-idx = (1- end)
            for idx from 0 upto e-idx
            for collect-p = (eql *search-url-search-item-delimiter* (aref url-string idx))
            when (and collect-p (or (<= (the fixnum (1+ p-idx)) idx)))
              collect (http:string-unescape-special-chars url-string p-idx idx) into search-keys
            when collect-p
              do (setq p-idx (the fixnum (1+ idx)))
            finally (return (if (<= (the fixnum (1+ p-idx)) idx)
                                `(,.search-keys ,(http:string-unescape-special-chars url-string p-idx idx))
                                search-keys))))))

(defun write-search-info (search-keys &optional (stream *standard-output*)
                                      (escape-p *escape-search-urls*))
  (cond (escape-p
         (write-string (http:string-escape-special-chars (car search-keys)) stream)
         (dolist (item (cdr search-keys))
           (write-char *search-url-search-item-delimiter* stream)
           (let ((string (http:string-escape-special-chars item)))
             (declare (dynamic-extent string))
             (write-string string stream))))
        (t (write-string (car search-keys) stream)
           (dolist (item (cdr search-keys))
             (write-char *search-url-search-item-delimiter* stream)
             (write-string item stream)))))

(defun parse-search-info-as-query-alist (string &optional (start 0) (end (length string)))
  "Parses the search component of a search URL according to URL encoding rules
for use with form submission via the GET method."
  (declare (values search-alist))
  (unless (= start end)
    (with-fast-array-references ((string string string))
      (loop for s1 = start then (1+ (the fixnum e2))
	    while (< s1 end)
	    for e1 = (or (char-position #\= string s1 end)
			 (error "Ill-formed query alist encoding in ~S" (subseq string start end)))
	    for s2 = (1+ (the fixnum e1))
	    for e2 = (or (char-position #\& string s2 end) end)
	    for keyword = (http::%tokenize-form-query-keyword string s1 e1)
	    for value = (unless (= s2 e2)
			  (string-unescape-special-chars string s2 e2))
	    collect `(,keyword ,value)))))

(defun write-search-info-as-query-alist (search-alist &optional (stream *standard-output*)
                                                      (escape-p *escape-search-urls*))
  (cond (escape-p
         (loop for ((keyword value) . alist) = search-alist then alist
               do (write-string (symbol-name keyword) stream)
                  (write-char #\= stream)
                  (when value
                    (let ((string (http:string-escape-special-chars value)))
                      (write-string string stream)))
               when alist
                 do (write-char #\& stream)
               while alist))
        (t (loop for ((keyword value) . alist) = search-alist then alist
                 do (write-string (symbol-name keyword) stream)
                    (write-char #\= stream)
                    (when value
                      (write-string value stream))
                 when alist
                   do (write-char #\& stream)
                 while alist))))

(declaim (inline search-url-form-encoding-p))

(defun search-url-form-encoding-p (string &optional (start 0) (end (length string)))
  "Returns non-null if STRING contains an HTTP form encoding."
  (with-fast-array-references ((string string string))
    (loop with equal-sign-p and amp
          for idx upfrom start below end
          for char = (aref string idx)
          do (case char
               (#\& (if equal-sign-p
                        (return t)
                        (setq amp t)))
               (#\= (if amp
                        (return t)
                        (setq equal-sign-p t)))
               (#\+ (return nil)))
          finally (return nil))))

(defun standard-parse-search-info (string &optional (start 0) (end (length string)))
  "Standard parser for search URL info coming after the ?"
  (cond ((= start end) nil)
        ((search-url-form-encoding-p string start end)
         (parse-search-info-as-query-alist string start end))
        (t (parse-search-info string start end))))

(defun standard-write-search-info (search-keys stream)
  "Standard writer for search URL info coming after the ?"
  (typecase (car search-keys)
    (cons
      (write-search-info-as-query-alist search-keys stream *escape-search-urls*))
    (t (write-search-info search-keys stream *escape-search-urls*))))

(declaim (notinline search-url-form-encoding-p))

(defun extract-wais-doc-info (url raw-path)
  (destructuring-bind (&optional database type size . path)
      raw-path
    (unless database (error 'no-wais-database :url-string url))
    (unless type (error 'no-wais-type :url-string url))
    (cond ((null size) (error 'no-wais-size :url-string url))
          ((every #'digit-char-p size))
          (t (error 'non-numeric-wais-size :url-string url)))
    (values database type (parse-integer size) path)))

(defun get-news-info (url-string start end)
  (declare (values (group message-id))
           (fixnum start end))
  (flet ((make-group-name (start end)
           (subseq url-string start end)))
    (declare (inline make-group-name))
    (cond
      ((or (= start end)                        ;no content implies root
           (char= #\* (aref url-string start))) ;wild
       `("*"))
      ((char-position #\@ url-string start end t)
       (values nil (subseq url-string start end)))
      (t (loop with p-idx = start
               for idx from start upto (the fixnum (1- end))
               for char = (aref url-string idx)
               when (char= char #\.)
                 collect (make-group-name p-idx idx) into group
                 and do (setq p-idx (the fixnum (1+ idx)))
               finally (return
                         (if (char= char #\.)
                             group
                             (nconc group (list (make-group-name p-idx idx))))))))))

(defun get-mailto-info (url-string &optional (start 0) end)
  (declare (values userid host-string))
  (let ((pos (char-position #\@ url-string start end))
        user-id host)
    (cond (pos
           (setq user-id (subseq url-string start pos)
                 host (subseq url-string (1+ (the fixnum pos)) end))
           ;; check the hostname fo syntactic well-formedness
           (check-host-string host url-string start end)
           ;; return values
           (values user-id host))
          (t (error 'no-host-found :url-string (maybe-trim-url-string url-string start end))))))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defmethod print-object ((url url) stream)
  (print-unreadable-object (url stream :type t :identity t)
    (write-string (handler-case
                    (name-string url)
                    (error () "Error Getting Name"))
                  stream)))

;(define ensure-escaped-search-url (string)
;  "Ensures that search URL strings are escaped after the ?."
;  (let* ((end (length string))
;         (start (position *search-url-delimiter* string :start 0 :end end :test #'char=)))
;    (if (and start (not (= (incf (the fixnum start)) end)))
;       (multiple-value-bind (nsearch escaped-chars-p)
;            (http:string-escape-special-chars string start end nil)    ;don't escape URL fragments
;          (declare (dynamic-extent nsearch))
;          (cond (escaped-chars-p
;                 (let* ((len (length nsearch))
;                        (end1 start)
;                        (end2 (+ (the fixnum end1) (the fixnum len)))
;                        (string2 (make-string end2)))
;                   (http:nfill-array string2 string :start1 0 :end1 end1 :start2 0 :end2 end)
;                   (http:nfill-array string2 nsearch :start1 end1 :end1 end2 :start2 0 :end2 len)
;                   string2))
;                (t string)))
;        string)))

(define ensure-escaped-search-url (string &optional (escape-fragment-char-p t))
  "Ensures that search URL strings are escaped after the ?.
Each item delimited by + is individually escaped."
  (labels ((escape-search-p (string start end)
             (with-fast-array-references ((string string string))
               (loop for idx upfrom start below end
                     for char = (aref string idx)
                     when (and (not (eql char *search-url-search-item-delimiter*))
                               (http:escaped-character-p char))
                       return t
                     finally (return nil))))
           (fragment-position (string start end)
             (with-fast-array-references ((string string string))
               (loop for idx downfrom (1- end) to start
                     for char = (aref string idx)
                     do (cond ((eql char *fragment-delimiter-character*) (return idx))
                              ((or (eql char *search-url-search-item-delimiter*)
                                   (eql char *search-url-delimiter*))
                               (return nil)))
                     finally (return nil))))
           (escaped-strings (string start end)
             (loop with search-item and item-start and item-end
                   with fragment-idx = (fragment-position string start end)
                   for s = start then (1+ (the fixnum e))
                   for e = (or (char-position *search-url-search-item-delimiter* string s end) end)
                   when (and (or (null fragment-idx) (< e fragment-idx))
                             (position-if #'http:escaped-character-p string :start s :end e))
                     do (multiple-value-bind (nsearch escaped-chars-p)
                            (http:string-escape-special-chars string s e escape-fragment-char-p)
                          (if escaped-chars-p
                              (setq search-item nsearch
                                    item-start 0
                                    item-end (length search-item))
                              (setq search-item string
                                    item-start s
                                    item-end e)))
                   else do (setq search-item string
                                 item-start s
                                 item-end e)
                   collect `(,search-item ,item-start . ,item-end) into items
                   sum (- (the fixnum item-end) (the fixnum item-start)) into total-length
                   sum 1 into n-items
                   until (= e end)
                   finally (return (values items total-length n-items))))
           (insert-search-items (nstring search-items start)
             (loop with s = start
                   for (item . more) = search-items then more
                   while item
                   for (search-item item-start . item-end) = item
                   for e = (+ (- (the fixnum item-end) (the fixnum item-start)) 1 (the fixnum s))
                   do (http:nfill-array nstring search-item :start1 s :end1 e :start2 item-start :end2 item-end)
                   when more
                     do (setf (aref nstring (1- e)) *search-url-search-item-delimiter*)
                   do (setq s e))))
    (declare (inline escape-search-p fragment-position escaped-strings insert-search-items))
    (let* ((end (length string))
           (start (char-position *search-url-delimiter* string 0 end)))
      (cond ((and start
                  (not (= (incf (the fixnum start)) end))
                  (escape-search-p string start end))
             (multiple-value-bind (search-items total-length number)
                 (escaped-strings string start end)
               (declare (dynamic-extent search-items))
               (let* ((len2 (+ (the fixnum start) (the fixnum total-length) (1- (the fixnum number))))
                      (nstring (make-string len2)))
                 ;; Insert the first part of the string.
                 (http:nfill-array nstring string :start1 0 :end1 start :start2 0 :end2 end)
                 ;; insert the escaped pieces
                 (insert-search-items nstring search-items start)
                 nstring)))
            (t string)))))

(define coerce-url-string (url &optional (escape-search-url-p *escape-search-urls*)
                               (downcase-p *downcase-url-strings*)
                               &aux s)
  "Returns a string for URL."
  (declare (notinline))
  (setq s (cond (escape-search-url-p
                 (etypecase url
                   (string (ensure-escaped-search-url url))
                   (url (name-string url))))
                (t (etypecase url
                     (string url)
                     (url (name-string-with-unescaped-search-suffix url))))))
  (cond-every
    (downcase-p
      (setq s (string-downcase s))))
  s)

(define-macro with-value-cached ((url key &key (recompute-p nil recompute-supplied-p)) &body value-form)
  "Caches the value returned by value-form on URL's property list under the indicator KEY.
When RECOMPUTE-P is non-null, the value is recomputed and recached.
The returned values are VALUE and RETRIEVE-FROM-CACHE-P."
  (declare (values retrieved-from-cache-p))
  (let ((form `(let ((val (getf plist ,key :+not-found+)))
                 (case val
                   (:+not-found+ 
                     (setf (getf plist ,key) (progn  . ,value-form)))
                   (t (values val t))))))      
    (cond (recompute-supplied-p
           `(with-slots (plist) ,url
              (cond (,recompute-p
                     (setf (getf plist ,key) (progn . ,value-form)))
                    (t ,form))))
          (t `(with-slots (plist),url ,form))))) 

;;;------------------------------------------------------------------- 
;;;
;;; CANONICALIZING URLS 
;;;

;(defmacro %canonicalize-host-prefixed-url (scheme url-string start end)
;  (let* ((prefix (string-downcase scheme))
;         (prefix-length (length prefix)))
;    `(macrolet ((downcase-char (string idx)
;                  `(let ((idx ,idx))
;                     (setf (aref ,string idx) (char-downcase (aref ,string idx))))))
;       ;; Ensure that URLs are in canonical form when doing parses or interns.
;       (let ((url-string ,url-string)
;             (start ,start)
;             (end ,end)
;             host-delim)
;         (declare (fixnum start end)) 
;         (multiple-value-bind (canonical-url chars-unescaped-p new-url-string-p) 
;             (string-unescape-special-chars url-string start end t)
;           (declare (ignore chars-unescaped-p))
;           (let* ((canonical-url (if (or new-url-string-p
;                                         (= (- end start) (length canonical-url)))
;                                     canonical-url
;                                     (subseq canonical-url start end)))
;                  (len (length canonical-url))
;                  (scheme-prefixed-p (string-equal ,prefix canonical-url 
;                                                   :start1 0 :end1 ,prefix-length :start2 0 :end2 (min ,prefix-length len))))
;             (with-fast-array-references ((canonical-url canonical-url string))
;               ;; downcase host
;               (loop with slash-number fixnum = (if scheme-prefixed-p 0 2)
;                     for idx upfrom 0 below len
;                     for char = (aref canonical-url idx)
;                     do (case char
;                          (#\/ 
;                           (when (= 3 (incf slash-number))
;                             (setq host-delim idx)
;                             (return)))
;                          (t (unless (lower-case-p char)
;                               (setf (aref canonical-url idx) (char-downcase char))))))
;               ;; downcase any escapes
;               (when host-delim 
;                 (loop for idx fixnum upfrom (1+ (the fixnum host-delim)) below len
;                       for char = (aref canonical-url idx)
;                       do (case char
;                            (#\%
;                             (downcase-char canonical-url (1+ idx))
;                             (downcase-char canonical-url (+ 2 idx))
;                             (incf idx 2)))))
;               canonical-url)))))))

(declaim (inline %unescape-url))

(defun %unescape-url (url-string start end destructive-p)
  (declare (values canonical-url new-start new-end))
  (http::with-bad-escaping-resignalled (url-string :start start :end end
						   :reason "Bad Escaping: Ill-escaped URL")
    (cond (destructive-p
	   (multiple-value-bind (unescaped-string new-end)
	       (http::nstring-unescape-special-chars url-string start end t #\space t)
	     (values unescaped-string start new-end)))
	  (t (multiple-value-bind (unescaped-string chars-unescaped-p new-url-string-p)
		 (string-unescape-special-chars url-string start end t)
	       (declare (ignore chars-unescaped-p))
	       (unless new-url-string-p
		 (setq unescaped-string (subseq url-string start end)))
	       (values unescaped-string 0 (length unescaped-string)))))))

(declaim (inline %canonicalize-host-prefixed-url))

(defun %canonicalize-host-prefixed-url (prefix prefix-length url-string start end &optional destructive-p)
  (declare (values canonical-url new-string-p)
	   (fixnum start end))
  (flet ((downcase-char (string idx)
	   (setf (aref string idx) (char-downcase (aref string idx)))))
    (declaim (inline downcase-char))
    ;; Ensure that URLs are in canonical form when doing parses or interns.
    (multiple-value-bind (canonical-url start end)
	(%unescape-url url-string start end destructive-p)
      (let ((scheme-prefixed-p (string-equal prefix canonical-url :start1 0 :end1 prefix-length :start2 start :end2 (min prefix-length end)))
	    host-delim)
	(with-fast-array-references ((canonical-url canonical-url string))
	  ;; downcase host
	  (loop with slash-number fixnum = (if scheme-prefixed-p 0 2)
		for idx fixnum upfrom start below end
		for char = (aref canonical-url idx)
		do (case char
		     (#\/ 
		      (when (= 3 (incf slash-number))
			(setq host-delim idx)
			(return)))
		     (t (unless (lower-case-p char)
			  (setf (aref canonical-url idx) (char-downcase char))))))
	  ;; downcase any escapes
	  (when host-delim 
	    (loop for idx fixnum upfrom (1+ (the fixnum host-delim)) below end
		  for char = (aref canonical-url idx)
		  do (case char
		       (#\%
			(let ((idx2 (+ 2 idx)))
			  (when (< end idx2)
			    (error 'bad-escaping :url (subseq url-string start end)))
			  (downcase-char canonical-url (1+ idx))
			  (downcase-char canonical-url idx2)
			  (incf idx 2))))))
	  (values canonical-url (not destructive-p)))))))

(defun %canonicalize-basic-url (url-string start end &optional destructive-p)
  (declare (fixnum start end))
  (if destructive-p
      (nstring-downcase url-string)
      (values (nstring-downcase (subseq url-string start end)) t)))

(define-generic canonicalize-url (scheme url-string &optional start end destructive-p)
  (:documentation "Returns a canonicalized version of URL-STRING based on SCHEME,
which is a keyword for the URL scheme.
The return string is trimmed to include only that part of URL-STRING from START to END.
When DESTRUCTIVE-P is non-null, URL-STRING is canonicalized in place.
If characters are unescaped, blank spaces may appear before END.")) 

(defmethod canonicalize-url (scheme url-string &optional start end destructive-p)
  (declare (ignore url-string start end destructive-p))
  (error "No canonicalizer defined for the scheme, ~S." scheme))

(defmethod canonicalize-url (scheme (url-string null) &optional start end destructive-p)
  (declare (ignore scheme start end destructive-p))
  url-string) 

(defmethod canonicalize-url ((scheme (eql :http)) url-string &optional (start 0) (end (length url-string)) destructive-p)
  (%canonicalize-host-prefixed-url "http" 4 url-string start end destructive-p))

(defmethod canonicalize-url ((scheme (eql :ftp)) url-string &optional (start 0) (end (length url-string)) destructive-p)
  (%canonicalize-host-prefixed-url "ftp" 3 url-string start end destructive-p))

(defmethod canonicalize-url ((scheme (eql :wais)) url-string &optional (start 0) (end (length url-string)) destructive-p)
  (%canonicalize-host-prefixed-url "wais" 4 url-string start end) destructive-p)

(defmethod canonicalize-url ((scheme (eql :gopher)) url-string &optional (start 0) (end (length url-string)) destructive-p)
  (%canonicalize-host-prefixed-url "gopher" 6 url-string start end destructive-p)) 

(defmethod canonicalize-url ((scheme (eql :news))  url-string &optional (start 0) (end (length url-string)) destructive-p)
  (%canonicalize-basic-url url-string start end destructive-p))

(defmethod canonicalize-url ((scheme (eql :telnet))  url-string &optional (start 0) (end (length url-string)) destructive-p)
  (%canonicalize-basic-url url-string start end destructive-p))

(defmethod canonicalize-url ((scheme (eql :mailto))  url-string &optional (start 0) (end (length url-string)) destructive-p)
  (declare (fixnum start end)) 
  (multiple-value-bind (canonical-url start end) 
      (%unescape-url url-string start end destructive-p)
    (let* ((prefix-end (+ 6 (the fixnum start)))
	   (at-sign (and (< prefix-end end) (char-position #\@ canonical-url prefix-end end))))
      (when at-sign
        (nstring-downcase canonical-url :start start :end prefix-end)
        (nstring-downcase canonical-url :start at-sign :end end))
      canonical-url))) 

(defmethod canonicalize-url ((scheme (eql :file))  url-string &optional (start 0) (end (length url-string)) destructive-p)
  (multiple-value-bind (canonical-url)
      (%unescape-url url-string start end destructive-p)
    canonical-url))

;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

;; Case-sensitive for compatibility with the universe.
(defvar *url-table* (make-hash-table :test #'equal)) 

(declaim (inline get-url))

(defun get-url (url-string)
  (gethash url-string *url-table*))

(defsetf get-url (url-string) (url)
  `(setf (gethash ,url-string *url-table*) ,url))

(define clear-url-table ()
  "Clears all URLs, making any existing objects available for GC."
  (clrhash *url-table*))

(define map-url-table (function)
  "Maps FUNCTION over all interned URLs.
FUNCTION is called with url-string url-object."
  (maphash function *url-table*))

(define gc-url-table (&optional (good-url-predicate #'local-url-p))
  "Maps over all interned URLs and uninterns an URLs that do not satisfy GOOD-URL-PREDICATE."
  (let ((garbage nil))
    (declare (dynamic-extent garbage))
    (flet ((collect-garbage (string url)
             (declare (ignore string))
             (unless (funcall good-url-predicate url)
               (push url garbage))))
      (declare (dynamic-extent #'collect-garbage))
      (map-url-table #'collect-garbage)
      (dolist (url garbage)
        (unintern-url url)))))

(defgeneric register (url &optional return-existing-interned-url-p)
  (declare (values registered-url)))

(defmethod register ((url url) &optional return-existing-interned-url-p)
  (declare (values registered-url))
  (let* ((name-string (name-string url))
         (interned-url (get-url name-string)))
    (cond ((null interned-url)
           (setf (get-url name-string) url)
           url)
          ((eq url interned-url) interned-url)
          (return-existing-interned-url-p interned-url)
          (t (error 'interned-url-already-using-name
                    :interned-url (get-url name-string)
                    :uninterned-url url)))))

(defmethod unregister ((url url))
  (remhash (name-string url) *url-table*))

(defgeneric initialize-url (url &optional register-p))

(defmethod initialize-url ((url url) &optional (register-p t))
  (cond (register-p
         ;; in the case where DNS resolution to the canonical host name yields
         ;; another URL, the second argument ensures that we don't clobber the first
         ;; one, but instead return it, in place of the newly minted one.  That latter
         ;; becomes garbage right away.  This situation can arise in proxy service.
         ;; This problem goes away once the url-table becomes host-based.  6/20/95 -- JCMa.
         (values (register url t) :interned))
        (t (name-string url)
           (values url :uninterned))))

(define-macro define-scheme-parser (scheme (&key classes) (url start end) &body body)
  "Defines a parser and object creator for scheme URL.
CLASSES are the valid URL classes which this parser will return.
 (one spanning class is fine). The set of arguments specifies
the names of the arguments passed into BODY."
  (let* ((prefix (string-thin (string scheme)))
         (parser-name (intern (format nil "%PARSE-URL-~A-SCHEME" prefix) *package*)))
    `(progn
       (defun ,parser-name (url &optional (start 0) end (if-does-not-exist :create))
         (flet ((create-url (,url ,start ,end interned-p)
                  (let ((url-object (progn . ,body)))
                    (initialize-url url-object interned-p))))
           (declare (dynamic-extent #'create-url))
           (typecase ,url
             (string
               ;; method dispatch can be removed when debugged -- JCMa 8/15/1997.
               (multiple-value-bind (canonical-url new-url-p)
                   (canonicalize-url ,(intern prefix :keyword) ,url ,start ,end)
                 (declare (dynamic-extent canonical-url))
                 (or (get-url canonical-url)
                     (ecase if-does-not-exist
                       (:soft nil)
                       (:uninterned (create-url canonical-url 0 (length canonical-url) nil))
                       (:create (create-url canonical-url 0 (length canonical-url) t))
                       (:error (error 'no-interned-url-found :url-string (if new-url-p (copy-seq canonical-url) canonical-url)))))))
             ,.(loop for class in classes
                     collect `(,class ,url))
             (t (error 'bad-data-type-for-scheme :url-string ,url)))))
       (register-scheme ',prefix (function ,parser-name)))))

(defun parse-url (url &optional (start 0) end (if-does-not-exist :create)) 
  "Parses URL and returns and interned URL object."
  (declare (values url-object newly-interned-p))
  (flet ((whitespace (char)
           (member char '(#\space #\tab) :test #'eql)))
    (declare (inline whitespace))
    (etypecase url
      (string
        (let* ((len (length url))
               (start-idx (position-if #'alpha-char-p url :start start :end (or end len)))
               (end-idx (1+ (the fixnum (position-if-not #'whitespace url :start start-idx :end (or end len) :from-end t)))))
          (funcall (get-scheme-parser url start-idx end-idx) url start-idx end-idx if-does-not-exist)))
      (url
        (case if-does-not-exist
          (:create (register url)))
        url)
      (uri url))))                              ;allow URIs to fall through

(define intern-url (url &key (if-does-not-exist :create) (start 0) end)
  "Top-level function for interning a URL (Universal Resource Locator).
IF-DOES-NOT-EXIST can be  

     :SOFT       - Return the interned URL if it exists, otherwise NIL.
     :CREATE     - Return interned URL, if it exists, otherwise create it.
     :UNINTERNED - Return an uninterned URL, regardless of whether an interned one
                   already exists.
     :ERROR      - Return interned URL, if it exists, otherwise signal the
                   error condition URL:NO-INTERNED-URL-FOUND.

The intern operation automatically escapes search URLs when they are created to
ensure that their names are valid URL strings. URL:COERCE-URL-STRING accepts
an argument that allows the callers to obtain an escaped or unescaped url string,
whether the URL is an object or merely a string. In the case where it is
necessary to intern an unescaped search URL, the variable URL:*ESCAPE-SEARCH-URLS*
may be set to NIL around the call to intern URL."
  (declare (inline)
           (values url newly-interned-p))
  (parse-url url start end if-does-not-exist))

;; Don't escape URLs on intern.  7/4/96 -- JCMa.
(defmethod register :around ((url search-mixin) &optional return-existing-interned-url-p)
  (declare (values registered-url))
  (let ((*escape-search-urls* nil))
    (call-next-method url return-existing-interned-url-p)))

(define-generic unintern-url (url)
  (:documentation "Top-level function for uninterning URL."))

(defmethod unintern-url ((url url))
  (unregister url)
  t)

(defmethod unintern-url ((url string))
  (let ((interned-url (intern-url url :if-does-not-exist :soft)))
    (cond (interned-url
           (unintern-url interned-url))
          (t nil))))

(defun url (x)
  "Coerces x into an interned URL, creating one if necessary."
  (declare (values url-object new-interned-p)
           (inline))
  (parse-url x 0 (length x) :create))

(define url-p (x)
  "Returns non-null if X is a URL."
  (typep x 'url))


;;;------------------------------------------------------------------- 
;;;
;;; HTTP URL PARSER
;;; 

(define-scheme-parser
  http
  (:classes (http-url))
  (url start end)
  (let (object extension search-keys search-parent)
    ;; extract the host parameters
    (multiple-value-bind (host-string port path-index)
        (get-host-port-info url start end)
      ;; extract the path components
      (multiple-value-bind (path object-index next-index search-p)
          (get-path-info url path-index end)
        ;; get the object components when present
        (when object-index
          (multiple-value-setq (object extension)
            (get-object-info url object-index next-index)))
        ;; get the search keys where necessary
        (when search-p
          (let ((s-suffix (1+ (the fixnum next-index))))
            (unless (= s-suffix end)
              (setq search-parent (intern-url url :start start :end s-suffix)
                    search-keys (funcall (search-parser search-parent) url s-suffix end)))))
        ;; create the appropriate URL
        (cond
          (search-p
           (let ((object (if extension
                             ;; searchable object (used for searchable images)
                             (make-instance 'http-searchable-object
                                            :name-string (when *retain-original-name-string* (subseq url start end))
                                            :host-string host-string
                                            :port port
                                            :path path
                                            :object object
                                            :extension extension
                                            :search-keys search-keys
                                            :search-parent search-parent)
                             ;; regular search urls
                             (make-instance 'http-search
                                            :name-string (when *retain-original-name-string* (subseq url start end))
                                            :host-string host-string
                                            :port port
                                            :path path
                                            :object object
                                            :search-keys search-keys
                                            :search-parent search-parent))))
             ;; inherit the parent's security properties on creation
             (if search-keys
                 (inherit-parent-access-controls object)
                 ;; set the search parent so we know we're the root.
                 (setf (%search-parent object) object))
             object))
          (object
           (make-instance 'http-object
                          :name-string (when *retain-original-name-string* (subseq url start end))
                          :host-string host-string
                          :port port
                          :path path
                          :object object
                          :extension extension))
          (t (make-instance 'http-path
                            :name-string (when *retain-original-name-string* (subseq url start end))
                            :host-string host-string
                            :port port
                            :path path)))))))

(initialize-http-scheme-parser)


;;;------------------------------------------------------------------- 
;;;
;;; FTP URL PARSER
;;;

;The URL is now parsed according to the (latest) specifications:   6/23/94 -- JCMa.
;
;           url     : f t p: / / login / path [  ftptype ]
;           login   : [ user [ : password ] @ ] hostport
;           hostport: host [ : port ]
;; the above implemented, but not the below.  
;           ftptype : A formcode | E formcode | I | L digits
;           formcode:  N | T | C

(define-scheme-parser
  ftp
  (:classes (ftp-url))
  (url start end)
  (multiple-value-bind (user-id pw host-index host-end)
      (get-user-id-and-password url start end)
    (multiple-value-bind (host-string port path-index)
        (get-host-port-info url host-index host-end t)
      ;; extract the path components
      (multiple-value-bind (path object-index next-index)
          (get-path-info url path-index end)
        ;; The spec requires a path. Should we signal an error when there
        ;; is no path?  create the appropriate URL
        (cond ;; get the object components when present         
          (object-index
           (multiple-value-bind (object extension)
               (get-object-info url object-index next-index)
             (make-instance 'ftp-pathname
                            :name-string (when *retain-original-name-string* (subseq url start end))
                            :host-string host-string
                            :port port
                            :user-id user-id
                            :password pw
                            :path path
                            :object object
                            :extension extension)))
          (t (make-instance 'ftp-directory
                            :name-string (when *retain-original-name-string* (subseq url start end))
                            :host-string host-string
                            :port port
                            :user-id user-id
                            :password pw
                            :path path)))))))


;;;------------------------------------------------------------------- 
;;;
;;; NETNEWS URL PARSER
;;;

;; See RFC 850 for rules on group names and article identifiers.
(define-scheme-parser
  news
  (:classes (news-url))
  (url start end)
  (multiple-value-bind (group message-id)
      (get-news-info url start end)
    (cond (group
           (make-instance 'news-group
                          :name-string (when *retain-original-name-string* (subseq url start end))
                          :group group))
          (t (make-instance 'news-article
                            :name-string (when *retain-original-name-string* (subseq url start end))
                            :message-id message-id)))))


;;;------------------------------------------------------------------- 
;;;
;;; GOPHER URL PARSER
;;;

;; Gopher links to different protocols should be converted to those
;; protocols.
(define-scheme-parser
  gopher
  (:classes (gopher-url))
  (url start end)
  (let (object extension search-keys search-parent)
    ;; extract the host parameters
    (multiple-value-bind (host-string port path-index)
        (get-host-port-info url start end)
      ;; extract the path components
      (multiple-value-bind (path object-index next-index search-p)
          (get-path-info url path-index end)
        ;; get the object components when present
        (when object-index
          (multiple-value-setq (object extension)
            (get-object-info url object-index next-index)))
        ;; get the search keys where necessary
        (when search-p
          (let ((s-suffix (1+ (the fixnum next-index))))
            (unless (= s-suffix end)
              (setq search-parent (intern-url url :start (- (the fixnum start) 7) :end s-suffix)
                    search-keys (parse-search-info url s-suffix end)))))
        ;; create the appropriate URL
        (let ((type (or (pop path) "1")))       ;default when type and selector omitted
          (cond
            (search-p
             (make-instance 'gopher-search
                            :name-string (when *retain-original-name-string* (subseq url start end))
                            :host-string host-string
                            :port port
                            :type type
                            :path (if object
                                      ;; ignore extension until we have
                                      ;; searchable gopher urls 7/28/94 -- JCMa.
                                      (nconc path (list object))
                                      path)
                            :search-keys search-keys
                            :search-parent search-parent))
            (object
             (make-instance 'gopher-object
                            :name-string (when *retain-original-name-string* (subseq url start end))
                            :host-string host-string
                            :type type
                            :port port
                            :path path
                            :object object
                            :extension extension))
            (t (make-instance 'gopher-path
                              :name-string (when *retain-original-name-string* (subseq url start end))
                              :host-string host-string
                              :port port
                              :type type
                              :path path))))))))


;;;------------------------------------------------------------------- 
;;;
;;; WAIS URL PARSER
;;;

(define-scheme-parser
  wais
  (:classes (wais-url))
  (url start end)
  (multiple-value-bind (host-string port path-index)
      (get-host-port-info url start end)
    ;; extract the path components
    (multiple-value-bind (raw-path object-index next-index search-p)
        (get-path-info url path-index end)
      (cond
        (search-p
         (when raw-path
           (error 'path-components-in-wais-database :url-string url))
         (let ((s-suffix (1+ (the fixnum next-index)))
               search-parent search-keys)
           (unless (= s-suffix end)
             (setq search-parent (intern-url url :start (- (the fixnum start) 5) :end s-suffix)
                   search-keys (parse-search-info url s-suffix end)))
           (make-instance 'wais-search
                          :name-string (when *retain-original-name-string* (subseq url start end))
                          :host-string host-string
                          :port port
                          :database (subseq url object-index next-index)
                          :search-keys search-keys
                          :search-parent search-parent)))
        ((null raw-path) (error 'no-wais-database :url-string url))
        ((cdr raw-path) ;; WAIS document
         ;; extract required fields
         (multiple-value-bind (database type size path)
             (extract-wais-doc-info url raw-path)
           ;; extract object if present
           (multiple-value-bind (object extension)
               (when object-index
                 (get-object-info url object-index next-index))
             ;; make the WAIS document
             (make-instance 'wais-doc
                            :name-string (when *retain-original-name-string* (subseq url start end))
                            :host-string host-string
                            :port port
                            :database database
                            :type type
                            :size size
                            :path path
                            :object object
                            :extension extension))))
        (t (error 'raw-path-components-in-wais-database :url-string url)))))) 

;;;------------------------------------------------------------------- 
;;;
;;; TELNET URL PARSER
;;;

(define-scheme-parser
  telnet
  (:classes (telnet-url))
  (url start end)
  (multiple-value-bind (userid host-string port)
      (get-telnet-info url start end)
    (make-instance 'telnet-url
                   :name-string (when *retain-original-name-string* (subseq url start end))
                   :host-string host-string
                   :port port
                   :user-id userid)))


;;;------------------------------------------------------------------- 
;;;
;;; MAILTO URL PARSER
;;;

(define-scheme-parser
  mailto
  (:classes (mailto-url))
  (url start end)
  (multiple-value-bind (userid host-string)
      (get-mailto-info url start end)
    (make-instance 'mailto-url
                   :name-string (when *retain-original-name-string* (subseq url start end))
                   :user-id userid
                   :host-string host-string)))


;;;------------------------------------------------------------------- 
;;;
;;; FILE URL PARSER
;;;

(define-scheme-parser
  file
  (:classes (file-url))
  (url start end)
  (let ((path-index (string-search= "//" url 0 2 start end))
        object extension)
    (unless path-index
      (error 'parsing-error :url-string url))
    ;; extract the path components
    (multiple-value-bind (path object-index next-index)
        (get-path-info url (+ 2 path-index) end)
      (let ((host-string (or (pop path) "localhost")))
        ;; get the object components when present
        (when object-index
          (multiple-value-setq (object extension)
            (get-object-info url object-index next-index)))
        (if object
            (make-instance 'file-pathname
                           :name-string (when *retain-original-name-string* (subseq url start end))
                           :host-string host-string
                           :path path
                           :object object
                           :extension extension)
            (make-instance 'file-directory
                           :name-string (when *retain-original-name-string* (subseq url start end))
                           :host-string host-string
                           :path path))))))


;;;------------------------------------------------------------------- 
;;;
;;; NAME OPERATIONS
;;;

(declaim (inline %url-host-object))

(defun %url-host-object (host-mixin no-error-p reparse-p)
  (with-slots (host-string host-object) host-mixin
    (cond ((or reparse-p (null host-object))
           (let ((host (parse-host host-string no-error-p)))
             (when host
               (setq host-object host))))
          (t host-object)))) 

(define-generic host-object (url &optional no-error-p reparse-p)
  (:documentation "Returns the parsed host object for URL.      
When REPARSE-P is no-null, the host is reparsed, otherwise
the cache is used whenever available."))

(defmethod  host-object ((url host-mixin) &optional no-error-p reparse-p)
  (%url-host-object url no-error-p reparse-p))

(define-generic host-port (url)
  (:documentation "Returns the host port associated with the URL."))

(defmethod host-port ((url http-url))
  (with-slots (port) url
    (or port 80.)))

(defmethod host-port ((url url))
  (with-slots (port) url
    port))

(define-generic canonicalize-host-domain-name (url &optional no-error-p reparse-p)
  (declare (values primary-host-string))
  (:documentation "Canonicalizes the host-string for the URL by resolving the domain name and obtaining the primary host name.
Remember to recompute the URL namestring after running this function."))

(defmethod canonicalize-host-domain-name ((url host-mixin) &optional no-error-p reparse-p)
  (with-slots (host-string) url
    (let ((host (host-object url no-error-p reparse-p))
          primary-name)
      (when host
        (unless (string= (setq primary-name (host-http-name host))
                         host-string)
          (setq host-string primary-name)))))) 

(define-generic host-for-pathname (url)
  (:documentation "Returns a host specification for the Lisp platform
which is suitable for the host argument to MAKE-PATHNAME."))

;; Only the lisp machine does useful stuff with the host object.
(defmethod host-for-pathname ((url host-mixin))
  #+genera (%url-host-object url nil nil)
  #-genera nil)

(define-generic device-for-pathname (url &optional host)
  (:documentation "Returns a pathname device for URL based on
the default pathname associated with the host to which the URL refers."))

(defmethod device-for-pathname ((url host-mixin) &optional host)
  (pathname-device (http:host-default-pathname
                     #+genera (or host (host-for-pathname url))
                     #-genera host)))

(define-generic local-url-p (url)
  (:documentation "Returns non-null if URL resides on the local host."))

(defmethod local-url-p ((url url))
  (www-utils:host-eq (host-object url t) (local-host)))

(defmethod local-url-p (url)
  (declare (ignore url))
  nil)

(defgeneric write-scheme-prefix (url &optional stream))

(defmethod write-scheme-prefix ((url url) &optional (stream *standard-output*))
  (write-string (scheme url) stream)
  (write-string "://" stream))

(defmethod write-scheme-prefix ((url colon-scheme-prefix-mixin) &optional (stream *standard-output*))
  (write-string (scheme url) stream)
  (write-char #\: stream))

(defgeneric write-host-port-string (url &optional stream))

(defmethod write-host-port-string ((url host-port-mixin) &optional (stream *standard-output*))
  (let ((port (port url))
        (host-string (host-string url)))
    (when host-string
      (write-string (string-downcase host-string)  stream)
      (when port
        (write-char #\: stream)
        (write port :stream stream)))))

(defmethod write-host-port-string ((url host-mixin) &optional (stream *standard-output*))
  (with-slots (host-string) url
    (when host-string
      (write-string host-string stream))))

(defgeneric host-port-string (url))

(defmethod host-port-string ((url host-port-mixin))
  (with-output-to-string (stream)
    (write-host-port-string url stream)))

(defmethod host-port-string ((url host-mixin))
  (with-slots (host-string) url
    host-string))

(defgeneric write-path (url &optional stream))

(defmethod write-path ((url path-mixin) &optional (stream *standard-output*))
  (with-slots (path) url
    (dolist (item path)
      (write-char #\/ stream)
      (write-string item stream))))

(defgeneric write-search (url &optional stream))

(defmethod write-search ((url search-mixin) &optional (stream *standard-output*))

  (let ((search-keys (search-keys url)))
    (write-char *search-url-delimiter* stream)
    (typecase (car search-keys)
      (null)
      (cons
        (write-search-info-as-query-alist search-keys stream *escape-search-urls*))
      (t (write-search-info search-keys stream *escape-search-urls*)))))

(defmethod write-search ((url search-parser-mixin) &optional (stream *standard-output*))
  (let ((search-writer (search-writer url))
        (search-keys (search-keys url)))
    (write-char *search-url-delimiter* stream)
    (when search-keys
      (funcall search-writer search-keys stream))))

(defgeneric write-object-name-string (url &optional stream include-extension-p))

(defmethod write-object-name-string ((url object-mixin) &optional (stream *standard-output*) (include-extension-p t))
  (with-slots (object extension) url
    (when object
      (write-string object stream)
      (when (and include-extension-p extension)
        (write-char #\. stream)
        (write-string extension stream)))))

(define-generic write-local-name (url &optional stream))

(defmethod write-local-name ((url http-path) &optional stream)
  (write-path url stream)
  (write-char #\/ stream))

(defmethod write-local-name ((url http-minimum-object) &optional stream)
  (write-path url stream)
  (write-char #\/ stream)
  (write-object-name-string url stream))

(defmethod write-local-name :after ((url search-mixin) &optional stream)
  (write-search url stream))

(defgeneric compute-object-name-string (url))

(defmethod compute-object-name-string ((url object-mixin))
  (with-output-to-string (stream)
    (write-object-name-string url stream)))

(define-generic compute-name-string (url))

(defmethod compute-name-string ((url url))
  (error "Computing this namestring has not been implemented yet."))

(defmethod compute-name-string ((url telnet-url))
  (let ((user-id (user-id url)))
    (with-output-to-string (stream)
      (write-scheme-prefix url stream)
      (when user-id
        (write-char #\@ stream)
        (write-string user-id stream))
      (write-host-port-string url stream))))

(defmethod compute-name-string ((url mailto-url))
  (let ((user-id (user-id url)))
    (with-output-to-string (stream)
      (write-scheme-prefix url stream)
      (write-string user-id stream)
      (write-char #\@ stream)
      (write-host-port-string url stream))))

;; close off the FTP path specification.
(defmethod write-path :after ((url ftp-url) &optional (stream *standard-output*))
  (write-char #\/ stream))

(defmethod compute-name-string ((url ftp-directory))
  (with-output-to-string (stream)
    (write-scheme-prefix url stream)
    (write-host-port-string url stream)
    (write-path url stream)))

(defmethod compute-name-string ((url ftp-pathname))
  (with-output-to-string (stream)
    (write-scheme-prefix url stream)
    (write-host-port-string url stream)
    (write-path url stream)
    (write-object-name-string url stream)))

(defmethod write-path ((url file-url) &optional (stream *standard-output*))
  (with-slots (path) url
    (dolist (item path)
      (write-string item stream)
      (write-char #\/ stream))))

(defmethod compute-name-string ((url file-directory))
  (with-output-to-string (stream)
    (write-scheme-prefix url stream)
    (write-host-port-string url stream)
    (write-path url stream)))

(defmethod compute-name-string ((url file-pathname))
  (with-output-to-string (stream)
    (write-scheme-prefix url stream)
    (write-host-port-string url stream)
    (write-path url stream)
    (write-object-name-string url stream)))

(defmethod compute-name-string ((url news-url))
  (with-slots (group) url
    (with-output-to-string (stream)
      (write-scheme-prefix url stream)
      (when group
        (write-string (car group) stream))
      (dolist (item (cdr group))
        (write-char #\. stream)
        (write-string item stream)))))

(defmethod compute-name-string ((url news-article))
  (with-slots (message-id) url
    (with-output-to-string (stream)
      (write-scheme-prefix url stream)
      (when message-id
        (write-string message-id stream)))))

(define-parameter *url-host-name-resolution* :always
                  "Controls whether host names are DNS resolved
and connonicalized on the primary DNS name for the host when the URL is interned.
Values can be :ALWAYS    -- either resolve the name or signal an error.
              :PREFERRED -- try to resolve but don't signal an error
              :NEVER     -- never try to resolve until the host object is explicitly requested.")

;; cache the host object in order to canonicalize the name entry.  since
;; we're going to have to resolve this sooner or later anyway.
(defmethod compute-name-string :before ((url pathname-caching-mixin))
  (case *url-host-name-resolution*
    ((:always t) (host-object url nil t))
    ((:never nil) nil)
    (:preferred (host-object url t t))))

(defmethod compute-name-string ((url http-url))
  (with-output-to-string (stream)
    (write-scheme-prefix url stream)
    (write-host-port-string url stream)
    (write-local-name url stream)))

(defgeneric write-database (url &optional stream))

(defmethod write-database ((url wais-database-mixin) &optional (stream *standard-output*))
  (with-slots (database) url
    (write-string database stream)))

;; close off the WAIS path specification.
(defmethod write-path :after ((url wais-doc) &optional (stream *standard-output*))
  (with-slots (path) url
    (when path
      (write-char #\/ stream))))

(defmethod compute-name-string ((url wais-doc))
  (with-slots (type size) url
    (with-output-to-string (stream)
      (write-scheme-prefix url stream)
      (write-host-port-string url stream)
      (write-char #\/ stream)
      (write-database url stream)
      (write-char #\/ stream)
      (write-string type stream)
      (write-char #\/ stream)
      (write size :stream stream)
      (write-char #\/ stream)
      (write-path url stream)
      (write-object-name-string url stream))))

(defmethod compute-name-string ((url wais-search))
  (with-output-to-string (stream)
    (write-scheme-prefix url stream)
    (write-host-port-string url stream)
    (write-char #\/ stream)
    (write-database url stream)
    (write-search url stream)))

;; close off the Gopher path specification.
(defmethod write-path :after ((url gopher-path) &optional (stream *standard-output*))
  (with-slots (path) url
    (when path
      (write-char #\/ stream))))

(defmethod compute-name-string ((url gopher-path))
  (with-slots (type path) url
    (with-output-to-string (stream)
      (write-scheme-prefix url stream)
      (write-host-port-string url stream)
      (write-char #\/ stream)
      (write-string type stream)
      (if path
          (write-path url stream)
          (write-char #\/ stream)))))

;; close off the HTTP path specification.
(defmethod write-path :after ((url gopher-object) &optional (stream *standard-output*))
  (with-slots (path) url
    (when path
      (write-char #\/ stream))))

(defmethod compute-name-string ((url gopher-object))
  (with-slots (type) url
    (with-output-to-string (stream)
      (write-scheme-prefix url stream)
      (write-host-port-string url stream)
      (write-char #\/ stream)
      (write-string type stream)
      (write-path url stream)
      (write-object-name-string url stream))))

(defmethod compute-name-string ((url gopher-search))
  (with-slots (type) url
    (with-output-to-string (stream)
      (write-scheme-prefix url stream)
      (write-host-port-string url stream)
      (write-char #\/ stream)
      (write-string type stream)
      (write-path url stream)
      (write-search url stream))))

(define-generic name-string (url &optional compute-p)
  (:documentation "Returns the name string for URL.
If compute-p is non-null, this computes if from the parsed data."))

(defmethod name-string ((url url) &optional compute-p)
  (with-slots (name-string plist) url
    (cond ((or (null name-string) compute-p)
           (prog1 (setq name-string (compute-name-string url))
                  (remf plist :relative-name)
                  (remf plist :local-context-string)))
          (t name-string))))

(defmethod name-string ((url string) &optional compute-p)
  (declare (ignore compute-p))
  url)

(defun %relative-name-string (url-string &optional (length (length url-string)) (errorp-p t))
  (let ((pos1 (string-search= "://" url-string 0 3 0 (+ 3 (the fixnum *scheme-maximum-length*))))
        pos2)
    (cond ((and pos1 (setq pos2 (char-position #\/ url-string (+ 3 (the fixnum pos1)) length)))
           (make-array (- (the fixnum length) (the fixnum pos2))
                       :element-type (array-element-type url-string)
                       :displaced-to url-string :displaced-index-offset pos2))
          (pos1 "/")                            ;handle http://www.adobe.com
          (errorp-p (error 'no-scheme-found :url-string url-string))
          (t url-string))))

(define-generic relative-name-string (url &optional compute-p)
  (:documentation "Returns the name string for the URL without the scheme or host/port prefix."))

(defmethod relative-name-string ((url string) &optional compute-p)
  (declare (ignore compute-p))
  (%relative-name-string url (length url) nil))

(defmethod relative-name-string ((url url) &optional compute-p)
  (with-value-cached (url :relative-name :recompute-p compute-p)
    (let* ((name (name-string url compute-p))
           (length (length name)))
      (%relative-name-string name length t))))

(defun %relative-path-string (url-string &optional (end (length url-string)))
  (let* ((pos1 (string-search= "://" url-string 0 3 0 (+ 3 (the fixnum *scheme-maximum-length*))))
         pos2 pos3)
    (cond ((null pos1) (error 'no-scheme-found :url-string url-string))
          ((null (setq pos2 (char-position #\/ url-string (+ 3 (the fixnum pos1)) end)))
           nil)
          ((setq pos3 (char-position #\/ url-string (1+ (the fixnum pos2)) end t))
           (make-array (- (1+ (the fixnum pos3)) (the fixnum pos2))
                       :element-type (array-element-type url-string)
                       :displaced-to url-string :displaced-index-offset pos2))
          (t "/"))))

(define-generic relative-path-string (url &optional compute-p)
  (:documentation "Returns the path string for the URL without the scheme or host/port prefix."))

(defmethod relative-path-string ((url url) &optional compute-p)
  (with-value-cached (url :relative-path :recompute-p compute-p)
    (let* ((name (name-string url compute-p))
           (length (length name)))
      (%relative-path-string name length))))

(define-generic local-context-string (url &optional recompute-p)
  (:documentation "Returns a string consisting of the scheme, host, and path."))

(defmethod local-context-string ((url url:http-url) &optional recompute-p)
  (with-value-cached (url :local-context-string :recompute-p recompute-p)
    (with-output-to-string (stream)
      (write-scheme-prefix url stream)
      (write-host-port-string url stream)
      (write-path url stream))))

(define-generic root-context-string (url &optional recompute-p)
  (:documentation "Returns a string consisting of the scheme and host with the root /."))

(defmethod root-context-string ((url url:http-url) &optional recompute-p)
  (with-value-cached (url :root-context-string :recompute-p recompute-p)
    (with-output-to-string (stream)
      (write-scheme-prefix url stream)
      (write-host-port-string url stream)
      (write-char #\/ stream))))

(define-generic write-url-context (url &optional stream)
  (:documentation "Writes the URL scheme and host/port info on stream."))

(defmethod write-url-context ((url http-url) &optional (stream *standard-output*))
  (write-scheme-prefix url stream)
  (write-host-port-string url stream))

(defmethod write-url-context ((url ftp-url) &optional (stream *standard-output*))
  (write-scheme-prefix url stream)
  (write-host-port-string url stream))

(define-generic name-string-without-extension (url)
  (:documentation "Returns the name-string of URL without any extension.")) 

(defmethod name-string-without-extension ((url object-mixin))
  (with-output-to-string (stream)
    (write-scheme-prefix url stream)
    (write-host-port-string url stream)
    (write-path url stream)
    (write-char #\/ stream)
    (write-object-name-string url stream nil)))

(defmethod name-string-without-extension ((url url))
  (name-string url))

(define-generic load-balanced-url-string (cluster url)
  (:documentation "Returns a fully qualified URL string with random host/port
selected from the load balancing cluster, CLUSTER.
Because this operation conses, applications may wish to arrange
for the consing to take place on the stack by using a dynamic extent
declaration. However, when URL is an interned URL, as opposed to a string,
(RELATIVE-NAME-STRING URL) must be called outside a dynamic extent declaration
in order to initialize the relative name string for URL. Otherwise,
unpredictable results will occur because the stack-consed relative name string
will be cached and reused."))

(defmethod load-balanced-url-string (cluster (url url:http-url))
  (%load-balanced-url cluster (relative-name-string url)))

(defmethod load-balanced-url-string (cluster (url string))
  (%load-balanced-url cluster (%relative-name-string url)))


;;;------------------------------------------------------------------- 
;;;
;;; REMAPPING HOSTS
;;;

(define-generic change-host (url host-name port)
  (declare (values registered-url))
  (:documentation "Changes the host and port number of URL to HOST-NAME and PORT."))

(defmethod change-host ((url host-mixin) (host-name string) (port integer))
  (unregister url)
  ;; Remove any cached values the refer to the old host or port
  (dolist (cache-key '(:local-context-string :directory-string))
    (remove-value url cache-key))
  (setf (host-string url) host-name
        (port url) (if (and (= port 80) (typep url 'http-url)) nil port)
        (%host-object url) nil)
  (name-string url t)
  (register url))

(define-generic host-match-p (url host port)
  (:documentation "Returns non-null if URL is defined on HOST and PORT.
HOST can be either a string or a host object (host number).
If PORT is an integer, the URL must be one the same port, whereas
if PORT is null, the URL may be on any port."))

(defmethod host-match-p ((url host-mixin) (host-name string) (port integer))
  (and (= port (host-port url))
       (equalp host-name (host-string url))))

(defmethod host-match-p ((url host-mixin) (host-name string) (port null))
  (equalp host-name (host-string url)))

(defmethod host-match-p ((url host-mixin) host (port integer))
  (and (= port (host-port url))
       (host-eq host (host-object url))))

(defmethod host-match-p ((url host-mixin) host (port null))
  (host-eq host (host-object url)))

(define remap-url-host (old-host new-host &key (old-port http:*standard-http-port*) (new-port nil new-port-supplied-p)
                                 (report-stream *standard-output*))
  "Remaps all known URLs from OLD-HOST OLD-PORT to NEW-HOST NEW-PORT.
OLD-PORT defaults to the standard HTTP port. NEW-PORT defaults to OLD-PORT.
However, if NEW-PORT is NIL, the current port value is preserved."
  (let ((n-port (if new-port-supplied-p
                    new-port
                    (or old-port http:*standard-http-port*))))
    (format report-stream "~&Remapping URLs from ~A:~:[*~;~:*~D~] to ~A:~:[*~;~:*~D~] . . . ."
            old-host old-port new-host n-port)
    (loop for url being each hash-value in *url-table*
          when (host-match-p url old-host old-port)
            do (change-host url new-host (or n-port (port url)))
            and sum 1 into count
          finally (format report-stream "~&~D URLs remapped from ~A:~:[*~;~:*~D~] to ~A:~:[*~;~:*~D~]."
                          count old-host old-port new-host n-port))
    *url-table*))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;


(define-generic url-superior (url)
  (:documentation "Returns the url that is the immediate directory superior of url.
If URL is the root url, it returns self."))

(defmethod url-superior ((url string))
  (url-superior (intern-url url)))

(defmethod url-superior ((url object-mixin))
  (let* ((name (name-string url))
         (pos (char-position #\/ name 0 (length name) t)))
    (intern-url name :start 0 :end (1+ pos))))

(defmethod url-superior ((url path-mixin))
  (let* ((name (name-string url))
         (length (length name))
         (pos (char-position #\/ name 0 (1- length) t)))
    (declare (fixnum length))
    (if (and (eql #\/ (aref name (1- pos)))
             (eql #\: (aref name (- pos 2))))
        url                                     ;root URL
        (intern-url name :start 0 :end (1+ pos)))))

(define-generic display-string (url)
  (:documentation "Returns the pretty human-readable string for the URL (typically the title)."))

(defmethod display-string ((url displayable-name-mixin))
  (with-slots (display-string) url
    (or display-string
        (setf (display-string url) (name-string url)))))

(defmethod (setf display-string) ((string string) (url displayable-name-mixin))
  (with-slots (display-string) url
    (setq display-string string)))

(define-generic write-name (url &optional stream)
  (:documentation "Writes the URLs print name on STREAM."))

(defmethod write-name ((url url) &optional (stream *standard-output*))
  (write-string (name-string url) stream))

(defmethod write-name ((url string) &optional (stream *standard-output*))
  (write-string url stream))

(define-generic path-title (url)
  (:documentation "Returns a title for a URL that can be used in a document.
Heuristically converts two-digit month to the appropriate month string."))

(defmethod path-title ((url url:http-path))
  (loop with month and year
        for item in (path url)
        for len = (length item)
        for string = (cond
                       ((and (= len 2)
                             (every #'digit-char-p item))
                        (setq month (www-utils:month-string (parse-integer item :end 2)))
                        nil)
                       ((and (= len 4)
                             (every #'digit-char-p item))
                        (setq year (parse-integer item :end 4))
                        nil)
                       (t (substitute #\space #\- item)))
        when string
          collect string into result
        finally (return (format nil "~{~:(~A~)~^ ~}~:[~; ~:*~:(~A~)~]~:[~; ~:*~A~]" result month year))))

(define-generic path-most-specific-name (url)
  (:documentation "Returns the most specific component of the path for a URL."))

(defmethod path-most-specific-name ((url url:http-path))
  (car (last (path url))))

(define-generic path-directory-string (url &optional recompute-p)
  (:documentation "Returns the directory string containing the path component of URL."))

(defmethod path-directory-string ((url url:http-path) &optional recompute-p)
  (with-value-cached (url :directory-string :recompute-p recompute-p)
    (format nil "/~{~A/~}" (path url))))

(define-generic directory-name-string (url &optional recompute-p)
  (:documentation "Returns a string consisting of the scheme, host, and path, with all directory components,
ending with a slash."))

(defmethod directory-name-string ((url url:http-url) &optional recompute-p)
  (with-value-cached (url :directory-name-string :recompute-p recompute-p)
    (with-output-to-string (stream)
      (write-scheme-prefix url stream)
      (write-host-port-string url stream)
      (write-path url stream)
      (write-char #\/ stream))))

(defmethod object ((path-mixin path-mixin)) nil)

(defmethod extension ((path-mixin path-mixin)) nil)

;;;------------------------------------------------------------------- 
;;;
;;;  DOCUMENTATION OPERATIONS
;;;

(define-generic keywords (url)
  (:documentation "Returns a list of keywords for URL. When none exists, returns nil."))

(defmethod keywords ((url documentation-mixin))
  (with-slots (plist) url
    (getf plist :keywords)))

(defmethod %set-keywords ((url documentation-mixin) keywords)
  (with-slots (plist) url
    (unless (every #'keywordp keywords)
      (error "Attempt to set keywords to items not all of which are keywords."))
    (if keywords
        (setf (getf plist :keywords) keywords)
        (remf plist :keywords))))

(defsetf keywords %set-keywords)

(define-generic add-keyword (url keyword)
  (:documentation "Adds a keyword to the keywords associated with URL."))

(defmethod add-keyword ((url documentation-mixin) keyword) 
  (with-slots (plist) url
    (check-type keyword keyword)
    (pushnew keyword (getf plist :keywords))))

(define-generic remove-keyword (url keyword)
  (:documentation "Removes a keyword from the keywords associated with URL."))

(defmethod remove-keyword ((url documentation-mixin) keyword)
  (with-slots (plist) url
    (setf (getf plist :keywords) (delete keyword (getf plist :keywords)))))

(define-generic description (url)
  (:documentation "Returns a documentation string for URL. When none exists, returns nil."))

(defmethod description ((url documentation-mixin))
  (with-slots (plist) url
    (getf plist :description)))

(defmethod %set-description ((url documentation-mixin) description)
  (with-slots (plist) url
    (cond (description
           (check-type description string)
           (setf (getf plist :description) description))
          (t (remf plist :description)))))

(defsetf description %set-description)

(defmethod documentation ((url documentation-mixin) #-(or CMU LispWorks4) &optional doc-type)
  (declare (ignore doc-type))
  (with-slots (plist) url
    (getf plist :description)))

(define-generic initialize-documentation (url keywords description)
  (:documentation "Initializes URLs keywords as KEYWORDS and documentation as DESCRIPTION."))

(defmethod initialize-documentation ((url documentation-mixin) keywords description)
  (setf (keywords url) keywords
        (description url) description)
  url) 

;;;------------------------------------------------------------------- 
;;;
;;; SPECIAL METHODS ON SEARCH URLS
;;;

(setf (documentation 'search-keys 'function) 
      "Returns the parsed search keys as a list of strings. The search keys are the
information following the ?.  This information is unescaped according to the
URL standard and each item (delimited by +) becomes a single element in the
returned list.")

(declaim (inline %strip-url-suffix))

(defun %strip-url-suffix (string delimiter include-suffix-delimiter-p)
  (let* ((len (length string))
         (pos (char-position delimiter string 0 len)))
    (declare (fixnum len))
    (cond ((or (null pos)
               (and include-suffix-delimiter-p (= len (setq pos (1+ (the fixnum pos))))))
           string)
          (t (subseq string 0 pos)))))

(define-generic name-string-without-search-suffix (url &optional include-suffix-delimiter-p)
  (:documentation "Returns the name-string of URL without the search suffix."))

(defmethod name-string-without-search-suffix ((url string) &optional include-suffix-delimiter-p)
  (%strip-url-suffix url *search-url-delimiter* include-suffix-delimiter-p))

(defmethod name-string-without-search-suffix ((url search-mixin) &optional include-suffix-delimiter-p)
  (with-slots (name-string) url
    (%strip-url-suffix name-string *search-url-delimiter* include-suffix-delimiter-p)))

(defmethod name-string-without-search-suffix ((url http-url) &optional include-suffix-delimiter-p)
  (declare (ignore include-suffix-delimiter-p))
  (with-slots (name-string) url
    name-string))

(declaim (inline %url-string-with-unescaped-search-suffix))

(defun %url-string-with-unescaped-search-suffix (string)
  (let* ((len (length string))
         (pos (char-position *search-url-delimiter* string 0 len)))
    (declare (fixnum len))
    (cond ((or (null pos) (= (1+ (the fixnum pos)) len))
           string)
          (t (let* ((idx (1+ (the fixnum pos)))
                    (stem (subseq string 0 idx))
                    (suffix (http:string-unescape-special-chars string idx len)))
               (declare (dynamic-extent stem suffix))
               (concatenate 'string stem suffix))))))

(define-generic name-string-with-unescaped-search-suffix (url &optional recache-p)
  (:documentation "Returns the name-string of URL without escaping the search suffix."))

(defmethod name-string-with-unescaped-search-suffix ((url search-mixin) &optional recache-p)
  (with-slots (name-string) url
    (with-value-cached (url 'name-string-with-unescaped-search-suffix :recompute-p recache-p)
      (%url-string-with-unescaped-search-suffix name-string))))

(defmethod compute-name-string :before ((url search-mixin))
  (remove-value url 'name-string-with-unescaped-search-suffix))

(define-generic search-suffix (url &optional include-suffix-delimiter-p)
  (:documentation "Returns the search suffix of a search URL."))

(defmethod search-suffix ((url search-mixin) &optional include-suffix-delimiter-p)
  (with-slots (name-string) url
    (let* ((len (length name-string))
           (pos (char-position *search-url-delimiter* name-string 0 len)))
      (declare (fixnum len pos))
      (if (and pos (/= pos len))
          (subseq name-string (if include-suffix-delimiter-p pos (the fixnum (1+ pos))) len)
          ""))))

(defmethod search-suffix ((url http-url) &optional include-suffix-delimiter-p)
  (declare (ignore include-suffix-delimiter-p))
  nil)

(defparameter *url-search-parent* nil
  "Holds the dynamic URL which is the parent for the current search.")

(define-macro with-url-search-parent ((parent-url) &body body)
  "Asserts that parent-url is the search parent within the scope of BODY.
This allows operations with the scope of body to determine the URL which
initiated the search."
  `(let ((*url-search-parent* ,parent-url)) ,@body))

(define-generic search-parent (url &key if-does-not-exist lexical-parent recache-p)
  (declare (values url intern-status))
  (:documentation "Returns the URL that governs the current search url.
When LEXICAL-PARENT is non-null, this returns the lexical search parent for
URL.  When LEXICAL-PARENT is null, this returns the dynamic search parent, if
different from the lexical parent."))

;(defmethod search-parent ((url search-mixin) &key (lexical-parent t) (if-does-not-exist :create) &aux pos)
;  (with-slots (name-string) url
;    (cond ((and (null lexical-parent) *url-search-parent*))
;          ((setq pos (position *search-url-delimiter* name-string :test #'char=))
;           (with-value-cached (url :search-parent)
;             (intern-url name-string :start 0 :end (the fixnum (1+ pos)) :if-does-not-exist if-does-not-exist)))
;          ((eq if-does-not-exist :soft) nil)
;          (t (error 'no-search-parent-found :url url)))))

(defmethod search-parent ((url search-mixin) &key (lexical-parent t) (if-does-not-exist :create) recache-p &aux pos)
  (with-slots (name-string) url
    (cond ((and (null lexical-parent) *url-search-parent*))
          ((and (null recache-p) (%search-parent url)))
          ((setq pos (char-position *search-url-delimiter* name-string))
           (setf (%search-parent url) (intern-url name-string :start 0 :end (the fixnum (1+ pos))
                                                  :if-does-not-exist if-does-not-exist)))
          ((eq if-does-not-exist :soft) nil)
          (t (error 'no-search-parent-found :url url)))))

;; return NIL for URLs other than ones mixing in search-mixin.
(defmethod search-keys ((url url)) nil)

(define-generic valid-search-url-p (url)
  (declare (values valid-search-url-p search-parent next-index))
  (:documentation "Returns non-null if URL is a search URL with a interned search parent."))

(defmethod valid-search-url-p ((url string) &aux pos parent)
  (cond ((and (setq pos (char-position *search-url-delimiter* url))
              (setq parent (url:intern-url url :if-does-not-exist :soft :end (setq pos (1+ (the fixnum pos))))))
         (values t parent pos))
        (t nil)))

(defmethod valid-search-url-p ((url search-mixin))
  (if (search-parent url :if-does-not-exist :soft) t nil))

(defmethod valid-search-url-p ((url url:url)) nil)

(declaim (inline %translation-method))

(defun %translation-method (url)
  (flet ((inherit-translation-method (url)
           (let ((parent (search-parent url :if-does-not-exist :error))
                 method)
             (unless (eql parent url)
               (when (setq method (translation-method parent))
                 (setf (translation-method url) method))
               method))))
    (declare (inline inherit-translation-method))
    (with-slots (translation-method) url
      (or translation-method
          (inherit-translation-method url)))))

(defmethod  translation-method ((url http-search))
  (%translation-method url))

(defmethod  translation-method ((url http-searchable-object))
  (%translation-method url))

(defmethod header-function ((url search-mixin) &aux parent)
  (with-slots (header-function) url
    (cond (header-function)
          ((and (setq parent (search-parent url :if-does-not-exist :error))
                (not (eq parent url)))
           (header-function parent))
          (t nil))))

(defmethod response-function ((url search-mixin))
  (with-slots (response-function) url
    (cond (response-function)
          (t (let ((parent (search-parent url :if-does-not-exist :error)))
               (response-function parent))))))

(define-generic search-database (search-mixin)
  (:documentation "Returns the search database associated with a search URL at export time."))

(defmethod search-database ((url search-mixin))
  (with-slots (search-database) url
    (cond (search-database)
          (t (let ((parent (search-parent url :if-does-not-exist :error)))
               (search-database parent))))))

(define-generic inherit-parent-access-controls (search-mixin)
  (:documentation "Inherits the security slots of the parent url."))

(defmethod inherit-parent-access-controls ((url search-mixin))
  (let ((parent (search-parent url :if-does-not-exist :soft)))
    (when parent
      (setf (secure-subnets url) (secure-subnets parent)
            (authentication-realm url) (authentication-realm parent)
            (capabilities url) (capabilities parent)))))


;;;------------------------------------------------------------------- 
;;;
;;; PATHNAME TRANSLATION
;;;

(define-variable *text-copy-mode-content-types* nil
                 "The  data types that use :TEXT copy-mode.")

(define-variable *copy-mode-content-type-alist* nil
                 "An alist of (data-type copy-mode).")

(define-variable *crlf-copy-mode-content-types* nil
                 "The  data types that use :CRLF copy-mode.")

(defun %note-content-type-copy-mode (content-type copy-mode)
  (ecase copy-mode
    (:text (pushnew content-type *text-copy-mode-content-types*))
    (:binary t)
    (:crlf (pushnew content-type *crlf-copy-mode-content-types*)))
  (let ((entry (assoc content-type *copy-mode-content-type-alist*)))
    (cond (entry
           (setf (second entry) copy-mode))
          (t (push `(,content-type ,copy-mode) *copy-mode-content-type-alist*)))
    copy-mode))

(defun %content-type-copy-mode (content-type &optional (error-p t))
  (cond ((second (assoc content-type *copy-mode-content-type-alist*)))
        (error-p (error "~S is not a known content type keyword." content-type))
        (t nil)))

(defun %unnote-content-type-copy-mode (content-type)
  (case (%content-type-copy-mode content-type nil)
    (:text 
      (setq *text-copy-mode-content-types*
            (delete content-type *text-copy-mode-content-types*)))
    (:crlf
      (setq *crlf-copy-mode-content-types*
            (delete content-type *crlf-copy-mode-content-types*)))
    (t nil))
  (setq *copy-mode-content-type-alist* (delete content-type *copy-mode-content-type-alist* :key #'first)))

(declaim (inline content-type-copy-mode-is-text-p))

(defun content-type-copy-mode-is-text-p (content-type) 
  (member content-type *text-copy-mode-content-types* :test #'equalp))

(declaim (inline content-type-copy-mode-is-crlf-p))

(defun content-type-copy-mode-is-crlf-p (content-type) 
  (member content-type *crlf-copy-mode-content-types* :test #'equalp))

(define-variable *text-copy-mode-pathname-extensions* nil
                 "The pathname extension types that use :TEXT copy-mode.")

(define-variable *crlf-copy-mode-pathname-extensions* nil
                 "The pathname extension types that use :CRLF copy-mode.")

(defun %note-pathname-extension-type-copy-mode (pathname-type copy-mode)
  (ecase copy-mode
    (:text (pushnew pathname-type *text-copy-mode-pathname-extensions*))
    (:binary t)
    (:crlf (pushnew pathname-type *crlf-copy-mode-pathname-extensions*))))

(defun %unnote-pathname-extension-type-copy-mode (pathname-type)
  (setq *text-copy-mode-pathname-extensions*
        (delete pathname-type *text-copy-mode-pathname-extensions*))
  (setq *crlf-copy-mode-pathname-extensions*
        (delete pathname-type *crlf-copy-mode-pathname-extensions*)))

(declaim (inline copy-mode-is-text-p))

(defun copy-mode-is-text-p (pathname-extension) 
  (member pathname-extension *text-copy-mode-pathname-extensions* :test #'equalp))

(declaim (inline copy-mode-is-crlf-p))

(defun copy-mode-is-crlf-p (pathname-extension) 
  (member pathname-extension *crlf-copy-mode-pathname-extensions* :test #'equalp))

(define-variable *pathname-application-extensions* nil
                 "Extensions used to denote application files on local file servers.")

(define-variable *pathname-text-extensions* '(:text :txt)
                 "Extensions used to denote text files on local file servers.")

(define-variable *pathname-html-extensions* '(:html :htm)
                 "Extensions used to denote html files on local file servers.")

(define-variable *pathname-lisp-extensions* '(:lisp :lsp)
                 "Extensions used to denote lisp files on local file servers.")

(define-variable *pathname-image-extensions* '(:xbm :gif :jpeg)
                 "Extensions used to denote image files on local file servers.")

(define-variable *pathname-audio-extensions* '(:au)
                 "Extensions used to denote audio files on local file servers.")

(define-variable *pathname-video-extensions* '(:mpeg)
                 "Extensions used to denote video files on local file servers.")

(define-variable *pathname-world-extensions* '(:wrl)
                 "Extensions used to denote world files on local file servers.")

(define-variable *pathname-message-extensions* '()
                 "Extensions used to denote message files on local file servers.") 

(define note-pathname-extension-type (pathname-type data-type &optional (copy-mode :binary))
  "Records that the pathname extension, PATHNAME-TYPE, is of type, data-type,
which can be any of :TEXT, :LISP, :HTML ,:IMAGE, :AUDIO, :VIDEO, :APPLICATION, :WORLD, :MESSAGE."
  (check-type pathname-type keyword) 
  (ecase data-type
    (:text (pushnew pathname-type *pathname-text-extensions*))
    (:lisp (pushnew pathname-type *pathname-lisp-extensions*))
    (:html (pushnew pathname-type *pathname-html-extensions*))
    (:image (pushnew pathname-type *pathname-image-extensions*))
    (:audio (pushnew pathname-type *pathname-audio-extensions*))
    (:video (pushnew pathname-type *pathname-video-extensions*))
    (:application (pushnew pathname-type  *pathname-application-extensions*))
    (:world (pushnew pathname-type  *pathname-world-extensions*))
    (:message (pushnew pathname-type  *pathname-message-extensions*)))
  (%note-pathname-extension-type-copy-mode pathname-type copy-mode)
  pathname-type)

(define unnote-pathname-extension-type (pathname-type data-type)
  "Forgets that the pathname extension, PATHNAME-TYPE, is of type, data-type¬,
which can be any of :TEXT, :LISP, :HTML ,:IMAGE, :AUDIO, :VIDEO, :APPLICATION, :WORLD, :MESSAGE."
  (macrolet ((deletef (item place)
               `(setq ,place (delete ,item ,place))))
    (ecase data-type
      (:text (deletef pathname-type *pathname-text-extensions*))
      (:lisp (deletef pathname-type *pathname-lisp-extensions*))
      (:html (deletef pathname-type *pathname-html-extensions*))
      (:image (deletef pathname-type *pathname-image-extensions*))
      (:audio (deletef pathname-type *pathname-audio-extensions*))
      (:video (deletef pathname-type *pathname-video-extensions*))
      (:application (deletef pathname-type  *pathname-application-extensions*))
      (:world (deletef pathname-type  *pathname-world-extensions*))
      (:message (deletef pathname-type  *pathname-message-extensions*)))
    (%unnote-pathname-extension-type-copy-mode pathname-type)))

(define-variable *data-type-pathname-extension-variable-alist*
  '((:text . *pathname-text-extensions*)
    (:lisp . *pathname-lisp-extensions*)
    (:html . *pathname-html-extensions*)
    (:image . *pathname-image-extensions*)
    (:audio . *pathname-audio-extensions*)
    (:video . *pathname-video-extensions*)
    (:application . *pathname-application-extensions*)
    (:world . *pathname-world-extensions*)
    (:message . *pathname-message-extensions*))
  "Maps data type keywords to variable name that tell what pathname extnesions belong to the class.")

(defun %data-type-pathname-extensions-variable (data-type)
  (let ((entry (assoc data-type *data-type-pathname-extension-variable-alist* :test #'eq)))
    (cond (entry (symbol-value (cdr entry)))
          (t (error "~S is not a known data type." data-type)))))

(defun data-type-pathname-extensions (data-type)
  (let ((var (cdr (assoc data-type *data-type-pathname-extension-variable-alist* :test #'eq))))
    (cond (var (symbol-value var))
          (t (error "~S is not a known data type." data-type)))))

;; Could be coded inline but that would reduce extensibiliy.
(defmacro %pathname-type-test (pathname variable &key null-pathname-type-matches-p)
  (let ((test `(member type ,variable :test #'(lambda (type possibility)
                                                (equalp type (symbol-name possibility))))))
    `(let ((type (pathname-type (pathname ,pathname))))
       (if ,null-pathname-type-matches-p
           (or (null type) ,test)
           ,test)))) 

(define pathname-text-file-p (pathname &optional match-null-pathname-type-p)
  (%pathname-type-test pathname *pathname-text-extensions* 
                       :null-pathname-type-matches-p match-null-pathname-type-p))

(define pathname-lisp-file-p (pathname &optional match-null-pathname-type-p)
  (%pathname-type-test pathname *pathname-lisp-extensions* 
                       :null-pathname-type-matches-p match-null-pathname-type-p))

(define pathname-html-file-p (pathname &optional match-null-pathname-type-p)
  (%pathname-type-test pathname *pathname-html-extensions*
                       :null-pathname-type-matches-p match-null-pathname-type-p))

(define pathname-image-file-p (pathname &optional match-null-pathname-type-p)
  (%pathname-type-test pathname *pathname-image-extensions*
                       :null-pathname-type-matches-p match-null-pathname-type-p))

(define pathname-audio-file-p (pathname &optional match-null-pathname-type-p)
  (%pathname-type-test pathname *pathname-audio-extensions*
                       :null-pathname-type-matches-p match-null-pathname-type-p))

(define pathname-video-file-p (pathname &optional match-null-pathname-type-p)
  (%pathname-type-test pathname *pathname-video-extensions*
                       :null-pathname-type-matches-p match-null-pathname-type-p))

(define pathname-application-file-p (pathname &optional match-null-pathname-type-p)
  (%pathname-type-test pathname *pathname-application-extensions*
                       :null-pathname-type-matches-p match-null-pathname-type-p))

(define pathname-world-file-p (pathname &optional match-null-pathname-type-p)
  (%pathname-type-test pathname *pathname-world-extensions*
                       :null-pathname-type-matches-p match-null-pathname-type-p))

(define pathname-message-file-p (pathname &optional match-null-pathname-type-p)
  (%pathname-type-test pathname *pathname-message-extensions*
                       :null-pathname-type-matches-p match-null-pathname-type-p))

(define-variable *data-type-pathname-predicate-alist* 
  '((:html . pathname-html-file-p)
    (:text . pathname-text-file-p)
    (:lisp . pathname-lisp-file-p)
    (:image . pathname-image-file-p)
    (:audio . pathname-audio-file-p)
    (:video . pathname-video-file-p)
    (:application . pathname-application-file-p)
    (:world . pathname-world-file-p)
    (:message . pathname-message-file-p))
  "Maps data type keywords to predicates that tell whether a pathname belongs to the class.")

(defun data-type-pathname-predicate (data-type)
  (or (cdr (assoc data-type *data-type-pathname-predicate-alist* :test #'eq))
      (error "~S is not a known data type." data-type)))

(define data-type-keyword-p (keyword)
  (member keyword *data-type-pathname-predicate-alist* :test #'(lambda (x y) (eq x (car y)))))

(defun %make-http-url-string (host port path name extension search-args)
  (flet ((host-string (host)
                      (let* ((l (length host))
                             (pos1 (if (string-equal "http://" HOST :start1 0 :end1 7. :start2 0 :end2 (min 7 l))
                                       7.))
                             (pos2 (or (char-position #\: host (or pos1 0) l t)
                                       (char-position #\/ host (1- (the fixnum l)) l t))))
                        (declare (fixnum l))
                        (cond ((or pos1 pos2)
                               (subseq host (or pos1 0) (or pos2 l)))
                              (t host))))
         (name-spec ()
           `(,name
             ,.(when extension `("." ,extension))
             ,.(cond ((eql search-args t) (list "?"))
                     (search-args
                      (check-type search-args cons)
                      (loop for (item . next) = search-args then next
                            collect item into args
                            while next
                            collect "+" into args
                            finally (return (cons "?" args))))
                     (t nil)))))
    (declare (dynamic-extent #'host-string)
             (inline name-spec))
    (loop for item in path
          collect item into path-spec
          collect "/" into path-spec
          finally (when name
                    (if path-spec
                        (setf (cdr (last path-spec)) (name-spec))
                        (setq path-spec (name-spec))))
                  (push "/" path-spec)
                  (when host
                    (etypecase port
                      (null)
                      (number
                        (unless (= port 80.)
                          (push (write-to-string port :base 10 :escape nil) path-spec)
                          (push ":" path-spec)))
                      (string
                        (push port path-spec)
                        (push ":" path-spec)))
                    (push (host-string host) path-spec)
                    (push "http://" path-spec))
                  (return (apply #'concatenate 'string path-spec)))))

(define-macro make-http-url-string (&key host port path name extension search-args search-p)
  "Makes an HTTP string from the components."
  `(%make-http-url-string ,host ,port ,path ,name ,extension
                          ,(or search-args (when search-p 't))))

(define make-http-url-spec-from-pathname (pathname &optional directory-file-p http-host port extension)
  "Returns an HTTP URL spec for pathname using pathname host as the host.
If DIRECTORY-FILE-P, name and type of pathname are ignored.  HTTP-HOST overides
the pathnamehost.  PORT  specifies port for HTTP."
  (flet ((host-string (pathname)
                      (let ((host (pathname-host pathname)))
                        (case host
                          ((:unspecific nil) (www-utils:local-host-domain-name))
                          (t (host-http-name host))))))
    (declare (inline host-string))
    (let* ((path (pathname pathname))
           (host-string (or http-host (host-string path)))
           (directory (pathname-directory path))
           (name (pathname-external-name-string (pathname-name path))))
      (if directory-file-p
          (make-http-url-string
            :host host-string
            :port port
            :path (if name                      ; handle lispm case of directory file pathnames path-n.directory
                      `(,@(cdr directory) ,name)
                      (cdr directory)))
          (make-http-url-string
            :host host-string
            :port port
            :path (cdr directory)
            :name name
            :extension (or extension (pathname-type path)))))))

(define intern-pathname-as-http-url (pathname &key (if-does-not-exist :create) directory-file-p
                                              http-host port)
  "Returns an HTTP URL for pathname using pathname host as the host.
If DIRECTORY-FILE-P, name and type of pathname are ignored.  HTTP-HOST overides
the pathnamehost.  PORT  specifies port for HTTP."
  (declare (values url intern-status))
  (let ((spec (make-http-url-spec-from-pathname pathname directory-file-p http-host port)))
    (intern-url spec :if-does-not-exist if-does-not-exist)))

(define-generic intern-pathname-as-url (pathname url-scheme &key if-does-not-exist http-host port)
  (declare (values url intern-status))
  (:documentation "Interns a URL based on PATHNAME and URL-SCHEME,
which can be :HTTP. HTTP-HOST overides the pathnamehost.  PORT 
specifies port for HTTP."))

(defmethod intern-pathname-as-url ((pathname pathname) (url-scheme (eql :http))
                                   &key (if-does-not-exist :create) http-host port)
  (let ((spec (make-http-url-spec-from-pathname pathname
                                                (pathname-directory-p pathname)
                                                http-host port)))
    (intern-url spec :if-does-not-exist if-does-not-exist)))

(define-generic intern-pathname-as-url-inferior (pathname url &key extension if-does-not-exist directory-p)
  (declare (values url intern-status))
  (:documentation
    "Interns a URL as an inferior of URL-PATH with name and extension derived from pathname.
When EXTENSION is non-null, it overrides the pathname type in the resulting URL.
DIRECTORY-P may be used to indicate that pathname is already known to be a directory."))

(defmethod intern-pathname-as-url-inferior ((pathname pathname) (url path-mixin) &key extension if-does-not-exist
                                            (directory-p (pathname-directory-p pathname)))
  (cond (directory-p
         (let* ((name (pathname-external-name-string (pathname-directory-most-specific-name pathname)))
                (string (concatenate 'string (url:name-string url) name "/")))
           (declare (dynamic-extent name string))
           (url:intern-url string :if-does-not-exist if-does-not-exist)))
        (t (let* ((name (pathname-external-name-string (pathname-name pathname)))
                  (string (concatenate 'string (url:name-string url) name "." (or extension (pathname-type pathname)))))
             (declare (dynamic-extent name string))
             (url:intern-url string :if-does-not-exist if-does-not-exist)))))

(defmethod intern-pathname-as-url-inferior :before ((pathname pathname) (url object-mixin) &key extension
                                                    if-does-not-exist directory-p)
  (declare (ignore extension if-does-not-exist directory-p))
  (error "This operation is not defined for url objects."))


(define-generic intern-url-inferior (superior-url export-type http-method url-string)
  (declare (values url newly-interned-p))
  (:documentation "Interns URL-STRING an inferior of SUPERIOR-URL according to EXPORT-TYPE."))

;; Default method - most URLs don't support URL inferiors.
(defmethod intern-url-inferior (superior export-type http-method url-string)
  (declare (ignore superior export-type http-method url-string))
  nil)

(defmethod intern-url-inferior :around ((superior http-path) export-type http-method url-string)
  (multiple-value-bind (url newly-interned-p)
      (call-next-method superior export-type http-method url-string)
    (when newly-interned-p
      (cond ((and (typep url 'http-object)
                  (or (directory-export-type-p export-type) (hierarchical-directory-export-type-p export-type)))
             (let ((extension (extension url))
                   translation)
               (when extension
                 (when (and (setq translation (http:export-type-for-pathname-type extension nil))
                            (directory-type-exports-pathname-export-type-p export-type translation))
                   (setf (translation-method url) translation)))))
            (t (setf (translation-method url) (translation-method superior)))))
    (values url newly-interned-p)))

(defmethod intern-url-inferior :around ((superior http-minimum-object) export-type http-method url-string)
  (multiple-value-bind (url newly-interned-p)
      (call-next-method superior export-type http-method url-string)
    (when newly-interned-p
      (setf (translation-method url) (translation-method superior)))
    (values url newly-interned-p)))

(defmethod intern-url-inferior ((superior http-url) (export-type (eql :computed)) http-method url-string)
  (case http-method
    ((:head :get)                               ;always specialize to correct methods to avoid security and scope issues
     (multiple-value-bind (url newly-interned-p)
         (intern-url url-string :if-does-not-exist :uninterned)
       (when newly-interned-p
         (change-class url (type-of superior))
         ;; inherit all relevant parameters from superior, including security properties
         (inherit-export-parameters url superior))
       (values url newly-interned-p)))
    (t nil)))

(defmethod translated-pathname (url)
  (declare (ignore url))
  nil)

(defmethod translated-pathname :around ((url pathname-caching-mixin))
  (with-slots (pathname) url
    (or pathname
        (setq pathname (call-next-method)))))

(defmethod translated-pathname ((url path-mixin))
  (let ((directory `(:absolute ,. (path url))))
    (make-pathname :host (host-for-pathname url)
                   :device (device-for-pathname url)
                   :directory directory)))

(defmethod translated-pathname ((url object-mixin))
  (let ((directory `(:absolute ,. (path url))))
    (make-pathname :host (host-for-pathname url)
                   :device (device-for-pathname url)
                   :directory directory
                   :name (object url)
                   :type (extension url)
                   :version nil)))

(defmethod (setf translated-pathname) (value (url pathname-caching-mixin))
  (with-slots (pathname) url
    ;; lose versions for easy update.
    (let ((path (setf (pathname-version (pathname value)) nil)))
      (setq pathname path))))

(defmethod (setf translation-method) (method (url translation-method-mixin))
  (with-slots (translation-method) url
    (setq translation-method method)))

(defmethod (setf cached-pathname) (value (url pathname-caching-mixin))
  (with-slots (pathname) url
    (let ((path (and value (setf (pathname-version (pathname value)) nil))))
      (setq pathname path))))

(defmethod (setf alternate-urls) (value (url alternate-url-mixin))
  (with-slots (urls) url
    (setq urls (mapcar #'(lambda (x)
                           (intern-url x :if-does-not-exist :create))
                       value))))

(defmethod (setf header-function) (function (url computed-headers-mixin))
  (check-type function (or null (satisfies good-response-function-p)))
  (with-slots (header-function) url
    (setq header-function function)))

(defmethod (setf response-function) (function (url computed-url-mixin))
  (check-type function (satisfies good-response-function-p))
  (with-slots (response-function) url
    (setq response-function function)))

(defmethod (setf form-server) (set-server (url form-processing-mixin))
  (with-slots (server) url
    (setq server set-server)))


;;;------------------------------------------------------------------- 
;;;
;;; FORM URLS
;;;

(defmacro with-class-change-for-initialize-specialization ((url class init-args) &body body)
  `(cond ((typep ,url ,class) ,@body)
         (t (let ((new-object (change-class ,url ,class)))
              (initialize-specialization new-object ,class ,init-args)))))

(define-generic initialize-specialization (url class init-args)
  (declare (values url))
  (:documentation "Initializes a specialization of CLASS according to init-args."))

(defmethod initialize-specialization ((url http-object) class init-args)
  (with-class-change-for-initialize-specialization
    (url class init-args)
    url))

(defmethod initialize-specialization ((url http-path) class init-args)
  (with-class-change-for-initialize-specialization
    (url class init-args)
    url))

(defmethod initialize-specialization ((url http-form) class init-args)
  (with-class-change-for-initialize-specialization (url class init-args)
    (destructuring-bind (server response-function &optional header-function) init-args
      (setf (form-server url) server
            (response-function url) response-function
            (header-function url) header-function)
      url)))

(defmethod initialize-specialization ((url http-computed-url) class init-args)
  (with-class-change-for-initialize-specialization (url class init-args)
    (destructuring-bind (response-function &optional header-function) init-args
      (setf (response-function url) response-function
            (header-function url) header-function)
      url)))

(defmethod initialize-specialization ((url http-computed-form) class init-args)
  (with-class-change-for-initialize-specialization (url class init-args)
    (destructuring-bind (form-function response-function &optional header-function) init-args
      (setf (form-function url) form-function
            (response-function url) response-function
            (header-function url) header-function)
      url)))

(defmethod initialize-specialization ((url http-dynamic-form) class init-args)
  (with-class-change-for-initialize-specialization (url class init-args)
    (destructuring-bind (server form) init-args
      (setf (form-server url) server
            (dynamic-form url) form)
      url)))

(defmethod initialize-specialization ((url http-dynamic-form-instructions) class init-args)
  (with-class-change-for-initialize-specialization (url class init-args)
    (destructuring-bind (form) init-args
      (setf (dynamic-form url) form)
      url)))

(defmethod initialize-specialization ((url http-client-script) class init-args)
  (with-class-change-for-initialize-specialization (url class init-args)
    (destructuring-bind (script &optional header-function) init-args
      (setf (script url) script
            (header-function url) header-function)
      url)))

(defmethod initialize-specialization ((url http-template-object) class init-args)
  (with-class-change-for-initialize-specialization (url class init-args)
    url))

;; Used by COMLINK form processing
(define-generic revert-to-http-object (http-specialized-object)
  (:documentation "Reverts HTTP-SPECIALIZED-OBJECT back to an ordinary http-object."))

(defmethod revert-to-http-object ((url http-form))
  (change-class url 'http-object))

(define-generic revert-class (mutating-instance)
  (declare (values instance reverted-p old-class))
  (:documentation "Reverts the class of MUTATING-CLASS-MIXIN to the original class at instance creation time."))

(defmethod revert-class ((instance mutating-class-mixin))
  (let ((class (type-of instance))
        (origin-class (origin-class instance)))
    (cond ((eql class origin-class)
           (values instance nil class))
          (t (change-class instance origin-class)
             (values instance t class)))))

(define-generic note-origin-class (mutating-instance)
  (declare (values origin-class))
  (:documentation "Records the current class of mutating-instance as the origin class."))

(defmethod note-origin-class ((instance mutating-class-mixin))
  (setf (origin-class instance) (type-of instance)))

(defmethod initialize-instance :after ((instance mutating-class-mixin) &rest args)
  (declare (ignore args))
  (unless (slot-boundp instance 'origin-class)
    (note-origin-class instance))
  instance)


;;;------------------------------------------------------------------- 
;;;
;;; TYPE OPERATIONS ON URLS
;;;

(define-generic raw-data-type (doc))

(defmethod raw-data-type ((doc object-mixin))
  (with-slots (extension) doc
    extension))

(defmethod raw-data-type ((doc wais-type-mixin))
  (with-slots (type) doc
    type))

(defmethod raw-data-type ((doc gopher-type-mixin))
  (with-slots (type) doc
    type))

(define-generic data-type (url &optional error-if-unknown-p)
  (:documentation "Returns the data type of material referenced by URL.
This is the keyword for the primary EXTENSION defined by DEFINE-EXPORT-TYPE."))

(defmethod data-type ((url url) &optional error-if-unknown-p)
  (declare (ignore error-if-unknown-p))
  (error 'data-type-error :url url
         :format-string "The data-type operation is undefined on ~A URLS."
         :format-args (list (type-of url))))

(defmethod data-type :around ((url url) &optional error-if-unknown-p)
  (cond ((call-next-method))
        (error-if-unknown-p
         (error 'data-type-error :url url
                :format-string "Can't infer the canonical data-type for the URL, ~S."
                :format-args (list url)))
        (t :unknown)))

(declaim (inline %data-type-for-pathname-extension))

(defun %data-type-for-pathname-extension (keyword &optional (error-p t))
  (let ((primary-ext (http:primary-pathname-extension keyword error-p)))
    (when primary-ext
      (case primary-ext
        ((:text :html :lisp) primary-ext)
        (:rtf :application)
        (t (http:%mime-content-type-major-type  primary-ext)))))) 

(defmethod data-type ((pathname pathname) &optional error-if-unknown-p)
  (let ((type (pathname-type pathname)))
    (%data-type-for-pathname-extension (and (http:symbolize type http:*keyword-package*)) error-if-unknown-p)))

(defmethod data-type ((object object-mixin) &optional error-if-unknown-p)
  (let ((type (raw-data-type object)))
    (%data-type-for-pathname-extension (and (http:symbolize type http:*keyword-package*)) error-if-unknown-p)))

(defmethod data-type ((article news-article) &optional error-if-unknown-p)
  (declare (ignore error-if-unknown-p))
  :text)

(defmethod data-type ((search http-search) &optional error-if-unknown-p)
  (declare (ignore error-if-unknown-p))
  :html)

(define-generic mime-content-type-spec (url-pathname-or-data-type-keyword)
  (:documentation "Returns the MIME content type for a URL.
The values are MAJOR MINOR followed by alternating keyword parameters."))

(defmethod mime-content-type-spec ((keyword symbol))
  (http:%mime-content-type-spec keyword))

(defmethod mime-content-type-spec ((url translation-method-mixin))
  (let ((translation-method (translation-method url))
        extension pathname)
    (cond (translation-method
           (cond ((setq extension (http:primary-pathname-extension-for-export-type translation-method nil))
                  (http:%mime-content-type-spec extension))
                 (t (case translation-method
                      ((:html-form :computed-form :redirect :html-computed-form)
                       '(:text :html))
                      ;; Normal case is HTML, but someone could use different writers which makes this heuristic.   8/19/98 -- JCMa.
                      ((:directory :text-directory-hierarchy :image-directory :lisp-directory-hierarchy :directory-hierarchy :lisp-directory)
                       '(:text :html))
                      (:text-file '(:text :plain))
                      ;; Try extension case
                      ((:computed) nil)
                      (:search
                        (if (extension url)
                            nil
                            '(:text :html)))
                      (t (error "Can't determine the content-type automatically for ~S." translation-method))))))
          ((and (setq extension (extension url))
                (setq extension (http:primary-pathname-extension (http:symbolize extension http:*keyword-package*))))
           (http:%mime-content-type-spec extension))
          ((setq pathname (cached-pathname url))
           (mime-content-type-spec pathname))
          (t (error "Can't determine the content-type automatically for ~S." url)))))

(defmethod mime-content-type-spec ((pathname pathname))
  (mime-content-type-spec  (pathname-primary-extension pathname)))

(define-generic mime-content-type-major-type (url-of-data-type-keyword)
  (:documentation "Returns the data type of material referenced by URL.
This is the keyword for the primary EXTENSION defined by DEFINE-EXPORT-TYPE."))

(defmethod mime-content-type-major-type ((keyword symbol))
  (http:%mime-content-type-major-type keyword))

(defmethod mime-content-type-major-type ((url url))
  (first (mime-content-type-spec url)))

(define-generic character-set (url)
  (declare (values character-set-keyword))
  (:documentation "Returns the Character set keyword for URL."))

(defmethod character-set ((url http-url))
  (with-slots (plist) url
    (getf plist :charset)))

(defmethod (setf character-set) (charset (url http-url))
  (cond (charset
         (check-character-set-for-mime-content-type-text charset)
         (setf (get-value url :charset) charset))
        (t (remove-value url :charset))))

(define-generic languages (url)
  (declare (values language-keywords))
  (:documentation "Returns a sequence of language keywords for URL."))

(defmethod (setf languages) (keywords (url content-language-mixin))
  (with-slots (languages) url
    (typecase keywords
      (list (setq languages keywords))
      (keyword 
        (setq languages (list keywords))))))

(define-generic copy-mode (url)
  (:documentation "Returns a keyword indicating the mode in which to copy data accessed by URL.
Current modes are :TEXT, :CRLF, and :BINARY."))

(defmethod copy-mode ((url url)) :text)

;; extension should be stored in an IV extension-keyword for faster evaluation if it turns out
;; we call this predicate much. Right now, it is only used by the PUT method. -- JCMa 5/19/1995.
(defmethod copy-mode ((url object-mixin))
  (with-slots (extension) url
    (if extension
        (%content-type-copy-mode (http:symbolize extension http:*keyword-package*) t)
        :text)))

(define-generic export-type (url)
  (:documentation "Returns the appropriate HTTP export type for use with URL
by inference from the pathname extension."))

(defmethod export-type ((url url))
  (error "No HTTP export type has been defined for object of class, ~S." (type-of url)))

(defmethod export-type ((url object-mixin))
  (with-slots (extension) url
    (http:export-type-for-pathname-type 
      (if extension
          (http:symbolize extension http:*keyword-package*)
          :text-file))))

;;;------------------------------------------------------------------- 
;;;
;;; UTILS
;;;

(define make-ftp-url-string (host-string dir &optional name extension)
  "Returns an FTP URL string."
  (let ((args `(,.(loop for item in dir
			collect "/"
			collect item)
		"/"
		,.(when name `(,name))
		,.(when extension 
		    `("." ,extension)))))
    (apply #'concatenate 'string "ftp://" host-string args)))

;; Ports may specialize this to handle vagaries of their pathname implementation
(define-generic pathname-ftp-url-string (pathname &optional force-directory-p)
  (:documentation "Returns an FTP URL string for PATHNAME."))

(defmethod pathname-ftp-url-string (pathname &optional force-directory-p &aux (path (pathname pathname)))
  (let ((host (pathname-host path))
        (dir (pathname-directory path))
        (name (pathname-external-name-string (pathname-name path)))
        (extension (pathname-type path)))
    (cond (force-directory-p
	   (if name
	       (make-ftp-url-string (host-http-name host) `(,@(cdr dir) ,name))
	       (make-ftp-url-string (host-http-name host) (cdr dir))))
	  (t (make-ftp-url-string (host-http-name host) (cdr dir) name extension)))))

(define-generic ftp-url-pathname (url)
  (:documentation "Returns a pathname for the FTP URL."))

(defmethod ftp-url-pathname ((url ftp-directory))
  (with-slots (host-string path object extension version) url
    (make-pathname :host host-string
                   :directory (cons :absolute PATH))))

(defmethod ftp-url-pathname ((url ftp-pathname))
  (with-slots (HOST-STRING PATH object extension version) url
    (make-pathname :host HOST-STRING
                   :directory (cons :absolute PATH)
                   :name object
                   :type #+Genera(or extension :unspecific) #-Genera extension)))

(define-generic user-id-and-password (url)
  (declare (values user-id password))
  (:documentation "Returns the user-id and password associated with URL."))

(defmethod user-id-and-password ((url user-id-and-pw-mixin))
  (with-slots (user-id password) url
    (values user-id password)))


;;;------------------------------------------------------------------- 
;;;
;;; ACCESS CONTROL
;;;

(define-generic initialize-secure-subnets (url subnets)
  (:documentation "Initializes SUBNETS as secure for access to URL."))

(defmethod initialize-secure-subnets ((url secure-subnets-mixin) subnets)
  (with-slots (secure-subnets) url
    (etypecase subnets
      (null (setq secure-subnets nil))
      (cons (setq secure-subnets (www-utils:parse-internet-addresses subnets))))))

(define-generic trusted-host-p (url addresss)
  (:documentation "Returns non-null if ADDRESSS is trusted for access to URL."))

(defmethod trusted-host-p ((url secure-subnets-mixin) addresss)
  (with-slots (secure-subnets) url
    (www-utils:ip-host-trusted-p addresss secure-subnets)))

(define-generic initialize-authentication (url realm capabilities)
  (:documentation "Initializes realm and capabilities for access authentication."))


;;;------------------------------------------------------------------- 
;;;
;;; EXPIRATION TIMES
;;;

(define-generic expiration-universal-time (expiration-mixin)
  (declare (values universal-time-or-null))
  (:documentation
    "Returns the universal time when the data denoted by EXPIRATION-MIXIN becomes invalid.
This is the standard way to compute the value of the expires argument for HTTP:WITH-SUCCESSFUL-RESPONSE
or HTTP:WITH-CONDITIONAL-GET-RESPONSE"))

(defmethod expiration-universal-time ((expiration-mixin expiration-mixin) &aux fctn)
  (with-slots (expiration-function) expiration-mixin
    (cond ((setq fctn expiration-function)
           (funcall fctn expiration-mixin))
          (t nil))))

(defmethod expiration-universal-time ((url search-mixin) &aux fctn)
  (with-slots (expiration-function) url
    (cond ((setq fctn expiration-function)
           (funcall fctn url))
          (t (let ((parent (search-parent url :if-does-not-exist :error)))
               (when (and parent (not (eq parent url)))
                 (expiration-universal-time parent)))))))

(define-generic max-age-seconds (expiration-mixin)
  (declare (values universal-time-or-null))
  (:documentation "Returns the number of seconds until the data denoted by EXPIRATION-MIXIN becomes invalid."))

(defmethod max-age-seconds ((expiration-mixin expiration-mixin) &aux fctn)
  (with-slots (max-age-function) expiration-mixin
    (cond ((setq fctn max-age-function)
           (funcall fctn expiration-mixin))
          (t nil))))

(defmethod max-age-seconds ((url search-mixin) &aux fctn)
  (with-slots (max-age-function) url
    (cond ((setq fctn max-age-function)
           (funcall fctn url))
          (t (let ((parent (search-parent url :if-does-not-exist :error)))
               (when (and parent (not (eq parent url)))
                 (max-age-seconds parent)))))))

(define-generic initialize-expiration (expiration-mixin args)
  (:documentation "Top-level method for initializing the expiration of a URL.

Arguments are: (expiration-type &rest arguments)

EXPIRATION-type can be any of:

:NO-EXPIRATION-HEADER -- (no argument) no EXPIRES headers are issued.
:NEVER                -- (no argument) EXPIRES headers indicate one year from now.
:TIME                 -- (universal time) EXPIRES header indicates  the universal time.
:INTERVAL             -- (interval in universal time) EXPIRES headers indicate an now + interval.
:FUNCTION             -- (function) EXPIRES headers indicate universal time computed by
applying function to URL.  Function should return a universal
for use in the EXPIRES header or NIL, in which case no EXPIRES
header is issued."))

(defmethod initialize-expiration ((expiration-mixin expiration-mixin) args)
  (etypecase args
    (null
      (set-expiration-function expiration-mixin :no-expiration-header)
      (set-max-age-function expiration-mixin :no-expiration-header))
    (cons
      (apply #'set-expiration-function expiration-mixin args)
      (apply #'set-max-age-function expiration-mixin args))
    (keyword
      (set-expiration-function expiration-mixin args)
      (set-max-age-function expiration-mixin args))))

(define-generic set-expiration-function (expiration-mixin type &rest arguments))

(defmethod set-expiration-function ((expiration-mixin expiration-mixin) type &rest arguments)
  (declare (ignore arguments))
  (error "There is no expiration type defined for ~S." type))

(defmethod set-expiration-function ((expiration-mixin expiration-mixin) (type (eql :no-expiration-header)) &rest arguments)
  (declare (ignore arguments))
  (with-slots (expiration-function) expiration-mixin
    (setf expiration-function nil)))

(define-constant *one-year-interval* #.(* 365 24 60 60))

(declaim (fixnum *one-year-interval*))

(defmethod set-expiration-function ((expiration-mixin expiration-mixin) (type (eql :never)) &rest arguments)
  (declare (ignore arguments))
  (with-slots (expiration-function) expiration-mixin
    (setf expiration-function #'(lambda (url)
                                  (declare (ignore url))
                                  (the bignum (+ *one-year-interval*  (get-universal-time)))))))

(defmethod set-expiration-function ((expiration-mixin expiration-mixin) (type (eql :time)) &rest arguments)
  (with-slots (expiration-function) expiration-mixin
    (destructuring-bind (argument) arguments
      (check-type argument integer)
      (setf expiration-function #'(lambda (url)
                                    (declare (ignore url))
                                    argument)))))

(defmethod set-expiration-function ((expiration-mixin expiration-mixin) (type (eql :interval)) &rest arguments)
  (with-slots (expiration-function) expiration-mixin
    (destructuring-bind (argument) arguments
      (check-type argument integer)
      (setf expiration-function #'(lambda (url)
                                    (declare (ignore url))
                                    (the bignum (+ (get-universal-time) argument)))))))

(defmethod set-expiration-function ((expiration-mixin expiration-mixin) (type (eql :function)) &rest arguments )
  (with-slots (expiration-function) expiration-mixin
    (destructuring-bind (function &rest arguments) arguments
      (let ((fctn (etypecase function
                    (symbol (fdefinition function))
                    (function function))))
        (cond (arguments
               (let ((args arguments))
                 (flet ((call-function+args (url)
                          (declare (ignore url))
                          (apply fctn args)))
                   (declare (dynamic-extent #'call-function+args))
                   (setq expiration-function #'call-function+args))))
              (t (setq expiration-function fctn)))))))

(define-generic set-max-age-function (expiration-mixin type &rest arguments))

(defmethod set-max-age-function ((expiration-mixin expiration-mixin) type &rest arguments)
  (declare (ignore arguments))
  (error "There is no max age type defined for ~S." type))

(defmethod set-max-age-function ((expiration-mixin expiration-mixin) (type (eql :no-expiration-header)) &rest arguments)
  (declare (ignore arguments))
  (with-slots (max-age-function) expiration-mixin
    (setf max-age-function nil)))

(defmethod set-max-age-function ((expiration-mixin expiration-mixin) (type (eql :never)) &rest arguments)
  (declare (ignore arguments))
  (with-slots (max-age-function) expiration-mixin
    (setf max-age-function #'(lambda (url) (declare (ignore url)) *one-year-interval*))))

(defmethod set-max-age-function ((expiration-mixin expiration-mixin) (type (eql :time)) &rest arguments)
  (with-slots (max-age-function) expiration-mixin
    (destructuring-bind (argument) arguments
      (check-type argument integer)
      (setf max-age-function #'(lambda (url)
                                 (declare (ignore url))
                                 (- (the bignum (get-universal-time))
                                    (the integer argument)))))))

(defmethod set-max-age-function ((expiration-mixin expiration-mixin) (type (eql :interval)) &rest arguments)
  (with-slots (max-age-function) expiration-mixin
    (destructuring-bind (argument) arguments
      (check-type argument integer)
      (setf max-age-function #'(lambda (url) (declare (ignore url)) argument)))))

(defmethod set-max-age-function ((expiration-mixin expiration-mixin) (type (eql :function)) &rest arguments)
  (declare (ignore arguments))
  (with-slots (max-age-function) expiration-mixin
    (setf max-age-function #'(lambda (url)
                               (- (the bignum (get-universal-time))
                                  (the integer (expiration-universal-time url)))))))


;;;------------------------------------------------------------------- 
;;;
;;; CACHE CONTROL
;;;

(define-generic initialize-response-cache-control-directives (http-cache-control-mixin export-args)
  (:documentation "Top-level method for initializing the HTTP Cache Control of a URL.
This method allows cache control directives to be associated with a url and served
at run time. See HTTP:EXPORT-URL for the available directives."))

(defmethod initialize-response-cache-control-directives ((url http-cache-control-mixin) args)
  (with-slots (directives) url
    (destructuring-bind (&key public private no-cache no-store must-revalidate proxy-revalidate
			      no-transform max-age &allow-other-keys) args
      (setq directives nil)
      (remove-value url :proxy-revalidate-directives)	; remove any cached proxy directives see RESPONSE-CACHE-CONTROL-DIRECTIVES
      (cond-every
	((and public (not private))
	 (setf (getf directives :public) t))
	(private
	  (setf (getf directives :private) (if (and (consp no-cache) (every #'keywordp no-cache)) no-cache t)))
	(no-cache
	  (setf (getf directives :no-cache) (if (and (consp no-cache) (every #'keywordp no-cache)) no-cache t)))
	((and no-store (not (eql no-cache t)))	;subsumed by no-cache
	 (setf (getf directives :no-store) t))
	(must-revalidate
	  (setf (getf directives :must-revalidate) t))
	((and proxy-revalidate (not must-revalidate))	;subsumed by must-revalidate
	 (setf (getf directives :proxy-revalidate) t))
	(no-transform
	  (setf (getf directives :no-transform) t))
	(max-age
	  (check-type max-age integer)
	  (setf (getf directives :max-age) max-age)))
      url)))

(define-generic response-cache-control-directives (http-cache-control-mixin)
  (declare (values cache-control-directive-plist))
  (:documentation "Returns the cache control directives for use with HTTP:WITH-SUCCESSFUL-RESPONSE
or HTTP:WITH-CONDITIONAL-GET-RESPONSE. Returns directives to prevent proxy caching of
access controlled resources."))

(defmethod response-cache-control-directives ((url http-cache-control-mixin))
  (flet ((proxy-revalidation-required-p (directives)
	   (loop for (keyword value) on directives by #'cddr
		 when (and (member keyword '(:proxy-revalidate :must-revalidate))
			   value)
		   return nil			; already specified don't need it again.
		 finally (return (or *secure-subnets*
				     (url:secure-subnets url)
				     (http:server-user-object *server*))))))
    (declare (inline proxy-revalidation-required-p))
    (let ((directives (cache-control-directives url))
	  (max-age (max-age-seconds url)))
      (cond-every 
	;; Extend directives first time when computed from expiration function
	;; must precede revalidation check.
	((and max-age (not (getf directives :max-age)))
	 (setf (cache-control-directives url) `(:max-age ,max-age ,. directives)))
	;; Require revalidation when security present
	((proxy-revalidation-required-p directives)
	 (setq directives (with-value-cached (url :proxy-revalidate-directives)
			    `(:proxy-revalidate t ,.directives))))
	;; update max when a new time is computed
	(max-age
	  (setf (getf directives :max-age) max-age)))
      directives)))

(defmethod response-cache-control-directives :around ((url search-mixin))
  (let ((parent (search-parent url :if-does-not-exist :error)))
    (if (and parent (not (eq parent url)))
        (response-cache-control-directives parent)
        (call-next-method url))))

;; typo wedged into lispm worlds   8/6/96 -- JCMa.
(setf (symbol-function 'repsonse-cache-control-directives) #'response-cache-control-directives)
(export 'repsonse-cache-control-directives :url)


;;;------------------------------------------------------------------- 
;;;
;;; VERSIONS AND ENTITY TAGS
;;;

(define-generic document-version (url-or-pathname)
  (:documentation "Returns a version token for URL-OR-PATHNAME."))

;; Unless defined, versions default to NIL.
(defmethod document-version (thing) 
  (declare (ignore thing))
  nil) 

(defmethod document-version ((url caching-object-mixin))
  (with-slots (pathname) url
    (www-utils:file-version pathname)))

(defmethod document-version ((pathname pathname))
  (www-utils:file-version pathname))

(define-generic entity-tag (url)
  (declare (values entity-tag))
  (:method-combination or :most-specific-first)
  (:documentation "Returns an entity tag for URL."))

;; default to the current time.
(defmethod entity-tag or ((url http-url))
  (allocate-strong-entity-tag (http:server-request-time http:*server*)))

;; use the last modification date as the file version/entity tag
(defmethod entity-tag or ((url caching-object-mixin))
  (with-slots (pathname) url
    (when pathname
      (let ((version (www-utils:file-version pathname)))
        (when version
          (allocate-strong-entity-tag version))))))

;; no static entity so default to the current time.
(defmethod entity-tag or ((url http-computed-form))
  (allocate-strong-entity-tag (http:server-request-time http:*server*)))

;; no static entity so default to the current time.
(defmethod entity-tag or ((url http-computed-url))
  (allocate-strong-entity-tag (http:server-request-time http:*server*)))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(define-generic image-size (url &optional recompute-p)
  (declare (values width height))
  (:documentation "Returns the WIDTH and HEIGHT of the image denoted by URL.
or null values when size is unavailable"))

(defmethod image-size ((url http-object) &optional recompute-p)
  (with-slots (translation-method) url
    (when translation-method 
      (get-image-size url translation-method recompute-p))))

(defmethod image-size ((url string) &optional recompute-p)
  (let* ((url-string (http:merge-url url))
	 (url-obj (intern-url url-string :if-does-not-exist :soft)))
    (when url-obj
      (image-size url-obj recompute-p))))
 
(define-generic get-image-size (url translation-method &optional recompute-p)
  (declare (values width height))
  (:documentation "Internal methods for obtaining the size of an image.
Specialize this method according to the translation method to automatically insert size
information when generating image tags in versions of HTML supporting the feature.
An after method needs to be added to HTTP:EXPORT-URL for each image format handled.
See the one for :GIF-IMAGE as a model."))

;; Ignore unhandled image formats
(defmethod get-image-size ((url http-object) translation-method &optional recompute-p)
  (declare (ignore translation-method recompute-p))
  nil)

(defmethod get-image-size ((url http-object) (translation-method (eql :gif-image)) &optional recompute-p)
  (declare (ignore recompute-p))
  (with-slots (pathname) url
    (gif-image-dimensions pathname t)))

(defmethod get-image-size ((url http-object) (translation-method (eql :jpeg-image)) &optional recompute-p)
  (declare (ignore recompute-p))
  (with-slots (pathname) url
    (jpeg-image-dimensions pathname t)))

(defmethod get-image-size :around ((url http-object) translation-method &optional recompute-p)
  (flet ((report-an-error (url error)
	   (let ((error-type (type-of error)))
	     (report-bug http:*bug-http-server* (format nil "Image Size Error: ~S" error-type)
			 "~&URL: ~S~&Pathname: ~A~&Format: ~A~&Function: ~S~&Error: ~S~:[~;~&Error Report: ~:*~A~]"
			 (name-string url) (cached-pathname url) translation-method 'get-image-size
			 error-type (report-string error)))))
    (values-list
      (with-value-cached (url :image-size :recompute-p recompute-p)
	(multiple-value-bind (width height match-image-type-p)
	    (handler-case
	      (call-next-method url translation-method recompute-p)
	      (error (err) (report-an-error url err) nil))
	  (when (and width height)
	    (list width height match-image-type-p)))))))

(defmethod file-length-in-bytes (url &optional recompute-p)
  (declare (ignore url recompute-p))
  nil)

(defmethod file-length-in-bytes ((url pathname-caching-mixin) &optional recompute-p)
  (with-slots (pathname) url
    (with-value-cached (url :file-size :recompute-p recompute-p)
      (when (and pathname (not (pathname-directory-p pathname)))
        (file-length-in-bytes pathname recompute-p)))))

(defmethod file-modification-date ((url pathname-caching-mixin))
  (let ((pathname (cached-pathname url)))
    (when pathname
      (file-modification-date pathname))))


;;;------------------------------------------------------------------- 
;;;
;;; METERING
;;;

(define-generic clear-response-times (url)
  (:documentation "Clears metering values for URL."))

(defmethod clear-response-times ((url http-url))
  (remove-value url :cpu-time)
  (remove-value url :elapsed-time)
  (remove-value url :n-requests))

(define map-metered-urls (function &optional ports)
  "Maps function over all metered URLs on PORTS.
When ports is null, it clears all metering caches for all URLS.
FUNCTION is called with the URL object."
  (declare (dynamic-extent function))
  (flet ((fctn (string object)
           (declare (ignore string))
           (when (and (or (null ports) (member (port object) ports))
                      (get-value object :cpu-time))
             (funcall function object))))
    (declare (dynamic-extent #'fctn))
    (map-url-table #'fctn)))

(define clear-url-metering-caches (&optional ports)
  "Clears metering caches for URLs on PORTS.
When ports is null, it clears all metering caches for all URLS."
  (map-metered-urls #'clear-response-times ports))

(define-generic average-response-times (url)
  (declare (values cpu-time elapsed-time n-requests))
  (:documentation "Returns the average CPU and elapsed time spent serving URL.  CPU-TIME
is average microseconds of processor time. ELAPSED-TIME is the average
elapsed time in units of INTERNAL-TIME-UNITS-PER-SECOND.  The third
value, N-REQUESTS, is the number of requests."))

(defmethod average-response-times ((url http-url))
  (destructuring-bind (&key (cpu-time 0) (elapsed-time 0) (n-requests -1) &allow-other-keys)
      (property-list url)
    (values (truncate cpu-time n-requests)
            (truncate elapsed-time n-requests)
            n-requests)))

(define show-metered-urls (&key ports (stream *standard-output*) &aux urls)
  "Displays metered URLs on PORTS on STREAM."
  (flet ((collect (url)
           (multiple-value-bind (cpu-time elapsed-time n-requests)
               (average-response-times url)
             (when cpu-time
               (push `(,url ,cpu-time ,elapsed-time . ,n-requests) urls)))))
    (declare (dynamic-extent #'collect))
    (map-metered-urls #'collect ports))
  (loop for (url cpu-time elapsed-time . n-requests) in (sort urls #'> :key #'second)
        do (format stream "~&~A~&~5T~D CPU Msecs/request~&~5T~D Msecs/request~&~5T~D Requests"
                   (name-string url) (truncate cpu-time 1000.)
                   (truncate (* elapsed-time 1000.) internal-time-units-per-second)
                   n-requests)))


;;;------------------------------------------------------------------- 
;;;
;;; TEMPLATE OPERATIONS
;;;

(define-generic parse-template (template-mixin content-type &optional pathname)
  (declare (values template-parameters))
  (:documentation "Parses a template for content-type into an executable form.
Specialize this method on content-type for each kind of template.
A parser MUST return a list of (START END INSERTION-FUNCTION). START and END are
byte offsets in the template. INSERTION-FUNCTION is a function called on (URL STREAM).
The runtime server sends the bytes from START to end followed by an application 
of INSERTION-FUNCTION. When START is EQL to END, no bytes are sent. When 
INSERTION-FUNCTION is null, no function application occurs."))

(defmethod parse-template (url content-type &optional pathname)
  (declare (ignore pathname))
  (error "No template parser method is defined for (url:parse-template ,S ,S)." url content-type))

(defmethod parse-template (url (content-type (eql :shtml)) &optional pathname)
  (declare (ignore url))
  (http:parse-shtml-template (and pathname (pathname pathname))))

(define-generic template-update-lock (template-mixin)
  (:documentation "Returns the template update lock."))

(defmethod template-update-lock ((url template-mixin))
  (with-slots (template-lock) url
    (or template-lock
        (setq template-lock (make-lock (name-string url) :type :multiple-reader-single-writer)))))

(defmethod parse-template :around ((url template-mixin) content-type &optional pathname)
  (let ((last-update (template-update-time url)))
    (with-lock-held ((template-update-lock url) :write "Parse URL Template")
      (cond ((equal last-update (template-update-time url))     ;handles multithreading
             (let ((new-parameters (call-next-method url content-type pathname))
                   (update-time (get-universal-time)))
               (clear-template-cache url)
               (setf (template-parameters url) new-parameters
                     (template-update-time url) update-time)
               new-parameters))
            (t (template-parameters url))))))

(define-generic clear-template-cache (template-mixin)
  (:documentation "Clears cached parameters and update times on TEMPLATE-MIXIN."))

(defmethod clear-template-cache (url)
  (declare (ignore url)))

(defmethod clear-template-cache ((url template-mixin))
  (setf (template-parameters url) nil
        (template-update-time url) nil)
  (remove-value url :template-secure-subnets))  ;decache a security info

(define clear-all-template-caches ()
  "Clears all cached parameters and update times on URL templates."
  (map-url-table #'(lambda (name-string url)
                     (declare (ignore name-string))
                     (url:clear-template-cache url))))

(define-generic template-secure-subnets (template-mixin cache-key subnets-parameter &optional recache-p)
  (declare (values secure-subnets retrieved-from-cache-p))
  (:documentation "Returns SUBNETS-PARAMETER as a parsed ip number list.
SUBNETS-PARAMETER is a comma separated string of IP addresses.
CACHE-KEY for caching the value. RECACHE-P forces recomputation and recaching."))

(defmethod template-secure-subnets ((url template-mixin) cache-key subnets-parameter &optional recache-p)
  (declare (values secure-subnets retrieved-from-cache-p))
  (flet ((parse-subnets (subnets)
           (let ((ip-addresses (parse-comma-separated-header subnets)))
             (declare (dynamic-extent ip-addresses))
             (parse-internet-addresses ip-addresses))))
    (let* ((alist (get-value url :template-secure-subnets))
           (entry (assoc cache-key alist :test #'equal)))
      (cond (entry
             (destructuring-bind (&optional c-subnets . secure-subnets)
                 (cdr entry)
               (cond ((and (not recache-p) (equal subnets-parameter c-subnets))
                      (values secure-subnets t))
                     (t (setf secure-subnets (parse-subnets subnets-parameter)
                              (second entry) subnets-parameter
                              (cddr entry) secure-subnets)
                        secure-subnets))))
            (t (let ((secure-subnets (parse-subnets subnets-parameter)))
                 (push (list* cache-key subnets-parameter secure-subnets)
                       (get-value url :template-secure-subnets))
                 secure-subnets))))))
