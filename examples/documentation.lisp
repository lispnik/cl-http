;;;   -*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-

;;;
;;; (c) Copyright  1995, John C. Mallery
;;;     All Rights Reserved.
;;;


;;;------------------------------------------------------------------- 
;;;
;;; SERVER SELF DOCUMENTATION
;;;

(in-package :http)

;(eval-when (compile eval load)
;  (unless (fboundp 'image-line)
;
;;; Spruce up computed lines by using this instead of  horizontal-line 
;    (defparameter *image-line-url* "/cl-http/icons/line-rain.gif")
;
;    (defun image-line (&key (stream *output-stream*) (fresh-line t))
;      (when fresh-line
;        (fresh-line stream))
;      (with-paragraph (:stream stream)
;        (image *image-line-url* "----" :stream stream)))
;
;    (mapc #'(lambda (x) (export x :http)) '(image-line *image-line-url*))))

(defun submit-and-reset-buttons (&optional (stream *output-stream*))
  (multiple-value-bind (user-agent version)
      (current-user-agent)
    (cond ;; use tables when possible.
      ((and user-agent (user-agent-capability-p :tables user-agent version))
       (with-table (:cell-spacing 1 :cell-padding 5 :stream stream)
         (with-table-row (:stream stream)
           (with-table-cell (:horizontal-alignment :left :stream stream)
             (with-rendition (:bold :stream stream)
               (write-string "Action:" stream)))
           (with-table-cell (:horizontal-alignment :center :stream stream)
             (accept-input 'reset-button "Reset" :stream stream))
           (with-table-cell (:horizontal-alignment :center :stream stream))
           (with-table-cell (:horizontal-alignment :center :stream stream)
             (accept-input 'submit-button "Submit" :stream stream)))))
      ;; otherwise create a similar effect with preformatted text
      (t (with-paragraph (:stream stream)
           (with-verbatim-text (:fresh-line nil :stream stream)
             (with-rendition (:bold :stream stream)
               (write-string "Action:  " stream))
             (accept-input 'reset-button "Reset" :stream stream)
             (write-string "          " stream)
             (accept-input 'submit-button "Submit" :stream stream)))))))

(export 'submit-and-reset-buttons :http)

;;;------------------------------------------------------------------- 
;;;
;;;  FILL-OUT FORM EXAMPLE THAT FINDS DOCUMENTATION ABOUT CL-HTTP
;;;

(defun variable-p (sym)
  (boundp sym))

(defun function-p (sym)
  (and (fboundp sym)                            ; should subsume special-form-p
       (not (macro-function sym))))

(defun class-p (sym)
  (find-class sym nil)) 

(defparameter *lisp-type-predicate-alist*
              '((:all nil "All") (:class class-p "Class") (:function function-p "Function") (:macro macro-function "Macro") 
                (:variable variable-p "Variable"))) 

(defun get-lisp-type-predicate (lisp-type &optional (type :predicate))
  (let ((entry (assoc lisp-type *lisp-type-predicate-alist*)))
    (cond (entry
           (ecase type
             (:predicate 
               (and (second entry) (fdefinition (second entry))))
             (:documentation (third entry))))
          (t (error "~S is not one of the known lisp predicates, ~S." 
                    lisp-type (mapcar #'car *lisp-type-predicate-alist*)))))) 

;; Make this use a hashtable resource for the muliple package sometime.
(defun find-symbols (substring &key (module :http-user) sort-p (external-p t) 
                               (lisp-type :all) &aux symbols pred)
  (flet ((find-syms-in-package (pkg)
           (flet ((collect-matching (sym)
                    (when (and (search substring (symbol-name sym) :test #'char-equal)
                               (or (null pred) (funcall pred sym)))
                      (push sym symbols))))
             (declare (inline collect-matching))
             (let ((package (typecase pkg
                              (null nil)
                              (package pkg)
                              (t (or (find-package (string-upcase pkg))
                                     (return-from find-symbols nil))))))
               (cond ((and package external-p)
                      (do-external-symbols (sym package)
                        (collect-matching sym)))
                     (package
                      (do-symbols (sym package)
                        (collect-matching sym)))
                     (t (do-all-symbols (sym)
                          (collect-matching sym))))))))
    (declare (dynamic-extent #'find-syms-in-package))
    ;; set the predicate for constrained searches
    (setq pred (get-lisp-type-predicate lisp-type :predicate))
    (etypecase module
      (atom (find-syms-in-package module))
      (cons
        (mapc #'find-syms-in-package module)
        (setq symbols (delete-duplicates symbols :test #'eq))))
    (if sort-p 
        (sort symbols #'string< :key #'symbol-name)
        symbols))) 

(defun write-lisp-expression (thing &optional (stream *output-stream*))
  (let* ((*package* (find-package :common-lisp))
         (*print-readably* nil)
         (string (write-to-string thing :base 10. :circle t)))
    (declare (dynamic-extent string))
    (write-string-quoting-specials string stream)))

(defun describe-symbol (sym &key reference  documentation-p (stream *output-stream*) 
                            &aux describe-handled-p)
  (flet ((note-symbol (symbol reference)
           (let* ((*package* (find-package :common-lisp))) 
             (break-line :stream stream)
             (with-rendition (:bold :stream stream)
               (if reference
		   (let ((string (write-to-string symbol :case :upcase)))
                     (declare (dynamic-extent string))
		     (flet ((write-url-string (stream)
			      (write-string reference stream)
			      (write-string-escaping-special-chars string stream)))
		       (declare (dynamic-extent #'write-url-string))
                     (note-anchor string :reference #'write-url-string :stream stream)))
		   (write symbol :stream stream :readably t :case :upcase)))))
         (note-documentation (symbol doc-type)
           (let ((docs (documentation symbol doc-type)))
             (break-line :stream stream)
             (cond  ((and docs (position #\newline docs :test #'eql))
                     (with-verbatim-text (:fresh-line nil :stream stream)
                       (write-string-quoting-specials docs stream)))
                    (docs
                     (write-string-quoting-specials docs stream))
                    (t (write-string "[Undocumented]" stream))))))
    (declare (inline note-symbol note-documentation))
    (macrolet ((%describe-symbol ((symbol reference doc-type lisp-type-string) &body body)
                 `(progn
                    (setq describe-handled-p t)
                    (note-symbol ,symbol ,reference)
                    (write-string ,lisp-type-string stream)
                    ,@body
                    (when documentation-p
                      (note-documentation ,symbol ,doc-type)))))
      (cond-every 
        ((fboundp sym)
         (%describe-symbol
           (sym reference 'function
                (cond ((macro-function sym) " [macro]: ")
                      ((special-operator-p sym) " [special form]: ")
                      ((functionp (symbol-function sym)) " [function]: ")
                      (t " [???]: ")))
           (write (or (arglist sym) " ()") :stream stream :escape nil)))
        ((boundp sym)
         (%describe-symbol
           (sym reference 'variable
                " [variable] : ")
           (if (boundp sym)
               (write-lisp-expression (symbol-value sym) stream)
               (write-string "Unbound" stream))))
        ((find-class sym nil)
         (%describe-symbol (sym reference 'type " [class] : ")))
        ;; handle more classes of lisp objects like methods....
        ((not describe-handled-p) (%describe-symbol (sym reference 'function " [random] : ")))))))



(defun find-documentation (substring module external-p lisp-type documentation-p stream)
  (let* ((wild-p  (or (null substring) (null-string-p substring)))
         (lisp-type-doc (unless (eq lisp-type :all) (get-lisp-type-predicate lisp-type :documentation)))
         (title (format nil "~A~:[~; ~:*~A~]  Documentation~:[ matching ~S~;~]" 
                        module lisp-type-doc wild-p (or substring "")))
         (candidates (find-symbols substring :module module :sort-p t :external-p external-p
                                   :lisp-type lisp-type)))
    (with-html-document (:declare-dtd-version-p t :stream stream)
      (with-document-preamble (:stream stream)
        (declare-title title :stream stream))
      (with-standard-document-body (:stream stream)
        (with-section-heading (title :stream stream)
          (image-line :stream stream)
          (with-paragraph (:stream stream)
            (note-anchor "Find Other Documentation"
                              :reference "/cl-http/find-documentation.html"
                              :stream stream))
          (with-paragraph (:stream stream)
            (cond (candidates
                   (loop with reference = "/cl-http/show-documentation?"
                         for item in candidates
                         do (describe-symbol item
                                             :reference reference
                                             :documentation-p documentation-p
                                             :stream stream)))
                  (t (write-string "No candidates found." stream))))
          (image-line :stream stream)
          (cl-http-signature stream))))))

(defmethod respond-to-find-documentation ((url http-form) stream query-alist)
  (bind-query-values (substring module external lisp-type documentation)
                     (url query-alist)
    (with-successful-response (stream :html :expires (url:expiration-universal-time url)
                                      :cache-control (url:response-cache-control-directives url)
                                      :content-language (languages url)
                                      :additional-headers (ns4.0:client-target-window-http-headers))
      (find-documentation substring 
                          module 
                          (and  external (equalp external "YES"))
                          (intern lisp-type *keyword-package*)
                          (and documentation (equalp documentation "YES"))
                          stream))))

(defparameter *modules-for-find-documentation*
              #-Genera '("HTTP" "HTML2" "HTML3.2" "NETSCAPE1.1" "NETSCAPE2.0" "NETSCAPE3.0" "NETSCAPE4.0" "URL" "VRML1.0" "W3P" "WWW-UTILS"
                         "BASE64" "MD5" "SHA" #+:CCL-3"SMTP" "COMMON-LISP" #+MCL "CCL" #+CL-HTTP-MENU "CL-HTTP-MENU")
              #+Genera '("HTTP" "HTML2" "HTML3.2" "NETSCAPE1.1" "NETSCAPE2.0" "NETSCAPE3.0" "NETSCAPE4.0" "URL" "VRML1.0" "W3P" "WWW-UTILS"
                         "BASE64" "MD5" "SHA"))

(defun add-module-for-find-documentation (module)
  (let ((pkg (find-package module))
	name)
    (cond (pkg
	   (setq name (package-name pkg))
	   (unless (member name *modules-for-find-documentation* :test #'equalp)
	     (push-ordered name *modules-for-find-documentation* #'string-lessp)))
	  (t (error "There is no module (package) named, ~S." module)))))

(export 'add-module-for-find-documentation)

(defparameter *lisp-types-for-find-documentation* 
              (loop for item in *lisp-type-predicate-alist*
                    for string = (third item)
                    collect (list* string (string-upcase string)))) 

(defun %write-find-documentation-form (url stream modules default-modules title description)
  (macrolet ((with-query-environment ((label) &body body)
               `(with-paragraph (:stream stream)
                  (with-rendition (:bold :stream stream)
                    (fresh-line stream)
                    (write-string ,label stream)
                    ,@body))))
    (with-html-document (:declare-dtd-version-p t :stream stream)
      (with-document-preamble (:stream stream)
        (declare-base-reference url :stream stream)
        (declare-title title :stream stream))
      (with-standard-document-body (:stream stream)
        (with-section-heading (title :stream stream)
          (image-line :stream stream)
          (with-paragraph (:stream stream)
            (write-string description stream))
          (with-fillout-form (:post url :stream stream)
            (with-query-environment
              ("Find Documentation (Substring Match): ")
              (accept-input 'string "SUBSTRING" :size 30 :stream stream)) 
            (with-query-environment
              ("Module:")
              (fresh-line stream)
              (accept-input 'select-choices "MODULE" :choices modules :size (length modules)
                                 :default default-modules :sequence-p t :stream stream))
            (with-query-environment
              ("Lisp Type:")
              (accept-input 'radio-button "LISP-TYPE"
                                 :choices *lisp-types-for-find-documentation*
                                 :default "ALL" :linebreaks nil :stream stream))
            (with-query-environment
              ("Show Documentation: ")
              (accept-input 'radio-button "DOCUMENTATION" :choices '(("Yes" . "YES") ("No" . "NO"))
                                 :default "NO" :linebreaks nil :stream stream))
            (with-query-environment
              ("External Interface: ")
              (accept-input 'radio-button "EXTERNAL" :choices '(("Yes" . "YES") ("No" . "NO"))
                                 :default "YES" :linebreaks nil :stream stream))
            (submit-and-reset-buttons stream))
          (image-line :stream stream)
          (cl-http-signature stream))))))

;; Instead of using a static form, gain cross platform flexibility by using a computed form.
(defmethod compute-find-documentation-form ((url url:http-form) stream)
  (with-conditional-get-response (stream :html :expires (url:expiration-universal-time url)
                                         :cache-control (url:response-cache-control-directives url)
                                         :content-language (languages url))
    (%write-find-documentation-form url
                                    stream
                                    *modules-for-find-documentation*
                                    '("HTML")
                                    "Find CL-HTTP Documentation"
                                    "Search for functions and variables in the Common Lisp Hypermedia Server.")))

(export-url #u"/cl-http/find-documentation.html"
            :html-computed-form
            :form-function #'compute-find-documentation-form
            :response-function #'respond-to-find-documentation
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "A form interface for looking up documentation for CL-HTTP.")

(defun %respond-to-find-documentation-search (url stream default-module)
  (with-slots (url:search-keys) url
    (let* ((spec (and url:search-keys (string-trim '(#\space #\tab) (first url:search-keys))))
           (length (length spec))
           (pos (unless (zerop length) (position #\: spec :test #'eql)))
           (lisp-type :all)
           (documentation-p nil))
      (declare (dynamic-extent spec))
      (cond 
        (pos (let* ((external (< (count #\: spec :test #'eql :start pos) 2))
                    (module (if (zerop pos) default-module (string-upcase (subseq spec 0 pos))))
                    (pos2 (position-if-not  #'(lambda (x) (eql x #\:)) spec :start pos :end length))
                    (substring (if pos2 (subseq spec pos2 length) "")))
               (declare (dynamic-extent substring module))
               (find-documentation substring module external lisp-type documentation-p stream)))
        (t (find-documentation "" default-module t lisp-type documentation-p stream))))))

(defmethod respond-to-find-documentation-search ((url http-search) stream)
  (with-conditional-get-response (stream :html :expires (url:expiration-universal-time url)
                                         :cache-control (url:response-cache-control-directives url)
                                         :content-language (languages url)
                                         :additional-headers (ns4.0:client-target-window-http-headers))
    (%respond-to-find-documentation-search url stream "HTML")))

(export-url #u"/cl-http/find-documentation?"
            :search
            :response-function #'respond-to-find-documentation-search
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "A search URL interfaced into the same functions that respond to the find-documentation form.
To use, provide a symbol with a package prefix after the ?, e.g., [package]:[substring].
The search for [substring] will be performed in the package, [package]. 
A single colon will cause only external symbols to be returned, whereas a double colon
will retrieve both internal and external symbols.")

;;;------------------------------------------------------------------- 
;;;
;;;  SEARCH URL THAT DISPLAYS DOCUMENTATION 
;;;

(defun get-symbol (pkg-string &optional (start 0) (end (length pkg-string)))
  (let* ((pos1 (position #\: pkg-string :start start :end end))
         (pkg (if pos1 (subseq pkg-string 0 pos1) "HTTP-USER"))
         (pname (if pos1
		    (unless (= (1+ pos1) end)
		      (subseq pkg-string (position-if-not #'(lambda (x) (eql x #\:)) pkg-string
							  :start (the fixnum (1+ pos1)) :end end)))
		    pkg-string)))
    (when pname
      (intern pname (if (null-string-p pkg) *keyword-package* (or (find-package pkg) :http))))))

(defmethod respond-to-show-documentation  ((url url:http-search) stream)
  (with-slots (url:search-keys) url
    (with-conditional-get-response (stream :html :expires (url:expiration-universal-time url)
                                           :cache-control (url:response-cache-control-directives url)
                                           :content-language (languages url))
      (let* ((*print-case* :upcase)
	     (title (format nil "Documentation for ~{~A~^~}" url:search-keys)))
        (with-html-document (:declare-dtd-version-p t :stream stream)
          (with-document-preamble (:stream stream)
            (declare-base-reference url :stream stream)
            (declare-title title :stream stream))
          (with-standard-document-body (:stream stream)
            (with-section-heading (title :stream stream)
              (image-line :stream stream)
              (loop for item in url:search-keys
                    for sym = (get-symbol item)
		    when sym
		      do (describe-symbol sym :documentation-p t :stream stream))
              (image-line :stream stream)
              (cl-http-signature stream))))))))

(export-url #u"/cl-http/show-documentation?"
            :search
            :response-function #'respond-to-show-documentation
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "Shows the documentation for a Lisp symbol in CL-HTTP.
To use, provide a package-prefixed name of a symbol in uppercase (unless slashified versions are desired).")

;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defun find-url (&optional search-spec sorted-p
                           &aux urls wild-p constraints start2 end2 
                           (local-host (local-host)))
  "Returns URLs matching SUBSTRING, optionally sorting them when SORTED-P is non-null."
  (flet ((make-constraint (substring)
           (let ((start1 0)
                 (end1 (length substring)))
             #'(lambda (name)
                 (search substring name :start1 start1 :end1 end1 :start2 start2 :end2 end2 :test #'eql))))
         (collect (name url)
           (handler-case-if (not *debug-server*)
              (when (and (translation-method url)	;is it exported
			 (host-eq local-host (url:host-object url))	;by this host?
                         (or wild-p
                             (loop initially (setq end2 (length name))
                                             (unless start2
                                               (setq start2 (position #\/ name :start 7 :test #'eql :end end2)))
                                   for constraint in constraints
                                   unless (funcall constraint name)
                                     do (return nil)
                                   finally (return t))))
                (push url urls))
             (error () nil))))
    (declare (dynamic-extent #'make-constraint))
    ;; Prepare to match
    (unless (setq wild-p (or (null search-spec)
                             (and (stringp search-spec) (null-string-p search-spec)))) 
      (setq constraints (loop for str in (ensure-list search-spec)
                              collect (make-constraint str))))
    ;; Collect matching URLs
    (url:map-url-table #'collect)
    ;; Optionally sort them -- better to use the push-ordered macro from CML
    (cond (sorted-p
           (stable-sort urls #'(lambda (x y)
                                 (unless start2
                                   (setq start2 (position #\/ (url:name-string x) :start 7 :test #'eql :end end2)))
                                 (string< (url:name-string x)
                                          (url:name-string y)
                                          :start1 start2 :start2 start2))))
          (t urls))))

(defun find-url-and-respond (url substrings stream)
  (let* ((urls (find-url substrings t))
         (title (if (and substrings (not (null-string-p (first substrings))))
                    (format nil "~D URL~:*~P Matching ~{~A~^, ~}" (length urls) substrings)
                    (format nil "~D URL~:*~P Exported by Server" (length urls)))))
    (with-successful-response (stream :html :expires (url:expiration-universal-time url)
                                      :cache-control (url:response-cache-control-directives url)
                                      :content-language (languages url)) 
      (with-html-document (:declare-dtd-version-p t :stream stream)
        (with-document-preamble (:stream stream)
          (declare-base-reference url :stream stream)
          (declare-title title :stream stream))
        (with-standard-document-body (:stream stream)
          (with-section-heading (title :stream stream)
            (image-line :stream stream)
            (with-enumeration (stream :enumerate)
              (loop for url in urls
                    for url-string = (url:name-string url)
                    do (enumerating-item (stream)
                         (with-rendition (:bold :stream stream)
                           (let ((describe-url (concatenate 'string "/cl-http/describe-url?" url-string)))
                             (declare (dynamic-extent describe-url))
                             (note-anchor "[?] " :reference describe-url :stream stream)
                             (note-anchor url-string :reference url-string :stream stream))))))
            (image-line :stream stream)
            (cl-http-signature stream)))))))

(defmethod respond-to-find-url  ((url url:http-search) stream)
  (with-slots (url:search-keys) url
    (find-url-and-respond url url:search-keys stream)))

(export-url #u"/cl-http/find-url?"
            :search 
            :response-function #'respond-to-find-url
            :public t
            :keywords '(:cl-http :documentation)
            :documentation "Search for URLs containing a particular substring.")

(defmethod compute-find-url-form ((url url:http-form) stream)
  (with-conditional-get-response (stream :html :expires (url:expiration-universal-time url)
                                         :cache-control (url:response-cache-control-directives url)
                                         :content-language (languages url))
    (with-html-document (:declare-dtd-version-p t :stream stream)
      (with-document-preamble (:stream stream)
        (declare-base-reference url :stream stream)
        (declare-title "Find Exported URLs" :stream stream))
      (with-standard-document-body (:stream stream)
        (with-section-heading ("Find Exported URLs" :stream stream)
          (image-line :stream stream)
          (with-paragraph (:stream stream)
            (write-string "This form will list all exported URLS on the server that match a substring.
You may provide multiple substrings if you separate the with a space." stream))
          (with-fillout-form (:post url :stream stream)
            (with-paragraph (:stream stream)
              (with-rendition (:bold :stream stream)
                (fresh-line stream)
                (write-string "Match (Substring): " stream))
              (accept-input 'string "URL-SUBSTRINGS" :size 30 :stream stream))
            (submit-and-reset-buttons stream))
          (image-line :stream stream)
          (cl-http-signature stream))))))

(defmethod respond-to-find-url-form ((url url:http-form) stream query-alist)
  (labels ((delimiter-p (char)
             (member char '(#\space #\tab #\return #\Linefeed) :test #'eql))
           (breakup-pattern (string)
             (loop with length = (length string)
                   for start = (or (position-if-not #'delimiter-p string :start 0 :end length) 0)
                             then (or (position-if-not #'delimiter-p string :start end :end length) length)
                   for end = (or (position-if #'delimiter-p string :start start :end length) length)
                   while (< start end)
                   for item = (subseq string start end)
                   unless (null-string-p item)
                     collect item)))
    (bind-query-values (url-substrings)
                       (url query-alist)
      (find-url-and-respond url (breakup-pattern url-substrings) stream))))

(export-url #u"/cl-http/find-url.html"
            :html-computed-form
            :form-function #'compute-find-url-form
            :response-function #'respond-to-find-url-form
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "A form interface to search for URLs containing a particular substring.")


;;;------------------------------------------------------------------- 
;;;
;;; SPECIALIZE URL NOT FOUND CONDITION
;;;

(defmethod report-status-message :around ((condition document-not-found) stream &optional format-string format-args)
   (cond ;; Don't lose on the web walker. Kludgey. 4/20/97 -- JCMa.
     ((or (null *server*)
	  (and (find :w4 *features*) (boundp (intern "*URL-STACK*" :w4))))
       (call-next-method))
     (t (let* ((status-code (status-code condition))
	           (reason (or (http-reason condition)
			             (get-string-for-status-code status-code)))
	           (url (http-url condition)))
	   (unless format-string
	      (setq format-string (format-string condition)))
	   (unless format-args
	      (setq format-args (format-args condition)))
	   (with-html-document (:declare-dtd-version-p t :stream stream)
	       (with-document-preamble (:stream stream)
	           (declare-title reason :stream stream))
	       (with-standard-document-body (:stream stream)
	           (with-section-heading (reason :stream stream)
	               (horizontal-line :stream stream)
	               (with-paragraph (:stream stream)
		           (cond (format-string
			              (apply #'format stream format-string format-args))
		                    (url (format stream "~A for URL ~A"
				                        reason (typecase url
					                              (url (url:name-string url))
					                              (t url))))
		                    (t (format stream "~&Error ~D not properly reported.~&Please advise the server maintainer at ~A"
				                     (server-status *server*)
				                     (server-mail-address)))))
		       (typecase url		;don't lose on URNs   8/19/98 -- JCMa.
			 (url
			   (horizontal-line :stream stream)
			   (with-section-heading ("Find URL" :stream stream)
			     (with-fillout-form (:post #u"/cl-http/find-url.html" :stream stream)
		               (with-paragraph (:stream stream)
				 (with-rendition (:bold :stream stream)
				   (fresh-line stream)
				   (write-string "Match (Substring): " stream))
				 (accept-input 'string "URL-SUBSTRINGS" :size 72
					       :default (if url (url:relative-name-string url) "")
					       :stream stream))
		               (submit-and-reset-buttons stream)))))
	               (horizontal-line :stream stream)
	               (cl-http-signature stream))))))))


;;;------------------------------------------------------------------- 
;;;
;;; DESCRIBE URL
;;;

#+Genera
(defun fixed-describe (object &optional stream)
  (let ((*standard-output* stream))
    (describe object stream)))

(defgeneric describe-url (url translation &optional stream)
  (:documentation "Describes URL on STREAM using TRANSLATION."))

;; Inefficient. Define this by writing a describe for HTML rather than using this kludge.  This
;; requires portable access to certain operations in the CLOS meta-protocol.
(defmethod describe-url ((url url:url) (translation (eql :html)) &optional (stream *output-stream*))
  (let ((*package* (find-package :common-lisp))
        (description (with-output-to-string (string)
		       #+Genera(fixed-describe url string)
		       #-Genera(describe url string))))
    (declare (dynamic-extent description))
    (fresh-line stream)
    (with-verbatim-text (:stream stream)
      (fresh-line stream)
      (write-string-quoting-specials description stream))))

(defmethod respond-to-describe-url ((url url:http-search) stream)
  (with-slots (url:search-keys) url
    (with-successful-response (stream :html :expires (url:expiration-universal-time url)
                                      :cache-control (url:response-cache-control-directives url)
                                      :content-language (languages url)) 
      (with-html-document (:declare-dtd-version-p t :stream stream)
        (cond ((car url:search-keys)
               (let*  ((url-string (car url:search-keys))
                       (url2 (url:intern-url url-string ;; catch any search URLs that appear.
                                             :if-does-not-exist (if (url:valid-search-url-p url-string) 
                                                                    *search-url-intern-mode*
                                                                    :soft)))
                       (title (format nil "Describe: ~A" url-string)))
                 (with-document-preamble (:stream stream)
                   (declare-base-reference url :stream stream)
                   (declare-title title :stream stream))
                 (with-standard-document-body (:stream stream)
                   (with-section-heading (title :stream stream) 
                     (image-line :stream stream)
                     (cond (url2
                            (describe-url url2 :html stream))
                           (t (format stream "~&The URL, ~S, was not found on the server." url-string)))
                     (image-line :stream stream)
                     (cl-http-signature stream)))))
              (t (let ((title "No URL to Describe"))
                   (with-document-preamble (:stream stream)
                     (declare-title title :stream stream))
                   (with-standard-document-body (:stream stream)
                     (with-section-heading (title :stream stream) 
                       (image-line :stream stream)
                       (format stream "~&You need to supply a URL to describe.~
                                        The URL specification should follow the ?. Please try again.")
                       (image-line :stream stream)
                       (cl-http-signature stream))))))))))

(export-url #u"/cl-http/describe-url?"
            :search
            :response-function #'respond-to-describe-url
            :keywords '(:cl-http :documentation)
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :documentation "Describe for a specific URL on the server.")

(pushnew :cl-http-documentation-facility *features*)
