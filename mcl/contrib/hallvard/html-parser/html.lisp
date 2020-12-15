;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: html-base; Base: 10 -*-
;;;;
;;;
;;; Copyright 1996, Hallvard Tr¾tteberg
;;; You can use the code if you like, but include this notice.
;;;
;;;;
;;; Last edited by smishra on Mon Jul  1 14:33:55 1996

(in-package :html-base)

(defclass html-class ()
  (
   (version          :allocation :class :initform 'html-class :reader html-version)
   (code             :allocation :class                       :reader html-code)
   (options          :allocation :class :initform nil         :reader html-options)
   (content          :allocation :class :initform nil         :reader html-content)
   (required-options :allocation :class :initform nil         :reader html-required-options)
   (optional-end     :allocation :class :initform nil         :reader html-optional-end)
   (fresh-line-p     :allocation :class :initform nil         :reader html-fresh-line-p)
   (unknown-options                     :initform nil         :accessor html-unknown-options)
   (out-of-context-p :initarg :out-of-context-p               :accessor html-out-of-context-p)
   )
  )

;;; dummy classes

(deftype boolean () `(member t nil))
#-:CL-HTTP
(defclass url () ())

;;;

(defclass html-clause (html-class)
  ()
  )

(defclass html-comment ()
  (
   (comment-text :initform "" :initarg :comment-text :accessor html-comment-text)
   )
  )

(defclass html-environment (html-class)
  (
   (parts :initform nil :accessor html-environment-parts)
   )
  )

(defclass html-document (html-environment)
  (
   (content :allocation :class :initform '(t))
   )
  )

(defclass unknown-html (html-environment)
  (
   (content :allocation :class :initform '(t) :reader html-content)
   (code    :accessor html-code)
   (options :initform nil :accessor html-options)
   )
  )

;;;

(defvar *html-class-alist* nil)
(defvar *html-version* nil)

;;;

(defun html-prototype (class-name)
  #+:mcl     (ccl:class-prototype  (find-class class-name))
  #+(or allegro lispworks) (clos:class-prototype (find-class class-name))
  )

(defmethod html-version ((class-name symbol))
  (html-version (html-prototype class-name)))

(defmethod html-code ((class-name symbol))
  (html-code (html-prototype class-name)))

(defmethod html-options ((class-name symbol))
  (html-options (html-prototype class-name)))

(defmethod html-content ((class-name symbol))
  (html-content (html-prototype class-name)))

;;;

(defun html-option-def (code option)
  (let ((acons (assoc option (html-options code))))
    (cdr acons))) 

(defun html-option-type (code option)
  (getf (html-option-def code option) :type))

(defun html-option-code (code option)
  (or (getf (html-option-def code option) :code)
      (string option)))

;;;

(defun find-html-option (html option)
  (find-if #'(lambda (html-option)
               (or (string-equal option (getf (cdr html-option) :code (car html-option)))
                   (string-equal option (car html-option))))
           (html-options html)))

(defvar *intern-url-values* nil)

(defun typed-html-option (value option)
  (let ((type (getf (cdr option) :type)))
    (typecase value
      (string (case type
                (symbol  (intern (string-upcase value) :keyword))
                (string value)
                (integer (parse-integer value))
                (number  (multiple-value-bind (num pos) (parse-integer value :junk-allowed t)
                           (cond ((eql pos (length value)) num)
                                 ((eql (char value pos) #\.)
                                  (let ((dec (parse-integer value :start (1+ pos))))
                                    (+ num (* 0.1 (or dec  0)))))
                                 (t 0))))
                (boolean (if value t nil))
                (url #+:CL-HTTP (if *intern-url-values*
                                  (let ((merged-url (http:merge-url value (http:local-context)))
                                        (url:*url-host-name-resolution* :never))
                                    (values (url:intern-url merged-url)))
                                  (values value))
                     #-:CL-HTTP value
                     )
                (t (cond ((and (consp type) (eq (car type) 'member))
                          (if (some #'(lambda (symbol) (string-equal value symbol))
                                    (cdr type))
                            (intern (string-upcase value) :keyword)
                            (values nil t)))
                         (t (values nil t)))
                   )))
      (symbol (case type
                (symbol value)
                (string value)
                (boolean (if value t nil))
                (t (cond ((and (consp type) (eq (car type) 'member))
                          (if (some #'(lambda (symbol) (string-equal value symbol))
                                    (cdr type))
                            (values value)
                            (values nil t)))
                         (t (values nil t)))
                   )))
      (integer (case type
                 (integer value)
                 (number  (truncate value))
                 (t (values nil t))))
      (number (case type
                (integer value)
                (number  value)
                (t (values nil t))))
      (t (values nil t)))
    ))

(defun set-html-options (html options &optional (only-validate-p nil))
  (labels ((set-option (option value)
             (let ((html-option (find-html-option html option)))
               (if html-option
                 (unless only-validate-p
                   (multiple-value-bind (value errorp) (typed-html-option value html-option)
                     (setf (slot-value html (car html-option)) value)
                     (if errorp
                       (list (cons option value))
                       (values nil))))
                 (list (cons option value)))))
           (set-options (options)
             (cond ((atom options) nil)
                   ((atom (car options))
                    (nconc (set-option (car options) (cadr options))
                           (set-options (cddr options))))
                   (t (nconc (set-option (caar options) (cdar options))
                             (set-options (cdr options))))))
           )
    (let ((unknown-options (set-options options))
          (missing-options (mapcan #'(lambda (slot)
                                       (unless (slot-boundp html slot)
                                         (list slot)))
                                   (html-required-options html))))
      (unless only-validate-p
        (setf (html-unknown-options html) unknown-options))
      (values unknown-options missing-options))
    ))

(defmethod validate-html-options ((code html-class) options)
  (let ((unknown-options (set-html-options code options t)))
    (values (null unknown-options)))
  )

;;;

(defun html-instance-class (html)
  (cond ((stringp html)
         (let ((class
		(cdr (find-if #'(lambda (class)
				  (and (string-equal (car class) html)
				       (subtypep *html-version* (html-version (cdr class)))))
			      *html-class-alist*))))
           (or class 'unknown-html)))
        ((typep html 'html-class) html)
        ((find-class html nil) html)
        (t 'unknown-html)))

(defmethod html-code ((class-name string))
  (let ((instance-class (html-instance-class class-name)))
    (if (eql instance-class 'unknown-html)
      class-name
      (html-code (html-prototype instance-class)))))

(defun make-html (html-code options &optional (parts nil))
  (let* ((html-class (html-instance-class html-code))
         (html (if (typep html-class 'html-class) html-class (make-instance html-class))))
    (if (typep html 'unknown-html)
      (progn
        (setf (html-code    html) html-code
              (html-options html) options))
      (progn
        (set-html-options html options)
        (when (typep html 'html-environment)
          (setf (html-environment-parts html) parts))))
    (values html)))

(defun html-reader (stream char arg)
  (declare (ignore char arg))
  (let ((html-form (read stream t nil t)))
    (cond ((atom html-form)
           (make-html html-form nil))
          ((atom (car html-form))
           (make-html (car html-form) (cdr html-form)))
          (t (make-html (caar html-form) (cdar html-form) (cdr html-form))))
    ))

(set-dispatch-macro-character #\# #\h #'html-reader)

;;; HTML output

#-:CL-HTTP
(progn
  (defparameter *special-character-translation-alist*
    '((#\> . "&gt;")
      (#\< . "&lt;")
      (#\& . "&amp;")
      (#\" . "&quot;"))
    "&; delimited tokens are used to print special tokens.")

  (defun special-char-for-token (token)
    (car (rassoc token *special-character-translation-alist* :test #'equalp)))
  )

;;;

(defvar *html-stream*  nil)

(defparameter +option-unboundp+ (cons nil nil))

(defun write-html-options (html &optional (stream *html-stream*) (readablyp nil))
  (multiple-value-bind (html options) (if (listp html)
                                        (values (html-prototype (car html)) (cdr html))
                                        (values html t))
    (mapc #'(lambda (option-def)
              (let ((option (car option-def))
                    (option-type (getf (cdr option-def) :type)))
                (let ((option-value (if (eq options t)
                                      (if (slot-boundp html option)
                                        (slot-value html option)
                                        +option-unboundp+)
                                      (or (getf options option +option-unboundp+)
                                          (getf options (getf (cdr option-def) :code) +option-unboundp+))
                                      )))
                  (unless (eq option-value +option-unboundp+)
                    (write-char #\Space stream)
                    (if readablyp
                      (prin1 (cons option option-value) stream)
                      (let ((option (getf (cdr option-def) :code option)))
                        (case option-type
                          (string (format stream "~a=~s" option (string option-value)))
                          (url (if (typep option-value 'url)
                                 (format stream "~a=~s" option
                                         #+:CL-HTTP (url:coerce-url-string option-value t)
                                         #-:CL-HTTP option-value)
                                 (format stream "~a=~a" option option-value)))
                          (boolean (when option-value
                                     (format stream "~a" option)))
                          (t (if (and (consp option-type) (eq (car option-type) 'member))
                               (format stream "~a=~a" option option-value)
                               (format stream "~a=~s" option option-value))))))
                    ))
                ))
          (and (slot-boundp html 'options)
               (html-options html)))
    (values html)))

(defmethod print-object ((html html-class) stream)
  (if (or *print-readably* *print-escape*)
    (progn
      (format stream "#h(~a" (if (slot-boundp html 'code) (html-code html) 'unknown-html))
      (write-html-options html stream t)
      (format stream ")"  ))
    (print-unreadable-object (html stream :type t :identity t)
      (write-html-options html stream)
      (when (typep html 'html-environment)
        (format stream " ~a part~:p" (length (html-environment-parts html)))))
    )
  )

(defmethod print-object ((html html-comment) stream)
  (print-unreadable-object (html stream :type t :identity t)))

(defmethod print-object ((html html-document) stream)
  (print-unreadable-object (html stream :type t :identity t)))

(defun write-html-code (html stream end-p &optional (indent nil))
  (let* ((fresh-line-p (and (typep html 'html-class) (html-fresh-line-p html)))
         (fresh-lines (cond ((not fresh-line-p) 0)
                            ((numberp fresh-line-p) fresh-line-p)
                            (t 1))))
    (when (or (numberp indent) (not end-p))
      (format stream "~v&" fresh-lines))
    (when (numberp indent)
      (dotimes (i (abs indent))
	#-MCL (declare (ignore i))
        (write-string "  " stream)))
    (write-string "<" stream)
    (cond ((stringp html) (write-string "!" stream))
          (end-p          (write-string "/" stream)))
    (write-string (cond ((stringp html) html)
                        ((listp html) (string (car html)))
                        (t (html-code html)))
                  stream)
    (unless (or end-p (stringp html))
      (write-html-options html stream))
    (write-string ">" stream)
    (when end-p
      (format stream "~v&" fresh-lines))
    ))

(defmethod write-html :around (html &optional (stream *html-stream*) (type t) (indent nil))
  (when (or (null stream) (eq stream t))
    (setf stream (or *html-stream* *standard-output*)))
  (call-next-method html stream type indent))

(defmethod write-html ((html list) &optional (stream *html-stream*) (type t) (indent nil))
  (declare (ignore type))
  (write-html-code html stream nil indent))

(defmethod write-html ((html html-clause) &optional (stream *html-stream*) (type t) (indent nil))
  (declare (ignore type))
  (write-html-code html stream nil indent))

(defmethod write-html ((html html-comment) &optional (stream *html-stream*) (type t) (indent nil))
  (declare (ignore type))
  (let ((comment-text (if indent "COMMENT" (html-comment-text html))))
    (write-html-code comment-text stream nil indent)))

(defmethod write-html ((html html-document) &optional (stream *html-stream*) (type t) (indent nil))
  (write-html-parts html stream type indent))

(defmethod write-html ((html html-environment) &optional (stream *html-stream*) (type t) (indent nil))
  (write-html-code  html stream nil  indent)
  (write-html-parts html stream type indent)
  (if (numberp indent)
    (unless (minusp indent)
      (write-html-code html stream t indent))
    (write-html-code html stream t indent))
  )

(defun write-html-part (part stream &optional (type t) (indent nil))
  (when (typep part type)
    (cond ((stringp part) (write-string part stream))
          ((typep part '(or html-class html-comment))
           (write-html part stream type
                       (if (numberp indent) (+ indent (signum indent)) indent)))
          (t nil))
    ))

(defmethod write-html-parts ((html html-environment) stream &optional (type t) (indent nil))
  (mapc #'(lambda (part) (write-html-part part stream type indent))
        (if (listp html)
          html
          (html-environment-parts html))))

;;; search functions: map-html, match-options, match-html, html-match

(defun match-options (html options &key (test #'eql))
  (labels ((match-option (option value)
             (let ((option (find-html-option html option)))
               (and option
                    (slot-boundp html (car option))
                    (funcall test (slot-value html (car option)) value))))
           (match-options (options)
             (cond ((atom options) t)
                   ((atom (car options))
                    (and (match-option (car options) (cadr options))
                         (match-options (cddr options))))
                   (t (and (match-option (caar options) (cdar options))
                           (match-options (cdr options))))))
           )
    (match-options options)))

(defun match-html (html &key (code nil) (content nil) (options nil)
                        (optional-end nil) (test nil))
  (and (or (not test) (funcall test html))
       (or (not code)    (string-equal (html-code html) code))
       (or (not content) (multiple-value-bind (list item)
                                              (if (listp content)
                                                (values content html)
                                                (values html content))
                           (some #'(lambda (type)
                                     (typep item type))
                                 list)))
       (or (not optional-end) (html-optional-end html))
       (or (not options) (match-options html options))))
           
(defun html-match (html1 html2 &key (code nil) (content nil) (options nil)
                         (optional-end nil) (test nil))
  (and (or (not test) (funcall test html1 html2))
       (or (not (functionp html1)) (funcall html1 html2))
       (or (not (functionp html2)) (funcall html2 html1))
       (or (not code) (string-equal (html-code html1) (html-code html2)))
       (or (not content) (some #'(lambda (type)
                                   (if (symbolp html1)
                                     (subtypep html1 type)
                                     (typep html1 type)))
                               (html-content html2)))
       (or (not optional-end) (html-optional-end html2))
       (or (not options) (match-options html2 options))))

(defun map-html (fun html &key (type 'html-class) (collect-p nil) (context-p nil))
  (let ((result nil))
    (labels ((map-html (html context)
               (when context-p
                 (push html context))
               (when (typep html type)
                 (when (and (funcall fun (if context-p context html)) collect-p)
                   (push (if context-p context html) result)))
               (when (typep html 'html-environment)
                 (mapc #'(lambda (sub-html)
                           (map-html sub-html (when context-p context)))
                       (html-environment-parts html))))
             )
      (if (listp html)
        (mapc #'(lambda (html)
                  (map-html html nil))
              html)
        (map-html html nil)))
    (values result)))

;;;

(defvar *html-context* nil)

(defun make-html-stream ()
  (make-string-output-stream))

(defun add-html-part (part &optional (html-context *html-context*))
  (cond ((null html-context) (values nil))
        ((and (stringp part) (zerop (length part))) (values html-context))
        ((typep part
		#+LISPWORKS 'common-lisp::string-stream
		#-LISPWORKS 'string-stream)
         (let ((output-string (get-output-stream-string part)))
           (add-html-part output-string html-context)))
        (t (push part (html-environment-parts (car html-context)))
           (values html-context)))
  )

(defun pop-html-context (html &optional (key :code) (html-context *html-context*))
  (labels ((pop-html (htmls)
             (cond ((null htmls)                       (values nil))
                   ((typep html        'html-document) (values nil))
                   ((typep (car htmls) 'html-document) (values html-context))
                   ((or (eq html (car htmls))
                        (and key (html-match html (car htmls) key t))) (values (cdr htmls)))
                   ((and (functionp html)
                         (html-match html (car htmls)))                (values htmls))
                   (t (pop-html (cdr htmls)))))
           )
    (let ((new-context (pop-html html-context)))
      (mapc #'(lambda (html)
                (when (typep html 'html-environment)
                  (setf (html-environment-parts html)
                        (reverse (html-environment-parts html)))))
            (ldiff html-context new-context))
      (values new-context))
    ))

(defun push-html-context (html &optional (html-context *html-context*) (dont-pop-p nil))
  (flet ((valid-context-p (html2)
           (or (not (match-html html2 :optional-end t))
               (html-match html html2 :content t)))
         )
    (unless dont-pop-p
      (setf html-context (pop-html-context #'valid-context-p nil html-context)))
    (unless (and html-context (html-match html (car html-context) :content t))
        (setf (html-out-of-context-p html) t))
    (when html-context
      (add-html-part html html-context))
    (setf (html-environment-parts html) nil)
    (cons html html-context)))

(defun with-html-environment-fun (stream code options cont)
  (let ((html (and code (make-html code options))))
    (cond ((typep stream 'string-stream)
           (add-html-part stream *html-context*))
          (stream (write-html-code (or html options) stream nil)))
    (let ((*html-context* (when html
                            (push-html-context html *html-context* t)))
          (*html-stream* stream))
      (multiple-value-prog1 (funcall cont stream)
        (cond ((typep stream 'string-stream)
               (add-html-part stream *html-context*))
              (stream (write-html-code (or html options) stream t)))
        (when code
          (pop-html-context code :code *html-context*))
        ))
    ))

(defun html-class-fun (stream code options &rest parts)
  (let ((html (and code (make-html code options))))
    (when (typep html 'html-environment)
      (setf (html-environment-parts html) parts))
    (cond ((typep stream 'string-stream)
           (add-html-part stream *html-context*))
          (stream (write-html (or html options) stream)))
    (when html
      (add-html-part html *html-context*)))
  )

(defmacro with-html-environment ((stream-var code . options) &body body)
  `(flet ((body (,(if (eq stream-var t) '*html-stream* stream-var)) . ,body))
     (with-html-environment-fun (or ,stream-var *html-stream*)
       ,code (list . ,options) #'body))
  )

(defmacro with-html-document ((&optional (html-doc-var (gensym))) &body body)
  `(let* ((,html-doc-var (make-html 'html-document nil))
          (*html-context* (list ,html-doc-var)))
     (progn . ,body)
     (pop-html-context nil)
     (values ,html-doc-var))
  )

;;;
	 
(defun html-symbol (package format &rest args)
  (let* ((symbol (car (last args)))
         (package (cond ((not (eq package t)) package)
                        ((symbolp symbol) (symbol-package symbol))
                        (t *package*)))
	 (symbol-name (apply #'format nil format args))
	 (symbol (find-symbol symbol-name package)))
    (cond ((null symbol) (intern symbol-name package))
	  ((eq (symbol-package symbol) (find-package :common-lisp))
	   (shadow symbol package)
	   (find-symbol (symbol-name symbol) package))
	  (t (values symbol))))
  )

(defun html-class-name (name)       (html-symbol t "~a"          name))
(defun html-macro-name (name)       (html-symbol t "WITH-~a"     name))
(defun html-fun-name   (name)       (html-symbol t "~a"          name))
(defun html-slot-name  (class name) (html-symbol t "~a-~a" class name))
(defun html-keyword    (name)       (html-symbol :keyword "~a"   name))

(defun define-html-macro-with-macro-fun (name)
  (let ((macro-name (html-macro-name name)))
    `(defmacro ,macro-name ((stream . options) &body body)
       `(with-html-environment (,stream ',',name . ,options) . ,body))
    ))

(defun define-html-class-fun (name &rest args)
  (let ((fun-name (html-fun-name name)))
    `(defun ,fun-name (,@args &optional (stream nil) &rest options)
       (html-class-fun (or stream *html-stream*) ',name options ,@args))
    ))

#|
(let* ((html-doc (make-html 'html-document nil))
       (*html-context* (list html-doc))
       (stream nil)) ; (make-html-stream)
  (html-2:with-head (stream)
     (html-2:with-title (stream)
      (write-string "Test-tittel" stream))
     (html-2:html-title "Test-tittel" stream))
  (html-2:with-body (stream)
    (write-string "her" stream)
    (html-2:html-image stream 'url "http://www.si.sintef.no/" 'align 'left)
    (write-string "der" stream))
  (pop-html-context html-doc nil *html-context*)
  (write-html html-doc t 't))

(etypecase url
      (null 'url)
      (symbol
       `(url:intern-url (merge-url ,url (local-context))
                        :if-does-not-exist :create))
      ((or cons string)
       `(url:intern-url (merge-url ',url (http-user::local-context))
                        :if-does-not-exist :create)))
|#


;;;

;;;

(defmacro define-html-versions (&rest versions)
  (labels ((version-name (version)
             (string version))
           (version-form (version)
             (destructuring-bind (version &rest supers) version
               (let ((version-name (version-name version)))
                 `((defclass ,version ,(or supers '(html-class))
                     ((version :allocation :class :initform ',version))
                     )
                   (defpackage ,version-name
                     (:use "COMMON-LISP" "HTML-BASE")
                     (:import-from ,(package-name (symbol-package version)) ,version-name)
                     ,@(mapcar #'(lambda (super)
                                   `(:import-from ,(version-name super) ,(version-name super)))
                               supers)
                     (:export ,version-name
                              ))
                   )
                 )))
           )
    `(progn
       ,@(mapcan #'version-form versions)
       )
    ))

;;;

(defun listify (list)
  (if (listp list) list (list list)))

(defun define-html-macro-defclass-fun
       (base-html-class name super code content required-options optional-end
                        fresh-line-p
                        options)
  (setf super   (listify super)
        content (listify content))
  (let ((class-name (html-class-name name))
        (super-names (cond ((listp super) (mapcar #'html-class-name super))
                           (t (html-class-name super))))
        (code (if (eq code t) (string name) code)))
    (setf super-names (append super-names (list base-html-class)))
    (flet ((slot-def (option)
             (destructuring-bind (name &key (type nil) (code nil)) option
               (declare (ignore code))
               `(,name :type ,type
                       :initarg ,name :initarg ,(html-keyword name)
                       :accessor ,(html-slot-name class-name name))
               ))
           )
      (let* ((options options)
             (slot-definitions (mapcar #'slot-def options))
             (content (when content
                        `(append (mapcan #'(lambda (class-name)
                                             (copy-list (html-content class-name)))
                                         ',super-names)
                                 ',(mapcar #'html-class-name content))))
             (options `(append (mapcan #'(lambda (class-name)
                                           (copy-list (html-options class-name)))
                                       ',super-names)
                               ',options))
             )
        `(let ((code              ,code)
               ,@(when content
		       `((content           ,content)))
               ,@(when required-options
		       `((required-options ',required-options)))
               ,@(when optional-end
		       `((optional-end     ',optional-end))))
           (defclass ,class-name (,@super-names)
             (
              (code    :allocation :class :initform code)
              (options :allocation :class :initform ,options)
              ,@(when content
		      `((content :allocation :class :initform content)))
              ,@(when required-options
		      `((required-options :allocation :class :initform required-options)))
              ,@(when optional-end
		      `((optional-end :allocation :class :initform optional-end)))
              ,@(when fresh-line-p
		      `((fresh-line-p :allocation :class :initform ',fresh-line-p)))
              ,@slot-definitions
              ))
             )
        ))))

(defun define-html-macro-fun
       (base-html-class name super code content required-options optional-end
                        fresh-line-p
                        options)
  (let ((defclass-form
          (define-html-macro-defclass-fun base-html-class name super code content
            required-options optional-end fresh-line-p options))
        (macro-forms (cond ((subtypep base-html-class 'html-environment)
                            (list (define-html-macro-with-macro-fun name)
                                  (define-html-class-fun name 'text)))
                           ((subtypep base-html-class 'html-clause)
                            (list (define-html-class-fun name)))))
        (class-name (html-class-name name))
        (code (if (eq code t) (string name) code)))
    `(progn
       ,defclass-form
       ,(when code
          `(let ((code ,code))
             ,@macro-forms
             (pushnew (cons code ',class-name) *html-class-alist* :test #'equal)
             )
          )
       (let ((symbols '(,class-name ,@(mapcar #'cadr macro-forms)
                        . ,(mapcan #'(lambda (slot-form)
                                       (let ((accessor (getf (cdr slot-form) :accessor)))
                                         (when accessor
                                           `(,(car slot-form) ,accessor))))
                                   (fourth (find-if #'(lambda (form)
                                                        (and (consp form) (eq (car form) 'defclass)))
                                                    defclass-form))
                                   ))))
         (import (remove-if #'(lambda (symbol) (find-symbol (string symbol))) symbols))
         (export symbols))
       (find-class ',class-name)
       )
    ))

(defmacro define-html
          (name super (&optional code
                                 &key content required-attributes optional-end fresh-line-p)
                &rest options)
  (define-html-macro-fun 'html-environment name super code content
    required-attributes optional-end fresh-line-p options)
  )

(defmacro define-sub-html (supers &rest subs)
  (flet ((sub-form (sub)
           (unless (listp sub)
             (setf sub (list sub)))
           (destructuring-bind (sub &optional code) sub
             `(define-html ,sub ,supers (,code))))
         )
    `(progn . ,(mapcar #'sub-form subs))
    ))

(defmacro define-html-clause
          (name super (&optional code
                                 &key content required-attributes optional-end fresh-line-p)
                &rest options)
  (define-html-macro-fun 'html-clause name super code content
    required-attributes optional-end fresh-line-p options)
  )

