;;;   -*- Mode: LISP; Package: :netscape2.0; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-

;;;
;;; (c) Copyright 1996, John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CLIENT-SIDE SCRIPT SUPPORT
;;;
(in-package :netscape2.0)

(defclass script-writer-mixin
          ()
    ((writer :initform nil :initarg :writer :accessor script-writer)
     (writer-arglist :initform nil :initarg :writer-arglist :accessor script-writer-arglist)))

(defclass script-caller-mixin
          ()
    ((caller :initform nil :initarg :caller :accessor script-caller)
     (caller-arglist :initform nil :initarg :caller-arglist :accessor script-caller-arglist)))

(defclass script
          (http:property-list-mixin script-writer-mixin script-caller-mixin)
    ((name :initarg :name :reader script-name)
     (language :initarg :language :reader script-language :allocation :class)
     (content-type :initarg :content-type :reader script-content-type :allocation :class)
     (bytes :initform nil :initarg :bytes :accessor script-bytes)
     (url :initform nil :initarg :url :accessor script-url))
  (:documentation "The basic script for mobile code applications."))

(defclass client-side-script
          (script)
    ()
  (:documentation "The client-side script for mobile code applications."))

(defclass java-script
          (client-side-script)
    ((language :initform :java-script :allocation :class)
     (content-type :initform :javascript :allocation :class))
  (:documentation "A JavaScript program suitable for the Netscape 2.0 client."))

(defclass java-script1.1
          (java-script)
    ((language :initform :java-script1.1 :allocation :class))
  (:documentation "A JavaScript 1.1 program suitable for the Netscape 3.0 client."))

(defclass java-script1.2
          (java-script)
    ((language :initform :java-script1.2 :allocation :class))
  (:documentation "A JavaScript 1.1 program suitable for the Netscape 4.0 client."))

(defmethod qualified-name ((script script))
  (with-slots (name language) script
    (http:with-value-cached (script :qualified-name)
      (concatenate 'string name "|" language))))

(defmethod print-object ((script script) stream)
  (with-slots (name language) script
    (print-unreadable-object (script stream :type t :identity t)
      (write name :stream stream :escape nil)
      (write-char #\| stream)
      (write language :stream stream :escape nil))))


;;;------------------------------------------------------------------- 
;;;
;;; DEFINE SCRIPT LANGUAGE
;;;

(defvar *script-language-table-alist* nil)

(defun %initialize-script-language-table (language)
  (check-type language keyword)
  (let ((entry `(,language . ,(make-hash-table :test #'equal))))
    (push entry *script-language-table-alist*)))

(declaim (inline %get-script-language-table))

(defun %get-script-language-table (language &optional create-p)
  (cond ((cdr (assoc language *script-language-table-alist* :test #'eq)))
        (create-p (%initialize-script-language-table language))
        (t (error "~S is not one of the known lanuages: ~S."
                  language (mapcar #'car *script-language-table-alist*)))))

(defun %get-script-language-class (language)
  (or (get language 'class)
      (error "~S is not one of the known lanuages: ~S."
             (mapcar #'car *script-language-table-alist*))))

(defun %define-script-language (language class content-type)
  (%get-script-language-table language t)
  (setf (get language 'class) class
        (get language 'content-type) content-type))

(define-macro define-script-language (name &key class content-type)
  "Declares NAME to denote a scripting language.
A scripting language must be declared before scripts can be defined for it."
  `(%define-script-language ',(http:symbolize (string name) http:*keyword-package*) ',class ',content-type))

;; Suitable for Netscape 2.0
(define-script-language java-script
                        :class java-script
                        :content-type :javascript)

;; Suitable for Netscape 3.0
(define-script-language java-script1.1
                        :class java-script1.1
                        :content-type :javascript)

;; Suitable for Netscape 4.0
(define-script-language java-script1.2
                        :class java-script1.2
                        :content-type :javascript)


;;;------------------------------------------------------------------- 
;;;
;;; INTERN SCRIPT
;;;

(declaim (inline %get-script))

(defun %get-script (language name)
  (let ((table (%get-script-language-table language)))
    (gethash name table)))

(defun %set-get-script (language name script)
  (let ((table (%get-script-language-table language)))
    (setf (gethash name table) script)))

(defsetf %get-script %set-get-script)

(defmethod %register-script ((script script))
  (with-slots (name language) script
    (setf (%get-script language name) script)
    script))

(define-generic unintern-script (script)
  (:documentation "Uninterns SCRIPT so that it is no longer accessible."))

(defmethod unintern-script ((script script))
  (with-slots (name language) script
    (remhash name (%get-script-language-table language))
    script))

(defmethod unintern-script :after ((script script))
  (with-slots (url) script
    (typecase url
      (url:http-client-script
        (when (eq (url:script url) script)
          (unintern-url url)
          (setf url nil))))
    script))

(define intern-script (name language &key url (if-does-not-exist :error))
  "Interns a script named NAME in language LANGUAGE.
If URL is provided, the script is associated with URL but not exported.
IF-DOES-NOT-EXIST can be any of :SOFT, :CREATE, or :ERROR."
  (flet ((%intern-script (name language url if-does-not-exist &aux object)
           (cond ((setq object (%get-script language name))
                  (cond-every
                    (url (setf (script-url object) (url:intern-url url :if-does-not-exist :error))))
                  object)
                 (t (ecase if-does-not-exist
                      (:soft nil)
                      (:create
                        (let ((script (make-instance (%get-script-language-class language)
                                                     :name name
                                                     :url (when url
                                                            (url:intern-url url :if-does-not-exist :error)))))
                          (%register-script script)
                          (values script t)))
                      (:error
                        (error "No script named, ~S, is known for the scripting language, ~S." name language)))))))
    (etypecase name
      (symbol (%intern-script (symbol-name name) language url if-does-not-exist))
      (string (%intern-script name language url if-does-not-exist))
      (script
        (unless (eq language (script-language name))
          (error "The script ~S is not in the language, ~S." name language))
        name))))

(defun %script-caller-name (script-name language)
  (intern (format nil "~A-~A-CALLER" script-name language)))

(defun %define-script-caller (name language caller-spec)
  (when caller-spec
    (let ((fname (%script-caller-name name language)))
      (etypecase caller-spec
        (cons
          (destructuring-bind ((script stream &rest arglist) &body body)
              caller-spec
            (let ((lambda-list `(,script ,@arglist)))
              `((defun ,fname ,lambda-list
                  ,script                       ;potentially ignore
                  (flet ((write-event-call (,stream) ,@body))
                    #'write-event-call))
                (setf (script-caller-arglist script) '(,script ,@arglist)
                      (script-caller script) (fdefinition ',fname))))))
        (string
          `((defun ,fname (script)
              (declare (ignore script))
              (flet ((write-event-call (stream)
                       (write-string ,caller-spec stream)))
                #'write-event-call))
            (setf (script-caller-arglist script) '(script)
                  (script-caller script) (fdefinition ',fname))))))))

(define-macro define-script-caller (script-name (language) &body body)
  (let* ((name (string script-name))
         (language (http:symbolize (string language) http:*keyword-package*)))
    `(let ((script (intern-script ',name ',language)))
       ,.(%define-script-caller name language body))))

(defun %script-writer-name (script-name language)
  (intern (format nil "~A-~A-WRITER" script-name language)))

(defun %define-script-writer (name language writer-spec)
  (when writer-spec
    (let ((fname (%script-writer-name name language)))
      (etypecase writer-spec
        (cons
          (destructuring-bind ((script stream &rest arglist) &body body)
              writer-spec
            (let ((lambda-list `(,script ,stream ,@arglist)))
              `((defun ,fname ,lambda-list 
                  ,script                       ;potentially ignore
                  ,@body)
                (setf (script-writer-arglist script) ',lambda-list 
                      (script-writer script) (fdefinition ',fname))))))
        (string
          `((defun ,fname (script stream)
              (declare (ignore script))
              (write-string ,writer-spec stream))
            (setf (script-writer-arglist script) '(script stream)
                  (script-bytes script) ,(http:compute-transmitted-bytes writer-spec)
                  (script-writer script) (fdefinition ',fname))))))))

(define-macro define-script-writer (script-name (language) &body body)
  (let* ((name (string script-name))
         (language (http:symbolize (string language) http:*keyword-package*)))
    `(let ((script (intern-script ',name ',language)))
       ,.(%define-script-writer name language body))))

(defun %define-script (name language url)
  (intern-script name language :url url :if-does-not-exist :create))

(define-macro define-script (name (language) &key  documentation url caller script)
  "Defines a script named NAME in the scripting language, LANGUAGE.
URL is a url associated with the script.

SCRIPT defines a method to write a textual version of the script to a stream.
SCRIPT takes the arguments: ((SCRIPT STREAM &rest lambda-list) &body body)


CALLER defines a method to write a textual call to the script to a stream.
CALLER takes the arguments: ((SCRIPT STREAM &rest lambda-list) &body body).

A short argument form for both SCRIPT and CALLER accepts a constant string.

The following generic functions are generally useful: DECLARE-SCRIPT, 
EVENT-CALLER, EXPORT-SCRIPT, INTERN-SCRIPT, WRITE-SCRIPT."
  (let* ((name (string name))
         (language (http:symbolize language http:*keyword-package*)))
    `(let ((script (intern-script ',name ',language :url ,url :if-does-not-exist :create)))
       ,(if documentation
            `(setf (http:get-value script :documentation) ,documentation)
            `(http:remove-value script :documentation))
       ,.(%define-script-writer name language script)
       ,.(%define-script-caller name language caller)
       script)))

;;;------------------------------------------------------------------- 
;;;
;;; METHODS ON SCRIPTS
;;;

(define-generic script-language (script)
  (:documentation "Returns a keyword denoting the scripting language in which SCRIPT is written."))

(define-generic script-name (script)
  (:documentation "Returns the name string for SCRIPT."))

(define-generic script-writer-arglist (script)
  (:documentation "Returns the lambda list required to write SCRIPT."))

(define-generic script-caller-arglist (script)
  (:documentation "Returns the lambda list required to call SCRIPT."))

(define-generic declare-script (script stream &optional force-write-p)
  (:documentation "Declares SCRIPT on STREAM.
Use to emit an embed element in HTML containing either
a reference to the URL hosting the script or the text
of the script.  The URL unless fORCE-WRITE-P is non-null.
A script can only be declared in this way when it does not
take write arguments.  When write arguments are required,
use WRITE-SCRIPT."))

(defmethod declare-script ((script script-writer-mixin) stream &optional force-write-p &aux url)
  (if (or force-write-p (null (setq url (script-url script))))
      (with-embedded-script ((script-language script) :stream stream)
        (funcall (script-writer script) script stream))
      (with-embedded-script ((script-language script) :base-code-url url :stream stream))))

(define-generic write-script (script stream &rest lambda-list)
  (:documentation "Writes the text of SCRIPT on STREAM with.
LAMBDA-LIST is the set of argument required to write the script."))

(defmethod write-script ((script script-writer-mixin) stream &rest lambda-list)
  (netscape2.0:with-embedded-script ((script-language script)
                                     :base-code-url (script-url script)
                                     :stream stream)
    (apply (script-writer script) script stream lambda-list)))

(define-generic write-raw-script (script stream &rest lambda-list)
  (:documentation "Writes the text of SCRIPT on STREAM.
LAMBDA-LIST is the set of argument required to write the script."))

(defmethod write-raw-script ((script script-writer-mixin) stream &rest lambda-list)
  (apply (script-writer script) script stream lambda-list))

(define-generic event-caller (script &rest lambda-list)
  (declare (values call-writer-function))
  (:documentation "Returns a function that can be applied to a stream
in order to write a call to the script within an HTML generation context"))

(defmethod event-caller ((script script-caller-mixin) &rest lambda-list)
  (with-slots (caller) script
    (apply caller script lambda-list)))

(define-generic export-script (script &rest export-args &key url &allow-other-keys)
  (:documentation "Exports SCRIPT according to export-args."))

(defmethod export-script ((script script) &rest args &key &allow-other-keys)
  (destructuring-bind (&key url &allow-other-keys) args
    (apply #'http:export-url
           (or (and url
                    (setf (script-url script) (intern-url url)))
               (script-url script)
               (error "No URL is available for the export of ~S." script))
           :script
           :script script args)))

(defmethod documentation ((script script) #-(or CMU LispWorks4) &optional doc-type)
  (declare (ignore doc-type))
  (http:get-value script :documentation))


;;;------------------------------------------------------------------- 
;;;
;;; SCRIPTS DEFINED
;;;

#|

(export-script (intern-script "Check_Integer_Bounds" :java-script) :url #u"/script.javascript")

(define-script "Check_Integer_Bounds"
               (java-script)
  :documentation "Checks to make sure an integer is within bounds."
  :caller ((script stream min max required-p)
           (check-type min integer)
           (check-type max integer)
           (html:fast-format stream "\"if (!Check_Integer_Bounds(this.value, ~D, ~D, ~A))~% {this.focus();this.select();}\""
                             min max (if required-p "true" "false")))
  :script ((script stream)
           (html:fast-format stream
                             "function Check_Integer_Bounds(str, min, max, rqrdp) 
{
    if ((rqrdp == true) && (str == '')) {
        alert('Please enter a whole number between ' + min + ' and ' + max + '.')
        return false
    }
    for (var i = 0; i < str.length; i++) {
        var ch = str.substring(i, i + 1)
        if (ch < '0' || ch > '9') {
            alert('Please enter a whole number between ' + min + ' and ' + max + '.')
            return false
        }
    }

    var val = parseInt(str, 10)
    if ((val < min) || (val > max)) {
        alert('Please enter a whole number between ' + min + ' and ' + max + '.')
        return false
    }
    return true
}")))

(define-script Check_Integer_Bounds
               (java-script)
  :documentation "Checks to make sure an integer is within bounds."
  :caller "\"if (!Check_Integer_Bounds(this.value, ~D, ~D, ~A))~% {this.focus();this.select();}\""
  :script "function Check_Integer_Bounds(str, min, max, rqrdp) 
{
    if ((rqrdp == true) && (str == '')) {
        alert('Please enter a whole number between ' + min + ' and ' + max + '.')
        return false
    }
    for (var i = 0; i < str.length; i++) {
        var ch = str.substring(i, i + 1)
        if (ch < '0' || ch > '9') {
            alert('Please enter a whole number between ' + min + ' and ' + max + '.')
            return false
        }
    }

    var val = parseInt(str, 10)
    if ((val < min) || (val > max)) {
        alert('Please enter a whole number between ' + min + ' and ' + max + '.')
        return false
    }
    return true
}" )

(intern-script "Check_Integer_Bounds" :java-script)

|#
