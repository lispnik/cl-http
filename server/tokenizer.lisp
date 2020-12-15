;;;   -*- Mode: LISP; Package: (tk1 use future-common-lisp :colon-mode :external); BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-

;;;
;;; (c) Copyright  1995, John C. Mallery
;;;     All Rights Reserved.
;;;
;;; SIMPLE TOKENIZER
;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defpackage tk1
  (:use future-common-lisp)
  (:import-from "WWW-UTILS"
   "WITH-FAST-ARRAY-REFERENCES")
  (:import-from "HTTP"
   "PUSH-ORDERED")
  (:export
    "DEFINE-TOKENIZER"
    "UNDEFINE-TOKENIZER"))

(in-package :tk1)


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defstruct (entry
             (:print-function print-entry))
  (key nil :read-only t)
  (level nil))

(defstruct (value-entry
             (:include entry))
  (value))

(defstruct (level
             (:print-function print-level))
  (entries nil :type list))

(defstruct (tokenizer)
  (test nil)
  (function nil)
  (top-level nil :type (or null level))
  (name "tokenizer" :type string)
  (documentation nil :type (or null string)))

(defun print-entry (entry stream depth)
  (declare (ignore depth))
  (print-unreadable-object (entry stream :type t :identity t)
    (when (entry-key entry)
      (write (entry-key entry) :stream stream :escape nil))))

(defun print-level (level stream depth)
  (declare (ignore depth))
  (print-unreadable-object (level stream :type t :identity t)
    (when (level-entries level)
      (dolist (entry (level-entries level))
        (write (entry-key entry) :stream stream :escape nil)))))

(defvar *tokenizer*)

(declaim (inline tokenize-value))

(defun tokenize-value (string start end)
  (funcall (tokenizer-function *tokenizer*) string start end))

(defun %make-level-entry (string index start end)
  (declare (values new-entry token))
  (with-fast-array-references ((string string string))
    (loop with token = (tokenize-value string start end)
          with entry = (make-value-entry :key (aref string (1- (the fixnum end)))
                                         :level nil
                                         :value token)
          for idx downfrom (- (the fixnum end) 2)
          until (< idx index)
          do (setq entry (make-entry :key (aref string idx)
                                     :level (make-level :entries (list entry))))
          finally (return (values entry token)))))

(defun %set-value-entry (entry value level test)
  (declare (values value))
  (etypecase entry
    (value-entry
      (setf (value-entry-value entry) value))
    (entry
      (let ((new-entry (make-value-entry :key (entry-key entry)
                                         :level (entry-level entry)
                                         :value value))
            (pos (position (entry-key entry) (level-entries level) :test test :key #'entry-key)))
        (setf (nth pos (level-entries level)) new-entry))))
  value)

(declaim (inline find-entry))

(defun find-entry (key level test)
  (loop for entry in (level-entries level)
        when (funcall test key (entry-key entry))
          return entry
        finally (return nil)))

(defun %insert-new-level-entry (level string index start end)
  (declare (values token))
  (multiple-value-bind (new-entry token)
      (%make-level-entry string index start end)
    (let ((entries (level-entries level)))
      (if entries
          (push-ordered new-entry (level-entries level) #'char< :key #'entry-key)
          (setf (level-entries level) (list new-entry))))
    token))

(defun %put-token (level string test index start end)
  (declare (values token))
  (with-fast-array-references ((string string string))
    (loop with level = level
          for idx upfrom index
          while (< idx end)
          for char = (aref string idx)
          for entry = (find-entry char level test)
          when entry
            do (setq level (entry-level entry))
          else
            return (%insert-new-level-entry level string idx start end)
          finally (return (%set-value-entry entry (tokenize-value string start end) level test)))))

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

(defun %get-token (tokenizer string &optional (start 0) (end (length string)))
  (declare (values token found-p))
  (with-fast-array-references ((string string string))
    (loop with level = (tokenizer-top-level tokenizer)
          with test = (tokenizer-test tokenizer)
          for idx upfrom start
          while (< idx end)
          for char = (aref string idx)
          for entry = (find-entry char level test)
          when entry
            do (setq level (entry-level entry))
          else do (return-from %get-token nil)
          finally (return-from %get-token (etypecase entry
                                            (value-entry
                                              (values (value-entry-value entry) t))
                                            (entry nil))))))

(defun %tokenizer-variable (name &optional (package *package*))
  (intern (format nil "*~A-TOKENIZER*" name) package))

(defun %tokenizer-function (name &optional (package *package*))
  (intern (format nil "TOKENIZE-~A" name) package))

(defun %tokenizer-fast-function (name &optional (package *package*))
  (intern (format nil "%TOKENIZE-~A" name) package))

(defun %define-tokenizer (name tokenizer test documentation definer)
  (let* ((name (string name))
         (package *package*)
         (variable (%tokenizer-variable name package))
         (function-name (%tokenizer-function name package))
         (fast-function-name (%tokenizer-fast-function name package)))
    (check-type definer symbol)
    `(progn
       (defvar ,variable)
       (cond ((boundp ',variable)
              (let ((tk ,variable))
                (setf (tokenizer-name tk) ,name
                      (tokenizer-function tk) (fdefinition ,tokenizer)
                      (tokenizer-test tk) (fdefinition ,test)
                      (tokenizer-documentation tk) ,documentation)))
             (t (setq ,variable (make-tokenizer :name ,name
                                                :test (fdefinition ,test)
                                                :function (fdefinition ,tokenizer)
                                                :documentation ,documentation))))
       ;; define a fast tokenizer
       (declaim (inline ,fast-function-name))
       (,definer ,fast-function-name (string &optional (start 0) end)
        (declare (values token newly-interned-p))
        (%get-or-put-token ,variable string start (or end (length string))))
       ;; define a general-purpose interface function.
       (,definer ,function-name (string &optional (start 0) end (if-does-not-exist :create))
        (declare (values token newly-interned-p))
        (ecase if-does-not-exist
          (:create
            (%get-or-put-token ,variable string start (or end (length string))))
          (:soft (%get-token ,variable string start (or end (length string))))
          (:error
            (or (%get-token ,variable string start (or end (length string)))
                (error "Unknown token: ~S." (subseq  string start end))))))
       ,variable)))

(defmacro define-tokenizer (name &key test tokenizer documentation (definer 'defun))
  (%define-tokenizer name tokenizer test documentation definer))

(defun undefine-tokenizer (name &optional (package *package*))
  (let ((var (%tokenizer-variable name package)))
    (makunbound var)
    (unintern var package)))

#|

(defun describe-tokenizer (tokenizer &optional (stream *standard-output*))
  (labels ((%describe-level (level &optional (stream *standard-output*) (depth 0) (branch 0))
             (loop with entries = (etypecase level
                                    (tokenizer (level-entries (tokenizer-top-level level)))
                                    (level (level-entries level)))
                   with l-entries = (length entries)
                   for entry in entries
                   for idx upfrom  1
                   for n-level = (entry-level entry)
                   do (write-char (entry-key entry) stream)
                      (typecase entry
                        (value-entry
                          (format stream " => ~A" (value-entry-value entry))
                          (fresh-line stream)
                          (dotimes (idx (if (entry-level entry) (1+ depth) branch))
                            (write-char #\. stream))))
                   when n-level
                     do (%describe-level n-level stream (1+ depth)
                                         (if (or (= 1 l-entries) (= idx l-entries)) branch depth)))))
    (fresh-line stream)
    (%describe-level tokenizer stream 0)))

(defun intern-header (string &optional (start 0) (end (length string)))
  (http::%header-keyword
    (intern (subseq string start end) http::*keyword-package*)))

(undefine-tokenizer 'header)

(define-tokenizer header
                  :tokenizer 'intern-header
                  :test 'eql
                  :documentation "Tokenizes headers without consing.")

(mapc #'tokenize-header headers)

(defparameter headers '("accept"
                        "authorization"
                        "connection"
                        "content-id"
                        "content-length"
                        "content-type"
                        "date"
                        "derived-from"
                        "expires"
                        "forwarded"
                        "from"
                        "if-modified-since"
                        "keep-alive"
                        "last-modified"
                        "location"
                        "method"
                        "mime-version"
                        "referer"
                        "Server"
                        "uri"
                        "User-Agent"
                        "version"
                        "www-authenticate"))

(tokenize-header "Accept-Language")
(tokenize-header "authorization")

(mapc #'tokenize-header headers)

(tokenize-header "content")

(defparameter circular (apply #'scl:circular-list headers))

(defun test (n)
  (loop for idx upfrom 0 below n
        for item in circular
        do #+ignore (intern-header item)
           (%tokenize-header item)))

|#
