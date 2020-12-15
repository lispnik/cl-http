;;;   -*- Mode: LISP; Package: lambda-ir; BASE: 10; SYNTAX: ansi-common-lisp -*-

;;; (C) Copyright 1997, Andrew J. Blumberg (blumberg@ai.mit.edu).
;;;     All Rights Reserved.
;;;
;;;
;;;------------------------------------------------------------------- 
;;;
;;; Lambdavista example
;;;

(in-package :lambda-ir)

;;;------------------------------------------------------------------- 
;;;
;;; Sample code for reading in text files 
;;;

(defgeneric read-text-file (message-pathname &key if-does-not-exist)
  (:documentation "A standard function for reading a text file into a string."))

(defmethod read-text-file ((message-pathname null) &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  "")

(defmethod read-text-file ((message-pathname string) &key if-does-not-exist)
  (read-text-file (pathname message-pathname) :if-does-not-exist if-does-not-exist))

(defmethod read-text-file ((message-pathname pathname) &key (if-does-not-exist :error))
  (with-server-line-buffer ()
    (with-open-file (stream message-pathname :direction :input :if-does-not-exist if-does-not-exist)
      (if stream
          (return-from read-text-file (http::read-delimited-line stream nil 'eof http::*server-line-buffer*))
          (return-from read-text-file "")))))

;;;------------------------------------------------------------------- 
;;;
;;; Sample tokenizing code 
;;;


(defparameter *token-delimiters*
              '(#\Space #\+ #\! #\$ #\& #\* #\, #\. #\/ #\: #\= #\? #\[ #\\ #\] #\_ #\` #\{ #\} #\Linefeed #\Return #\; #\Tab))

(defparameter *skip-chars* (concatenate 'list '(#\' #\( #\)) *token-delimiters*))

(declaim (inline delimiter-p skip-char-p))

(defun delimiter-p (x)
  (member x *token-delimiters*))

(defun skip-char-p (x)
  (member x *skip-chars*))

(defparameter *search-string-skip-chars* '(#\Space))

(defun search-string-skip-char-p (x)
  (member x *search-string-skip-chars*))

(defun string-to-list (text &key (position 0) (length (length text))
                            (skip-function #'search-string-skip-char-p) (delimiter-function #'search-string-skip-char-p))
  (declare (optimize speed))
  (flet ((find-quote (x)
	   (eq x #\")))
    (let ((point position)
	  (result))
      (loop for next = (position-if-not skip-function text :start point :end length)
	    while next
	    while (< next length)
	    do (cond ((eq (aref text next) #\")
		      (setf point (or (1+ (position-if #'find-quote text :start (1+ next) :end length))
				       length)))
		     (t (setf point (or (position-if delimiter-function text :start next :end length)
					length))))
	       (push (subseq text next point) result))
      result)))

(defun string-to-list-ignore-quoting
       (text &key (position 0) (length (length text))
	     (skip-function #'search-string-skip-char-p) (delimiter-function #'search-string-skip-char-p))
  (declare (optimize speed))
  (let ((point position)
	(*search-string-skip-chars* (cons #\" *search-string-skip-chars*))
	(result))
    (loop for next = (position-if-not skip-function text :start point :end length)
	  while next
	  while (< next length)
	  do (setf point (or (position-if delimiter-function text :start next :end length)
				      length))
	     (push (subseq text next point) result))
    result))

#+Genera
(defun no-cons-subseq (sequence start end dest)
  (let ((res-length (- end start)))
    (scl:copy-array-portion sequence start end dest 0 res-length)
    dest))

#+Genera
(declaim (inline no-cons-subseq))

#+Genera
(defun stock-tokenizer (tokenizer text token-cluster initialization-function storage-function output-function
                                  &key storage (position 0) (length (length text)) (store-p t))
  (declare (optimize speed))
  (declare (ignore tokenizer))
  (let ((point position))
    (funcall initialization-function storage)
    (loop for next = (position-if-not #'skip-char-p text :start point :end length)
          while next
          while (< next length)
          do (setf point (or (position-if #'delimiter-p text :start next :end length)
                             length))
             (sys:with-stack-array (word (the fixnum (- point next)) :element-type http::*standard-character-type*)
               (no-cons-subseq text next point word)
               (funcall storage-function storage (%token-intern-returning-index word token-cluster store-p t))))
    (funcall output-function storage)))

(defparameter *token-buffer* (make-array 0 :adjustable t :fill-pointer t :element-type http::*standard-character-type*))

(defun destructive-subseq (string start end destination)
  (setf (fill-pointer destination) 0)
  (loop for n from start below end
        do (vector-push-extend (aref string n) destination)))

#-Genera
(defun stock-tokenizer (tokenizer text token-cluster initialization-function storage-function output-function
                                  &key storage (position 0) (length (length text)) (store-p t))
  (declare (optimize speed))
  (declare (ignore tokenizer))
  (let ((point position))
    (funcall initialization-function storage)
    (loop for next = (position-if-not #'skip-char-p text :start point :end length)
          while next
          while (< next length)
          do (setf point (or (position-if #'delimiter-p text :start next :end length)
                             length))
             (funcall storage-function storage (%token-intern-returning-index (subseq text next point) token-cluster store-p t))))
  (funcall output-function storage))


;;;------------------------------------------------------------------- 
;;;
;;; Loading the universe 
;;;

(defun set-up-universe ()
  (let ((universe (make-document-universe))
        (tokenizer (make-bit-vector-storing-tokenizer "tok" #'stock-tokenizer 9600 9600 5 '(:TEXT))))
    (add-tokenizer universe tokenizer)
    universe))

(set-default-document-universe (set-up-universe))

(defun filter-words (token)
  (if (> (length (token-datum token)) 2)
      t
      nil))

(make-base-computation-type "filter-type" #'filter-words '("token"))

(defparameter filter-computation (make-base-computation "filter" "filter-type" nil))

(defparameter lexical-features-to-use nil)

(defun set-up-lexical-features-to-use ()
  (setf lexical-features-to-use (list (make-lexical-feature-set "lexical-feature-set" filter-computation
                                                                (list (make-token-feature)) nil #'update-token-stats))))

(set-up-lexical-features-to-use)

(defparameter *document-context* (lambda-ir::make-document-context :in-document-universe (lambda-ir::default-document-universe) :in-tokenizer-names '("tok")
                                                                   :in-lexical-features (list lambda-ir::lexical-features-to-use)))
(defun add-url-to-document-universe (pathname url)
  (let* ((document-name (pathname-name pathname))
         (document (make-document document-name `((:PATHNAME ,pathname) (:URL ,url)) '(:TEXT))))
    (catch :already-exists
      (add-document *document-context* document
                    :type-list '(:TEXT)
                    :code-list (list #'read-text-file))
      (lambda-ir::tokenize-and-apply-lexical-features *document-context* document))))

;;;------------------------------------------------------------------- 
;;;
;;; Actual search commands 
;;;


;;; constraint-based search stuff

(defun altavista-style-query-processing (search-string)
  (let ((attractors)
        (repulsors)
	(use-string (if (typep search-string 'list) 
			search-string
			(string-to-list search-string))))
    (loop for item in use-string
          do (if (char-equal (aref item 0) #\-)
                 (push (subseq item 1) repulsors)
                 (push item attractors)))
    (setq attractors (remove "" attractors :test 'string-equal)
	  repulsors (remove "" repulsors :test 'string-equal))
    (cond (repulsors
           `(:andc2 (:and ,@attractors) (:or ,@repulsors)))
          (t `(:and ,@attractors)))))

(defun display-names (context result-vector tags)
  (let ((document-universe (obtain-document-universe context)))
    (flet ((retrieve-document (index current)
             (let ((document (absolute-get-document document-universe index)))
               (if (and document (or (not tags) (degenerate-member tags (absolute-get-document-tags document-universe index))))
                   (push (object-name document) current)))))
      (declare (dynamic-extent #'retrieve-document))
      (map-bit-vector-accumulating result-vector #'retrieve-document nil))))

(defparameter archive-search-constraint
              (make-search-constraint :in-search-arguments '(search-string)
                                      :in-arg-function #'altavista-style-query-processing
                                      :in-limiting-args nil
                                      :in-limiting-function #'display-names))








