;;; -*- Syntax: Ansi-Common-Lisp; Package: html-parser; Base: 10; Mode: lisp -*-

;;; File: html-utilities.lisp
;;; Last edited by smishra on Wed Jul 30 23:22:52 1997

;;; (c) Copyright 1996-97, Sunil Mishra (smishra@cc.gatech.edu)
;;;     All Rights Reserved

(in-package :html-parser)

;;; Miscellaneous utilities for handling HTML structures

(defun attr-val (attr tag-data &optional default-value error-if-not-found)
  (let ((attr-val (find (if (typep attr 'html-name-token)
			    attr
			    (tokenize-name (string attr)))
			(attr-values tag-data)
			:key #'(lambda (val-pair) (name (car val-pair))))))
    (cond (attr-val (cdr attr-val))
          (error-if-not-found
           (error "Attribute ~A unbound in ~A" attr tag-data))
          (t default-value))))

(declaim (inline return-if-not-whitespace))

(defun return-if-not-whitespace (str)
  (and (notevery #'html-whitespace-p str)
       str))

(defun collect-pcdata-seqs (parts collect-alt-p)
  (loop for part in parts
	if (stringp part)
	  collect part
	else if (numberp part)
	  nconc (list '(#\& #\#) (write-to-string part) '(#\;))
	else if (eql (class-name (class-of part)) 'html-entity-token)
	  nconc (list '(#\&) (token-name part) '(#\;))
	else if (typep part 'abstract-tag-instance)
	  if (and collect-alt-p (attr-val #t"ALT" part))
	    collect it
	  else nconc (collect-pcdata-seqs (parts part) collect-alt-p)
	else do (error "Invalid part ~A found in input")))

(defun make-pcdata-string (parts &optional collect-alt-p)
  (let ((str (apply #'concatenate (cons 'string (collect-pcdata-seqs parts collect-alt-p)))))
    (return-if-not-whitespace str)))

;;; The parser definition below shall take a string as argument and
;;; return a parsed structure.

(define-html-parser-context simple-parse-context ()
  :on-open-tag (:any (save it))
  :on-pcdata (save it)
  :on-close-tag (html (exit-context it)))

(define-html-parser simple-parser ()
  :transitions (:start (simple-parse-context))
               (simple-parse-context t :end))

(defun file->string (path)
  (with-open-file (stream path :direction :input :element-type *string-char-type*)
    (stream->string stream)))

(defun stream->string (stream)
  (with-output-to-string (string-stream)
    (loop for char = (read-char stream nil *eof*)
          until (eq char *eof*)
          do (write-char char string-stream))))

(defun ensure-html-parser-tokens-destructive (tree ignore-if)
  (loop for rest-tree on tree
	for item = (car rest-tree)
	if (consp item)
	  do (ensure-html-parser-tokens-destructive item ignore-if)
	else unless (and ignore-if (funcall ignore-if item))
	  if (or (symbolp item) (stringp item))
	    do (setf (car rest-tree) (tokenize-name (string item))))
  tree)

(defun ensure-html-parser-tokens-constructive (tree ignore-if)
  (when tree
    (let ((item (car tree))
	  (rest-tree (cdr tree)))
      (cond ((consp item)
	     (let ((r-item (ensure-html-parser-tokens-constructive item ignore-if))
		   (r-rest-tree (ensure-html-parser-tokens-constructive rest-tree ignore-if)))
	       (if (and (eql r-item item)
			(eql r-rest-tree rest-tree))
		   tree
		   (cons r-item r-rest-tree))))
	    ((and (or (symbolp item) (stringp item))
		  (not (and ignore-if (funcall ignore-if item))))
	     (cons (tokenize-name (string item))
		   (ensure-html-parser-tokens-constructive rest-tree ignore-if)))
	    (t (let ((r-rest-tree (ensure-html-parser-tokens-constructive rest-tree ignore-if)))
		 (if (eql r-rest-tree rest-tree)
		     tree
		     (cons item r-rest-tree))))))))

(defun ensure-html-parser-tokens (tree &key ignore-if (destructive t))
  (if destructive
      (ensure-html-parser-tokens-destructive tree ignore-if)
      (ensure-html-parser-tokens-constructive tree ignore-if)))
