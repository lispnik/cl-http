;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: USER; Base: 10 -*-

(in-package "USER")

;;; Minimal update to ACLPC.
;;; Copyright (C) 1994, 1995 Olivier (OBC) all rights reserved.
;;; See copyright notice in CLIM;CLIM-SYS;MINIPROC file.
;;;

;;; This will support the missing readably keyword for now
;;;
(define-function write :redefine (object &rest options)
  (remf options :readably)
  (apply (original-function) object options))

;;; This allows compiling of most format calls containing
;;; the non portable Tilde-Return directive (or Tilde-Linefeed
;;; depending on the OS)...
;;;
(define-function format :redefine (stream string-or-fn &rest args)
  (flet ((search-tilder (string &key (start 0) (end (length string)))
	   (search #.(make-array 2 :element-type 'character :initial-contents '(#\~ #\Linefeed)) string :start2 start :end2 end)))
    (if (and (stringp string-or-fn)
	     (search-tilder string-or-fn))
	(setq string-or-fn
	  (with-output-to-string (out)
	    (loop as start = 0 then end
		as end = (search-tilder string-or-fn :start start)
		if end
		do (write-string string-or-fn out :start start :end (incf end))
		   (write-char #\Return out)
		else do (write-string string-or-fn out :start start)
		     (return)))))
    (apply (original-function) stream string-or-fn args)))

;;; Enable read-line across from non dos file servers!
;;;
(define-function read-line :redefine (&optional stream (eof-error-p t) eof-value recursive-p)
  (case (stream-title stream)
    (#1="Unix Stream"
	(unix-read-line stream eof-error-p eof-value recursive-p))
    (#2="Dos Stream"
	(funcall (original-function) stream eof-error-p eof-value recursive-p))
    (t
     (typecase stream
       (cg:text
	(multiple-value-bind (line dosmode)
	    (unix-read-line stream eof-error-p eof-value recursive-p)
	  (setf (stream-title stream) (if dosmode #2# #1#))
	  line))
       (t
	(funcall (original-function) stream eof-error-p eof-value recursive-p))))))

#+Debug
(defun read-line1 (&optional stream (eof-error-p t) eof-value recursive-p)
  (case (stream-title stream)
    (#1="unix"
	(unix-read-line stream eof-error-p eof-value recursive-p))
    (#2="dos"
	(funcall #'read-line stream eof-error-p eof-value recursive-p))
    (t
     (typecase stream
       (cg:text
	(multiple-value-bind (line dosmode)
	    (unix-read-line stream eof-error-p eof-value recursive-p)
	  (setf (stream-title stream) (if dosmode #2# #1#))
	  line))
       (t
	(funcall #'read-line stream eof-error-p eof-value recursive-p))))))

(defvar *read-line-buffer* (make-string 64))

(defvar *max-line-buffer-size* (ash 1 16))

;;; Raw read-line for UNIX file format
(defun unix-read-line (&optional stream (eof-error-p t) eof-value recursive-p)
  (let (buffers
	(buffer (if recursive-p
		    (make-string (length *read-line-buffer*))
		  *read-line-buffer*))
	(size (length *read-line-buffer*))
	(pos (file-position stream))
	incpos eofp dosmode line)
    (loop (setq incpos (cg:device-nread-string stream buffer size))
      (if (zerop incpos)
	  (if (and eof-error-p (not eofp))
	      (error "Unexpected end of file at position ~a in ~a." pos stream)
	    (return eof-value))
	(let ((eolpos (or (position #\Linefeed buffer :end incpos)
			  (if (setq eofp (cg:device-eof-p stream))
			      incpos))))
	  (cond (eolpos
		 (if (not eofp)
		     (file-position stream (incf pos (1+ eolpos))))
		 (if (> eolpos 0)
		     (when (char= (elt buffer (1- eolpos)) #\Newline)
		       (decf eolpos)
		       (setq dosmode t)))
		 (setf buffer (subseq buffer 0 eolpos))
		 (cond (buffers
			(push buffer buffers)
			(setq line (apply #'concatenate 'string (nreverse buffers))))
		       (t
			(setq line buffer)))
		 (if dosmode
		     (return (values line dosmode))
		   (return line)))
		(t
		 (file-position stream (incf pos incpos))
		 (push buffer buffers)
		 (setq buffer (make-string (setq size (min *max-line-buffer-size* (ash size 1))))))))))))

;;; This patches the macro DESTRUCTURING-BIND on ACLPC
;;; to handle dotted list.

(unless (fboundp 'destructuring-bind-orig)
  (setf (macro-function 'destructuring-bind-orig)
    (macro-function 'destructuring-bind)))

(eval-when (compile eval load)
(defmacro handle-redefinition (&rest body)
  `(handler-bind ((simple-error
		   #'(lambda (c)
		       (if (equal (simple-condition-format-control c)
				  "Attempt to redefine protected function ~s")
			   (progn (format t "~&;;;; Redefining Protected Function,~&~s.~%"
					  (simple-condition-format-arguments c))
				  (continue))
			 c)))
		   (cerror
		   #'(lambda (c)
		       (if (equal (simple-condition-format-control c)
				  "~ ~S has been defined in ~S")
			   (progn (format t "~&;;;; Redefining previously defined Function,~&~s.~%"
					  (simple-condition-format-arguments c))
				  (continue))
			 c))))
     ,@body))
)

;;; This patch exploits the advantages of BUTLAST and is
;;; probably not meaningful to other CL.
;;;
(handle-redefinition
(defmacro destructuring-bind (args form &rest body)
   (let ((#1=#:args args)
	 (#2=#:tail)
	 (#3=#:position))
       (if (setq #2# (rest (last #1#)))
          ;; Simple dotted list tail
	   (setq #1# (butlast #1# 0)
		 #3# (length #1#))
         (if (and (setq #2# (rest (member '&rest #1#)))
                  ;; Simple rest case may not be a cons
                  (null (rest #2#)))
            (setq #3# (position '&rest #1#)
                  #1# (butlast #1# 2)
		  #2# (first #2#))
	   ;; rest case with keys must be a cons
	   (setq #2# nil)))
       (if #2#
          ;; Dotted list
          `(let ((#4=#:form ,form))
	     (destructuring-bind-orig ,#1#
		 (nthcar ,#3# #4#)
	        (let ((,#2# (nthcdr ,#3# #4#)))
		  ,@body)))
          ;; All other cases
          `(destructuring-bind-orig ,#1# ,form
            ,@body))))
)

(defun nthcar (n l)
  (loop for elt in l
     for i from 0 below n
     collect elt))

;;; ANSI style last
;;;
(define-function last :redefine (list &optional n)
  (if n
      (last2 list n)
    ;; Original definition does not do much good
    ;; but if you take it out the compiler will break!
    (funcall (original-function) list)))

(defun last2 (list n)
  (let ((length (loop for l upfrom 0 as rest on list
		    while (consp rest)
		    finally (return l))))
    (decf length n)
    (loop for rest = list then (rest rest)
	when (< (decf length) 0)
	do (return rest))))

(unless (fboundp 'defgeneric-orig)
  (setf (macro-function 'defgeneric-orig)
    (macro-function 'defgeneric)))

;;; Eliminate VALUES declaration from defgeneric
;;; This should not be required since ALCPC is ANSI compliant
;;; in this regard, but the numerous warnings compiling CL-HTTP
;;; get very annoying...
;;;
(handle-redefinition
(defmacro defgeneric (ref . body)
  (let* ((#2=#:ldeclare (member-if #'(lambda (x) (and (consp x)
						      (eql (first x) 'declare)))
				   (rest (member-if #'listp body))))
	 (#1=#:declare (first #2#)))
    (if #1#
	(setf (rest #1#) (delete 'values (rest #1#) :key #'first)))
    (when (and #1# (null (rest #1#)))
      (if (and (null (second #2#))
	       (null (cddr #2#)))
	  (setf (rest (last2 body 2)) nil)
	(setf (first #2#) (second #2#)
	      (rest #2#) (cddr #2#))))
    `(defgeneric-orig ,ref ,@body))))
