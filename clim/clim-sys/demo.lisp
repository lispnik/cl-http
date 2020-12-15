;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: CL-USER; Base: 10 -*-

;;; Copyright (C) 1994, 1995 Olivier (OBC) all rights reserved.
;;; See copyright notice in MINIPROC file.

(in-package "CL-USER")

(defvar *top-level-input* nil)

(defvar *top-level-output* nil)

;;; Example: Assisted testing of MINIPROC. Let's you use the "Step" button
;;; from Mini Listener interface on ACLPC to step through the test scripts.
;;;
(defun demo-systest (&optional (filename (merge-pathnames "systest.lisp" *minp-directory*)))
  (let ((*top-level-input* *standard-input*)
	(*top-level-output* *standard-output*))
  (clim-sys:make-process #'(lambda ()
			     (minp:with-open-file (stream filename :direction :input)
			       (clim-sys:process-loop
				:with ((eof :eof)
				       read
				       #-ACLPC (cnt 0))
				:until (eql read eof)
				:do 
				#+ACLPC
				(unless (user::listener-value)
				  (when (listen stream)
				    (setq read (read stream nil eof))
				    (user::listener-write read)))
				#-ACLPC
				(let ((*standard-input* *top-level-input*)
				      (*standard-output* *top-level-output*))
				  (when (progn (format *standard-output* "~&Next (return) Stop (:EOF)? ")
					       (setq read (ignore-errors (read-from-string (read-line *standard-input*) nil nil)))
					       (not (eql read :eof)))
				    (format *standard-output* "~&Demo(~a): " (incf cnt))
				    (setq read (read stream nil eof))
				    (print read *standard-output*)
				    (mapc #'print (multiple-value-list (eval read))))))))
			 :name (format nil "Demo using ~a" filename))))

(defun show-systest (&optional (filename (merge-pathnames "systest.lisp" *minp-directory*)))
  (clim-sys:make-process #'(lambda ()
			     (minp:with-open-file (stream filename :direction :input)
			       (clim-sys:process-loop
				:with ((eof :eof) read)
				:until (eql read eof)
				:do (progn (setq read (read stream nil eof))
					   (print read)))))
			 :name (format nil "Reading ~a" filename)))
