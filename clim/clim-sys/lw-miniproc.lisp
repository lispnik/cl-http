;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: MINP; Base: 10 -*-

;;; Single definition needed for LispWorks, extracted from miniproc.lisp

(in-package "MINP")

(defun make-process (function &key name &allow-other-keys)
  "When using this macro it is assumed the user provided FUNCTION
is written in a portable way for all platforms including non
MP implementations based on MINIPROC. For example FUNCTION can
contain a PROCESS-WAIT form or a PROCESS-LOOP form but should
NOT include tight and time consumming LOOP clauses unless it
is implementing a MINP:SCHEDULER take-over and comprises sufficient
calls to PROCESS-YIELD. This standard CLIM-SYS:MAKE-PROCESS standard
definition is extended to support a NIL function argument this
is used to create a minimum empty process that can be reused
via an interrupt or using the WITH-CURRENT-PROCESS form. This may
be used to provide process resources -- such processes should not be
destroyed to insure they can later be interrupted and restarted."
  (unless name
    (setq name "Anonymous"))
  (cond ((null function)
	 #+MINIPROC (make-mini-process nil :name name)
	 ;; Must be interruptable for use as resource
	 #-MINIPROC (make-process-mp-interruptable #'nop :name name))
	(t
	 #+MINIPROC (make-process-minp function :name name)
	 ;; We assume this process is either throw away on termination
	 ;; or interruptable after termination on all MP implementations.
	 #-MINIPROC (make-process-mp function :name name))))

(defun nop ())
 
(defmacro unwind-process (form &rest unwinds)
  "Similar to unwind-protect but extended for MINIPROC to provide
an unwind completion within the current mini process if available."
  `(unwind-protect ,form ,@unwinds))
