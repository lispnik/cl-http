;;; -*- Mode: LISP; Syntax: Common-lisp; Package: MINP; Base: 10; Lowercase: Yes -*-

;;; Portable Mini Process System - 4/94.
;;; Version commented for CL-HTTP 07/5/95.
;;; A few pages of CLOS code provide simple multiple processes scheduling.
;;;
;;; The standard version of this program is available by anonymous FTP
;;; from ftp.ai.mit.edu as part of CL-HTTP (currently /pub/users/jcma/cl-http).
;;; This software comes with examples in a separate SYSTEST file.
;;;
;;; Copyright (C) 1994, 1995 Olivier (OBC) all rights reserved.
;;; No restrictive copyright notice shall be added to this
;;; software by other parties.
;;;
;;; Permission to use, copy, change, and distribute this software for
;;; any purpose and without fee is hereby granted, provided that this
;;; copyright and permission notice appears in all copies and supporting
;;; documentation. There is no guaranty of suitability of this software
;;; for any purpose whatsoever. This software is provided `as is' without
;;; express or implied warranty.
;;;

;;; MINI PROCESS (MINIPROC):
;;; An attempt to provide a minimal multiprocesses utility using
;;; simple closures (where process stacks are normally required).
;;; This was designed to be compatible with CLIM-SYS as the defacto
;;; process interface "standard" [CLIM need not be present for MINIPROC.]
;;;
;;; Several new symbols are exported from CLIM-SYS for convenience:
;;; MAKE-PROCESS-LOOP - portable way to create iterative processes
;;;			a short-hand macro functionally equivalent to:
;;;		(MAKE-PROCESS #'(LAMBDA () (PROCESS-LOOP ...)) :NAME ...)
;;; PROCESS-LOOP      - portable mini non blocking loop macro.
;;; PROCESS-SLEEP
;;; UNWIND-PROCESS    - portable unwind extended for mini processes.
;;; All other Mini Process interface functions are accessed via
;;; the standard CLIM-SYS process interface for portability across
;;; all CL implementations that have (or don't have) CLIM and multiprocessing.
;;;
;;; Only processes created using MAKE-PROCESS, and a lambda using
;;; PROCESS-LOOP, UNWIND-PROCESS and all other CLIM-SYS process interface
;;; functions reasonably (or declared using MAKE-PROCESS-LOOP) will likely
;;; run identically across most of these CL implementations (with or
;;; without MP). See also the documentation for these functions.
;;;
;;; A PROCESS-LOOP expression explicitely declares the current iteration
;;; loop for a process. The computational granularity (i.e. tick) of each
;;; process is the code specified by the :WHILE :DO clauses in
;;; PROCESS-LOOP and/or the granularity of a function (lambda)
;;; placed on the process queue via a call to PROCESS-INTERRUPT.
;;;
;;; PROCESS-SLEEP and other waiting constructs should be carefully
;;; wrapped with and UNWIND-PROCESS clause to insure the rest of
;;; the code will only execute upon completion of the wait reason.
;;; See for example the definition of WITH-LOCK-HELD in MINILOCK.
;;; 
;;; A Mini Scheduler started using (MINP:SCHEDULING) executes in
;;; "parallel" all enabled processes (one tick at a time) and starts
;;; (optionally) a non blocking Mini Listener. Independent process stacks
;;; are replaced  by closures (a "CONTEXT" lambda capturing declared
;;; variables required for a process to execute independently from others)
;;; and unwind completions (declared using UNWIND-PROCESS).
;;;
;;; The magic stops here: CONTEXT closures only comprise the variables
;;; of a process, and have no impact on control flow: the first dead-locking
;;; Mini Process will prevent all others from executing. PROCESS-LOOP is
;;; used in code to prevent deadlocks or for long lasting computations.
;;;
;;; This code was kept simple for clarity. There is little need for
;;; declarations and performance optimizations as most of the work is done
;;; by the compiler generating code for PROCESS-LOOP. Peformance tuning
;;; can be achieved by implementing a specialized scheduler in a separate
;;; application file.
;;;

#+ignore   ;See package file
(defpackage "MINIPROC"
  (:use "COMMON-LISP")
  (:nicknames "MINP")
  ;; Export for CLIM-SYS
  (:export
   "ATOMIC-DECF"
   "ATOMIC-INCF")
  (:export
   "MAKE-PROCESS-LOOP"		;portable way to create processes
   "PROCESS-LOOP"		;non blocking process iteration macro
   "PROCESS-ITERATE"		;method to take-over process
   "UNWIND-PROCESS")		;form to extend unwind-protect to mini process
  (:export
   "MAKE-PROCESS"
   "CURRENT-PROCESS" "ALL-PROCESSES" "FIND-PROCESS-NAMED"
   "PROCESSP" "PROCESS-NAME" "PROCESS-YIELD"
   "PROCESS-WAIT" "PROCESS-WAIT-WITH-TIMEOUT" "PROCESS-SLEEP"
   "DISABLE-PROCESS" "ENABLE-PROCESS" "RESTART-PROCESS"
   "PROCESS-INTERRUPT"
   "PROCESS-STATE" "PROCESS-WHOSTATE" "PROCESS-ACTIVE-P"
   "DESTROY-PROCESS"
   "WITHOUT-SCHEDULING" "MAKE-LOCK" "MAKE-RECURSIVE-LOCK"
   "WITH-LOCK-HELD" "WITH-RECURSIVE-LOCK-HELD")
  (:export
   "ALLOCATE-RESOURCE" "CLEAR-RESOURCE" "DEALLOCATE-RESOURCE" "DEFRESOURCE"
   "MAP-RESOURCE" "USING-RESOURCE")
  ;; Export for MINIPROC convenience
  (:export
   "*SCHEDULING-OPTIONS*"
   "*WARN-IF-NO-PROCESS-LOOP*"
   "*WITHOUT-SCHEDULING-PROCESS-YIELD-P*"
   "CLEAR-PROCESS"
   "DESTROY-ALL-PROCESSES" "DESTROY-CURRENT-PROCESS"
   "DISABLE-LISTENER" "DISABLE-POKER"
   "ENABLE-LISTENER" "ENABLE-POKER"
   "MAKE-CONTEXT"		;make :CONTEXT for process-loop
   "SCHEDULING" "SCHEDULING-EXIT"
   "VARIABLE-INFORMATION"
   "WITH-CURRENT-PROCESS"))

(in-package "MINP")

(defvar *multiprocessing* :MINIMAL)

(defun multiprocessing ()
  *multiprocessing*)

(defvar *all-processes* nil)

#+MINIPROC
(defun all-processes ()
  *all-processes*)

(defun find-process-named (name &key collect (processes (all-processes))
                                (test #'string-equal))
  (let ((l1 (length name))
	result pname l2)
    (or (dolist (proc processes)
	  (setq pname (process-name proc)
		l2 (length pname))
	  (when (and (>= l2 l1)
		     (funcall test name pname :end1 l1 :end2 l1))
	    (if collect
		(push proc result)
	      (return proc))))
	result)))

(defvar *current-process* nil)

#+MINIPROC
(defun current-process ()
  *current-process*)

(defmacro with-current-process ((process) &rest body)
  `(let ((*current-process* ,process))
     ,@body))

#-MINIPROC
(defun check-current-process ()
  (declare (inline current-process))
  (case *current-process*
    (:create nil)
    (t (or *current-process*
	   (current-process)))))

(defun destroy-current-process ()
  (destroy-process (current-process)))

(defun nop ())

;;; Extended to support (make-process nil) to create an empty process
;;; dead on arrival, possibly for use in resource. This was a design
;;; decision to avoid relying on less portable preset mechanisms.
;;; PROCESS-PRESET is not part of CLIM-SYS and is not worth adding here.
;;;
(defmacro make-process (function &key name)
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
  `(let ((#1=#:function (or ,function #'nop))
	 (#2=#:name (or ,name "Anonymous")))
     (if (eql #1# #'nop)
	 #+MINIPROC (make-mini-process nil :name #2#)
	 ;; Must be interruptable for use as resource
	 #-MINIPROC (make-process-mp-interruptable #1# :name #2#)
       #+MINIPROC (make-process-minp #1# :name #2#)
       ;; We assume this process is either throw away on termination
       ;; or interruptable after termination on all MP implementations.
       #-MINIPROC (make-process-mp #1# :name #2#))))

;;; Poorfellow's version of VARIABLE-INFORMATION (not part of ANSI-DRAFT-2).
;;; This must be provided here so that we can determine at
;;; compile time if some variables declared for our process
;;; are specials on the top stack. These SPECIAL variables
;;; will be treated with caution to emulate independent execution
;;; stacks for process instances.
;;;
#+FRANZ-INC
(eval-when (compile load eval)
(defmethod variable-information ((var symbol) &optional env)
  "Since VARIABLE-INFORMATION is not fully implemented here (and we don't
have access to the evaluation environment) there is no way to identify
if local variables have been made special in the local environment
 (versus the top level). We will use the term DYNAMIC later on to
declare such variables. VARIABLE-INFORMATION returns :SPECIAL :CONSTANT
or NIL."
  (if env
      (error "not implemented for environment ~a." env))
  (let ((info (with-output-to-string (stream)
		(describe var stream))))
    (flet ((find-info (string)
	     (let ((loc (search string info :test #'char-equal)))
	       (when loc
		 (incf loc (length string))
		 #+ACLPC (with-input-from-string (stream info :start loc)
			   (and (peek-char #\: stream nil nil)
				(read-char stream nil nil)
				(read stream nil nil)))))))
      (cond ((or (find-info "global variable")
		 (find-info "special variable"))
	     :special)
	    ((or (find-info "constant")
		 #-ACLPC (search "KEYWORD package" info)
		 #+ACLPC (search "package KEYWORD" info))
	     :constant)))))
)

(defvar *iterate-context* nil)

(defmacro make-context ((&key context special dynamic))
  "MAKE-CONTEXT constructs a lambda enclosing a list of declared variables
from the current environment.
CONTEXT is the result of a previous call to MAKE-CONTEXT (recursive)
SPECIAL is a list of variables to be enclosed to form the new context
the resulting context is a lambda of one argument (#:FUNCTION), #:FUNCTION
is called via funcall (with no argument) in the context constructed.
DYNAMIC is used to explicitely declare local variables that have been
declared special locally. DYNAMIC declares a locally defined SPECIAL variable.
Note: PROCESS-LOOP is also extended through the use of :DECLARE
to recognize DYNAMIC as a legal type declaration."
  (flet ((make-lexical-refs (vars)
           (let (bindings lexvar updatebindings)
             (values
              (mapcar #'(lambda (var)
                          (cond ((eql (variable-information var) :CONSTANT)
				 (error "Variable ~s is already a defined constant." var))
				;; check if var is special when its context is formed
				((or (eql (variable-information var) :SPECIAL)     ;May not be (boundp var)
				     (member var dynamic))       ;Dynamically special
				 (setf lexvar (make-symbol (symbol-name var)))
				 (push (list lexvar var) bindings)
				 (push lexvar updatebindings)
				 (push var updatebindings)
				 lexvar)
				(t var)))
                      vars)
              (nreverse bindings)
	      (nreverse updatebindings)))))
    (setf special (union special dynamic))
    (if special
        (multiple-value-bind (lexvars lexbindings updatebindings)
            (make-lexical-refs special)
          (let* ((slambda
		  `(function (lambda ()
			       (funcall #'(lambda ,special
					    (declare (special ,@special))
					    ,(if updatebindings
						 `(unwind-protect
						    (funcall #1=#:function)
						    ,(cons 'setq updatebindings))
					       '(funcall #1#)))

					,@lexvars))))
                 (clambda
		  (if context
		      `(let ((#2=#:calling-context ,context))
			 (function (lambda (#1#)
				   (if #2#
					 (funcall #2# ,slambda)
				       (funcall ,slambda)))))
		    `(function (lambda (#1#)
				 (funcall ,slambda))))))
            (if lexbindings
              `(let ,lexbindings
		 ,clambda)
	      clambda)))
        context)))

;;; As example, this version does not handle global and special variables
;;; from the current stack very well... See testsys file.
;;;
#+ignore
(defmacro make-simple-context (&key context special)
  (if special
    (if context
      `(function (lambda (#1=#:function)
                   (funcall ,context
                            #'(lambda ()
                                (funcall #'(lambda ,special
                                             (declare (special ,@special))
                                             (funcall #1#))
                                         ,@special)))))
      `(function (lambda (#1#)
                   (funcall #'(lambda ,special
                                (declare (special ,@special))
                                (funcall #1#))
                            ,@special))))
    context))

#||
;;; Example: Using Richard Watters MEXP utility:
(write (mexp:macroexpand-all
	'(with-process-context (bargaz :context ctx1
				:special (*foo* *bar* *newspecial*))
	  (gazonk a b c)))
       :length nil :level nil :circle t)
(LET ((BARGAZ
  (LET ((#1=#:CALLING-CONTEXT CTX1))
    #'(LAMBDA (#2=#:FUNCTION)
	(IF #1#
            (FUNCALL #1#
                     #'(LAMBDA NIL
                         (FUNCALL #'(LAMBDA
                                     (*FOO* *BAR* *NEWSPECIAL*)
                                     #3=(DECLARE
                                         (SPECIAL
                                          *FOO*
                                          *BAR*
                                          *NEWSPECIAL*))
                                     (FUNCALL #2#))
                                  *FOO* *BAR* *NEWSPECIAL*)))
           (FUNCALL #'(LAMBDA NIL
                        (FUNCALL #'(LAMBDA
                                    (*FOO* *BAR* *NEWSPECIAL*)
                                    #3#
                                    (FUNCALL #2#))
                                 *FOO* *BAR* *NEWSPECIAL*))))))))
     (GAZONK A B C))
||#

;;; Handy abstractions to create a process contexts.
;;;
(defmacro with-process-context ((context-var &rest options &key context special dynamic) . body)
  (declare (ignore context special dynamic))
  `(let ((,context-var (make-context ,options)))
     ,@body))

(defmacro with-process-loop-context ((context-var &key context with declare) . body)
  (flet ((checkargs (args)
	   (loop for arg? in args
	       as arg = (if (consp arg?) (first arg?) arg?)
	       if (and arg (symbolp arg) (not (keywordp arg)))
	       collect arg
	       else do (error "Unexpected :WITH contents ~a at variable name: ~a"  args arg)))
	 (splitdeclares (exprs)
	   (if (consp exprs)
	       (if (eql (first exprs) 'declare)
		   (flet ((symbol-equalp (word type)
			    (and (symbolp word)
				 (equal (symbol-name word) (symbol-name type)))))
		     (flet ((declare-typep (expr type)
			      (or (if (symbol-equalp (first expr) type)
				      (rest expr)
				    (and (eql (first expr) 'type)
					 (symbol-equalp (second expr) type)
					 (cddr expr))))))
		       (let (vars types specials dynamics)
			 (dolist (decl (rest exprs) (return (values types specials dynamics)))
			   (cond ((setq vars (declare-typep decl 'SPECIAL))
				  (setq specials (append specials vars)))
				 ((setq vars (declare-typep decl 'DYNAMIC))
				  (setq dynamics (append dynamics vars)))
				 (t
				  (setq types (nconc types (list decl)))))))))
		 (error "Unexpected declaration (:DECLARE) ~a." exprs))
	     (if exprs
		 (error "Unexpected :DECLARE contents ~a." exprs)))))
    (multiple-value-bind (types specials dynamically-specials)
	(splitdeclares declare)
      `(let ,(if (checkargs with) with)
	 ,@(let ((ds dynamically-specials))
	     (if (or types ds)
		 `((declare ,@(append types (if ds `((special ,@ds))))))))
         (with-process-context (,context-var :context ,context :special ,specials :dynamic ,dynamically-specials)
	   ,@body)))))

(defmacro with-process-initializer ((init-lambda &key with initially) . body)
  (flet ((initargs (args)
			   (cons 'setq (loop for arg in args
					   when (consp arg)
					   collect (first arg) and collect (second arg)
					   else collect arg and collect nil))))
    `(let ,(if with
	       (list '#1=#:restart))
       ;; This insures that initialization is not
       ;; done twice the first time the process starts
       (let ,(if (or with initially)
		 `((,init-lambda
		    (function (lambda ()
				,@(if with
				      `((if #1#
					    ,(initargs with)
					  (setq #1# t))))
				,@(if initially (list initially)))))))
	 ,@body))))

(defmacro protect ((default) (conditional clause &rest body))
  `(let ((#1=#:condition ,default))
     (unwind-protect (setq #1# ,clause)
       (,conditional #1#
		     ,@body))))

#|| Synopsis:
(process-loop :context bound-to-a-lambda
	      :with ((a 1) b) :initially (foo a)
	      :while (bar b) :do (this a b)
	      :finally (that))
||#

#+MINIPROC
(defmacro process-loop (&key name context with declare initially while until do finally)
  "A mini loop macro to declare MINIPROC process iterations.
See examples in SYSTEST file."
  `(with-process-loop-context (#2=#:context :context ,context :with ,with :declare ,declare)
     (with-process-initializer (#1=#:init-lambda :with ,with :initially ,initially)
       (process-iterate
	*current-process*
	:name ,name
	:context #2#
	:initially ,(if (or with initially)
			'#1#)
	:while ,(cond ((and while until)
		       (error ":WHILE or :UNTIL you must choose one."))
		      (while
		       `(function (lambda () ,while)))
		      (until
		       `(function (lambda () (not ,until)))))	  
	:do ,(if do
		 `(function (lambda () ,do)))
	:finally (function (lambda (#3=#:unwinder)
			     (unwind-protect
				 (progn ,@(if finally (list finally))
					(if #3#
					    ;; Unwind to previous process-loop
					    (funcall #3#)))
			       (process-complete *current-process*)
			       (unless #3#	;Done unwinding process
				 (exhaust-process *current-process*)))))))))

#-MINIPROC	;Standard MP definition
(defmacro process-loop (&key name context with declare initially while until do finally)
  "A mini loop macro to declare process iterations in a way that is portable
to non MP implementations based on MINIPROC. See examples in SYSTEST file."
  `(with-process-loop-context (#2=#:context :context ,context :with ,with :declare ,declare)
     (with-process-initializer (#1=#:initialize :with ,with :initially ,initially)
       (let ((#3=#:name ,name)
	     (#5=#:process (check-current-process)))
	 (if #2#
	     (process-iterate
	      #5#
	      :name #3#
	      :context #2#
	      :initially ,(if (or with initially)
			      '#1#)
	      :while ,(cond ((and while until)
			     (error ":WHILE or :UNTIL you must choose one."))
			    (while
			     `(function (lambda () ,while)))
			    (until
			     `(function (lambda () (not ,until)))))
	      :do ,(if do
		       `(function (lambda ()
				    ,do)))
	      :finally ,(if finally
			    `(function (lambda ()
					 ,finally))))
	   ;; Barebone code generated for efficiency if no context
	   ;; must match the behavior of process-iterate - only faster.
	   (let ((#4=#:lambda
		     #'(lambda ()
			 (let (*iterate-context*)
			   (if (and #3# #5#)
			       (rotatef (process-name #5#) #3#))
			   ,@(if (or with initially)
				 '((funcall #1#)))
			   ,@(if (or while until do finally)
				 `((loop
				     ,@(if while `(while ,while))
				     ,@(if until `(until ,until))
				     ,@(if do `(do ,do))
				     ,@(if finally `(finally ,finally)))))
			   (if (and #3# #5#)	;Restore original name if any
			       (setf (process-name #5#) #3#))))))
	     ,@(if (not (or while do))
		  '((warn-no-loop (or #5# #3#))))
	     (cond (#5#
		    (process-context-interrupt #5# #4#)
		    #5#)
		   (t
		    ;; Note: Don't pay the price of making it "interruptable"
		    (make-process-mp #4# :name #3#)))))))))

(defvar *warn-if-no-process-loop* t
  "Warn user when PROCESS-LOOP is missing a :WHILE or :DO clause.")

(defun warn-no-loop (process)
  (if *warn-if-no-process-loop*
      (warn "Process ~s has no :WHILE or :DO clause." process)))

;;; For standard MP processes
;;;
#-MINIPROC
(defmethod process-iterate ((process t) &key name context initially while do finally)
  (let ((function
	 #'(lambda ()
	     (let ((*iterate-context* context))
	       (if (and name process)
		   (rotatef (process-name process) name))
	       (unwind-protect
		   (progn (if initially
			      (funcall context initially))
			  (cond ((and while do)
				 (loop while (funcall context while)
				     do (funcall context do)))
				(while
				 (loop while (funcall context while)))
				(do
				    (loop do (funcall context do)))))
		 (if finally (funcall context finally))
		 (if (and name process)	;Restore original name if any
		     (setf (process-name process) name))))))
	   procfun)
    (setq procfun (if context
		      #'(lambda ()
			  (funcall context function))
		    function))
    (if (not (or while do))
	(warn-no-loop (or process name)))
    (cond (process
	   (process-context-interrupt process procfun)
	   process)
	  (t
	   ;; Note: Don't pay the price of making it "interruptable"
	   (make-process-mp procfun :name name)))))

;;; Standard MP processes may need interruptions
;;; to occur in the previous context of the process.
;;; As in MINIPROC this should issue a similar warning.
;;;
(defmethod process-context-interrupt ((process t) function)
  (process-interrupt process #'(lambda ()
				 (cond (*iterate-context*
					(funcall *iterate-context* function))
				       (t
					(funcall function))))))

;;; Minimal process definition
;;;
(defclass mini-process ()
  ((context :initform nil :initarg :context :accessor process-context)
   (queue :initform nil :accessor process-queue)
   (initial :initform nil :initarg :function :accessor process-initial)
   (function :initform nil :initarg :function :accessor process-function)
   (state :initform nil :initarg :state :accessor process-state)
   (reason :initform nil :initarg :reason :accessor process-reason)
   (name :initform nil :initarg :name :accessor process-name)
   (locks :initform nil)
   (completions :initform nil)))

(defmethod initialize-instance :after ((process mini-process) &key function &allow-other-keys)
  (with-slots (reason state) process
    (setf state :enabled
	  reason (if function "Running" "Waiting for input")))
  (push process *all-processes*))

(defmacro make-mini-process (function &key name)
  `(make-instance 'mini-process :function ,function :name ,name))

(defmacro make-process-minp (function &key name)
  `(let ((#1=#:process (make-mini-process nil)))
     (let ((#2=#:function #'(lambda ()
			      (with-current-process (#1#)
				(with-slots (function) #1#
				  ;; Prevent unexpected iterations
				  ;; from non MINIPROC lambdas
				  (setf function nil)
				  (funcall ,function))))))
       (initialize-instance #1# :function #2# :name ,name))))

(defmacro make-process-loop (&rest loopkeys &key name context with declare initially while until do finally)
  "MAKE-PROCESS-LOOP (&rest LOOPKEYS) is a short-hand macro equivalent to:
`(MAKE-PROCESS #'(LAMBDA () (PROCESS-LOOP ,@LOOPKEYS)) :NAME ,NAME).
It is the original portable declaration for process iterations.
Make-Process-Loop is intended to provide a uniform declaration
for iterative processes that should behave identically in a multiprocess
system (MP) or in a mini process system (MINPROC)."
  (declare (ignore #-MINIPROC name context with declare initially while until do finally))
  `(with-current-process (#+MINIPROC (make-mini-process nil :name ,name)
				     #-MINIPROC :create)
     (process-loop ,@loopkeys)))

(defmethod print-object ((process mini-process) stream)
  (with-slots (name reason state queue) process
    (format stream "#<MINI-PROCESS ~a [~a~:[, Disabled~;, Enabled~]~:[~;, Queue~]]>"
	    name reason state queue)))

(defmacro unwind-process (form &rest unwinds)
  "Similar to unwind-protect but extended for MINIPROC to provide
an unwind completion within the current mini process if available."
  `(if *current-process*
       (with-slots ((#1=#:completions completions)) *current-process*
	 ;; Install unwinds as a completion
	 (first (push #'(lambda () ,@unwinds) #1#))
	 ,form)
     (unwind-protect ,form ,@unwinds)))

;;; Completion handler for process-loop
;;;
(defmethod process-complete ((process mini-process))
  (with-slots (completions) process
    (cond ((rest completions)
	   (let ((orders (nreverse completions)))
	     #+debug
	     (print (list 'standard-completions orders))
	     (setf completions nil)
	     (mapc #'funcall orders)))
	  (completions
	   (let ((completion (first completions)))
	     (setf completions nil)
	     (funcall completion))))))

;;; Completion handler for process-wait
;;;
(defmethod process-wait-complete ((process mini-process))
  (declare (inline process-complete))
  (with-slots (queue completions) process
    (push #'(lambda ()
	      (process-complete process))
	  queue)))

(defun compose-contexts (outer-context context)
  (if context
      #'(lambda (#1=#:function)
	  (funcall outer-context
		   (funcall context #1#)))
    outer-context))

(defmethod process-iterate ((process mini-process) &key name context initially while do finally)
  (with-slots ((call-context context) initial function state reason (proc-name name) completions) process
    (let (unwinder)
      (if (or call-context function)
	  ;; Make an unwinder to go back to original process context
	  ;; and iteration loop when this new process iteration completes.
	  (let ((ocontext call-context)
		(oinitial initial)
		(ofunction function)
		(ostate state)
		(oreason reason)
		(oname name)
		(ocompletions completions))
	    (setf unwinder
	      #'(lambda ()
		  (setf call-context ocontext initial oinitial function ofunction
			state ostate reason oreason name oname completions ocompletions)))))
      (if (not (or while do))
	  (warn-no-loop process))
      (if name
	  (setf proc-name name))
      (cond ((null call-context)
	     (setf call-context context))
	    (t
	     (setf call-context (compose-contexts call-context context))))
      (setf function #'(lambda ()
			 (if initially
			     (progn (setf reason "Initializing")
				    (funcall initially)
				    (setf reason "Initialized")))
			 (if do
			     (setf reason "Running"))
			 (if (or while do)
			     (setf function
			       #'(lambda ()
				   (if (if while (funcall while) t)
				       (if do (funcall do))
				     (progn
				       (setf state nil)
				       (if while
					   (if finally
					       (progn
						 (setf state nil
						       reason "Finalizing")
						 (setf reason "Terminated")
						 (funcall finally unwinder))
					     (setf reason "Terminated")))))))
			   (if finally
			       (setf function
				 #'(lambda ()
				     (setf state nil
					   reason "Finalizing")
				     (setf reason "Exhausted")
				     (funcall finally unwinder)))
			     (setf state nil
				   reason "Exhausted"))))
	    initial function
	    state :enabled)))
  process)

(defmethod process-whostate ((process mini-process))
  (process-reason process))

(defmethod (setf process-whostate) (val (process mini-process))
  (setf (process-reason process) val))

(defmethod process-active-p ((process mini-process))
  (and (process-state process) t))

#+MINIPROC
(defmethod processp ((process t))
  nil)

(defmethod processp ((process mini-process))
  *multiprocessing*)

#+MINIPROC
(defun process-sleep (timeout)
  (if (numberp timeout)
      (process-wait-with-timeout "Sleeping" timeout #'nop)))

(defvar *process-queue-fifo* nil)    ;Default LIFO

(defmethod process-interrupt ((process mini-process) lambda)
  (with-slots (queue) process
    (if *process-queue-fifo*
	(setf queue (nconc queue (list lambda)))
      ;; LIFO
      (push lambda queue)))
  nil)

(defmethod exhaust-process ((process mini-process))
  (setq *all-processes* (delete process *all-processes* :count 1))
  (with-slots (queue function state reason locks completions) process
    (setf queue nil function nil reason "Exhausted" state nil locks nil completions nil)))

(defmethod destroy-process ((process mini-process))
  (setq *all-processes* (delete process *all-processes* :count 1))
  (with-slots (context queue initial function state reason locks completions) process
      (setf context nil queue nil initial nil function nil state nil reason "Destroyed" locks nil completions nil)))

;; Clear name and state as well
(defmethod clear-process ((process mini-process))
  (with-slots (context queue initial function state reason name locks completions) process
    (setf context nil queue nil initial nil function nil state nil reason nil name nil locks nil completions nil)))

(defmethod enable-process ((process mini-process))
  (with-slots (state reason) process
    (setf reason "Enabled" state :enabled))
  nil)

(defmethod disable-process ((process mini-process))
  (with-slots (state reason) process
    (setf reason "Disabled" state nil)))

(defmethod restart-process ((process mini-process))
  (with-slots (function state initial reason) process
    (if state
	(setf function initial state (if initial t) reason "Restart")
      (setf function #'(lambda ()
			 (process-wait
			  "Restarting yet disabled..."
			  #'(lambda ()
			      (when state
				(setf function initial state (if initial t)
				      reason "Restart")))))
	    reason "Restarting"))
    ;; Resurect defunct process
    (pushnew process *all-processes*)
    (if state process)))

#+MINIPROC
(defun process-wait (reason predicate)
  (process-wait-with-timeout reason t predicate))

;;; By default this waiting applies to the process function only,
;;; during wait, the process is still responsive to PROCESS-INTERRUPT.
;;; To change this default behavior change the variable
;;; *WAIT-ON-PROCESS-QUEUE-P*.
;;;
#+MINIPROC
(defun process-wait-with-timeout (reason timeout predicate)
  (with-slots ((areason reason) function state completions) *current-process*
    (let ((oreason areason)
	  (ofunction function)
	  (ostate state)
	  (ocompletions completions))
      (setf areason reason)
      (if (not (eql timeout t))
	  (if (numberp timeout)
	      (let ((target-time (+ (get-internal-real-time)
				    (* timeout internal-time-units-per-second)))
		    out-of-time)
		 (setf out-of-time #'(lambda () (>= (get-internal-real-time)
						    target-time)))
		 (setf function #'(lambda ()
				    (protect (t)
				      (when (or (funcall predicate)
						(funcall out-of-time))
					(setf function ofunction
					      areason oreason
					      state ostate
					      completions ocompletions))))
		       state :wait))
	    (error "unexpected timeout argument ~a." timeout))
	(setf function #'(lambda ()
			   (protect (t)
			    (when (funcall predicate)
			      (setf function ofunction
				    areason oreason
				    state ostate
				    completions ocompletions)
			      (process-wait-complete *current-process*))))
	      state :wait))))
  nil)

(defvar *without-scheduling-process-yield-p* t
  "This variable can be use to disable process-yield activity
in the body of without-scheduling (value NIL). The default (value T)
allows explicit calls to process-yield to actually activate the scheduler
from within without-scheduling for MINIPROC processes.
This seems to be the standard way for MP.
Note: If you change this variable all code must be recompiled.")

;;; This actually protects against user code calling process-yield
;;;
#+MINIPROC
(eval-when (compile load eval)
(defmacro without-scheduling (&rest body)
  (if *without-scheduling-process-yield-p*
      `(progn ,@body)
    `(let (*process-yield*)
       (declare (special *process-yield*))
       ,@body)))
)

;;; Sleeping time adjustments - Tune this to leave reasonable CPU
;;; time for non CL processes. This is only given as an example
;;; to get started, it is not intended to be an efficient or
;;; even useful way to give-up CPU time for processes outside
;;; the mini process world...
;;;
(defparameter *scheduling-options*
    ;; 0 - sleep time per scheduler cycle (~30ms is reasonable since we
    ;;     spend similar time polling).
    ;; 1 - length in mini process ticks of one scheduler cycle.
    ;; 2 - long sleep time chunk in seconds.
    ;; 3 - number of ticks required for one long sleep chunck.
    ;;
    (or #+KCL #1A(.03 50 1 400 0 0)
	#+UNIX #1A(.03 50 .5 400 0 0)
	#-UNIX #1A(.03 100 .2 1000 0 0)))

(defun scheduling-sleep-time ()
  (cond ((> (incf (elt *scheduling-options* 4))
	    (elt *scheduling-options* 1))
	 (sleep (elt *scheduling-options* 0))
	 (scheduling-ticks :wait)
	 (setf (elt *scheduling-options* 4) 0))
	((> (elt *scheduling-options* 5)
	    (elt *scheduling-options* 3))
	 (sleep (elt *scheduling-options* 2))
	 (setf (elt *scheduling-options* 5) 0))))

(defun scheduling-ticks (state)
  (incf (elt *scheduling-options* 5)
	(case state (:wait 10) (t 1))))

(defvar *process-yield* nil)

#+MINIPROC
(defun process-yield ()
  #+Common-Graphics
  (cg:process-pending-events)
  (when *process-yield*
    (let (*process-yield*) ;; Prevent recursion
      (declare (special *process-yield*))
      (mapc #'process-tick *all-processes*))))

(defun process-tick (process)
  (with-current-process (process)
    (let ((busy (process-queue process))
	  (state (process-state process)))
      (if busy
	  (with-simple-restart
	      (abort "Return from error in queue of Mini Proccess ~a."
		     (process-name process))
	    (process-queue-call process)))
      (when state
	(scheduling-ticks state)
	(with-slots (context function) process
	  (if function
	      (with-simple-restart
		  (abort "Return from error in Mini Proccess ~a."
			 (process-name process))
		(if context
		    (funcall context function)
		  (funcall function))))))))
    (scheduling-sleep-time))

(defparameter *wait-on-process-queue-p* nil)

(defun process-queue-call (proc)
  (with-slots (context queue state) proc
    (let ((call (pop queue)))
      (when (if call (if *wait-on-process-queue-p*
			  (not (eql state :wait))
                        t))
	(scheduling-ticks :queue)
	(if context
	    (funcall context call)
	  (funcall call))))))

(defvar *top-level-input* nil)

(defvar *top-level-output* nil)

(defvar *scheduling* nil)

(defun scheduling (&key initform (listener t))
  (if *scheduling*
      (error "Cannot start the Mini Scheduler recursively")
    (unwind-protect
	(progn
	  (setq *scheduling* t)
	  (setq *top-level-input* *standard-input*
		*top-level-output* *standard-output*)
	  (when listener
	    (enable-listener))
	  (when initform
	    (eval initform))
	  ;; Do the work
	  (with-simple-restart (abort "Ignore error and exit the Mini Scheduler.")
	    (catch 'scheduling-exit
	      (let ((*process-yield* t))
		(declare (special *process-yield*))
		(loop while (and *all-processes* *scheduling*)
		    do (process-yield))))))
      (setq *scheduling* nil))))

;;; You can use this to exit the process from
;;; outside the scheduling loop.
;;;
(defun scheduling-exit ()
  (setq *scheduling* nil))

(defun destroy-all-processes ()
  (loop while *all-processes*
     do (destroy-process (first *all-processes*))))

;;; Add a mini listener so you can do minimal debugging
;;; from within the scheduler.
;;;
(defvar *mini-listener* nil)

;;; The default is to ignore errors. This prevents the first error
;;; in one process to stop all other Mini Processes. A better approach
;;; will provide a non blocking Mini Debugger upon error.
;;;
(defvar *ignore-errors* t)

(defun enable-listener (&optional (listener "Mini Lisp Listener"))
  (if *mini-listener*
      (restart-process *mini-listener*)
    (setq *mini-listener* (or (let ((found (find-process-named listener)))
                                   (when found
                                      (restart-process found)
                                      found))
				(make-mini-listener :name listener)))))

(defun disable-listener ()
  (if *mini-listener*
      (setq *all-processes* (delete *mini-listener* *all-processes*))))

;;; LISTEN often block on certain implementations.
;;; One may have to enter a number and #\Return to release LISTEN.
(defun listen-stream (stream)
  #+Allegro
  (unless (typep stream 'EXCL::BACKGROUND-STREAM)
    (listen stream))
  #+(and ACLPC (not ACL3.0))
  (cl:peek-char nil stream)	;listen causes fatal error!
  #+ACL3.0
  (peek-char nil stream)	;listen not fixed yet.
  #-(or Allegro ACLPC)
  (listen stream))

#+MINIPROC
(defun make-mini-listener (&key name (count 0))
  (make-process-loop
   :name (or name "Mini Lisp Listener")
   :with ((next t)
	  #-ACLPC
	  (*standard-input* *top-level-input*)
	  #-ACLPC
	  (*standard-output* *top-level-output*))
   #-ACLPC :declare #-ACLPC (declare (special *standard-input* *standard-output*))
   :do (progn
	 (when next
	   (setf next nil)
	   (format *standard-output* "~&+~:(~a~)(~a): "
		   (or (first (package-nicknames *package*))
		       (package-name *package*))
		   (incf count)))
	 (finish-output *standard-output*)
	 (if (listen-stream *standard-input*)
	   (let ((command (read *standard-input* nil nil)))
	     (clear-input *standard-input*)
	     (setf next t)
	     (case command
	       ((:help :?)
		(format *standard-output* "~&Type :END to suspend the mini listener
     :EXIT to exit the mini scheduler."))
	       (:end
		(disable-listener))
	       (:exit
		(throw 'scheduling-exit command))
	       (t
		(multiple-value-bind (results error args)
		    (if *ignore-errors*
			(ignore-errors (multiple-value-list (eval command)))
		      (multiple-value-list (eval command)))
		  (when error
		    (warn "The following ERROR was ignored.~%Type :HELP to get help." error args)
		    (describe error)
		    (enable-listener))
		  (loop for result in results
		      do #+ACLPC (terpri *standard-output*)	;else fatal error!
			 (write result :stream *standard-output*
				:circle t :pretty t)
			 #-ACLPC (terpri *standard-output*))))))))))

;;; Example:
;;;
(defvar *progress-poker* nil)

#+MINIPROC
(defun enable-poker (&optional (poker "Progress Poker"))
  (if *progress-poker*
      (pushnew *progress-poker* *all-processes*)
    (setq *progress-poker* (or (find-process-named poker)
			       (make-process-loop
				:name poker
				:do (progn (write-char #\. *top-level-output*)
					   (finish-output *top-level-output*)))))))

(defun disable-poker ()
  (if *progress-poker*
      (setq *all-processes* (delete *progress-poker* *all-processes*))))

;;; Implementation specific patches
#+CCL-2
(defmacro with-simple-restart ((restart-name format-string
				&rest format-arguments)
			       &body forms)
  `(restart-case (progn ,@forms)
     (,restart-name ()
	 :report
	   (lambda (stream)
	     (format stream ,format-string ,@format-arguments))
       (values nil t))))



