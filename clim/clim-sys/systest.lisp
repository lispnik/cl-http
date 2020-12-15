;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: CL-USER; Base: 10 -*-

;;; Copyright (C) 1994, 1995 Olivier (OBC) all rights reserved.
;;; See copyright notice in MINIPROC file.

(in-package "CL-USER")

#+ACLPC
(princ ";;; Use STEP to step through each expression from SYSTEST demo.
;;; Use RUN to try other commands as well. You will need to STEP
;;; through the entire SYSTEST demo file before the demo process
;;; completes or you can terminate the last process started using:
;;; (CLIM-SYS:DESTROY-PROCESS (FIRST (CLIM-SYS:ALL-PROCESSES)))
;;; Try not to delete the Mini Listener process...
;;;")

;;; Here is the actual code
;;; to read and evaluate one at a time
;;;
(princ ";;; This is a test of MINI PROCESSES examples running in CLIM-SYS
;;; [CLIM is NOT loaded] only the portable CLIM-SYS system definition
;;; is loaded in a generic CL implementation. These examples are not
;;; fancy but useful to verify that the MINIPROC version of CLIM-SYS
;;; works on a given implementation; this also illustrate how to use
;;; Mini Process declarations properly MAKE-PROCESS-LOOP is only
;;; used as a short-hand notation for MAKE-PROCESS #'PROCESS-LOOP-lambda
;;;
;;; Note: These test cases can be useful to try on all platform as
;;; they have uncovered fatal bugs in supposedly long proven exiting
;;; MP implementations.
;;;")

(setq xxx (clim-sys:make-process nil))
xxx
(minp:with-current-process (xxx)
	     (clim-sys:process-loop :with ((cnt 10))
				    :while (plusp (decf cnt))
				    :do (and (print cnt) (clim-sys:process-sleep 1))))
(clim-sys:enable-process xxx)
(clim-sys:restart-process xxx)

(defun test0 ()
  (clim-sys:make-process #'(lambda ()
			     (clim-sys:process-loop
			      :with ((cnt 7))
			      :while (plusp (decf cnt))
			      :do (and (print cnt) (clim-sys:process-sleep 1))))))
(compile 'test0)
(setq xxx (test0))
(setq xxx (clim-sys:make-process nil :name "nobody there"))
(clim-sys:all-processes)
xxx
(setq xxx (clim-sys:make-process-loop
	   :name "hello there"
	   :with ((count 3))
	   :initially (format t "~&~a" (clim-sys:process-name
					(clim-sys:current-process)))
	   :while (>= (decf count) 0)
	   :do (and (write-char #\.) (clim-sys:process-sleep 1))
	   :finally (format t " done.")))
(clim-sys:all-processes)
xxx
(clim-sys:process-interrupt xxx #'(lambda () (format t "~&~a is still going." (clim-sys:current-process))))
(clim-sys:enable-process xxx)
(clim-sys:restart-process xxx)
xxx
(defun test1 ()
  (declare (special yyy))
  (format t "~&~a" yyy))
(setq xxx (clim-sys:make-process-loop
	   :name "New Hello There"
	   :with ((count 3) (yyy "YYY is a Special!"))
	   :declare (declare (special yyy) (fixnum count))
	   :initially (format t "~&~a" (clim-sys:process-name (clim-sys:current-process)))
	   :while (progn (test1)
			 (>= (decf count) 0))
	   :do (progn 
		 (write-char #\.)
		 (clim-sys:process-sleep 4))
	   :finally (format t " done.")))
(clim-sys:all-processes)
(defun testdcl ()
  (clim-sys:make-process-loop
   :name "New Hello There"
   :with ((count 3) (yyy "YYY is a Special!")
		    (sleeptime 3.5))
   :declare (declare (special yyy) (fixnum count) (single-float sleeptime))
   :initially (format t "~&~a" (clim-sys:process-name (clim-sys:current-process)))
   :while (progn (test1)
		 (>= (decf count) 0))
   :do (progn 
	 (write-char #\.)
	 (clim-sys:process-sleep sleeptime))
   :finally (format t " done.")))
(compile 'testdcl)
(testdcl)
(clim-sys:make-process-loop
 :name "New Hello There"
 :with ((count 3) (yyy "YYY is a Special!"))
 :declare (declare (special yyy) (fixnum count))
 :initially (format t "~&~a" (clim-sys:process-name (clim-sys:current-process)))
 :while (progn (test1)
	       (>= (decf count) 0))
 :do (progn 
       (write-char #\.)
       (clim-sys:process-sleep 4))
 :finally (format t " done."))
(clim-sys:make-process-loop
 :name "New Hello There"
 :with ((count 3) (yyy "YYY is a Special!"))
 :declare (declare (special yyy) (fixnum count))
 :initially (format t "~&~a" (clim-sys:process-name (clim-sys:current-process)))
 :until (progn (test1)
	       (< (decf count) 0))
 :do (progn 
       (write-char #\.)
       (clim-sys:process-sleep 4))
 :finally (format t " done."))
(let ((count 3) (yyy "YYY is a Special!"))
  (declare (fixnum count))
  (clim-sys:make-process-loop
   :name "New Hello There"
   :declare (declare (special yyy))
   :initially (format t "~&~a" (clim-sys:process-name (clim-sys:current-process)))
   :until (progn (test1)
		 (< (decf count) 0))
   :do (progn 
	 (write-char #\.)
	 (clim-sys:process-sleep 4))
   :finally (format t " done.")))
(let ((count 3) (yyy "YYY is a Special!"))
  (let ((context1 (minp:make-context (:special (yyy) ))))
    (clim-sys:make-process-loop
     :name "New Hello There"
     :context context1
     :initially (format t "~&~a" (clim-sys:process-name (clim-sys:current-process)))
     :until (progn (test1)
		   (< (decf count) 0))
     :do (progn 
	   (write-char #\.)
	   (clim-sys:process-sleep 4))
     :finally (format t " done."))))
(defun test2 (x) (declare (special zzz)) (format t zzz x))
(let ((count 3) (yyy "YYY is a Special!"))
  (let ((context1 (minp:make-context (:special (yyy) ))))
    (clim-sys:make-process-loop
     :name "New Hello There"
     :context context1
     :with ((name 'ZZZ)
	    (zzz "Another Special ~a."))
     :declare (declare (special zzz) (symbol name))
     :initially (format t "~&~a" (clim-sys:process-name (clim-sys:current-process)))
     :until (progn (test1) (test2 name)
		   (< (decf count) 0))
     :do (progn 
	   (write-char #\.)
	   (clim-sys:process-sleep 4))
     :finally (format t " done."))))
(defvar *foo* 123)
(defvar *bar*)
(defun test3 ()
	     (declare (special *foo* *bar*))
	     (format t "~&foo's value is ~s, bar's value is ~s"
		     *foo* (if (boundp '*bar*) *bar* '#:unbound)))
(setq xxx (clim-sys:make-process-loop
	   :name "Clobering top stack special variables"
	   :with ((*foo* 'alpha) (*bar* 'omega))
	   :initially (test3)))
(progn (clim-sys:enable-process xxx)
		  (clim-sys:restart-process xxx))
*foo*
*bar*
(setq *foo* 123)
(makunbound '*bar*)

(progn (clim-sys:enable-process xxx)
		  (clim-sys:restart-process xxx))
*foo*
(boundp '*bar*)
(setq xxx (clim-sys:make-process-loop
	   :name "Shadowing top stack special variables"
	   :with ((*foo* 'alpha) (*bar* 'omega))
	   :declare (declare (special *foo* *bar*))
	   :initially (test3)))

(defun proctest (name value)
  (clim-sys:make-process-loop
   :name name
   :with ((count 19) (*foo* 'alpha) (*bar* 'omega))
   :declare (declare (special *foo* *bar*))
   :initially (progn (test3) (setq *foo* value) (test3))
   :while (> count 0)
   :do (progn (setq *bar* (decf count)) (clim-sys:process-sleep 2) (test3))
   :finally (test3)))
(compile 'proctest)

(proctest "Proc-1" 'ONE)
(proctest "Proc-2" 'TWO)
(minp:variable-information '*foo*)
*foo*
(minp:variable-information '*bar*)
(boundp '*bar*)
(defun ztest3 ()
 (declare (special *zfoo* *zbar*))
 (format t "~&foo's value is ~s, bar's value is ~s"
           *zfoo* (if (boundp '*zbar*) *zbar* '#:unbound)))
(compile 'ztest3)
(defun zproctest (name value)
  (clim-sys:make-process-loop
   :name name
   :with ((count 19) (*zfoo* 'alpha) (*zbar* 'omega))
   :declare (declare (special *zfoo* *zbar*))
   :initially (progn (ztest3) (setq *zfoo* value) (ztest3))
   :while (> count 0)
   :do (progn (setq *zbar* (decf count)) (clim-sys:process-sleep 2) (ztest3))
   :finally (ztest3)))
(compile 'zproctest)
(zproctest "Zproc-1" 'ONE)
(zproctest "Zproc-2" 'TWO)
(minp:variable-information '*zfoo*)(boundp '*zfoo*)
(minp:variable-information '*zbar*)
(boundp '*zbar*)
(defun test3 ()
 (declare (special *foo* *bar*))
 (format t "~&BEFORE: foo's value is ~s, bar's value is ~s"
           *foo* (if (boundp '*bar*) *bar* '#:unbound)))
(defun test4 ()
 (declare (special *foo* *bar*))
 (setq *foo* (rest *foo*))
 (setq *bar* (1+ *bar*)))
(defun test5 ()
 (declare (special *foo* *bar*))
 (format t "~&AFTER: foo's value is ~s, bar's value is ~s"
           *foo* (if (boundp '*bar*) *bar* '#:unbound)))
(defun proctest0 (name value)
  (clim-sys:make-process-loop
   :name name
   :with ((count 19) (*foo* 'alpha) (*bar* 35))
   :declare (declare (special *foo* *bar*))
   :initially (progn (test3) (setq *foo* value) (test5))
   :while (> count 0)
   :do (progn (test3) (decf count) (test4) (clim-sys:process-sleep 2) (test5))
   :finally (test5)))
(compile 'proctest0)
(proctest0 "Final-1" '(a b c d e f g h i j k l m n o))
(proctest0 "Final-2" '(1 2 3 4 5 6 7 8 9 0 1 2 3 4 5))
(clim-sys:all-processes)
:help

(princ ";;; New tests added to check MINILOCK implementations.")
(setq x (clim-sys:make-lock "testing"))
(clim-sys:with-lock-held (x "waiting for Z")
	    (describe x)
	    (clim-sys:process-loop :with ((n 7))
				:while (plusp (decf n))
				:do (progn (print n) (clim-sys:process-sleep 2))))
(describe x)
(setq x (clim-sys:make-lock "butter" #+MINIPROC 1))
(setq y (clim-sys:make-lock "knife" #+MINIPROC 2))
(clim-sys:with-lock-held (x "waiting for the butter")
	     (describe x)
	     (clim-sys:with-lock-held (y "waiting for the knife")
	       (describe y)
	       (clim-sys:process-loop :with ((n 7))
				   :while (plusp (decf n))
				   :do (progn (print n) (clim-sys:process-sleep 2)))))
(describe x)
(describe y)
(clim-sys:with-lock-held (y "waiting for the knife")
	     (describe y)
	     (clim-sys:with-lock-held (x "waiting for the butter")
	       (describe x) (clim-sys:process-loop :with ((n 7))
						:while (plusp (decf n))
						:do (progn (print n) (clim-sys:process-sleep 2)))))

;;; REASON TO STOP BEFORE FILE TOO LONG FOR ACLPC
;;;
:EOF

	    
;;; This is a trace of MINI PROCESSES examples running in CLIM-SYS
;;; [CLIM is NOT loaded] only the portable CLIM-SYS system definition
;;; is loaded in a generic CL implementation. These examples are not
;;; fancy but useful to verify that the MINIPROC version of CLIM-SYS
;;; works on a given implementation; this also illustrate how to use
;;; Mini Process declarations properly MAKE-PROCESS-LOOP is only
;;; used as a short-hand notation for MAKE-PROCESS #'PROCESS-LOOP-lambda
;;;
;;; Note: These test cases can be useful to try on all platform as
;;; they have uncovered fatal bugs in supposedly long proven exiting
;;; MP implementations.
;;;

#||
USER(46): #+MINIPROC (minp:scheduling)	;Where MINPROC is present
+User(01): (setq xxx (clim-sys:make-process nil))
#<MINI-PROCESS Anonymous [Waiting for input, Enabled]>
+User(02): xxx
#<MINI-PROCESS NIL [Exhausted, Disabled]>
+User(03): (minp:with-current-process (xxx)
	     (clim-sys:process-loop :with ((cnt 10))
				    :while (plusp (decf cnt))
				    :do (and (print cnt) (clim-sys:process-sleep 1))))
#<MINI-PROCESS NIL [Exhausted, Enabled]>
+User(04): (clim-sys:enable-process xxx)
NIL
+User(05): (clim-sys:restart-process xxx)
#<MINI-PROCESS NIL [Restart, Enabled]>
+User(06): 
9 
8 
7 
6 
5 
4 
3 
2 
1 nil
+User(07):
(defun test0 ()
  (clim-sys:make-process #'(lambda ()
			     (clim-sys:process-loop
			      :with ((cnt 7))
			      :while (plusp (decf cnt))
			      :do (and (print cnt) (clim-sys:process-sleep 1))))))
NIL
+User(8): (compile 'test0)
TEST0
NIL
NIL
+User(9): (setq xxx (test0))
#<MINI-PROCESS Anonymous [Running, Enabled]>
+User(10): 
6 
5 
4 
3 
2 
1 

+User(1): (setq xxx (clim-sys:make-process nil :name "nobody there"))
#<MINI-PROCESS nobody there [Waiting for input, Enabled]>

+User(2): (clim-sys:all-processes)
(#<MINI-PROCESS Mini Lisp Listener [Running, Enabled]>)

+User(3): xxx
#<MINI-PROCESS nobody there [Exhausted, Disabled]>

+User(4):
(setq xxx (clim-sys:make-process-loop
	   :name "hello there"
	   :with ((count 3))
	   :initially (format t "~&~a" (clim-sys:process-name
					(clim-sys:current-process)))
	   :while (>= (decf count) 0)
	   :do (and (write-char #\.) (clim-sys:process-sleep 1))
	   :finally (format t " done.")))
#<MINI-PROCESS hello there [Waiting for input, Enabled]>

hello there
+User(5): ... done.

(clim-sys:all-processes)
(#<MINI-PROCESS Mini Lisp Listener [Running, Enabled]>)

+User(6): xxx
#<MINI-PROCESS hello there [Terminated, Disabled]>

+User(7): (clim-sys:process-interrupt xxx #'(lambda () (format t "~&~a is still going." (clim-sys:current-process))))
NIL

+User(8): (clim-sys:enable-process xxx)
NIL
+User(9): (clim-sys:restart-process xxx)
#<MINI-PROCESS hello there [Restart, Enabled, Queue]>
#<MINI-PROCESS hello there [Restart, Enabled]> is still going.
hello there
+User(10): xxx
#<MINI-PROCESS hello there [Running, Enabled]>
.
+User(11): .. done.

(defun test1 ()
  (declare (special yyy))
  (format t "~&~a" yyy))
TEST1

+User(12):
(setq xxx (clim-sys:make-process-loop
	   :name "New Hello There"
	   :with ((count 3) (yyy "YYY is a Special!"))
	   :declare (declare (special yyy) (fixnum count))
	   :initially (format t "~&~a" (clim-sys:process-name (clim-sys:current-process)))
	   :while (progn (test1)
			 (>= (decf count) 0))
	   :do (progn 
		 (write-char #\.)
		 (clim-sys:process-sleep 4))
	   :finally (format t " done.")))
#<MINI-PROCESS New Hello There [Waiting for input, Enabled]>

New Hello There
+User(13): 
YYY is a Special!.
YYY is a Special!.
YYY is a Special!.
YYY is a Special! done.

(clim-sys:all-processes)
(#<MINI-PROCESS Mini Lisp Listener [Running, Enabled]>)

+User(14):
(defun testdcl ()
  (clim-sys:make-process-loop
   :name "New Hello There"
   :with ((count 3) (yyy "YYY is a Special!")
		    (sleeptime 3.5))
   :declare (declare (special yyy) (fixnum count) (single-float sleeptime))
   :initially (format t "~&~a" (clim-sys:process-name (clim-sys:current-process)))
   :while (progn (test1)
		 (>= (decf count) 0))
   :do (progn 
	 (write-char #\.)
	 (clim-sys:process-sleep sleeptime))
   :finally (format t " done.")))
TESTDCL

+User(20): (compile 'testdcl)
TESTDCL
T
NIL

+User(24): (testdcl)
#<MINI-PROCESS New Hello There [Waiting for input, Enabled]>
New Hello There
+User(25): 
YYY is a Special!.
YYY is a Special!.
YYY is a Special!.
YYY is a Special! done.

;;; Declare a process loop using a special variable YYY.
;;;
(clim-sys:make-process-loop
 :name "New Hello There"
 :with ((count 3) (yyy "YYY is a Special!"))
 :declare (declare (special yyy) (fixnum count))
 :initially (format t "~&~a" (clim-sys:process-name (clim-sys:current-process)))
 :while (progn (test1)
	       (>= (decf count) 0))
 :do (progn 
       (write-char #\.)
       (clim-sys:process-sleep 4))
 :finally (format t " done."))
#<MINI-PROCESS New Hello There [Waiting for input, Enabled]>

New Hello There
+User(26): 
YYY is a Special!.
YYY is a Special!.
YYY is a Special!.
YYY is a Special! done.

;;; Same example but using :UNTIL instead of :WHILE.
;;;
(clim-sys:make-process-loop
 :name "New Hello There"
 :with ((count 3) (yyy "YYY is a Special!"))
 :declare (declare (special yyy) (fixnum count))
 :initially (format t "~&~a" (clim-sys:process-name (clim-sys:current-process)))
 :until (progn (test1)
	       (< (decf count) 0))
 :do (progn 
       (write-char #\.)
       (clim-sys:process-sleep 4))
 :finally (format t " done."))
#<MINI-PROCESS New Hello There [Waiting for input, Enabled]>

New Hello There
+User(27): 
YYY is a Special!.
YYY is a Special!.
YYY is a Special!.
YYY is a Special! done.

;;; Here the same example except that the variables used in the process loop
;;; are not declared using :WITH but using a surrounding LET.
;;;
(let ((count 3) (yyy "YYY is a Special!"))
  (declare (fixnum count))
  (clim-sys:make-process-loop
   :name "New Hello There"
   :declare (declare (special yyy))
   :initially (format t "~&~a" (clim-sys:process-name (clim-sys:current-process)))
   :until (progn (test1)
		 (< (decf count) 0))
   :do (progn 
	 (write-char #\.)
	 (clim-sys:process-sleep 4))
   :finally (format t " done.")))
#<MINI-PROCESS New Hello There [Waiting for input, Enabled]>

New Hello There
+User(28): 
YYY is a Special!.
YYY is a Special!.
YYY is a Special!.
YYY is a Special! done.

;;; Here is an example where the context is precomputed and
;;; passed as argument to MAKE-PROCESS-LOOP.
;;;
(let ((count 3) (yyy "YYY is a Special!"))
  (let ((context1 (minp:make-context (:special (yyy) ))))
    (clim-sys:make-process-loop
     :name "New Hello There"
     :context context1
     :initially (format t "~&~a" (clim-sys:process-name (clim-sys:current-process)))
     :until (progn (test1)
		   (< (decf count) 0))
     :do (progn 
	   (write-char #\.)
	   (clim-sys:process-sleep 4))
     :finally (format t " done."))))
#<MINI-PROCESS New Hello There [Waiting for input, Enabled]>

New Hello There
+User(29): 
YYY is a Special!.
YYY is a Special!.
YYY is a Special!.
YYY is a Special! done.

;;; Here is a more involved example where a context is precomputed
;;; passed as argument and a new context is formed to include a new
;;; special variable ZZZ.
;;; This demonstrate that any top-level loop can be rewritten
;;; using MAKE-PROCESS-LOOP to achieve the same behavior (with
;;; a few more declaration) yet being portable to all LISP platforms.
;;;
(defun test2 (x) (declare (special zzz)) (format t zzz x))
TEST2

+User(30):
(let ((count 3) (yyy "YYY is a Special!"))
  (let ((context1 (minp:make-context (:special (yyy) ))))
    (clim-sys:make-process-loop
     :name "New Hello There"
     :context context1
     :with ((name 'ZZZ)
	    (zzz "Another Special ~a."))
     :declare (declare (special zzz) (symbol name))
     :initially (format t "~&~a" (clim-sys:process-name (clim-sys:current-process)))
     :until (progn (test1) (test2 name)
		   (< (decf count) 0))
     :do (progn 
	   (write-char #\.)
	   (clim-sys:process-sleep 4))
     :finally (format t " done."))))
#<MINI-PROCESS New Hello There [Waiting for input, Enabled]>

New Hello There
+User(31): 
YYY is a Special!Another Special ZZZ..
YYY is a Special!Another Special ZZZ..
YYY is a Special!Another Special ZZZ..
YYY is a Special!Another Special ZZZ. done.

;;; And the next challenge: successfully shadowing existing
;;; system variables that are declared globals or specials.
;;; (This required switching to MAKE-CONTEXT from MAKE-SIMPLE-CONTEXT.)
;;;
(defvar *foo* 123)
*FOO*
+User(32): (defvar *bar*)
*BAR*
+User(33): (defun test3 ()
	     (declare (special *foo* *bar*))
	     (format t "~&foo's value is ~s, bar's value is ~s"
		     *foo* (if (boundp '*bar*) *bar* '#:unbound)))
TEST3
+User(34):

;;; This is a failure case under MINIPROC (as well as standard MP).
;;; It's dangerous since when you restart this process (below) it
;;; resets the values of the globals *foo* and *bar* to 'alpha and 'omega!
;;; This is the place where not having process stacks can cause short-circuits.
;;; See next example for a simple solution.
;;;
+User(35):
(setq xxx (clim-sys:make-process-loop
	   :name "Clobering top stack special variables"
	   :with ((*foo* 'alpha) (*bar* 'omega))
	   :initially (test3)))

Warning: Process
         #<MINI-PROCESS Clobering top stack special variables [Waiting for input, Enabled]>
         has no :WHILE or :DO clause.
#<MINI-PROCESS Clobering top stack special variables [Waiting for input, Enabled]>
foo's value is 123, bar's value is #:UNBOUND

+User(36): (progn (clim-sys:enable-process xxx)
		  (clim-sys:restart-process xxx))
#<MINI-PROCESS Clobering top stack special variables [Restart, Enabled]>
foo's value is ALPHA, bar's value is OMEGA

+User(37): *foo*
ALPHA
+User(38): *bar*
OMEGA
+User(39): (setq *foo* 123)
123
+User(40): (makunbound '*bar*)
*BAR*
+User(41): 

;;; This is how you avoid this problem. You must know before-hand
;;; all the special variables that should be encapsulated in your
;;; process context and declare them all. Those you miss will end
;;; up being shared by all process instances of this type via the
;;; top level stack. This can be used as an advantage to share top
;;; variable cheaply among Mini Processes.
;;;
(setq xxx (clim-sys:make-process-loop
	   :name "Shadowing top stack special variables"
	   :with ((*foo* 'alpha) (*bar* 'omega))
	   :declare (declare (special *foo* *bar*))
	   :initially (test3)))
Warning: Process
         #<MINI-PROCESS Shadowing top stack special variables [Waiting for input, Enabled]>
         has no :WHILE or :DO clause.
#<MINI-PROCESS Shadowing top stack special variables [Waiting for input, Enabled]>
foo's value is ALPHA, bar's value is OMEGA
+User(42): (progn (clim-sys:enable-process xxx)
		  (clim-sys:restart-process xxx))
#<MINI-PROCESS Shadowing top stack special variables [Restart, Enabled]>
foo's value is ALPHA, bar's value is OMEGA

+User(43): *foo*
123
+User(44): (boundp '*bar*)
NIL
+User(45): 
(defun proctest (name value)
  (clim-sys:make-process-loop
   :name name
   :with ((count 19) (*foo* 'alpha) (*bar* 'omega))
   :declare (declare (special *foo* *bar*))
   :initially (progn (test3) (setq *foo* value) (test3))
   :while (> count 0)
   :do (progn (setq *bar* (decf count)) (clim-sys:process-sleep 2) (test3))
   :finally (test3)))

PROCTEST
+User(46): (compile 'proctest)
PROCTEST
NIL
NIL

+User(47): 
;;; The above example led to a new change to MAKE-CONTEXT.
;;; At this point the only think that may not be handled
;;; well is a case where you want your process to be intialized
;;; differently depending if a globally special is bound or not...
;;; Below I start two process instances and show that they
;;; behave as if they were running in independent stacks.
;;;
(proctest "Proc-1" 'ONE) ;First process
#<MINI-PROCESS Proc-1 [Waiting for input, Enabled]>
foo's value is ALPHA, bar's value is OMEGA
foo's value is ONE, bar's value is OMEGA
+User(48): 
foo's value is ONE, bar's value is 18
foo's value is ONE, bar's value is 17
foo's value is ONE, bar's value is 16
foo's value is ONE, bar's value is 15
foo's value is ONE, bar's value is 14(proctest "Proc-2" 'TWO) ;Second process
#<MINI-PROCESS Proc-2 [Waiting for input, Enabled]>
foo's value is ALPHA, bar's value is OMEGA
foo's value is TWO, bar's value is OMEGA
+User(49): 
foo's value is TWO, bar's value is 18
foo's value is ONE, bar's value is 13
foo's value is TWO, bar's value is 17
foo's value is ONE, bar's value is 12
foo's value is TWO, bar's value is 16
foo's value is ONE, bar's value is 11
foo's value is TWO, bar's value is 15
foo's value is ONE, bar's value is 10
foo's value is TWO, bar's value is 14
foo's value is ONE, bar's value is 9
foo's value is TWO, bar's value is 13
foo's value is ONE, bar's value is 8
foo's value is TWO, bar's value is 12
foo's value is ONE, bar's value is 7
foo's value is TWO, bar's value is 11
foo's value is ONE, bar's value is 6
foo's value is TWO, bar's value is 10
foo's value is ONE, bar's value is 5
foo's value is TWO, bar's value is 9
foo's value is ONE, bar's value is 4
foo's value is TWO, bar's value is 8
foo's value is ONE, bar's value is 3
foo's value is TWO, bar's value is 7
foo's value is ONE, bar's value is 2
foo's value is TWO, bar's value is 6
foo's value is ONE, bar's value is 1
foo's value is TWO, bar's value is 5
foo's value is ONE, bar's value is 0
foo's value is TWO, bar's value is 4
foo's value is ONE, bar's value is 0
foo's value is TWO, bar's value is 3
foo's value is TWO, bar's value is 2
foo's value is TWO, bar's value is 1
foo's value is TWO, bar's value is 0
foo's value is TWO, bar's value is 0

+User(52): (minp:variable-information '*foo*)
:SPECIAL
+User(53): *foo* ;Top level bindings are untouched
123
+User(54): (minp:variable-information '*bar*)
:SPECIAL
+User(55): (boundp '*bar*)
NIL
+User(56): 

;;; In comparison, this is the test case where no
;;; top level specials are currently used by our
;;; process construction function. Note that the code
;;; generated is simpler but the behavior is the same
;;; even if you later declare the variables *zfoo* and *zbar*
;;; to be globally special!
;;;
(defun ztest3 ()
 (declare (special *zfoo* *zbar*))
 (format t "~&foo's value is ~s, bar's value is ~s"
           *zfoo* (if (boundp '*zbar*) *zbar* '#:unbound)))
ZTEST3
+User(57): (compile 'ztest3)
ZTEST3
NIL
NIL
+User(58):
(defun zproctest (name value)
  (clim-sys:make-process-loop
   :name name
   :with ((count 19) (*zfoo* 'alpha) (*zbar* 'omega))
   :declare (declare (special *zfoo* *zbar*))
   :initially (progn (ztest3) (setq *zfoo* value) (ztest3))
   :while (> count 0)
   :do (progn (setq *zbar* (decf count)) (clim-sys:process-sleep 2) (ztest3))
   :finally (ztest3)))
ZPROCTEST

+User(59): (compile 'zproctest)
ZPROCTEST
NIL
NIL

+User(60): (zproctest "Zproc-1" 'ONE) ;First process
#<MINI-PROCESS Zproc-1 [Waiting for input, Enabled]>
foo's value is ALPHA, bar's value is OMEGA
foo's value is ALPHA, bar's value is OMEGA
+User(61): 
foo's value is ONE, bar's value is OMEGA
foo's value is ONE, bar's value is 18
foo's value is ONE, bar's value is 17
foo's value is ONE, bar's value is 16
foo's value is ONE, bar's value is 15
foo's value is ONE, bar's value is 14
foo's value is ONE, bar's value is 13(zproctest "Zproc-2" 'TWO) ;Second process
#<MINI-PROCESS Zproc-2 [Waiting for input, Enabled]>
foo's value is ALPHA, bar's value is OMEGA
foo's value is ALPHA, bar's value is OMEGA
+User(62): 
foo's value is TWO, bar's value is OMEGA
foo's value is ONE, bar's value is 12
foo's value is TWO, bar's value is 18
foo's value is ONE, bar's value is 11
foo's value is TWO, bar's value is 17
foo's value is ONE, bar's value is 10
foo's value is TWO, bar's value is 16
foo's value is ONE, bar's value is 9
foo's value is TWO, bar's value is 15
foo's value is ONE, bar's value is 8
foo's value is TWO, bar's value is 14
foo's value is ONE, bar's value is 7
foo's value is TWO, bar's value is 13
foo's value is ONE, bar's value is 6
foo's value is TWO, bar's value is 12
foo's value is ONE, bar's value is 5
foo's value is TWO, bar's value is 11
foo's value is ONE, bar's value is 4
foo's value is TWO, bar's value is 10
foo's value is ONE, bar's value is 3
foo's value is TWO, bar's value is 9
foo's value is ONE, bar's value is 2
foo's value is TWO, bar's value is 8
foo's value is ONE, bar's value is 1
foo's value is TWO, bar's value is 7
foo's value is ONE, bar's value is 0
foo's value is TWO, bar's value is 6
foo's value is TWO, bar's value is 5
foo's value is TWO, bar's value is 4
foo's value is TWO, bar's value is 3
foo's value is TWO, bar's value is 2
foo's value is TWO, bar's value is 1
foo's value is TWO, bar's value is 0

(minp:variable-information '*zfoo*)
NIL
+User(63): (boundp '*zfoo*)
NIL
+User(64): (minp:variable-information '*zbar*)
NIL
+User(65): (boundp '*zbar*)
NIL
+User(66):
(defun test3 ()
 (declare (special *foo* *bar*))
 (format t "~&BEFORE: foo's value is ~s, bar's value is ~s"
           *foo* (if (boundp '*bar*) *bar* '#:unbound)))

TEST3
+User(67):
(defun test4 ()
 (declare (special *foo* *bar*))
 (setq *foo* (rest *foo*))
 (setq *bar* (1+ *bar*)))
TEST4

+User(68):
(defun test5 ()
 (declare (special *foo* *bar*))
 (format t "~&AFTER: foo's value is ~s, bar's value is ~s"
           *foo* (if (boundp '*bar*) *bar* '#:unbound)))
TEST5

+User(69):
(defun proctest0 (name value)
  (clim-sys:make-process-loop
   :name name
   :with ((count 19) (*foo* 'alpha) (*bar* 35))
   :declare (declare (special *foo* *bar*))
   :initially (progn (test3) (setq *foo* value) (test5))
   :while (> count 0)
   :do (progn (test3) (decf count) (test4) (clim-sys:process-sleep 2) (test5))
   :finally (test5)))
PROCTEST0

+User(70): (compile 'proctest0)
PROCTEST0
NIL
NIL
+User(71): 

;;; Final stress test: verify that changes to the special vars
;;; in the process context are actually properly shared between
;;; a set of functions where they are declared specials and from
;;; invocation to invocation avoiding any cross-over between
;;; multiple process instances... The basic good behavior of processes.
;;;
(proctest0 "Final-1" '(a b c d e f g h i j k l m n o)) ;First instance
#<MINI-PROCESS Final-1 [Waiting for input, Enabled]>
BEFORE: foo's value is ALPHA, bar's value is 35
AFTER: foo's value is (A B C D E F G H I J K L M N O), bar's value is 35
+User(72): 
BEFORE: foo's value is (A B C D E F G H I J K L M N O), bar's value is 35
AFTER: foo's value is (B C D E F G H I J K L M N O), bar's value is 36
BEFORE: foo's value is (B C D E F G H I J K L M N O), bar's value is 36
AFTER: foo's value is (C D E F G H I J K L M N O), bar's value is 37
BEFORE: foo's value is (C D E F G H I J K L M N O), bar's value is 37
AFTER: foo's value is (D E F G H I J K L M N O), bar's value is 38
(proctest0 "Final-2" '(1 2 3 4 5 6 7 8 9 0 1 2 3 4 5)) ;Add second instance
#<MINI-PROCESS Final-2 [Waiting for input, Enabled]>
BEFORE: foo's value is ALPHA, bar's value is 35
AFTER: foo's value is (1 2 3 4 5 6 7 8 9 0 1 2 3 4 5), bar's value is 35
+User(73): 
BEFORE: foo's value is (1 2 3 4 5 6 7 8 9 0 1 2 3 4 5), bar's value is 35
AFTER: foo's value is (2 3 4 5 6 7 8 9 0 1 2 3 4 5), bar's value is 36
BEFORE: foo's value is (D E F G H I J K L M N O), bar's value is 38
AFTER: foo's value is (E F G H I J K L M N O), bar's value is 39
BEFORE: foo's value is (2 3 4 5 6 7 8 9 0 1 2 3 4 5), bar's value is 36
AFTER: foo's value is (3 4 5 6 7 8 9 0 1 2 3 4 5), bar's value is 37
BEFORE: foo's value is (E F G H I J K L M N O), bar's value is 39
AFTER: foo's value is (F G H I J K L M N O), bar's value is 40
BEFORE: foo's value is (F G H I J K L M N O), bar's value is 40
AFTER: foo's value is (G H I J K L M N O), bar's value is 41
BEFORE: foo's value is (3 4 5 6 7 8 9 0 1 2 3 4 5), bar's value is 37
AFTER: foo's value is (4 5 6 7 8 9 0 1 2 3 4 5), bar's value is 38
BEFORE: foo's value is (4 5 6 7 8 9 0 1 2 3 4 5), bar's value is 38
AFTER: foo's value is (5 6 7 8 9 0 1 2 3 4 5), bar's value is 39
BEFORE: foo's value is (G H I J K L M N O), bar's value is 41
AFTER: foo's value is (H I J K L M N O), bar's value is 42
BEFORE: foo's value is (H I J K L M N O), bar's value is 42
AFTER: foo's value is (I J K L M N O), bar's value is 43
BEFORE: foo's value is (5 6 7 8 9 0 1 2 3 4 5), bar's value is 39
AFTER: foo's value is (6 7 8 9 0 1 2 3 4 5), bar's value is 40
BEFORE: foo's value is (6 7 8 9 0 1 2 3 4 5), bar's value is 40
AFTER: foo's value is (7 8 9 0 1 2 3 4 5), bar's value is 41
BEFORE: foo's value is (I J K L M N O), bar's value is 43
AFTER: foo's value is (J K L M N O), bar's value is 44
BEFORE: foo's value is (J K L M N O), bar's value is 44
AFTER: foo's value is (K L M N O), bar's value is 45
BEFORE: foo's value is (7 8 9 0 1 2 3 4 5), bar's value is 41
AFTER: foo's value is (8 9 0 1 2 3 4 5), bar's value is 42
BEFORE: foo's value is (8 9 0 1 2 3 4 5), bar's value is 42
AFTER: foo's value is (9 0 1 2 3 4 5), bar's value is 43
BEFORE: foo's value is (K L M N O), bar's value is 45
AFTER: foo's value is (L M N O), bar's value is 46
BEFORE: foo's value is (L M N O), bar's value is 46
AFTER: foo's value is (M N O), bar's value is 47
BEFORE: foo's value is (9 0 1 2 3 4 5), bar's value is 43
AFTER: foo's value is (0 1 2 3 4 5), bar's value is 44
BEFORE: foo's value is (0 1 2 3 4 5), bar's value is 44
AFTER: foo's value is (1 2 3 4 5), bar's value is 45
BEFORE: foo's value is (M N O), bar's value is 47
AFTER: foo's value is (N O), bar's value is 48
BEFORE: foo's value is (N O), bar's value is 48
AFTER: foo's value is (O), bar's value is 49
BEFORE: foo's value is (1 2 3 4 5), bar's value is 45
AFTER: foo's value is (2 3 4 5), bar's value is 46
BEFORE: foo's value is (2 3 4 5), bar's value is 46
AFTER: foo's value is (3 4 5), bar's value is 47
BEFORE: foo's value is (O), bar's value is 49
AFTER: foo's value is NIL, bar's value is 50
BEFORE: foo's value is NIL, bar's value is 50
AFTER: foo's value is NIL, bar's value is 51
BEFORE: foo's value is (3 4 5), bar's value is 47
AFTER: foo's value is (4 5), bar's value is 48
BEFORE: foo's value is (4 5), bar's value is 48
AFTER: foo's value is (5), bar's value is 49
BEFORE: foo's value is NIL, bar's value is 51
AFTER: foo's value is NIL, bar's value is 52
BEFORE: foo's value is NIL, bar's value is 52
AFTER: foo's value is NIL, bar's value is 53
BEFORE: foo's value is (5), bar's value is 49
AFTER: foo's value is NIL, bar's value is 50
BEFORE: foo's value is NIL, bar's value is 50
AFTER: foo's value is NIL, bar's value is 51
BEFORE: foo's value is NIL, bar's value is 53
AFTER: foo's value is NIL, bar's value is 54
AFTER: foo's value is NIL, bar's value is 54
BEFORE: foo's value is NIL, bar's value is 51
AFTER: foo's value is NIL, bar's value is 52
BEFORE: foo's value is NIL, bar's value is 52
AFTER: foo's value is NIL, bar's value is 53
BEFORE: foo's value is NIL, bar's value is 53
AFTER: foo's value is NIL, bar's value is 54
AFTER: foo's value is NIL, bar's value is 54
(clim-sys:all-processes)
(#<MINI-PROCESS Mini Lisp Listener [Running, Enabled]>)
+User(74): :help
Type :END to suspend the mini listener
     :EXIT to exit the mini scheduler.
+User(75): :exit
:EXIT
USER(47): 
;;;
;;; Conclusion:
;;; This almost behaves like true processes. Actually if you
;;; try the above example on CL implementations that have
;;; multiprocessing (load system CLIM-SYS first) you should
;;; get the same behavior as above. [Expect minor differences
;;; in special cases: restarting a disabled process that has
;;; received interrupts (while disabled), versus restarting
;;; it after enabling it. Also the process interleaving details
;;; varies between implementations.]
;;;
;;; What's the magic?
;;; None. There are several known limitations using MINIPROC:
;;; 1) a PROCESS-SLEEP included in a Mini Process puts the entire DO
;;;    clause to rest (SLEEP) and it will restart the entire clause
;;;    when its time expires. This is not quite like true MP.
;;;    That's because the body of the process *is* its granularity.
;;; 2) processes should be careful not to dead-loop or they
;;;    will prevent other processes from running.
;;; 3) when accessing foreign and OS level functions make sure that
;;;    you are not using them in on-blocking mode. Clearly the Mini Scheduler
;;;    expects all processes to be friendly and non blocking.
;;; 4) error handling is not a problem as long as the restarts
;;;    are defined in the :DO clause or withing MAKE-PROCESS-LOOP,
;;;    otherwise each top level restart is shared by multiple processes.
;;;    That's why *IGNORE-ERRORS* is T by default.
;;; 5) the Mini Scheduler should not be run when an existing CL
;;;    scheduler is already running as the native scheduler may
;;;    cause interferences.
;;;
;;; With this in mind you might even port multiprocessing code
;;; using this MINIPROC version of CLIM-SYS to systems where
;;; multithreading or multiprocessing was unknown before...
;;;
||#

;;; New tests added to check MINILOCK implementations.
;;;
#||
USER(19): #+MINIPROC (minp:scheduling)
+User(64): (setq x (clim-sys:make-lock "testing"))
#<MINIPROC::LOCK testing #x5e9762>
+User(65): (clim-sys:with-lock-held (x "waiting for Z")
	    (describe x)
	    (clim-sys:process-loop :with ((n 7))
				:while (plusp (decf n))
				:do (progn (print n) (clim-sys:process-sleep 2))))
NIL
#<MINIPROC::LOCK testing #x5aa4a2> is an instance of #<STANDARD-CLASS MINIPROC::LOCK>:
 The following slots have :INSTANCE allocation:
  OWNER   #<MINI-PROCESS Mini Lisp Listener [waiting for Z, Enabled]>
  NAME    "testing"

6 
5 
4 
3 
2 
1 
+User(66): (describe x)
#<MINIPROC::LOCK testing #x3c2c12> is an instance of #<STANDARD-CLASS MINIPROC::LOCK>:
 The following slots have :INSTANCE allocation:
  OWNER   NIL
  NAME    "testing"

;;; Testing ordered locks. Lock ordering is only "on" when
;;; MINIPROC is active. But the default behavior should be
;;; identical on all implementations except for the failure
;;; case below.
;;;
+User(67): (setq x (clim-sys:make-lock "butter" #+MINIPROC 1))
#<MINIPROC::ORDERED-LOCK butter 1 #x635ad2>
+User(68): (setq y (clim-sys:make-lock "knife" #+MINIPROC 2))
#<MINIPROC::ORDERED-LOCK knife 2 #x734d02>
+User(69): (clim-sys:with-lock-held (x "waiting for the butter")
	     (describe x)
	     (clim-sys:with-lock-held (y "waiting for the knife")
	       (describe y)
	       (clim-sys:process-loop :with ((n 7))
				   :while (plusp (decf n))
				   :do (progn (print n) (clim-sys:process-sleep 2)))))
NIL
#<MINIPROC::ORDERED-LOCK butter 1 #x6b044a> is an instance of
    #<STANDARD-CLASS MINIPROC::ORDERED-LOCK>:
 The following slots have :INSTANCE allocation:
  OWNER   #<MINI-PROCESS Mini Lisp Listener [waiting for the butter, Enabled]>
  NAME    "butter"
  LEVEL   1
#<MINIPROC::ORDERED-LOCK knife 2 #x6b0852> is an instance of
    #<STANDARD-CLASS MINIPROC::ORDERED-LOCK>:
 The following slots have :INSTANCE allocation:
  OWNER   #<MINI-PROCESS Mini Lisp Listener [waiting for the knife, Enabled]>
  NAME    "knife"
  LEVEL   2

6 
5 
4 
3 
2 
1 
+User(70): (describe x)
#<MINIPROC::ORDERED-LOCK butter 1 #x6b044a> is an instance of
    #<STANDARD-CLASS MINIPROC::ORDERED-LOCK>:
 The following slots have :INSTANCE allocation:
  OWNER   NIL
  NAME    "butter"
  LEVEL   1
+User(71): (describe y)
#<MINIPROC::ORDERED-LOCK knife 2 #x5aa80a> is an instance of
    #<STANDARD-CLASS MINIPROC::ORDERED-LOCK>:
 The following slots have :INSTANCE allocation:
  OWNER   NIL
  NAME    "knife"
  LEVEL   2

;;; Testing out of order locking case this will only fail in MINPROC
;;; which supports ordered locks.
;;;
+User(72): (clim-sys:with-lock-held (y "waiting for the knife")
	     (describe y)
	     (clim-sys:with-lock-held (x "waiting for the butter")
	       (describe x) (clim-sys:process-loop :with ((n 7))
						:while (plusp (decf n))
						:do (progn (print n) (clim-sys:process-sleep 2)))))
NIL
#<MINIPROC::ORDERED-LOCK knife 2 #x3c37aa> is an instance of
    #<STANDARD-CLASS MINIPROC::ORDERED-LOCK>:
 The following slots have :INSTANCE allocation:
  OWNER   #<MINI-PROCESS Mini Lisp Listener [waiting for the knife, Enabled]>
  NAME    "knife"
  LEVEL   2
Error: Locks would be out of order: Can't seize
       #<MINIPROC::ORDERED-LOCK butter 1 #x3c349a> while owning
       #<MINIPROC::ORDERED-LOCK knife 2 #x3c37aa>.

Restart actions (select using :continue):
 0: Return from error in Mini Proccess Mini Lisp Listener.
 1: Ignore error and exit the Mini Scheduler.
[1] USER(4): :cont 0 ;Get the butter anyway
#<MINIPROC::ORDERED-LOCK butter 1 #x3c349a> is an instance of
    #<STANDARD-CLASS MINIPROC::ORDERED-LOCK>:
 The following slots have :INSTANCE allocation:
  OWNER   NIL
  NAME    "butter"
  LEVEL   1

6 
5 
4 
3 
2 
1 
Error: #<MINIPROC::ORDERED-LOCK butter 1 #x3c349a> is not owned by the current
       process. Can't release lock.

Restart actions (select using :continue):
 0: Return from error in Mini Proccess Mini Lisp Listener.
 1: Ignore error and exit the Mini Scheduler.
[1] USER(5): :cont 0 ;Forget about the butter we ate it
+User(73): (describe x)
#<MINIPROC::ORDERED-LOCK butter 1 #x3c349a> is an instance of
    #<STANDARD-CLASS MINIPROC::ORDERED-LOCK>:
 The following slots have :INSTANCE allocation:
  OWNER   NIL
  NAME    "butter"
  LEVEL   1
+User(74): (describe y)
#<MINIPROC::ORDERED-LOCK knife 2 #x3c37aa> is an instance of
    #<STANDARD-CLASS MINIPROC::ORDERED-LOCK>:
 The following slots have :INSTANCE allocation:
  OWNER   NIL
  NAME    "knife"
  LEVEL   2
+User(75): :exit
:EXIT
||#

;;;
;;; *** MINILOCK DEBUGGING ***
;;;
;;; Same LOCK tests as above.
;;; Note: The :DEBUG feature was "on" during compilation of CLIM-SYS
;;; for these tests. This is MINIPROC specific.
;;;
#||
+User(1): (setq x (clim-sys:make-lock "testing"))
#<MINIPROC::LOCK testing #x3a2a8a>
+User(2): (clim-sys:with-lock-held (x "waiting for Z")
	    (describe x)
	    (clim-sys:process-loop :with ((n 7))
				:while (plusp (decf n))
				:do (progn (print n) (clim-sys:process-sleep 2))))
Add #<MINI-PROCESS Mini Lisp Listener [Running, Enabled]> completion 1.
NIL
Run #<MINI-PROCESS Mini Lisp Listener [Running, Enabled]> completion 1.
Add #<MINI-PROCESS Mini Lisp Listener [waiting for Z, Enabled]> completion 2.
#<MINIPROC::LOCK testing #x3a2a8a> is an instance of #<STANDARD-CLASS MINIPROC::LOCK>:
 The following slots have :INSTANCE allocation:
  OWNER   #<MINI-PROCESS Mini Lisp Listener [waiting for Z, Enabled]>
  NAME    "testing"

6 
5 
4 
3 
2 
1 
Run #<MINI-PROCESS Mini Lisp Listener [Running, Enabled]> completion 2.
+User(3): (describe x)
#<MINIPROC::LOCK testing #x3a2a8a> is an instance of #<STANDARD-CLASS MINIPROC::LOCK>:
 The following slots have :INSTANCE allocation:
  OWNER   NIL
  NAME    "testing"

;;; Testing ordered locks. Lock ordering is only on when
;;; MINIPROC is active. But the default behavior should be
;;; identical on all implementations except for the failure
;;; case below.
;;;
+User(10): (setq x (clim-sys:make-lock "butter" 1))
#<MINIPROC::ORDERED-LOCK butter 1 #x76eff2>
+User(11): (setq y (clim-sys:make-lock "knife" 2))
#<MINIPROC::ORDERED-LOCK knife 2 #x6dc2d2>
+User(12): (clim-sys:with-lock-held (x "waiting for the butter")
	     (describe x)
	     (clim-sys:with-lock-held (y "waiting for the knife")
	       (describe y)
	       (clim-sys:process-loop :with ((n 7))
				   :while (plusp (decf n))
				   :do (progn (print n) (clim-sys:process-sleep 2)))))
Add #<MINI-PROCESS Mini Lisp Listener [Running, Enabled]> completion 1.
NIL
Run #<MINI-PROCESS Mini Lisp Listener [Running, Enabled]> completion 1.
Add #<MINI-PROCESS Mini Lisp Listener [waiting for the butter, Enabled]> completion 2.
#<MINIPROC::ORDERED-LOCK butter 1 #x3ac292> is an instance of
    #<STANDARD-CLASS MINIPROC::ORDERED-LOCK>:
 The following slots have :INSTANCE allocation:
  OWNER   #<MINI-PROCESS Mini Lisp Listener [waiting for the butter, Enabled]>
  NAME    "butter"
  LEVEL   1
Add #<MINI-PROCESS Mini Lisp Listener [waiting for the butter, Enabled]> completion 1.

(MINIPROC::STANDARD-COMPLETIONS
 (#<Interpreted Closure (unnamed) @ #x70ed8a>
  #<Interpreted Closure (unnamed) @ #x710eb2>)) 
Run #<MINI-PROCESS Mini Lisp Listener [Running, Enabled]> completion 2.
Run #<MINI-PROCESS Mini Lisp Listener [Running, Enabled]> completion 1.
Add #<MINI-PROCESS Mini Lisp Listener [waiting for the knife, Enabled]> completion 2.
#<MINIPROC::ORDERED-LOCK knife 2 #x3ac65a> is an instance of
    #<STANDARD-CLASS MINIPROC::ORDERED-LOCK>:
 The following slots have :INSTANCE allocation:
  OWNER   #<MINI-PROCESS Mini Lisp Listener [waiting for the knife, Enabled]>
  NAME    "knife"
  LEVEL   2

6 
5 
4 
3 
2 
1 
Run #<MINI-PROCESS Mini Lisp Listener [Running, Enabled]> completion 2.
+User(13): (describe x)
#<MINIPROC::ORDERED-LOCK butter 1 #x3ac292> is an instance of
    #<STANDARD-CLASS MINIPROC::ORDERED-LOCK>:
 The following slots have :INSTANCE allocation:
  OWNER   NIL
  NAME    "butter"
  LEVEL   1
+User(14): (describe y)
#<MINIPROC::ORDERED-LOCK knife 2 #x3ac65a> is an instance of
    #<STANDARD-CLASS MINIPROC::ORDERED-LOCK>:
 The following slots have :INSTANCE allocation:
  OWNER   NIL
  NAME    "knife"
  LEVEL   2
+User(15): 

;;; Testing out of order locking case this will only fail in MINPROC
;;; which supports ordered locks.
;;;
+User(16): (clim-sys:with-lock-held (y "waiting for the knife")
	     (describe y)
	     (clim-sys:with-lock-held (x "waiting for the butter")
	       (describe x) (clim-sys:process-loop :with ((n 7))
						:while (plusp (decf n))
						:do (progn (print n) (clim-sys:process-sleep 2)))))
Add #<MINI-PROCESS Mini Lisp Listener [Running, Enabled]> completion 1.
NIL
Run #<MINI-PROCESS Mini Lisp Listener [Running, Enabled]> completion 1.
Add #<MINI-PROCESS Mini Lisp Listener [waiting for the knife, Enabled]> completion 2.
#<MINIPROC::ORDERED-LOCK nife 2 #x3ac65a> is an instance of
    #<STANDARD-CLASS MINIPROC::ORDERED-LOCK>:
 The following slots have :INSTANCE allocation:
  OWNER   #<MINI-PROCESS Mini Lisp Listener [waiting for the knife, Enabled]>
  NAME    "nife"
  LEVEL   2
Add #<MINI-PROCESS Mini Lisp Listener [waiting for the knife, Enabled]> completion 1.
Error: Locks would be out of order: Can't seize
       #<MINIPROC::ORDERED-LOCK butter 1 #x3ac292> while owning
       #<MINIPROC::ORDERED-LOCK nife 2 #x3ac65a>.

Restart actions (select using :continue):
 0: Return from error in Mini Proccess Mini Lisp Listener.
 1: Ignore error and exit the Mini Scheduler.
[1] USER(168):
||#
