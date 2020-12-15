;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: MINP; Base: 10 -*-

;;; Copyright (C) 1994, 1995 Olivier (OBC) all rights reserved.
;;; See copyright notice in MINIPROC file.
;;;
;;; CLIM-SYS/UNIFIED PROCESS INTERFACE
;;;
;;; Awaiting a consistent CLIM-SYS implementation everywhere.
;;; This substitutes a common process interface compatible with
;;; CLIM-SYS wherever CLIM is not present. Wherever NO multiprocessing
;;; facility is not provided then this is designed to integrate
;;; with MINIPROC (the Mini Process system based on closures).
;;; More reasons to program multi processes portably.
;;;
;;; For any new implementation you should check this file carefully.
;;; Perhaps Franz or Harlequin have more to contribute
;;; for portability... There is not guaranty that this works,
;;; a few symbol names might have been imagined hoping they are
;;; supported by the corresponding implementation.
;;;
;;; Please fill-in the missing symbols for your favorite implementation
;;; and make this available to all of us. Thanks - OBC.
;;;

(in-package "MINP")

#+ignore
(defpackage "MINP"
  (:use)
  (:export
   "ATOMIC-DECF"
   "ATOMIC-INCF"
   "CURRENT-PROCESS" "ALL-PROCESSES" "FIND-PROCESS-NAMED"
   "PROCESSP" "PROCESS-NAME" "PROCESS-YIELD"
   "PROCESS-WAIT" "PROCESS-WAIT-WITH-TIMEOUT" "PROCESS-SLEEP"
   "DISABLE-PROCESS" "ENABLE-PROCESS" "RESTART-PROCESS"
   "PROCESS-INTERRUPT"
   "PROCESS-STATE" "PROCESS-WHOSTATE" "PROCESS-ACTIVE-P"
   "DESTROY-PROCESS"
   "WITHOUT-SCHEDULING" "MAKE-LOCK" "MAKE-RECURSIVE-LOCK"
   "WITH-LOCK-HELD" "WITH-RECURSIVE-LOCK-HELD"))

(defvar *multiprocessing-p*
    (or #+(or Allegro CCL-3 Genera lucid) t
	#+LispWorks mp::*multiprocessing*
	#+MINIPROC t))

#-MINIPROC
(defun all-processes ()
  (or #+(or LispWorks Allegro)
      mp:*all-processes*
      #+Symbolics
      process:*all-processes*
      #+lcl4.2
      lcl:*all-processes*))

;;; A suggestion is that MAKE-PROCESS could accept
;;; other keywords besides name that are mostly ignored
;;; or passed to the actual implementation such as
;;; QUANTUM, PRIORITY, etc.
;;;
#-MINIPROC
(defmacro make-process-mp (fun &key name)
  `(or
    #+LispWorks
    (mp:process-run-function ,name () ,fun)
    #+Allegro
    (mp:process-run-function ,name ,fun)
    #+Symbolics
    (process:process-run-function ,name ,fun)
    #+CCL-3
    (ccl::process-run-function `(:name ,,name) ,fun)
    #+lcl4.2
    (lcl:make-process :name ,name :function ,fun)
    (funcall ,fun)))

#-MINIPROC
(defmacro make-process-mp-interruptable (fun &key name)
  ;; Fix to insure an MP process can be interrupted
  ;; and restarted even if dead on arrival. So we don't have to
  ;; rely on process-preset to process-loop and restart existing
  ;; processes that are exhausted. For example process resources.
  `(make-process-mp #'(lambda ()
			(process-sleep 1E-1)	;A minimum time is required
						;may be implementation depdt
			(funcall ,fun))
		      :name ,name))

#-MINIPROC
(defun current-process ()
  #+(or Allegro LispWorks)
  mp:*current-process*
  #+Symbolics
  process:*current-process*
  #+MCL
  ccl::*current-process*
  #+lcl4.2
  lcl:*current-process*)

(defmethod destroy-process ((process t))
  #+Symbolics
  (process:process-kill process)
  #+(or Allegro LispWorks)
  (mp:process-kill process)
  #+lcl4.2
  (lcl:kill-process process))

(defmethod process-whostate ((process t))
  #+Symbolics
  (process:process-whostate process)
  #+(or Allegro LispWorks)
  (mp:process-whostate process)
  #+CCL-3
  (ccl::process.whostate process)
  #+lcl4.2
  (lcl:process-whostate process))

(defmethod process-state ((process t))
  (process-whostate process))

#-MINIPROC
(defmethod processp ((process t))
  #+Symbolics
  (process:process-p process)
  #+(or Allegro LispWorks)
  (mp:process-p process)
  #+CCL-3
  (ccl:process-p process)
  #+lcl4.2
  (lcl:processp process))

(defmethod process-name ((process t))
  #+Symbolics
  (process:process-name process)
  #+(or Allegro LispWorks)
  (mp:process-name process)
  #+CCL-3
  (ccl::process-name process)
  #+lcl4.2
  (lcl:process-name process))

(defmethod (setf process-name) (val (process t))
  #+Symbolics
  (setf (process:process-name process) val)
  #+(or Allegro LispWorks)
  (setf (mp:process-name process) val)
  #+CCL-3
  (setf (ccl::process-name process) val)
  #+lcl4.2
  (setf (lcl:process-name process) val))

(defmethod enable-process ((process t))
  #+(or Allegro LispWorks)
  (mp:process-enable process)
  #+Symbolics
  (process:process-enable process)
  #+CCL-3
  (ccl::process-enable process)
  #+lcl4.2
  (lcl:activate-process process))

(defmethod disable-process ((process t))
  #+(or Allegro LispWorks)
  (mp:process-disable process)
  #+Symbolics
  (process:process-disable process)
  #+CCL-3
  (ccl::process-disable process)
  #+lcl4.2
  (lcl::deactivate-process process))

(defmethod restart-process ((process t))
  #+(or Allegro LispWorks)
  (mp:process-reset process)
  #+Symbolics
  (process:process-restart process)
  #+CCL-3
  (ccl:process-reset process)
  #+lcl4.2
  (lcl:restart-process process))

(defmethod process-interrupt ((proc t) lambda)
  #+(or Allegro LispWorks)
  (mp:process-interrupt proc lambda)
  #+Symbolics
  (process:process-interrupt proc lambda)
  #+CCL-3
  (ccl:process-interrupt proc lambda)
  #+lcl4.2
  (lcl:interrupt-process proc lambda))

#-MINIPROC
(defun process-yield ()
  #+lispworks
  (mp:process-allow-scheduling)
  #+allegro
  (mp:process-allow-schedule)
  #+Symbolics
  (process:process-allow-scheduler)
  #+CCL-3
  (ccl::process-allow-scheduling)
  #+lcl4.2
  (lcl:process-allow-schedule))

#-MINIPROC
(defun process-wait (wait-reason predicate)
  #+(or Allegro LispWorks)
  (mp:process-wait wait-reason predicate)
  #+Symbolics
  (process:process-wait wait-reason predicate)
  #+CCL-3
  (ccl::process-wait wait-reason predicate)
  #+lcl4.2
  (lcl:process-wait wait-reason predicate))

#-MINIPROC
(defun process-wait-with-timeout (wait-reason timeout predicate)
  #+(or Allegro LispWorks)
  (mp:process-wait-with-timeout wait-reason timeout predicate)
  #+Symbolics
  (process:process-wait-with-timeout wait-reason timeout predicate)
  #+CCL-3
  (ccl:process-wait-with-timeout wait-reason timeout predicate)
  #+lcl4.2
  (lcl:process-wait-with-timeout wait-reason timeout predicate))

;;; Add PROCESS-SLEEP to CLIM-SYS in non MINIPROC case.
;;; Note that the substitute definition based on PROCESS-WAIT-WITH-TIMEOUT
;;; is there for future ports. However in the case of ACL it caused
;;; an irrecoverable error in our test cases so the PROCESS-SLEEP definition
;;; is prefered for most implementations just in case.
;;; Bug reported on 9/9/95 at bugs@franz.com.
;;;
#-MINIPROC
(defun process-sleep (timeout)
  (if (numberp timeout)
      #+Allegro (mp:process-sleep timeout)
      #+LispWorks (mp:sleep-for-time timeout)
      #+Symbolics (process:process-sleep timeout)
      #+CCL-3 (ccl:process:process-sleep timeout)
      #+(and (not (or Allegro CCL-3 LispWorks Symbolics)) CLIM-2)
      (clim-sys:process-wait-with-timeout "Sleeping" timeout #'(lambda ()))
      #+lcl4.2
      (cl:sleep timeout)))

#-MINIPROC
(eval-when (compile eval load)
(defmacro without-scheduling (&rest body)
  `(,(first '(#+Allegro excl:without-interrupts
	      #+CCL-3 ccl:without-interrupts
	      #+LispWorks sys:without-interrupts
	      #+Symbolics process:without-interrupts
	      #+lcl4.2 lcl::with-interruptions-inhibited
	      progn))
       ,@body))
)

#-MINIPROC
(defun make-lock (&optional lock-name)
  #+Symbolics
  (process:make-lock name)
  #+(or Allegro LispWorks)
  (mp:make-process-lock :name lock-name)
  #+CCL-3
  (ccl::make-process-queue lock-name)
  #+lcl4.2
  (set (intern lock-name) ()))

#-MINIPROC
(defun make-recursive-lock (&optional lock-name)
  (make-lock lock-name))

#+(and Symbolics (not MINIPROC))
(defmacro with-lock-held ((lock &optional state) &body body)
  `(loop (when (process:lock-idle-p ,lock)
	   (process:with-lock (,lock ,state) ,@body)
	   (return))))

#+(and CCL-3 (not MINIPROC))
(defmacro with-lock-held ((lock &optional state) &body body)
  (declare (ignore state))
  `(ccl::with-process-enqueued 
     (,lock ccl::*current-process* "Wait for Lock")
     ,@body))

#+(and Lispworks (not MINIPROC))
(defmacro with-lock-held ((lock &rest lock-args) &body body)
  (declare (ignore state))
  `(mp:with-lock (,lock ,@lock-args) ,@body))

#+(and lcl4.2 (not MINIPROC))
(defmacro with-lock-held ((lock &rest lock-args) &body body)
  (declare (ignore lock-args))
  `(lcl:with-process-lock (,lock) ,@body))

#+(and Symbolics (not MINIPROC))
(defmacro with-recursive-lock-held ((lock &optional state) &body body)
  `(loop (when (process:lock-idle-p ,lock)
	   (process:with-lock (,lock ,state) ,@body)
	   (return))))

#+(and CCL-3 (not MINIPROC))
(defmacro with-recursive-lock-held ((lock &optional state) &body body)
  (declare (ignore state))
  `(ccl::with-process-enqueued 
     (,lock ccl::*current-process* "Wait for Lock")
     ,@body))

#+(and Lispworks (not MINIPROC))
(defmacro with-recursive-lock-held ((lock &rest lock-args) &body body)
  (declare (ignore state))
  `(mp:with-lock (,lock ,@lock-args) ,@body))

#+(and lcl4.2 (not MINIPROC))
(defmacro with-recursive-lock-held ((lock &rest lock-args) &body body)
  `(lcl:with-process-lock (,lock ,@lock-args) ,@body))

#+(and Allegro (not MINIPROC))
(defmacro with-lock-held ((lock &optional state) &body body)
  `(mp:with-process-lock (,lock :norecursive t :whostate ,state) ,@body))

#+(and Allegro (not MINIPROC))
(defmacro with-recursive-lock-held ((lock &optional state) &rest body)
  (declare (ignore state))
  `(mp:with-process-lock ,lock ,@body))

(defmacro atomic-incf (reference &optional (delta 1))
  `(without-scheduling (incf ,reference ,delta)))

(defmacro atomic-decf (reference &optional (delta 1))
  `(without-scheduling (decf ,reference ,delta)))
