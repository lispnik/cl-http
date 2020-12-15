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
   "PROCESS-STATE" "PROCESS-WHOSTATE"
   "DESTROY-PROCESS"
   "WITHOUT-SCHEDULING" "MAKE-LOCK" "MAKE-RECURSIVE-LOCK"
   "WITH-LOCK-HELD" "WITH-RECURSIVE-LOCK-HELD")
  (:export "PROCESS-ACTIVE-P" "PROCESS-PRESET")
)

(defvar *multiprocessing-p*
    (or #+(or Allegro CCL-3 Genera) t
	#+LispWorks mp::*multiprocessing*
	#+MINIPROC t))

#-MINIPROC
(defun all-processes ()
  (or #+Allegro
      mp:*all-processes*
      #+LispWorks
      (mp::list-all-processes)
      #+Symbolics
      process:*all-processes*))

;;; A suggestion is that MAKE-PROCESS could accept
;;; other keywords besides name that are mostly ignored
;;; or passed to the actual implementation such as
;;; QUANTUM, PRIORITY, etc.
;;;
#-MINIPROC
(defun make-process-mp (fun &rest initialization-args &key name &allow-other-keys)
  #+LispWorks
  (mp:process-run-function name initialization-args fun)
  #+Allegro
  (mp:process-run-function (or initialization-args name) fun)
  #+Symbolics
  (process:process-run-function (or initialization-args name) fun)
  #+CCL-3
  (ccl::process-run-function (or initialization-args `(:name ,name)) fun)
  #-(or LispWorks Allegro Symbolics CCL-3)
  (error "Undefined MAKE-PROCESS-MP for this port."))

#-MINIPROC
(defun make-process-mp-interruptable (fun &rest initialization-args)
  ;; Fix to insure an MP process can be interrupted
  ;; and restarted even if dead on arrival. So we don't have to
  ;; rely on process-preset to process-loop and restart existing
  ;; processes that are exhausted. For example process resources.
  (apply #'make-process-mp
	 #'(lambda ()
	     (process-sleep 1E-1)	;A minimum time is required
	     ;may be implementation depdt
	     (funcall fun))
	 initialization-args))

#-MINIPROC
(defun current-process ()
  #+(or Allegro LispWorks)
  mp:*current-process*
  #+Symbolics
  process:*current-process*
  #+MCL
  ccl::*current-process*)

(defmethod destroy-process ((process t))
  #+Symbolics
  (process:process-kill process)
  #+(or Allegro LispWorks)
  (mp:process-kill process))

(setf (fdefinition 'process-kill) (fdefinition 'destroy-process))

(defmethod process-whostate ((process t))
  #+Symbolics
  (process:process-whostate process)
  #+Allegro
  (mp:process-whostate process)
  #+LispWorks
  (mp::process-whostate process)
  #+CCL-3
  (ccl::process.whostate process))

(defmethod process-state ((process t))
  (process-whostate process))

(defmethod process-active-p ((process t))
  #+Symbolics
  (process:process-active-p process)
  #+Allegro
  (mp:process-active-p process)
  #+LispWorks
  (mp::process-active-p process)
  #+CCL-3
  (ccl::process.active-p process))

(defmethod process-preset ((process t) initial-function &rest initial-args)
  #+Symbolics
  (apply #'process:process-preset process initial-function initial-args)
  #+Allegro
  (apply #'mp:process-preset process initial-function initial-args)
  #+LispWorks
  (apply 'mp::process-preset process initial-function initial-args)
  #+CCL-3
  (apply #'ccl::process.preset process initial-function initial-args))

#-MINIPROC
(defmethod processp ((process t))
  #+Symbolics
  (process:process-p process)
  #+Allegro
  (mp:process-p process)
  #+LispWorks
  (mp::process-p process)
  #+CCL-3
  (ccl:process-p process))

(defmethod process-name ((process t))
  #+Symbolics
  (process:process-name process)
  #+(or Allegro LispWorks)
  (mp:process-name process)
  #+CCL-3
  (ccl::process-name process))

(defmethod (setf process-name) (val (process t))
  #+Symbolics
  (setf (process:process-name process) val)
  #+(or Allegro LispWorks)
  (setf (mp:process-name process) val)
  #+CCL-3
  (setf (ccl::process-name process) val))

(defmethod enable-process ((process t))
  #+(or Allegro LispWorks)
  (mp:process-enable process)
  #+Symbolics
  (process:process-enable process)
  #+CCL-3
  (ccl::process-enable process))

(setf (fdefinition 'process-enable) (fdefinition 'enable-process))

(defmethod disable-process ((process t))
  #+(or Allegro LispWorks)
  (mp:process-disable process)
  #+Symbolics
  (process:process-disable process)
  #+CCL-3
  (ccl::process-disable process))

(setf (fdefinition 'process-disable) (fdefinition 'disable-process))

(defmethod restart-process ((process t))
  #+(or Allegro LispWorks)
  (mp:process-reset process)
  #+Symbolics
  (process:process-restart process)
  #+CCL-3
  (ccl:process-reset process))

(setf (fdefinition 'process-restart) (fdefinition 'restart-process))

(defmethod process-interrupt ((proc t) lambda)
  #+(or Allegro LispWorks)
  (mp:process-interrupt proc lambda)
  #+Symbolics
  (process:process-interrupt proc lambda)
  #+CCL-3
  (ccl:process-interrupt proc lambda))

#-MINIPROC
(defun process-yield ()
  #+lispworks
  (mp:process-allow-scheduling)
  #+allegro
  (mp:process-allow-schedule)
  #+Symbolics
  (process:process-allow-scheduler)
  #+CCL-3
  (ccl::process-allow-scheduling))

#-MINIPROC
(defun process-wait (wait-reason predicate)
  #+(or Allegro LispWorks)
  (mp:process-wait wait-reason predicate)
  #+Symbolics
  (process:process-wait wait-reason predicate)
  #+CCL-3
  (ccl::process-wait wait-reason predicate))

#-MINIPROC
(defun process-wait-with-timeout (wait-reason timeout predicate)
  #+(or Allegro LispWorks)
  (mp:process-wait-with-timeout wait-reason timeout predicate)
  #+Symbolics
  (process:process-wait-with-timeout wait-reason timeout predicate)
  #+CCL-3
  (ccl:process-wait-with-timeout wait-reason timeout predicate))

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
      (clim-sys:process-wait-with-timeout "Sleeping" timeout #'(lambda ()))))

#-MINIPROC
(eval-when (compile eval load)
(defmacro without-scheduling (&rest body)
  `(,(first '(#+Allegro excl:without-interrupts
	      #+CCL-3 ccl:without-interrupts
	      #+LispWorks lw:without-interrupts
	      #+Symbolics process:without-interrupts
	      progn))
       ,@body))
)

#-MINIPROC
(defun make-lock (&optional lock-name)
  #+Symbolics
  (process:make-lock name)
  #+Allegro
  (mp:make-process-lock :name lock-name)
  #+LispWorks
  (mp:make-lock :name lock-name)
  #+CCL-3
  (ccl::make-process-queue lock-name))

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
