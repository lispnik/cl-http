;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: CL-USER; Base: 10 -*-

;;; Copyright (C) 1994, 1995 Olivier (OBC) all rights reserved.
;;; See copyright notice in MINIPROC file.

(in-package "CL-USER")

;;; Exported feature
(pushnew :CLIM-SYS *features*)

(eval-when (compile load eval)
(defpackage "MINIPROC"
  (:use "COMMON-LISP")
  (:nicknames "MINP")
  ;; Export for CLIM-SYS
  (:export
   "ATOMIC-DECF"
   "ATOMIC-INCF")
  ;; Standard
  (:export
   "MAKE-PROCESS"
   "CURRENT-PROCESS" "ALL-PROCESSES" "FIND-PROCESS-NAMED"
   "PROCESSP" "PROCESS-NAME" "PROCESS-YIELD"
   "PROCESS-WAIT" "PROCESS-WAIT-WITH-TIMEOUT"
   "DISABLE-PROCESS" "ENABLE-PROCESS" "RESTART-PROCESS"
   "PROCESS-INTERRUPT"
   "PROCESS-STATE" "PROCESS-WHOSTATE"
   "DESTROY-PROCESS"
   "WITHOUT-SCHEDULING" "MAKE-LOCK" "MAKE-RECURSIVE-LOCK"
   "WITH-LOCK-HELD" "WITH-RECURSIVE-LOCK-HELD")
  (:export
   "ALLOCATE-RESOURCE" "CLEAR-RESOURCE" "DEALLOCATE-RESOURCE" "DEFRESOURCE"
   "MAP-RESOURCE" "USING-RESOURCE")
  ;; Extensions
  (:export
   "MAKE-PROCESS-LOOP"		;portable way to create processes
   "PROCESS-ACTIVE-P"
   "PROCESS-DISABLE"		;really disable-process
   "PROCESS-ENABLE"		;really enable-process
   "PROCESS-KILL"		;really destroy-process
   "PROCESS-LOOP"		;portable non blocking loop macro
   "PROCESS-ITERATE"		;method to take-over process
   "PROCESS-PRESET"
   "PROCESS-RESTART"		;really restart-process
   "PROCESS-SLEEP"
   "UNWIND-PROCESS")		;portable unwind extended for mini process
  ;; Export for MINIPROC convenience
  (:export
   "*SCHEDULING-OPTIONS*"
   "*WARN-IF-NO-PROCESS-LOOP*"
   "*WITHOUT-SCHEDULING-PROCESS-YIELD-P*"
   "CLEAR-PROCESS" "CLEAR-LISTENER"
   "DESTROY-ALL-PROCESSES" "DESTROY-CURRENT-PROCESS"
   "DISABLE-LISTENER" "DISABLE-POKER"
   "ENABLE-LISTENER" "ENABLE-POKER"
   "MAKE-CONTEXT"		;make :CONTEXT for process-loop
   "SCHEDULING" "SCHEDULING-EXIT"
   "VARIABLE-INFORMATION"
   "WITH-CURRENT-PROCESS")
  (:shadow "WITH-OPEN-FILE")
  (:export "WITH-OPEN-FILE"))
)

;;; This package is defined for portability across all CL
;;; implementations. If CLIM and multiprocessing are already
;;; present loading this will minimally add to CLIM-SYS so
;;; that MINIPROC extensions are supported.
;;;
(eval-when (compile load eval)
(if (read-from-string "#-(or MINIPROC (not CLIM-2)) T ()")
(defpackage "CLIM-SYS"
  ;; Now cover all implementations
  (:use)
  ;; Useful additions
  (:import-from
   "MINP"
   "MAKE-PROCESS-LOOP"		;portable way to create processes
   "PROCESS-ACTIVE-P"
   "PROCESS-DISABLE"		;really disable-process
   "PROCESS-ENABLE"		;really enable-process
   "PROCESS-KILL"		;really destroy-process
   "PROCESS-LOOP"		;portable non blocking loop macro
   "PROCESS-ITERATE"		;method to take-over process
   "PROCESS-PRESET"
   "PROCESS-RESTART"		;really restart-process
   "PROCESS-SLEEP"
   "UNWIND-PROCESS")		;portable unwind extended for mini process
  ;; Standard definitions (should already exist)
  (:export
   "*MULTIPROCESSING-P*" "ALL-PROCESSES" "ALLOCATE-RESOURCE" "ATOMIC-DECF"
   "ATOMIC-INCF" "CLEAR-RESOURCE" "CURRENT-PROCESS" "DEALLOCATE-RESOURCE"
   "DEFGENERIC*" "DEFMETHOD*" "DEFRESOURCE" "DESTROY-PROCESS" "DISABLE-PROCESS"
   "ENABLE-PROCESS" "MAKE-LOCK" "MAKE-PROCESS" "MAKE-RECURSIVE-LOCK"
   "MAP-RESOURCE" "PROCESS-INTERRUPT" "PROCESS-NAME" "PROCESS-STATE"
   "PROCESS-WAIT" "PROCESS-WAIT-WITH-TIMEOUT" "PROCESS-WHOSTATE" "PROCESS-YIELD"
   "PROCESSP" "RESTART-PROCESS" "SETF*" "USING-RESOURCE" "WITH-LOCK-HELD"
   "WITH-RECURSIVE-LOCK-HELD" "WITHOUT-SCHEDULING")
  ;; Extensions
  (:export
   "MAKE-PROCESS-LOOP"		;portable way to create processes
   "PROCESS-ACTIVE-P"
   "PROCESS-DISABLE"		;really disable-process
   "PROCESS-ENABLE"		;really enable-process
   "PROCESS-KILL"		;really destroy-process
   "PROCESS-LOOP"		;portable non blocking loop macro
   "PROCESS-ITERATE"		;method to take-over process
   "PROCESS-PRESET"
   "PROCESS-RESTART"		;really restart-process
   "PROCESS-SLEEP"
   "UNWIND-PROCESS")		;portable unwind extended for mini process
  )
)
)

;;; This is for bare bone CL we add all we can
;;;
(eval-when (compile load eval)
(if (read-from-string "#+(or MINIPROC (not CLIM-2)) T ()")
(defpackage "CLIM-SYS"
  ;; Now cover all implementations
  (:use)
  ;; Useful additions
  (:import-from
   "MINP"
   "MAKE-PROCESS-LOOP"		;portable way to create processes
   "PROCESS-ACTIVE-P"
   "PROCESS-DISABLE"		;really disable-process
   "PROCESS-ENABLE"		;really enable-process
   "PROCESS-KILL"		;really destroy-process
   "PROCESS-LOOP"		;portable non blocking loop macro
   "PROCESS-ITERATE"		;method to take-over process
   "PROCESS-PRESET"
   "PROCESS-RESTART"		;really restart-process
   "PROCESS-SLEEP"
   "UNWIND-PROCESS")		;portable unwind extended for mini process
  ;; Basic CLIM-SYS
  (:import-from
   "MINP"
   "ATOMIC-DECF"
   "ATOMIC-INCF"
   "CURRENT-PROCESS" "ALL-PROCESSES" "FIND-PROCESS-NAMED"
   "PROCESSP" "PROCESS-NAME" "PROCESS-YIELD"
   "MAKE-PROCESS"
   "PROCESS-WAIT" "PROCESS-WAIT-WITH-TIMEOUT"
   "DISABLE-PROCESS" "ENABLE-PROCESS" "RESTART-PROCESS"
   "PROCESS-INTERRUPT"
   "PROCESS-STATE" "PROCESS-WHOSTATE"
   "DESTROY-PROCESS"
   "WITHOUT-SCHEDULING" "MAKE-LOCK" "MAKE-RECURSIVE-LOCK"
   "WITH-LOCK-HELD" "WITH-RECURSIVE-LOCK-HELD")
  ;; Basic resources
  (:import-from
   "MINP"
   "ALLOCATE-RESOURCE" "CLEAR-RESOURCE" "DEALLOCATE-RESOURCE" "DEFRESOURCE"
   "MAP-RESOURCE" "USING-RESOURCE")
  ;; Standard definitions
  (:export
   "*MULTIPROCESSING-P*" "ALL-PROCESSES" "ALLOCATE-RESOURCE" "ATOMIC-DECF"
   "ATOMIC-INCF" "CLEAR-RESOURCE" "CURRENT-PROCESS" "DEALLOCATE-RESOURCE"
   "DEFGENERIC*" "DEFMETHOD*" "DEFRESOURCE" "DESTROY-PROCESS" "DISABLE-PROCESS"
   "ENABLE-PROCESS" "MAKE-LOCK" "MAKE-PROCESS" "MAKE-RECURSIVE-LOCK"
   "MAP-RESOURCE" "PROCESS-INTERRUPT" "PROCESS-NAME" "PROCESS-STATE"
   "PROCESS-WAIT" "PROCESS-WAIT-WITH-TIMEOUT" "PROCESS-WHOSTATE" "PROCESS-YIELD"
   "PROCESSP" "RESTART-PROCESS" "SETF*" "USING-RESOURCE" "WITH-LOCK-HELD"
   "WITH-RECURSIVE-LOCK-HELD" "WITHOUT-SCHEDULING")
  ;; Extensions
  (:export
   "MAKE-PROCESS-LOOP"		;portable way to create processes
   "PROCESS-ACTIVE-P"
   "PROCESS-DISABLE"		;really disable-process
   "PROCESS-ENABLE"		;really enable-process
   "PROCESS-KILL"		;really destroy-process
   "PROCESS-LOOP"		;portable non blocking loop macro
   "PROCESS-ITERATE"		;method to take-over process
   "PROCESS-PRESET"
   "PROCESS-RESTART"		;really restart-process
   "PROCESS-SLEEP"
   "UNWIND-PROCESS")		;portable unwind extended for mini process
  )
)
)
