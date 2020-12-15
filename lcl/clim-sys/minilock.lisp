;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: MINP; Base: 10 -*-

;;; Copyright (C) 1994, 1995 Olivier (OBC) all rights reserved.
;;; See copyright notice in MINIPROC file.

(in-package "MINP")

#+ignore
(defpackage "MINP"
  (:use)
  (:export
   "MAKE-LOCK" "MAKE-RECURSIVE-LOCK"
   "WITH-LOCK-HELD" "WITH-RECURSIVE-LOCK-HELD"))

;;; WITH-LOCK-HELD and friends is actually important
;;; even with MINIPROC because it underlines the need
;;; to provide constructs for lengthy computations that
;;; don't overlap with others over limited resources.
;;;

(defclass lock ()
  ((name :initarg :name :accessor lock-name)
   (owner :initform nil :accessor lock-owner))
  (:documentation "A lock that can only be owned by one process and only once."))

(defmethod print-object ((lock lock) stream)
  (format stream "#<~S ~A #x~X>"
	  (type-of lock)
	  (if (slot-boundp lock 'name)
	      (lock-name lock)
	    "(no name)")
	  #+Allegro
	  (excl::pointer-to-address lock)
	  #-Allegro
	  0))

(defclass recursive-lock (lock)
  ()
  (:documentation "A lock that can be safely seized multiple times by the same process."))

(defgeneric seize-lock (lock)
  (:documentation "Seize the lock and return it if successful."))

(defgeneric release-lock (lock &optional #-ACLPC (error-p t) #+ACLPC error-p)
  (:documentation
   "Release the lock if it is owned by the current process.
If ERROR-P signal an error if if fails."))

(defmethod check-lock-owner ((lock recursive-lock) (process mini-process))
  (eql (lock-owner lock) process))

(defmethod check-lock-owner ((lock lock) (process mini-process))
  (when (eql (lock-owner lock) process)
    (error "Can't seize ~A because its process already owns it." lock)))

(defmethod process-seize-lock ((process mini-process) (lock lock))
  (or (check-lock-owner lock process)
      (if (null (lock-owner lock))
	  (setf (lock-owner lock) process))))

(defmethod process-release-lock ((process mini-process) (lock lock))
  (if (eql (lock-owner lock) process)
      (not (setf (lock-owner lock) nil))))

;;; Note that LEVEL (a number) is provided as an extension
;;; to CLIM-SYS to provide ordered locks that prevent certain
;;; kinds of dead-locks.
;;;
(defun make-lock (&optional lock-name level recursive)
  (if recursive
      (make-recursive-lock lock-name level)
    (if level
	(make-instance 'ordered-lock :name lock-name :level level)
      (make-instance 'lock :name lock-name))))

;;; Not sure what recursive locks really meant initially n CLIM-SYS,
;;; however here a recursive lock can be locked on multiple times by the
;;; same process.
;;;
(defun make-recursive-lock (&optional lock-name level)
  (if level
      (make-instance 'ordered-recursive-lock :name lock-name :level level)
    (make-instance 'recursive-lock :name lock-name)))

(defmacro with-lock-held ((lock &optional whostate recursive) &rest body)
  `(let ((#1=#:lock ,lock)
	 (#2=#:whostate ,whostate)
	 (#3=#:recursive ,recursive))
     (if (typep #1# 'recursive-lock) 
	 (or #3# (error "~A is a recursive lock." #1#))
       (and #3# (error "~A is not a recursive lock." #1#)))
     (check-lock-owner #1# *current-process*)
     #+debug (format t "~&Add ~S completion 1.~%" *current-process*)
     (unwind-process
      (process-wait
       "Seizing lock"
       #'(lambda ()
	   (without-scheduling	;Protect secondary methods too
	     (process-seize-lock *current-process* #1#))))
      #+debug (format t "~&Run ~A completion 1.~%" *current-process*)
      (if #2#
	  (rotatef (process-whostate *current-process*) #2#))
      #+debug (format t "~&Add ~A completion 2.~%" *current-process*)
      (unwind-process
       (progn ,@body)
       (if #2#
	   (rotatef (process-whostate *current-process*) #2#))
       #+debug (format t "~&Run ~A completion 2.~%" *current-process*)
       (or (without-scheduling	;Protect secondary methods too
	     (process-release-lock *current-process* #1#))
	   #3#
	   (error "~A is not owned by the current process. Can't release lock."
		  #1#))))
     ;; Be nice
     (process-yield)))

(defmacro with-recursive-lock-held ((lock &optional whostate (recursive t)) &rest body)
  `(with-lock-held (,lock ,whostate :recursive ,recursive) ,@body))

;;; From Sonya A. Keene's book.
;;; This was edited to provide better protection from using
;;; without-interrupts (eventhough this is not require for MINIPROC).
;;; In a true MP system the implementation originally published may
;;; not work 100%. There are cases where processes could have
;;; been interrupted while releasing seizing or releasing a lock...

;;; Use this mixin when you want to avoid the dining philosophers
;;; reaching for butter dead-lock situation.
;;;
(defclass ordered-lock-mixin ()
  ((level :initarg :level :reader lock-level :type integer))
  (:documentation "Avoids deadlock at run time by checking lock order."))

(defmethod print-object ((lock ordered-lock-mixin) stream)
  (format stream "#<~S ~A ~A #x~X>"
	  (type-of lock)
	  (if (slot-boundp lock 'name)
	      (lock-name lock)
	    "(no name)")
	  (if (slot-boundp lock 'level)
	      (lock-level lock)
	    "?")
	  #+Allegro
	  (excl::pointer-to-address lock)
	  #-Allegro
	  0))

(defclass ordered-lock (ordered-lock-mixin lock)
  ()
  (:documentation
   "Avoids deadlock by ensuring that a process seizes locks in a specific order."))

;;; Built on top of recusive lock, you can seize the same lock
;;; multiple times, but you will have to releaze it the same number of times.
;;;
(defclass ordered-recursive-lock (ordered-lock-mixin recursive-lock)
  ()
  (:documentation
   "Avoids deadlock by ensuring that a process seizes locks in a specific order.
But allow same lock to be seized multiple times as long as the order is preserved."))

(defun make-ordered-lock (name level &key recursive)
  (make-instance (if recursive
		     'ordered-recursive-lock
		   'ordered-lock)
    :name name :level level))

(defmethod process-add-lock ((process mini-process) (lock ordered-lock-mixin))
  (without-scheduling
    (with-slots (locks) process
      (push lock locks))))

(defmethod process-delete-lock ((process mini-process) (lock ordered-lock-mixin))
  (without-scheduling
    (with-slots (locks) process
      (setf locks (delete lock locks :count 1)))))

(defmethod process-seize-lock :before ((process mini-process) (lock ordered-lock-mixin))
  "Check if the current process can seize this lock based
on the highest lock level it currently owns - if any."
  (or (check-lock-owner lock process)
      ;; The first lock if any is the higher order lock by construction
      (with-slots (locks) process
	(and locks
	     (<= (lock-level lock) (lock-level (first locks)))
	     (error "Locks would be out of order: Can't seize ~A while owning ~A."
		    lock (first locks))))))

(defmethod process-seize-lock :after ((process mini-process) (lock ordered-lock-mixin))
  "Add lock to current process lock list"
  (process-add-lock process lock))

(defmethod process-release-lock :after ((process mini-process) (lock ordered-lock-mixin))
  (process-delete-lock process lock))
