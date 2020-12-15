;;; -*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; (c) Copyright  1997-99, John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; GENERIC TASK QUEUE FACILITY
;;;
;;; Branched off cml:cml-utils;task-queue.lisp.7 8/25/97 -- JCMa.

(defpackage task-queue
  (:nicknames tq)
  (:use future-common-lisp)
  (:import-from "WWW-UTILS"
   "MAKE-LOCK"
   "MAKE-PROCESS"
   "PROCESS-ACTIVE-P"
   "PROCESS-DISABLE"
   "PROCESS-ENABLE"
   "PROCESS-KILL"
   "PROCESS-PRESET"
   "PROCESS-PRIORITY"
   "PROCESS-WAIT"
   "PROCESS-WHOSTATE"
   "WITH-LOCK-HELD")
  (:export
    "CLEAR-TASK-QUEUE" 
    "ENSURE-ACTIVE-TASK-QUEUE"
    "POP-TASK-QUEUE"
    "PUSH-TASK-QUEUE"
    "START-TASK-QUEUE"
    "STOP-TASK-QUEUE"
    "TASK-QUEUE"
    "TASK-QUEUE-EXECUTE-PENDING-TASKS"
    "TASK-QUEUE-EXECUTE-TASK"
    "TASK-QUEUE-NEXT"
    "TASK-QUEUE-PROCESS-KILL"
    "TASK-QUEUE-PROCESS-NAME"
    "TASK-QUEUE-RUN-P"
    "TASK-QUEUE-WAIT-WHOSTATE"))

(in-package :task-queue)


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defclass task-queue
          ()
    ((queue :initform nil :accessor task-queue-queue)   ;queue containing the entries
     (pointer :initform nil :accessor task-queue-pointer)       ;pointer to the last cons in the queue
     (n-pending-tasks :initform 0 :accessor task-queue-pending-tasks)   ;number of incomplete tasks
     (run-p :initform t :accessor task-queue-run-p)     ;return non-null when the process should run.
     (lock :initform nil :initarg :lock :accessor task-queue-lock)
     (process-priority :initform 0 :initarg :process-priority :accessor task-queue-process-priority)
     (process :initform nil :accessor task-queue-process)       ;process running the queue
     (process-name :initform "Task Queue" :initarg :process-name :accessor task-queue-process-name)
     (wait-whostate :initform "Task Wait" :initarg :wait-whostate :accessor task-queue-wait-whostate))
  (:documentation "A mixin that executes task entries with a separate process."))

(defmethod print-object ((task-queue task-queue) stream)
  (print-unreadable-object (task-queue stream :type t :identity t)
    (write-string (task-queue-process-name task-queue) stream)))

(defmethod initialize-instance :after ((task-queue task-queue) &key)
  (setf (task-queue-lock task-queue) (make-lock (task-queue-process-name task-queue) :type :simple))
  task-queue)

(defmethod (setf task-queue-process-priority) :after ((process-priority integer) (task-queue task-queue))
  (with-slots (process) task-queue
    (when process
      (setf (process-priority process) process-priority))))

(defgeneric push-task-queue (task-queue task)
  (:documentation "Push an entry onto task-queue with inter-process locking."))

(defmethod push-task-queue ((task-queue task-queue) task)
  (with-slots (lock queue pointer n-pending-tasks) task-queue 
    (let ((entry (list task)))
      (with-lock-held (lock :write "Task Queue Push")
        (if queue
            (setf (cdr pointer) entry)
            (setq queue entry))
        (setq pointer entry)
        (incf (the fixnum n-pending-tasks))))))

(defgeneric pop-task-queue (task-queue)
  (declare (values first-entry))
  (:documentation "Pops an entry off the queue with inter-process locking."))

(defmethod pop-task-queue ((task-queue task-queue) &aux entry)
  (with-slots (lock queue pointer n-pending-tasks) task-queue
    (with-lock-held (lock :write "Task Queue Pop")
      (when (setq entry (pop queue))
        (decf (the fixnum n-pending-tasks))
        (unless queue
          (setq pointer nil))))
    entry))

(defgeneric task-queue-next (task-queue)
   (declare (values first-entry))
   (:documentation "Gets the first without removing it from the queue  with inter-process locking."))

(defmethod task-queue-next ((task-queue task-queue))
  (with-lock-held ((task-queue-lock task-queue) :read "Task Queue Next")
    (car (task-queue-queue task-queue))))

(defgeneric clear-task-queue (task-queue)
  (declare (values flushed-queue))
  (:documentation "Clears all entries from the task queue with inter-process locking."))

(defmethod clear-task-queue ((task-queue task-queue))
  (with-slots (lock queue pointer n-pending-tasks) task-queue
    (with-lock-held (lock :write "Task Queue Clear")
      (prog1 queue
             (setq queue nil
                   pointer nil
                   n-pending-tasks 0)))))

(defgeneric task-queue-pending-tasks-p (task-queue)
  (:documentation "Returns non-null when there are pending tasks and the process is active."))

(defmethod task-queue-pending-tasks-p ((task-queue task-queue))
  (with-slots (queue run-p) task-queue
    (and queue run-p)))

(defgeneric task-queue-waiting-p (task-queue)
  (:documentation "Returns non-null if TASK is in a wait state rather than executing a task."))

(defmethod task-queue-waiting-p ((task-queue task-queue))
  (with-slots (process wait-whostate) task-queue
    (equalp (process-whostate process) wait-whostate)))

(defgeneric task-queue-execute-task (task-queue task)
  (:documentation "Execute a single task. Specialize this as needed for applications."))

(defmethod task-queue-execute-task ((task-queue task-queue) task)
  (funcall task task-queue))

(defgeneric task-queue-execute-pending-tasks (task-queue)
  (:documentation "Executes enqueued tasks."))

(defmethod task-queue-execute-pending-tasks ((task-queue task-queue))
  (loop for task = (pop-task-queue task-queue)
        while task
        do (task-queue-execute-task task-queue task)
        while (task-queue-run-p task-queue)))

(defgeneric task-queue-main-loop (task-queue)
  (:documentation "The main loop for executing tasks.
Waits until tasks are enqueued."))

(defmethod task-queue-main-loop ((task-queue task-queue))
  (loop with wait-whostate = (task-queue-wait-whostate task-queue)
        doing (process-wait wait-whostate #'task-queue-pending-tasks-p task-queue)
              (task-queue-execute-pending-tasks task-queue)))

(defgeneric start-task-queue (task-queue)
  (:documentation "Starts TASK-QUEUE executing tasks by activing its process."))

(defmethod start-task-queue ((task-queue task-queue))
  (with-slots (process process-name) task-queue
    (cond (process
           (process-preset process #'task-queue-main-loop task-queue)
           (process-enable process))
          (t (setq process (make-process process-name
					 :background-p t
                                         :priority (task-queue-process-priority task-queue)
                                         :restart-after-reset t
                                         :warm-boot-action :delayed-restart))
             (process-preset process #'task-queue-main-loop task-queue)
             (process-enable process)))
    (setf (task-queue-run-p task-queue) t)
    process))

(defgeneric task-queue-process-kill (task-queue)
  (:documentation "Stops the task queue process and kills it."))

(defmethod task-queue-process-kill ((task-queue task-queue))
  (with-slots (process) task-queue
    (when process
      (stop-task-queue task-queue)
      (prog1 (process-kill process)
             (setq process nil)))))

(defgeneric stop-task-queue (task-queue)
  (:documentation "Stops TASK-QUEUE queue from executing task by stopping its process."))

;; specialize this method to perform clean up activity.
(defmethod stop-task-queue ((task-queue task-queue))
  (declare (values task-queue)))

(defmethod stop-task-queue :around ((task-queue task-queue))
  (with-slots (process) task-queue
    (when process
      ;; set a flag tell the process to shutdown
      (setf (task-queue-run-p task-queue) nil)
      ;; wait until the shutdown is complete
      (process-wait "Task Queue Shutdown" #'task-queue-waiting-p task-queue)
      (call-next-method)
      ;; disable the processes run reasons
      (process-disable process)
      process)))

(declaim (inline %task-queue-active-p))

(defun %task-queue-active-p (process-queued-task)
  (let ((process (task-queue-process process-queued-task)))
    (and process (process-active-p process))))

(defgeneric task-queue-active-p (task-queue)
  (:documentation "Returns no-null whe the task queues process is active."))

(defmethod task-queue-active-p ((task-queue task-queue))
  (%task-queue-active-p task-queue))

(defgeneric ensure-active-task-queue (task-queue)
  (declare (values task-queue))
  (:documentation "Ensures that task-queue is active and executing tasks."))

(defmethod ensure-active-task-queue ((task-queue task-queue))
  (prog1 task-queue
         (or (task-queue-active-p task-queue)
             (start-task-queue task-queue))))

(defmethod ensure-active-task-queue ((task-queue-class symbol))
  (ensure-active-task-queue (make-instance task-queue-class)))


;;;------------------------------------------------------------------- 
;;;
;;; TESTING
;;;

#|

(setq tq (ensure-active-task-queue 'task-queue))

(defparameter *task-number* 0)

(defun task (tq)
  (declare (ignore tq))
  (format tv:initial-lisp-listener "~&Task: ~D" (incf *task-number*)))

(loop for i from 1 to 100
      do (push-task-queue tq #'task))

|#
