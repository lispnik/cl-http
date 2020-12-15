;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-

(defclass START-task-queue (tq::task-queue)
    ((tq::process-name :initform "START Task Queue" :allocation :class)
     (tq::wait-whostate :initform "Task Wait" :allocation :class))
  (:documentation "Need a task queue for START because due to use of dynamic programming
constructs (global variables, property lists), only one START query can be
processed at a time.  Also, only one entry at a time can be written to a
log file."))


(defvar *A-task-queue* (tq::ensure-active-task-queue 'START-task-queue))

(defun calling-test-queue (num)
  ;; Because it is dangerous to process more
  ;; than one request at a time (because
  ;; START modifies global variables and
  ;; properties), use a task queue to ensure
  ;; that queries are processed one at a time
  ;; and in the order in which they were
  ;; received.  -- 17 Mar 1998, sfelshin
  ;;
  ;; First, set up (a unique identifier for)
  ;; this task so we can tell when it's done.
  (let ((done-yet nil))
    ;; Next, push our task.
    (tq::push-task-queue
      *A-task-queue*
      #'(lambda (task-queue)
	  (format nil "~D was procesed!" num)
	  (setq done-yet t)))
    (tq::pop-task-queue *A-task-queue*)
    ;; Finally, wait until our task has been
    ;; completed.  Must not exit this function
    ;; until the task is executed because when
    ;; we exit the code to process the WWW
    ;; request, CL-HTTP will close the HTTP
    ;; stream.
    (loop until (eq done-yet t))
    (format nil "Exiting ~D" num)))

(calling-test-queue 1)

