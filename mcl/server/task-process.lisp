;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; task-process.lisp
;;; Copyright 1996 Digitool, Inc. 
;;;

;;; A process that can be assigned a task.
;;; When it's done with the task it goes to sleep until
;;; assigned another task.
;;; This allows you to have a queue of ready-to-run processes.
;;; Starting one up takes very little time.

;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modification History
;;;
;;; 05/28/99 jcma Process-poll* replaces process-wait in TASK-PROCESS-PRESET and TASK-PROCESS-RUN-FUNCTION.
;;;                          TASK-PROCESS-RUN-FUNCTION now checks process to make sure it is not already unblocked,
;;;                          which works around a bug that should be fixed.
;;; 11/28/96 bill   New file
;;;

(in-package :ccl)

(export '(task-process-state
          task-process-idle-p
          make-task-process
          task-process-run-function
          task-process-kill))

#|

A task process is designed to be process-block'ed while it
waits for something to do. You give it something to do
with task-process-run-function. You should not process-disable
or process-preset a task process. If you process-reset it, it
will return to :idle state. If you have a queue of idle task processes,
you can start up a task in much less time than if you were to
process-run-function (or make-process & process-preset). Giving
an idle task process a task also doesn't cons.

make-task-process name &rest make-process-keys
  Create a task process.
  name & make-process-keys are arguments for make-process.
  Starts up the process and returns when it is in the :idle state.

task-process-run-function process function &rest args
  Assign a task to a task process.
  process is either a task-process as returned by make-task-process,
    NIL, or a string.
    If NIL, makes a new process with a computed name.
    If a string, makes a new process with that name.
  function is a valid first argument to APPLY.
  args are arguments to function.
  Gets the process in the :idle state, using process-reset if necessary.
  Causes it to apply function to args.
  Returns process or the newly created process.

task-process-state process
  Returns a keyword denoting the state of the given task process.
  NIL means it isn't really a task process.
  :dead means its exhausted.
  :idle means its blocked waiting for a task to run.
  :running means its running a task.
  There are a few other states that happen for short times.

task-process-idle-p process
  (eq :idle (task-process-state process))

task-process-kill process
  You can't just process-kill a task process since if it's idle,
    it is process-block'ed.
  task-process-kill does what is necesssary to actually kill the process.

|#

(defvar *task-process-state* nil)
(defvar *task-process-function*)
(defvar *task-process-args*)

(defun task-process-loop ()
   (let ((tag (process-reset-tag *current-process*))
           (*task-process-state* :startup)
           (*task-process-function* nil)
           (*task-process-args* nil)
           function args done)
      (flet ((doit ()
                  (without-interrupts
                    (setq *task-process-state* :idle)
                    (process-block *current-process* "Idle")
                    (setq *task-process-state* :starting)
                    (setq function *task-process-function*
                             args *task-process-args*
                             *task-process-function* nil
                             *task-process-args* nil))
                  (unwind-protect
                     (when function
                         (setq *task-process-state* :running)
                         (apply function args))
                     (setq *task-process-state* :finished)
                     (cheap-free-list args))))
         (declare (dynamic-extent #'doit))
         (loop
            (block party
               (unwind-protect
                  (let ((kill (catch tag 
                                    (doit)
                                    (return-from party))))
                     (when kill
                         (setq done t)
                         (throw tag kill)))
                  ; Don't let anybody throw out except process-kill.
                  (unless done
                     (return-from party))))))))

(defun task-process-state (process)
   (without-interrupts
     (if (process-exhausted-p process)
        :dead
        (symbol-value-in-process '*task-process-state* process))))

(defun task-process-idle-p (process)
   (eq :idle (task-process-state process)))

(defun task-process-running-p (process)
   (eq :running (task-process-state process)))

(defvar *task-process-population*
   (%cons-population nil))

(defun make-task-process (name &rest make-process-keys)
   (declare (dynamic-extent make-process-keys))
   (let* ((process (apply 'make-process name make-process-keys)))
      (task-process-preset process)))

(defun task-process-preset (process)
   (unless (process-exhausted-p process)
      (error "Attempt to preset non-exhausted ~s" process))
   (process-preset process #'task-process-loop)
   (pushnew process (population-data *task-process-population*) :test 'eq)
   (process-enable process)
   (process-poll "Task process startup" #'(lambda (process) (task-process-idle-p process)) process)
   process)

(defvar *task-process-count* 0)

(defun task-process-run-function (process function &rest args)
   (declare (dynamic-extent args))
   (when (or (null process) (stringp process))
       (setq process 
                (make-task-process (or process
                                                     (format nil "Task process ~d"
                                                                  (incf *task-process-count*))))))
   (let ((already-reset-p nil))
      (loop
         (without-interrupts
           (let ((state (task-process-state process)))
              (case state
                 (:dead (task-process-preset process))
                 (:idle (return))
                 (:running (unless already-reset-p
                                   (process-reset process)
                                   (setq already-reset-p t))))
              (suspend-current-process)))))
   (setf (symbol-value-in-process '*task-process-function* process) function
            (symbol-value-in-process '*task-process-args* process) (cheap-copy-list args))
   ;; Patch around bug whereby process is somehow already unblocked when reaching here. -- JCMa 5/28/1999.
   (when (memq process *blocked-processes*)
       (process-unblock process))
   (process-poll "Task Process running"
                        #'(lambda (p)
                              (memq (task-process-state p) '(:running :idle)))
                        process)
   process)

; Just like process-kill, but ensures that the process runs so
; that it has a chance to die.
(defun task-process-kill (process)
   (without-interrupts
     (unless (process-exhausted-p process)
        (ensure-process-active process)
        (process-kill process))))

(defun kill-task-processes ()
   (let (process)
      (loop
         (without-interrupts
           (unless (population-data *task-process-population*)
              (return))
           (setq process (pop (population-data *task-process-population*))))
         (task-process-kill process))))

(pushnew 'kill-task-processes *lisp-cleanup-functions*)
