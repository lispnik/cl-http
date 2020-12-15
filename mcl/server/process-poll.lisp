;;; process-poll.lisp
;;;
;;; process-poll & process-poll-with-timeout are identical to
;;; process-wait & process-wait-with-timeout except the wait function
;;; is always called in the waiting process instead of in the
;;; scheduler. This means it will see the process'es special bindings.
;;; It also means that the wait function can call process-wait or
;;; process-poll.
;;;
;;; If you set ccl:*always-process-poll-p* true, then process-wait will use
;;; process-poll instead of calling the wait function in the scheduler.
;;; This slows the scheduler a little, but will fix code that is
;;; currently erroring due to a recursive process-wait call.

;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modification History
;;;
;;; 03/07/97 bill  modify deactivate-process so a blocked process can't get scheduled
;;; 11/21/96 gb    make process-wait save/restore process.whostate.
;;; 10/29/96 bill  New file.
;;;                Tested in MCL 4.0, 3.9p1, 3.1, 3.0.
;;;

#|

; This form errors in 3.1 & 4.0 and causes a random process to block in 3.0 & 3.9
; (Before the patch. After the patch it errors in all 4).
(process-wait "Error"
              (let ((not-first-time nil))
              #'(lambda ()
                  (sleep 1)
                  (prog1 not-first-time
                    (setq not-first-time t)))))

; This works fine:
(process-poll "Error"
              (let ((not-first-time nil))
              #'(lambda ()
                  (sleep 1)
                  (prog1 not-first-time
                    (setq not-first-time t)))))

; This also works fine:
(let ((*always-process-poll-p* t))
  (process-wait "Error"
                (let ((not-first-time nil))
                  #'(lambda ()
                      (sleep 1)
                      (prog1 not-first-time
                        (setq not-first-time t))))))
|#

(in-package :ccl)

(export '(process-poll
          process-poll-with-timeout
          *always-process-poll-p*))

; This is for 3.0 and 3.9
(eval-when (:compile-toplevel :execute)

(unless (fboundp 'recursive-process-wait)
  (declaim (ftype (function *) recursive-process-wait %process-wait-p bug)
           (special *in-process-wait-function*)))

)  ; end of eval-when

; This is for 3.0 and 3.9
(unless (fboundp 'recursive-process-wait)

(defvar *in-process-wait-function* nil)

(defun recursive-process-wait (process)
  (if (eq process *current-process*)
    (error "process-wait called from inside a process-wait function")
    (process-interrupt process
                       #'(lambda (process)
                           (let ((*in-process-wait-function* nil))
                             ; try to signal the error in context
                             (%process-wait-p process)
                             ; didn't signal in context, so signal out of context
                             (recursive-process-wait process)))
                       process))
  (throw '%process-wait-p nil))

(defun bug (&optional message)
  (if message
    (with-pstrs ((msg message))
      (#_DebugStr msg))
    (#_Debugger)))

)  ; end of unless


(defvar *process-polling-p* nil)

(defvar *always-process-poll-p* nil)

(defvar *active-processes-tail* nil)

(declaim (type t
               *always-process-poll-p*
               *process-polling-p*
               *active-processes-tail*))

(defun clear-process-polling ()
  (without-interrupts
   (when *process-polling-p*
     (setq *active-processes-tail* nil)
     (dolist (p *active-processes*)
       (when (eq (process.wait-function p) :polled)
         (setf (process.wait-function p) :polling)))
     (setq *process-polling-p* nil))))

(defun process-poll (whostate function &rest args)
  (declare (dynamic-extent args))
  (or (apply function args)
      (let* ((p *current-process*))
        (if *in-process-wait-function*
          (recursive-process-wait *in-process-wait-function*)
          (unwind-protect
            (progn
              (setf (process.wait-function p) :polling)
              (loop
                (suspend-current-process whostate)
                (when (apply function args)
                  (clear-process-polling)
                  (return))
                (setf (process.wait-function p) :polled
                      *process-polling-p* t
                      *active-processes-tail* (cdr (memq p *active-processes*)))))
            (setf (process.wait-function p) nil
                  (process.wait-argument-list p) nil)))))
  nil)

(defun process-poll-with-timeout (whostate time function &rest args)
  (declare (dynamic-extent args))
  (cond ((null time)  (apply #'process-wait whostate function args) t)
        (t (let* ((win nil)
                  (when (%tick-sum (get-tick-count) time))
                  (f #'(lambda () (if (apply function args) 
                                    (setq win t)
                                    (> (%tick-difference (get-tick-count) when) 0)))))
             (declare (dynamic-extent f))
             (process-poll whostate f)
             win))))

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

(defun %process-wait-p (process &optional 
                                (wf (process.wait-function process))
                                (args (process.wait-argument-list process)))
  (cond ((null wf) 
         (clear-process-polling)
         t)
        ((eq wf :polling) t)
        ((eq wf :polled) nil)
        (t (catch '%process-wait-p
             (let* ((*in-process-wait-function* process)
                    (old-debugger-hook *debugger-hook*)
                    (*debugger-hook* #'(lambda (condition hook)
                                         (declare (ignore hook))
                                         (if (eq process *current-process*)
                                           (let ((*in-process-wait-function* nil))
                                             (if old-debugger-hook
                                               (funcall old-debugger-hook condition old-debugger-hook)
                                               (error condition)))
                                           (process-interrupt process
                                                              #'(lambda (process wait-function wait-args)
                                                                  ; Hope to signal the error in context
                                                                  (%process-wait-p process wait-function wait-args)
                                                                  ; Didn't signal in context, so signal out of context
                                                                  (error "Error inside process-wait function"))
                                                              process wf args))
                                         (throw '%process-wait-p nil))))
               (declare (dynamic-extent *debugger-hook*))
               (when (apply wf args)
                 (clear-process-polling)
                 t))))))

(defun scheduler ()
  (let-globally ((*in-scheduler* t))
    (if (null *current-process*) (bug "*current-process* is nil in scheduler"))
    ; give first preference to the guy who is processing events or who doesnt
    ; want event processing to happen quite yet.    
    (let ((pe *processing-events*))
      (when (and pe 
                 (neq pe t)
                 (not (%process-blocked-p pe))
                 (%process-wait-p pe))
        (return-from scheduler (%activate-process pe)))
      (loop
        (let ((active-processes-tail *active-processes-tail*))
          (if active-processes-tail
            (setq *active-processes-tail* nil)
            (setq active-processes-tail *active-processes*))
          (dolist (p active-processes-tail)
            (unless (%process-blocked-p p)
              (when (%process-wait-p p)
                (return-from scheduler (%activate-process p))))))
        ; Nobody ready to run, must be idle so poll for events
        (clear-process-polling)
        (event-poll)))))

(defun process-wait (whostate function &rest args)
  (declare (dynamic-extent args))
  (if *always-process-poll-p*
    (apply 'process-poll whostate function args)
    (or (apply function args)
        (let* ((p *current-process*)
               (old-whostate (process.whostate p)))
          (if *in-process-wait-function*
            (recursive-process-wait *in-process-wait-function*)
            (unwind-protect
              (progn
                (setf (process.wait-function p) (require-type function 'function)
                      (process.wait-argument-list p) args)
                (suspend-current-process whostate))
              (setf (process.wait-function p) nil
                    (process.wait-argument-list p) nil
                    (process.whostate p) old-whostate))))))
  nil)

; process-block-patch.lisp
;
; Make sure a blocked process can't get scheduled
; by ensuring that it is removed from *active-processes-tail*

(defun deactivate-process (p)
  (without-interrupts
   (when (eq p (car *active-processes-tail*))
     (setq *active-processes-tail* (cdr *active-processes-tail*)))
   (setf *active-processes* (delete p *active-processes*)
         (cdr (process.splice p)) nil)))

)
