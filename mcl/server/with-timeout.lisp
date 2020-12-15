; -*- Mode: Lisp; Package: CCL; -*- 

;;; Copyright John C. Mallery,  1999.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;;  WITH-TIMEOUT MACRO
;;;
;;; The implementation provides signalling even though some consing overhead is incurred because
;;;  it allows user code to handle the conditions form-timeout conditions within the body.
;;;
;;; This has an issue when there are nested calls to with-timeout in the case where 
;;; process-interrupts obstruct excuting unwind protect forms that are cleaning up
;;; a timeout that has already gone off. -- JCMa 8/8/1999.
;;;
;;; Issues:
;;; 
;;; 	1. Make the process associated with the form timer go away from the
;;; 	process list when the timer is inactive.
;;; 	
;;; 	2. Ensure that the timers go off on time.  
;;; 	This is important for small units, like a couple of seconds.
;;; 
;;; 	3. Consider using the task process facility with timers in order
;;; 	to speed up the start up of these processes.
;;; 
(in-package :ccl) 

#+ignore
(defun reset-timer-relative (timer seconds)
   "Resets TIMER to SECONDS from the time of invocation."
   (declare (fixnum seconds))
   (reset-timer-absolute timer (+ seconds (get-universal-time)))) 

;; get-time-words is in my sources but not in the upcoming release -- AKH 8/9/1999.
#+ppc-target
(defppclapfunction get-time-words ()
   ;(twnei nargs 0)
   (mflr loc-pc)
   (bla .spsavecontextvsp)
   ;(lwz nargs 331 rnil)
   ;(twgti nargs 0)
   (stwu sp -72 sp)
   (stw rzero 8 sp)
   ;(mr arg_z slep)
   (lwz arg_z '#.(get-shared-library-entry-point "LMGetTime") fn)
   (vpush arg_z)
   (lwz arg_z 0 vsp)
   (la vsp 4 vsp)
   (bla .spffcallslep)
   (rlwinm arg_z imm0 (+ 16 ppc::fixnum-shift) (- 16 ppc::fixnum-shift) (- 31 ppc::fixnum-shift))
   (vpush arg_z)  
   (rlwinm arg_z imm0 ppc::fixnum-shift (- 16 ppc::fixnum-shift) (- 31 ppc::fixnum-shift))
   (vpush arg_z)                     
   (set-nargs 2)
   (ba .spnvalret))

;; Here is reset-timer-relative that does not cons.
(defun reset-timer-relative (timer seconds)
   (declare (fixnum seconds))
   ;; like get-decoded-universal-time
   (rlet ((dt :longDateRec))
      (multiple-value-bind (time-zone dst) (get-time-zone)
          (declare (fixnum time-zone))
          (unless (eql dst 0) (decf time-zone 1))
          (multiple-value-bind (time-high time-low)(get-time-words)
              (let* ((seconds (+ seconds (* 3600 time-zone)))) ; <<
                 (declare (fixnum time-low time-high seconds))
                 (incf time-low seconds)
                 (while (>= time-low (expt 2 16))
                     (decf time-low (expt 2 16))
                     (incf time-high))
                 (when (< time-low 0)
                     (incf time-low (expt 2 16))
                     (decf time-high))
                 (decode-long-time time-high time-low dt)    
                 (let* ((second (rref dt longDateRec.second))
                           (minute (rref dt longDateRec.minute))
                           (hour (rref dt longDateRec.hour))
                           (date (rref dt longDateRec.day))
                           (month (rref dt longDateRec.month))
                           (year (rref dt longDateRec.year))
                           ;(day (mod (%i- (rref dt longDateRec.dayofweek) 2) 7))
                           (time (timer-universal-time timer)))
                    ;; like innards of reset-timer-absolute
                    (clear-timer timer)
                    (setf (%svref time 0) second
                             (%svref time 1) minute
                             (%svref time 2) hour
                             (%svref time 3) date
                             (%svref time 4) month
                             (%svref time 5) year)
                    (%reset-timer-absolute timer second minute hour date month year)))))))

(defstruct (form-timeout-args (:conc-name %form-timeout-))
   (state nil)                           ; :PENDING, :TIMED-OUT, :COMPLETE 
   (process nil)                       ; process to interrupt
   (timeout 0)                         ; current timeout in seconds
   (error-p nil)                       ; whether to signal an error on timeout
   (lock (make-lock))              ; lock prevents race conditions between completion of execution and timeout signalling
   (timer nil))                         ; timer associate with these args 

(declaim (inline %form-timer-args))

(defun %form-timer-args (timer)
   (first (timer-args timer)))

(define-condition form-timeout
                           (condition)
                           ((timer :initarg :timer :reader form-timeout-timer)))

(define-condition form-timed-out
                           (form-timeout)
                           ()
   (:report (lambda (condition stream)  
              (let* ((timer (form-timeout-timer condition))
                     (timeout (%form-timeout-timeout (%form-timer-args timer))))
                      (format stream "Timeout Signalled: Body took longer than ~D second(s) to complete." timeout))))
   (:documentation "This condition is used to signal timeouts for executing forms."))

(define-condition form-timeout-expired
                           (form-timeout error)
                           ()
   (:report (lambda (condition stream)
                   (let* ((timer (form-timeout-timer condition))
                             (timeout (%form-timeout-timeout (%form-timer-args timer))))
                      (format stream "Timeout Expired: Body took longer than ~D second(s) to complete." timeout))))
   (:documentation "This error is used to signal timeouts for executing forms.")) 

(defun invoke-timeout-action (args)
   (flet ((form-timeout-action (args)
               (if (%form-timeout-error-p args)
                  (error 'form-timeout-expired :timer (%form-timeout-timer args))
                  (signal 'form-timed-out :timer (%form-timeout-timer args)))))
      (with-lock ((%form-timeout-lock args))              ; avoid race conditions
          #+ignore(format *standard-output* "~&Processing timeout action.")
          (case (%form-timeout-state args)
             (:pending
               (setf (%form-timeout-state args) :timed-out)
               (process-interrupt (%form-timeout-process args) #'form-timeout-action args))
             (t nil)))))
                                               
(defun make-form-timeout  (resource)
  (declare (ignore resource))
  (let* ((args (make-form-timeout-args))
         (timer (create-timer-call 'invoke-timeout-action (list args) :name "Form Timeout Timer")))
    (setf (%form-timeout-timer args) timer)
    timer))
   
(defun deinitialize-form-timeout (resource timer)
   (declare (ignore resource))
   (let ((args (%form-timer-args timer)))
      (with-lock ((%form-timeout-lock args))     ; avoid race conditions
          (clear-timer timer)
          (setf (%form-timeout-state args) :complete
                   (%form-timeout-process args) nil)))
   timer) 

(resources:defresource form-timeout-timers ()
   :constructor make-form-timeout
   :deinitializer deinitialize-form-timeout) 

(defun with-timeout-internal (timeout continuation &optional error-p)
   (resources:using-resource (timer form-timeout-timers)
      (flet ((initialize-form-timeout (args timeout error-p)
                  (setf (%form-timeout-timeout args) timeout
                           (%form-timeout-error-p args) error-p
                           (%form-timeout-process args) *current-process*
                           (%form-timeout-state args) :pending))
               (handle-timeout-return (condition)
                  (let* ((current-timer (form-timeout-timer condition)))
                     (when (eql timer current-timer)
                         (return-from with-timeout-internal nil))
                     nil)))
         (declare (inline initialize-form-timeout)
                       (dynamic-extent #'handle-timeout-return))
         (let ((args (%form-timer-args timer)))
            (initialize-form-timeout args timeout error-p)        ; initialize timeout timer
            (handler-bind
               ((form-timed-out #'handle-timeout-return))
               (reset-timer-relative timer timeout)
               (multiple-value-prog1 
                   (funcall continuation)          ; run time bounded form
                   (setf (%form-timeout-state args) :complete)))))))       ; set complete state as soon as possible

(defmacro with-timeout ((timeout &key error-p) &body body)
   "Executes BODY and returns the values of the last form in BODY. However, if
the execution takes longer than TIMEOUT seconds, abort it. If :ERROR-P is
unsupplied or false, just return nil. If :ERROR-P is true, signal an error."
   `(flet ((do-body () ,.body))
       (declare (dynamic-extent #'do-body))
       (with-timeout-internal,timeout #'do-body ,error-p))) 

(export '(with-timeout form-timeout-expired form-timed-out) :ccl) 

;;;------------------------------------------------------------------- 
;;;
;;; TESTING CODE
;;;

#|
(resources:clear-resource form-timeout-timers)

(defun funcall-with-timeout (function seconds &key error-p)
   (with-timeout (seconds :error-p error-p)
       (funcall function))) 

(funcall-with-timeout #'(lambda () (loop for count upfrom 0 do (format t "~&round: ~D" count) (sleep .5)))
  2)

(funcall-with-timeout #'(lambda () (message-dialog "go away")) 2)       ; wait for it
(funcall-with-timeout #'(lambda () (message-dialog "go away")) 100)     ; click OK
(funcall-with-timeout #'(lambda () (message-dialog "go away"))

(defun test ()
  (with-timeout (3)
    (loop for i from 0
          do (print i)
          do (sleep 0.5))))

(test)

|#
