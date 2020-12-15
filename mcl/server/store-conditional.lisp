;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: ccl; Base: 10 -*-

; store-conditional.lisp
;
; Patterned after the symbolics store-conditional.
; Since MCL doesn't have locatives, the first arg must be
; a lock and the lock value is the cell that is tested and set.

(eval-when (:compile-toplevel :execute)
  (require "LISPEQU"))                  ; lock.value

(in-package :ccl)

#+ppc-target
(eval-when (:compile-toplevel :execute :load-toplevel)

(define-ppc-compiler-macro lockp (lock)
  `(eq ppc::subtag-lock (ppc-typecode ,lock)))

)

; This will need a without-interrupts if we ever go to a preemptive scheduler
(defun store-conditional (lock old new)
  (unless (typep lock 'lock)
    (setq lock (require-type lock 'lock)))
  (locally (declare (type lock lock))
    (when (eq (lock.value lock) old)
      (setf (lock.value lock) new)
      t)))

; IBID on the without-interrupts
(define-compiler-macro store-conditional (&whole whole &environment env
                                                 lock old new)
  (if (nx-form-typep lock 'lock env)
    (let ((lock-var (gensym))
          (old-var (gensym))
          (new-var (gensym)))
      `(let ((,lock-var ,lock)
             (,old-var ,old)
             (,new-var ,new))
         (when (eq (lock.value ,lock-var) ,old-var)
           (setf (lock.value ,lock-var) ,new-var)
           t)))
    whole))

(define-compiler-macro process-lock (&whole whole &environment env
                                            lock &optional
                                            lock-value (whostate "Lock")
                                            interlock-function)
  (if (and (null interlock-function)
           (nx-form-typep lock 'lock env))
    (let ((lock-var (gensym))
          (lock-value-var (gensym)))
      `(let ((,lock-var ,lock)
             (,lock-value-var ,(or lock-value '*current-process*)))
         (declare (type lock ,lock-var))
         (if (store-conditional ,lock-var nil ,lock-value-var)
           ,lock-value-var
           (if (eq ,lock-value-var (lock.value ,lock-var))
             ,lock-value-var
             (funcall 'process-lock ,lock-var ,lock-value-var ,whostate)))))
    whole))

(define-compiler-macro process-unlock (&whole whole &environment env
                                              lock &optional lock-value (error-p t))
  (if (nx-form-typep lock 'lock env)
    (let ((lock-var (gensym))
          (lock-value-var (gensym)))
      `(let ((,lock-var ,lock)
             (,lock-value-var ,(or lock-value '*current-process*)))
         (declare (type lock ,lock-var))
         (unless (store-conditional ,lock-var ,lock-value-var nil)
           (funcall 'process-unlock ,lock-var ,lock-value-var ,error-p))))
    whole))

(defmacro with-lock-grabbed ((lock &optional (lock-value '*current-process*)
                                   (whostate "Lock")) &body body)
  (let ((flag (gensym))
        (setter (gensym))
        (lock-var (gensym))
        (lock-value-var (gensym)))
    `(let* ((,flag nil)
            (,setter #'(lambda () (setq ,flag t)))
            (,lock-var ,lock)
            (,lock-value-var ,lock-value))
       (declare (dynamic-extent ,setter))
       (unwind-protect
	   (progn
	     (process-lock ,lock-var ,lock-value-var ,whostate ,setter)
	     ,@body)
         (when ,flag
           (process-unlock ,lock-var ,lock-value-var))))))
