;;;-*- Mode: Lisp; Package: CCL -*-
;;
;; LOCKS WITH MULTIPLE SIMULTANEOUS READERS 
;;
;; Patch to MCL 4.2 Should work with all versions after
;;
;; Written by Alice Hartley 10/20/1998.
;; Changes from 00lock-w-type-3.lisp
;;
;; 10/21/1998 JCMa cleaned up for inclusion in CL-HTTP, Kernel redefinition warnings suppressed,
;;                 Locking macro named something useful WITH-LOCK in the hope that future MCL releases will
;;                 support the interface so we don't have to add more conditionalizations to CL-HTTP
;;                 code.
(in-package :ccl) 

(eval-when (compile eval load)

(let ((*warn-if-redefine* nil)
        (*warn-if-redefine-kernel* nil))
(def-accessors (lock) %svref
  lock.value
  lock.name
  lock.type
  lock.nreaders
  ;lock.what-for
  ) 
)

(defun lock-type (lock)
  (lock.type lock))

(defun lock-nreaders (lock)
  (lock.nreaders lock))

(defun (setf lock-nreaders) (val lock)
  (setq lock (require-type lock 'lock))
  (setf (lock.nreaders lock) val))

(defun (setf lock-type) (val lock)
  (setq lock (require-type lock 'lock))
  (setf (lock.type lock) val)) 

)

(let ((*warn-if-redefine* nil)
        (*warn-if-redefine-kernel* nil))

;; this used to make a structure containing only the lock value/owner if name was nil
(defun make-lock (&optional name type)
  (require-type type '(member nil :multiple-reader-single-writer :simple))
  (if (eq type :simple) (setq type nil))
  (gvector :lock nil name type  0))
) 

(defmacro with-lock ((lock &key mode (lock-value '*current-process*) (whostate "Lock")) &body body)
  (let ((flag (gensym))
        (setter (gensym))
        (lock-var (gensym))
        (lock-value-var (gensym))
        (what-for-var (gensym))
        )
    `(let* ((,flag nil)
            (,setter #'(lambda () (setq ,flag t)))
            (,lock-var ,lock)
            (,lock-value-var ,lock-value)
            (,what-for-var ,mode))
       (declare (dynamic-extent ,setter))       
       (require-type ,what-for-var '(member nil :read :write))
       (require-type ,lock-var 'lock)
       (when (neq (lock-type ,lock-var) :multiple-reader-single-writer)
         (setq ,what-for-var nil))
       #+ignore
       (when (memq ,what-for-var '(:read :write))
         (when (neq (lock-type ,lock-var) :multiple-readers)
           (error "Lock ~s is not of type :multiple-readers" ,lock-var)))
       (unwind-protect
         (progn 
           (if (and (eq ,what-for-var :read)
                    (eq (lock.value ,lock-var) :read))
             ;; its already locked for reading so just do it
             (progn (incf (lock.nreaders ,lock-var))
                    (setq ,flag :not-first))
             (progn
               ;; can be interrupted on entry to process-lock
               (process-lock ,lock-var (if (eq ,what-for-var :read) :read ,lock-value-var) ,whostate ,setter)               
               ;; newly locked for reading
               (when (eq ,what-for-var :read)
                 (if ,flag
                   (setf (lock.nreaders ,lock-var) 1)
                   ;; if somebody else got there first??
                   (progn
                     (setq ,flag t)
                     (incf (lock.nreaders ,lock-var)))))))
           ,@body)
         (when ,flag  ;; flag is nil if already locked with this lock.value (unless not first read).             
           (if (or (null ,what-for-var)(eq ,what-for-var :write))                         
             (process-unlock ,lock-var ,lock-value-var)
             (if (eq ,what-for-var :read)
               (without-interrupts ;; too bad
                 (decf (lock.nreaders ,lock-var))
                 (when (<= (lock.nreaders ,lock-var) 0)
                   (process-unlock ,lock-var :read))))))))))

(pushnew :multiple-reader-locks *features*) 

