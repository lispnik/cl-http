;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: future-common-lisp-user; Base: 10 -*-

;;;; PATCH.LISP
;;;

(in-package "CL-USER")

(defadvice (define-condition declosify) (form env)
  (labels ((extract-extra-funcs (s)
	     (if (consp s)
		 (let ((p1 (position :initarg s))
		       (p2 (position :reader s)))
		   (if (and p1
			    (not (equal (symbol-name (nth (1+ p1) s))
					(symbol-name (car s)))))
		       (let ((na (intern (format () "~a-~a" 
						 (nth 1 form) (car s)))))
			 (setf (car s) 
			   (intern (symbol-name (nth (1+ p1) s))))
			 (if p2
			     (list (list (car s) (nth (1+ p2) s))
				   (list (car s) na))
			   (list (car s) na)))
		     (if p2
			 (list (list (car s) (nth (1+ p2) s)))
		       ())))
	       ()))
	   (make-func (f)
	     `(progn
		(unless (fboundp ',(cadr f))
		  (defgeneric ,(cadr f) (x)))
		(defmethod ,(cadr f) ((x ,(nth 1 form)))
		(,(intern (format () "~a-~a" (nth 1 form) (car f))) x))))
	   (rewrite-slot-spec (s)
	     (if (consp s)
		 (let ((p (position :initform s)))
		   (if p 
		       (list (car s) (nth (1+ p) s))
		     (car s)))
	       s)))
    (let ((funcs     (mapcar #'make-func
			     (mapcan #'extract-extra-funcs (nth 3 form))))
	  (slot-spec (mapcar #'rewrite-slot-spec (nth 3 form))))
      (setf (nth 3 form) slot-spec)
      (if (null (nth 2 form)) 
	  (setf (nth 2 form) '(error)))
      (if (> (length (nth 2 form)) 1) 
	  (setf (nth 2 form) (list (car (nth 2 form)))))
      (if (and (nth 4 form)
	       (atom (nth 1 (nth 4 form))))
	  (setf (nth 1 (nth 4 form))
	    (list 'lambda (list 'c 's)
		  (list (nth 1 (nth 4 form)) 'c 's))))
      (append (butlast (advice-continue form env))
	      funcs
	      (list (list 'quote (nth 1 form)))))))

(export 'maximizing)
(import 'maximizing "HTTP")
(defadvice (loop replace-keywords) (form env)
    (advice-continue (sublis '((maximizing . maximize)) form) env))

(defadvice (defgeneric only-docu) (form env)
  (if (fboundp (cadr form))
      `(setf (documentation ',(cadr form) 'function) 
	 (cadr (assoc :documentation ',(cdddr form))))
    (advice-continue form env)))

;;; The code of `with-null-stream' in `mac/server/www-utils.lisp'
;;; doesn't work in lucid: the varaibles are bound to a functional
;;; object `make-broadcast-stream' instead of a broadcast-stream instance
;(if (fboundp 'www-utils::with-null-stream)
;    (unintern 'www-utils::with-null-stream 'www-utils))
;(defmacro www-utils::with-null-stream ((&rest streams) &body body)
;  "Binds STREAMS to the null-stream within BODY."
;  (loop for stream in streams
;        collect `(,stream (make-broadcast-stream)) into bindings
;      finally (return `(let ,bindings ,@body))))

;;; also redefine the only using function in 'mac/server/www-utils.lisp'
;;; to avoid problems with compiled code.
;(if (fboundp 'www-utils::run-daily-server-tasks)
;    (unintern 'www-utils::run-daily-server-tasks 'www-utils))
;(defun www-utils::run-daily-server-tasks ()
;  (www-utils::with-null-stream (*standard-output* *query-io*)
;    (loop with week-day = (www-utils::weekday)
;	for (name periodicity form) in www-utils::*daily-server-tasks*
;	when (case periodicity
;	       (:daily t)
;	       (:weekly (eq week-day :sunday))
;	       (t (eq periodicity week-day)))
;	do (handler-case
;	       (apply (car form) (cdr form))
;	     (error (err) 
;	       (www-utils::log-http-server-error 
;		"Error in daily server task, ~S:~&~A"
;		name (report-string err))))))
;  ;; Reset so we run again.
;  (www-utils::synchronize-daily-server-tasks))
