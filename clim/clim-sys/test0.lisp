(in-package "MINP")

#+ignore
(defun listen-stream (stream)
  #+Allegro
  (unless (typep stream 'EXCL::BACKGROUND-STREAM)
    (listen stream))
  #+(and ACLPC (not ACL3.0))
  (cl:peek-char nil stream)	;listen causes fatal error!
  #-(or Allegro (and ACLPC (not ACL3.0)))
  (listen stream))


(defun listen-stream (stream) (peek-char nil stream))

(minp:destroy-process minp::*mini-listener*)
(setq *mini-listener* nil)

#+MINIPROC
(defun make-mini-listener (&key name (count 0))
  (make-process-loop
   :name (or name "Mini Lisp Listener")
   :with ((next t)
	  #-ACLPC
	  (*standard-input* *top-level-input*)
	  #-ACLPC
	  (*standard-output* *top-level-output*))
   #-ACLPC :declare #-ACLPC (declare (special *standard-input* *standard-output*))
   :do (progn
	 (when next
	   (setf next nil)
	   (format *standard-output* "~&+~:(~a~)(~a): "
		   (or (first (package-nicknames *package*))
		       (package-name *package*))
		   (incf count)))
	 (finish-output *standard-output*)
	 (clear-input *standard-input*)
	 (if (listen-stream *standard-input*)
	   (let ((command (read *standard-input* nil nil)))
	     #-ACLPC (clear-input *standard-input*)
	     (setf next t)
	     (case command
	       ((:help :?)
		(format *standard-output* "~&Type :END to suspend the mini listener
     :EXIT to exit the mini scheduler."))
	       (:end
		(disable-listener))
	       (:exit
		(throw 'scheduling-exit command))
	       (t
		(multiple-value-bind (results error args)
		    (if *ignore-errors*
			(ignore-errors (multiple-value-list (eval command)))
		      (multiple-value-list (eval command)))
		  (when error
		    (warn "The following ERROR was ignored.~%Type :HELP to get help." error args)
		    (describe error)
		    (enable-listener))
		  (loop for result in results
		      do #+ACLPC (terpri *standard-output*)	;else fatal error!
			 (write result :stream *standard-output*
				:circle t :pretty t)
			 #-ACLPC (terpri *standard-output*))))))))))
