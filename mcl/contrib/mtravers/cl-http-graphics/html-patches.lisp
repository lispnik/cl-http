(in-package :html)

;;; this is a Netscape HTML extension
#+ignore
(define-macro with-centering ((&key (stream '*output-stream*)) &body body)
  "Establishes a centering environment."
  `(with-environment ("CENTER" :stream ,stream)
     ,@body))

;;; this is patched to accept an additional keyword, :inline, which suppresses the creation
;;; of a verbatim environment around the buttons, letting them be on the same line as 
;;; other stuff.  
(defmethod accept-input ((radio-button radio-button) query-name &rest args)
  (with-standard-input-type-args (query-name radio-button args :bind-default nil)
    (destructuring-bind (&key choices (linebreaks t) (inline nil) (enumeration :plain) compact default selected-choice
			      (stream *output-stream*) &allow-other-keys) args
      (let ((values-provided-p (consp (first choices)))
	    (default-value (or default selected-choice)))	;selected-choice is obsolete  2/26/95 -- JCMa.
	(cond (linebreaks
	       (with-enumeration (stream enumeration :compact compact)
		 (loop for choice in choices
		       do (multiple-value-bind (value choice-string)
			      (cond (values-provided-p 
				     (values (cdr choice) (car choice)))
				    (t (values choice (princ-to-string (the fixnum (1+ (position choice choices)))))))
			    (enumerating-item (stream) 
			      (let ((args 
				      (cond ((equal value default-value)
					     `(,@standard-args ("VALUE" ,value) "CHECKED"))
					    (t `(,@standard-args ("VALUE" ,value))))))
				(declare (dynamic-extent args))
				(issue-command* "INPUT" args stream t)
				(write-string choice-string stream)
				(break-line :stream stream)))))))
	      (t (flet ((output-choices ()
		          (loop for choice in choices
			        do (multiple-value-bind (value choice-string)
				                        (cond (values-provided-p 
				                               (values (cdr choice) (car choice)))
				                              (t (values choice (princ-to-string (the fixnum (1+ (position choice choices)))))))
			             (let ((args (cond ((equal value default-value)
						        `(,@standard-args ("VALUE" ,value) "CHECKED"))
						       (t `(,@standard-args ("VALUE" ,value))))))
				       (declare (dynamic-extent args))
				       (write-char #\space stream)
				       (issue-command* "INPUT" args stream)
				       (write-string choice-string stream))))))
                   (if inline
                     (output-choices)
                     (with-verbatim-text (:fresh-line nil :stream stream)
                       (output-choices))
                   ))))))))

#+ignore
(export 'with-centering)
