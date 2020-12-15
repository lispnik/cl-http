;;;   -*- Mode: lisp; Package: http-user; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-

;;;
;;; (c) Copyright  1997, John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; LISP LISTENER OVER HTTP
;;;
;;; You may need to adjust the secure subnets on the URL export below.
;;;
(in-package :http-user)

(defparameter *listener-color-scheme* (create-color-scheme :background :black
							   :foreground :yellow
							   :link :green
							   :visited-link :orange
							   :active-link :pink)
  "The standard color scheme for the CL-HTTP Lisp Listener.")

(defgeneric write-listener-form (url stream &key value typeout state))

(defmethod write-listener-form ((url url:http-form) stream &key (value nil value-supplied-p) typeout state
				&aux (columns 80.) (rows 5.) (foreground (html3.2::color-scheme-foreground *listener-color-scheme*))
				(foreground-inverse (inverse-color foreground)))
  (flet ((write-heading (stream)
	   (with-anchor-noted (:reference *cl-http-home-page-url-string* :stream stream)
	     (image "/cl-http/icons/power.gif" "CL-HTTP" :stream stream
		    :vertical-space 0 :horizontal-space 0 :width 67 :height 30))
	   (with-rendition (:bold :stream stream)
	     (fast-format stream "Web Lisp Listener")))
	 (write-blurb (stream foreground-inverse foreground)
	   (with-centering (:stream stream)
	     (ns4.0:with-table (:background foreground :cell-spacing 0 :cell-padding 10. :width .9 :stream stream)
	       (ns4.0:with-table-row (:stream stream)
		 (ns4.0:with-table-cell (:stream stream)
		   (with-font (:color foreground-inverse :size 2 :stream stream)
		     (with-paragraph (:stream stream)
		       (fast-format stream "This Web Lisp Listener reads Lisp s-expressions from the Typein Window,
evaluates them, and prints the results.")) 
		     (with-paragraph (:stream stream)
		       (fast-format stream "Any output to
*STANDARD-OUTPUT* and any errors are reported in a Typeout Window.  The
default package is HTTP-USER.  Previous s-expressions read are recorded in +,
++, +++.  The value lists of previous evaluations are recorded in //, ////,
and //////, whereas previous first values are recorded in *, **, and ***. Some useful
fucntions include ARGLIST, APROPOS, APROPOS-LIST, DOCUMENTATION, and DESCRIBE."))
		     (with-paragraph (:stream stream)
		       (fast-format stream "Please note that unreadable lisp objects cannot be accessed directly 
through this interface, including via the value recording variables above.")))))))))
    (with-successful-response (stream :html :status :reset-content	;HTTP 1.1 browsers will keep one page.
				      :content-location url
				      :expires (url:expiration-universal-time url)
				      :cache-control (url:response-cache-control-directives url)
				      :content-language (languages url))
      (with-html-document (:stream stream)
	(with-document-preamble (:stream stream)
	  (declare-base-reference url :stream stream)
	  (declare-title "Web Lisp Listener" :stream stream))
	(with-standard-document-body (:color-scheme *listener-color-scheme* :stream stream)
	  (with-section-heading (#'write-heading :alignment :left :stream stream)
	    (horizontal-line :stream stream)
	    ;; Write some instructions for users.
	    (write-blurb stream foreground-inverse foreground)
	    (with-fillout-form (:post url :stream stream)
	      (with-centering (:stream stream)
		(ns4.0:with-table
		  (:cell-spacing 0 :cell-padding 10. :width .9 :stream stream :background foreground-inverse)
		  ;; Show type when applicable
		  (when (and typeout (not (null-string-p typeout)))
		    (ns4.0:with-table-row (:horizontal-alignment :center :vertical-alignment :middle :stream stream)
		      (ns4.0:with-table-cell (:column-span 3 :stream stream)
			(with-section-heading ("Typeout Window" :level 3 :alignment :left :stream stream)
			  (accept-input 'multi-line-text "TYPEOUT-WINDOW" :default typeout
					:rows (min (+ 2 (count #\newline typeout)) 30) :columns columns :stream stream)))))
		  ;; accept typein
		  (ns4.0:with-table-row (:horizontal-alignment :center :vertical-alignment :middle :stream stream)
		    (ns4.0:with-table-cell (:column-span 3 :stream stream)
		      (with-section-heading ("Typein Window" :level 3 :alignment :left :stream stream)
			(accept-input 'multi-line-text "S-EXPRESSION" :default (when value-supplied-p value)
				      :rows rows :columns columns :stream stream)
			;; Write a hidden field to carry the state and avoid collisions across threads.
			(when state
			  (accept-input 'hidden "STATE" :default state :stream stream)))))
		  ;; put up buttons
		  (with-table-row (:stream stream)
		    (with-table-cell (:horizontal-alignment :center :stream stream)
		      (with-font (:size 4 :stream stream)
			(with-rendition (:bold :stream stream)
			  (fast-format stream "Action:"))))
		    (with-table-cell (:horizontal-alignment :center :stream stream)
		      (accept-input 'reset-button "Reset" :display-string "Revert" :stream stream))
		    (with-table-cell (:horizontal-alignment :center :stream stream)
		      (accept-input 'submit-button "Submit" :display-string "Eval" :stream stream))))))
	    (horizontal-line :stream stream)
	    (cl-http-signature stream)))))))

(defmethod read-eval-print-form ((url url:http-form) stream query-alist)
  (labels ((print-value-to-string (value)
	     (write-to-string value :base 10 :pretty t))
	   (print-value-to-strings (values typeout)
	     (loop with newline = (make-string 1 :initial-element #\return)
		   for (val . more) = values then more
		   collect (print-value-to-string val) into result
		   while more
		   collect newline into result
		   finally (return (apply #'concatenate 'string typeout result))))
	   (eval-s-expression (s-expression state &aux values typeout new-state)
	     (destructuring-bind (&optional /// // / *** ** * +++ ++ +) state
	       (let* ((*read-eval* t)
		      (*read-base* 10.)
		      (*print-pretty* t)
		      (*package* (find-package :http-user))
		      (s-exp (read-from-string s-expression))
		      (*print-base* 10.)
		      (*print-radix* nil)
		      (*print-readably* nil))
		 ;; evaluate expressions with typeout captured
		 (setq typeout (with-output-to-string (*standard-output*)
				 (let ((*query-io* *standard-output*)
				       (*error-output* *standard-output*))
				   (setq values (multiple-value-list (eval s-exp))))))
		 (shiftf /// // / values)
		 (shiftf *** ** * (first values))
		 (shiftf +++ ++ + s-exp)
		 (setq new-state (list /// // / *** ** * +++ ++ +))
		 ;; print up multiple values
		 (when (cdr values)
		   (setq typeout (print-value-to-strings values typeout))))
	       (values values new-state typeout))))
    (declare (inline print-value-to-string))
    (bind-query-values (s-expression state)
		       (url query-alist)
      (http::handler-case-if (not *debug-server*) 
	 (if (every #'(lambda (ch) (member ch '(#\space #\tab #\newline))) s-expression)
	     (write-listener-form url stream :state state)
	     (multiple-value-bind (values new-state typeout-string)
		 (eval-s-expression s-expression (when state (read-from-armor-plated-string state)))
	       ;; generate another version of the form with the new values.
	       (write-listener-form url stream :value (print-value-to-string (car values))
				    :typeout typeout-string
				    :state (write-to-armor-plated-string new-state))))
	;; Catch read or eval errors and report them back to the user.
	(error (err) (write-listener-form url stream :typeout (report-string err) :state state))))))

;; Be careful to set the access control correctly to avoid unwanted security
;; breaches. We recommend that you specify secure subnets for safest
;; operation.
(export-url #u"/cl-http/listener.html"
            :html-computed-form
            :form-function #'write-listener-form
            :expiration '(:no-expiration-header)
            :response-function #'read-eval-print-form
	    :secure-subnets #+MIT-Site '("128.52.0.0") #-MIT-Site (list (local-host-ip-address))
            :private t
            :language :en
            :keywords '(:cl-http :demo)
            :documentation "A CL-HTTP Lisp listener that runs over the Web.")
