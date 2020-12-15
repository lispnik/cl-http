;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: www-present -*-

;;; (C) Copyright 1996, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;
;;; Christopher R. Vincent
;;; cvince@ai.mit.edu
;;;
;;; (C) Enhancements Copyright 1997, 1999 John C. Mallery.
;;;     All Rights Reserved.
;;;

;;;------------------------------------------------------------------- 
;;;
;;; BASIC PRESENTATION SYSTEM FOR THE WORLD-WIDE WEB
;;;

#|

html-definitions contains some general html definitions
these definitions necessarily have some visual attributes hard-wired in.
this is meant to provide a basic html-view appropriate for most forms, etc.
subviews of html-view can be created to allow methods with a specific 
look or feel, use html extensions, etc.

|#


(in-package :www-present)


;;;------------------------------------------------------------------- 
;;;
;;; PRESENTATION METHODS
;;;

(defparameter *html-input-box-default-size* 72
  "The default size for boxes in html forms.")

(defparameter *html-input-box-default-hysterisis* 10
  "The percentage to increase input box size beyond the default value length.")

(defun heuristic-input-box-size (value-string)
  (if value-string
      (let ((length (length value-string)))
	(min (+ length (round length *html-input-box-default-hysterisis*)) *html-input-box-default-size*))
      *html-input-box-default-size*))

(defmacro with-standard-html-prompt ((type &key stream default default-supplied-p prompt prompt-mode display-default)
                                     &body body)
  `(progn
     (when ,prompt 
       (typecase ,prompt 
         (string (write-string ,prompt ,stream))
         (t (write ,type :stream ,stream)))
       (when (and ,prompt ,default-supplied-p ,display-default)
         (write-string " [" ,stream)
         (typecase ,default
           (string (write-string ,default ,stream))
           (t (present ,default ,type :stream ,stream :view +textual-view+)))
         (write-char #\] ,stream))
       (case ,prompt-mode 
         (:normal (write-string ": " ,stream))
         (t (write-char #\Space ,stream))))
     ,@body))




;;;------------------------------------------------------------------- 
;;;
;;; HTML-VIEW ACCEPT-PRESENT-DEFAULT METHODS
;;;

(define-presentation-method accept-present-default ((type t) stream (view html-view) default default-supplied-p 
                                                    present-p query-identifier &key prompt prompt-mode
                                                    display-default insert-default active-p default-string)
  "catch unhandled calls to accept-present-default in the html-view, render as one-line text box."
  (declare (ignore present-p))
  (with-standard-html-prompt
    (type :stream stream :default (or default-string default) :default-supplied-p default-supplied-p
          :prompt prompt :prompt-mode prompt-mode :display-default display-default)
    (cond (active-p
	   (let ((default-value (or default-string (when default-supplied-p (present-to-string default type)))))
	     (declare (dynamic-extent default-value))
	     (html:accept-input 'html:string+ query-identifier :stream stream
				:default (when insert-default default-value)
				:size (heuristic-input-box-size default-value) :max-length *html-input-box-default-size*)))
          (t (when default-supplied-p
               (or default-string (present default type :stream stream :view +textual-view+ :acceptably t))
               default)))))

(define-presentation-method accept-present-default ((type boolean) stream (view html-view) default default-supplied-p 
                                                    present-p query-identifier &key prompt prompt-mode display-default
                                                    insert-default active-p)
  (declare (ignore present-p))
  (with-standard-html-prompt
    (type :stream stream :default default :default-supplied-p default-supplied-p
          :prompt prompt :prompt-mode prompt-mode :display-default display-default)
    (cond (active-p 
           (html:accept-input 'html:radio-button query-identifier :stream stream
                              :choices '(("Yes" . "Yes") ("No" . "No"))
                              :default (when insert-default (present-to-string default type)) :linebreaks nil))
          (t (when default-supplied-p
               (present default type :stream stream :view +textual-view+ :acceptably t)
               default)))))

(define-presentation-method accept-present-default ((type integer) stream (view html-view) default default-supplied-p 
                                                    present-p query-identifier &key prompt prompt-mode display-default
                                                    insert-default active-p default-string)
  (declare (ignore present-p))
  (with-presentation-type-parameters (integer type)
    (let ((size (when (and low high)
		  (the fixnum (max (the fixnum (cond ((< low 0) (+ 2 (ceiling (the single-float (log (- low) 10)))))
						     (t (ceiling (the single-float (log (1+ low) 10))))))
				   (the fixnum (cond ((< high 0) (+ 2 (ceiling (the single-float (log (- high) 10)))))
						     (t (ceiling (the single-float (log (1+ high) 10)))))))))))
      (with-standard-html-prompt
        (type :stream stream :default (or default-string default) :default-supplied-p default-supplied-p
              :prompt prompt :prompt-mode prompt-mode :display-default display-default) 
        (cond (active-p
	       (let ((default-value (when insert-default
				      (or default-string
					  (when default-supplied-p (present-to-string default type))))))
		 (declare (dynamic-extent default-value))
		 (html:accept-input 'html:string+ query-identifier :stream stream
				    :default default-value
				    :size size :max-length *html-input-box-default-size*)))
              (t (when default-supplied-p
                   (or default-string (present default type :stream stream :view +textual-view+ :acceptably t))
                   default)))))))

(define-presentation-method accept-present-default ((type bounded-string) stream (view html-view) default default-supplied-p 
                                                    present-p query-identifier &key prompt prompt-mode display-default
                                                    insert-default active-p default-string)
  (declare (ignore present-p))
  (with-presentation-type-parameters (bounded-string type)
    (with-standard-html-prompt
      (type :stream stream :default (or default-string default) :default-supplied-p default-supplied-p
	    :prompt prompt :prompt-mode prompt-mode :display-default display-default)
      (when break-line-after-prompt
	(html:break-line :stream stream))
      (cond (active-p
	     (let ((default-value (or default-string (when default-supplied-p default))))
	       (html:accept-input 'html:string+ query-identifier :stream stream
				  :default (when insert-default default-value)
				  :size (cond (input-box-size)
					      (max-length (min (heuristic-input-box-size default-value) max-length))
					      (t (heuristic-input-box-size default-value)))
				  :max-length (or input-box-size max-length))))
	    (t (when default-supplied-p
		 (or default-string (present default type :stream stream :view +textual-view+ :acceptably t))
		 default))))))

(define-presentation-method accept-present-default ((type member-sequence) stream (view html-view) default default-supplied-p 
                                                    present-p query-identifier &key prompt prompt-mode display-default
                                                    insert-default active-p)
  (declare (ignore present-p))
  (with-presentation-type-parameters (member-sequence type)
    (let ((presented-sequence 
            (loop for item in sequence
                  collect (funcall name-key item) into result
                  finally (return result))))
      (with-standard-html-prompt
        (type :stream stream :default default :default-supplied-p default-supplied-p
              :prompt prompt :prompt-mode prompt-mode :display-default display-default) 
        (cond (active-p 
               (html:accept-input 'html:select-choices query-identifier :stream stream :choices presented-sequence
                                  :sequence-p nil 
                                  :default (when insert-default (funcall name-key default))
                                  :size :pull-down-menu))
              (t (when default-supplied-p
                   (present default type :stream stream :view +textual-view+ :acceptably t)
                   default)))))))

(define-presentation-method accept-present-default ((type member-alist) stream (view html-view) default default-supplied-p 
                                                    present-p query-identifier &key prompt prompt-mode display-default
                                                    insert-default active-p)
  (declare (ignore present-p))
  (with-presentation-type-parameters (member-alist type)
    (let ((presented-alist 
            (loop for item in alist
		  collect `(,(funcall name-key item)
			    :value ,(http:write-to-armor-plated-string (funcall value-key item))))))
      (declare (dynamic-extent presented-alist))
      (with-standard-html-prompt
        (type :stream stream :default default :default-supplied-p default-supplied-p
              :prompt prompt :prompt-mode prompt-mode :display-default display-default) 
        (cond (active-p 
               (html:accept-input 'html:select-choices query-identifier :stream stream :choices presented-alist
                                  :sequence-p nil 
                                  :default (when insert-default (http:write-to-armor-plated-string default))
                                  :size :pull-down-menu))
              (default-supplied-p
	       (present default type :stream stream :view +textual-view+ :acceptably t)
	       default)
	      (t nil))))))

(define-presentation-method accept ((type member-alist) stream (view html-view) &key)
  (with-presentation-type-parameters (member-alist type)
    (let* ((raw-value (read-delimited-string '(#\Return #\Linefeed) stream))
	   (value (http:read-from-armor-plated-string raw-value nil)))
      (declare (dynamic-extent raw-value))
      (unless (member value alist :test test :key value-key)
	(handle-input-error raw-value type))
      (values value type))))

(define-presentation-method accept-present-default ((type sequence) stream (view html-view) default default-supplied-p 
                                                    present-p query-identifier &key prompt prompt-mode display-default
                                                    insert-default active-p)
  (declare (ignore present-p))
  (with-standard-html-prompt
    (`(sequence ,type) :stream stream :default default :default-supplied-p default-supplied-p
     :prompt prompt :prompt-mode prompt-mode :display-default display-default)
    (cond (active-p
	   (let ((default-value (when default-supplied-p (present-to-string default `(sequence ,type)))))
	     (declare (dynamic-extent default-value))
	     (html:accept-input 'html:string+ query-identifier :stream stream
				:default (when insert-default default-value)
				:size *html-input-box-default-size* :max-length *html-input-box-default-size*)))
          (t (when default-supplied-p
               (present default type :stream stream :view +textual-view+ :acceptably t)
               default)))))
  
(define-presentation-method accept-present-default ((type null-or-type) stream (view html-view) default default-supplied-p
                                                    present-p query-identifier &key prompt prompt-mode display-default
                                                    insert-default active-p)
  "this could be a problem, since user should always be able to enter None.
Setting default-string should override the type's default rendering"
  (with-presentation-type-decoded (type-name parameters options) type
    (funcall-presentation-generic-function accept-present-default type-name parameters options type
                                           stream view 
                                           default
                                           default-supplied-p present-p
                                           query-identifier :prompt prompt :prompt-mode prompt-mode
                                           :display-default display-default :insert-default insert-default
                                           :active-p active-p
                                           :default-string (unless default "None"))))

(define-presentation-method accept-present-default ((type pathname) stream (view html-view) default default-supplied-p
                                                    present-p query-identifier &key prompt prompt-mode display-default
                                                    insert-default active-p default-string)
  "for implementations without a pathname class"
  (declare (ignore present-p))
  (with-standard-html-prompt
    (type :stream stream :default (or default-string default) :default-supplied-p default-supplied-p
          :prompt prompt :prompt-mode prompt-mode :display-default display-default)
    (cond (active-p
	   (let ((default-value (or default-string (when default-supplied-p (present-to-string default type)))))
	     (declare (dynamic-extent default-value))
	     (html:accept-input 'html:string+ query-identifier :stream stream
				:default (when insert-default default-value)
				:size (heuristic-input-box-size default-value) :max-length *html-input-box-default-size*)))
          (t (when default-supplied-p
               (or default-string (present default type :stream stream :view +textual-view+ :acceptably t))
               default)))))


;;;------------------------------------------------------------------- 
;;;
;;; HTML-SPECIFIC PRESENTATIONS
;;;
;;; Written 12/16/99 -- JCMa.

(define-presentation-type armor-plated-string
			  () 
  :inherit-from 'basic-string
  :description "an armor-plated string")

(export '(armor-plated-string) :w3p)

(define-presentation-method presentation-typep (string (type armor-plated-string))
  (typep string 'string))

(define-presentation-method accept ((type armor-plated-string) stream (view textual-view) &key)
  (let ((armor-plated-string (read-line stream nil nil)))
    (unless (stringp armor-plated-string)
      (handle-input-error armor-plated-string 'string))
    (handler-case
      (http:read-from-armor-plated-string armor-plated-string t)
      (end-of-file () (handle-input-error armor-plated-string type))))) 

(define-presentation-method present (s-exp (type armor-plated-string) stream (view textual-view) &key)
  (let ((armor-plated-string (http:write-to-armor-plated-string s-exp)))
    (declare (dynamic-extent armor-plated-string))
    (write-string armor-plated-string stream)
    s-exp)) 

(define-presentation-type hidden-field
			  (embedded-type) 
  :inherit-from 'basic-string
  :description "a hidden field that wraps the actual presentation")

(export '(hidden-field) :w3p)

(define-presentation-method presentation-typep (object (type hidden-field))
  (with-presentation-type-parameters (hidden-field type)
    (presentation-typep object embedded-type)))

(define-presentation-method accept ((type hidden-field) stream (view textual-view) &key (default nil default-supplied-p)  (default-type type) 
				    (prompt t) (prompt-mode ':normal) (display-default prompt))
  (with-presentation-type-parameters (hidden-field type)
    (if default-supplied-p
	(accept embedded-type :stream stream :view view :default default :default-type default-type 
		:prompt prompt :prompt-mode prompt-mode :display-default display-default)
	(accept embedded-type :stream stream :view view :prompt prompt :prompt-mode prompt-mode))))

(define-presentation-method present (object (type hidden-field) stream (view textual-view) &key acceptably for-context-type)
  (with-presentation-type-parameters (hidden-field type) 
    (present object embedded-type :stream stream :view view :acceptably acceptably :for-context-type for-context-type))) 

(define-presentation-method accept-present-default ((type hidden-field) stream (view html-view) default default-supplied-p 
						    present-p query-identifier &key prompt prompt-mode
						    display-default insert-default active-p default-string)
  (declare (ignore default-supplied-p present-p prompt prompt-mode display-default insert-default active-p default-string))
  (with-presentation-type-parameters (hidden-field type) 
    (let* ((value-string (present-to-string default embedded-type :view +textual-view+))
	   (armor-plated-string (present-to-string value-string 'armor-plated-string :view +textual-view+)))
      (declare (dynamic-extent value-string armor-plated-string))
      (html:accept-input 'html:hidden query-identifier :default armor-plated-string :stream stream)
      (values default type))))

(define-presentation-method accept ((type hidden-field) stream (view html-view) &key default)
  (with-presentation-type-parameters (hidden-field type)
    (let ((raw-string (accept 'armor-plated-string :stream stream :view view)))
      (accept-from-string  embedded-type raw-string :view +textual-view+ :default default :default-type embedded-type))))


;;;------------------------------------------------------------------- 
;;;
;;; VALUE BINDING MACRO
;;;
;;; Written 12/16/99 -- JCMa.

(defmacro accept-query-values (queries (url query-alist) input-failure-form &body body)
  "Binds variables to the names of queries with the values of queries in a form.
Query is  (name presentation-type &optional default-value).
Sometimes more than one input field will use the same query identifier.  In
these cases, the values returned for the query will be collected in a list.
The predicate VALUES-COLLECTED-P can be applied to the query keyword in order
to find out if collection has taken place. QUERY-BAGGAGE returns any
associated string that may have been packed with HTML:PACK-QUERY-NAME and
transmitted in a form by HTML:ACCEPT-INPUT."
  (flet ((raw-query-name (query)
	   (intern (concatenate 'string "RAW-" (string query)) *package*)))
    (loop for (query presentation-type default-value) in queries
	  for raw-query = (raw-query-name query)
	  collect `(,raw-query ,(http::%make-query-binding url (intern (symbol-name query) :keyword) query-alist :unbound))
	    into bindings
	  collect `(,query (case ,raw-query
			     (:unbound ,default-value)
			     (t (handler-case 
				  (accept-from-string ,presentation-type ,raw-query)
				  (input-not-of-required-type (input-error) 
							      (setq input-failure-p t)
							      input-error)))))
	    into bindings
	  finally (return `(macrolet
			     ((values-collected-p (query)
				`(%values-collected-p ,query ,',query-alist))
			      (query-baggage (query)
				`(%query-baggage ,query ,',query-alist)))
			     (let* ((input-failure-p nil) 
				    ,@bindings) 
			       (cond (input-failure-p ,input-failure-form)
				     (t ,@body)))))))) 

(export '(accept-query-values) :w3p)
