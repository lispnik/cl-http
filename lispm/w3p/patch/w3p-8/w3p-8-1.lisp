;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for W3P version 8.1
;;; Reason: Function W3P::ACCEPT-QUERY-VALUES:  new macro.
;;; DEFINE-PRESENTATION-TYPE W3P::HIDDEN-FIELD:  new presentation type.
;;; DEFINE-PRESENTATION-TYPE W3P::ARMOR-PLATED-STRING:  new presentation type.
;;; Written by JCMa, 12/16/99 18:20:31
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.8, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.23,
;;; W3 Presentation System 8.0, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 41, HTTP Client 49.2,
;;; HTTP Client Substrate 3.2, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, HTTP Proxy Server 4.2,
;;; W4 Constraint-Guide Web Walker 41.2, W4 Examples 13.0, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.11, DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x994 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189585,
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.24),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:W3P;HTML-DEFINITIONS.LISP.19")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W3P;HTML-DEFINITIONS.LISP.19")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: www-present -*-")

(PROGN

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

)
