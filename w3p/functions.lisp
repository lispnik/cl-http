;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: www-present -*-

;;; (C) Copyright 1996, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;
;;; Christopher R. Vincent
;;; cvince@ai.mit.edu
;;;
;;;------------------------------------------------------------------- 
;;;
;;; BASIC PRESENTATION SYSTEM FOR THE WORLD-WIDE WEB
;;;

#|

functions.lisp contains definitions for the basic
presentration-generic-functions and the regular functions that the user
calls. if necessary, the user can define new presentation-generic-functions
in the manner demonstrated here. basic views are defined here as well.

|#


(in-package :www-present)

;;;------------------------------------------------------------------- 
;;;
;;; PRESENTATION VIEWS
;;;

(eval-when (compile load eval)
  (define-presentation-view-classes 
    ((view :description "the basic view class.")
     (textual-view :inherits-from (view))
     (html-view :inherits-from (textual-view))))
  (define-presentation-view +textual-view+ textual-view)
  (define-presentation-view +html-view+ html-view))

(progn
  (setf (stream-default-view *standard-input*) +textual-view+)
  (setf (stream-default-view *standard-output*) +textual-view+))

;;;------------------------------------------------------------------- 
;;;
;;; PRESENTATION GENERIC FUNCTIONS
;;;

(eval-when (compile load eval)

  (define-presentation-generic-function accept-method 
                                        accept (type-key parameters options type stream view &key &allow-other-keys)
                                        (:documentation "generic function for accept methods."))

  (define-presentation-generic-function present-method 
                                        present (type-key parameters options object type stream view &key &allow-other-keys)
                                        (:documentation "generic function for present methods."))

  (define-presentation-generic-function presentation-typep-method
                                        presentation-typep (type-key parameters object type)
                                        (:documentation "generic function for presentation-typep methods."))

  (define-presentation-generic-function presentation-subtypep-method
                                        presentation-subtypep (type-key type putative-supertype &key &allow-other-keys)
                                        (:documentation "generic function for presentation-subtypep methods."))

  (define-presentation-generic-function accept-present-default-method
                                        accept-present-default (type-key parameters options type stream view default
									 default-supplied-p present-p
                                                                         query-identifier &key &allow-other-keys)
                                        (:documentation "generic function for accept-present-default methods."))

  (define-presentation-generic-function presentation-type-specifier-p-method
                                        presentation-type-specifier-p (type-key parameters options type &key &allow-other-keys)
                                        (:documentation "generic function for presentation-type-specifier-p methods."))

  (define-presentation-generic-function describe-presentation-type-method
                                        describe-presentation-type (type-key parameters options type stream plural-count
									     &key &allow-other-keys)
                                        (:documentation "generic function for presentation-type-specifier-p methods."))

  (define-presentation-generic-function handle-input-error-method
                                        handle-input-error (type-key parameters options failed-input type stream view
								     &key &allow-other-keys)
                                        (:documentation "generic function for input-not-of-required-type methods."))

  )


;;;------------------------------------------------------------------- 
;;;
;;; PRESENTATION FUNCTIONS
;;;

(defun accept-present-default (type stream view default default-supplied-p present-p query-identifier 
                                    &key 
                                    (prompt t)
                                    (prompt-mode ':normal)
                                    (display-default prompt)
                                    insert-default
                                    (active-p t)
                                    default-string)
  (with-presentation-type-decoded (type-name parameters options) type
    (multiple-value-bind (returned-object returned-type)
        (funcall-presentation-generic-function accept-present-default type-name parameters options type
                                               stream view default default-supplied-p present-p
                                               query-identifier :prompt prompt :prompt-mode prompt-mode
                                               :display-default display-default :insert-default insert-default
                                               :active-p active-p :default-string default-string)
      (values returned-object (or returned-type type)))))

(defun accept (type &key 
                    (stream *standard-input*) 
                    (view (stream-default-view stream)) 
                    (default nil default-supplied-p)
                    (default-type type)
                    (prompt t)
                    (prompt-mode ':normal)
                    (display-default prompt)
                    query-identifier
                    insert-default
                    present-p
                    (active-p t)
                    (error-stream *standard-output*)
                    &allow-other-keys)
  "accept an object from the user, returned type is nil if input failed"
  (etypecase type (presentation-type-specifier))
  (when present-p
    (return-from accept
      (accept-present-default type stream view default default-supplied-p present-p query-identifier 
                              :prompt prompt :prompt-mode prompt-mode :display-default display-default
                              :insert-default insert-default :active-p active-p)))
  (unless active-p 
    (return-from accept
      (values default default-type)))
  (handler-bind ((presentation-parse-error
                   #'(lambda (condition)
		       (let ((object (handle-input-error-object condition))
 			     (type (handle-input-error-type condition)))
			 (with-presentation-type-decoded (type-name parameters options) type
			   (return-from accept
			     (funcall-presentation-generic-function handle-input-error
								    type-name parameters options 
								    object type
								    (or (handle-input-error-stream condition) error-stream)
								    (or (handle-input-error-view condition) view))))))))
    (with-presentation-type-decoded (type-name parameters options) type
      (multiple-value-bind (returned-object returned-type)
          (funcall-presentation-generic-function accept type-name parameters options type stream view 
                                                 :default default :default-type default-type)
        (values returned-object (or returned-type type))))))

(defun accept-from-string (type string
                                &key 
                                (view +textual-view+) 
                                default 
                                (default-type type) 
                                (start 0)
                                end
                                (error-stream *standard-output*)
                                &allow-other-keys)
  "accept an object from a string, returned type is nil if input failed"
  (when string
    (with-input-from-string (stream string :start start :end end)
      (accept type :stream stream :view view :default default :default-type default-type :error-stream error-stream))))

(defun present (object &optional (type (presentation-type-of object))
                       &key
                       (stream *standard-output*)
                       (view (stream-default-view stream))
                       acceptably
                       (for-context-type type)
                       &allow-other-keys)
  "present an object to the user"
  (etypecase type (presentation-type-specifier))
  (with-presentation-type-decoded (type-name parameters options) type
    (multiple-value-bind (returned-object returned-type)
        (funcall-presentation-generic-function present type-name parameters options object 
                                               type stream view :acceptably acceptably 
                                               :for-context-type for-context-type)
      (values returned-object (or returned-type type)))))

(defun present-to-string (object &optional (type (presentation-type-of object))
                                 &key
                                 (view +textual-view+)
                                 acceptably
                                 (for-context-type type)
                                 string
                                 index
                                 &allow-other-keys)
  "present an object to a string"
  (cond (string (when index (setf (fill-pointer string) index))
                (with-output-to-string (stream string)
                  (present object type :stream stream :view view :acceptably acceptably 
                           :for-context-type for-context-type))
                (values string (fill-pointer string)))
        (t (with-output-to-string (stream)
             (present object type :stream stream :view view :acceptably acceptably 
                      :for-context-type for-context-type)))))
 
(defun presentation-typep (object type)
  "determine if an object satisfies a presentation-type"
  (check-type type presentation-type-specifier)
  (with-presentation-type-decoded (type-name parameters nil) type
    (funcall-presentation-generic-function presentation-typep type-name parameters object type)))

(defun presentation-subtypep (type putative-supertype)
  "determines if type is a subtype of putative-supertype"
  (check-type type presentation-type-specifier)
  (check-type putative-supertype presentation-type-specifier)
  (with-presentation-type-decoded (type-name) type
    (funcall-presentation-generic-function presentation-subtypep type-name type putative-supertype)))

(defun presentation-type-specifier-p (type)
  "determine if parameters and options are valid for a presentation-type-specifier."
  (check-type type presentation-type-specifier)
  (with-presentation-type-decoded (type-name parameters options) type
    (funcall-presentation-generic-function presentation-type-specifier-p type-name parameters options type)))
  
(defun describe-presentation-type (type &optional (stream *standard-output*) (plural-count 1))
  "describes presentation-type on stream"
  (check-type type presentation-type-specifier)
  (with-presentation-type-decoded (type-name parameters options) type
    (cond (stream
           (funcall-presentation-generic-function describe-presentation-type type-name parameters options 
                                                  type stream plural-count))
          (t (with-output-to-string (string-stream)
               (funcall-presentation-generic-function describe-presentation-type type-name parameters options 
                                                      type string-stream plural-count))))))

(defun handle-input-error (object type &rest args &key stream view)
  "when called inside accept, signals that accept should return the results of a handle-input-error method."
  (check-type type presentation-type-specifier)
  (signal 'handle-input-error :object object :type type :stream stream :view view :args args)
  (error 'handle-input-error :object object :type type :stream stream :view view :args args))
