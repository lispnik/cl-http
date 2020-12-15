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

documentation.lisp contains documentation for www-present
this documentation uses w3p and the html synthesis tools in cl-http

|#

(in-package :www-present)


;;;------------------------------------------------------------------- 
;;;
;;; FUNCTION DOCUMENTATION
;;;

(eval-when (load eval compile)
  (defclass function-documentation ()
      ((name :initarg :name :accessor documentation-function-name)
       (arguments :initarg :arguments :accessor documentation-function-arguments)
       (description :initarg :description :accessor documentation-description))
    (:documentation "stores documentation info for a function")))

(defmethod print-object ((function-documentation function-documentation) stream)
  (with-slots (name) function-documentation
    (print-unreadable-object (function-documentation stream :type t :identity t)
      (when (slot-boundp function-documentation 'name)
        (write-string (symbol-name name) stream)))))

(defmethod make-load-form ((function-documentation function-documentation)
			   #+(or ANSI-CL DRAFT-ANSI-CL-2) &optional
                           #+(or ANSI-CL DRAFT-ANSI-CL-2) environment)
  #+(or ANSI-CL DRAFT-ANSI-CL-2)
  (declare (ignore environment))
  (make-load-form-saving-slots function-documentation))

(eval-when (load eval compile)
  (w3p:define-presentation-type function-documentation () :inherit-from t :options (verbose)))

(w3p:define-presentation-method present (object (type function-documentation) stream 
                                                (view html-view) &key)
  (with-presentation-type-options (function-documentation type)
    (cond (verbose
           (html:with-paragraph (:stream stream)
             (html:with-rendition (:bold :stream stream)
               (w3p:present (documentation-function-name object) 'symbol :view w3p:+textual-view+ :stream stream))
             (html:break-line :stream stream)
             (w3p:present (documentation-function-arguments object) 'expression :view w3p:+textual-view+ :stream stream)
             (html:break-line :stream stream)
             (html:with-paragraph (:stream stream)
               (w3p:present (documentation-description object) 'string :view w3p:+textual-view+ :stream stream))))
          (t (w3p:present (documentation-function-name object) 'symbol :view w3p:+textual-view+ :stream stream)))
    object))

(defun document-function (hash-table name arguments description)
  "create a documentation object for a function in specified hash-table"
  (check-type name symbol)
  (setf (gethash name hash-table)
        (make-instance 'function-documentation
                       :name name
                       :arguments arguments
                       :description description)))

(defmacro document-functions (hash-table arg-lists)
  "invoke multiple calls of document-function on a list of arg-lists"
  `(progn ,.(loop for list in arg-lists
                  collect `(document-function ,hash-table ,@list) into result
                  finally (return result))))
   
(defun remove-function-documentation (hash-table name)
  "remove a function documentation object"
  (check-type name symbol)
  (remhash name hash-table))

(defun get-function-documentation (hash-table name &optional (errorp t))
  "get the doumentation object for a function from its symbol"
  (check-type name symbol)
  (let ((match (gethash name hash-table)))
    (unless (or match (not errorp))
      (error "Function documentation not found."))
    match))

(defvar *basic-function-documentation-table* (make-hash-table)
  "function documentation objects stored in hash table are keyed on symbols")

(defvar *presentation-function-documentation-table* (make-hash-table)
  "function documentation objects stored in hash table are keyed on symbols")

(defun clear-all-documentation ()
  (clrhash *basic-function-documentation-table*)
  (clrhash *presentation-function-documentation-table*))
       
(defun document-basic-function (name arguments description)
  (document-function *basic-function-documentation-table* name arguments description))

(defmacro document-basic-functions (arg-lists)
  `(document-functions *basic-function-documentation-table* ,arg-lists))

(defun get-basic-function-documentation (name &optional (errorp t))
  (get-function-documentation *basic-function-documentation-table* name errorp))

(defun document-presentation-function (name arguments description)
  (document-function *presentation-function-documentation-table* name arguments description))

(defmacro document-presentation-functions (arg-lists)
  `(document-functions *presentation-function-documentation-table* ,arg-lists))

(defun get-presentation-function-documentation (name &optional (errorp t))
  (get-function-documentation *presentation-function-documentation-table* name errorp))

    
;;;------------------------------------------------------------------- 
;;;
;;; PRESENTATION TYPE DOCUMENTATION
;;;

(eval-when (load eval compile)
  (w3p:define-presentation-type presentation-type () :inherit-from t :options (verbose)))

(w3p:define-presentation-method present (object (type presentation-type) stream (view html-view) &key)
  (w3p:with-presentation-type-options (presentation-type type)
    (cond
      (verbose
       (html:with-paragraph (:stream stream)
         (html:with-rendition (:bold :stream stream)
           (w3p:present (presentation-type-name object) 'symbol :view w3p:+textual-view+ :stream stream :acceptably nil))
         (html:break-line :stream stream)
         (html:with-rendition (:bold :stream stream)
           (write-string "Parameters: " stream))
         (w3p:present (presentation-type-parameters object) 'expression :view w3p:+textual-view+ :stream stream)
         (html:break-line :stream stream)
         (html:with-rendition (:bold :stream stream)
           (write-string "Options: " stream))
         (w3p:present (presentation-type-options object) 'expression :view w3p:+textual-view+ :stream stream)
         (html:break-line :stream stream)
         (html:with-rendition (:bold :stream stream)
           (write-string "Superiors: " stream))
         (w3p:present (presentation-type-superiors object) '(sequence presentation-type) :view w3p:+html-view+ :stream stream)
         (html:break-line :stream stream)
         (html:with-rendition (:bold :stream stream)
           (write-string "Description: " stream))
         (w3p:present (presentation-type-description object) 'string :view w3p:+textual-view+ :stream stream)))
      (t (html:note-anchor (w3p:present-to-string (presentation-type-name object) 'symbol :view w3p:+textual-view+ :acceptably nil)
                           :reference (concatenate 'string "presentation-type?w3p:"
                                                   (symbol-name (presentation-type-name object)))
                           :stream stream)))
    object))

;;;------------------------------------------------------------------- 
;;;
;;; DOCUMENTATION INTERFACE
;;;

(defun documentation-menu (stream)
  (flet ((divider (stream) (write-string " | " stream)))
    (declare (inline divider))
    (html:with-paragraph (:stream stream)
      (html:note-anchor "Overview" 
                        :reference "/cl-http/w3p/w3p.html"
                        :stream stream)
      (divider stream)
      (html:note-anchor "Functions &amp Macros"
                        :reference "/cl-http/w3p/basic-function?"
                        :stream stream)
      (divider stream)
      (html:note-anchor "Presentation Functions"
                        :reference "/cl-http/w3p/presentation-function?"
                        :stream stream)
      (divider stream)
      (html:note-anchor "Presentation Types"
                        :reference "/cl-http/w3p/presentation-type?"
                        :stream stream))))
    
(defun cvince-address (stream)
  (html:with-paragraph (:stream stream)
    (html:with-emphasis (:address :stream stream)
      (html:note-anchor "cvince@ai.mit.edu" :reference "mailto:cvince@ai.mit.edu" :stream stream))))

(defun server-note (stream)
  (html:with-paragraph (:stream stream)
    (html:horizontal-line :stream stream)
    (html:note-anchor http::*server-version* 
                      :reference "http://www.ai.mit.edu/projects/iiip/doc/cl-http/home-page.html" :stream stream)))

(defun show-all-functions (hash-table stream &key query-string &aux doc-list)
  "list all functions in the documentation hash table"
  (flet ((collect (key value)
           (declare (ignore key))
           (push value doc-list))
         (display-function-link (documentation)
           (html:enumerating-item (stream)
             (html:note-anchor (w3p:present-to-string documentation '((function-documentation) :verbose nil) 
                                                      :view w3p:+html-view+)
                               :reference (concatenate 'string query-string 
                                                       (symbol-name (documentation-function-name documentation)))
                               :stream stream))))
    (maphash #'collect hash-table)
    (setq doc-list (stable-sort doc-list  
                                #'(lambda (x y)
                                    (string< (symbol-name x)
                                             (symbol-name y)))
                                :key #'documentation-function-name))
    (html:with-enumeration (stream :definition)
      (mapc #'display-function-link doc-list))))

(defun show-basic-function-documentation (name stream)
  "display a documentation object"
  (let ((documentation (get-basic-function-documentation name nil)))
    (cond (documentation
           (w3p:present documentation '((function-documentation) :verbose t) :view w3p:+html-view+ :stream stream))
          (t (write-string "Function documentation not found." stream)))))

(defun show-presentation-function-documentation (name stream)
  "display a documentation object"
  (let ((documentation (get-presentation-function-documentation name nil)))
    (cond (documentation
           (w3p:present documentation '((function-documentation) :verbose t) :view w3p:+html-view+ :stream stream))
          (t (write-string "Function documentation not found." stream)))))

(defun show-all-presentation-types (stream &aux doc-list)
  "list all presentation types"
  (flet ((collect (key value)
           (declare (ignore key))
           (push value doc-list))
         (display-type-link (type)
           (html:enumerating-item (stream)
             (w3p:present type '((presentation-type) :verbose nil)  :view w3p:+html-view+ :stream stream))))
    (maphash #'collect *presentation-type-table*)
    (setq doc-list (stable-sort doc-list  
                                #'(lambda (x y)
                                    (string< (symbol-name x)
                                             (symbol-name y)))
                                :key #'presentation-type-name))
    (html:with-enumeration (stream :definition)
      (mapc #'display-type-link doc-list))))

(defun show-presentation-type-documentation (name stream)
  "display documentation for a presentation type"
  (let ((presentation-type (get-presentation-type name nil)))
    (cond (presentation-type
           (w3p:present presentation-type '((presentation-type) :verbose t) :view w3p:+html-view+ :stream stream))
          (t (write-string "Presentation type not found." stream)))))

(defmethod respond-to-basic-functions ((url url:http-search) stream)
  (http:with-conditional-get-response (stream :html :expires (url:expiration-universal-time url))
    (html:with-html-document (:stream stream)
      (html:with-document-preamble (:stream stream)
        (html:declare-base-reference url :stream stream)
        (html:declare-title "W3P Functions &amp Macros" :stream stream))
      (html:with-standard-document-body (:stream stream)
        (documentation-menu stream)
        (html:with-section-heading ("W3P Functions &amp Macros" :stream stream)
          (with-slots (url:search-keys) url
	    (handler-case
	      (let ((function-name (w3p:accept-from-string 'symbol (car url:search-keys))))
		(cond ((null url:search-keys) 
		       (html:with-paragraph (:stream stream)
			 (write-string "These functions and macros are part of the basic W3P system. They are used to define the standard set of presentation types, generic functions, and presentation methods that are included with W3P." stream))
		       (show-all-functions *basic-function-documentation-table* stream :query-string "basic-function?w3p:"))
		      (t (show-basic-function-documentation function-name stream))))
	      (w3p:input-not-of-required-type () (write-string "Invalid input provided to documentation search." stream)))))
      (server-note stream)))))

(defmethod respond-to-presentation-functions ((url url:http-search) stream)
  (http:with-conditional-get-response (stream :html :expires (url:expiration-universal-time url))
    (html:with-html-document (:stream stream)
      (html:with-document-preamble (:stream stream)
        (html:declare-base-reference url :stream stream)
        (html:declare-title "W3P Presentation Functions" :stream stream))
      (html:with-standard-document-body (:stream stream)
        (documentation-menu stream)
        (html:with-section-heading ("W3P Presentation Functions" :stream stream)
          (with-slots (url:search-keys) url
	    (handler-case 
	      (let ((function-name (w3p:accept-from-string 'symbol (car url:search-keys))))
		(cond ((null url:search-keys) 
		       (html:with-paragraph (:stream stream)
			 (write-string "These presentation functions are included with W3P.
Most are normal functions that invoke presentation-generic-functions with the same name.  
Through CLOS method combination, the proper presentation methods are then dispatched.  
Most presentation methods automatically have lexical access to presentation type parameters or options. 
Presentation methods can be defined for all these functions except for accept-from-string and present-from-string, 
which call accept and present respectively." stream))
		       (show-all-functions *presentation-function-documentation-table* stream
					   :query-string "presentation-function?w3p:"))
                    (t (show-presentation-function-documentation function-name stream))))
	      	    (w3p:input-not-of-required-type () (write-string "Invalid input provided to documentation search." stream)))))
      (server-note stream)))))

(defmethod respond-to-presentation-types ((url url:http-search) stream)
  (http:with-conditional-get-response (stream :html :expires (url:expiration-universal-time url))
    (html:with-html-document (:stream stream)
      (html:with-document-preamble (:stream stream)
        (html:declare-base-reference url :stream stream)
        (html:declare-title "W3P Presentation Types" :stream stream))
      (html:with-standard-document-body (:stream stream)
        (documentation-menu stream)
        (html:with-section-heading ("W3P Presentation Types" :stream stream)
          (with-slots (url:search-keys) url
	    (handler-case
	      (let ((type-name (w3p:accept-from-string 'symbol (car url:search-keys))))
		(cond ((null url:search-keys) 
		       (html:with-paragraph (:stream stream)
			 (write-string "These presentation type descriptions are automatically generated 
from the current W3P configuration." stream))
		       (show-all-presentation-types stream))
		      (t (show-presentation-type-documentation type-name stream))))
	      (w3p:input-not-of-required-type () (write-string "Invalid input provided to documentation search." stream)))))
      (server-note stream)))))

#| move to exports.lisp
(http:export-url #u"/cl-http/w3p/basic-function?"
                 :search
                 :response-function #'respond-to-basic-functions
                 :expiration `(:interval ,(* 15. 60.))
                 :keywords '(:cl-http :w3p :documentation)
                 :documentation "describe a basic w3p function")

(http:export-url #u"/cl-http/w3p/presentation-function?"
                 :search
                 :response-function #'respond-to-presentation-functions
                 :expiration `(:interval ,(* 15. 60.))
                 :keywords '(:cl-http :w3p :documentation)
                 :documentation "describe a w3p presentation function")

(http:export-url #u"/cl-http/w3p/presentation-type?"
                 :search
                 :response-function #'respond-to-presentation-types
                 :expiration `(:interval ,(* 15. 60.))
                 :keywords '(:cl-http :w3p :documentation)
                 :documentation "describe a w3p presentation type")
|#

;;;------------------------------------------------------------------- 
;;;
;;; DEFINE STATIC DOCUMENTATION
;;;

(document-basic-functions
  (
   ('w3p:define-presentation-type
    '(name parameters &key options inherit-from description)
    "Defines a presentation-type. Presentation-types usually have the
same name as a CLOS class. parameters and options are lambda-lists
that establish the format of a presentation-type-specifier for that
type. inherit-from is a symbol, a backquote form that evaluates to a
presentation-type-specifier, or an AND expression denoting multiple
inheritance. Only presentation type names matter for method
inheritance.")
   
   ('w3p:define-presentation-types
    '(arg-lists)    
    "Invoke multiple calls of define-presentation-type. Accepts a list of
argument lists to be passed to define-presentation-type.")
   
   ('w3p:remove-presentation-type
    '(name)
    "Removes a presentation type.  Accepts a symbol.")
   
   ('w3p:remove-presentation-types
    '(name-list)
    "Invokes multiple calls of remove-presentation-type.")
   
   ('w3p:define-presentation-generic-function
    '(generic-function-name presentation-function-name lambda-list &rest options)
    "Registers a presentation-generic-function with a specified lambda
list and options. Once a presentation-generic-function has been
defined, define-presentation-method can be used to specify its
accompanying methods. generic-function-name is a unique internal name
that is used for CLOS generic function dispatch.
presentation-function-name the user-visible name that is used in
calls to define-presentation-method. The first arguments of
lambda-list are type-key, and optionally one or both of parameters
and options. type also must be an argument somewhere in lambda-list.
Most developers will not need to define new
presentation-generic-functions.") 
   
   ('w3p:define-presentation-method
    '(presentation-function-name qualifiers specialized-lambda-list &body body)
    "Defines a presentation method for presentation-generic-function
presentation-function-name. qualifiers can be zero or more CLOS
method qualifiers. specialized-lambda-list is a CLOS specialized
lambda list for the presentation method. body is the body of the
presentation method, and may refer to the parameters and options
defined for the type specified in specialized-lambda-list.")
   
   ('w3p:define-presentation-view
    '(name class &optional errorp environment)
    "Defines an instance of a view class. Presentation methods are
dispatched by view class, so presentation functions can be called on
a view instance registered by define-presentation-view.")  
   
   ('w3p:define-presentation-view-class
    '(name &key inherits-from description)
    "Define a view class, allowing presentation method dispatch by view.
inherits-from is a list of other view classes.")
   
   ('w3p:define-presentation-view-classes
    '(arg-lists)
    "Invoke multiple calls of define-presentation-view-class Accepts a
list of argument lists to be passed to
define-presentation-view-class.")
   
   ('w3p:input-not-of-required-type
    '(object type)
    "When called inside an accept method, signals the error
input-not-of-required-type. This function does not return. This
function is provided mainly to facilitate CLIM compatability. W3P
accept methods should call the handle-input-error presentation
function instead.")
   
   ('w3p:presentation-type-of
    '(object &optional errorp environment)
    "Given a CLOS object, searches by class for the most specific defined
presentation type corresponding to it.")
   
   ('w3p:read-token
    '(stream)
    "Read a token from stream.")
   
   ('w3p:remove-presentation-generic-function
    '(presentation-function-name)
    "Removes a presentation-generic-function.  Accepts a symbol.")
   
   ('w3p:stream-default-view
    '(stream)
    "Returns the default view for a stream. Use setf to set a default
view.")
   
   ('w3p:presentation-type-superior-p
    '(presentation-type putative-superior)
    "Determines if putative-superior is a superior of presentation-type,
either directly or indirectly. Accepts the names of two presentation
types. This function is provided as a utility for writing
presentation methods such as presentation-subetypep.")
   
   ('w3p:with-presentation-type-options 
    '(type-name type &body body)    
"This macro binds the presentation type options for a
presentation-type-specifier.")
   
   ('w3p:with-presentation-type-parameters
    '(type-name type &body body)    
    "This macro binds the presentation type options for a
presentation-type-specifier.")
   
   ('w3p:with-presentation-type-decoded
    '((name-var &optional parameters-var options-var) type &body body)    
"This macro binds the specified variable to the corresponding parts
of presentation-type-specifier type.")
   ))

(document-presentation-functions
  (
   ('w3p:present-to-string
    '(object type &key view acceptably for-context-type string index)
    "Presents an object to a string. String and index are optional
arguments, other arguments are the same as for present.")
   
   ('w3p:accept-from-string
    '(type string &key view default default-type start end)
    "Accepts of a value of type from a string. Allows a default to be
specified, as well as substring parameters.")

   ('w3p:accept
    '(type &key stream view default default-type prompt prompt-mode display-default query-identifier insert-default present-p active-p)
    "Accepts an object of presentation-type type from stream. When
present-p is t, accept-present-default is called, generating a prompt
rather than actually parsing a value.")

   ('w3p:accept-present-default
    '(type stream view default default-supplied-p present-p query-identifier &key prompt prompt-mode display-default insert-default active-p)
    "Generates a prompt for a value of presentation-type type over
stream. This function is usually invoked by accept.")

   ('w3p:describe-presentation-type
    '(presentation-type &optional stream plural-count)
    "Describes presentation-type on stream. If stream is nil, a string is
returned. plural-count is either nil to denote singular form of the
name, t to denote plural, or a specific positive integer.")

   ('w3p:handle-input-error
    '(object type &key stream view)  
    "When called inside an accept method, responds to an input error.
Error reports may be generated over stream. A handle-input-error
method is responsible for returning two values, an object and a
presentation-type. If handle-input-error returns nil for a
presentation-type, no acceptable input was obtained. Error handling
is likely to be rather application specific. handle-input-error
allows the developer to specify what (or if) accept returns when it
encounters errors, signal special conditions, etc.")

   ('w3p:present
    '(object &optional type &key stream view acceptably for-context-type)
    "Present an object of presentation-type type over the stream. When
acceptably is t, object is presented in such a manner that it can be
parsed by accept. This only matters for presentation-types like
symbol, which may or may not have a package in its rendition.")
   
   ('w3p:presentation-subtypep
    '(type putative-supertype)
    "Determines if presentation-type type is a subtype of
putative-supertype. Returns two values, supertype and known-p.
supertype answers the question, and known-p specifies if the answer
was successfully determined.")

   ('w3p:presentation-type-specifier-p
    '(type)
    "This is a presentatation method that checks the validy of parameters
and options for a presentation-type-specifier.")

   ('w3p:presentation-typep
    '(object type)
    "Determines if object is of the presentation-type type. Accept
methods can use this function to determine if a value should be
returned, or handle-input-error invoked.")
   ))
