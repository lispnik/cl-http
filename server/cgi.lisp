;;;   -*- Mode: lisp; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-

;;;
;;; (c) Copyright 1995, John C. Mallery
;;;     All Rights Reserved.
;;;

;;;------------------------------------------------------------------- 
;;;
;;; COMMON GATEWAY INTERFACE
;;;
;;; This CGI 1.1 interface starts from the one described by NCSA, which
;;; can be found at: //hoohoo.ncsa.uiuc.edu/cgi/env.html

;;;Clean out the FORMATs sometime

(in-package :http)

(defconstant *cgi-version* "CGI/1.1")

(define-variable *cgi-variable-bindings* nil
                 "The bindings for CGI variables to forms that return their values.
The format is (cgi-keyword value-getter documentation).")

(defun format-cgi-documentation (&optional (stream *standard-output*))
  (format stream "~:{~&~A~*~&~A~&~%~}" *cgi-variable-bindings*))

(defun all-cgi-variables (&optional (package *keyword-package*))
  (flet ((get-symbol (entry)
           (intern (symbol-name (car entry)) package)))
    (declare (dynamic-extent #'get-symbol))
    (mapcar (if (eq package *keyword-package*) #'car #'get-symbol) *cgi-variable-bindings*)))

(defun %define-cgi-variable (variable value-getter documentation)
  (let* ((key (symbolize (string variable) *keyword-package*))
         (entry (assoc key *cgi-variable-bindings* :test #'eq)))
    (cond (entry
           (setf (second entry) value-getter
                 (third entry) documentation))
          (t (push-ordered `(,key ,value-getter ,documentation) *cgi-variable-bindings* #'string< :key #'car)))))

(define-macro define-cgi-variable (name &key value documentation)
  "Defines a variable in the Common Gateway Interface named NAME.
VALUE is a form that returns the variable value in the runtime server thread."
  `(%define-cgi-variable ',name ,value ',documentation))

(defun commafy (thing)
  (cond ((cdr thing)
         (loop for l = thing then (cdr l)
               while l
               for (item) = l
               collect item into list
               when (cdr l)
                 collect "," into list
               finally (return (apply #'concatenate 'string list))))
        (t (car thing))))

(declaim (inline server-get-cgi-header))

(defun server-get-cgi-header (server header)
  (commafy (server-get-raw-header server header)))

(defun get-cgi-header-binding (variable)
  (let ((string (string variable)))
    (cond ((string-equal "http_" string :start1 0 :end1 5 :start2 0 :end2 5)
           `(commafy
              (server-get-cgi-header ,(symbolize (nsubstitute #\- #\_ (subseq string 5) :test #'char-equal) *keyword-package*))))
          (t nil))))

(defun get-cgi-variable-binding (variable &aux entry)
  (flet ((get-key (var)
           (symbolize (symbol-name var) *keyword-package*)))
    (declare (dynamic-extent #'get-key))
    (cond ((setq entry (assoc (get-key variable) *cgi-variable-bindings* :test #'eq))
           (second entry))
          ((get-cgi-header-binding variable))
          (t (error "~S is not one of the known CGI variables, ~{~A~:^, ~}."
                    variable (mapcar #'car *cgi-variable-bindings*))))))

(defun get-cgi-variable-bindings (variables)
  (loop for var in variables
        for form = (get-cgi-variable-binding var)
        collect `(,var ,form))) 

(defun replace-symbol (old new tree)
  (typecase tree
    (symbol (if (eq old tree) new tree))
    (cons (loop for item in tree
                collect (replace-symbol old new item)))
    (t tree)))

;; A symbol macrolet would not work in MCL because it blows out when
;; when a special variable is macrolet. Tree replacement would seem the only
;; portable method. -- JCMa 7/9/1995.
(define-macro with-cgi-environment ((&rest variables) &body body)
  (let ((bindings (replace-symbol '*server* '.server. (get-cgi-variable-bindings variables))))
    `(let ((.server. *server*))                 ;avoid repeated special variable references
       ;; establish the symbol macro in the local context.
       (symbol-macrolet ,bindings ,.(replace-symbol '*server* '.server. body)))))


;;;------------------------------------------------------------------- 
;;;
;;; CGI SUPPORT FUNCTIONS
;;;

(defun cgi-translated-pathname (server)
  (let ((path (url:translated-pathname (server-url server))))
    (cond (path
           (write-to-string path :escape nil))
          (t nil))))

(declaim (inline cgi-request-method))

(defun cgi-request-method (server)
  (symbol-name (server-method server)))

(declaim (inline cgi-query-string))

(defun cgi-query-string (server)
  (search-suffix (server-url server)))

(defun %cgi-path-info (url-string parent-url-string)
  (let ((start (length parent-url-string))
        (end (or (position url::*search-url-delimiter* url-string)
                 (length url-string))))
    (and (> end start)
         (url::url-equal url-string parent-url-string :end1 start :end2 start)
         (subseq url-string start end))))

(define-generic cgi-path-info (url-or-server)
  (:documentation "Returns the CGI path-info for URL-OR-SERVER."))

(defmethod cgi-path-info ((url url:http-url))
  (let* ((url-string (name-string url))
         (parent (most-specific-exported-parent-url url-string -1)))
    (and parent
         (eq (translation-method parent) :cgi)
         (%cgi-path-info url-string (name-string parent)))))

(defmethod cgi-path-info ((server basic-server-mixin))
  (cgi-path-info (server-url server)))

(defun cgi-script-name (server)
  (let* ((string (url::name-string-without-search-suffix (server-url server)))
         (len (length string)))
    (subseq string (position #\/ string :start 7 :end len) len)))

;;;------------------------------------------------------------------- 
;;;
;;; DEFINE CGI VARIABLES
;;;

;; The following environment variables are not request-specific and are set
;; for all requests:

(define-cgi-variable
  server_software
  :value '(server-software-version *server*)
  :documentation "The name and version of the information server software answering the
request (and running the gateway). Format: name/version")

(define-cgi-variable
  server_name
  :value '(server-local-host-domain-name *server*)
  :documentation "The server's hostname, DNS alias, or IP address as it would appear in
self-referencing URLs.")

(define-cgi-variable
  gateway_interface
  :value '*cgi-version*
  :documentation "The revision of the CGI specification to which this server complies.
Format: CGI/revision")

;; The following environment variables are specific to the request being
;; fulfilled by the gateway program.

(define-cgi-variable 
  server_protocol
  :value '(server-http-version-string *server*)
  :documentation "The name and revision of the information protcol this request came in
with. Format: protocol/revision")

(define-cgi-variable
  server_port
  :value '(server-host-local-port *server*)
  :documentation "The port number to which the request was sent.")

(define-cgi-variable
  request_method
  :value '(cgi-request-method *server*)
  :documentation "The method with which the request was made. For HTTP, this is \"get\",
\"head\", \"post\", etc.")

;; not well defined.
(define-cgi-variable
  path_info
  :value '(cgi-path-info *server*)
  :documentation "The extra path information, as given by the client. In other words,
scripts can be accessed by their virtual pathname, followed by extra
information at the end of this path. The extra information is sent as
PATH_INFO. This information should be decoded by the server if it comes
from a URL before it is passed to the CGI script.")

;; not well defined.
(define-cgi-variable
  path_translated
  :value '(cgi-translated-pathname *server*)
  :documentation "The server provides a translated version of PATH_INFO, which takes the
path and does any virtual-to-physical mapping to it.")

;;; get the response function off it to compare them
(define-cgi-variable
  script_name
  :value '(cgi-script-name *server*)
  :documentation "A virtual path to the script being executed, used for self-referencing URLs.")

(define-cgi-variable
  query_string
  :value '(cgi-query-string *server*)
  :documentation "The information which follows the ? in the URL which referenced this
script. This is the query information. It should not be decoded in any
fashion. This variable should always be set when there is query
information, regardless of command line decoding.")

(define-cgi-variable
  remote_host
  :value '(server-host-domain-name *server*)
  :documentation "The hostname making the request. If the server does not have this
information, it should set REMOTE_ADDR and leave this unset.
CL-HTTP Format: a domain name or NIL")

(define-cgi-variable
  remote_addr
  :value '(server-host-ip-address *server*)
  :documentation "The IP address of the remote host making the request.")

(define-cgi-variable
  auth_type
  :value '(server-authentication-method-string *server*)
  :documentation "If the server supports user authentication, and the script is protects,
this is the protocol-specific authentication method used to validate the user.")

(define-cgi-variable
  remote_user
  :value '(user-name *server*)
  :documentation "If the server supports user authentication, and the script is
     protected, this is the username they have authenticated as.")

(define-cgi-variable
  remote_ident
  :value '(user-qualified-name *server*)
  :documentation "If user authentication is in use on a resource, then this variable
returns the qualified user name (realm|name). The 1.1 specification stated that
if the HTTP server supports RFC 931 identification, then this variable
will be set to the remote user name retrieved from the server. 
Usage of this variable should be limited to logging only.")

(define-cgi-variable
  content_type
  :value '(server-get-cgi-header *server* :content-type)
  :documentation "For queries which have attached information, such as HTTP POST and PUT,
this is the content type of the data.")

(define-cgi-variable
  content_length
  :value '(server-get-cgi-header *server* :content-length)
  :documentation "The length of the said content as given by the client.")

;;In addition to these, the header lines recieved from the client, if any, are
;;placed into the environment with the prefix HTTP_ followed by the header
;;name. Any - characters in the header name are changed to _ characters. The
;;server may exclude any headers which it has already processed, such as
;;Authorization, Content-type, and Content-length. If necessary, the server
;;may choose to exclude any or all of these headers if including them would
;;exceed any system environment limits.

;;An example of this is the HTTP_ACCEPT variable which was defined in CGI/1.0.
;;Another example is the header User-Agent.

(define-cgi-variable
  http_accept
  :value '(server-get-cgi-header *server* :accept)
  :documentation "The MIME types which the client will accept, as given by HTTP headers.
other protocols may need to get this information from elsewhere. each
item in this list should be separated by commas as per the http spec.
Format: type/subtype, type/subtype
CL-HTTP Format: a list of content type specifications.")

(define-cgi-variable
  http_referer
  :value '(server-get-cgi-header *server* :referer)
  :documentation "Not listed in CGI/1.1 but used in the examples.")

(define-cgi-variable
  http_user_agent
  :value '(server-get-cgi-header *server* :user-agent)
  :documentation "The browser the client is using to send the request.
General format: software/version library/version.")


;;;------------------------------------------------------------------- 
;;;
;;; SELF DOCUMENTATION CODE
;;;

;; set the documentation for WITH-CGI-ENVIRONMENT
(setf (documentation 'with-cgi-environment 'function)
      (format NIL "Binds the CGI variables in VARIABLES within the scope of BODY.
In addition to the variables listed below, any HTTP header may be accessed by
prefixing the header name with \"http_\".

CGI 1.1 Variables

~A" (format-cgi-documentation nil)))


;;;------------------------------------------------------------------- 
;;;
;;; LISP CGI
;;;

(define-variable *server-interface-version* (symbolize "SI/1.1" *keyword-package*))

(define-variable *server-interface-alist* nil
                 "The defined lisp interface for use in response functions.
The format is (SI-VERSION (si-keyword value-getter type documentation).")

(defun server-interface-alist (&optional (si-version *server-interface-version*) (error-p t))
  (cond ((second (assoc si-version *server-interface-alist*)))
        (error-p (error "Unknown server interface version, ~S." si-version))
        (t nil)))

(defun set-server-interface-alist (si-version value)
  (push `(,si-version ,value) *server-interface-alist*))

(defsetf server-interface-alist set-server-interface-alist)

(define format-server-interface-documentation (&key (version *server-interface-version*)
                                                    (types '(:variable :method))
                                                    (stream *standard-output*))
  (flet ((do-it (stream)
           (loop for (key value type documentation) in (server-interface-alist version)
                 when (member type types)
                   do (ecase type
                        ((:method :variable)
                         (format stream "~&~A (variable) ~*~&~A~&~%" key value documentation))
                        (:function
                          (format stream "~&~A (function) ~*~&~A~&~%" key value documentation))))))
    (cond (stream (do-it stream))
          (t (with-output-to-string (string)
               (do-it string))))))

(define map-server-interface (function &key (version *server-interface-version*) (types '(:variable :method)))
  "Maps FUNCTION over the server interfaces whose type belongs to TYPES in SI version ,VERSION."
  (loop for (key value-getter type documentation) in (server-interface-alist version)
        when (member type types)
          do (funcall function key type value-getter documentation)))

(define all-server-interface-variables (&key (version *server-interface-version*) (package *keyword-package*))
  "All server interface variables for version, version."
  (flet ((get-symbol (entry)
           (intern (symbol-name (car entry)) package)))
    (declare (dynamic-extent #'get-symbol))
    (mapcar (if (eq package *keyword-package*) #'car #'get-symbol) (server-interface-alist version))))

(defparameter *server-interface-types* '(:variable :method :function))

(defun %define-server-interface (variable value-getter si-version type documentation)
  (unless (member type *server-interface-types*)
    (error "TYPE, ~S, is not one of the known interface types, ~S." type *server-interface-types*)) 
  (flet ((make-entry (key getter type doc)
           `(,key ,getter ,type ,doc)))
    (declare (inline make-entry))
    (let* ((key (symbolize (string variable) *keyword-package*))
           (alist (server-interface-alist si-version nil))
           entry)
      (cond ((null alist)
             (setf (server-interface-alist si-version) `(,(make-entry key value-getter type documentation))))
            ((setq entry (assoc key alist :test #'eq))
             (setf (second entry) value-getter
                   (third entry) documentation))
            ((string< key (caar alist))
             (setf (server-interface-alist si-version) (cons (make-entry key value-getter type documentation) alist)))
            (t (push-ordered (make-entry key value-getter type documentation) (cdr alist) #'string< :key #'car))))))

(define-macro define-server-interface (name &key value (type :method)
                                            documentation
                                            (version '*server-interface-version*))
  "Defines a server interface for CL-HTTP.
These are the defined ways to access server state or operation from response functions."
  `(%define-server-interface ',name ,value ,version ',type ',documentation))

(defun get-server-interface-header-binding (variable)
  (let* ((string (string variable))
         (len (length string)))
    (cond ((and (< 7 len)
                (string-equal "HEADER-" string :start1 0 :end1 7 :start2 0 :end2 7))
           `(server-get-header *server* ,(symbolize string *keyword-package*)))
          (t nil))))

(defun get-server-interface-variable-binding (variable &optional (si-version *server-interface-version*) &aux entry)
  (flet ((get-key (var)
           (symbolize (symbol-name var) *keyword-package*)))
    (declare (dynamic-extent #'get-key))
    (let ((alist (server-interface-alist si-version t)))
      (cond ((setq entry (assoc (get-key variable) alist :test #'eq))
             (second entry))
            ((get-server-interface-header-binding variable))
            (t (error "~S is not one of the known Server Interface (~A) variables, ~{~A~:^, ~}."
                      variable (symbol-name si-version) (mapcar #'car alist)))))))

(defun get-server-interface-variable-bindings (variables)
  (loop for var in variables
        for form = (get-server-interface-variable-binding var)
        collect `(,var ,form))) 

;; A symbol macrolet would not work in MCL because it blows out when
;; when a special variable is macrolet. Tree replacement would seem the only
;; portable method. -- JCMa 7/9/1995.
(define-macro with-server-interface-environment ((&rest variables) &body body)
  (let ((bindings (replace-symbol '*server* '.server. (get-server-interface-variable-bindings variables))))
    `(let ((.server. *server*))                 ;avoid repeated special variable references
       ;; establish the symbol macro in the local context.
       (symbol-macrolet ,bindings ,.(replace-symbol '*server* '.server. body)))))


;;;------------------------------------------------------------------- 
;;;
;;; SERVER INTERFACES
;;;

(define-server-interface
  server-software
  :value '(server-software-version *server*)
  :type :method 
  :documentation "The name and version of the HTTP server software answering the
request. Format: name/version")

(define-server-interface
  server-name
  :value '(server-local-host-domain-name *server*)
  :type :method
  :documentation "The server's hostname, DNS alias, or IP address as it would appear in
self-referencing URLs.")

(define-server-interface
  server-interface
  :value '*server-interface-version*
  :type :variable
  :documentation "The revision of the server interface specification to which this server complies.
Format: SI/revision")

;; The following environment variables are specific to the request being
;; fulfilled by the gateway program.

(define-server-interface 
  server-protocol
  :value '(server-http-version *server*)
  :type :method
  :documentation "A keyword for name and revision of the information protcol this request came in
with. Format: protocol/revision")

(define-server-interface
  server-port
  :value '(server-host-local-port *server*)
  :type :method
  :documentation "The port number to which the request was sent.")

(define-server-interface
  request-method
  :value '(server-method *server*)
  :type :method
  :documentation "The HTTP method with which the request was made. This is :GET, :HEAD, :POST, :PUT, :DELETE.")

(define-server-interface
  translated-pathname
  :value '(server-translated-pathname *server*)
  :type :method
  :documentation "If the URL access by the active request to the server is stored in a file,
this returns the translated pathname, otherwise NIL.")

;;; get the response function off it to compare them
(define-server-interface
  active-url
  :value '(server-url *server*)
  :type :method
  :documentation "Returns the URL object which is the active URL in the current HTTP transaction.")

(define-server-interface
  search-keys
  :value '(server-url-search-keys *server*)
  :type :method
  :documentation "When the active URL is a search URL, this returns the parsed search
keys as a list of strings. The search keys are the information following the ?.
This information is unescaped according to the URL standard and each item (delimited
by +) becomes a single element in the returned list. Order is maintained. When the active
URL is not a search URL, this returns NIL.")

(define-server-interface
  remote-host
  :value '(server-host-domain-name *server*)
  :type :method
  :documentation "Returns the domain name of the host making the request as a string. 
If the domain name is unavailable, this returns NIL.")

(define-server-interface
  remote-address
  :value '(server-host-ip-address *server*)
  :type :method
  :documentation "Returns the IP address of the host making the request as a string.")

(define-server-interface
  authentication-method
  :value '(server-authentication-method *server*)
  :type :method
  :documentation "If the server supports user authentication, and the resource is protected,
this is the protocol-specific authentication method used to validate the user.")

(define-server-interface
  authenticated-user
  :value '(server-user-object *server*)
  :type :method
  :documentation "If the server supports user authentication, and the resource is
protected, this is the user object for the authenticated user.")

(define-server-interface
  header-content-type
  :value '(server-get-header *server* :content-type)
  :type :method
  :documentation "For queries which have attached information, such as HTTP POST and PUT,
this is the content type of the data. The format is (major-type minor-type &rest parameter-plist).")

(define-server-interface
  header-content-length
  :value '(server-get-header *server* :content-length)
  :type :method
  :documentation "The length of the said content as given by the client.")

(define-server-interface
  header-accept
  :value '(server-get-header *server* :accept)
  :type :method
  :documentation "The MIME types which the client will accept, as given by HTTP headers.
This is a list of content type specifications, each of the format 
(major-type minor-type &rest parameter-plist).")

(define-server-interface
  header-referer
  :value '(server-get-header *server* :referer)
  :type :method
  :documentation "This optional header is the URL string which refer to active URL being requested.")

(define-server-interface
  header-user-agent
  :value '(server-get-header *server* :user-agent)
  :type :method
  :documentation "The browser the client is using to send the request.
General format: software/version library/version.")


;;;------------------------------------------------------------------- 
;;;
;;; SET UP THE DOCUMENTATION FOR
;;;

;; set the documentation for WITH-SERVER-INTERFACE-ENVIRONMENT
(setf (documentation 'with-server-interface-environment 'function)
      (format NIL "Binds the Server Interface (SI) variables in VARIABLES within the scope of BODY.
In addition to the variables listed below, any HTTP header may be accessed by
prefixing the header name with \"header-\".

Server Interface 1.0 Variables

~A" (format-server-interface-documentation :version *server-interface-version*
                                           :types '(:variable :method) :stream nil)))
