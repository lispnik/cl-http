;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-

;;; (C) Copyright 1994-99, John C. Mallery
;;;     All Rights Reserved.
;;;


;;;------------------------------------------------------------------- 
;;;
;;; GENERIC SIGNALLING
;;;

(in-package :http)

(define-parameter *status-code-alist*
  '((100 "Continue")                            ;1.1
    (101 "Switching Protocols")                 ;1.1
    (200 "OK")
    (201 "Created")
    (202 "Accepted")
    (203 "Non-Authoritative Information")
    (204 "No Content")
    (205 "Reset Content")                       ;1.1
    (206 "Partial Content")                     ;1.1
    (300 "Multiple Choices")                    ;1.1
    (301 "Moved Permanently" document-moved-permanently)
    (302 "Found" document-moved-temporarily)
    (303 "See Other" document-forwarded)        ;1.1
    (304 "Not Modified")
    (305 "Use Proxy")                           ;1.1
    (306 "Switch Proxy")                        ;1.1
    (307 "Temporary Redirect")                  ;1.1
    (400 "Bad Request" bad-syntax-provided)
    (401 "Unauthorized" recoverable-unauthorized-client-access)
    (402 "Payment Required" access-requires-payment)
    (403 "Forbidden" access-forbidden)
    (404 "Not Found" document-not-found)
    (405 "Method Not Allowed" method-not-allowed)
    (406 "Not Acceptable" acceptable-resource-not-found)
    (407 "Proxy Authentication Required" unauthorized-proxy-access)
    (408 "Request Timeout" request-timeout)
    (409 "Conflict" document-put-conflict)
    (410 "Gone" document-gone)
    (411 "Length Required" content-length-required)     ;1.1
    (412 "Precondition Failed" precondition-failed)     ;1.1
    (413 "Request Entity Too Large" request-entity-too-large)   ; 1.1
    (414 "Request URI Too Long" request-uri-too-long)   ; 1.1
    (415 "Unsupported Media Type" unsupported-media-type)       ; 1.1
    (416 "Requested Range Not Satisfiable" invalid-range-request)       ; 1.1
    (417 "Expectation Failed")                  ; 1.1
    (500 "Internal Server Error" server-internal-error)
    (501 "Not Implemented" server-not-implemented)
    (502 "Bad Gateway" bad-gateway)
    (503 "Service Unavailable" service-unavailable)
    (504 "Gateway Timeout" gateway-timeout)
    (505 "HTTP Version Not Supported" http-version-not-supported)       ; 1.1
    (506 "Redirection Failed"))                 ; 1.1
  "An alist of status code number, documentation, and optionally, condition name.")

(define get-condition-for-status-code (code &optional no-error-p)
  (cond ((and (integerp code) (third (assoc code *status-code-alist* :test #'=))))
        (no-error-p nil)
        (t (error "Unknown status code."))))

(define get-string-for-status-code (code &optional no-error-p)
  (cond ((and (integerp code) (second (assoc code *status-code-alist* :test #'=))))
        (no-error-p nil)
        (t (error "Unknown status code."))))

(define client-signal-http-code (url code method &key headers reason version)
  (let ((cond-type (get-condition-for-status-code code t)))
    (cond (cond-type
           (signal cond-type :url url :method method :headers headers
                   :reason reason :version version))
          (t (error "Applying Method ~S to ~A elicited a ~D (~A) in HTTP version ~A. ~:[~;~:*~{~A: ~A~^~}~]"
                    method (url:name-string url) code reason *http-version*
		    (and headers (header-set-header-plist headers #'header-raw-value)))))))

(define describe-status-codes (&optional (stream *standard-output*) (print-legend-p t))
  "Describes the HTTP status codes on stream."
  (loop initially (when print-legend-p
                    (format stream "~&~2TCode ~8TDescription ~40TCondition Class~2%"))
        for (code description error-class) in *status-code-alist*
        do (format stream "~&~2T~D~8T~A~:[~;~40T~:*~(~S~)~]" code description error-class)))


;;;------------------------------------------------------------------- 
;;;
;;; NETWORK ERRORS
;;;

(define-condition http-host-stopped-responding
                  (www-utils:host-stopped-responding www-utils:network-error-mixin)
  ((host :initform nil :initarg :host :reader http-host)
   (port :initform 80 :initarg :port :reader http-port))
  (:report (lambda (cond stream)
             (let ((host (http-host cond))
                   (port (http-port cond)))
               (format stream "The host, ~A, stopped responding on port ~D."
                       (host-domain-name host) port)))))

;;;------------------------------------------------------------------- 
;;;
;;; CONDITIONS
;;;

(define-condition http-condition
                  (condition)
  ((close-connection-p :initform nil :initarg :close-connection :reader http-close-connection-p)))

(define close-connection-p (condition)
  "Returns non-null unless CONDITION warrants that the connekction may safely remain open."
  (typecase condition
    (http-condition (http-close-connection-p condition))
    ;;always close connections when error state unkonwn.  1/12/97 -- JCMa.
    (t nil)))

(define-condition http-abort (http-condition) ())

(define-condition reportable-condition
                  (http-condition)
  ((url :initform nil :initarg :url :reader http-url)
   (method :initform nil :initarg :method :reader http-method)
   (status-code :reader status-code)
   (reason :initform nil :initarg :reason :reader http-reason)
   (version :initform *http-version* :initarg :version :reader http-version)
   (headers :initform nil :initarg :headers :reader http-transaction-headers)
   (format-string :initform nil :initarg :format-string :reader format-string)
   (format-args :initform nil :initarg :format-args :reader format-args))
  (:report report-status-message))

(define-condition client-condition (http-condition) ())

(define-condition server-condition (http-condition) ())

(define-condition client-reportable-condition (reportable-condition client-condition) ())

(define-condition bad-syntax-provided
                  (client-reportable-condition)
  ((status-code :initform 400)
   (reason :initform "Bad Request")
   (close-connection-p :initform (default-condition-close-connection-p)
                       :initarg :close-connection :reader http-close-connection-p)))

(define-condition request-missing-host-header
                  (bad-syntax-provided)
  ((reason :initform "Bad Request: Missing Host Header")))

(define-condition unknown-virtual-host
                  (bad-syntax-provided)
  ((reason :initform "Bad Request: Unknown Virtual Host")))

(define-condition bad-range-header-value
                  (bad-syntax-provided)
  ((reason :initform "Bad Request: Ill-Formed Range"))
  (:documentation "Signalled when the value for a range header is ill-formed."))

(define-condition bad-cookie-header-value
                  (bad-syntax-provided)
  ((reason :initform "Bad Request: Ill-Formed Cookie"))
  (:documentation "Signalled when the value for a cookie header is ill-formed."))

(define-condition bad-server-response
                  (bad-syntax-provided)
  ((reason :initform "Bad Response: Ill-Formed Server Response")
   (response :initform nil :initarg :response))
  (:documentation "Signalled a server response to an HTTP request is unparsable."))

(define-condition bad-form-data-posted
                  (bad-syntax-provided)
  ((status-code :initform 400)
   (reason :initform "Bad Form Data Posted: Client provided unparsable URL-encoded data.")))

(define-condition bad-escaping
		  (bad-syntax-provided)
  ((reason :initform "Bad Escaping: Error while unescaping a URL or string."))
  (:documentation "Signalled when an ill-formed escape sequence is encountered while unescaping a URL or string."))

(define-condition request-missing-content-type-header
                  (bad-syntax-provided)
  ((reason :initform "Bad Request: Missing Content-Type Header")))

(define-condition access-control-condition
                  (client-reportable-condition)
  ())

(define-condition unauthorized-access (access-control-condition) ())

(define-condition unauthorized-client-access (unauthorized-access) ())

(define-condition recoverable-unauthorized-client-access
                  (unauthorized-client-access)
  ((authentication-method :initarg :authentication-method :reader http-authentication-method)
   (authentication-realm :initarg :authentication-realm :reader http-authentication-realm)
   (status-code :initform 401)
   (reason :initform "Unauthorized")))

(define-condition unknown-authentication-method
                  (access-control-condition)
  ((authentication-method :initarg :authentication-method :reader http-authentication-method)
   (status-code :initform 400)))

(define-condition client-access-with-stale-nonce (recoverable-unauthorized-client-access) ())

(define-condition access-requires-payment
                  (access-control-condition)
  ((status-code :initform 402)
   (reason :initform "Payment Required")))

(define-condition access-forbidden
                  (access-control-condition)
  ((status-code :initform 403)
   (reason :initform "Access Forbidden")))

(define-condition document-not-found
                  (client-reportable-condition)
  ((status-code :initform 404)
   (reason :initform "Not Found")))

(define-condition too-many-redirects
                  (document-not-found)
  ((reason :initform "Too Many Redirects")
   (n-redirects :initform 0 :initarg :n-redirects))
  (:documentation "Used to signal that the document was not found
because too many redirects were encountered."))

(define-condition method-not-allowed
                  (access-control-condition)
  ((status-code :initform 405)
   (method :initform nil :initarg :method :reader http-method)
   (reason :initform "Method Not Allowed")))

;; needs to send the headers -- JCMa 5/29/1995.
(define-condition acceptable-resource-not-found
                  (access-control-condition)
  ((status-code :initform 406)
   (headers :initform nil :initarg :headers :reader headers)
   (reason :initform "None Acceptable")))

(define-condition unauthorized-proxy-access
                  (unauthorized-access)
  ((status-code :initform 407)
   (reason :initform "Proxy Authentication Required")))

(define-condition request-timeout
                  (access-control-condition)
  ((status-code :initform 408)
   (reason :initform "Request Timeout"))) 

(define-condition document-put-conflict
                  (access-control-condition)
  ((status-code :initform 409)
   (reason :initform "Conflict"))) 

(define-condition document-gone
                  (document-not-found)
  ((status-code :initform 410)
   (reason :initform "Gone")))

(define-condition content-length-required
                  (client-reportable-condition)
  ((status-code :initform 411)
   (reason :initform "Length Required")))

(define-condition precondition-failed
                  (client-reportable-condition)
  ((status-code :initform 412)
   (reason :initform "Precondition Failed")))

(define-condition request-entity-too-large
                  (client-reportable-condition)
  ((status-code :initform 413)
   (reason :initform "Reques Entity Too Large")
   (retry-after :initform nil)))                ; should send a retry after header when non-null

(define-condition request-uri-too-long
                  (bad-syntax-provided)
  ((status-code :initform 414)
   (reason :initform "Request URI Too Long")))

(define-condition unsupported-media-type
                  (client-reportable-condition)
  ((status-code :initform 415)
   (reason :initform "Unsupported Media Type")))

(define-condition invalid-range-request
                  (bad-syntax-provided)
  ((status-code :initform 416)
   (reason :initform "Requested Range Not Valid")))

(define-condition server-reportable-condition
                  (reportable-condition server-condition) ())

(define-condition server-internal-error
                  (server-reportable-condition)
  ((status-code :initform 500)
   (server-error :initform nil :initarg :server-error :reader server-error)))

(define-condition error-computing-response
                  (server-internal-error)
  ((headers :initform nil :initarg :headers :reader http-headers)
   (stack-backtrace :initform nil :initarg :stack-backtrace :reader http-stack-backtrace)))

(define-condition error-handling-post-method
                  (error-computing-response)
  ((form-alist :initform nil :initarg :form-alist :reader http-form-alist)))

(define-condition error-handling-put-method (server-internal-error) ())

(define-condition error-handling-delete-method (server-internal-error) ())

(define-condition server-error
                  (server-reportable-condition)
  ((close-connection-p :initform t :initarg :close-connection  :reader http-close-connection-p)))

(define-condition server-not-implemented
                  (server-error)
  ((status-code :initform 501)))

(define-condition unsupported-method
                  (server-not-implemented)
  ((method :initarg :method :reader http-method)))

(define-condition bad-gateway
                  (server-error)
  ((status-code :initform 502)))

(define-condition service-unavailable
                  (server-error)
  ((status-code :initform 503)))

(define-condition server-overloaded (service-unavailable) ())

(define-condition gateway-timeout
                  (server-error)
  ((status-code :initform 504)))

(define-condition http-version-not-supported
                  (server-error)
  ((status-code :initform 505)))

(define-condition client-timed-out (gateway-timeout) ())

(define-condition redirection (reportable-condition) ())

(define-condition document-moved
                  (redirection)
  ((new-urls  :initarg :new-urls :reader new-urls)
   (target-window :initform nil :initarg :target-window :reader http-target-window)))

(define-condition document-moved-permanently
                  (document-moved)
  ((status-code :initform 301)
   (reason :initform "Document Moved Permanently")))

(define-condition document-moved-temporarily
                  (document-moved)
  ((status-code :initform 302)
   (reason :initform "Document Moved Temporarily")))

(define-condition document-forwarded
                  (document-moved)
  ((status-code :initform 303)
   (reason :initform "See Other")))
