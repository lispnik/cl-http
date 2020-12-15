;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-
;;;
;;; (C) Copyright 1996-97, Christopher R. Vincent and John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CL-HTTP REMOTE SERVER CONFIGURATION
;;;

#|

allow server configuration over the web via md5 passswords. subnet security defaults
to the local host. uses the presentation system defined in package :w3p and
preference objects defined in preferences.lisp.

example of providing a simple, functional interface to an existing lisp
system. the form-function simply loops through a list of values used to
access preference objects, letting the presentation system worry about
generating the right kind of prompt.

for now, when input is invalid just don't change that value. (no error reports)

doesn't handle right now:
(log-dynamic-logging-on (current-access-logs) nil) is always set.

|#

(in-package :http)


;;;------------------------------------------------------------------- 
;;;
;;; WRITING SERVER CONFIGURATION FILES
;;;

(defmacro with-open-configuration-file ((var . args) &body body)
  "sets up a cl-http configuration file, binding the output stream to var, same args as with-open-file"
  `(with-open-file (,var ,@args)
     (let ((gmt-time (with-output-to-string  (string) (print-gmt-time string)))
           (*package* (find-package :http)))
       (declare (dynamic-extent gmt-time))
       (fast-format ,var
                    ";;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: ~A -*-
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CONFIGURATION FILE FOR CL-HTTP 
;;;

;;; File automatically generated on ~A

(in-package :http)

;; Initialize all server variables for current host
;; (reset-server-local-host-variables) and perform any other required
;; initializations.
(run-server-initializations t)~%~%" (package-name *package*) gmt-time)
       ,@body
       (fast-format ,var ";; Recache email addresses to be @ *default-mailer-host*
(email-address-for-bug-reports t)
(server-mail-address t)

;; Don't build a datastructure of the log entries
(log-dynamic-logging-on (current-access-logs) nil)

;; Make sure the log object has been initialized.
(ensure-current-log)

;; Write a common log file.
(log-file-logging-on (current-access-logs) t)

;; Password data is automatically initialized by the server.
(run-server-launch-initializations t)"))))

(define save-standard-server-configuration (&key (pathname *standard-configuration-pathname*))
  "Save the standard server configuration based on the current values."
  (with-open-configuration-file (file-stream pathname :direction :output :element-type *standard-character-type*
                                             :if-exists :supersede :if-does-not-exist :create)
    (map-standard-preferences #'(lambda (preference)
                                  (write-preference preference file-stream)
                                  (dotimes (n 2)
                                    (write-char #\Return file-stream))))))

(define standard-configure-server (&key (pathname *standard-configuration-pathname*))
  "Loads the standard server configuration file."
  (when pathname
    (load pathname ::verbose nil)))

(define standard-export-urls (&key (pathnames *standard-export-pathnames*) (clear-url-table-p t))
  "Loads PATHNAMES that export the standard URLs exported by the server.
When CLEAR-URL-TABLE-P, all previously exported URLs are uninterned."
  (when pathnames
    (when clear-url-table-p
      (url::clear-url-table)
      (export-web-configuration-interface))
    (loop for file in pathnames
          do (load file :verbose nil))))


;;;------------------------------------------------------------------- 
;;;
;;; CONFIGURATION FORM PROTOTYPE
;;;

;;; This form prototype specifies which preferences users are allowed to set
;;; through this interface.  also controls ordering of the layout. keywords
;;; are used to access preference objects.  set :http-port here?
(defparameter *remote-configuration-form-prototype*
              '(:listening-processes
                 :max-connections
                
                 :accept-write-methods
                 :authentication-data-pathname
                 :secure-subnets
                
                 :auto-export
                 :standard-export-pathnames
                
                 :mail-host
                 :bug-list
                 :maintainer
                 #+CCL-3 :network-mail-host
                 #+CCL-3 :store-and-forward-mail-hosts
                
                 #+MCL :host-name-for-apple-talk-operation
                 :resolve-ip
                 :url-host-name-resolution

                 
                 :log-resolve-ip
                 :log-times-in-gmt
                 :log-file-stream-stays-open
                 :log-directory
                 :log-class
                 :log-notifications             ;creates log objects must follow :log-class
                
                 :write-config-file
                 :config-pathname
                 ))

(define standard-preferences ()
  "Returns the preference objects governing the current server configuration."
  (loop for item in *remote-configuration-form-prototype*
        for preference = (find-preference item nil)
        when preference
          collect preference))

(define map-standard-preferences (function)
  "Maps function over the preference objects governing the current server configuration."
  (loop for item in *remote-configuration-form-prototype*
        for preference = (find-preference item nil)
        when preference
          do (funcall function preference)))


;;;------------------------------------------------------------------- 
;;;
;;; GENERATE WEB CONFIGURATION INTERFACE
;;;

(defmethod standard-preference-p ((preference preference))
  (member (preference-keyword preference) *remote-configuration-form-prototype*))

(defun show-preferences (stream types)
  (flet ((accept-preference (preference stream)
           (with-paragraph (:stream stream)
             (with-rendition (:bold :stream stream)
               (enumerating-item (stream)
                 (w3p:accept (preference-presentation-type preference)
                             :stream stream 
                             :view w3p:+html-view+ 
                             :present-p t
                             :default (funcall (preference-value-getter preference))
                             :prompt (preference-prompt preference) 
                             :prompt-mode :raw
                             :display-default nil
                             :query-identifier (preference-query-identifier preference)
                             :insert-default t
                             :active-p t)))
             (break-line :stream stream)
             (with-rendition (:italic :stream stream)
               (write-string (preference-description preference) stream)))))
    (declare (inline accept-preference))
    (with-emphasis (:quotation :stream stream)
      (with-font (:size 2 :stream stream)
        (with-rendition (:italic :stream stream)
          (with-section-heading ("Overview" :stream stream)
            (with-enumeration (stream :enumerate :type :capital-letters)
              (loop for type in types
                    for i from 0
                    do (let ((ref (princ-to-string i)))
                         (declare (dynamic-extent ref))
                         (enumerating-item (stream)
                           (note-anchor (preference-type-display-string type) :local-reference ref :stream stream)))))))))
    (horizontal-line :stream stream)
    (with-enumeration (stream :definition)
      (loop for type in types
            for i from 0
            for head = (preference-type-display-string type)
            for documentation = (preference-type-description type)
            do (flet ((note-preference-type (stream)
                        (let ((ref (princ-to-string i)))
                          (declare (dynamic-extent ref))
                          (with-anchor-noted (:tag ref :stream stream)
                            (with-font (:size 5 :stream stream)
                              (with-rendition (:bold :stream stream)
                                (fast-format stream "~C. ~A" (aref "ABCDEFGHIJKLMNOPQRSTUVWXYZ" i) head)))))))
                 (declare (dynamic-extent #'note-preference-type))
                 (enumerating-item (stream :head #'note-preference-type)
                   (when documentation
                     (write-string documentation stream))
                   (with-enumeration (stream :enumerate)
                     (loop for preference in (preference-type-inferiors type)
                           when (standard-preference-p preference)
                             do (accept-preference preference stream)))))))))

(define-cached-computation preference-types
                           (:life-time (* 60 60 60)     ;one hour
                            :documentation "The sorted preference types.")
  (loop for type being the hash-values of http::*preference-type-table*
        when (some #'standard-preference-p (preference-type-inferiors type))
          collect type into types
        finally (return (sort types #'string< :key #'preference-type-display-string))))

(defun %compute-configure-server (url stream)
  (flet ((write-heading (stream)
           (write-string "Configure CL-HTTP Server" stream)
           (break-line :stream stream)
           (write-string (local-context) stream)))
    (with-html-document (:declare-dtd-version-p t :stream stream)
      (with-document-preamble (:stream stream)
        (declare-base-reference url :stream stream)
        (declare-title (concatenate 'string "Configure CL-HTTP Server" ) :stream stream))
      (with-standard-document-body (:stream stream) 
        (with-section-heading (#'write-heading :stream stream)
          (write-string "This form allows maintainers to perform server configuration over the Web."
                        stream)
          (horizontal-line :stream stream)
          (with-paragraph (:stream stream)
            (let ((types (preference-types)))
              (if types
                  (with-fillout-form (:post url :stream stream)
                    (show-preferences stream types)
                    (horizontal-line :stream stream)
                    (with-paragraph (:stream stream)
                      (with-verbatim-text (:fresh-line nil :stream stream)
                        (with-rendition (:bold :stream stream)
                          (write-string "Action:  " stream))
                        (accept-input 'reset-button "RESET" :display-string "Reset" :stream stream)
                        (write-string "  " stream)
                        (accept-input 'submit-button "SUBMIT" :display-string "Configure" :stream stream))))
                  (write-string "No preference information available" stream))))
          (horizontal-line :stream stream)
          (cl-http-signature stream))))))

(defmethod compute-configure-server ((url url:http-form) stream)
  (with-successful-response (stream :html :content-location url
                                    :expires (url:expiration-universal-time url))
    (%compute-configure-server url stream)))

(defun report-configuration-error (url stream &key preference input-string)
  "Simply report an error as soon as invalid input is detected."
  (with-html-document (:declare-dtd-version-p t :stream stream)
    (with-document-preamble (:stream stream)
      (declare-base-reference url :stream stream)
      (declare-title "Configuration Error" :stream stream))
    (with-standard-document-body (:stream stream) 
      (with-section-heading ("Configuration Error" :stream stream)
        (with-paragraph (:stream stream) 
          (write-string "Error accepting the value for " stream)
          (with-rendition (:bold :stream stream)
            (write-string (preference-name preference) stream))
          (write-char #\. stream)
          (break-line :stream stream)
          (with-rendition (:bold :stream stream)
            (write-string "Description: " stream))
          (write-string (preference-description preference) stream))
        (with-paragraph (:stream stream) 
          (write-string "The input " stream)
          (write input-string :stream stream :escape t)
          (write-string " was determined not to be " stream)
          (with-rendition (:bold :stream stream)
            (w3p:describe-presentation-type (preference-presentation-type preference) stream))
          (write-char #\. stream))
        (with-paragraph (:stream stream) 
          (write-string "No values were changed or written to file." stream))
        (with-paragraph (:stream stream) 
          (horizontal-line :stream stream)
          (note-anchor *server-version* :reference *cl-http-home-page-url-string* :stream stream))))))

(defmethod respond-to-configure-server ((url url:http-form) stream query-alist)
  (flet ((valid-actions-vector (query-alist)
           (let ((*package* (find-package "HTTP")))
             (loop with valid-actions = (make-array 0 :adjustable t :fill-pointer t)
                   for (key raw-value) in query-alist
                   for preference = (find-preference key nil)
                   when (and preference (member key *remote-configuration-form-prototype*))
                     do (handler-case 
                          (multiple-value-bind (value) 
                              (w3p:accept-from-string (preference-presentation-type preference) raw-value)
                            (vector-push-extend `(,preference ,value) valid-actions))
                          (w3p:input-not-of-required-type 
                            () (report-configuration-error url stream :preference preference :input-string raw-value)))
                   finally (return valid-actions))))
         (set-valid-actions (valid-actions)
           (loop for (preference value) across valid-actions
                 do (set-active-preference preference value))))
    (multiple-value-bind (valid-actions abort-p)
        (valid-actions-vector query-alist)
      (when abort-p (return-from respond-to-configure-server))
      (disable-http-service)
      (run-server-initializations t)
      (set-valid-actions valid-actions)
      (email-address-for-bug-reports t)
      (server-mail-address t)
      (ensure-current-log)
      (log-dynamic-logging-on (current-access-logs) nil)
      (log-file-logging-on (current-access-logs) t)
      (run-server-launch-initializations t)
      (standard-export-urls :pathnames *standard-export-pathnames* :clear-url-table-p t)
      (enable-http-service :on-ports (list *standard-http-port*))
      (when *standard-configuration-write-file-p*
        (save-standard-server-configuration :pathname *standard-configuration-pathname*))
      ;; The response comes afterwards in case of an error and to get status code right.  3/15/97 -- JCMa.
      (with-successful-response (stream :html :content-location url
                                        :expires (url:expiration-universal-time url))
        (%compute-configure-server url stream)))))

(defun export-web-configuration-interface (&key (secure-subnets (list (http::local-host-ip-address)))
                                                (authentication-realm :server) (capabilities :webmasters))
  "Exports the web configuration interface for the server.
SECURE-SUBNETS is a list of authorized IP addresses
AUTHENTICATION-REALM and CAPABILITIES should be supplied when password access
control is also desired. AUTHENTICATION-REALM should be a digest realm.
This facility should not be used over a wide area network because digest authentication
does not protect from man-in-the-middle attacks."
  (let ((realm (and authentication-realm (intern-realm authentication-realm :if-does-not-exist :soft))))
    (export-url #u"/cl-http/maintenance/configure-server.html"
                :html-computed-form
                :form-function #'compute-configure-server
                :expiration '(:no-expiration-header)
                :response-function #'respond-to-configure-server
                :secure-subnets secure-subnets
                ;; Digest Authentication may be a useful control in addition to subnet security.
                :authentication-realm (when realm authentication-realm)
                :capabilities (when realm capabilities)
                :keywords '(:cl-http :maintenance :configuration)
                :documentation "Configure the http server over the web.")))
