"-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: url; Base: 10 -*-"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; cgi-application.lisp - version 1.0, October 20, 1998.
;; Support for using CGI applications with Mac CL-HTTP. 
;; 
;; Copyright (C) 1998 Terje Norderhaug and Media Design in*Progress.
;;
;; This module can be freely used as long as this license is kept in place
;; and eventual modifications are documented in a version history and submitted
;; to the author at <terje@in-progress.com>. Any submitted improvements
;; automatically fall under this license unless otherwise specified.
;;
;; This module is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#| INSTALLATION

1. Load and start the CL-HTTP server with demo configuration.
2. Load the defappleevents.lisp and the acgi.lisp modules.
3. Load the cgi-application.lisp module (this file).
4. Make sure that *url-areas* is T (setf http::*url-areas* T)

** A SIMPLE EXAMPLE: **

A. Start the "echo.acgi" applescript executable.
B. Evaluate the following:

  (http:export-url #u"/echo/" :cgi
    :pathname "ccl:cgi-bin;echo.acgi")

C. Access <http://www.yourserver.com/echo/path/name> from a web browser.

** A REAL EXAMPLE: **

Interaction is a stand-alone CGI implemented in LISP that generates dynamic, social, adaptive websites based on server-side XML. It can be downloaded from <http://interaction.in-progress.com>.

A. Start Interaction.
B. Evaluate the following:

(http:export-url #u"/interaction/" :cgi
  :pathname "ccl:cgi-bin;Interaction")

C. Access <http://www.yourserver.com/interaction/index> from a web browser.

|#

(eval-when (:load-toplevel :execute)
  (setf http::*url-areas* T))

(in-package :url)

(defclass cgi-application (http-path)
  ((application-pathname :initarg pathname :accessor cgi-application-pathname))
  (:documentation "Represents an external application that creates a response for URLs in an area"))

(defmethod intern-url-inferior ((superior cgi-application) (export-type (eql :cgi)) http-method url-string)
  (case http-method
    ((:head :get :post)  ; temporary limitation as all http methods should be passed to a CGI.
      (multiple-value-bind (url newly-interned-p)
        (intern-url url-string :if-does-not-exist :uninterned)
        (when newly-interned-p
          (change-class url 'cgi-url)
          (http::inherit-export-parameters url superior)
          (setf (superior url) superior))
        (values url newly-interned-p)))
     (T NIL)))

(defclass cgi-url (search-mixin http-path http-object)
  ((superior :accessor superior :initform NIL))
  (:documentation "A URL handeled by a CGI application"))

(eval-when (:load-toplevel :execute)
  (export 'cgi-application)
  (export 'cgi-url))

(in-package :http)

(defmethod export-url (url (translation (eql :cgi)) &rest args)
  (let ((application-pathname (pathname (getf args :pathname))))
    (url:initialize-specialization url 'url:cgi-application args)
    (setf (http::translation-method url) translation)
    (setf (url::cgi-application-pathname url) application-pathname)))

;; lose consing for effect.
;(defmethod post-document ((url url:cgi-url) type subtype stream)
;  (%write-cgi-response url stream (read-line stream nil)))

;; pretend to have a clue.
(defun %post-document-cgi-handle-url-coded-form (url stream)
  (using-resource (buffer post-form-buffer *post-form-buffer-size*)
    (%write-cgi-response url stream (read-delimited-line '(#\Return #\Linefeed) stream nil buffer))))

(defmethod post-document ((url url:cgi-url) (type (eql :application)) (subtype (eql :www-form-url-encoded)) stream)
  (%post-document-cgi-handle-url-coded-form url stream))

(defmethod post-document ((url url:cgi-url) (type (eql :application)) (subtype (eql :x-www-form-urlencoded)) stream)
  (%post-document-cgi-handle-url-coded-form url stream))

(defmethod write-document ((url url:cgi-url) (translation (eql :cgi)) stream)
  (%write-cgi-response url stream))

(defun %write-cgi-response (url stream &optional body)
  (with-cgi-environment (server_software
                         server_name
                         gateway_interface
                         server_protocol
                         server_port
                         request_method
                         path_info
                         path_translated
                         script_name
                         query_string 
                         remote_host
                         remote_addr
                         auth_type
                         remote_user     
                         remote_ident 
                         content_type
                         content_length
                         http_accept
                         http_referer
                         http_user_agent)
      (acgi:with-open-cgi (in (url::cgi-application-pathname (url::superior url))
                         :server-software server_software
                         :server-name server_name
                         :server-port (write-to-string server_port)
                         :request-method (string-upcase request_method)
                         :path-info path_info
                         :script-name (string-right-trim '(#\/) script_name)
                         :query-string query_string
                         :post-arguments body
                         :remote-host remote_host 
                         :remote-addr remote_addr
                         :remote-user remote_user
                         :password (subseq remote_ident (or (position #\| remote_ident) 0)) 
                         :content-type content_type
                         :http-referer http_referer
                         :http-user-agent http_user_agent)
      (stream-copy-until-eof in stream)
      (close stream))))

(in-package :acgi)

(defmethod http:stream-copy-until-eof ((from-stream cgi-input-stream) to-stream &optional copy-mode)
  ;; cgi input is already crlf encoded and possibly binary data
  (declare (ignore copy-mode))
  (loop while (or (response from-stream)
		  (stream-advance from-stream))
	for chunk = (atomic-pop (response from-stream)) 
	do (dotimes (ix (length chunk))
	     (write-byte (char-code (aref chunk ix)) to-stream))))

(defmethod http:stream-copy-until-eof ((from-stream cgi-input-stream) (to-stream buffered-binary-output-stream-mixin)
				       &optional copy-mode)
  ;; cgi input is already crlf encoded and possibly binary data
  (declare (ignore copy-mode))
  (loop while (or (response from-stream)
		  (stream-advance from-stream))
	for chunk = (atomic-pop (response from-stream))	;conses lists of arrays, poor.
	when chunk
	  do (stream-write-vector to-stream chunk 0 (length chunk))))      
