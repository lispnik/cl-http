;-*- Mode: Lisp; Package: acgi -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; acgi.lisp - version 1.0.1, October 20, 1998.
;; Streams for communication between web server and Mac CGI 
;; applications using Apple Events.
;; <http://www.in-progress.com/src/acgi.lisp>
;; 
;; Copyright (C) 1997-98 Terje Norderhaug and Media Design in*Progress.
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Library General Public
;; License version 2 as published by the Free Software Foundation.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; Library General Public License version 2 for details.
;; 
;; The GNU Library General Public License is available on the web
;; from <http://www.gnu.org/copyleft/lgpl.html> or by writing to the
;; Free Software Foundation, Inc., 59 temple Place - Suite 330,
;; Boston, MA 02111-1307, USA. 
;;
;; Note that this is a different license than the more restrictive GNU GPL license.
;; If this license by any reason doesn't suit your needs, please contact
;; the copyright holder of this library by sending an email to
;; Terje Norderhaug at <terje@in-progress.com>. Please submit modifications of
;; the library to the same address. Any submitted modifications will 
;; automatically fall under the terms of this license unless otherwise specified.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| EXAMPLE OF QUERYING A CGI APPLICATION FROM MCL:

1. Load this library into MCL.

2. Start up the "Interaction" CGI application found on the MCL CD-ROM
   or downloaded from <http://interaction.in-progress.com>

3. Evaluate the following to query the Interaction application via CGI:

(acgi:with-open-cgi (in "Interaction")
  (loop
    for line = (read-line in NIL)
    while line
    collect line))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ccl)

(eval-when (:execute :load-toplevel)
  ; (require :defappleevents)
  (require :appleevent-toolkit)
  (require :io-buffer))

(defpackage acgi
  (:export
    "OPEN-CGI" 
    "WITH-OPEN-CGI"
    "WITH-CGI-OUTPUT")
  (:import-from :ccl
    "AE-GET-PARAMETER-CHAR"
    "AE-ERRORP-HANDLER"
    "BUFFERED-CHARACTER-INPUT-STREAM-MIXIN"
    "BUFFERED-CHARACTER-OUTPUT-STREAM-MIXIN"
    "IO-BUFFER-OUTPTR"
    "IO-BUFFER-OUTSIZE"
    "IO-BUFFER-OUTCOUNT"
    "IO-BUFFER-OUTBUF"
    "IO-BUFFER-LISTEN"
    "IO-BUFFER-EOFP"
    "IO-BUFFER-ADVANCE"
   "IO-BUFFER-FORCE-OUTPUT"
    "STREAM-BYTES-TRANSMITTED"
    "STREAM-IO-BUFFER"
    "STREAM-FINISH-OUTPUT"
    "GET-SENDER-ADDRESS"
    "%FREE-AEDESC"
    "SET-SLOT-VALUE")
  (:use
    "CCL" 
    "COMMON-LISP"
    "DEFAPPLEEVENTS"))

#| TODO

Need to implement support for streaming post-arguments.
WSAPI_ReadHTTPData:
Read incoming data from a HTTP client (e.g. post args).
Should be supported both for input and output.
Circumvents a 32K limit on POST args as it can be called multiple times.
Can lead to a timeout if called when no data is pending.
|#

(in-package :acgi)

(defappleevent acgi :|WWW½| :|sdoc|
  ((direct-object #$keyDirectObject)
   (path-args :|----|)
   (search-args :|kfor|)
   (username :|user|)
   (password :|pass|)
   (from-user :|frmu|)
   (client-address :|addr|)
   (server-address :|svnm|) ; The name of the web server
   (server-IP-port :|svpt|) ; The TCP port on which the server listens for connections
   (script-name :|scnm|)    ; The URL path to the CGI application
   (content-type :|ctyp|)   ; The MIME type of POST data, if present
   (referrer :|refr|)       ; The URL the visitor came from
   (user-agent :|Agnt|)     ; The product code name of the client software
   (action :|Kact|)         ; The type of CGI being run, e.g. "CGI", "ACGI" "PREPROCESSOR", or name of action 
   (action-path :|Kapt|)    ; When running as an action, this contains the path to the action.    
   (post-arguments :|post|) 
   (method :|meth|)
   (client-ip-address :|Kcip|)
   (full-request :|Kfrq|)   ; The entire text of the request as received by the server
   (directory-path :|DIRE|)
   (connection-ID :|Kcid|))  ; :desired-type #$typeinteger Identifies the connection
  ())

(defappleevent partial :|WWW½| :|SPar|
  ((direct-object #$keyDirectObject)
   (connection-ID :|Kcid|)  ; :desired-type #$typeinteger
   (more :|Kmor| :desired-type #$typeBoolean))
   ())
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-open-cgi ((label filename &rest rest) 
                         &rest body)
  "Open a stream with a request to a CGI application"
  `(with-open-stream (,label (open-cgi ,filename 
                               ,@(apply #'append rest 
                                   (remove-if-not #'keywordp body 
                                      :key #'(lambda (item)
                                               (when (listp item)
                                                  (car item)))))))
     ,@(remove-if #'keywordp body 
         :key #'(lambda (item)
                  (when (listp item)
                    (car item))))))

(defun open-cgi (filename &rest rest)
  "Opens a stream to read a response for a CGI request"
  (apply #'make-instance 'cgi-input-stream
    :filename filename rest))

(defvar last-id 0)

(defun generate-id ()
  "Generates a long integer to identify a CGI connection"
  (mod 
    (without-interrupts 
      (incf last-id))
    #,(expt 256 4)))

(defvar cgi-connections (make-hash-table :size 8))

(defclass cgi-input-stream (input-stream)
  ((filename :reader filename :initarg :filename)
   (response :accessor response :type list :initform NIL)
   (index :initform 0)
   (end :initform 0)
   (completed :accessor completed-p :initform NIL
     :documentation "True when the full response has been received from the CGI")
   (timeout :accessor timeout 
     :initform (* 15 60)
     :initarg :timeout
     :documentation "Max time to wait without a response from the CGI")
   (id :reader id :initform (generate-id)))
  (:documentation "Stream to send a CGI request and receive an asynchronous response"))

(defmethod initialize-instance :after ((stream cgi-input-stream) 
  &key filename mode timeout
     ;; CGI 
       server-software 
       server-name
       gateway-interface
       server-protocol
       server-port
       request-method
       path-info
       path-translated
       script-name       
       query-string
       remote-host 
       remote-addr
       auth-type
       remote-user
       remote-ident
       content-type
       content-length
       http-user-agent
       http-accept
       http-referer
     ;; Mac Extensions:
       password
       post-arguments
       full-request
       action
       action-path
       directory-path)
  ;; try to look moderately competent
  (declare (ignore mode timeout server-software gateway-interface
		   server-protocol path-translated auth-type remote-ident content-length http-accept))
  (setf (gethash (id stream) cgi-connections) stream)
  (with-aedescs (event reply target)
      (create-named-process-target target (file-namestring filename))
      (make-acgi event target
        :username (or remote-user "")
        :password (or password "")
        :from-user ""
        :client-address (or remote-host remote-addr "")
        :server-address (or server-name "")
        :server-ip-port (or server-port "80")
        :method (or request-method "GET")
        :script-name (or script-name "/")
        :content-type (or content-type "")
        :referrer (or http-referer "")
        :user-agent (or http-user-agent "")
        :action (or action "PREPROCESSOR")
        :action-path (or action-path "")
        :post-arguments (or post-arguments "")
        :search-args (or query-string "")
        :path-args (or path-info "")
        :client-ip-address (or remote-addr "0.0.0.0")
        :full-request (or full-request "")
        :directory-path (or directory-path "")
        :connection-id (id stream))
      (install-queued-reply-handler event 'cgi-reply-handler (id stream))
      (send-appleevent event reply 
         :reply-mode :queue-reply
         :can-switch-layer T
         :timeout (timeout stream)
         :filterproc appleevent-filter-proc)))

(defmethod append-response ((stream cgi-input-stream) response &optional more)
  (assert (not (completed-p stream)) () 
    "A CGI ~A already claimed the response is complete, yet it sent an event with more data" (filename stream))
  (without-interrupts
    (unless (or (null response) (equal response ""))
      (cond
        ((response stream)
          (setf (response stream)
            (append (response stream)
              (list response))))
        (T
          (set-slot-value stream 'index 0)
          (set-slot-value stream 'end (length response))
          (setf (response stream) (list response)))))
     (setf (completed-p stream) (not more)))
     response)

(defmethod cgi-reply-handler (application appleevent reply refcon)
  (declare (ignore application reply))
  (let ((stream (gethash refcon cgi-connections)))
    (when stream
      (let ((response (ae-get-parameter-char appleevent #$keyDirectObject nil)))
        (unless (equal response "<SEND_PARTIAL>")
          (append-response stream response))))))

(defmethod cgi-partial-handler (application event reply refcon)
  (declare (ignore application refcon))
  (let* ((id (partial-connection-id event))
         (stream (gethash id cgi-connections))
         (response (ae-get-parameter-char event #$keyDirectObject nil))
         (more (partial-more event)))
    (cond
      (stream
        (append-response stream response more))
      (T
        (ccl::ae-put-parameter-longinteger reply #$keyErrorNumber #$userCanceledErr nil)
        (ccl::ae-put-parameter-char reply #$keyErrorString "Request Aborted" nil)))))

(install-appleevent-handler :|WWW½| :|SPar| #'cgi-partial-handler)

(defmethod stream-advance ((stream cgi-input-stream))
  "Called when there is no data in the input buffer - return T if there is data afterwards"
  (unless
    (process-wait-with-timeout "CGI Response" (timeout stream)
      #'(lambda ()
          (or 
             (stream-listen stream)
             (stream-eofp stream))))
    (error "Timeout after waiting ~A ticks for CGI response for ~A" (timeout stream) stream))
  (not (stream-eofp stream)))

(defmethod stream-listen ((stream cgi-input-stream))
  (if (response stream) 
    (not (stream-eofp stream))))

(defmethod stream-tyi ((stream cgi-input-stream))
  (without-interrupts
    (when (or (response stream)
              (stream-advance stream))
      (let ((idx (slot-value stream 'index)))
        (set-slot-value stream 'index (ccl::%i+ idx 1))
        (prog1
          (elt (first (response stream)) idx)
          (when (= (slot-value stream 'index) (slot-value stream 'end))
            (pop (response stream))
            (set-slot-value stream 'index 0)
            (set-slot-value stream 'end (length (first (response stream))))))))))

(defmethod stream-untyi ((stream cgi-input-stream) char)
  (without-interrupts
    (let ((index (slot-value stream 'index)))
      (cond
        ((= index 0)
          (push (string char) (response stream))
          (set-slot-value stream 'index 0)
          (set-slot-value stream 'end 1))
        (T 
          (unless (eq char (aref (first (response stream)) (1- index)))
            (error "Attempt to unread illegal char ~S" char))
          (set-slot-value stream 'index (1- index)))))))

(defmethod stream-clear-input ((stream cgi-input-stream))
  (without-interrupts 
    (setf (response stream) NIL)
    (set-slot-value stream 'index 0)
    (set-slot-value stream 'end 0)))

(defmethod stream-eofp ((stream cgi-input-stream))
  (and 
    (null (response stream))
    (completed-p stream)))

(defmethod stream-close :before ((stream cgi-input-stream))
  (remhash (id stream) cgi-connections))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ANSWERING A CGI REQUEST:

(defmacro with-cgi-output ((label event reply &key class) 
                                      &rest body)
  "Executes body with variable bound to a stream to reply to a CGI request"
  (assert (and label event reply))
 `(with-open-stream (,label (make-instance (or ,class 'cgi-output-stream)
                               :event ,event
                               :reply ,reply))
       ,@body))

(defparameter *allow-partial-responses* NIL 
  "If true, the response can be passed back to the server as partial events, improving the response time")

(defclass cgi-output-stream (ccl::BUFFERED-CHARACTER-OUTPUT-STREAM-MIXIN
                             ccl::buffered-binary-output-stream-mixin)
  ((event :accessor ae-event :initarg :event)
   (reply :accessor ae-reply :initarg :reply)
   (id :accessor id)
   (opened :initform 1
     :documentation "How many times to call stream-close before stream closes")
   (closed :accessor stream-closed-p :initform NIL
     :documentation "True if the stream is closed"))
  (:default-initargs
   :outsize (* 16 1024))
  (:documentation "A stream for returning the reply of a request using appleevents"))

(defmethod initialize-instance ((stream cgi-output-stream) &rest rest &key event outsize)
  (setf (id stream)
    (when *allow-partial-responses*
      (handler-case
        (acgi-connection-id event)
        (appleevent-error () NIL))))
  (cond
    ((id stream)
      (call-next-method)
      ;; speed up response by providing a small chunk first:
      (setf (io-buffer-outcount (stream-io-buffer stream))
        (* 4 1024)))
    (T
      (apply #'call-next-method stream
        :outsize (max (* 128 1024) outsize)
        rest))))

(defmethod io-buffer-force-output ((stream cgi-output-stream) io-buffer count finish-p)
  ;; this can definitely be speeded up by e.g. eliminating multiple accesses to slots
  (unless (stream-closed-p stream)
    (let ((outbuf (io-buffer-outbuf io-buffer))
          (event (ae-event stream))
          (reply (ae-reply stream)))
      (assert outbuf ())
      (handler-case
        (cond
          ((and finish-p 
              (or (= count (stream-bytes-transmitted stream)) ; only one chunk
                  (null (id stream))))
            (ae-put-parameter-string-ptr reply #$keyDirectObject 
              outbuf count)
            (resume-free-event event reply))
          ((id stream)
             (send-partial
               (get-sender-address event reply)
               (id stream)
                outbuf 
                count
                finish-p)
             (%setf-macptr (io-buffer-outptr io-buffer) outbuf)
             (setf (io-buffer-outcount io-buffer)
               (io-buffer-outsize io-buffer))))
        (appleevent-error (condition)           
           (setf (stream-closed-p stream) T) ;; avoid that stream-close tries to force output
           (stream-close stream)
           (signal condition)
         )))
      count))

(defmethod ccl::stream-position ((stream cgi-output-stream) &optional dummy)
  (declare (ignore dummy))
  (stream-bytes-transmitted stream))

(defmethod ccl::stream-suspend ((stream cgi-output-stream))
  "Return the stream after setting it to ignore the next stream-close"
  (without-interrupts (incf (slot-value stream 'opened)))
  stream)

(defmethod stream-close ((stream cgi-output-stream))
  (when (zerop (without-interrupts (decf (slot-value stream 'opened))))
    (stream-finish-output stream)
    (call-next-method)
    (setf (stream-closed-p stream) T)))

(defun ae-put-parameter-string-ptr (the-desc keyword ptr length &optional (errorp T))
  "Assign a pointer to a string as the parameter for an apple event"
  ;; Careful - the pointer should not be deallocated by LISP later on!
  (ae-errorp-handler
    errorp
    (ae-error (#_AEPutParamPtr the-desc keyword #$typeChar
                 ptr length))))

(defun ae-put-parameter-boolean (the-desc keyword value &optional (errorp t))
  ;; perhaps Digitool should add this with the other ae-put functions?
  (rlet ((buffer :boolean))
    (%put-word buffer (if value -1 0))
    (ae-errorp-handler
      errorp
      (ae-error (#_AEPutParamPtr the-desc keyword #$typeBoolean buffer
                 (record-length :boolean))))))

(defun resume-free-event (event reply)
  (without-interrupts ;; to avoid that reply hangs on multiprocessing
    (ae-error (#_AESetTheCurrentEvent event))
    (ae-error (#_AEResumeTheCurrentEvent event reply (%int-to-ptr #$kAENoDispatch) 0))
    (%free-aedesc event NIL)
    (%free-aedesc reply NIL)))

(defun send-partial (target connection-id ptr length &optional finish-p)
  "Sends a partial response with the string referenced by the ptr"
  ;; target is available using sender-address on an incoming event.
  (with-aedescs (event reply)
    (make-partial event target
      :connection-id connection-id)
    (ae-put-parameter-boolean event :|Kmor| (not finish-p))
    (ae-put-parameter-string-ptr event #$keyDirectObject ptr length)
     (send-appleevent event reply :reply-mode
       ;; no need to wait if we are done anyway... 
       (if finish-p :no-reply :wait-reply))))













 

