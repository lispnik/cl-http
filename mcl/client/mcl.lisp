;;; -*- Syntax: ansi-common-lisp; Base: 10; Package: http; Mode: LISP -*-

;;; (C) Copyright 1995-99, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; MAC HTTP CLIENT NETWORK INTERFACE
;;;

(in-package :http)

(declaim (inline deallocate-client-http-stream))

(defun deallocate-client-http-stream (stream)
  (resources:deallocate-resource 'http-stream stream))

(defun open-http-stream-to-host (host port &optional process)
   (declare (values stream))
   (resources:allocate-resource 'http-stream host port (ceiling (the fixnum *client-timeout*) 60.) process))

;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defclass username+password-dialog (ccl:dialog) ())

(defclass exit-box 
   (ccl:button-dialog-item)
   ((selected-p :initform nil :accessor exit-box-selected-p)))

(defclass exit-box-cancel (exit-box) ())

(defclass exit-box-ok (exit-box) ())

(defclass text-input-item (ccl:editable-text-dialog-item) ())

(defmethod select-item ((exit-box exit-box))
  (setf (exit-box-selected-p exit-box) t))

(defmethod exit-p ((dialog username+password-dialog))
  (declare (values exit-p abort-p))
  (ccl:do-subviews (dialog-item dialog 'exit-box)
		   (when (exit-box-selected-p dialog-item)
		     (etypecase dialog-item
		       (exit-box-cancel (return :abort))
		       (exit-box-ok (return t))))))

(defmethod ccl:view-named (nickname (dialog username+password-dialog))
  (ccl:do-subviews (dialog-item dialog 'text-input-item)
                   (when (eq (ccl:view-nick-name dialog-item) nickname)
                     (return-from ccl:view-named dialog-item)))
  (error "No dialog item corresponds to ~S." nickname))

(defmethod user-name ((dialog username+password-dialog))
  (ccl:dialog-item-text (ccl:view-named 'user-name dialog)))

(defmethod password ((dialog username+password-dialog))
  (ccl:dialog-item-text (ccl:view-named 'password dialog)))

(defmethod get-username+password ((window username+password-dialog))
   (loop with  exit-p
	    initially (ccl:window-select window)
	    doing (multiple-value-setq (exit-p)
		         (exit-p window))
	    until exit-p
	    finally (case exit-p
		          (:abort (ccl:window-close window)
                                      (return  (values nil nil t)))
		          (t (let ((username (user-name window))
			              (password (password window)))
		                (ccl:window-close window)
		                (return (values username password)))))))

(defun allocate-username+pw-dialog (url-string default-user-name default-password)
  (make-instance 'username+password-dialog :window-type :double-edge-box
		 :window-title "User Name & Password"
		 :view-position '(:top 60)
		 :view-size #@(300 200)
		 :close-box-p nil
		 :view-font '("chicago" 12 :srcor :plain)
		 :view-subviews
		 (list  (ccl:make-dialog-item 'ccl:static-text-dialog-item
					      #@(4 3) #@(292 39)
					      "Please enter your username 
and password to access the url:"
					      'NIL
					      :view-font '("Chicago" 14 :SRCOR :PLAIN))
			(ccl:make-dialog-item 'ccl:static-text-dialog-item
					      #@(3 45) #@(294 22) url-string 'nil )
			(ccl:make-dialog-item 'ccl:static-text-dialog-item
					      #@(0 88) #@(80 16) "User Name: " 'nil)
			(ccl:make-dialog-item 'text-input-item
					      #@(86 88) #@(141 15)
					      default-user-name 'nil  :view-nick-name 'user-name :allow-returns nil)
			(ccl:make-dialog-item 'ccl:static-text-dialog-item
					      #@(0 118) #@(75 16) "Password: " 'nil)
			(ccl:make-dialog-item 'text-input-item
					      #@(87 116) #@(140 16)
					      default-password 'nil :view-nick-name 'password :allow-returns nil)
			(ccl:make-dialog-item 'exit-box-cancel
					      #@(33 156) #@(62 16) "Cancel" #'select-item :default-button nil)
			(ccl:make-dialog-item 'exit-box-ok
					      #@(145 157) #@(62 16) "OK" #'select-item :default-button t))))

(defparameter *default-client-user-id* nil)

(define default-client-user-id ()
  (or *default-client-user-id* ""))

(defun set-default-client-user-id (user-id)
   (when user-id
       (setq *default-client-user-id* user-id)))

(defparameter *default-client-pw* nil)

(define default-client-pw ()
  (or *default-client-pw* ""))

(defun set-default-client-pw (pw)
  (when pw
    (setq *default-client-pw* pw)))

(define-macro with-client-user-id+pw ((user-name password) &body body)
  "Binds the default the default user name and password within BODY."
  `(let ((*default-client-user-id* ,user-name)
	 (*default-client-pw* ,password))
     ,@body))

(export '(with-client-user-id+pw) :http)

;; this is the function used by the portable client code.
(defun %get-user-name+pw (url-string  realm method &optional (stream *standard-output*))
   (declare (ignore stream realm method)
                 (values user-id pw abort-p))
   (let* ((user-name (default-client-user-id))
	     (password (default-client-pw))
	     (window (allocate-username+pw-dialog url-string user-name password))) 
      (multiple-value-bind (uid pw abort-p)
                                      (get-username+password window)
          (unless abort-p
	     (cond-every
               (uid (set-default-client-user-id uid))
	       (pw (set-default-client-pw pw))))
          (values uid pw abort-p))))

#|(defun %get-user-name+pw (url-string &optional stream)
   (declare (values user-id pw))
   (let (user-id pw)
      (dw:accepting-values 
        (stream :own-window t
                     :label (format nil "~'bPassword Information~"))
        (dw:redisplayable-format stream "URL: ~v~A~" '(nil nil :small) string)
        (terpri stream)
        (setq user-id (scl:accept 'scl:string :prompt "User" :default user-id :stream stream))
        (setq pw (scl:accept 'scl:string :prompt "Password" :default pw :stream stream)))
      (values user-id pw)))|#

;;;------------------------------------------------------------------- 
;;;
;;;  FINGER FACILITY BUILT ON HTTP STREAMS
;;;

(defun open-tcp-stream (host port &optional process)
  (declare (values stream))
  (resources:allocate-resource 'http-stream host port (ceiling (the fixnum *client-timeout*) 60.) process))

(defmacro with-open-tcp-stream ((stream host port &optional process) &body body)
  `(unwind-protect
       (let ((,stream (open-tcp-stream,host ,port ,process)))
	 (multiple-value-prog1
	   (progn . ,body)
	   (when ,stream
	     (resources:deallocate-resource 'http-stream ,stream)))))) 

(defun finger (host &optional user whois-p (stream *standard-output*) (no-error-p t) &aux (success-p t))
  "Uses the finger protocol to find out information about USER at HOST  and reports on STREAM.
HOST is either a fully qualified domain-name or an IP address. USER is either a string or NIL.
When WHOIS-P is non-null and USER is supplied, a whois request is made to HOST."
  (handler-case-if no-error-p
     (with-open-tcp-stream (tcp-stream host #.(tcp-service-port-number "Finger" t))
         (when user
	   (write-string user tcp-stream)
	   (when whois-p
	     (write-string " /W" tcp-stream)))	; send whois flag
	 (terpri tcp-stream) 
	 (force-output tcp-stream)
	 (stream-decode-crlf-until-eof tcp-stream stream))
    (network-error (err) (write-string (report-string err) stream)
		   (setq success-p nil)))
  success-p)

(export 'finger :http)
