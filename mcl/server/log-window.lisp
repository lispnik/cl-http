;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: www-utils -*- 

;;; Copyright 1995, 1999, John C. Mallery.
;;; All rights reserved. 
;;;------------------------------------------------------------------- 
;;;
;;; LOGGING EVENTS
;;; 
;;; This should use the task queue facility to avoid impairing HTTP performance on loaded servers. 
;;;  Since an HTTP connection must wait until it can write the MCL FRED log entry in a process queue,
;;; all HTTP connections bottleneck on this single resource, slowing HTTP performance to the rate
;;; at which entries can be written to the log. This facility should be turned off on production servers
;;; using (http:log-notifications-on (current-access-logs) nil) in your server configuration file.
;;; A better tool for monitoring production servers is the http log-window in http:server;examples.lisp
;;; -- JCMa 4/14/1999.
;;;
(in-package :www-utils)

#+CCL-3
(defparameter *log-text-color* ccl:*purple-color*
   "Controls the color of log entrires.") 

(defparameter *log-window-title* "CL-HTTP Log")

;;Bound to the HTTP server log window when one exists.
(defvar *log-window* nil)

(defvar *log-window-entries* 0)

;;Controls how big a log window can get before being reset.
(defparameter *log-window-maximum-entries*  1000
   "Maximum number of entries before resetting the log window.")

(defun initialize-log-window-position (window &key (width 750) )
   (let* ((dims (ccl:view-size window))
             (h (min width (- ccl:*screen-width* 3)))
             (v (truncate (ccl:point-v dims) 2))
             (x 3)
             (y (- ccl:*screen-height* v 3)))
      (ccl:set-view-size window h v)
      (ccl:set-view-position window (ccl:make-point x y))))

(define expose-log-window ()
   "Exposes the window."
   (multiple-value-bind (log new-p)
                                   (log-window)
       (let ((selected-window ccl::*selected-window*))
          (when new-p
              (initialize-log-window-position log))
          (ccl:window-select log)
          ;; but don't lose selection of the current window
          (ccl:window-select selected-window))))

;; Returns the active log window.
;; Must be called within a serializing scheme because there are no locks.
(defun log-window ()
   (declare (values log-window newly-created-p))
   (labels  ((make-log-window (host-name)
                     (make-instance 'ccl::fred-window
                         :scratch-p t
                         :color-p t
                         #+ccl-3 :view-font
                         #+ccl-3 `((:color-index ,(ccl:fred-palette-closest-entry *log-text-color*)))
                         :window-show t
                         :window-title (format nil "~A [~:(~A~)]" *log-window-title* host-name)
                         :window-layer 0))
                 (setup-fresh-log-window ()
                    (let ((window (make-log-window (local-host-domain-name))))
                     
                       (setf *log-window* window
                                *log-window-entries* 0)
                       (values window t))))
      (let ((window *log-window*))
         (cond ((not (and window (ccl::wptr window)))
                    (setup-fresh-log-window))
                  ((< *log-window-entries* *log-window-maximum-entries*)
                    window)
                  (t (let ((entries `(,*log-window-entries*)))
                        (declare (dynamic-extent entries)) 
                        (ccl:window-close window)
                        (expose-log-window)
                        (%notify-log-window *log-window* "Cleared ~D log entries." entries)
                        *log-window*))))))

;; big hammer clears the log window.
(defun reset-log-window (&optional (window *log-window*))
   (when window
       (let ((entries *log-window-entries*)) 
          (ccl:without-interrupts 
            (ccl:window-close window)
            (expose-log-window))
          (notify-log-window "Cleared ~D log entries." entries)
          *log-window*))) 

#-CCL-3
(defmacro with-log-window-stream ((stream) &body body)
   `(let* ((window (log-window))
              ;;;Karsten 5/17/95 A Fred-Window is already the right stream in 2.01, 
              ;; window-key-handler is undefined
              (,stream window))
       (ccl::set-mark (ccl::fred-buffer window) t)
       (fresh-line ,stream)
       (ccl::window-show-cursor window)
       (prog1 (progn . ,body)
          (ccl::fred-update window))))

;; Bound to the process queue for the HTTP server log window when one exists.
#+CCL-3
(defvar *log-window-process-queue* nil)

#+CCL-3
(define log-window-process-queue ()
   "Returns the process queue for the CL-HTTP log window."
   (ccl:without-interrupts
     (cond (*log-window-process-queue*)
              (t (setq *log-window-process-queue* (ccl::make-process-queue "HTTP Log Window Process Queue"))))))

#+CCL-3
(define-macro with-log-window-stream ((stream) &body body)
   "Use this macro to write to the log window, which is bound to STREAM.
A process queue prevents forms executed within BODY from colliding with
other processes trying to write the log simultaneously."
   `(ccl::with-process-enqueued 
       ((log-window-process-queue) ccl::*current-process* "HTTP Log Window Wait")
       (let* ((window (log-window))
                 (,stream (ccl::window-key-handler window))) 
          ;how to make sure we're inserting at the end of the buffer????
          (ccl::set-mark (ccl::fred-buffer window) t)
          (fresh-line ,stream)
          (ccl::window-show-cursor window)
          (prog1 (progn . ,body)
             (incf *log-window-entries*)        ; keep track of number of entries
             (ccl::fred-update window))))) 

(declaim (type function http::write-standard-time))

(defun %notify-log-window (stream format-string format-args)
   (fresh-line stream)
   (write-char #\[ stream)
   (http::write-standard-time (get-universal-time) stream)
   (write-string "]  " stream)
   (apply #'format stream format-string format-args))

(define notify-log-window (format-string &rest format-args)
   "Top-level method for writing to the HTTP log window."
   (declare (dynamic-extent format-args))
   (with-log-window-stream (stream)
       (%notify-log-window stream format-string format-args))) 

(define common-logfile-notify (server)
   "Issues a notification of server activity on a window."
   (with-log-window-stream (stream)
       (http::write-common-logfile-entry server stream)))

(define log-http-server-error (format-string &rest format-args)
   (declare (dynamic-extent format-args))
   (apply #'notify-log-window  format-string format-args))

(define http::log-http-request (client-host method url-string case)
   #|(log-http-access client-host
                   (if accepted-p "Serving ~A ~S" "Rejected ~A ~S") method url-string)|#
   (let ((host-name (http::host-domain-name client-host)))
      (ecase case
         (:accepted
           (notify-log-window "HTTP Serving ~A: Method ~S: ~S" host-name method url-string))
         (:rejected
           (notify-log-window "HTTP Rejected ~A: Method ~S: ~S" host-name method url-string))
         (:timeout
           (notify-log-window "HTTP Timed Out Serving ~A: Method ~S: ~S" host-name method url-string)))))

;; export the logging symbols
(export '(log-window notify-log-window expose-log-window common-logfile-notify *log-window-maximum-entries*) :www-utils)
