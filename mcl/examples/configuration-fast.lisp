;;;-*- Mode: Lisp; Package: HTTP -*- 
;;; Copyright John C. Mallery,  1999.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;;  CONFIGURATION FOR HIGH-VOLUME PRODUCTION SERVICE
;;;

(in-package :http) 

;;;------------------------------------------------------------------- 
;;;
;;;  MCL Parameters
;;;

;; How long to wait for wait-next-event when no event
(setq ccl::*idle-sleep-ticks* 1)

;; Same when lisp is a background MAC application.
(setq ccl::*background-sleep-ticks* 1) 

;; Don't call ccl::idle-update to avoid crashing.  This provides a workaround for
;; an MCL bug that can crash powerbooks.  If you find your powerbook falling asleep,
;; you will need to adjust the powerbook control panels.  -- JCMa 6/7/1995.
(setq ccl::*cpu-idle-p* nil) 

;;;------------------------------------------------------------------- 
;;;
;;; OPENTRANSPORT PARAMETERS 
;;;

;; Causes OpenTransport to use our buffers if T (notifying when done), or copy when NIL
(setq ccl::*ack-sends* nil)

;; Controls how many seconds TCP waits for replys from the other end of a connection.
;;  in seconds, the time to wait for replys from the other end 
(setq ccl::*tcp-read-timeout* 5)

;; The seconds to wait for a connection to close.
;; in seconds, the time to wait before aborting a slow closing connection
(setq ccl::*tcp-close-timeout* 5)

;;;------------------------------------------------------------------- 
;;;
;;;  MCL HTTP Parameters
;;; 

;; The number of threads simultaneously listening for HTTP connections.
(setq *number-of-listening-processes* 20) 

;; Number of seconds before a command times out on a TCP stream listening for connections.
(setq *command-timeout-for-listeners* 5) 

;; The process priority of HTTP listener processes.
(setq *listener-process-priority* 1)

;; The interval of execution quanta allocated for each scheduling of an HTTP thread.
(setq *server-run-interval* 12.) 

;  8192 , default is 1024, 4096 improves standalone performance.
(set-http-stream-buffer-size 4096)

;; Sets the maximum number of simultaneous connections that the server will accept.
(set-maximum-number-of-connections 150) 

;;;------------------------------------------------------------------- 
;;;
;;; STANDARD CL-HTTP PARAMETERS 
;;;

;; Controls whether the file log stream remains open all the time, or it is reopenned 
;; for each log transaction. Production servers should keep the log file stream open.
(log-file-stream-stays-open t)

;; Show transactions in a log window
;; Turn this off in production servers to save time writing log entries to the notification window.
(log-notifications-on (current-access-logs) nil)

;; The maximum number of requests served for each persistent connection.
(setq *persistent-connection-maximum-requests* 100.) 

;; The seconds before an inactive persistent connection times out.
(setq *persistent-connection-timeout* 3.)

;; Controls whether the times in log entries are in written in Greenwich Mean Time or not.
(setq *log-times-in-gmt* t) 

;; Controls whether stack backtraces are included in automatic bug reporting.
(setq *stack-backtraces-in-bug-reports* t) 

;; Controls the amount of idle time before the HTTP server drops the connection.
;; Time is in 60ths of a second.
;; Setf SERVER-REQUEST within response functions to change this value for specific URLs
(setq *server-timeout* (the fixnum (* 60. 30.)))         ; disconnect lusers in 30 seconds

;; Controls the maximum time an HTTP server can live.
;; Time is in milliseconds.
;; Setf SERVER-LIFE-TIME within response functions to change this value for specific URLs
(setq *server-life-time* (* 1000. 60. 2.))    ;default is two minutes 

;; rerun initializations for good measure
(run-server-initializations t)
