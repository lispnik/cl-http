;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: (w4 :use (future-common-lisp www-utils url http)); -*-

;;; Copyright John C. Mallery,  1995-96.
;;; All rights reserved.



;;;------------------------------------------------------------------- 
;;;
;;; VARIABLES
;;;
(in-package :w4)

(defvar *activity* nil
  "Bound to the ACTIVITY controlling the current walk.")

(define-parameter *debug-walker* nil
  "Controls debugging of the Web walker.")

(defparameter *cache-indicators* '(:headers :content :header-status-code :content-status-code
                                            :redirection :http-version :url-inferiors :local-context-string))

(define-parameter *cache-url-data* t
                  "Controls whether URL data is cached or not.")

(defvar *report-stream*)

(define-parameter *retries-on-network-error* 5)

(define-parameter *standard-get-robot-headers* '(:accept ((:* :*)) :accept ((:image :gif)) :accept ((:image :jpeg))))

(define-parameter *standard-head-robot-headers* nil)

(define-parameter *trace-walker* nil
  "Traces the activities of the walking printing results on the *report-stream*.")

(defvar *url-stack* nil
  "Bound to the list of urls currently being walked.")

(define-parameter *user-agent* "W4 Constraint-Guided Walker"
  "The name of this Web walker.")

(define-parameter *wait-interval-before-retry* 1)

(defparameter *constraint-class-alist*
              '((:context . context-constraint)
                (:header . header-constraint)
                (:html-body . html-body-constraint)
                (:html-head . html-head-constraint)
                (:url . url-constraint)
                (:dns-url . dns-url-constraint)
                (:circumstance . circumstance-constraint))
  "An alist of constraint classes accessible from define-constraint-type.")

(defparameter *action-class-alist*
              '((:standard . action)
                (:open-http-stream . open-http-action)
                (:encapsulating . encapsulating-action)
                (:generator . generator))
  "An alist of action classes accessible from define-action-type.")

(defparameter *queue-class-alist*
              '((:depth-first . depth-first-queue)
                (:breadth-first . breadth-first-queue)
                (:best-first . best-first-queue)))

(define-parameter *trace-constraints* nil
  "Constraints whose application to trace on the *report-stream*.
The value can be either a list of constraint names or T for all constraints.")
