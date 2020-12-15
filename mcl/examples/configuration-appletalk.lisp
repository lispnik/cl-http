;;; -*- Mode: LISP; Package: HTTP; Syntax: ANSI-Common-Lisp -*-

;;; Copyright John C. Mallery,  1994-1997.
;;; All rights reserved.
;;;-------------------------------------------------------------------
;;;
;;;  CONFIGURATION FILE FOR CL-HTTP RUNNING OVER APPLETALK
;;;
;;; This file configures CL-HTTP for AppleTalk HTTP service when DNS is available only
;;; via the hosts file in the system directory.  When DNS is available, use the standard configuration.
;;;

(in-package :http) 

;; Tell the MAC its domain name for AppleTalk operation.
(initialize-apple-talk-configuration "Local-Host.Apple-Talk.Net" t)

;; Do the standard configuration
(load "http:examples;configuration.lisp")

;; Repeat because some of the standard configurations step on these
(initialize-apple-talk-configuration "Local-Host.Apple-Talk.Net" t)
