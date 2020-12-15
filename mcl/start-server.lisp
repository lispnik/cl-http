;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: common-lisp-user -*-
;;;
;;; Copyright 1994-97, John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;; LAUNCH MAC CL-HTTP WITH INTERNET DEMO CONFIGURATION
;;;

;; If you load this file, you can access the URL
;; http://your.host.domain.name/ and peruse the server documentation,
;; assuming a network connection.

(in-package :cl-user) 

;; Set up MCL appropriately
#-MCL-CL-HTTP
(load (merge-pathnames "mcl-configuration" ccl:*loading-file-source-file*) :verbose t)

;; Load the server.
#-MCL-CL-HTTP
(load (merge-pathnames "envdcl" ccl:*loading-file-source-file*) :verbose t) 

;; Load the init file to start the CL-HTTP demo.
(load "http:mcl;examples;init-server-internet.lisp" :verbose t) 

;; start http services
(http:enable-http-service)
