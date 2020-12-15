;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: common-lisp-user -*-
;;;
;;; Copyright 1994-97, John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;; LAUNCH CL-HTTP FROM LISP IMAGE WITH INTERNET DEMO CONFIGURATION
;;;


;; This file initializes CL-HTTP with the default demo.
;; It assumes that you have saved an MCL image of CL-HTTP
;; and that you have made this file be the init file for that image.
;; You can then place an alias to the image file in the system folder's
;; start up items and have the server automatically launch at boot time.

(in-package :cl-user) 

;; Load the init file to start the CL-HTTP demo.
(load "http:mcl;examples;init-server-internet.lisp" :verbose t) 

;; start http services
(http:enable-http-service)
