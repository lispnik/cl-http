;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: common-lisp-user -*-
;;;
;;; Copyright 1995-97, John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;;  MCL SYSTEM DEFINITION FOR W4 CONSTRAINT-GUIDED WEB WALKER EXAMPLES
;;; 

(in-package :cl-user)

;; This requires that the server and web walker be loaded first as there is
;; considerable shared code and the demos are accessed via the web,
;; served by the server.
;;
;; Options are :load :compile :eval-load :compile-load-always, :compile-load 

(define-system (w4-web-walker-demo)
	                ()
   "http:w4;examples;trace"			; Standard examples
   "http:w4;examples;search"			; Salton style search example.
   "http:w4;examples;web-archive"		; Web Whacker
   "http:w4;examples;exports")			; Basic examples
