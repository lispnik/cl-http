;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: common-lisp-user -*-
;;;
;;; Copyright 1995-97, John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;;  MCL SYSTEM DEFINITION FOR W4 CONSTRAINT-GUIDED WEB WALKER
;;; 

(in-package :cl-user)

;; This requires that the server be loaded first as there is
;; considerable shared code and the demos are accessed via the web,
;; served by the server.
;;
;; Options are :load :compile :eval-load :compile-load-always, :compile-load

(define-system (w4-web-walker)
	       ()
  ;; client code.
  "http:mcl;client;sysdcl-substrate"		; Load client substrate
  "http:client;w4-client"			; W4 client support methods
  "http:w4;package"				; Package Definition
  "http:w4;variables"				; Variables
  "http:w4;utils"				; Utility functions and macros
  "http:w4;class"				; Class definitions and Print methods
  "http:w4;walker"				; Main Walker code
  "http:w4;constraints"				; Constraint Definitions
  "http:w4;actions"				; Action definitions
  "http:w4;activity")				; Activity definitions
