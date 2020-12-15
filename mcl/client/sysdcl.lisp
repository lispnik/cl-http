;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: common-lisp-user -*-
;;;
;;; Copyright 1994-97, John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;;  SYSTEM DEFINITION FOR BASIC CL-HTTP CLIENT
;;;

(in-package :cl-user)

;; This requires that the server be loaded first as there is considerable shared code.
;; options are :load :compile :eval-load :compile-load-always, :compile-load

(define-system (cl-http-client)
                        ()
   ;; client code.
   "http:mcl;client;sysdcl-substrate"            ; Client substrate
   "http:client;sexp-browser"                   ; S-Expression browser application
   "http:client;flogger")           ; CL-HTTP flogger
