;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: common-lisp-user -*-
;;;
;;; Copyright 1994-97, Christopher R Vincent & John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;;  SYSTEM DEFINITION CL-HTTP PROXY SERVICE
;;;

(in-package :cl-user)

;; This requires that the server be loaded first as there is considerable shared code.
;; options are :load :compile :eval-load :compile-load-always, :compile-load

(define-system (cl-http-proxy)
                        ()
   "http:mcl;proxy;patches"                      ; Patches to MCL 4.1
   "http:mcl;proxy;mcl"                          ; MCL specific code
   ;; client layer
   "http:mcl;client;sysdcl-substrate"            ; Client substrate
   ;; Proxy layer
   "http:proxy;utils"
   "http:proxy;database"                         ;Database
   "http:proxy;cache"
   "http:proxy;proxy-cache"
   "http:proxy;proxy"                            ;Proxy Server
   "http:proxy;documentation")
