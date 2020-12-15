;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: common-lisp-user -*-
;;;
;;; Copyright 1994-97, John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;;  SYSTEM DEFINITION FOR CL-HTTP CLIENT SUBSTRATE
;;;

(in-package :cl-user)

;; This requires that the server be loaded first as there is considerable shared code.
;; options are :load :compile :eval-load :compile-load-always, :compile-load

(define-system (cl-http-client-substrate)
               (:compile-load)          ; required because define-system does not implement embedded systems
  ;; client code.
  "http:client;variables"                       ; Client variables
  "http:mcl;client;mcl"                         ; Platform support for client
  "http:client;connection"                      ; Persistent connections
  "http:client;client")                         ; Portable client code
