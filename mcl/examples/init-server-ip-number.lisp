;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: common-lisp-user -*-
;;;
;;; Copyright 1995-97, John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;;  INITIALIZES CL-HTTP FOR OPERATION WITHOUT DOMAIN NAME SERVICE
;;;
;;; Use this for operation when no DNS service is available. It will work
;;; for both AppleTalk and Internet Networks, in standalone mode or not.
;;; You probably want to step up to the appletalk configuration after
;;; you create a hosts file in your system directory. It is more rewarding
;;; to have real names on your URLs rather than just IP numbers.

(in-package :cl-user)

;; options are :load :compile :eval-load :compile-load-always, :compile-load
(define-system 
   (cl-http-demo)
   (:compile-load)
   "http:mcl;examples;configuration-ip-number"         ; server configuration file
   "http:examples;exports")              ; server example exports
