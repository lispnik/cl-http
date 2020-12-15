;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: common-lisp-user -*-
;;;
;;; Copyright 1995-97, John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;;  CONFIGURES MCL CL-HTTP FOR OPERATION OVER APPLETALK NETWORKS
;;;
;;; Use this for standalone operation in a loopback mode via AppleTalk or for
;;; a network of hosts on an AppleTalk network. You must specify domain name
;;; mappings from domain names to IP addresses in the HOsts file of your system folder
;;; in order for clients to resolve the names. Otherwise, you must use only the IP addresses.

(in-package :cl-user)

;; options are :load :compile :eval-load :compile-load-always, :compile-load
(define-system 
   (cl-http-demo)
   (:compile-load)
   "http:mcl;examples;configuration-appletalk"         ; server configuration file
   "http:examples;exports")              ; server example exports
