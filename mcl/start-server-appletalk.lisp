;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: common-lisp-user -*-
;;;
;;; Copyright 1994-97, John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;;  LAUNCH MCL CL-HTTP WITH APPLETALK DEMO CONFIGURATION
;;;
;;; Use this launcher to run the server in demo mode on an AppleTalk network or
;;; in standalone mode. If you load this file, you can access the URL /
;;; on your host and peruse the server documentation running HTTP over AppleTalk
;;; connected to other hosts or standalone. 
;;;
;;; You need to follow the instructions for configuring your Mac's AppleTalk and TCP/IP
;;; control panels, which you can find in http:www;cl-http;configure.html. You can read that
;;; file into a web browser directly from the filesystem.
;;;
;;; After you have configured the mac side, be sure to add the appropriate entries in the 
;;; Hosts file in you system folder.  Otherwise, you need to access via the IP number of the
;;; serving host.

(in-package :cl-user)

;; Set up MCL appropriately
#-MCL-CL-HTTP
(load (merge-pathnames "mcl-configuration" ccl:*loading-file-source-file*) :verbose t)

;; Load the server.
#-MCL-CL-HTTP
(load (merge-pathnames "envdcl" ccl:*loading-file-source-file*) :verbose t) 

;; Load the init file to start the CL-HTTP demo.
(load "http:mcl;examples;init-server-appletalk.lisp" :verbose t)

;; start http services
(http:enable-http-service)
