;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: common-lisp-user -*-
;;;
;;; Copyright 1994-97, John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;;  LAUNCH MCL CL-HTTP WITHOUT DOMAIN NAME SERVICE DEMO CONFIGURATION
;;;
;;; Use this launcher to run the server in demo mode when no DNS is available, including
;;; a Hosts file in your system folder. This initialization will work for Internet or 
;;; AppleTalk networks or in standalone mode. 

;;;  After you load this file, you can access the root URL / on your host (specified as an IP number)
;;; and peruse the server documentation in your Web browser.
;;;
;;; You need to follow the instructions for configuring your Mac's AppleTalk and TCP/IP
;;; control panels, which you can find in http:www;cl-http;configure.html. You can read that
;;; file into a web browser directly from the filesystem.
;;;

(in-package :cl-user)

;; Set up MCL appropriately
#-MCL-CL-HTTP
(load (merge-pathnames "mcl-configuration" ccl:*loading-file-source-file*) :verbose t)

;; Load the server.
#-MCL-CL-HTTP
(load (merge-pathnames "envdcl" ccl:*loading-file-source-file*) :verbose t) 

;; Load the init file to start the CL-HTTP demo.
(load "http:mcl;examples;init-server-ip-number.lisp" :verbose t)

;; start http services
(http:enable-http-service)
