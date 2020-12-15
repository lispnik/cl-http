;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: common-lisp-user -*-
;;;
;;; Copyright 1994-99, John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;;  SYSTEM DEFINITION FOR MAC CL-HTTP
;;;

;; If you want to just start up the server using the standard demo configuration,
;; you should load http:mcl;start-server.lisp instead.

;; If you load this file and evaluate the form
;; (http-user:enable-http-service), you can access the URL
;; http://your.host.domain.name/ and peruse the server documentation.

(in-package :cl-user) 

;;;------------------------------------------------------------------- 
;;;
;;; DEFINE THE COMMON LISP HTTP SYSTEM 
;;;

;; Add feature for process polling
#+ccl-3
(when (fboundp 'ccl::process-poll) (pushnew :process-poll *features*))

;; Check whether to run the native Open Transport interface
#+ppc-target
(when (probe-file "ccl:library;OpenTransport.lisp")
  (pushnew :open-transport *features*))

;; Establish whether we have multiple reader locks.
#+(and ppc-target CCL-4.3)
(pushnew :multiple-reader-locks *features*)


;; options are :load :compile :eval-load :compile-load-always, :compile-load
(define-system 
  (cl-http)
  ()
  ;; Run with the new scheduler which is putatively more efficient -- JCMa 5/15/1999.
  (:in-order-to-compile :compile-load "ccl:library;new-scheduler")
  ;; load MCL libraries
  (:in-order-to-compile :compile-load "ccl:library;loop")       ; loop macro, required for compilation only?
  #+(and ccl-3 (not (or ppc-target ccl-3.1)))   ; needed for process interface code in MCL 3.0
  (:in-order-to-compile :compile-load "ccl:library;lispequ")
  #+(and ccl-3 (not (or ppc-target ccl-3.1)))   ; locking primitive used by xmactcp
  (:in-order-to-compile :compile-load "http:mcl;server;store-conditional")
  #+(and ccl-3 (not process-poll))              ; polling process wait facility
  (:in-order-to-compile :compile-load "http:mcl;server;process-poll")
  #+(and ccl-3 (not :multiple-reader-locks))    ; multiple simultaneous reader locks
  (:in-order-to-compile :compile-load "http:mcl;server;lock-multiple-readers")
  #+ccl-3 ;; processes that stick around and can be assigned new tasks
  (:in-order-to-compile :compile-load "http:mcl;server;task-process")
  #-ccl-3                                       ; TCP interface MCL 2.0.1
  (:in-order-to-load :compile-load "ccl:library;mactcp")
  #+(and ccl-3 (not Open-Transport))            ; TCP interface MCL 3.0
  (:in-order-to-load :compile-load "http:mcl;server;xmactcp")
  #+Open-Transport                              ; Stream buffer package used by OT native
  (:in-order-to-load :compile-load 
   "http:mcl;server;xio-buffer")		; Released version "ccl:library;io-buffer"
  #+Open-Transport                              ; TCP - OpenTransport Native Interface PPC MCL 3.9, 4.0, 4.1, 4.2
  (:in-order-to-load :compile-load
   "http:mcl;server;xopentransport")		; Released version "ccl:library;opentransport"
  #-ccl-3                                       ; fast file access loaded for 2.0.1 but built-in for 3.0
  (:in-order-to-load :compile-load "ccl:examples;mac-file-io" )
  #+(and ppc-target (not ccl-4))                ; CLOS patch fixes dispatch on strings (essential for Web Walker)
  (:in-order-to-load :compile-load "http:mcl;server;mcl-clos-patch")
  #+(and (or ccl-3 ccl-3.1 ccl-4) (not CCL-4.2))
  "http:mcl;server;timers"                      ; Timer facility for MCL 3.0, 3.9, 3.1, 4.0 (3.1/4.0 CD version is buggy)
  #+ccl-4.2"ccl:examples;timers"                ; Released Timer Facility
  "http:mcl;server;resources"                   ; portable resource package 
  "http:mcl;server;with-timeout.lisp"		; timeouts based on timer facility    
  ;; server code.
  "http:server;package"                         ; all package definitions
  #-Open-Transport
  "http:mcl;server;tcp-stream"                  ; TCP stream building on mactcp or xmactcp
  #+Open-Transport
  "http:mcl;server;tcp-ot-stream"               ; TCP stream building on OpenTransport
  #-ccl-3"http:mcl;server;tcp-conditions2"      ; TCP conditions for MCL 2.0.1 but built into MCL 3.0 mactcp
  "http:server;preliminary"                     ; documentation macros
  "http:server;variables"                       ; variable definitions
  "http:mcl;server;www-utils"                   ; Portable non-Genera utilities
  "http:mcl;server;mcl"                         ; Platform dependent code
  "http:mcl;server;log-window"                  ; Log window and logging
  "http:server;base64-encoding"                 ; Base 64 utility RFC-1113 and RFC-1341.
  "http:server;md5"                             ; MD5 Digests based on RFC 1321
  "http:mcl;server;md5"                         ; Fast, cons-free MD5 specialization code
  "http:server;sha"                             ; SHA Digests based on Internet FIPS 180
  #+ccl-3 "http:smtp;package"			; SMTP package
  #+ccl-3 "http:smtp;smtp"                      ; Simple SMTP mailer
  #+ccl-3 "http:smtp;mail"                      ; Interfaces for sending email
  "http:server;task-queue"                      ; Thread-Safe queuing facility
  "http:server;class"                           ; Server class definitions
  "http:server;url-class"                       ; URL class definitions 
  "http:server;http-conditions"                 ; HTTP conditions
  "http:server;plist"                           ; property list mixin for CLOS
  "http:server;utils"                           ; utility functions
  "http:server;tokenizer"                       ; Simple tokenizer
  "http:server;headers"                         ; HTTP headers
  "http:server;host"                            ; Host objects and operations
  "http:server;url"                             ; Object-Oriented URLs
  "http:server;html2"                           ; HTML generation
  "http:server;netscape-1-1"                    ; Netscape 1.1 HTML generation
  "http:server;vrml-1-0"                        ; VRML 1.0 Generation
  "http:server;image-maps"                      ; Image maps
  "http:server;netscape-2-0"                    ; Netscape 2.0 HTML generation
  "http:server;netscape-3-0"                    ; Netscape 3.0 HTML generation
  "http:server;html-3-2"                        ; HTML 3.2 Generation   
  "http:server;scripts"                         ; Client-Side Scripts
  "http:server;netscape-4-0"                    ; Netscape 4.0 HTML generation
  "http:server;shtml"                           ; Server-Parsed HTML
  "http:mcl;w3p;sysdcl"                         ; W3P Presentation System
  "http:server;report"				; Reporting for HTTP Conditions
  "http:server;log"                             ; Logging HTTP transactions
  "http:server;authentication"                  ; User authentication
  "http:server;data-cache"			; Data caching
  "http:server;server"                          ; Main server code
  "http:server;cgi"                             ; Common Gateway Interface and Server Interface
  "http:server;preferences"                     ; Server preference system
  "http:server;web-configuration"               ; Server Configuration via the Web
  #-ccl-3"http:mcl;server;tcp-interface2"       ; Server TCP Interface for MCL 2.0.1
  #+ccl-3"http:mcl;server;tcp-interface"        ; Server TCP Interface for MCL 3.0
  "http:mcl;server;control"                     ; Control Browser via AppleEvents
  "http:mcl;server;cl-http-menu.lisp"
  #+CLIM-2"http:clim;package"                   ; CLIM Interface Package
  #+CLIM-2"http:clim;interface")                ; CLIM Server Interface
