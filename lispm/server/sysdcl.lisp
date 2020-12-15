;;;   -*- Mode: LISP; Package: cl-user; BASE: 10; SYNTAX: ansi-common-lisp; Default-character-style: (:FIX :ROMAN :NORMAL);-*-
;;;
;;; (C) Copyright 1994-1997, John C. Mallery.
;;;     All Rights Reserved.
;;;


;;;------------------------------------------------------------------- 
;;;
;;; SYSTEM DEFINITION
;;;

#+Genera
(unless (fs:get-logical-pathname-host "HTTP" t)
  (fs:make-logical-pathname-host "HTTP"))

(sct:defsystem cl-http                          
    (:pretty-name "HTTP Server"
     :default-pathname "HTTP:SERVER;"
     :journal-directory "HTTP:LISPM;SERVER;PATCH;"
     :initial-status :experimental
     :patchable t
     :source-category :basic)
  (:module pointers
   ("SYS:SITE;HTTP.TRANSLATIONS"
    "SYS:SITE;CL-HTTP.SYSTEM"
    "SYS:SITE;W3P.SYSTEM"
    "HTTP:LISPM;HTTP.TRANSLATIONS")
   (:type :lisp-example))
  (:module showable-procedures
   ("SHOWABLE-PROCEDURES")                      ; Utility for finding procedures
   (:type :system))
  #+Genera
  (:module lispm
   ("HTTP:LISPM;SERVER;TCP-LISPM-STREAM"        ; Modal ASCII or binary TCP streams
    "HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE" ; Timeout TCP close in 5 seconds (Genera 8.3)
    "HTTP:LISPM;SERVER;LISPM"))                 ; LispM Interface functions
  (:module w3p
   ("W3P")                                      ; W3P Presentation System
   (:type :system))
  (:serial
    showable-procedures
    "PACKAGE"                                   ; HTTP Packages
    "PRELIMINARY"                               ; Showable procedures
    "VARIABLES"                                 ; Variables and constants
    #+Genera lispm                              ; Load Lisp Machine specific code
    "BASE64-ENCODING"                           ; Base 64 utility RFC-1113 and RFC-1341.
    "MD5"                                       ; MD5 Digests based on RFC 1321
    "SHA"                                       ; SHA Digests based on Internet FIPS 180
    "TASK-QUEUE"                                ; Thread-Safe Queuing Facility
    "CLASS"                                     ; Server Class Definitions
    "URL-CLASS"                                 ; URL Class Definitions
    "HTTP-CONDITIONS"                           ; HTTP conditions
    "PLIST"                                     ; Property list mixin for CLOS
    "UTILS"                                     ; Server utility functions
    "TOKENIZER"                                 ; Simple tokenizer
    "HEADERS"                                   ; Header hacking, including MIME
    "HOST"                                      ; Host objects and operations
    "URL"                                       ; URL implementation
    "HTML2"                                     ; HTML 2.0 Generation
    "NETSCAPE-1-1"                              ; Netscape 1.1 HTML Generation
    "VRML-1-0"                                  ; VRML 1.0 Generation
    "IMAGE-MAPS"                                ; Client-Side Image Maps
    "NETSCAPE-2-0"                              ; Netscape 2.0 HTML Generation
    "NETSCAPE-3-0"                              ; Netscape 3.0 HTML Generation
    "HTML-3-2"                                  ; HTML 3.2 Generation
    "SCRIPTS"                                   ; Client-Side Scripts
    "NETSCAPE-4-0"                              ; Netscape 4.0 HTML Generation
    "SHTML"                                     ; Server-Parsed HTML
    w3p                                         ; W3P Presentation System
    "REPORT"					; Reporting for HTTP Conditions
    "LOG"                                       ; Logging
    "AUTHENTICATION"                            ; User authentication
    "DATA-CACHE"                                ; Data caching  
    "SERVER"                                    ; HTTP Server
    "CGI"                                       ; Common Gateway Interface
    "PREFERENCES"                               ; Configuration Preference Facility
    "WEB-CONFIGURATION"))                       ; Server Configuration via the Web
