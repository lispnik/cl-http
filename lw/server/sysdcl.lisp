;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: cl-user; Base: 10 -*-

(in-package "CL-USER")

;; Based on lispm/server/sysdcl.lisp
(sct-defsystem cl-http                          
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
  #+Genera
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
    #+Genera showable-procedures
    #-LispWorks CLIM-SYS                        ; Port your own CLIM-SYS
    #-Genera "HTTP:mcl;server;resources"        ; portable resource package     
    "PACKAGE"                                   ; HTTP Packages
    #+LispWorks
    "HTTP:lw;server;package"                    ; LispWorks specific package changes
    #+LispWorks4
    "http:smtp;package"
    "PRELIMINARY"                               ; Showable procedures
    "VARIABLES"                                 ; Variables and constants
    #+Genera lispm                              ; Load Lisp Machine specific code
    #+LispWorks3.2
    "HTTP:lw;server;tcp-stream"                 ; LispWorks 3.2.2 specific
    #+LispWorks4
    "HTTP:lw;server;tcp-stream-4"               ; LispWorks 4 specific
    #-Genera "HTTP:mcl;server;www-utils"        ; Some portable utils are there
    #+(or UNIX Harlequin-PC-Lisp)
    "HTTP:lw;server;unix"                       ; add-ons for UNIX-like ports
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
    "SHTML"					; Server-Parsed HTML
    w3p                                         ; W3P Presentation System
    "REPORT"					; Reporting for HTTP Conditions
    "LOG"                                       ; Logging
    "AUTHENTICATION"                            ; User authentication
    "DATA-CACHE"				; Data caching
    "SERVER"                                    ; HTTP Server
    "CGI"                                       ; Common Gateway Interface
    "PREFERENCES"                               ; Configuration Preference Facility
    "WEB-CONFIGURATION"                         ; Server Configuration via the Web
    #+LispWorks "HTTP:lw;server;tcp-interface"
    #+LispWorks4 "http:smtp;smtp"               ; Simple SMTP mailer
    #+LispWorks4 "http:smtp;mail"               ; Interfaces for sending email
    ))

#-Genera
(sct-defsystem cl-http-platform-specific-examples
    (:pretty-name "HTTP Platform specific examples"
     :default-pathname "HTTP:examples;")
  (:serial
   #+LispWorks "HTTP:lw;examples;configuration"  ; standard-http-port value other than 80
   #+LispWorks "HTTP:lw;examples;exports"
   ))

(sct-defsystem cl-http-base-examples
    (:pretty-name "HTTP examples"
     :default-pathname "HTTP:examples;"
     :journal-directory "HTTP:LISPM;EXAMPLES;PATCH;"
     :initial-status :experimental
     :patchable t
     :source-category :basic)
  (:module CL-HTTP-PLATFORM-SPECIFIC-EXAMPLES
    ("CL-HTTP-PLATFORM-SPECIFIC-EXAMPLES")
    (:type :system))
  (:serial
   "configuration"               ; server configuration file
   #+LispWorks CL-HTTP-PLATFORM-SPECIFIC-EXAMPLES
   "documentation"
   "access-control"
   #+W3P "http:w3p;documentation"
   "log-window"
   #+VRML1.0 "http:examples;vrml;vrml"
   "http:examples;twistdown-tree;twistdown"
   "http:examples;mcf095;mcf"
   "listener"
   "slides"
   "exports"                     ; server example exports
   ))


;; Based on w3p/sysdcl.lisp
(sct-defsystem w3p
    (:pretty-name "W3 Presentation System"
     :default-pathname "HTTP:W3P;"
     :journal-directory "HTTP:LISPM;W3P;PATCH;"
     :initial-status :experimental
     :patchable t
     :source-category :basic)
  (:module documentation
   ("SYS:SITE;W3P.SYSTEM"
    "HTTP:W3P;DOCUMENTATION")
   (:type :lisp-example))
  (:serial
    "HTTP:W3P;PACKAGE"
    "HTTP:W3P;CLASS"
    "HTTP:W3P;W3P-SYSTEM"
    "HTTP:W3P;FUNCTIONS"
    "HTTP:W3P;STANDARD-TYPES"
    "HTTP:W3P;STANDARD-METHODS"
    "HTTP:W3P;HTML-DEFINITIONS"
    ;; Recompiling defs referenced here undoes indentation changes made
    ;; here so load last (and should reload whenever referenced defs are
    ;; recompiled).  With modules, could say (:uses-definitions-from
    ;; w3p-system).
    #+Genera
    "HTTP:W3P;ZWEI-INDENTATION"))

;; Based on lispm/lambda-ir/sysdcl.lisp
(sct-defsystem lambda-ir
    (:pretty-name "Lambda Information Retrieval System"
     :default-pathname "http:lambda-ir;"
     :journal-directory "http:lispm;lambda-ir;patch;"
     :initial-status :experimental
     :patchable t
     :source-category :basic)
  (:module pointers
   ("sys:site;lambda-ir.system")
   (:type :lisp-example))
  (:serial
    "package"
    "ir-utils"
    "variables"
    "class"
    "bit-vectors"
    "data-structures"
    "computations"
    "ir-base"
    "contexts"
    "constraints"
    "bin-dump-utils"
    "http:lambda-ir;examples;lambdavista"
    "http:lambda-ir;examples;stemming"
    "http:lambda-ir;examples;lambdavista-exports"))

(sct-defsystem mail-archive
    (:pretty-name "Mail Archive Server"
     :default-pathname "http:examples;")
  (:serial
   "mail-archive"))

(sct-defsystem lambda-ir-and-mail-archive-index
    (:pretty-name "Mail Archive Server"
     :default-pathname "http:examples;")
  (:module MAIL-ARCHIVE ("MAIL-ARCHIVE") (:type :system))
  (:module LAMBDA-IR ("LAMBDA-IR") (:type :system))
  (:serial
   LAMBDA-IR
   MAIL-ARCHIVE
   "mail-archive-index"))


;; Based on lispm/proxy/sysdcl.lisp
(sct-defsystem http-proxy
    (:pretty-name "HTTP Proxy Server"
     :default-pathname "HTTP:PROXY;"
     :journal-directory "HTTP:lispm;PROXY;PATCH;"
     :initial-status :experimental
     :patchable t
     :source-category :basic)
  (:module pointers
   ("sys:site;http-proxy.system")
   (:type :lisp-example))
  (:module cl-http
   (cl-http)
   (:type :system))
  (:module http-client-substrate
   (http-client-substrate)
   (:type :system))
  (:serial
    cl-http					;Server
    http-client-substrate			;Client Substrate
    "http:proxy;utils"
    "http:proxy;database"			;Database
    "http:proxy;cache"
    "http:proxy;proxy-cache"
    "http:proxy;proxy"				;Proxy Server
    "http:proxy;documentation"))

