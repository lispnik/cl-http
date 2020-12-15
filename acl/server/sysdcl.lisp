;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: cl-user; Base: 10 -*-

(in-package "USER")

(defsystem "cl-http"
	   (:pretty-name "HTTP Server"
	    :default-pathname "HTTP:server;"
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
  (:module lispm
   ("HTTP:LISPM;SERVER;TCP-LISPM-STREAM"        ; Modal ASCII or binary TCP streams
    "HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE" ;timeout TCP close in 5 seconds
    "HTTP:LISPM;SERVER;LISPM"))                 ; LispM Interface functions
  (:module w3p
   ("W3P")					; W3P Presentation System
   (:type :system))
  (:serial
   #+Genera showable-procedures
    CLIM-SYS					; Port your own CLIM-SYS
   #+ACLPC ACLPC-UPDATE
    "package"					; HTTP Packages
    "preliminary"                               ; Showable procedures
    "variables"                                 ; Variables and constants
    #+Genera lispm                              ; Load Lisp Machine specific code
    #+ACLPC
    "HTTP:acl;aclpc;ipc"
    #+ACL5 "HTTP:acl;acl5;ipc"
    #+(and Allegro (not ACL5))
    "HTTP:acl;server;multivalent-stream"	; Franz specific
    #+(and Allegro-version>= (version< 4 3))
    "HTTP:acl;server;tcp-stream-svr3"		; Franz specific
    #+(and Allegro-version>= (version>= 4 3) (not ACL5))
    "HTTP:acl;server;tcp-stream"		; Franz copied for FF Solaris2
    #+(and Allegro-version>= (version< 4 3))
    "HTTP:acl;server;stream-sequence-svr3"	; Optimizations
    #+(and Allegro-version>= (version>= 4 3) (not ACL5))
    "HTTP:acl;server;stream-sequence"		; Optimizations
    #+(and Allegro (not ACL5))
    "HTTP:acl;server;stream-chunk"		; Chunking hooks
    #+(and Allegro (not ACL5))
    "HTTP:acl;server;chunk-transfer"		; Extract from mac;server;tcp-stream
    "HTTP:mcl;server;resources"
    #+(or UNIX ACLPC ACL5)
    "HTTP:acl;server;unix"			; add-ons for UNIX ports
    "HTTP:mcl;server;www-utils"			; Some portable utils are there
    "base64-encoding"                           ; base 64 utility
    "md5"					; MD5 utilitity
    "sha"				; SHA Digests based on Internet FIPS 180
    "task-queue"
    "http:smtp;package"
    "http:smtp;smtp"				; Simple SMTP mailer
    "http:smtp;mail"				; Interfaces for sending email
    "class"					; Server class definitions
    "url-class"					; URL class definitions 
    "plist"					; property list mixin for CLOS
    "utils"					; utility functions
    "tokenizer"					; Simple tokenizer
    "headers"                                   ; Header hacking, including MIME
    "host"                                      ; Host objects and operations
    "url"                                       ; URL implementation
    "html2"                                     ; HTML authoring functions
    "netscape-1-1"		                ; Netscape 1.1 html generation
    "vrml-1-0"					; VRML 1.0 Generation
    "image-maps"				; Image maps
    "netscape-2-0"				; Netscape 2.0 HTML generation
    "netscape-3-0"				; Netscape 3.0 HTML generation
    "html-3-2"					; HTML 3.2 Generation	
    "scripts"					; Client-Side Scripts
    "netscape-4-0"				; Netscape 4.0 HTML generation
    "shtml"					; Server-Parsed HTML
    #+W3P
    W3P						; W3P Presentation System
    "http-conditions"				; HTTP conditions
    "log"                                       ; Logging 
    "authentication"				; User authentication
    "data-cache"				; Data caching
    "server"                                    ; HTTP Server
    "cgi"					; Common Gateway Interface
    #+W3P
    "preferences"				; Configuration Preference Facility
    #+W3P
    "web-configuration"				; Server Configuration via the Web
    #+FRANZ-INC "HTTP:acl;server;url"		; UNIX CGI prototype
    #+FRANZ-INC "HTTP:acl;server;update"	; Dynamic updates to URLs
    #+Allegro "HTTP:acl;server;cgi"		; UNIX CGI prototype
    #+FRANZ-INC "HTTP:acl;server;proxy"		; Poor proxy experiment
    #+FRANZ-INC "HTTP:acl;server;tcp-interface"
    #+FRANZ-INC "HTTP:acl;server;patched"	; Required future patches
    #+ACLPC "HTTP:acl;aclpc;patched"		; Required PC src redefinitions
    #+ACL5 "HTTP:acl;acl5;patched"
    ))

(defsystem "cl-http-examples-ports"
    (:pretty-name "HTTP examples ports"
     :default-pathname #+FRANZ-INC "HTTP:acl;examples;" #-FRANZ-INC nil
     :journal-directory "HTTP:LISPM;EXAMPLES;PATCH;"
     :initial-status :experimental
     :patchable t
     :source-category :basic)
  (:serial
   "configuration"			; server configuration file
   ;;"exports"                     ; server example exports
   ))

(defsystem "cl-http-examples"
    (:pretty-name "HTTP examples"
     :default-pathname "HTTP:examples;"
     :journal-directory "HTTP:LISPM;EXAMPLES;PATCH;"
     :initial-status :experimental
     :patchable t
     :source-category :basic)
  (:module CL-HTTP-EXAMPLES-PORTS
    ("CL-HTTP-EXAMPLES-PORTS")
    (:type :system))
  (:serial
   "configuration"               ; server configuration file
   CL-HTTP-EXAMPLES-PORTS
   "documentation"
   "access-control"
   #+W3P "http:w3p;documentation"
   "log-window"
   #+VRML1.0 "http:examples;vrml;vrml"
   "http:examples;twistdown-tree;twistdown"
   "http:examples;mcf095;mcf"
   "exports"                     ; server example exports
   ))

(defsystem "cl-http-clim"
    (:pretty-name "HTTP Server Interface"
     :default-pathname "HTTP:server;"
     :journal-directory "HTTP:LISPM;SERVER;PATCH;"
     :initial-status :experimental
     :patchable t
     :source-category :basic)
  (:module pointers
   ("SYS:SITE;CL-HTTP-CLIM.SYSTEM")
   (:type :lisp-example))
  (:module cl-http ("CL-HTTP") (:type :system))
  (:serial
   #-Genera
   CL-HTTP
   "HTTP:clim;clim-interface-package"  ; CLIM Interface
   "HTTP:clim;server;clim-interface"))

;; Based on w3p/sysdcl.lisp
(defsystem "w3p"
    (:pretty-name "W3 Presentation System"
     :default-pathname "HTTP:w3p;"
     :journal-directory "HTTP:LISPM;W3P;PATCH;"
     :initial-status :experimental
     :patchable t
     :source-category :basic)
  (:module documentation
   ("SYS:SITE;W3P.SYSTEM"
    "HTTP:W3P;DOCUMENTATION")
   (:type :lisp-example))
  (:serial
   "package"
   "class"
   "w3p-system"
   "functions"
   "standard-types"
   "standard-methods"
   "html-definitions"))

(defsystem "lambda-ir"
    (:pretty-name "Lambda Information Retrieval"
     :default-pathname "http:lambda-ir;"
     :journal-directory "http:lispm;lambda-ir;patch;"
     :initial-status :experimental
     :patchable t
     :source-category :basic)
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
  ;;"save-utils"
  "bin-dump-utils"
  #+Genera "http:lambda-ir;bin-dump-utils"      ; to be ported
  ";examples;lambdavista"
  ";examples;stemming"
  ";examples;lambdavista-exports"))

(defsystem "mail-archive"
    (:pretty-name "Mail Archive Server"
     :default-pathname "http:examples;")
  (:serial
   LAMBDA-IR
   "mail-archive"
   "mail-archive-index"))

;; Based on lispm/proxy/sysdcl.lisp
(defsystem "http-proxy"
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
