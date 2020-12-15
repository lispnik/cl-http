;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: cl-user; Base: 10 -*-

(in-package "CL-USER")

(defsystem cl-http                          
  :source-pathname "HTTP:"
  :components
  ("cmucl;server;cmucl-patch"	; CMU CL patches.

   "mcl;server;resources"	; Portable resource package.

   "server;package"		; HTTP Packages
   "cmucl;server;package.lisp"  ; CMU CL package changes.
   "smtp;package.lisp"		; SMTP Package.

   "w3p;package"		; Setup the www-present package for precom.
   #+nil "cmucl;server;precom"	; May want to load early once compiled.

   "server;preliminary"		; Showable procedures
   "server;variables"		; Variables and constants

   "mcl;server;www-utils"	; Portable utilities.
   
   #-mp "cmucl;server;tcp-stream"
   #+mp "cmucl;server;tcp-stream-mp"
   "cmucl;server;ftp"
   "cmucl;server;unix"

   (:module server2
      :source-pathname "server;"
      :components
      ("base64-encoding"	; base 64 utility
       "md5"			; MD5 utilitity
       "sha"			; SHA Digests based on Internet FIPS 180
       "task-queue"
       "class"
       "url-class"
       "http-conditions"	; HTTP conditions
       "plist"			; property list mixin for CLOS
       "utils"			; Server utility functions
       "tokenizer"		; Simple tokenizer
       "headers"		; Header hacking, including MIME
       "host"			; Host objects and operations
       "url"			; URL implementation
       "html2"			; HTML authoring functions
       "netscape-1-1"		; Netscape 1.1 html generation
       "vrml-1-0"		; VRML 1.0 Generation
       "image-maps"		; Image maps
       "netscape-2-0"		; Netscape 2.0 HTML generation
       "netscape-3-0"		; Netscape 3.0 HTML generation
       "html-3-2"		; HTML 3.2 Generation	
       "scripts"		; Client-Side Scripts
       "netscape-4-0"		; Netscape 4.0 HTML generation
       "shtml"			; Server-Parsed HTML
       ))

   (:module w3p
      :source-pathname "w3p;"
      :components
      ("package"
       "class"
       "w3p-system"
       "functions"
       "standard-types"
       "standard-methods"
       "html-definitions"
       "documentation"
       ))

   (:module server3
      :source-pathname "server;"
      :components
      ("report"			; Reporting for HTTP Conditions
       "log"			; Logging 
       "authentication"		; User authentication
       "data-cache"		; Data caching
       "server"			; HTTP Server
       "cgi"			; Common Gateway Interface
       "preferences"	        ; Server preference system
       "web-configuration"      ; Server Configuration via the Web
       ))

   "cmucl;server;server-patch"  ; Patches

   (:module tcp-interface
      :source-pathname "cmucl;server;"
      :components
      (#-mp "tcp-interface"
       #+mp "tcp-interface-mp"))
   
   #+mp
   (:module smtp
      :source-pathname "smtp;"
      :components
      ("smtp"			; Simple SMTP mailer
       "mail"))			; Interfaces for sending email

   (:module pcl-precomp
      :source-pathname "cmucl;server;"
      :components ("precom"))))

(defsystem cl-http-examples
    :source-pathname "HTTP:examples;"
    :components
    ("configuration"	; server configuration file
     "mcf095;mcf"
     "exports"		; server example exports
     "vrml;vrml"	; best to compile this
     "documentation"
     "log-window"
     "mail-archive"
     ;; Requires Lambda-Vista.
     #+nil "mail-archive-index"
     ;; Listener not loaded by default as it is a potential security
     ;; risk.
     #+nil "listener"))
