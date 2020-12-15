;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: cl-user; Base: 10 -*-

(in-package "USER")

;; Based on lispm/client/sysdcl-substrate.lisp
;; subsystem
(sct-defsystem http-client-substrate
    (:pretty-name "HTTP Client Substrate"
     :default-pathname "HTTP:CLIENT;"
     :journal-directory "HTTP:LISPM;CLIENT;PATCH;"
     :initial-status :experimental
     :patchable t
     :source-category :basic)
  (:module pointers
   ("sys:site;http-client-substrate.system")
   (:type :lisp-example))
  (:module cl-http
   (cl-http)
   (:type :system))
  (:serial
    cl-http
    #+Genera
    "HTTP:LISPM;CLIENT;LISPM"                   ;Network Support
    "VARIABLES"                                 ;Variables Controlling Client
    "CONNECTION"                                ;Persistent Connections
    "CLIENT"                                    ;WWW Client substrate
    #+LispWorks
    "HTTP:lw;client;unix"
    ))

;; Based on lispm/client/sysdcl.lisp
(sct-defsystem http-base-client
    (:pretty-name "HTTP Client"
     :default-pathname "HTTP:CLIENT;"
     :journal-directory "HTTP:LISPM;CLIENT;PATCH;"
     :initial-status :experimental
     :patchable t
     :source-category :basic)
  (:module pointers
   ("sys:site;http-base-client.system")
   (:type :lisp-example))
  (:module http-client-substrate
   (http-client-substrate)
   (:type :system))
  #+Genera
  (:module image-substrate                      ;Lispm image substrate for client
   (image-substrate)
   (:type :system :version :latest))
  (:serial
    #-LispWorks
    image-substrate                             ;Fails on 36xx machines  7/29/94 -- Benjamin.
    http-client-substrate
    #+Genera
    "http:lispm;client;images-lispm"            ;Image hacking for lispm client
    #+Genera
    "http:lispm;client;client"                  ;Client support 
    "sexp-browser"                              ;S-exp oriented client interface
    "flogger"))					;Flogger for testing server apps


;; This requires that the server be loaded first as there is considerable shared code.
;; options are :load :compile :eval-load :compile-load-always, :compile-load


;; Based on lispm/w4/sysdcl.lisp
(sct-defsystem w4
    (:pretty-name "W4 Constraint-Guide Web Walker"
     :default-pathname "HTTP:W4;"
     :journal-directory "HTTP:LISPM;W4;PATCH;"
     :initial-status :released
     :patchable t
     :source-category :basic)
  (:module pointers
   ("SYS:SITE;HTTP.TRANSLATIONS"
    "SYS:SITE;W4.SYSTEM")
   (:type :lisp-example))
  #+Genera
  (:module patches
   ("HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH"))
  (:module http-client-substrate
   (http-client-substrate)
   (:type :system))
  (:serial
    http-client-substrate			; Client Substrate
    "http:client;w4-client"			; W4 client support
    "http:w4;package"                           ; Package Definition
    "http:w4;variables"                         ; Variables
    "http:w4;utils"                             ; Utility functions and macros
    "http:w4;class"                             ; Class definitions and Print methods
    "http:w4;walker"                            ; Main Walker code
    "http:w4;constraints"                       ; Constraint Definitions
    "http:w4;actions"                           ; Action definitions
    "http:w4;activity"                          ; Activity definitions
    #+Genera
    patches))


;; Based on lispm/w4/examples-sysdcl.lisp
(sct-defsystem w4-examples
    (:pretty-name "W4 Examples"
     :default-pathname "http:w4;examples;"
     :journal-directory "http:lispm;w4;patch;"
     :initial-status :released
     :patchable t
     :source-category :basic
     :required-systems w4)
  (:module pointers
   ("sys:site;http.translations"
    "sys:site;w4-examples.system")
   (:type :lisp-example))
  (:module examples
   ("http:w4;examples;trace"			;basic examples
    "http:w4;examples;search"			;Salton-style search example.
    "http:w4;examples;web-archive"))		;Web Whacker
  (:serial
    "http:w4;examples;exports"))		;Export the example URLS


