;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: cl-user; Base: 10 -*-

(in-package "USER")

;; Based on lispm/client/sysdcl-substrate.lisp
;; subsystem
(defsystem "http-client-substrate"
    (:pretty-name "HTTP Client Substrate"
     :default-pathname "HTTP:client;"
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
    "HTTP:lispm;client;lispm"
    "variables"				;variables controlling the client
    "connection"			;symbols
    "client"				; WWW Client substrate
    #+Allegro
    "HTTP:acl;client;unix"))

;; Based on lispm/client/sysdcl.lisp
(defsystem "http-base-client"
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
    #-(or ACLPC LispWorks)
    image-substrate                             ;Fails on 36xx machines  7/29/94 -- Benjamin.
    http-client-substrate
    #+Genera
    "HTTP:LISPM;CLIENT;IMAGES-LISPM"            ;Image hacking for lispm client
    #+Genera
    "http:lispm;client;client"                  ;Client support 
    "sexp-browser"))                            ;S-exp oriented client interface

;; This requires that the server be loaded first as there is considerable shared code.
;; options are :load :compile :eval-load :compile-load-always, :compile-load

;; Based on lispm/w4/sysdcl.lisp
(defsystem "w4"
    (:pretty-name "W4 Constraint-Guided Web Walker"
     :default-pathname "HTTP:w4;"
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
   #-Franz-Inc
   cl-http
   #-Franz-Inc
   http-client
   "http:client;w4-client"		; W4 client support
    "package"                           ; Package Definition
    "variables"                         ; Variables
    "utils"                             ; Utility functions and macros
    "class"                             ; Class definitions and Print methods
    "walker"                            ; Main Walker code
    "constraints"                       ; Constraint Definitions
    "actions"                           ; Action definitions
    "activity"                          ; Activity definitions
    #+Genera
    patches))
    
(defsystem "w4-examples"
    (:pretty-name "W4 Examples"
     :default-pathname "HTTP:w4;examples;"
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
   "trace"
   "search"				; Salton style search example.
   "web-archive"
   "exports"					; Basic examples
))

;; Based on http:html-parser;v8;sysdcl.lisp

(setf (logical-pathname-translations "html-parser")
  `(("html-parser;*.*.*" ,(translate-logical-pathname "HTTP:html-parser;v8;"))
    ("**;*.*.*" ,(translate-logical-pathname "HTTP:html-parser;v8;"))
    ("*.*.*" ,(translate-logical-pathname "HTTP:html-parser;v8;"))))

(defsystem "HTML-Parser"
    (:pretty-name "HTML Parser"
     :default-pathname "html-parser:html-parser;"
     :journal-directory "html-parser:journal;"
     :initial-status :released
     :patchable t
     :source-category :basic)
  (:module pointers
   ("sys:site;html-parser.translations"
    "sys:site;html-parser.system")
   (:type :lisp-example))
  (:module examples
   ("html-parser:html-parser;readme.text")
   (:type :lisp-example))
  (:serial
    "packages"
    #-(or Franz-Inc cl-http) "tokenizer"
    #-(or Franz-Inc cl-http) "plist"
    "defs"
    "patmatch"
    "rewrite-engine"
    "rewrite-rules"
    "html-tags"
    "html-2-0"
    "html-3-2"
    "html-reader"
    "html-parser"
    "html-utilities"))

(defsystem "dtd-compiler"
    (:pretty-name "dtd compiler"
     :default-pathname "html-parser:html-parser;"
     :journal-directory "html-parser:journal;"
     :initial-status :released
     :patchable t
     :source-category :basic)
  (:module html-parser
   (html-parser)
   (:type :system))
  (:serial
    html-parser
    "defs"
    "patmatch"
    "rewrite-engine"
    "rewrite-rules"
    "html-tags"))
