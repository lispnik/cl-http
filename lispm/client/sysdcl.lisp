;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: future-common-lisp-user; Base: 10 -*-

;;; (C) Copyright 1994-1999, John C. Mallery.
;;;     All Rights Reserved.
;;;

#+Genera
(unless (fs:get-logical-pathname-host "HTTP" t)
  (fs:make-logical-pathname-host "HTTP"))

(sct:defsystem http-base-client
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
  (:module image-substrate                      ;Lispm image substrate for client
   (image-substrate)
   (:type :system :version :latest))
  (:serial
    image-substrate                             ;Fails on 36xx machines  7/29/94 -- Benjamin.
    http-client-substrate
    "http:lispm;client;images-lispm"            ;Image hacking for lispm client
    "http:lispm;client;client"                  ;Client support 
    "sexp-browser"				;S-exp oriented client interface
    "flogger"))					;Flogger for testing server apps
