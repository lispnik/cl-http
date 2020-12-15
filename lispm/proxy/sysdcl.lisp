;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: future-common-lisp-user; Base: 10 -*-

;;; (C) Copyright 1997, Christopher R. Vincent
;;;     All Rights Reserved.
;;;

#+Genera
(unless (fs:get-logical-pathname-host "HTTP" t)
  (fs:make-logical-pathname-host "HTTP"))

(sct:defsystem http-proxy
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
