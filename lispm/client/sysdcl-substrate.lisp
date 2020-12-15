;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: future-common-lisp-user; Base: 10 -*-

;;; (C) Copyright 1994-1997, John C. Mallery.
;;;     All Rights Reserved.
;;;

#+Genera
(unless (fs:get-logical-pathname-host "HTTP" t)
  (fs:make-logical-pathname-host "HTTP"))

(sct:defsystem http-client-substrate
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
    "HTTP:LISPM;CLIENT;LISPM"                   ;Network Support
    "VARIABLES"                                 ;Variables Controlling Client
    "CONNECTION"                                ;Persistent Connections
    "CLIENT"))                                  ;WWW Client substrate
