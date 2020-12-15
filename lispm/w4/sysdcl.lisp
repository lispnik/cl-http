;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: future-common-lisp-user; Base: 10 -*-

;;; (C) Copyright 1995-97, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; W4: CONSTRAINT-GUIDED WORLD-WIDE WEB WALKER
;;;

#+Genera
(unless (fs:get-logical-pathname-host "HTTP" t)
  (fs:make-logical-pathname-host "HTTP"))

#+Genera
(sct:defsystem w4
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
    patches))
