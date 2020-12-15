;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: future-common-lisp-user; Base: 10 -*-

;;; (C) Copyright 1995-97, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; W4: EXAMPLES FOR THE CONSTRAINT-GUIDED WORLD-WIDE WEB WALKER
;;;

#+Genera
(unless (fs:get-logical-pathname-host "HTTP" t)
  (fs:make-logical-pathname-host "HTTP"))

#+Genera
(sct:defsystem w4-examples
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
