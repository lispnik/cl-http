;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: cl-user -*-

;;; (C) Copyright 1996, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;
;;; Christopher R. Vincent
;;; cvince@ai.mit.edu
;;;
;;;------------------------------------------------------------------- 
;;;
;;; BASIC PRESENTATION SYSTEM FOR THE WORLD-WIDE WEB
;;;

(in-package :cl-user)

#+Genera
(unless (fs:get-logical-pathname-host "HTTP" t)
  (fs:make-logical-pathname-host "HTTP"))

#+Genera
(sct:defsystem w3p
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
    "HTTP:W3P;ZWEI-INDENTATION"))
