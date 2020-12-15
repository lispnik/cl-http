;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: common-lisp-user; Base: 10 -*-

;;;;
;;;
;;; Copyright 1996, Hallvard Tr¾tteberg
;;; You can use the code if you like, but include this notice.
;;;
;;;;

(in-package :cl-user)

(defpackage :html-base
  (:use "COMMON-LISP")
  #+:CL-HTTP (:import-from "URL" "URL")
  (:export "HTML-CLASS" "HTML-CLAUSE" "HTML-COMMENT" "HTML-ENVIRONMENT"
           "HTML-DOCUMENT" "UNKNOWN-HTML"

           "HTML-CODE" "HTML-OPTIONS" "HTML-CONTENT"
           "HTML-REQUIRED-OPTIONS" "HTML-OPTIONAL-END"
           "HTML-OUT-OF-CONTEXT-P" "HTML-UNKNOWN-OPTIONS"

           "HTML-OPTION-DEF" "HTML-OPTION-TYPE" "HTML-OPTION-CODE"
           "FIND-HTML-OPTION" "SET-HTML-OPTIONS" "VALIDATE-HTML-OPTIONS"
           "*HTML-CLASS-ALIST*" "*HTML-VERSION*"
           "HTML-INSTANCE-CLASS" "MAKE-HTML"

           "BOOLEAN" "URL"
           "WRITE-HTML"

           "MAP-HTML" "MATCH-OPTIONS" "MATCH-HTML" "HTML-MATCH" "FIND-HTML"
           "*HTML-CONTEXT*" "*HTML-STREAM*"
           "MAKE-HTML-STREAM" "DEFAULT-HTML-STREAM" "DEFAULT-HTML"
           
           "ADD-HTML-PART" "POP-HTML-CONTEXT" "PUSH-HTML-CONTEXT"
           "WITH-HTML-ENVIRONMENT" "WITH-HTML-DOCUMENT"
           "DEFINE-HTML-VERSION" "DEFINE-HTML" "DEFINE-HTML-CLAUSE" "DEFINE-SUB-HTML"
           ))


