;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: (w4 :use (future-common-lisp www-utils url http)); -*-

;;; Copyright John C. Mallery,  1995-96.
;;; All rights reserved.

;;;------------------------------------------------------------------- 
;;;
;;; PACKAGE DEFINITION
;;;

(in-package :cl-user)

(pushnew :w4 *features*)

(defpackage w4
  (:use future-common-lisp www-utils url http)
  (:import-from "HTTP" "*HEADERS*" "*LOCAL-CONTEXT*" "CLIENT-CONDITION"
                "GET-STRING-FOR-STATUS-CODE"
                "HANDLER-CASE-IF" "HANDLING-REDIRECTS" "INVOKE-HTTP-SERVICE" "PLIST"
                "PRINT-HEADERS" "PUSH-ORDERED" "WITH-HTTP-REQUEST")
  (:import-from "URL" "*URL-TABLE*")
  #+Genera(:import-from "SYS" "FUNCTION-PARENT")
  (:export
    ;; classes
    "CONSTRAINT-TYPE"
    "CONSTRAINT"
    "CONTEXT-CONSTRAINT"
    "GENERATOR-TYPE"
    "HEADER-CONSTRAINT"
    "RESOURCE-CONSTRAINT"
    "HTML-CONSTRAINT"
    "HTML-HEAD-CONSTRAINT"
    "HTML-BODY-CONSTRAINT"
    ;;; functions
    "*DEBUG-WALKER*"
    "*REPORT-STREAM*"
    "*STANDARD-GET-ROBOT-HEADERS*"
    "*STANDARD-HEAD-ROBOT-HEADERS*"
    "*TRACE-WALKER*"
    "*USER-AGENT*"
    "DEBUG-WALKER"
    "DEFINE-ACTION-TYPE"
    "DEFINE-ACTIVITY"
    "DEFINE-CONSTRAINT-TYPE"
    "INTERN-ACTION-TYPE"
    "INTERN-ACTIVITY"
    "INTERN-CONSTRAINT-TYPE"
    "LET-CONSTRAINT-STRUCTURE"
    "SATISFIES-P"
    "UNINTERN-ACTION"
    "UNINTERN-ACTIVITY"
    "WITH-ACTIVITY"
    "WITH-ACTIVITY-VALUE-CACHED"))
