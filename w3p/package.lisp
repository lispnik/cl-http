;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: cl-user; -*-

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

;; advise the lisp environment of the presence of W3P
(pushnew :w3p *features*)

(defpackage www-present
  (:use future-common-lisp)
  (:nicknames w3p)
  (:export 

    ;;classes
    "HTML-VIEW"
    "PRESENTATION-TYPE"
    "TEXTUAL-VIEW"
    "VIEW"

    ;;symbols
    "TYPE-KEY"
    "PARAMETERS"
    
    ;;functions and macros
    "CALL-PRESENTATION-GENERIC-FUNCTION"
    "DEFINE-PRESENTATION-GENERIC-FUNCTION"
    "DEFINE-PRESENTATION-METHOD"
    "DEFINE-PRESENTATION-TYPE"
    "DEFINE-PRESENTATION-TYPES"
    "DEFINE-PRESENTATION-VIEW"
    "DEFINE-PRESENTATION-VIEW-CLASS"
    "DEFINE-PRESENTATION-VIEW-CLASSES"
    "INPUT-NOT-OF-REQUIRED-TYPE"
    "PRESENTATION-TYPE-SUPERIOR-P"
    "PRESENTATION-TYPE-OF"
    "READ-TOKEN"
    "REMOVE-PRESENTATION-GENERIC-FUNCTION"
    "REMOVE-PRESENTATION-TYPE"
    "REMOVE-PRESENTATION-TYPES"
    "STREAM-DEFAULT-VIEW"
    "WITH-STANDARD-HTML-PROMPT"
    "WITH-PRESENTATION-TYPE-DECODED"
    "WITH-PRESENTATION-TYPE-PARAMETERS"
    "WITH-PRESENTATION-TYPE-OPTIONS"

    ;;presentation-generic-functions
    "ACCEPT"
    "ACCEPT-FROM-STRING"
    "ACCEPT-PRESENT-DEFAULT"
    "DESCRIBE-PRESENTATION-TYPE"
    "HANDLE-INPUT-ERROR"
    "PRESENT"
    "PRESENTATION-SUBTYPEP"
    "PRESENT-TO-STRING"
    "PRESENTATION-TYPE-SPECIFIER-P"
    "PRESENTATION-TYPEP"

    ;;presentation-views
    "+TEXTUAL-VIEW+"
    "+HTML-VIEW+"

    ;;presentation-types
    "AND"
    "BASIC-STRING"
    "BOOLEAN"
    "BOUNDED-STRING"
    "CHARACTER"
    "COMPLETION"
    "COMPLEX"
    "DOUBLE-FLOAT"
    "EXISTING-PATHNAME"
    "EXPRESSION"
    "FIXNUM"
    "FLOAT"
    "FORM"
    "INTEGER"
    "KEYWORD"
    "MEMBER"
    "MEMBER-ALIST"
    "MEMBER-SEQUENCE"
    "MIXED-SEQUENCE"
    "NULL"
    "NULL-OR-TYPE"
    "NUMBER"
    "OR"
    "PATHNAME"
    "RATIO"
    "RATIONAL"
    "REAL"
    "SEQUENCE"
    "SEQUENCE-ENUMERATED"
    "SHORT-FLOAT"
    "STRING"
    "SUBSET"
    "SUBSET-ALIST"
    "SUBSET-COMPLETION"
    "SUBSET-SEQUENCES"
    "SYMBOL"
    "T"
    "TOKEN-OR-TYPE"
    "TYPE-OR-STRING"

    ;;presentation-conditions
    "CLASS-NOT-ACCEPTABLE-PRESENTATION-TYPE"
    "HANDLE-INPUT-ERROR"
    "INPUT-NOT-OF-REQUIRED-TYPE"
    "PARSE-ERROR"
    "PRESENTATION-CONDITION"
    "PRESENTATION-GENERIC-FUNCTION-NOT-FOUND"
    "PRESENTATION-INPUT-CONDITION"
    "PRESENTATION-TYPE-NOT-FOUND"))
