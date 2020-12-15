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

(define-system 
  (w3p)
  (:compile-load)                       ; needed because embedded systems are not defined yet
  "http:w3p;package"
  "http:w3p;class"
  "http:w3p;w3p-system"
  "http:w3p;functions"
  "http:w3p;standard-types"
  "http:w3p;standard-methods"
  "http:w3p;html-definitions")
