;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: common-lisp-user; Base: 10 -*-

;;; Copyright 1996, Hallvard Tr¾tteberg
;;; You can use the code if you like, but include this notice.
;;;D,#TD1PsT[Begin using 006 escapes](1 0 (NIL 0) (NIL :ITALIC NIL) "CPTFONTI") 0Hallvard Traetteberg
;;;1 0Sect. of Information Systems, SINTEF Tele og Data
;;;1 0P.O.Box 124 Blindern, N-0314 Oslo, Norway
;;;1 0Tlf: +47 2206 7983 or +47 2206 7300, Fax: +47 2206 7350
;;;1 0Email: Hallvard.Tretteberg@informatics.sintef.no
;;;
;;;------------------------------------------------------------------- 
;;;
;;; HTML PARSER
;;;

(in-package :cl-user)

(define-system 
  (html-parser)
  (:compile-load)
  "http:contrib;hallvard;parser;packages"
  "http:contrib;hallvard;parser;html"
  "http:contrib;hallvard;parser;html-versions"
  "http:contrib;hallvard;parser;html-2-0"
  "http:contrib;hallvard;parser;netscape-2-0"
  "http:contrib;hallvard;parser;html-parser")

