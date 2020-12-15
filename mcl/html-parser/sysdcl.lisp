;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: common-lisp-user -*-
;;;
;;; Copyright 1997, John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;;  SYSTEM DEFINITION FOR HTML-PARSER
;;;  Sunil Mishra

(in-package :cl-user)

(setf (logical-pathname-translations "html-parser")
      `(("*.*" ,(merge-pathnames "*.*" (truename  (pathname "http:html-parser;v8;"))))))

(define-system 
   (html-parser)
   (:load :compile)
   ;; parser code
   "html-parser:packages"
   "html-parser:defs"
   "html-parser:patmatch"
   "html-parser:rewrite-engine"
   "html-parser:rewrite-rules"
   "html-parser:html-tags"
   "html-parser:html-reader"
   "html-parser:html-parser"
   "html-parser:html-utilities")

(html-parser:initialize-parser)
