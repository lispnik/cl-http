;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: common-lisp-user -*-
;;;
;;; Copyright 1994-97, John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;; MAKE CL-HTTP IMAGE
;;;

;; If you load this file, you can access the URL
;; http://your.host.domain.name/ and peruse the server documentation,
;; assuming a network connection.

(in-package :cl-user) 

;; Set up MCL appropriately
(load (merge-pathnames "mcl-configuration" ccl:*loading-file-source-file*) :verbose t)

;; Load the server.
(load (merge-pathnames "envdcl" ccl:*loading-file-source-file*) :verbose t) 

