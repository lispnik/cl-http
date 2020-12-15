;;;   -*- Mode: LISP; Package: cl-user; BASE: 10; SYNTAX: ansi-common-lisp -*-

;;; (C) Copyright 1997, Andrew J. Blumberg (blumberg@ai.mit.edu).
;;;     All Rights Reserved.
;;;
;;;
;;;------------------------------------------------------------------- 
;;;
;;; INFORMATION RETRIEVAL SUBSTRATE
;;;

(in-package :cl-user)

(define-system 
   (lambda-ir)
   ()
   "http:lambda-ir;package"
   "http:lambda-ir;ir-utils"
   "http:lambda-ir;variables"
   "http:lambda-ir;class"
   "http:lambda-ir;bit-vectors"
   "http:lambda-ir;data-structures"
   "http:lambda-ir;computations"
   "http:lambda-ir;ir-base"
   "http:lambda-ir;contexts"
   "http:lambda-ir;constraints"
   "http:lambda-ir;bin-dump-utils"
   "http:lambda-ir;examples;lambdavista"
   "http:lambda-ir;examples;stemming"
   "http:lambda-ir;examples;lambdavista-exports")
