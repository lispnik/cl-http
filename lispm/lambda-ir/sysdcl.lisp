;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: cl-user -*-

;;; (C) Copyright 1997, Andrew J. Blumberg (blumberg@ai.mit.edu).
;;;     All Rights Reserved.
;;;
;;;
;;;------------------------------------------------------------------- 
;;;
;;; Basic information retrieval system for CL-HTTP
;;;

(in-package :cl-user)

#+Genera
(unless (fs:get-logical-pathname-host "HTTP" t)
  (fs:make-logical-pathname-host "HTTP"))

#+Genera
(sct:defsystem lambda-ir
    (:pretty-name "Lambda Information Retrieval System"
     :default-pathname "http:lambda-ir;"
     :journal-directory "http:lispm;lambda-ir;patch;"
     :initial-status :experimental
     :patchable t
     :source-category :basic)
  (:module pointers
   ("sys:site;lambda-ir.system")
   (:type :lisp-example))
  (:serial
    "package"
    "ir-utils"
    "variables"
    "class"
    "bit-vectors"
    "data-structures"
    "computations"
    "ir-base"
    "contexts"
    "constraints"
    "bin-dump-utils"
    "http:lambda-ir;examples;lambdavista"
    "http:lambda-ir;examples;stemming"
    "http:lambda-ir;examples;lambdavista-exports"))
