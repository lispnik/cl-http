;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: common-lisp-user -*-
;;;
;;; Copyright 1994-97, John C. Mallery.
;;; All rights reserved. 

;;;------------------------------------------------------------------- 
;;;
;;;  CONFIGURE MCL FOR SERVER OPERATION
;;;

(in-package :ccl)

;; Make compilation switches are set to preserve function information for better demo
;; value and user hackability.
(setq ccl:*fasl-save-local-symbols* t
         ccl:*fasl-save-doc-strings* t
         ccl:*fasl-save-definitions* t
         ccl::*save-arglist-info* t
         ccl:*save-doc-strings* t
         ccl:*record-source-file* t)
