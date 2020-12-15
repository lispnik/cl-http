"-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: common-lisp-user; Base: 10 -*-"

(in-package :cl-user)

;; possibly already loaded
#-:sql
(unless (ignore-errors (logical-pathname-translations "sql"))
  ;; possibly already defined (e.g. if SQL-ODBC normally resides somewhere else
  ;; Is there a more elegant way to check this?
  (setf (logical-pathname-translations "sql")
        '(("**;*.*.*" "http:contrib;pmeurer;sql;**;"))))

#+mcl
(define-system 
  (db-auth)
  (:load)
  #+(and :mcl (not :sql))
  "sql:mcl;use-module"
  #-:sql
  "sql:sql;sql-system"
  "http:contrib;pmeurer;dbauth;sql-authentication"
  "http:contrib;pmeurer;dbauth;db-authentication")

#+lispworks
(sct-defsystem 
 db-auth
 (:pretty-name "Databased Authentication"
  :default-pathname "HTTP:contrib;pmeurer;dbauth"
  :initial-status :experimental)
 (:serial
  #-:sql
  "sql:sql;sql-system"
  "sql-authentication"
  "db-authentication"))

; EOF
