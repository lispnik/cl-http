"-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: common-lisp-user; Base: 10 -*-"

(in-package :cl-user)

(define-system 
  (cgi-support)
  (:load)
  "http:mcl;contrib;tnorderhaug;cgi;defappleevents.lisp"
  "http:mcl;contrib;tnorderhaug;cgi;acgi"
  "http:mcl;contrib;tnorderhaug;cgi;cgi-application")



