;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: cl-user; Base: 10 -*-
;;
;; start.lisp
;;
;; this shows one way to load and start cl-http on acl.  
;; It always starts the server on port 8000 since this will work regardless
;; of whether you're on Windows or Unix.
;;
;; See the -read-me-.text file for alternative ways.
;; 

(in-package :cl-user)

(load "load.lisp")
(build-cl-http)
(http:start :port 8000)
(load-test)


