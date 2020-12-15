;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: cl-user; Base: 10 -*-

;;; This file is the default for the -cl-http-init option.
;;; The server is started automatically after loading this file.

(in-package :cl-user) 

;; Load the demo examples and configuration.
(load-system 'cl-http-examples)
