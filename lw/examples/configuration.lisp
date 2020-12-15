;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: future-common-lisp-user; Base: 10 -*-

(in-package :HTTP)

(set-standard-http-port 8000)

(setf (listening-on-http-ports) (list *standard-http-port*))

