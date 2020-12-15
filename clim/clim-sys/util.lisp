;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: MINP; Base: 10 -*-

;;; Copyright (C) 1994, 1995 Olivier (OBC) all rights reserved.
;;; See copyright notice in MINIPROC file.

(in-package "MINP")

(defmacro with-open-file ((file-stream pathname . options) . body)
  `(let ((,file-stream (open ,pathname ,@options)))
     (clim-sys:unwind-process (progn ,@body)
       (close ,file-stream))))
