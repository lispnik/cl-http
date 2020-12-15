;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: ccl; Base: 10 -*-
;;;
;;; (C) Copyright 1995, Karsten Poeck.
;;;     All Rights Reserved.
;;;


;;;------------------------------------------------------------------- 
;;;
;;; REINDENT FILES FOR DISTRIBUTION
;;;
;;; This fixes indentation problems arising because MCL has a different theory
;;; of how it interprets tabs from the Lisp Machine that used to manage
;;; cl-http sources.

(in-package :ccl) 

(defmethod reindent-complete-buffer ((window fred-window))
  (reindent-complete-buffer (current-key-handler window)))

(defmethod reindent-complete-buffer ((window fred-mixin))
  (let ((buffer (fred-buffer window)))
    (ed-indent-for-lisp window 0 (buffer-size buffer))))

(defmethod reindent-and-update ((window fred-mixin))
  (reindent-complete-buffer window)
  (fred-update window))

(defmethod reindent-and-update-file (filename)
  (let ((window (make-instance 'fred-window 
                               :filename filename
                               :window-show nil)))
    (reindent-complete-buffer window)
    (window-save window)
    (window-close window)))

(defmethod reindent-pathname ((pathname string) &optional (stream *standard-output*))
  (reindent-pathname (pathname pathname) stream))

(defmethod reindent-pathname ((pathname pathname) &optional (stream *standard-output*))
  (cond
    ((directoryp pathname)
     (mapc #'reindent-pathname (directory (merge-pathnames pathname "*.*")
                                          :directories t 
                                          :files t 
                                          :resolve-aliases t)) t)
    (t (let ((extension (pathname-type pathname)))
         (when (string-equal extension "lisp")
           (when stream
             (format stream "~&Reindent: ~A" pathname))
           (reindent-and-update-file pathname))))))

#|

(mapc #'ccl::reindent-pathname
      '("http:examples;" "http:server;" "http:mac;server;" "http:mac;client;" "http:mac;examples;"))

(reindent-pathname "http:server;")l

(comtab-set-key ccl:*control-x-comtab* '(:control #\t) 
                'reindent-and-update
                "Reindents complete buffer")

(reindent-pathname (choose-directory-dialog))
|#

