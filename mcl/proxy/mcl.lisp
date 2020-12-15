;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-

;;; (C) Copyright 1996-97, Christopher R. Vincent
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CL-HTTP PROXY SERVER
;;;

(in-package :http)

(declaim (special *http-proxy-cache*))

(defun make-cache-mcl-readable (&optional (cache *http-proxy-cache*))
  (map-cached-resources
   #'(lambda (uri res)
       (declare (ignore uri))
       (with-slots (representations) res
         (loop for rep in representations
               doing (ccl:set-mac-file-type 
                      (database-identifier-value (cached-representation-entity rep))
                      :text))))
   cache))
