;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4 -*-

;;; (C) Copyright 1997, Christopher R. Vincent
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CL-HTTP PROXY SERVER
;;;

(in-package :w4)

;; Walker needs to handle chunk-decoding.

;; Needs access to the request headers.
;; Also needs to always get a (untranslated) byte array from the walker
(define-action-type
  add-to-proxy-cache
  (:standard
   :documentation "An action that adds a web resource to the HTTP proxy cache.")
  (action activity url &key (cache *http-proxy-cache*))
  (declare (ignore action))
  (let* ((report-stream (report-stream activity))
         (uri-string (name-string url))
         (request-headers)
         (response-headers (get-resource-headers activity url))
         (content (get-resource-content activity url)))
    (cond (content
           (let* ((resource (http:intern-cached-resource uri-string 
                                                         :if-does-not-exist :create
                                                         :cache cache))
                  (representation (http:intern-cached-representation resource request-headers 
                                                                     :if-does-not-exist :create)))
             (format report-stream "~&;Adding content to ~S." representation)
             (unwind-protect
               (progn
                 (http:with-output-to-representation-entity (entity-stream representation)
                   (etypecase content
                     (string 
                      (loop for c across content
                            doing (write-byte (char-code c) entity-stream)))
                     (vector (http::binary-stream-copy-from-8-bit-array content entity-stream 0 (length content)))))
                 (setf (http:cached-representation-response-headers representation) response-headers
                       (http:cached-representation-verified-date representation) (get-header :date response-headers)))
               (when (zerop (cache-object-size representation))
                 (format report-stream "~&;Removing ~S." representation)
                 (http:unintern-cached-representation representation)))))
          (t (format report-stream "~&;No content for ~A." uri-string)))
    (force-output report-stream)))

; Fix report-stream!
(defmethod prefetch-cache ((root-url url) (cache http:http-cache) &key
                           (depth 0)  
                           operator 
                           hosts
                           constraints 
                           minimize-dns-p
                           (report-stream *standard-output*) 
                           (respect-no-robots-p t))
  (declare (ignore report-stream))
  (let ((host (host-string root-url)))
    (with-activity
      ("proxy-cache-prefetch"
       (:url-host-name-resolution (if minimize-dns-p :never :preferred) 
                                  :if-does-not-exist :uninterned 
                                  :operator operator
                                  :report-stream '*standard-output*)
       :constraints `((depth ,depth)
                      (no-cycles)
                      ,.(when respect-no-robots-p
                          (list '(header-robots-allowed)))
                      ,.(when hosts
                          `((url-referrer-host ,host)))
                      ,.(when constraints constraints))
       :actions `((add-to-proxy-cache :cache ,cache)
                  (generate-inferiors)))
      (walk root-url activity))))

#||

(prefetch-cache (intern-url "http://wilson.ai.mit.edu/cl-http/cl-http.html" :if-does-not-exist :uninterned)
                *http-proxy-cache*
                :depth 5
                :respect-no-robots-p nil
                :hosts '("wilson.ai.mit.edu"))

||#
