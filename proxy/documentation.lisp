;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-

;;; (C) Copyright 1996-1997, Christopher R. Vincent
;;;     All Rights Reserved.
;;;
;;; (C) Enhancements Copyright 1999, John C. Mallery.
;;;     All Rights Reserved.
;;;


;;;------------------------------------------------------------------- 
;;;
;;; CL-HTTP PROXY SERVER
;;;


(in-package :http)


;;;------------------------------------------------------------------- 
;;;
;;; DATABASE DOCUMENTATION
;;;

(w3p:define-presentation-type cache-identifier ()
  :inherit-from t)

(w3p:define-presentation-method w3p:present (identifier (type cache-identifier) stream (view w3p:html-view) &key)
  "Call generic function on identifier, varys with database type."
  (write-database-identifier identifier stream))


;;;------------------------------------------------------------------- 
;;;
;;; CACHE DOCUMENTATION
;;;

(w3p:define-presentation-type cached-resource ()
  :inherit-from t 
  :options ((verbose-p t)))

(w3p:define-presentation-type cached-representation ()
  :inherit-from t 
  :options ((verbose-p t)))

(w3p:define-presentation-method w3p:accept ((type cached-resource) stream (view w3p:textual-view) &key)
  (let* ((string (w3p:accept 'string :view w3p:+textual-view+ :stream stream))
         (res (intern-cached-resource string *http-proxy-cache* :if-does-not-exist :soft)))
    (unless res
      (w3p:handle-input-error string type))
    res))

(w3p:define-presentation-method w3p:present (resource (type cached-resource) stream (view w3p:html-view) &key)
  (cond (verbose-p
         (with-slots (uri-string creation-date vary representations) resource
           (html:with-paragraph (:stream stream)
             (html:with-rendition (:bold :stream stream)
               (write-string "Resource: " stream))
             (write-string uri-string stream)
             (html:break-line :stream stream)
             (html:with-rendition (:bold :stream stream)
               (write-string "Creation Date: " stream))
             (write-time creation-date stream)
             (html:break-line :stream stream)
             (html:with-rendition (:bold :stream stream)
               (write-string "Vary: " stream))
             (w3p:present vary '(sequence symbol) :view w3p:+textual-view+ :stream stream)
             (html:break-line :stream stream)
             (html:with-rendition (:bold :stream stream)
               (write-string "Total Size: " stream))
             (write (cached-resource-size resource) :stream stream)
             (write-string " bytes" stream)
             (html:break-line :stream stream)
             (html:with-rendition (:bold :stream stream)
               (write-string "Representations: " stream))
             (cond (representations
                    (html:break-line :stream stream)
                    (loop for item in representations
                          do (w3p:present item '((cached-representation) :verbose-p nil) 
                                       :view w3p:+html-view+ :stream stream)
                          (html:break-line :stream stream)))
                   (t (write-string "None" stream))))))
        (t (with-slots (uri-string) resource
             (html:note-anchor uri-string 
                               :reference (concatenate 'string "cached-resource?" uri-string)
                               :stream stream)))))

(w3p:define-presentation-method w3p:present (representation (type cached-representation) stream (view w3p:html-view) &key)
  (macrolet ((writing-entry ((name stream) &body body)
	       `(progn
		  (html:with-rendition (:bold :stream ,stream)
		    (html:fast-format ,stream "~A: " ,name))
		  ,@body
		  (html:break-line :stream ,stream)))
	     (writing-headers ((name stream) &body body)
	       `(writing-entry (,name ,stream)
			       (html:with-paragraph-style (:quotation :fresh-line nil :stream ,stream)
				 (html:with-verbatim-text (:fresh-line nil :width 120 :stream ,stream)
				   ,@body)))))
    (cond (verbose-p
	   (with-slots (resource identifier etag entity creation-date verified-date last-reference must-revalidate-p
				 last-modification) representation
	     (html:with-paragraph (:stream stream)
	       (writing-entry ("Resource" stream)
			      (w3p:present resource '((cached-resource) :verbose-p nil) :view w3p:+html-view+ :stream stream))
	       (writing-entry ("Representation Identifier" stream) (write identifier :stream stream))
	       (when etag
		 (writing-entry ("Etag" stream) (print-entity-tag-header etag stream)))
	       (writing-entry ("Size" stream) (fast-format stream "~D Bytes" (cache-object-size representation)))
	       (writing-entry ("Creation Date" stream) (write-time creation-date stream))
	       (when verified-date
		 (writing-entry ("Last Verified" stream) (write-time verified-date stream)))
	       (when last-modification
		 (writing-entry ("Last Modified" stream) (write-time last-modification stream)))
	       (writing-entry ("Expiration Date" stream)
			      (write-time (cached-representation-expiration-time representation) stream))
	       (when last-reference
		 (writing-entry ("Last Reference" stream)(write-time last-reference stream)))
	       (when must-revalidate-p 
		 (writing-entry ("Must Revalidate" stream) (fast-format stream "yes")))
	       (writing-entry ("Server HTTP Version" stream) (fast-format stream "~A" (cached-representation-http-version representation)))
	       (writing-entry ("Cached Entity" stream)
			      (flet ((write-ref (stream)
				       (fast-format stream "/cl-http/proxy/cached-representation?~A+~A+entity"
						    (cached-resource-uri-string resource) identifier)))
				(declare (dynamic-extent #'write-ref))
				(html:with-anchor-noted (:reference #'write-ref :stream stream)
				  (w3p:present entity '((cache-identifier) :representation representation) 
					       :view w3p:+html-view+ :stream stream)))))
	     (html:with-paragraph (:stream stream)
	       (writing-headers ("Response Headers" stream)
			      (write-response-headers representation stream))
	       (writing-headers ("Request Headers" stream)
			      (write-request-headers representation stream)))))
	  (t (with-slots (resource identifier) representation
	       (fast-format stream "Representation: ")
	       (flet ((write-ref (stream)
			(fast-format stream "/cl-http/proxy/cached-representation?~A+~A"
				     (cached-resource-uri-string resource) identifier)))
		 (declare (dynamic-extent #'write-ref))
		 (html:with-anchor-noted (:reference #'write-ref :stream stream)
		   (fast-format stream "ID#~D" identifier))))))))
             
(defmacro write-back-to-resources (stream)
  `(html:with-paragraph (:stream ,stream)
     (write-string "Back to " stream)
     (note-anchor "cached resources" :reference "cached-resource?" :stream stream)
     (write-char #\. stream)))

(defmethod respond-to-show-resource ((url url:http-search) stream)
  (with-successful-response (stream :html :expires (get-universal-time)
                                    :content-location url)
    (let ((title "Proxy Server Cached Resource"))
      (html:with-html-document (:stream stream)
        (html:with-document-preamble (:stream stream)
          (html:declare-base-reference url :stream stream)
          (html:declare-title title :stream stream))
        (ns1.1:with-document-body (:stream stream :background :white :foreground :black)
          (html:with-section-heading (title :level 2 :stream stream)
            (with-slots (url:search-keys) url
              (cond (url:search-keys
                     (handler-case 
                       (let ((resource (w3p:accept-from-string 'cached-resource (car url:search-keys))))
                         (cond (resource (w3p:present resource '((cached-resource) :verbose t) 
                                                      :view w3p:+html-view+
                                                      :stream stream))
                               (t (format stream "Cached resource ~A not found." 
                                          (car url:search-keys)))))
                       (w3p:input-not-of-required-type 
                        () (write-string "Cached resource not found." stream)))
                     (write-back-to-resources stream))
                    (t (let ((uris))
                         (html:with-paragraph (:stream stream)
                           (html:with-rendition (:bold :stream stream)
                             (write-string "Total Cache Size: " stream))
                           (write (cached-resources-count *http-proxy-cache*) :stream stream)
                           (write-string " resources, " stream)
                           (write (cache-size *http-proxy-cache*) :stream stream)
                           (write-string " bytes" stream))
                         (html:with-paragraph (:stream stream)
                           (write-string "These are the resources currently cached by the server." stream))
                         (html:with-paragraph (:stream stream)
                           (map-cached-resources
                            #'(lambda (uri res)
                                (push (cons uri res) uris))
                            *http-proxy-cache*)
                           (setq uris (stable-sort uris  
                                                   #'(lambda (x y)
                                                       (string< (first x) (first y)))))
                           (html:with-enumeration (stream :definition)
                             (loop for item in uris
                                   for res = (cdr item)
                                   do (html:enumerating-item (stream)
                                        (w3p:present res '((cached-resource) :verbose-p nil)
                                                     :view w3p:+html-view+ :stream stream))))))))))
          (html:horizontal-line :stream stream)
          (cl-http-signature stream))))))

(defmethod respond-to-show-representation ((url url:http-search) stream)
  (macrolet ((with-html-response (&body body)
               `(with-successful-response (stream :html :expires (get-universal-time)
                                                  :content-location url)
                  (let ((title "Proxy Server Cached Representation"))
                    (html:with-html-document (:stream stream)
                      (html:with-document-preamble (:stream stream)
                        (html:declare-base-reference url :stream stream)
                        (html:declare-title title :stream stream))
                      (ns1.1:with-document-body (:stream stream :background :white :foreground :black)
                        (html:with-section-heading (title :level 2 :stream stream)
                          ,@body)))))))
    (with-slots (url:search-keys) url
      (handler-case 
        (let* ((resource (w3p:accept-from-string 'cached-resource (car url:search-keys)))
               (id (w3p:accept-from-string 'integer (second url:search-keys)))
               (representation (when resource (get-cached-representation resource id nil))))
          (cond ((and  (= 3 (length url:search-keys)) (string-equal (car (last url:search-keys)) "entity"))
                 (proxy-respond-with-representation *server* representation (server-http-version *server*)))
                (t (with-html-response
                     (cond (representation
                            (w3p:present representation '((cached-representation) :verbose-p t) 
                                         :view w3p:+html-view+
                                         :stream stream))  
                           (t (format stream "Cached representation ~A for resource ~A not found." 
                                      (second url:search-keys) (car url:search-keys))))
                     (write-back-to-resources stream)
                     (html:horizontal-line :stream stream)
                     (cl-http-signature stream)))))
        (w3p:input-not-of-required-type 
         () (with-html-response
              (write-string "Cached representation not found." stream)
              (write-back-to-resources stream)
              (html:horizontal-line :stream stream)
              (cl-http-signature stream)))))))

(define export-proxy-interfaces (&rest export-args &key (context (local-context)) &allow-other-keys)
  "Exports the proxy interfaces.
CONTEXT is the host and port on which to export the interfaces.
EXPORT-ARGS are any valid arguments to export URL, such as security parameters."
  (declare (dynamic-extent export-args))
  (with-local-context (context)
    (apply #'export-url #u"/cl-http/proxy/cached-resource?"
	   :search
	   :response-function #'respond-to-show-resource
	   :expiration '(:no-expiration-header)
	   :keywords '(:cl-http :proxy :documentation)
	   :documentation "Describes proxy constraints."
	   export-args)
    (apply #'export-url #u"/cl-http/proxy/cached-representation?"
	   :search
	   :response-function #'respond-to-show-representation
	   :expiration '(:no-expiration-header)
	   :keywords '(:cl-http :proxy :documentation)
	   :documentation "Describes proxy predicates."
	   export-args)))
