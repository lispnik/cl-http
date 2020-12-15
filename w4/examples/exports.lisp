;;; -*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4 -*-

;;; (C) Copyright 1995, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; EXAMPLES OF W4
;;;
;;;------------------------------------------------------------------- 
;;;
;;; TRACING WEB STRUCTURE
;;;

(in-package :w4)

#+cl-http-documentation-facility
(http:add-module-for-find-documentation "W4")

(export-url #u"/cl-http/find-constraints?"
            :search
            :response-function #'respond-to-find-constraint-types-search
            :expiration `(:time ,(* 15. 60.))
            :keywords '(:cl-http :documentation :walker)
            :documentation "A search URL that finds constraint that control the W4 Constraint-Guided
Web Walker.  The search component is a substring to match.")

(export-url #u"/cl-http/find-actions?"
            :search
            :response-function #'respond-to-find-action-types-search
            :expiration `(:time ,(* 15. 60.))
            :keywords '(:cl-http :documentation :walker)
            :documentation "A search URL that finds predefined actions for the W4 action-Guided
Web Walker.  The search component is a substring to match.")

(export-url #u"/cl-http/show-link-map.html"
            :html-computed-form
            :form-function #'compute-show-web-link-map-form
            :expiration '(:no-expiration-header)
            :response-function #'respond-to-show-link-map
            :keywords '(:cl-http :demo :walker)
            :documentation "Walk a web structure and report an overview of the structure.")

(export-url #u"/cl-http/w4.html"
            :html-computed
            :response-function #'respond-to-w4-overview
            :expiration '(:no-expiration-header)
            :keywords '(:cl-http :demo :walker)
            :documentation "Top-level page for the W4 Constraint-Guided Web Walker.")

(export-url #u"/cl-http/w4-search.html"
            :html-computed-form
            :form-function #'compute-salton-search-form
            :expiration '(:no-expiration-header)
            :response-function #'respond-to-search
            :keywords '(:cl-http :demo)
            :documentation "An example of copmuting the form html on the fly and responding to the resulting submissions.")


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(export-url #u(concatenate 'string *web-archive-base-uri-path* "top.html")
            :computed
            :response-function #'respond-to-show-archives
            :keywords '(:cl-http :web-walker :archive)
            :documentation "Displays the web archives on the server.")

(export-url #u(merge-url *web-archive-base-uri-path* (local-context)) 
            :redirect
            :alternate-urls (merge-url (concatenate 'string *web-archive-base-uri-path* "top.html") (local-context))
            :keywords '(:cl-http :web-walker :archive))

(export-url #u(concatenate 'string *web-archive-base-uri-path* "define-archive.html")
            :html-computed-form
            :form-function #'compute-define-archive
            :response-function #'respond-to-define-archive
            :keywords '(:cl-http :web-walker :archive)
            :documentation "Define a new web archive.")

(export-url #u(concatenate 'string *web-archive-base-uri-path* "build-archive.html")
            :html-computed-form
            :form-function #'compute-build-archive
            :response-function #'respond-to-build-archive
            :keywords '(:cl-http :web-walker :archive)
            :documentation "Add to a web archive.")
