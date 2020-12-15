;;;   -*- Mode: LISP; Package: http; BASE: 10; SYNTAX: ansi-common-lisp -*-

;;; (C) Copyright 1997, Andrew J. Blumberg & John C. Mallery
;;;     All Rights Reserved.
;;;
;;;
;;;------------------------------------------------------------------- 
;;;
;;; Lambdavista exports
;;;

(in-package :http)

(mapc #'(lambda (x)
          (export (intern x :http) :http))
      '("*LAMBDAVISTA-INDEX-ON-EXPORT*" "INDEX-URL" "*LAMBDAVISTA-AUTHENTICATION-REALM*"
        "*LAMBDAVISTA-CAPABILITIES*" "*LAMBDAVISTA-SECURE-SUBNETS*" "*LAMBDAVISTA-SEARCH-URL*"
	"EXPORT-LAMBDAVISTA-SEARCH-PAGE"))

;;;------------------------------------------------------------------- 
;;;
;;; CAPTURE TEXT FOR INDEXATION
;;;

(define-parameter *lambdavista-index-on-export* nil
                  "Controls whether URLs are indexed on export to support site search with LambdaVista.
The value can be a function on (URL) that returns non-null for indexable URLS.")

(defvar *auto-export-original-value* nil
  "Holds the original value of *auto-export* during indexation.")

(define-parameter *lambdavista-authentication-realm* nil
                  "Authentication realm for search page.")

(define-parameter *lambdavista-capabilities* nil
                  "Authentication capabilities for search page.")

(define-parameter *lambdavista-secure-subnets* nil
                  "Secure subnets for search page.")

(define-parameter *lambdavista-search-url* "/cl-http/lambdavista.html"
                  "Holds the location for the search page.")

(define-parameter *lambdavista-site-tag* :site
                  "The tag for site indexed document.")

(define-generic index-url (url export-type)
  (:documentation "Primary method for indexing URLs with LambdaVista."))

(defmethod index-url (url export-type)
  (declare (ignore url export-type)))

(defmethod index-url :around ((url http-url) export-type)
  (handler-case-if (not *debug-server*)
     (progn
       (lambda-ir:set-default-tags *lambdavista-site-tag*)
       (call-next-method url export-type))
    (error
      (error)
      (let ((error-type (type-of error)))
        (report-bug *bug-http-server*
                    (format nil "LambdaVista Indexing Error: ~S" error-type)
                    "~&URL: ~A~&Error: ~S~:[~;~&Error Report: ~:*~A~]"
                    (name-string url) error-type (report-string error))))))

(defmethod index-url ((url http-object) (export-type (eql :text-file)))
  (let ((pathname (url:translated-pathname url)))
    (when pathname
      (lambda-ir:add-url-to-document-universe pathname url))))

(defmethod index-url ((url http-object) (export-type (eql :html-file)))
  (let ((pathname (url:translated-pathname url)))
    (when pathname
      (lambda-ir:add-url-to-document-universe pathname url))))

(defmethod export-url :after ((url http-object) (export-type symbol) &rest args)
  (declare (ignore args))
  (let ((predicate *lambdavista-index-on-export*))
    (when (and predicate
               (or (eql predicate t) (funcall predicate url)))
      (index-url url export-type)))
  url)

(defmethod export-url :around ((url http-path) (export-type symbol) &rest args)
  (declare (ignore args))
  (let ((predicate *lambdavista-index-on-export*))
    (cond ((and predicate
                (or (eql predicate t) (funcall predicate url)))
           (let* ((*auto-export-original-value* (or *auto-export-original-value* *auto-export*))
                  (*auto-export* t))
             (prog1 (call-next-method)
                    (index-url url export-type))))
          (*auto-export-original-value*
           (let ((*auto-export* *auto-export-original-value*))
             (call-next-method)))
          (t (call-next-method)))))

;;;------------------------------------------------------------------- 
;;;
;;; WRITE SEARCH FORM
;;;

(defparameter *lambdavista-search-instructions*
              "Enter a search query consisting of relevant terms.  Terms with a (+) before them
(+cl-http) must be included in the target document.  terms with a (-) before
them must not be included in the returned documents.")

(defun %write-lambdavista-search-form (url stream description &optional default-substring)
  (flet ((write-heading (stream)
           (with-anchor-noted (:reference *cl-http-home-page-url-string* :stream stream)
             (image "/cl-http/icons/power-small.gif" "CL-HTTP" :stream stream
                    :vertical-space 0 :horizontal-space 0 :width 67 :height 30))
           (with-rendition (:bold :stream stream)
             (fast-format stream "LambdaVista"))))
    (with-section-heading (#'write-heading :alignment :left :stream stream)
      (horizontal-line :stream stream)
      (with-paragraph (:stream stream)
        (with-centering (:stream stream)
          (ns4.0:with-table (:border nil :width .85 :background :grey-light-very :cell-spacing 0 :cell-padding 6 :stream stream)
            (with-table-row (:stream stream)
              (with-table-cell (:stream stream)
                (with-font (:size 2 :stream stream)
                  (format stream description)))))))
      (with-fillout-form (:post url :stream stream)
        (with-paragraph (:stream stream)
          (with-rendition (:bold :stream stream)
            (fast-format stream "Find Documents Satisfying the Query"))
          (break-line :stream stream)
          (accept-input 'string "SUBSTRING" :default default-substring :size 50 :stream stream))
        (with-paragraph (:stream stream)
          (with-rendition (:italic :stream stream)
            (fast-format stream "Return to search")))))))

(defmethod perform-search (url stream substring)
  (declare (ignore url))
  (html:with-section-heading ("Search Results" :stream stream)
    (multiple-value-bind (urls unused-words)
        ;; this should return a count and accept an ordering argument (forward
        ;; or backward) and possibly a sort predicate 4/23/97 -- JCMa.
        (lambda-ir::perform-search-constraint lambda-ir::*document-context* lambda-ir::archive-search-constraint substring nil) 
      (cond (unused-words
             (with-paragraph (:stream stream)
               (format stream "The index does not contain these words: ~{~A~^, ~}." (ensure-list unused-words))))
            (urls
             (with-paragraph (:stream stream)
               (format stream "~D document~:P satisfy the search specification: "
                       (length urls))
               (fast-format stream "[~I]."
                            (with-rendition (:italic :stream stream)
                              (write-string substring stream))))
             (with-enumeration (stream :enumerate)
               (loop for url in urls
                     do (enumerating-item (stream)
                          (cond ((typep url 'url)
                                 (with-anchor-noted (:reference url :stream stream)
                                   (write-string (name-string url) stream)))
                                ((and (stringp url)
                                      (scheme-prefixed-url-p url))
                                 (with-anchor-noted (:reference url :stream stream)
                                   (write-string url stream)))
                                (t (with-rendition (:italic :stream stream)
                                     (fast-format stream "The URL ~A is unavailable." url))))))))
            (t (with-paragraph (:stream stream)
                 (format stream "Lamentably, no documents satisfy ~s." substring)))))))

(defmethod respond-to-lambdavista-search ((url http-form) stream query-alist)
  (bind-query-values (substring)
                     (url query-alist)
    (with-successful-response (stream :html :expires (url:expiration-universal-time url)
                                      :cache-control (url:response-cache-control-directives url)
                                      :content-language (languages url))
      (with-html-document (:declare-dtd-version-p t :stream stream)
        (html:with-document-preamble (:stream stream)
          (html:declare-title "LambdaVista" :stream stream)
          (html:declare-base-reference url :stream stream))
        (with-standard-document-body (:stream stream)
          (%write-lambdavista-search-form url stream *lambdavista-search-instructions* substring)
          (horizontal-line :stream stream)
          (perform-search url stream substring)
          (horizontal-line :stream stream)
          (cl-http-signature stream))))))
 
(defmethod compute-lambdavista-search-form ((url url:http-form) stream)
  (with-conditional-get-response (stream :html :expires (url:expiration-universal-time url)
                                         :cache-control (url:response-cache-control-directives url)
                                         :content-language (languages url))
    (with-html-document (:declare-dtd-version-p t :stream stream)
      (with-document-preamble (:stream stream)
        (declare-base-reference url :stream stream)
        (declare-title "LambdaVista" :stream stream))
      (with-standard-document-body (:stream stream)
        (%write-lambdavista-search-form url stream *lambdavista-search-instructions*)
        (horizontal-line :stream stream)
        (cl-http-signature stream)))))

(defun export-lambdavista-search-page (&rest init-args)
  (let ((public-p (not (or *secure-subnets*
                           *lambdavista-secure-subnets*
                           *lambdavista-authentication-realm* *lambdavista-capabilities*))))
    (apply #'export-url #u*lambdavista-search-url*
           :html-computed-form
           :form-function #'compute-lambdavista-search-form
           :response-function #'respond-to-lambdavista-search
           :expiration `(:interval ,(* 15. 60.))
           :authentication-realm *lambdavista-authentication-realm*
           :capabilities *lambdavista-capabilities*
           :public public-p
           :private (not public-p)
           :language :en
           :keywords '(:cl-http :documentation)
           :documentation "A form interface for searching the index of the local site."
           init-args)))
