;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: (w4 :use (future-common-lisp www-utils url http)); -*-

;;; File: w4-parser-defs.lisp
;;; Last edited by smishra on Mon Aug 11 12:48:03 1997

;;; (c) Copyright 1996-97, Sunil Mishra & John C. Mallery.
;;;     All Rights Reserved

(in-package :w4)

;;; This file is to be loaded after the web walker system and the HTML
;;; parser have been loaded. It provides an alternate implementation of
;;; show-web-link-map from the one provided in the web walker that uses the
;;; HTML parser to map out portions of the web. It's advantages are that it
;;; shows a larger set of links from a page, along with some descriptive
;;; text (if possible) of the link. The additional links are from the links
;;; in the head and the specified sources in the images.

;;;----------------------------------------
;;; Testing tag structures
;;; Collecting URL's
;;; Collecting associated text

(defun make-absolute-url (url-string)
  ;; Trim spaces, trim out name reference, intern the url
  (let ((url-buffer (make-array (length url-string) :fill-pointer 0
				:element-type http::*standard-character-type*)))
    ;; (declare (dynamic-extent url-buffer))
    (loop for char across url-string
	  unless (html-parser:html-whitespace-p char)
	    if (char= char #\#)
	      do (return url-string)
	  else do (vector-push char url-buffer))
    (unless (= (fill-pointer url-buffer) 0)
      (multiple-value-bind (http-p relative-p)
	  (url:http-url-string-p url-buffer)
	(cond ((or http-p relative-p)
	       (handler-case-if (not *debug-walker*)
		  (let ((url:*escape-search-urls* nil))
		    (url:intern-url (merge-walker-url url-buffer) :if-does-not-exist :create))
		 (url::host-parsing-error
		  () (record-url-note *activity* (current-url) :bad-url-syntax
				      (merge-walker-url url-buffer))
		  nil)
		 (url::no-parser-for-scheme
		  () (record-url-note *activity* (current-url) :bad-url-syntax
				      (merge-walker-url url-buffer))
		  nil)))
	      (t nil))))))

(eval-when (load compile eval)
(defun open-resource-with-content-test-def (stream-var headers-var activity url major-mime-type minor-mime-type body)
  (let ((headers-var-internal (gensym))
	(status-code-var (gensym))
	(major-var (gensym))
	(minor-var (gensym))
	(plist-var (gensym)))
    `(multiple-value-bind (,headers-var-internal ,status-code-var)
	 (handler-case-if (not *debug-walker*)
	    (get-resource-headers ,activity ,url)
	   (error (err)
		  (record-url-note ,activity ,url :error-getting-headers (report-string err))
		  (values nil 500)))
       (case ,status-code-var
	 (200
	   (destructuring-bind (&optional ,major-var ,minor-var &rest ,plist-var)
	       (get-header :content-type ,headers-var-internal)
	     (cond ((and ,(if (atom major-mime-type)
			      `(eql ,major-var ',major-mime-type)
			      `(member ,major-var ',major-mime-type))
			 ,(if (atom minor-mime-type)
			      `(eql ,minor-var ',minor-mime-type)
			      `(member ,minor-var ',minor-mime-type)))
		    (multiple-value-bind (,stream-var ,headers-var ,status-code-var)
			(get-resource-content ,activity ,url)
		      (when (<= 200 ,status-code-var 300)
			,@body)))
		   (t nil))))
	 (t nil)))))
  
(defun open-resource-def (stream-var headers-var activity url body)
  (let ((status-code-var (gensym)))
    `(multiple-value-bind (,stream-var ,headers-var ,status-code-var)
	 (handler-case-if (not *debug-walker*)
	    (get-resource-content ,activity ,url)
	   (error (err)
		  (record-url-note ,activity ,url :error-getting-headers (report-string err))
		  (values nil 500)))
       (when (<= 200 ,status-code-var 300)
	 ,@body))))
)

(defmacro with-resource-stream ((stream-var headers-var activity url
				&key (major-mime-type t) (minor-mime-type t)) &body body)
  (if (and (eql major-mime-type t) (eql minor-mime-type t))
      (open-resource-def stream-var headers-var activity url body)
      (open-resource-with-content-test-def stream-var headers-var activity url
					   major-mime-type minor-mime-type
					   body)))


;;;-------------------------------------------------------------------
;;;
;;; Parser Definition
;;;

(html-parser:define-html-parser-context href-filter-context ()
  :use-variables url url-list inside-anchor-p
  :on-open-tag (a (setq inside-anchor-p t)  ; We need all the anchor contents
		  (save it))
               (:any (when inside-anchor-p (save it)))
  :on-close-tag ((a img href) ; Figure out what the URL is
		 (let ((href-val (html-parser:attr-val
                                  (if (eq (html-parser:name it) #t"img") "src" "href") it)))
		   (setq url (and href-val (make-absolute-url href-val)))
		   (if (and url (not (member url url-list)))
		       (push url url-list)
		       (setq url nil))))
                ;; Give the URL a description
                (a (setq inside-anchor-p nil)
		   (when url
		     (setf (get-value url :description)
			   (or (html-parser:make-pcdata-string (html-parser:parts it))
			       (html-parser:make-pcdata-string (html-parser:parts it) t)
			       (url:name-string url)))))
		(img (when url
		       (setf (get-value url :description)
			     (or (html-parser:return-if-not-whitespace
                                  (html-parser:attr-val #t"ALT" it))
				 (url:name-string url)))))
		(link (when url
			(setf (get-value url :description)
			      (or (html-parser:return-if-not-whitespace
                                   (html-parser:attr-val #t"TITLE" it))
				  (html-parser:return-if-not-whitespace
                                   (html-parser:attr-val #t"REL" it))
				  (html-parser:return-if-not-whitespace
                                   (html-parser:attr-val #t"REV" it))
				  (url:name-string url)))))
                (html (exit-context url-list))
  :on-pcdata (save it))

(html-parser:define-html-parser href-collector ()
  :transitions (:start (href-filter-context))
               (href-filter-context #'null (href-filter-context))
               (href-filter-context t :end))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defgeneric generate-inferiors-via-parsing (url activity))

(defmethod generate-inferiors-via-parsing ((url url:http-url) (activity activity))
  (with-resource-stream (document *headers* activity url
				  :major-mime-type :text :minor-mime-type :html)
    ;; even telling the walker to continue the walk will now
    ;; have to be up to the handler defined
    (let ((local-context (url:local-context-string url)))
      (unwind-protect
	   (with-local-context (local-context)
	     (loop with constraint-set = (activity-constraint-set activity)
		   for inferior in (href-collector document)
		   when (and (satisfies-context-constraints-p inferior activity)
			     (satisfies-p inferior activity constraint-set))
		     collect inferior))
	(remove-value url :local-context-string)))))


;;;-------------------------------------------------------------------
;;;
;;; Actions & Activities
;;;

(define-action-type
  generate-via-parsing
  (:generator
    :documentation "Primary action for extracting and using parts of a URL resource"
    :class generator-type)
  (action activity url)
  (declare (ignore action))
  (values (generate-inferiors-via-parsing url activity) t))

(define-action-type
  html-show-url-overview-new
  (:standard
    :documentation "An action that overviews the URL in HTML on STREAM.")
  (action activity url stream)
  (declare (ignore action))
  ;; (format stream "~&Walk Depth: ~D  - " (depth))
  (multiple-value-bind (headers status-code)
      (handler-case-if (not *debug-walker*) 
	 (get-resource-headers activity url)
	(error (err)
	       (record-url-note activity url :error-getting-headers (report-string err))
	       (values nil 500)))
    (let ((content-length (get-header :content-length headers))
          (last-modified (get-header :last-modified headers)))
      (html:with-rendition (:bold :stream stream)
	(html:note-anchor (or (get-value url :description) (name-string url))
			  :reference url :stream stream))
      (cond-every
	(content-length (format stream " [~D bytes]" content-length))
	((and status-code (not (<= 200 status-code 299)))
	 (html:break-line :stream stream)
	 (format stream "~&Status: ~D ~A" status-code (get-string-for-status-code status-code t))) 
	(last-modified
	  (html:break-line :stream stream)
	  #+genera
	  (let ((date (get-header :date headers)))
	    (if date
		(format stream "~&Resource Age: ~\\time-interval\\" (- date last-modified))
		(http::write-header :last-modified last-modified stream)))
	  #-genera
	  (http::write-header :last-modified last-modified stream))))))

(defmethod show-web-link-map ((url http-url) &key (operator http:*server-mail-address*)
                              headers-p (depth 3) subdirectories-p hosts
                              minimize-dns-p respect-no-robots-p constraints
                              (stream *standard-output*))
  (let ((title "Web Link Structure")
	(host (url:host-string url))
	(*report-stream* stream))
    (html:with-html-document (:stream stream)
      (html:with-document-preamble (:stream stream)
	(html:declare-title title :stream stream))
      (html:with-document-body (:stream stream)
	(html:with-section-heading (title :stream stream)
	  (write-show-web-link-map-legend
	    url operator depth subdirectories-p minimize-dns-p respect-no-robots-p hosts constraints stream)
	  (http:image-line :stream stream)
	  (html:with-paragraph (:stream stream)
	    (with-activity
	      ("Show-Web-Link-Map"
	       (:operator operator
		:search-method :depth-first
		:url-host-name-resolution (if minimize-dns-p :never :preferred)
		:if-does-not-exist :uninterned)
	       :constraints `(,.(when depth
				  `((depth ,depth)))
			      (no-cycles)
			      ,.(when hosts
				  `((url-referrer-host ,host)))
			      ,.(when subdirectories-p
				  `((url-parent-subsumed-by-directory-path ,(url:path url))))
			      ,.(when respect-no-robots-p
				  (list '(header-robots-allowed)))
			      ,.(when constraints constraints))
	       :actions `((html-with-enumeration
			    ((html-enumerating-item
			       ((html-show-url-overview-new ,stream)
				,.(when headers-p
				    `((html-write-headers ,stream)))
				(html-force-output ,stream)
				(generate-via-parsing))
			       ,stream))
			    ,stream :itemize)))
	      (walk url activity))))
	(http:image-line :stream stream)
	(cl-http-signature stream)))))

