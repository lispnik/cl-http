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
;;; DOCUMENTATION URLS
;;;

(in-package :w4)

(eval-when (compile eval load)
  (unless (fboundp 'http::image-line)

    (defun http::image-line (&key (stream html:*output-stream*) (fresh-line t))
      (when fresh-line
        (fresh-line stream))
      (html:with-paragraph (:stream stream)
        (html:image "/cl-http/icons/line-rain.gif" "----" :stream stream))))
  (export (intern "IMAGE-LINE" :http) :http)
  (unless (member :cl-http-documentation-facility *features*)
    (load "http:examples;documentation"))
  )

#+cl-http-documentation-facility
(http:add-module-for-find-documentation "W4")

(defmethod respond-to-find-constraint-types-search ((url http-search) stream)
  (with-slots (url:search-keys) url
    (with-conditional-get-response (stream :html :expires (url:expiration-universal-time url))
      (let ((spec (and url:search-keys (string-trim '(#\space #\tab #\Return) (first url:search-keys)))))
        (declare (dynamic-extent spec))
        (html-find-constraint-types
          :substring (when (and spec (not (null-string-p spec)))
                       spec)
          :stream stream)))))

(defmethod respond-to-find-action-types-search ((url http-search) stream)
  (with-slots (url:search-keys) url
    (with-conditional-get-response (stream :html :expires (url:expiration-universal-time url))
      (let ((spec (and url:search-keys (string-trim '(#\space #\tab #\Return) (first url:search-keys)))))
        (declare (dynamic-extent spec))
        (html-find-action-types
          :substring (when (and spec (not (null-string-p spec)))
                       spec)
          :stream stream)))))

;;;------------------------------------------------------------------- 
;;;
;;; SHOW THE URL STRUCTURE OF A SITE
;;;

(defmethod create-show-web-link-map-form ((url url:http-form) title description query-alist stream)
  (macrolet ((with-query-environment ((label &key paragraph-p break-line-p) &body body)
               `(flet ((do-it ()
                         (http::with-rendition (:bold :stream stream)
                           (fresh-line stream)
                           (write-string ,label stream)
                           (when ,break-line-p (html:break-line :stream stream))
                           ,@body)))
                  (declare (dynamic-extent #'do-it))
                  (cond (,paragraph-p
                         (html:with-paragraph (:stream stream) (do-it)))
                        (t (do-it))))))
    (with-conditional-get-response (stream :html :expires (url:expiration-universal-time url))
      (html:with-html-document (:stream stream)
        (html:with-document-preamble (:stream stream)
          (html:declare-base-reference url :stream stream)
          (html:declare-title title :stream stream))
        (html:with-standard-document-body (:stream stream)
          (html:with-section-heading (title :stream stream)
            (http:image-line :stream stream)
            (html:with-paragraph (:stream stream)
              (write-string description stream))
            (html:with-fillout-form (:post url :stream stream)
              (dolist (entry query-alist)
                (destructuring-bind ((label &key paragraph-p break-line-p)
                                     input-type query &rest args) entry
                  (with-query-environment
                    (label :paragraph-p paragraph-p :break-line-p break-line-p)
                    (apply #'html:accept-input input-type query :stream stream args))))
              (html:with-verbatim-text (:fresh-line nil :stream stream)
                (write-string "          " stream)
                (html:accept-input 'html:reset-button "Reset" :stream stream)
                (write-string "          " stream)
                (html:accept-input 'html:submit-button "Submit" :stream stream))
              (http:image-line :stream stream)
              (cl-http-signature stream)))))))) 

(defun write-show-web-link-map-legend (url operator depth subdirectories-p minimize-dns-p respect-no-robots-p hosts constraints stream)
  (html:with-paragraph (:stream stream)
    (html:with-enumeration (stream :itemize)
      (html:enumerating-item (stream)
        (write-string "Start URL: " stream)
        (html:note-anchor (name-string url) :reference url :stream stream))
      (when operator
	(html:enumerating-item (stream)
	  (write-string "Operator: " stream)
	  (html:note-anchor operator :reference (concatenate 'string "mailto:" operator) :stream stream)))
      (html:enumerating-item (stream)
        (format stream "Depth: ~:[unlimited~;~:*~D~]" depth))
      (html:enumerating-item (stream)
        (format stream "Only Subdirectories: ~:[no~;yes~]" subdirectories-p))
      (html:enumerating-item (stream)
        (format stream "Minimize DNS: ~:[no~;yes~]" minimize-dns-p))
      (html:enumerating-item (stream)
        (format stream "Respect No Robots: ~:[no~;yes~]" respect-no-robots-p))
      (html:enumerating-item (stream)
        (format stream "Constrain to hosts: ~:[no~;~:*~{~A~^, ~}~]" hosts))
      (html:enumerating-item (stream)
        (write-string "Constraints: " stream)
        (if constraints
            (html:with-emphasis (:code :stream stream)
              (html:with-verbatim-text (:fresh-line nil :width 120 :stream stream)
                (write constraints :stream stream :escape nil :pretty t :base 10.)))
            (write-string "no" stream)))))) 

(define-action-type
  html-show-url-overview
  (:standard
    :documentation "An action that overviews the URL in HTML on STREAM.")
  (action activity url stream)
  (declare (ignore action))
  ;; (format stream "~&Walk Depth: ~D  - " (depth))
  (multiple-value-bind (headers status-code redirection cached-p http-version)
      (handler-case-if (not *debug-walker*) 
         (get-resource-headers activity url)
        (error (err)
               (record-url-note activity url :error-getting-headers (report-string err))
               (values nil 500)))
    (declare (ignore redirection cached-p))
    (let ((content-length (get-header :content-length headers))
          (last-modified (get-header :last-modified headers)))
      (html:with-rendition (:bold :stream stream)
        (html:note-anchor (name-string url) :reference url :stream stream))
      (cond-every
        (content-length (html:fast-format stream " [~D bytes]" content-length))
	(http-version
	  (html:break-line :stream stream)
	  (html:fast-format stream "~&Server Version: ~A" http-version))
        ((and status-code (not (<= 200 status-code 299)))
         (html:break-line :stream stream)
         (html:fast-format stream "~&Status: ~D ~A" status-code (get-string-for-status-code status-code t))) 
        (last-modified
          (html:break-line :stream stream)
          #+Genera
          (let ((date (get-header :date headers)))
            (if date
                (format stream "~&Resource Age: ~\\time-interval\\" (- date last-modified))
                (http::write-header :last-modified last-modified stream)))
          #-Genera
          (http::write-header :last-modified last-modified stream))))))

(defgeneric show-web-link-map (url &key operator headers-p depth subdirectories-p hosts
                                   minimize-dns-p respect-no-robots-p constraints stream))

(defmethod show-web-link-map ((url http-url) &key (operator http:*server-mail-address*)
                              headers-p (depth 3) subdirectories-p hosts
                              minimize-dns-p respect-no-robots-p constraints
                              (stream *standard-output*))
  (flet ((sort-inferiors (x y)
           (string< (url:name-string x) (url:name-string y))))
    (declare (dynamic-extent #'sort-inferiors))
    (let ((title "Web Link Structure")
          (host (url:host-string url))
          (*report-stream* stream))
      (html:with-html-document (:stream stream)
        (html:with-document-preamble (:stream stream)
          (html:declare-title title :stream stream))
        (html:with-standard-document-body (:stream stream)
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
                                    `((url-host ,host)))
                                ,.(when subdirectories-p
                                    `((url-parent-subsumed-by-directory-path ,(url:path url))))
                                ,.(when respect-no-robots-p
                                    (list '(header-robots-allowed)))
                                ,.(when constraints constraints))
                 :actions `((html-with-enumeration
                              ((html-enumerating-item
                                 ((html-show-url-overview ,stream)
                                  ,.(when headers-p
                                      `((html-write-headers ,stream)))
                                  (html-force-output ,stream)
                                  (generate-sorted-inferiors ,#'sort-inferiors))
                                 ,stream))
                              ,stream :itemize)))
                (walk url activity))))
          (http:image-line :stream stream)
          (cl-http-signature stream)))))) 

(defmethod compute-show-web-link-map-form ((url url:http-form) stream)
  (create-show-web-link-map-form
    url
    "Show Web Link Structure"
    "Traverse a Web structure displaying the link structure."
    '((("URL [starting point]: " :paragraph-p t :break-line-p t)
       html:string "ROOT-URL" :size 80)
      (("Operator [Email Address]: " :paragraph-p t :break-line-p t)
       html:string "OPERATOR" :size 30)
      (("Show Headers [display header information]:" :paragraph-p t)
       html:radio-button "HEADERS-P"
       :choices (("Yes" . "Yes") ("No" . "No")) :default "No" :linebreaks nil)
      (("Depth [maximum steps explored]: " :paragraph-p t)
       html:string "DEPTH" :size 5 :default "2")
      (("Origin Host Only [explore no other servers]:" :paragraph-p t)
       html:radio-button "ORIGIN-HOST-ONLY-P" :choices (("Yes" . "Yes") ("No" . "No"))
       :default "Yes" :linebreaks nil)
      (("Only Subdirectories:" :paragraph-p t)
       html:radio-button "SUBDIRECTORIES-P"
       :choices (("Yes" . "Yes") ("No" . "No")) :default "No"  :linebreaks nil)
      (("Hosts [limit walk to servers]: " :paragraph-p t  :break-line-p t)
       html:string "HOSTS" :size 80)
      (("Minimize DNS [non-canonical host names]: " :paragraph-p t)
       html:radio-button "MINIMIZE-DNS-P"
       :choices (("Yes" . "Yes") ("No" . "No")) :default "Yes" :linebreaks nil)
      (("Respect No Robots:" :paragraph-p t)
       html:radio-button "RESPECT-NO-ROBOTS-P"
       :choices (("Yes" . "Yes") ("No" . "No")) :default "Yes" :linebreaks nil )
      (("Constraints [specify a list of <a href=\"/cl-http/find-constraints?\">constraints</a>]:"
        :paragraph-p t :break-line-p t)
       html:multi-line-text "CONSTRAINTS"))
    stream))

(declaim (type function listify-string))

(defmethod respond-to-show-link-map ((url http-form) stream query-alist)
  (bind-query-values (root-url operator headers-p depth subdirectories-p origin-host-only-p
                               minimize-dns-p hosts respect-no-robots-p constraints)
                     (url query-alist)
    (let ((start-url (intern-url root-url))
          (operator (and operator (string-trim '(#\space #\tab) operator)))
          (show-headers-p (equalp headers-p "yes"))
          (max-depth (when depth
                       (unless (null-string-p (setq depth (string-trim '(#\space #\tab) depth)))
                         (parse-integer depth))))
          (subdirs-p (equalp subdirectories-p "yes"))
          (dns-p (equalp minimize-dns-p "yes"))
          (host-list (listify-string hosts))
          (robots-p (equalp respect-no-robots-p "yes"))
          (constraint-list (read-constraints-from-string constraints)))
      (when (equalp origin-host-only-p "yes")
	(setf host-list (list (url:host-string start-url))))
      (with-successful-response (stream :html :expires (url:expiration-universal-time url))
        (show-web-link-map
          start-url
          :operator operator
          :headers-p show-headers-p
          :depth max-depth
          :subdirectories-p subdirs-p
          :hosts host-list
          :minimize-dns-p dns-p
          :respect-no-robots-p robots-p 
          :constraints constraint-list
          :stream stream)))))

#|(show-web-link-map (intern-url "http://www.ai.mit.edu/") :depth 3 :stream *standard-output*)|#

;;;------------------------------------------------------------------- 
;;;
;;; 
;;;


(eval-when (compile eval load)
   
  (defun make-enumeration-code (stream enumerations &optional (enumeration-style :itemize))
    `(html:with-enumeration (,stream ,enumeration-style)
       ,.(loop for entry in enumerations
               collect (destructuring-bind (anchor-text reference &key description icon-url)
                           entry
                         `(html:with-paragraph (:stream ,stream)
                            (html:enumerating-item (,stream :icon-url ,icon-url)
                              (html:with-rendition (:bold :stream ,stream)
                                (html:note-anchor ,anchor-text :reference ,reference :stream ,stream)
                                (write-char #\: ,stream))
                              (write-char #\space ,stream)
                              (write-string ,description ,stream)))))))
   
  )

(defmacro make-enumeration-page ((url stream) &key title description enumerations (enumeration-style :itemize))
  `(with-conditional-get-response (stream :html :expires (url:expiration-universal-time ,url))
     (html:with-html-document (:stream ,stream)
       (html:with-document-preamble (:stream ,stream)
         (html:declare-base-reference url :stream ,stream)
         (html:declare-title ,title :stream ,stream))
       (html:with-standard-document-body (:stream ,stream)
         (html:with-section-heading (,title :stream ,stream)
           (http:image-line :stream ,stream)
           ,@(when description
               `((html:with-paragraph (:stream ,stream)
                   ,(etypecase description
                      (string `(write-string ,description ,stream))
                      (cons `(funcall ,description url ,stream))))))
           ,@(when enumerations
               (list (make-enumeration-code stream enumerations enumeration-style)))
           (http:image-line :stream ,stream)
           (cl-http-signature ,stream))))))


;;;------------------------------------------------------------------- 
;;;
;;; DEFINE THE TOP-LEVEL PAGE
;;;

(declaim (special *web-archive-base-uri-path*))

(defmethod respond-to-w4-overview ((url http-computed-url) stream)
  (make-enumeration-page
    (url stream)
    :title "W4 Constraint-Guided Web Walker"
    :description "This page centralizes information about the <a href=\"/cl-http/w4/w4.html\">W4 Constraint-Guided Web Walker</a>."
    :enumerations
    (("Constraints" "/cl-http/find-constraints?"
      :description "These are the constraints currently defined.")
     ("Actions" "/cl-http/find-actions?"
      :description "These are the actions currently defined.")
     ("Link Map" "/cl-http/show-link-map.html"
      :description "This facility allow you to walker and view the
structure of a Web starting from a root url.")
     ("Search" "/cl-http/w4-search.html"
      :description "This allows you to search for a set of keywords
words in Web pages denoted by a series of constraints.")
     ("Archive" (concatenate 'string *web-archive-base-uri-path* "build-archive.html")
      :description "Walk a region of the Web guided by constraints and
archive it locally."))))
