;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-user -*-

;;; Copyright John C. Mallery,  1995-1997.
;;; All rights reserved.


;;;------------------------------------------------------------------- 
;;;
;;; EXAMPLES OF EXPORTING URLs
;;;
;;; The documentation page /cl-http/response-functions.html explains how
;;; to write response functions like the ones in this file.  The main
;;; documentation page /cl-http/cl-http.html also points additional
;;; information like the HTML generation tools that will be helpful when
;;; writing response functions.

(in-package :http-user)

;; Both of these files contain examples of writing response functions and
;; getting the server to do interesting things.

(eval-when (load eval compile)
  ;; load the self documentation utilities
  (load "http:examples;documentation" :verbose nil)
  ;; load the authentication interface
  (load "http:examples;access-control" :verbose nil)
  ;; load the self documentation utilities for W3P
  #+W3P
  (load "http:w3p;documentation" :verbose nil)
  ;; Load the remote log window
  (load "http:examples;log-window" :verbose nil))

;; Load the VRML examples
#+VRML1.0
(load "http:examples;vrml;vrml" :verbose nil)

;; Load Java directory example.
(load "http:examples;twistdown-tree;twistdown" :verbose nil)

;; Start the server by evaluating the form: (enable-http-service)

;; Note that #U is a reader macro that completes a partial URL according
;; to the local context in which is it evaluated, see HTTP:MERGE-URL

;; Export CL-HTTP Documentation
(export-url #u"/cl-http/"
            :directory
            :recursive-p t
            :pathname "http:www;cl-http;"
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation))

(export-url #u"/cl-http/release-notes.text"
            :text-file
            :pathname "http:http;-release-notes-.text"
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation :release-notes))

(export-url #u"/cl-http/license.text"
            :text-file
            :pathname "http:http;-license-.text"
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation :license)) 

;; obsolete here for backward compatibility
(export-url #u"/cl-http.html"
            :redirect
            :alternate-urls #u"/cl-http/cl-http.html"
            :keywords '(:cl-http :demo)) 

;;;------------------------------------------------------------------- 
;;;
;;; TOP-LEVEL FRAME
;;;

(defparameter *sections* '(("Overview"
                            ("Top Features" "/cl-http/cl-http.html#features")
                            ("Selected Applications" "http://www.ai.mit.edu/projects/iiip/doc/cl-http/home-page.html#applications")
                            ("Guided Tour" "/cl-http/features.html")
                            ("Home Page"  "http://www.ai.mit.edu/projects/iiip/doc/cl-http/home-page.html")
                            ("FTP Site" "ftp://ftp.ai.mit.edu/pub/users/jcma/cl-http/")
                            ("Discussion Group" "http://wilson.ai.mit.edu/cl-http/archives/www-cl.html"))
                           ("Getting Started"
                            ("Configuration" "/cl-http/cl-http.html#configuration")
                            ("Platforms" "/cl-http/cl-http.html#platforms")
                            ("Release Notes, General" "/cl-http/release-notes.text")
                            ("Release Notes, Platform" "/cl-http/cl-http.html#platform-release-notes")
                            ("License" "/cl-http/license.text"))
                           ("Conference Papers"
                            ("WWW-94 Server Architecture" "/projects/iiip/doc/cl-http/server-abstract.html")
                            ("DOW-96 W3P Presentation System" "/cl-http/w3p/dow96/w3p.html")
                            ("DOW-96 W4 Web Walker" "/cl-http/w4/w4.html"))
                           ("Documentation"
                            ("Accepting Input" "/cl-http/show-documentation?ACCEPT-INPUT")
                            ("Class Inheritence Structure" "/cl-http/server-structure/class-structure.html")
                            ("Client-Side Cookies" "/cl-http/cookies.html")
                            ("Computing Responses"  "/cl-http/response-functions.html")
                            ("Developer Tools" "/cl-http/developer-tools.html")
                            ("Documentation" "/cl-http/cl-http.html#documentation")
                            ("Exporting URLs" "/cl-http/show-documentation?HTTP:EXPORT-URL")
                            ("Find Documentation" "/cl-http/find-documentation.html")
                            ("Find URL" "/cl-http/find-url.html")
                            ("HTML Parser" "/cl-http/sources/html-parser/html-parser.html")
                            ("Image Maps" "/cl-http/image-maps/image-maps.html")                                                  
                            ("Presentation System" "/cl-http/w3p/w3p.html") 
                            ("Security &amp; Authentication" "/cl-http/authentication/authentication.html")
                            ("Server Extensions" "/cl-http/extensions.html")
                            ("Source Code" "/cl-http/cl-http.html#sources")
                            ("Standards" "/cl-http/standards/")
                            ("VRML Generation" "/cl-http/vrml/vrml.html"))
                           ("Other"
                            ("Acknowledgments" "/cl-http/acknowledgments.html")
                            ("Brief History" "/cl-http/history.html")
                            ("Bug Reports" "/cl-http/cl-http.html#bugs")
                            ("Contribution Guidelines" "/cl-http/guidelines.html")
                            ("Development Projects" "/cl-http/projects.html")
                            ("References" "/cl-http/reference.html"))))

(defmethod write-cl-http-index-pane ((url http-url) stream)
  (flet ((write-heading (stream)
           (with-centering (:stream stream)
             (write-string "Contents" stream)))
         (index (count)
           (flet ((get-letter (idx)
                    (aref "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (1- idx))))
             (cond ((> count 26)
                    (multiple-value-bind (quotient remainder)
                        (truncate count 26)
                      (coerce (list (get-letter quotient) (get-letter remainder)) 'string)))
                   (t (get-letter count))))))
    (http:with-conditional-get-response (stream :html :expires (url:expiration-universal-time url)
                                                :cache-control (url:response-cache-control-directives url)
                                                :content-language (url:languages url))
      (with-html-document (:declare-dtd-version-p t :stream stream)
        (with-document-preamble (:stream stream)
          (declare-title "Contents" :stream stream))
        (with-standard-document-body (:stream stream)
          (with-section-heading (#'write-heading :stream stream :level 2) 
            (loop for (heading . entries) in *sections*
                  for count upfrom 1
                  do (flet ((write-subheading (stream)
                              (fast-format stream "~D. ~A" (index count) heading)))
                       (declare (dynamic-extent #'write-subheading))
                       (with-section-heading (#'write-subheading :stream stream)
                         (with-enumeration (stream :definition)
                           (with-font (:size 2 :stream stream)
                             (loop for (display-string  reference) in entries
                                   for count upfrom 1
                                   do (enumerating-item (stream)
                                        (with-rendition (:bold :stream stream)
                                          (fast-format stream "~D. " count ) 
                                          (ns4.0:note-anchor Display-string :reference reference
                                                             :target "display-pane" :stream stream)))))))))))))))

(export-url #u"/cl-http/frame-index.html"
            :computed
            :response-function #'write-cl-http-index-pane
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords `(:cl-http :documentation))

(defmethod write-cl-http-title-pane ((url http-url) stream)
  (http:with-conditional-get-response (stream :html :expires (url:expiration-universal-time url)
                                              :cache-control (url:response-cache-control-directives url)
                                              :content-language (url:languages url))
    (with-html-document (:declare-dtd-version-p t :stream stream)
      (with-standard-document-body (:stream stream)
        (with-centering (:stream stream)
          (image "/cl-http/icons/cl-http.gif" "Common Lisp Hypermedia Server" :stream stream
                 :width 425 :height 26 :alignment :texttop))))))

(export-url #u"/cl-http/frame-title.html"
            :computed
            :response-function #'write-cl-http-title-pane
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords `(:cl-http :documentation))

(defmethod write-cl-http-frame-set ((url http-url) stream)
  (http:with-conditional-get-response (stream :html
                                              :expires (url:expiration-universal-time url)
                                              :cache-control (url:response-cache-control-directives url)
                                              :content-location url
                                              :content-language (url:languages url))
    (with-html-document (:stream stream)
      (with-document-preamble (:stream stream)
        (declare-title "Common Lisp Hypermedia Server (CL-HTTP)" :stream stream))
      (ns4.0:with-document-frameset (:rows '((:pixel 45) :wild) :stream stream)
        (ns4.0:note-document-frame :name "title-pane" :reference #u"/cl-http/frame-title.html"
                                   :target "display-pane" :resizable-p t :scrolling nil :stream stream)
        (ns4.0:with-document-frameset (:columns '((:percentage 25) :wild) :stream stream)
          (ns4.0:note-document-frame :name "index-pane" :reference #u"/cl-http/frame-index.html"
                                     :target "display-pane" :resizable-p t :stream stream)
          (ns4.0:note-document-frame :name "display-pane" :reference #u"/cl-http/cl-http.html#features" 
                                     :target "display-pane" :resizable-p t :stream stream))))))

(export-url #u"/cl-http/frame.html"
            :computed
            :response-function #'write-cl-http-frame-set
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords `(:cl-http :documentation))

(defmethod redirect-to-documentation-root ((url http-url) stream)
  (declare (ignore stream))
  (multiple-value-bind (user-agent version)
      (current-user-agent)
    (redirect-request *server*
                      (if (user-agent-capability-p :frames user-agent version)
                          #u"/cl-http/frame.html"
                          #u"/cl-http/cl-http.html"))))

;; redirect top-level requests to the documentation.
(export-url #u"/"
            :computed
            :response-function #'redirect-to-documentation-root
            :keywords '(:cl-http :demo))

;; provide a URL in the documentation directory in case the server root is
;; otherwise engaged.
(export-url #u"/cl-http/docs.html"
            :computed
            :response-function #'redirect-to-documentation-root
            :keywords '(:cl-http :demo))

;;;------------------------------------------------------------------- 
;;;
;;; MULTIMEDIA EXPORTS 
;;;

(export-url #u"/cl-http/yosemite.gif"
            :gif-image
            :pathname "http:www;cl-http;examples;yosemite-valley.gif"
            :expiration `(:interval ,(* 24. 60. 60.))
            :public t
            :keywords '(:cl-http :demo))

(export-url #u"/cl-http/yosemite-large.gif"
            :gif-image
            :pathname "http:www;cl-http;examples;yosemite-valley-l.gif"
            :expiration `(:interval ,(* 24. 60. 60.))
            :public t
            :keywords '(:cl-http :demo))

(export-url #u"/cl-http/audio.au"
            :basic-audio
            :pathname "http:www;cl-http;examples;us-vp2.au"
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :demo))

(export-url #u"/cl-http/video.mpeg"
            :mpeg-video
            :pathname "http:www;cl-http;examples;us-radar.mpg"
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :keywords '(:cl-http :demo)) 

;;;------------------------------------------------------------------- 
;;;
;;;  DIRECTORY EXPORTS
;;;

(export-url #u"/cl-http/sources/examples/"
            :lisp-directory
            :pathname "http:examples;*.lisp"
            :recursive-p t
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "Example Lisp files showing configuration of the server and export of URLs.")

#+(or mcl mit-site)
(export-url #u"/cl-http/sources/mcl/examples/"
            :lisp-directory
            :pathname "http:mcl;examples;*.lisp"
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "Example Lisp files for the MAC that show configuration of the server and export of URLs.")

;; export the directory containing standards relevant to the world wide web.
(export-url #u"/cl-http/standards/"
            :text-directory
            :pathname "http:standards;*.text"
            :recursive-p t
            :expiration `(:interval ,(* 182. 24. 60. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "The set of standards documents on which the World Wide Web is based.
Read the ones on HTTP and HTML if you want to learn more about this server and be able to extend it.")

;; export portable common lisp sources.
(export-url #u"/cl-http/sources/common-lisp/"
            :lisp-directory
            :pathname "http:server;*.lisp"
            :expiration `(:interval ,(* 24. 60. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "The portable Lisp source for CL-HTTP.")

;; Export portable CLIM interface code
(export-url #u"/cl-http/sources/clim/"
            :lisp-directory
            :pathname "http:clim;*.lisp"
            :recursive-p t
            :expiration `(:interval ,(* 24. 60. 60.))
            :public t
            :language :en
            :keywords '(:w3p :cl-http :documentation)
            :documentation "The CLIM interface source for CL-HTTP.")

;; Export the client code.
(export-url #u"/cl-http/sources/client/"
            :lisp-directory
            :pathname "http:client;*.lisp"
            :expiration `(:interval ,(* 24. 60. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "The portable Lisp source for the basic HTTP client.")

(export-url #u"/cl-http/sources/proxy/"
            :lisp-directory
            :pathname "http:proxy;*.lisp"
            :expiration `(:interval ,(* 24. 60. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "The portable Lisp source for the HTTP proxy.")

(export-url #u"/cl-http/sources/smtp/"
            :lisp-directory
            :pathname "http:smtp;*.lisp"
            :expiration `(:interval ,(* 24. 60. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "The portable Lisp source for sending mail via SMTP.")

;; Export W4 Constraint Guided Web Walker
(export-url #u"/cl-http/sources/w4/"
            :lisp-directory
            :pathname "http:w4;*.lisp"
            :expiration `(:interval ,(* 24. 60. 60.))
            :public t
            :language :en
            :keywords '(:w4 :cl-http :documentation)
            :documentation "The W4 Constraint-Guided Web Walker for CL-HTTP.")

;; Export W3 Presentation System
(export-url #u"/cl-http/sources/w3p/"
            :lisp-directory
            :pathname "http:w3p;*.lisp"
            :expiration `(:interval ,(* 24. 60. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "The W3p Presentation System for CL-HTTP.")

;; Export HTML Parser
(export-url #u"/cl-http/sources/html-parser/"
            :directory
            :pathname "http:html-parser;v8;*.lisp"
            :expiration `(:interval ,(* 24. 60. 60.))
            :public t
            :language :en
            :keywords '(:html-parser :cl-http :documentation)
            :documentation "An HTML parser for W4.")

;; Export the LambdaVista System
(export-url #u"/cl-http/sources/lambda-ir/"
            :lisp-directory
            :pathname "http:lambda-ir;*.lisp"
            :recursive-p t
            :expiration `(:interval ,(* 24. 60. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "The LambdaVista automatic site indexing for CL-HTTP.") 

;; export MAC-specific sources.
(export-url #u"/cl-http/sources/mcl/"
            :lisp-directory
            :pathname "http:mcl;*.lisp"
            :expiration `(:interval ,(* 24. 60. 60.))
            :public t
            :recursive-p t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "The platform-specific sources that allow CL-HTTP to run on the MAC.")

;; export Lisp Machine specific sources.
(export-url #u"/cl-http/sources/lispm/"
            :lisp-directory
            :pathname "http:lispm;*.lisp"
            :recursive-p t
            :expiration `(:interval ,(* 24. 60. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "The platform-specific sources that allow CL-HTTP to run on the Lisp Machine.")

(export-url #u"/cl-http/contributions/"
            :directory
            :pathname "http:contrib;"
            :recursive-p t
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :contributions)
            :documentation "Contributions containing code deemed generally useful for CL-HTTP hackers.")

(export-url #u"/cl-http/contributions/mcl/"
            :directory
            :pathname "http:mcl;contrib;"
            :recursive-p t
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :mac :contributions)
            :documentation "Contributions containing code deemed generally useful for Apple Macintosh CL-HTTP hackers.")

(export-url #u"/cl-http/contributions/acl/"
            :directory
            :pathname "http:acl;contrib;"
            :recursive-p t
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :acl :contributions)
            :documentation "Contributions containing code deemed generally useful for Franz Allegro CL-HTTP hackers.")

;;;------------------------------------------------------------------- 
;;;
;;; COMPUTING SOME RESULTS 
;;;

(defun write-headers-as-html (&optional (stream *output-stream*))
  (labels ((write-header (header header-object)
             (let ((value (http:header-value header-object)))
               (enumerating-item (stream)
                 (with-rendition (:bold :stream stream)
                   (write header :stream stream))
                 (write-string-quoting-specials " => " stream)
                 (write-header-value value stream))))
           (write-header-value (value stream)
             (typecase value
               (cons (with-enumeration (stream :itemize)
                       (dolist (val value)
                         (enumerating-item (stream)
                           (write val :base 10. :stream stream)))))
               (t (write value :stream stream)))))
    (declare (dynamic-extent #'write-header #'write-header-value))
    (with-enumeration (stream :enumerate)
      (map-headers #'write-header))))

(defmethod compute-headers-page ((url url:http-computed-url) stream)
  (with-successful-response (stream :html :expires (url:expiration-universal-time url)
                                    :cache-control (url:response-cache-control-directives url)
                                    :content-location url :content-language (languages url))
    (let ((title (format nil "Client Headers for ~A (~A)" (server-host *server*) (server-http-version *server*))))
      (with-html-document (:declare-dtd-version-p t :stream stream)
        (with-document-preamble (:stream stream)
          (declare-base-reference url :stream stream)
          (declare-title title :stream stream))
        (with-standard-document-body (:stream stream)
          (with-section-heading (title :stream stream)
            (http::image-line :stream stream)
            (write-headers-as-html stream)
            (with-paragraph (:stream stream)
              (fast-format  stream "Get the headers again?  ~I" (note-anchor "Yes" :reference url :stream stream)))
            (http::image-line :stream stream)
            (cl-http-signature stream)))))))

(export-url #u"/cl-http/headers.html"
            :computed
            :response-function #'compute-headers-page
            :expiration '(:no-expiration-header)
            :public t
            :no-cache t
            :language :en
            :keywords '(:cl-http :demo)
            :documentation "Shows the headers sent by the client to the server.") 

;;;------------------------------------------------------------------- 
;;;
;;;  COMPUTED FORM WHERE THE FORM IS DETERMINED DYNAMICALLY
;;;

(defparameter *computed-choices* '("Computed Form"))

(defparameter *default-computed-choices* '("Not Selected"))

(defmethod compute-form ((url url:http-form) stream)
  (with-successful-response (stream :html :content-location url
                                    :expires (url:expiration-universal-time url)
                                    :cache-control (url:response-cache-control-directives url)
                                    :content-language (languages url))
    (with-html-document (:declare-dtd-version-p t :stream stream)
      (with-document-preamble (:stream stream)
        (declare-base-reference url :stream stream)
        (declare-title "Computed Form Example" :stream stream))
      (with-standard-document-body (:stream stream)
        (with-section-heading ("Computed Form Example" :stream stream)
          (http::image-line :stream stream)
          (with-fillout-form (:post url :stream stream)
            (with-paragraph (:stream stream)
              (with-rendition (:bold :stream stream)
                (fresh-line stream)
                (write-string "Choices: " stream))
              (let ((choices `("Not Selected" ,@*computed-choices*)))
                (declare (dynamic-extent choices))
                (accept-input 'select-choices "CHOICES" :choices choices
                              :default *default-computed-choices* :sequence-p t :stream stream)))
            (when (< (length *computed-choices*) 5)
              (with-paragraph (:stream stream)
                (with-rendition (:bold :stream stream)
                  (fresh-line stream)
                  (write-string "Add Choice: " stream))
                (accept-input 'string "ADD-CHOICE" :size 30 :stream stream)))
            (when (cdr *computed-choices*)
              (with-paragraph (:stream stream)
                (with-rendition (:bold :stream stream)
                  (fresh-line stream)
                  (write-string "Delete Choice: " stream))
                (accept-input 'string "DELETE-CHOICE" :size 30 :stream stream)))
            ;; Write a hidden field to carry the state and avoid collisions across threads.
            (accept-input 'hidden "COMPUTED-CHOICES"
                          :default (write-to-armor-plated-string *computed-choices*) :stream stream)
            (submit-and-reset-buttons stream))
          (http::image-line :stream stream)
          (cl-http-signature stream))))))

(defmethod respond-to-computed-form ((url url:http-form) stream query-alist)
  (flet ((clean-up (item)
           (and item                            ; don't let NIL through
                (not (null-string-p (setq item (string-trim '(#\space #\tab #\return #\Linefeed) item))))
                item)))
    (declare (dynamic-extent #'clean-up))
    (bind-query-values (choices add-choice delete-choice computed-choices)
                       (url query-alist)
      (let ((real-choices (delete "Not Selected" (ensure-list choices) :test #'equalp))
            (*computed-choices* (read-from-armor-plated-string computed-choices)))
        (setq *default-computed-choices* (if real-choices real-choices '("Not Selected")))
        (cond-every
          ((setq add-choice (clean-up add-choice))
           (pushnew add-choice (cdr *computed-choices*) :test #'equalp))
          ((setq delete-choice (clean-up delete-choice))
           ;; Don't allow deletion of no selection and keep at least two choices.
           (when (cdr *computed-choices*)
             (setq *computed-choices* (delete delete-choice *computed-choices* :test #'equalp)))
           ;; Keep the default in sync
           (setq *default-computed-choices* (or (delete delete-choice *default-computed-choices* :test #'equalp)
                                                '("Not Selected")))))
        ;; generate another version of the form with the new values.
        (compute-form url stream)))))

(export-url #u"/cl-http/computed-form.html"
            :html-computed-form
            :form-function #'compute-form
            :expiration '(:no-expiration-header)
            :response-function #'respond-to-computed-form
            :public t
            :language :en
            :keywords '(:cl-http :demo)
            :documentation "An example of copmuting the form html on the fly and responding to the resulting submissions.")

;;;------------------------------------------------------------------- 
;;;
;;;  EXPORTING A FEW ICONS TO SPRUCE UP WEB PAGES
;;;

(export-url #u"/cl-http/icons/"
            :image-directory
            :pathname "http:www;cl-http;icons;"
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :keywords '(:cl-http :demo)
            :documentation "A directory of icons distributed with the server.")

;;  Write a specialized directory index for icon directories
(defun write-image-directory-index (base-url stream &aux path-url)
  (flet ((intern-path-url (path url)
           (setq path-url (url:intern-pathname-as-url-inferior path url :if-does-not-exist :create)))
         (anchor-text (url pathname directory-file-p)
           (unless directory-file-p
             (let ((name (url:object url))
                   (type (pathname-type pathname))
                   (version (pathname-version pathname)))
               (with-output-to-string (string)
                 (image path-url "o" :alignment :middle :stream string)
                 (write-char #\space string)
                 (write-string name string)
                 (write-char  #\. string)
                 (write-string type string)
                 (unless (or (keywordp version) (null version))
                   (write-char  #\. string)
                   (write version :base 10. :escape nil :stream string))))))
         (directory-title (url)
           (declare (ignore url))
           "Index of Icons" ))
    (declare (dynamic-extent #'intern-path-url #'anchor-text #'directory-title))
    (http::%write-directory-listing-html2 base-url stream #'url:pathname-image-file-p #'intern-path-url
                                          #'anchor-text #'directory-title t :definition))) 

(defmethod compute-icons-index ((url url:http-computed-url) stream)
  (with-conditional-get-response (stream :html :expires (url:expiration-universal-time url)
                                         :cache-control (url:response-cache-control-directives url)
                                         :content-language (languages url))
    (write-image-directory-index (intern-url  #u"/cl-http/icons/" :if-does-not-exist :error) stream)))

(export-url #u"/cl-http/icons/index.html"
            :computed
            :response-function #'compute-icons-index
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :keywords '(:cl-http :demo)
            :documentation "An index of the icon directory which is computed on the fly.") 

;;;------------------------------------------------------------------- 
;;;
;;; IMAGE MAPS
;;;

(export-url #u"/cl-http/image-maps/cern-shapes.gif?"
            :image-map
            :export-type :gif-image
            :pathname "http:www;cl-http;image-maps;cern-shapes.gif"
            :map-format :cern
            :map-pathname (pathname "http:www;cl-http;image-maps;cern-shapes.cern-map")
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :demo)
            :documentation "An example of a cern image-map.")

(export-url #u"/cl-http/image-maps/ncsa-shapes.gif?"
            :image-map
            :export-type :gif-image
            :pathname "http:www;cl-http;image-maps;ncsa-shapes.gif"
            :map-format :ncsa
            :map-pathname "http:www;cl-http;image-maps;ncsa-shapes.ncsa-map"
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :demo)
            :documentation "An example of an ncsa image-map.")

(export-url #u"/cl-http/image-maps/cern-shapes.cern-map"
            :text-file
            :pathname "http:www;cl-http;image-maps;cern-shapes.cern-map"
            :public t)

(export-url #u"/cl-http/image-maps/ncsa-shapes.ncsa-map"
            :text-file
            :pathname "http:www;cl-http;image-maps;ncsa-shapes.ncsa-map"
            :public t)


;;;------------------------------------------------------------------- 
;;;
;;;  DOCUMENTATION OF BASIC CLASSES USING AN IMAGE MAP
;;;

(defmethod respond-to-inspect-node ((url url:http-search) stream)
  (flet ((construct-symbol (string)
           (intern (string-upcase string) :http)))
    (declare (inline construct-symbol))
    (with-slots (url:search-keys) url
      (with-successful-response (stream :html :content-location url
                                        :expires (url:expiration-universal-time url)
                                        :cache-control (url:response-cache-control-directives url)
                                        :content-language (languages url)) 
        (with-html-document (:declare-dtd-version-p t :stream stream)
          (with-document-preamble (:stream stream)
            (declare-base-reference url :stream stream)
            (declare-title "Basic Class Description" :stream stream))
          (with-standard-document-body (:stream stream)
            (let ((node-string (car search-keys)))
              (cond (node-string
                     (with-section-heading ((concatenate 'string "Class: " node-string) :stream stream)
                       (let* ((node-symbol (construct-symbol node-string))
                              (node-class (find-class node-symbol nil)))
                         (cond (node-class
				;; MCL 4.2 lacks ANSI implementation -- JCMa 10/26/1998.
                                (let ((node-documentation #+(or Genera MCL) (documentation (class-name node-class) 'type)
							  #- (or Genera MCL) (documentation node-class 'type)))
                                  (with-rendition (:bold :stream stream) (write-string "Package: " stream))
                                  (write-string (package-name (symbol-package node-symbol)) stream)
                                  (break-line :stream stream)
                                  (with-rendition (:bold :stream stream) (write-string "Documentation: " stream))
                                  (cond (node-documentation (write-string node-documentation stream))
                                        (t (write-string "No documentation string available for this class." stream)))))
                               (t (write-string "Class not found or has not been exported." stream))))))
                    (t (write-string "No search keys given to inspect-node." stream)))
              (horizontal-line :stream stream)
              (cl-http-signature stream))))))))

(export-url #u"/cl-http/server-structure/inspect-node?"
            :search
            :response-function #'respond-to-inspect-node
            :keywords '(:cl-http :documentation)
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :documentation "Describe a node on the basic-classes diagram.")

(export-url #u"/cl-http/server-structure/basic-classes.gif?"
            :image-map
            :export-type :gif-image
            :pathname "http:www;cl-http;server-structure;basic-classes.gif"
            :map-format :cern
            :map-pathname "http:www;cl-http;server-structure;basic-classes.cern-map"
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :language :en
            :keywords '(:cl-http :documentation)
            :documentation "Shows basic cl-http class structure.")


;;;------------------------------------------------------------------- 
;;;
;;;  IMAGE SEARCHES
;;;

(defmethod respond-to-image-search ((url url:http-searchable-object) stream)
  (with-conditional-get-response (stream :html :expires (url:expiration-universal-time url)
                                         :cache-control (url:response-cache-control-directives url)
                                         :content-language (languages url))
    (let ((title "Image Search"))
      (with-image-coordinates (url) 
        (with-html-document (:declare-dtd-version-p t :stream stream)
          (with-document-preamble (:stream stream)
            (declare-title title :stream stream))
          (with-standard-document-body (:stream stream)
            (with-section-heading (title :stream stream)
              (http::image-line :stream stream)
              (with-enumeration (stream :itemize)
                (with-enumeration (stream :itemize)
                  (with-paragraph (:stream stream)
                    (with-rendition (:bold :stream stream)
                      (format stream "~&Mouse click at X=~D, Y=~D" x y)))
                  (with-paragraph (:stream stream)
                    (with-section-heading ("Click To View More Coordinates" :stream stream)
                      (let ((parent (url:search-parent url )))
                        (image parent "[searchable image]" 
                               :accept-coordinates-at-url parent
                               :stream stream))))))
              (http::image-line :stream stream)
              (cl-http-signature stream))))))))

(export-url #u"/cl-http/yosemite-image-map.gif?"
            :gif-image
            :pathname "http:www;cl-http;examples;yosemite-valley.gif"
            :response-function #'respond-to-image-search
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :keywords '(:cl-http :demo)
            :documentation "An example of image search returning the coordinate where the user clicks.") 

;;;------------------------------------------------------------------- 
;;;
;;; CL-HTTP SERVER INTERFACE FOR RESPONSE FUNCTIONS
;;;

(defun write-server-interface-as-html (&optional (stream *output-stream*))
  (labels ((write-server-interface-variable-value (value stream)
             (typecase value
               (cons
                 (with-paragraph (:stream stream)
                   (with-enumeration (stream :itemize)
                     (dolist (val value)
                       (enumerating-item (stream)
                         (write val :base 10. :stream stream))))))
               (t (write-string-quoting-specials (write-to-string value) stream))))
           (write-server-interface (key type form documentation)
             (declare (ignore type))
             (fresh-line stream)
             (with-paragraph (:stream stream)
               (enumerating-item (stream)
                 (with-rendition (:bold :stream stream)
                   (write key :stream stream :escape nil))
                 (write-string-quoting-specials " => " stream)
                 (with-emphasis (:code :stream stream)
                   (write-server-interface-variable-value (eval form) stream))
                 (fresh-line stream)
                 (with-enumeration (stream :plain)
                   (with-paragraph (:stream stream)
                     (with-rendition (:bold :stream stream)
                       (write-string "Value Computer: " stream))
                     (with-emphasis (:code :stream stream)
                       (write form :stream stream))
                     (fresh-line stream)
                     (break-line :stream stream)
                     (with-rendition (:bold :stream stream)
                       (write-string "Documentation: " stream))
                     (write-string documentation stream)))))))
    (declare (dynamic-extent #'write-server-interface #'write-server-interface-variable-value))
    (with-enumeration (stream :itemize)
      (http::map-server-interface #'write-server-interface :types '(:variable :method)))))

(defmethod compute-server-interface-page ((url url:http-computed-url) stream)
  (with-successful-response (stream :html :content-location url
                                    :expires (url:expiration-universal-time url)
                                    :cache-control (url:response-cache-control-directives url)
                                    :content-language (languages url))
    (let ((title (format nil "Server Interface for ~A" (server-host-domain-name *server*))))
      (with-html-document (:declare-dtd-version-p t :stream stream)
        (with-document-preamble (:stream stream)
          (declare-base-reference url :stream stream)
          (declare-title title :stream stream))
        (with-standard-document-body (:stream stream)
          (with-section-heading (title :stream stream)
            (http::image-line :stream stream)
            (write-server-interface-as-html stream)
            (http::image-line :stream stream)
            (cl-http-signature stream)))))))

(export-url #u"/cl-http/server-interface-environment.html"
            :computed
            :response-function #'compute-server-interface-page
            :expiration '(:no-expiration-header)
            :public t
            :language :en
            :keywords '(:cl-http :demo)
            :documentation "Shows the Server Interface available within the macro HTTP:WITH-SERVER-INTERFACE-ENVIRONMENT.")

;;;------------------------------------------------------------------- 
;;;
;;; COMMON GATEWAY INTERFACE FOR RESPONSE FUNCTIONS
;;;

(defun write-cgi-variables-as-html (&optional (stream *output-stream*))
  #.(let ((variables (http::all-cgi-variables *package*)))
      `(with-cgi-environment ,variables
         (labels ((write-cgi-variable-value (value stream)
                    (typecase value
                      (cons
                        (with-paragraph (:stream stream)
                          (with-enumeration (stream :itemize)
                            (dolist (val value)
                              (enumerating-item (stream)
                                (write val :base 10. :stream stream))))))
                      (t (write-string-quoting-specials (write-to-string value) stream))))
                  (write-cgi-variable (key form value documentation stream)
                    (fresh-line stream)
                    (with-paragraph (:stream stream)
                      (enumerating-item (stream)
                        (with-rendition (:bold :stream stream)
                          (write key :stream stream :escape nil))
                        (write-string-quoting-specials " => " stream)
                        (write-cgi-variable-value value stream)
                        (fresh-line stream)
                        (with-enumeration (stream :plain)
                          (with-paragraph (:stream stream)
                            (with-rendition (:bold :stream stream)
                              (write-string "Value Computer: " stream))
                            (write form :stream stream)
                            (fresh-line stream)
                            (break-line :stream stream)
                            (with-rendition (:bold :stream stream)
                              (write-string "Documentation: " stream))
                            (write-string documentation stream)))))))
           (declare (inline write-cgi-variable-value))
           (with-enumeration (stream :itemize)
             (loop for (key form documentation) in http::*cgi-variable-bindings*
                   for value in (list ,@variables)
                   do (write-cgi-variable key form value documentation stream)))))))

(defmethod compute-cgi-variables-page ((url url:http-computed-url) stream)
  (with-successful-response (stream :html :content-location url
                                    :expires (url:expiration-universal-time url)
                                    :cache-control (url:response-cache-control-directives url)
                                    :content-language (languages url))
    (let ((title (format nil "Common Gateway Interface for ~A" (server-host-domain-name *server*))))
      (with-html-document (:declare-dtd-version-p t :stream stream)
        (with-document-preamble (:stream stream)
          (declare-base-reference url :stream stream)
          (declare-title title :stream stream))
        (with-standard-document-body (:stream stream)
          (with-section-heading (title :stream stream)
            (http::image-line :stream stream)
            (write-cgi-variables-as-html stream)
            (http::image-line :stream stream)
            (cl-http-signature stream)))))))

(export-url #u"/cl-http/cgi-environment.html"
            :computed
            :response-function #'compute-cgi-variables-page
            :expiration '(:no-expiration-header)
            :public t
            :language :en
            :keywords '(:cl-http :demo)
            :documentation "Shows the CGI Variables available within the macro HTTP:WITH-CGI-ENVIRONMENT.")


;;;------------------------------------------------------------------- 
;;;
;;; EXPORTS FOR AUTHENTICATION EXAMPLES USING BASIC METHOD
;;;

;; Call DEFINE-ACCESS-CONTROL-REALMS in your configuration.lisp file.
;; ADD-REALM is used here to avoid clobbering existing authentication realms

(add-realm :minimum-security  :basic)
(add-realm :system-admin  :basic)

;; Some groups within realms must be created before references are made to
;; them in user objects.

(add-groups :minimum-security :users :developers)
(add-group :system-admin :system-hackers)

;; Set up a number of users, assigning realms and groups
(add-user "ernest" :minimum-security
          :password "ernest-foo"
          :groups '(:users)
          :personal-name "Ernest Jones"
          :email-address "ernest@foo.com")

(add-user "jim" :minimum-security
          :password "jim-foo"
          :groups '(:developers :users)
          :personal-name "Jim Thompson"
          :email-address "jim@foo.com")

(add-user "frank" :minimum-security
          :password "frank-foo"
          :groups '(:developers :users)
          :personal-name "Frank Smith"
          :email-address "frank@foo.com")

(add-user "luke" :system-admin
          :password "luke-foo"
          :groups '(:system-hackers)
          :personal-name "Luke Miller"
          :email-address "luke@foo.com")

;; Define some capabilities

;; A set of capabilities giving the :developers group basic access
(add-access-control-group :minimum-security-developers
                          :minimum-security
                          :capabilities '((:get :developers)
                                          (:head :developers)
                                          (:post :developers)))
  

;; A set of capabilities allowing anyone named "frank" to do everything but delete or unlink. 
(add-access-control-group :minimum-security-frank-capabilities
                          :minimum-security
                          :capabilities '((:delete)     ;frank can't delete URLs
                                          (:default "frank")))

;; Export some access-controlled urls
(export-url #u"/cl-http/authentication/minimum.html"
            :computed
            :response-function #'http:display-url-authentication-status
            :authentication-realm :minimum-security
            :capabilities nil   ;;no capabilities means anyone in the realm has access
            :private t
            :proxy-revalidate t
            :language :en
            :expiration '(:no-expiration-header)
            :keywords '(:cl-http :authentication :demo))

(export-url #u"/cl-http/authentication/minimum-developers.html"
            :computed
            :response-function #'http:display-url-authentication-status
            :authentication-realm :minimum-security
            ;; the keyword denotes a access-control group created earlier
            :capabilities :minimum-security-developers
            :expiration '(:no-expiration-header)
            :private t
            :proxy-revalidate t
            :language :en
            :keywords '(:cl-http :authentication :demo))

(export-url #u"/cl-http/authentication/minimum-frank.html"
            :computed
            :response-function #'http:display-url-authentication-status
            :authentication-realm :minimum-security
            ;; the keyword denotes a access-control group created earlier
            :capabilities :minimum-security-frank-capabilities
            :expiration '(:no-expiration-header)
            :private t
            :proxy-revalidate t
            :language :en
            :keywords '(:cl-http :authentication :demo))

(export-url #u"/cl-http/authentication/system-hackers.html"
            :computed
            :response-function #'http:display-url-authentication-status
            :authentication-realm :system-admin
            ;; Method capabilties suitable for add-access-control-group are
            ;; passed in directly.
            :capabilities '((:default :system-hackers))  ;;system-hackers do everything
            :expiration '(:no-expiration-header)
            :private t
            :proxy-revalidate t
            :language :en
            :keywords '(:cl-http :authentication :demo))


;;;------------------------------------------------------------------- 
;;;
;;; EXPORTS FOR AUTHENTICATION EXAMPLES USING MD5 DIGEST METHOD
;;;

;; ADD-REALM a new authentication realm using the digest authentication scheme.
(add-realm :digest-realm :digest)

;; The realm must contain groups before user objects refer to them.
(add-groups :digest-realm :members :elite-members)

;; Set up two users, assigning realms and groups.
(add-user "mike" :digest-realm
          :password "mike-foo"
          :groups '(:members)
          :personal-name "Mike Smith"
          :email-address "mike@foo.com")

(add-user "joe" :digest-realm
          :password "joe-foo"
          :groups '(:members :elite-members)
          :personal-name "Joe Doe"
          :email-address "joe@foo.com")

;; Define a set of capabilities giving the :elite-members group basic access
(add-access-control-group :elite-members-access
                          :digest-realm
                          :capabilities '((:get :elite-members)
                                          (:head :elite-members)))

;; Export some access-controlled urls
(export-url #u"/cl-http/authentication/members.html"
            :computed
            :response-function #'http:display-url-authentication-status
            :authentication-realm :digest-realm
            :capabilities nil ;;no capabilities means anyone in the realm has access
            :expiration '(:no-expiration-header)
            :private t
            :proxy-revalidate t
            :language :en
            :keywords '(:cl-http :authentication :demo))

(export-url #u"/cl-http/authentication/elite-members.html"
            :computed
            :response-function #'http:display-url-authentication-status
            :authentication-realm :digest-realm
            :capabilities :elite-members-access
            :expiration '(:no-expiration-header)
            :private t
            :proxy-revalidate t
            :language :en
            :keywords '(:cl-http :authentication :demo)) 

;;;------------------------------------------------------------------- 
;;;
;;; EXAMPLE OF EXPORTING A DIRECTORY ALLOWING THE HTTP PUT METHOD
;;;

#|(defun local-host-subnet-ip-address ()
   (let* ((ip (local-host-ip-address))
             (mask (position #\. ip :from-end t)))
      (concatenate 'string (subseq ip 0 mask) ".0")))

;; See the documentation on security policies in the authentication documentation.
;; The PUT method requires access control to protect you from undesirables.
(export-url #u"/cl-http/users/"
            :directory
            :recursive-p t
            :pathname "http:www;cl-http;users;"
            :expiration `(:interval ,(* 15. 60.))
            :secure-subnets (list (local-host-subnet-ip-address))
            :keywords '(:cl-http :documentation))|# 

;;;------------------------------------------------------------------- 
;;;
;;; NETSCAPE 1.1 EXAMPLES: COMBINING TABLES, FORMS, BACKGROUND COLORS, AND SERVER PUSH
;;;

(defparameter *ccs-built-in-backgrounds-choices* `(:none :random ,.*built-in-backgrounds*))
(defparameter *ccs-built-in-client-colors-choices* `(:none :random ,.*built-in-client-colors*))
(defvar *ccs-randomize-colors* nil)
(defvar *ccs-randomize-background-url* nil)
(defvar *ccs-default-update-interval* 10)
(defparameter *ccs-update-interval-choices* '(0 5 10 15 20 25 30 45 60))

(defmacro ccs-with-item-list ((&rest items) &body body)
  `(let ((items (list ,@items)))
     (declare (dynamic-extent items))
     ,@body))

(declaim (inline ccs-make-writer))

(defun ccs-make-writer (label)
  #'(lambda (stream)
      (with-rendition (:bold :stream stream)
        (write-string label stream))))

(defun ccs-write-row (stream items font-size &optional (horizontal-alignment :center) (vertical-alignment :middle))
  (with-table-row (:stream stream)
    (dolist (item items)
      (with-table-cell (:horizontal-alignment horizontal-alignment
                        :vertical-alignment vertical-alignment
                        :break-lines-p nil
                        :stream stream)
        (with-font (:size font-size :stream stream)
          (typecase item
            (string (write-string item stream))
            (t (funcall item stream))))))))

(defun ccs-write-color-row (stream label color query-name url-p font-size horizontal-alignment vertical-alignment)
  (flet ((default-color (color query-name)
           (cond ((member query-name *ccs-randomize-colors*)
                  :random)
                 ((null color) :none)
                 (t color)))
         (default-background-url (color)
           (cond (*ccs-randomize-background-url* :random)
                 ((null color) :none)
                 (t color))))
    (declare (inline default-color default-background-url))
    (ccs-with-item-list
      ((ccs-make-writer label)
       #'(lambda (stream)
           (if url-p
               (accept-input 'select-choices (symbol-name query-name)
                             :choices *ccs-built-in-backgrounds-choices*
                             :size :pull-down-menu
                             :default (default-background-url color) :stream stream)
               (accept-input 'select-choices (symbol-name query-name)
                             :choices *ccs-built-in-client-colors-choices*
                             :size :pull-down-menu
                             :default (default-color color query-name)
                             :stream stream)))
       #'(lambda (stream) (when color (write color :stream stream)))
       #'(lambda (stream)
           (cond ((null color))
                 (url-p
                  (let ((bg-url (background-url color)))
                    (note-anchor (subseq bg-url (+ 3 (search #."/bg/" bg-url :test #'char=)))
                                 :reference bg-url :stream stream)))
                 (t (write (color-mapping color) :escape nil :stream stream)))))
      (ccs-write-row stream items font-size horizontal-alignment vertical-alignment))))

(defun ccs-make-caption (url)
  (multiple-value-bind (user-agent version)
      (current-user-agent)
    (with-output-to-string (stream)
      (with-rendition (:bold :stream stream)
        (note-anchor "Choose" :reference url :stream stream)
        (write-char #\space stream)
        (note-anchor"CL-HTTP" :reference *cl-http-home-page-url-string* :stream stream)
        (write-string " Colors for " stream)
        (case user-agent
          (:mozilla (note-anchor (string user-agent) :reference "http://www.mcom.com/" :stream stream))
          (t (write user-agent :escape nil :stream stream)))
        (when version
          (write-char #\space stream)
          (write version :escape nil :stream stream))))))

(defun write-choose-color-scheme-form (url stream &optional background-url background
                                           foreground link visited-link active-link
                                           (font-size 4) (border 5))
  (let ((title "Choose CL-HTTP Color Options")
        (caption (ccs-make-caption url))
        (choices-p (or background-url background foreground link visited-link active-link))
        (components `(("Background URL" :background-url ,background-url t)
                      ("Background" :background ,background)
                      ("Foreground" :foreground ,foreground)
                      ("Link" :link ,link)
                      ("Visited Link" :visited-link ,visited-link)
                      ("Active Link" :active-link ,active-link))))
    (declare (dynamic-extent caption components))
    (with-html-document (:stream stream)
      (with-document-preamble (:stream stream)
        (declare-title title :stream stream)
        (declare-default-font :size 4 :stream stream))
      (with-document-body (:background-url background-url :background background
                                           :foreground foreground :link link :visited-link visited-link
                                           :active-link active-link :stream stream)
        (with-centering (:stream stream)
          (with-fillout-form (:post url :stream stream)
            (with-table (:stream stream :border border :cell-padding 2 :cell-spacing border)
              (with-caption (:alignment :top :stream stream)
                (with-font (:size (+ 2 font-size) :stream stream)
                  (write-string caption stream)))
              ;; write the column headings
              (if choices-p
                  (ccs-with-item-list ((ccs-make-writer "Parameter") (ccs-make-writer "Choice")
                                       (ccs-make-writer "Keyword") (ccs-make-writer "Hex"))
                                      (ccs-write-row stream items (1+ font-size)))
                  (ccs-with-item-list ((ccs-make-writer "Parameter") (ccs-make-writer "Choice"))
                                      (ccs-write-row stream items (1+ font-size))))
              ;; write the rows of the table decribing the components.
              (loop for (label query-name color url-p) in components
                    do (ccs-write-color-row stream label color query-name url-p font-size :center :middle))
              ;; show the update interval if some field is randomized
              (when (or *ccs-randomize-colors* *ccs-randomize-background-url*)
                (ccs-with-item-list ((ccs-make-writer "Interval")
                                     #'(lambda (stream)
                                         (with-rendition (:bold :stream stream)
                                           (accept-input 'select-choices "UPDATE-INTERVAL"
                                                         :choices *ccs-update-interval-choices*
                                                         :default *ccs-default-update-interval*
                                                         :size :pull-down-menu :stream stream))))
                                    (ccs-write-row stream items font-size)))
              ;; insert the form submission buttons.
              (ccs-with-item-list ((ccs-make-writer "Action")
                                   #'(lambda (stream)
                                       (with-rendition (:bold :stream stream)
                                         (accept-input 'reset-button "Reset" :stream stream)
                                         (write-string "     " stream)
                                         (accept-input 'submit-button "Submit" :stream stream))))
                                  (ccs-write-row stream items font-size)))))))
    ;; sign the document
    (cl-http-signature stream)))

(defun write-permuting-choose-color-scheme-form (url stream update-interval &optional
                                                     background-url background foreground
                                                     link visited-link active-link)
  (macrolet ((maybe-randomize-colors (&rest items)
               `(cond-every
                  ,.(loop for item in items
                          collect `((member ,(symbolize (string item) *keyword-package*) *ccs-randomize-colors* :test #'eq)
                                    (setq ,item (random-color-keyword)))))))
    ;; This macro establishes the environment for replacing the output on the
    ;; client's display using server push.
    (ns4.0:with-server-push-response (stream)
      ;; with-block demarcates each block of data for the client.
      (loop doing (ns4.0:with-block (stream :force-output t :sleep-interval update-interval :content-type :html
                                            :content-location url)
                                    (write-choose-color-scheme-form url stream background-url background
                                                                    foreground link visited-link active-link))
                  (cond-every
                    (*ccs-randomize-background-url*
                      (setq background-url (random-background-url-keyword)))
                    (*ccs-randomize-colors*
                      (maybe-randomize-colors background foreground link visited-link active-link)))))))

(defmethod write-form-for-choose-color-scheme ((url url:http-form) stream)
  (with-successful-response (stream :html :cache-control (url:response-cache-control-directives url)
                                    :content-location url
                                    :content-language (languages url))
    (write-choose-color-scheme-form url stream)))

(defmethod respond-to-choose-color-scheme ((url url:http-form) stream query-alist)
  (let ((*ccs-randomize-colors* nil)
        (*ccs-randomize-background-url* nil)
        (*ccs-default-update-interval* 15))
    (flet ((keywordize (string var)
             (let ((keyword (symbolize string *keyword-package*)))
               (case keyword
                 ((:none :nil) nil)
                 (:random
                   (case var
                     (:background-url
                       (setq *ccs-randomize-background-url* t)
                       (random-background-url-keyword))
                     (t (push var *ccs-randomize-colors*)
                        (random-color-keyword))))
                 (t keyword))))
           (dynamic-p (interval)
             (and (or *ccs-randomize-colors* *ccs-randomize-background-url*)
                  (not (zerop (if (and interval (find-if #'digit-char-p interval))
                                  (setq *ccs-default-update-interval* (abs (parse-integer interval :junk-allowed t)))
                                  *ccs-default-update-interval*))))))
      (declare (inline dynamic-p))
      (bind-query-values (background-url background foreground link visited-link active-link update-interval)
                         (url query-alist)
        (let ((background-url-key (keywordize background-url :background-url))
              (background-key (keywordize background :background))
              (foreground-key (keywordize foreground :foreground))
              (link-key (keywordize link :link))
              (visited-link-key (keywordize visited-link :visited-link))
              (active-link-key (keywordize active-link :active-link)))
          (if (dynamic-p update-interval)
              (write-permuting-choose-color-scheme-form
                url stream *ccs-default-update-interval* background-url-key background-key
                foreground-key link-key visited-link-key active-link-key)
              (with-successful-response (stream :html :cache-control (url:response-cache-control-directives url)
                                                :content-location url
                                                :content-language (languages url))
                (write-choose-color-scheme-form
                  url stream background-url-key background-key foreground-key
                  link-key visited-link-key active-link-key))))))))

(export-url #u"/cl-http/choose-color-scheme.html"
            :html-computed-form
            :form-function #'write-form-for-choose-color-scheme
            :response-function #'respond-to-choose-color-scheme
            :public t
            :no-cache t
            :language :en
            :documentation "Choose color schemes for Netscape compatible clients."
            :keywords '(:cl-http :demo))

;;;------------------------------------------------------------------- 
;;;
;;; MIXING RED-GREEN-BLUE COLORS
;;;
;;; This example uses the W3P Presentation System to provide input validation rather than 
;;; (re-)writing input validators for every response function.

#+W3P
(w3p:define-presentation-type color-keyword 
                              ()
  :inherit-from 'w3p:member-sequence
  :description "a CL-HTTP color keyword")

;; default presentation method for any view
#+W3P
(w3p:define-presentation-method w3p:present (keyword (type color-keyword) stream view &key)
  (declare (ignore view))
  (etypecase keyword
    (null (write-string "none" stream))
    (keyword (write-string (symbol-name keyword) stream)))
  keyword) 

#+W3P
(w3p:define-presentation-method w3p:accept ((type color-keyword) stream view &key)
  (let ((string (w3p:read-token stream)))
    (unless (null-string-p string)
      (let ((sym (symbolize string http:*keyword-package*)))
        (cond ((eql sym :none) nil)
              ((color-mapping sym nil) sym)
              (t (w3p:handle-input-error string type :stream stream :view view)))))))

#+W3P
(w3p:define-presentation-method w3p:accept-present-default ((type color-keyword) stream (view w3p:html-view) default default-supplied-p 
                                                            present-p query-identifier &key prompt prompt-mode display-default
                                                            insert-default active-p)
  (declare (ignore present-p))
  (w3p:with-presentation-type-parameters (color-keyword type)
    (let ((choices `(:none . ,*built-in-client-colors*)))
      (declare (dynamic-extent choices))
      (w3p:with-standard-html-prompt
        (type :stream stream :default default :default-supplied-p default-supplied-p
              :prompt prompt :prompt-mode prompt-mode :display-default display-default) 
        (cond (active-p 
               (accept-input 'select-choices query-identifier :stream stream :choices choices
                             :sequence-p nil 
                             :default (when insert-default default)
                             :size :pull-down-menu))
              (t (when default-supplied-p
                   (w3p:present default type :stream stream :view w3p:+textual-view+ :acceptably t)
                   default)))))))

#+W3P
(defun write-mix-color-form (url stream &optional background-url background foreground link visited-link active-link)
  (labels ((write-headings (&rest headings)
             (declare (dynamic-extent headings)) 
             (with-table-row (:stream stream)
               (loop for heading in headings
                     do (with-table-cell (:horizontal-alignment :center :stream stream)
                          (with-rendition (:bold :stream stream)
                            (write-string heading stream))))))
           (make-query-identifier (var color)
             (concatenate 'string (substitute #\- #\space (string var)) "-" color))
           (accept-value (query subquery value stream)
             (let ((query-id (make-query-identifier query subquery)))
               (declare (dynamic-extent query-id))
               (with-table-cell (:stream stream)
                 (w3p:accept `(w3p:integer 0 255) :stream stream :view w3p:+html-view+ 
                             :present-p t :default (or value 0) :prompt nil :prompt-mode :raw
                             :query-identifier query-id :insert-default t :active-p t))))
           (accept-color-keyword (name hexadecimal-color stream)
             (let ((color-keyword (color-mapping-keyword hexadecimal-color nil))
                   (query-id (make-query-identifier name "KEYWORD"))
                   (query-id2 (make-query-identifier name "USE-RGB")))
               (declare (dynamic-extent query-id query-id2))
               (with-table-cell (:stream stream)
                 (w3p:accept 'w3p:boolean :stream stream :view w3p:+html-view+ :present-p t :default (null color-keyword)
                             :prompt nil :prompt-mode :raw :insert-default t :query-identifier query-id2))
               (with-table-cell (:stream stream)
                 (w3p:accept 'color-keyword :stream stream :view w3p:+html-view+ :present-p t :default color-keyword 
                             :prompt nil :prompt-mode :raw :insert-default t :query-identifier query-id))))
           (accept-parameter (name hexadecimal-color stream)
             (multiple-value-bind (red green blue)
                 (when hexadecimal-color
                   (decode-color hexadecimal-color))
               (with-table-row (:stream stream)
                 (with-table-cell (:stream stream)
                   (with-rendition (:italic :stream stream)
                     (write-string name stream))) 
                 (with-table-cell (:horizontal-alignment :center :stream stream)
                   (if hexadecimal-color
                       (write-string hexadecimal-color stream)
                       (with-rendition (:italic :stream stream)
                         (write-string "None" stream))))
                 (accept-value name "RED" red stream)
                 (accept-value  name "GREEN" green stream)
                 (accept-value name "BLUE" blue stream)                          
                 (accept-color-keyword name hexadecimal-color stream)))))
    (let ((title "Mix CL-HTTP Colors")
          (caption (ccs-make-caption url)))
      (declare (dynamic-extent caption))
      (with-html-document (:stream stream)
        (with-document-preamble (:stream stream)
          (declare-title title :stream stream))
        (with-document-body (:background-url background-url :background background :foreground foreground
                                             :link link :visited-link visited-link :active-link active-link :stream stream)
          (with-section-heading (caption :stream stream)
            (horizontal-line :stream stream)
            (with-centering (:stream stream)
              (with-fillout-form (:post url :stream stream)
                (ns4.0:with-table (:background (if foreground (inverse-color foreground) :grey-light)
                                   :stream stream :border 2 :cell-padding 2 :cell-spacing 2)
                  (write-headings "Component" "Hexadecimal" "Red" "Green" "Blue" "Use RGB" "Keyword")
                  (accept-parameter "Background" background stream)
                  (accept-parameter "Foreground" foreground stream)
                  (accept-parameter "Link" link stream)
                  (accept-parameter "Visited Link" visited-link stream)
                  (accept-parameter "Active Link" active-link stream) 
                  (with-table-row (:stream stream)
                    (with-table-cell (:horizontal-alignment :center :stream stream)
                      (with-rendition (:bold :stream stream)
                        (write-string "Action" stream)))
                    (with-table-cell (:horizontal-alignment :center :column-span 4 :stream stream)
                      (accept-input 'reset-button "Reset" :stream stream))
                    (with-table-cell (:horizontal-alignment :center  :column-span 2 :stream stream)
                      (accept-input 'submit-button "Submit" :stream stream))))))
            (horizontal-line :stream stream)
            ;; sign the document
            (cl-http-signature stream)))))))

#+W3P
(defmethod write-form-for-mix-colors ((url url:http-form) stream)
  (with-successful-response (stream :html :cache-control (url:response-cache-control-directives url)
                                    :content-location url
                                    :content-language (languages url))
    (write-mix-color-form url stream)))

;; Cheezy approach forces the user to fix each input until he wins.  Fixed in
;; the dynamic forms system.
#+W3P
(define-condition bad-user-input
                  (http::bad-syntax-provided)
  ((reason :initform "User Input Not of Required Type")
   (response :initform nil :initarg :response))
  (:documentation "Signalled when invalid user input is encountered."))

#+W3P
(defun report-invalid-input (url query-id presentation-type raw-value &optional (plural-count 1))
  (cond (raw-value
         (error 'bad-user-input :url url
                :format-string "The value of ~A was ~S, which is not ~A. Please use the back button on your browser and try again."
                :format-args (list query-id raw-value (w3p:describe-presentation-type presentation-type nil plural-count))))
        (t (error 'bad-user-input :url url
                  :format-string "No value was supplied for ~A. Please use the back button on your browser and try again."
                  :format-args (list query-id)))))

#+W3P
(defmethod respond-to-mix-colors ((url url:http-form) stream query-alist)
  (labels ((make-query-identfier (var color)
             (symbolize (concatenate 'string (substitute #\- #\space (string var)) "-" color) *keyword-package*))
           (accept-value (query-id presentation-type query-alist &optional no-error-p)
             (let ((raw-value (second (assoc query-id query-alist :test #'eq))))
               (cond (raw-value
                      (handler-case
                        (w3p:accept-from-string presentation-type raw-value)
                        (w3p:input-not-of-required-type
                          ()
                          (if no-error-p
                              (values nil nil)
                              (report-invalid-input url query-id presentation-type raw-value)))))
                     (no-error-p (values nil nil))
                     (t (report-invalid-input url query-id presentation-type raw-value)))))
           (get-rgb-value (query-id)
             (accept-value query-id '(w3p:integer 0 255) query-alist nil))
           (use-rgb-p (query-id)
             (accept-value query-id '(w3p:boolean) query-alist t))
           (get-color-key (query-id)
             (let ((key (accept-value query-id 'color-keyword query-alist t)))
               (when key
                 (color-mapping key nil))))
           (get-parameter-hex (parameter)
             (let ((color-hex-from-key (get-color-key (make-query-identfier parameter "KEYWORD"))) 
                   (hex-color (encode-color (get-rgb-value (make-query-identfier parameter "RED"))
                                            (get-rgb-value (make-query-identfier parameter "GREEN"))
                                            (get-rgb-value (make-query-identfier parameter "BLUE"))))
                   (use-rgb-p (use-rgb-p (make-query-identfier parameter "USE-RGB"))))
               (cond ((or use-rgb-p (null color-hex-from-key) (equalp hex-color color-hex-from-key))
                      hex-color)
                     (t color-hex-from-key)))))
    (write-mix-color-form url stream
                          nil
                          (get-parameter-hex :background)
                          (get-parameter-hex :foreground)
                          (get-parameter-hex :link)
                          (get-parameter-hex :visited-link)
                          (get-parameter-hex :active-link))))

#+W3P
(export-url #u"/cl-http/mix-colors.html"
            :html-computed-form
            :form-function #'write-form-for-mix-colors
            :response-function #'respond-to-mix-colors
            :public t
            :no-cache t
            :language :en
            :documentation "Mix a new color using RGB."
            :keywords '(:cl-http :demo))


;;;------------------------------------------------------------------- 
;;;
;;; NETSCAPE 2.0 EXAMPLES OF FRAMES AND CLIENT-SIDE IMAGES: client-side image maps and frames
;;;

(defun %show-client-side-image-map (url stream &optional target)
  (flet ((write-header (caption stream)
           (with-table-cell (:header-p t :horizontal-alignment :center :stream stream)
             (with-font (:size 5 :stream stream)
               (with-rendition (:bold :stream stream)
                 (write-string caption stream)))))
         (write-image (url client-map stream width height)
           (with-table-cell (:horizontal-alignment :center :stream stream)
             (image url "No Image Map" :stream stream :client-side-image-map client-map
                    :border 3 :width width :height height))))
    (let ((title "Client Side Image Maps")
          (cern-image-map-url #u"/cl-http/image-maps/cern-shapes.gif?")
          (ncsa-image-map-url #u"/cl-http/image-maps/ncsa-shapes.gif?"))
      (with-html-document (:stream stream)
        (with-document-preamble (:stream stream)
          (declare-title title :stream stream)
          (when target
            (ns4.0:declare-base-reference :reference url :target target :stream stream))
          (declare-default-font :color :pink-spicy :stream stream))
        (with-standard-document-body (:stream stream)
          (with-section-heading (title :stream stream)
            (http::image-line :stream stream)
            (with-paragraph (:stream stream)
              (write-string "This example generates client side image maps from CERN and NCSA image maps
and associated image search URLs.  This capability means that the server mjay choose to emit a client
side image map for those clients capable of handling them, but performing normal server side processing
for those that cannot." stream))
            (write-client-side-image-map
              cern-image-map-url "cern-example"
              :target target :stream stream :width 335 :height 138)
            (write-client-side-image-map
              ncsa-image-map-url "ncsa-example"
              :target target :stream stream :width 335 :height 138)
            (with-centering (:stream stream)
              (with-table (:stream stream)
                (with-caption (:stream stream)
                  (with-font (:size 5 :stream stream)
                    (note-anchor "Client Side Image Maps From Server Side" :tag "image-maps" :stream stream)))
                (with-table-row (:stream stream)
                  (write-header "CERN Image Map" stream)
                  (write-header "NCSA Image Map" stream))
                (with-table-row (:stream stream)
                  (write-image cern-image-map-url "#cern-example" stream nil nil)
                  (write-image ncsa-image-map-url "#ncsa-example" stream nil nil))))
            (http::image-line :stream stream)
            (http:cl-http-signature stream)))))))

(defmethod respond-to-show-client-side-image-map ((url url:http-computed-url) stream)
  (http:with-successful-response (stream :html :content-location url
                                         :expires (url:expiration-universal-time url)
                                         :cache-control (url:response-cache-control-directives url)
                                         :content-language (languages url))
    (%show-client-side-image-map url stream)))

(http:export-url #u"/cl-http/image-maps/show-client-side-image-map.html"
                 :computed
                 :response-function #'respond-to-show-client-side-image-map
                 :expiration `(:interval ,(* 15. 60.))
                 :public t
                 :language :en
                 :keywords '(:cl-http :demo)
                 :documentation "An example of client side image maps.")

(defmethod respond-to-show-client-side-image-map-frame ((url url:http-computed-url) stream)
  (http:with-successful-response (stream :html :content-location url
                                         :expires (url:expiration-universal-time url)
                                         :cache-control (url:response-cache-control-directives url)
                                         :content-language (languages url))
    (%show-client-side-image-map url stream "display")))

(http:export-url #u"/cl-http/image-maps/show-client-side-image-map-frame.html"
                 :computed
                 :response-function #'respond-to-show-client-side-image-map-frame
                 :expiration `(:interval ,(* 15. 60.))
                 :public t
                 :language :en
                 :keywords '(:cl-http :demo)
                 :documentation "An example of client side image maps.")

(defmethod respond-to-show-frame-layout ((url url:http-computed-url) stream)
  (http:with-successful-response (stream :html :content-location url
                                         :expires (url:expiration-universal-time url)
                                         :cache-control (url:response-cache-control-directives url)
                                         :content-language (languages url))
    (with-html-document (:stream stream)
      (with-document-preamble (:stream stream)
        (declare-title "Frame Layout Example" :stream stream))
      (ns4.0:with-document-frameset (:rows '((:constraint 7)
                                             (:constraint 5)) :stream stream)
        (ns4.0:note-document-frame
          :name "index" :reference #u"/cl-http/image-maps/show-client-side-image-map-frame.html"
          :target "display" :stream stream)
        (ns4.0:note-document-frame
          :name "display" :reference #u"/cl-http/image-maps/image-maps.html" :stream stream)))))

(http:export-url #u"/cl-http/show-frame-layout.html"
                 :computed
                 :response-function #'respond-to-show-frame-layout
                 :expiration `(:interval ,(* 15. 60.))
                 :public t
                 :language :en
                 :keywords '(:cl-http :demo)
                 :documentation "An example of frames using client side image maps.")


;;;------------------------------------------------------------------- 
;;;
;;; CLIENT-SIDE COOKIES
;;;

(defun write-compute-cookie-form (url stream)
  (macrolet ((with-query (Label input stream)
               `(with-table-row (:stream ,stream)
                  (with-table-cell (:horizontal-alignment :right :stream ,stream)
                    (with-rendition (:bold :stream ,stream) ,Label))
                  (with-table-cell (:stream ,stream) ,input))))
    (with-html-document (:declare-dtd-version-p t :stream stream)
      (with-document-preamble (:stream stream)
        (declare-base-reference url :stream stream)
        (declare-title "Client-Side Cookies" :stream stream))
      (with-standard-document-body (:stream stream)
        (with-section-heading ("Client-Side Cookies" :stream stream)
          (http::image-line :stream stream)
          (with-paragraph (:stream stream)
            (fast-format stream "~&The server provides facilities for accessing
and setting client-side cookies on clients supporting this feature.  Client-Side cookies
are documented ")
            (note-anchor "here." :reference "http://www.netscape.com/newsref/std/cookie_spec.html" :stream stream))
          (horizontal-line :stream stream)
          (with-section-heading ("Cookies Sent" :stream stream)
            (with-paragraph (:stream stream)
              ;; this macro allows programs to access client-side cookies.
              (http:with-cookie-values (nil)
                (cond (http:cookies
                       (fast-format stream "~&The client, ~A (~A), sent the following cookies:"
                                    http:current-user-agent http:current-user-agent-version)
                       (with-enumeration (stream :itemize)
                         (loop for (key value) on http:cookies by #'cddr
                               do (enumerating-item (stream)
                                    (with-rendition (:bold :stream stream)
                                      (fast-format stream "~A: " key))
                                    (write-string-quoting-specials value stream)))))
                      (t (fast-format stream "~&No cookies were sent by the client, ~A (~A),
or, if you just set a cookie, you may need to reload the page to see the cookies."
                                      http:current-user-agent http:current-user-agent-version))))))
          (horizontal-line :stream stream)
          (with-section-heading ("Set Cookies" :stream stream)
            (with-paragraph (:stream stream)
              (fast-format stream "~&To add a new cookies to your client, you must supply name and value.
To remove a cookie from the client, you must supply name and delete."))
            (with-fillout-form (:post url :stream stream)
              (with-table (:stream stream)
                (with-query (fast-format stream "~&Name: ")
                            (accept-input 'string+ "NAME" :stream stream :size 30)
                            stream)
                (with-query (fast-format stream "~&Value: ")
                            (accept-input 'string+ "VALUE" :stream stream :size 30)
                            stream)
                (with-query (fast-format stream "~&Domain: ")
                            (accept-input 'string+ "Domain" :stream stream :size 30)
                            stream)
                (with-query (fast-format stream "~&Path: ")
                            (accept-input 'string+ "PATH" :stream stream :size 30)
                            stream)
                (with-query (fast-format stream "~&Expires: ")
                            (accept-input 'string+ "EXPIRES" :stream stream :size 30)
                            stream)
                (with-query (fast-format stream "~&Delete: ")
                            (accept-input 'radio-button "DELETE-P" :choices '(("No" . :no) ("yes" . :yes))
                                          :default :no :linebreaks nil :stream stream)
                            stream))
              (submit-and-reset-buttons stream)))
          (http::image-line :stream stream)
          (cl-http-signature stream))))))

(defmethod compute-cookie-form ((url url:http-form) stream)
  (with-successful-response (stream :html :content-location url
                                    :expires (url:expiration-universal-time url)
                                    :cache-control (url:response-cache-control-directives url)
                                    :content-language (languages url))
    (write-compute-cookie-form url stream)))

(defmethod respond-to-compute-cookie-form ((url url:http-form) stream query-alist)
  (flet ((clean-up (item)
           (and item                            ; don't let NIL through
                (not (null-string-p (setq item (string-trim '(#\space #\tab #\return #\Linefeed) item))))
                item))
         (local-domain ()
           (let ((host-name (local-host-domain-name)))
             (subseq host-name (1+ (position #\. (local-host-domain-name))))))
         (expires (expires delete-p)
           (cond ((equalp delete-p "yes")
                  (- (get-universal-time) (* 60 60)))
                 (expires
                  (parse-gmt-time expires))
                 (t (+ (get-universal-time) (* 60 60))))))
    (bind-query-values (name value domain path expires delete-p) (url query-alist)
      (let* ((name (clean-up name))
             (value (clean-up value))
             (domain (clean-up domain))
             (path (clean-up path))
             (expires (clean-up expires))
             (delete-p (clean-up delete-p))
             (headers (when (and name value)
                        ;; construct the cookie setting header  using the defined interface.
                        (http:set-cookie-http-headers ((http::intern-keyword name) value
                                                       :expires (expires expires delete-p)
                                                       :domain (or domain (local-domain))
                                                       :path path)))))
        (declare (dynamic-extent headers))
        (with-successful-response (stream :html :content-location url :expires (url:expiration-universal-time url)
                                          :cache-control (url:response-cache-control-directives url)
                                          :content-language (languages url)
                                          :additional-headers headers)
          ;; generate another version of the form with the new values.
          (write-compute-cookie-form url stream))))))

(export-url #u"/cl-http/computed-cookie-form.html"
            :html-computed-form
            :form-function #'compute-cookie-form
            :response-function #'respond-to-compute-cookie-form
            :expiration '(:no-expiration-header)
            :public t
            :no-cache t
            :language :en
            :keywords '(:cl-http :demo)
            :documentation "An example of Netscape cookies.")


;;;------------------------------------------------------------------- 
;;;
;;; INLNE SPEECH SYNTHESIS USING A CLIENT-SIDE PLUG-IN
;;;

;; The Netscape plug-in fr Macintalk speech synthesis is available from
;; http://www.mvpsolutions.com/PlugInSite/Talker.html

(defparameter *welcome-message* 
              "Welcome - to the Common Lisp Hyper-media Server for - the World Wide Web.")

(defmethod computed-welcome-message ((url url:http-computed-url) stream)
  (with-successful-response (stream :talk
                                    :bytes (http:compute-transmitted-bytes *welcome-message*)
                                    :content-location url
                                    :last-modification (get-universal-time)
                                    :cache-control (url:response-cache-control-directives url)
                                    :content-language (languages url))
    (write-string *welcome-message* stream)))

(export-url #u"/cl-http/welcome.talk"
            :computed
            :response-function #'computed-welcome-message
            :public t
            :language :en
            :keywords '(:cl-http :demo :plug-in)
            :documentation "An example of generating inline speech for a client-side plug-in.")


;;;------------------------------------------------------------------- 
;;;
;;; MARQUEE SCRIPT
;;;

;; This should stop after a while so that it does not burn up all of Netscapes
;; memory and crash the client. Where's the GC?
(ns4.0:define-script
  marquee
  (:Java-Script)
  :caller ((script stream)
           (fast-format stream "timerONE=window.setTimeout('scrollit_r2l(100)',500);"))
  :script ((script stream text)
           (check-type text string)
           (let ((marquee (substitute-if #\space #'(lambda (x)
                                                     (member x '(#\Return #\Linefeed #\Newline) :test #'eql))
                                         text)))
             (declare (dynamic-extent marquee))
             (fast-format stream "~%/*~%~A~%*/~%
     function scrollit_r2l(seed)
         {var m1  = ~S;
          var msg=m1;
          var out = ' ';
          var c   = 1;
          if (seed > 100)
             {seed--;
              var cmd='scrollit_r2l(' + seed + ')';
              timerTwo=window.setTimeout(cmd,100);
              }
             else if (seed <= 100 && seed > 0)
                     {for (c=0 ; c < seed ; c++)
                     {out+=' ';}
             out+=msg;
             seed--;
             var cmd='scrollit_r2l(' + seed + ')';
             window.status=out;
             timerTwo=window.setTimeout(cmd,100);
             }
           else if (seed <= 0)
                   {if (-seed < msg.length)
                       {out+=msg.substring(-seed,msg.length);
                        seed--;
                        var cmd='scrollit_r2l(' + seed + ')';
                        window.status=out;
                        timerTwo=window.setTimeout(cmd,100);
                        }
                    else {window.status=' ';
                          timerTwo=window.setTimeout('scrollit_r2l(100)',75);
                          }
                    }
            }"
                          (documentation script t) marquee)))
  

  :documentation 
  "Copyright (C) 1996 Web Integration Systems, Inc. DBA Websys, Inc.
All Rights Reserved.

This applet can be re-used or modified, if credit is given in 
the source code.

We will not be held responsible for any unwanted effects due to the 
usage of this applet or any derivative.  No warrantees for usability 
for any specific application are given or implied.

Chris Skinner, January 30th, 1996.
   
Hacked for CNNfn by RD, Jan. 31, 1996

Revised for CL-HTTP synthesis by JCMa 2/22/96")

(defmethod compute-marquee-headers-page ((url url:http-computed-url) stream)
  (with-successful-response (stream :html :content-location url
                                    :expires (url:expiration-universal-time url)
                                    :cache-control (url:response-cache-control-directives url)
                                    :content-language (languages url))
    (let* ((title (format nil "Client Headers for ~A" (server-host *server*)))
           (script (ns4.0:intern-script :marquee :java-script)))
      (with-html-document (:stream stream)
        (with-document-preamble (:stream stream)
          (declare-base-reference url :stream stream)
          (declare-title title :stream stream)
          (ns4.0:write-script script stream (documentation url t)))
        (ns4.0:with-event-handlers
          (events
            (:java-script :load (ns4.0:event-caller script)))
          (ns4.0:with-document-body (:background :random :foreground :random
                                                 :events events :stream stream)
            (with-section-heading (title :stream stream)
              (http::image-line :stream stream)
              (write-headers-as-html stream)
              (with-paragraph (:stream stream)
                (write-string "Run the headers test again? " stream)
                (note-anchor "Yes" :reference url :stream stream))
              (http::image-line :stream stream)
              (cl-http-signature stream))))))))

(export-url #u"/cl-http/marquee.html"
            :computed
            :response-function #'compute-marquee-headers-page
            :expiration '(:no-expiration-header)
            :public t
            :language :en
            :keywords '(:cl-http :demo)
            :documentation "CL-HTTP rules ... This Marquee implemented with JavaScript ... 
by emitting appropriate code and calls from ... Common Lisp ...")


;;;------------------------------------------------------------------- 
;;;
;;; LAYERS and JAVASCRIPT
;;;

(ns4.0:define-script
  select-layer
  (:Java-Script1.2)
  :caller ((script stream)
           (write "SelectLayer(this.selectedIndex); return false;" :stream stream))
  :script ((script stream start end)
           (check-type start integer)
           (check-type end integer)
           (let ((layer-name "layer")
                 (visible "inherit")
                 (invisible "hide"))
             (fast-format stream "~%// This function hide all layers~%")
             (loop initially (fast-format stream "~%function HideAllLayers() {")
                   for idx upfrom start below end
                   do (fast-format stream "~%document.layers[\"~A~D\"].visibility = ~S;" layer-name idx invisible)
                   finally (fast-format stream "}~%"))
             (fast-format stream "~%// This function makes a single layer visible.~
                                                     ~%// We have cunningly named the layers so we~
                                                     ~%// can tell which one to show based on the selectedIndex of the menu.")
             (fast-format stream "~%function SelectLayer(n) {~
                                                     ~%HideAllLayers();~
                                                     ~%idx = n + ~D~
                                                     ~%document.layers[~S + idx].visibility = ~S;}~%" start layer-name visible))))

(export-url #u "/cl-http/examples/layers/images/flowers/"
            :image-directory
            :pathname "http:examples;layers;images;"
            :expiration `(:interval ,(* 15. 60.))
            :public t
            :keywords '(:cl-http :example :layers))

(defparameter *layers-example-image-url* 
              #-Netscape-Source (concatenate 'string (http:local-context) "/cl-http/examples/layers/")
              #+Netscape-Source "http://developer.netscape.com/library/documentation/communicator/layers/"
              "Base URL for computed layers example.")

(defparameter *layered-images*
              '(("Mona Lisa Tulip" :show "images/flowers/redtul.jpg"
                 "These tulips have been specially designed to withstand late winter
                  frost in areas with harsh winters.  They are a beautiful red color, and
                  we guarantee that they'll grow for at least four years in a row.  Don't
                  wait to order them, they sell fast!"
                 "Priced at only $1 a bulb, they are a bargain.")
                ("Mixed Dutch Tulips" :hide "images/flowers/tulmulti.jpg"
                 "These colorful tulips have been specially bred for us by Dr. Hans Tulip in
                  Amsterdam.
                  He has spent the last ten years perfecting the hybrid. These tulips start
                  blooming early, sometimes they beat the crocuses out of the ground!
                  They come in a variety of intense colors, and they have a velvety,
                  sweet-smelling bloom."
                 "Priced at $5 for ten, these tulips are a cheap way to bring color to your garden.")
                ("Bijou Violets" :hide "images/flowers/violets.jpg"
                 "These pale purple African violets are much hardier than most violets. You
                  don't need green fingers to keep these flowers thriving!
                  Just water them four times a day at regular intervals, and they will thrive
                  forever!"
                 "These flowers are VERY small, the picture has been magnified so you can
                  see their shape. The plants usually grow to about an inch high. Thus they make
                  excellent indoor plants for tiny apartments."
                 "The price for these lovely lilac blooms is $4 for a half inch pot, or $10 for four pots.")
                ("Punk Chrysanthemum" :hide "images/flowers/spikey.jpg"
                 "These modern chrysanthemums look delicate but are very hardy. They come in
                  a variety of colors, and they can grow to 5 feet tall."
                 "They start blooming in autumn, and will keep flowering until the snow falls.
                  So if you live somewhere that never gets snow, they'll never stop blooming!"
                 "These flowers sell for $6 for a 4 inch pot, or $10 for 2 pots."))) 

(defmethod compute-layered-images (url stream)
  (with-successful-response (stream :html :content-location url
                                    :expires (url:expiration-universal-time url)
                                    :cache-control (url:response-cache-control-directives url)
                                    :content-language (languages url))
    (let ((script (ns4.0:intern-script :select-layer :java-script1.2)))
      (ns4.0:with-event-handlers
        (events (:java-script1.2 :new-value (ns4.0:event-caller script)))
        (with-html-document (:stream stream)
          (with-document-preamble (:stream stream)
            (declare-title "Flowering Layers Example" :stream stream)
            (ns4.0:write-script script stream 0 (length *layered-images*)))
          (with-standard-document-body (:stream stream)
            (with-section-heading ("Selecting Layers Example" :stream stream)
              (with-paragraph (:stream stream)
                (fast-format stream "This is an example from the ")
                (note-anchor "Netscape layers documentation"
                             :reference "http://developer.netscape.com/library/documentation/communicator/layers/index.htm"
                             :stream stream)
                (fast-format stream " where JavaScript is used to expose and hide different layers.~
                                                           ~%In this case, both the HTML and JavaScript are being generated from Lisp."))
              (image-line :stream stream)
              (loop with base-url = *layers-example-image-url*
                    for (heading visibility url . paragraphs) in *layered-images*
                    for count upfrom 0
                    for name = (concatenate 'string "layer" (write-to-string count))
                    for imageurl = (concatenate 'string base-url url)
                    do (ns4.0:with-layer (:name name :visibility visibility :x-origin 50 :y-origin 120 :width 400 :stream stream)
                         (with-rendition (:heading-3 :stream stream)
                           (write-string heading stream)
                           (image imageurl name :horizontal-space 5 :stream stream)
                           (loop for paragraph in paragraphs
                                 do (with-paragraph (:stream stream)
                                      (write-string paragraph stream)))
                           (break-line :clear :all :stream stream))))
              (comment "Position the form layer 40 pixels below the bottom of layer 0" :stream stream)
              (ns4.0:with-layer (:name "FormLayer" :visibility :show :x-origin 500 :y-origin 140 :stream stream)
                (with-paragraph (:stream stream)
                  (with-rendition (:bold :stream stream)
                    (fast-format stream "Please select a flower:")))
                (ns4.0:with-fillout-form (:none url :name "form1" :stream stream)
                  (ns4.0:accept-input 'select-choices "MENU1"
                                      :choices (mapcar #'car *layered-images*)
                                      :default (caar *layered-images*)
                                      :size :pull-down-menu
                                      :events events
                                      :stream stream)))
              (ns4.0:with-layer (:name "FooterLayer" :class :fixed :z-index 5 :x-origin 5 :y-origin 450 :stream stream)
                (image-line :stream stream)
                (http:cl-http-signature stream)))))))))

(http:export-url #u"/cl-http/layers.html"
                 :computed
                 :response-function #'compute-layered-images
                 :public t
                 :no-cache t
                 :language :en
                 :keywords '(:cl-http :demo)
                 :documentation "Shows an examples of Netscape 4.0 layers and JavaScript 1.2 emission.")

;;;------------------------------------------------------------------- 
;;;
;;; META-CONTENT FORMAT 
;;; from http://mcf.research.apple.com
;;;
;;; DIRECTORY INDEXING FUNCTION FOR MCF DISPLAY 
;;;

;; Load the generation tools
(eval-when (load eval compile)
  (load "http:examples;mcf095;mcf"))

(defun %write-directory-mcf (url stream inclusion-predicate path-url-intern-function anchor-function 
                                 &optional directories-p superior-url superior-node)
  #+Genera(declare (sys:downward-funarg inclusion-predicate path-url-intern-function anchor-function))
  (labels ((export-inferior-url (path url-inf url)
             (let ((export-type  (typecase url-inf
                                   (http-path :directory-hierarchy)     ;export directory paths correctly
                                   (t (http::pathname-export-type path nil)))))
               (cond (export-type
                      (export-url url-inf export-type :pathname path)
                      (http::inherit-export-parameters url-inf url))
                     (t (report-bug *bug-http-server* "No Export Type for Pathname" 
                                    "Incorrect autoexport within directory listing:~
                                                        ~&Superior-URL: ~S~&Pathname: ~S" url path )))))
           (write-item (path plist directory-file-p superior-node stream)
             (declare (ignore plist))
             (multiple-value-bind (url-inf newly-created-p)
                 (funcall path-url-intern-function path url)
               (when url-inf
                 ;; Compute anchor text
                 (let ((anchor-text (funcall anchor-function url-inf path directory-file-p)))
                   (declare (dynamic-extent anchor-text))
                   (cond-every ;; ensure that it has been exported.
                     ((or newly-created-p (null (url::translation-method url-inf)))
                      (export-inferior-url path url-inf url))
                     (anchor-text
                       (mcf:hotsauce-node stream (url:name-string url-inf) :name anchor-text :superior superior-node)))))))
           (write-directory (url stream directories-p superior-url superior-node)
             (declare (ignore superior-url))
             (let* ((default (translated-pathname url))
                    (anchor-text (funcall anchor-function url default t))
                    (node (url:name-string url)))
               (mcf:hotsauce-node stream node :name anchor-text :superior superior-node)
               (let ((superior-node (mcf:note-unit-identifier node nil :object-ref-p t)))
                 (declare (dynamic-extent superior-node))
                 (loop for (path . plist) in (if directories-p
                                                 (www-utils:directory-list* default inclusion-predicate :files :directories :properties :sorted)
                                                 (www-utils:directory-list* default inclusion-predicate :files :properties :sorted))
                       for translated = (translated-pathname path)
                       do (cond ((and directories-p (www-utils:pathname-directory-p translated))
                                 (multiple-value-bind (url-inf)
                                     (funcall path-url-intern-function path url)
                                   (when url-inf
                                     (write-directory url-inf stream directories-p url superior-node)))
                                 (write-item translated plist t  superior-node stream))
                                (t (write-item translated plist nil superior-node stream))))))))
    (declare (dynamic-extent #'write-item #'write-directory))
    (write-directory url stream directories-p superior-url superior-node)
    url)) 

(define write-directory-mcf (url stream &optional (directories-p t))
  "When provided as the :directory-writer to a directory export,
this writes a URL directory in the Meta-Content Format understood by
the Apple HotSauce browser plug-in available from
http://mcf.research.apple.com/"
  (flet ((intern-path-url (path url)
           (url:intern-pathname-as-url-inferior path url :if-does-not-exist :create))
         (anchor-text (url pathname directory-file-p)
           (cond (directory-file-p
                  (url:path-most-specific-name url))
                 (t (with-value-cached (url :directory-string)
                      (let ((name (url:object url))
                            (type (pathname-type pathname))
                            (version (pathname-version pathname)))
                        (declare (dynamic-extent name))
                        (typecase version
                          ((or keyword null) (concatenate 'string name "." type))
                          (t (concatenate 'string name "." type "." (write-to-string version :base 10. :escape nil)))))))))
         (inclusion-predicate (pathname)
           (or (pathname-directory-p pathname)
               (find (url:data-type pathname) url::*data-type-pathname-extension-variable-alist* :test #'eq :key #'car))))
    (declare (dynamic-extent #'intern-path-url #'anchor-text #'inclusion-predicate))
    (with-conditional-get-response (stream :mcf :last-modification (file-modification-date (url::cached-pathname url))
                                           :expires (url:expiration-universal-time url) :location url
                                           :cache-control (url:response-cache-control-directives url)
                                           :content-language (languages url))
      (let ((title (funcall #'url:path-directory-string url)))
        (mcf:with-hotsauce-document (stream :name title)
                                    (%write-directory-mcf url stream #'inclusion-predicate #'intern-path-url #'anchor-text  directories-p))))))

(http:export-url #u"/mcf/cl-http/"
                 :directory
                 :directory-writer 'write-directory-mcf
                 :recursive-p t
                 :pathname "http:www;cl-http;"
                 :expiration `(:interval ,(* 15. 60.))
                 :public t
                 :language :en
                 :keywords '(:cl-http :documentation)) 

;;;------------------------------------------------------------------- 
;;;
;;; EXPORT THE URLS NEEDED FOR PORT-SPECIFIC DOCUMENTATION
;;;

;; Provide a little bullet proofing for the user who deletes the ACL directory
;; inadvertently.
(when (and (probe-directory "http:acl;examples;")
           (probe-file "http:acl;examples;exports.lisp"))
  (load "http:acl;examples;exports"))

(when (and (probe-directory "http:acl501;examples;")
           (probe-file "http:acl501;examples;exports.lisp"))
  (load "http:acl501;examples;exports"))

(when (and (probe-directory "http:lw;examples;")
           (probe-file "http:lw;examples;exports.lisp"))
  (load "http:lw;examples;exports"))

(when (and (probe-directory "http:lcl;examples;")
           (probe-file "http:lcl;examples;exports.lisp"))
  (load "http:lcl;examples;exports"))

(when (and (probe-directory "http:cmucl;examples;")
           (probe-file "http:cmucl;examples;exports.lisp"))
  (load "http:cmucl;examples;exports"))

;;;------------------------------------------------------------------- 
;;;
;;;  Export the urls for the CL-HTTP paper from WWW-94
;;;

(export-url #u"/projects/iiip/doc/cl-http/"
            :directory
            :pathname "http:www;cl-http;www94;code-example.html"
            :public t
            :language :en
            :keywords '(:cl-http :demo :www94)
            :documentation "The paper describing CL-HTTP at the First International Conference on the World Wide Web.") 

;;;------------------------------------------------------------------- 
;;;
;;; EXPORT THE ENTIRE DISTRIBUTION DIRECTORY HIERARCHY
;;;

;; Log Window and Web Configuration URL
#-mit-site
(export-log-window
  ;; Webmaster access only on server
  :authentication-realm :server
  :authentication :server
  :capabilities :webmasters
  :secure-subnets `(,(local-host-ip-address)))

;; export the log files for the server.
#-mit-site
(export-url #u"/cl-http/logs/"
            :text-directory
            :pathname "http:logs;*.text"
            :recursive-p t
            :expiration '(:no-expiration-header)
            :keywords '(:cl-http :demo)
            :documentation (format nil "The log files for CL-HTTP running on ~A." (www-utils:local-host-domain-name))
            ;; Webmaster access only on server
            :authentication-realm :server
            :authentication :server
            :capabilities :webmasters
            :secure-subnets `(,(local-host-ip-address))
            :private t
            :proxy-revalidate t
            :language :en)

#-(or mit-site lispworks)
(export-url #u"/cl-http/pw/"
            :lisp-directory
            :pathname "http:pw;"
            :recursive-p t
            :recache t
            :keywords '(:cl-http :maintenance)
            ;; Webmaster access only on server
            :authentication-realm :server
            :authentication :server
            :capabilities :webmasters
            :secure-subnets `(,(local-host-ip-address))
            :private t
            :proxy-revalidate t
            :language :en)

#-mit-site
(export-url #u"/cl-http/distribution/log/pw/"
            :directory
            :pathname "http:pw;"
            :recursive-p t
            :recache t
            :keywords '(:cl-http :distribution)
            ;; Webmaster access only on server
            :authentication-realm :server
            :authentication :server
            :capabilities :webmasters
            :secure-subnets `(,(local-host-ip-address))
            :private t
            :proxy-revalidate t
            :language :en)
#-mit-site
(export-url #u"/cl-http/distribution/"
            :directory
            :pathname "http:http;"
            :recursive-p t
            :recache nil
            :public t
            :language :en
            :keywords '(:cl-http :distribution))


;;;------------------------------------------------------------------- 
;;;
;;; W3P DOCUMENTATION
;;;

;; The supporting code is loaded at the top of this file.
;; Export documentation for w3p
#+W3P
(http:export-url #u"/cl-http/w3p/basic-function?"
                 :search
                 :response-function #'w3p::respond-to-basic-functions
                 :expiration `(:interval ,(* 15. 60.))
                 :public t
                 :language :en
                 :keywords '(:cl-http :w3p :documentation)
                 :documentation "describe a basic w3p function")

#+W3P
(http:export-url #u"/cl-http/w3p/presentation-function?"
                 :search
                 :response-function #'w3p::respond-to-presentation-functions
                 :expiration `(:interval ,(* 15. 60.))
                 :public t
                 :language :en
                 :keywords '(:cl-http :w3p :documentation)
                 :documentation "describe a w3p presentation function")

#+W3P
(http:export-url #u"/cl-http/w3p/presentation-type?"
                 :search
                 :response-function #'w3p::respond-to-presentation-types
                 :expiration `(:interval ,(* 15. 60.))
                 :public t
                 :language :en
                 :keywords '(:cl-http :w3p :documentation)
                 :documentation "describe a w3p presentation type")



;;;------------------------------------------------------------------- 
;;;
;;; PROXY INTERFACE
;;;

#+CL-HTTP-PROXY
(when (member :cl-http-proxy *features*)
  (http:export-proxy-interfaces))

#+Lambda-IR
(when (member :lambda-ir *features*)
  (add-initialization "Export LambdaVista Search Page"
                      '(http:export-lambdavista-search-page)
                      '(:now)                   ;evaluate now and add to initialization list
                      'http:*server-launch-initialization-list*))
