;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: URL; Base: 10 -*-

;;; (C) Copyright 1994-97, John C Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; UNIVERSAL RESOURCE LOCATORS CLASSES
;;;

(in-package :url)

(defclass colon-scheme-prefix-mixin () ())

(defclass displayable-name-mixin
          ()
    ((display-string :initform nil :initarg :display-string)))

(defclass host-mixin
          ()
    ((host-string :initarg :host-string :accessor host-string)
     (host-object :initform nil :accessor %host-object)))

(defclass host-port-mixin
          (host-mixin)
    ((port :initarg :port :accessor port)))

(defclass user-id-mixin
          ()
    ((user-id :initarg :user-id :accessor user-id)))

(defclass user-id-and-pw-mixin
          (user-id-mixin)
    ((password :initform nil :initarg :password :accessor password)))

(defclass path-mixin
          ()
    ((path :initarg :path :accessor path)))

(defclass object-mixin
          ()
    ((object :initform nil :initarg :object :accessor object)
     (extension :initform nil :initarg :extension :accessor extension)))

(defclass pathname-caching-mixin
          ()
    ((pathname :initform nil :reader cached-pathname)))

(defclass content-language-mixin
          ()
    ((languages :initform nil :reader languages)))

(defclass alternate-url-mixin
          ()
    ((urls :initform nil :reader alternate-urls)))

(defclass computed-headers-mixin
          ()
    ((header-function :initform nil :initarg header-function :reader header-function)))

(defclass computed-url-mixin
          (computed-headers-mixin)
    ((response-function :initform nil :initarg :response-function :reader response-function)))

(defclass form-processing-mixin
          (computed-url-mixin)
    ((server :initform nil :initarg :server :reader form-server)))

(defclass caching-path-mixin (pathname-caching-mixin path-mixin) ())

(defclass caching-object-mixin (pathname-caching-mixin object-mixin) ())

(defclass search-mixin
          (computed-url-mixin)
    ((search-keys :initform nil :initarg :search-keys :accessor search-keys)
     (search-database :initform nil :initarg :search-database :writer (setf search-database))
     (search-parent :initform nil :accessor %search-parent :initarg :search-parent)))

(defclass search-parser-mixin
          ()
    ((search-parser :initform #'standard-parse-search-info :accessor search-parser :initarg :search-parser)
     (search-writer :initform #'standard-write-search-info :accessor search-writer :initarg :search-writer)))

(defclass mutating-class-mixin
          ()
    ((origin-class  :initarg :origin-class :accessor origin-class))
  (:documentation "A mixin that allows class changes to be undone."))

(defclass translation-method-mixin
          (mutating-class-mixin)
    ((translation-method :initform nil :reader translation-method)))

(defclass cache-mixin
          ()
    ((cache :accessor cache)
     (creation-time  :accessor cache-creation-time)
     (reference-time :accessor cache-reference-time)))

(defclass expiration-mixin
          ()
    ((expiration-function :initform nil :initarg :expiration-function :accessor expiration-function)
     (max-age-function :initform nil :initarg :max-age-function :accessor max-age-function)))

(defclass secure-subnets-mixin
          ()
    ((secure-subnets :initform nil :accessor secure-subnets))
  (:documentation "A mixin that provide for subnet security on access to URLs."))

(defclass authentication-mixin
          ()
    ((authentication-realm :initform nil :accessor authentication-realm)
     (capabilities :initform nil :accessor capabilities))
  (:documentation "A mixin that provides for authentication security on access to URLs."))

(defclass http-cache-control-mixin
          ()
    ((directives :initform nil :accessor cache-control-directives))
  (:documentation "A mixin that provides for cache control directives that are sent when
the url is served."))

(defclass documentation-mixin () () 
  (:documentation "Allows keywords and a documentation string to be associated with a URL."))

;; Two mixins for COMLINK Dynamic form processing.
(defclass dynamic-form-mixin
          ()
    ((dynamic-form :initform nil :initarg :dynamic-form :accessor dynamic-form)))

(defclass dynamic-form-processing-mixin
          (form-processing-mixin dynamic-form-mixin)
    ())

(defclass client-side-script-mixin
          ()
    ((script :initform nil :initarg :script :accessor script)))

(defclass template-mixin
          (computed-headers-mixin)
    ((template-lock :initform nil :initarg :template-lock :accessor template-lock)
     (template-parameters :initform nil :initarg :template-parameters :accessor template-parameters)
     (template-update-time :initform nil :initarg :template-update-time :accessor template-update-time))
  (:documentation "Add templates to static files."))


;;;------------------------------------------------------------------- 
;;;
;;; URL CLASSES
;;;

(defclass uri
          (property-list-mixin)                 ; add property lists to all URIs
    ()
  (:documentation "Root class of Uniform Resource Identifiers."))

(defclass url
          (displayable-name-mixin
            documentation-mixin                 ; keyword and docoumentation mixin
            uri)                                ;URLs are URIs
    ((name-string :initform nil :initarg :name-string))
  (:documentation "Root class of Universal Resource Locators."))

(defclass telnet-url
          (user-id-mixin host-port-mixin url)
    ((scheme :initform "telnet" :reader scheme :allocation :class))
  (:documentation "Telnet url class."))

(defclass ftp-url
          (url user-id-and-pw-mixin)
    ((scheme :initform "ftp" :reader scheme :allocation :class))
  (:documentation "File transfer protocol url class."))

(defclass ftp-directory
          (path-mixin host-port-mixin ftp-url)
    ())

(defclass ftp-pathname
          (object-mixin path-mixin host-port-mixin ftp-url)
    ())

(defclass news-url (colon-scheme-prefix-mixin url)
    ((scheme :initform "news" :reader scheme :allocation :class))
  (:documentation "News url class."))

(defclass news-group
          (news-url)
    ((group :initarg :group :accessor news-group)))

(defclass news-article
          (news-url)
    ((message-id :initarg :message-id :accessor message-id)))

(defclass http-url
          (translation-method-mixin
            host-port-mixin
            http-cache-control-mixin
            expiration-mixin
            secure-subnets-mixin
            authentication-mixin
            content-language-mixin
            url)
    ((scheme :initform "http" :reader scheme :allocation :class))
  (:documentation "Root class of hypertext transfer protocol urls."))

(defclass http-path
          (alternate-url-mixin caching-path-mixin http-url)
    ((directory-writer :initform nil :initarg :directory-writer :accessor directory-writer))
  (:documentation "URL path to a http resource."))

(defclass http-minimum-object
          (object-mixin path-mixin http-url)
    ()
  (:documentation "The minimum class for HTTP objects."))

(defclass http-object
          (alternate-url-mixin caching-object-mixin http-minimum-object)
    ()
  (:documentation "Root class for standard HTTP objects on the server."))

(defclass http-template-object
          (template-mixin alternate-url-mixin caching-object-mixin http-minimum-object)
    ()
  (:documentation "Root class for standard HTTP template objects on the server."))

;; An object without any pathname-related slots.
(defclass http-virtual-object
          (http-minimum-object)
    ()
  (:documentation "The root class for virtual objects without any pathname related slots."))

(defclass http-computed-url
          (computed-url-mixin http-object)
    ()
  (:documentation "Computes a response to HTTP GET method."))

(defclass http-search
          (search-parser-mixin search-mixin http-object)
    ()
  (:documentation "Computes a response based on search suffix supplied in url."))

(defclass http-searchable-object
          (search-parser-mixin search-mixin http-object)
    ()
  (:documentation "Allow searches of http-objects such as images."))

(defclass http-form
          (form-processing-mixin http-object)
    ()
  (:documentation "Computes response to HTTP POST method."))

(defclass http-computed-form
          (http-form)
    ((form-function :accessor form-function))
  (:documentation "Computes HTML fillout form and response to HTTP POST method."))

(defclass http-dynamic-form
          (http-virtual-object dynamic-form-processing-mixin)
    ()
  (:documentation "A COMLINK Dynamic Form."))

(defclass http-dynamic-survey-instrument
          (http-dynamic-form)
    ()
  (:documentation "A COMLINK Dynamic Survey Instrument."))

(defclass http-dynamic-form-instructions
          (http-virtual-object dynamic-form-mixin)
    ())

(defclass http-client-script
          (http-virtual-object client-side-script-mixin)
    ()
  (:documentation "A client-side script."))

(defclass gopher-url (url)
    ((scheme :initform "gopher" :reader scheme :allocation :class))
  (:documentation "Gopher url class."))

(defclass gopher-type-mixin
          ()
    ((type :initarg :type :reader gopher-type)))

(defclass gopher-path
          (path-mixin gopher-type-mixin host-port-mixin gopher-url)
    ())

(defclass gopher-object
          (object-mixin path-mixin gopher-type-mixin host-port-mixin gopher-url)
    ())

(defclass gopher-search
          (search-mixin path-mixin gopher-type-mixin host-port-mixin gopher-url)
    ())

(defclass mailto-url
          (colon-scheme-prefix-mixin host-mixin url)
    ((scheme :initform "mailto" :reader scheme :allocation :class)
     (user-id :initarg :user-id :reader user-id))
  (:documentation "Mailto url class."))

(defclass wais-url (url)
    ((scheme :initform "wais" :reader scheme :allocation :class))
  (:documentation "Wide area information server url class."))

(defclass wais-database-mixin
          ()
    ((database :initarg :database :reader database)))

(defclass wais-type-mixin
          ()
    ((type :initarg :type :reader wais-type)
     (size :initarg :size :reader wais-size)))

(defclass wais-doc
          (object-mixin path-mixin wais-type-mixin
                        wais-database-mixin host-port-mixin wais-url)
    ())

(defclass wais-search
          (search-mixin wais-database-mixin host-port-mixin wais-url)
    ())

(defclass file-url
          (host-mixin url)
    ((scheme :initform "file" :reader scheme :allocation :class)))

(defclass file-directory (file-url path-mixin) ())

(defclass file-pathname (object-mixin file-url path-mixin) ())

(index-definitions cached-pathname alternate-url host-string port user-id path object extension search-keys
                   translation-method
                   cache-creation-time cache-reference-time scheme news-group message-id gopher-type
                   database wais-type wais-size)
