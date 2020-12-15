;;;   -*- Mode: LISP; Package: (html4.0 :use (future-common-lisp html3.2 www-utils url)); BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-

;;;
;;; (c) Copyright  1999, John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; HTML 4.0 GENERATION
;;;
;;;------------------------------------------------------------------- 
;;;
;;; PACKAGE DEFINITION
;;;

(eval-when (load eval compile)

(defpackage html4.0
  (:use future-common-lisp html3.2 www-utils url)
  #+ignore(:nicknames html)
  (:shadow
    "*DTD-VERSION*"
    "DECLARE-HTML-VERSION"
    "WITH-HTML-DOCUMENT")
  (:import-from html2
   "*SECTION-LEVEL*"                            ;interoperate between different packages
   "%WRITE-COMMAND-KEY-ARG"
   "%ISSUE-COMMAND"
   "%WITH-ENVIRONMENT"
   "ISSUE-COMMAND"
   "ISSUE-COMMAND*"
   "WITH-ENVIRONMENT")
  (:import-from ns1.1
   "%WRITE-WIDTH-ARGUMENT")
  (:export
    "*DTD-VERSION*"
    "DECLARE-HTML-VERSION"
    "WITH-HTML-DOCUMENT"))

  ;; Make sure we're exporting a compete set of symbols.
  (let* ((ancestor-pkg (find-package :html3.2))
         (pkg (find-package :html4.0))
         (shadowed-symbols (mapcar #'symbol-name (package-shadowing-symbols pkg))))
    (do-external-symbols (sym ancestor-pkg)
      (let ((name (symbol-name sym)))
        (unless (member name shadowed-symbols :test #'equalp)   ; don't export shadowed symbols
          (export (intern name pkg) pkg))))))   ;close eval-when 

(in-package :html4.0) 

;;;------------------------------------------------------------------- 
;;;
;;;  DOCUMENT LEVEL OPERATIONS
;;;

(defconstant *dtd-version* "-//W3C//DTD HTML 4.0//EN")

(define declare-html-version (&optional (stream *output-stream*) (dtd-version :frameset))
  "Declares the document type as the current HTML generation DTD.
All HTML 4.0 must declare the document type definition version.

   DTD-VERSION can be any of:

      :STRICT       - includes all elements that have not been deprecated and do not appear in frameset documents.
      :TRANSITIONAL - includes everything is the STRICT DTD plus deprecated elements and attributes.
      :FRAMESET     - includes everything in TRANSITIONAL plus frames."
  (%issue-command ("!DOCTYPE HTML PUBLIC" stream :fresh-line t :trailing-line t)
    (ecase dtd-version
      (:frameset
	(fast-format stream " ~S ~S" "-//W3C//DTD HTML 4.0 Frameset//EN" "http://www.w3.org/TR/REC-html40/frameset.dtd"))
      (:strict
	(fast-format stream " ~S ~S" *dtd-version* "http://www.w3.org/TR/REC-html40/strict.dtd"))
      (:transitional
	(fast-format stream " ~S ~S" "-//W3C//DTD HTML 4.0 Transitional//EN" "http://www.w3.org/TR/REC-html40/loose.dtd")))))

(define-macro with-html-document ((&key (stream '*output-stream*) declare-dtd-version-p
					(language nil language-supplied-p) (text-directionality nil text-directionality-supplied-p))
				  &body body)
  "Asserts the contents of BODY is an HTML document.

  DECLARE-DTD-VERSION-P will declare the version of the DTD implemented by the current generation
  package. This should be T whenever generation strictly conforms to the HTML version
  associated with the macro WITH-HTML-DOCUMENT. HTML 4.0 offers three DTD versions.
  Consequently, DECLARE-DTD-VERSION-P can be any of :FRAMESET, :TRANSITIONAL, or :STRICT.
  A value of T is interpreted as :FRAMESET. DECLARE-DTD-VERSION-P should always be NIL,
  whenever extension tags or features outside these specifications are used.

  LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)

  TEXT-DIRECTIONALITY is ht base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT."
  (let ((args `(,.(when language-supplied-p
		    `((%write-language-argument ,stream ,language)))
		,.(when text-directionality-supplied-p
		    `((%write-text-directionality-argument ,stream ,text-directionality)))))
	(version (ecase declare-dtd-version-p
		   ((nil) nil)
		   ((t) :frameset)
		   (:transitional :transitional)
		   (:strict :strict))))
    (if version
	`(progn (declare-html-version ,stream ,version)
		(%with-environment ("HTML" :stream ,stream)
				   ,(when args (cons 'progn args))
		  . ,body))
	`(%with-environment ("HTML" :stream ,stream)
			    ,(when args (cons 'progn args))
	   . ,body))))


(with-html-document (:declare-dtd-version-p t :language :en :text-directionality :LEFT-TO-RIGHT)
  (print 'foo))

;;;------------------------------------------------------------------- 
;;;
;;; GENERATION UTILITIES
;;;

;; check the percentage spec to make sure it remains the same   3/23/99 -- JCMa.
(defun %write-length-argument (tag length stream)
  (if (<= 0 length 1)
      (fast-format stream " ~A=~D%" tag (floor (* length 100)))
      (%write-command-key-arg stream tag length t)))

(declaim (inline %write-id-argument))

;; the data passed to these could be check to assure valid formation   3/23/99 -- JCMa.
(defun %write-id-argument (stream id)
  (%write-command-key-arg stream "ID" id))

(defun %write-class-argument (stream class)
  (typecase class
    (cons
      (fast-format stream " CLASS=\"~I\""
		   (loop for items = class then (cdr items)
			 while (cdr items)
			 do (fast-format stream "~A " (car items))
			 finally (fast-format stream "~A" (car items)))))
    (t (%write-command-key-arg stream "CLASS" class))))

(defun %write-style-argument (stream style)
  (%write-command-key-arg stream "STYLE" style))

(declaim (inline %write-name-argument))

(defun %write-name-argument (stream name)
  (%write-command-key-arg stream "NAME" name))

;; define the languages sometime.   3/23/99 -- JCMa.
(defun %write-language-argument (stream language)
  (%write-command-key-arg stream "LANG" language))

(defun %write-text-directionality-argument (stream text-directionality)
  (%write-command-key-arg stream "DIR" (case text-directionality
					 (:left-to-right "LTR")
					 (:right-to-left "RTL")
					 (t (error "TEXT-DIRECTIONALITY ~S is not one of: ~S."
						   '(:left-to-right :right-to-left))))))

(defun %write-tab-position-argument (stream tab-position)
  (unless (and (integerp tab-position) (< 0 tab-position 32767))
    (error "tab-position is ~S, which is not an integer between 0 and 32767." tab-position))
  (%write-command-key-arg stream "TABINDEX" tab-position))


;;;------------------------------------------------------------------- 
;;;
;;; INLINE FRAMES
;;;

(defconstant *inline-frame-alignment-values* '(:top :middle :bottom :left :right))

(defun inline-frame-alignment-value (alignment)
  (unless (member alignment *inline-frame-alignment-values*)
    (error "Unknown alignment, ~S, for an inline frame." alignment))
  (symbol-name alignment))

(define note-inline-frame (&key name reference title (alignment :middle) (height .3) (width .3)
				(scrolling :auto) (frame-border t) border
				margin-width margin-height alternative-text
				style class id (stream *output-stream*))
  "Notes an inline frame on STREAM.

  The frame loads its own URL independently of the containing HTML. It can be targeted
  by regular frames using its NAME.
         
  NAME is a string denoting the name for targetting output.
  REFERENCE is a URL or url-string.
  TITLE is a string used as a caption.
  ALIGNMENT is one of :TOP, :MIDDLE, :BOTTOM, :LEFT, or :RIGHT.
  HEIGHT is either the number of pixels or a fraction of the window.
  WIDTH is either the number of pixels or fraction of the window.
  SCROLLING can be :AUTO, T, or NIL
  FRAME-BORDER is a boolean argument that controls whether the frame has
  borders or not.
  BORDER is an integer that sets the thickness in pixels of frame's borders.
  MARGIN-WIDTH is a size in pixels.
  MARGIN-HEIGHT is a size in pixels.
  ALTERNATIVE-TEXT is a string or function to inform users of pre-html4.0
  clients where to find the inline URL.
  STYLE denotes the style sheet to use.
  CLASS is the class for the element
  ID is the identifier for the element."
  (%with-environment
    ("IFRAME" :stream stream)
    (cond-every
      (name
	(%write-name-argument stream name))
      (reference
	(%write-command-key-arg stream "SRC" (coerce-url-string reference)))
      (title (%write-command-key-arg stream "TITLE" title))
      (alignment
	(%write-command-key-arg stream "ALIGN" (inline-frame-alignment-value alignment)))
      (height (%write-length-argument "HEIGHT" height stream))
      (width (%write-length-argument "WIDTH" width stream))
      ((null frame-border)
       (%write-command-key-arg stream "FRAMEBORDER" "NO"))
      (border
	(%write-command-key-arg stream "BORDER" border t))
      (margin-height
	(%write-command-key-arg stream "MARGINHEIGHT" margin-height t))
      (margin-width
	(%write-command-key-arg stream "MARGINWIDTH" margin-width t))
      (scrolling
	(%write-command-key-arg stream "SCROLLING" (ecase scrolling
						   (:auto "AUTO")
						   ((t) "YES")
						   ((nil) "NO"))))
      (style (%write-style-argument stream style))
      (class (%write-class-argument stream class))
      (id (%write-id-argument stream id)))
    (etypecase alternative-text
      (null
	(with-emphasis (:quotation :stream stream)
	  (with-rendition (:italic :stream stream)
	    (fast-format stream "View content which your client cannot display inline at ~I."
			 (note-anchor (or title "inline document" :reference reference :stream stream))))))
      (string (write-string alternative-text stream))
      (function (funcall alternative-text stream)))))


;;;------------------------------------------------------------------- 
;;;
;;; EMBEDDED OBJECTS
;;;

(defun %write-media-type-argument (stream tag media-type-spec)
  (typecase media-type-spec
    (cons
      (flet ((writer (stream) (http::write-mime-content-type media-type-spec stream)))
	(declare (dynamic-extent #'writer))
	(%write-command-key-arg stream tag #'writer)))
    (t (%write-command-key-arg stream tag media-type-spec))))

(defun %write-archive-argument (stream archive)
  (etypecase archive
    (atom (%write-command-key-arg stream "ARCHIVE" (coerce-url-string archive)))
    (cons (loop initially (write-string "ARCHIVE=" stream)
		for items = archive then (cdr items)
		while (cdr items)
		do (fast-format stream "~A " (coerce-url-string (car items)))
		finally (fast-format stream "~A" (coerce-url-string (car items)))))))

(define note-parameter (name value &key (interpretation :data) data-type id (stream *output-stream*))
  "Notes a runtime parameter for an applet or object named NAME with VALUE on STREAM.
This function must be used within the scope of a WITH-APPLET or WITH-OBJECT macro.

  NAME is a string or symbol specifying the attribute name. 
  VALUE is a string holding the parameter value or a 
  function that writes the value as an escaped string on STREAM.
  INTERPRETATION specifies how to interpret VALUE, and can be any of :DATA, :REFERENCE, or :OBJECT.
  DATA-TYPE specifies the media type of VALUE only when INTERPRETATION is :REFERENCE
  and can be a content type list, a string, or a function.
  ID is an element identifier."
  (%issue-command ("PARAM" stream :fresh-line t :trailing-line t)
    (%write-command-key-arg stream "NAME" name)
    (%write-command-key-arg stream "VALUE" value)
    (%write-command-key-arg stream "VALUE-TYPE" (ecase interpretation
						  (:DATA "DATA")
						  (:REFERENCE "REF")
						  (:OBJECT "OBJECT")))
    (cond-every
      (data-type (%write-media-type-argument stream "TYPE" data-type))
      (id (%write-id-argument stream id)))))

(defun %write-object-arguments (stream args)
  (destructuring-bind (name data code resources base data-type code-type height width
			    client-side-image-map title progress-message
			    language text-directionality
			    style class id tab-position events)
      args
    (cond-every
      (name (%write-name-argument stream name))
      (data (%write-command-key-arg stream "DATA" (coerce-url-string data)))
      (code (%write-command-key-arg stream "CLASSID" (coerce-url-string code)))
      (resources (%write-archive-argument stream resources))
      (base (%write-command-key-arg stream "CODEBASE" (coerce-url-string base)))
      (data-type (%write-media-type-argument stream "TYPE" data-type))
      (code-type (%write-media-type-argument stream "CODETYPE" code-type))
      (height (%write-length-argument "HEIGHT" height stream))
      (width (%write-length-argument "WIDTH" width stream))
      (client-side-image-map
	(%write-command-key-arg stream "USEMAP" (url:name-string-without-search-suffix client-side-image-map nil)))
      (title (%write-command-key-arg stream "TITLE" title))
      (progress-message (%write-command-key-arg stream "STANDBY" progress-message))
      (language (%write-language-argument stream language))
      (text-directionality (%write-text-directionality-argument stream text-directionality))
      (style (%write-style-argument stream style))
      (class (%write-class-argument stream class))
      (id (%write-id-argument stream id))
      (tab-position (%write-tab-position-argument stream tab-position))
      (events
	(dolist (event events)
	  (html2::%write-input-type-event-arg stream event))))))

(defun %write-alterative-text (stream alternative-text)
  (etypecase alternative-text
    (null)
    (string (write-string alternative-text stream))
    (function (funcall alternative-text stream))))

(define-macro with-object ((&key name title code data resources base code-type data-type client-side-image-map
				 height width progress-message (alternative-text nil alternative-text-supplied-p)
				 language text-directionality events
				 style class id tab-position (stream *output-stream*)) &body body)
  "Allows embedding of inline objects within HTML documents.
This generalization of inline images and inline applets provides a powerful
mechanism to include data or program output. This operator declares the object
and associates it with NAME. Objects may appear in the document preamble only
when they do not render data, for example, in the case of a program object.
In other cases, for example, when only DATA is specified, objects must appear
in the document body. When the object is a program requiring parameters, the
function NOTE-PARAMETER should be called with BODY for each parameter.
         
  NAME is a string denoting the name for the element.
  TITLE is a string used as a caption.
  CODE may a URI specifying the location of the program that performs the rendering.
  DATA may a URI specifying the location of data to be rendered.
  RESOURCES can be a list of URIs containing resources relevant to this object.
  CODE-TYPE is the media type of CODE, which can be content type list, a string, or a function.
  DATA-TYPE is the media type of DATA, which can be content type list, a string, or a function.
  BASE is a base URI against which to merge any relative URIs appearing in CODE, DATA, or resources.
  CLIENT-SIDE-IMAGE-MAP indicates the client side image map to use.
   Normally, this is a named URI (/url.html#map-name) and often, all
   client side image maps are served from a single url. The function
   WRITE-CLIENT-SIDE-IMAGE-MAP writes a client side image
   map from a server-side image map URL (CERN or NCSA formats).
  CLIENT-SIDE-IMAGE-MAP
  HEIGHT is either the number of pixels or a fraction of the window.
  WIDTH is either the number of pixels or fraction of the window.
  PROGRESS-MESSAGE is a string or function that provides a message to the user while the object loads.
  ALTERNATIVE-TEXT is a string or function that provides a message for clients unable to display the object.
  LANGUAGE is the two-digit language code for the displayed content (see RFC 1766)
  TEXT-DIRECTIONALITY is ht base directionality of neutral text and can be either :LEFT-TO-RIGHT or :RIGHT-TO-LEFT.
  STYLE denotes the style sheet to use.
  CLASS is the class for the element
  ID is an element identifier.
  TAB-POSITION is a number that specifies the position of the element in the current document.
  The number must be between 0 and 32767."
  `(%with-environment ("OBJECT" :stream ,stream)
		      (let ((args (list ,name ,data ,code ,resources ,base ,data-type ,code-type ,height ,width
					,client-side-image-map ,title ,progress-message
					,language ,text-directionality
					,style ,class ,id ,tab-position ,events)))
			(declare (dynamic-extent args))
			(%write-object-arguments ,stream args))
		     
     ,@body
     ,@(when alternative-text-supplied-p
	 `((%write-alterative-text ,stream ,alternative-text)))))
