;;;   -*- Mode: lisp; Package: netscape2.0; BASE: 10; Syntax: ansi-common-lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-

;;;
;;; (c) Copyright 1995-97, John C. Mallery
;;;     All Rights Reserved.
;;;


;;;------------------------------------------------------------------- 
;;;
;;; INTERFACE FOR AUTHORING HTML USING NETSCAPE 2.0 EXTENSIONS
;;;
;;; Specification from http://home.netscape.com/assist/net_sites/html_extensions.html
;;; and http://home.netscape.com/assist/net_sites/html_extensions_3.html


;; * client side image maps work.  Polygons and NCSA ellipses are not handled
;; by the Netscape code as of 2.0b1 There is some confusion about polygons in the NCSA format.
;;
;; * Frames work.
;;
;; * A number of Netscape 2.0 enhancements work
;;
;; * Embedding Java applets in HTML implemented.
;;
;; 
;;


;;;------------------------------------------------------------------- 
;;;
;;; PACKAGE DEFINITION
;;;

(eval-when (load eval compile)
  (dolist (string '("BOUNDING-SHAPE"
                    "CENTER"
                    "CIRCLE"
                    "DESTINATION"
                    "IMAGE-MAP-DATA"
                    "POINT-COORDINATES"
                    "POINT-LIST"
                    "POINT1"
                    "POINT2"
                    "POLYGON"
                    "RADIUS"
                    "RECTANGLE"
                    "REGION"
                    "REGION-LIST"
                    "SHAPE"
                    "URL-DEFAULT"
                    "WITH-POINT-COORDINATES"
                    "WITH-STRING-FOR-NULL-STREAM"
                    "X1" "X2" "Y1" "Y2"))
    (import (intern string :http) :ns2.0))
  (let ((html-pkg (find-package :ns1.1))
        (netscape-pkg (find-package :netscape2.0)))
    (do-external-symbols (sym html-pkg)
      (export (intern (symbol-name sym) netscape-pkg) netscape-pkg))))  ;close eval-when

(in-package :netscape2.0)


;;;------------------------------------------------------------------- 
;;;
;;; MAGIC TARGET WINDOW PROCESSING
;;;

(declaim (inline special-target-window-name target-window-name-string))

(define special-target-window-name (keyword)
  "Returns the special target window names for keyword:

  :BLANK  load this link into a new, unnamed window
  :SELF   load this link over yourself
  :PARENT load this link over your parent (becomes self if there is no parent)
  :TOP    load this link over your parent (becomes self if your at the top).
  NOTE: The values associated with the keywords below MUST BE lowercase to work."
   (ecase keyword
      (:blank "_blank")                           ;always load this link into a new, unnamed window
      (:self "_self")                             ;always load this link over yourself
      (:parent "_parent")                         ;Always load this link over your parent (becomes self if your at the top)
      (:top "_top")))

(define target-window-name-string (name)
  "Coerces special target keywords to the relevant strings. See special-target-window-name."
  (etypecase name
    (string name)
    (keyword (special-target-window-name name)))) 

(define %write-target-window-command-key-arg (stream target)
  (fast-format stream " TARGET=~S" 
               (etypecase target
                 (string target)
                 (keyword (special-target-window-name target))))) 

(declaim (notinline special-target-window-name target-window-name-string))

(define client-target-window-http-headers (&optional (target-window-name "Display-Window"))
  "If the client supports frames, returns the HTTP header plist to make the client 
use a separate window for display of some response."
  (multiple-value-bind (user-agent version)
      (http:current-user-agent)
    (when (http:user-agent-capability-p :frames user-agent version)
      `(:window-target ,(target-window-name-string target-window-name)))))


;;;------------------------------------------------------------------- 
;;;
;;; NOTE ANCHOR
;;;

(declaim (inline %write-anchor-command-arguments))

(defun %write-anchor-command-arguments
       (stream reference local-reference tag target events relation inverse urn title methods)
  (declare (notinline html2::%write-command-key-arg))   ;don't bloat the working set
  ;; write anchor arguments
  (cond ((and local-reference reference)
         (%write-command-key-arg stream "HREF" (coerce-url-string reference url:*escape-search-urls*))
         (fast-format stream "#~A" local-reference))
        (reference ;; URL for a anchored document retrieved by the anchor.
         ;; ensure generated URLs are escaped.
         (%write-command-key-arg stream "HREF" (coerce-url-string reference url:*escape-search-urls*)))
        (local-reference ;; tags for a position in the current document.
         (fast-format stream " HREF=#~A" local-reference)))
  (cond-every 
    (tag ;; "A tag anchor."
      (html2::%write-command-key-arg stream "NAME" tag))
    (target ;;target window Netscape 2.0
      (%write-target-window-command-key-arg stream target))
    (events ;; Netscape's Javascript events
      (dolist (event events)
        (funcall event stream)))
    (relation ;; "The relationship the anchored document has to this one."
      (html2::%write-command-key-arg stream "REL" relation)) 
    (inverse ;; "The reverse relationship type, and the inverse of REL."
      (html2::%write-command-key-arg stream "REV" inverse))
    (urn ;;  "URN for a anchored document retrieved by the anchor."
      (html2::%write-command-key-arg stream "URN" urn))
    (title ;;  "Title to use for otherwise untitled anchored document."
      (html2::%write-command-key-arg stream"TITLE" title))
    (methods ;;  "Comma separated list of HTTP methods supported by the anchored  object."
      (html2::%write-command-key-arg stream"METHODS" methods))))

(defun %note-anchor (stream text reference local-reference tag target events relation inverse urn title methods)
  (%with-environment ("A" :string-for-null-stream :inline :fresh-line nil :stream stream)
                     (%write-anchor-command-arguments stream reference local-reference tag
                                                      target events relation inverse urn title methods)
    (write-string text stream)))

(declaim (notinline %write-anchor-command-arguments))

(declaim (inline note-anchor))

(define note-anchor (text &key reference local-reference tag target events relation inverse urn title methods (stream *output-stream*))
  "Notes a hypertext anchor for TEXT on STREAM.
   REFERENCE is a URL (object or string).
   LOCAL-REFERENCE is reference to a tag local to the current page.
   TAG is a string denoting a named point in the document which can be referenced with a local reference (#name).
   TARGET is a string naming a window on the client. When the user clicks on the anchor, the URL is displayed
   in the window named by the target argument.
   EVENTS is a list of events to be attached to the anchor.
   RELATION is the link type from the referring document to the the anchored document.
   INVERSE is the inverse relationship back from the anchored document to the referring document.
   URN is the URN for a anchored document retrieved by the anchor.
   TITLE is title to use for otherwise untitled anchored document.
   METHODS are a comma separated list of HTTP methods supported by the anchored  object."
  (%note-anchor stream text reference local-reference tag target events relation inverse urn title methods))

(define-macro with-anchor-noted ((&key reference local-reference tag target events (stream '*output-stream*)
                                       relation inverse urn title methods) &body body)
  "Like NOTE-ANCHOR except forms in BODY can compute and write the anchored text on STREAM.
   REFERENCE is a URL (object or string).
   LOCAL-REFERENCE is reference to a tag local to the current page.
   TAG is a string denoting a named point in the document which can be referenced with a local reference (#name).
   TARGET is a string naming a window on the client. When the user clicks on the anchor, the URL is displayed
   in the window named by the target argument.
   EVENTS is a list of events to be attached to the anchor.
   RELATION is the link type from the referring document to the the anchored document.
   INVERSE is the inverse relationship back from the anchored document to the referring document.
   URN is the URN for a anchored document retrieved by the anchor.
   TITLE is title to use for otherwise untitled anchored document.
   METHODS are a comma separated list of HTTP methods supported by the anchored  object."
  `(locally
     (declare (notinline %write-anchor-command-arguments))
     (let ((stream ,stream))
       (%with-environment
         ("A" :string-for-null-stream :inline :stream stream :fresh-line nil)
         (%write-anchor-command-arguments stream ,reference ,local-reference
                                          ,tag ,target ,events ,relation ,inverse ,urn ,title ,methods)
         ,@body))))

(define declare-base-reference (&key reference target (stream *output-stream*))
  "Declares REFERENCE to be the base URL for a html page.
   Also, declares TARGET to be the default target window.
   This should appear once in the headers section of a document."
  (declare (notinline %write-command-key-arg))
  (%issue-command ("BASE" stream :fresh-line t :trailing-line t)
    (cond-every
      (reference
        (html2::%write-command-key-arg stream "HREF" (coerce-url-string reference t)))
      (target
        (%write-target-window-command-key-arg stream target)))))


;;;------------------------------------------------------------------- 
;;;
;;; WITH-DOCUMENT-BODY -- UPDATE FOR EVENTS
;;;

(defun body-arguments (stream background-url background foreground link visited-link active-link events)
  (labels ((%body-arg-value (value url-ok-p)
             (if url-ok-p
                 `(typecase ,value
                    (keyword (background-url ,value))
                    (t (url:coerce-url-string ,value nil nil)))
                 `(color-mapping ,value)))
           (clause (stream arg value &optional url-ok-p)
             (typecase value
               (null nil)
               (keyword
                 `((%write-command-key-arg ,stream ,(%body-arg-key arg) ,(if url-ok-p (background-url value) (color-mapping value)))))
               (t `((let ((.val. ,value))
                      (when .val.
                        (%write-command-key-arg ,stream ,(%body-arg-key arg) ,(%body-arg-value '.val. url-ok-p)))))))))
    (when (or background-url background foreground link visited-link active-link events)
      `(progn
         ,.(clause stream 'background-url background-url t)
         ,.(clause stream 'background background)
         ,.(clause stream 'foreground foreground)
         ,.(clause stream 'link link)
         ,.(clause stream 'visited-link visited-link)
         ,.(clause stream 'active-link active-link)
         ,.(when events
             `((dolist (event ,events)
                 (funcall event ,stream))))))))

(define-macro with-document-body ((&key background-url background foreground link visited-link active-link events
                                        (stream '*output-stream*)) &body body)
  "Asserts the contents of BODY are the body of the document (see BODY).

  BACKGROUND-URL   -- an image URL to use as the background 
  BACKGROUND       -- a keyword denoting the appropriate built-in color.
  FOREGROUND       -- color keyword denoting the foreground.
  LINK             -- color keyword denoting the link.
  VISITED-LINK     -- color keyword denoting the visited-link.
  ACTIVE-LINK      -- color keyword denoting the active-link.
  EVENTS           -- JavaScript events See: WITH-EVENT-HANDLERS

  See the variable *BUILT-IN-CLIENT-COLORS* for a complete list of colors 
  built into the client. For information on how to use these, 
  see: http://home.netscape.com/assist/net_sites/bg/index.html
  The variable *built-in-backgrounds* contains a list of backgrounds
  and please use BACKGROUND-URL to map the keyword into the URL. 
  Note that Background URLs must be specified as either URL strings or 
  interned URLS because keywords are interpreted as referring to built-in 
  colors. For a sampling of backgrounds, 
  see: http://home.netscape.com/assist/net_sites/bg/backgrounds.html"
   `(%with-environment
        ("BODY" :stream ,stream)
        ,(body-arguments stream background-url background foreground link visited-link active-link events)
        ,@body))


;;;------------------------------------------------------------------- 
;;;
;;; DOCUMENT FRAMESETS AND FRAMES
;;;

(defun write-frameset-arguments (stream rows columns events)
  (flet ((make-frame-arg (spec)
           (flet ((get-elt-string (element)
                    (etypecase element
                      (keyword (ecase element (:wild "*")))
                      (cons
                        (destructuring-bind (type number) element
                          (ecase type
                            (:percentage
                              (unless (and (integerp number) (< 0 number 101))
                                (error "The percentage, ~A, was not an integer between 0 and 100." type))
                              (concatenate 'string (write-to-string number :base 10.) "%"))
                            (:constraint
                              (unless (and (integerp number) (< 0 number))
                                (error "The fraction, ~A, was not an integer greater than 0." type))
                              (cond ((< number 2) "*")
                                    (t (concatenate 'string (write-to-string number :base 10.) "*"))))
                            (:pixel
                              (check-type number integer)
                              (write-to-string number :base 10.))))))))
             (declare (inline get-elt-string))
             (let ((result (loop for entry = spec then (cdr entry)
                                 while entry
                                 collect (get-elt-string (first entry))
                                 when (cdr entry)
                                   collect ",")))
               (declare (dynamic-extent result))
               (apply #'concatenate 'string result)))))
    (cond-every
      (columns
        (%write-command-key-arg stream "COLS" (make-frame-arg columns)))
      (rows
        (%write-command-key-arg stream "ROWS" (make-frame-arg rows)))
      (events
        (dolist (event events)
          (funcall event stream))))))

(define-macro with-document-frameset ((&key rows columns events (stream '*output-stream*)) &body body)
  "Asserts the contents of BODY are a frameset with the document.  

  Frameset allow the client to display multiple URLs that are simultaneously
  visible in different window frames. A frameset document has no document BODY,
  and only document preamable markup may appear before the frameset, or the
  frameset will be ignored by the client.

  NOTE-DOCUMENT-FRAME is used to emit a frame specification in a cell of a
  frameset specification.

  The dimensions of a frame are analogous to a table. It dimensions are implicit
  in the number of elements in the specifications of ROWS and COLUMNS. Omission
  of an element is interpreted as :WILD.

  ROWS and COLUMNS are lists of values characterized the dimension.  Each value
  can be:

  * (:PERCENTAGE integer) An integer between 0 and 100 denoting a percentage.

  * (:CONSTRAINT integer) An integer greater than 0 denoting denoting the 
  fraction of relative space to use.

  * :WILD any remaining space.

  * (:PIXEL integer) An integer denoting fixed size in pixels. This should be 
  used only in rare circumstances because screen sizes on 
  clients vary so much.

  EVENTS can be a list of JavaScript events. See: WITH-EVENT-HANDLERS

  Framesets can be nested inside other framesets, in which case an entire
  frameset appears in the space of an element of the superior frame.

  For further documentation, see:
  http://www.mcom.com/assist/net_sites/frames.html"
  `(%with-environment
     ("FRAMESET" :stream ,stream)
     (write-frameset-arguments ,stream ,rows ,columns ,events)
     ,@body))

#|(with-document-frameset (:rows '((:constraint 2) (:constraint 4) :WILD)
                         :columns '((:PERCENTAGE 20) (:PERCENTAGE 40) :WILD))
  (print 'foo))|# 

(define note-document-frame (&key name reference target margin-width margin-height
                                  (scrolling :auto) (resizable-p t)
                                  (stream *output-stream*))
  "Notes a document frame on STREAM.

  The frame loads its own URL independently of other frames. It can be targeted
  by other frames using its NAME. It can resize itself dynamically in response
  to changes in the size of the visible client area.  The function must be
  called within the scope of a WITH-DOCUMENT-FRAMESET.

         
  NAME is a string denotingname for the frame, 
  but special keywords can be used instead of a string:

  :BLANK  - Always load this link into a new, unnamed window
  :SELF   - Always load this link over yourself
  :PARENT - Always load this link over your parent (becomes self if your at the top)
  :TOP    - Always load this link at top level (becomes self if your at the top)

  REFERENCE is a URL or url-string.
  TARGET is the name of frame to which display should be sent.
  SCROLLING can be :AUTO, T, or NIL
  RESIZABLE-P controls whether the user can change the size of the frame.
  MARGIN-WIDTH is a size in pixels.
  MARGIN-HEIGHT is a size in pixels."
  (flet ((%get-frame-name-arg (name)
             (typecase name
               (string
                 (unless (alpha-char-p (aref name 0))
                   (error "Frame names must begin with an alphabetical character."))
                 name)
               (keyword (special-target-window-name name)))))
    (declare (inline %get-frame-name-arg))
    (%issue-command ("FRAME" stream)
      (cond-every
        (name
          (%write-command-key-arg stream "NAME" (%get-frame-name-arg name)))
        (reference
          (%write-command-key-arg stream "SRC" (coerce-url-string reference t)))
        ((not resizable-p)
         (%write-command-key-arg stream "NORESIZE"))
        (target
          (%write-target-window-command-key-arg stream target))
        (margin-height
          (%write-command-key-arg stream "MARGINHEIGHT" margin-height t))
        (margin-width
          (%write-command-key-arg stream "MARGINWIDTH" margin-width t)))
      (%write-command-key-arg stream "SCROLLING" (ecase scrolling
                                                         (:auto "AUTO")
                                                         ((t) "YES")
                                                         ((nil) "NO"))))))

(define-macro without-frame-capability ((&key (stream '*output-stream*)) &body body)
  "Any HTML emitted within BODY is ignored by frames capable clients but can be used
to provide alternate displays for clients without frames capability."
  `(with-environment ("NOFRAMES" :stream ,stream)
     ,@body))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(eval-when (load eval compile)
  (defconstant *rendition*
               '((:italic . "I")
                 (:bold . "B")
                 (:teletype . "TT")
                 (:heading-1 . "H1")
                 (:heading-2 . "H2")
                 (:heading-3 . "H3")
                 (:heading-4 . "H4")
                 (:heading-5 . "H5")
                 (:heading-6 .  "H6")
                 (:superscript . "SUP")
                 (:subscript . "SUB")
                 (:big . "BIG")
                 (:small . "SMALL")
                 #|(:underline . "U")
                 (:strike . "S")|#)))

(declaim (inline rendition-command))

(defun rendition-command (rendition)
  (or (cdr (assoc rendition *rendition*))
      (error "Unknown type of rendition, ~S." rendition)))

(define-macro with-rendition ((rendition &key (stream '*output-stream*)) &body body)
  "Text output within BODY on STREAM is rendered according to RENDITION,
which can be any of :ITALIC, :BOLD, :TELETYPE, :HEADING-1, :HEADING-2,  :HEADING-3, 
:HEADING-4, :HEADING-5, :HEADING-6, :SUPERSCRIPT, :SUBSCRIPT, :BIG, :SMALL"
  `(with-environment (,(typecase rendition
                         (keyword (rendition-command rendition))
                         (t `(rendition-command ,rendition)))
                      :fresh-line nil :stream ,stream)
     ,@body))

(defconstant *division-alignment-values* '(:left :right :center))

(defun division-alignment-value (alignment)
  (unless (member alignment *division-alignment-values*)
    (error "Unknown alignment, ~S, for a paragraph or division." alignment))
  (symbol-name alignment))

(defun division-class-value (class)
  (symbol-name class))

(define-macro with-division ((class &key (alignment :left) (stream '*output-stream*)) &body body)
  "Establishes a division environment."
  `(%with-environment
     ("DIV" :stream ,stream)
     (progn
       (%write-command-key-arg ,stream "CLASS" (division-class-value ,class))
       (%write-command-key-arg ,stream "ALIGN" (division-alignment-value ,alignment)))
     ,@body))

(declaim (inline %write-make-font-args))

(defun %write-make-font-args (stream size color)
  (cond-every
    (size
      (unless (< 0 size 8)
        (error "SIZE, ~S, is not an integer from 1 through 7." size))
      (%write-command-key-arg stream "SIZE" size t))
    (color
      (%write-command-key-arg stream "COLOR" (color-mapping color t)))))

(define declare-default-font (&key (size 3) color (stream *output-stream*))
  "Declares the base font size from which all relative font size changes are determined.
The default size is 3."
  (%issue-command ("BASEFONT" stream)
    (%write-make-font-args stream size color)))

(define-macro with-font ((&key (size 3) color (stream '*output-stream*)) &body body)
  "Declares the current font size to be SIZE and color to COLOR within BODY."
  `(%with-environment ("FONT" :fresh-line nil :stream ,stream)
                      (%write-make-font-args ,stream ,size ,color)
     ,@body))


;;;------------------------------------------------------------------- 
;;;
;;; CLIENT-SIDE IMAGE MAPS
;;;

;; see http://www.mcom.com/assist/net_sites/html_extensions_3.html"
;; see also http://www.spyglass.com/techspec/img_maps.html
(define-macro with-client-image-map ((name &key (stream '*output-stream*)) &body body)
  "Establishes a client-side image map environment.
Use CLIENT-IMAGE-AREA to specify image area and mappings to URLs.
Whenever successive calls to CLIENT-IMAGE-AREA declare regions
with overlapping coordinates, the first one takes precedence.
Regions not covered by an area are interpreted as dead areas."
  `(%with-environment ("MAP" :stream ,stream)
                      (%write-command-key-arg ,stream "NAME" ,name)
     ,@body))

;: circle takes center-x, center-y, radius-x
;; polygon takes a series of coordinate pairs, x1,y1,x2,y2,...
;; Cirlce and polygon are implemented by Spy glass, but netscape   10/1/95 -- JCMa.
(defparameter *client-side-image-map-shapes-alist* '((:rectangle "RECT" 4)
                                                     (:circle "CIRCLE" 3)
                                                     (:polygon "POLYGON")))

(define client-image-area (shape coordinates &key reference alternative-text target (stream *output-stream*))
  "Asserts a hot region in a client-side image map on STREAM.
This should be used with the environment of a WITH-CLIENT-IMAGE-MAP.

SHAPE can be :RECTANGLE, which accepts the COORDINATES (left top right bottom)
coordinates use a zero origin and are in pixels of the image displayed.

REFERENCE is the URL associated with the area or null for dead areas.
This argument can be a string or a URL objects. URL object are fully
specified. Relative URLs can be provided as strings. They are defaulted by
the client according to the document (URL) in which the client-side image map
is specified.  When a base declaration is present in the that document (URL),
it is used for defaulting relative URLs.

ALTERNATIVE-TEXT is the text to display when the image is not loaded."
  (flet ((get-shape-arg (shape coordinates)
           (let ((entry (assoc shape *client-side-image-map-shapes-alist*)))
             (cond (entry
                    (destructuring-bind (shape-arg &optional coordinate-num) (cdr entry)
                      (unless (or (eq :polygon shape)
                                  (= (length coordinates) coordinate-num))
                        (error "Wrong number of coordinates: ~A requires ~D, but coordinates is ~S."
                               shape coordinate-num coordinates))
                      (unless (every #'integerp coordinates)
                        (error "Non-numeric coordinates provided in ~S." coordinates))
                      shape-arg))
                   (t (error "Unknown shape, ~S, for a client side area." shape)))))
         (get-coordinate-arg (coordinates)
           (loop for (idx . more) = coordinates then more
                 while idx
                 collect (write-to-string idx :base 10.) into result
                 when more
                   collect "," into result
                 finally (return (apply #'concatenate 'string result)))))
    (declare (inline get-shape-arg get-coordinate-arg))
    (%issue-command ("AREA" stream :fresh-line t)
      (%write-command-key-arg stream "SHAPE" (get-shape-arg shape coordinates))
      (%write-command-key-arg stream "COORDS" (get-coordinate-arg coordinates))
      (if reference
          (%write-command-key-arg stream "HREF" (coerce-url-string reference))
          (%write-command-key-arg stream "NOHREF"))
      (when alternative-text
        (check-type alternative-text string)
        (%write-command-key-arg stream "ALT" alternative-text))
      (when target
        (%write-target-window-command-key-arg stream target)))))


(defmethod client-side-area-shape ((shape shape)) :rectangle)

;; it would be better if we knew the correct positions and did not have to
;; guess. CVince job!  10/7/95 -- JCMa.
(defmethod client-side-area-coordinates ((shape shape))
  (with-slots (point1 point2) shape
    (with-point-coordinates (point1 point2)
      ;; origin is upper left
      (list (min x1 x2)                         ;left
            (min y1 y2)                         ;top
            (max x1 x2)                         ;right
            (max y1 y2)))))                     ;bottom

(defmethod client-side-area-shape ((circle circle)) :circle)

(defmethod client-side-area-coordinates ((circle circle))
  (with-slots (center radius) circle
    (multiple-value-bind (x y)
        (point-coordinates center)
      ;; origin is upper left
      (list x y radius))))

(defmethod client-side-area-shape ((polygon polygon)) :polygon)

(defmethod client-side-area-coordinates ((polygon polygon))
  (with-slots (point-list) polygon
    (loop with x and y
          for point in point-list
          do (multiple-value-setq (x y) (point-coordinates point))
          collect x
          collect y)))

(define-generic write-client-side-area (region &optional stream target))

(defmethod write-client-side-area ((region region) &optional (stream *output-stream*) target)
  (with-slots (bounding-shape destination) region
    (let ((coordinates (client-side-area-coordinates bounding-shape))
          (keyword (client-side-area-shape bounding-shape)))
      (declare (dynamic-extent coordinates))
      (client-image-area keyword coordinates :reference destination :target target :stream stream))))

(define-generic write-client-side-image-map (image-map-or-url name &key target stream width height)
  (:documentation "Writes a client-side image map from a server side image map or a URL serving such a map."))

(defmethod write-client-side-image-map ((image-map image-map-data) name &key target (stream *output-stream*) width height)
  (with-slots (url-default region-list) image-map
    (with-string-for-null-stream (stream)
      (with-client-image-map (name :stream stream)
        ;; enumerate the regions
        (dolist (region region-list)
          (write-client-side-area region stream target))
        ;; write the default area
        (when (and url-default width height)
          (let ((coordinates (list 0 0 (1- (the fixnum width)) (1- (the fixnum height)))))
            (declare (dynamic-extent))
            (client-image-area :rectangle coordinates :reference url-default :target target :stream stream)))))))

(defmethod write-client-side-image-map ((url url:http-searchable-object) name &key target (stream *output-stream*) width height)
  (write-client-side-image-map
    (url:search-database url) name
    :target target :stream stream :width width :height height))

(defmethod write-client-side-image-map (thing name &key target stream width height)
  (declare (ignore name target stream width height))
  (error "No method is implemented for generating a client side image map from a ~S like ~S."
         (type-of thing) thing))

;;;------------------------------------------------------------------- 
;;;
;;; IMAGE EXTENSIONS
;;;

;; html spec says to always provide alternative text
(defun %note-image (stream image-url alternative-text alignment accept-coordinates-at-url
                           client-side-image-map
                           border vertical-space horizontal-space width height)
  (flet ((write-element (stream image-url alignment alternative-text accept-coordinates-at-url)
           (flet ((alignment-value (alignment)
                    (unless (member alignment *image-alignment-values*)
                      (error "Unknown alignment, ~S, for an image." alignment))
                    (symbol-name alignment))
                  (write-integer-arg (stream option value)
                    (check-type value integer)
                    (%write-command-key-arg stream option value t)))
             (declare (inline alignment-value write-integer-arg))
             ;; Automagically insert image sizes when algorithms available.
             (when (and image-url (not (or width height)) http:*image-sizes-default-automatically*)
	       (multiple-value-setq (width height)
		 (url:image-size image-url)))
             (%issue-command ("IMG" stream)
               (cond-every
                 (image-url
                   (%write-command-key-arg stream "SRC" image-url))
                 (alignment
                   (%write-command-key-arg stream "ALIGN" (alignment-value alignment)))
                 (alternative-text
                   (check-type alternative-text string)
                   (%write-command-key-arg stream "ALT" alternative-text))
                 (accept-coordinates-at-url (%write-command-key-arg stream "ISMAP"))
                 (client-side-image-map
                   (%write-command-key-arg
                     stream "USEMAP" (url:name-string-without-search-suffix client-side-image-map nil)))
                 (border (write-integer-arg stream "BORDER" border))
                 (vertical-space (write-integer-arg stream "VSPACE" vertical-space))
                 (horizontal-space (write-integer-arg stream "HSPACE" horizontal-space))
                 (width (write-integer-arg stream "WIDTH" width))
                 (height (write-integer-arg stream "HEIGHT" height)))))))
    (declare (inline write-element))
    (let ((url-string (url:name-string-without-search-suffix image-url nil)))
      (declare (dynamic-extent url-string))
      (case accept-coordinates-at-url
        ((nil :no-url)
         (write-element stream url-string alignment alternative-text accept-coordinates-at-url))
        (t (with-anchor-noted (:reference (if (eq accept-coordinates-at-url t)
                                              url-string
                                              (url:name-string-without-search-suffix accept-coordinates-at-url nil))
                               :stream stream)
             (write-element stream url-string alignment alternative-text accept-coordinates-at-url)))))))

(declaim (inline image))

(define image (image-url alternative-text
                         &key (alignment :left) accept-coordinates-at-url
                         client-side-image-map
                         border vertical-space horizontal-space width height
                         (stream *output-stream*))
  (%note-image stream image-url alternative-text alignment accept-coordinates-at-url
               client-side-image-map
               border vertical-space horizontal-space width height))

(setf (documentation 'image 'function)
      "IMAGE-URL is the URL for an image and ALTERNATIVE-TEXT is the text to display when the image
   is not loaded.

  ACCEPT-COORDINATES-URL can be:
   
                * URL to which coordinates will be returned when the user
                clicks on the image.

                * T, indicating that returned coordinates should go to a
                search URL version of IMAGE-URL
                
                * :NO-URL, indicating not to emit an anchor for accepting the
                returns but to mark the IMG as a coordinate search.

   CLIENT-SIDE-IMAGE-MAP indicates the client side image map to use.
   Normally, this is a named URL (/url.html#map-name) and often, all
   client side image maps are served from a single url. The function
   NETSCAPE2.0:WRITE-CLIENT-SIDE-IMAGE-MAP writes a client side image
   map from a server-side image map URL (CERN or NCSA formats).

   ALIGNMENT can be:

        HTML2 Arugments

                TOP    -- align with the tallest item on the line.
                MIDDLE -- align with the baseline with the middle of the image.
                BOTTOM -- align with the baseline of the current line with the image.
        
        Text Flow Options

                LEFT -- float down and over to the next available space on
                the left margin, and subsequent text wraps around the right
                side of the image.

                RIGHT -- float down and over to the next available space on
                the right margin, and subsequent text wraps around the left
                side of the image.

        Semi-Random Options

                TEXTTOP -- align the image top with the top of the current
                line.

                ABSMIDDLE -- aling the middle of the image with the middle of
                the current line.

                ABSBOTTOM -- align the image bottom with the bottom of the
                current line.

    BORDER is an integer indicating the thickness of the border with which to
    surround the image.

    VERTICAL-SPACE is an integer indicating the amount of vertical space
    above and below a floating image.

    HORIZONTAL-SPACE is an integer indicating the amount of horizontal space
    above and below a floating image.

    Allow browsers to layout the display before the image has loaded and thus
    eliminate the delay for the user otherwise incurred.

   WIDTH is width of the image in pixels.

   HEIGHT is the height of the image in pixels.")

(html2::define-input-type
  multi-line-text
  :type-arg "TEXTAREA"
  :lisp-type 'string)

(defconstant *multi-line-text-wrap-modes* '(:virtual :physical))

(defmethod accept-input ((multi-line-text multi-line-text) query-name &rest args &key (stream *output-stream*) )
  (with-slots (html2::type-arg) multi-line-text
    (destructuring-bind (&key (rows 5.) (columns 72.) wrap default &allow-other-keys) args
      (check-type rows integer)
      (check-type columns integer)
      (when default
        (html2::check-value-type multi-line-text default))
      (%with-environment (html2::type-arg :stream stream :fresh-line t)
                         (progn
                           (%write-command-key-arg stream "NAME" query-name)
                           (%write-command-key-arg stream "ROWS" rows t)
                           (%write-command-key-arg stream "COLS" columns t)
                           (cond ((null wrap) nil)
                                 ((member wrap *multi-line-text-wrap-modes*)
                                  (%write-command-key-arg stream "WRAP" (symbol-name wrap)))
                                 (t (error "~S is not one of the known arguments for :WRAP: ~S"
                                           wrap *multi-line-text-wrap-modes*))))
        (when default
          (write-string default stream))))))

(defun write-form-command-args (stream action value &optional (encoding-type '(:application :x-www-form-urlencoded))
                                       name target events )
  (let ((encoding-type-string (http::write-mime-content-type encoding-type)))
    (declare (dynamic-extent encoding-type-string))
    (when name
      (%write-command-key-arg stream "NAME" name))
    (ecase action
      (:post
        (%write-command-key-arg stream "ACTION" (coerce-url-string value))
        (%write-command-key-arg stream "METHOD" "POST")
        (%write-command-key-arg stream "ENCTYPE" encoding-type-string)
        (when target
          (%write-target-window-command-key-arg stream target)))
      (:mail
        (check-type value string)
        (unless (http::char-position #\@ value)
          (error "Ill-formed email address, ~S." value))
        (let ((url (concatenate 'string "mailto:" value)))
          (declare (dynamic-extent url))
          (%write-command-key-arg stream "ACTION" url)
          (%write-command-key-arg stream "ENCTYPE" encoding-type-string)))
      (:get
        (%write-command-key-arg stream "ACTION" (coerce-url-string value))
        (%write-command-key-arg stream "METHOD" "GET")
        (%write-command-key-arg stream "ENCTYPE" encoding-type-string))
      (:none))
    (dolist (event events)
      (html2::%write-input-type-event-arg stream event))))

(define-macro with-fillout-form ((action url &key name (stream '*output-stream*)
                                         (encoding-type ''(:application :www-form-url-encoded))
                                         target events) &body body)
  "Establishes an fillout-form environment.  
ACTION is either :POST, :MAIL, :GET, :NONE.
URL is the URL to which the form values are returned.
NAME is a name identifying the form element.
TARGET names the window in a frame to which the results of form submission are returned.
If ACTION is :NONE, TARGET can be NIL.
EVENTS is a list of client-side events processed when the form is submitted.

ENCODING-TYPE is MIME content type to use when return the form values to URL.
ENCODING-TYPE defaults to application/x-www-form-URLencoded.
See ACCEPT-INPUT for documentation on client-side events.             
:GET should only be used in exceptional circumstances as not only is
it considered obsolete but it also is limited to 1024 characters 
including the rest of the the URL URL.

When using the :POST action, the encoding type is normally (:application :www-URL-form-encoded).
The encoding type (:multipart :form-data) can be used to allow the user to
post data from a file.  The input types FILE-UPLOAD and SUBMIT-FILE-BUTTON
are available for this purpose, but
with-fillout-form must be invoked with compatible arguments."
  `(cond (*open-form*
          (error "HTML does not allow nesting of forms."))
         (t (%with-environment ("FORM" :fresh-line t :stream ,stream)
                               (write-form-command-args ,stream ,action ,url ,encoding-type
                                                        ,name ,target ,events)
              (let ((*open-form* t))
                ,@body)))))


;;;------------------------------------------------------------------- 
;;;
;;; JAVA APPLET SUPPORT
;;;
;; http://www.javasoft.com/JDK-prebeta1/converting/#html
;; http://www.javasoft.com/JDK-prebeta1/filesinkit/README

(define note-java-parameter (name value &optional (stream *output-stream*))
  "Writes a parameter for a Java Applet.  NAME is a string or symbol
giving the attribute name. VALUE is a string holding the value of the parameter
or a function that writes the parameter value as an escaped string on STREAM."
  (%issue-command ("PARAM" stream :fresh-line t :trailing-line t)
    (%write-command-key-arg stream "NAME" name)
    (%write-command-key-arg stream "VALUE" value)))

(defun %write-java-applet-args (stream code width height alignment name base-code-url
                                       horizontal-space vertical-space alternate-text)
  (check-type width integer)
  (check-type height integer)
  (unless (member alignment *image-alignment-values*)
    (error "Alignment, ~S, is not one of the known values, ~S."
           alignment *image-alignment-values*))
  (%write-command-key-arg stream "CODE" code)
  (%write-command-key-arg stream "WIDTH" width t)
  (%write-command-key-arg stream "HEIGHT" height t)
  (%write-command-key-arg stream "ALIGN" (symbol-name alignment))
  (cond-every
    (name
      (%write-command-key-arg stream "NAME" name))
    (base-code-url
      (%write-command-key-arg stream "CODEBASE" (coerce-url-string base-code-url)))
    (horizontal-space
      (check-type horizontal-space integer)
      (%write-command-key-arg stream "HSPACE" horizontal-space t))
    (vertical-space
      (check-type vertical-space integer)
      (%write-command-key-arg stream "VSPACE" vertical-space t))
    (alternate-text
      (%write-command-key-arg stream "ALT" alternate-text))))

(define-macro with-java-applet ((code width height alignment
                                      &key parameters name base-code-url horizontal-space vertical-space
                                      alternate-text (stream '*output-stream*)) &body body)
  `(%with-environment
     ("APPLET" :stream ,stream)
     (%write-java-applet-args ,stream ,code ,width ,height ,alignment
                              ,name ,base-code-url ,horizontal-space ,vertical-space
                              ,alternate-text)
     ,.(when parameters
         `((loop for (param value) in ,parameters
                 do (note-java-parameter param value ,stream))))
     ,@body))

(setf (documentation 'with-java-applet 'function)
      "Writes invocation of a Java applet of code, CODE, on STREAM.  This macro is
intended for display-oriented applets embedded in HTML documents. BODY may emit
HTML for display on browser not supporting Java. 

The Java applet must be exported for a client to invoke it with this form.
The export type :JAVA-BINARY exports byte compiled Java binaries.  :JAVA-FILE
may be used to provide access to the Java source code.

Applet Parameters 

     PARAMETERS is an alist of (name value). When supplied, these are
     automatically transmitted to the applet.  Alternatively,
     NOTE-JAVA-PARAMETER can be called within BODY.



Required Arguments

     CODE is an appletFile giving the name of the file that contains the
     applets compiled Applet subclass.  This file is relative to the base URL
     of the applet.  It cannot be absolute.

     WIDTH is the initial width in pixels of the applet display
     area, not counting any windows or dialogs that the applet brings up.

     HEIGHT is the corresponding height in pixels.

     ALIGNMENT can be:

        HTML2 Arguments

                TOP    -- align with the tallest item on the line.
                MIDDLE -- align with the baseline with the middle of the image.
                BOTTOM -- align with the baseline of the current line with the image.
        
        Text Flow Options

                LEFT -- float down and over to the next available space on
                the left margin, and subsequent text wraps around the right
                side of the image.

                RIGHT -- float down and over to the next available space on
                the right margin, and subsequent text wraps around the left
                side of the image.

        Semi-Random Options

                TEXTTOP -- align the image top with the top of the current
                line.

                ABSMIDDLE -- align the middle of the image with the middle of
                the current line.

                ABSBOTTOM -- align the image bottom with the bottom of the
                current line.

Optional Arguments

     base-code-url is a URL specifying the base URL of the applet -- the directory
     that contains the applets code.  If this attribute is not specified, then
     the documents URL is used.

     NAME is a string specifying a name for the applet instance, which makes
     it possible for applets on the same page to find (and communicate with)
     each other.

     HORIZONTAL-SPACE is an integer specifying the margin on each side of the
     applet.

     VERTICAL-SPACE is an integer specifying the margin above and below the
     applet.

     ALTERNATE-TEXT is a string specifying any text that should be displayed
     if the browser understands the APPLET tag but can't run applets written in
     the Java(tm) Programming Language.")


;;;------------------------------------------------------------------- 
;;;
;;; LIVE SCRIPT SUPPORT
;;;

(defparameter *embedded-script-languages* '((:java-script . "JavaScript")
                                            (:java-script1.1 . "JavaScript1.1")
                                            (:java-script1.2 . "JavaScript1.2")
                                            (:live-script . "LiveScript")
                                            (:talker . "Talker"))
  "The known scripting languages that Netscape 2.0 embedded in HTML.")

(defun get-script-language (language)
  (let ((entry (assoc language *embedded-script-languages*)))
    (cond (entry (cdr entry))
          (t (error "~S is not one of the known scripting languages, ~S" language (mapcar #'car *embedded-script-languages*))))))

(define-macro with-embedded-script ((language &key base-code-url (stream '*output-stream*)) &body body)
  "Emits HTML to embed a script in LANGUAGE on STREAM.
BODY contains the source text for the script.
LANGUAGE can be :JAVA-SCRIPT
When BASE-CODE-URL is provided, it should refer to a URL that loads the
text of the script. In this case, BODY will no be evaluated.
JavaScript source code can be exported using the export-tyle :java-script-file
and the extension JavaScript.
Further information is available from:
http://home.netscape.com/comprod/products/navigator/version_2.0/script/script_info/index.html"
  (let ((n-body (when body
                  `((fresh-line ,stream)
                    (with-comment (:stream ,stream)
                      (write-string "hide script from old browsers that don't implement JavaScript" ,stream)
                      (terpri ,stream)
                      ,@body
                      (fresh-line ,stream)
                      (write-string "// end hiding from old browsers" ,stream))
                    (terpri ,stream)))))
    (if base-code-url
        `(let ((code-url ,base-code-url))
           ,(if n-body
                `(%with-environment
                   ("SCRIPT" :stream ,stream)
                   (progn
                     (%write-command-key-arg ,stream "LANGUAGE" (get-script-language ,language))
                     (when code-url
                       (%write-command-key-arg ,stream "SRC" (coerce-url-string code-url))))
                   ,.n-body)
                `(%issue-command ("SCRIPT" ,stream :fresh-line t :trailing-line t)
                   (%write-command-key-arg ,stream "LANGUAGE" (get-script-language ,language))
                   (when code-url
                     (%write-command-key-arg ,stream "SRC" (coerce-url-string code-url))))))
        `(%with-environment
           ("SCRIPT" :stream ,stream)
           (%write-command-key-arg ,stream "LANGUAGE" (get-script-language ,language))
           ,.n-body))))

(define embed-object (object-url &rest extension-keys &key height width (stream '*output-stream*) &allow-other-keys)
  "Emits HTMl to embed the object referred to by object-url on STREAM.
This facility allows the insertion of arbitrary objects directly into an HTML page. 
Embedded objects are supported by application-specific plug-ins.  When present, 
the output image will be scaled to fit the specified HEIGHT and WIDTH.
Extension keywords and values for specific plug-ins or embedded object can be supplied."
  (%issue-command ("EMBED" stream)
    (%write-command-key-arg stream "SRC" (coerce-url-string object-url))
    (cond-every
      (height
        (%write-command-key-arg stream "HEIGHT" height t))
      (width
        (%write-command-key-arg stream "WIDTH" width t)))
    ;; emit any extension keywords provided by the user.
    (loop for (key value) on extension-keys by #'cddr
          unless (member key '(:height :width :stream))
            do (%write-command-key-arg stream (symbol-name key) (or value :no-value) (integerp value)))))

