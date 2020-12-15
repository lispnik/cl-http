;;;   -*- Mode: lisp; Package: netscape3.0; BASE: 10; Syntax: ansi-common-lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-

;;;
;;; (c) Copyright 1996-97, John C. Mallery
;;;     All Rights Reserved.
;;;


;;;------------------------------------------------------------------- 
;;;
;;; INTERFACE FOR AUTHORING HTML USING NETSCAPE 3.0 EXTENSIONS
;;;
;;; Implemented   7/13/96 -- JCMa.
;;; from http://home.netscape.com/eng/mozilla/3.0/relnotes/mac-3.0b5Gold.html#Layout
;;;
;;; Items not implemented
;;;
;;; * JavaScript evaluation for HTML attribute value 

;;;------------------------------------------------------------------- 
;;;
;;; PACKAGE DEFINITION
;;;

(eval-when (load eval compile)
  (let ((html-pkg (find-package :ns2.0))
        (netscape-pkg (find-package :netscape3.0)))
    (do-external-symbols (sym html-pkg)
      (export (intern (symbol-name sym) netscape-pkg) netscape-pkg))))  ;close eval-when

(in-package :netscape3.0)


;;;------------------------------------------------------------------- 
;;;
;;; DOCUMENT FRAMESETS AND FRAMES
;;;

(defun write-frameset-arguments (stream rows columns frame-border border-color border events)
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
      ((null frame-border)
       (%write-command-key-arg stream "FRAMEBORDER" "NO"))
      (border
        (%write-command-key-arg stream "BORDER" border t))
      (border-color
        (%write-command-key-arg stream "BORDERCOLOR" (color-mapping border-color)))
      (events
        (dolist (event events)
          (funcall event stream))))))

(define-macro with-document-frameset ((&key rows columns border border-color (frame-border t)
                                            events (stream '*output-stream*)) &body body)
  "Asserts the contents of BODY are a frameset with the document.  

  Frameset allow the client to display multiple URLs that are simultaneously
  visible in different window frames. A frameset document has no document
  BODY, and only document preamable markup may appear before the frameset, or
  the frameset will be ignored by the client.

  NOTE-DOCUMENT-FRAME is used to emit a frame specification in a cell of a
  frameset specification.

  The dimensions of a frame are analogous to a table. It dimensions are
  implicit in the number of elements in the specifications of ROWS and
  COLUMNS. Omission of an element is interpreted as :WILD.

  ROWS and COLUMNS are lists of values characterized the dimension.  Each
  value can be:

  * (:PERCENTAGE integer) An integer between 0 and 100 denoting a percentage.

  * (:CONSTRAINT integer) An integer greater than 0 denoting denoting the
  fraction of relative space to use.

  * :WILD any remaining space.

  * (:PIXEL integer) An integer denoting fixed size in pixels. This should be
  used only in rare circumstances because screen sizes on clients vary so
  much.

  FRAME-BORDER is a boolean argument that controls whether all frames in the
  frameset have borders or not.

  BORDER is an integer globally sets the thickness in pixels of frame borders
  within the scope of the frameset.

  BORDER-COLOR is globally set the color of frame borders within the scope of
  the frameset.

  EVENTS can be a list of JavaScript events. See: WITH-EVENT-HANDLERS

  Framesets can be nested inside other framesets, in which case an entire
  frameset appears in the space of an element of the superior frame.

  For further documentation, see:
  http://www.mcom.com/assist/net_sites/frames.html"
  `(%with-environment
     ("FRAMESET" :stream ,stream)
     (write-frameset-arguments ,stream ,rows ,columns ,frame-border ,border-color ,border ,events)
     ,@body))

(define note-document-frame (&key name reference target margin-width margin-height (frame-border t)
                                  border border-color (scrolling :auto) (resizable-p t) events
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
  MARGIN-HEIGHT is a size in pixels.
  FRAME-BORDER is a boolean argument that controls whether the frame has
  borders or not.
  BORDER is an integer that sets the thickness in pixels of frame's borders.
  BORDER-COLOR is sets the color of frame's borders."
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
          (%write-command-key-arg stream "SRC" (coerce-url-string reference)))
        ((not resizable-p)
         (%write-command-key-arg stream "NORESIZE"))
        (target
          (%write-target-window-command-key-arg stream target))
        (margin-height
          (%write-command-key-arg stream "MARGINHEIGHT" margin-height t))
        (margin-width
          (%write-command-key-arg stream "MARGINWIDTH" margin-width t))
        ((null frame-border)
         (%write-command-key-arg stream "FRAMEBORDER" "NO"))
        (border
          (%write-command-key-arg stream "BORDER" border t))
        (border-color
          (%write-command-key-arg stream "BORDERCOLOR" (color-mapping border-color)))
        (events
          (dolist (event events)
            (html2::%write-input-type-event-arg stream event))))
      (%write-command-key-arg stream "SCROLLING" (ecase scrolling
                                                         (:auto "AUTO")
                                                         ((t) "YES")
                                                         ((nil) "NO"))))))


;;;------------------------------------------------------------------- 
;;;
;;; MULTICOLUMN TEXT
;;;

(define-macro with-multiple-columns ((columns &key width gutter (stream '*output-stream*)) &body body)
  "Establishes an environment STREAM in which HTML is displayed in multiple
columns.  COLUMNS is the numberof columns. WIDTH controls the width of an
individual column. GUTTER is the number of pixels between columns. This macro
maybe reentered to further subdivide columns."
  (flet ((make-args (stream columns width gutter)
           (let ((args nil)
                 (constant-args nil))
             (cond-every
               (gutter
                 (if (integerp gutter)
                     (push `(%write-command-key-arg ,stream "GUTTER" ,gutter t) constant-args)
                     (push `(,gutter (%write-command-key-arg ,stream "GUTTER" ,gutter t)) args)))
               (width
                 (if (integerp width)
                     (push `(%write-command-key-arg ,stream "WIDTH" ,width t) constant-args)
                     (push `(,width (%write-command-key-arg ,stream "WIDTH" ,width t)) args))))
             `(progn
                (%write-command-key-arg ,stream "COLS" ,columns t)
                ,.constant-args
                ,.(when args
                    `((cond-every ,.args)))))))
    `(%with-environment
       ("MULTICOL" :stream ,stream)
       ,(make-args stream columns width gutter)
       ,@body)))


;;;------------------------------------------------------------------- 
;;;
;;; HORIZONTAL & VERTICAL SPACING
;;;

(define note-spacing (type &key size width height alignment (stream *output-stream*))
  "Provides control over whitespace.

  TYPE can be: :HORIZONTAL, :VERTICAL OR :BLOCK.

  SIZE controls the extent in pixels when TYPE is :HORIZONTAL or :VERTICAL.

  WIDTH and HEIGHT control rectangular area in pixels when TYPE is :BLOCK.

  ALIGNMENT is relevant when TYPE is :BLOCK, and be take on the values>

  HTML2 Arugments

  :TOP    -- align with the tallest item on the line.
  :MIDDLE -- align with the baseline with the middle of the image.
  :BOTTOM -- align with the baseline of the current line with the image.
        
  Text Flow Options

  :LEFT -- float down and over to the next available space on
  the left margin, and subsequent text wraps around the right
  side of the image.

  :RIGHT -- float down and over to the next available space on
  the right margin, and subsequent text wraps around the left
  side of the image.

  Semi-Random Options

  :TEXTTOP -- align the image top with the top of the current
  line.

  :ABSMIDDLE -- aling the middle of the image with the middle of
  the current line.

  :ABSBOTTOM -- align the image bottom with the bottom of the
  current line."
  (%issue-command ("SPACER" stream)
    (case type
      ((:horizontal :vertical)
       (%write-command-key-arg stream "TYPE" (symbol-value type))
       (if size
           (%write-command-key-arg stream "SIZE" size t)
           (error "SIZE was not supplied when type is ~S." type)))
      (:block
        (%write-command-key-arg stream "TYPE" "BLOCK")
        (if width
            (%write-command-key-arg stream "WIDTH" width t)
            (error "WIDTH was not supplied when type is ~S." type))
        (if height
            (%write-command-key-arg stream "HEIGHT" height t)
            (error "HEIGHT was not supplied when type is ~S." type))
        (when alignment
          (unless (member alignment *image-alignment-values*)
            (error "Unknown alignment, ~S, for a a block spacing." alignment))
          (%write-command-key-arg stream "ALIGN" (symbol-name alignment))))
      (t (error "Unknown TYPE, ~S, for NOTE-SPACING." type)))))


;;;------------------------------------------------------------------- 
;;;
;;; RENDITION
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
                 (:plain . "PLAINTEXT")
                 (:superscript . "SUP")
                 (:subscript . "SUB")
                 (:big . "BIG")
                 (:small . "SMALL")
                 (:underline . "U")
                 (:strike . "S"))))

(declaim (inline rendition-command))

(defun rendition-command (rendition)
  (or (cdr (assoc rendition *rendition*))
      (error "Unknown type of rendition, ~S." rendition)))

(define-macro with-rendition ((rendition &key (stream '*output-stream*)) &body body)
  "Text output within BODY on STREAM is rendered according to RENDITION, which
can be any of :BIG, :BOLD, :HEADING-1, :HEADING-2, :HEADING-3, :HEADING-4,
:HEADING-5, :HEADING-6,:ITALIC, :PLAIN, :SMALL, :STRIKE :SUBSCRIPT, :SUPERSCRIPT,
:TELETYPE, :UNDERLINE."
  `(with-environment (,(typecase rendition
                         (keyword (rendition-command rendition))
                         (t `(rendition-command ,rendition)))
                      :fresh-line nil :stream ,stream)
     ,@body))



;;;------------------------------------------------------------------- 
;;;
;;; FONT FACE
;;;

(declaim (inline %write-make-font-args))

(defun %write-make-font-args (stream face size color)
  (cond-every
    (size
      (unless (< 0 size 8)
        (error "SIZE, ~S, is not an integer from 1 through 7." size))
      (%write-command-key-arg stream "SIZE" size t))
    (face
      (typecase face
        (atom (%write-command-key-arg stream "FACE" face))
        (cons
          (%write-command-key-arg stream "FACE" (car face))
          (loop for item in (cdr face)
                do (write-char #\, stream)
                   (write (etypecase item
                            (keyword (symbol-name item))
                            (string item))
                          :escape t :stream stream)))))
    (color
      (%write-command-key-arg stream "COLOR" (color-mapping color t)))))

(define-macro with-font ((&key face size color (stream '*output-stream*)) &body body)
  "Declares the current font size to be SIZE, the font face to be FACE, and the
color to COLOR within BODY."
  `(%with-environment ("FONT" :fresh-line nil :stream ,stream)
                      (%write-make-font-args ,stream ,face ,size ,color)
     ,@body))


;;;------------------------------------------------------------------- 
;;;
;;; HTML 3 TABLES
;;;

(defconstant *table-horizontal-alignments* '(:left :center :right))

(defconstant *table-vertical-alignments* '(:top :middle :bottom :baseline))

(defun %write-table-environment-arguments (stream width horizontal-alignment vertical-alignment background
                                                  &optional no-wrap column-span row-span)
  (macrolet ((check-alignment-arg (alignment possible-alignments)
               `(unless (member ,alignment ,possible-alignments)
                  (error "~S is not one of the possible alignments, ~S, for ~S."
                         ,alignment ,possible-alignments
                         ,(http:symbolize (symbol-name alignment) http:*keyword-package*)))))
    (cond-every
      (width (ns1.1::%write-width-argument width stream))
      (horizontal-alignment
        (check-alignment-arg horizontal-alignment *table-horizontal-alignments*)
        (%write-command-key-arg stream "ALIGN" horizontal-alignment))
      (vertical-alignment
        (check-alignment-arg vertical-alignment *table-vertical-alignments*)
        (%write-command-key-arg stream "VALIGN" vertical-alignment))
      (background
        (%write-command-key-arg stream "BGCOLOR" (color-mapping background)))
      (no-wrap
        (%write-command-key-arg stream "NOWRAP"))
      (column-span
        (%write-command-key-arg stream "COLSPAN" column-span t))
      (row-span
        (%write-command-key-arg stream "ROWSPAN" row-span t)))))

(defun %write-table-arguments (stream height width border cell-spacing cell-padding background)
  (cond-every
    (height (%write-command-key-arg stream "HEIGHT" height t))
    (width (ns1.1::%write-width-argument width stream))
    (border
      (if (integerp border)
          (%write-command-key-arg stream "BORDER" border t)
          (%write-command-key-arg stream "BORDER")))
    (cell-spacing
      (%write-command-key-arg stream "CELLSPACING" cell-spacing t))
    (cell-padding
      (%write-command-key-arg stream "CELLPADDING" cell-padding t))
    (background
      (%write-command-key-arg stream "BGCOLOR" (color-mapping background)))))

(define-macro with-table-row ((&key width horizontal-alignment vertical-alignment background (stream '*output-stream*)) &body body)
  "Asserts that the contents of BODY is a row in a table.
BACKGROUND is the color for the rows background."
  `(%with-environment
     ("TR" :fresh-line t :stream ,stream)
     (%write-table-environment-arguments ,stream ,width ,horizontal-alignment ,vertical-alignment ,background)
     ,@body))

(define-macro with-table-cell ((&key width header-p horizontal-alignment vertical-alignment background (break-lines-p t)
                                     column-span row-span (stream '*output-stream*)) &body body)
  "Asserts that the contents of BODY is a cell withing a table environment
    WIDTH is an integer specifying the absolute length of the line in pixels or
    a float between zero and one indicating the percent of the window with to occupy.
    HEADER-P             controls whether the cell is a header cell or an ordinary cell.
    HORIZONTAL-ALIGNMENT can be any of :LEFT, :CENTER, or :RIGHT.
    VERTICAL-ALIGNMENT   can be any of :TOP, :MIDDLE, :BOTTOM, or :BASELINE.
    BREAK-LINES-P        prevents lines from being broken to fit the width of a cell.
    COLUMN-SPAN          is an integer that controls the number of columns a cell spans.
    ROW-SPAN             is an integer that controls the number of columns a cell spans.
    BACKGROUND           is the color for the cell background."
  `(let ((tag (if ,header-p "TH" "TD")))
     (%with-environment
       (tag :fresh-line nil :stream ,stream)
       (%write-table-environment-arguments ,stream ,width ,horizontal-alignment ,vertical-alignment ,background
                                           (not ,break-lines-p) ,column-span ,row-span)
       ,@body)))

;; Does not implement HSPACE and VSPACE because these seem redundant with WIDTH and HEIGHT.
(define-macro with-table ((&key caption height width border cell-spacing cell-padding background
                                (caption-alignment :top) (caption-size 3) caption-color
                                (caption-rendition :bold) (stream '*output-stream*)) &body body)
  "Establishes a table environment with BODY.

  CAPTION is a string positioned at CAPTION-ALIGNMENT, :either :TOP or
  :BOTTOM. When complete control is required, CAPTION can also be a function
  called with (STREAM). In this case, CAPTION-ALIGNMENT remains active but
  CAPTION-SIZE, CAPTION-COLOR and CAPTION-RENDITION are ignored.

  HEIGHT is an integer specifying the absolute length of the table in pixels.

  WIDTH is an integer specifying the absolute length of the table in pixels or
  a float between zero and one indicating the percent of the window with to
  occupy.

  BORDER is either T, NIL or an integer.

  BACKGROUND is the color for the table background.

  CELL-SPACING is an integer, defaulting to 2, that controls the space
  between cells.

  CELL-PADDING is an integer, defaulting to 1 that controls the space
  between text contained in a cell and the wall."
   `(%with-environment ("TABLE" :fresh-line t :stream ,stream)
                       (%write-table-arguments ,stream ,height ,width ,border ,cell-spacing ,cell-padding ,background)
      ,.(cond (caption
               `((with-caption (:alignment ,caption-alignment :stream ,stream)
                   ,(typecase caption
                      (string
                        `(with-font (:size ,caption-size :color ,caption-color :stream ,stream)
                           (with-rendition (,caption-rendition :stream ,stream)
                             (write-string ,caption ,stream))))
                      (t `(let ((caption ,caption))
                            (etypecase caption
                              (function (funcall caption ,stream))
                              (string
                                (with-font (:size ,caption-size :color ,caption-color :stream ,stream)
                                  (with-rendition (,caption-rendition :stream ,stream)
                                    (write-string caption ,stream)))))))))))
              (t caption-alignment caption-size caption-color caption-rendition ; ignore
                 nil))
      ,@body))


;;;------------------------------------------------------------------- 
;;;
;;; IMAGE EXTENSIONS
;;;

;; html spec says to always provide alternative text
(defun %note-image (stream image-url alternative-text alignment accept-coordinates-at-url
                           client-side-image-map
                           border vertical-space horizontal-space width height events)
  (flet ((write-element (stream image-url alignment alternative-text accept-coordinates-at-url events)
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
                 (height (write-integer-arg stream "HEIGHT" height))
                 (events
                   (dolist (event events)
                     (html2::%write-input-type-event-arg stream event))))))))
    (let ((url-string (url:name-string-without-search-suffix image-url nil)))
      (declare (dynamic-extent url-string))
      (case accept-coordinates-at-url
        ((nil :no-url)
         (write-element stream url-string alignment alternative-text accept-coordinates-at-url events))
        (t (with-anchor-noted (:reference (if (eq accept-coordinates-at-url t)
                                              url-string
                                              (url:name-string-without-search-suffix accept-coordinates-at-url nil))
                               :stream stream)
             (write-element stream url-string alignment alternative-text accept-coordinates-at-url events)))))))

(declaim (inline image))

(define image (image-url alternative-text
                         &key alignment accept-coordinates-at-url
                         client-side-image-map
                         border vertical-space horizontal-space width height events
                         (stream *output-stream*))
  (%note-image stream image-url alternative-text alignment accept-coordinates-at-url
               client-side-image-map
               border vertical-space horizontal-space width height events))

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

   HEIGHT is the height of the image in pixels.

   EVENTS are a set of image related events.")
