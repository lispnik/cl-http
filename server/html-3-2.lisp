;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: html3.2; -*- 
;;;
;;; Copyright John C. Mallery,  1997.
;;; All rights reserved. 
;;;------------------------------------------------------------------- 
;;;
;;; HTML 3.2 GENERATION
;;;
;;;------------------------------------------------------------------- 
;;;
;;; PACKAGE DEFINITION
;;;

(eval-when (load eval compile)
  ;; Make sure we're exporting a compete set of symbols.
  (let* ((ancestor-pkg (find-package :html2))
         (pkg (find-package :html3.2))
         (shadowed-symbols (mapcar #'symbol-name (package-shadowing-symbols pkg))))
    (do-external-symbols (sym ancestor-pkg)
      (let ((name (symbol-name sym)))
        (unless (member name shadowed-symbols :test #'equalp)   ; don't export shadowed symbols
          (export (intern name pkg) pkg))))))   ;close eval-when 

(in-package :html3.2) 

;;;------------------------------------------------------------------- 
;;;
;;;  DOCUMENT LEVEL OPERATIONS
;;;

(defconstant *dtd-version* "-//W3C//DTD HTML 3.2 Final//EN")

(define declare-html-version (&optional (stream *output-stream*))
  "Declares the document type as the current HTML generation DTD.
All HTML 3.2 must declare the document type definition version."
  (%issue-command ("!DOCTYPE HTML PUBLIC" stream :fresh-line t :trailing-line t)
    (fast-format stream " ~S" *dtd-version*))) 

(define-macro with-html-document ((&key (stream '*output-stream*) declare-dtd-version-p) &body body)
  "Asserts the contents of BODY is an HTML document.
DECLARE-DTD-VERSION-P will declare the version of the DTD implemented by the current generation
package. This should be T whenever generation strictly conforms to the HTML version
associated with the macro WITH-HTML-DOCUMENT. When extension tags or features are used that
do not appear in the HTML DTD, DECLARE-DTD-VERSION-P should always be NIL."
  (if declare-dtd-version-p
      `(progn (declare-html-version ,stream)
              (with-environment ("HTML" :stream ,stream)
                ,@body))
      `(with-environment ("HTML" :stream ,stream)
         ,@body))) 

;;;------------------------------------------------------------------- 
;;;
;;; HYPERLINKS 
;;;

(declaim (inline %write-anchor-command-arguments))

(defun %write-anchor-command-arguments (stream reference local-reference tag relation inverse title)
  ;; write anchor arguments
  (cond ((and local-reference reference)
         (%write-command-key-arg stream "HREF" (typecase reference
                                                 (function reference)
                                                 (t (coerce-url-string reference url:*escape-search-urls*))))
         (fast-format stream "#~A" local-reference))
        (reference ;; URL for a anchored document retrieved by the anchor.
         ;; ensure generated URLs are escaped.
         (%write-command-key-arg stream "HREF" (typecase reference
                                                 (function reference)
                                                 (t (coerce-url-string reference url:*escape-search-urls*)))))
        (local-reference ;; tags for a position in the current document.
         (fast-format stream " HREF=#~A" local-reference)))
  (cond-every 
    (tag ;; "A tag anchor."
      (%write-command-key-arg stream "NAME" tag))
    (relation ;; "The relationship the anchored document has to this one."
      (%write-command-key-arg stream "REL" relation)) 
    (inverse ;; "The reverse relationship type, and the inverse of REL."
      (%write-command-key-arg stream "REV" inverse))
    (title ;;  "Title to use for otherwise untitled anchored document."
      (%write-command-key-arg stream"TITLE" title))))

(defun %note-anchor (stream text reference local-reference tag relation inverse title)
  (%with-environment ("A" :string-for-null-stream :inline :stream stream :fresh-line nil)
                     (%write-anchor-command-arguments stream reference local-reference tag relation inverse title)
    (write-string text stream)))

(declaim (notinline %write-anchor-command-arguments))

(declaim (inline note-anchor))

(define note-anchor (text &key reference local-reference tag relation inverse title (stream *output-stream*))
  "Notes a hypertext anchor for TEXT on STREAM.
   REFERENCE is a URL (object or string).
   LOCAL-REFERENCE is reference to a tag local to the current page.
   TAG is a string denoting a named point in the document which can be referenced with a local reference (#name).
   RELATION is the link type from the referring document to the the anchored document.
   INVERSE is the inverse relationship back from the anchored document to the referring document.
   TITLE is title to use for otherwise untitled anchored document."
  (%note-anchor stream text reference local-reference tag relation inverse title))

(define-macro with-anchor-noted ((&key reference local-reference tag relation inverse title (stream '*output-stream*))
                                 &body body)
  "Like NOTE-ANCHOR except forms in BODY can compute and write the anchored text on STREAM.
   REFERENCE is a URL (object or string).
   LOCAL-REFERENCE is reference to a tag local to the current page.
   TAG is a string denoting a named point in the document which can be referenced with a local reference (#name).
   RELATION is the link type from the referring document to the the anchored document.
   INVERSE is the inverse relationship back from the anchored document to the referring document.
   TITLE is title to use for otherwise untitled anchored document."
  `(locally
     (declare (notinline %write-anchor-command-arguments))
     (let ((stream ,stream))
       (%with-environment ("A" :string-for-null-stream :inline :stream stream :fresh-line nil)
                          (%write-anchor-command-arguments stream ,reference ,local-reference ,tag ,relation ,inverse  ,title)
         ,@body))))

;;;------------------------------------------------------------------- 
;;;
;;;  HEAD ELEMENTS
;;; 

(define declare-document-style (style &optional (stream *output-stream*))
  (declare (ignore style stream))
  (error "Not defined for HTML 3.2")
  #+ignore(issue-command "STYLE" stream))

(defun %declare-link (stream reference local-reference tag relation inverse title)
  (declare (inline %write-anchor-command-arguments))
  (%issue-command ("LINK" stream :fresh-line t :trailing-line t)
    (%write-anchor-command-arguments stream reference local-reference tag relation inverse title)))

(declaim (inline declare-link))

(define declare-link (&key reference local-reference tag relation inverse title (stream *output-stream*)) 
  (%declare-link stream reference local-reference tag relation inverse title))

(setf (documentation 'declare-link 'function)
      "Declares a link between the current document and other documents or resources.
   REFERENCE is a URL (object or string) denoting the target resource.
   LOCAL-REFERENCE is reference to a tag local to the current page.
   TAG is a string denoting a named point in the document which can be referenced with a local reference (#name).
   RELATION is the link type from the referring document to the the target document.
   INVERSE is the inverse relationship back from the anchored document to the referring document.
   TITLE is title to use for otherwise untitled anchored document.

LINK elements can be used
     a.for document specific navigation toolbars or menus 
     b.for controlling how collections of HTML files are rendered into printed documents 
     c.for linking associated resources such as style sheets and scripts 
     d.for providing alternative forms of the current document 

Some proposed values for RELATION:

TOP       -- The link references the top of a hierarchy, e.g. the first or cover page in a collection. 
CONTENTS  -- The link references a document serving as a table of contents. 
INDEX     -- The link references a document providing an index for the current document. 
GLOSSARY  -- The link references a document providing a glossary of terms that are relevant to 
             the current document. 
COPYRIGHT -- The link references a copyright statement for the current document. 
NEXT      -- The link references the next document to visit in a guided tour. It can be used, for example, 
             to preload the next page. 
PREVIOUS  -- The link references the previous document in a guided tour. 
HELP      -- The link references a document offering help, e.g. describing the wider context and offering 
             further links to relevant documents. This is aimed at reorienting users who have lost their way. 
SEARCH    -- The link references a page for searching material related to a collection of pages.

     Examples:

     (declare-link :RELATION \"Contents\" :reference \"toc.html\")
     (declare-link :RELATION \"Previous\" :reference \"doc31.html\")
     (declare-link :RELATION \"Next\" :reference \"doc33.html\")
     (declare-link :RELATION \"Chapter\" :inverse \"Contents\" :reference \"Chapter2.html\")")

;;;------------------------------------------------------------------- 
;;;
;;;  TEXT BLOCKS
;;; 

(defconstant *horizontal-alignment-values* '(:left :center :right))

(defun horizontal-alignment-value (alignment)
  (unless (member alignment *horizontal-alignment-values*)
    (error "Unknown alignment, ~S, for a paragraph or division." alignment))
  (symbol-name alignment)) 

(define-macro with-division ((alignment &key (stream '*output-stream*)) &body body)
  "Establishes a division environment.
ALIGNMENT can be any of :LEFT, :RIGHT, OR :CENTER."
  `(%with-environment 
     ("DIV" :stream ,stream)
     (%write-command-key-arg ,stream "ALIGN" ,(typecase alignment
                                                (keyword (horizontal-alignment-value alignment))
                                                (t `(horizontal-alignment-value ,alignment))))
     ,@body))

(define-macro with-centering ((&key (stream '*output-stream*)) &body body)
  "Centers the contents of BODY."
  `(with-division (:CENTER :stream ,stream)
     ,@body))

(defun section-heading (heading &optional alignment (stream *output-stream*) (level *section-level*)
                                &aux command)
  (setq command (case level
                  (1 "H1")
                  (2 "H2")
                  (3 "H3")
                  (4 "H4")
                  (5 "H5")
                  (6 "H6")
                  (t (cond ((< 6 level) (error "Exceeded the maximun section heading depth of 6."))
                           ((< level 1) (error "SECTION-HEADING called outside of WITH-SECTION-HEADING."))))))
  (%with-environment (command :stream stream :fresh-line t)
                     (when alignment 
                       (%write-command-key-arg stream "ALIGN" (horizontal-alignment-value alignment)))
    (etypecase heading
      (string (write-string heading stream))
      (function (funcall heading stream)))))

(define-macro with-section-heading ((heading &key (level '*section-level*) alignment
                                             (stream '*output-stream*)) &body body)
  "Excutes BODY within a section heading of depth LEVEL.
HEADING can be a string or a function called with (stream).
ALIGNMENT is any of :LEFT, :CENTER, :RIGHT, and defaults to :LEFT"
  (if body
      `(progn
         (section-heading ,heading ,alignment ,stream ,level)
         (let ((*section-level* (the fixnum (1+ ,level))))
           ,@body))
      `(section-heading ,heading ,alignment ,stream ,level)))

(define-macro with-paragraph ((&key alignment (stream '*output-stream*)) &body body)
  "Establishes a paragraph environment.
ALIGNMENT can be any of :LEFT, :RIGHT or :CENTER, and defaults to :LEFT"
  `(%with-environment 
     ("P" :stream ,stream)
     ,(typecase alignment
        (null nil)
        (keyword `(%write-command-key-arg ,stream "ALIGN" ,alignment))
        (t `(when ,alignment
              (%write-command-key-arg ,stream "ALIGN" (horizontal-alignment-value ,alignment)))))
     ,@body)) 

(eval-when (compile eval load)   
  (defconstant *paragraph-styles* '((:quotation . "BLOCKQUOTE")
                                    (:address . "ADDRESS")
                                    (:citation . "CITE")
                                    (:code . "CODE")))) 

(defun paragraph-style-command (style)
  (or (cdr (assoc style *paragraph-styles*))
      (error "Unknown paragraph style, ~A." style)))

(define-macro with-paragraph-style ((style &key alignment (fresh-line t) (stream '*output-stream*)) &body body)
  #.(format nil "Puts the enclosed paragraph into STYLE, which can be any of: ~{~S~^, ~}.
ALIGNMENT can be any of :LEFT, :RIGHT or :CENTER, and defaults to :LEFT" (mapcar #'car *paragraph-styles*))
  `(with-paragraph (:alignment ,alignment :stream ,stream)
     (with-environment (,(typecase style
                           (keyword (paragraph-style-command style))
                           (t `(paragraph-style-command ,style)))
                        :stream ,stream :fresh-line ,fresh-line)
       ,@body))) 

;;;------------------------------------------------------------------- 
;;;
;;;  RENDITION
;;; 

(eval-when (compile eval load)
  (defconstant *rendition* '((:bold . "B")
                             (:italic . "I") 
                             (:large . "BIG")
                             (:small . "SMALL")
                             (:strike . "S")
                             (:subscript . "SUB")
                             (:superscript . "SUP")
                             (:teletype . "TT")
                             (:underline . "U")
                             (:heading-1 . "H1")
                             (:heading-2 . "H2")
                             (:heading-3 . "H3")
                             (:heading-4 . "H4")
                             (:heading-5 . "H5")
                             (:heading-6 .  "H6")))) 

(defun rendition-command (rendition)
  (or (cdr (assoc rendition *rendition*))
      (error "Unknown type of rendition, ~S." rendition))) 

(define-macro with-rendition ((rendition &key (stream '*output-stream*)) &body body)
  "Text output within BODY on STREAM is rendered according to RENDITION,
which can be any of :

     :ITALIC         -- Italic text style.
     :BOLD           -- Bold text style.
     :LARGE          -- Places text in a large font.
     :SMALL          -- Places text in a small font.
     :STRIKE         -- Strike-through text style.
     :SUBSCRIPT      -- Places text in subscript style. 
     :SUPERSCRIPT    -- Places text in superscript style.
     :TELETYPE       -- Teletype or monospaced text.
     :UNDERLINE      -- Underlined text style.
     :HEADING-1      -- Chapter heading.
     :HEADING-2      -- Section heading.
     :HEADING-3      -- Subsection heading.
     :HEADING-4      -- Subsubsection heading.
     :HEADING-5      -- Subsubsubsection heading.
     :HEADING-6      -- Subsubsubsubsection heading."
  `(with-environment (,(typecase rendition
                         (keyword (rendition-command rendition))
                         (t `(rendition-command ,rendition)))
                      :fresh-line nil :stream ,stream)
     ,@body)) 

;;;------------------------------------------------------------------- 
;;;
;;; EMPHASIS
;;; 

(eval-when (compile eval load)
  (defconstant *emphasis*
               `((:emphasis . "EM")
                 (:strong . "STRONG")
                 (:address . "ADDRESS")
                 (:quotation . "BLOCKQUOTE")
                 (:definition . "DFN")
                 (:citation . "CITE")
                 (:keyboard . "KBD")
                 (:variable . "VAR")
                 (:code . "CODE")
                 (:sample . "SAMP"))))

(defun emphasis-command (emphasis)
  (or (cdr (assoc emphasis *emphasis*))
      (error "Unknown type of emphasis, ~S." emphasis)))

(define-macro with-emphasis ((emphasis &key (stream '*output-stream*)) &body body)
  "Renders output within BODY on STREAM according to EMPHASIS, 
which can be any of: 

     :ADDRESS    -- Address block style.
     :CITATION   -- Used for citations or references to other sources.
     :CODE       -- Used for extracts from program code. 
     :DEFINITION -- Defining instance of the enclosed term.
     :EMPHASIS   -- basic emphasis typically rendered in an italic font.
     :KEYBOARD   -- Used for text to be typed by the user. 
     :QUOTATION  -- Block quotation. 
     :SAMPLE     -- Used for sample output from programs, and scripts etc. 
     :STRONG     -- strong emphasis typically rendered in a bold font.
     :VARIABLE   -- Used for variables or arguments to commands."
  `(with-environment (,(typecase emphasis
                         (keyword (emphasis-command emphasis))
                         (t `(emphasis-command ,emphasis)))
                      :fresh-line nil :stream ,stream)
     ,@body))


;;;------------------------------------------------------------------- 
;;;
;;; FONTS
;;;

(defun %write-make-font-args (stream size relative-p color)
  (declare (fixnum size))
  (cond-every
    (size
      (unless (< 0 size 8)
        (error "SIZE, ~S, is not an integer from 1 through 7." size))
      (if relative-p
	  (fast-format stream "SIZE=~C~D" (if (plusp size) #\+ #\-) (abs size))
	  (%write-command-key-arg stream "SIZE" size t)))
    (color
      (%write-command-key-arg stream "COLOR" (color-mapping color t)))))

(define-macro with-font ((&key (size 3) relative-size-p color (stream '*output-stream*)) &body body)
  "Declares the current font size to be SIZE and color to COLOR within BODY.
SIZE is an absolute size denoted by an integer from 1 to 7, or a relative size.
When RELATIVE-SIZE-P is non-null, SIZE is interpreted as a relative size. Negative
integers indicate reduction in font size whereas positive integers denote an increase."
  `(%with-environment ("FONT" :fresh-line nil :stream ,stream)
                      (%write-make-font-args ,stream ,size ,relative-size-p ,color)
     ,@body))

;;;------------------------------------------------------------------- 
;;;
;;; ENUMERATION 
;;; 

(defvar *enumeration-function*)

(defconstant *enumerate-bullet-styles* '((:capital-letters . "A")
                                         (:small-letters . "a")
                                         (:large-roman . "I")
                                         (:small-roman . "i")
                                         (:arabic . "1")))

(defconstant *itemize-bullet-styles* '((:solid-disc . "DISC")
                                       (:circle . "CIRCLE")
                                       (:square . "SQUARE")))

(defmacro %get-bullet-style (type alist)
  `(and ,type (or (cdr (assoc ,type ,alist))
                  (error "Bullet type, ~S, is not one of the known types, ~{~S~^, ~}."
                         ,type (mapcar #'car ,alist))))) 

(defun enumerate-itemized-item (stream continuation icon-url type)
  (declare (ignore icon-url))
  (%issue-command ("LI" stream :fresh-line t)
    (%get-bullet-style type *itemize-bullet-styles*))
  (multiple-value-prog1
    (funcall continuation stream)
    (fresh-line stream)))

(defun enumerate-enumerated-item (stream continuation icon-url type)
  (declare (ignore icon-url))
  (%issue-command ("LI" stream :fresh-line t)
    (%get-bullet-style type *enumerate-bullet-styles*))
  (multiple-value-prog1
    (funcall continuation stream)
    (fresh-line stream)))

(defun enumerate-normal-item (stream continuation icon-url head)
  (declare (ignore head))
  (%issue-command ("LI" stream :fresh-line t))
  (when icon-url
    (image icon-url "o" :stream stream)
    (write-char #\space stream)) 
  (funcall continuation stream)
  (fresh-line stream)) 

(defun enumerate-definition-item (stream continuation icon-url head)
  (flet ((write-dd (stream)
           (issue-command "DD" stream nil)
           (write-char #\space stream)))
    (declare (inline write-dd))
    (fresh-line stream)
    (issue-command "DT" stream)
    (when icon-url
      (image icon-url "o" :stream stream)
      (write-char #\space stream))
    (etypecase head
      (null nil)
      (string
        (write-string head stream)
        (write-dd stream))
      (cons
        (dolist (item head)
          (write item :stream stream))
        (write-dd stream))
      (function
        (funcall head stream)
        (write-dd stream)))
    (funcall continuation stream)))

(define-macro enumerating-item ((stream &key icon-url head type) &body body)
  "Enumerates an item on STREAM according to the enclosing enumeration style.
BODY generates the body of the item whereas icon-url head type control the item's header.

TYPE can be provided for the styles :ENUMERATE and :ITEMIZE to override the default bullet given by the
enclosing WITH-ENUMERATION. For the style ENUMERATE, TYPE can be any of :CAPITAL-LETTERS,
:SMALL-LETTERS, :LARGE-ROMAN, :SMALL-ROMAN, or :ARABIC (the default). For the style :ITEMIZE, TYPE
can be any of :SOLID-DISC, :CIRCLE, or :SQUARE.

HEAD specifies the heading for an item in the :DEFINITION style.  Head can be a string, a list of
strings or a function called on STREAM.

ICON-URL is available for the styles :DEFINITION,  :DIRECTORY, and :MENU. When provided,
an image is emitted at the start of the item using the URL supplied as the value of ICON-URL."
  `(flet ((continuation (,stream) ,@body))
     (declare (dynamic-extent #'continuation))
     (funcall *enumeration-function* ,stream #'continuation ,icon-url ,(or head type))))

(defun %enumeration-style-parameters (style type)
  (declare (values function tag type))
  (case style
    (:itemize
      (values #'enumerate-itemized-item "UL" (%get-bullet-style type *itemize-bullet-styles*)))
    (:enumerate
      (values #'enumerate-enumerated-item "OL" (%get-bullet-style type *enumerate-bullet-styles*)))
    (:definition
      (values #'enumerate-definition-item "DL"))
    (:directory
      (values #'enumerate-normal-item "DIR"))
    ((:menu :plain)
     (values #'enumerate-normal-item "MENU"))
    (t (error "Unknown enumeration style, ~A." style)))) 

(define-macro with-enumeration ((stream style &key compact type start) &body body)
  "Establishes an enumeration environment using the style, STYLE, on STREAM. 
Within this environment, each item is emitted by generation code executed with the ENUMERATING-ITEM macro.
STYLE can be :DEFINITION, :DIRECTORY, :ENUMERATE :ITEMIZE :MENU. COMPACT advises the client to render
lists in a more compact style. 

TYPE allows the default styles of enumeration to be overridden for some styles. For the style :ENUMERATE,
TYPE can be any of: :CAPITAL-LETTERS, :SMALL-LETTERS, :LARGE-ROMAN, :SMALL-ROMAN, or :ARABIC (the
default). For the style :ITEMIZE, TYPE can be any of :SOLID-DISC, :CIRCLE, :SQUARE.

For the :ENUMERATE style, START can cause the enumeration to begin at a number other than the default 1.

You must use the ENUMERATING-ITEM from the same package for reliable results."
  (flet ((enumeration-arguments (compact start type)
           (let ((args nil))
             (cond-every
               (compact (push `(,compact (write-string " COMPACT" ,stream)) args))
               (type (push `(enumeration-type (fast-format stream " TYPE=~A" enumeration-type)) args))
               (start
                 (push `(,start 
                         (check-type ,start integer)
                         (fast-format ,stream " START=~D" ,start))
                       args)))
             (when args `(cond-every ,.args)))))
    `(multiple-value-bind (*enumeration-function* tag enumeration-type)
         (%enumeration-style-parameters ,style ,type)
       enumeration-type                         ;no compiler warning
       (%with-environment (tag :fresh-line t :stream ,stream)
                          ,(enumeration-arguments compact start type)
         ,@body))))

(define enumerate-item-list (item-list &key (style :itemize) type compact (stream *output-stream*))
  "Enumerates the elements of ITEM-LIST in STYLE on STREAM."
  (with-enumeration (stream style :compact compact :type type)
    (dolist (item item-list)
      (enumerating-item (stream)
        (write item :stream stream :escape nil))))) 


;;;------------------------------------------------------------------- 
;;;
;;; FILLOUT FORMS
;;;

(define-macro with-fillout-form ((action target &key name (stream '*output-stream*)
                                         (encoding-type ''(:application :www-url-form-encoded))) &body body)
  "Establishes an fillout-form environment.  
ACTION is either :POST, :MAIL, or :GET, or :NONE.
NAME is a name identifying the form element.
TARGET is the URL to which the form values are returned.
If ACTION is :NONE, TARGET can be NIL.

ENCODING-TYPE is MIME content type to use when return the form values to TARGET.
ENCODING-TYPE defaults to application/x-www-form-urlencoded.
See ACCEPT-INPUT for documentation on client-side events.             
:GET should only be used in exceptional circumstances as not only is
it considered obsolete but it also is limited to 1024 characters 
including the rest of the the Target URL."
  `(html2:with-fillout-form (,action ,target :name ,name :stream ,stream :encoding-type ,encoding-type)
     ,@body))

;;;------------------------------------------------------------------- 
;;;
;;; CLIENT-SIDE IMAGE MAPS 
;;;

(declaim (inline client-image-area))

;; Drops the target argument from NS2.0
(define client-image-area (shape coordinates &key reference alternative-text (stream *output-stream*))
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
  (ns2.0:client-image-area shape coordinates :reference reference :alternative-text alternative-text :stream stream)) 

;;;------------------------------------------------------------------- 
;;;
;;;  TABLES (widely deployed subset of RFC 1942)
;;; 

(defconstant *vertical-alignment-values* '(:top :middle :bottom))

(defun vertical-alignment-value (alignment)
  (unless (member alignment *vertical-alignment-values*)
    (error "~S is not one of the possible vertical alignments, ~S, for ~S." alignment *vertical-alignment-values*))
  (symbol-name alignment))

(defun %write-table-arguments (stream width border cell-spacing cell-padding)
  (flet ((%write-width-argument (width stream)
           (if (<= 0 width 1)
               (let ((w (concatenate 'string
                                     (write-to-string (floor (* width 100)) :base 10. :escape nil)
                                     "%")))
                 (declare (dynamic-extent w))
                 (%write-command-key-arg stream "WIDTH" w))
               (error "Table WIDTH is ~S, which is not a float between 0 and 1 as required by HTML 3.2."))))
    (declare (inline %write-width-argument))
    (cond-every
      (width (%write-width-argument width stream))
      (border
        (%write-command-key-arg stream "BORDER" (if (integerp border) border 1.) t))
      (cell-spacing
        (%write-command-key-arg stream "CELLSPACING" cell-spacing t))
      (cell-padding
        (%write-command-key-arg stream "CELLPADDING" cell-padding t)))))

(defun %write-table-environment-arguments (stream horizontal-alignment vertical-alignment
                                                  &optional height width  no-wrap column-span row-span) 
  (cond-every
    (height (%write-command-key-arg stream "HEIGHT" height t))
    (width (%write-command-key-arg stream "WIDTH" width t))
    (horizontal-alignment
      (%write-command-key-arg stream "ALIGN"  (horizontal-alignment-value horizontal-alignment)))
    (vertical-alignment
      (%write-command-key-arg stream "VALIGN" (vertical-alignment-value vertical-alignment)))
    (no-wrap
      (%write-command-key-arg stream "NOWRAP"))
    (column-span
      (%write-command-key-arg stream "COLSPAN" column-span t))
    (row-span
      (%write-command-key-arg stream "ROWSPAN" row-span t))))

(define-macro with-table-cell ((&key height width header-p horizontal-alignment vertical-alignment (break-lines-p t)
                                     column-span row-span (stream '*output-stream*)) &body body)
  "Asserts that the contents of BODY is a cell withing a table environment
    HEIGHT is an integer specifying the absolute length of the line in pixels.
    WIDTH is an integer specifying the absolute length of the line in pixels.
    HEADER-P controls whether the cell is a header cell or an ordinary cell.
    HORIZONTAL-ALIGNMENT can be any of :LEFT, :CENTER, or :RIGHT.
    VERTICAL-ALIGNMENT can be any of :TOP, :MIDDLE, or :BOTTOM.
    BREAK-LINES-P prevents lines from being broken to fit the width of a cell.
    COLUMN-SPAN is an integer that controls the number of columns a cell spans.
    ROW-SPAN is an integer that controls the number of columns a cell spans."
  `(let ((tag (if ,header-p "TH" "TD")))
     (%with-environment
       (tag :fresh-line nil :stream ,stream)
       (%write-table-environment-arguments ,stream ,horizontal-alignment ,vertical-alignment
                                           ,height ,width (not ,break-lines-p) ,column-span ,row-span)
       ,@body)))

(define-macro with-table-row ((&key horizontal-alignment vertical-alignment (stream '*output-stream*)) &body body)
  "Asserts that the contents of BODY is a row in a table.
HORIZONTAL-ALIGNMENT can be any of :LEFT, :CENTER, or :RIGHT.
VERTICAL-ALIGNMENT can be any of :TOP, :MIDDLE, or :BOTTOM."
  `(%with-environment
     ("TR" :fresh-line t :stream ,stream)
     (%write-table-environment-arguments ,stream ,horizontal-alignment ,vertical-alignment)
     ,@body)) 

(define-macro with-caption ((&key (alignment :top) (stream '*output-stream*)) &body body)
  "Asserts the contents of BODY is a caption within a table environment.
:ALIGNMENT can be :TOP or :BOTTOM."
  `(%with-environment
     ("ALIGN" :stream ,stream)
     (%write-command-key-arg ,stream "ALIGN" (ecase ,alignment
                                               (:top "TOP")
                                               (:bottom "BOTTOM")))
     ,@body))

(define-macro with-table ((&key width border cell-spacing cell-padding (stream '*output-stream*)) &body body)
  "Establishes a table environment with BODY within which WITH-TABLE-ROW and WITH-TABLE-CELL can be used.
A caption can be specified using the macro WITH-CAPTION.
WIDTH is a float between zero and one indicating the percent of the window with to occupy.
BORDER is NIL or an integer specifying the width in pixels of the border.
CELL-SPACING is an integer, defaulting to 2, that controls the space between cells. 
CELL-PADDING is an integer, defaulting to 1 that controls the space between text contained in a cell and the wall."
  `(%with-environment ("TABLE" :fresh-line t :stream ,stream)
                      (%write-table-arguments ,stream ,width ,border ,cell-spacing ,cell-padding) 
     ,@body))

;;;------------------------------------------------------------------- 
;;;
;;; COLOR SCHEMES AND STANDARD DOCUMENT BODY
;;;

(defparameter *standard-color-scheme* nil
  "The standard color scheme to use on automatically generated HTML pages.
See HTML:CREATE-COLOR-SCHEME and HTML:WITH-STANDARD-DOCUMENT-BODY.")

(defstruct color-scheme
  (background nil)
  (foreground nil)
  (link nil)
  (visited-link nil)
  (active-link nil)
  (background-url nil))

(setf (documentation 'color-scheme 'structure)
      "Holds the color and background URL information for an HTML 3.2 page.")

(define create-color-scheme (&key background-url background foreground link visited-link active-link)
  "Create a reusable color scheme for an HTML page."
  (flet ((get-hexadecimal-color (color)
           (when color
             (color-mapping color t))))
    (make-color-scheme :background-url (and background-url (url:coerce-url-string background-url))
                       :background (get-hexadecimal-color background)
                       :foreground (get-hexadecimal-color foreground)
                       :link (get-hexadecimal-color link)
                       :visited-link (get-hexadecimal-color visited-link)
                       :active-link (get-hexadecimal-color active-link))))

(defun write-color-scheme-arguments (color-scheme stream)
  (macrolet ((write-params (vars)
               `(cond-every
                  ,.(loop for var in (reverse vars)
                          collect `(,var
                                    (%write-command-key-arg stream ,(ns1.1::%body-arg-key var) ,var))))))
    (when color-scheme
      (check-type color-scheme color-scheme)
      (let ((background-url (color-scheme-background-url color-scheme))
            (background (color-scheme-background color-scheme))
            (foreground (color-scheme-foreground color-scheme))
            (link (color-scheme-link color-scheme))
            (visited-link (color-scheme-visited-link color-scheme))
            (active-link (color-scheme-active-link color-scheme)))  
        (write-params (background-url background foreground link visited-link active-link))))))

(define-macro with-standard-document-body ((&key (color-scheme '*standard-color-scheme*) (stream '*output-stream*)) &body body)
  "Asserts the contents of BODY are the body of the document.
The page is displayed with the standard color scheme from *STANDARD-COLOR-SCHEME*."
  `(%with-environment ("BODY" :stream ,stream)
                      (write-color-scheme-arguments ,color-scheme ,stream)
     ,@body))
