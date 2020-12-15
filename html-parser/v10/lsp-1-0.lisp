;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: HTML-PARSER -*-
;;
;; Lisp Server Page (LSP) DTD transcription, based upon:
;; ----
;;   File: html-4-0-transitional.lisp
;;   (c) Copyright 1996-97, Sunil Mishra (smishra@cc.gatech.edu)
;;       All Rights Reserved
;; ----
;; Copyright (c) 1999 Kaelin Colclasure <kaelin@acm.org>, all rights reserved.
;;
;; $Id: //depot/rev/lisp/html-parser-10.0-alpha/lsp-1-0.lisp#1 $

(in-package :html-parser)

;; This file contains a lispified transcription of the
;;    HTML 4.0 Transitional DTD. It does not do frames.
;; Based on the DTD at
;;    http://www.w3.org/TR/1998/REC-html40-19980424/loose.dtd
;; All entities beginning with a % are special SGML declarations

;;   :set	one or more of these is allowed, in any order
;;   :sequence	all of these are required, in the given order
;;   :or	one of these is allowed
;;   :and	all of these are required, in any order

;;   +		one or more
;;   *		zero or more
;;   ?		zero or one

;; All entity definitions MUST precede element definitions, otherwise
;; writing the code to process the definitions becomes too much of a
;; hassle

;; All comments beginning with *** are taken from the DTD.


;;; Entities

;; ***Imported names

(define-html-entity %ContentType
    cdata)

(define-html-entity %ContentTypes
    cdata)

(define-html-entity %Charset
    cdata)

(define-html-entity %Charsets
    cdata)

(define-html-entity %LanguageCode
    name)

(define-html-entity %Character
    cdata)

(define-html-entity %LinkTypes
    cdata)

(define-html-entity %MediaDesc
    cdata)

(define-html-entity %URI
    cdata)

(define-html-entity %Datetime
    cdata)

(define-html-entity %Script
    cdata)

(define-html-entity %StyleSheet
    cdata)

(define-html-entity %FrameTarget
    cdata)

(define-html-entity %Text
    cdata)

;; ***Parameter Entities

(define-html-entity %lsp.script
    (:or eval lisp))

(define-html-entity %head.misc		; Repeatable head elements
    (:or %lsp.script script style meta link object))

(define-html-entity %heading
    (:or h1 h2 h3 h4 h5 h6))

(define-html-entity %list
    (:or ul ol dir menu))

(define-html-entity %preformatted
    pre)

(define-html-entity %Color		; A color using sRGB:
    cdata)				; #RRGGBB as Hex values

(define-html-entity %bodycolors
    ((bgcolor %Color :implied)		; Document background color
     (text %Color :implied)		; Document text color
     (link %Color :implied)		; Color of links
     (vlink %Color :implied)		; Color of visited links
     (alink %Color :implied)))		; Color of selected links

;; ***Generic attributes

(define-html-entity %lspattrs		; LSP-specific attributes
    ((name CDATA :implied)		; Lisp symbol binding
     (lambda-list CDATA :implied)	; Lambda list (arguments)
     (defer (defer) :implied)))		; Don't execute an implicit call

(define-html-entity %coreattrs
    ((id id :implied)			; Document-wide unique id
     (class cdata :implied)		; Space separated list of classes
     (style %StyleSheet :implied)	; Associated style info
     (title %Text :implied)		; Advisory title/amplification
     ))

(define-html-entity %i18n
    ((lang %LanguageCode :implied)	; Language code
     (dir (:or ltr rtl) :implied)	; Direction for weak/neutral text
     ))

(define-html-entity %events
    ((onclick %Script :implied)		; A pointer button was clicked
     (ondblclick %Script :implied)	; A pointer button was double clicked
     (onmousedown %Script :implied)	; A pointer button was pressed down
     (onmouseup %Script :implied)	; A pointer button was released
     (onmouseover %Script :implied)	; A pointer was moved onto
     (onmousemove %Script :implied)	; A pointer was moved within
     (onmouseout %Script :implied)	; A pointer was moved away
     (onkeypress %Script :implied)	; A key was pressed and released
     (onkeydown %Script :implied)	; A key was pressed down
     (onkeyup %Script :implied)		; A key was released
     ))

(define-html-entity %attrs
    (%coreattrs %i18n %events))

(define-html-entity %align		; Default is left for ltr paragraphs,
					; right for rtl
    ((align (:or left center right justify) :implied)))

;; ***Text Markup

(define-html-entity %fontstyle
    (:or tt i b u s strike big small))

(define-html-entity %phrase
    (:or em strong dfn code samp kbd var cite abbr acronym))

(define-html-entity %special
    (:or a img applet object font basefont br %lsp.script script
	 map q sub sup span bdo iframe))

(define-html-entity %formctrl
    (:or input select textarea label button))

(define-html-entity %inline
    (:or pcdata %fontstyle %phrase %special %formctrl))

;; ***HTML content models

(define-html-entity %block
    (:or p %heading %list %preformatted dl div center flet
	 noscript noframes blockquote form isindex hr
	 table fieldset address))

(define-html-entity %flow
    (:or %block %inline))

;; ***The Anchor Element

(define-html-entity %Shape
    (:or rect circle poly default))

(define-html-entity %Coords
    cdata)				; Comma separated list of lengths

;; ***Images

(define-html-entity %Length
    cdata)				; nn for pixels or nn% for length %

(define-html-entity %MultiLength
    cdata)				; Pixel, percentage, or relative

(define-html-entity %MultiLengths
    cdata)				; Comma-separated list of MultiLength

(define-html-entity %Pixels
    cdata)				; Integer representing length in pixels

(define-html-entity %IAlign
    (:or top middle bottom left right))	; Center?

;; ***Preformatted text

(define-html-entity %pre.exclusion
    (:or img object applet big small sub sup font basefont))

;; ***Lists

(define-html-entity %OLStyle
    cdata)				; Constrained to: (1|a|A|i|I)

(define-html-entity %ULStyle
    (:or disc square circle))

(define-html-entity %LIStyle
    cdata)				; Constrained to: (%ULStyle;|%OLStyle;)

;; ***Forms

(define-html-entity %InputType
    (:or text password checkbox
	 radio submit reset
	 file hidden image button))

(define-html-entity %LAlign
    (:or top bottom left right))

;; ***IETF HTML table standard, see [RFC1942]

(define-html-entity %TFrame
    (:or void above below hsides lhs rhs vsides box border))

(define-html-entity %TRules
    (:or none groups rows cols all))

(define-html-entity %TAlign
    (:or left center right))

(define-html-entity %cellhalign
    ((align (:or left center right justify char) :implied)
     (char %Character :implied)		; Alignment char, e.g. char=':'
     (charoff %Length :implied)		; Offset for alignment char
     ))

(define-html-entity %cellvalign
    ((valign (:or top middle bottom baseline) :implied)))

(define-html-entity %CAlign
    (:or top bottom left right))

(define-html-entity %Scope
    (:or row col rowgroup colgroup))

;; ***Document Frames

;; Since this DTD does not include frames...

(define-html-entity %noframes.content
    (* %flow))

;; ***Document Head

(define-html-entity %head.content
    (:and title
	  (? isindex)
	  (? base)))

;; ***Document Structure

;; Skipping version

;;<!ENTITY % version "version CDATA #FIXED '%HTML.Version;'">

(define-html-entity %lsp.flets
    (:or body head flet))

(define-html-entity %html.content
    (:sequence (* %lsp.flets) (? lisp)))


;;; Elements

;; I have left out the attributes %version.attr because it cannot be
;; set by the user; it is of no consequence to parsing.

;; ***Text Markup

(define-html-element (:or %fontstyle %phrase)
    :content (* %inline)
    :attributes (%attrs			; %coreattrs, %i18n, %events
    ))

(define-html-element (:or sub sup)	; Subscript, superscript
    :content (* %inline)
    :attributes (%attrs			; %coreattrs, %i18n, %events
    ))

(define-html-element span		; Generic language/style container
    :content (* %inline)
    :attributes (%attrs			; %coreattrs, %i18n, %events
    ))

(define-html-element bdo
    :content (* %inline)		; I18N BiDi over-ride
    :attributes (%coreattrs		; id, class, style, title
		 (lang %LanguageCode :implied) ; Language code
		 (dir (:or ltr rtl) :required) ; Directionality
		 ))

(define-html-element basefont		; Base font size
    :end-optional t
    :attributes ((id id :implied)	; Document-wide unique id
		 (size cdata :required)	; Base font size for FONT elements
		 (color %Color :implied) ; Text color
		 (face cdata :implied)	; Comma separated list of font names
		 ))

(define-html-element font		; Local change to font
    :content (* %inline)
    :attributes (%coreattrs		; id, class, style, title
		 %i18n			; lang, dir
		 (size cdata :implied)	; [+|-]nn e.g. size="+1", size="4"
		 (color cdata :implied)	; Text color
		 (face cdata :implied)	; Comma separated list of font names
		 ))

(define-html-element br			; Forced line break
    :end-optional t
    :attributes (%coreattrs		; id, class, style, title
		 (clear (:or left all right none) none) ; Control of text flow
		 ))

;; ***Document Body

(define-html-element body		; Document body
    :start-optional t
    :end-optional t
    :content (* %flow)
    :inclusions (:or ins del)
    :attributes (%lspattrs		; LSP-specific attributes
		 %attrs			; %coreattrs, %i18n, %events
		 (onload %Script :implied) ; The document has been loaded
		 (onunload %Script :implied) ; The document has been removed
		 (background %uri :implied) ; Document background
		 %bodycolors		; bgcolor, text, link, vlink, alink
		 ))

(define-html-element address
    :content (* (:or %inline p))	; Information on author
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 ))

(define-html-element div		; Generic language/style container
    :content (* %flow)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 %align			; align, text alignment
		 ))

(define-html-element center		; Shorthand for DIV align=center
    :content (* %flow)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 ))

;; ***The Anchor Element

(define-html-element a			; Anchor
    :content (* %inline)
    :exclusions a
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (charset %Charset :implied) ; Char encoding of linked resource
		 (type %ContentType :implied) ; Advisory content type
		 (name cdata :implied)	; Named link end
		 (href %uri :implied)	; URI for linked resource
		 (hreflang %LanguageCode :implied) ; Language code
		 (target %FrameTarget :implied)	; Render in this frame
		 (rel %LinkTypes :implied) ; Forward link types
		 (rev %LinkTypes :implied) ; Reverse link types
		 (accesskey %Character :implied) ; Accessibility key character
		 (shape %Shape rect)	; For client-side image maps
		 (coords %Coords :implied) ; For client-side image maps
		 (tabindex NUMBER :implied) ; Position in tabbing order
		 (onfocus %Script :implied) ; The element got the focus
		 (onblur %Script :implied) ; The element lost the focus
		 ))

;; ***Client-side image maps

(define-html-element map		; Client-side image map
    :content (:or (+ %block) (+ area))
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (name cdata :required)	; For reference by usemap
		 ))

(define-html-element area		; Client-side image map area
    :end-optional t
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (shape %Shape rect)	; Controls interpretation of coords
		 (coords %Coords :implied) ; Comma separated list of lengths
		 (href %uri :implied)	; URI for linked resource
		 (target %FrameTarget :implied)	; Render in this frame
		 (nohref (nohref) :implied) ; This region has no action
		 (alt %Text :required)	; Short description
		 (tabindex number :implied) ; Position in tabbing order
		 (accesskey %Character :implied) ; Accessibility key character
		 (onfocus %Script :implied) ; The element got the focus
		 (onblur %Script :implied) ; The element lost the focus
		 ))

;; ***The LINK Element

(define-html-element link		; A media-independent link
    :end-optional t
    :attributes (%attrs			; %coreattrs, %i18n, %events 
		 (charset %Charset :IMPLIED) ; Char encoding of linked resource
		 (href %URI :IMPLIED)	; URI for linked resource
		 (hreflang %LanguageCode :IMPLIED) ; Language code
		 (type %ContentType :IMPLIED) ; Advisory content type
		 (rel %LinkTypes :IMPLIED) ; Forward link types
		 (rev %LinkTypes :IMPLIED) ; Reverse link types
		 (media %MediaDesc :IMPLIED) ; For rendering on these media
		 (target %FrameTarget :IMPLIED)	; Render in this frame
		 ))

;; ***Images

(define-html-element img		; Embedded image
    :end-optional t
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (src %URI :REQUIRED)	; URI of image to embed
		 (alt %Text :REQUIRED)	; Short description
		 (longdesc %URI :IMPLIED) ; Link to long description
					  ; (complements alt)
		 (height %Length :IMPLIED) ; Override height
		 (width %Length :IMPLIED) ; Override width
		 (usemap %URI :IMPLIED)	; Use client-side image map
		 (ismap (ismap) :IMPLIED) ; Use server-side image map
		 (align %IAlign :IMPLIED) ; Vertical or horizontal alignment
		 (border %Length :IMPLIED) ; Link border width
		 (hspace %Pixels :IMPLIED) ; Horizontal gutter
		 (vspace %Pixels :IMPLIED) ; Vertical gutter
		 ))

;; ***Object

(define-html-element object
    :content (* (:or param %flow))
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (declare (declare) :implied) ; Declare but don't instantiate
		 (classid %URI :implied) ; Identifies an implementation
		 (codebase %URI :implied) ; Base URI for classid, data, archive
		 (data %URI :implied)	; Reference to object's data
		 (type %ContentType :implied) ; Content type for data
		 (codetype %ContentType :implied) ; Content type for code
		 (archive %URI :implied) ; Space separated archive list
		 (standby %Text :implied) ; Message to show while loading
		 (height %Length :implied) ; Override height
		 (width %Length :implied) ; Override width
		 (usemap %URI :implied) ; Use client-side image map
		 (name CDATA :implied)	; Submit as part of form
		 (tabindex NUMBER :implied) ; Position in tabbing order
		 (align %IAlign :implied) ; Vertical or horizontal alignment
		 (border %Length :implied) ; Link border width
		 (hspace %Pixels :implied) ; Horizontal gutter
		 (vspace %Pixels :implied) ; Vertical gutter
		 ))

(define-html-element param
    :end-optional t
    :attributes ((id ID :implied)	; Document-wide unique id
		 (name CDATA :required) ; Property name
		 (value CDATA :implied) ; Property value
		 (valuetype (:or DATA REF OBJECT) data) ; How to interpret val
		 (type %ContentType :implied) ; Content type (valuetype = ref)
		 ))

;; *** Java APPLET

(define-html-element applet
    :content (* (:or param %flow))
    :attributes (%coreattrs		; id, class, style, title
		 (codebase %URI :implied) ; Optional base URI for applet
		 (archive CDATA :implied) ; Comma separated archive list
		 (code CDATA :implied)	; Applet class file
		 (object CDATA :implied) ; Serialized applet file
		 (alt %Text :implied)	; Short description
		 (name CDATA :implied)	; Allows applets to find each other
		 (width %Length :required) ; Initial width
		 (height %Length :required) ; Initial height
		 (align %IAlign :implied) ; Vertical or horizontal alignment
		 (hspace %Pixels :implied) ; Horizontal gutter
		 (vspace %Pixels :implied) ; Vertical gutter))
		 ))

(define-html-element hr			; Horizontal rule
    :end-optional t
    :attributes (%coreattrs		; id, class, style, title
		 %events
		 (align (:or left center right) :implied)
		 (noshade (noshade) :implied)
		 (size %Pixels :implied)
		 (width %Length :implied)))

;; ***Paragraphs

(define-html-element p			; Paragraph
    :end-optional t
    :content (* %inline)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 %align			; Align, text alignment
		 ))

;; ***Headings

(define-html-element %heading		; Heading
    :content (* %inline)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 %align			; Align, text alignment
		 ))

;; ***Preformatted text

(define-html-element pre		; Preformatted text
    :content (* %inline)
    :exclusions %pre.exclusion
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (width number :implied)))

;; ***Inline quotes

(define-html-element Q			; Short inline quotation
    :content (* %inline)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (cite %URI :implied)	; URI for source document or msg
		 ))

;; ***Block-like Quotes

(define-html-element blockquote		; Long quotation
    :content (* %flow)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (cite %URI :implied)	; URI for source document or msg
		 ))

;; ***Inserted/Deleted Text

(define-html-element (:or ins del)	; Inserted text, deleted text
    :content (* %flow)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (cite %URI :implied)	; Info on reason for change
		 (datetime %Datetime :implied) ; Date and time of change
		 ))

;; ***Lists

;; Definition lists - DT for term, DD for its definition

(define-html-element DL			; Definition list
    :content (+ (:or DT DD))
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (compact (compact) :implied) ; Reduced interitem spacing
		 ))
(define-html-element DT			; Definition term
    :end-optional t
    :content (* %inline)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 ))

(define-html-element DD			; Definition description
    :end-optional t
    :content (* %flow)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 ))

;; Ordered lists (OL) Numbering style
;;
;;   1   arablic numbers     1, 2, 3, ...
;;   a   lower alpha         a, b, c, ...
;;   A   upper alpha         A, B, C, ...
;;   i   lower roman         i, ii, iii, ...
;;   I   upper roman         I, II, III, ...
;;
;;   The style is applied to the sequence number which by default
;;   is reset to 1 for the first list item in an ordered list.
;;
;;   This can't be expressed directly in SGML due to case folding.

(define-html-element OL			; Ordered list
    :content (+ LI)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (type %OLStyle :implied) ; Numbering style
		 (compact (compact) :implied) ; Reduced interitem spacing
		 (start NUMBER :implied) ; Starting sequence number
		 ))

(define-html-element UL			; Unordered list
    :content (+ LI)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (type %ULStyle :implied) ; Bullet style
		 (compact (compact) :implied) ; Reduced interitem spacing
		 ))

(define-html-element DIR		; Directory list, menu list
    :content (+ LI)
    :exclusions %block
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (compact (compact) :implied)))

(define-html-element MENU		; Directory list, menu list
    :content (+ LI)
    :exclusions %block
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (compact (compact) :implied)))

(define-html-element LI			; List item
    :end-optional t
    :content (* %flow)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (type %LIStyle :implied) ; List item style
		 (value NUMBER :implied) ; Reset sequence number
		 ))

;; ***Forms

(define-html-element FORM		; Interactive form
    :content (* %flow)
    :exclusions FORM
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (action %URI :required) ; Server-side form handler
		 (method (:or GET POST) get) ; HTTP method for submit
		 (enctype %ContentType "application/x-www-form-urlencoded")
		 (onsubmit %Script :implied) ; The form was submitted
		 (onreset %Script :implied) ; The form was reset
		 (target %FrameTarget :implied) ; Render in this frame
		 (accept-charset %charsets :implied) ; Supported charsets
		 ))

;; Each label must not contain more than ONE field

(define-html-element LABEL		; Form field label text
    :content (* %inline)
    :exclusions LABEL
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (for IDREF :implied)	; Matches field ID value
		 (accesskey %Character :implied) ; Accessibility key character
		 (onfocus %Script :implied) ; The element got the focus
		 (onblur %Script :implied) ; The element lost the focus
		 ))

;; Attribute name required for all but submit & reset

(define-html-element INPUT		; Form control
    :end-optional t
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (type %InputType text) ; What kind of widget is needed
		 (name CDATA :implied)	; Submit as part of form
		 (value CDATA :implied) ; Required for radio and checkboxes
		 (checked (checked) :implied) ; For radio buttons & check boxes
		 (disabled (disabled) :implied) ; Unavailable in this context
		 (readonly (readonly) :implied) ; For text and passwd
		 (size CDATA :implied)	; Specific to each type of field
		 (maxlength NUMBER :implied) ; Max chars for text fields
		 (src %URI :implied)	; For fields with images
		 (alt CDATA :implied)	; Short description
		 (usemap %URI :implied) ; Use client-side image map
		 (tabindex NUMBER :implied) ; Position in tabbing order
		 (accesskey %Character :implied) ; Accessibility key character
		 (onfocus %Script :implied) ; The element got the focus
		 (onblur %Script :implied) ; The element lost the focus
		 (onselect %Script :implied) ; Some text was selected
		 (onchange %Script :implied) ; The element value was changed
		 (accept %ContentTypes :implied) ; MIME types for file upload
		 (align %IAlign :implied) ; Vertical or horizontal alignment
		 ))

(define-html-element SELECT		; Option selector
    :content (+ (:or OPTGROUP OPTION))
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (name CDATA :implied)	; Field name
		 (size NUMBER :implied) ; Rows visible
		 (multiple (multiple) :implied) ; Default is single selection
		 (disabled (disabled) :implied) ; Unavailable in this context
		 (tabindex NUMBER :implied) ; Position in tabbing order
		 (onfocus %Script :implied) ; The element got the focus
		 (onblur %Script :implied) ; The element lost the focus
		 (onchange %Script :implied) ; The element value was changed
		 ))

(define-html-element OPTGROUP		; Option group
    :content (+ OPTION)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (disabled (disabled) :implied) ; Unavailable in this context
		 (label %Text :required) ; For use in hierarchical menus
		 ))

(define-html-element OPTION		; Selectable choice
    :end-optional t
    :content PCDATA
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (selected (selected) :implied)
		 (disabled (disabled) :implied) ; Unavailable in this context
		 (label %Text :implied) ; For use in hierarchical menus
		 (value CDATA :implied) ; Defaults to element content
		 ))

(define-html-element TEXTAREA		; Multi-line text field
    :content CDATA
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (name CDATA :implied) 
		 (rows NUMBER :required)
		 (cols NUMBER :required)
		 (disabled (disabled) :implied) ; Unavailable in this context
		 (readonly (readonly) :implied)
		 (tabindex NUMBER :implied) ; Position in tabbing order
		 (accesskey %Character :implied) ; Accessibility key character
		 (onfocus %Script :implied) ; The element got the focus
		 (onblur %Script :implied) ; The element lost the focus
		 (onselect %Script :implied) ; Some text was selected
		 (onchange %Script :implied) ; The element value was changed
		 ))

;;  PCDATA is to solve the mixed content problem,
;;  per specification only whitespace is allowed there!

(define-html-element FIELDSET		; Form control group
    :content (:sequence (PCDATA LEGEND (* %flow)))
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 ))

(define-html-element LEGEND		; Fieldset legend
    :content (* %inline)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (accesskey %Character :implied) ; Accessibility key character
		 (align %LAlign :implied) ; Relative to fieldset
		 ))

(define-html-element BUTTON		; Push button
    :content (* %flow)
    :exclusions (:or A %formctrl FORM ISINDEX FIELDSET IFRAME)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (name CDATA :implied)
		 (value CDATA :implied) ; Sent to server when submitted
		 (type (:or button submit reset) submit) ; For use as button
		 (disabled (disabled) :implied) ; Unavailable in this context
		 (tabindex NUMBER :implied) ; Position in tabbing order
		 (accesskey %Character :implied) ; Accessibility key character
		 (onfocus %Script :implied) ; The element got the focus
		 (onblur %Script :implied) ; The element lost the focus
		 ))

;; ***Tables

;; IETF HTML table standard, see [RFC1942]
;;
;; The BORDER attribute sets the thickness of the frame around the
;; table. The default units are screen pixels.
;;
;; The FRAME attribute specifies which parts of the frame around
;; the table should be rendered. The values are not the same as
;; CALS to avoid a name clash with the VALIGN attribute.
;;
;; The value "border" is included for backwards compatibility with
;; <TABLE BORDER> which yields frame=border and border=implied
;; For <TABLE BORDER=1> you get border=1 and frame=implied. In this
;; case, it is appropriate to treat this as frame=border for backwards
;; compatibility with deployed browsers.
;;
;; The RULES attribute defines which rules to draw between cells:
;;
;; If RULES is absent then assume:
;;     "none" if BORDER is absent or BORDER=0 otherwise "all"

(define-html-element TABLE
    :content (:sequence (? CAPTION) (:or (* COL) (* COLGROUP)) 
			(? THEAD) (? TFOOT) (+ TBODY))
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (summary %Text :implied) ; Purpose/structure for speech output
		 (width %Length :implied) ; Table width
		 (border %Pixels :implied) ; Controls frame width around table
		 (frame %TFrame :implied) ; Which parts of frame to render
		 (rules %TRules :implied) ; Rulings between rows and cols
		 (cellspacing %Length :implied) ; Spacing between cells
		 (cellpadding %Length :implied) ; Spacing within cells
		 (align %TAlign :implied) ; Table position relative to window
		 (bgcolor %Color :implied) ; Background color for cells
		 ))

(define-html-element CAPTION		; Table caption
    :content (* %inline)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (align %CAlign :implied) ; Relative to table
		 ))

;;    Use THEAD to duplicate headers when breaking table
;;    across page boundaries, or for static headers when
;;    TBODY sections are rendered in scrolling panel.

(define-html-element THEAD		; Table header
    :end-optional t
    :content(+ TR)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 %cellhalign		; Horizontal alignment in cells
		 %cellvalign		; Vertical alignment in cells
		 ))

;;    Use TFOOT to duplicate footers when breaking table
;;    across page boundaries, or for static footers when
;;    TBODY sections are rendered in scrolling panel.

(define-html-element TFOOT		; Table footer
    :end-optional t
    :content (+ TR)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 %cellhalign		; Horizontal alignment in cells
		 %cellvalign		; Vertical alignment in cells
		 ))

;;    Use multiple TBODY sections when rules are needed
;;    between groups of table rows.

(define-html-element TBODY		; Table body
    :start-optional t
    :end-optional t
    :content (+ TR)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 %cellhalign		; Horizontal alignment in cells
		 %cellvalign		; Vertical alignment in cells
		 ))

;;COLGROUP groups a set of COL elements. It allows you to group
;;several semantically related columns together.

(define-html-element COLGROUP		; Table column group
    :end-optional t
    :content (* col)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (span NUMBER 1)	; Default number of columns in group
		 (width %MultiLength :implied) ; Default width enclosed COLs
		 %cellhalign		; Horizontal alignment in cells
		 %cellvalign		; Vertical alignment in cells
		 ))

;; COL elements define the alignment properties for cells in
;; one or more columns.
;;
;; The WIDTH attribute specifies the width of the columns, e.g.
;;
;;     width=64        width in screen pixels
;;     width=0.5*      relative width of 0.5
;;
;; The SPAN attribute causes the attributes of one
;; COL element to apply to more than one column.

(define-html-element COL		; Table column
    :end-optional t
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (span NUMBER 1)	; COL attributes affect N columns
		 (width %MultiLength :implied) ; Column width specification
		 %cellhalign		; Horizontal alignment in cells
		 %cellvalign		; Vertical alignment in cells
		 ))

(define-html-element TR			; Table row
    :end-optional t
    :content (+ (:or TH TD))
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 %cellhalign		; Horizontal alignment in cells
		 %cellvalign		; Vertical alignment in cells
		 (bgcolor %Color :implied) ; Background color for row
		 ))

(define-html-element (:or TH TD)	; Table header cell, table data cell
    :end-optional t
    :content (* %flow)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (abbr %Text :implied)	; Abbreviation for header cell
		 (axis CDATA :implied)	; Names groups of related header
		 (headers IDREFS :implied) ; List of id's for header cells
		 (scope %Scope :implied) ; Scope covered by header cells
		 (rowspan NUMBER 1)	; Number of rows spanned by cell
		 (colspan NUMBER 1)	; Number of cols spanned by cell
		 %cellhalign		; Horizontal alignment in cells
		 %cellvalign		; Vertical alignment in cells
		 (nowrap (nowrap) :implied) ; Suppress word wrap
		 (bgcolor %Color :implied) ; Cell background color
		 (width %Pixels :implied) ; Width for cell
		 (height %Pixels :implied) ; Height for cell
		 ))

;; ***Document Frames

;;  The content model for HTML documents depends on whether the HEAD is
;;  followed by a FRAMESET or BODY element. The widespread omission of
;;  the BODY start tag makes it impractical to define the content model
;;  without the use of a marked section.

(define-html-element IFRAME		; Inline subwindow
    :content (* %flow)
    :attributes (%coreattrs		; id, class, style, title
		 (longdesc %URI :implied) ; Link to long description
		 (name CDATA :implied)	; Name of frame for targetting
		 (src %URI :implied)	; Source of frame content
		 (frameborder (:or 1 0) 1) ; Request frame borders?
		 (marginwidth %Pixels :implied) ; Margin widths in pixels
		 (marginheight %Pixels :implied) ; Margin height in pixels
		 (scrolling (:or yes no auto) auto) ; Scrollbar or none
		 (align %IAlign :implied) ; Vertical or horizontal alignment
		 (height %Length :implied) ; Frame height
		 (width %Length :implied) ; Frame width
		 ))

(define-html-element NOFRAMES		; Alternate content container for 
					; non-frame-based rendering
    :content %noframes.content
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 ))

;; ***Document Head

(define-html-element HEAD		; Document head
    :start-optional t
    :end-optional t
    :content %head.content
    :inclusions %head.misc
    :attributes (%lspattrs		; LSP-specific attributes
		 %i18n			; Lang, dir
		 (profile %URI :implied) ; Named dictionary of meta info
		 ))

;; The TITLE element is not considered part of the flow of text.
;; It should be displayed, for example as the page header or
;; window title. Exactly one title is required per document.

(define-html-element TITLE		; Document title
    :content PCDATA
    :exclusions %head.misc
    :attributes (%i18n))

(define-html-element ISINDEX		; Single line prompt
    :end-optional t
    :attributes (%coreattrs		; id, class, style, title
		 %i18n			; Lang, dir
		 (prompt %Text :implied) ; Prompt message
		 ))

(define-html-element BASE		; Document base URI
    :end-optional t
    :attributes ((href %URI :implied)	; URI that acts as base URI
		 (target %FrameTarget :implied) ; Render in this frame
		 ))

(define-html-element META		; Generic metainformation
    :end-optional t
    :attributes (%i18n			; Lang, dir, for use with content
		 (http-equiv NAME :implied) ; HTTP response header name
		 (name NAME :implied)	; Metainformation name
		 (content CDATA :required) ; Associated information
		 (scheme CDATA :implied) ; Select form of content
		 ))

(define-html-element STYLE		; Style info
    :content %StyleSheet
    :attributes (%i18n			; Lang, dir, for use with content
		 (type %ContentType :required) ; Content type of style language
		 (media %MediaDesc :implied) ; Designed for these media
		 (title %Text :implied) ; Advisory title
		 ))

(define-html-element SCRIPT		; Script statements
    :content %Script
    :attributes ((charset %Charset :implied) ; Char encoding of linked resource
		 (type %ContentType :required) ; Content type of script
		 (language CDATA :implied) ; Predefined script language name
		 (src %URI :implied)	; URI for an external script
		 (defer (defer) :implied) ; UA may defer execution of script
		 ))

(define-html-element NOSCRIPT		; Alternate content container for
					; non-script-based rendering
    :content (* %flow)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 ))

;; ***LSP Script

(define-html-element EVAL		; Eval and print form
    :content %Script)

(define-html-element FLET		; Declare a label
    :content (* %flow)
    :exclusions (:or %formctrl ISINDEX FIELDSET IFRAME)
    :attributes (%lspattrs		; LSP-specific attributes
		 %attrs			; %coreattrs, %i18n, %events
		 %align			; align, text alignment
		 ))

(define-html-element LISP		; Insert Lisp forms
    :content %Script)

;; ***Document Structure

(define-html-element PACKAGE		; Package declaration and defs
    :content %Script
    :attributes ((name CDATA :required)
		 (uses CDATA :implied)
		 ))

(define-html-element HTML		; Document root element
    :start-optional t
    :end-optional t
    :content %html.content
    :attributes ((name CDATA :implied)
		 %i18n			; Lang, dir
		 ))


;;; Characters

(define-html-characters
    "copy" "reg" "amp" "gt" "lt" "quot" "nbsp"
    "AElig" "Aacute" "Acirc" "Agrave"
    "Aring" "Atilde" "Auml" "Ccedil"
    "ETH" "Eacute" "Ecirc" "Egrave"
    "Euml" "Iacute" "Icirc" "Igrave"
    "Iuml" "Ntilde" "Oacute" "Ocirc"
    "Ograve" "Oslash" "Otilde" "Ouml"
    "THORN" "Uacute" "Ucirc" "Ugrave"
    "Uuml" "Yacute" "aacute" "acirc"
    "aelig" "agrave" "aring" "atilde"
    "auml" "ccedil" "eacute" "edirc"
    "egrave" "eth" "euml" "iacute"
    "icirc" "igrave" "iuml" "ntilde"
    "oacute" "ocirc" "ograve" "oslash"
    "otilde" "ouml" "quot" "szlig"
    "thorn" "uacute" "ucirc" "ugrave"
    "uuml" "yacute" "yuml")

;;; EOF
