;;; -*- Syntax: Ansi-Common-Lisp; Package: CL-USER; Base: 10; Mode: lisp -*-

;;; File: html-4-0-transitional.lisp
;;; Last edited by smishra on Wed Sep  2 16:31:46 1998

;;; (c) Copyright 1996-97, Sunil Mishra (smishra@cc.gatech.edu)
;;;     All Rights Reserved

(in-package :html-parser)

;;; This file contains a lispified transcription of the
;;;    HTML 4.0 Transitional DTD. It does not do frames.
;;; Based on the DTD at
;;;    http://www.w3.org/TR/1998/REC-html40-19980424/loose.dtd
;;; All entities beginning with a % are special SGML declarations

;;;   :set	one or more of these is allowed, in any order
;;;   :sequence	all of these are required, in the given order
;;;   :or	one of these is allowed
;;;   :and	all of these are required, in any order

;;;   +		one or more
;;;   *		zero or more
;;;   ?		zero or one

;;; All entity definitions MUST precede element definitions, otherwise
;;; writing the code to process the definitions becomes too much of a
;;; hassle

;;; All comments beginning with *** are taken from the DTD.

;;;----------------------------------------
;;; Entities

;;; ***Imported names

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

;;; ***Parameter Entities

(define-html-entity %head.misc
    (:or script style meta link object)) ; Repeatable head elements

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

;;; ***Character mnemonic entities

;;; !!!Skipped!!!

;;;<!ENTITY % HTMLlat1 PUBLIC
;;;   "-//W3C//ENTITIES Latin1//EN//HTML"
;;;   "http://www.w3.org/TR/1998/REC-html40-19980424/HTMLlat1.ent">
;;;%HTMLlat1;
;;;
;;;<!ENTITY % HTMLsymbol PUBLIC
;;;   "-//W3C//ENTITIES Symbols//EN//HTML"
;;;   "http://www.w3.org/TR/1998/REC-html40-19980424/HTMLsymbol.ent">
;;;%HTMLsymbol;
;;;
;;;<!ENTITY % HTMLspecial PUBLIC
;;;   "-//W3C//ENTITIES Special//EN//HTML"
;;;   "http://www.w3.org/TR/1998/REC-html40-19980424/HTMLspecial.ent">
;;;%HTMLspecial;

;;; ***Generic attributes

(define-html-entity %coreattrs
    ((id id :implied)			; Document-wide unique id
     (class cdata :implied)		; Space separated list of classes
     (style %StyleSheet :implied)	; Associated style info
     (title %Text :implied)		; advisory title/amplification
     ))

(define-html-entity %i18n
    ((lang %LanguageCode :implied)	; language code
     (dir (:or ltr rtl) :implied)	; direction for weak/neutral text
     ))

(define-html-entity %events
    ((onclick %Script :implied)		; a pointer button was clicked
     (ondblclick %Script :implied)	; a pointer button was double clicked
     (onmousedown %Script :implied)	; a pointer button was pressed down
     (onmouseup %Script :implied)  ; a pointer button was released
     (onmouseover %Script :implied) ; a pointer was moved onto
     (onmousemove %Script :implied) ; a pointer was moved within
     (onmouseout %Script :implied) ; a pointer was moved away
     (onkeypress %Script :implied) ; a key was pressed and released
     (onkeydown %Script :implied) ; a key was pressed down
     (onkeyup %Script :implied) ; a key was released
     ))

(define-html-entity %attrs
    (%coreattrs %i18n %events))

(define-html-entity %align		; default is left for ltr paragraphs, right for rtl
    ((align (:or left center right justify) :implied)))

;;; ***Text Markup

(define-html-entity %fontstyle
    (:or tt i b u s strike big small))

(define-html-entity %phrase
    (:or em strong dfn code samp kbd var cite abbr acronym))

(define-html-entity %special
    (:or a img applet object font basefont br script
	 map q sub sup span bdo iframe))

(define-html-entity %formctrl
    (:or input select textarea label button))

(define-html-entity %inline
    (:or pcdata %fontstyle %phrase %special %formctrl))

;;; ***HTML content models

(define-html-entity %block
    (:or p %heading %list %preformatted dl div center
	 noscript noframes blockquote form isindex hr
	 table fieldset address))

(define-html-entity %flow
    (:or %block %inline))

;;; ***The Anchor Element

(define-html-entity %Shape
    (:or rect circle poly default))

(define-html-entity %Coords
    cdata)				; comma separated list of lengths

;;; ***Images

(define-html-entity %Length
    cdata)				; nn for pixels or nn% for percentage length

(define-html-entity %MultiLength
    cdata)				; pixel, percentage, or relative

(define-html-entity %MultiLengths
    cdata)				; comma-separated list of MultiLength

(define-html-entity %Pixels
    cdata)				; integer representing length in pixels

(define-html-entity %IAlign
    (:or top middle bottom left right))	; center?

;;; ***Preformatted text

(define-html-entity %pre.exclusion
    (:or img object applet big small sub sup font basefont))

;;; ***Lists

(define-html-entity %OLStyle
    cdata)				; constrained to: "(1|a|A|i|I)"

(define-html-entity %ULStyle
    (:or disc square circle))

(define-html-entity %LIStyle
    cdata)				; constrained to: "(%ULStyle;|%OLStyle;)

;;; ***Forms

(define-html-entity %InputType
    (:or text password checkbox
	 radio submit reset
	 file hidden image button))

(define-html-entity %LAlign
    (:or top bottom left right))

;;; ***IETF HTML table standard, see [RFC1942]

(define-html-entity %TFrame
    (:or void above below hsides lhs rhs vsides box border))

(define-html-entity %TRules
    (:or none groups rows cols all))

(define-html-entity %TAlign
    (:or left center right))

(define-html-entity %cellhalign
    ((align (:or left center right justify char) :implied)
     (char %Character :implied)		; alignment char, e.g. char=':'
     (charoff %Length :implied)		; offset for alignment char
     ))

(define-html-entity %cellvalign
    ((valign (:or top middle bottom baseline) :implied)))

(define-html-entity %CAlign
    (:or top bottom left right))

(define-html-entity %Scope
    (:or row col rowgroup colgroup))

;;; ***Document Frames

;;; Since this DTD does not include frames...

(define-html-entity %noframes.content
    (* %flow))

;;; ***Document Head

(define-html-entity %head.content
    (:and title
	  (? isindex)
	  (? base)))

;;; ***Document Structure

;;; Skipping version

;;;<!ENTITY % version "version CDATA #FIXED '%HTML.Version;'">

(define-html-entity %html.content
  (:sequence head body))

;;;----------------------------------------
;;; Elements

;;; I have left out the attributes %version.attr because it cannot be
;;; set by the user; it is of no consequence to parsing.

;;; ***Text Markup

(define-html-element (:or %fontstyle %phrase)
    :content (* %inline)
    :attributes (%attrs			; %coreattrs, %i18n, %events
    ))

(define-html-element (:or sub sup)	; subscript, superscript
    :content (* %inline)
    :attributes (%attrs			; %coreattrs, %i18n, %events
    ))

(define-html-element span		; generic language/style container
    :content (* %inline)
    :attributes (%attrs			; %coreattrs, %i18n, %events
    ))

(define-html-element bdo
    :content (* %inline)		; I18N BiDi over-ride
    :attributes (%coreattrs		; id, class, style, title
		 (lang %LanguageCode :implied) ; language code
		 (dir (:or ltr rtl) :required) ;  directionality
		 ))

(define-html-element basefont		; base font size
    :end-optional t
    :attributes ((id id :implied)	; document-wide unique id
		 (size cdata :required)	; base font size for FONT elements
		 (color %Color :implied) ; text color
		 (face cdata :implied)	; comma separated list of font names
		 ))

(define-html-element font		; local change to font
    :content (* %inline)
    :attributes (%coreattrs		; id, class, style, title
		 %i18n			; lang, dir
		 (size cdata :implied)	; [+|-]nn e.g. size="+1", size="4"
		 (color cdata :implied)	; text color
		 (face cdata :implied)	; comma separated list of font names
		 ))

(define-html-element br			; forced line break
    :end-optional t
    :attributes (%coreattrs		; id, class, style, title
		 (clear (:or left all right none) none) ; control of text flow
		 ))

;;; ***Document Body

(define-html-element body		; document body
    :start-optional t
    :end-optional t
    :content (* %flow)
    :inclusions (:or ins del)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (onload %Script :implied) ; the document has been loaded
		 (onunload %Script :implied) ; the document has been removed
		 (background %uri :implied) ; texture tile for document background
		 %bodycolors		; bgcolor, text, link, vlink, alink
		 ))

(define-html-element address
    :content (* (:or %inline p))	; information on author
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 ))

(define-html-element div		; generic language/style container
    :content (* %flow)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 %align			; align, text alignment
		 ))

(define-html-element center		; shorthand for DIV align=center
    :content (* %flow)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 ))

;;; ***The Anchor Element

(define-html-element a			; anchor
    :content (* %inline)
    :exclusions a
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (charset %Charset :implied) ; char encoding of linked resource
		 (type %ContentType :implied) ; advisory content type
		 (name cdata :implied)	; named link end
		 (href %uri :implied)	; URI for linked resource
		 (hreflang %LanguageCode :implied) ; language code
		 (target %FrameTarget :implied)	; render in this frame
		 (rel %LinkTypes :implied) ; forward link types
		 (rev %LinkTypes :implied) ; reverse link types
		 (accesskey %Character :implied) ; accessibility key character
		 (shape %Shape rect)	; for use with client-side image maps
		 (coords %Coords :implied) ; for use with client-side image maps
		 (tabindex NUMBER :implied) ; position in tabbing order
		 (onfocus %Script :implied) ; the element got the focus
		 (onblur %Script :implied) ; the element lost the focus
		 ))

;;; ***Client-side image maps

(define-html-element map		; client-side image map
    :content (:or (+ %block) (+ area))
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (name cdata :required)	; for reference by usemap
		 ))

(define-html-element area		; client-side image map area
    :end-optional t
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (shape %Shape rect)	; controls interpretation of coords
		 (coords %Coords :implied) ; comma separated list of lengths
		 (href %uri :implied)	; URI for linked resource
		 (target %FrameTarget :implied)	; render in this frame
		 (nohref (nohref) :implied) ; this region has no action
		 (alt %Text :required)	; short description
		 (tabindex number :implied) ; position in tabbing order
		 (accesskey %Character :implied) ; accessibility key character
		 (onfocus %Script :implied) ; the element got the focus
		 (onblur %Script :implied) ; the element lost the focus
		 ))

;;; ***The LINK Element

(define-html-element link		; a media-independent link
    :end-optional t
    :attributes (%attrs			; %coreattrs, %i18n, %events 
		 (charset %Charset :IMPLIED) ; char encoding of linked resource
		 (href %URI :IMPLIED)	; URI for linked resource
		 (hreflang %LanguageCode :IMPLIED) ; language code
		 (type %ContentType :IMPLIED) ; advisory content type
		 (rel %LinkTypes :IMPLIED) ; forward link types
		 (rev %LinkTypes :IMPLIED) ; reverse link types
		 (media %MediaDesc :IMPLIED) ; for rendering on these media
		 (target %FrameTarget :IMPLIED)	; render in this frame
		 ))

;;; ***Images

(define-html-element img		; Embedded image
    :end-optional t
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (src %URI :REQUIRED)	; URI of image to embed
		 (alt %Text :REQUIRED)	; short description
		 (longdesc %URI :IMPLIED) ; link to long description (complements alt)
		 (height %Length :IMPLIED) ; override height
		 (width %Length :IMPLIED) ; override width
		 (usemap %URI :IMPLIED)	; use client-side image map
		 (ismap (ismap) :IMPLIED) ; use server-side image map
		 (align %IAlign :IMPLIED) ; vertical or horizontal alignment
		 (border %Length :IMPLIED) ; link border width
		 (hspace %Pixels :IMPLIED) ; horizontal gutter
		 (vspace %Pixels :IMPLIED) ; vertical gutter
		 ))

;;; ***Object

(define-html-element object
    :content (* (:or param %flow))
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (declare (declare) :implied) ; declare but don't instantiate flag
		 (classid %URI :implied) ; identifies an implementation
		 (codebase %URI :implied) ; base URI for classid, data, archive
		 (data %URI :implied)	; reference to object's data
		 (type %ContentType :implied) ; content type for data
		 (codetype %ContentType :implied) ; content type for code
		 (archive %URI :implied) ; space separated archive list
		 (standby %Text :implied) ; message to show while loading
		 (height %Length :implied) ; override height
		 (width %Length :implied) ; override width
		 (usemap %URI :implied) ; use client-side image map
		 (name CDATA :implied)	; submit as part of form
		 (tabindex NUMBER :implied) ; position in tabbing order
		 (align %IAlign :implied) ; vertical or horizontal alignment
		 (border %Length :implied) ; link border width
		 (hspace %Pixels :implied) ; horizontal gutter
		 (vspace %Pixels :implied) ; vertical gutter
		 ))

(define-html-element param
    :end-optional t
    :attributes ((id ID :implied)	; document-wide unique id
		 (name CDATA :required) ; property name
		 (value CDATA :implied) ; property value
		 (valuetype (:or DATA REF OBJECT) data) ; How to interpret value
		 (type %ContentType :implied) ; content type for value when valuetype=ref
		 ))

;;; *** Java APPLET

(define-html-element applet
    :content (* (:or param %flow))
    :attributes (%coreattrs		; id, class, style, title
		 (codebase %URI :implied) ; optional base URI for applet
		 (archive CDATA :implied) ; comma separated archive list
		 (code CDATA :implied)	; applet class file
		 (object CDATA :implied) ; serialized applet file
		 (alt %Text :implied)	; short description
		 (name CDATA :implied)	; allows applets to find each other
		 (width %Length :required) ; initial width
		 (height %Length :required) ; initial height
		 (align %IAlign :implied) ; vertical or horizontal alignment
		 (hspace %Pixels :implied) ; horizontal gutter
		 (vspace %Pixels :implied) ; vertical gutter))
		 ))

(define-html-element hr			; horizontal rule
    :end-optional t
    :attributes (%coreattrs		; id, class, style, title
		 %events
		 (align (:or left center right) :implied)
		 (noshade (noshade) :implied)
		 (size %Pixels :implied)
		 (width %Length :implied)))

;;; ***Paragraphs

(define-html-element p			; paragraph
    :end-optional t
    :content (* %inline)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 %align			; align, text alignment
		 ))

;;; ***Headings

(define-html-element %heading		; heading
    :content (* %inline)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 %align			; align, text alignment
		 ))

;;; ***Preformatted text

(define-html-element pre		; preformatted text
    :content (* %inline)
    :exclusions %pre.exclusion
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (width number :implied)))

;;; ***Inline quotes

(define-html-element Q			; short inline quotation
    :content (* %inline)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (cite %URI :implied)	; URI for source document or msg
		 ))

;;; ***Block-like Quotes

(define-html-element blockquote		; long quotation
    :content (* %flow)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (cite %URI :implied)	; URI for source document or msg
		 ))

;;; ***Inserted/Deleted Text

(define-html-element (:or ins del)	; inserted text, deleted text
    :content (* %flow)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (cite %URI :implied)	; info on reason for change
		 (datetime %Datetime :implied) ; date and time of change
		 ))

;;; ***Lists

;;; definition lists - DT for term, DD for its definition

(define-html-element DL			; definition list
    :content (+ (:or DT DD))
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (compact (compact) :implied) ; reduced interitem spacing
		 ))
(define-html-element DT			; definition term
    :end-optional t
    :content (* %inline)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 ))

(define-html-element DD			; definition description
    :end-optional t
    :content (* %flow)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 ))

;;; Ordered lists (OL) Numbering style
;;;
;;;    1   arablic numbers     1, 2, 3, ...
;;;    a   lower alpha         a, b, c, ...
;;;    A   upper alpha         A, B, C, ...
;;;    i   lower roman         i, ii, iii, ...
;;;    I   upper roman         I, II, III, ...
;;;
;;;    The style is applied to the sequence number which by default
;;;    is reset to 1 for the first list item in an ordered list.
;;;
;;;    This can't be expressed directly in SGML due to case folding.

(define-html-element OL			; ordered list
    :content (+ LI)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (type %OLStyle :implied) ; numbering style
		 (compact (compact) :implied) ; reduced interitem spacing
		 (start NUMBER :implied) ; starting sequence number
		 ))

(define-html-element UL			; unordered list
    :content (+ LI)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (type %ULStyle :implied) ; bullet style
		 (compact (compact) :implied) ; reduced interitem spacing
		 ))

(define-html-element DIR		; directory list, menu list
    :content (+ LI)
    :exclusions %block
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (compact (compact) :implied)))

(define-html-element MENU		; directory list, menu list
    :content (+ LI)
    :exclusions %block
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (compact (compact) :implied)))

(define-html-element LI			; list item
    :end-optional t
    :content (* %flow)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (type %LIStyle :implied) ; list item style
		 (value NUMBER :implied) ; reset sequence number
		 ))

;;; ***Forms

(define-html-element FORM		; interactive form
    :content (* %flow)
    :exclusions FORM
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (action %URI :required) ; server-side form handler
		 (method (:or GET POST) get) ; HTTP method used to submit the for
		 (enctype %ContentType "application/x-www-form-urlencoded")
		 (onsubmit %Script :implied) ; the form was submitted
		 (onreset %Script :implied) ; the form was reset
		 (target %FrameTarget :implied) ; render in this frame
		 (accept-charset %charsets :implied) ; list of supported charsets
		 ))

;;; Each label must not contain more than ONE field

(define-html-element LABEL		; form field label text
    :content (* %inline)
    :exclusions LABEL
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (for IDREF :implied)	; matches field ID value
		 (accesskey %Character :implied) ; accessibility key character
		 (onfocus %Script :implied) ; the element got the focus
		 (onblur %Script :implied) ; the element lost the focus
		 ))

;;; attribute name required for all but submit & reset

(define-html-element INPUT		; form control
    :end-optional t
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (type %InputType text) ; what kind of widget is needed
		 (name CDATA :implied)	; submit as part of form
		 (value CDATA :implied) ; required for radio and checkboxes
		 (checked (checked) :implied) ; for radio buttons and check boxes
		 (disabled (disabled) :implied) ; unavailable in this context
		 (readonly (readonly) :implied) ; for text and passwd
		 (size CDATA :implied)	; specific to each type of field
		 (maxlength NUMBER :implied) ; max chars for text fields
		 (src %URI :implied) ; for fields with images
		 (alt CDATA :implied)	; short description
		 (usemap %URI :implied) ; use client-side image map
		 (tabindex NUMBER :implied) ; position in tabbing order
		 (accesskey %Character :implied) ; accessibility key character
		 (onfocus %Script :implied) ; the element got the focus
		 (onblur %Script :implied) ; the element lost the focus
		 (onselect %Script :implied) ; some text was selected
		 (onchange %Script :implied) ; the element value was changed
		 (accept %ContentTypes :implied) ; list of MIME types for file upload
		 (align %IAlign :implied) ; vertical or horizontal alignment
		 ))

(define-html-element SELECT		; option selector
    :content (+ (:or OPTGROUP OPTION))
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (name CDATA :implied)	; field name
		 (size NUMBER :implied) ; rows visible
		 (multiple (multiple) :implied) ; default is single selection
		 (disabled (disabled) :implied) ; unavailable in this context
		 (tabindex NUMBER :implied) ; position in tabbing order
		 (onfocus %Script :implied) ; the element got the focus
		 (onblur %Script :implied) ; the element lost the focus
		 (onchange %Script :implied) ; the element value was changed
		 ))

(define-html-element OPTGROUP		; option group
    :content (+ OPTION)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (disabled (disabled) :implied) ; unavailable in this context
		 (label %Text :required) ; for use in hierarchical menus
		 ))

(define-html-element OPTION		; selectable choice
    :end-optional t
    :content PCDATA
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (selected (selected) :implied)
		 (disabled (disabled) :implied) ; unavailable in this context
		 (label %Text :implied) ; for use in hierarchical menus
		 (value CDATA :implied) ; defaults to element content
		 ))

(define-html-element TEXTAREA		; multi-line text field
    :content PCDATA
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (name CDATA :implied) 
		 (rows NUMBER :required)
		 (cols NUMBER :required)
		 (disabled (disabled) :implied) ; unavailable in this context
		 (readonly (readonly) :implied)
		 (tabindex NUMBER :implied) ; position in tabbing order
		 (accesskey %Character :implied) ; accessibility key character
		 (onfocus %Script :implied) ; the element got the focus
		 (onblur %Script :implied) ; the element lost the focus
		 (onselect %Script :implied) ; some text was selected
		 (onchange %Script :implied) ; the element value was changed
		 ))

;;;  PCDATA is to solve the mixed content problem,
;;;  per specification only whitespace is allowed there!

(define-html-element FIELDSET		; form control group
    :content (:sequence (PCDATA LEGEND (* %flow)))
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 ))

(define-html-element LEGEND		; fieldset legend
    :content (* %inline)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (accesskey %Character :implied) ; accessibility key character
		 (align %LAlign :implied) ; relative to fieldset
		 ))

(define-html-element BUTTON		; push button
    :content (* %flow)
    :exclusions (:or A %formctrl FORM ISINDEX FIELDSET IFRAME)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (name CDATA :implied)
		 (value CDATA :implied) ; sent to server when submitted
		 (type (:or button submit reset) submit) ; for use as form button
		 (disabled (disabled) :implied) ; unavailable in this context
		 (tabindex NUMBER :implied) ; position in tabbing order
		 (accesskey %Character :implied) ; accessibility key character
		 (onfocus %Script :implied) ; the element got the focus
		 (onblur %Script :implied) ; the element lost the focus
		 ))

;;; ***Tables

;;; IETF HTML table standard, see [RFC1942]
;;;
;;; The BORDER attribute sets the thickness of the frame around the
;;; table. The default units are screen pixels.
;;;
;;; The FRAME attribute specifies which parts of the frame around
;;; the table should be rendered. The values are not the same as
;;; CALS to avoid a name clash with the VALIGN attribute.
;;;
;;; The value "border" is included for backwards compatibility with
;;; <TABLE BORDER> which yields frame=border and border=implied
;;; For <TABLE BORDER=1> you get border=1 and frame=implied. In this
;;; case, it is appropriate to treat this as frame=border for backwards
;;; compatibility with deployed browsers.
;;;
;;; The RULES attribute defines which rules to draw between cells:
;;;
;;; If RULES is absent then assume:
;;;     "none" if BORDER is absent or BORDER=0 otherwise "all"

(define-html-element TABLE
    :content (:sequence (? CAPTION) (:or (* COL) (* COLGROUP)) (? THEAD) (? TFOOT) (+ TBODY))
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (summary %Text :implied) ; purpose/structure for speech outpu
		 (width %Length :implied) ; table width
		 (border %Pixels :implied) ; controls frame width around table
		 (frame %TFrame :implied) ; which parts of frame to render
		 (rules %TRules :implied) ; rulings between rows and cols
		 (cellspacing %Length :implied) ; spacing between cells
		 (cellpadding %Length :implied) ; spacing within cells
		 (align %TAlign :implied) ; table position relative to window
		 (bgcolor %Color :implied) ; background color for cells
		 ))

(define-html-element CAPTION		; table caption
    :content (* %inline)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (align %CAlign :implied) ; relative to table
		 ))

;;;    Use THEAD to duplicate headers when breaking table
;;;    across page boundaries, or for static headers when
;;;    TBODY sections are rendered in scrolling panel.

(define-html-element THEAD		; table header
    :end-optional t
    :content(+ TR)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 %cellhalign		; horizontal alignment in cells
		 %cellvalign		; vertical alignment in cells
		 ))

;;;    Use TFOOT to duplicate footers when breaking table
;;;    across page boundaries, or for static footers when
;;;    TBODY sections are rendered in scrolling panel.

(define-html-element TFOOT		; table footer
    :end-optional t
    :content (+ TR)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 %cellhalign		; horizontal alignment in cells
		 %cellvalign		; vertical alignment in cells
		 ))

;;;    Use multiple TBODY sections when rules are needed
;;;    between groups of table rows.

(define-html-element TBODY		; table body
    :start-optional t
    :end-optional t
    :content (+ TR)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 %cellhalign		; horizontal alignment in cells
		 %cellvalign		; vertical alignment in cells
		 ))

;;;COLGROUP groups a set of COL elements. It allows you to group
;;;several semantically related columns together.

(define-html-element COLGROUP		; table column group
    :end-optional t
    :content (* col)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (span NUMBER 1)	; default number of columns in group
		 (width %MultiLength :implied) ; default width for enclosed COLs
		 %cellhalign		; horizontal alignment in cells
		 %cellvalign		; vertical alignment in cells
		 ))

;;; COL elements define the alignment properties for cells in
;;; one or more columns.
;;;
;;; The WIDTH attribute specifies the width of the columns, e.g.
;;;
;;;     width=64        width in screen pixels
;;;     width=0.5*      relative width of 0.5
;;;
;;; The SPAN attribute causes the attributes of one
;;; COL element to apply to more than one column.

(define-html-element COL		; table column
    :end-optional t
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (span NUMBER 1)	; COL attributes affect N columns
		 (width %MultiLength :implied) ; column width specification
		 %cellhalign		; horizontal alignment in cells
		 %cellvalign		; vertical alignment in cells
		 ))

(define-html-element TR			; table row
    :end-optional t
    :content (+ (:or TH TD))
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 %cellhalign		; horizontal alignment in cells
		 %cellvalign		; vertical alignment in cells
		 (bgcolor %Color :implied) ; background color for row
		 ))

(define-html-element (:or TH TD)	; table header cell, table data cell
    :end-optional t
    :content (* %flow)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 (abbr %Text :implied)	; abbreviation for header cell
		 (axis CDATA :implied)	; names groups of related header
		 (headers IDREFS :implied) ; list of id's for header cells
		 (scope %Scope :implied) ; scope covered by header cells
		 (rowspan NUMBER 1)	; number of rows spanned by cell
		 (colspan NUMBER 1)	; number of cols spanned by cell
		 %cellhalign		; horizontal alignment in cells
		 %cellvalign		; vertical alignment in cells
		 (nowrap (nowrap) :implied) ; suppress word wrap
		 (bgcolor %Color :implied) ; cell background color
		 (width %Pixels :implied) ; width for cell
		 (height %Pixels :implied) ; height for cell
		 ))

;;; ***Document Frames

;;;  The content model for HTML documents depends on whether the HEAD is
;;;  followed by a FRAMESET or BODY element. The widespread omission of
;;;  the BODY start tag makes it impractical to define the content model
;;;  without the use of a marked section.

(define-html-element IFRAME		; inline subwindow
    :content (* %flow)
    :attributes (%coreattrs		; id, class, style, title
		 (longdesc %URI :implied) ; link to long description (complements title)
		 (name CDATA :implied)	; name of frame for targetting
		 (src %URI :implied)	; source of frame content
		 (frameborder (:or 1 0) 1) ; request frame borders?
		 (marginwidth %Pixels :implied) ; margin widths in pixels
		 (marginheight %Pixels :implied) ; margin height in pixels
		 (scrolling (:or yes no auto) auto) ; scrollbar or none
		 (align %IAlign :implied) ; vertical or horizontal alignment
		 (height %Length :implied) ; frame height
		 (width %Length :implied) ; frame width
		 ))

(define-html-element NOFRAMES		; alternate content container for non frame-based rendering
    :content %noframes.content
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 ))

;;; ***Document Head

(define-html-element HEAD		; document head
    :start-optional t
    :end-optional t
    :content %head.content
    :inclusions %head.misc
    :attributes (%i18n			; lang, dir
		 (profile %URI :implied) ; named dictionary of meta info
		 ))

;;; The TITLE element is not considered part of the flow of text.
;;; It should be displayed, for example as the page header or
;;; window title. Exactly one title is required per document.

(define-html-element TITLE		; document title
    :content PCDATA
    :exclusions %head.misc
    :attributes (%i18n))

(define-html-element ISINDEX		; single line prompt
    :end-optional t
    :attributes (%coreattrs		; id, class, style, title
		 %i18n			; lang, dir
		 (prompt %Text :implied) ; prompt message
		 ))

(define-html-element BASE		; document base URI
    :end-optional t
    :attributes ((href %URI :implied)	; URI that acts as base URI
		 (target %FrameTarget :implied) ; render in this frame
		 ))

(define-html-element META		; generic metainformation
    :end-optional t
    :attributes (%i18n			; lang, dir, for use with content
		 (http-equiv NAME :implied) ; HTTP response header name
		 (name NAME :implied)	; metainformation name
		 (content CDATA :required) ; associated information
		 (scheme CDATA :implied) ; select form of content
		 ))

(define-html-element STYLE		; style info
    :content %StyleSheet
    :attributes (%i18n			; lang, dir, for use with content
		 (type %ContentType :required) ; content type of style language
		 (media %MediaDesc :implied) ; designed for use with these media
		 (title %Text :implied) ; advisory title
		 ))

(define-html-element SCRIPT		; script statements
    :content %Script
    :attributes ((charset %Charset :implied) ; char encoding of linked resource
		 (type %ContentType :required) ; content type of script language
		 (language CDATA :implied) ; predefined script language name
		 (src %URI :implied)	; URI for an external script
		 (defer (defer) :implied) ; UA may defer execution of script
		 ))

(define-html-element NOSCRIPT		; alternate content container for non script-based rendering
    :content (* %flow)
    :attributes (%attrs			; %coreattrs, %i18n, %events
		 ))

;;; ***Document Structure

(define-html-element HTML		; document root element
    :start-optional t
    :end-optional t
    :content %html.content
    :attributes (%i18n			; lang, dir
		 ))

;;;----------------------------------------
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
