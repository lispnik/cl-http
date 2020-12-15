;;; -*- Syntax: Ansi-Common-Lisp; Package: CL-USER; Base: 10; Mode: lisp -*-

;;; File: html-3-2.lisp
;;; Last edited by smishra on Tue Jul 28 15:58:10 1998

;;; (c) Copyright 1996-97, Sunil Mishra (smishra@cc.gatech.edu)
;;;     All Rights Reserved

(in-package :html-parser)

;;; This file contains a transcription of the HTML 3.2 DTD
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

;;;----------------------------------------
;;; Entities

(define-html-entity %html.content
    (:sequence head
	       body))

(define-html-entity %head.content
    (:and title
	  (? isindex)
	  (? base)))

(define-html-entity %head.misc
    (:or script
	 style
	 meta
	 link))

(define-html-entity %body.content
    (:set (* (:or %heading
		  %text
		  %block
		  address))))

(define-html-entity %color
    cdata)

(define-html-entity %body-color-attrs
    ((bgcolor %color :implied)
     (text %color :implied)
     (link %color :implied)
     (vlink %color :implied)
     (alink %color :implied)))

(define-html-entity %address.content
    (* (:or %text p)))

(define-html-entity %content-type
    cdata)

(define-html-entity %http-method
    (:or get post))

(define-html-entity %url
    cdata)

(define-html-entity %heading
    (:or h1 h2 h3 h4 h5 h6))

(define-html-entity %list
    (:or ul ol dir menu))

(define-html-entity %preformatted
    pre)

(define-html-entity %font
    (:or tt i b u strike big small sub sup))

(define-html-entity %phrase
    (:or em strong dfn code samp kbd var cite))

(define-html-entity %special
    (:or a img applet font br script map))

(define-html-entity %form
    (:or input select textarea))

(define-html-entity %text
    (:or pcdata %font %phrase %special %form))

(define-html-entity %block
    (:or p %list %preformatted dl div center blockquote form
	 isindex hr table))

(define-html-entity %flow
    (* (:or %text %block)))

(define-html-entity %shape
    (:or rect circle poly default))

(define-html-entity %coords
    cdata)

(define-html-entity %types
    cdata)

(define-html-entity %length
    cdata)

(define-html-entity %pixels
    cdata)

(define-html-entity %ialign
    (:or top middle bottom left right))

(define-html-entity %pre.exclusion
    (:or img big small sub sup font))

(define-html-entity %olstyle
    cdata)

(define-html-entity %ulstyle
    (:or disc square circle))

(define-html-entity %listyle
    cdata)

(define-html-entity %inputType
    (:or text password checkbox radio submit reset file hidden image))

(define-html-entity %where
    (:or left center right))

(define-html-entity %cell.halign
    (align (:or left center right) :implied))

(define-html-entity %cell.valign
    (valign (:or top middle bottom baseline) :implied))

;;;----------------------------------------
;;; Elements

;;; I have left out the attributes %version.attr because it cannot be
;;; set by the user; it is of no consequence to parsing.

(define-html-element html
    :start-optional t
    :end-optional t
    :content %html.content)

(define-html-element head
    :start-optional t
    :end-optional t
    :content %head.content
    :inclusions %head.misc)

(define-html-element title
    :content (* pcdata)
    :exclusions %head.misc)

(define-html-element isindex
    :end-optional t
    :attributes ((prompt cdata :implied)))

(define-html-element base
    :end-optional t
    :attributes ((href %url :required)))

(define-html-element meta
    :end-optional t
    :attributes ((http-equiv name :implied)
		 (name name :implied)
		 (content cdata :required)))

(define-html-element style
    :content (* pcdata)
    :exclusions %head.misc)

(define-html-element script
    :content (* pcdata)
    :exclusions %head.misc)

(define-html-element body
    :start-optional t
    :end-optional t
    :content %body.content
    :attributes ((background %url :implied)
		 %body-color-attrs))

(define-html-element address
    :content %address.content)

(define-html-element div
    :content %body.content
    :attributes ((align (:or left center right) left)))

(define-html-element center
    :content %body.content)

(define-html-element (:or %font %phrase)
    :content (* %text))

(define-html-element font
    :content (* %text)
    :attributes ((size cdata :implied)
		 (color cdata :implied)))

(define-html-element br
    :end-optional t
    :attributes ((clear (:or left all right none) none)))

(define-html-element a
    :content (* %text)
    :exclusions a
    :attributes ((name cdata :implied)
		 (href %url :implied)
		 (rel cdata :implied)
		 (rev cdata :implied)
		 (title cdata :implied)))

(define-html-element map
    :content (* area)
    :attributes ((name cdata :implied)))

(define-html-element area
    :end-optional t
    :attributes ((shape %shape rect)
		 (coords %coords :implied)
		 (href %url :implied)
		 (nohref (nohref) :implied)
		 (alt cdata :required)))

(define-html-element link
    :end-optional t
    :attributes ((id id :implied)
		 (href %url :implied)
		 (rel %types :implied)
		 (rev %types :implied)
		 (title cdata :implied)))

(define-html-element img
    :end-optional t
    :attributes ((src %url :required)
		 (alt cdata :implied)
		 (align %ialign :implied)
		 (height %pixels :implied)
		 (width %pixels :implied)
		 (border %pixels :implied)
		 (hspace %pixels :implied)
		 (vspace %pixels :implied)
		 (usemap %url :implied)
		 (ismap (ismap) :implied)))

(define-html-element applet
    :content (:sequence (* param)
			textflow)
    :attributes ((codebase %url :implied)
		 (code cdata :required)
		 (name cdata :implied)
		 (alt cdata :implied)
		 (align %ialign :implied)
		 (height %pixels :required)
		 (width %pixels :required)
		 (hspace %pixels :implied)
		 (vspace %pixels :implied)))

(define-html-element param
    :end-optional t
    :attributes ((name name :required)
		 (value cdata :implied)))

(define-html-element textflow
    :start-optional t
    :end-optional t
    :content (* %text))

(define-html-element hr
    :end-optional t
    :attributes ((align (:or left right center) :implied)
		 (noshade (noshade) :implied)
		 (size %pixels :implied)
		 (width %length :implied)))

(define-html-element p
    :end-optional t
    :content (* %text)
    :attributes ((align (:or left right center) :implied)))

(define-html-element %heading
    :content (* %text)
    :attributes ((align (:or left right center) :implied)))

(define-html-element pre
    :content (* %text)
    :exclusions %pre.exclusion
    :attributes ((width number :implied)))

(define-html-element blockquote
    :content %body.content)

(define-html-element dl
    :content (* (:or dt dd))
    :attributes ((compact (compact) :implied)))

(define-html-element dt
    :end-optional t
    :content (* %text))

(define-html-element dd
    :end-optional t
    :content %flow)

(define-html-element ol
    :content (* li)
    :attributes ((type %olstyle :implied)
		 (start number :implied)
		 (compact (compact) :implied)))

(define-html-element ul
    :content (* li)
    :attributes ((type %ulstyle :implied)
		 (compact (compact) :implied)))

(define-html-element (:or dir menu)
    :content (* li)
    :exclusions %block
    :attributes ((compact (compact) :implied)))

(define-html-element li
    :end-optional t
    :content %flow
    :attributes ((type %listyle :implied)
		 (value number :implied)))

(define-html-element form
    :content %body.content
    :exclusions form
    :attributes ((action %url :required)
		 (method %http-method get)
		 (enctype %content-type
			  "application/x-www-form-urlencoded")))

(define-html-element input
    :end-optional t
    :attributes ((type %inputtype text)
		 (name cdata :implied)
		 (value cdata :implied)
		 (checked (ckecked) :implied)
		 (size cdata :implied)
		 (maxlength number :implied)
		 (src %url :implied)
		 (align (:or top middle bottom left right) top)))

(define-html-element select
    :content (+ option)
    :attributes ((name cdata :required)
		 (size number :implied)
		 (multiple (multiple) :implied)))

(define-html-element option
    :end-optional t
    :content (* pcdata)
    :attributes ((selected (selected) :implied)
		 (value cdata :implied)))

(define-html-element textarea
    :content (* pcdata)
    :attributes ((name cdata :required)
		 (rows number :required)
		 (cols number :required)))

(define-html-element table
    :content (:sequence (? caption)
			(+ tr))
    :attributes ((align %where :implied)
		 (width %length :implied)
		 (border %pixels :implied)
		 (dummy (border) :implied)
		 (cellspacing %pixels :implied)
		 (cellpadding %pixels :implied)))

(define-html-element caption
    :content (* %text)
    :attributes ((align (:or top bottom) :implied)))

(define-html-element tr
    :end-optional t
    :content (* (:or th td))
    :attributes (%cell.halign
		 %cell.valign))

(define-html-element (:or th td)
    :end-optional t
    :content %body.content
    :attributes ((nowrap (nowrap) :implied)
		 (rowspan number 1)
		 (colspan number 1)
		 %cell.halign
		 %cell.valign))

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
