;;; -*- Syntax: ansi-common-lisp; Base: 10; Mode: lisp; Package: html-parser -*-

;;; File: html-2-0.lisp
;;; Last edited by smishra on Thu Oct 22 17:20:20 1998

;;; (c) Copyright 1996-1998, Sunil Mishra (smishra@cc.gatech.edu)
;;;     All Rights Reserved

(in-package :html-parser)

;;; This file contains a transcription of the HTML 2.0 DTD, STRICT.
;;; I have chosen this simply because of convenience.

;;; All entities beginning with a % are special SGML declarations
;;; All fixed attributes have been ignored.

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

(define-html-entity %content-type
    cdata)

(define-html-entity %http-method
    (:or get post))

(define-html-entity %heading
    (:or h1 h2 h3 h4 h5 h6))

(define-html-entity %list
    (:or ul ol dir menu))

(define-html-entity %font
    (:or tt b i))

(define-html-entity %phrase
    (:or em strong code samp kbd var cite))

(define-html-entity %text
    (:or pcdata a img br %phrase %font))

(define-html-entity %pre.content
    (:or pcdata a hr br %font %phrase))

(define-html-entity %text
    (:or pcdata a img br))

(define-html-entity %linktype
    names)

(define-html-entity %linkExtraAttributes
    ((rel %linktype :implied)
     (rev %linktype :implied)
     (urn cdata :implied)
     (title cdata :implied)
     (methods names :implied)))

(define-html-entity %a.content
    (* %text))

(define-html-entity %block.forms
    (:or blockquote form isindex))

(define-html-entity %preformatted
    pre)

(define-html-entity %block
    (:or p %list dl %preformatted %block.forms))

(define-html-entity %flow
    (* (:or %text %block)))

(define-html-entity %body.content
    (:or %heading %block hr address img))

(define-html-entity %inputType
    (:or text password checkbox radio submit reset image hidden))

(define-html-entity %head.extra
    nil)

(define-html-entity %head.content
    (:and title (? isindex) (? base) %head.extra))

(define-html-entity %html.content
    (:sequence head body))

;;;----------------------------------------
;;; Elements

(define-html-element (:or %font %phrase)
    :content (* %text))

(define-html-element br
    :end-optional t)

(define-html-element a
    :content %a.content
    :exclusions a
    :attributes ((href cdata :implied)
		 (name cdata :implied)
		 %linkExtraAttributes))

(define-html-element img
    :end-optional t
    :attributes ((src cdata :required)
		 (alt cdata :implied)
		 (align (:or top middle bottom) :implied)
		 (ismap (ismap) :implied)))

(define-html-element p
    :end-optional t
    :content (* %text))

(define-html-element hr
    :end-optional t)

(define-html-element %heading
    :content (* %text))

(define-html-element pre
    :content (* %pre.content)
    :attributes ((width number :implied)))

(define-html-element dl
    :content (+ (:or dt dd))
    :attributes ((compact (compact) :implied)))

(define-html-element dt
    :end-optional t
    :content (* %text))

(define-html-element dd
    :end-optional t
    :content %flow)

(define-html-element (:or ol ul)
    :content (+ li)
    :attributes ((compact (compact) :implied)))

(define-html-element (:or dir menu)
    :content (+ li)
    :exclusions %block
    :attributes ((compact (compact) :implied)))

(define-html-element li
    :end-optional t
    :content %flow)

(define-html-element body
    :start-optional t
    :end-optional t
    :content %body.content)

(define-html-element blockquote
    :content %body.content)

(define-html-element address
    :content (* (:or %text p)))

(define-html-element form
    :content %body.content
    :exclusions form
    :inclusions (:or input select textarea)
    :attributes ((action cdata :implied)
		 (method %http-method get)
		 (enctype %content-type "application/x-www-form-urlencoded")))

(define-html-element input
    :end-optional t
    :attributes ((type %inputType text)
		 (name cdata :implied)
		 (value cdata :implied)
		 (src cdata :implied)
		 (checked (checked) :implied)
		 (size cdata :implied)
		 (maxlength number :implied)
		 (align (:or top middle bottom) :implied)))

(define-html-element select
    :content (+ option)
    :exclusions (:or input select textarea)
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
    :exclusions (:or input select textarea)
    :attributes ((name cdata :required)
		 (rows number :required)
		 (cols number :required)))

(define-html-element head
    :start-optional t
    :end-optional t
    :content %head.content
    :inclusions (:or meta link))

(define-html-element title
    :content (* pcdata)
    :exclusions (:or meta link))

(define-html-element link
    :end-optional t
    :attributes ((href cdata :required)
		 %linkExtraAttributes))

(define-html-element isindex
    :end-optional t)

(define-html-element base
    :end-optional t
    :attributes ((href cdata :required)))

(define-html-element meta
    :end-optional t
    :attributes ((http-equiv name :implied)
		 (name name :implied)
		 (content cdata :required)))

(define-html-element html
    :start-optional t
    :end-optional t
    :content %html.content)
