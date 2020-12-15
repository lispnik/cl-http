;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: html-2; Base: 10 -*-
;;;;
;;;
;;; Copyright 1996, Hallvard Tr¾tteberg
;;; You can use the code if you like, but include this notice.
;;;
;;;;
;;; Last edited by smishra on Mon Jul  1 14:32:40 1996

;;; HTML 2.0 element description, based on DTD

(in-package :html-2)

;;;

(define-html html (html-2) (t :content (head body) :fresh-line-p t)
  (version :type number)
  )

(define-html head (html-2)
  (t :content (title isindex base meta nextid link) :fresh-line-p t))

(define-html body-content ()
  (nil :content (headerN paragraph itemize preformatted blockquote
                         form isindex horisontal-ruler address)))

(define-html body (body-content html-2) (t :fresh-line-p t))

(define-html text-content ()
  (nil :content (anchor image line-break character-markup))
  )

(define-html character-markup (text-content html-2) (nil))

(define-sub-html (character-markup)
  (emphasize "EM") cite strong code (sample "SAMP")
  (keyboard "KBD") (html-variable "VAR")
  (typed-text "TT") (bold "B") (italic "I")
  )

(define-html basic-link (html-2) ()
  (url :type url :code "HREF")
  )

(define-html hyperlink (basic-link) ()
  (rel   :type symbol)
  (rev   :type symbol)
  (urn   :type url)
  (title :type string)
  (methods :type (member get put post))
  )

(define-html anchor (text-content hyperlink) ("A")
  (name :type string)
  )

;;; head markup

(define-html title (html-2) (t :content nil :fresh-line-p t))

(define-html-clause link (hyperlink) (t :required-attributes (url)))

(define-html-clause base (basic-link) (t :required-attributes (url)))

(define-html-clause isindex (html-2) (t))

(define-html-clause nextid (html-2) (t :required-attributes (n))
  (n :type symbol)
  )

(define-html-clause meta (html-2) (t :required-attributes (content))
  (name :type symbol)
  (http-equivalent :type string :code "HTTP-EQUIV")
  (content :type string)
  )

;;; headers

(define-html headerN (text-content html-2) ())

(define-sub-html (headerN)
  (header1 "H1") (header2 "H2") (header3 "H3") (header4 "H4")
  (header5 "H5") (header6 "H6"))

;;;

(define-html paragraph (text-content html-2) ("P" :optional-end t :fresh-line-p t))

(define-html-clause line-break (html-2) ("BR" :fresh-line-p t))

(define-html-clause horisontal-ruler (html-2) ("HR"))

(define-html preformatted (html-2)
  ("PRE" :content (anchor horisontal-ruler line-break character-markup) :fresh-line-p t)
  (width :type integer)
  )

(define-html blockquote (body-content html-2) (t :fresh-line-p t))

(define-html address (text-content html-2) (t))

(define-html-clause image (html-2) ("IMG" :required-attributes (url))
  (url :type url :code "SRC")
  (alternative-string :type string :code "ALT")
  (align :type (member top middle bottom left right))
  (is-map? :type boolean :code "ISMAP")
  )

;;; lists

(define-html list-environment (html-2) (nil :fresh-line-p t))

(define-html itemize (list-environment) (nil :content (list-item)))

(define-html ordered-list (itemize) ("OL"))
(define-html directory-list (itemize) ("DIR"))
(define-html menu-list (itemize) ("MENU"))
(define-html unordered-list (itemize) ("UL")
  (compact? :type boolean)
  )

(define-html definition-list (list-environment) ("DL" :content (definition-term definition-data))
  (compact? :type boolean)
  )

(define-html basic-list-item (html-2) (nil :optional-end t :fresh-line-p t))

(define-html basic-list-item-content ()
  (nil :content (anchor image line-break character-markup paragraph
                        list-environment preformatted blockquote form isindex)))

(define-html list-item (basic-list-item basic-list-item-content) ("LI"))

(define-html definition-term (basic-list-item text-content)            ("DT"))
(define-html definition-data (basic-list-item basic-list-item-content) ("DD"))

;;; forms

(define-html form (html-2)
  (t :content (headerN paragraph horisontal-ruler list-environment
                       preformatted blockquote isindex address
                       form-dialog)
     :required-attributes (action) :fresh-line-p t)
  (action :type url)
  (method :type (member get put post))
  (enctype :type symbol)
  )

(define-html form-dialog (html-2) ()
  (name :type symbol)
  )

(define-html-clause input (form-dialog) (t)
  (type  :type (member button text password checkbox radio submit reset image hidden))
  (value :type t)
  (src   :type url)
  (checked? :type boolean :code "CHECKED")
  (size  :type number)
  )

(define-html select (form-dialog) (t :content (option) :required-attributes (name))
  (size :type number)
  (multiple? :type boolean)
  )

(define-html option (html-2) (t  :optional-end t)
  (selected? :type boolean)
  (value :type string)
  )

(define-html textarea (form-dialog) (t :required-attributes (name rows cols))
  (rows    :type integer)
  (columns :type integer :code "COLS")
  )

