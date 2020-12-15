;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: netscape-2; Base: 10 -*-


(in-package :netscape-2)

;; (define-html-version netscape-2 html-2)

;;; extensions

(define-html-clause isindex (html-2:isindex netscape-2) (t)
  (prompt :type string)
  )

(define-html-clause horisontal-ruler (html-2:horisontal-ruler netscape-2) ("HR")
  (size  :type number)
  (width :type number)
  (align :type (member left right center))
  (noshade :type boolean)
  )

(define-html-clause unordered-list (html-2:unordered-list netscape-2) ("UL")
  (type :type (member disc circle square))
  )

(define-html ordered-list (html-2:ordered-list netscape-2) ("OL")
  (type  :type (member \A \a \I \i 1))
  (start :type number)
  )

(define-html list-item (html-2:list-item netscape-2) ("LI")
  (type  :type (member disc circle square \A \a \I \i 1))
  (value :type number)
  )

(define-html-clause image (html-2:image netscape-2) ("IMG" :required-attributes (url))
  (align :type (member top middle bottom left right texttop absmiddle baseline absbottom))
  (width  :type number)
  (height :type number)
  (border :type number)
  (vspace :type number)
  (hspace :type number)
  )

(define-html-clause line-break (html-2:line-break netscape-2) ("BR")
  (clear :type (member left right all))
  )

;;; new tags

(define-html no-break (netscape-2) ("NOBR"))

(define-html word-break (netscape-2) ("WBR"))

(define-html font (netscape-2) (t)
  (size :type number)
  )

(define-html basefont (netscape-2) (t)
  (size :type number)
  )

(define-html center (netscape-2) (t))

;;; 3.0 tags, not fully implemented

(define-html body-content (html-2:body-content netscape-2)
  (nil :content (table)))

(define-html blockquote (html-2:blockquote body-content) (t :fresh-line-p t))
(define-html body       (html-2:body       body-content) (t :fresh-line-p t))

;;;

(define-html html (html-2:html netscape-2)
  (t :content (frameset) :fresh-line-p t))

(define-html head (html-2:head netscape-2)
  (t :content (script) :fresh-line-p t))

(define-html script (netscape-2) (t :fresh-line-p 2)
  (language :type string)
  (src      :type url)
  )

(define-html frameset (netscape-2) (t :content (frameset frame) :fresh-line-p 2)
  (rows :type string)
  (cols :type string)
  )

(define-html-clause frame (netscape-2) (t :fresh-line-p 1)
  (src  :type url)
  (name :type string)
  (scrolling :type boolean)
  (marginwidth  :type number)
  (marginheight :type number)
  )

;;;

(define-html-clause input (html-2:input netscape-2) (t)
  (focus-event  :type string :code "onFocus")
  (blur-event   :type string :code "onBlur")
  (change-event :type string :code "onChange")
  )

(define-html table (netscape-2) (t :content (table-row caption) :fresh-line-p t)
  (border       :type (or boolean number))
  (cell-spacing :type number :code "CELLSPACING")
  (cell-padding :type number :code "CELLPADDING")
  (width        :type number)
  )

(define-html table-row (netscape-2) ("TR" :content table-cell :fresh-line-p t)
  (align          :type (member left center right))
  (vertical-align :type (member top middle bottom baseline) :code "VALIGN")
  )

(define-html table-cell (netscape-2) (nil :content html-2:body-content)
  (column-span :type number :code "COLSPAN")
  (row-span    :type number :code "ROWSPAN")
  (width       :type number)
  (align          :type (member left center right))
  (vertical-align :type (member top middle bottom baseline) :code "VALIGN")
  (no-wrap? :type boolean :code "NOWRAP")
  )

(define-html table-data   (table-cell) ("TD"))
(define-html table-header (table-cell) ("TH"))

(define-html caption (netscape-2) (t)
  (align :type (member top bottom))
  )

