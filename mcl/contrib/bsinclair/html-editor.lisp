;;;-*- Mode: Lisp; Package: (HTML-EDITOR) -*-

;;	Change History (most recent first):
;;  8 10/20/98 dbl  Added minimal support for HTML 3.2 styles and tables.
;;                  (You still have to insert table attributes manually.)
;;                  Added <!doctype> for HTML 2.0 and 3.2.  Added <dir> and
;;                  <menu>.
;;  7 10/18/98 dbl  Runtime update of HTML menu to accomodate on-the-fly
;;                  change of system font possible in Mac OS 8.5; without
;;                  this, the pseudo right-justification of the key binding
;;                  info gets badly messed up.
;;  6 9/15/96  dbl  Merged with Bill's and Shannon's most recent changes.
;;  5 1/21/96  dbl  Fix timestamp - was inserting wrong date.
;;  4 1/8/96   dbl  Fix compile-time problem.
;;  3 1/8/96   dbl  Two or three key sequences (starting with c-x) defined
;;                  in the html-command structure are processed by the
;;                  define-html-fred-commands function, creating additional
;;                  comtabs as needed.  Timestamp no longer brackets
;;                  selected text.
;;  2 1/7/96   dbl  Bind commands to just about everything.  (This isn't
;;                  done as nicely as I'd like -- should be integrated with
;;                  Bill's define-html-fred-commands instead of building the
;;                  chained comtabs by hand.)  Added timestamp compatible
;;                  with Emacs' html-mode.
;;  (do not edit before this line!!)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; HTML-Editor.lisp
;;;
;;; Some Fred commands and a menu for creating HTML documents
;;;
;;; © Copyright 1995 Bill St. Clair, All Rights Reserved
;;; You are encouraged to use this code, but give me credit, and I make no promises
;;; to you or your lawyer.
;;;
;;; Direct questions and bug reports to bill@cambridge.apple.com
;;;

#|

These HTML tags came from "http://www.ncsa.uiuc.edu/demoweb/html-primer.html"
If you extend this package, please send me the changes.
I'll add more commands as I use them.

Puts an "HTML" menu on your menubar.
All the commands surround the selection with the repective HTML Markup tag.
If there is some text inside parens at the end of the menu item title, that
is the Fred command: "m-" = meta (option), "s-" = shift, <arg> = prefix numeric argument
Markup tags are inserted in red if your Fred supports colored text.

The "Conversion" commands are for switching between Unix and Mac newline conventions.
Browsers work on both types of files, but editors often look very funny if the
newlines aren't as expected. They convert the entire front window, ignoring the
selection. They currently tickle a bug in MCL 3.0's redisplay command, so don't
use "Return -> Linefeed" in MCL 3.0 until the bug is fixed.

"Insert Paragraph Markers" inserts a "<p>" marker in every double newline in
the selection, unless there is already one there. It can be used to start
the conversion of a text document to HTML.

The "Black Text" command sets the color of the selection to black.
It's necessary in MCL 3.0, because the insertion font changes to red
when you click after red text.

The "Red Text" command changes the color of the selection to red.

The "Highlight Tags" command changes all the markup tags in the selection to red.

The "Black Text" and "Hightlight Tags" commands are only available in
an MCL that supports colored text (e.g. MCL 3.0).

A good way to use this code is to open a NetScape window on the text
you're editing with MCL, and use my "processes.lisp" contribution
to make "c-sh-N" go to NetScape and "c-m-cmd" come back to MCL.
When you get to NetScape, use its "Reload" command (cmd-R) to
view your changes.

I have tested this code in MCL 2.0.1 and MCL 3.0b1.

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modification History
;;;
;;
;; 08/06/96 bill  buffer-set-font-codes -> ed-set-view-font in some important places.
;;                This makes m-k and m-sh-r work and makes text black after m-p.
;; 04/03/96 bill  #.#$ShiftKey instead of $ShiftKey-value
;; 10/24/95 bill  Shannon V. Spires (svs) addition of the <center> tag.
;; 05/25/95 bill  The "Title" command now puts in <html><head><title>... stuff,
;;                the necessities for a new HTML document.
;;                Add comment tag (m-sh-!).
;; 05/23/95 bill  Select all in anchor dialog URL box to ease pasting.
;;                "HTML" package -> "HTML Mode" to avoid conflict with
;;                John Mallery's Mac HTTP system.
;; 05/20/95 bill  $ShiftKey-value constant because #$ShiftKey doesn't work
;;                compiled inside an advise.
;; 05/14/95 bill  Add "Image URL" to anchor dialog for clickable images.
;;                Enter HTML mode on file open as well as first char typed.
;; 05/13/95 bill  "Red Text" command.
;;                "Remove enclosing tag pair" selects the text inside the tag pair.
;;                  This makes it easy to replace a tag pair with a different one
;;                  (e.g. change the heading number or style).
;;                "Insert paragraph markers".
;;                Choose file dialogs for anchor & image commands.
;;                *auto-choose-files* & *gif-image-files* user preferences parameters.
;; 05/11/95 bill  Put a black space after the suffix to keep the red from bleeding.
;;                For the same reason, always use black for text color. Don't try
;;                  to copy what's there.
;;                "Black Text" becomes m-k instead of m-sh-K.
;;                Many movement and deletion commands (See the "Movement sub-menu").
;;                LF<->CR conversion (see the "Conversion" sub-menu) (CR->LF
;;                  currently tickles a bug in MCL 3.0's display code).
;;                *html-comtab* and toggle-html-mode command to install it.
;;                  Auto-intallation for ".html" files.
;; 05/10/95 bill  heading-prefix defaults to the last heading value, initially 1.
;;                Motivated by Carl L. Gay's bug report.
;; 05/08/95 bill  New file
;;

(defpackage "HTML-EDITOR"
  (:export "*AUTO-CHOOSE-FILES*"        ; set true to automatically choose 
                                        ; a file for Anchor & Image commands
           "*GIF-IMAGE-FILES*"          ; Set true to choose only GIF files for image command
           ))

(in-package "HTML-EDITOR")

(provide "HTML-EDITOR")

; This package is for showing on the mode line
(defpackage "HTML Mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; User preferences parameters
;;;

(defvar *auto-choose-files* nil
  "True to automatically bring up a file chooser for the Anchor and Image commands")

(defvar *gif-image-files* nil
  "True to limit the files selected by the image file chooser to GIF files")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Timestamp markers
;;; These are the same as used by Emacs' html-mode

(eval-when (:compile-toplevel :execute :load-toplevel)
(defconstant *timestamp-begin-marker* "<!-- hhmts start -->")
(defconstant *timestamp-end-marker* "<!-- hhmts end -->")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Balloon help strings
;;; Should be one for most of the entries in the *html-commands* list below
;;;

(defparameter *html-help-strings*
  (flet ((fn (x) 
           (and x (format nil x))))
    (mapcar #'(lambda (x) 
                (list (first x) (fn (second x))))
            `((:paragraph "Insert a paragraph break.~%HTML fills until it sees one.")
              (:heading "Insert a heading.~%The Fred command takes a numeric arg~%~
                         which defaults to the last heading number inserted.")
              (:break "Insert a line break.~%Like a paragraph break, but no blank space lines")
              (:rule "Insert a horizontal rule.")
              (:comment "Surround the selection with a comment tag")
              ;---
              (:anchor "Bring up a dialog to specify a Hypertext REFerence (HREF).")
              (:image "Bring up a dialog to specify an image file.")
              (:name "Bring up a dialog to specify a section name.")
              ;---
              (:styles "Choose a text style")
              (:dfn-style "For a word being defined. Typically displayed in italics.")
              (:em-style "For emphasis. Typically displayed in italics.")
              (:cite-style "For titles of books, films, etc. Typically displayed in italics.")
              (:code-style "For snippets of computer code. Displayed in a fixed-width font.")
              (:center-style "To center text on the page.") ; svs
              (:kbd-style "For user keyboard entry. Should be displayed in a bold fixed-width ~
                           font, but many browsers render it in the plain fixed-width font.")
              (:samp-style "For computer status messages. Displayed in a fixed-width font.")
              (:strong-style "For strong emphasis. Typically displayed in bold.")
              (:var-style "For a ``metasyntactic'' variable, where the user is to replace~
                           the variable with a specific instance. Typically displayed in italics.")
              (:b-style "Bold text.")
              (:i-style "Italic text.")
              (:tt-style "Typewriter text, e.g. fixed-width font.")
              (:strike-style "Strikethrough text.")
              (:big-style "Larger font size.")
              (:small-style "Smaller font size.")
              (:subscript-style "Subscript text.")
              (:superscript-style "Superscript text.")
              ;---
              (:movement "Commands for moving the cursor or deleting text")
              (:select-enclosing-tag-pair "Moves out to or over a tag pair.~%~
                                           Put the cursor inside some nested tags and ~
                                           type c-m-return a few times.")
              (:delete-enclosing-tag-pair "Removes the tag pair enclosing the selection, ~
                                           but not its contents.~%~
                                           Selects its contents.~%~
                                           Useful to change a tag pair to another one, ~
                                           e.g. to change a heading number.")
              (:fwd-tag "Move forward to after a tag.")
              (:bwd-tag "Move backward to before a tag.")
              (:fwd-over-tag-pair "Move forward over a tag pair.")
              (:bwd-over-tag-pair "Move backward over a tag pair.")
              (:delete-fwd-tag "Delete between the cursor and the end of the next tag.")
              (:delete-bwd-tag "Delete between the cursor and the beginning of the previous tag.")
              (:delete-fwd-over-tag-pair "Delete between the cursor and the ~
                                          end of the next tag pair.")
              (:delete-bwd-over-tag-pair "Delete between the cursor and the ~
                                          beginning of the previous tag pair.")
              ;---
              (:conversion "Commands for converting files to the format created by this editor.")
              (:insert-paragraph-markers "Change double newlines to paragraph ~
                                          markers in the selected region.")
              (:lf->cr "Change Unix style linefeed characters to Mac style return characters.~%~
                        Ignores the selection.")
              (:cr->lf "Change Mac style return characters to Unix style linefeed characters.~%~
                        Ignores the selection.")
              (:NetScape-it "Make NetScape be the creator of the file. ~
                             Convenient if you want NetScape ~
                             to open it when you double click on it in the Finder.")
              (:table "Commands for building tables.")
              (:table-body "This wraps a table definition.")
              (:table-header "This gives the table a title.")
              (:table-row "Define a row in the table.")
              (:table-data "Define a column in the current table row.")
              (:table-caption "This gives the table a caption.")
              ;---
              (:doctype "Document type definitions.")
              (:doctype-20 "Insert a document type definition for HTML 2.0.~%This should always be the first line of the file.")
              (:doctype-32 "Insert a document type definition for HTML 3.2.~%This should always be the first line of the file.")
              (:title "Insert the document skeleton and a title spec.~%This should follow the document type definition.")
              ;---
              (:preformatted "Enclose a block of preformatted text.")
              (:block-quote "Enclose a quotation. Usually indented.")
              (:address "For the address line at the bottom of the page. Usually formatted as “name / email”")
              (:time-stamp "Insert a timestamp to be automatically updated each time the file is saved.")
              ;---
              (:unnumbered-list "Enclose an unnumbered list. Lists may be nested.")
              (:numbered-list "Enclose a numbered list. Lists may be nested.")
              (:other-list "Additional list types.")
              (:directory-list "Enclose a compact list of one-line entries.")
              (:menu-list "Enclose a list of choices.")
              (:list-entry "Prefix for a list entry.")
              ;---
              (:definition-list "Enclose a definition list. ~
                                 Contents should be alternating terms and definitions.")
              (:term "A term to be defined.")
              (:definition "The definition of the term.")
              ;---
              (:black-text "Change the color of the selection to black.")
              (:red-text "Change the color of the selection to red.")
              (:highlight-tags "Change the color of the tags in the selection to red.")
              ;---
              (:toggle-html-mode "Toggle HTML mode (install or deinstall the HTML Fred commands).")
              ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code
;;;

(defstruct (html-command (:constructor
                            make-html-command
                            (name prefix suffix fred-keystroke menu-title)))
  name
  prefix
  suffix
  fred-keystroke
  menu-title)

(defvar *colored-text-p*
  (member :color-index (font-spec 0 0)          ; don't #. the font-spec call; it's an environment query
          :key #'(lambda (x) (and (consp x) (car x)))))

(defparameter *command-font-spec*
  (if *colored-text-p*
    `((:color ,*red-color*))))

(defparameter *text-font-spec*
  (if *colored-text-p*
    '((:color-index 0))))               ; black

; To install a change to this command list, evaluate this form,
; then pull down a menu. It will auto-install the changes in both
; the menu and the variables (*HTML-comtab*, *HTML-control-x-comtab*,
; and *second-comtabs*).
(defparameter *html-commands*
  (flet ((anl (string) (format nil "~a~%" string))      ; Append New Line
         (pnl (string) (format nil "~%~A" string)))     ; Prepend New Line
    (mapcar #'(lambda (x)
                (destructuring-bind (&optional name prefix suffix keystroke menu-title) x
                  (make-html-command name prefix suffix keystroke menu-title)))
            `((:paragraph "<p>" "" (:meta #\p) 
               "Paragraph <p> (m-p)")
              (:heading heading-prefix heading-suffix (:meta #\h)
                        ("Heading <hN> (<arg>m-h)"
                         ("1" :heading-1)
                         ("2" :heading-2)
                         ("3" :heading-3)
                         ("4" :heading-4)
                         ("5" :heading-5)
                         ("6" :heading-6)))
              (:heading-1 heading-1-prefix "</h1>")
              (:heading-2 heading-2-prefix "</h2>")
              (:heading-3 heading-3-prefix "</h3>")
              (:heading-4 heading-4-prefix "</h4>")
              (:heading-5 heading-5-prefix "</h5>")
              (:heading-6 heading-6-prefix "</h6>")
              (:break "<br>" "" (:meta :shift #\P) "Line Break <br> (m-sh-P)")
              (:rule ,(anl "<hr>") "" (:control-x #\-) "Horizontal Rule <hr> (c-x -)")
              (:comment "<!-- " " -->" (:meta #\!) "Comment <!-- --> (m-sh-!)")
              (nil nil nil nil "-")
              (:anchor anchor-prefix "</a>" (:meta #\a) "Anchor <a …> (m-a)")
              (:image image-prefix "" (:meta :shift #\i) "Image <img …> (m-sh-i)")
              (:name name-prefix "</a>" (:meta #\n) "Name <a name=...> (m-n)")
              (nil nil nil nil "-")
              (:styles nil nil nil ("Styles"
                                    ("Definition <dfn> (c-x s d)" :dfn-style)
                                    ("Emphasis <em> (c-x s e)" :em-style)
                                    ("Citation <cite> (c-x s sh-C)" :cite-style)
                                    ("Code <code> (c-x s c)" :code-style)
                                    ("Center <center> (c-x s |)" :center-style) ; svs
                                    ("Keyboard <kbd> (c-x s k)" :kbd-style)
                                    ("Status Msg <samp> (c-x s sh-S)" :samp-style)
                                    ("Strong <strong> (c-x s s)" :strong-style)
                                    ("Variable <var> (c-x s v)" :var-style)
                                    ("-" nil)
                                    ("Bold <b> (c-x s b)" :b-style)
                                    ("Italic <i> (c-x s i)" :i-style)
                                    ("Typewriter <tt> (c-x s t)" :tt-style)
                                    ("Strike Through <strike>" :strike-style)
                                    ("Big <big>" :big-style)
                                    ("Small <small>" :small-style)
                                    ("Subscript <sub>" :subscript-style)
                                    ("Superscript <sup>" :superscript-style)))
              (:dfn-style "<dfn>" "</dfn>" (:control-x #\s #\d))
              (:em-style "<em>" "</em>" (:control-x #\s #\e))
              (:cite-style "<cite>" "</cite>" (:control-x #\s (:shift #\C)))
              (:code-style "<code>" "</code>" (:control-x #\s #\c))
              (:center-style "<center>" "</center>" (:control-x #\s #\|)) ; svs
              (:kbd-style "<kbd>" "</kbd>" (:control-x #\s #\k))
              (:samp-style "<samp>" "</samp>" (:control-x #\s (:shift #\S)))
              (:strong-style "<strong>" "</strong>" (:control-x #\s #\s))
              (:var-style "<var>" "</var>" (:control-x #\s #\v))
              (:b-style "<b>" "</b>" (:control-x #\s #\b))
              (:i-style "<i>" "</i>" (:control-x #\s #\i))
              (:tt-style "<tt>" "</tt>" (:control-x #\s #\t))
              (:strike-style "<strike>" "</strike>")
              (:big-style "<big>" "</big>")
              (:small-style "<small>" "</small>")
              (:subscript-style "<sub>" "</sub>")
              (:superscript-style "<sup>" "</sup")
              ;(nil nil nil nil "-")
              (nil nil nil nil ("Special Characters"
                                ("< (c-x c <)" :<-special)
                                ("> (c-x c >)" :>-special)
                                ("& (c-x c &)" :&-special)
                                ("\" (c-x c \")" :quot-special)
                                ("-" nil)
                                ("ö" :Ouml-special)
                                ("ñ" :ntilde-special)
                                ("è" :Egrave-special)))
              (:<-special "&lt;" "" (:control-x #\c #\<))
              (:>-special "&gt;" "" (:control-x #\c #\>))
              (:&-special "&amp;" "" (:control-x #\c #\&))
              (:quot-special "&quot;" "" (:control-x #\c #\"))
              (:Ouml-special "&Ouml;" "")
              (:ntilde-special "&ntilde;" "")
              (:Egrave-special "&Egrave;" "")
              (:movement nil nil nil ("Movement"
                                      ("Select out to tag pair (c-m-return)" :select-enclosing-tag-pair)
                                      ("Remove enclosing tag pair (m-sh-K)" :delete-enclosing-tag-pair)
                                      ("-" nil)
                                      ("Move Fwd tag (c-sh-F)" :fwd-tag)
                                      ("Move Bwd tag (c-sh-B)" :bwd-tag)
                                      ("Move Fwd Over Tag Pair (m-sh-F)" :fwd-over-tag-pair)
                                      ("Move Bwd Over Tag Pair (m-sh-B)" :bwd-over-tag-pair)
                                      ("-" nil)
                                      ("Delete Fwd over tag (c-sh-D)" :delete-fwd-tag)
                                      ("Delete Bwd over tag (c-sh-Delete)" :delete-bwd-tag)
                                      ("Delete Fwd Over Tag Pair (m-sh-D)" :delete-fwd-over-tag-pair)
                                      ("Delete Bwd Over Tag Pair (m-sh-Delete)" :delete-bwd-over-tag-pair)))
              (:select-enclosing-tag-pair ed-select-enclosing-tag-pair nil (:control :meta #\return))
              ; I wanted to use c-sh-space, but Fred won't shift the space bar
              (:delete-enclosing-tag-pair ed-delete-enclosing-tag-pair nil (:meta :shift #\K))
              (:fwd-tag ed-fwd-tag nil (:control :shift #\F))
              (:bwd-tag ed-bwd-tag nil (:control :shift #\B))
              (:fwd-over-tag-pair ed-fwd-over-tag-pair nil (:meta :shift #\F))
              (:bwd-over-tag-pair ed-bwd-over-tag-pair nil (:meta :shift #\B))
              (:delete-fwd-tag ed-delete-fwd-tag nil (:control :shift #\D))
              (:delete-bwd-tag ed-delete-bwd-tag nil (:control :shift #\delete))
              (:delete-fwd-over-tag-pair ed-delete-fwd-over-tag-pair nil (:meta :shift #\D))
              (:delete-bwd-over-tag-pair ed-delete-bwd-over-tag-pair nil (:meta :shift #\delete))
              (:conversion nil nil nil ("Conversion"
                                        ("Insert paragraph markers <p>" :insert-paragraph-markers)
                                        ("Linefeed (Unix) -> Return (Mac)" :lf->cr)
                                        ("Return (Mac) -> Linefeed (Unix)" :cr->lf)
                                        ("Change file creator to NetScape" :NetScape-it)))
              (:insert-paragraph-markers insert-paragraph-markers)
              (:lf->cr ed-lf->cr)
              (:cr->lf ed-cr->lf)
              (:NetScape-it NetScape-it)
              (:table nil nil nil ("Table"
                                   ("Table <table>" :table-body)
                                   ("Table header <th>" :table-header)
                                   ("Table row <tr>" :table-row)
                                   ("Table data <td>" :table-data)
                                   ("Table caption <caption>" :table-caption)))
              (:table-body ,(anl "<table>") ,(pnl "</table>") nil)
              (:table-header "<th>" "</th>" nil)
              (:table-row "<tr>" "</tr>" nil)
              (:table-data "<td>" "</td>" nil)
              (:table-caption "<caption>" "</caption>" nil)
              (nil nil nil nil "-")
              (:doctype nil nil nil ("Doctype"
                                     ("HTML 2.0" :doctype-20)
                                     ("HTML 3.2" :doctype-32)))
              (:doctype-20 ,(anl "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">") "")
              (:doctype-32 ,(anl "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">") "")
              (:title #.(format nil "<html>~%<head>~%<title>")
                      #.(format nil "</title>~%</head>~%<body>~%~%~%~%</body>~%</html>")
                      (:meta :shift #\T)
                      "Skeleton <html>…<title> (m-sh-T)")
              (nil nil nil nil "-")
              (:preformatted
               ,(anl "<pre>")
               ,(pnl "</pre>")
               (:control-x #\b #\p)
               "Preformatted Text <pre> (c-x b p)")
              (:block-quote
               ,(anl "<blockquote>")
               ,(pnl "</blockquote>")
               (:control-x #\b #\b)
               "Block Quote <blockquote> (c-x b b)")
              (:address
               ,(anl "<address>")
               ,(pnl "</address>")
               (:control-x #\b #\a)
               "Address <address> (c-x b a)")
              (:time-stamp
               #.(format nil "~%~A~2%~A~%" 
                         *timestamp-begin-marker* *timestamp-end-marker*)
               ""
               (:control-x #\b #\t)
               "Insert timestamp (c-x b t)")
              (nil nil nil nil "-")
              (:unnumbered-list
               ,(anl "<ul>")
               ,(pnl "</ul>")
               (:meta :shift #\U)
               "Unnumbered list <ul> (m-sh-U)")
              (:numbered-list
               ,(anl "<ol>")
               ,(pnl "</ol>")
               (:meta :shift #\O)
               "Numbered list <ol> (m-sh-O)")
              (:other-list nil nil nil ("Other"
                                        ("Directory list <dir>" :directory-list)
                                        ("Menu list <menu>" :menu-list)))
              (:directory-list ,(anl "<dir>") ,(pnl "</dir>") nil)
              (:menu-list ,(anl "<menu>") ,(pnl "</menu>") nil)
              (:list-entry "<li>" "" (:meta :shift #\n)
               "List entry <li> (m-sh-N)")
              (nil nil nil nil "-")
              (:definition-list
                ,(anl "<dl>")
                ,(pnl "</dl>")
                (:meta :shift #\L)
                "Definition list <dl> (m-sh-L)")
              (:term "<dt>" "" (:meta :shift #\M)
               "Term <dt> (m-sh-M)")
              (:definition "<dd>" "" (:meta #\s)
               "Definition <dd> (m-s)")
              ,@(when *colored-text-p*
                  `((nil nil nil nil "-")
                    (:black-text black-text "" (:meta #\k) "Black Text (m-K)")
                    (:red-text red-text "" (:meta :shift #\R) "Red Text (m-sh-R)")
                    (:highlight-tags highlight-tags "" nil "Highlight HTML Tags")))
              (nil nil nil nil "-")
              (:toggle-html-mode
               toggle-html-mode
               nil
               (:control :meta :shift #\H)
               "Toggle HTML Mode (c-m-s-H)")
              ))))

(defparameter *HTML-control-x-comtab*
  (make-comtab *control-x-comtab*))

(defvar *save-control-x-comtab* nil)

(defparameter *HTML-comtab*
  (let ((comtab (make-comtab *comtab*)))
    (comtab-set-key comtab '(:control #\x) *HTML-control-x-comtab*)))

(defparameter *second-comtabs*
  (make-hash-table :test #'equal))

(defun get-comtab (key)
  (cond ((gethash key *second-comtabs*))
        (t
         (setf (gethash key *second-comtabs*) (make-comtab 'ed-beep)))))

; Key definition is one of three forms, where <keyN> is a key spec:
;   <key1>
;   (:control-x <key2>)
;   (:control-x <key2> <key3>)
; This code doesn't check for a key clobbering a comtab, or vice versa.
; Last definition rules!  Also, this won't remove any bindings you edit
; out of *html-commands* unless you first re-evaluate all of the comtab
; variable definitions, above.
(defun define-html-fred-commands ()
  (dolist (command *html-commands*)
    (let ((name (html-command-name command))
          (key (html-command-fred-keystroke command)))
      (let ((fn #'(lambda (w) (do-html-command w name))))
        (when key
          (if (eq :control-x (first key))
            (ecase (length key)
              (2 (comtab-set-key *HTML-control-x-comtab* (second key) fn))
              (3 (let* ((2nd-key (second key))
                        (2nd-comtab (get-comtab 2nd-key)))
                   (comtab-set-key *HTML-control-x-comtab* 2nd-key 2nd-comtab)
                   (comtab-set-key 2nd-comtab (third key) fn))))
            (comtab-set-key (if (eq name :toggle-html-mode) 
                              *comtab* *HTML-comtab*) key fn)))))))

; For MCL 2.0
(unless (fboundp 'ccl::fred-comtab)
  (defmethod ccl::fred-comtab ((w fred-mixin))
    (slot-value w 'ccl::comtab)))

(unless (fboundp '(setf ccl::fred-comtab))
  (defmethod (setf ccl::fred-comtab) (value (w fred-mixin))
    (setf (slot-value w 'ccl::comtab) value)))

(defun toggle-html-mode (w)
  (setf (view-get w :html-mode-set) t)
  (if (eq *HTML-comtab* (ccl::fred-comtab w))
    (progn
      (set-window-package w (view-get w :saved-window-package))
      (mini-buffer-update w)
      (set-mini-buffer w "Regular Fred Mode")
      (when *control-x-comtab*
        (setq *control-x-comtab* *save-control-x-comtab*))
      (setf (ccl::fred-comtab w) *comtab*))
    (progn
      (setf (view-get w :saved-window-package) (window-package w))
      (set-window-package w (find-package "HTML Mode"))
      (mini-buffer-update w)
      (set-mini-buffer w "HTML Mode")
      (setq *save-control-x-comtab* *control-x-comtab*)
      (setq *control-x-comtab* *HTML-control-x-comtab*)
      (setf (ccl::fred-comtab w) *HTML-comtab*)))
  nil)                                  ; nothing to insert

(defvar *html-menu*
  (make-instance 'menu
    :menu-title "HTML"
    :help-spec "HyperText Markup Language commands"
    :update-function 'html-menu-update
    ))

(defvar *last-html-commands* nil)

(defun html-menu-update (menu &optional force)
  (unless (and (not force)
               (not (menu-font-changed-p))
               (eq menu (car *last-html-commands*))              ; auto-reinstall if command list changes
               (eq *html-commands* (cdr *last-html-commands*)))  ;   or if menu changes.
    (setq *last-html-commands* (cons menu *html-commands*))
    (right-justify-fred-commands *html-commands*)
    (apply 'remove-menu-items menu (menu-items menu))
    (dolist (command *html-commands*)
      (let* ((command-name (html-command-name command))
             (title (html-command-menu-title command)))
        (flet ((get-help-string (ignore)
                 (declare (ignore ignore))
                 (second (assq command-name *html-help-strings*))))
          (when title
            (cond ((stringp title)
                   (add-new-item menu 
                                 title
                                 #'(lambda (w)
                                     (do-html-command w command-name))
                                 :class 'window-menu-item
                                 :help-spec #'get-help-string))
                  ((listp title)
                   (let* ((sub-menu-title (car title))
                          (item-specs (cdr title))
                          (sub-menu (make-instance 'menu :menu-title sub-menu-title)))
                     (add-menu-items menu sub-menu)
                     (right-justify-fred-commands item-specs 'first #'(lambda (v s) (setf (first s) v)))
                     (dolist (spec item-specs)
                       (let* ((spec-title (first spec))
                              (spec-command (second spec)))
                         (flet ((get-help-string (ignore)
                                  (declare (ignore ignore))
                                  (second (assq spec-command *html-help-strings*))))
                           (add-new-item sub-menu
                                         spec-title
                                         (when spec-command
                                           #'(lambda (w)
                                               (do-html-command w spec-command)))
                                         :class 'window-menu-item
                                         :help-spec #'get-help-string)))))))))))
    (define-html-fred-commands)))

(defun get-menu-font-info ()
  ;; This rlet snipped from font-utils.lisp in the Appearance Manager
  ;; package written by Myrland J Gray and Eric Russell <eric-r@nwu.edu>.
  (let ((menu-font-spec (rlet ((name (:string 256)))
                          (#_GetFontName (#_LMGetSysFontFam) name)
                          (list (%get-string name)
                                (#_LMGetSysFontSize)
                                :srcor
                                :plain))))
    (values
     menu-font-spec
     (string-width " " menu-font-spec))))

(defvar *menu-font*)

(defvar *space-width*)

(defun update-menu-font-info ()
  (multiple-value-setq (*menu-font* *space-width*)
    (get-menu-font-info)))

(update-menu-font-info)

(defun menu-font-changed-p ()
  (not (equalp (get-menu-font-info) *menu-font*)))

; This does its best to right justify all the parenthesized
; fred commands at the end of the menu titles.
; It ends up being a little bit jaggy, but that's the best we can do.
(defun right-justify-fred-commands (commands &optional 
                                                  (getter 'html-command-menu-title)
                                                  (setter #'(lambda (v s)
                                                              (setf (html-command-menu-title s) v))))
  (update-menu-font-info)
  (let ((fred-commands nil)
        (max-width 0))
    (dolist (command commands)
      (let* ((title (funcall getter command))
             (length (and (stringp title) (length title))))
        (when (and length (> length 0) (eql #\) (aref title (1- length))))
          (let ((width (string-width title *menu-font*)))
            (setq max-width (max max-width width))
            (push (cons command width) fred-commands)))))
    (dolist (command.width fred-commands)
      (let* ((command (car command.width))
             (width (cdr command.width))
             (title (funcall getter command))
             (length (length title))
             (open-paren-pos (position #\( title :from-end t :end (1- length))))
        (when (and open-paren-pos (< width max-width))
          (let* ((prefix (subseq title 0 open-paren-pos))
                 (suffix (subseq title open-paren-pos))
                 (middle-chars (round (- max-width width) *space-width*))
                 (middle (make-string middle-chars :initial-element #\space)))
            (funcall setter
                     (concatenate 'string prefix middle suffix)
                     command)))))))

(defun install-menus-and-fred-commands ()
  (html-menu-update *html-menu*)        ; installs fred commands
  (menu-install *html-menu*))

(defvar *heading-number* 1)

(defun heading-prefix (w)
  (let ((arg (or (fred-prefix-argument w) *heading-number*)))
    (unless (<= 1 arg 6)
      (message-dialog "Heading number must be between 1 & 6")
      (cancel))
    (setq *heading-number* arg)
    (format nil "<h~d>" arg)))

(defun heading-suffix (w)
  (declare (ignore w))
  (format nil "</h~d>" *heading-number*))

(defun heading-1-prefix (&optional w)
  (declare (ignore w))
  (setq *heading-number* 1)
  "<h1>")

(defun heading-2-prefix (&optional w)
  (declare (ignore w))
  (setq *heading-number* 2)
  "<h2>")

(defun heading-3-prefix (&optional w)
  (declare (ignore w))
  (setq *heading-number* 3)
  "<h3>")

(defun heading-4-prefix (&optional w)
  (declare (ignore w))
  (setq *heading-number* 4)
  "<h4>")

(defun heading-5-prefix (&optional w)
  (declare (ignore w))
  (setq *heading-number* 5)
  "<h5>")

(defun heading-6-prefix (&optional w)
  (declare (ignore w))
  (setq *heading-number* 6)
  "<h6>")

(defun anchor-prefix (&optional w)
  (declare (ignore w))
  (anchor-parameters-dialog)
  nil)                                  ; no insertion

(defun do-anchor-insert (anchor-href image-url alternate-text text-position)
  (let ((w (get-key-handler (target)))
        (prefix (format nil "<a href=~s>~@[~a~]"
                        anchor-href
                        (unless (equal "" image-url)
                          (image-insert-prefix image-url alternate-text text-position)))))
    (insert-prefix-and-suffix w prefix "</a>")))

(defun image-prefix (&optional w)
  (declare (ignore w))
  (image-parameters-dialog)
  nil)                                  ; no insertion

(defun image-insert-prefix (url alternate-text text-position)
  (format nil "<img~@[ align=~(~a~)~] src=~s~@[ alt=~s~]>"
          (unless (eq text-position :bottom) text-position)
          url
          (unless (equal alternate-text "") alternate-text)))

(defun do-image-insert (url alternate-text text-position)
  (let ((w (get-key-handler (target)))
        (prefix (image-insert-prefix url alternate-text text-position)))
    (insert-prefix-and-suffix w prefix "</img>")))

(defparameter *gif-file-type* :|GIFf|)
(defparameter *netscape-creator* :|MOSS|)

(defun choose-url (&optional relative-to-pathname mac-file-type)
  (let ((path (choose-file-dialog :mac-file-type mac-file-type)))
    (relative-pathname-string path relative-to-pathname)))

(defun choose-new-url (prompt &optional relative-to-pathname)
  (let ((path (choose-new-file-dialog :prompt prompt :button-string "Create")))
    (unless (probe-file path)
      (create-file path
                   :mac-file-type (if (equalp "GIF" (pathname-type path))
                                    *gif-file-type*
                                    "TEXT")
                   :mac-file-creator *netscape-creator*))
    (relative-pathname-string path relative-to-pathname)))

(defun relative-pathname-string (path relative-to-path)
  (let* ((path (translate-logical-pathname path))
         (relative-to-path (if relative-to-path
                             (translate-logical-pathname relative-to-path)
                             ""))
         (path-dir (cdr (pathname-directory path)))
         (original-path-dir path-dir)
         (relative-dir (cdr (pathname-directory relative-to-path))))
    (flet ((can-be-relative? ()
             (and path-dir relative-dir
                  (equalp (car path-dir) (car relative-dir)))))
      (loop
        (unless (can-be-relative?) (return))
        (pop path-dir)
        (pop relative-dir))
      (unless (null relative-dir) (setq path-dir original-path-dir))
      (format nil "~a~{~a/~}~a"
              (if (null relative-dir) "" "/")
              path-dir
              (mac-file-namestring path)))))

(defclass url-text-item (editable-text-dialog-item)
  ((button-nick-names :accessor button-nick-names
                      :initarg :button-nick-names
                      :initform nil)
   (dependent-nick-names :accessor dependent-nick-names 
                         :initarg :dependent-nick-names
                         :initform nil)))

(defmethod enter-key-handler ((item url-text-item) last-key-handler)
  (declare (ignore last-key-handler))
  (flet ((enabler (nick-name)
           (let ((view (find-named-sibling item nick-name)))
             (when view (dialog-item-enable view)))))
    (declare (dynamic-extent #'enabler))
    (mapc #'enabler (button-nick-names item))))

(defmethod button-nick-names ((view t))
  nil)

(defmethod exit-key-handler ((item url-text-item) next-key-handler)
  (let ((next-buttons (button-nick-names next-key-handler)))
    (flet ((disabler (nick-name)
             (unless (memq nick-name next-buttons)
               (let ((view (find-named-sibling item nick-name)))
                 (when view (dialog-item-disable view))))))
      (declare (dynamic-extent #'disabler))
      (mapc #'disabler (button-nick-names item)))))

; For MCL 2.0
(unless (fboundp 'set-dialog-item-enabled-p)
  (defmethod set-dialog-item-enabled-p ((item dialog-item) enabled-p)
    (unless (eq (not enabled-p) (not (dialog-item-enabled-p item)))
      (if enabled-p
        (dialog-item-enable item)
        (dialog-item-disable item)))
    enabled-p))

(defmethod dialog-item-action :after ((item url-text-item))
  (let ((enable? (not (eql 0 (buffer-size (fred-buffer item))))))
    (flet ((frobber (nick-name)
             (let ((view (find-named-sibling item nick-name)))
               (when view
                 (set-dialog-item-enabled-p view enable?)))))
      (declare (dynamic-extent #'frobber))
      (mapc #'frobber (dependent-nick-names item)))))  

(defun anchor-parameters-dialog ()
  (let* ((message "[scheme://host.domain[:port]/path/]filename.type[#name]
scheme = file, http, gopher, WAIS, news, telnet, mailto
type = html, txt, gif, tiff, xbm, jpg, jpeg, ps, aiff, au, mov, mpeg, mpg
name = section name assigned by name command")
         (width 500)
         (v-spacing 10)
         (h-spacing 10)
         (message-v 10)
         (message-lines (count #\newline message))
         (message-height (* 16 message-lines))
         (url-v (+ message-v message-height v-spacing))
         (image-url-v (+ url-v 16 v-spacing))
         (alternate-text-v (+ image-url-v 16 v-spacing))
         (radio-1-v (+ alternate-text-v 16 v-spacing))
         (radio-2-v (+ radio-1-v 16 v-spacing))
         (radio-3-v (+ radio-2-v 16 v-spacing))
         (height (+ radio-3-v 16 v-spacing))
         (labels-h 10)
         (labels-width 100)
         (labels-size (make-point labels-width 16))
         (edit-box-h (+ labels-h labels-width h-spacing))
         (edit-box-width (- width edit-box-h 10))
         (edit-box-size (make-point edit-box-width 16))
         (radio-button-size (make-point 100 16))
         (insert-button-width 62)
         (insert-button-h (- width insert-button-width 8))
         (insert-button-v (- height 29))
         (choose-url-button-v insert-button-v)
         (choose-new-url-button-v (- choose-url-button-v 29))
         (choose-url-button-width 82)
         (choose-url-button-h (- insert-button-h choose-url-button-width 10))
         (choose-new-url-button-width 115)
         (choose-new-url-button-h (- insert-button-h choose-new-url-button-width 10))
         (dialog (make-instance 
                  'dialog
                  :view-position '(:top 100)
                  :view-size (make-point width height)
                  :window-title "Anchor HREF"
                  :window-show nil)))
    (add-subviews dialog
                  (make-instance 'static-text-dialog-item
                    :dialog-item-text message
                    :view-position (make-point 10 message-v)
                    :view-size (make-point (+ labels-width edit-box-width) message-height))
                  (make-instance 
                    'default-button-dialog-item
                    :dialog-item-text "Insert"
                    :view-position (make-point insert-button-h insert-button-v)
                    :view-size (make-point insert-button-width 20)
                    :dialog-item-enabled-p t
                    :dialog-item-action
                    #'(lambda (item)
                        (do-anchor-insert (dialog-item-text (find-named-sibling item :anchor-url))
                                          (dialog-item-text (find-named-sibling item :image-url))
                                          (dialog-item-text (find-named-sibling item :alternate-text))
                                          (view-nick-name (pushed-radio-button dialog)))
                        (window-close (view-window item))))
                  (make-instance 'button-dialog-item
                    :view-nick-name :choose-url-button
                    :dialog-item-text "Choose URL"
                    :view-position (make-point choose-url-button-h choose-url-button-v)
                    :view-size (make-point choose-url-button-width 20)
                    :dialog-item-action
                    #'(lambda (item)
                        (let ((key-handler (current-key-handler (view-window item))))
                          (set-dialog-item-text key-handler (choose-url (window-filename (target))))
                          (dialog-item-action key-handler))))
                  (make-instance 'button-dialog-item
                    :view-nick-name :choose-new-url-button
                    :dialog-item-text "Choose New URL"
                    :view-position (make-point choose-new-url-button-h choose-new-url-button-v)
                    :view-size (make-point choose-new-url-button-width 20)
                    :dialog-item-action
                    #'(lambda (item)
                        (let ((key-handler (current-key-handler (view-window item))))
                          (set-dialog-item-text key-handler
                                                (choose-new-url "Create an HREF file (default \".html\""
                                                                (window-filename (target))))
                          (dialog-item-action key-handler))))
                  (make-instance 'static-text-dialog-item
                    :dialog-item-text "Anchor URL:"
                    :view-position (make-point 10 url-v)
                    :view-size labels-size
                    :text-justification :right)
                  (make-instance 'url-text-item
                    :dialog-item-text ".html"
                    :view-nick-name :anchor-url
                    :button-nick-names '(:choose-url-button :choose-new-url-button)
                    :view-position (make-point edit-box-h url-v)
                    :view-size edit-box-size)
                  (make-instance 'static-text-dialog-item
                    :dialog-item-text "Image URL:"
                    :view-position (make-point 10 image-url-v)
                    :view-size labels-size
                    :text-justification :right)
                  (make-instance 'url-text-item
                    :dialog-item-text ""
                    :view-nick-name :image-url
                    :button-nick-names '(:choose-url-button :choose-new-url-button)
                    :dependent-nick-names '(:alternate-text :bottom :middle :top)
                    :view-position (make-point edit-box-h image-url-v)
                    :view-size edit-box-size)
                  (make-instance 'static-text-dialog-item
                    :dialog-item-text "Alternate Text:"
                    :view-position (make-point 10 alternate-text-v)
                    :view-size labels-size
                    :text-justification :right)
                  (make-instance 'editable-text-dialog-item
                    :dialog-item-text ""
                    :view-nick-name :alternate-text
                    :view-position (make-point edit-box-h alternate-text-v)
                    :view-size edit-box-size)
                  (make-instance 'static-text-dialog-item
                    :dialog-item-text "Alignment:"
                    :view-position (make-point 10 radio-1-v)
                    :view-size edit-box-size)
                  (make-instance 'radio-button-dialog-item
                    :dialog-item-text "Bottom"
                    :view-nick-name :bottom
                    :view-position (make-point edit-box-h radio-1-v)
                    :view-size radio-button-size)
                  (make-instance 'radio-button-dialog-item
                    :dialog-item-text "Middle"
                    :view-nick-name :middle
                    :view-position (make-point edit-box-h radio-2-v)
                    :view-size radio-button-size)
                  (make-instance 'radio-button-dialog-item
                    :dialog-item-text "Top"
                    :view-nick-name :top
                    :view-position (make-point edit-box-h radio-3-v)
                    :view-size radio-button-size)
                    )
    ;(set-selection-range (view-named :anchor-url dialog) 0 0)
    (dialog-item-action (view-named :image-url dialog))
    (window-select dialog)
    (when *auto-choose-files*
      (view-focus-and-draw-contents dialog)
      (dialog-item-action (view-named :choose-url-button dialog)))
    dialog))

(defun image-parameters-dialog ()
  (let* ((width 500)
         (v-spacing 10)
         (h-spacing 10)
         (message-v 10)
         (message-height 16)
         (url-v (+ message-v message-height v-spacing))
         (alternate-text-v (+ url-v 16 v-spacing))
         (radio-1-v (+ alternate-text-v 16 v-spacing))
         (radio-2-v (+ radio-1-v 16 v-spacing))
         (radio-3-v (+ radio-2-v 16 v-spacing))
         (height (+ radio-3-v 16 v-spacing))
         (labels-h 10)
         (labels-width 100)
         (labels-size (make-point labels-width 16))
         (edit-box-h (+ labels-h labels-width h-spacing))
         (edit-box-width (- width edit-box-h 10))
         (edit-box-size (make-point edit-box-width 16))
         (radio-button-size (make-point 100 16))
         (insert-button-width 62)
         (insert-button-h (- width insert-button-width 8))
         (insert-button-v (- height 29))
         (choose-url-button-v insert-button-v)
         (choose-new-url-button-v (- choose-url-button-v 29))
         (choose-url-button-width 82)
         (choose-url-button-h (- insert-button-h choose-url-button-width 10))
         (choose-new-url-button-width 115)
         (choose-new-url-button-h (- insert-button-h choose-new-url-button-width 10))
         (dialog (make-instance 
                  'dialog
                  :view-position '(:top 100)
                  :view-size (make-point width height)
                  :window-title "Inline Image Parameters"
                  :window-show nil)))
    (add-subviews dialog
                  (make-instance 'static-text-dialog-item
                    :dialog-item-text "Specify Image file"
                    :view-position (make-point 10 message-v)
                    :view-size (make-point (+ labels-width edit-box-width) message-height))
                  (make-instance 
                    'default-button-dialog-item
                    :dialog-item-text "Insert"
                    :view-position (make-point insert-button-h insert-button-v)
                    :view-size (make-point insert-button-width 20)
                    :dialog-item-enabled-p t
                    :dialog-item-action
                    #'(lambda (item)
                        (do-image-insert (dialog-item-text (find-named-sibling item :url))
                                         (dialog-item-text (find-named-sibling item :alternate-text))
                                         (view-nick-name (pushed-radio-button dialog)))
                        (window-close (view-window item))))
                  (make-instance 'button-dialog-item
                    :view-nick-name :choose-url-button
                    :dialog-item-text "Choose URL"
                    :view-position (make-point choose-url-button-h choose-url-button-v)
                    :view-size (make-point choose-url-button-width 20)
                    :dialog-item-action
                    #'(lambda (item)
                        (set-dialog-item-text (find-named-sibling item :url)
                                              (choose-url (window-filename (target))
                                                          (when *gif-image-files*
                                                            *gif-file-type*)))))
                  (make-instance 'button-dialog-item
                    :view-nick-name :choose-new-url-button
                    :dialog-item-text "Choose New URL"
                    :view-position (make-point choose-new-url-button-h choose-new-url-button-v)
                    :view-size (make-point choose-new-url-button-width 20)
                    :dialog-item-action
                    #'(lambda (item)
                        (set-dialog-item-text (find-named-sibling item :url)
                                              (choose-new-url "Create an image file (default \".gif\")"
                                                              (window-filename (target))))))
                  (make-instance 'static-text-dialog-item
                    :dialog-item-text "URL:"
                    :view-position (make-point 10 url-v)
                    :view-size labels-size
                    :text-justification :right)
                  (make-instance 'editable-text-dialog-item
                    :dialog-item-text ".gif"
                    :view-nick-name :url
                    :view-position (make-point edit-box-h url-v)
                    :view-size edit-box-size)
                  (make-instance 'static-text-dialog-item
                    :dialog-item-text "Alternate Text:"
                    :view-position (make-point 10 alternate-text-v)
                    :view-size labels-size
                    :text-justification :right)
                  (make-instance 'editable-text-dialog-item
                    :dialog-item-text ""
                    :view-nick-name :alternate-text
                    :view-position (make-point edit-box-h alternate-text-v)
                    :view-size edit-box-size)
                  (make-instance 'static-text-dialog-item
                    :dialog-item-text "Alignment:"
                    :view-position (make-point 10 radio-1-v)
                    :view-size edit-box-size)
                  (make-instance 'radio-button-dialog-item
                    :dialog-item-text "Bottom"
                    :view-nick-name :bottom
                    :view-position (make-point edit-box-h radio-1-v)
                    :view-size radio-button-size)
                  (make-instance 'radio-button-dialog-item
                    :dialog-item-text "Middle"
                    :view-nick-name :middle
                    :view-position (make-point edit-box-h radio-2-v)
                    :view-size radio-button-size)
                  (make-instance 'radio-button-dialog-item
                    :dialog-item-text "Top"
                    :view-nick-name :top
                    :view-position (make-point edit-box-h radio-3-v)
                    :view-size radio-button-size)
                    )
    (set-selection-range (view-named :url dialog) 0 0)
    (window-select dialog)
    (when *auto-choose-files*
      (view-focus-and-draw-contents dialog)
      (dialog-item-action (view-named :choose-url-button dialog)))
    dialog))

(defun name-prefix (w)
  (declare (ignore w))
  (let ((href (get-string-from-user "Enter a label name:")))
    (format nil "<a name=~s>" href)))

(defun black-text (w)
  (when *colored-text-p*
    (ed-set-view-font w '((:color-index 0))))
  nil)                                  ; no insertion

(defun red-text (w)
  (when *colored-text-p*
    (ed-set-view-font w *command-font-spec*))
  nil)                                  ; no insertion

; Change all the HTML tags in the selection to red
(defun highlight-tags (view)
  (if (not *colored-text-p*)
    (ed-beep)
    (multiple-value-bind (start end) (selection-range view)
      (multiple-value-bind (ff ms ff-mask ms-mask) (font-codes *command-font-spec*)
        (let ((buf (fred-buffer view))
              (<-pos start)
              (>-pos start))
          (loop
            (setq <-pos (ccl::buffer-forward-find-char buf #\< >-pos end))
            (unless <-pos (return))
            (setq >-pos (ccl::buffer-forward-find-char buf #\> <-pos end))
            (unless >-pos (return))
            (multiple-value-bind (cff cms) (ccl::buffer-char-font-codes buf <-pos)
              (multiple-value-bind (ff ms) (merge-font-codes cff cms ff ms ff-mask ms-mask)
                (buffer-set-font-codes buf ff ms (1- <-pos) >-pos))))))))
  nil)                                   ; no insertion

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Some simple parsing commands
;;;

(defun fwd-find-tag-start (buf &optional pos end)
  (let ((pos (ccl::buffer-forward-find-char buf #\< pos end)))
    (and pos (1- pos))))

(defun fwd-find-tag-end (buf &optional pos end)
  (ccl::buffer-forward-find-char buf #\> pos end))

(defun bwd-find-tag-start (buf &optional pos start)
  (ccl::buffer-backward-find-char buf #\< pos start))

(defun bwd-find-tag-end (buf &optional pos start)
  (let ((pos (ccl::buffer-backward-find-char buf #\> pos start)))
    (and pos (1+ pos))))

(defun fwd-find-tag (buf &optional pos end)
  (let* ((tag-end (fwd-find-tag-end buf pos end))
         (tag-start (and tag-end (bwd-find-tag-start buf tag-end))))
    (and tag-start tag-end (values tag-start tag-end))))

(defun bwd-find-tag (buf &optional pos start)
  (let* ((tag-start (bwd-find-tag-start buf pos start))
         (tag-end (and tag-start (fwd-find-tag-end buf tag-start))))
    (and tag-start tag-end (values tag-start tag-end))))

(defun tag-p (buf start end)
  (and (eql #\< (buffer-char buf start))
       (eql #\> (buffer-char buf (1- end)))))

; If start/end surrounds an end tag, returns its string, otherwise nil
(defun end-tag-p (buf start end)
  (and (tag-p buf start end)            ; redundant
       (eql #\/ (buffer-char buf (1+ start)))
       (buffer-substring buf (+ start 2) (1- end))))

(defun start-tag-p (buf start end)
  (and (tag-p buf start end)            ; redundant
       (not (eql #\/ (buffer-char buf (1+ start))))
       ; Maybe #\space should be any whitespace
       (let ((name-end (or (ccl::buffer-forward-find-char buf #\space (1+ start) (1- end))
                           end)))
         (buffer-substring buf (1+ start) (1- name-end)))))

(defun matching-tag-p (buf tag start end)
  (when (tag-p buf start end)
    (let* ((end-tag? (eql #\/ (buffer-char buf (1+ start))))
           (title-start (if end-tag? (+ start 2) (1+ start)))
           (tag-length (length tag)))
      (and (ccl::buffer-forward-search buf tag title-start (1- end))
           (or (eql end (+ title-start tag-length 1))
               (and (not end-tag?)
                    ; This should probably be any whitespace, not just a space
                    (eql #\space (buffer-char buf (+ title-start tag-length)))))))))

(defun fwd-find-any-end-tag (buf &optional pos end)
  (loop
    (multiple-value-bind (tag-start tag-end) (fwd-find-tag buf pos end)
      (if tag-start
        (let ((tag (end-tag-p buf tag-start tag-end)))
          (if tag 
            (return (values tag tag-start tag-end))
            (setq pos tag-end)))
        (return nil)))))

; This could error better if it knew which tags must have end tags.
(defun bwd-until-start-tag (buf tag &optional pos outstanding-tags)
  (let ((tags (cons tag outstanding-tags)))
    (declare (dynamic-extent tags))
    (loop
      (maybe-abort)                       ; for debugging
      (multiple-value-bind (tag-start tag-end) (bwd-find-tag buf pos)
        (unless tag-start
          (return nil))
        (let* ((end-tag (end-tag-p buf tag-start tag-end)))
          (declare (dynamic-extent tags))
          (if end-tag
            (unless (setq pos (bwd-until-start-tag buf end-tag tag-start tags))
              (return nil))
            (if (matching-tag-p buf tag tag-start tag-end)
              (return (values tag-start tag-end))
              (flet ((matcher (outstanding-tag)
                       (matching-tag-p buf outstanding-tag tag-start tag-end)))
                (declare (dynamic-extent #'matcher))
                (setq pos tag-start)
                (when (find-if #'matcher outstanding-tags)
                  (return nil))))))))))

; Some tags don't have end tags.
; This would be faster and would error better if it knew which ones.
; Instead it just assumes there is no end tag if it doesn't find one.
(defun fwd-until-end-tag (buf tag &optional pos outstanding-tags)
  (flet ((return-it (value)
           (return-from fwd-until-end-tag value)))
    (declare (dynamic-extent #'return-it))
    (let* ((pair (cons tag #'return-it))
           (tags (cons pair outstanding-tags)))
      (declare (dynamic-extent pair tags))
      (loop
        (maybe-abort)                         ; This algorithm is n**2.
        (multiple-value-bind (tag-start tag-end) (fwd-find-tag buf pos)
          (unless tag-start
            (return nil))
          (let* ((start-tag (start-tag-p buf tag-start tag-end)))
            (if start-tag
              (unless (setq pos (fwd-until-end-tag buf start-tag tag-end tags))
                (return nil))
              (progn
                (flet ((matcher (pair)
                         (when (matching-tag-p buf (car pair) tag-start tag-end)
                           (funcall (cdr pair) tag-end))))
                  (declare (dynamic-extent #'matcher))
                  (setq pos tag-end)
                  (mapc #'matcher tags)
                  (return nil))))))))))

; A dialog free event processing abort for MCL 3.0
; It works in 2.0 as well, but isn't necessary there.
(defun maybe-abort ()
  (when (ccl::abort-event-pending-p)
    (#_FlushEvents #x003f 0)
    (abort)))

; MCL 2.0 doesn't have this function, and it handles the aborts OK anyway
(unless (fboundp 'ccl::abort-event-pending-p)
  (defun ccl::abort-event-pending-p () nil))

; This find a tag pair that surrounds the start & end args
(defun find-enclosing-tag-pair (buf &optional (start (buffer-position buf)) (end start))
  (multiple-value-bind (tag end-start end-end) (fwd-find-any-end-tag buf end)
    (when end-start
      (multiple-value-bind (start-start start-end) (bwd-until-start-tag buf tag start)
        (when start-start
          (values start-start start-end end-start end-end))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Movement and deletion commands
;;;

(defun bwd-over-tag-pair (buf &optional pos)
  (multiple-value-bind (tag-start tag-end) (bwd-find-tag buf pos)
    (when tag-start
      (let ((end-tag (end-tag-p buf tag-start tag-end)))
        (if end-tag
          (values (bwd-until-start-tag buf end-tag tag-start))
          tag-start)))))

(defun fwd-over-tag-pair (buf &optional pos)
  (multiple-value-bind (tag-start tag-end) (fwd-find-tag buf pos)
    (when tag-start
      (let ((start-tag (start-tag-p buf tag-start tag-end)))
        (if start-tag
          (or (fwd-until-end-tag buf start-tag tag-end) tag-end)
          tag-end)))))

(defun move-command (w mover &optional (fwd-p t))
  (collapse-selection w fwd-p)
  (let* ((buf (fred-buffer w))
         (pos (buffer-position buf)))
    (dotimes (i (or (fred-prefix-argument w) 1))
      (setq pos (funcall mover buf pos))
      (if pos
        (set-mark buf pos)
        (progn  
          (when (eql 0 i) (ed-beep))
          (return)))))
  nil)                                  ; don't insert anything

(defun ed-fwd-tag (w)
  (move-command w #'(lambda (buf pos) (nth-value 1 (fwd-find-tag buf pos)))))

(defun ed-bwd-tag (w)
  (move-command w 'bwd-find-tag nil))

(defun ed-fwd-over-tag-pair (w)
  (move-command w 'fwd-over-tag-pair))

(defun ed-bwd-over-tag-pair (w)
  (move-command w 'bwd-over-tag-pair nil))

(defun delete-command (w mover &optional reverse?)
  (let* ((buf (fred-buffer w))
         (pos (buffer-position buf)))
    (funcall mover w)
    (let ((end-pos (buffer-position buf)))
      (unless (eql pos end-pos)
        (ed-delete-with-undo w pos end-pos t reverse?))))
  nil)                                  ; don't insert anything

(defun ed-delete-fwd-tag (w)
  (delete-command w 'ed-fwd-tag))

(defun ed-delete-bwd-tag (w)
  (delete-command w 'ed-bwd-tag t))

(defun ed-delete-fwd-over-tag-pair (w)
  (delete-command w 'ed-fwd-over-tag-pair))

(defun ed-delete-bwd-over-tag-pair (w)
  (delete-command w 'ed-bwd-over-tag-pair t))

(defun ed-select-enclosing-tag-pair (w)
  (let ((buf (fred-buffer w)))
    (multiple-value-bind (start end) (selection-range w)
      (multiple-value-bind (start-start start-end end-start end-end) (find-enclosing-tag-pair buf start end)
        (if start-start
          (if (and (eql start-end start) (eql end-start end))
            (set-selection-range w start-start end-end)
            (set-selection-range w start-end end-start))
          (ed-beep)))))
  nil)                                  ; nothing to insert

(defun ed-delete-enclosing-tag-pair (w)
  (let ((buf (fred-buffer w)))
    (multiple-value-bind (start end) (selection-range w)
      (multiple-value-bind (start-start start-end end-start end-end) (find-enclosing-tag-pair buf start end)
        (when start-start
          (set-selection-range w start-end end-start)
          (ed-delete-with-undo w end-start end-end)
          (ed-delete-with-undo w start-start start-end t nil t)))))
  nil)                                  ; nothing to insert

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conversion routines
;;; Most HTML files have newlines instead of carriage returns
;;;

(defun buffer-lf->cr (buf &optional (start 0) (end t))
  (buffer-lf<->cr buf start end #\linefeed #\return))

(defun buffer-cr->lf (buf &optional (start 0) (end t))
  (buffer-lf<->cr buf start end #\return #\linefeed))

(defun buffer-lf<->cr (buf start end from to)
  (setq start (buffer-position buf start)
        end (buffer-position buf end))
  (let ((pos start)
        (last-to-pos nil))
    (loop
      (when (>= pos end) (return))
      (setq pos (ccl::buffer-forward-find-char buf from pos end))
      (unless pos (return))
      (let ((char-pos (1- pos)))
        (cond ((and (> char-pos 0) 
                    (not (eql last-to-pos (1- char-pos)))
                    (eql to (buffer-char buf (1- char-pos))))
               (buffer-delete buf char-pos pos)
               (decf end)
               (setq last-to-pos (1- char-pos)))
              ((and (< pos end) (not (eql last-to-pos pos))
                    (eql to (buffer-char buf pos)))
               (buffer-delete buf char-pos pos)
               (decf end)
               (setq last-to-pos pos))
              (t (buffer-char-replace buf to char-pos)
                 (setq last-to-pos char-pos)))))))

(defun ed-cr->lf (w)
  (buffer-cr->lf (fred-buffer w))
  nil)                                  ; dont' insert anything
  
(defun ed-lf->cr (w)
  (buffer-lf->cr (fred-buffer w))
  nil)                                  ; dont' insert anything

(defconstant *wsp&cr*
  #.(let ((str (make-string 7)))
      (setf (schar str 0) #\Space)
      (setf (schar str 1) #\Tab)
      (setf (schar str 2) #\Page)
      (setf (schar str 3) #\Null)
      (setf (schar str 4) #\Linefeed)
      (setf (schar str 5) (code-char #xCA))
      (setf (schar str 6) #\newline)
      str))

(defconstant <p> "<p>")
(defconstant <p>-length (length <p>))

; This should be smarter about <pre> ... </pre> sections.
(defun insert-paragraph-markers (w)
  (multiple-value-bind (start end) (selection-range w)
    (let* ((buf (fred-buffer w))
           (pos start)
           (append-p nil))
      (loop
        (when (>= pos end) (return))
        (setq pos (ccl::buffer-forward-search buf #.(format nil "~%~%") pos end))
        (unless pos (return))
        (let* ((insert-pos (1- pos))
               (end-of-insert-pos (+ insert-pos <p>-length))
               (previous-non-white-pos (ccl::buffer-backward-find-not-char buf *wsp&cr* insert-pos))
               (next-non-white-pos (ccl::buffer-forward-find-not-char buf *wsp&cr* insert-pos)))
          ; This ensures that we don't add extra paragraph markers if they're already there
          (unless (or (and previous-non-white-pos
                           (let* ((search-pos (- previous-non-white-pos 2))
                                  (found-pos (ccl::buffer-forward-search buf <p> search-pos)))
                             (and found-pos (< found-pos insert-pos))))
                      (and next-non-white-pos
                           (let* ((search-pos (1- next-non-white-pos))
                                  (found-pos (ccl::buffer-forward-search buf <p> search-pos)))
                             (and found-pos (eql found-pos (+ search-pos <p>-length))))))
            ; The next three forms could just be a call to ed-insert-with-undo
            ; in MCL 3.0. MCL 2.0 doesn't have a font arg to ed-insert-with-undo.
            (buffer-insert buf <p> insert-pos)
            (when *command-font-spec*
              (buffer-set-font-spec buf *command-font-spec* insert-pos end-of-insert-pos))
            (ccl::ed-history-add w insert-pos <p> append-p)
            (setq append-p t)
            (incf pos <p>-length)
            (incf end <p>-length)))))))

(defun NetScape-it (w)
  (let ((file (window-filename (view-window w))))
    (when file
      (set-mac-file-creator file *netscape-creator*)))
  nil)                                  ; no insertion

(defun get-key-handler (view)
  (cond ((typep view 'fred-mixin) view)
        ((and (typep view 'window)
              (typep (setq view (current-key-handler view)) 'fred-mixin))
         view)
        (t (ed-beep) (cancel))))

; Insert an HTML command around the selection
(defun do-html-command (view command-name)
  (let ((view (get-key-handler view))
        (command (find command-name *html-commands* :key 'html-command-name)))
    (unless command
      (error "~s is not an HTML command" command-name))
    (let ((prefix (html-command-prefix command))
          (suffix (html-command-suffix command)))
      (unless (or (null prefix) (stringp prefix))
        (setq prefix (funcall prefix view)))
      (when prefix
        (unless (stringp suffix)
          (setq suffix (funcall suffix view)))
        (insert-prefix-and-suffix view prefix suffix))
      (fred-update view))))

(defun insert-prefix-and-suffix (view prefix suffix)
  (multiple-value-bind (start end) (selection-range view)
    (let* ((buf (fred-buffer view))
           (prefix-size (length prefix))
           (suffix-size (length suffix))
           (new-start (+ start prefix-size))
           (font-spec *command-font-spec*)
           (text-spec *text-font-spec*)
           (space-pos (+ end suffix-size))
           (need-space? (and suffix 
                             (not (eql end space-pos))
                             (or (>= end (buffer-size buf))
                                 (not (eql #\space (buffer-char buf end)))))))
      (flet ((do-the-undo-thing (string start)
               (ccl::ed-history-add view start string)          ; this should really be exported
               ; This removed code is here in case ed-history-add goes away
               #+remove
               (let* ((end (+ start (length string)))
                      (style (buffer-get-style buf start end)))
                 (buffer-delete buf start end)
                 (ed-insert-with-undo view (cons string style) end))
               ))
        (declare (dynamic-extent #'do-the-undo-thing))
        (buffer-insert buf suffix end)
        (when font-spec
          (buffer-set-font-spec buf font-spec end (+ end suffix-size)))
        (when need-space?
          (buffer-insert buf " " space-pos)
          (when text-spec
            (buffer-set-font-spec buf text-spec space-pos (1+ space-pos))))
        (do-the-undo-thing (concatenate 'string suffix (if need-space? " " "")) end)
        (buffer-insert buf prefix start)
        (when font-spec
          (buffer-set-font-spec buf font-spec start new-start))
        (do-the-undo-thing prefix start)
        (when (eql start end)
          (set-selection-range view new-start new-start)
          (when text-spec
            (ed-set-view-font view text-spec)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Patches
;;;

(defun maybe-html-mode (w)
  (when (and (not (view-get w :html-mode-set))
             (not (eq *html-comtab* (ccl::fred-comtab w)))
             (let ((file (window-filename w)))
               (and file
                    (equalp "html" (pathname-type file)))))
    (toggle-html-mode w)))

; This one catches the first key after saving with a ".html" name
(advise (:method view-key-event-handler (fred-mixin t))
        (destructuring-bind (w char) arglist
          (declare (ignore char))
          (maybe-html-mode w)
          (:do-it))
        :when :around
        :name :HTML-Editor)

; This one catches newly opened files
(advise (:method view-activate-event-handler (fred-mixin))
        (let ((w (car arglist)))
          (maybe-html-mode w)
          (:do-it))
        :when :around
        :name :HTML-Editor)

(defvar *week-day-names* '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
(defvar *month-names* '(nil "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                           "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

#|
This is what a timestamp looks like.  First and third lines are provided
by *timestamp-begin-marker* and *timestamp-end-marker* constants.  Second
line is provided by make-timestamp-string function.
<!-- htmts begin -->
Last update: Sat, Feb 01 1996 at 21:20:35
<!-- htmts end -->
|#

(defun make-timestamp-string ()
  (multiple-value-bind (seconds
                        minutes hours
                        date month year
                        day-of-week
                        daylight-saving-time-p
                        time-zone)
                       (get-decoded-time)
    (declare (ignorable seconds minutes hours 
                        date month year
                        day-of-week 
                        daylight-saving-time-p 
                        time-zone))
    (format nil "~%Last update: ~A, ~A ~2,'0D ~4D at ~
                 ~2,'0D:~2,'0D:~2,'0D~%"
            (elt *week-day-names* day-of-week)
            (elt *month-names* month) date year
            hours minutes seconds)))  

(defun maybe-update-timestamp (w)
  (when (and (window-needs-saving-p w)
             (view-get w :html-mode-set)
             (eq *html-comtab* (ccl::fred-comtab w)))
    (let ((mark (make-mark (fred-buffer w) t t)))
      (let ((ts-begin (buffer-string-pos mark *timestamp-begin-marker*
                                         :from-end t))
            (ts-end (buffer-string-pos mark *timestamp-end-marker*
                                       :from-end t)))
        (when (and ts-begin ts-end
                   (= (- (buffer-line mark ts-end)
                         (buffer-line mark ts-begin))
                      2))
          (setq ts-begin (+ ts-begin (length *timestamp-begin-marker*)))
          (buffer-delete mark ts-begin ts-end)
          (buffer-insert mark (make-timestamp-string) ts-begin))))))

(advise (:method window-save (fred-mixin))
        (let ((w (car arglist)))
          (maybe-update-timestamp w)
          (:do-it))
        :when :around
        :name :HTML-Editor)

(advise (:method window-save-as (fred-mixin))
        (let ((w (car arglist)))
          (maybe-update-timestamp w)
          (:do-it))
        :when :around
        :name :HTML-Editor)

(defconstant $fred-shift-modifier
  (logand (keystroke-code '(:shift #\a)) (lognot #xff)))

; Make c-s-delete & m-s-delete get the shift modifier
(advise event-keystroke
        (let ((res (:do-it)))
          (destructuring-bind (message modifiers) arglist
            (if (and message 
                     (not (eql 0 (logand #.#$ShiftKey modifiers)))
                     (member (logand res #xff) '(#\delete #\return)))
              (logior res $fred-shift-modifier)
              res)))
        :when
        :around
        :name :HTML-Editor)

; Fix a bug
(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

(fmakunbound 'ccl::installed-item-p)

(defmethod ccl::installed-item-p (item)
  (let ((dialog (view-container item)))
    (and dialog (wptr dialog))))

(defmethod ccl::installed-item-p ((item ccl::control-dialog-item))
  (and (dialog-item-handle item)
       (call-next-method)))

)  ; end of let

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Finally, install the menu
;;;

(install-menus-and-fred-commands)
