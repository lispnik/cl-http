;;; -*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-
;;;
;;; (C) Copyright 1994-97, John C. Mallery.
;;;     All Rights Reserved.
;;;


;;;------------------------------------------------------------------- 
;;;
;;; INTERFACE CODE FOR AUTHORING HTML DOCUMENTS
;;;
;;; See http:standards;draft-ietf-html-spec-00.txt
;;; Updated to html 2.0 conformance -- JCMa 1/21/1995.

(in-package :html2)

(eval-when (compile eval load)
  (mapc #'(lambda (x) 
            (import (intern x :http)  :html2))
        '("WITH-STRING-FOR-NULL-STREAM")))

(define-parameter *output-stream* *standard-output*
                  "The standard place where HTML operator output text.")

(declaim (notinline %write-command-key-arg))

(defun %write-command-key-arg (stream option &optional (value :no-value) no-string)
  (write-char #\space stream)
  (write-string option stream)
  (unless (eq value :no-value)
    (write-char #\= stream)
    (cond (no-string
           (write value :stream stream :base 10. :escape nil))
          ((numberp value)
           (write value :stream stream :base 10))
          ((functionp value)
           (funcall value stream))
          (t (write-char #\" stream)
             (typecase value
               (string
                 (write-string-quoting-specials value stream))
               (symbol
                 (write-string-quoting-specials (symbol-name value) stream))
               (t (let ((string (write-to-string value :escape nil :base 10)))
                    (write-string-quoting-specials string stream))))
             (write-char #\" stream)))))

(declaim (inline %write-command-args))

(defun %write-command-args (stream args)
  (etypecase args
    (cons
      (loop for spec in args
            for (option value no-string) = spec
            when (cdr spec)
              do (%write-command-key-arg stream option (if (cdr spec) value :no-value) no-string)))
    (string (write-string args stream))))

(declaim (notinline %write-command-key-arg))

(defun issue-command* (command arguments &optional (stream *output-stream*) fresh-line trailing-line)
  (when fresh-line
    (fresh-line stream))
  (write-char #\< stream)
  (write-string command stream)
  (etypecase arguments
    (cons (%write-command-args stream arguments))
    (function (funcall arguments stream))
    (null arguments))
  (write-char #\> stream)
  (when trailing-line
    (fresh-line stream)))

(defun issue-command (command &optional (stream *output-stream*) new-line close)
  (with-string-for-null-stream (stream :inline t)
    (when new-line (fresh-line stream))
    (write-char #\< stream)
    (when close (write-char #\/ stream))
    (write-string command stream)
    (write-char #\> stream)))

;; Extra fast version
(defmacro %issue-command ((command stream &key fresh-line trailing-line) &body argument-body)
  `(progn
     ,.(when fresh-line
         `((fresh-line ,stream)))
     (write-char #\< ,stream)
     (write-string ,command ,stream)
     ,@argument-body
     (write-char #\> ,stream)
     ,.(when trailing-line
         `((fresh-line ,stream)))))

(defun environment (command &optional (stream *output-stream*) close)
  (write-char #\< stream)
  (when close (write-char #\/ stream))
  (write-string command stream)
  (write-char #\> stream))

(defmacro with-environment ((environment &key arguments close-command (fresh-line t)
                                         string-for-null-stream (stream '*output-stream*))
                            &body body)
  (let ((code `(multiple-value-prog1
                 (progn ,.(if arguments
                              `((issue-command* ,environment ,arguments ,stream ,fresh-line))
                              `(,@(when fresh-line `((fresh-line ,stream)))
                                (environment ,environment ,stream)))
                        ,@body)
                 (environment ,(or close-command environment) ,stream t)
                 ,.(when fresh-line `((fresh-line ,stream))))))
    (if string-for-null-stream
        `(with-string-for-null-stream (,stream :inline ,(eq string-for-null-stream :inline))
           ,code)
        code)))

;; Extra fast environment macro that avoids consing the arguments by
;; executing the body of code passed in there
(defmacro %with-environment ((environment &key close-command string-for-null-stream (fresh-line t) (stream '*output-stream*))
                             arguments
                             &body body)
  (let ((code `(multiple-value-prog1
                 (progn ,.(if arguments
                              `((%issue-command (,environment ,stream :fresh-line ,fresh-line) ,arguments))
                              `(,@(when fresh-line `((fresh-line ,stream)))
                                (environment ,environment ,stream)))
                        ,@body)
                 (environment ,(or close-command environment) ,stream t)
                 ,.(when fresh-line `((fresh-line ,stream))))))
    (if string-for-null-stream
        `(with-string-for-null-stream (,stream :inline ,(eq string-for-null-stream :inline))
           ,code)
        code)))

(define-macro with-comment ((&key (stream '*output-stream*)) &body body)
  "Establishes comment environment around BODY."
  `(multiple-value-prog1 
     (progn (fresh-line ,stream)
            (write-string "<!-- " ,stream)
            ,@body)
     (write-string " -->" ,stream)))

(define comment (string &key (stream *output-stream*))
  "Writes STRING as a comment on STREAM."
  (with-comment (:stream stream)
    (write-string string stream)))

;;;------------------------------------------------------------------- 
;;;
;;; TRANSLATING SPECIAL HTML CHARACTERS
;;;

(eval-when (compile eval load)
  (define-constant *special-character-translation-alist*
    '((#\> . "&gt;")
      (#\< . "&lt;")
      (#\& . "&amp;")
      (#\" . "&quot;"))
    "&; delimited tokens are used to print special tokens.")

  (define-constant *max-length-quotation-token* 6))

(eval-when (compile eval load)

  (declaim (inline token-for-special-char))

  (define token-for-special-char (char)
    "If CHAR is a special token, this returns the corresponding quotation token,
otherwise it returns NIL. &; delimited tokens are used to print special tokens.
CHAR is compared with CHAR=."
    #.`(case char
         ,.(loop for (char . string) in *special-character-translation-alist*
                 collect `(,char ,string))
         (t nil)))

  )                                             ;close eval-when

;(define write-string-quoting-specials (string &optional (stream *output-stream*))
;  "Writes STRING to STREAM being careful to translated any special characters for HTML."
;  (with-fast-array-references ((vector string))
;    (loop for idx upfrom 0 to (the fixnum (1- (length vector)))
;         for char = (aref vector idx)
;         for token = (token-for-special-char char)
;         when token
;           do (write-string token stream)
;         else do (write-char char stream))))

;; faster version
(define write-string-quoting-specials (string &optional (stream *output-stream*) (start 0) end)
  "Writes STRING to STREAM being careful to translated any special characters for HTML."
  (flet ((%token-spec-for-special-char (char)
           #.`(case char
                ,.(loop for (char . string) in *special-character-translation-alist*
                        collect `(,char '(,string . ,(length string))))
                (t nil)))
         (write-part (string stream start end)
           (unless (= start end)
             (write-string string stream :start start :end  end))))
    (declare (inline %token-spec-for-special-char write-part))
    (with-fast-array-references ((vector string string))
      (loop with scan-idx
            for idx upfrom start below (or end (length vector))
            for char = (aref vector idx)
            for token-spec = (%token-spec-for-special-char char)
            do (when token-spec
                 (write-part vector stream (or scan-idx start) idx)
                 (write-string (car token-spec) stream :start 0 :end (cdr token-spec))
                 (setq scan-idx (1+ (the fixnum idx))))
            finally (if scan-idx
                        (write-part vector stream scan-idx idx)
                        (write-string vector stream :start start :end idx))))))

(declaim (inline special-char-for-token))

(define special-char-for-token (token)
  "If TOKEN is a quotation token, this returns the corresponding special character,
otherwise it returns NIL."
  (car (rassoc token *special-character-translation-alist* :test #'equalp)))

(define write-char-quoting-specials (char &optional (stream *output-stream*))
  "Writes CHAR on STREAM, using HTML quotation for special HTML characters."
  (let ((token (token-for-special-char char)))
    (if token
        (write-string token stream)
        (write-char char stream)))) 

(define read-char-unquoting-specials (&optional input-stream (eof-errorp t) eof-value recursive-p)
  "Reads a character from INPUT-STREAM, 
translating quotation tokens to corresponding special character."
  (macrolet ((back-out-of-bad-markup (buffer)
               `(loop for char = (pop ,buffer)
                      do (if (null ,buffer)
                             (return-from read-char-unquoting-specials char)
                             (unread-char char input-stream))))
             (push-buffer (char buffer pos)
               `(progn (push ,char ,buffer)
                       (the fixnum (incf ,pos))))
             (reverse-buffer (buffer)
               `(setq ,buffer (nreverse ,buffer))))
    (loop with special-p and buffer and len = 0
          for char = (read-char input-stream eof-errorp eof-value recursive-p)
          until (eql char eof-value)
          do (cond
               (special-p
                (cond ((= len *max-length-quotation-token*)
                       (back-out-of-bad-markup buffer))
                      ((char-equal char #\;)
                       (push-buffer char buffer len)
                       (reverse-buffer buffer)
                       (let* ((token (coerce buffer 'string))
                              (special-char (special-char-for-token token)))
                         (declare (dynamic-extent token))
                         (cond (special-char
                                (return-from read-char-unquoting-specials special-char))
                               (t (reverse-buffer buffer)
                                  (back-out-of-bad-markup buffer)))))
                      (t (push-buffer char buffer len))))
               ((char-equal char #\&)
                (push-buffer char buffer len)
                (setq special-p t))
               (t (return-from read-char-unquoting-specials char)))
          finally (return eof-value))))

;;;------------------------------------------------------------------- 
;;;
;;; HYPERTEXT LINKS
;;;

(declaim (inline %write-anchor-command-arguments))

(defun %write-anchor-command-arguments
       (stream reference local-reference tag relation inverse urn title methods)
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
      (%write-command-key-arg stream "NAME" tag))
    (relation ;; "The relationship the anchored document has to this one."
      (%write-command-key-arg stream "REL" relation)) 
    (inverse ;; "The reverse relationship type, and the inverse of REL."
      (%write-command-key-arg stream "REV" inverse))
    (urn ;;  "URN for a anchored document retrieved by the anchor."
      (%write-command-key-arg stream "URN" urn))
    (title ;;  "Title to use for otherwise untitled anchored document."
      (%write-command-key-arg stream"TITLE" title))
    (methods ;;  "Comma separated list of HTTP methods supported by the anchored  object."
      (%write-command-key-arg stream"METHODS" methods))))

(defun %note-anchor (stream text reference local-reference tag relation inverse urn title methods)
  (%with-environment ("A" :string-for-null-stream :inline :stream stream :fresh-line nil)
                     (%write-anchor-command-arguments stream reference local-reference
                                                      tag relation inverse urn title methods)
    (write-string text stream)))

(declaim (notinline %write-anchor-command-arguments))

(declaim (inline note-anchor))

(define note-anchor (text &key reference local-reference tag relation inverse urn title methods
                          (stream *output-stream*))
  "Notes a hypertext anchor for TEXT on STREAM.
   REFERENCE is a URL (object or string).
   LOCAL-REFERENCE is reference to a tag local to the current page.
   TAG is a string denoting a named point in the document which can be referenced with a local reference (#name).
   RELATION is the link type from the referring document to the the anchored document.
   INVERSE is the inverse relationship back from the anchored document to the referring document.
   URN is the URN for a anchored document retrieved by the anchor.
   TITLE is title to use for otherwise untitled anchored document.
   METHODS are a comma separated list of HTTP methods supported by the anchored  object."
  (%note-anchor stream text reference local-reference tag relation inverse urn title methods))

(define-macro with-anchor-noted ((&key reference local-reference tag (stream '*output-stream*)
                                       relation inverse urn title methods) &body body)
  "Like NOTE-ANCHOR except forms in BODY can compute and write the anchored text on STREAM.
   REFERENCE is a URL (object or string).
   LOCAL-REFERENCE is reference to a tag local to the current page.
   TAG is a string denoting a named point in the document which can be referenced with a local reference (#name).
   RELATION is the link type from the referring document to the the anchored document.
   INVERSE is the inverse relationship back from the anchored document to the referring document.
   URN is the URN for a anchored document retrieved by the anchor.
   TITLE is title to use for otherwise untitled anchored document.
   METHODS are a comma separated list of HTTP methods supported by the anchored  object."
  `(locally
     (declare (notinline %write-anchor-command-arguments))
     (let ((stream ,stream))
       (%with-environment ("A" :string-for-null-stream :inline :stream stream :fresh-line nil)
                          (%write-anchor-command-arguments stream ,reference ,local-reference
                                                           ,tag ,relation ,inverse ,urn ,title ,methods)
         ,@body))))

(defun write-string-anchoring-urls (string stream &optional (start 0) (end (length string)))
  "Writes STRING to STREAM spotting any URLS and anchoring them
while being careful to translated any special characters for HTML."
  (loop with s = start
        doing (multiple-value-bind (url-start url-end)
                  (url::url-position string s end)
                (cond ((and url-start url-end)
                       (unless (= s url-start)
                         (write-string-quoting-specials string stream s url-start))
                       (let ((url (subseq string url-start url-end)))
                         (declare (dynamic-extent url))
                         (with-anchor-noted (:reference url :stream stream)
                           (write-string-quoting-specials string stream url-start url-end)))
                       (if (< url-end end)
                           (setq s url-end)
                           (return-from write-string-anchoring-urls)))
                      (t (write-string-quoting-specials string stream s end)
                         (return-from write-string-anchoring-urls))))))

(defun %declare-link (stream reference local-reference tag relation inverse urn title methods)
  (declare (notinline %write-anchor-command-arguments)) ; speed not essential so conserve working set  12/4/95 -- JCMa.
  (%issue-command ("LINK" stream :fresh-line t :trailing-line t)
    (%write-anchor-command-arguments stream reference local-reference
                                     tag relation inverse urn title methods)))

(declaim (inline declare-link))

(define declare-link (&key reference local-reference tag relation inverse urn title methods (stream *output-stream*))
  "Declares a link between the current document and some other object
   REFERENCE is a URL (object or string).
   LOCAL-REFERENCE is reference to a tag local to the current page.
   TAG is a string denoting a named point in the document which can be referenced with a local reference (#name).
   RELATION is the link type from the referring document to the the anchored document.
   INVERSE is the inverse relationship back from the anchored document to the referring document.
   URN is the URN for a anchored document retrieved by the anchor.
   TITLE is title to use for otherwise untitled anchored document.
   METHODS are a comma separated list of HTTP methods supported by the anchored  object."
  (%declare-link stream reference local-reference tag relation inverse urn title methods))

(define declare-base-reference (reference &key (stream *output-stream*))
  "Declares REFERENCE to be the base URL for a html page.
   This should appear once in the headers section of a document.
   When REFERENCE is a URL, the directory URL is automatically computed."
  (%issue-command ("BASE" stream :fresh-line t :trailing-line t)
    (%write-command-key-arg stream "HREF" (etypecase reference
                                            (url:url (url::directory-name-string reference))
                                            (string reference)))))

;;;------------------------------------------------------------------- 
;;;
;;; IMAGES
;;;

(defconstant *image-alignment-values* '(:top :middle :bottom))

;; html spec says to always provide alternative text
;; "~&<IMG SRC=\"~A\" ALIGN=\"~A\" ALT=\"~A\">~&"
(defun %note-image (stream image-url alternative-text alignment accept-coordinates-at-url)
  (flet ((write-element (stream image-url alignment alternative-text accept-coordinates-at-url)
           (flet ((alignment-value (alignment)
                    (unless (member alignment *image-alignment-values*)
                      (error "Unknown alignment, ~S, for an image." alignment))
                    (symbol-name alignment)))
             (declare (inline alignment-value))
             (%issue-command ("IMG" stream)
               (cond-every
                 (image-url
                   (%write-command-key-arg stream "SRC" image-url))
                 (alignment
                   (%write-command-key-arg stream "ALIGN" (alignment-value alignment)))
                 (alternative-text
                   (check-type alternative-text string)
                   (%write-command-key-arg stream "ALT" alternative-text))
                 (accept-coordinates-at-url
                   (%write-command-key-arg stream "ISMAP")))))))
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

(define image (image-url alternative-text &key (alignment :top) accept-coordinates-at-url (stream *output-stream*))
  "IMAGE-URL is the URL for an image and ALTERNATIVE-TEXT is the text to display when the image
   is not loaded. 

   ALIGNMENT can be:

                      :TOP align with the tallest item on the line.   :MIDDLE
                    align with the baseline with the middle of the image.
                    :BOTTOM align with the baseline of the current line with
                    the image.

   
   ACCEPT-COORDINATES-URL can be:
   
   * A URL to which coordinates will be returned when the user clicks on the image.
   * T, indicating that returned coordinates should go to a search URL version of IMAGE-URL
   * :NO-URL, indicating not to emit an anchor for accepting the returns but to mark
   the IMG as a coordinate search."
  (%note-image stream image-url alternative-text alignment accept-coordinates-at-url))


;;;------------------------------------------------------------------- 
;;;
;;; SECTIONS
;;;

(define-macro with-html-document ((&key (stream '*output-stream*)) &body body)
  "Asserts the contents of BODY is an html document(see HTML)."
  `(with-environment ("HTML" :stream ,stream)
     ,@body))

(define-macro with-document-preamble ((&key (stream '*output-stream*)) &body body)
  "Asserts the contents of BODY are headings for the document (see HEAD)."
  `(with-environment ("HEAD" :stream ,stream)
     ,@body))

(define-macro with-document-body ((&key (stream '*output-stream*)) &body body)
  "Asserts the contents of BODY are the body of the document (see BODY)."
  `(with-environment ("BODY" :stream ,stream)
     ,@body))

(declaim (inline declare-title))

(define declare-title (title &key (stream *output-stream*))
  "Declares the title for a document.
Every HTML document must declare exactly one TITLE element in the preamble.
TITLE can be either a string or a function which is called with (stream)."
  (with-environment ("TITLE" :stream stream)
    (etypecase title
      (string (write-string title stream))
      (function (funcall title stream)))))

(define declare-meta-info (value &key name header (stream *output-stream*))
  "Declares meta information within an HTML document.
NAME and/or HEADER specify the type of meta information.
VALUE is the content of the declaration.
HEADER is used only for defined HTTP response header information.
NAME is used in other cases."
  (unless (or name header)
    (error "At least one of NAME or HEADER must be provided."))
  (%issue-command ("META" stream :fresh-line t :trailing-line t)
    (cond-every
      (name (%write-command-key-arg stream "NAME" name))
      (header
        (check-type header keyword)
        (%write-command-key-arg stream "HTTP-EQUIV" (symbol-name header))))
    (%write-command-key-arg stream "CONTENT" value)))

(define declare-next-identifier (identifier &key (stream *output-stream*))
  "Declares the next document-wide alpha-numeric IDENTIFIER on STREAM."
  (%issue-command ("NEXTID" stream)
    (%write-command-key-arg stream "N" identifier t)))

(define declare-search-index (&key url (stream *output-stream*))
  "Declares that the current document is searchable."
  (%issue-command ("ISINDEX" stream :fresh-line t :trailing-line t)
    (when url
      (%write-command-key-arg stream "HREF" (coerce-url-string url)))))

(defvar *section-level* 1)

(defun section-heading (heading &optional identifier (stream *output-stream*) (level *section-level*)
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
                     (when identifier
                       (check-type identifier string)
                       (%write-command-key-arg stream "ID" identifier))
    (etypecase heading
      (string (write-string heading stream))
      (function (funcall heading stream)))))

(define-macro with-section-heading ((heading &key (level '*section-level*) identifier (stream '*output-stream*)) &body body)
  "Excutes BODY within a section heading of depth LEVEL.
HEADING can be a string or a function called with (stream)."
  (if body
      `(progn
         (section-heading ,heading ,identifier ,stream ,level)
         (let ((*section-level* (the fixnum (1+ ,level))))
           ,@body))
      `(section-heading ,heading ,identifier ,stream ,level)))

;;;------------------------------------------------------------------- 
;;;
;;; BREAKS
;;;

(declaim (inline break-line))

(define break-line (&key  (stream *output-stream*))
  "Issues a line break on stream."
  (issue-command "BR" stream))

(declaim (inline horizontal-line))

(define horizontal-line (&key fresh-line (stream *output-stream*))
  "Writes a horizontal line across the output."
  (issue-command "HR" stream fresh-line))

(declaim (inline new-paragraph))

(define new-paragraph (&key (stream *output-stream*))
  "Signals the start of a new paragraph. Obsolete.  Use WITH-PARAGRAPH."
  (issue-command* "P" nil  stream t))

(define-macro with-paragraph ((&key (stream '*output-stream*)) &body body)
  "Establishes a paragraph environment."
  `(with-environment ("P" :stream ,stream)
     ,@body))

;;;------------------------------------------------------------------- 
;;;
;;; PARAGRAPH STYLES
;;;

(eval-when (compile eval load)
  (defconstant *paragraph-styles* '((:quotation . "BLOCKQUOTE")
                                    (:address . "ADDRESS")
                                    (:citation . "CITE")
                                    (:code . "CODE"))))

(declaim (inline paragraph-style-command))

(defun paragraph-style-command (style)
  (or (cdr (assoc style *paragraph-styles*))
      (error "Unknown paragraph style, ~A." style)))

(define-macro with-paragraph-style ((style &key (fresh-line t) (stream '*output-stream*)) &body body)
  #.(format nil "Puts the enclosed paragraph into STYLE, which can be any of: ~{~S~^, ~}." (mapcar #'car *paragraph-styles*))
  `(with-paragraph (:stream ,stream)
     (with-environment ((paragraph-style-command ,style) :stream ,stream :fresh-line ,fresh-line)
       ,@body)))

;;;------------------------------------------------------------------- 
;;;
;;; LITERAL ENVIRONMENTS
;;;

(define-macro with-verbatim-text ((&key (fresh-line t)(width 80.) (stream '*output-stream*)) &body body)
  "Formats text within BODY in a fixed width font exactly as printed,
as might be used for computer output or plain text files.
WIDTH controls how wide the preformated text.  User agents support widths of
40, 80 (the default), 132, with upward rounding.
Character styles can be controlled via the WITH-RENDITION macro.
Line and paragraph breaks functions as CR. ASCII tab is interpreted
as spaces in multiples of 8, but its use is not recommended"
  `(%with-environment ("PRE" :fresh-line ,fresh-line :stream ,stream)
                      (when ,width
                        (%write-command-key-arg ,stream "WIDTH" ,width nil))
     ,@body))

;;;------------------------------------------------------------------- 
;;;
;;; ENUMERATION ENVIRONMENT
;;;

;; comment out in lieu of a plain enumeration tag in HTML2   2/3/96 -- JCMa.
(defun enumerate-plain-item (stream continuation icon-url head)
  (declare (ignore head icon-url))
;  (%issue-command ("LI" stream :fresh-line t)
;    (when icon-url
;      (%write-command-key-arg stream "SRC" (coerce-url-string icon-url))))
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
    (prog1 (funcall continuation stream))))

(defun enumerate-normal-item (stream continuation icon-url head)
  (declare (ignore head))
  (%issue-command ("LI" stream :fresh-line t)
    (when icon-url
      (%write-command-key-arg stream "SRC" (coerce-url-string icon-url))))
  (funcall continuation stream)
  (fresh-line stream))

(defvar *enumeration-function*)

(define-macro enumerating-item ((stream &key icon-url head) &body body)
  `(funcall *enumeration-function* ,stream
            #'(lambda (,stream) ,@body)
            ,icon-url ,head))

(defconstant *enumeration-styles* '((:enumerate "OL" . "OL")
                                    (:itemize "UL" . "UL")
                                    (:plain "MENU" . "MENU")
                                    (:menu "MENU" . "MENU")
                                    (:directory "DIR" . "DIR")
                                    (:definition "DL" . "DL")))

(defun enumeration-style-commands (style &optional compact)
  (declare (values open-command close-command)
           (ignore compact))                    ;obsolete   12/4/95 -- JCMa.
  (let ((entry (assoc style *enumeration-styles*)))
    (cond (entry
           (destructuring-bind (open . close) (cdr entry)
             (values open close)))
          (t (error "Unknown enumeration style, ~A." style)))))

(define-macro with-enumeration ((stream style &key compact) &body body)
  "Enumerates items according to STYLE.
STYLE can be :ENUMERATE :ITEMIZE :PLAIN :MENU :DIRECTORY :DEFINITION
COMPACT tries to keep the entry compact.
Each item for enumeration is called inside the enumerating-item macro.
this macro accepts a keyword argument ICON-URL, which is useful when
an icon is desired as the bullet.

You must use the ENUMERATING-ITEM from the same package for reliable results."
  `(multiple-value-bind (open close)
       (enumeration-style-commands ,style)
     (%with-environment (open :close-command close :stream ,stream)
                        ,(when compact
                           `(when ,compact
                              (%write-command-key-arg ,stream "COMPACT")))
       (let ((*enumeration-function* (case ,style
                                       (:plain #'enumerate-plain-item)
                                       (:definition #'enumerate-definition-item)
                                       (t #'enumerate-normal-item))))
         ,@body))))

(define enumerate-item-list (item-list &key (style :itemize) compact (stream *output-stream*))
  (with-enumeration (stream style :compact compact)
    (dolist (item item-list)
      (enumerating-item (stream)
        (write item :stream stream :escape nil)))))

;;;------------------------------------------------------------------- 
;;;
;;; TEXT RENDITION
;;;

(eval-when (compile eval load)
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
                 #|(:underline . "U")
               (:strike . "S")
               (:superscript . "SUP")
               (:subscript . "SUB")|#
                 )))

(declaim (inline rendition-command))

(defun rendition-command (rendition)
  (or (cdr (assoc rendition *rendition*))
      (error "Unknown type of rendition, ~S." rendition)))

(define-macro with-rendition ((rendition &key (stream '*output-stream*)) &body body)
  "Text output within BODY on STREAM is rendered according to RENDITION,
which can be any of :ITALIC, :BOLD, :TELETYPE, :HEADING-1, :HEADING-2,  :HEADING-3, 
:HEADING-4, :HEADING-5, :HEADING-6."
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
                 (:citation . "CITE")
                 (:keyboard . "KBD")
                 (:variable . "VAR")
                 (:code . "CODE")
                 (:sample . "SAMP"))))

(declaim (inline emphasis-command))

(defun emphasis-command (emphasis)
  (or (cdr (assoc emphasis *emphasis*))
      (error "Unknown type of emphasis, ~S." emphasis)))

(define-macro with-emphasis ((emphasis &key (stream '*output-stream*)) &body body)
  "Renders output within BODY on STREAM according to EMPHASIS, 
which can be any of :EMPHASIS, :STRONG, :ADDRESS, :QUOTATION, :CITATION, :KEYBOARD, :VARIABLE, :CODE, :SAMPLE."
  `(with-environment (,(typecase emphasis
                         (keyword (emphasis-command emphasis))
                         (t `(emphasis-command ,emphasis)))
                      :fresh-line nil :stream ,stream)
     ,@body))

;;;------------------------------------------------------------------- 
;;;
;;; INPUT TYPES
;;;

;;; When an INPUT-TYPE is defined (using DEFINE-INPUT-TYPE), we also
;;; make a single instance of that input type and associate it with all
;;; of the INPUT-TYPE's nicknames in *INPUT-TYPES*.  This is not done if
;;; the input type is :ABSTRACT T.  There's a bunch of machinery below
;;; to achieve this.

(defclass input-type
          ()
    ((name :initarg :name :reader name)
     (type-arg :initarg :type-arg :reader type-arg)
     (lisp-type :initarg :lisp-type :reader lisp-type)
     (nicknames :initform nil :initarg :nicknames :reader nicknames)))

(defvar *input-types* (make-hash-table))

(defmethod unregister ((input-type input-type))
  (with-slots (name nicknames) input-type
    (remhash name *input-types*)
    (dolist (item nicknames)
      (remhash item *input-types*))))

(defmethod register ((input-type input-type))
  (with-slots (name nicknames) input-type
    (setf (gethash name *input-types*) input-type)
    (dolist (item nicknames)
      (setf (gethash item *input-types*) input-type))
    input-type))

(defmethod initialize-input-type ((input-type input-type))
  (register input-type))

(eval-when (eval load compile )
  (defun make-input-type-code (class name type-arg lisp-type nicknames)
    `(initialize-input-type
       (make-instance ',class :name ',name :type-arg ',type-arg :lisp-type ,lisp-type :nicknames ',nicknames))))

(define-macro define-input-type (name &key (superclass 'input-type) type-arg lisp-type parameters nicknames abstract)
  "Top-level form for defining new input types."
  `(progn
     (defclass ,name
               (,superclass)
         ,parameters)
     ,(unless abstract
        (make-input-type-code name name type-arg lisp-type nicknames))))

(define-generic check-value-type (input-type value))

(defmethod check-value-type ((input-type input-type) value)
  (with-slots (name lisp-type) input-type
    (unless (typep value lisp-type)
      (error "The value, ~S, is not a ~S as required by ~S."
             value lisp-type name))))

(define verify-query-name (query-name)
  "Signals an error when QUERY-NAME is an illegal query-name for an HTML input type."
  (cond-every
    ((not (stringp query-name))
     (error "The query name, ~S, is not a string." query-name))
    ((some #'http:escaped-character-p query-name)
     (error "The query name, ~S, contains illegal characters for HTML." query-name))))

(define-parameter *event-handlers* 
  '((:java-script
      (:input-defocus    . "onBlur")
      (:input-focus      . "onFocus")
      (:load             . "onLoad")
      (:mouse-click      . "onClick")
      (:mouse-over       . "onMouseOver")
      (:new-value        . "onChange")
      (:select           . "onSelect")
      (:submit           . "onSubmit")
      (:unload           . "onUnload"))
    (:java-script1.1
      (:abort-image-load . "onAbort")           ; netscape 3.0 -- user aborts loading of an image
      (:form-reset       . "onReset")           ; netscape 3.0
      (:input-defocus    . "onBlur")
      (:input-focus      . "onFocus")
      (:load             . "onLoad")
      (:load-error       . "onError")           ; netscape 3.0 -- image or document
      (:mouse-click      . "onClick")
      (:mouse-out        . "onMouseOut")        ; netscape 3.0
      (:mouse-over       . "onMouseOver")
      (:new-value        . "onChange")
      (:select           . "onSelect")
      (:submit           . "onSubmit")
      (:unload           . "onUnload"))
    (:java-script1.2
      (:abort-image-load . "onAbort")           ; netscape 3.0 -- user aborts loading of an image
      (:form-reset       . "onReset")           ; netscape 3.0
      (:input-defocus    . "onBlur")
      (:input-focus      . "onFocus")
      (:key-down . "onKeyDown")                 ; Netscape 4.0
      (:key-press . "onKeyPress")               ; Netscape 4.0
      (:key-up . "onKeyUp")                     ; Netscape 4.0 
      (:load             . "onLoad")
      (:load-error       . "onError")           ; netscape 3.0 -- image or document
      (:mouse-click      . "onClick")           ; Netscape 4.0
      (:mouse-double-click      . "onDblClick")
      (:mouse-down . "onMouseDown")             ; Netscape 4.0 
      (:mouse-drag-drop . "onDragDrop")         ; Netscape 4.0
      (:mouse-move . "onMouseMove")             ; Netscape 4.0
      (:mouse-out        . "onMouseOut")        ; netscape 3.0
      (:mouse-over       . "onMouseOver")
      (:mouse-up . "onMouseUp")                 ; Netscape 4.0 
      (:new-value        . "onChange")
      (:select           . "onSelect")
      (:submit           . "onSubmit")
      (:unload           . "onUnload")
      (:window-move . "onMove")                 ; Netscape 4.0 
      (:window-resize . "onResize")))           ; Netscape 4.0 
  "An alist of (SCRIPTING-LANGUAGE (EVENT-KEYWORD . EVENT-TAG)).
This variable describes all known client-side scripting languages.")

(defun %get-input-type-event-tag (event language)
  (let ((entry (assoc language *event-handlers* :test #'eq)))
    (if entry
        (or (cdr (assoc event (cdr entry) :test #'eq))
            (error "~S is an unknown event for ~S." event language))
        (error "~S is not one of the known event languages: ~S" language (mapcar #'car *event-handlers*)))))

(defun %write-input-type-event-arg (stream event)
  (etypecase event
    ;; when function are pass in, just call them because they were compiled
    ;; with the with-event-handlers macro.
    (function (funcall event stream))
    ;; otherwise run interpreted code.
    #+ignore(cons
              (destructuring-bind ((event . language) command) event
                (write-char #\space stream)
                (write-string (%get-input-type-event-tag event language) stream)
                (write-char #\= stream)
                (etypecase command
                  (string (write command :stream stream :escape nil))
                  (function (funcall command stream)))))))

(defun %collect-input-type-events (event-clauses)
  (loop with fnames2 and fbodies2
        for (language . clause) in event-clauses
        do (multiple-value-setq
             (function-names function-bodies)
             (loop for (event command) on clause by #'cddr
                   for tag = (%get-input-type-event-tag event language)
                   for function-name = (gensym "EVENT-HANDLER-")
                   for key-arg-code = `((write-char #\space stream)
                                        (write-string ,tag stream)
                                        (write-char #\= stream))
                   collect `(function ,function-name) into fnames
                   collect (typecase command
                             (string
                               `(,function-name (stream)
                                 ,@key-arg-code
                                 (write ,command :stream stream :escape nil)))
                             (t `(,function-name (stream)
                                  (let ((command ,command))
                                    ,@key-arg-code
                                    (etypecase command
                                      (function (funcall command stream))
                                      (string (write command :stream stream :escape nil))
                                      ((and symbol (satisfies fboundp))
                                       (funcall command stream)))))))
                     into fbodies
                   finally (setq fnames2 fnames
                                 fbodies2 fbodies)))
        nconc fnames2 into function-names
        nconc fbodies2 into function-bodies
        finally (return (values function-names function-bodies))))

;; compiled version does less work at runtime.
(define-macro with-event-handlers ((variable &rest clauses) &body body)
  "Binds VARIABLE within BODY to client-side event handlers specified by CLAUSES .

  CLAUSES is list of languages and event-clauses of the form 
  (SCRIPTING-LANGUAGE &rest EVENTS).
  EVENTS are a sequence of EVENT COMMAND pairs. EVENT can be any
  event keyword for SCRIPTING-LANGUAGE and COMMAND is either a string or a
  function which receives STREAM as an argument. Any number of EVENT and COMMAND
  pairs can be bound.

  *EVENT-HANDLERS* contain the known scripting languages with their
  associated event keywords.

  See also: NS2.0:DEFINE-SCRIPT

  Here is an example of how to use this macro.

  (with-document-preamble (:stream stream)
    (ns2.0:declare-script script stream))
  (with-document-body (:stream stream)
    (with-event-handlers
      (events
        (:java-script
          :new-value (ns2.0:event-caller script 10 20)))
      (accept-input 'string+ query-name
                    :events events
                    :stream stream)))"
  (multiple-value-bind (function-names function-bodies)
      (%collect-input-type-events clauses)
    `(flet ,function-bodies
       (declare (dynamic-extent ,@function-names))
       (let ((,variable (list ,@function-names)))
         (declare (dynamic-extent ,@function-names))
         ,@body))))

;; interpreted version
;(define-macro with-event-handlers ((variable &rest clauses) &body body)
;  `(let ((,variable (list ,.(loop for (language . clause) in clauses
;                                 nconc (loop for (event command) on clause by #'cddr
;                                             do (%get-input-type-event-tag event language)
;                                             collect `(list '(,event . ,language) ,command))))))
;     (declare (dynamic-extent ,variable))
;     ,@body))

(define-macro write-standard-input-type-args ((stream query-name input-type args &key (bind-default t)
                                                      (handle-events t))
                                              &body body)
  "Writes standard aurgments to input types."
  `(destructuring-bind (&key ,@(when bind-default '(default)) disabled error
                             ,@(when handle-events '(events)) &allow-other-keys) ,args
     (with-slots (type-arg) ,input-type
       (%write-command-key-arg ,stream "TYPE" type-arg)
       (cond (,query-name
              (verify-query-name ,query-name)
              (%write-command-key-arg ,stream "NAME" ,query-name))
             (t (error "No QUERY-NAME provided for input type.")))
       (cond-every
         ,.(when bind-default
             `((default
                 (check-value-type ,input-type default)
                 (%write-command-key-arg ,stream "VALUE" default))))
         (disabled
           (%write-command-key-arg ,stream "DISABLED"))
         (error
           (%write-command-key-arg ,stream "ERROR"))
         ,@(when handle-events
             `((events
                 (dolist (event events)
                   (%write-input-type-event-arg stream event))))))
       ,@body)))

(defmethod standard-input-type-args-writer ((input-type input-type) query-name args)
  (with-slots (type-arg) input-type
    (verify-query-name query-name)
    (destructuring-bind (&key disabled error events &allow-other-keys) args
      (flet ((write-standard-args (stream)
               (%write-command-key-arg stream "TYPE" type-arg)
               (%write-command-key-arg stream "NAME" query-name)
               (cond-every
                 (disabled
                   (%write-command-key-arg stream "DISABLED"))
                 (error
                   (%write-command-key-arg stream "ERROR"))
                 (events
                   (dolist (event events)
                     (%write-input-type-event-arg stream event))))))
        #'write-standard-args))))

(define-generic accept-input (input-type query-name &rest args &key stream &allow-other-keys)
  (declare (#+Genera scl:arglist
            #-Genera arglist
            input-type query-name &key default max-length size stream
            align choices columns compact display-string events image-url linebreaks preamble rows sequence-p
            ))
  (:documentation
    "The primary interface for accepting input from users via the WWW fill-out form
facility.

That really means presenting the choices to the user.  The returns are
accepted by the response function to an exported URL. See EXPORT-URL and the
macro BIND-QUERY-VALUES used within response functions.

Required Arguments: INPUT-TYPE -- the symbol denoting an defined input type.

                    QUERY-NAME -- a unique name for the value returned by form
                    submission.

Keyword Arguments: :STREAM -- the stream on which to write raw HTML generated
                    by the input-type.
                   
                    :DEFAULT -- the default value to use in lieu of user input 
                    (available for most input types).

                    :EVENTS -- If a browser supports client-side events on
                    form input types, they can be passed in via this argument.
                    Use the macro WITH-EVENT-HANDLERS to establish the
                    binding for input events Note that form submission events
                    are supplied via the macro WITH-FILLOUT-FORM.

Additional Arguments are defined on a per input-type basis.

Input Type          Keyword Arguments

CHECKBOX         -- Allows the user to select a sequence of values by checking
                    boxes.

                    [Required] :CHOICES is a list of choice values or an alist
                    of (choice-string . choice), or null.   Except in the
                    ALIST version, each choice string is the position of the
                    choice in the list.

                    [Optional] :DEFAULT is a list of values that are checked
                    by default. These are compared to the values in CHOICES
                    with EQUAL.
                                 
                    [Optional] (:LINEBREAKS T) causes choices to appear on new
                    lines within an enumeration environment. When null, they
                    appear on a single line.
                        
                    [Optional] (:ENUMERATION :PLAIN) is any valid argument to
                    WITH-ENUMERATION.
                                
                    [Optional] :COMPACT is the same as its counterpart in
                    WITH-ENUMERATION.

FILE                Accepts file data in form submission. The user is prompted
                    for a file on the client machine. The file data is then 
                    transmitted as the value of the query in a mime multipart 
                    form value return, which requires special handling by user code.

                    [Optional] :DEFAULT should be a string or null.

                    [Optional] (:SIZE 72) is an integer specifying the size to
                    make the visible input field.

                    [Optional] (:MAX-LENGTH 240) is an integer specifying the
                    maximum size which the input field may reach but not top
                    exceed 1024 characters.

HIDDEN           -- Allows data passed through forms and back to the server.
                    These are known as hidden fields because they are invisible to
                    the user.

                    [Required] :DEFAULT should be a string or number.

                    HTTP:WRITE-TO-ARMOR-PLATED-STRING and HTTP:READ-FROM-ARMOR-PLATED-FORM
                    can help avoid damage in transit.

IMAGE            -- A likely obsolete method way to add a submit button to a
                    form that uses a image and returns image coordinates.

                    In the form returns, client returns two query values whose
                    names are constructed by appending .X and .Y to
                    QUERY-NAME. The bindings for X and Y are in pixels using
                    the upper left corner of the image as the origin.  The
                    effect is just like the SUBMIT input type in that a user
                    mouse click on this field causes the form to be sent
                    immediately.

                    [Required] :IMAGE-URL is an image URL.
                      
                    [Optional] (:ALIGN :TOP) can be any of :TOP, :MIDDLE,
                    :BOTTOM.

MULTI-LINE-TEXT  -- Allows the user to type in or edit multi line text in a
                    scrollable input box.

                    [Optional] :DEFAULT should be a string, null, or a function
                    which is called on STREAM.

                    [Optional] (:COLUMNS 72.) is an integer specifying the
                    width of the visible input field.

                    [Optional] (:ROWS 5.) is an integer specifying the height
                    of the visible input field.

PASSWORD         -- Just like the STRING input type except the client does not
                    display the input.

PREAMBLE         -- A function or string that is written before the input-type.
                    If PREAMBLE is a function, it is called with (INPUT-TYPE STREAM ARGS)
                    A typical use is to emit a JavaScript invoked by an input type.

RADIO-BUTTON     -- Allows the user to select a single value by checking a box.

                    [Required] :CHOICES is a list of choice values or an alist
                    of (choice-string . choice), or null. Except in the ALIST
                    version, each choice string is the position of the choice
                    in the list.

                    [Optional] :DEFAULT is a single value which is the
                    default. It is compared to the values in CHOICES with
                    EQUAL.
                    
                    [Optional] (:LINEBREAKS T) causes choices to appear on new
                    lines within an enumeration environment. When 
                    null, they appear on a single line.
                                 
                    [Optional] (:ENUMERATION :PLAIN) is any valid argument to
                    WITH-ENUMERATION.
                                
                    [Optional] :COMPACT is the same as its counterpart in
                    WITH-ENUMERATION.

RESET-BUTTON     -- Reset the values of the form to the default when pressed by the user.

                    [Optional] (:DISPLAY-STRING \"Reset\") is the label
                    displayed by the client.

SELECT-CHOICES   -- Allows the user to select either a single or multiple choices.

                    [Required] :CHOICES is a list of choice values or an alist
                    of (<choice-string> :VALUE <choice>), or null.   Except in the
                    ALIST version, each choice string is the position of the
                    choice in the list.

                    [Optional] :DEFAULT is either a single value or a list of
                    values, which are compared to choices with EQUALP.

                    [Optional] :SEQUENCE-P specifies whether a sequences of
                    values should be returned.

                    [Optional] :SIZE is an integer specifying the visible
                    number of rows visible in a scrolling inset choice box.
                    When :SEQUENCE-P is null, you can specify SIZE to be
                    :PULL-DOWN-MENU to have the choices rendered as
                     a pull down menu. When size is unspecified, it is
                    defaulted to a heuristic value.

SUBMIT-BUTTON    -- Submits the form when pressed by the user.

                    [Optional] (:DISPLAY-STRING \"Submit\") is the label
                    displayed by the client.

STRING           -- Allow the user to type in a string on a single line.

                    [Optional] :DEFAULT should be a string or null.

                    [Optional] (:SIZE 72) is an integer specifying the size to
                    make the visible input field.

                    [Optional] (:MAX-LENGTH 240) is an integer specifying the
                    maximum size which the input field may reach but not top
                    exceed 1024 characters."))

;;(name type size maxlength value checked disabled error src align)

(define get-input-type (name &optional (error-p t))
  (typecase name
    (symbol
      (cond ((gethash name *input-types*))
            (error-p (error "The input type, ~S, is undefined." name))
            (t nil)))
    (input-type name)))

(defmethod accept-input ((input-type symbol) query-name &rest args &key &allow-other-keys)
  (apply #'accept-input (get-input-type input-type) query-name args))

(declaim (inline %accept-input-write-preamble))

(defun %accept-input-write-preamble (preamble input-type stream args)
  (when preamble
    (etypecase preamble
      (function (funcall preamble input-type stream args))
      (string (write-string preamble stream)))))

;;;------------------------------------------------------------------- 
;;;
;;; DEFINE INPUT TYPES
;;;

(define-input-type
  hidden
  :type-arg "hidden"
  :lisp-type '(or string number))

(defmethod accept-input ((hidden hidden) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (unless (getf args :default)
    (error "No default value provided for a HIDDEN input type."))
  (%issue-command ("INPUT" stream)
    (write-standard-input-type-args (stream query-name hidden args :bind-default t))))

(define-input-type
  string+
  :type-arg "text"
  :lisp-type 'string
  :parameters ((default-size :initform 72 :reader default-size)
               (default-max-size :initform 240 :reader default-max-size))
  :nicknames (string))

(defmethod accept-input ((string+ string+) query-name &rest args &key preamble (stream *output-stream*) &allow-other-keys)
  (with-slots (default-size default-max-size) string+
    (%accept-input-write-preamble preamble string+ stream args)
    (%issue-command ("INPUT" stream)
      (write-standard-input-type-args (stream query-name string+ args :bind-default t)
        (destructuring-bind (&key size max-length &allow-other-keys) args
          (let ((local-size (or size default-size))
                (max (or max-length default-max-size)))
            (cond-every
              (local-size
                (%write-command-key-arg stream "SIZE" local-size))
              (max
                (unless (< max 1024.)
                  (error "String fields cannot exceed 1024 characters in HMTL 2.0"))
                (%write-command-key-arg stream "MAXLENGTH" max t)))))))))

(define-input-type
  password
  :type-arg "PASSWORD"
  :superclass string+
  :lisp-type 'string
  :parameters ((default-size :initform 10)))

(define-input-type
  multi-line-text
  :type-arg "TEXTAREA"
  :lisp-type 'string)

(defmethod accept-input ((multi-line-text multi-line-text) query-name &rest args &key (stream *output-stream*) )
  (with-slots (type-arg) multi-line-text
    (destructuring-bind (&key (rows 5.) (columns 72.) default events &allow-other-keys) args
      (check-type rows integer)
      (check-type columns integer)
      (%with-environment (type-arg :stream stream :fresh-line t)
                         (progn
                           (%write-command-key-arg stream "NAME" query-name)
                           (%write-command-key-arg stream "ROWS" rows t)
                           (%write-command-key-arg stream "COLS" columns t)
                           (dolist (event events)
                             (%write-input-type-event-arg stream event)))
        (etypecase default
          (null)
          (string (write-string default stream))
          (function (funcall default stream)))))))

(define-input-type
  image
  :type-arg "image"
  :lisp-type 'null)

(defmethod accept-input ((image image) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (%issue-command ("INPUT" stream)
    (write-standard-input-type-args (stream query-name image args :bind-default nil)
      (destructuring-bind (&key image-url align &allow-other-keys) args
        (unless (member align *image-alignment-values*)
          (error "Unknown image alignment, ~S, was specified." align))
        (%write-command-key-arg "SRC" (coerce-url-string image-url))
        (when align
          (%write-command-key-arg "ALIGN" align))))))

;;;------------------------------------------------------------------- 
;;;
;;; CHOICES
;;;

(define-input-type
  radio-button
  :type-arg "radio"
  :lisp-type 'atom)

;; enumerates from 0 to n
(defmethod accept-input ((radio-button radio-button) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (macrolet ((with-decoded-choice ((choice default values-provided-p count) &body body)
               `(multiple-value-bind (value choice-string check-p)
                    (if ,values-provided-p 
                        (values (cdr ,choice) (car ,choice) (equal (cdr ,choice) ,default))
                        (values (princ-to-string ,count) ,choice (equal ,choice ,default)))
                  ,@body)))
    (flet ((write-element (stream choice-string value check-p standard-args-writer)
             (%issue-command ("INPUT" stream :fresh-line t)
               (funcall standard-args-writer stream)
               (%write-command-key-arg stream "VALUE" value)
               (when check-p
                 (%write-command-key-arg stream "CHECKED")))
             (write-string choice-string stream)))
      (declare (inline write-element))
      (let ((standard-args-writer (standard-input-type-args-writer radio-button query-name args)))
        (declare (dynamic-extent standard-args-writer))
        (destructuring-bind (&key choices (linebreaks t) (enumeration :plain) compact default selected-choice
                                  &allow-other-keys) args
          (let ((values-provided-p (consp (first choices)))
                (default-value (or default selected-choice)))   ;selected-choice is obsolete  2/26/95 -- JCMa.
            (if linebreaks
                (with-enumeration (stream enumeration :compact compact)
                  (loop for choice in choices
                        for count upfrom 0
                        do (with-decoded-choice
                             (choice default-value values-provided-p count)
                             (enumerating-item (stream)
                               (write-element stream choice-string value check-p standard-args-writer)
                               (break-line :stream stream)))))
                (loop for choice in choices
                      for count upfrom 0
                      do (with-decoded-choice
                           (choice default-value values-provided-p count)
                           (write-element stream choice-string value check-p standard-args-writer))))))))))

(define-input-type
  checkbox
  :type-arg "checkbox"
  :lisp-type '(or atom list))

;; enumerates from 0 to (1- n)
(defmethod accept-input ((checkbox checkbox) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (macrolet ((with-decoded-choice ((choice default values-provided-p count) &body body)
               `(multiple-value-bind (value choice-string check-p)
                    (if ,values-provided-p 
                        (values (cdr ,choice) (car ,choice) (member (cdr ,choice) ,default :test #'equal))
                        (values (princ-to-string ,count) ,choice (member ,choice ,default :test #'equal)))
                  ,@body)))
    (flet ((write-element (stream choice-string value check-p standard-args-writer)
             (%issue-command ("INPUT" stream :fresh-line t)
               (funcall standard-args-writer stream)
               (%write-command-key-arg stream "VALUE" value)
               (when check-p
                 (%write-command-key-arg stream "CHECKED")))
             (write-string choice-string stream)))
      (declare (inline write-element))
      (let ((standard-args-writer (standard-input-type-args-writer checkbox query-name args)))
        (declare (dynamic-extent standard-args-writer))
        (destructuring-bind (&key choices (linebreaks t) (enumeration :plain) compact default selected-choice
                                  &allow-other-keys) args
          (let ((values-provided-p (consp (first choices)))
                (default-value (or default selected-choice)))   ;selected-choice is obsolete  2/26/95 -- JCMa.
            (if linebreaks
                (with-enumeration (stream enumeration :compact compact)
                  (loop for choice in choices
                        for count upfrom 0
                        do (with-decoded-choice
                             (choice default-value values-provided-p count)
                             (enumerating-item (stream)
                               (write-element stream choice-string value check-p standard-args-writer)
                               (break-line :stream stream)))))
                (loop for choice in choices
                      for count upfrom 0
                      do (with-decoded-choice
                           (choice default-value values-provided-p count)
                           (write-element stream choice-string value check-p standard-args-writer))))))))))

(define-input-type
  select-choices
  :type-arg "SELECT"
  :lisp-type 'cons)

(define-parameter *select-choices-max-default-size* 5
                  "Controls the maximum number of choices exposed by default in scollable select choice insets.")

;; if the size argument is present, the choices appear in a scrollable
;; indented choice window.
(defmethod accept-input ((select-choices select-choices) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (flet ((heuristic-size-default (choices)
           (let ((n (length choices))
                 (default-size *select-choices-max-default-size*))
             (declare (fixnum n default-size))
             (cond ((> n default-size)
                    (if (> n (* 5 default-size))
                        (* 2 default-size)
                        default-size))
                   ;;((< n 2) (error "Scrollable inset choices don't make sense for fewer than two choices."))
                   (t n))))
         (emit-item (stream choice value disabled default value-provided-p sequence-p events)
           (%issue-command ("OPTION" stream :fresh-line t)
             (cond-every
               ((and value-provided-p value)
                (%write-command-key-arg stream "VALUE" value))
               ((if sequence-p
                    (member value default :test #'equalp)
                    (equalp value default))
                (%write-command-key-arg stream "SELECTED"))
               (disabled
                 (%write-command-key-arg stream "DISABLED"))
               (events
                 (dolist (event events)
                   (%write-input-type-event-arg stream event)))))
           (write choice :escape nil :base 10. :stream stream)))
    (declare (dynamic-extent #'emit-item #'heuristic-size-default))
    (with-slots (type-arg) select-choices
      (destructuring-bind (&key choices default size (sequence-p (consp default)) events &allow-other-keys) args
        (cond ((null default))
              (sequence-p
               (unless (listp default)
                 (error "Default, ~S, is not a list, which is required when sequence-p is not null." default)))
              (t (unless (atom default)
                   (error "Default, ~S, is not an atom, which is required when sequence-p is null." default))))
        (%issue-command (type-arg stream :fresh-line t :trailing-line t)
          (if query-name
              (%write-command-key-arg stream "NAME" query-name)
              (error "No QUERY-NAME provided for input type."))
          (cond (sequence-p  ;; html spec says use "SEVERAL" but Mosaic uses "MULTIPLE"
                 (%write-command-key-arg stream "SIZE" (or size (heuristic-size-default choices)) t)
                 (%write-command-key-arg stream "MULTIPLE"))
                ((eq size :pull-down-menu)
                 (%write-command-key-arg stream "SIZE" 1 t))
                (size
                 ;; Scrollable inset choices don't make sense for fewer than two choices.
                 (when (< 1 size)
                   (%write-command-key-arg stream "SIZE" size t)))
                (t (%write-command-key-arg stream "SIZE" (heuristic-size-default choices) t)))
          (dolist (event events)
            (%write-input-type-event-arg stream event)))
        (loop for item in choices
              do (etypecase item
                   (cons 
                     (destructuring-bind (choice &key value disabled events &allow-other-keys) item
                       (emit-item stream choice value disabled default t sequence-p events)))
                   (atom (emit-item stream item item nil default nil sequence-p nil))))
        (issue-command type-arg stream t t))))) 

;;;------------------------------------------------------------------- 
;;;
;;; META CHOICES
;;;

(define-input-type
  reset-button
  :type-arg "reset"
  :lisp-type 'null)

(defmethod accept-input ((reset-button reset-button) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (destructuring-bind (&key display-string image-url &allow-other-keys) args
    (%issue-command ("INPUT" stream)
      (write-standard-input-type-args (stream query-name reset-button args :bind-default nil)
        (%write-command-key-arg stream "VALUE" (or display-string "Reset"))
        (when image-url
          (%write-command-key-arg stream "SRC" (coerce-url-string image-url)))))))

(define-input-type
  submit-button
  :type-arg "submit"
  :lisp-type 'null)

(defmethod accept-input ((submit-button submit-button) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (destructuring-bind (&key display-string image-url &allow-other-keys) args
    (%issue-command ("INPUT" stream)
      (write-standard-input-type-args (stream query-name submit-button args :bind-default nil)
        (%write-command-key-arg stream "VALUE" (or display-string "Submit"))
        (when image-url
          (%write-command-key-arg stream "SRC" (coerce-url-string image-url)))))))


;;;------------------------------------------------------------------- 
;;;
;;; BUTTON
;;;

(define-input-type
  client-side-button
  :type-arg "button"
  :lisp-type 'null)

(defmethod accept-input ((client-side-button client-side-button) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (destructuring-bind (&key display-string image-url &allow-other-keys) args
    (%issue-command ("INPUT" stream)
      (write-standard-input-type-args (stream query-name client-side-button args :bind-default nil)
        (%write-command-key-arg stream "VALUE" (or display-string "Submit"))
        (when image-url
          (%write-command-key-arg stream "SRC" (coerce-url-string image-url)))))))

;;;------------------------------------------------------------------- 
;;;
;;; FILE UPLOAD
;;;

(define-input-type
  file
  :superclass string+
  :type-arg "FILE"
  :lisp-type 'string)

(defmethod accept-input ((file file) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (with-slots (default-size default-max-size) file
    (destructuring-bind (&key size max-length content-type &allow-other-keys) args
      (%issue-command ("INPUT" stream)
        (html2::write-standard-input-type-args (stream query-name file args :bind-default t)
 
          (let ((local-size (or size default-size))
                (max (or max-length default-max-size)))
            (cond-every
              (local-size
                (%write-command-key-arg stream "SIZE" local-size))
              (max
                (unless (< max 1024.)
                  (error "String fields cannot exceed 1024 characters in HMTL 2.0"))
                (%write-command-key-arg stream "MAXLENGTH" max t))
              (content-type
                (fast-format stream " ACCEPT=\"")
                (etypecase content-type
                  (cons
                    (etypecase (car content-type)
                      (keyword (http::print-mime-content-type-header content-type stream))
                      (cons (http::print-mime-content-type-sequence-header content-type stream))))
                  (keyword
                    (http::print-mime-content-type-header (http::%mime-content-type-spec content-type) stream))
                  (string
                    (write-string content-type stream)))
                (write-char #\" stream)))))))))

;;;------------------------------------------------------------------- 
;;;
;;; FILL-OUT FORMS
;;;

(defvar *open-form* nil)

(defun write-form-command-args (stream action value &optional (encoding-type '(:application :x-www-form-urlencoded))
                                       name events)
  (let ((encoding-type-string (http::write-mime-content-type encoding-type)))
    (declare (dynamic-extent encoding-type-string))
    (when name
      (%write-command-key-arg stream "NAME" name))
    (ecase action
      (:post
        (%write-command-key-arg stream "ACTION" (coerce-url-string value))
        (%write-command-key-arg stream "METHOD" "POST")
        (%write-command-key-arg stream "ENCTYPE" encoding-type-string)
        (dolist (event events)
          (%write-input-type-event-arg stream event)))
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

(define-macro with-fillout-form ((action target &key name (stream '*output-stream*)
                                         (encoding-type ''(:application :www-url-form-encoded))
                                         events) &body body)
  "Establishes an fillout-form environment.  
ACTION is either :POST, :MAIL, or :GET, or :NONE.
NAME is a name identifying the form element.
TARGET is the URL to which the form values are returned.
If ACTION is :NONE, TARGET can be NIL.
EVENTS is a list of client-side events processed when the form is submitted.

ENCODING-TYPE is MIME content type to use when return the form values to TARGET.
ENCODING-TYPE defaults to application/x-www-form-urlencoded.
See ACCEPT-INPUT for documentation on client-side events.             
:GET should only be used in exceptional circumstances as not only is
it considered obsolete but it also is limited to 1024 characters 
including the rest of the the Target URL."
  `(cond (*open-form*
          (error "HTML does not allow nesting of forms."))
         (t (%with-environment ("FORM" :fresh-line t :stream ,stream)
                               (write-form-command-args ,stream ,action ,target ,encoding-type
                                                        ,name ,events)
              (let ((*open-form* t))
                ,@body)))))

(define pack-query-name (query-name baggage &optional escape-p)
  "Makes a query name for ACCEPT-INPUT carrying baggage.
Pass the result in just like a normal query name."
  (let* ((stuff (etypecase baggage
                  (string baggage)
                  (number (write-to-string baggage :base 10))))
         (packed-query (concatenate 'string query-name "(" stuff ")")))
    (declare (dynamic-extent stuff))
    (if escape-p
        (http:string-escape-special-chars packed-query)
        packed-query)))

(define unpack-query-name (query-name &optional unescape-p)
  "Makes a query name for accept-input carrying baggage."
  (declare (type string query-name)
           (values query-name baggage))
  (let ((string (if unescape-p
                    (http:string-unescape-special-chars query-name)
                    query-name)))
    (declare (dynamic-extent string))
    (with-fast-array-references ((string string string))
      (let ((pos2 (1- (the fixnum (length string))))
            pos1)
        (if (and (eql (aref string pos2) #\))
                 (setq pos1 (http::char-position #\( string 0 pos2)))
            (values (subseq string 0 pos1) (subseq string (1+ (the fixnum pos1)) pos2))
            (values string nil))))))

;;;------------------------------------------------------------------- 
;;;
;;; ISO 8859/1 CHARACTERS
;;;

(define-variable *iso-readtable* nil
                 "Holds the readtable for 8859/1 characters. This array is 256x4.
The index is the character code positions are:

0 character object (optional)
1 name (optional case-sensitive string)
2 keyword
3 description string.")

(define-variable *iso-character-name-table* nil
                 "Table mapping character names to iso readtable index.")

(defun %define-iso-readtable (spec &aux (keyword-package (find-package :keyword)))
  (flet ((keyword-for-description (string)
           (loop with delimiters = '(#\, #\space #\) #\()
                 with l = (length string)
                 for s = (position-if-not #'(lambda (x)
                                              (member x delimiters))
                                          string :start (or e 0) :end l)
                 for e = (and s
                              (or (position-if #'(lambda (x)
                                                   (member x delimiters))
                                               string
                                               :start s :end l)
                                  l))
                 while (and s e)
                 when result
                   collect "-" into result
                 collect (string-upcase (subseq string s e))
                   into result
                 while (< e l)
                 finally (return (intern (apply #'concatenate 'string result) keyword-package)))))
    (declare (dynamic-extent #'keyword-for-description))
    (flet ((initialize-entry (array name-table idx char name description)
             (let ((keyword (keyword-for-description description)))
               (setf (aref array idx 0) char
                     (aref array idx 1) name
                     (aref array idx 2) keyword
                     (aref array idx 3) description)
               (setf (get keyword 'iso-character-index) idx
                     (gethash name name-table) idx))))
      (declare (inline initialize-entry))
      (loop with iso-readtable = (or *iso-readtable*
                                     (setq *iso-readtable* (make-array '(256 4) :initial-element nil)))
            with iso-name-table = (or (prog1 *iso-character-name-table*
                                             (and *iso-character-name-table* (clrhash *iso-character-name-table*)))
                                      (setq *iso-character-name-table* (make-hash-table :test #'equal :size 256.)))
            for (indices description) in spec
            do (etypecase indices
                 (integer
                   (initialize-entry iso-readtable iso-name-table indices nil nil description))
                 (cons
                   (destructuring-bind (idx &rest names) indices
                     (initialize-entry iso-readtable
                                       iso-name-table
                                       idx
                                       (find-if #'characterp names)
                                       (find-if #'stringp names)
                                       description))))
            finally (return iso-readtable)))))

(define-macro define-iso-readtable (&body entries)
  "Toplevel method for defining the ISO readtable."
  `(%define-iso-readtable ',entries))

(declaim (inline %unused-iso-character-code-p))

(defun %unused-iso-character-code-p (char-code)
  (or (<= 11 char-code 31)
      (<= 127 char-code 160)))

(declaim (inline valid-iso-char-code-p))

(defun valid-iso-character-code-p (char-code)
  (and (integerp char-code)
       (< 8 char-code 256)
       (not (%unused-iso-character-code-p char-code))))

(declaim (inline check-iso-character-code))

(define check-iso-character-code (char-code)
  (unless (valid-iso-character-code-p char-code)
    (error "CHAR-CODE, ~S, is not a valid char-code for an iso character." char-code)))

(declaim (inline iso-code-char))

(define iso-code-char (char-code &optional (iso-readtable *iso-readtable*))
  (or (aref iso-readtable char-code 0)
      (error "No lisp character object for ISO character code, ~D." char-code)))

(declaim (inline iso-character-name))

(define iso-character-name (char-code &optional (iso-readtable *iso-readtable*))
  (aref iso-readtable char-code 1))

(declaim (inline iso-character-keyword))

(define iso-character-keyword (char-code &optional (iso-readtable *iso-readtable*))
  (aref iso-readtable char-code 2))

(declaim (inline iso-character-description))

(define iso-character-description (char-code &optional (iso-readtable *iso-readtable*))
  (aref iso-readtable char-code 3))

(declaim (inline iso-character-code-from-character))

(define iso-character-code-from-character (char &optional (iso-readtable *iso-readtable*))
  (let ((char-code (char-code char)))
    (if (aref iso-readtable char-code 0)
        char-code
        (error "No ISO character code or the character, ~S." char))))

(define iso-character-code-from-name (name &optional error-p)
  (cond ((gethash name *iso-character-name-table*))
        (error-p (error "NAME, ~S, is not an ISO character name." name))
        (t nil)))

(define iso-character-code-from-keyword (keyword &optional error-p)
  (cond ((get keyword 'iso-character-index))
        (error-p (error "KEYWORD, ~S, is not an ISO character keyword." keyword))
        (t nil)))

(define write-iso-character (char-code &optional (stream *standard-output*))
  "Writes the ISO character denoted by CHAR-CODE on STREAM."
  (check-iso-character-code char-code)
  (with-fast-array-references ((array *iso-readtable* array))
    (let ((name (iso-character-name char-code array)))
      (write-char #\& stream)
      (cond (name
             (write-string name stream))
            (t (write-char #\# stream)
               (write char-code :stream stream :escape nil :base 10.)))
      (write-char #\; stream))))

(declaim (inline write-iso-character-for-name))

(define write-iso-character-for-name (name &optional (stream *standard-output*))
  "Writes the ISO character denoted by NAME on STREAM."
  (write-iso-character (iso-character-code-from-name name t) stream))

(declaim (inline write-iso-character-for-keyword))

(define write-iso-character-for-keyword (keyword &optional (stream *standard-output*))
  "Writes the ISO character denoted by KEYWORD on STREAM."
  (write-iso-character (iso-character-code-from-keyword keyword t) stream))

(define parse-iso-character (string &optional (start 0) (end (length string)))
  "Parses STRING as the textual representation of an ISO character.
It assumes that the character name or code are delimited by & and ;."
  (flet ((get-char-code (start end)
           (when (and start end)
             (let ((s1 (1+ (the fixnum start))))
               (case (aref string s1)
                 (#\#
                  (let ((code (parse-integer string :start s1 :end end :radix 10.)))
                    (when (valid-iso-character-code-p code)
                      code)))
                 (t (let ((key (subseq string s1 end)))
                      (declare (dynamic-extent key))
                      (iso-character-code-from-name key))))))))
    (declare (inline get-char-code))
    (or (get-char-code (http::char-position #\& string start end)
                       (http::char-position #\; string start end))
        (error "STRING, ~S, cannot be parsed into an ISO character code." string))))

(define apropos-iso-character (substring &optional (type :keyword))
  "Searches for the names of ISO characters containing substring.
Type can be:
              :NAME defined names for characters
              :keyword keyword standing for characters.
              :DESCRIPTION the description strings for characters."
  (flet ((ensure-string (thing)
	   (etypecase thing
	     (string thing)
	     (symbol (symbol-name thing)))))
    (declare (inline ensure-string))
    (with-fast-array-references ((array *iso-readtable* array))
      (loop with type-idx = (ecase type
			      (:keyword 2)
			      (:name 1)
			      (:description 3))
	    and current-item
	    for idx upfrom 8 to 255
	    when (and (not (%unused-iso-character-code-p idx))
		      (setq current-item (aref array idx type-idx))
		      (search substring (ensure-string current-item) :test #'char-equal))
	      collect current-item))))

(define describe-iso-character (char-code &optional (stream *standard-output*)
                                          (iso-readtable *iso-readtable*))
  (cond ((%unused-iso-character-code-p char-code)
         (format stream "~&~3,,D~50TUnused" char-code))
        (t (let ((char (aref iso-readtable char-code 0))
                 (name (aref iso-readtable char-code 1))
                 (keyword (aref iso-readtable char-code 2))
                 (description (aref iso-readtable char-code 3)))
             (format stream "~&~3,,D~:[~;~:*~5T~C~]~:[~;~:*~8T~A~]~20T~S~50T~A"
                     char-code char name keyword description)))))

(define describe-iso-readtable (&optional (stream *standard-output*))
  (loop with iso-readtable = *iso-readtable*
        for char-code upfrom 9 to 255
        do (describe-iso-character char-code stream iso-readtable)))

;;;------------------------------------------------------------------- 
;;;
;;; NUMERICAL CHARACTER REFERENCES FOR ISO LATIN 1 CHARACTER ENTITIES
;;;
;;;
;;; This list, sorted numerically, is derived from ISO 8859/1 8-bit
;;; single-byte coded graphic character set:

(define-iso-readtable
  ;; &#00; - &#08;       Unused
  ((09 #\tab)             "Horizontal tab")
  ((10 #\Linefeed)        "Line feed")
   
  ;; &#11; - &#31;       Unused
   
  ((32 #\space)           "Space")
  ((33 #\!)               "Exclamation mark")
  ((34 #\")               "Quotation mark")
  ((35 #\#)               "Number sign")
  ((36 #\$)               "Dollar sign")
  ((37 #\%)               "Percent sign")
  ((38 #\&)               "Ampersand")
  ((39 #\')               "Apostrophe")
  ((40 #\()               "Left parenthesis")
  ((41 #\))               "Right parenthesis")
  ((42 #\*)               "Asterisk")
  ((43 #\+)               "Plus sign")
  ((44 #\,)               "Comma")
  ((45 #\-)               "Hyphen")
  ((46 #\.)               "Period (fullstop)")
  ((47 #\/)               "Solidus (slash)")
   
  ;; &#48; - &#57;       Digits 0-9
   
  ((48 #\0)               "Digit 0")
  ((49 #\1)               "Digit 1")
  ((50 #\2)               "Digit 2")
  ((51 #\3)               "Digit 3")
  ((52 #\4)               "Digit 4")
  ((53 #\5)               "Digit 5")
  ((54 #\6)               "Digit 6")
  ((55 #\7)               "Digit 7")
  ((56 #\8)               "Digit 8")
  ((57 #\9)               "Digit 9")
   
  ((58 #\:)               "Colon")
  ((59 #\;)               "Semi-colon")
  ((60 #\<)               "Less than")
  ((61 #\=)               "Equals aign")
  ((62 #\>)               "Greater than")
  ((63 #\?)               "Question mark")
  ((64 #\@)               "Commercial at")
   
  ;; &#65; - &#90;       Letters A-Z
   
  ((65 #\A)               "Letter A")
  ((66 #\B)               "Letter B")
  ((67 #\C)               "Letter C")
  ((68 #\D)               "Letter D")
  ((69 #\E)               "Letter E")
  ((70 #\F)               "Letter F")
  ((71 #\G)               "Letter G")
  ((72 #\H)               "Letter H")
  ((73 #\I)               "Letter I")
  ((74 #\J)               "Letter J")
  ((75 #\K)               "Letter K")
  ((76 #\L)               "Letter L")
  ((77 #\M)               "Letter M")
  ((78 #\N)               "Letter N")
  ((79 #\O)               "Letter O")
  ((80 #\P)               "Letter P")
  ((81 #\Q)               "Letter Q")
  ((82 #\R)               "Letter R")
  ((83 #\S)               "Letter S")
  ((84 #\T)               "Letter T")
  ((85 #\U)               "Letter U")
  ((86 #\V)               "Letter V")
  ((87 #\W)               "Letter W")
  ((88 #\X)               "Letter X")
  ((89 #\Y)               "Letter Y")
  ((90 #\Z)               "Letter Z")
   
  ((91 #\[)               "Left square bracket")
  ((92 #\\)               "Reverse solidus (backslash)")
  ((93 #\])               "Right square bracket")
  ((94 #\^)               "Hat")                ;missing from specification? added
  ((95 #\_)               "Horizontal bar")
  ((96 #\`)               "Acute accent")
   
  ;; &#97; - &#122;      Letters a-z
   
  ((97 #\a)               "Letter a")
  ((98 #\b)               "Letter b")
  ((99 #\c)               "Letter c")
  ((100 #\d)              "Letter d")
  ((101 #\e)              "Letter e")
  ((102 #\f)              "Letter f")
  ((103 #\g)              "Letter g")
  ((104 #\h)              "Letter h")
  ((105 #\i)              "Letter i")
  ((106 #\j)              "Letter j")
  ((107 #\k)              "Letter k")
  ((108 #\l)              "Letter l")
  ((109 #\m)              "Letter m")
  ((110 #\n)              "Letter n")
  ((111 #\o)              "Letter o")
  ((112 #\p)              "Letter p")
  ((113 #\q)              "Letter q")
  ((114 #\r)              "Letter r")
  ((115 #\s)              "Letter s")
  ((116 #\t)              "Letter t")
  ((117 #\u)              "Letter u")
  ((118 #\v)              "Letter v")
  ((119 #\w)              "Letter w")
  ((120 #\x)              "Letter x")
  ((121 #\y)              "Letter y")
  ((122 #\z)              "Letter z")
   
  ((123 #\{)              "Left curly brace")
  ((124 #\|)              "Vertical bar")
  ((125 #\})              "Right curly brace")
  ((126 #\~)              "Tilde")
   
  ;;         &#127; - &#160;     Unused
   
  (161              "Inverted exclamation")
  (162              "Cent sign")
  (163              "Pound sterling")
  (164              "General currency sign")
  (165              "Yen sign")
  (166              "Broken vertical bar")
  (167              "Section sign")
  (168              "Umlaut (dieresis)")
  (169              "Copyright")
  (170              "Feminine ordinal")
  (171              "Left angle quote, guillemotleft")
  (172              "Not sign")
  (173              "Soft hyphen")
  (174              "Registered trademark")
  (175              "Macron accent")
  (176              "Degree sign")
  (177              "Plus or minus")
  (178              "Superscript two")
  (179              "Superscript three")
  (180              "Acute accent")
  (181              "Micro sign")
  (182              "Paragraph sign")
  (183              "Middle dot")
  (184              "Cedilla")
  (185              "Superscript one")
  (186              "Masculine ordinal")
  (187              "Right angle quote, guillemotright")
  (188              "Fraction one-fourth")
  (189              "Fraction one-half")
  (190              "Fraction three-fourths")
  (191              "Inverted question mark")
   
  ((192 "Agrave")   "Capital A, grave accent")
  ((193 "Aacute")   "Capital A, acute accent")
  ((194 "Acirc")    "Capital A, circumflex accent")
  ((195 "Atilde")   "Capital A, tilde")
  ((196 "Auml")     "Capital A, dieresis or umlaut mark")
  ((197 "Aring")    "Capital A, ring")
  ((198 "AElig")    "Capital AE diphthong (ligature)")
  ((199 "Ccedil")   "Capital C, cedilla")
  ((200 "Egrave")   "Capital E, grave accent")
  ((201 "Eacute")   "Capital E, acute accent")
  ((202 "Ecirc")    "Capital E, circumflex accent")
  ((203 "Euml")     "Capital E, dieresis or umlaut mark")
  ((204 "Igrave")   "Capital I, grave accent")
  ((205 "Iacute")   "Capital I, acute accent")
  ((206 "Icirc")    "Capital I, circumflex accent")
  ((207 "Iuml")     "Capital I, dieresis or umlaut mark")
  ((208 "ETH")      "Capital Eth, Icelandic")
  ((209 "Ntilde")   "Capital N, tilde")
  ((210 "Ograve")   "Capital O, grave accent")
  ((211 "Oacute")   "Capital O, acute accent")
  ((212 "Ocirc")    "Capital O, circumflex accent")
  ((213 "Otilde")   "Capital O, tilde")
  ((214 "Ouml")     "Capital O, dieresis or umlaut mark")
   
  (215              "Multiple Sign")
   
  ((216 "Oslash")   "Capital O, slash")
  ((217 "Ugrave")   "Capital U, grave accent")
  ((218 "Uacute")   "Capital U, acute accent")
  ((219 "Ucirc")    "Capital U, circumflex accent")
  ((220 "Uuml")     "Capital U, dieresis or umlaut mark")
  ((221 "Yacute")   "Capital Y, acute accent")
  ((222 "THORN")    "Capital THORN, Icelandic")
  ((223 "szlig")    "Small sharp s, German(sz ligature)")
  ((224 "agrave")   "Small a, grave accent")
  ((225 "aacute")   "Small a, acute accent")
  ((226 "acirc")    "Small a, circumflex accent")
  ((227 "atilde")   "Small a, tilde")
  ((228 "auml")     "Small a, dieresis or umlaut mark")
  ((229 "aring")    "Small a, ring")
  ((230 "aelig")    "Small ae diphthong (ligature)")
  ((231 "ccedil")   "Small c, cedilla")
  ((232 "egrave")   "Small e, grave accent")
  ((233 "eacute")   "Small e, acute accent")
  ((234 "ecirc")    "Small e, circumflex accent")
  ((235 "euml")     "Small e, dieresis or umlaut mark")
  ((236 "igrave")   "Small i, grave accent")
  ((237 "iacute")   "Small i, acute accent")
  ((238 "icirc")    "Small i, circumflex accent")
  ((239 "iuml")     "Small i, dieresis or umlaut mark")
  ((240 "eth")      "Small eth, Icelandic")
  ((241 "ntilde")   "Small n, tilde")
  ((242 "ograve")   "Small o, grave accent")
  ((243 "oacute")   "Small o, acute accent")
  ((244 "ocirc")    "Small o, circumflex accent")
  ((245 "otilde")   "Small o, tilde")
  ((246 "ouml")     "Small o, dieresis or umlaut mark")
   
  (247              "Division Sign")
   
  ((248 "oslash")   "Small o, slash")
  ((249 "ugrave")   "Small u, grave accent")
  ((250 "uacute")   "Small u, acute accent")
  ((251 "ucirc")    "Small u, circumflex accent")
  ((252 "uuml")     "Small u, dieresis or umlaut mark")
  ((253 "yacute")   "Small y, acute accent")
  ((254 "thorn")    "Small thorn, Icelandic")
  ((255 "yuml")     "Small y, dieresis or umlaut mark"))
