 ;;;   -*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-

;;;
;;; (c) Copyright  1994-96, John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; KEEPING TRACK OF PROCEDURES AND VARIABLES
;;;

(in-package :www-utils)

#+LispWorks
(defun note-editor-definition-parser (definer deftype)
  (let ((parsing-function (get deftype 'editor::section-parsing-function)))
    (when parsing-function
      (setf (get definer 'editor::section-parsing-function) parsing-function))))

#+Franz-Inc
(eval-when (compile eval load)
  (defmacro clean-macro-firstarg (arglist body)
    `(if (and (consp ,arglist)
              (null (first ,arglist)))
         (cons (cons '(&optional #1=#:ignore) (rest ,arglist))
               (if (and (consp (first ,body))
                        (eq (caar ,body) 'declare))
                   (cons (append (first ,body) '((ignore #1#)))
                         (rest ,body))
                   (cons '(declare (ignore #1#)) ,body)))
         (cons ,arglist ,body))))

(defmacro defshowable-procedure-type (type &key 
                                      definition-function
                                      doc-string
                                      other-btrees
                                      zmacs-meta-x-command-string
                                      zmacs-key-binding
                                      (zmacs-prompt-string "Apropos:")
                                      b-tree-var
                                      (b-tree-var-type 'defvar)
                                      doc-and-arglist-function
                                      (function-parent 'defshowable-procedure-type))
  #+Showproc
  `(spt:defshowable-procedure-type
     ,type
     :definition-function ,definition-function
     :doc-string ,doc-string
     :other-btrees ,other-btrees
     :zmacs-meta-x-command-string ,zmacs-meta-x-command-string
     :zmacs-key-binding ,zmacs-key-binding
     :zmacs-prompt-string ,zmacs-prompt-string
     :b-tree-var ,b-tree-var
     :b-tree-var-type ,b-tree-var-type
     :doc-and-arglist-function ,doc-and-arglist-function
     :function-parent ,function-parent)
  #-Showproc
  (declare (ignore doc-string other-btrees zmacs-meta-x-command-string
                   b-tree-var b-tree-var-type doc-and-arglist-function 
                   zmacs-prompt-string zmacs-key-binding function-parent))
  #-Showproc
  `(progn
     #+LispWorks
     (note-editor-definition-parser ',type ',definition-function)
     (defmacro ,type (name arglist &body body)
       #-Franz-Inc
       `(,',definition-function ,name ,arglist . ,body)
       #+Franz-Inc
       `(,',definition-function ,name ,@(clean-macro-firstarg arglist body)))))

;;;------------------------------------------------------------------- 
;;;
;;; DEFINING THE VARIOUS DEFINITION FORMS 
;;;

#+Showproc
(eval-when (compile eval load)
  (spt:define-procedure-defining-procedure
    :name 'defun
    :pretty-description "a function")
  (spt:define-procedure-defining-procedure
    :name 'defgeneric
    :pretty-description "a CLOS generic function"))

(defshowable-procedure-type
  define
  :definition-function defun
  :doc-string "Defines a procedure for the WWW Lisp System."
  :zmacs-meta-x-command-string "Show WWW Procedures"
  :b-tree-var *www-procedures*
  :b-tree-var-type defvar
  #+(or Genera MCL) :zmacs-key-binding #+Genera #\hyper-shift-w #+MCL #\W
  :zmacs-prompt-string "Apropos (WWW Procedures)")

(defshowable-procedure-type
  define-macro
  :definition-function defmacro
  :doc-string "Defines a macro for the WWW Lisp System."
  :other-btrees '(*www-procedures*))

(defshowable-procedure-type
  define-generic
  :definition-function defgeneric
  :doc-string "Defines a CLOS generic function for the WWW Lisp System."
  :other-btrees '(*www-procedures*))

(defshowable-procedure-type
  define-variable
  :definition-function defvar
  :doc-string "Defines a variable for the Communications Linker System."
  :other-btrees '(*www-procedures*))

(defshowable-procedure-type
  define-constant
  :definition-function defconstant
  :doc-string "Defines a constant in the Communications Linker System."
  :other-btrees '(*www-procedures*))

(defshowable-procedure-type
  define-parameter
  :definition-function defparameter
  :doc-string "Defines a parameter in the Communications Linker System."
  :other-btrees '(*www-procedures*))

(defmacro index-definitions (&rest definition-names)
  "Indexes DEFINITION-NAMES for access via the showable procedures facility."
  #+Showproc`(mapc #+Showproc #'index-the-define ',definition-names)
  #-Showproc`',definition-names)

;; index the definition functions.
(index-definitions define define-constant define-generic define-macro
                   define-parameter define-variable index-definitions)

