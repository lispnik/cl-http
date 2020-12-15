;;; -*- Syntax: Ansi-Common-Lisp; Package: CL-USER; Base: 10; Mode: lisp -*-

(in-package :ccl)

#| #######################################################################

 Automatically get Netscape (or other browser) to display files edited in MCL
 and vice versa.

Copyright (c) 1995, 1996, 1997 Michael Travers 

Permission is given to use and modify this code
as long as the copyright notice is preserved.

Send questions, comments, and fixes to mt@media.mit.edu.

-------------------------------------------------------------------------

This file makes it easier to edit HTML files in MCL.  It augments
Bill St. Clair's HTML-Editor.lisp by providing the following 
additional functions:

- When you write out a file in HTML mode, Netscape will display it automatically.
  Optionally, a tag is inserted to the file so that the browser can go to
  the area of the file corresponding to the Fred cursor.
- If you are in Netscape browsing a local file (that is, a URL that begins
  with "file:"), and you return to MCL via pressing Command, Option, and
  Control, MCL will open the file for editing.  

-------------------------------------------------------------------------
Todo:
-  edit point thing doesn't really work, since it can get inserted in
bad places (ie, in the middle of an existing anchor).

History:
7/14/96 11:32  Seperated out (see netscape-control)
2/9/97 18:27   generalized for other browsers (see browser-control)
2/15/97 20:44  Made variables to control functions (edit-point is off by default)
-------------------------------------------------------------------------

####################################################################### |#
(require :html-editor)
(require :browser-control)
(require :processes)

(defvar *show-html-in-browser* t "T to tell browser to display HTML files")
(defvar *insert-edit-point* nil "T to insert edit point anchor into HTML files")       

(defmethod view-html-file ((w fred-mixin) &optional at-edit-point)
  (browser-open-file (window-filename w) :window 1 :anchor (if at-edit-point "editpoint" nil)))

;;; called when MCL is selected by magic keyboard hack
(defun maybe-read-html-file ()
  (let ((filename (browser-file-name)))
    (when (stringp filename)
      (ignore-errors (ed filename)))))

;;; patch to bill's processes.lisp, to deal with new MCL3 lossage
;;; and do special hack from URL
(defun select-mcl-eventhook (&rest ignore)
  (declare (ignore ignore))
  (unless *foreground*
    (let ((*current-event* nil)
          #+CCL-3 (*control-key-mapping* nil)   ; deal with MCL3 modifier-key lossage
          )
      (makunbound '*current-event*)
      (when (and (control-key-p) (option-key-p) (command-key-p))
        (maybe-read-html-file)          ; read the file?
        (select-mcl))))
  nil)

(advise window-save-file 
        (let* ((window (car arglist))
               (html-view? (and *show-html-in-browser*
                                (typep window 'fred-mixin)
                                (view-get window :html-mode-set))))
          (when (and html-view? *insert-edit-point*)
            (insert-edit-point window))
          (prog1
             (:do-it)
             (when html-view?
               (view-html-file window *insert-edit-point*))))
        :when :around
        :name :html-editor)

(defvar *edit-point-string* "<a name=editpoint>")

; +++ might check for presence of <title> command
(defmethod insert-edit-point ((w fred-mixin))
  (let* ((buffer (fred-buffer w))
         (marker-pos (buffer-forward-search buffer *edit-point-string* 0))
         #+CCL-3 (line-begin-pos (buffer-line-start buffer)))
    (when marker-pos
      (buffer-delete buffer (- marker-pos (length *edit-point-string*)) marker-pos))
    #-CCL-3
    (set-mark buffer (buffer-line-start buffer))
    (buffer-insert buffer
                   *edit-point-string*
                   #+CCL-3 line-begin-pos 
                   #+CCL-3 html-editor::*command-font-spec*)))  

(export '(*show-html-in-browser*
          *insert-edit-point*))
