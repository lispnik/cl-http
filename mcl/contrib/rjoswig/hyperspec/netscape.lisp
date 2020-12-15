(in-package :ccl)

#| #######################################################################

 Automatically get Netscape to display files edited in MCL (and vice versa)

 Author: Michael Travers (mt@media.mit.edu)

 Created: Wednesday May 24,1995 1:11pm

-------------------------------------------------------------------------

This package provides an interface to Netscape via AppleEvents. The routines
OPEN-URL and GET-URL allow you to force Netscape to open up a particular URL, or
to get the currently open URL from Netscape. 

When used with Bill St. Clair's HTML-Editor.lisp and processes.lisp,
the following additional functions are provided:

- When you write out a file in html mode, Netscape will display it.
- If you are in Netscape browsing a local file (that is, a URL that begins
  with "file:"), and you return to MCL via pressing Command, Option, and
  Control, MCL will open the file for editing.  

-------------------------------------------------------------------------
Todo:
- it would be nice if Netscape could somehow scroll to the current
position in the file.

History:
5/26/95 12:09: deal with #s in URLs

####################################################################### |#

;;; Interface to Netscape via AppleEvents

(require :appleevent-toolkit)

(defvar *netscape-control* t)
(defparameter *netscape-creator* :|MOSS|)

;;; Make Netscape open an URL
(defun open-url (url &optional no-cache)
   (when (find-process-with-signature *netscape-creator*)
       (with-aedescs (appleevent target reply)
           (create-signature-target target *netscape-creator*)
           (create-appleevent appleevent :|WWW!| :|OURL| target)
           (ae-put-parameter-char appleevent #$keyDirectObject url)
           (when no-cache
               (ae-put-parameter-longinteger appleevent :|FLGS| #x1))
           (send-appleevent appleevent reply))))

;;; Get current URL from Netscape
(defun get-url ()
   (when (find-process-with-signature *netscape-creator*)
       (with-aedescs (appleevent target reply list)
           (create-signature-target target *netscape-creator*)
           (create-appleevent appleevent :|WWW!| :|LSTW| target)
           (send-appleevent appleevent reply :reply-mode :wait-reply)
           (#_AEGetParamDesc reply #$keyDirectObject #$typeAEList list)
           (rlet ((aekeyword :ostype)
                     (actual-type :ostype)
                     (actual-size :signed-long)
                     (data :str255))
              ;; arbitrarily use the first window in the list
              (#_AEGetNthPtr list 1 #$typeLongInteger aekeyword actual-type data 256 actual-size)
              (let ((window-id (%get-signed-long data)))
                 ;;; send a second event to get the URL
                 (create-appleevent appleevent :|WWW!| :|WNFO| target)
                 (ae-put-parameter-longinteger appleevent #$keyDirectObject window-id)
                 (send-appleevent appleevent reply :reply-mode :wait-reply)
                 (#_AEGetParamDesc reply #$keyDirectObject #$typeAEList list)
                 (#_AEGetNthPtr list 1 #$typeChar aekeyword actual-type data 256 actual-size)
                 (%str-from-ptr data (%get-long actual-size))
                 )))))

;;; Hooks for HTML editing within MCL

(defmethod maybe-view-html-file ((w fred-mixin))
   (when (and *netscape-control*
                     (view-get w :html-mode-set))
       (open-url (format nil "file:///~A" (nsubstitute #\/ #\: (mac-namestring (window-filename w))))
                      t)))

(advise window-save-file 
            (when (typep (car arglist) 'fred-mixin)
                (maybe-view-html-file (car arglist)))
            :when :after
            :name :html-editor)

;;; called when MCL is selected by magic keyboard hack
(defun maybe-read-html-file ()
   (when *netscape-control*
       (let ((url (ignore-errors (get-url))))
          (when (stringp url)
              (if (string= url "file:///" :end1 8)
                 (let* ((sharp-pos (position #\# url))
                           (filename (unencode-url (subseq url 8 sharp-pos))))
                    (nsubstitute #\: #\/ filename)
                    (ignore-errors (ed filename))))))))


;;; undo the %-encoding used in URLs
(defun unencode-url (string &key (start 0) (end nil))
   (do ((from start)
          (substrings nil)
          (subst-start t)
          (*read-base* 16))
         ((null subst-start)
           (apply #'concatenate
                      'string
                      (nreverse substrings)))
      (setq subst-start (search "%" string :start2 from :end2 end))
      (push (subseq string from subst-start) substrings)
      (when subst-start
          (setf from (+ subst-start 3))
          (push (string (code-char (read-from-string string nil nil 
                                                                             :start (+ subst-start 1)
                                                                             :end (+ subst-start 3))))
                   substrings))))
        
;;; (require :processes)

;;; patch to bill's processes.lisp, to deal with new MCL3 lossage
;;; and do special hack from URL
(defun select-mcl-eventhook (&rest ignore)
   (declare (ignore ignore))
   (unless *foreground*
      (let ((*current-event* nil)
              (*control-key-mapping* nil))          ; deal with NCL3 shift-key lossage
         (makunbound '*current-event*)
         (when (and (control-key-p) (option-key-p) (command-key-p))
             (maybe-read-html-file)
             (select-mcl))))
   nil)
