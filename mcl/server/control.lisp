;;; -*- Syntax: Ansi-Common-Lisp; Package: CL-USER; Base: 10; Mode: lisp -*-

(in-package :ccl)

#| #######################################################################

 Control Netscape or other browsers from MCL

Copyright (c) 1995, 1996, 1997 Michael Travers 

Permission is given to use and modify this code
as long as the copyright notice is preserved.

Send questions, comments, and fixes to mt@media.mit.edu.

-------------------------------------------------------------------------

This package provides an interface to Netscape (or other browsers that obey its
protocols, including Microsoft Explorer) via AppleEvents. The routines OPEN-URL
and GET-URL allow you to force Netscape to open up a particular URL, or to get
the currently open URL from Netscape. 

*browser-creator* (variable) The Mac ostype code for the browser of your choice.
   (default value is :|MOSS| for Netscape)

open-url (url &key no-cache window (browser *browser-creator*) activate-p)
  Cause the browser to view the document specified by the URL.
  :NO-CACHE 
     a non-nil value will force the browser to retrieve the real document
     rather than looking in its cache
  :WINDOW n
     where n is an integer, will use the nth browser window
  :ACTIVATE-P
     when non-nil, make the browser the active application

get-url (&key (browser *browser-creator*) (window 1))
  Return the URL in the specified browser window, as a string.

browser-open-file (file &key (browser *browser-creator*) (window 1))
   view the file in the browser

browser-file-name (&key (browser *browser-creator*) (window 1))
   return the file name of the document being viewed, or NIL if it's not a file.

-------------------------------------------------------------------------
Notes:

The Netscape API has other capabilities that this interface does not
yet provide.  You are encouraged to add anything you need and send the
changes back to me for inclusion.  The API is documented at:
   http://home.netscape.com/newsref/std/mac-remote-control.html 
-------------------------------------------------------------------------
History:

5/26/95 12:09: deal with #s in URLs
5/27/95 16:55: Conditional for MCL2
 7/2/95 13:40: Added edit point 
9/17/95 18:58: More conditionals for MCL3 vs 2
9/27/95 14:51: fix bug that made windows forget their file
9/28/95 23:24: Put editpoint at beginning of line: try to avoid syntax problems
 7/7/96 14:23  Better handling of multiple netscape windows
7/14/96 11:32  Split into netscape-control and netscape-edit
 2/9/97  2:21  From Rainer Joswig:  generalize to other browsers
                   startup browser if necessary
                   add keywords to force browser to activate

####################################################################### |#

;;; Interface to Netscape and other browsers via AppleEvents

(require :appleevent-toolkit)
(require :processes)

(defparameter *browser-creator* :|MOSS|
  "Netscape Navigator = :|MOSS| , Microsoft Internet Explorer = :|MSIE|")

;;; Make browser open an URL
(defun open-url (url &key no-cache window (browser *browser-creator*) activate-p)
  (ensure-application-active browser)
  (when (find-process-with-signature browser)
    (with-aedescs (appleevent target reply)
      (create-signature-target target browser)
      (create-appleevent appleevent :|WWW!| :|OURL| target)
      (ae-put-parameter-char appleevent #$keyDirectObject url)
      (when no-cache
        (ae-put-parameter-longinteger appleevent :|FLGS| #x1))
      (when window
        (ae-put-parameter-longinteger appleevent :|WIND| window))
      (send-appleevent appleevent reply))
    (when activate-p (select-process browser))))

;;; Get current URL from browser
(defun get-url (&key (browser *browser-creator*) (window 1))
  (ensure-application-active browser)
  (when (find-process-with-signature browser)
    (let ((window-id (get-browser-window browser window)))
      (with-aedescs (appleevent target reply list)
        (create-signature-target target browser)
        (rlet ((aekeyword :ostype)
               (actual-type :ostype)
               (actual-size :signed-long)
               (data :str255))
          ;;; send a second event to get the URL
          (create-appleevent appleevent :|WWW!| :|WNFO| target)
          (ae-put-parameter-longinteger appleevent #$keyDirectObject window-id)
          (send-appleevent appleevent reply :reply-mode :wait-reply)
          (#_AEGetParamDesc reply #$keyDirectObject #$typeAEList list)
          (#_AEGetNthPtr list 1 #$typeChar aekeyword actual-type data 256 actual-size)
          (%str-from-ptr data (%get-long actual-size))
          )))))

;;; returns window id of the nth browser window
(defun get-browser-window (browser n)
  (when (find-process-with-signature browser)
    (with-aedescs (appleevent target reply list)
      (create-signature-target target browser)
      (create-appleevent appleevent :|WWW!| :|LSTW| target)
      (send-appleevent appleevent reply :reply-mode :wait-reply)
      (#_AEGetParamDesc reply #$keyDirectObject #$typeAEList list)
      (rlet ((count :signed-long))
        (#_AECountItems list count)
        (unless (<= n (%get-long count))
          (error "There is no ~Ath browser window." n)))
      (rlet ((aekeyword :ostype)
             (actual-type :ostype)
             (actual-size :signed-long)
             (data :str255))
        (#_AEGetNthPtr list n #$typeLongInteger aekeyword actual-type data 256 actual-size)
        (if (and (eq (%get-ostype actual-type) :|long|)
                 (eq (%get-long actual-size) 4))
          (%get-long data)
          (error "Don't know how to deal with window ids of type ~A" (%get-ostype actual-type)))))))
  
        
(defun browser-open-file (file &key (window 1) anchor)
  (open-url (format nil "file:///~A~@[#~A~]"
                    (nsubstitute #\/ #\: (mac-namestring file))
                    anchor)
            :no-cache t
            :window (get-browser-window *browser-creator* window)))

(defun browser-file-name (&key (window 1) (browser *browser-creator*))
  (let ((url (ignore-errors (get-url :browser browser :window window))))
    (when (stringp url)
      (if (string= url "file:///" :end1 8)
        (let* ((sharp-pos (position #\# url))
               (filename (unencode-url (subseq url 8 sharp-pos))))
          (nsubstitute #\: #\/ filename)
          filename)))))

;;; undo the %-encoding used in URLs (probably not valid for all codes)
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

(export '(*browser-creator*
          open-url
          get-url
          browser-open-file
          browser-file-name
          unencode-url))
