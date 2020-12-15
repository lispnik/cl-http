;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: cl-user; -*- 

;;; Copyright John C. Mallery,  1997.
;;; All rights reserved. 

;;;------------------------------------------------------------------- 
;;;
;;; META CONTENT FORMAT GENERATION FACILITY 
;;;
;;; Based on the MCF specification at http://mcf.research.apple.com/hs/mcf.html
;;;
;;;
;;; MCF 0.95 specification is poorly specified, with many ambiguities and tons of
;;; dead spec. It seems like Guha was attempting to devise a knowledge exchange mark up 
;;; based on Greiner's (1980)  units, as found in CYC (Lenat). There are numerous unused 
;;; facilities and no explanation of what is relevant or why.  The generation facility below
;;; takes the Hot Sauce plug-in as the cash value of MCF, and therefore, implements
;;; only enough MCF to drive the plug-in, which is available for both MAC and Windows
;;; platforms.  A better approach would use VRML generation to achieve similar, or better, 
;;; results. If someone finds reasons to extend this MCF implementation, please let us know.
;;;  -- JCMa 7/29/1997.
;;;
;;; Apple has flushed research.  The new url is http://www.xspace.net/   11/7/97 -- JCMa.

(defpackage meta-content-format
  (:use future-common-lisp)
  (:nicknames mcf)
  (:import-from http
   "COND-EVERY"
   "FAST-FORMAT"
   "WITH-STRING-FOR-NULL-STREAM")
  (:export
    "*MCF-VERSION*"
    "HOTSAUCE-NODE"
    "NOTE-UNIT-IDENTIFIER"
    "WITH-HEADERS"
    "WITH-HOTSAUCE-DOCUMENT"
    "WITH-UNIT"
    "WRITE-COMMENT"))

(in-package :mcf) 

(defun note-slot (stream slot value)
  (flet ((write-value (val stream)
           (write-char #\space stream)
           (write val :escape (if (and (stringp val) (eql (aref val 0) #\#)) nil t) :stream stream)))
    (fresh-line stream)
    (write-string (symbol-name slot) stream)
    (write-char #\: stream)
    (typecase value
      (cons (dolist (val value)
              (write-value val stream)))
      (t (write-value value stream))))) 

(defmacro with-headers ((stream) &body slot-specs)
  `(progn 
     (fast-format ,stream "~&begin-headers:")
     (cond-every
       ,@(loop for (slot value) on slot-specs by #'cddr
               collect `(,value 
                         (note-slot ,stream ,slot ,value))))
     (fast-format ,stream "~&end-headers:" ,stream)))

(defparameter *mcf-version* 0.9) 

(defmacro with-hotsauce-document ((stream &key name layer include-mcf-urls site-table-of-contents) &body body)
  "Establishes the context for a HOTSAUCE document."
  `(progn (with-headers (,stream)
                        :|MCFVersion| *mcf-version*     ; decimal version number for MCL
                        :name ,name             ; document name
                        :filelayer ,layer       ;  the layer that the contents of this file belong to. Defaults to the most local layer. 
                        :include ,include-mcf-urls      ; list of urls for the other MCF urls logically included
                        :tocof ,site-table-of-contents) ; table of contents for a web site, then this slot contains the url for that site
          ,@body)) 

(defun note-unit-identifier (name stream &key url object-ref-p)
  (http::with-string-for-null-stream (stream)
    (cond-every
      (object-ref-p (write-char #\# stream))
      (url (fast-format stream "~I#" (url:coerce-url-string url))))
    (write name :escape t :stream stream)) ) 

(defmacro with-unit ((name stream &key location) &body slot-specs) 
  `(progn (fast-format stream "~&UNIT: ~I" (note-unit-identifier ,name ,stream :url ,location))
          (cond-every
            ,@(loop for (slot value) on slot-specs by #'cddr
                    collect `(,value (note-slot ,stream ,slot ,value))))))

(defun write-comment (stream string &optional (start 0) (end (length string)))
  (loop for s = start then (1+ (the fixnum s))
        for e = (or (position #\; string :start s :end end)
                    end)
        until (= e end)
        do (write-char #\; stream)
           (write-string string stream :start s :end e))) 

(defun hotsauce-node (stream url &key name title description superior x-offset y-offset)
  "Emits a hotsauce node on STREAM."
  (with-unit (url stream)
             :name name
             :description description
             :contentFrame title
             :parent superior
             :default_genl_x x-offset
             :default_genl_y y-offset)) 

