;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: USER; Base: 10 -*-

(in-package "USER")

;;; Minimal update to ACLPC.
;;; Copyright (C) 1994, 1995 Olivier (OBC) all rights reserved.
;;; See copyright notice in CLIM;CLIM-SYS;MINIPROC file.
;;;

(defpackage "COMMON-LISP" (:use) (:export "READ-SEQUENCE" "WRITE-SEQUENCE"))

;;; Most efficient use is from character stream to character buffer!
(defmethod read-sequence ((sequence string) (stream stream) &key (start 0) (end (length sequence)))
  (case (stream-element-type stream)
    (character
     (case (array-element-type sequence)
       (character
	(if (eql start 0)
	    (cg:device-nread-string stream sequence end)
	  (loop for n from start below end
	      as character = (read-char stream nil :eof)
	      until (eql character :eof)
	      do (setf (elt sequence n) character)
		 #+Debug
		 (write-char character)
	      finally (return n))))
       (t
	(loop for n from start below end
	     as character = (read-char stream nil :eof)
	     until (eql character :eof)
	     do (setf (elt sequence n) (char-code character))
		#+Debug
		(write-char character)
	     finally (return n)))))
     (t
      (case (array-element-type sequence)
	(character
	 (loop for n from start below end
	    as byte = (read-byte stream nil :eof)
	    until (eql byte :eof)
	    do (setf (elt sequence n) (character byte))
	       #+Debug
	       (write-char (character byte))
	    finally (return n)))
	(t
	 (loop for n from start below end
	    as byte = (read-byte stream nil :eof)
	    until (eql byte :eof)
	    do (setf (elt sequence n) byte)
	       #+Debug
	       (write-char (character byte))
	    finally (return n)))))))

;;; Only method that does not hangs NT: can't write bytes one at a time.
;;;
(defmethod write-sequence ((sequence string) (stream stream) &key (start 0) (end (length sequence)))
    (case (stream-element-type stream)
      (character
       (cg:device-write-string stream sequence start end))
      (t
       (write-string sequence stream :start start :end end))))
