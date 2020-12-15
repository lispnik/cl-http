;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for HTML-PARSER version 3.4
;;; Reason: Patch in new html-reader.lisp
;;; Written by JCMa, 8/28/96 13:53:54
;;; while running on Lispm Machine Thomas Jefferson from FEP0:>ComLink-38-11-HTTP-60-D-MIT-8-3.ilod.1
;;; with Genera 8.3, Logical Pathnames Translation Files NEWEST, NFS Server 435.0,
;;; Metering 439.0, Metering Substrate 439.0, Conversion Tools 430.0, Hacks 435.0,
;;; CLIM 66.5, Genera CLIM 66.0, PostScript CLIM 66.2, CLIM Documentation 66.0,
;;; 8-3-Patches 1.30, MAC 412.7, Statice Runtime 460.4, Statice 460.1,
;;; Statice Browser 460.0, Statice Documentation 423.0, DBFS Utilities 439.0,
;;; Showable Procedures 36.2, Binary Tree 34.0, Mailer 434.0,
;;; Experimental Working LispM Mailer 6.0, HTTP Server 60.22,
;;; Experimental W3 Presentation System 2.1, CL-HTTP Server Interface 48.1,
;;; Symbolics Common Lisp Compatibility 3.0, Experimental Comlink Packages 4.2,
;;; Experimental Comlink Utilities 9.10, Experimental Routing Taxonomy 8.0,
;;; Experimental COMLINK Database 10.15, Experimental Email Servers 11.4,
;;; Experimental Comlink Customized LispM Mailer 6.6,
;;; Experimental Dynamic Forms 11.7, Experimental Communications Linker Server 38.13,
;;; Jcma 41, HTML Parser 3.3, Ivory Revision 4A, FEP 328,
;;; FEP0:>I328-loaders.flod(24), FEP0:>I328-info.flod(24), FEP0:>I328-debug.flod(24),
;;; FEP0:>I328-lisp.flod(25), FEP0:>I328-kernel.fep(44), Boot ROM version 320,
;;; Device PROM version 325, Genera application 5.6.1a1,
;;; MacIvory SCSI Manager Server 4.3.2a1, Toolbox Servers 4.2,
;;; MacIvory & RPC library 6.3.3a1, MacIvory life support 4.3.8a1,
;;; Symbolics keyboard 2.1, Macintosh System Software 7.5.3,
;;; 1152x820 Screen with Genera fonts, Machine serial number 30376, Macintosh,
;;; Symbolics Keyboard,
;;; Add support for Apple's Gestalt and Speech Managers. (from SYS:MAC;MACIVORY-SPEECH-SUPPORT.LISP.5),
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.22),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.8),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Deny some hosts access to some servers. (from CML:LISPM;HOST-SERVICE-ACCESS-CONTROL.LISP.4),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.102),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.47).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTML-PARSER:HTML-PARSER;HTML-READER.LISP.9")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTML-PARSER:HTML-PARSER;HTML-READER.LISP.9")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: html-parser -*-")

(PROGN
;;; -*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: html-parser -*-

;;; Last edited by smishra on Sun Aug 25 20:23:55 1996

;;; (c) Copyright 1996, Sunil Mishra (smishra@cc.gatech.edu)
;;;     All Rights Reserved

(in-package :html-parser)

;;;----------------------------------------
;;;  operation

(defun next-html-char (&aux (html-string *html-string*))
  (when (< *current-pos* *input-length*)
    (if (html-whitespace-p (char html-string *current-pos*))
	(loop with last-whitespace-p = (and (> *current-pos* 0)
					    (html-whitespace-p
					      (char html-string (1- *current-pos*))))
	      for nchar = (char html-string *current-pos*)
	      when (= (incf *current-pos*) *input-length*)
	        if (and last-whitespace-p (html-whitespace-p nchar))
		  return nil
	      else return nchar
	      unless (and last-whitespace-p (html-whitespace-p nchar))
	        return nchar)
	(prog1 (char html-string *current-pos*)
	       (incf *current-pos*)))))

(defun peek-html-char (&aux (html-string *html-string*))
  (when (< *current-pos* *input-length*)
    (if (html-whitespace-p (char html-string *current-pos*))
	(loop with last-whitespace-p = (and (> *current-pos* 0)
					    (html-whitespace-p
					      (char html-string (1- *current-pos*))))
	      for nchar = (char html-string *current-pos*)
	      for *current-pos* = (1+ *current-pos*)
	      when (= *current-pos* *input-length*)
	        if (and last-whitespace-p (html-whitespace-p nchar))
		  return nil
	      else return nchar
	      unless (and last-whitespace-p (html-whitespace-p nchar))
	        return nchar)
	(char html-string *current-pos*))))

(defun current-html-char ()
  (and (<= 1 *current-pos* *input-length*)
       (char *html-string* (1- *current-pos*))))

(defun previous-html-char (&aux (html-string *html-string*))
  (cond ((> *current-pos* 0)
	 (decf *current-pos*)
	 (if (html-whitespace-p (char html-string *current-pos*))
	     (loop for bchar = (current-html-char)
		   while (html-whitespace-p bchar)
		   do (decf *current-pos*)
		   finally (return bchar))
	     (current-html-char)))
	(t (error "Unread beyond beginning of input"))))

(defun move-to-char (pos)
  (setq *current-pos* pos))

;;;----------------------------------------
;;; Parser

(defun next-html-token (&aux (start-pos *current-pos*))
  (let ((char (next-html-char)))
    (case char
      ((nil) (values *eof* nil))
      (#\& (multiple-value-bind (type val)
	       (parse-entity-or-char-ref)
	     (if type
		 (values type val)
		 (read-body-text start-pos))))
      (#\< (multiple-value-bind (type val)
	       (parse-tag-decl-comment)
	     (if type
		 (values type val)
		 (read-body-text start-pos))))
      (t (read-body-text start-pos)))))

(defun parse-entity-or-char-ref ()
  (case (next-html-char)
    ((nil) nil)
    (#\# (parse-char-ref))
    (t (parse-html-entity))))

(defun parse-tag-decl-comment ()
  (case (next-html-char)
    ((nil) nil)
    (#\/ (parse-html-close-tag))
    (#\! (parse-html-declaration))
    (t (previous-html-char)
       (parse-html-open-tag))))

;;;----------------------------------------
;;; Character reference

(defun parse-char-ref ()
  (declare (values tk-type tk-value))
  (let ((char-no (parse-html-number)))
    (and char-no
	 (cond ((eql (current-html-char) #\;)
		(next-html-char)
		(values :character char-no))
	       ((html-whitespace-p (current-html-char))
		(values :character char-no))
	       (t
		;; I have not attempted an error recovery from this
		;; situation, should I bother?
		nil)))))

;;;----------------------------------------
;;; Entity reference

(defun parse-html-entity ()
  (declare (values tk-type tk-value))
  (let ((entity-name (parse-html-name #'tokenize-entity)))
    (and (html-entity-p entity-name)
	 (cond ((eql (current-html-char) #\;)
		(next-html-char)
		(values :entity entity-name))
	       ((html-whitespace-p (current-html-char))
		(values :entity entity-name))
	       (t nil)))))

;;;----------------------------------------
;;; Close tag

(defun parse-html-close-tag ()
  (declare (values tk-type tk-value))
  (let ((tag-name (parse-html-name #'tokenize-name)))
    (when tag-name
      (html-skip-whitespace)
      (case (next-html-char)
	((nil #\>)
	 (values :close-tag tag-name))
	(t
	  ;; Accept the erroneous close tag anyway
	  (previous-html-char)
	  (values :close-tag tag-name))))))

;;;----------------------------------------
;;; Declaration

;;; I could make things here into tokens, but why bother if I'm not going
;;; to ever look at them? In fact, why even take a subseq?
;;; In fact, I'm not even going to bother building up a return value right
;;; now, though I have put in forms for how they would otherwise be
;;; constructed

(defun no-processing-action (string &optional start end)
  (declare (ignore string start end))
  t)
  
(defun parse-html-declaration ()
  (declare (values tk-type tk-value))
  (case (next-html-char)
    ((nil #\>) (values :comment nil))
    (#\- (when (eql (next-html-char) #\-)
	   (parse-rest-html-comment)))
    (t (previous-html-char)
       (let ((decl-name (parse-html-name #'no-processing-action)))
	 (when decl-name
	   (html-skip-whitespace)
	   (parse-rest-html-declaration #+ignore (list decl-name)))))))

(defun parse-rest-html-declaration (#+ignore rdecl)
  (declare (values tk-type tk-value))
  (do ((char (next-html-char)
	     (next-html-char)))
      ((or (null char) (eql char #\>))
       (values :declaration
	       #+ignore (nreverse rdecl)
	       #-ignore nil))
    (case char
      (#\-
       (cond ((eql (next-html-char) #\-)
	      #+ignore (push (cons :comment (parse-html-comment-string))
			     rdecl)
	      #-ignore (parse-html-comment-string)
	      (html-skip-whitespace))
	     (t (previous-html-char)
		(return-from parse-rest-html-declaration nil))))
      ((#\" #\')				;" <- just to make emacs happy
       (previous-html-char)
       #+ignore (push (parse-html-literal  #'no-processing-action) rdecl)
       #-ignore (parse-html-literal  #'no-processing-action)
       (html-skip-whitespace))
      (t
	(previous-html-char)
	(cond ((html-name-char-p char)
	       #+ignore (parse-rest-html-declaration
			  (cons (parse-html-name-token #'no-processing-action)
				rdecl))
	       #-ignore (parse-html-name-token #'no-processing-action)
	       #-ignore (parse-rest-html-declaration)
	       (html-skip-whitespace))
	      (t (return-from parse-rest-html-declaration
		   (values :declaration #+ignore (nreverse rdecl)
			   #-ignore nil))))))))

;;;----------------------------------------
;;; Comment

;;; Once again, I am not interested in comments, and shall simply
;;; ignore their existence

;;;(defun parse-rest-html-comment (&optional rstrings)
;;;  ;; The start of this function is when the initial -- has been read
;;;  (declare (values tk-type tk-value)
;;;           #-ignore (ignore rstrings))
;;;  (let ((com-str (parse-html-comment-string)))
;;;    #-ignore (declare (ignore com-str))
;;;    (html-skip-whitespace)
;;;    (case (next-html-char)
;;;      (#\- (when (eql (next-html-char) #\-)
;;;             (parse-rest-html-comment #+ignore (cons com-str rstrings))))
;;;      (#\> (values :comment #+ignore (nreverse (cons com-str rstrings))
;;;                   #-ignore nil)))))

(defun parse-rest-html-comment (&optional rstrings)
  ;; The start of this function is when the initial -- has been read
  ;; This version of the comment reader is to handle cases where
  ;; authors have written broken comment strings. The end test for
  ;; such a comment string shall be --{ws}>
  (declare (values tk-type tk-value)
	   #-ignore (ignore rstrings))
  (let ((com-str (parse-broken-html-comment-string)))
    #-ignore (declare (ignore com-str))
    (values :comment #+ignore (list rstrings)
	    #-ignore nil)))

(defun parse-broken-html-comment-string ()
  (do* (#+ignore (com-start *current-pos*)
        (char (next-html-char) (next-html-char))
	(comment-close-start (parse-broken-html-comment-close char)
			     (parse-broken-html-comment-close char)))
       (comment-close-start
	 #+ignore (subreference *html-string* com-start
				(if (>= *current-pos* *input-length*)
				    comment-close-start
				    (1- comment-close-start))))))

(defun parse-broken-html-comment-close (char)
  (case char
    ((nil) *current-pos*)			; Exhausted input
    (#\-					; Possible comment close start
     (let ((com-close-start *current-pos*))
       (cond ((and (eql (next-html-char) #\-)
		   (progn (html-skip-whitespace)
			  (eql (next-html-char) #\>)))
	      com-close-start)
	     (t (move-to-char com-close-start)
		nil))))))

(defun parse-html-comment-string ()
  (do (#+ignore (com-start *current-pos*)
       (char (next-html-char) (next-html-char))
       (last-char nil char))
      ((or (null char)
	   (and (eql char #\-) (eql last-char #\-)))
       #+ignore (subreference *html-string* com-start
			      (if (null char)
				  *current-pos*
				  (- *current-pos* 2))))))

;;;----------------------------------------
;;; Open tag

(defun parse-html-open-tag ()
  (declare (values tk-type tk-value))
  (let* ((tag-name (parse-html-name  #'tokenize-name))
	 (tag-defn (and tag-name (tag-definition tag-name)))
	 (tag-instance (and tag-name
			    (make-tag-instance tag-name tag-defn))))
    (when tag-instance
      #+debug (push tag-instance (instances (tag-definition tag-name)))
      (html-skip-whitespace)
      (setf (attr-values tag-instance)
	    (parse-html-open-tag-attributes  (tag-definition tag-name)))
      (values :open-tag tag-instance))))

(defun make-tag-instance (tag-name tag-defn)
  (and tag-name
       (if tag-defn
	   (make-instance 'html-tag-instance
			  :instance-of tag-defn)
	   (make-instance 'unknown-tag-instance
			  :instance-of (tag-definition #t"UNKNOWN")
			  :name tag-name))))

(defun parse-html-open-tag-attributes (tag-defn &optional rattr-list)
  ;; This function should be entered only on one of the following states:
  ;; 1. Read tag name and skipped whitespace
  ;; 2. Read attribute value and skipped whitespace
  ;; 3. Read attribute name = attribute value and skipped whitespace
  ;; If a malformed attribute or value is found, the tag is closed and
  ;; returned.
  (case (next-html-char)
    ((nil #\>) (nreverse rattr-list))
    (t (previous-html-char)
       (parse-html-attr-name-or-val tag-defn rattr-list))))

(defun parse-html-attr-name-or-val (tag-defn rattr-list)
  (let ((name-or-val (parse-html-name #'tokenize-name)))
    (cond (name-or-val
	   (html-skip-whitespace)
	   (case (next-html-char)
	     ((nil)
	      ;; out of characters, name-or-val must be val
	      (nreverse (cons (cons (and tag-defn
					 (tag-value-attribute name-or-val tag-defn))
				    name-or-val)
			      rattr-list)))
	     (#\=
	      ;; name-or-val must be an attribute name
	      (html-skip-whitespace)
	      (let* ((attr-defn (and tag-defn
				     (tag-attribute-definition name-or-val tag-defn)))
		     (attr-val (parse-html-attribute-value attr-defn)))
		(if attr-val
		    (parse-html-open-tag-attributes
		      tag-defn
		      (cons (cons (or attr-defn name-or-val) attr-val)
			    rattr-list))
		    ;; valid value not found - close tag
		    (nreverse rattr-list))))
	     (t
	       ;; name-or-val must be val
	       (previous-html-char)
	       (html-skip-whitespace)
	       (parse-html-open-tag-attributes
		 tag-defn
		 (cons (cons (and tag-defn
				  (tag-value-attribute name-or-val tag-defn))
			     name-or-val)
		       rattr-list)))))
	  ;; not a name or value - close the open tag
	  (t (nreverse rattr-list)))))

(defun parse-html-attribute-value (attr-defn)
  ;; The input  should have exhausted {attr-name}{ws}={ws}
  (prog1
    (case (peek-html-char)
      ((#\' #\")				; " <- make emacs happy
       (parse-quoted-attribute-value attr-defn))
      (t (parse-unquoted-attribute-value attr-defn)))
    (html-skip-whitespace)
    ))

(defun initialize-parse-dispatch-functions ()
  (let ((cdata-token (tokenize-name "CDATA"))
	(name-token (tokenize-name "NAME"))
	(number-token (tokenize-name "NUMBER")))
    (setf
      (get-value cdata-token :quoted-parser) #'parse-html-literal
      (get-value cdata-token :unquoted-parser) #'parse-html-cdata
      (get-value name-token :quoted-parser) #'(lambda () (parse-html-literal  #'tokenize-name))
      (get-value name-token :unquoted-parser) #'(lambda () (parse-html-name  #'tokenize-name))
      (get-value number-token :quoted-parser) #'(lambda () (parse-html-literal  #'parse-number))
      (get-value number-token :unquoted-parser) #'parse-html-number)))
	
(defun parse-quoted-attribute-value (attr-defn)
  (if attr-defn
      (etypecase (allowed-values attr-defn)
	(html-parser-token
	 (funcall (get-value (allowed-values attr-defn) :quoted-parser)))
	(list (parse-html-literal #'tokenize-name)))
      ;; No attribute definition - assume CDATA
      (parse-html-literal)))

(defun parse-unquoted-attribute-value (attr-defn)
  (if attr-defn
      (etypecase (allowed-values attr-defn)
	(html-parser-token
	  (funcall (get-value (allowed-values attr-defn) :unquoted-parser)))
	(list (parse-html-name)))
      (parse-html-cdata)))

(defun tag-value-attribute (value tag-defn)
  ;; If the attribute definition is not found, then nil is left it's place
  (loop for item in (attributes tag-defn)
	for allowed-value = (allowed-values item)
	when (typecase allowed-value
	       (null nil)
	       (cons (member value allowed-value))
	       (symbol (eql value allowed-value)))
	  return item
	finally (return nil)))

;;;----------------------------------------
;;; Character data

(defun read-body-text (start-pos)
  (do ((char (next-html-char) (next-html-char)))
      ((or (null char) (eql char #\<) (eql char #\&))
       (and char
	    (previous-html-char))
       (values :pcdata (subreference *html-string* start-pos *current-pos*)))))

;;;----------------------------------------
;;; Primitive parse functions

(defun subreference (string &optional (start 0) (end (length string)))
  (make-array (- end start) :element-type *string-char-type*
	      :displaced-to string :displaced-index-offset start))

;;; Name parsing

(defun parse-html-name (&optional (processing-fn #'subreference))
  (parse-html-token-string #'html-name-start-char-p #'html-name-char-p
			   processing-fn))

;;; Number parsing

(defun parse-number (str &optional (start nil startp) (end nil endp))
  (cond ((and startp endp)
	 (parse-integer str :start start :end end))
	(startp
	 (parse-integer str :start start))
	(endp
	 (parse-integer str :end end))
	(t (parse-integer str))))

(defun parse-html-number ()
  (parse-html-token-string #'html-digit-char-p #'html-digit-char-p
			   #'parse-number))

;;; Number token parsing

(defun parse-html-number-token (&optional (processing-fn #'subreference))
  (parse-html-token-string #'html-digit-char-p #'html-name-char-p
			   processing-fn))

;;; Name token parsing

(defun parse-html-name-token (&optional (processing-fn #'subreference))
  (parse-html-token-string #'html-name-char-p #'html-name-char-p
			   processing-fn))

;;; Token string parsing

(defun parse-html-token-string (1st-char-pred rest-char-pred processing-fn)
  (let ((1st-char (next-html-char)))
    (cond ((funcall 1st-char-pred 1st-char)
	   (parse-rest-html-token-string rest-char-pred processing-fn))
	  (t (previous-html-char)
	     nil))))

(defun parse-rest-html-token-string (rest-char-pred processing-fn)
  (do ((str-start (1- *current-pos*))
       (char (next-html-char)
	     (next-html-char)))
      ((not (funcall rest-char-pred char))
       (and char (previous-html-char))
       (funcall processing-fn *html-string* str-start *current-pos*))))

;;; Literal parsing

(defun parse-html-literal (&optional (processing-fn #'subreference))
  (let ((literal-start-char (next-html-char)))
    (and (or (char= literal-start-char #\")	;" -> just to make emacs happy
	     (char= literal-start-char #\'))
	 (do ((start-pos *current-pos*)
	      (char (next-html-char)
		    (next-html-char)))
	     ((or (null char)
		  (eql char literal-start-char))
	      (funcall processing-fn *html-string* start-pos
		       (if char
			   (1- *current-pos*)
			   *current-pos*)))))))

;;; Parsing CDATA

(defun parse-html-cdata ()
  ;; One might wish to modify this function to make it more robust against
  ;; the appearance of malformed attribute values
  (let ((char (next-html-char)))
    (when char
      (previous-html-char)
      (case char
	((#\" #\')			; " -> just to make emacs happy
	 (parse-html-literal))
	(t (parse-html-token-string  #'cdata-char-p #'cdata-char-p
				     #'subreference))))))

(defun cdata-char-p (char)
  ;; This is a hack to handle the cases where netscape allows severely
  ;; broken text in as CDATA
  (not (or (html-whitespace-p char)
	   (eql char #\<)
	   (eql char #\>))))

;;; Skip whitespace

(defun html-skip-whitespace ()
  (do ((char (next-html-char) (next-html-char)))
      ((not (html-whitespace-p char))
       (and char (previous-html-char)))))

;;;----------------------------------------
;;; Character test predicates

(defun html-digit-char-p (char)
  (and (characterp char) (digit-char-p char)))

(defun html-name-start-char-p (char)
  (and (characterp char) (alpha-char-p char)))

(defun html-name-char-p (char)
  (and (characterp char)
       (or (alphanumericp char)
	   (char= char #\-)
	   (char= char #\/)
	   (char= char #\.))))

(defun html-whitespace-p (char)
  (member char '(#\space #\tab #\newline #\linefeed #\return)
	  :test #'eql))

;;;----------------------------------------
;;; Test function
;;;
;;;(defun test-reader (fname)
;;;  (with-open-file (fstr fname :direction :input)
;;;      (do ((type nil)
;;;           (item nil))
;;;          ((eql type *eof*)
;;;           (break))
;;;        (multiple-value-setq (type item)
;;;          (next-html-token fstr))
;;;        (format t "~A: ~A~%" type item))))

(defun test-reader (string)
  (do ((*html-string* string)
       (*input-length* (length string))
       (*current-pos* 0)
       (type nil)
       (item nil))
      ((eql type *eof*))
    (multiple-value-setq (type item)
      (next-html-token))
    (format t "~A: ~A~%" type item)))

)
