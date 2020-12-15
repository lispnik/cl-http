;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: HTML-PARSER -*-
;;
;; Read an HTML file as a sequence of tokens. This parser uses the metadata
;; provided by a DTD.
;;
;; Copyright (c) 1996-97 Sunil Mishra <smishra@cc.gatech.edu>,
;; portions copyright (c) 1999 Kaelin Colclasure <kaelin@acm.org>,
;; all rights reserved.
;;
;; $Id: //depot/rev/lisp/html-parser-10.0-alpha/html-reader.lisp#1 $

(in-package :html-parser)

;;; Utilities

(defun subreference (vector &optional (start 0) (end (length vector)))
  "Create a displayed vector referencing the original one"
  (assert (null (cdr (array-dimensions vector))))
  (make-array (- end start) :element-type (array-element-type vector)
	      :displaced-to vector :displaced-index-offset start))

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

(defun cdata-char-p (char)
  ;; This is a hack to handle the cases where netscape allows severely
  ;; broken text in as CDATA
  (not (or (html-whitespace-p char)
	   (eql char #\<)
	   (eql char #\>))))

;;; Input string movement

(defun next-html-char (parser &optional (update-p t))
  (let ((input (parser-input parser))
        (pos (parser-read-position parser))
        (input-length (length (parser-input parser))))
    (when (< pos input-length)
      (if (and (html-whitespace-p (char input pos))
               (or (= pos 0) (html-whitespace-p (char input (1- pos)))))
	  ;; previous was whitespace, current is whitespace, so find
	  ;; next non-whitespace
	  (loop for npos from pos below input-length
	      for nchar = (char input npos)
	      while (html-whitespace-p nchar)
	      finally (return
			(cond ((< npos input-length)
			       (when update-p
				 (setf (parser-read-position parser)
				   (1+ npos)))
			       nchar)
			      (t
			       (when update-p
				 (setf (parser-read-position parser) npos))
			       nil))))
	;; just find the next
	(prog1 (char input pos)
	  (when update-p
	    (incf (parser-read-position parser))))))))

(declaim (inline peek-html-char))

(defun peek-html-char (parser)
  (next-html-char parser nil))

(defun current-html-char (parser)
  (let ((input (parser-input parser))
        (pos (parser-read-position parser)))
    (and (< 0 pos) (char input (1- pos)))))

(defun previous-html-char (parser)
  (let ((input (parser-input parser))
        (pos (decf (parser-read-position parser))))
    (cond ((>= pos 0)
	   (if (and (html-whitespace-p (char input pos))
                    (> pos 0) (html-whitespace-p (char input (1- pos))))
	     (loop for ppos from (1- pos) downto 0
                   for pchar = (char input (1- ppos))
                   while (html-whitespace-p pchar)
                   finally (setf (parser-read-position parser)
                                 (max ppos 0))
                           (return (and (> ppos 0) pchar)))
             (and (> pos 0) (char input (1- pos)))))
          (t (error "Unread beyond beginning of input")))))

(declaim (inline move-to-char))

(defun move-to-char (parser pos)
  (setf (parser-read-position parser) pos))

;;; Skip whitespace

(declaim (inline html-skip-whitespace))

(defun html-skip-whitespace (parser)
  (when (html-whitespace-p (peek-html-char parser))
    (next-html-char parser)
    t))

;;; Primitive parse functions

;;; Token string parsing

(defun parse-rest-html-token-string (parser rest-char-pred processing-fn)
  (do ((str-start (1- (parser-read-position parser)))
       (char (next-html-char parser)
	     (next-html-char parser)))
      ((not (funcall rest-char-pred char))
       (and char (previous-html-char parser))
       (funcall processing-fn (parser-input parser) str-start
                (parser-read-position parser)))))

(defun parse-html-token-string (parser 1st-char-pred rest-char-pred 
				processing-fn)
  (let ((1st-char (next-html-char parser)))
    (cond ((funcall 1st-char-pred 1st-char)
	   (parse-rest-html-token-string parser rest-char-pred processing-fn))
	  (t (previous-html-char parser)
	     nil))))

;;; Literal parsing

(defun parse-html-literal (parser &optional (processing-fn #'subreference))
  (let ((literal-start-char (next-html-char parser)))
    (and (or (char= literal-start-char #\")
	     (char= literal-start-char #\'))
	 (do ((start-pos (parser-read-position parser))
	      (char (next-html-char parser)
		    (next-html-char parser)))
	     ((or (null char)
		  (when (and (> (parser-read-position parser) start-pos)
			     (or (eql char #\>) (eql char #\<)))
		    (previous-html-char parser)
		    t)
		  (eql char literal-start-char))
	      (funcall processing-fn (parser-input parser) start-pos
		       (if (eql char literal-start-char)
			   (1- (parser-read-position parser))
			   (parser-read-position parser))))))))

;;; Parsing CDATA

(defun parse-html-cdata (parser)
  ;; One might wish to modify this function to make it more robust against
  ;; the appearance of malformed attribute values
  (let ((char (next-html-char parser)))
    (when char
      (previous-html-char parser)
      (case char
	((#\" #\')
	 (parse-html-literal parser))
	(t (parse-html-token-string parser #'cdata-char-p #'cdata-char-p
                                    #'subreference))))))

;;; Name parsing

(defun parse-html-name (parser &optional (processing-fn #'subreference))
  (parse-html-token-string parser #'html-name-start-char-p #'html-name-char-p
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

(defun parse-html-number (parser)
  (parse-html-token-string parser #'html-digit-char-p #'html-digit-char-p
			   #'parse-number))

;;; Number token parsing

(defun parse-html-number-token (parser
				&optional (processing-fn #'subreference))
  (parse-html-token-string parser #'html-digit-char-p #'html-name-char-p
			   processing-fn))

;;; Name token parsing

(defun parse-html-name-token (parser &optional (processing-fn #'subreference))
  (parse-html-token-string parser #'html-name-char-p #'html-name-char-p
			   processing-fn))

;;; Character data

(defun read-cdata-text (parser start-pos)
  (let ((input (parser-input parser))
	(pos (parser-read-position parser))
        (input-length (length (parser-input parser))))
    (when (< pos input-length)
      (loop
	  for npos from pos below input-length
	  until (when (and (eql (char input npos) #\<)
			   (> (- input-length npos) 1)
			   (eql (char input (1+ npos)) #\/))
		  (move-to-char parser (+ npos 2))
		  (eq (name (parser-cdata-element parser))
		      (parse-html-name parser #'tokenize-name)))
	  finally (move-to-char parser npos)))	
    (setf (parser-cdata-element parser) nil)
    (values :cdata (subreference input
				 start-pos (parser-read-position parser))
	    start-pos (parser-read-position parser))))

(defun read-pcdata-text (parser start-pos)
  (move-to-char parser (1+ start-pos))
  (do ((char (next-html-char parser) (next-html-char parser)))
      ((or (null char) (eql char #\<) (eql char #\&))
       (and char
	    (previous-html-char parser))
       (values :pcdata (subreference (parser-input parser)
				     start-pos (parser-read-position parser))
               start-pos (parser-read-position parser)))))

;;; Open tag

(defun initialize-parse-dispatch-functions ()
  (setf (get-value #t"CDATA" :quoted-parser) #'parse-html-literal
	(get-value #t"CDATA" :unquoted-parser) #'parse-html-cdata
	(get-value #t"NAME" :quoted-parser) 
	#'(lambda (parser)
	    (parse-html-literal parser #'tokenize-name))
	(get-value #t"NAME" :unquoted-parser)
	#'(lambda (parser)
	    (parse-html-name parser #'tokenize-name))
	(get-value #t"NUMBER" :quoted-parser)
	#'(lambda (parser)
	    (parse-html-literal parser #'parse-number))
	(get-value #t"NUMBER" :unquoted-parser) #'parse-html-number))

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

(defun parse-quoted-attribute-value (parser attr-defn)
  (if attr-defn
      (etypecase (allowed-values attr-defn)
	(html-name-token
	 (funcall (get-value (allowed-values attr-defn) :quoted-parser)
		  parser))
	(list (parse-html-literal parser #'tokenize-name)))
    ;; No attribute definition - assume CDATA
    (parse-html-literal parser)))

(defun parse-unquoted-attribute-value (parser attr-defn)
  (if attr-defn
      (etypecase (allowed-values attr-defn)
	(html-name-token
         (funcall (get-value (allowed-values attr-defn) :unquoted-parser)
		  parser))
	(list (parse-html-name parser #'tokenize-name)))
    (parse-html-cdata parser)))

(defun parse-html-attribute-value (parser attribute-name tag-defn rattr-list)
  ;; The general form of the input is <attr-name>[ws]=[ws]
  ;; Given that the last whitespace is optional, the current
  ;; position should be such that either a next-html-char or
  ;; a peek-html-char ought to return the first value character.
  (let* ((attr-defn (when tag-defn
		      (tag-attribute-definition attribute-name tag-defn)))
	 (attr-val (case (peek-html-char parser)
		     ((#\' #\") (parse-quoted-attribute-value parser 
							      attr-defn))
		     (t (parse-unquoted-attribute-value parser attr-defn)))))
    (cond (attr-val
	   (html-skip-whitespace parser)
	   (parse-html-open-tag-attributes
	    parser tag-defn (acons (or attr-defn attribute-name) attr-val
				   rattr-list)))
	  (t (nreverse rattr-list)))))

(defun parse-html-attr-name-or-val (parser tag-defn rattr-list)
  (let ((name-or-val (parse-html-name parser #'tokenize-name)))
    (labels ((acons-rattr-list ()
	       (acons (if tag-defn
			  (tag-value-attribute name-or-val tag-defn)
			name-or-val)
		      name-or-val rattr-list)))
      (cond (name-or-val
	     (html-skip-whitespace parser)
	     (let ((next-char (next-html-char parser)))
	       (when nil		; (eq name-or-val #t"align")
		 (break))
	       (cond ((null next-char)
		      ;; out of input, name-or-val must be val
		      (nreverse (acons-rattr-list)))
		     ((eql next-char #\=)
		      ;; name-or-val must be an attribute name
		      (html-skip-whitespace parser)
		      (parse-html-attribute-value parser name-or-val tag-defn 
						  rattr-list))
		     ((or (eql next-char #\") (eql next-char #\'))
		      ;; name-or-val must be an attribute name
		      ;; Only the first is a valid SGML marker. The remaining
		      ;; have been included for error recovery
		      (previous-html-char parser)
		      (parse-html-attribute-value parser name-or-val tag-defn
						  rattr-list))
		     ((eql next-char #\>)
		      ;; must be close of tag, so return
		      (nreverse (acons-rattr-list)))
		     ((eql next-char #\<)
		      ;; start of next tag
		      (previous-html-char parser)
		      (nreverse (acons-rattr-list)))
		     ((html-name-start-char-p next-char)
		      ;; name-or-val must be val, since we are starting another
		      ;; html name
		      (previous-html-char parser)
		      (parse-html-open-tag-attributes parser tag-defn 
						      (acons-rattr-list)))
		     (t
		      ;; something else entirely unexpected, so signal error
		      #+ignore
		      (cerror "Close tag now"
			      "Unexpected char ~A while parsing tag opening"
			      next-char)
		      (nreverse (acons-rattr-list))))))
	    (t
	     ;; something else entirely unexpected, so signal error
	     #+ignore
	     (cerror "Close tag now"
		     "Could not parse name-or-value while parsing tag open")
	     (nreverse rattr-list))))))

(defun parse-html-open-tag-attributes (parser tag-defn &optional rattr-list)
  ;; This function should be entered only on one of the following states:
  ;; 1. Read tag name and skipped whitespace
  ;; 2. Read attribute value and skipped whitespace
  ;; 3. Read attribute name = attribute value and skipped whitespace
  ;; If a malformed attribute or value is found, the tag is closed and
  ;; returned.
  (case (next-html-char parser)
    ((nil #\>) (nreverse rattr-list))
    (t (previous-html-char parser)
       (parse-html-attr-name-or-val parser tag-defn rattr-list))))

(defun make-tag-instance (tag-name tag-defn parser)
  (and tag-name
       (if tag-defn
	   (make-instance (parser-tag-instance-class parser)
			  :instance-of tag-defn)
	   (make-instance (parser-unknown-instance-class parser)
			  :instance-of (tag-definition #t"UNKNOWN")
			  :name tag-name))))

(defun parse-html-open-tag (parser)
  (declare (values tk-type tag-instance))
  (let* ((tag-name (parse-html-name parser #'tokenize-name))
	 (tag-defn (and tag-name (tag-definition tag-name)))
	 (tag-instance (and tag-name
			    (make-tag-instance tag-name tag-defn parser))))
    (when tag-instance
      #+debug (push tag-instance (instances (tag-definition tag-name)))
      (html-skip-whitespace parser)
      (setf (attr-values tag-instance)
	(parse-html-open-tag-attributes parser (tag-definition tag-name)))
      (when (member #t"CDATA" (contains tag-instance))
	(setf (parser-cdata-element parser) tag-instance))
      (values :open-tag tag-instance))))

;;; Comment

;; Once again, I am not interested in comments, and shall simply
;; ignore their existence
#|
(defun parse-rest-html-comment (&optional rstrings)
  ;; The start of this function is when the initial -- has been read
  (declare (values tk-type tk-value)
           #-ignore (ignore rstrings))
  (let ((com-str (parse-html-comment-string)))
    #-ignore (declare (ignore com-str))
    (html-skip-whitespace)
    (case (next-html-char)
      (#\- (when (eql (next-html-char) #\-)
             (parse-rest-html-comment #+ignore (cons com-str rstrings))))
      (#\> (values :comment #+ignore (nreverse (cons com-str rstrings))
                   #-ignore nil)))))
|#

(defun parse-broken-html-comment-close (parser char)
  (case char
    ((nil) (parser-read-position parser)) ; Exhausted input
    (#\-				; Possible comment close start
     (let ((com-close-start (parser-read-position parser)))
       (cond ((and (eql (next-html-char parser) #\-)
		   (progn (html-skip-whitespace parser)
			  (eql (next-html-char parser) #\>)))
	      com-close-start)
	     (t (move-to-char parser com-close-start)
		nil))))))

(defun parse-broken-html-comment-string (parser)
  (do* (#+ignore (com-start (parser-read-position parser))
        (char (next-html-char parser) (next-html-char parser))
	(comment-close-start (parse-broken-html-comment-close parser char)
			     (parse-broken-html-comment-close parser char)))
       (comment-close-start
        #+ignore (subreference (parser-input parser) com-start
                               (if (>= (parser-read-position parser)
                                       (length (parser-input parser)))
                                 comment-close-start
                                 (1- comment-close-start))))))

(defun parse-html-comment-string (parser)
  (do (#+ignore (com-start (parser-read-position parser))
       (char (next-html-char parser) (next-html-char parser))
       (last-char nil char))
      ((or (null char)
	   (and (eql char #\-) (eql last-char #\-)))
       #+ignore (subreference (parser-input parser) com-start
			      (if (null char)
				  (parser-read-position parser)
				  (- (parser-read-position parser) 2))))))

(defun parse-rest-html-comment (parser &optional rstrings)
  ;; The start of this function is when the initial -- has been read
  ;; This version of the comment reader is to handle cases where
  ;; authors have written broken comment strings. The end test for
  ;; such a comment string shall be --{ws}>
  (declare (values tk-type tk-value)
	   #-ignore (ignore rstrings))
  (let ((com-str (parse-broken-html-comment-string parser)))
    #-ignore (declare (ignore com-str))
    (values :comment
            #+ignore (list rstrings)
	    #-ignore nil)))

;;; Declaration

;; I could make things here into tokens, but why bother if I'm not going
;; to ever look at them? In fact, why even take a subseq?
;; In fact, I'm not even going to bother building up a return value right
;; now, though I have put in forms for how they would otherwise be
;; constructed

(defun no-processing-action (string &optional start end)
  (declare (ignore string start end))
  t)

(defun parse-rest-html-declaration (parser #+ignore rdecl)
  (declare (values tk-type tk-value))
  (do ((char (next-html-char parser)
	     (next-html-char parser)))
      ((or (null char) (eql char #\>))
       (values :declaration
	       #+ignore (nreverse rdecl)
	       #-ignore nil))
    (case char
      (#\-
       (cond ((eql (next-html-char parser) #\-)
	      #+ignore (push (cons :comment (parse-html-comment-string parser))
			     rdecl)
	      #-ignore (parse-html-comment-string parser)
	      (html-skip-whitespace parser))
	     (t (previous-html-char parser)
		(return-from parse-rest-html-declaration nil))))
      ((#\" #\')
       (previous-html-char parser)
       #+ignore (push (parse-html-literal parser #'no-processing-action) rdecl)
       #-ignore (parse-html-literal parser #'no-processing-action)
       (html-skip-whitespace parser))
      (t
	(previous-html-char parser)
	(cond ((html-name-char-p char)
	       #+ignore (parse-rest-html-declaration
                         parser
                         (cons (parse-html-name-token #'no-processing-action)
                               rdecl))
	       #-ignore (parse-html-name-token parser #'no-processing-action)
	       #-ignore (parse-rest-html-declaration parser)
	       (html-skip-whitespace parser))
	      (t (return-from parse-rest-html-declaration
		   (values :declaration
                           #+ignore (nreverse rdecl)
			   #-ignore nil))))))))

(defun parse-html-declaration (parser)
  (declare (values tk-type tk-value))
  (case (next-html-char parser)
    ((nil #\>) (values :comment nil))
    (#\- (when (eql (next-html-char parser) #\-)
	   (parse-rest-html-comment parser)))
    (t (previous-html-char parser)
       (let ((decl-name (parse-html-name parser #'no-processing-action)))
	 (when decl-name
	   (html-skip-whitespace parser)
	   (parse-rest-html-declaration parser #+ignore (list decl-name)))))))

;;; Close tag

(defun parse-html-close-tag (parser)
  (declare (values tk-type tk-value))
  (let ((tag-name (parse-html-name parser #'tokenize-name)))
    (when tag-name
      (html-skip-whitespace parser)
      (case (next-html-char parser)
	((nil #\>)
	 (values :close-tag tag-name))
	(t
	  ;; Accept the erroneous close tag anyway
	  (previous-html-char parser)
	  (values :close-tag tag-name))))))

;;; Entity reference

(defun parse-html-entity (parser)
  (declare (values tk-type tk-value))
  (let ((entity-name (parse-html-name parser #'tokenize-entity)))
    (when (html-entity-p entity-name)
      (let ((nchar (next-html-char parser)))
        (cond ((or (null nchar) (eql nchar #\;))
	       (values :entity entity-name))
	      ((html-whitespace-p nchar)
               (previous-html-char parser)
	       (values :entity entity-name))
	      (t nil))))))

;;; Character reference

(defun parse-char-ref (parser)
  (declare (values tk-type tk-value))
  (let ((char-no (parse-html-number parser)))
    (when char-no
      (let ((nchar (next-html-char parser)))
	(cond ((or (null nchar) (eql nchar #\;))
	       (values :character char-no))
	      ((html-whitespace-p nchar)	
               (previous-html-char parser)
               (values :character char-no))
	      (t
	       ;; I have not attempted an error recovery from this
	       ;; situation, should I bother?
	       nil))))))

;;; Parsing branch control routines

(defun parse-entity-or-char-ref (parser)
  (case (next-html-char parser)
    ((nil) nil)
    (#\# (parse-char-ref parser))
    (t (previous-html-char parser)
       (parse-html-entity parser))))

(defun parse-tag-decl-comment (parser)
  (case (next-html-char parser)
    ((nil) nil)
    (#\/ (parse-html-close-tag parser))
    (#\! (parse-html-declaration parser))
    (t (previous-html-char parser)
       (parse-html-open-tag parser))))

;;; Tokenizer - Primary function

(defun next-html-token (parser &aux (start-pos (parser-read-position parser)))
  (if (parser-cdata-element parser)
      (read-cdata-text parser start-pos)
    (let ((char (next-html-char parser)))
      (case char
	((nil) (values *eof* nil start-pos (parser-read-position parser)))
	(#\& (multiple-value-bind
		 (type val)
		 (parse-entity-or-char-ref parser)
	       
	       (if type
		   (values type val start-pos (parser-read-position parser))
		 (read-pcdata-text parser start-pos))))
	(#\< (multiple-value-bind
		 (type val)
		 (parse-tag-decl-comment parser)
	       
	       (if type
		   (values type val start-pos (parser-read-position parser))
		 (read-pcdata-text parser start-pos))))
	(t (read-pcdata-text parser start-pos))))))

;;; Test function
  
#|
(defun test-reader (fname)
  (with-open-file (fstr fname :direction :input)
    (do ((type nil)
	 (item nil))
	((eql type *eof*)
	 (break))
      (multiple-value-setq (type item)
	(next-html-token fstr))
      (format t "~A: ~A~%" type item))))
|#

(defun test-reader (string)
  (do ((parser (make-parser :input string))
       (type nil)
       (item nil))
      ((eql type *eof*))
    (multiple-value-setq (type item)
      (next-html-token parser))
    (format t "~A: ~A~%" type item)))

;;; EOF
