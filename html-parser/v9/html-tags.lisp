;;; -*- Syntax: Ansi-Common-Lisp; Package: html-parser; Base: 10; Mode: lisp -*-

;;; File: html-tags.lisp
;;; Last edited by smishra on Thu Oct 22 17:21:27 1998

;;; (c) Copyright 1996-98, Sunil Mishra (smishra@cc.gatech.edu)
;;;     All Rights Reserved

(in-package :html-parser)

;;; Contains code and definitions for handling tags and the DTD.

;;;----------------------------------------
;;; Constants & Variables

(defconstant *combination-operators* '(:or :set :sequence :and))
(defconstant *unary-operators* '(+ * ?))

(defconstant *character-data-classes* (list #t"CDATA" #t"PCDATA"))
(defvar *pcdata-default-container* #t"p")

(defvar *html-root*)
(defvar *html-tags* nil)
(defvar *html-characters* nil)

;;;----------------------------------------

;;;----------------------------------------
;;; Character entity test predicate

(defun html-entity-p (entity)
  (member entity *html-characters*))

;;;----------------------------------------

;;;----------------------------------------
;;; Tag content analysis

;;; Assumption:
;;; The operators :set, :and, :or and :sequence cannot interact
;;; with *, +, ?

(declaim (type function rewrite-sexp))

(defun tokenize-sgml-expression (content symbol-to-list-p)
  (etypecase content
    (null nil)
    (keyword content)
    (number content)
    (symbol (cond ((or (member content *combination-operators*)
		       (member content *unary-operators*))
		   (list 'quote content))
		  (symbol-to-list-p
		   `(list (tokenize-name ,(symbol-name content))))
		  (t `(tokenize-name ,(symbol-name content)))))
    (cons (cons 'list
                (loop for item in content
                      collect (tokenize-sgml-expression item nil))))))

;;;----------------------------------------

;;;----------------------------------------
;;; Attribute construction

(defun attribute-default-value-form (val)
  (typecase val
    (keyword val)
    (symbol (list 'tokenize-name (symbol-name val)))
    (string (list 'tokenize-name val))
    (t val)))

(defun collect-attr-values (values)
  (if (atom values)
      (tokenize-sgml-expression values nil)
      (cond ((keywordp (car values))
	     (assert (eq (car values) :or))
	     (tokenize-sgml-expression (cdr values) nil))
	    (t (tokenize-sgml-expression values nil)))))

(defun make-html-attribute-forms (attrs-list)
  (mapcar #'(lambda (attr)
	      `(make-instance 'html-attribute
		:name (tokenize-name ,(symbol-name (first attr)))
		:values ,(collect-attr-values (second attr))
		;; Are mime types case insensitive?
		:default-value ,(attribute-default-value-form (third attr))))
	  attrs-list))

;;;----------------------------------------

;;;----------------------------------------
;;; Entity substitution

;;; This process is quite brain dead, because it is easier to consider
;;; the substitution context in the function that actually does the
;;; construction bit

(defun substitute-html-entities (l)
  ;; Perhaps slightly inefficient, but I'm quite certain correct
  (cond ((null l) nil)
	((parameter-entity-name-p l) (substitute-html-entities (symbol-value l)))
	((atom l) l)
	((parameter-entity-name-p (car l))
	 (let ((entity-value (substitute-html-entities (symbol-value (car l)))))
	   (if (and (consp entity-value) (consp (car entity-value)))
	       (append entity-value (substitute-html-entities (cdr l)))
	       (cons entity-value (substitute-html-entities (cdr l))))))
	(t (let ((l-car (substitute-html-entities (car l)))
		 (l-cdr (substitute-html-entities (cdr l))))
	     (if (and (eq l-car (car l)) (eq l-cdr (cdr l)))
		 l
		 (cons l-car l-cdr))))))	

;;;----------------------------------------

;;;----------------------------------------
;;; Tag operations

(defun initialize-tags ()
  (mapc #'(lambda (html-tag)
	    #-debug (declare (ignore html-tag))
	    #+debug (setf (instances html-tag) nil))
	(mapcar #'tag-definition *html-tags*)))

(defmethod tag-attribute-definition ((attribute html-name-token) (tag-defn html-tag))
  (find attribute (attributes tag-defn) :key #'name))

(defmethod tag-attribute-definition ((attribute string) (tag-defn html-tag))
  (find (tokenize-name attribute) (attributes tag-defn) :key #'name))

(defun define-unknown-tag ()
  (let ((token (tokenize-name "UNKNOWN")))
    (push token *html-tags*)
    (setf (get-value token :tag-definition)
	  (make-instance 'html-tag
			 :name token
			 :contains nil
			 :inclusions nil
			 :exclusions nil
			 :default-container (tokenize-name "BODY")
			 :attributes nil))))

(defun set-html-tag-backpointers ()
  (loop with character-data-classes = *character-data-classes*
	for tag-token in *html-tags*
	for exclusions = (exclusions tag-token)
	for inclusions = (inclusions tag-token)
	for contains = (contains tag-token)
	do (loop for item in (flatten (list contains inclusions))
		 for content-tag = (if (or (symbolp item)
					   (member item character-data-classes))
				       nil
				       (tag-definition item))
		 when (and content-tag (not (member content-tag exclusions)))
		   do (push tag-token (containers content-tag))))
  (setq *html-root* (find-if #'null *html-tags* :key #'containers))
  #+debug (assert *html-root*))

(defun do-breadth-search (nodes child-fn do-fn &aux visited-nodes)
  "A generalized BFS algorithm that calls do-fn on a node, then
pushes the result of calling child-fn on node on the queue."
  (loop with q = (copy-list nodes)
        with q-end = (last q)
        for node = (pop q)
        until (or (null node) (funcall do-fn node))
	unless (member node visited-nodes)
          do (push node visited-nodes)
	     (let ((children (funcall child-fn node)))
	       (when children
		 (cond (q                     
			(setf (cdr q-end) (copy-list children))
			(setf q-end (last q-end))
			)
		       (t
			(setf q (copy-list children))
			(setf q-end (last q))))))))

(defun most-general-tag (tags &aux general-tag)
  (cond ((cdr tags)
	 (do-breadth-search
	     (list *html-root*)
	   #'(lambda (tag)
	       (unless (member tag *character-data-classes*)
		 (flatten (contains (tag-definition tag)) :key #'cdr)))
	   #'(lambda (tag)
	       (when (member tag tags)
		 (setq general-tag tag)
		 t)))
	 general-tag)
	(t (car tags))))

(defun set-html-tag-default-containers ()
  (dolist (tag *html-tags*)
    (unless (eq tag *html-root*)
      (let ((tag-definition (tag-definition tag)))
	(setf (default-container tag-definition) (most-general-tag (containers tag-definition)))))))

;;;----------------------------------------

;;;----------------------------------------
;;; Element construction

(defun make-single-html-element (name start-optional end-optional attributes content
				      inclusions exclusions)
  (let ((attribute-forms (make-html-attribute-forms (substitute-html-entities attributes)))
	(content (resolve-sgml-operators (substitute-html-entities content)))
	(inclusions (resolve-sgml-operators (substitute-html-entities inclusions)))
	(exclusions (resolve-sgml-operators (substitute-html-entities exclusions))))
    `((push (tokenize-name ,(symbol-name name)) *html-tags*)
      (setf (get-value (tokenize-name ,(symbol-name name)) :tag-definition)
       (make-instance 'html-tag
	:name (tokenize-name ,(symbol-name name))
	:attributes ,(when attribute-forms (cons 'list attribute-forms))
	:start-optional ,start-optional
	:end-optional ,end-optional
	:contains ,(tokenize-sgml-expression content t)
	:inclusions ,(tokenize-sgml-expression inclusions t)
	:exclusions ,(tokenize-sgml-expression exclusions t))))))

(defun make-multiple-html-elements (names start-optional end-optional attributes content
					  inclusions exclusions)
  (loop for name in (cdr (resolve-sgml-operators (substitute-html-entities names)))
	append (make-single-html-element name start-optional end-optional attributes content
					 inclusions exclusions)))

;;;----------------------------------------

;;;----------------------------------------
;;; Top level definitions

(defun resolve-sgml-operators (content)
  (etypecase content
    (cons (rewrite-sexp content :dtd-parser :recursive t :duplicates nil))
    (atom content)))

(defmacro define-html-element (name &key start-optional end-optional attributes content 
				    inclusions exclusions)
  (cons 'progn
	(if (or (consp name) (parameter-entity-name-p name))
	    (make-multiple-html-elements name start-optional end-optional attributes content
					 inclusions exclusions)
	    (make-single-html-element name start-optional end-optional attributes content
				      inclusions exclusions))))

(defmacro define-html-entity (name value)
  `(eval-when (load compile)
    (defparameter ,name ',value)))

(defun define-html-characters (&rest chars)
  (mapc #'(lambda (char)
	    (push (tokenize-entity char) *html-characters*))
	chars))

;;;----------------------------------------
