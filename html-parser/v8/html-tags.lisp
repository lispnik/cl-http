;;; -*- Syntax: Ansi-Common-Lisp; Package: html-parser; Base: 10; Mode: lisp -*-

;;; File: html-tags.lisp
;;; Last edited by smishra on Wed Jul 30 23:22:42 1997

;;; (c) Copyright 1996-97, Sunil Mishra (smishra@cc.gatech.edu)
;;;     All Rights Reserved

(in-package :html-parser)

;;;----------------------------------------
;;; Top level definitions

(defmacro define-html-element (name &key start-optional end-optional
				    attributes content default-container
				    inclusions exclusions)
  (cons 'progn
	(if (or (consp name) (parameter-entity-name-p name))
	    (make-multiple-html-elements name start-optional end-optional
					 attributes content default-container
					 inclusions exclusions)
	    (make-single-html-element name start-optional end-optional
				      attributes content default-container
				      inclusions exclusions))))

(defmacro define-html-entity (name value)
  `(eval-when (load compile)
    (defparameter ,name ',value)))

(defun define-html-characters (&rest chars)
  (mapc #'(lambda (char)
	    (push (tokenize-entity char) *html-characters*))
	chars))

;;;----------------------------------------
;;; Element construction

(defun make-single-html-element (name start-optional end-optional
				      attributes content default-container
				      inclusions exclusions)
  (let ((attribute-forms
	 (make-html-attribute-forms (substitute-html-entities attributes t)))
	(content
	 (resolve-sgml-operators (substitute-html-entities content)))
	(inclusions
	 (resolve-sgml-operators (substitute-html-entities inclusions)))
	(exclusions
	 (resolve-sgml-operators (substitute-html-entities exclusions))))
    `((push (tokenize-name ,(symbol-name name)) *html-tags*)
      (setf (get-value (tokenize-name ,(symbol-name name)) :tag-definition)
       (make-instance 'html-tag
	:name (tokenize-name ,(symbol-name name))
	:attributes ,(and attribute-forms (cons 'list attribute-forms))
	:start-optional ,start-optional
	:end-optional ,end-optional
	:contains ,(tokenize-sgml-expression content t)
	:default-container ,(and default-container
				 (list 'tokenize-name
				       (symbol-name default-container)))
	:inclusions ,(tokenize-sgml-expression inclusions t)
	:exclusions ,(tokenize-sgml-expression exclusions t))))))

(defun make-multiple-html-elements (names start-optional end-optional
					  attributes content default-container
					  inclusions exclusions)
  (apply #'append
	(mapcar #'(lambda (name)
		    (make-single-html-element
		     name start-optional end-optional attributes content
		     default-container inclusions exclusions))
		(cdr (resolve-sgml-operators
		      (substitute-html-entities names))))))

;;;----------------------------------------
;;; Attribute construction

(defun make-html-attribute-forms (attrs-list)
  (mapcar #'(lambda (attr)
	      `(make-instance
		'html-attribute
		:name (tokenize-name ,(symbol-name (first attr)))
		:values ,(collect-attr-values (second attr))
		;; Are mime types case insensitive?
		:default-value ,(attribute-default-value-form (third attr))))
	  attrs-list))

(defun attribute-default-value-form (val)
  (typecase val
    (keyword val)
    (symbol (list 'tokenize-name (symbol-name val)))
    (string (list 'tokenize-name val))
    (t val)))

(defun collect-attr-values (values)
  (let ((fvalues (flatten values
			  :key #'(lambda (vlist)
				   (assert (eql (car vlist) :or))
				   (cdr vlist)))))
    (assert (or (not (consp fvalues))
		(not (keywordp (car fvalues)))
		(eql (car fvalues) :or)))
    (if (and (consp fvalues) (eql (car fvalues) :or))
	(tokenize-sgml-expression (cdr fvalues))
	(tokenize-sgml-expression fvalues))))

;;;----------------------------------------
;;; Tag content analysis

;;; Assumption:
;;; The operators :set, :and, :or and :sequence cannot interact
;;; with *, +, ?

(declaim (type function rewrite-sexp))

(defun resolve-sgml-operators (content)
  (etypecase content
    (cons
     (rewrite-sexp content :dtd-parser :recursive t :duplicates nil))
    (atom content)))

(defun tokenize-sgml-expression (content &optional symbol-to-list-p)
  (etypecase content
    (null nil)
    (keyword content)
    (symbol (if (or (member content *combination-operators*)
                    (member content *unary-operators*))
              (list 'quote content)
              (if symbol-to-list-p
                `(list (tokenize-name ,(symbol-name content)))
                `(tokenize-name ,(symbol-name content)))))
    (cons (cons 'list
                (loop for item in content
                      collect (tokenize-sgml-expression item nil))))))

#|
(defun tokenize-sgml-expression-internal (content)
  (etypecase content
    (keyword `(,content))
    (symbol
      (if (or (member content *combination-operators*)
	      (member content *unary-operators*))
        (list 'quote content)
        `((tokenize-name ,(symbol-name content)))))))


(defun tokenize-sgml-expression (content)
  (etypecase content
    (null nil)
    (atom
      `(list (tokenize-sgml-expression-internal content)))
    (cons
      `(list ,.(loop for item in content
		     nconc (tokenize-sgml-expression item))))))
|#

;;;----------------------------------------
;;; Entity substitution

;;; This process is quite brain dead, because it is easier to consider
;;; the substitution context in the function that actually does the
;;; construction bit

(defun substitute-html-entities (l &optional attributes-p)
  ;; The attributes-p is a kluge to ensure that inserted attributes are
  ;; handled correctly
  (etypecase l
    (atom
     (if (parameter-entity-name-p l)
	 (substitute-html-entities (symbol-value l))
	 l))
    (cons
     (if attributes-p
	 (flatten (cons (substitute-html-entities (car l))
			(substitute-html-entities (cdr l)))
		  :test #'(lambda (item)
			    (and (consp item)
				 (consp (car item)))))
	 (cons (substitute-html-entities (car l))
	       (substitute-html-entities (cdr l)))))))

;;;----------------------------------------
;;; Character entity test predicate

(defun html-entity-p (entity)
  (member entity *html-characters*))

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

;;;----------------------------------------
;;; Initializations

;(defun set-html-tag-backpointers ()
;  (mapc #'(lambda (tag-token)
;	    (mapc #'(lambda (content-tag)
;		      (unless (member content-tag (exclusions tag-token))
;			(push tag-token (containers content-tag))))
;		  (mapcar #'tag-definition
;			  ;; On the premise that all tags are token names
;			  (delete-if #'(lambda (item)
;					 (or (symbolp item)
;					     (member item *character-data-classes*)))
;				     (flatten (list (contains tag-token)
;						    (inclusions tag-token)))))))
;	*html-tags*)
;  #+debug (assert (every #'(lambda (tag-defn)
;			     (or (null (default-container tag-defn))
;				 (member (default-container tag-defn)
;					 (containers tag-defn))))
;			 *html-tags*))
;  (setq *html-root* (find-if #'null *html-tags* :key #'containers))
;  #+debug (assert *html-root*))


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
  #+debug (assert (every #'(lambda (tag-defn)
			     (or (null (default-container tag-defn))
				 (member (default-container tag-defn)
					 (containers tag-defn))))
			 *html-tags*))
  (setq *html-root* (find-if #'null *html-tags* :key #'containers))
  #+debug (assert *html-root*))

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
