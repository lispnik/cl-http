;;; -*- Syntax: Ansi-Common-Lisp; Package: html-parser; Base: 10; Mode: lisp -*-

;;; File: rewrite-engine.lisp
;;; Last edited by smishra on Wed Jul 30 23:23:39 1997

;;; (c) Copyright 1996-97, Sunil Mishra (smishra@cc.gatech.edu)
;;;     All Rights Reserved

(in-package :html-parser)

;;; This is a simple rule based list rewriting system that relies on a
;;; pattern matcher capable of matching segment variables. This is a one
;;; time operation that shall be done at compile time, so I have not
;;; bothered to do any kind of optimization of this code.

;;;----------------------------------------
;;; Parameters

(defparameter *rewrite-rules* nil)
(defparameter *processing-instructions* nil)

;;;----------------------------------------
;;; Data structures

(defstruct (rewrite-rule (:conc-name "REWRITE-"))
  pattern result context)

;;;----------------------------------------
;;; Top level forms

(defmacro define-rewrite (context pattern result)
  `(push (make-rewrite-rule :pattern ',pattern :result ',result
                            :context ',context)
         *rewrite-rules*))

(defun rewrite-sexp (sexp context &key recursive (duplicates t) (bindings no-bindings))
  (if recursive
      (rewrite-sexp-recursive sexp context duplicates bindings)
      (rewrite-sexp-non-recursive sexp context duplicates bindings)))

(defun rewrite-sexp-recursive (sexp context duplicates bindings &optional (start-p t))
  (when (consp sexp)
    (setf (car sexp) (rewrite-sexp-recursive (car sexp) context duplicates bindings))
    (rewrite-sexp-recursive (cdr sexp) context duplicates bindings nil))
  (when start-p
    (rewrite-sexp-non-recursive sexp context duplicates bindings)))

(defun rewrite-sexp-non-recursive (sexp context duplicates bindings)
  (loop with rule and new-bindings
        for result-sexp = sexp then (construct-rewrite (rewrite-result rule) new-bindings)
        do (multiple-value-setq (rule new-bindings)
             (fetch-matching-rule result-sexp context bindings))
	while rule
	finally (return (if duplicates
                          result-sexp
                          (delete-duplicates-recursive result-sexp)))))

(defun fetch-matching-rule (sexp context bindings)
  (loop for rule in *rewrite-rules*
        for new-bindings = (when (eq (rewrite-context rule) context)
			     (pat-match (rewrite-pattern rule) sexp bindings))
        when new-bindings
          return (values rule new-bindings)))

(defun construct-rewrite (rewrite bindings)
  (flatten (execute-processing-instructions
	    (nsublis bindings (copy-tree rewrite)))
	   :test #'(lambda (item) (and (consp item) (eql (car item) '?append)))
	   :key #'cadr))

;;;----------------------------------------
;;; Processing instructions

(defun execute-processing-instructions (tree)
  (etypecase tree
    (cons
     (etypecase (car tree)
       (cons
	(setf (car tree) (execute-processing-instructions (car tree)))
	(if (processing-instruction-p (caar tree))
	    (setf (car tree) (dispatch-processing-instruction (car tree)))))
       (atom nil))
     (execute-processing-instructions (cdr tree))
     tree)
    (atom tree)))

(defun processing-instruction-p (sym)
  (assoc sym *processing-instructions*))

(defun dispatch-processing-instruction (instruction)
  (apply (cdr (assoc (car instruction) *processing-instructions*))
	 (cdr instruction)))

(defun define-processing-instruction (instruction fn)
  (push (cons instruction fn) *processing-instructions*))

(defun execute-mapping-instruction (item list)
  (mapcar #'(lambda (list-item)
	      (list item list-item))
	  list))

(define-processing-instruction '?map #'execute-mapping-instruction)

(defun delete-duplicates-recursive (list)
  (etypecase list
    (cons
     (etypecase (car list)
       (cons
	(setf (car list) (delete-duplicates-recursive (car list))))
       (atom nil))
     (delete-duplicates list :test #'equal))
    (atom list)))
