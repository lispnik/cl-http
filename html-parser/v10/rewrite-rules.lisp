;;; -*- Syntax: Ansi-Common-Lisp; Package: html-parser; Base: 10; Mode: lisp -*-

;;; File: rewrite-rules.lisp
;;; Last edited by smishra on Mon Jul 27 20:52:56 1998

;;; (c) Copyright 1996-97, Sunil Mishra (smishra@cc.gatech.edu)
;;;     All Rights Reserved

(in-package :html-parser)

;;; This file contains some simple rules that shall be used in
;;; reorganizing and processing the HTML element containment
;;; rules. I do not guarantee their completeness or accuracy,
;;; they are simply heuristics that I think ought to make program
;;; processing simpler.

(define-rewrite :dtd-parser
  (:or ?x)
  ?x)
(define-rewrite :dtd-parser
  (:and ?x)
  ?x)
(define-rewrite :dtd-parser
  (:set ?x)
  ?x)
(define-rewrite :dtd-parser
  (:sequence ?x)
  ?x)

(define-rewrite :dtd-parser
  (? (* ?x))
  (* ?x))
(define-rewrite :dtd-parser
  (? (+ ?x))
  (* ?x))

(define-rewrite :dtd-parser
  (* (:set (?* ?x)))
  (* (:or ?append ?x)))
(define-rewrite :dtd-parser
  (+ (:set (?* ?x)))
  (+ (:or ?append ?x)))

(define-rewrite :dtd-parser
  (* (:or (?* ?x) (* ?y) (?* ?z)))
  (* (:or ?append ?x ?y ?append ?z)))
(define-rewrite :dtd-parser
  (+ (:or (?* ?x) (+ ?y) (?* ?z)))
  (+ (:or ?append ?x ?y ?append ?z)))
(define-rewrite :dtd-parser
  (* (:or (?* ?x) (+ ?y) (?* ?z)))
  (* (:or ?append ?x ?y ?append ?z)))
(define-rewrite :dtd-parser
  (+ (:or (?* ?x) (* ?y) (?* ?z)))
  (* (:or ?append ?x ?y ?append ?z)))

(define-rewrite :dtd-parser
  (?op (?* ?x) (?op (?* ?y)) (?* ?z))
  (?op ?append ?x ?append ?y ?append ?z))

(define-rewrite :dtd-parser
  (:set (?* ?x) (:and (?* ?y)) (?* ?z))
  (:and ?append ?y
	?prefix ? ?x
	?prefix ? ?z))

(define-rewrite :parser-defn
  ((?* ?pre-exit) (?exit-context (?* ?x)) (?* ?post-exit))
  (?append ?pre-exit
   (unless ignore-exits
     (setq ?exitp-var t
	   ?exit-value-var (progn . ?x)))))

(define-rewrite :parser-defn
  (?save ?it)
  (?save ?it ?its-pd))
