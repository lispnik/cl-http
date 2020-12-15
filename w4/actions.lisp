;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-

;;; Copyright John C. Mallery,  1995-96.
;;; All rights reserved.
;;;
;;; ACTION DEFINITIONS
;;;
;;; This file contains definitions for the following kinds of actions:
;;;
;;;             HTML Generation
;;;             Standard Actions
;;;     
;;;------------------------------------------------------------------- 
;;;
;;; HTML GENERATING ACTIONS
;;;
(in-package :w4)

;; Yo, buddy! Check this action out.
(define-action-type
  html-with-enumeration
  (:encapsulating
    :class encapsulating-action-type
    :documentation "An action that wraps the enumeration of items on STREAM according to STYLE.
STYLE can be :ENUMERATE :ITEMIZE :PLAIN :MENU :DIRECTORY :DEFINITION")
  (action activity url stream style)
  (html:with-enumeration (stream style)
    (call-next-action action url activity)))

(define-action-type
  html-enumerating-item
  (:encapsulating
    :class encapsulating-action-type
    :documentation "An action that wraps a single enumeration of an item on STREAM.")
  (action activity url stream)
  (html:enumerating-item (stream)
    (declare (ignore stream))
    (call-next-action action url activity)))

(define-action-type
  html-with-paragraph
  (:encapsulating
    :class encapsulating-action-type
    :documentation "An action that wraps a paragraph on STREAM.")
  (action activity url stream)
  (html:with-paragraph (:stream stream)
    (call-next-action action url activity)))

(define-action-type
  html-force-output
  (:standard :documentation "Forces outout on an HTML stream.")
  (action activity url stream)
  (declare (ignore action activity url))
  (force-output stream))

(define-action-type
  html-write-headers
  (:standard
    :documentation "Write the headers formatted verbatim.")
  (action activity url stream)
  (declare (ignore action))
  (multiple-value-bind (headers)
      (get-resource-headers activity url)
    (when headers
      (html:with-verbatim-text (:stream stream)
        (print-headers stream headers t)))))

(define-action-type
  html-with-section
  (:encapsulating
    :class encapsulating-action-type
    :documentation "An action that wraps reports on STREAM  within a section heading of depth LEVEL.
HEADING can be a string or a function called with (stream).")
  (action activity url stream heading level)
  (html:with-section-heading (heading :level level :stream stream)
    (call-next-action action url activity)))


;;;------------------------------------------------------------------- 
;;;
;;; ACTIONS
;;;

(define-action-type
  generate-inferiors
  (:generator
    :documentation "Primary action for walking the inferiors of a URL."
    :class generator-type)
  (action activity url)
  (declare (ignore action))
  (url-inferiors activity url))

(define-action-type
  generate-sorted-inferiors
  (:generator
    :documentation "Primary action for walking the inferiors of a URL."
    :class generator-type)
  (action activity url predicate)
  (declare (ignore action))
  (values (stable-sort (url-inferiors-satisfying-activity url activity) predicate)
	  t))

(define-action-type
  trace
  (:standard
    :documentation "An action that traces the activity of the Web walker.")
  (action activity url)
  (declare (ignore action))
  (let ((stream (report-stream activity))
        (depth (depth)))
    (format stream "~&Walk Depth: ~D  - " depth)
    (dotimes (n depth)
      (write-char #\space stream))
    (write-name url stream)))

(define-action-type
  html-trace
  (:standard
    :documentation "An action that traces the activity of the Web walker 
and outputs HTML on STREAM.")
  (action activity url stream)
  (declare (ignore action activity))
  (html:with-paragraph (:stream stream)
    (format stream "~&Walk Depth: ~D  - " (depth))
    (html:note-anchor (name-string url) :reference url :stream stream))
  (force-output stream))

(define-action-type
  trace-headers
  (:standard
    :documentation "An action that traces the headers of URL accessed by the Web walker.")
  (action activity url)
  (declare (ignore action))
  (let ((*headers* (get-resource-headers activity url)))
    (when *headers*
      (let ((stream (report-stream activity))
            (depth (depth)))
        (fresh-line stream)
        (dotimes (n (+ depth 2 (length (format nil "Walk Depth: ~D  - " depth))))
          (write-char #\space stream))
        #+genera
        (scl:indenting-output (stream `(,(cdr (si:cursorpos stream)) :character))
          (print-headers stream))
        #-genera
        (print-headers stream)))))

#|
(define-action-type
  conditional-action
  (:standard
    :documentation "An action that dispatches to other actions based on constraints.")
  (action activity url constraint-spec)
  (declare (ignore action))
  (loop for (antecedent consequent) in constraint-spec
	when (or (eql antecedent 't) (satisfies-constraints-p url activity consequent))
	  do (loop for act in consequent
		   do (perform-action url act activity))))
|#
