;;;   -*- Mode: LISP; Package: lambda-ir; BASE: 10; SYNTAX: ansi-common-lisp -*-

;;; (C) Copyright 1997, Andrew J. Blumberg (blumberg@ai.mit.edu).
;;;     All Rights Reserved.
;;;
;;;
;;;------------------------------------------------------------------- 
;;;
;;; Various utility functions
;;;

(in-package :lambda-ir)

;;;------------------------------------------------------------------- 
;;;
;;; PLATFORM SPECIFIC CODE
;;;

#+MCL
(defun zero-bit-vector (bit-vector)
  (ccl::%simple-bit-boole boole-clr bit-vector bit-vector bit-vector))

#-MCL
(defun zero-bit-vector (bit-vector)
  (fill bit-vector 0))

#+Genera
(declaim (inline bit-vector-cardinality))

#+Genera
(defun bit-vector-cardinality (bit-vector)
  (scl:bit-vector-cardinality bit-vector))

#-Genera
(defun bit-vector-cardinality (bit-vector)
  (loop for idx fixnum from 0 below (length bit-vector)
        count (eq (sbit bit-vector idx) 1)))

#+Genera
(defun dump-forms-to-file (filename forms &optional file-attribute-list)
  (sys:dump-forms-to-file filename forms file-attribute-list))

;; Catch array bounds errors.
#+Genera
(import (intern "SUBSCRIPT-OUT-OF-BOUNDS" :sys) :lambda-ir)
;; Losing MCL implementation.
#+MCL
(defun subscript-out-of-bounds-trap-p (error)
  (with-slots (ccl::format-string) error
    (string-equal ccl::format-string "Array index ~S out of bounds for ~S .")))

#+MCL
(deftype subscript-out-of-bounds () `(satisfies subscript-out-of-bounds-trap-p))

;;;------------------------------------------------------------------- 
;;;
;;; PRIVATE DEFCLASS
;;;
;; a great way to lose!   11/23/97 -- JCMa.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %defclass-expand-slots (name-abbreviation slots)
    (flet ((nil-string (symbol)
             (typecase symbol
               (null "")
               (string (concatenate 'string symbol "-"))
               (symbol (concatenate 'string (symbol-name symbol) "-")))))

      (loop for slot in slots
            for string-slot = (string slot)
            collect `(,slot 
                      :initform nil 
                      :initarg ,(intern (string-upcase string-slot) :keyword) 
                      :accessor ,(intern (concatenate 'string 
                                                      (nil-string name-abbreviation)
                                                      string-slot)))))))

(defmacro define-class-local (name name-abbreviation inheritance-list &body slots)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name
               ,inheritance-list
         (,@(%defclass-expand-slots name-abbreviation slots)))))

(defmacro define-standard-value (name &optional default-value)
  (let* ((use-name (string name))
         (var-name (intern (concatenate 'string "*" use-name "*")))
         (set-var-function-name (intern (concatenate 'string "SET-" use-name)))
         (check-var-function-name name))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defparameter ,var-name ',default-value)
       (defun ,set-var-function-name (value)
         #+Genera (declare (sys:function-parent ,set-var-function-name define-standard-value))
         (setq ,var-name value))
       (declaim (inline ,check-var-function-name))
       (defun ,check-var-function-name ()
         #+Genera (declare (sys:function-parent ,check-var-function-name define-standard-value))
         ,var-name))))

(defgeneric parse-pdi (pdi)
  (:documentation "Returns the proper access list for PDI."))

(defmethod parse-pdi ((pdi pathname))
  (list (list :PATHNAME pdi)))

(defmethod parse-pdi (pdi)
  pdi)

(defun make-adjustable-array (&optional (size 0))
  (let ((array (make-array size :adjustable t :fill-pointer t)))
    (setf (fill-pointer array) 0)
    array))

(defmacro multi-vector-push-extend (&rest pair-list)
  `(progn . ,(loop for n upfrom 0 below (/ (length pair-list) 2)
                   for use-n = (* 2 n)
                   for pair = (list (elt pair-list use-n) (elt pair-list (1+ use-n)))
                   append `((vector-push-extend ,@pair)))))

(defun %create-unnamed-object (type)
  (make-instance type))

(defun subset (small-set larger-set)
  (loop for item in small-set
        always (member item larger-set)))

(defgeneric degenerate-member (item list)
  (:documentation "Handles the case when list is merely an item as equality testing."))

(defmethod degenerate-member (item (list list))
  (member item list :test 'equalp))

(defmethod degenerate-member ((item list) list)
  (not (loop for actual-item in item
             never (degenerate-member actual-item list))))

(defmethod degenerate-member (item list)
  (equalp item list))

(defgeneric set-intersection (list1 list2)
  (:documentation "Performs robust intersection of LIST1 and LIST2."))

(defmethod set-intersection ((list1 list) (list2 list))
  (intersection list1 list2 :test 'equalp))

(defmethod set-intersection (list1 list2)
  (degenerate-member list1 list2))

(defmacro set-a-list (a-list tag value)
  `(let ((already-there (assoc ,tag ,a-list)))
     (if already-there
         (setf (cdr already-there) ,value)
         (setf ,a-list (acons ,tag ,value ,a-list)))))
        
(defgeneric copy (item)
  (:documentation "Provides a copy of an item."))

(defmethod copy (item)
  item)

(defmethod copy ((item sequence))
  (copy-seq item))

(defun map-bitmask-accumulating (mask function initial-value)
  (let ((result initial-value))
    (flet ((handle-it (idx)
             (setq result (funcall function idx result))))
      (declare (dynamic-extent #'handle-it))
      (www-utils:map-bitmask mask #'handle-it))
    result))

(defmacro cond-every (&body clauses)
  `,(append
      '(progn)
      (loop for clause in clauses
            collect `(if ,(car clause)
                         ,@(cdr clause)))))

(defmacro listify (x)
  `(if (listp ,x)
       ,x
       (list ,x)))

(defun array-member (item array)
  (not
    (loop for thing across array
          never (equalp item thing))))

(defun array-max-value (vector)
  (loop for item across vector
        with max = 0
        do (when (> item max)
             (setf max item))
        finally (return max)))

(defun list-max-value (list)
  (loop for item in list
        with max = 0
        do (when (> item max)
             (setf max item))
        finally (return max)))

(defun binary-search (initial-top initial-bottom quantity-function test-function)
  (loop with top = initial-top
        with bottom = initial-bottom
        for mid = (+ bottom (round (- top bottom) 2))
        while (> (- top bottom) 1)
        for test-quantity = (funcall quantity-function mid)
        do (if (funcall test-function test-quantity)
               (setf top mid)
               (setf bottom mid))
        finally (return mid)))
