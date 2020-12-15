;;;   -*- Mode: LISP; Package: lambda-ir; BASE: 10; SYNTAX: ansi-common-lisp -*-

;;; (C) Copyright 1997, Andrew J. Blumberg (blumberg@ai.mit.edu).
;;;     All Rights Reserved.
;;;
;;;
;;;------------------------------------------------------------------- 
;;;
;;; Bit-vector stuff
;;;

(in-package :lambda-ir)

(defun increment-bit-vector-size (&optional (amount (bit-vector-increment)))
  (set-bit-vector-length (+ (bit-vector-length) amount)))

(defun make-bit-vector (&optional (size (bit-vector-length)) (initial-element 0))
  (make-array size :initial-element initial-element :element-type 'bit))

(defun bit-set-p (bit-vector index)
  (= (sbit bit-vector index) 1))

(defun set-bit (bit-vector index value)
  (setf (sbit bit-vector index) value))

(defun set-bit-extend (bit-vector index value &optional (increment (bit-vector-increment)))
  (let ((length (length bit-vector)))
    (if (> index length)
        (progn
          (increment-bit-vector-size (max (+ increment length) (1+ index)))
          (set-bit (copy-bit-vector bit-vector (make-bit-vector)) index value)
          :expanded)
        (set-bit bit-vector index value))))

(defun consing-expand-bit-vector (bit-vector &optional (size (bit-vector-length)))
  (when (< (length bit-vector) size)
    (let ((new-vector (make-bit-vector size)))
      (copy-bit-vector bit-vector new-vector))))

#|

(defun map-bit-vector (bit-vector function)
  (loop for idx upfrom 0 below (/ (length bit-vector) 32)
        do (let* ((bytes (make-array 1 :displaced-to bit-vector :displaced-index-offset (* idx 32)))
                  (byte-value (aref bytes 0)))
             (pprint idx)
             (pprint byte-value)
             (pprint (zerop byte-value))
             (cond ((zerop byte-value) nil)
                   (t (loop for bit-idx upfrom 0 below 32
                            for value = (ldb (byte 1 bit-idx) byte-value)
                            do (pprint bit-idx)
                            when (= value 1)
                              do (break)
                                 (funcall function (+ (* 32 idx) bit-idx))))))))

(defun reverse-map-bit-vector (bit-vector function)
  (loop for idx downfrom (1- (/ (length bit-vector) 32)) above -1
        do (let* ((bytes (make-array 1 :displaced-to bit-vector :displaced-index-offset (* idx 32)))
                  (byte-value (aref bytes 0)))
             (cond ((zerop byte-value) nil)
                   (t (loop for bit-idx downfrom 31 above -1
                            for value = (ldb (byte 1 bit-idx) byte-value)
                            when (= value 1)
                              do (funcall function (+ (* 32 idx) bit-idx))))))))

|#

(defun map-bit-vector (bit-vector function &optional (number-to-do (bit-vector-length)))
  (loop with counter = 0
	for idx from 0 below (length bit-vector)
        while (< counter number-to-do) 
	do (if (= (sbit bit-vector idx) 1)
               (progn
		 (incf counter)
                 (funcall function idx)))))

(defun reverse-map-bit-vector (bit-vector function &optional (number-to-do (bit-vector-length)))
  (loop with counter = 0
	for idx downfrom (1- (length bit-vector)) above -1
        while (< counter number-to-do)
	do (if (= (sbit bit-vector idx) 1)
               (progn
		 (incf counter)
		 (funcall function idx)))))

(defun map-bit-vector-accumulating (bit-vector function initial-value &optional (number-to-do (bit-vector-length)))
  (let ((result initial-value))
    (flet ((handle-it (idx)
             (setq result (funcall function idx result))))
      (declare (dynamic-extent #'handle-it))
      (map-bit-vector bit-vector #'handle-it number-to-do))
    result))

(defun reverse-map-bit-vector-accumulating (bit-vector function initial-value &optional (number-to-do (bit-vector-length)))
  (let ((result initial-value))
    (flet ((handle-it (idx)
	     (setq result (funcall function idx result))))
      (declare (dynamic-extent #'handle-it))
      (reverse-map-bit-vector bit-vector #'handle-it number-to-do))
    result))

(defun copy-bit-vector (from-vector &optional to-vector)
  (let ((output (or to-vector (make-bit-vector (length from-vector)))))
    (when to-vector
      (zero-bit-vector to-vector))
    (flet ((do-it (idx)
             (setf (sbit output idx) 1)))
      (declare (dynamic-extent #'do-it))
      (map-bit-vector from-vector #'do-it (length from-vector))
      output)))

(defun bit-vector-empty-p (bit-vector)
  (loop for idx from 0 below (length bit-vector)
        always (= (sbit bit-vector idx) 0)
        finally (return-from bit-vector-empty-p t))
  nil)

(defun bit-vector-intersection (bit-vector-1 bit-vector-2)
  (bit-and bit-vector-1 bit-vector-2))

(defun decode-bit-vector (bit-vector &optional (direction :forward) (number-to-return nil))
  (let ((result nil)
        (count 0))
    (ecase direction
      (:forward
        (loop for idx upfrom 0 below (length bit-vector)
              while (or (not number-to-return) (< count number-to-return))
              do (when (= 1 (sbit bit-vector idx))
                   (push idx result)
                   (incf count))))
      (:reverse
        (loop for idx downfrom (1- (length bit-vector)) to 0
              while (or (not number-to-return) (< count number-to-return))
              do (when (= 1 (sbit bit-vector idx))
                   (push idx result)
                   (incf count)))))
    (reverse result)))

(defun bit-vector->sparse-array (bit-vector &key (adjustable nil))
  (let ((result (make-array (bit-vector-cardinality bit-vector) :element-type 'fixnum :adjustable adjustable :fill-pointer t)))
    (loop with count = 0
          for idx from 0 below (length bit-vector)
          do (if (= (sbit bit-vector idx) 1)
                 (progn
                   (setf (aref result count) idx)
                   (incf count))))
    result))

(defun sparse-array->bit-vector (sparse-array &optional (length (1+ (array-max-value sparse-array))))
  (let ((result (make-bit-vector length)))
    (loop for item across sparse-array
          do (setf (sbit result item) 1))
    result))

(defun list->bit-vector (list &optional (length (1+ (list-max-value list))))
  (let ((result (make-bit-vector length)))
    (loop for item in list
          do (setf (sbit result item) 1))
    result))

(defmethod sparse-bit-and ((first bit-vector) (second bit-vector) &optional third)
  (bit-and first second third))

(defmethod sparse-bit-and ((first array) (second bit-vector) &optional third)
  (let* ((length (length second))
	 (desparsified-vector (sparse-array->bit-vector first length)))
    (bit-and desparsified-vector second (or third desparsified-vector))))

(defmethod sparse-bit-ior ((first bit-vector) (second bit-vector) &optional third)
  (bit-ior first second third))

(defmethod sparse-bit-ior ((first array) (second bit-vector) &optional third)
  (let ((output (or third (copy second))))
    (loop for item across first
          do (setf (sbit output item) 1))
    output))

(defmethod sparse-bit-andc2 ((first bit-vector) (second bit-vector) &optional third)
  (bit-andc2 first second third))

(defmethod sparse-bit-andc2 ((first array) (second bit-vector) &optional third)
  (let* ((length (1- (length second)))
	 (desparsified-vector (sparse-array->bit-vector first length)))
    (bit-andc2 desparsified-vector second (or third desparsified-vector))))

(defmethod sparse-bit-not ((argument bit-vector) &optional third)
  (let ((output (or third (make-bit-vector (bit-vector-length)))))
    (bit-not argument output)))

(defmethod sparse-bit-not ((argument array) &optional third)
  (let ((output (or third (make-bit-vector (bit-vector-length) 1))))
    (loop for item across argument
          do (setf (sbit item output) 0))
    output))

(defun shift-bit-vector (bit-vector pivot &optional (insert 0))
  (loop for idx upfrom pivot below (length bit-vector)
        with current = 0
        for old = insert then current
        do (setf current (aref bit-vector idx))
           (setf (aref bit-vector idx) old)))
