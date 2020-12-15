;;;   -*- Mode: LISP; Package: lambda-ir; BASE: 10; SYNTAX: ansi-common-lisp -*-

;;; (C) Copyright 1997, Andrew J. Blumberg (blumberg@ai.mit.edu).
;;;     All Rights Reserved.
;;;
;;;
;;;------------------------------------------------------------------- 
;;;
;;; Basic framework for computation objects
;;;

;; Static computation objects are expected to be computed infrequently.
;; Dynamic computation objects are expected to be recomputed constantly.
;; The difference makes itself felt mostly in the treatment of caching behavior.

(in-package :lambda-ir)
;; A variety of utilities are developed here to permit the initialization functions to be defined.

(defgeneric propagate-arguments (computations)
  (:documentation "Generates a list of all arguments needed for the sub-computations contained in COMPUTATIONS."))

(defmethod propagate-arguments ((computation base-computation-type))
  (with-slots (expected-arguments)
              computation
    expected-arguments))

(defmethod propagate-arguments ((computation composite-computation-type))
  (with-slots (subordinate-computations)
              computation
    (loop for subordinate in subordinate-computations
          append (propagate-arguments (substrate-type subordinate)))))

(defmethod propagate-arguments :around (computations)
  (declare (ignore computations))
  (remove-duplicates (call-next-method) :test 'equalp))

(defparameter *computation-registry* (make-hash-table :test 'equalp))
(defun %clear-registry ()
  (clrhash *computation-registry*))

(defgeneric register (name &optional item store-p)
  (:documentation "Interns ITEM via NAME in registry."))

(defmethod register (name &optional item (store-p nil))
  (if store-p
      (setf (gethash name *computation-registry*) item)
      (gethash name *computation-registry*)))

(defun pattern-match (argument-list names name-subset)
  (loop for n from 0 below (length names)
        append (if (member (elt names n) name-subset :test 'equalp) 
                   (list (elt argument-list n)))))

(defun partition-arguments (computations argument-names actual-arguments)
  (loop for computation in computations
        collect (with-slots (substrate-type)
                            computation
                  (with-slots (expected-arguments)
                              substrate-type
                    (pattern-match actual-arguments argument-names expected-arguments)))))

;; The code below permits computation sorting so as to permit reordering based on complexity.

(defgeneric get-sorting-factor (computation-or-computations)
  (:documentation "Computes a sorting factor for COMPUTATION-OR-COMPUTATIONS."))

(defmethod get-sorting-factor ((computation static-base-computation))
  (with-slots (substrate-type)
              computation
    (get-sorting-factor substrate-type)))

(defmethod get-sorting-factor ((computation dynamic-base-computation))
  (with-slots (substrate-type)
              computation
    (get-sorting-factor substrate-type)))

(defmethod get-sorting-factor ((computation composite-computation-type))
  (let ((output 0))
    (with-slots (subordinate-computations)
                computation
      (loop for computation in subordinate-computations
            do (setf output (max output (get-sorting-factor computation)))))
    output))

(defmethod get-sorting-factor ((computations list))
  (let ((output 0))
    (loop for computation in computations
          do (setf output (max output (get-sorting-factor computation))))
    output))

(defmethod get-sorting-factor ((computation base-computation-type))
  (sorting-factor computation))

;; Now the initialization code.

(defun make-base-computation-type (name in-code in-expected-arguments &optional (in-sorting-factor 0))
  (let ((computation (%create-unnamed-object 'base-computation-type)))
    (with-slots (object-name code expected-arguments sorting-factor)
                computation
      (setf object-name name
            code in-code
            sorting-factor in-sorting-factor
            expected-arguments in-expected-arguments))
    (register name computation t)))

(defun make-base-computation (name computation-type static-p)
  (let ((type-object (register computation-type)))
    (if type-object
        (let ((computation (if static-p 
                               (%create-unnamed-object 'static-base-computation)
                               (%create-unnamed-object 'dynamic-base-computation))))
          (with-slots (substrate-type object-name) 
                      computation
            (setf object-name name
                  substrate-type type-object)
            (initialize-cache computation))
          computation)
        (error "The type ~s is not a valid computation type." computation-type))))

(defun make-composite-computation-type (name in-code in-personal-arguments in-subordinate-computations &optional (in-sorting-factor 0))
  (let ((computation (%create-unnamed-object 'composite-computation-type)))
    (with-slots (object-name code expected-arguments inherited-arguments personal-arguments subordinate-computations sorting-factor)
                computation
      (setf object-name name
            code in-code
            sorting-factor (max in-sorting-factor (get-sorting-factor in-subordinate-computations))
            personal-arguments in-personal-arguments
            subordinate-computations in-subordinate-computations)
      (setf inherited-arguments (propagate-arguments computation))
      (setf expected-arguments (union personal-arguments inherited-arguments :test 'equalp)))
    (register name computation t)))

(defun make-composite-computation (name computation-type static-p)
  (let ((type-object (register computation-type)))
    (if type-object
        (let ((computation (if static-p 
                               (%create-unnamed-object 'static-composite-computation)
                               (%create-unnamed-object 'dynamic-composite-computation))))
          (with-slots (object-name substrate-type) 
                      computation
            (setf object-name name
                  substrate-type type-object)
            (initialize-cache computation))
          computation)
        (error "The type ~s is not a valid computation type." computation-type))))

;; An internal key which provides default caching of computations.

(defparameter *performance-storage-key* :default-comp)
(defgeneric perform-computation (computation arguments cache-p &optional storage-key)
  (:documentation "Performs COMPUTATION with ARGUMENTS, caching under STORAGE-KEY depending on CACHE-P."))

(defmethod perform-computation ((computation static-base-computation) arguments cache-p
                                &optional (storage-key *performance-storage-key*))
  (flet ((perform (computation arguments)
           (with-slots (substrate-type) computation
             (with-slots (code) substrate-type
               (apply code arguments)))))
    (declare (inline perform))
    (use-cached-value-if-available (computation storage-key cache-p)
      (perform computation arguments))))

(defmethod perform-computation ((computation dynamic-base-computation) arguments cache-p
                                &optional storage-key)
  (declare (ignore cache-p storage-key))
  (flet ((perform (computation arguments)
           (with-slots (substrate-type) computation
             (with-slots (code) substrate-type
               (apply code arguments)))))
    (declare (inline perform))
    (perform computation arguments)))

(defmethod perform-computation ((computation static-composite-computation) arguments cache-p
                                &optional (storage-key *performance-storage-key*))
  (flet ((perform (computation arguments)
           (with-slots (substrate-type) computation
             (with-slots (code subordinate-computations expected-arguments) substrate-type
               (apply code (loop for sub-computation in subordinate-computations
                                 for sub-arguments in (partition-arguments subordinate-computations expected-arguments arguments)
                                 collect (perform-computation sub-computation sub-arguments cache-p)))))))
    (declare (inline perform))
    (use-cached-value-if-available (computation storage-key cache-p)
      (perform computation arguments))))

(defmethod perform-computation ((computation dynamic-composite-computation) arguments cache-p
                                &optional (storage-key *performance-storage-key*))
  (flet ((perform (computation arguments)
           (with-slots (substrate-type) computation
             (with-slots (code subordinate-computations expected-arguments) substrate-type
               (apply code (loop for sub-computation in subordinate-computations
                                 for sub-arguments in (partition-arguments subordinate-computations expected-arguments arguments)
                                 collect (perform-computation sub-computation sub-arguments cache-p storage-key)))))))
    (declare (inline perform))
    (perform computation arguments)))

;;;------------------------------------------------------------------- 
;;;
;;; Specialization to the constraint metaphor
;;;

(defun make-constraint-set (name set-type in-subordinate-computations &optional (in-sorting-factor 0))
  (let ((computation (%create-unnamed-object (case set-type
                                               (:disjunctive 'disjunctive-constraint-set)
                                               (t 'conjunctive-constraint-set)))))
    (with-slots (object-name code expected-arguments inherited-arguments personal-arguments
                             subordinate-computations sorting-factor) computation
      (setf object-name name
            sorting-factor (max in-sorting-factor (get-sorting-factor in-subordinate-computations))
            subordinate-computations in-subordinate-computations
            inherited-arguments (propagate-arguments computation)
            expected-arguments (union personal-arguments inherited-arguments :test 'equalp)))
    (register name computation t)))

(defgeneric sort-constraints (constraint-set)
  (:documentation "Sorts constraints in CONSTRAINT-SET via sorting factors."))

(defmethod sort-constraints ((constraint-set constraint-set))
  (with-slots (subordinate-computations) constraint-set
    (flet ((pred (x y)
             (< (get-sorting-factor x) (get-sorting-factor y))))
      (setf subordinate-computations (sort subordinate-computations #'pred)))))

(defmethod perform-computation ((constraint (eql t)) args cache-p &optional storage-key)
  (declare (ignore args cache-p storage-key))
  t)

(defmethod perform-computation ((constraint (eql nil)) args cache-p &optional storage-key)
  (declare (ignore args cache-p storage-key))
  nil)

(defmethod perform-computation (constraint (args (eql nil)) cache-p &optional storage-key)
  (declare (ignore constraint cache-p storage-key))
  nil)

(defmethod perform-computation ((constraint-set conjunctive-constraint-set) args cache-p &optional storage-key)
  (declare (ignore cache-p storage-key))
  (with-slots (subordinate-computations) constraint-set
    (loop for computation in subordinate-computations
          always (perform-computation computation args nil))))

(defmethod perform-computation ((constraint-set disjunctive-constraint-set) args cache-p &optional storage-key)
  (declare (ignore cache-p storage-key))
  (with-slots (subordinate-computations) constraint-set
    (not (loop for computation in subordinate-computations
               never (perform-computation computation args nil)))))
