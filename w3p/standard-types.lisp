;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: www-present -*-

;;; (C) Copyright 1996, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;
;;; Christopher R. Vincent
;;; cvince@ai.mit.edu
;;;
;;; (C) Enhancements Copyright 1997, 1999 John C. Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; BASIC PRESENTATION SYSTEM FOR THE WORLD-WIDE WEB
;;;

#|

standard-types.lisp defines some standard presentation-types. applications that define new
presentation-types should contain a file like this.

|#

(in-package :www-present)

;;;------------------------------------------------------------------- 
;;;
;;; SUPPORT FUNCTIONS
;;;

(defun default-name-key (arg)
  "default name-key used for completion types"
  (typecase arg
    (string arg)
    (null "NIL")
    (cons (string (car arg)))
    (keyword (write-to-string arg))
    (symbol (symbol-name arg))
    (t (princ-to-string arg))))

(defun default-value-key (item)
  (etypecase item
    (atom item)
    (cons (cdr item))))

(defun default-documentation-key (element)
  (declare (ignore element))
  nil)

;;;------------------------------------------------------------------- 
;;;
;;; PRESENTATION TYPES
;;;

(define-presentation-type t 
                          ())

(define-presentation-type boolean 
                          () 
  :inherit-from t)

(define-presentation-type symbol 
                          () 
  :inherit-from t)

(define-presentation-type null 
                          () 
  :inherit-from 'symbol)

(define-presentation-type keyword 
                          () 
  :inherit-from 'symbol)

(define-presentation-type number 
                          () 
  :inherit-from t)

(define-presentation-type complex 
                          (&optional type) 
  :inherit-from 'number)

(define-presentation-type real 
                          (&optional low high)
  :inherit-from 'number 
  :options ((base 10) 
            (radix nil)))

(define-presentation-type rational 
                          (&optional low high) 
  :inherit-from `((real ,low ,high) 
                  :base ,base 
                  :radix ,radix)
  :options ((base 10) 
            (radix nil)))

(define-presentation-type integer 
                          (&optional low high) 
  :inherit-from `((rational ,low ,high) 
                  :base ,base 
                  :radix ,radix)
  :options ((base 10) 
            (radix nil)))

(define-presentation-type fixnum 
                          (&optional (low most-negative-fixnum) (high most-positive-fixnum)) 
  :inherit-from `((integer ,low ,high) 
                  :base ,base 
                  :radix ,radix)
  :options ((base 10) 
            (radix nil)))

(define-presentation-type ratio 
                          (&optional low high) 
  :inherit-from `((rational ,low ,high) 
                  :base ,base 
                  :radix ,radix)
  :options ((base 10) 
            (radix nil)))

(define-presentation-type float 
                          (&optional low high)
  :inherit-from `((real ,low ,high) 
                  :base ,base 
                  :radix ,radix)
  :options ((base 10) 
            (radix nil)))

(define-presentation-type short-float 
                          (&optional (low most-negative-short-float) (high most-positive-short-float)) 
  :inherit-from `((float ,low ,high) 
                  :base ,base 
                  :radix ,radix)
  :options ((base 10) 
            (radix nil)))

(define-presentation-type double-float 
                          (&optional (low most-negative-double-float) (high most-positive-double-float)) 
  :inherit-from `((float ,low ,high) 
                  :base ,base 
                  :radix ,radix)
  :options ((base 10) 
            (radix nil)))

(define-presentation-type character 
                          () 
  :inherit-from t)

(define-presentation-type basic-string 
                          () 
  :inherit-from t
  :description " a basic, unbounded string")

(define-presentation-type string 
                          (&optional length) 
  :inherit-from 'basic-string)

(define-presentation-type bounded-string 
                          (&optional max-length) 
  :inherit-from 'basic-string 
  :description "a string that can be parameterized by a maximum length."
  :options ((break-line-after-prompt t)
	    input-box-size))

(define-presentation-type pathname 
                          () 
  :inherit-from t 
  :options (default-type
             (default-version :newest) 
             (merge-default t)))

(define-presentation-type existing-pathname 
                          () 
  :inherit-from 'pathname
  :options (default-type
             (default-version :newest) 
             (merge-default t)))

(define-presentation-type completion 
                          (&optional (sequence nil) &key (test #'eql) (value-key #'identity)) 
  :inherit-from t
  :options ((name-key #'default-name-key) 
            (documentation-key #'(lambda (element) (declare (ignore element)) nil))
            (partial-completers '(#\Space))))

(define-presentation-type member 
                          (&rest elements) 
  :inherit-from `((completion ,elements) 
                  :name-key ,name-key 
                  :documentation-key ,documentation-key 
                  :partial-completers ,partial-completers)
  :options ((name-key #'default-name-key)
            (documentation-key #'default-documentation-key)
            (partial-completers '(#\Space))))

(define-presentation-type member-sequence 
                          (sequence &key (test #'eql)) 
  :inherit-from `((completion ,sequence :test ,test) 
                  :name-key ,name-key 
                  :documentation-key ,documentation-key 
                  :partial-completers ,partial-completers)
  :options ((name-key #'default-name-key)
            (documentation-key #'default-documentation-key)
            (partial-completers '(#\Space))))

(define-presentation-type member-alist 
                          (alist &key (test #'eql) (value-key #'default-value-key))
  :inherit-from `((completion ,alist :test ,test :value-key ,value-key) 
                  :name-key ,name-key 
                  :documentation-key ,documentation-key 
                  :partial-completers ,partial-completers)
  :options ((name-key #'default-name-key)
            (documentation-key #'default-documentation-key)
            (partial-completers '(#\Space))))

(define-presentation-type subset-completion
                          (&optional (sequence nil) &key (test #'eql) (value-key #'identity))
  :inherit-from t 
  :options ((name-key #'default-name-key)
            (documentation-key #'default-documentation-key)
            (partial-completers '(#\Space))
            (separator #\,) 
            (echo-space t)))

(define-presentation-type subset 
                          (&rest elements) 
  :inherit-from `((subset-completion ,elements)
                  :name-key ,name-key
                  :documentation-key ,documentation-key
                  :partial-completers ,partial-completers
                  :separator ,separator
                  :echo-space ,echo-space)
  :options ((name-key #'default-name-key)
            (documentation-key #'default-documentation-key)
            (partial-completers '(#\Space))
            (separator #\,) 
            (echo-space t)))

(define-presentation-type subset-sequence 
                          (sequence &key (test #'eql)) 
  :inherit-from `((subset-completion ,elements :test ,test)
                  :name-key ,name-key
                  :documentation-key ,documentation-key
                  :partial-completers ,partial-completers
                  :separator ,separator
                  :echo-space ,echo-space)
  :options ((name-key #'default-name-key)
            (documentation-key #'default-documentation-key)
            (partial-completers '(#\Space))
            (separator #\,) 
            (echo-space t)))

(define-presentation-type subset-alist 
                          (alist &key (test #'eql) (value-key #'(lambda (item) (typecase item (atom item) (t (cdr item))))))
  :inherit-from `((subset-completion ,elements :test ,test :value-key ,value-key)
                  :name-key ,name-key
                  :documentation-key ,documentation-key
                  :partial-completers ,partial-completers
                  :separator ,separator
                  :echo-space ,echo-space)
  :options ((name-key #'default-name-key)
            (documentation-key #'default-documentation-key)
            (partial-completers '(#\Space))
            (separator #\,)
            (echo-space t)))

(define-presentation-type sequence 
                          (type) 
  :inherit-from t 
  :options ((separator #\,) 
            (echo-space t)))

(define-presentation-type sequence-enumerated 
                          (&rest types) 
  :inherit-from t 
  :options ((separator #\,) 
            (echo-space t)))

(define-presentation-type mixed-sequence 
                          () 
  :inherit-from t 
  :options ((separator #\,) 
            (echo-space t)) 
  :description "a sequence of atoms of mixed type, as well as other mixed-sequences.")

(define-presentation-type or 
                          (&rest types) 
  :inherit-from t)

(define-presentation-type and 
                          (&rest types) 
  :inherit-from t)

(define-presentation-type token-or-type 
                          (tokens type) 
  :inherit-from t)

(define-presentation-type null-or-type 
                          (type) 
  :inherit-from t)

(define-presentation-type type-or-string 
                          (type) 
  :inherit-from t)

(define-presentation-type expression 
                          () 
  :inherit-from t
  :options (auto-activate))

(define-presentation-type form 
                          () 
  :inherit-from 'expression)
