;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: common-lisp-user -*-
;;;
;;; Copyright 1994-96, John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;;  SYSTEM DEFINITION FOR HTML-PARSER
;;;  ADAPTED BY SUNIL MISHRA FROM SYSTEM DEFINITION FOR MAC CL-HTTP
;;;
;;; Last edited by smishra on Thu Oct 22 17:24:00 1998

(in-package :cl-user) 

;; Logical pathname translations - extracted and modified from cl-http
;; Sunil Mishra

(setf (logical-pathname-translations "html-parser")
      `(("*.*" ,(merge-pathnames "*.*" *load-truename*))))

;; allow the values declaration
(define-declaration values nil)

(define-declaration arglist nil)

;;;------------------------------------------------------------------- 
;;;
;;; DEFINE A LOAD FORM 
;;;

(defparameter *always-compile* (if (boundp '*always-compile*) (symbol-value '*always-compile*) nil)
  "When non-null all files are compiled, whether or not they need it.")

(defmethod require-pathname ((pathname pathname))
  (ccl:require (pathname-name pathname) pathname))

(defun conditional-compile-file (file &key load  verbose  output-file always require
                                      (save-local-symbols *fasl-save-local-symbols*) 
                                      (save-doc-strings *fasl-save-doc-strings*)
                                      (save-definitions *fasl-save-definitions*))
  (let* ((source (merge-pathnames ".lisp" file))
         (fasl (merge-pathnames ccl::*.fasl-pathname* (or output-file file))))
    (unless (probe-file source)
      (error 'file-error :pathname file))
    (cond ((or always
               *always-compile*
               (not (probe-file fasl))
               (< (file-write-date fasl)
                  (file-write-date source)))
           (compile-file source :output-file fasl :load load :verbose verbose
                         :save-local-symbols save-local-symbols
                         :save-doc-strings save-doc-strings
                         :save-definitions save-definitions))
          (require (require-pathname fasl))
          (load (load fasl :verbose verbose))
          (t nil))))

(defun execute-system-operations (modules operations &aux ops)
  (macrolet ((processing-module ((module) &body clauses)
               `(destructuring-bind (case action &rest files) ,module
                  (ecase case ,@clauses))))
    (flet ((check-operation-arg (arg)
             (let ((known-operations '(:load :compile :eval-load :compile-load :compile-load-always)))
               (unless (member arg known-operations)
                 (error "~S is not one of the know options for OPERATIONS."arg known-operations))))
           (eval-load (pathname &rest args)
             (apply #'load (merge-pathnames ".lisp" pathname)  args))
           (perform-action (action files &optional require-only)
             (ecase action
               (:load 
                 (cond (require-only (mapc #'require-pathname files))
                       (t (dolist (file files)
                            (load file :verbose t)))))
               (:compile-load
                 (dolist (file files)
                   (conditional-compile-file file :verbose t :load t :require t))))))
      ;; check arguments to operations
      (mapc #'check-operation-arg operations)
      ;;Combined operations take precedence
      (unless (intersection  operations '(:compile-load :compile-load-always :eval-load))
        (when (member :load operations)
          (push '(load :verbose t) ops))
        (when (member :compile operations)
          (push '(conditional-compile-file :verbose t :always t) ops)))
      ;; Combined operations
      (cond ((member :compile-load-always operations)
             (push '(conditional-compile-file :always  t :load t  :verbose t :save-local-symbols t
                                              :save-doc-strings t :save-definitions t) ops))
            ((member :compile-load operations)
             (push '(conditional-compile-file :load t  :verbose t :save-local-symbols t
                                              :save-doc-strings t :save-definitions t) ops))
            ((member :eval-load operations)
             (push `(,#'eval-load :verbose t) ops)))
      ;; Perform operations
      (loop for module in modules
            do (typecase module
                 (cons
                   (processing-module 
                     (module)
                     (:in-order-to-load
                       (perform-action action files t))
                     (:in-order-to-compile
                       (when (intersection operations '(:compile-load :compile :compile-load-always))
                         (perform-action action files t)))))
                 (t (loop for (fctn . args) in ops
                          do (apply fctn module args))))))))

(defmacro define-system ((name) (&rest operations) &body files)
  "Operations can be :load and :compile."
  (let ((var (intern (concatenate 'string "*" (string name) "*"))))
    `(progn
       (defparameter ,var ',files)
       ,(when operations
          ` (execute-system-operations ,var ',operations))
       ',var)))

;;;------------------------------------------------------------------- 
;;;
;;; DEFINE THE HTML PARSER SYSTEM
;;; HTML parser definition by Sunil Mishra
;;;
   
(define-system 
  (html-parser)
  (:compile-load)
  ;; load MCL libraries 
  (:in-order-to-compile :compile-load "ccl:library;LOOP")       ; loop macro, required for compilation only?
  ;; parser code
  "html-parser:packages"
  #-CL-HTTP "html-parser:tokenizer"
  #-CL-HTTP "html-parser:plist"
  "html-parser:defs"
  "html-parser:patmatch"
  "html-parser:rewrite-engine"
  "html-parser:rewrite-rules"
  "html-parser:html-tags"
  "html-parser:html-reader"
  "html-parser:html-parser"
  "html-parser:html-utilities")
