;;;-*- Mode: Lisp; Package: (FFC) -*-

;; Foreign function compatibility module for MCL, LWW and ACL (ACL version)
;; Version 0.85
;; Copyright (C) Paul Meurer 1999. All rights reserved.
;; paul.meurer@hit.uib.no
;;
;; Documentation and the license agreement can be found in file 
;; "sql-odbc-documentation.lisp".
;; Bug reports and suggestions are highly welcome.

;; In this file the platform specific code is isolated.
;; The code in this file consists mostly of wrapper functions and macros 
;; around the platform-dependent foreign function interface.

;; This file contains Allegro Common Lisp (Version 5.0) specific code

(defpackage "FFC"
  (:use "COMMON-LISP")
  (:export "*FOREIGN-MODULE*" "DEFINE-FOREIGN-FUNCTION" 
    "MAKE-RECORD"
    "%WITH-TEMPORARY-ALLOCATION" "%WITH-SQL-POINTER" "%GET-CSTRING"
    "%CSTRING-INTO-STRING"
    "%CSTRING-INTO-VECTOR"
    "%GET-CSTRING-LENGTH" "WITH-CSTR" "%GET-PTR" "%NEW-PTR" "%DISPOSE-PTR"
    "%GET-SIGNED-WORD"
    "%GET-UNSIGNED-LONG"
    "%GET-SIGNED-LONG"
    "%GET-SINGLE-FLOAT"
    "%GET-DOUBLE-FLOAT"
    "%GET-WORD"
    "%GET-LONG"
    "%GET-SHORT"
    "%GET-SIGNED-LONG"
    "%PUT-STR"
    "%PUT-WORD"
    "%PUT-SHORT"
    "%PUT-LONG"
    "%NEW-CSTRING" 
    "%NULL-PTR"
    "%PTR-EQL"
    "SHORT-TO-SIGNED-SHORT" ; #+allegro
    "STRING-PTR" "SQL-HANDLE" "SQL-HANDLE-PTR"
    "%GET-BINARY" "%PUT-BINARY" "%NEW-BINARY"))

(in-package :ffc)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *foreign-module* nil))

(eval-when (:load-toplevel :compile-toplevel :execute)
 (use-package :ff)
 (require :foreign))

(defun %get-cstring-length (ptr)
   (foreign-strlen ptr))

#+allegro-old
(defun %cstring-into-string (ptr string offset size-in-bytes)
  "Copy C string into Lisp string."
  (declare (optimize (speed 3))
	   (integer ptr))
  (unless (integerp ptr)
    (excl::.type-error ptr 'integer))
   (when (zerop ptr)
      (excl::.error "0 is not a valid character pointer"))
   (dotimes (i size-in-bytes)
      (declare (optimize (safety 0))
        (fixnum i))
      (setf (char string offset)
            (code-char (sys:memref-int ptr 0 i :unsigned-byte)))
      (incf offset))
   offset)

(defun %cstring-into-vector (ptr vector offset size-in-bytes)
  "Copy C string into Lisp vector."
  (declare (optimize (speed 3))
	   (integer ptr))
  (unless (integerp ptr)
    (excl::.type-error ptr 'integer))
   (when (zerop ptr)
      (excl::.error "0 is not a valid character pointer"))
   (dotimes (i size-in-bytes)
      (declare (optimize (safety 0))
        (fixnum i))
      (setf (aref vector offset)
            (code-char (sys:memref-int ptr 0 i :unsigned-byte)))
      (incf offset))
   offset)

(defun %cstring-into-string (ptr string offset size-in-bytes)
   (%cstring-into-vector ptr string offset size-in-bytes))

(defun %new-ptr (type &optional bytecount)
  (allocate-fobject (canonical-to-acl-type type) :c bytecount))

(defmacro %dispose-ptr (p)
   `(free-fobject ,p))

(defmacro %with-sql-pointer ((pointer-var) &body body)
  `(let ((,pointer-var (allocate-fobject 'sql-handle-ptr :c)))
     ,@body))

(defun allocate-dynamic-string (size)
   (let ((str (make-string size :initial-element #\Space)))
      (string-to-char* str)))

(defmacro %new-cstring (size)
   `(allocate-dynamic-string ,size))

(defmacro %with-temporary-allocation (bindings &body body)
  (let ((simple-types ())
        (strings ())
        (free-strings ())) 
    (dolist (binding bindings)
      (case (cadr binding)
        (:string 
          (push (list (car binding)
                  (list 'allocate-dynamic-string (caddr binding))) strings)
          (push (list 'excl:aclfree (car binding)) free-strings))
        (:ptr (push (list (car binding) :long) simple-types))
        (otherwise (push (list (car binding) (cadr binding)) simple-types))))
    `(with-stack-fobjects ,simple-types
       (let ,strings
          (unwind-protect
           (progn ,@body)
           ,@free-strings)))))

(defmacro with-cstr ((ptr str) &body body)
   `(let ((,ptr (string-to-char* ,str)))
       (unwind-protect
        (progn ,@body)
        (excl:aclfree ,ptr))))

(defun %null-ptr ()
  (make-foreign-pointer :foreign-address 0))

(defmacro %ptr-eql (ptr1 ptr2)
  `(= ,ptr1 ,ptr2)) ;; ??

(defun %get-ptr (ptr)
  (fslot-value-typed '(* :void) nil ptr))

(defun %get-short (ptr)
  (fslot-value-typed :short nil ptr))

(defun %get-long (ptr)
  (fslot-value-typed :long nil ptr))

(defmacro %put-long (ptr long) 
  `(setf (fslot-value-typed :long nil ,ptr) ,long))

(defun %get-signed-word (ptr)
  (fslot-value-typed :short nil ptr))

(defun %get-word (ptr)
  (fslot-value-typed :unsigned-short nil ptr))

(defmacro %put-word (ptr word) 
  `(setf (fslot-value-typed :short nil ,ptr) ,word))

(defun %get-unsigned-long (ptr)
  (fslot-value-typed :unsigned-long nil ptr))

(defmacro %get-signed-long (ptr)
  `(fslot-value-typed :signed-long nil ,ptr))

(defmacro %get-single-float (ptr)
  `(fslot-value-typed :float nil ,ptr))

(defmacro %get-double-float (ptr)
  `(fslot-value-typed :double nil ,ptr))

(defmacro %get-cstring (ptr &optional (start 0))
  `(char*-to-string (+ ,ptr ,start)))

(defmacro %put-str (ptr string &optional max-length)
  (declare (ignore max-length))
  `(string-to-char* ,string ,ptr))

(defun %new-binary (bytecount)
   (allocate-fobject :unsigned-char :c bytecount))

(defun %get-binary (ptr len format)
  "FORMAT is one of :unsigned-byte-vector, :bit-vector (:string, :hex-string)"
  (ecase format
    (:unsigned-byte-vector
     (let ((vector (make-array len :element-type 'unsigned-byte)))
       (dotimes (i len)
         (setf (svref vector i)
           (sys:memref-int ptr 0 i :unsigned-byte)))
       vector))
    (:bit-vector
     (let ((vector (make-array (ash len 3) :element-type 'bit)))
       (dotimes (i len)
         (let ((byte (sys:memref-int ptr 0 i :unsigned-byte)))
           (dotimes (j 8)
             (setf (bit vector (+ (ash i 3) j)) (logand (ash byte (- j 7)) 1)))))
       vector))))

;; returns size in bytes
(defun %put-binary (ptr vector &optional max-length)
  (cond ((bit-vector-p vector)
         (let* ((bit-count (length vector))
                (byte-count (print (ceiling bit-count 8))))
           (when (and max-length (> byte-count max-length))
             (error "bit vector of length ~d is longer than max-length: ~d"
                    bit-count (* max-length 4)))
           (dotimes (i byte-count)
             (let ((byte 0))
               (dotimes (j 8)
                 (let ((index (+ (ash i 3) j)))
                   (if (< index bit-count)
                       (setf byte (logior byte (ash (bit vector (+ (ash i 3) j)) (- 7 j))))
                     (return))))
               (setf (sys:memref-int ptr 0 i :unsigned-byte) byte)))
           byte-count))
        (t (error "not yet implemented"))))
 
(defmacro make-record (type)
  `(allocate-fobject (canonical-to-acl-type ',type) :c))

;; There seems to be a bug with signed short integers as return values;
;; they are returned as unsigned shorts. Quick fix.
(defun short-to-signed-short (unsigned-short)
   (if (<= unsigned-short 16384)
      unsigned-short
      (- unsigned-short 65536)))

#+ignore
(defmacro defcstruct-make (name &rest other)
  `(progn
     (ct:defcstruct ,name ,@other)
     (defmacro ,(intern (format nil "~a-~s" :make name))
	 ()
       `(ct:callocate ,',name))))

(def-foreign-type sql-handle (* :void))

(def-foreign-type sql-handle-ptr (* sql-handle))

(def-foreign-type string-ptr (* :char))

(defun c-to-lisp-type (c-type)
   (ecase c-type
     ((:ptr sql-handle sql-handle-ptr) t)
     (string-ptr 'string)
     ((:word :short :signed-short :long) 'fixnum)))

(defun canonical-to-acl-type (type)
   (case type
     (:signed-short :short)
     ;;(string-ptr :long) ; ***
     (:ptr '(* :void))
     (otherwise type)))

#+allegro-35
(defmacro define-foreign-function (c-name args result-type
                                              &key documentation module)
  (declare (ignore documentation))
  (let ((lisp-name (intern (string-upcase c-name)))
        (type-list
         (mapcar #'(lambda (var+type)
                     (let ((type (cadr var+type)))
                        (list (car var+type)
                          (canonical-to-acl-type type)
                          (c-to-lisp-type type) )))
           args)))
     `(ct:defun-dll 
        ,lisp-name
        ,type-list
        :return-type (canonical-to-acl-type ,result-type)
        :library-name ,(or module *foreign-module*)
        :entry-name ,c-name)))

(defun fix-ctype-float (type)
  ;; from aclwffi.cl
  (cond ((eq type :single-float) :float)
	((eq type :double-float) :double)
	(t type)))

(defun make-ffi-args-compatible (arglist)
  ;; from aclwffi.cl
  (let (res)
    (dolist (arg arglist (nreverse res))
      (push
       (if (listp (second arg))
	   (list (first arg) (list '* (fix-ctype-float (first (second arg)))))
	 (list (first arg) (fix-ctype-float (second arg))))
       res))))

(defmacro define-foreign-function (c-name args result-type &key documentation module)
  (declare (ignore documentation))
  (let* ((lisp-name (intern (string-upcase c-name)))
	 (type-list
	  (mapcar (lambda (var+type)
		    (let ((type (cadr var+type)))
		      (list (car var+type)
			    (canonical-to-acl-type type)
			    (c-to-lisp-type type) )))
		  args))
	 (type-list (make-ffi-args-compatible type-list)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (unless (member ,(or module *foreign-module*) (excl::foreign-files)
			 :test (lambda (x y) (equal (namestring x) (namestring y))))
	   (load ,(or module *foreign-module*))))
       (def-foreign-call
	   (,lisp-name ,c-name) ;; ,lisp-name
	   ,type-list
	 :convention :stdcall
	 :returning ,(canonical-to-acl-type result-type)
         :release-heap :when-ok))))
