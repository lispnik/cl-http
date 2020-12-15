;;; -*- Package: lambda-ir; Mode: LISP; Syntax: Common-lisp; Base: 10 -*-

;;; (C) Copyright 1997, Rusty Johnson and Andrew J. Blumberg (blumberg@ai.mit.edu)
;;;     All Rights Reserved.
;;;
;;;     Thanks to Steve Hain (slh@digitoo.com) for various fixes and improvements.
;;;

;;;; This file contains a CUSTOM binary dumper and loader technology that has
;;;; been tuned for dumping and loading database tables. These tables should
;;;; be readable on any machine / Lisp implementation with some adjustments to
;;;; this file.

;;;; Binary encoding scheme

;;;; If the high order bit is a 0,  this 16-bit word is a positive 15-bit
;;;; integer (0 <= n < 1_15)
;;;;
;;;; If the high order bits are #b10,  then this word contains the 14 high
;;;; order bits of a positive 30-bit integer  (0 <= n < 1_30), the 16
;;;; low order bits are in the next word.
;;;;
;;;; If the this order bits are #b11,  then the following 14 bits are the
;;;; extended tag as follows:
;;;;
;;;;    TAG               Meaning
;;;;  #b0xxxxxxxxxxxxxxx  positive 15-bit integer (0 <= n < 1_15)
;;;;  #b10xxxxxxxxxxxxxx  positive 30-bit integer (0 <= n < 1_30), low order bits in next word.
;;;;  #b1100000000000000  Boolean False
;;;;  #b1111111111111111  Boolean True
;;;;  #b1100000000000001  32 bit fixnum
;;;;  #b1100000000000100  string
;;;;  #b1100000000000101  symbol
;;;;  #b1100000000000111  list
;;;;  #b1100000000001000  vise-server:index-table
;;;;  #b1100000000001001  vise-server:reversible-index-table
;;;;  #b1100000000001010  array
;;;;  #b1100000000001100  improper list
;;;;  #b1100110011001100  other


;;;; Should be:
;;;;  #b0xxxxxxxxxxxxxxx  positive 15-bit integer (0 <= n < 1_15)
;;;;  #b10xxxxxxxxxxxxxx  positive 14-bit increment from the previous integer
;;;;  #b110xxxxxxxxxxxxx  next N (13-bit) entries are increments from the previous integer
;;;;  #b1110xxxxxxxxxxxx  positive 27-bit integer (0 <= n < 1_27), low order bits in next word.
;;;;  #b1111000000000000  Boolean False
;;;;  #b1111111111111111  Boolean True
;;;;  #b1111000000000001  32 bit fixnum
;;;;  #b1111000000000010  ascending list of integers
;;;;  #b1111000000000100  string
;;;;  #b1111000000000101  symbol
;;;;  #b1111000000000111  list
;;;;  #b1111000000001000  vise-server:index-table
;;;;  #b1111000000001001  vise-server:reversible-index-table
;;;;  #b1111000000001010  array
;;;;  #b1111000000001100  improper list
;;;;  #b1111110011001100  other

(in-package :lambda-ir)
;;;------------------------------------------------------------------- 
;;;
;;; CONSTANTS
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +false-tag+                        #b11100000)
  (defconstant +true-tag+                         #b11111110)
  (defconstant +32-bit-fixnum-tag+                #b11100001)
  (defconstant +32-bit-float-tag+                 #b11100010)
  (defconstant +64-bit-float-tag+                 #b11100011)
  (defconstant +string-tag+                       #b11100100)
  (defconstant +symbol-tag+                       #b11100101)
  (defconstant +list-tag+                         #b11100111)
  (defconstant +document-array-tag+               #b11101000)
  (defconstant +document-hashed-tagged-array-tag+ #b11101001)
  (defconstant +runtime-encoded-array-tag+        #b11101101)
  (defconstant +array-tag+                        #b11101010)
  (defconstant +improper-list-tag+                #b11101100)
  (defconstant +hash-table-tag+                   #b11101110)
  (defconstant +document-universe-tag+            #b11101111)
  (defconstant +bit-vector-tag+                   #b11110000)
  (defconstant +token-cluster-tag+                #b11110001)
  (defconstant +mapping-token-cluster-tag+        #b11110010)
  (defconstant +lir-bit-vector-tag+               #b11110011)
  (defconstant +negative-tag+                     #b11110100)
  (defconstant +other-tag+                        #b11111100)
  (defconstant +end-tag+                          #b11111111)
  (defconstant +byte-8-0+  (byte 8  0))
  (defconstant +byte-8-8+ (byte 8 8))
  (defconstant +byte-8-16+ (byte 8 16))
  (defconstant +byte-8-24+ (byte 8 24))
  (defconstant +byte-5-16+ (byte 5 16))
  (defconstant +byte-3-5+ (byte 3 5))
  (defconstant +byte-6-8+ (byte 6 8))
  (defconstant +byte-6-0+ (byte 6 0))
  (defconstant +byte-5-0+ (byte 5 0))

  (defconstant +2-byte-fixnum-base+ #b10000000)
  (defconstant +3-byte-fixnum-base+ #b11000000)

  (defconstant +byte-fixnum+ (ash 1 7))
  (defconstant +2-byte-fixnum+ (ash 1 14))
  (defconstant +3-byte-fixnum+ (ash 1 21))

  (defconstant +byte-1-7+ (byte 1 7))
  (defconstant +byte-1-6+ (byte 1 6))
  (defconstant +byte-1-5+ (byte 1 5))

  (defconstant +ash-1-31+   (ash 1 31))
  (defconstant +ash-1-32+   (ash 1 32)))

;;;------------------------------------------------------------------- 
;;;
;;; PARAMETERS
;;;

(defparameter *op-code-table* nil)
(defparameter *version* 4)
(defparameter *number-of-lir-documents* 0)
(defparameter *default-hash-table-test* #'equalp)

(defparameter *2-power-array* #(1 2 4 8 16 32 64 128))

;;;------------------------------------------------------------------- 
;;;
;;; PLATFORM SPECIFIC
;;;

#+MCL
(defparameter *buffer* (make-array 0 :adjustable t :fill-pointer t
				   :element-type 'base-character))
#-MCL
(defparameter *buffer* (make-array 0 :adjustable t :fill-pointer t))

#+MCL
(eval-when (:execute :compile-toplevel)
  (shadow 'write-byte "LAMBDA-IR"))

#+MCL
(eval-when (:execute :compile-toplevel)
  (defmacro write-byte (byte stream)
    `(ccl::stream-write-byte ,stream ,byte)))

;; When in Genera, scl:pkg-find-package gives better error recovery.
#+Genera
(defmacro %find-package (package)
  `(scl:pkg-find-package ,package))
#-Genera
(defmacro %find-package (package)
  `(find-package ,package))

#+Genera 
(defmacro %read-next-byte (stream)
  `(si:bin-next-byte ,stream))

#+MCL
(defmacro %read-next-byte (stream)
  `(ccl::stream-read-byte ,stream))

#-(OR Genera MCL)
(defmacro %read-next-byte (stream)
  `(read-byte ,stream))

#+(OR Genera MCL)
(declaim (inline logdpb))
#+Genera
(defun logdpb (new-byte byte-spec integer)
  (sys:%logdpb new-byte byte-spec integer))

#-Genera
(defun logdpb (new-byte byte-spec integer)
  (let ((temp (dpb new-byte byte-spec integer)))
    (when (> temp +ash-1-31+)
      (setq temp (- temp +ash-1-32+)))
    temp))

#+Genera
(defmacro string-out (vector stream)
  `(compiler:string-out ,vector ,stream))

#-Genera
(defmacro string-out (vector stream)
  `(loop for item across ,vector
	do (write-byte item ,stream)))

#+Genera 
(defmacro string-in (string stream)
  `(flavor::send ,stream ':string-in nil ,string))

#-Genera
(defmacro string-in (string stream)
  `(loop for idx #+MCL fixnum from 0 below (length ,string)
	 do (setf (aref ,string idx) (code-char (read-byte ,stream)))))

#+Genera
(defmacro bit-vector-out (vector stream &optional length)
  `(let* ((vector-length (or ,length (length ,vector)))
	  (stack-array-length (floor vector-length 8))
	  (slop (* stack-array-length 8)))
     (sys:with-stack-array (n-vector stack-array-length :displaced-to ,vector :element-type '(unsigned-byte 8))
       (string-out n-vector ,stream))
     (write-byte (loop for idx upfrom slop below vector-length
		       for bit = (aref ,vector idx)
		       for entry across *2-power-array*
		       sum (* bit entry))
		 stream)))

#-Genera
(defmacro bit-vector-out (vector stream &optional length)
  `(loop for idx #+MCL fixnum from 0 below (or ,length (length ,vector))
	 do (write-byte (aref ,vector idx) ,stream)))

#+Genera 
(defmacro bit-vector-in (vector stream &optional length)
  `(let* ((vector-length (or ,length (length ,vector)))
	  (stack-array-length (floor vector-length 8))
	  (slop (* stack-array-length 8)))
     #+bob
     (sys:with-stack-array (n-vector (floor (or ,length (length ,vector)) 8) :displaced-to ,vector :element-type '(unsigned-byte 8))
       (string-in n-vector ,stream))
     (let ((n-vector (make-array (floor (or ,length (length ,vector)) 8) :displaced-to ,vector :element-type '(unsigned-byte 8))))
       (declare (dynamic-extent n-vector))
       (string-in n-vector ,stream))
     (loop with byte = (%read-next-byte stream)
	   for index upfrom slop below vector-length
	   for idx upfrom 0 
	   for bit = (ldb (byte 1 idx) byte)
	   do (setf (aref ,vector index) bit))))

#-Genera
(defmacro bit-vector-in (vector stream &optional length)
  `(loop for idx #+MCL fixnum from 0 below (or ,length (length ,vector))
	 do (setf (aref ,vector idx) (read-byte ,stream))))

#+Genera
(defun read-string (stream)
  (let* ((length (read-thing stream))
	 (string (make-array length :element-type 'lisp:string-char))
	 (displaced-string (make-array length :displaced-to string :element-type '(unsigned-byte 8))))
    (string-in displaced-string stream)
    string))

#-Genera
(defun read-string (stream)
  (let* ((length (read-thing stream))
	 (string (make-array length :element-type http::*standard-character-type*)))
    (string-in string stream)
    string))

;;;------------------------------------------------------------------- 
;;;
;;; CODE
;;;

(declaim (inline write-32-bit-fixnum
		 read-32-bit-fixnum
		 write-21-bit-fixnum
		 write-14-bit-fixnum
		 write-7-bit-fixnum
		 dump-negative))

(defun write-32-bit-fixnum-data (fixnum stream)
  (write-byte (ldb +byte-8-24+ fixnum) stream)
  (write-byte (ldb +byte-8-16+  fixnum) stream)
  (write-byte (ldb +byte-8-8+  fixnum) stream)
  (write-byte (ldb +byte-8-0+  fixnum) stream))

(defun read-32-bit-fixnum-data (stream)
  (logdpb (%read-next-byte stream) +byte-8-24+
	  (logdpb (%read-next-byte stream) +byte-8-16+
		  (logdpb (%read-next-byte stream) +byte-8-8+
			  (%read-next-byte stream)))))

(defun write-21-bit-fixnum-data (fixnum stream)
  (write-byte (logdpb (ldb +byte-5-16+ fixnum) +byte-5-0+ +3-byte-fixnum-base+) stream)
  (write-byte (ldb +byte-8-8+ fixnum) stream)
  (write-byte (ldb +byte-8-0+ fixnum) stream))

(defun write-14-bit-fixnum-data (fixnum stream)
  (write-byte (logdpb (ldb +byte-6-8+ fixnum) +byte-6-0+ +2-byte-fixnum-base+) stream)
  (write-byte (ldb +byte-8-0+ fixnum) stream))

(defun write-7-bit-fixnum-data (fixnum stream)
  (write-byte fixnum stream))

;;;; fixnums
(defun read-32-bit-fixnum (stream)
  (read-32-bit-fixnum-data stream))

(defun dump-negative (fixnum stream)
  (write-byte +negative-tag+ stream)
  (dump-fixnum (- fixnum) stream))

(defun read-negative (stream)
  (- (read-thing stream)))

(defun dump-fixnum (fixnum stream)
  (cond ((< fixnum 0)
	 (dump-negative fixnum stream))
	((< fixnum +byte-fixnum+)
	 (write-7-bit-fixnum-data fixnum stream))
	((< fixnum +2-byte-fixnum+)
	 (write-14-bit-fixnum-data fixnum stream))
	((< fixnum +3-byte-fixnum+)
	 (write-21-bit-fixnum-data fixnum stream))
	(t (write-byte +32-bit-fixnum-tag+ stream)
	   (write-32-bit-fixnum-data fixnum stream))))

;;;; strings

;; do not be tempted to convert this to use ':string-out; fat-strings screw you hard if you try this.
;; 4/28/98 09:52:48 AJB

(defun dump-string (string stream)
  (write-byte +string-tag+ stream)
  (let* ((length (length string)))
    (dump-fixnum length stream)
    (loop for item across string
	  do (write-byte (char-code item) stream))))

(defun list-length-blah (list)
  (do ((count 0 (1+ count))
       (p list (cdr p)))
      ((not (consp p)) (values count (null p)))))

;;;; lists (proper and improper)
(defun dump-list (list stream)
  (multiple-value-bind (length proper-p)
      (do ((count 0 (1+ count))
           (p list (cdr p)))
          ((not (consp p)) (values count (null p))))
    (write-byte (if proper-p +list-tag+ +improper-list-tag+) stream)
    (dump-fixnum length stream)
    (do ((p list (cdr p)))
        ((not (consp p))
         (when p
           (dump-thing p stream)))
      (dump-thing (car p) stream))))

(defun read-list (stream)
  (let ((new-list (make-list (read-thing stream))))
    (loop for temp on new-list
          do (setf (car temp) (read-thing stream)))
    new-list))

(defun read-improper-list (stream)
  (let ((new-list (make-list (read-thing stream))))
    (do ((p    new-list       next)
         (next (cdr new-list) (cdr next)))
        ((null next)
         (setf (car p) (read-thing stream)
               (cdr p) (read-thing stream)))
      (setf (car p) (read-thing stream)))
    new-list))

;;;; ARRAYs
(defun dump-array (array stream)
  (write-byte +array-tag+ stream)
  (let ((length (length array)))
    (dump-fixnum length stream)
    (loop for thing across array
          do (dump-thing thing stream))))

(defun read-array (stream)
  (let* ((length (read-thing stream))
	 (new-array (make-array length :adjustable t :fill-pointer t)))
    (loop for idx upfrom 0 below length
	  do (setf (aref new-array idx) (read-thing stream)))
    new-array))

;;;; SYMBOLs
(defun dump-symbol (symbol stream)
  (write-byte +symbol-tag+ stream)
  (dump-thing (symbol-name symbol) stream)
  (dump-thing (package-name (symbol-package symbol)) stream))

(defun read-symbol (stream)
  (let ((name (read-thing stream))
        (pkg  (read-thing stream)))
    (intern name (%find-package pkg))))

(defun dump-other (thing stream)
  (write-byte +other-tag+ stream)
  (setf (fill-pointer *buffer*) 0)
  (with-output-to-string (s1 *buffer*)
    (format s1 "~S" thing))
  (dump-string *buffer* stream))

(defun read-other (stream)
  (read-from-string (read-thing stream)))

;;;; DUMP- & READ- THING

(defgeneric dump-thing (thing stream)
  (:documentation "Method dispatch to dump; specialize to extend."))

#+Genera
(defmethod dump-thing ((thing fixnum) stream)
  (dump-fixnum thing stream))

;; This will lose on bignums and fixnums with sizes greater than 32 bits.  will be fixed in next release. (ajb) 

#-Genera
(defmethod dump-thing ((thing integer) stream)
  (dump-fixnum thing stream))

(defmethod dump-thing ((thing string) stream)
  (dump-string thing stream))

(defmethod dump-thing ((thing (eql nil)) stream)
  (write-byte +false-tag+ stream))

(defmethod dump-thing ((thing (eql t)) stream)
  (write-byte +true-tag+ stream))

(defmethod dump-thing ((thing mapping-token-cluster) stream)
  (dump-mapping-token-cluster thing stream))

(defmethod dump-thing ((thing token-cluster) stream)
  (dump-token-cluster thing stream))

(defmethod dump-thing ((thing list) stream)
  (dump-list thing stream))

(defmethod dump-thing ((thing symbol) stream)
  (dump-symbol thing stream))

(defmethod dump-thing ((thing bit-vector) stream)
  (dump-bit-vector thing stream))

(defmethod dump-thing ((thing array) stream)
  (dump-array thing stream))

(defmethod dump-thing ((thing hash-table) stream)
  (dump-hash-table thing stream))

(defmethod dump-thing (thing stream)
  (dump-other thing stream))

(defmethod dump-thing ((thing document-universe) stream)
  (dump-document-universe thing stream))

;;

(defun read-thing (stream)
  (let ((item (%read-next-byte stream))
	reader)
    (cond ((not (ldb-test +byte-1-7+ item))
	   item)
	  ((not (ldb-test +byte-1-6+ item))
	   (logdpb (ldb +byte-6-0+ item) +byte-8-8+ (%read-next-byte stream)))
	  ((not (ldb-test +byte-1-5+ item))
	   (logdpb (ldb +byte-5-0+ item) +byte-8-16+ (logdpb (%read-next-byte stream) +byte-8-8+ (%read-next-byte stream))))
	  ((setq reader (aref *op-code-table* item))
	   (funcall reader stream))
	  (t (error "Unknown opcode ~S" item)))))

(defun dump-runtime-encoded-array (array stream)
  (write-byte +runtime-encoded-array-tag+ stream)
  (let ((length (length array)))
    (dump-fixnum length stream)
    (loop with idx = 0
          while (< idx length)
          for initial-object = (aref array idx)
          for new-idx = (loop for check-idx upfrom idx below length
                              for item = (aref array check-idx)
                              while (equalp item initial-object)
                              finally (return check-idx))
          do (dump-thing initial-object stream)
             (dump-fixnum (- new-idx idx) stream)
             (setq idx new-idx))))
                  
(defun read-runtime-encoded-array (stream)
  (let* ((length (read-thing stream))
         (new-array (make-array length :adjustable t :fill-pointer t)))
    (loop with current-index = 0
          while (< current-index length)
          for current-thing = (read-thing stream)
          for number-of-them = (read-thing stream)
          do (loop for idx upfrom current-index below (+ current-index number-of-them)
                   do (setf (aref new-array idx) current-thing)
                   finally (setq current-index idx)))
    new-array))

(defun dump-hash-table (hash-table stream)
  (write-byte +hash-table-tag+ stream)
  (dump-fixnum (hash-table-count hash-table) stream)
  (loop for key being the hash-key of hash-table
        for value being the hash-value of hash-table
        do (dump-thing key stream)
           (dump-thing value stream)))

(defun read-hash-table (stream)
  (let* ((length (read-thing stream))
         (table (make-hash-table :test *default-hash-table-test* :size length)))
    (loop repeat length
          for key = (read-thing stream)
          for value = (read-thing stream)
          do (setf (gethash key table) value))
    table))

(defun dump-bit-vector (bit-vector stream)
  (write-byte +bit-vector-tag+ stream)
  (let ((length (length bit-vector)))
    (dump-fixnum length stream)
    (bit-vector-out bit-vector stream)))

(defun dump-lir-bit-vector (bit-vector stream)
  (write-byte +lir-bit-vector-tag+ stream)
  (bit-vector-out bit-vector stream *number-of-lir-documents*))

(defun read-bit-vector (stream)
  (let* ((length (read-thing stream))
	 (output-vector (make-bit-vector length)))
    (bit-vector-in output-vector stream)
    output-vector))

(defun read-lir-bit-vector (stream)
  (let* ((output-vector (make-bit-vector)))
    (bit-vector-in output-vector stream *number-of-lir-documents*)
    output-vector))

(defun dump-token-cluster (token-cluster stream)
  (with-slots (linear-structure) token-cluster
    (write-byte +token-cluster-tag+ stream)
    (let ((length (fill-pointer linear-structure)))
      (dump-fixnum length stream)
      (loop for token across linear-structure
	    for stats = (get-token-stats token)
	    do (typecase stats
		 (bit-vector (dump-lir-bit-vector stats stream))
		 (array (dump-array stats stream)))
	       (dump-thing (token-datum token) stream)))))

(defun read-token-cluster (stream)
  (let* ((length (read-thing stream))
         (table (make-hash-table :size length :test #'equalp))
         (linear-structure (make-array length :adjustable t :fill-pointer t)))
    (loop for idx upfrom 0 below length
	  for byte = (%read-next-byte stream)
	  for stats =  (cond ((eq byte +lir-bit-vector-tag+)
			      (read-lir-bit-vector stream))
			     ((eq byte +array-tag+)
			      (read-array stream))
			     (t (error "Bad token-cluster dump.")))
	  for word = (read-thing stream)
	  do (setf (gethash word table) idx)
	     (setf (aref linear-structure idx) (make-token word :stat-storage stats)))
    (make-token-cluster :access-hash table :linear-structure linear-structure)))

(defun dump-mapping-token-cluster (token-cluster stream)
  (with-slots (linear-structure) token-cluster
    (write-byte +mapping-token-cluster-tag+ stream)
    (let ((length (fill-pointer linear-structure)))
      (dump-fixnum length stream)
      (loop for token across linear-structure
	    for stats = (get-token-stats token)
	    do (typecase stats
		 (bit-vector (dump-lir-bit-vector stats stream))
		 (array (dump-array stats stream)))
	       (dump-thing (token-datum token) stream)))))

(defun read-mapping-token-cluster (stream)
  (let* ((length (read-thing stream))
         (table (make-hash-table :size length :test #'equalp))
         (linear-structure (make-array length :adjustable t :fill-pointer t)))
    (loop for idx upfrom 0 below length
	  for byte = (%read-next-byte stream)
	  for stats =  (cond ((eq byte +lir-bit-vector-tag+)
			      (read-lir-bit-vector stream))
			     ((eq byte +array-tag+)
			      (read-array stream))
			     (t (error "Bad token-cluster dump.")))
	  for word = (read-thing stream)
	  do (setf (gethash word table) idx)
	     (setf (aref linear-structure idx) (make-token word :stat-storage stats)))
    (make-mapping-token-cluster :access-hash table :linear-structure linear-structure)))

(defun dump-documents (hashed-tagged-array stream)
  (with-slots (linear-object tags names) hashed-tagged-array
    (let ((general-length (fill-pointer linear-object)))
      (write-byte +document-hashed-tagged-array-tag+ stream)
      ;; tags are likely to be homogeneous, so do runtime encoding --- this pays off big in the single tag case, of course.
      (dump-runtime-encoded-array tags stream)
      (loop for idx upfrom 0 below general-length
	    for item = (aref names idx)
	    do (dump-thing item stream))
      ;; now do the heavily redundant document informational items
      ;; this is all going to be stack-consed
      (let ((labels (make-array general-length))
	    (formats (make-array general-length)))
	(declare (dynamic-extent labels formats))
	(loop for document across linear-object
	      for idx upfrom 0
	      for label = (doc-label document)
	      for format = (doc-format document)
	      do (dump-thing (get-document-pdi document :url) stream)
		 (setf (aref labels idx) label
		       (aref formats idx) format))
	(dump-runtime-encoded-array labels stream)
	(dump-runtime-encoded-array formats stream)))))

(defun read-documents (stream &optional (cons-pdi nil))
  (let* ((tags (read-thing stream))
	 (length (length tags)))
    (www-utils::with-fast-array-references ((names (make-array length :adjustable t :fill-pointer t))
					    (urls (make-array length :adjustable t :fill-pointer t)))
      (loop for idx upfrom 0 below length
	    do (setf (aref names idx) (read-thing stream)))
      (loop for idx upfrom 0 below length
	    do (setf (aref urls idx) (read-thing stream)))
      (www-utils::with-fast-array-references ((formats (read-thing stream))
					      (labels (read-thing stream))
					      (documents (make-array length :adjustable t :fill-pointer t)))
	(loop for document-name across names
	      for url-string across urls
	      for idx upfrom 0 below length
	      for document-pdi = (if cons-pdi
				     (list (list :pathname document-name) (list :url url-string)))
	      for format across formats
	      for label across labels
	      do (setf (aref names idx) document-name)
		 (setf (aref documents idx) (make-document document-name document-pdi format label)))
	(make-hashed-tagged-array :linear-object documents :tags tags :names names :size length)))))

(defun dump-document-universe (document-universe stream)
  (with-slots (object-name documents tokens tokenizers access-codes supported-formats) document-universe
    (write-byte +document-universe-tag+ stream)
    (dump-thing object-name stream)
    (dump-documents documents stream)
    (let ((*number-of-lir-documents* (bit-vector-length)))
      (dump-thing tokens stream))))

(defun read-document-universe (stream)
  (let* ((name (read-thing stream))
	 (documents (read-thing stream))
	 (*number-of-lir-documents* (bit-vector-length))
	 (tokens (read-thing stream)))
    name
    (make-document-universe :documents documents :tokens tokens)))

(defun initialize-op-code-table ()
  (let ((table (make-array 256 :initial-element nil)))
    (loop for (code . symbol) in `((#.+hash-table-tag+ . read-hash-table)
				   (#.+bit-vector-tag+ . read-bit-vector)
				   (#.+lir-bit-vector-tag+ . read-lir-bit-vector)
				   (#.+token-cluster-tag+ . read-token-cluster)
				   (#.+mapping-token-cluster-tag+ . read-mapping-token-cluster)
				   (#.+32-bit-fixnum-tag+ . read-32-bit-fixnum)
				   (#.+string-tag+ . read-string)
				   (#.+array-tag+ . read-array)
				   (#.+list-tag+ . read-list)
				   (#.+document-universe-tag+ . read-document-universe)
				   (#.+symbol-tag+ . read-symbol)
				   (#.+runtime-encoded-array-tag+ . read-runtime-encoded-array)
				   (#.+document-hashed-tagged-array-tag+ . read-documents)
				   (#.+negative-tag+ . read-negative)
				   (#.+other-tag+ . read-other)
				   (#.+improper-list-tag+ . read-improper-list))
	  do (setf (aref table code) (symbol-function symbol)))
    (setf (aref table +true-tag+) #'(lambda (x) (declare (ignore x)) t))
    (setf (aref table +false-tag+) #'(lambda (x) (declare (ignore x)) nil))
    (setf *op-code-table* table)))

(initialize-op-code-table)

(defun obtain-version-number ()
  *version*)

#-MCL
(defun dump-one (thing filename &optional estimated-length)
  (lisp:with-open-file (str filename :direction :output :element-type '(unsigned-byte 8) :estimated-length estimated-length)
    (write-byte (obtain-version-number) str)
    (let ((*print-readably* t))  ; so dump-other will signal errors
      (dump-thing thing str))
    (write-byte +end-tag+ str)))

#+MCL
(defun dump-one (thing filename)
  (with-open-file (str filename :direction :output :element-type '(unsigned-byte 8)
		       :if-exists :supersede)
    (write-byte (obtain-version-number) str)
    (let ((*print-readably* t))  ; so dump-other will signal errors
      (dump-thing thing str))
    (write-byte +end-tag+ str)))

(defun read-one (filename &key (hash-table-test #'equalp))
  (declare (values object version))
  (with-open-file (str filename :direction :input :element-type '(unsigned-byte 8))
    (let ((version (read-byte str))
	  (*default-hash-table-test* hash-table-test)
	  (object (read-thing str))
	  (end-tag? (read-byte str)))
      (if (eq end-tag? +end-tag+)
	  (values object version)
	  (error "File corrupted or saved improperly; end-tag not found.")))))

(defgeneric save-image (document-universe pathname &optional estimated-file-size)
  (:documentation "Dumps a binary image of DOCUMENT-UNIVERSE to PATHNAME, after sparsifying."))

(defmethod save-image ((document-universe document-universe) pathname &optional estimated-file-size)
  (sparsify-document-universe document-universe)
  (dump-one document-universe pathname estimated-file-size))

(defgeneric load-image (pathname)
  (declare (values document-universe version.))
  (:documentation "Loads a saved-binary image of DOCUMENT-UNIVERSE from PATHNAME."))

(defmethod load-image (pathname)
  (read-one pathname))
