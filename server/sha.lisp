;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: (sha :USE FUTURE-COMMON-LISP); Base: 10 -*-
;;;
;;; Common Lisp implementation of the SHA Message-Digest Algorithm
;;; See Internet FIPS 180
;;; Copyright 1996,  Massachusetts Institute of Technology.
;;;
;;; Written by P. M. Hallam-Baker, based on MD5 code 
;;; by Mark Nahabedian, with enhancements by Tony Eng and John C. Mallery
;;; Artificial Intelligence Laboratory
;;; Massachusetts Institute of Technology
;;; 
;;;
;;; Major Efficiency, code beautification, and closification   10/2/96 -- JCMa.
;;; (C) Enhancements Copyright 1995-96, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;; Dramatic Performance improvements for Lispm   3/9/98 -- Rusty
;;; Not using bignums can improve performance similarly on other platforms

;(defpackage sha
;  (:use future-common-lisp)
;  (:export
;    "SHA-COMPRESS"
;    "SHA-ENCODE"
;    "SHA-MAC-ENCODE"
;    "SHA-DIGEST-BYTE-VECTOR"
;    "SHA-DIGEST-EQUAL"
;    "SHA-DIGEST-HEXADECIMAL-STRING"
;    "SHA-DIGEST-INTEGER"
;    "SHA-DIGEST-LIST"
;    "SHA-DIGEST-VECTOR"
;    "SHA-MAC-HEXADECIMAL-STRING"
;    "SHA-MAC-INTEGER"
;    "MAKE-SHA-BUFFER-FILLER-FROM-READER-FUNCTION"
;    "MAKE-FILTERED-READER-FUNCTION"))

(in-package :sha)

;;;------------------------------------------------------------------- 
;;;
;;; 32 BIT WORD OPERATORS
;;;
;;; Comments represent how the operation is expressed in the RFC.

(declaim (inline 32-add))

;;; a + b
(defun 32-add (a b)
  #+genera (sys:%32-bit-plus a b)
  #-genera (ldb (byte 32 0) (+ a b)))

(declaim (inline make-32-bit-unsigned-integer))

(defun make-32-bit-unsigned-integer (a)
  #+genera (sys:%logldb (byte 32 0) a)
  #-genera a)

(declaim (inline make-integer))

(defun make-integer (a)
  #+genera (dpb a (byte 32 0) 0)
  #-genera a)

(declaim (inline 32-left-rot))

;;; number <<< by
(defun 32-left-rot (number by)
  #+genera (si:rot number by)
  #-genera (let ((break (- 32 by)))
             (dpb (ldb (byte break 0) number)
                  (byte break by)
                  (ldb (byte by break) number))))

(declaim (inline 32-not))

;;; not num
(defun 32-not (num)
  (ldb (byte 32 0) (lognot num)))

(declaim (inline 32-or))

;;; a v b
(defun 32-or (a b)
  (logior a b))

(declaim (inline 32-xor))

;;; a XOR b
(defun 32-xor (a b)
  (logxor a b))

(declaim (inline 32-and))
;;; ab
(defun 32-and (a b)
  (logand a b))

;;;------------------------------------------------------------------- 
;;;
;;; PADDING CALCULATIONS
;;;

(declaim (inline sha-length64))

(defun sha-length64 (message-length-in-bits)
  (declare (values hi lo))
  (values (ldb (byte 32 32) message-length-in-bits)
          (ldb (byte 32 0) message-length-in-bits)))

;;;------------------------------------------------------------------- 
;;;
;;; MD5 CALCULATION STATE REGISTERS
;;;

(defmacro with-sha-state ((a b c d e) &body body)
  `(let (,a ,b ,c ,d, e)
     ,@body))

(defmacro initialize-sha-state (a b c d e)
  `(setf ,a (make-32-bit-unsigned-integer #x67452301)
         ,b (make-32-bit-unsigned-integer #xefcdab89)
         ,c (make-32-bit-unsigned-integer #x98badcfe)
         ,d (make-32-bit-unsigned-integer #x10325476)
         ,e (make-32-bit-unsigned-integer #xc3d2e1f0)))

;;;------------------------------------------------------------------- 
;;;
;;; FUNCTIONS
;;;

(declaim (inline sha-function-1))

(defun sha-function-1 (x y z)
  (logior (logand x y)(logand (lognot x) z) ))

(declaim (inline sha-function-2))

(defun sha-function-2 (x y z)
  (logxor x y z))

(declaim (inline sha-function-3))

(defun sha-function-3 (x y z)
  (logior (logand x y) (logand x z) (logand y z)))

(declaim (inline sha-function-4))

(defun sha-function-4 (x y z)
  (logxor x y z))

;;;------------------------------------------------------------------- 
;;;
;;; STACK CONSING
;;;

(defmacro with-stack-array ((var dimensions &rest keyargs) &body body)
  #+genera
  `(sys:with-stack-array (,var ,dimensions ,@keyargs) ,@body)
  #-genera
  `(let ((,var (make-array ,dimensions ,@keyargs)))
     (declare (dynamic-extent ,var))
     ,@body))

;;;------------------------------------------------------------------- 
;;;
;;; THE SHA ENCODER
;;;

;;
;; Expansion function.
;;
(defun sha-expand (m)
  (declare (values w))                          ; Is this the right declare form??? PHB
  (let ((array (make-array '(80))))
    (www-utils:with-fast-array-references ((w array))
      (dotimes (i 16 t)
        (setf (aref w i) (aref m i)  ) )
      (dotimes (i 64 t)
        (setf (aref w (+ i 16)) 
              (32-left-rot (logxor (aref w (+ i 13)) (aref w (+ i 8)) 
                                   (aref w (+ i 2)) (aref w (+ i ))) 1) ))
      ;; add in left circular shift
      w)))

(defun sha-compress (a b c d e x)
  (let ((w (sha-expand x)) 
        (temp))
    (www-utils:with-fast-array-references ((w w))
      ;; (dump-sha-expansion-buffer w) ; debug hack
      ;; Round 1
      ;;(format *trace-output* "round 1~%")
      (dotimes (i 20 t)
        ;;(format *trace-output* "~&A=~X  B=~X  C=~X  D=~X  E=~X ~%" a b c d e)
        (setf temp (32-add (32-left-rot a 5)
                           (32-add (sha-function-1 b c d)
                                   (32-add e
                                           (32-add (aref w i)
                                                   (make-32-bit-unsigned-integer #x5a827999))))))
        (setf e d) (setf d c) (setf c (32-left-rot b 30 )) (setf b a)
        (setf a temp))
      ;; Round 2
      ;;(format *trace-output* "round 2~%")
      (dotimes (i 20 t)
        ;;(format *trace-output* "~&A=~X  B=~X  C=~X  D=~X  E=~X ~%" a b c d e)
        (setf temp (32-add (32-left-rot a 5)
                           (32-add (sha-function-2 b c d)
                                   (32-add e
                                           (32-add (aref w (+ i 20))
                                                   (make-32-bit-unsigned-integer #x6ed9eba1))))))
        (setf e d) (setf d c) (setf c (32-left-rot b 30 )) (setf b a)
        (setf a temp))
      ;; Round 3
      ;;(format *trace-output* "round 3~%")
      (dotimes (i 20 t)
        ;;(format *trace-output* "~&A=~X  B=~X  C=~X  D=~X  E=~X ~%" a b c d e)
        (setf temp (32-add (32-left-rot a 5)
                           (32-add (sha-function-3 b c d)
                                   (32-add e
                                           (32-add (aref w (+ i 40))
                                                   (make-32-bit-unsigned-integer #x8f1bbcdc))))))
        (setf e d) (setf d c) (setf c (32-left-rot b 30 )) (setf b a)
        (setf a temp))
      ;; Round 4
      ;;(format *trace-output* "round 4~%")
      (dotimes (i 20 t)
        ;;(format *trace-output* "~&A=~X  B=~X  C=~X  D=~X  E=~X ~%" a b c d e)
        (setf temp (32-add (32-left-rot a 5)
                           (32-add (sha-function-4 b c d)
                                   (32-add e
                                           (32-add (aref w (+ i 60))
                                                   (make-32-bit-unsigned-integer #xca62c1d6))))))
        (setf e d) (setf d c) (setf c (32-left-rot b 30 )) (setf b a)
        (setf a temp))))
  (values a b c d e))

;;; GET-DATA-FUNCTION takes an array of 16 32 bit words as argument and
;;; fills the array from its source.  It returns T if there is more data
;;; to be read and NIL if all data (including padding and length) have
;;; been delivered)
(defun %sha-encode (get-data-function)
  (declare (values a b c d e))
  (with-stack-array (x 16
                       #+genera :type #+genera 'sys:art-fixnum
                       #-genera :element-type #-genera '(unsigned-byte 32))
    (with-sha-state (a b c d e)
      (initialize-sha-state a b c d e)
      (loop while (funcall get-data-function x)
            do #|(dump-sha-data-buffer x)|#     ; debug hack
               (multiple-value-bind (aa bb cc dd ee)
                   (sha-compress a b c d e x)
                 (setq a (32-add a aa)
                       b (32-add b bb)
                       c (32-add c cc)
                       d (32-add d dd)
                       e (32-add e ee))))
      (values (make-integer a)
              (make-integer b)
              (make-integer c)
              (make-integer d)
              (make-integer e)))))


;;;------------------------------------------------------------------- 
;;;
;;; MESSAGE AUTHENTICATION CODE (MAC) -- SHARED SECRET
;;;

;; digest compress with a secret key
(defun %sha-mac-encode (key get-data-function)
  (declare (values a b c d e))
  (check-type key integer)
  (macrolet ((explode-integer (integer n bits)
               `(values ,.(loop for idx upfrom 0 below n
                                collect `(make-32-bit-unsigned-integer (ldb (byte ,bits ,(* bits idx)) ,integer)))))
             (integer-values (&rest values)
               `(values . ,(loop for v in values
                                 collect `(make-integer ,v)))))
    (multiple-value-bind (ke kd kc kb ka)
        (explode-integer key 5 32)
      (with-stack-array (x 16 #+genera :type #+genera 'sys:art-fixnum
                           #-genera :element-type #-genera '(unsigned-byte 32))
        (with-sha-state (a b c d e)
          (initialize-sha-state a b c d e)
          (loop while (funcall get-data-function x)
                do #|(dump-sha-data-buffer x)|# ; debug hack
                   (multiple-value-bind (aa bb cc dd ee)
                       (sha-compress (logxor a ka)
                                     (logxor b kb)
                                     (logxor c kc)
                                     (logxor d kd)
                                     (logxor e ke)
                                     x)
                     (setq a (32-add a aa)
                           b (32-add b bb)
                           c (32-add c cc)
                           d (32-add d dd)
                           e (32-add e ee))))
          (integer-values a b c d e))))))

;;;------------------------------------------------------------------- 
;;;
;;; CONVERTING THE RESULT TO A BYTE VECTOR
;;;

(defmacro sha-digest-byte-vector-big-endian (a b c d e &optional (unitizer 'vector))
  (flet ((bytes (num32)
           `((ldb (byte 8 24) ,num32)
             (ldb (byte 8 16) ,num32)
             (ldb (byte 8 8) ,num32)
             (ldb (byte 8 0) ,num32))))
    `(let ((a ,a) (b ,b) (c ,c) (d ,d) (e, e))
       (,unitizer
        ,@(bytes 'a)
        ,@(bytes 'b)
        ,@(bytes 'c)
        ,@(bytes 'd)
        ,@(bytes 'e)))))

(defmacro sha-digest-byte-vector-little-endian (a b c d e &optional (unitizer 'vector))
  (flet ((bytes (num32)
           `((ldb (byte 8 0) ,num32)
             (ldb (byte 8 8) ,num32)
             (ldb (byte 8 16) ,num32)
             (ldb (byte 8 24) ,num32))))
    `(let ((a ,a) (b ,b) (c ,c) (d ,d) (e, e))
       (,unitizer
        ,@(bytes 'a)
        ,@(bytes 'b)
        ,@(bytes 'c)
        ,@(bytes 'd)
        ,@(bytes 'e)))))

;;; The SHA specification says that the bytes should be in little-endian order.
(defmacro sha-digest-byte-vector (a b c d e)
  "Constructs a byte vector from the 5 values returned by MD5-ENCODE."
  `(sha-digest-byte-vector-big-endian ,a ,b ,c ,d ,e vector))

(defmacro sha-digest-byte-list (a b c d e)
  "Constructs a byte list from the 5 values returned by MD5-ENCODE."
  `(sha-digest-byte-vector-big-endian ,a ,b ,c ,d ,e list))

;; changed hex chars to lower-case, it makes a difference. -- CVince 10/14/1995.
(defun sha-hexadecimal-encode (&rest sha-values)
  "Encodes 160 bit SHA as a 32 character ASCII hexadecimal string."
  (declare (dynamic-extent sha-values))
  (macrolet ((hexdecimal-digit (hexdecimal position)
               `(aref "0123456789abcdef" (ldb (byte 4 ,position) ,hexdecimal)))
             (next-idx (n)
               `(1+ (the fixnum ,n))))
    (let ((string (make-array 40 :element-type http::*standard-character-type*)))
      (www-utils:with-fast-array-references ((string string))
        (loop for item in sha-values
              for idx upfrom 0 by 2
              do (setf (aref string idx) (hexdecimal-digit item 4)
                       (aref string (next-idx idx)) (hexdecimal-digit item 0))))
      string))) 

(defmacro sha-digest-byte-hexadecimal-string (a b c d e)
  "Constructs a byte hexadecimal string from the 5 values returned by SHA-ENCODE."
  `(sha-digest-byte-vector-big-endian ,a ,b ,c ,d ,e sha-hexadecimal-encode))

(defmacro sha-digest-byte-integer (a b c d e)
  "Constructs a byte integer from the 5 values returned by SHA-ENCODE."
  `(dpb ,a (byte 32 128)     
        (dpb ,b (byte 32 96)     
             (dpb ,c (byte 32 64)     
                  (dpb ,d (byte 32 32) ,e)))))

;;;------------------------------------------------------------------- 
;;;
;;; USEFUL FOR DEFINING SOURCES
;;;

;;; Define a constructor function for a source.  The constructor
;;; function returns a function suitable as the GET-DATA-FUNCTION
;;; argument to MD5-ENCODE.

;;; Only knows how to deal with 8 bit sources.

(defun make-sha-buffer-filler-from-reader-function (reader-function)
  "Returns a function which can be used as the GET-DATA-FUNCTION argument to SHa-ENCODE.
The argument is a function of no arguments, which returns sucessive 8 bit bytes from 
the source to be encoded, or NIL when the source is exhausted"
  (let ((state :data)
        (count 0)
        ;; the following is used to indicate if the first (#80) pad byte was
        ;; written when the index i was 13:
        (flag13 nil))
    (flet ((fill-sha-buffer (buffer)
             (www-utils:with-fast-array-references ((buffer buffer))
               (dotimes (i 16 t)
                 ;; I is which 32 bit word of buffer to write to.
                 (flet ((gb ()
                          (ecase state
                            (:done (return-from fill-sha-buffer nil))
                            (:data
                              (let ((byte (funcall reader-function)))
                                (cond (byte
                                       (incf count)
                                       byte)
                                      (t (setq state :must-pad)
                                         (when (= i 13)
                                           (setq flag13 1))
                                         #x80))))       ;first pad byte
                            ;; If we start writing the padding during the 14th
                            ;; word, we must pad the entire buffer and write
                            ;; the length in the next one.
                            (:must-pad
                              ;; this takes care of case when #x80 is the
                              ;; last byte written when i=13, and the next
                              ;; byte is first byte of i=14, in which case
                              ;; length should be written, not another full 
                              ;; buffer of zeroes
                              (when (and (= i 14) flag13)
                                (setq state :pad))
                              (unless (= i 14)
                                (setq state :pad)
                                (setq flag13 nil))
                              0)
                            (:pad
                              (if (= i 14)
                                  (multiple-value-bind (sha-length64-hi sha-length64-lo)
                                      (sha-length64 (* 8 count))
                                    (setf (aref buffer 14)
                                          sha-length64-hi)
                                    (setf (aref buffer 15)
                                          sha-length64-lo)      ; NB Opposite order to MD5 !
                                    (setq state :done)
                                    (return-from fill-sha-buffer t))
                                  0)))))
                   (declare (inline gb))
                   ;; FIPS-80 Bytes are ordered in opposite manner to MD5 :-(
                   (let ((b3 (gb))
                         (b2 (gb))
                         (b1 (gb))
                         (b0 (gb)))
                     (setf (aref buffer i)
                           (make-32-bit-unsigned-integer
                             (dpb b3 (byte 8 24)
                                  (dpb b2 (byte 8 16)
                                       (dpb b1 (byte 8 8) b0)))))))))))
      #'fill-sha-buffer)))

;;;------------------------------------------------------------------- 
;;;
;;; SOURCES
;;;

;;; The reader function returns the next byte from the source, or NIL if the
;;; source is exhausted.

;;; provide a way to hack around the lisp system's idea of character sets in an
;;; implementation independent way.
(defmacro character-to-byte (char)
  #+genera `(scl:ascii-code ,char)
  #-genera `(char-int ,char))

(defun make-string-reader-function (string &optional (start 0) end)
  (let ((index start)
        (end (or end (length string))))
    (flet ((string-reader-function ()
             (if (< index end)
                 (prog1 (character-to-byte (aref string index))
                        (incf index))
                 nil)))
      #'string-reader-function)))

(defun make-integer-reader-function (value)
  (let ((index (ceiling (integer-length value) 8)))
    ;;(format *trace-output* "Hello ~X  ~X  ~%" value index)
    (flet ((integer-reader-function ()
             (when (> index 0)
               (prog1 (ldb (byte 8 (* (decf index) 8)) value)
                      ;;(format *trace-output* "value ~x ~%" (ldb (byte 8 (* index 8)) value))
                      ))))
      #'integer-reader-function)))

(defun make-vector-reader-function (vector &optional (start 0) end)
  (let ((index start)
        (end (or end (length vector))))
    (flet ((vector-reader-function ()
             (if (< index end)
                 (prog1 (aref vector index)
                        (incf index))
                 nil)))
      #'vector-reader-function)))

;;; should anything happen between strings?  eg. return newline?
(defun make-list-of-strings-reader-function (list-of-strings)
  (let ((strings list-of-strings)
        string index end)
    (flet ((list-of-strings-read-function ()
             (flet ((next-string ()
                      (setq string (pop strings))
                      (unless string
                        (return-from list-of-strings-read-function nil))
                      (setq index 0)
                      (setq end (length string))))
               (declare (inline next-string))
               (loop initially (unless string (next-string))
                     until (< index end)
                     doing (next-string)
                     finally (return (prog1 (aref string index)
                                            (incf index)))))))
      #'list-of-strings-read-function)))

;;; This reader function can be composed with others if you want to selectively
;;; ignore some elements of the input when computing the SHA hash.
(defun make-filtered-reader-function (reader-function predicate)
  (flet ((alpha-digit-only-reader-function ()
           (loop for c = (funcall reader-function)
                 while c
                 when (funcall predicate c)
                   return c
                 finally (return nil))))
    #'alpha-digit-only-reader-function))


;;;------------------------------------------------------------------- 
;;;
;;; ENCODING DIGESTS
;;;

(defgeneric sha-encode (object &key))

(defmethod sha-encode (object &key)
  (error "No method defined for an object, ~S, of type, ~S." object (type-of object)))

(defmethod sha-encode ((string string) &key (start 0) end)
  (let* ((reader-fctn (make-string-reader-function string start end))
         (filler-fctn (make-sha-buffer-filler-from-reader-function reader-fctn)))
    (declare (dynamic-extent reader-fctn filler-fctn))
    (%sha-encode filler-fctn)))

(defmethod sha-encode ((integer integer) &key)
  (let* ((reader-fctn (make-integer-reader-function integer))
         (filler-fctn (make-sha-buffer-filler-from-reader-function reader-fctn)))
    (declare (dynamic-extent reader-fctn filler-fctn))
    (%sha-encode filler-fctn)))

(defmethod sha-encode ((vector vector) &key (start 0) end)
  (let* ((reader-fctn (make-vector-reader-function vector start end))
         (filler-fctn (make-sha-buffer-filler-from-reader-function reader-fctn)))
    (declare (dynamic-extent reader-fctn filler-fctn))
    (%sha-encode filler-fctn)))

;; this should be specialized by different platforms to perform this operation
;; in a buffer oriented manner.   9/30/96 -- JCMa.

(defmethod character-stream-get-byte-reader (stream)
  (flet ((get-byte-from-char ()
           (let ((char (read-char stream nil nil)))
             (when char
               (character-to-byte char)))))
    #'get-byte-from-char))

#+Genera
(defmethod character-stream-get-byte-reader ((stream si:buffered-input-stream))
  (let (buffer index limit)
    (flet ((get-byte-from-char ()
             (flet ((get-input-buffer ()
                      (multiple-value-setq (buffer index limit)
                        (scl:send stream :read-input-buffer))
                      (unless buffer
                        (return-from get-byte-from-char nil))))
               (declare (inline get-input-buffer))
               (unless buffer (get-input-buffer))
               (unless (< index limit)
                 (scl:send stream :advance-input-buffer)
                 (get-input-buffer))
               (www-utils:with-fast-array-references ((buffer buffer))
                 (let ((char (aref buffer index)))
                   (prog1 (when char
                            (character-to-byte char))
                          (incf index)))))))
      #'get-byte-from-char)))

(defmethod binary-stream-get-byte-reader (stream)
  (flet ((get-byte-from-char ()
           (read-byte stream nil nil)))
    #'get-byte-from-char))

#+Genera
(defmethod binary-stream-get-byte-reader ((stream si:buffered-input-stream))
  (let (buffer index limit)
    (flet ((get-byte ()
             (flet ((get-input-buffer ()
                      (multiple-value-setq (buffer index limit)
                        (scl:send stream :read-input-buffer))
                      (unless buffer
                        (return-from get-byte nil))))
               (declare (inline get-input-buffer))
               (unless buffer (get-input-buffer))
               (unless (< index limit)
                 (scl:send stream :advance-input-buffer)
                 (get-input-buffer))
               (www-utils:with-fast-array-references ((buffer buffer))
                 (prog1 (aref buffer index) 
                        (incf index))))))
      #'get-byte)))

(defmethod sha-encode ((pathname pathname) &key (element-type '(unsigned-byte 8)))
  (with-open-file (stream pathname :direction :input
                          :element-type element-type :if-does-not-exist :error)
    (let* ((reader-fctn (if (subtypep element-type http::*standard-character-type*)
                            (character-stream-get-byte-reader stream)
                            (binary-stream-get-byte-reader stream)))
           (filler-fctn (make-sha-buffer-filler-from-reader-function reader-fctn)))
      (declare (dynamic-extent reader-fctn filler-fctn))
      (%sha-encode filler-fctn))))


;;;------------------------------------------------------------------- 
;;;
;;; ENCODING MACS
;;;

(defgeneric sha-mac-encode (key object &key))

(defmethod sha-mac-encode (key object &key)
  (declare (ignore key))
  (error "No method defined for an object, ~S, of type, ~S." object (type-of object)))

(defmethod sha-mac-encode ((key integer) (string string) &key (start 0) end)
  (let* ((reader-fctn (make-string-reader-function string start end))
         (filler-fctn (make-sha-buffer-filler-from-reader-function reader-fctn)))
    (declare (dynamic-extent reader-fctn filler-fctn))
    (%sha-mac-encode key filler-fctn)))

(defmethod sha-mac-encode ((key integer) (integer integer) &key)
  (let* ((reader-fctn (make-integer-reader-function integer))
         (filler-fctn (make-sha-buffer-filler-from-reader-function reader-fctn)))
    (declare (dynamic-extent reader-fctn filler-fctn))
    (%sha-mac-encode key filler-fctn)))

;; this should be specialized by different platforms to perform this operation
;; in a buffer oriented manner.   9/30/96 -- JCMa.
(defmethod sha-mac-encode ((key integer) (pathname pathname) &key (element-type '(unsigned-byte 8)))
  (with-open-file (stream pathname :direction :input
                          :element-type element-type :if-does-not-exist :error)
    (flet ((get-byte-from-char ()
             (let ((char (read-char stream nil nil)))
               (and char (character-to-byte char))))
           (get-byte ()
             (read-byte stream nil nil)))
      (declare (dynamic-extent #'get-byte-from-char #'get-byte))
      (let ((filler-fctn (make-sha-buffer-filler-from-reader-function
                           (if (subtypep element-type http::*standard-character-type*)
                               #'get-byte-from-char
                               #'get-byte))))
        (declare (dynamic-extent filler-fctn))
        (%sha-mac-encode key filler-fctn)))))


;;;------------------------------------------------------------------- 
;;;
;;; APPLICATION-LEVEL FUNCTIONS
;;;

(defun sha-digest-equal (digest1 digest2)
  "Compares two sha digests and returns when they are equivalent."
  (cond ((and (consp digest1) (consp digest2))
         (loop for l1 = digest1 then (cdr l1)
               for l2 = digest2 then (cdr l2)
               while (and l1 l2)
               unless (= (car l1) (car l2))
                 do (return-from sha-digest-equal nil)
               finally (return-from sha-digest-equal (not (or l1 l2)))))
        (t (loop for idx1 upto (1- (length digest1))
                 unless (= (elt digest1 idx1)
                           (elt digest2 idx1))
                   do (return-from sha-digest-equal nil)
                 finally (return-from sha-digest-equal t)))))

(defun sha-digest-vector (object &rest args)
  "Returns a sha digest vector for STRING."
  (declare (dynamic-extent args)
           (values sha-digest-vector))
  (multiple-value-bind (a b c d e)
      (apply #'sha-encode object args)
    (sha-digest-byte-vector a b c d e)))

;; The advantage of this one is that one can use EQUAL hashtables to hash MD5s
;; to values.  7/18/95 -- JCMa.
(defun sha-digest-list (object &rest args)
  "Returns a sha digest list for STRING."
  (declare (dynamic-extent args)
           (values sha-digest-vector))
  (multiple-value-bind (a b c d e)
      (apply #'sha-encode object args)
    (sha-digest-byte-list a b c d e)))

;; Defined by draft IETF RFC  HTTP Digest aa-01   10/1/95 -- JCMa.
(defun sha-digest-hexadecimal-string (object &rest args)
  "Returns a SHA digest hexadecimal string for OBJECT."
  (declare (dynamic-extent args))
  (multiple-value-bind (a b c d e)
      (apply #'sha-encode object args)
    (sha-digest-byte-hexadecimal-string a b c d e)))

(defun sha-digest-integer (object &rest args)
  "Returns a SHA digest integer for OBJECT."
  (declare (dynamic-extent args))
  (multiple-value-bind (a b c d e)
      (apply #'sha-encode object args)
    (sha-digest-byte-integer a b c d e)))

(defun sha-mac-hexadecimal-string (key object &rest args)
  "Returns a SHA MAC hexadecimal string for STRING."
  (declare (dynamic-extent args))
  (multiple-value-bind (a b c d e)
      (apply #'sha-mac-encode key object args)
    (sha-digest-byte-hexadecimal-string a b c d e)))

(defun sha-mac-integer (key object &rest args)
  "Returns a SHA MAC integer for STRING."
  (declare (dynamic-extent args))
  (multiple-value-bind (a b c d e)
      (apply #'sha-mac-encode key object args)
    (sha-digest-byte-integer a b c d e)))

#+ignore
(defun test (n string)
  (time (loop for idx upfrom 0 below n
              do (sha-digest-hexadecimal-string string))))
