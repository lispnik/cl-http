;;; -*- Mode: LISP; Syntax: Common-lisp; Package: (BASE64 :USE FUTURE-COMMON-LISP :colon-mode :external); Base: 10 -*-

;;; Common Lisp implementation of Internet Base64 encoding.
;;; See RFC-1113 and RFC-1341.

;;; Copyright Massachusetts Institute of Technology, 1994
;;; Written by Mark Nahabedian
;;; encode vector made to handle strings -- JCMa 12/28/1994.
;;; all references to svref converted to aref due to mcl problem -- JCMa 12/28/1994.
;;;
;;; (C) Enhancements Copyright 1996, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;; added base64-encode-integer-into-string base64-decode-string-into-integer
;;; 10/5/96 -- JCMa.
;;; Exported base64-invalid-data-error, base64-odd-size-error, base64-decode-error   11/21/97 -- JCMa.

;(eval-when (load compile eval)
;  (defpackage BASE64
;    (:use future-common-lisp)
;    (:export "BASE64-ENCODE-VECTOR" "BASE64-DECODE-VECTOR" "WITH-ENCODING-VECTOR"
;             "BASE64-ENCODE-INTEGER-INTO-STRING" "BASE64-DECODE-STRING-INTO-INTEGER"
;             "BASE64-INVALID-DATA-ERROR" "BASE64-ODD-SIZE-ERROR" "BASE64-DECODE-ERROR")))

(in-package :base64)

(defmacro byte-to-char (byte)
  `(code-char ,byte))

;; Must be symetrical with byte-to-char on lispm to avoid bugs   4/24/97 -- JCMa.
(defmacro char-to-byte (char)
  `(char-code ,char))

(eval-when (load eval compile)
  (defconstant +base64-encoding-vector+
               "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    "The standard set of characters used for BASE64 encoding"))

(defvar *encoding-vector* +base64-encoding-vector+
  "The encoding and decoding routines use this value as their encoding vector.
This allows the encoding vector to be changes for applications where + and / arn't safe.")

(defconstant +base64-fill-char+ #\=)

(defvar *fill-char* +base64-fill-char+)

(defconstant +base64-max-encoded-bytes-per-line+ 64
  "Maximum number of encoded characters before inserting +base64-line-break-chars+")

(eval-when (compile eval load)

  (defconstant +base64-line-break-chars+ '(#\return)
    "Insert these in order at end of line")

  (defconstant +base64-ignore-chars+ '(#\Return #\space #\tab #\Linefeed))
  )


(defmacro with-encoding-vector ((code62-char code63-char &optional (fill-char #\=)) &body body)
  `(let ((*encoding-vector*
           ,(let ((new-encoding (make-array 64 :initial-contents +base64-encoding-vector+)))
              (setf (aref new-encoding 62) code62-char)
              (setf (aref new-encoding 63) code63-char)
              new-encoding))
         (*fill-char* ,fill-char))
     ,@body))


(define-condition base64-decode-error
                  (error)
  ((encoded-string :initarg :encoded-string
                   :initform (error ":ENCODED-STRING required")
                   :reader base64-encoded-string)
   (decoded-data :initarg :decoded-data
                 :initform (error ":DECODED-DATA required")
                 :reader base64-decoded-data)
   (start :initarg :start
          :initform (error ":START required")
          :reader base64-encoding-start)
   (end :initarg :end
        :initform (error ":END required")
        :reader base64-encoding-end)
   (input-index :initarg :index
                :initform (error ":INDEX required")
                :reader base64-encoding-index))
  (:report
    (lambda (condition stream)
      (declare (ignore condition))
      (format stream "Erroneous Base64 encoding"))))

(define-condition base64-odd-size-error 
                  (base64-decode-error)
  ()
  (:report
    (lambda (condition stream)
      (declare (ignore condition))
      (format stream "The Base64 encoding ended abruptly without the requisite padding"))))

(define-condition base64-invalid-data-error
                  (base64-decode-error)
  ((bad-char :initarg :char
             :initform (error ":CHAR required")
             :reader base64-invalid-char))
  (:report
    (lambda (condition stream)
      (format stream "The character ~s was read from a Base64 encoded string"
              (base64-invalid-char condition)))))

(defun base64-encode-vector (byte-vector &key (start 0) end
                                         (max-line-length +base64-max-encoded-bytes-per-line+)
                                         (characters-p (stringp byte-vector)))
  (flet ((get-end (vector)
           (if (array-has-fill-pointer-p vector)
               (fill-pointer vector)
               (length vector)))
         (determine-string-length (n-bytes)
           (+ n-bytes
              (multiple-value-bind (lines chars)
                  (floor n-bytes max-line-length)
                (if (zerop lines)
                    0
                    (* #.(length +base64-line-break-chars+)
                       (if (zerop chars)
                           (1- lines)
                           lines)))))))
    (declare (inline get-end determine-string-length))
    (let* ((end (or end (get-end byte-vector)))
           (n-bytes (- end start))
           (n-bytes-encoded (* 4 (ceiling (* 8 n-bytes) 24)))
           (result (make-string (determine-string-length n-bytes-encoded)))
           (in start)
           (out 0)
           (out-this-line 0))
      (macrolet ((getbyte ()
                   `(prog1 (if characters-p
                               (char-to-byte (aref byte-vector in))
                               (aref byte-vector in))
                           (incf in)))
                 ;; 111111 112222 222233 333333
                 ;; 111111 222222 333333 444444
                 (first12 (byte1 byte2)
                   `(let ((byte1 ,byte1)
                          (byte2 ,byte2))
                      (dpb byte1 (byte 8 4)
                           (ldb (byte 4 4) byte2))))
                 (second12 (byte2 byte3)
                   `(let ((byte2 ,byte2)
                          (byte3 ,byte3))
                      (dpb (ldb (byte 4 0) byte2)
                           (byte 4 8) byte3)))
                 (putbyte (byte)
                   `(macrolet ((pb1 (byte)
                                 `(progn (setf (aref result out) ,byte)
                                         (incf out))))
                      (when (>= out-this-line max-line-length)
                        (setq out-this-line 0)
                        (dotimes (i #.(length +base64-line-break-chars+))
                          (pb1 (elt +base64-line-break-chars+ i))))
                      (pb1 ,byte)
                      (incf out-this-line)))
                 (outpad ()
                   `(putbyte *fill-char*))
                 (out6 (6bits)
                   `(putbyte (aref *encoding-vector* ,6bits)))
                 (out12 (12bits)
                   `(let ((n ,12bits))
                      (out6 (ldb (byte 6 6) n))
                      (out6 (ldb (byte 6 0) n)))))
        (loop
          (case (- end in)
            (0 (return))
            (1 (out12 (first12 (getbyte) 0))
               (outpad) (outpad))
            (2 (let ((byte1 (getbyte))
                     (byte2 (getbyte)))
                 (out12 (first12 byte1 byte2))
                 (out6 (dpb (ldb (byte 4 0) byte2)      ;is this right?
                            (byte 6 2) 0))      ;spec says right is padded with zeros.
                 (outpad)))
            (t (let ((byte1 (getbyte))
                     (byte2 (getbyte))
                     (byte3 (getbyte)))
                 (out12 (first12 byte1 byte2))
                 (out12 (second12 byte2 byte3))))))
        result))))

(defun base64-decode-vector (base64-string &key (start 0) end
                                           (bogus-character-action :ignore)
                                           (ignore-chars +base64-ignore-chars+)
                                           (decoded-byte-type '(unsigned-byte 8)))
  (flet ((get-end (vector)
           (if (array-has-fill-pointer-p vector)
               (fill-pointer vector)
               (length vector))))
    (declare (inline get-end))
    (let* ((end (or end (get-end base64-string)))
           (data-end (or (position *fill-char* base64-string
                                   :test-not #'char-equal
                                   :from-end t
                                   :start start
                                   :end end)
                         end))
           (result (make-array (ceiling (* 3 (- data-end start)) 4)     ;should be an integer anyway
                               :element-type decoded-byte-type
                               :fill-pointer 0))
           (putbyte-mode (cond ((subtypep decoded-byte-type 'character) :char)
                               ((equal decoded-byte-type '(unsigned-byte 8)) :simple)
                               (t :coerce)))
           (in start))
      (macrolet ((size-check (b)
                   `(unless ,b
                      (error 'base64-odd-size-error
                             :encoded-string base64-string
                             :decoded-data result
                             :start start
                             :end end
                             :index (1- in))))
                 (getbyte ()
                   `(if (>= in end)
                        nil
                        (prog1 (aref base64-string in)
                               (incf in))))
                 ;; 11111122 22223333 33444444
                 ;; 11111111 22222222 33333333
                 (first8 (b1 b2)                ;assemble first byte from 2 6-bit qantities
                   `(let ((b1 ,b1) (b2 ,b2))
                      (putbyte (dpb b1
                                    (byte 6 2)
                                    (ldb (byte 2 4) b2)))))
                 (second8 (b2 b3)               ;assemble second byte from 2 6-bit qantities
                   `(let ((b2 ,b2) (b3 ,b3))
                      (putbyte (dpb (ldb (byte 4 0) b2)
                                    (byte 4 4)
                                    (ldb (byte 4 2) b3)))))
                 (third8 (b3 b4)                ;assemble third byte from 2 6-bit qantities
                   `(let ((b3 ,b3) (b4 ,b4))
                      (putbyte (dpb (ldb (byte 2 0) b3)
                                    (byte 2 6)
                                    b4))))
                 (putbyte (byte)
                   `(vector-push
                      (let ((byte ,byte))
                        (ecase putbyte-mode
                          (:char (byte-to-char byte))
                          (:simple byte)
                          (:coerce (coerce byte decoded-byte-type))))
                      result)))
        (flet ((get6 ()
                 (loop
                   (let ((c (getbyte)) v)
                     (unless c (return nil))
                     (cond ((char-equal c *fill-char*)
                            (return :pad))
                           ((setq v (position c *encoding-vector*))
                            (return v))
                           ((member c ignore-chars))
                           (t (ecase bogus-character-action
                                (:ignore)
                                (:warn (warn "Bogus Base64 character ~s" c))
                                (:error (error 'base64-invalid-data-error
                                               :encoded-string base64-string
                                               :decoded-data result
                                               :start start
                                               :end end
                                               :index (1- in)
                                               :char c)))))))))
          (declare (dynamic-extent #'get6))
          (loop
            (let ((b1 (get6)) b2 b3 b4)
              (unless b1 (return))
              (when (eq b1 :pad)
                (error "Pad seen in first byte of Base64 encoding"))
              (setq b2 (get6))
              (size-check b2)
              (if (eq b2 :pad)
                  (progn (first8 b1 0)
                         (return))
                  (first8 b1 b2))
              (setq b3 (get6)) 
              (size-check b3)
              (if (eq b3 :pad)
                  (progn (second8 b2 0)
                         (return))
                  (second8 b2 b3))
              (setq b4 (get6))
              (size-check b4)
              (if (eq b4 :pad)
                  (progn (third8 b3 0)
                         (return))
                  (third8 b3 b4))))))
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Size computations

(defun base64-encoded-size (decoded-size &key
                                         (max-line-length +base64-max-encoded-bytes-per-line+)
                                         (line-break-chars +base64-line-break-chars+))
  (let ((n-bytes-encoded (* 4 (ceiling (* 8 decoded-size) 24))))
    (multiple-value-bind (lines chars)
        (floor n-bytes-encoded max-line-length)
      (+ n-bytes-encoded (* (length line-break-chars)
                            (if (zerop chars)
                                (1- lines)
                                lines))))))

(defun base64-decoded-size (encoded-size &key
                                         (pad-chars 0)
                                         (max-line-length +base64-max-encoded-bytes-per-line+)
                                         (line-break-chars +base64-line-break-chars+))
  ;; assume MAX-LINE-LENGTH and LINE-BREAK-CHARS are the same as for when the
  ;; data was encoded.  Determine the number of line breaks inserted and
  ;; subtract those characters from ENCODED-SIZE.
  (assert (<= 0 pad-chars 2))
  (let ((encoded-data-size
          (multiple-value-bind (lines chars-remaining)
              (floor encoded-size (+ max-line-length (length line-break-chars)))
            (+ chars-remaining (* lines max-line-length)))))
    (unless (zerop (mod encoded-data-size 4))
      ;; somethings wrong, assume the worst
      (return-from base64-decoded-size encoded-size))
    (ceiling (* 3 (- encoded-data-size
                     (if (= pad-chars 2) 1 0)))
             4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Encoding utilities

;;; These are implemented as hairy macros so that everything winds up being open
;;; coded and we can avoid doing additional function calls for I/O.

#|
(defmacro encoding-base64-triple ((enc1 enc2 enc3 enc4)
                                  byte1 byte2 byte3
                                  &body body)
  "BYTE1, BYTE2 and BYTE3 are expressions which evaluate to three bytes to
be encoded.  ENC1, ENC2, ENC3 and ENC4 are bound to the encoded values."
  #+Genera (declare (zwei:indentation 1 6 4 2))
  `(let (,enc1 ,enc2 ,enc3 ,enc4)
     (let ((byte1 ,byte1) (byte2 ,byte2) (byte3 ,byte3))
       (macrolet ((enc (b) `(aref *encoding-vector* ,b)))
         (setq ,enc1
               (enc (ldb (byte 6 2) byte1)))
         (setq ,enc2
               (enc (dpb (ldb (byte 2 0) byte1)
                         (byte 2 4)
                         (if byte2
                             (ldb (byte 4 4) byte2)
                             0))))
         (setq ,enc3
               (if byte2
                   (enc (dpb (ldb (byte 4 0) byte2)
                             (byte 4 2)
                             (if byte3
                                 (ldb (byte 2 6) byte3)
                                 0)))
                   *fill-char*))
         (setq ,enc4
               (if byte3
                   (enc (ldb (byte 6 0) byte3))
                   *fill-char*))))
     ,@body))

(defmacro encoding-base64 (source sink &key
                           (max-line-length +base64-max-encoded-bytes-per-line+)
                           (line-break-chars +base64-line-break-chars+))
  (let ((column-var '#:column)
        (byte1-var '#:byte1)
        (max-line-var '#:max-line-length)
        (break-chars-var '#:line-break-chars)
        (output '#:output))
    `(let ((,max-line-var ,max-line-length)
           (,break-chars-var ',line-break-chars)
           (,column-var 0))
       (loop
         (let ((,byte1-var (,source)))
           (unless ,byte1-var
             (return))
           (encoding-base64-triple (o1 o2 o3 o4)
                                   ,byte1-var (,source) (,source)
                                   (macrolet ((,output (o)
                                               `(progn
                                                  (when (and ,',max-line-var
                                                             (>= ,column-var ,',max-line-var))                      
                                                    (dolist (c ,',break-chars-var)
                                                      (,',sink c))
                                                    (setq ,column-var 0)) 
                                                  (,',sink ,o)
                                                  (incf column))))
                                     (,output o1)
                                     (,output o2)
                                     (,output o3)
                                     (,output o4))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Decoding utilities

(defmacro decoding-base64-quad ((byte1 byte2 byte3)
                                enc1 enc2 enc3 enc4
                                &body body)
  #+Genera (declare (zwei:indentation 1 6 5 2))
  `(let (,byte1 ,byte2 ,byte3
         (enc1 ,enc1) (enc2 ,enc2) (enc3 ,enc3) (enc4 ,enc4))
     ;; just repack bits.  The caller has to do the character decoding since he
     ;; also has to filter out line breaks and bad data.  Our input data ENC1,
     ;; ENC2, ENC3 and ENC4 are either 6 bit integers or :PAD representing a pad
     ;; character.
     (setq ,byte1
           (dpb enc1 (byte 6 2) (ldb (byte 2 6) enc2)))
     (setq ,byte2
           (dpb (ldb (byte 4 0) enc2)
                (byte 4 4)
                (if enc3
                    (ldb (byte 2 4) enc3)
                    0)))
     (setq ,byte3
           (if enc3
               (dpb (ldb (byte 4 0) enc3)
                    (byte 4 4)
                    (if enc4 enc4 0))
               nil))
     ,@body))

(defmacro decoding-base64-char (char &key
                                (line-break-chars +base64-line-break-chars+ lbc?)
                                (ignore-chars +base64-ignore-chars+ ic?)
                                (bogus-character-action :ignore))
  (let ((char-var '#:char)
        (line-break-chars-var '#:line-break-chars)
        (ignore-chars-var '#:ignore-chars))
    `(let ((,char-var ,char)
           (,line-break-chars-var ,(if lbc? `,line-break-chars '+base64-line-break-chars+))
           (,ignore-chars-var ,(if ic? `,ignore-chars '+base64-ignore-chars+)))
       ,line-break-chars-var ,ignore-chars-var  ;ignorable
       (or (position ,char-var *encoding-vector*)
           (when (char-equal ,char-var *fill-char*)
             :pad)
           (unless (eq ,bogus-character-action :ignore)
             (unless (member ,char-var ,ignore-chars-var)
               (ecase ,bogus-character-action
                 (:warn (warn "Bogus Base64 character ~s" ,char-var))
                 (:error (error "Invalid Base64 encoding character: ~c"
                                ,char-var)))))))))

(defmacro decoding-base64 (source sink &key 
                           (line-break-chars +base64-line-break-chars+ lbc?)
                           (ignore-chars +base64-ignore-chars+ ic?)
                           (bogus-character-action :ignore))
  (let ((input '#:input))
    `(block done
       (loop
         (let (enc1 enc2 enc3 enc4)
           (macrolet ((,input (v)
                       `(loop
                          (let ((c (,',source)))
                            (unless c
                              (if enc1
                                  (error "Unexpected end of input while decodeing Base64 data")
                                  (return-from done)))
                            (decoding-base64-char
                              c
                              :line-break-chars ,',(if lbc? line-break-chars '+base64-line-break-chars+)
                              :ignore-chars ,',(if ic? ignore-chars '+base64-ignore-chars+)
                              :bogus-character-action ',',bogus-character-action)
                            (when c (return c))))))
             (,input enc1) (,input enc2) (,input enc3) (,input enc4))
           (decoding-base64-quad (byte1 byte2 byte3)
                                 enc1 enc2 enc3 enc4
                                 (when byte1 (,sink byte1))
                                 (when byte2 (,sink byte2))
                                 (when byte3 (,sink byte3))))))))

;; both of the following would be better if they went directly into or out of
;; base64 without the intermediate string.   10/5/96 -- JCMa.
(defun base64-encode-integer-into-string (integer &key (base 10.))
  (let ((string (write-to-string integer :base base)))
    (declare (dynamic-extent string))
    (base64:base64-encode-vector string :characters-p t))))

(defun base64-decode-string-into-integer (string &key (start 0) end (radix 10.))
  (let ((string (base64:base64-decode-vector string :start start :end end
                                             :decoded-byte-type 'character)))
    (declare (dynamic-extent string))
    (parse-integer string :radix radix)))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; streams

;; Blows out in MCL 2.0.1
#|(defun base64-encode-stream (input-stream output-stream)
    (macrolet ((in () `(read-byte input-stream))
               (out (c) `(write-char ,c output-stream)))
      (encoding-base64 in out)))

(defun base64-decode-stream (input-stream output-stream)
  (macrolet ((in () `(read-char input-stream))
             (out (c) `(write-byte ,c output-stream)))
    (decoding-base64 in out)))|#


#||
;;; Testing.  Unfortunately the specifications don't provide us with any
;;; data to test against.

;;; From "The Internet Message", M. Rose:
;;;   #x1f9d90 ==> "H52Q"

(defparameter +base64-test-vectors+ '(("H52Q" '(#x1f #x9d #x90))))

(defun make-test-vector (n)
  (let ((v (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n)
      (setf (aref v i)
            (ldb (byte 8 0) i)))
    v))

(defun verify-test-vector (v)
  (dotimes (i (length v) (values t v))
    (unless (= (aref v i) (ldb (byte 8 0) i))
      (return (values nil v i)))))


(verify-test-vector (base64-decode-vector
                      (base64-encode-vector (make-test-vector 300))))

(with-encoding-vector (#\_ #\- #\.)
  (verify-test-vector (base64-decode-vector
                        (base64-encode-vector (make-test-vector 300)))))

||#
