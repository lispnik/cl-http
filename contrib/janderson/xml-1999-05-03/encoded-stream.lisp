;;; -*- package: "XML-PARSER"; -*-
;;;

"
<DOCUMENTATION>
 <DESCRIPTION>
  <P>
  <CODE>ENCODED-STREAM</CODE> implementes a class of stream which caches a delegated
  stream (either binary or string) and wraps its reader/writer-args in functions which
  perform decoding/encoding in the case of a binary stream.
  </P>
  <P>
  they operate similar to concatenated streams, but since their intended
  application domain is XML decoding/encoding, they also permit string content to be
  pushed back into the stream (as is necessary to handle parsed entity processing) and
  perform encoding autodetection.
  </P>
  </DESCRIPTION>
 <CHRONOLOGY>
  <DELTA><DATE>19981123</DATE>
   new</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
 "

(in-package "XML-PARSER")

#-(or ALLEGRO CCL)
(progn
  (error "where is the implementation for binary-stream?"))


(defClass encoded-stream (stream)
  ((filter
    :initarg :filter
    :accessor encoded-stream.filter
    :type function
    :documentation
    "a function of either no arguments (for input) or one argument (for output)
     which performs the requisite encoding/decoding. in addition it handles eof processing." )
   (sources
    :initform nil
    :accessor encoded-stream.sources
    :documentation
    "a list of either coding-filter or function elements. streams are transformed to coding-filters
     while strings are transformed to closures.")
   (stream
    :initarg :stream
    :reader encoded-stream.stream)
   #+CCL
   (element-type
    :initarg :element-type))
  (:documentation
   "an <CODE>ENCODED-STREAM</CODE> acts as a data filter for serialized XML-data.
    in order to effect the correct encoding for a stream, it performs data
    transfers through a filter (<CODE>CODED-STREAM.FILTER</CODE>), which is
    generated as appropriate for the stream's encoding.")
  (:metaclass abstract-class))

(defClass decoding-stream (encoded-stream #+CCL input-stream #+ALLEGRO fundamental-character-input-stream)
  ()
  (:metaclass qualified-class)
  (:default-initargs :element-type 'character))

(defClass encoding-stream (encoded-stream #+CCL output-stream #+ALLEGRO fundamental-character-output-stream)
  ()
  (:metaclass qualified-class)
  (:default-initargs :element-type 'character))

(defClass coding-filter ()
  ((stream
    :initarg :stream
    :reader coding-filter.stream)
   (encoding
    :initform nil
    :accessor coding-filter.encoding)
   (function
    :initform nil))
  (:documentation
   "an <CODE>coding-filter</CODE> wraps a target stream, to specify its encoding
    and the function necessary to perform stream operations on the respectively
    encoded target stream.
    the filter is generated dynamically based on the encoding as a
    closure over the reader/writer args to expedite the translation between
    characters and the encoding. the encoding is either specified as
    an initialization keyword or is autodetected by examining the stream."))

(defMethod coding-filter.filter
           ((source null))
  nil)


;;;
;;; standard stream interface

(defun encoded-stream-tyi (stream)
  (with-slots (filter) stream
    (when filter (funcall filter))))

(defMethod stream-tyi ((stream decoding-stream))
  (encoded-stream-tyi stream))

#+ALLEGRO
(defMethod stream-read-char
           ((stream decoding-stream))
  (encoded-stream-tyi stream))

(defMethod stream-untyi ((stream decoding-stream) (datum character))
  (prepend-stream datum stream)
  datum)

#+ALLEGRO
(defMethod stream-unread-char
           ((stream decoding-stream) (datum character))
  (prepend-stream datum stream)
  datum)


(defun encoded-stream-tyo
       (stream datum)
  (with-slots (filter) stream
    (funcall filter datum)))

(defMethod stream-tyo
           ((stream encoding-stream) char)
  (encoded-stream-tyo stream char))

(defun encoded-stream-eofp (stream &aux source)
  (with-slots (filter sources)
              stream
    (typecase (setf source (first sources))
      (function nil)
      (stream (stream-eofp source))
      (t t))))

(defMethod stream-eofp
           ((stream decoding-stream))
  (encoded-stream-eofp stream))

(defMethod stream-eofp
           ((source coding-filter))
  (stream-eofp (coding-filter.stream source)))


(defMethod stream-reader ((stream decoding-stream))
  (values #'encoded-stream-tyi stream))

(defMethod stream-decoder ((stream decoding-stream))
  #'(lambda (&aux (fun (slot-value stream 'filter)))
      (when fun (funcall fun))))

#+CCL\
(defMethod stream-decoder ((stream stream))
  #'(lambda () (stream-tyi stream)))

#+ALLEGRO
(defMethod stream-decoder ((stream stream))
  #'(lambda () (stream-read-char stream)))


(defMethod stream-writer ((stream encoding-stream))
  (values #'encoded-stream-tyo stream))

(defMethod stream-encoder ((stream encoding-stream))
  #'(lambda (char) (funcall (slot-value stream 'filter) char)))


;;; this is to be replaced with an efective method 
#+ALLEGRO
(defMethod stream-reader
           ((stream fundamental-binary-input-stream))
  (values #'(lambda (arg)
              (declare (ignore arg))
             (stream-read-byte stream))
          nil))

#+ALLEGRO
(defMethod stream-reader
           ((stream fundamental-character-input-stream))
  (values #'(lambda (arg)
              (declare (ignore arg))
             (stream-read-char stream))
          nil))


;;;
;;; vector streams for use with external entities

(defClass vector-input-stream (#+MCL ccl::input-binary-stream
                               #+ALLEGRO excl::fundamental-binary-input-stream)
  ((position :initform 0)
   (vector :initarg :vector :reader vector-input-stream.vector))
  (:default-initargs :direction :input))

(defMethod stream-position
           ((stream vector-input-stream) &optional new)
  (with-slots (position) stream
    (if new
      (setf position new)
      position)))

(defMethod print-object
           ((vs vector-input-stream) (stream t)
            &aux (*print-array* t) (*print-length* 32) (*print-base* 16))
  (print-unreadable-object (vs stream)
    (princ (vector-input-stream.vector vs) stream)))

(defMethod stream-eofp
           ((stream vector-input-stream))
  (with-slots (position vector) stream
    (>= position (length vector))))

(defMethod stream-tyi ((stream vector-input-stream))
  (with-slots (position vector) stream
    (when (< position (length vector))
      (prog1 (svref vector position)
        (incf position)))))

(defMethod stream-untyi ((stream vector-input-stream) (datum integer))
  (with-slots (position vector) stream
    (cond ((< position 0)
           (decf position)
           (setf (svref vector position) datum))
          (t
           (error 'end-of-file :stream stream)))))

(defMethod stream-reader ((stream vector-input-stream))
  (with-slots (vector position) stream
    #'(lambda (ignore) (declare (ignore ignore))
       (when (< position (length vector))
         (prog1 (svref vector position)
           (incf position))))))



;;;
;;; interface functions for constructing streams and reporting state

(defMethod encoded-stream
           ((stream #+CCL input-stream #+ALLEGRO fundamental-input-stream) &rest initargs)
  (apply #'make-instance 'decoding-stream
         :stream stream
         initargs))

(defMethod encoded-stream
           ((stream string-stream) &rest initargs &key (encoding "UTF-8"))
  (apply #'call-next-method stream
         :encoding encoding
         initargs))

(defMethod encoded-stream
           ((stream #+CCL output-stream #+ALLEGRO fundamental-output-stream) &rest initargs)
  (apply #'make-instance 'encoding-stream
         :stream stream
         initargs))

(defMethod encoded-stream
           ((stream encoded-stream) &key)
  stream)

(defMethod encoded-stream
           ((source string) &rest initargs)
  (apply #'make-instance 'decoding-stream
         :stream source
         initargs))



(defMethod stream-state
           ((stream encoded-stream))
  (stream-state (encoded-stream.stream stream)))

#| these are now wrapped in functions and not accessable
(defMethod stream-state
           ((stream coding-filter))
  (stream-state (encoded-stream.stream stream)))
|#

(defMethod stream-position
           ((stream encoding-stream) &optional where)
  (declare (ignore where))
  (with-slots (sources) stream
    (some #'stream-position sources)))

;;;
;;; support for stacked sources

(defMethod encoded-stream-pop
           ((stream decoding-stream)
            &aux source next)
  (with-slots (filter sources) stream
    (setf source (pop sources))
    (typecase source
      (stream (stream-close source))
      (t ))
    (typecase (setf next (first sources))
      (null (setf filter nil))
      (function (setf filter next))
      (coding-filter (setf filter (make-decoding-filter stream next))))
    source))

(defMethod encoded-stream-pop-and-read
           ((stream decoding-stream))
  (with-slots (filter) stream
    (encoded-stream-pop stream)
    (if filter
      (funcall filter)
      (error 'end-of-file :stream stream))))

(defMethod make-decoding-filter
           ((stream decoding-stream) (char character))
  (assert (typep char 'character))
  #'(lambda ()
      (prog1 char
        (encoded-stream-pop stream))))

(defMethod make-decoding-filter
       ((stream decoding-stream) (string string)
        &aux (pos 0) (length (length string)))
  #'(lambda ()
      (if (>= pos length)
        (encoded-stream-pop-and-read stream)
        (prog1 (char string pos)
          (incf pos)))))

(defMethod make-decoding-filter
       ((stream decoding-stream) (stream-to-filter stream)
        &aux reader arg)
  (multiple-value-setq (reader arg)
    (stream-reader stream-to-filter))
  #'(lambda (&aux char)
      (if (characterp (setf char (funcall reader arg)))
        char
        (encoded-stream-pop-and-read stream))))

(defMethod make-decoding-filter
           ((stream decoding-stream) (source-to-filter coding-filter)
        &aux (reader (coding-filter.function source-to-filter)))
  #'(lambda (&aux char)
      (if (characterp (setf char (funcall reader)))
        char
        (encoded-stream-pop-and-read stream))))
     



(defMethod prepend-stream
           ((pre-content string) (stream decoding-stream) &key encoding)
  (declare (ignore encoding))
  (with-slots (filter sources) stream
    (push (setf filter (make-decoding-filter stream pre-content)) sources))
  stream)

(defMethod prepend-stream
           ((pre-content vector) (stream decoding-stream) &key encoding)
  (prepend-stream (make-instance 'vector-input-stream :vector pre-content) stream
                  :encoding encoding))
  

(defMethod prepend-stream
           ((pre-content stream) (stream decoding-stream) &key encoding)
  (declare (ignore encoding))
  (with-slots (filter sources) stream
    (push (setf filter (make-decoding-filter stream pre-content)) sources))
  stream)

(defMethod prepend-stream
           ((datum symbol) (stream decoding-stream) &key)
  (prepend-stream (string datum) stream))

(defMethod prepend-stream
           ((datum null) (stream decoding-stream) &key)
  stream)

(defMethod prepend-stream
           ((pre-content character) (stream decoding-stream) &key)
  (with-slots (filter sources) stream
    (push (setf filter (make-decoding-filter stream pre-content)) sources)
    stream))

(defMethod prepend-stream
           ((pre-content fixnum) (stream decoding-stream) &key)
  (with-slots (filter sources) stream
    (push (setf filter (make-decoding-filter stream (code-char pre-content))) sources)
    stream))

(defMethod prepend-stream
           ((pre-content #+MCL ccl::input-binary-stream #+ALLEGRO excl::fundamental-binary-input-stream)
            (stream decoding-stream)
            &key
            (encoding :autodetect))
  (prepend-stream (make-instance 'coding-filter :stream pre-content
                                 :encoding encoding)
                  stream))


(defMethod prepend-stream
           ((coding-filter coding-filter) (stream decoding-stream) &key
            &aux encoding to-reread)
  (declare (ignorable encoding))
  (with-slots (filter sources) stream
    (unless (coding-filter.encoding coding-filter)
      (multiple-value-setq (encoding to-reread)
        (setf (coding-filter.encoding coding-filter) :autodetect)))
    (push coding-filter sources)
    (if to-reread
      (prepend-stream to-reread stream)
      (setf filter (make-decoding-filter stream coding-filter))))
  stream)


(defMethod stream-close :before
           ((stream encoded-stream))
  (map nil #'(lambda (source)
               (typecase source
                 (stream (stream-close source))
                 (t)))
       (encoded-stream.sources stream))
  (reinitialize-instance stream))

(defMethod shared-initialize :after
           ((instance encoded-stream) (slots t) &key stream (encoding :autodetect))
  (with-slots (filter sources) instance
    (setf filter nil
          sources nil)
    (when stream
      (prepend-stream stream instance :encoding encoding))))


(defMethod shared-initialize :after
           ((self coding-filter) (slots t) &key encoding)
  (when encoding
    (setf (coding-filter.encoding self) encoding)))

(defMethod coding-filter.function
           ((filter coding-filter))
  (with-slots (function encoding stream) filter
    (cond ((typep function 'function) function)
          (encoding
           (setf function (coding-function encoding stream)))
          (t
           (xml-form-error 'encoding
                           "no encoding specified for stream: ~s." (coding-filter.stream filter))))))

(defMethod encoded-stream.encoding
           ((stream encoded-stream))
  (with-slots (sources) stream
    (some #'coding-filter.encoding sources)))

(defMethod coding-filter.encoding
           ((source t))
  nil)

#|
(defMethod (setf encoded-stream.encoding)
           ((encoding symbol) (stream t))
  (setf (encoded-stream.encoding stream) (string encoding)))

(defMethod (setf encoded-stream.encoding)
           ((encoding string) (stream encoded-stream) &aux source)
  (with-slots (sources filter) stream
    (when (and (setf source (first sources))
               (setf encoding
                     (setf (encoded-stream.encoding source) encoding)))
      (setf filter (make-decoding-filter stream source))
      encoding)))

(defMethod (setf encoded-stream.encoding)
           ((encoding string) (source function))
  (unless (or (string-equal "UTF-8" encoding)
              (string-equal "AUTODETECT" encoding))
    (xml-form-error 'encoding
                    "encoding not supported for internal streams: ~s."
                    encoding))
  encoding)
|#

(defMethod (setf coding-filter.encoding)
           ((encoding (eql :autodetect)) (filter coding-filter))
  (with-slots (stream) filter
    (multiple-value-bind (encoding to-reread)
                         (autodetect-stream-encoding stream)
      (unless encoding
        (xml-warn 'decoding "assuming UTF-8 encoding for stream: ~s." stream)
        (setf encoding "UTF-8"))
      (setf (coding-filter.encoding filter) encoding)
      (values encoding to-reread))))

#|
 the stream encodings are accomplished with filter functions which map
 characters to and from the respective encodings.
 <UL>
 <LI>UTF-8 is direct
 <LI>UTF-16LE is UTF16 in "little-endian" byte order. (order mark is 0xFF 0xFE)
 <LI>UTF-16BE is UTF16 in "big-endian" byte order. (order mark is 0xFE 0xFF)
 </UL>
 |#

(defMethod coding-function
           ((encoding string) (stream #+CCL input-stream #+ALLEGRO fundamental-input-stream))
  (make-decoding-function (intern (string-upcase encoding) :keyword) stream))

(defMethod coding-function
           ((encoding string) (stream #+CCL output-stream #+ALLEGRO fundamental-output-stream))
  (make-encoding-function (intern (string-upcase encoding) :keyword) stream))

(defMethod coding-function
           ((encoding string) (stream t))
  (warn "no function implemented for encoding on stream: ~s: ~s."
        encoding stream)
  nil)

#+ALLEGRO
(defMacro lsh (x y) `(ash ,x ,y))

(defParameter *default-utf-16-encoding* "UTF-16-12")

(defMethod make-decoding-function
           ((encoding (eql :UTF-8))
            (stream #+MCL ccl::input-binary-stream #+ALLEGRO excl::fundamental-binary-input-stream))
  (multiple-value-bind (function arg)
                       (stream-reader stream)
    #'(lambda (&aux byte1)
        (block read-utf-8-datum
          (flet ((read-byte-code ()
                   (or (funcall function arg)
                       (return-from read-utf-8-datum nil))))
            (setf byte1 (read-byte-code))
            (cond ((zerop (logand #x80 byte1))
                   (code-char byte1))
                  ((= #xc0 (logand #xe0 byte1))
                   (code-char (logior (ash (logand byte1 #x1f) 6)
                                      (read-byte-code))))
                  ((= #xe0 (logand #xf0 byte1))
                   (code-char (logior (logior (ash (logand byte1 #x0f) 12)
                                              (ash (read-byte-code) 6))
                                      (read-byte-code))))
                  ((= #xf0 (logand #xf8 byte1))
                   (let ((byte2 (read-byte-code))
                         (byte3 (read-byte-code))
                         (byte4 (read-byte-code)))
                     (xml-form-error 'decoding
                                     "unsupported unicode datum: ~s."
                                     (list byte1 byte2 byte3 byte4))))
                  (t
                   (xml-form-error 'decoding
                                   "illegal UTF-8 data: ~~2,'0x." byte1))))))))


(defMethod make-encoding-function
           ((encoding (eql :UTF-8))
            (stream #+MCL ccl::output-binary-stream #+ALLEGRO excl::fundamental-binary-output-stream))
  (multiple-value-bind (function arg)
                       (stream-writer stream)
    #'(lambda (char &aux (code (char-code char)))
        (cond ((<= code 255)
               (funcall function arg code))
              ((<= code #x03ff)
               (funcall function arg (logior #b11000000 (ash code -6)))
               (funcall function arg (logior #b10000000 (logand code #b00111111))))
              ((<= code #xffff)
               (funcall function arg (logior #b11100000 (ash code -12)))
               (funcall function arg (logior #b10000000 (logand (ash code -6) #b00111111)))
               (funcall function arg (logior #b10000000 (logand code #b00111111))))
              (t
               (xml-form-error 'encoding
                               "unsupported unicode datum: ~s." code))))))

(defMethod make-decoding-function
           ((encoding (eql :ISO-8859-1))
            (stream #+MCL ccl::input-binary-stream #+ALLEGRO excl::fundamental-binary-input-stream))
  (multiple-value-bind (function arg)
                       (stream-reader stream)
    #'(lambda (&aux byte1)
        (block read-ISO-8859-1-datum
          (when (setf byte1 (funcall function arg))
            (code-char byte1))))))

(defMethod make-decoding-function
           ((encoding (eql :ISO-8859-1)) (stream string-stream))
  (multiple-value-bind (function arg)
                       (stream-reader stream)
    #'(lambda ()
        (funcall function arg))))

(defMethod make-decoding-function
           ((encoding (eql :UTF-16)) (stream stream))
  (make-decoding-function *default-utf-16-encoding* stream))
(defMethod make-encoding-function
           ((encoding (eql :UTF-16)) (stream stream))
  (make-encoding-function *default-utf-16-encoding* stream))

(defMethod make-decoding-function
           ((encoding (eql :UTF-16-12))
            (stream #+MCL ccl::input-binary-stream #+ALLEGRO excl::fundamental-binary-input-stream))
  (multiple-value-bind (function arg)
                       (stream-reader stream)
    #'(lambda ()
        (code-char (+ (lsh (funcall function arg) 8)
                      (funcall function arg))))))

(defMethod make-encoding-function
           ((encoding (eql :UTF-16-12)) (stream #+CCL output-stream #+ALLEGRO fundamental-output-stream))
  (multiple-value-bind (function arg)
                       (stream-writer stream)
    #'(lambda (datum)
        (setf datum (char-code datum))
        (funcall function arg (logand #xff (lsh datum -8)))
        (funcall function arg (logand #xff datum)))))

(defMethod make-decoding-function
           ((encoding (eql :UTF-16-21)) (stream #+CCL input-stream #+ALLEGRO fundamental-input-stream))
  (multiple-value-bind (function arg)
                       (stream-reader stream)
    #'(lambda ()
        (code-char (+ (funcall function arg)
                      (lsh (funcall function arg) 8))))))

(defMethod make-encoding-function
           ((encoding (eql :UTF-16-21)) (stream #+CCL output-stream #+ALLEGRO fundamental-output-stream))
  (multiple-value-bind (function arg)
                       (stream-writer stream)
    #'(lambda (datum)
        (setf datum (char-code datum))
        (funcall function arg (logand #xff datum))
        (funcall function arg (logand #xff (lsh datum -8))))))

(defMethod make-encoding-function
           ((encoding t) (stream t))
  (warn "no encoding defined for operation on stream: ~s: ~s."
        encoding stream)
  nil)

(defMethod make-decoding-function
           ((encoding t) (stream t))
  (warn "no decoding defined for operation on stream: ~s: ~s."
        encoding stream)
  nil)

;;; see also "dtd-entity.lisp" for the method which prepends an entity to
;;; the stream


(defMethod autodetect-stream-encoding
       ((stream #+MCL ccl::input-binary-stream #+ALLEGRO excl::fundamental-binary-input-stream)
        &aux
        byte0 byte1)
  "see PR-xml Appendix F"
  (case (setf byte0 (read-byte stream))
    (#x00 (case (read-byte stream)
            (#x00 (case (read-byte stream)
                    (#x3c (if (= (read-byte stream) #x00)
                            (values "UTF-4-2143" (make-string-input-stream "<"))
                            (error "markup stream corrupt: ~s." stream)))
                    (#x00 (if (= (read-byte stream) #x3c)
                            (values "UCS-4-1234" (make-string-input-stream "<"))
                            (error "markup stream corrupt: ~s." stream)))
                    (t (error "markup stream corrupt: ~s." stream))))
            (#x3c (if (= (read-byte stream) #x00)
                    (case (read-byte stream)
                      (#x00 (values "UCS-4-3412" (make-string-input-stream "<")))
                      (#x3f (warn "assuming UTF-16-12 encoding for stream: ~s." stream)
                       (values "UTF-16-21" (make-string-input-stream "<?")))
                      (t (error "markup stream corrupt: ~s." stream)))
                    (error "markup stream corrupt: ~s." stream)))
            (t
             (error "markup stream corrupt: ~s." stream))))
    (#x3c (case (setf byte1 (read-byte stream))
            (#x00 (case (read-byte stream)
                    (#x3f (case (read-byte stream)
                            (#x00 (warn "assuming UTF-16-12 encoding for stream: ~s." stream)
                             (values "UTF-16-12" (make-string-input-stream "<?")))
                            (t
                             (error "markup stream corrupt: ~s." stream))))
                    (#x00 (if (= (read-byte stream) #x00)
                            (values "UCS-4-4321" (make-string-input-stream "<"))
                            (error "markup stream corrupt: ~s." stream)))
                    (t
                     (error "markup stream corrupt: ~s." stream))))
            (#x3f (values "UTF-8" (make-string-input-stream "<?")))
            (t (warn "assuming UTF-8 encoding for stream: ~s." stream)
               (values "UTF-8" (make-string-input-stream (coerce (vector (code-char byte0) (code-char byte1))
                                                                 'string))))))
    (#xff
     (if (= (read-byte stream) #xfe)
       (values "UTF-16-21" nil)
       (error "markup stream corrupt: ~s." stream)))
    (#xfe
     (if (= (read-byte stream) #xff)
       (values "UTF-16-12" nil)
       (error "markup stream corrupt: ~s." stream)))
    (t
     (warn "assuming UTF-8 encoding for stream: ~s." stream)
     (values "UTF-8" (make-string-input-stream (coerce (vector (code-char byte0)) 'string))))))

(defMethod autodetect-stream-encoding
           ((stream stream))
  (warn "assuming UTF-8 encoding for stream: ~s." stream)
  "UTF-8")

(defMethod autodetect-stream-encoding
           ((stream string-stream))
  "ISO-8859-1")


#|

(mapcar #'(lambda (vector)
            (autodetect-stream-encoding (make-instance 'vector-input-stream :vector vector)))
        '(#(#x00 #x00 #x00 #x3c)
          #(#x3c #x00 #x00 #x00)
          #(#x00 #x00 #x3c #x00)
          #(#x00 #x3c #x00 #x00)
          #(#xff #xfe #x00 #x3c)
          #(#xfe #xff #x3c #x00)
          #(#x00 #x3c #x00 #x3f)
          #(#x3c #x00 #x3f #x00)
          #(#x3c #x3f #x78 #x60)
          #(#x12 #x12 #x3c #x3f)))
;("UCS-4-1234" "UCS-4-4321" "UTF-4-2143" "UCS-4-3412" "UTF-16-21" "UTF-16-12" "UTF-16-21" "UTF-16-12" "UTF-8" "UTF-8")

(defparameter *s* (encoded-stream "<?xml ?><x>asdfgh</x>"))
(inspect *s*)
(peek-char nil *s*)
(loop (unless (princ (encoded-stream-tyi *s*)) (return)))


(with-open-file (stream (choose-file-dialog) :direction :input
                        :element-type '(unsigned-byte 8))
  (let ((c nil)
        (w (make-instance 'fred-window))
        (cs (make-instance 'decoding-stream :stream stream)))
    (print (encoded-stream.encoding cs))
    (loop (unless (setf c (stream-tyi cs)) (return))
          (write-char c w))
    (fred-update w)))
|#             




"XMLP"
