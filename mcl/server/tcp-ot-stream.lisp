;;;   -*- Mode: LISP; Package: CCL; BASE: 10; Syntax: ANSI-Common-Lisp;;-*-
;;;
;;; (c) Copyright  1995-96, 1999, John C. Mallery
;;;     All Rights Reserved.
;;; 
;;;------------------------------------------------------------------- 
;;;
;;; MODAL STREAM THAT TRANSMITS ASCII OR BINARY AND COUNTS BYTES 
;;;
;;; Open Transport Version (branched off http:mac;server;tcp-stream.lisp.29

(in-package :ccl)

;;;------------------------------------------------------------------- 
;;;
;;; Modification History
;;;
;;;   12/15/99  JCMa Fast, cons-free writing of hex size in %insert-chunk-size. Remove calls to clear-coding-state in 
;;;                     (initialize-instance :after chunk-  transfer-coding-stream-mixin) and 
;;;                     (stream-passive-reconnect :after chunk-transfer-coding-stream-mixin) because we don't need gratuitous consing.
;;;  05/13/99 JCMa Defined %end-of-chunk-transfer-decoding-p, end-of-chunk-transfer-decoding-p. decoding-advance now signals 
;;;                    'end-of-chunk-transfer-decoding on any attempt to advance input buffer after all chunked input has been read. 
;;;                     end-of-chunk-transfer-decoding condition is handled by www-utils::%buffered-stream-read-delimited-line, 
;;;                     http::crlf-stream-copy-into-string, http::binary-stream-copy-into-8-bit-array, io-buffer-read-all-bytes-to-file.
;;;                     %stream-write-string checks arguments to avoid crashing MCL.
;;;  04/23/99 JCMa   In %note-chunk-start, Passed stream keyword argument to signal end-of-chunk-transfer-decoding
;;;  07/25/97 cvince Fixed chunk-transfer-decoding-mode to set the advance & listen functions each time.
;;;  07/25/97 JCMa  Made more methods for stream-copy-until-eof, stream-copy-bytes, stream-copy-byte-range, write-vector actually work.
;;;  07/25/97 cvince In chunk-transfer-decoding-mode, the advance and listen functions 
;;;                  were only getting set the first time.
;;;  07/24/97 JCMa   Added more methods for stream-copy-until-eof
;;;  12/02/96 bill   (method clear-coding-state (chunk-transfer-coding-stream-mixin))
;;;                  called from (method initialize-instance :after (chunk-transfer-coding-stream-mixin))
;;;                  and (method stream-passive-reconnect :after (chunk-transfer-coding-stream-mixin)).
;;;  11/20/96 bill   debug encoding-force-output & decoding-advance
;;;  11/18/96 bill   Update to conform to the new io-buffer mechanism

;; duplicated from http:server;utils.lisp but useful here.
(defmacro  handler-case-if (condition form &body clauses)
   "Sets up condition handlers when condition returns non-null."
   `(flet ((execute-form () ,form))
       (declare (inline execute-form))
       (cond (,condition
                  (handler-case (execute-form) ,@clauses))
                (t (execute-form)))))

(defclass modal-ascii-or-binary-tcp-stream-mixin
  ()
  ((ascii-output-mode :initform t :accessor %tcp-stream-ascii-output-mode)
   (ascii-input-mode :initform t :accessor %tcp-stream-ascii-input-mode)
   (process :initform nil :initarg :process :accessor tcp-stream-process)))

(defstruct coding-conn
  encoder
  decoder
  saved-advance-function
  saved-force-output-function
  saved-listen-function)

(defclass chunk-transfer-coding-stream-mixin ()
  ((coding-conn :initform (make-coding-conn) :accessor coding-conn)))

(defclass chunk-transfer-encoding-output-stream-mixin
  (chunk-transfer-coding-stream-mixin)
  ()
  (:documentation "An output stream mixin that provides chunked transfer encoding output on an HTTP stream."))

(defclass chunk-transfer-decoding-input-stream-mixin
  (chunk-transfer-coding-stream-mixin)
  ()
  (:documentation "An input stream mixin that provides chunked transfer decoding input on an HTTP stream.")) 

(defclass modal-ascii-or-binary-tcp-stream
  (chunk-transfer-decoding-input-stream-mixin
   chunk-transfer-encoding-output-stream-mixin
   modal-ascii-or-binary-tcp-stream-mixin
   binary-tcp-stream)
  ())

(mapc #'(lambda (x) (export x :ccl))
      '(modal-ascii-or-binary-tcp-stream #-:ccl-3 bytes-transmitted #-:ccl-3 timeout
        binary-output-mode binary-input-mode ascii-output-mode ascii-input-mode
        tcp-stream-process)) 

;;;------------------------------------------------------------------- 
;;;
;;; Switching the mode on the stream 
;;;

(defmethod ascii-output-mode ((stream modal-ascii-or-binary-tcp-stream-mixin))
  (unless (%tcp-stream-ascii-output-mode stream)
    (stream-force-output stream)
    (setf (%tcp-stream-ascii-output-mode stream) t)))

(defmethod binary-output-mode ((stream modal-ascii-or-binary-tcp-stream-mixin))
  (when (%tcp-stream-ascii-output-mode stream)
    (stream-force-output stream)
    (setf (%tcp-stream-ascii-output-mode stream) nil)))

(defmethod ascii-input-mode ((stream modal-ascii-or-binary-tcp-stream-mixin))
  (unless (%tcp-stream-ascii-input-mode stream)
    (setf (%tcp-stream-ascii-input-mode stream) t)))

(defmethod binary-input-mode ((stream modal-ascii-or-binary-tcp-stream-mixin))
  (when  (%tcp-stream-ascii-input-mode stream)
    (setf (%tcp-stream-ascii-input-mode stream) nil)))

(defmethod input-mode ((stream modal-ascii-or-binary-tcp-stream-mixin))
  (cond ((%tcp-stream-ascii-input-mode stream) :ascii)
        (t :binary)))

(defmethod set-input-mode ((stream modal-ascii-or-binary-tcp-stream-mixin) mode)
  (ecase mode
    (:ascii (ascii-input-mode stream))
    (:binary (binary-input-mode stream)))) 

;;;------------------------------------------------------------------- 
;;;
;;; HTTP 1.1 CHUNKED TRANFER ENCODING OUTPUT STREAM
;;; 

#+ignore
(defmacro with-chunk-transfer-encoding-traced (&body body)
   `(case http:*debug-client*
       (:chunk-transfer-encoding ,@body)))

(defmacro with-chunk-transfer-encoding-traced (&body body)
   (declare (ignore body))
   `(progn))

#|
(defmethod trace-chunk ((stream chunk-transfer-encoding-output-stream-mixin) start end)
   (trace-chunk (stream-io-buffer stream) start end))

(defmethod trace-chunk ((io-buffer io-buffer) start end)
    ;;(declare (ignore start))
    (let ((encoder (conn-encoder io-buffer))
            (write-buffer (io-buffer-outbuf io-buffer)))
       (check-type write-buffer macptr)
       (loop for idx upfrom (cte-chunk-start encoder)  below end
                for byte = (%get-byte write-buffer idx)
                for char = (code-char byte)
                do (write-char char *trace-output*)))) |#

(defun string-to-8-bit-vector (string &optional (start 0) (end (length string)))
   (declare (optimize (speed 3) (safety 0))
                 (fixnum start end)
                 (type string string))
   (let* ((length (- end start))
             (vector (make-array length :element-type '(unsigned-byte 8))))
      (declare (type vector vector))
      (loop for idx1 upfrom start below end
               for idx2 upfrom 0
               do (setf (aref vector idx2) (char-code (aref string idx1)))
               finally (return (values vector length)))))
   
(defun 8-bit-vector-to-string (vector &optional (start 0) (end (length vector)))
   (declare (type vector vector)
                 (fixnum start end)
                 (optimize (speed 3) (safety 0)))
   (let* ((length (- end start))
             (string (make-array length :element-type ccl::*default-character-type*)))
      (declare (type string string))
      (loop for idx1 upfrom start below end
               for idx2 upfrom 0
               do (setf (aref string idx2) (code-char (aref vector idx1)))
               finally (return (values string length))))) 

(declaim (inline chunk-size-integer chunk-size-string))

(defun chunk-size-integer (hexidecimal &optional (start 0) (end (length hexidecimal)))
  "Decodes the size of a chunked transfer encoding from HEXIDECIMAL to an integer."
  (parse-integer hexidecimal :radix 16. :junk-allowed t :start start :end end)) 

(defun chunk-size-string (size)
   "Encodes the size of a chunk for chunked transfer encoding from SIZE, an integer, to hexidecimal."
   ;(declare (values hexidecimal-size hexidecimal-digits))
   (let ((hex (write-to-string size :base 16. :readably nil)))
      (values hex (length hex)))) 

;; pick the maximum 4-digit hex, FFFF
;; (length (chunk-size-string 65535))
(defconstant +chunk-size-vector-length+ 4)

;; Minimum number of bytes free in a buffer to which a chunk will be written.
(defconstant +mininum-chunk-size+ 100)

(declaim (notinline write-8-bit-crlf write-chunk-size-header write-chunk-end-header))

(defun %write-8-bit-crlf (io-buffer &optional write-ptr)
   (cond (write-ptr
              (let ((ptr (io-buffer-outptr io-buffer)))
                 (setf (%get-byte ptr) #.(char-code #\Return))
                 (%incf-ptr ptr)
                 (setf (%get-byte ptr) #.(char-code #\Linefeed))
                 (%incf-ptr ptr)
                 (decf (the fixnum (io-buffer-outcount io-buffer)) 2)))
            (t (locally (declare (notinline %io-buffer-write-byte))
                  (%io-buffer-write-byte io-buffer #.(char-code #\Return))
                  (%io-buffer-write-byte io-buffer #.(char-code #\Linefeed))))))

(defun %write-chunk-size-header (io-buffer write-ptr)
   (declare (fixnum chunk-size-vector-length))
   (with-chunk-transfer-encoding-traced
       (check-type io-buffer io-buffer)
       (unless (> (io-buffer-outcount io-buffer) +chunk-size-vector-length+)
          (error "not enough room in tcp buffer.")))
   (loop repeat +chunk-size-vector-length+
            do (setf (%get-byte write-ptr) #.(char-code #\space))
            (%incf-ptr write-ptr)
            finally (decf (the fixnum (io-buffer-outcount io-buffer)) +chunk-size-vector-length+)
            (incf (the fixnum (io-buffer-bytes-written io-buffer)) +chunk-size-vector-length+))) 

#|
;; Old version works but conses to write the hex size
(defun %insert-chunk-size (io-buffer position size)
   (declare (fixnum size position))
   (check-type io-buffer io-buffer)
   (check-type size (integer 0))
   (check-type position (integer 0)) 
   (with-chunk-transfer-encoding-traced (setq *io-buffer* io-buffer)) 
   (multiple-value-bind (hex hex-digits)
                                   (chunk-size-string size)
       (declare (dynamic-extent hex)
                     (fixnum hex-digits))
       (unless (<= hex-digits +chunk-size-vector-length+)
          (error "Insufficient space in content-length header. ~D was allowed but ~D is needed."
                     +chunk-size-vector-length+ hex-digits))
       (loop with write-buffer = (io-buffer-outbuf io-buffer)
                for idx1 fixnum downfrom (1- hex-digits)  to 0
                for idx2 fixnum downfrom (+ position idx1)
                for byte = (char-code (aref hex idx1))
                do (%put-byte write-buffer byte idx2))
       size))|#

;; no space padding because the default chunk-size vector is already spaces,
;; and so the padding is already present in the place holder. -- JCMa 12/2/1999.
;; Fast, cons-free writing of hex size -- JCMa 12/15/1999.
(defun %insert-chunk-size (io-buffer position size)
   (declare (fixnum size position))
   (check-type io-buffer io-buffer)
   (check-type size (integer 0))
   (check-type position (integer 0)) 
   (with-chunk-transfer-encoding-traced (setq *io-buffer* io-buffer))
   (flet ((hex-bytes (size)
               (multiple-value-bind (hex-length delta)
                                               (floor (the fixnum (integer-length size)) 4)
                   (declare (fixnum hex-length))
                   (if (eql delta 0) (1- hex-length) hex-length)))
            (hex-digit-char-code (size byte-pos)
               (let* ((charset #.(make-array 16 :element-type 'integer :initial-contents (map 'list #'char-code "0123456789ABCDEF")))
                         (byte-specifier (byte 4 byte-pos))
                         (char-idx (ldb byte-specifier size)))
                  (declare (type (simple-vector 16) charset))
                  (locally (declare (optimize (speed 3) (safety 0))) 
                     (aref charset char-idx)))))
      (declare (inline hex-bytes hex-digit-char-code))
      (let ((hex-bytes (hex-bytes size)))
         (declare (fixnum hex-bytes))
         ;; make sure there is room
         (cond ((< hex-bytes +chunk-size-vector-length+)        ; hex digits is (1+ hex-bytes)
                    (loop with write-buffer = (io-buffer-outbuf io-buffer)
                             for byte-pos fixnum downfrom (* hex-bytes 4) to 0 by 4
                             for idx fixnum upfrom position
                             do (%put-byte write-buffer (hex-digit-char-code size byte-pos) idx)))
                  (t (error "Insufficient space in chunk content-length header for size ~D. ~&~D was allowed but ~D is needed."
                                 size +chunk-size-vector-length+ (1+ hex-bytes))))))
   size)

(defstruct (chunked-transfer-encoder
              (:print-function tcp-print-chunked-transfer-encoder)
              (:conc-name cte-))
   (chunk-output nil :type boolean)              ;whether output chunking is on or off
   (chunk-start 0 :type fixnum)          ; output buffer position at start of chunk
   (chunk-body-start 0 :type fixnum)     ; output buffer position at start of chunk data excluding chunk headers
   (chunk-body-end 0 :type fixnum)         ;output buffer position at end of chunk data
   (io-buffer-end-delta 0 :type fixnum)         ; bytes reserved at buffer end
   (chunks-transmitted 0 :type integer)          ; number of chunks transmitted so far
   (chunk-function nil :type (or null function)))        ; specialized chunk encoder

(defun tcp-print-chunked-transfer-encoder (encoder stream ignore)
   (declare (ignore ignore))
   (print-unreadable-object (encoder stream :identity t :type t)
      (format stream "Chunking ~:[Off~;On~]; Transmitted: ~D ~:[~;;Function: ~:*~S~]" 
                   (cte-chunk-output encoder) (cte-chunks-transmitted encoder) (cte-chunk-function encoder)))) 

(defmacro conn-encoder (io-buffer)
  `(coding-conn-encoder
    (coding-conn (io-buffer-stream ,io-buffer))))

(defmacro conn-decoder (io-buffer)
   `(coding-conn-decoder
      (coding-conn (io-buffer-stream ,io-buffer))))

(defconstant +chunk-encoder-io-buffer-end-delta+ 2)     ; room for CR-LF at end of buffer

(declaim (inline %reserve-buffer-space %release-buffer-space))

(defun %reserve-buffer-space (io-buffer encoder)
   (without-interrupts
     (decf (the fixnum (io-buffer-outcount io-buffer)) +chunk-encoder-io-buffer-end-delta+)
     (incf (the fixnum (cte-io-buffer-end-delta encoder)) +chunk-encoder-io-buffer-end-delta+)))
   
(defun %release-buffer-space (io-buffer encoder)
   (without-interrupts
     (incf (the fixnum (io-buffer-outcount io-buffer)) (cte-io-buffer-end-delta encoder))     ; undo the dirty deed done by %note-chunk-body-start
     (setf (cte-io-buffer-end-delta encoder) 0)))

(defun %note-chunk-body-start (io-buffer &optional (encoder (conn-encoder io-buffer))
                                                                        &aux (write-ptr (io-buffer-outptr io-buffer)))
   (with-chunk-transfer-encoding-traced 
       (check-type io-buffer io-buffer)
       (check-type encoder chunked-transfer-encoder)
       (unless (> (io-buffer-outcount io-buffer) +mininum-chunk-size+)
          (error "not the expected amount of room in TCP buffer")))
   (setf  (cte-chunk-start encoder) (io-buffer-outpos io-buffer))
   (%write-chunk-size-header io-buffer write-ptr)
   (%write-8-bit-crlf io-buffer write-ptr)
   (%reserve-buffer-space io-buffer encoder)      ; room for CR-LF from %note-body-end
   (setf (cte-chunk-body-start encoder) (io-buffer-outpos io-buffer))
   (with-chunk-transfer-encoding-traced
       (format *trace-output* "~& %NOTE-CHUNK-BODY-START: Buffer-Pos: ~D Chunk-Start: ~D Body-Start: ~D~&" 
                    (io-buffer-outpos io-buffer) (cte-chunk-start encoder)(cte-chunk-body-start encoder)))) 

(defun %note-body-end (io-buffer encoder)
   (with-chunk-transfer-encoding-traced
       (check-type io-buffer io-buffer)
       (check-type encoder chunked-transfer-encoder))
   (let ((buffer-pos (io-buffer-outpos io-buffer)))
      (declare (fixnum buffer-pos))
      (check-type buffer-pos integer)
      (setf (cte-chunk-body-end encoder) buffer-pos)    ; record body  end
      (with-chunk-transfer-encoding-traced
          (format *trace-output* "~& %NOTE-BODY-END: Buffer-Pos: ~D Chunk-Start: ~D Body-Start: ~D Body-End: ~D~&"
                       buffer-pos (cte-chunk-start encoder) (cte-chunk-body-start encoder)(cte-chunk-body-end encoder)))
      (%insert-chunk-size io-buffer (cte-chunk-start encoder) (- buffer-pos (cte-chunk-body-start encoder)))
      (%release-buffer-space io-buffer encoder)         ; undo the dirty deed done by %note-chunk-body-start
      (%write-8-bit-crlf io-buffer (io-buffer-outptr io-buffer))
      (let ((fctn (cte-chunk-function encoder)))
         (when fctn
             (funcall fctn (io-buffer-stream io-buffer) (cte-chunk-body-start encoder) buffer-pos)))
      (incf (cte-chunks-transmitted encoder))))

(defmethod chunk-transfer-encoding-mode ((s chunk-transfer-encoding-output-stream-mixin)
                                                                            &optional function &aux (io-buffer (stream-io-buffer s)))
   (check-type function (or null function))
   (flet ((get-encoder (s io-buffer) 
               (without-interrupts
                 (or (conn-encoder io-buffer)
                       (let ((encoder (make-chunked-transfer-encoder))
                               (coding-conn (coding-conn s)))
                          (setf (conn-encoder io-buffer) encoder
                                   (coding-conn-encoder coding-conn) encoder)
                          encoder)))))
      (declare (inline get-encoder))
      (unless (> (io-buffer-outcount io-buffer) +mininum-chunk-size+)         ; don't waste packets, try to fill them before sending. -- JCMa 12/9/1999.
         (flush-io-buffer io-buffer nil))
      (let  ((encoder (get-encoder s io-buffer))
               (coding-conn (coding-conn s)))
         (setf (cte-chunks-transmitted encoder) 0
                  (coding-conn-saved-force-output-function coding-conn) (io-buffer-force-output-function io-buffer)
                  (io-buffer-force-output-function io-buffer) #'encoding-force-output
                  (cte-chunk-function encoder) function
                  (cte-chunk-output encoder) t)
         (with-chunk-transfer-encoding-traced
             (format *trace-output* "~&CHUNK-TRANSFER-ENCODING-MODE: Buffer-Pos: ~D Chunking:  ~:[Off~;On~] Connection: ~S~&" 
                          (io-buffer-outpos io-buffer)  (cte-chunk-output encoder)  (opentransport-stream-connection-state s))))
      s))

(defmethod note-first-chunk ((s chunk-transfer-encoding-output-stream-mixin))
   (%note-chunk-body-start (stream-io-buffer s)))

;; Needs to handle the case some error has stopped the transmission and appropriate stream clean up is done. -- JCMa 12/9/1999.
(defmethod note-last-chunk ((s chunk-transfer-encoding-output-stream-mixin) &optional footers-plist
                                                 &aux (io-buffer (stream-io-buffer s)) (encoder (conn-encoder io-buffer)))
   
   ;; Don't get errors on streams which are in inconsistent states, e.g. just got a TCP error
   (let ((zero-length-chunk-p (eql (cte-chunk-body-start encoder) (io-buffer-outpos io-buffer))))
      (with-chunk-transfer-encoding-traced
          (unless (cte-chunk-output encoder)
             (error "Attempt to note last chunk on stream which is not in chunking mode."))
          (format *trace-output* "~&NOTE-LAST-CHUNK:  Buffer-Pos: ~D Chunking: ~:[Off~;On~] Body-Start: ~D Connection: ~S~&" 
                       (io-buffer-outpos io-buffer) (cte-chunk-output encoder) (cte-chunk-body-start encoder) (opentransport-stream-connection-state s)))
      (%note-body-end io-buffer encoder)
      ;; Prevent standard output buffer send and setup from occurring again.
      (setf (io-buffer-force-output-function io-buffer) (coding-conn-saved-force-output-function (coding-conn s))
               (cte-chunk-output encoder) nil)
      ;; below here is buffer overflow safe -- JCMa 12/9/1999.      
      (case (ot-conn-get-endpoint-state io-buffer)
         (:dataxfer                     ; :ESTABLISHED
           (unless zero-length-chunk-p
              (%io-buffer-write-byte io-buffer #.(char-code #\0))      ; insert zero length chunk
              (%write-8-bit-crlf io-buffer nil))
           ;; write the footers
           (http::write-headers s footers-plist t))))
   s) 

;; Enter here when a buffer is (all but two bytes) full.
(defun encoding-force-output (stream io-buffer count finish-p)
   (let* ((coding-conn (coding-conn stream))
             (encoder (coding-conn-encoder coding-conn)))
      (with-chunk-transfer-encoding-traced
          (format *trace-output* "~& ENCODING-FORCE-OUTPUT: Chunking: ~S Position: ~D Count: ~D~&" 
                       (cte-chunk-output encoder) (io-buffer-outpos io-buffer) count))
      (cond ((cte-chunk-output encoder)
                 (%note-body-end io-buffer encoder)
                 (funcall (coding-conn-saved-force-output-function coding-conn) stream io-buffer (io-buffer-outpos io-buffer) finish-p)
                 (%note-chunk-body-start io-buffer encoder))
               (t (funcall (coding-conn-saved-force-output-function coding-conn) stream io-buffer count finish-p))))) 

;;;------------------------------------------------------------------- 
;;;
;;; HTTP 1.1 CHUNKED TRANSFER DECODING
;;;

(defstruct (chunked-transfer-decoder
              (:print-function tcp-print-chunked-transfer-decoder)
              (:conc-name ctd-))
   (chunked-input nil :type boolean)             ;whether output input-chunking is on or off
   (inside-input-chunk-p nil :type boolean)      ; whether we're processing a chunk
   (old-mode nil :type symbol)
   (input-content-length 0 :type integer)
   (input-chunk-size 0 :type integer)   
   (input-scan-start 0 :type integer)
   (input-scan-end 0 :type integer)
   (input-scan-length 0 :type integer)
   (input-chunk-content-length 0 :type integer)
   (input-chunk-crosses-buffer-p nil :type boolean)
   (input-buffer-limit 0 :type integer)
   (input-chunks-received 0 :type integer)
   (input-chunk-size-vector nil :type (or null vector))
   (input-chunk-args-vector nil :type (or null vector)))

(defun tcp-print-chunked-transfer-decoder (decoder stream ignore)
  (declare (ignore ignore))
  (print-unreadable-object (decoder stream :identity t :type t)
    (format stream "Chunking ~:[Off~;On~]; Received: ~D"
            (ctd-chunked-input decoder) (ctd-input-chunks-received decoder))))

(defmethod clear-coding-state ((stream chunk-transfer-coding-stream-mixin))
   (let ((coding-conn (coding-conn stream)))
      (when coding-conn
          (let ((encoder (coding-conn-encoder coding-conn))
                  (decoder (coding-conn-decoder coding-conn)))
             (when encoder
                 (setf (cte-chunk-output encoder) nil))
             (when decoder
                 (setf (ctd-chunked-input decoder) nil))))))

#|
;; We don't want extra consing every time we need another chunking-capable stream. -- JCMa 12/15/1999. 
(defmethod initialize-instance :after ((stream chunk-transfer-coding-stream-mixin) &key)
  (clear-coding-state stream))

(defmethod stream-passive-reconnect :after ((stream chunk-transfer-coding-stream-mixin))
  (clear-coding-state stream))
|#

;; tracing can be done by setting http:*debug-client* to :chunk-transfer-decoding
#+ignore
(defmacro with-chunk-transfer-decoding-traced (&body body)
   `(case http:*debug-client*
       (:chunk-transfer-decoding ,@body))) 

(defmacro with-chunk-transfer-decoding-traced (&body body)
   (declare (ignore body))
   `(progn))

(defun read-cr (conn)
   (with-chunk-transfer-decoding-traced
       (format *error-output* "Read CR: ~D~&" (io-buffer-incount conn)))
   (let ((byte (io-buffer-read-byte conn t)))
      (or (= byte #.(char-code #\return))
            (error "~@C was found  when ~@C  was expected  in chunked transfer decoding." (code-char byte) #\return))))

(defun read-lf (conn)
   (with-chunk-transfer-decoding-traced
       (format *error-output* "Read LF: ~D~&" (io-buffer-incount conn)))
   (let ((byte (io-buffer-read-byte conn t)))
      (or (= byte #.(char-code #\linefeed))
            (error "~@C was found  when ~@C  was expected  in chunked transfer decoding." (code-char byte) #\linefeed)))) 

#+ignore
(defun test-byte (io-buffer &optional (offset 0))
   (%get-unsigned-byte (io-buffer-inptr io-buffer) offset)) 

(defconstant *input-chunk-size-vector-size* 6
  "Controls the standard size of the hex vector resource used to read http chunk sizes.")

(defun allocate-input-chunk-size-vector (decoder)
   (cond ((ctd-input-chunk-size-vector decoder))
            (t (let ((v (make-array *input-chunk-size-vector-size* :element-type ccl:*default-character-type* :adjustable t :fill-pointer t)))
                  (setf (fill-pointer v) 0
                           (ctd-input-chunk-size-vector decoder) v)
                  v))))

(defun deallocate-input-chunk-size-vector (decoder vector)
   (setf (fill-pointer vector) 0
            (ctd-input-chunk-size-vector decoder) vector)) 

(defparameter *input-chunk-args-vector-size* 130
   "Controls the standard size of the vector resource used to read HTTP chunk  arguments.") 

(defun allocate-input-chunk-args-vector (decoder)
   (cond ((ctd-input-chunk-args-vector decoder))
            (t (let ((v (make-array *input-chunk-args-vector-size* :element-type ccl:*default-character-type* :adjustable t :fill-pointer t)))
                  (setf (fill-pointer v) 0
                           (ctd-input-chunk-args-vector decoder) v)
                  v))))

(defun deallocate-input-chunk-args-vector (decoder vector)
   (setf (fill-pointer vector) 0
            (ctd-input-chunk-args-vector decoder) vector))

(defun grow-chunk-string (string &optional (delta 10) (size (array-total-size string)))
   (declare ; (values new-string new-size)
     (fixnum delta size))
   (let ((n-size (+ size delta)))
      (values (adjust-array string n-size :element-type ccl:*default-character-type* :fill-pointer t)
                  n-size)))

(defun read-chunk-size (conn hex-vector)
   (declare ; (values integer-size byte hex-vector)
     (type string hex-vector)
     (optimize (speed 3) (safety 0)))
   (with-chunk-transfer-decoding-traced
       (format *error-output* "~&Read-Chunk-Size-Start: ~D~&" (io-buffer-incount conn)))
   (let* ((hex-vector-size (array-total-size hex-vector))
             (idx2 (fill-pointer hex-vector)))
      (loop for byte = (io-buffer-read-byte conn t)
               until (member byte '#.(mapcar #'char-code '(#\; #\return #\linefeed)) :test #'=)
               do (unless (< idx2 hex-vector-size)
                       (multiple-value-setq (hex-vector hex-vector-size)
                           (grow-chunk-string hex-vector 10. hex-vector-size)))
               do (unless (= byte #.(char-code #\space))
                       (setf (aref hex-vector idx2) (code-char byte))
                       (incf idx2))
               finally (setf (fill-pointer hex-vector) idx2)
               (with-chunk-transfer-decoding-traced
                   (format *error-output* "~&Read-Chunk-Size-End: ~D (~S)~&" (io-buffer-incount conn) hex-vector))
               (return (values (chunk-size-integer hex-vector) byte hex-vector)))))

(defun parse-chunk-size-arguments (conn vector byte)
   ;(declare (values args byte args-vector))
   (with-chunk-transfer-decoding-traced
       (format *error-output* "~&Parse-Chunk-Size-Args-Start: ~D~&" (io-buffer-incount conn)))
   (labels ((return-values (vector byte)
                   (let ((end (fill-pointer vector)))
                      (values (if (zerop end)
                                     nil
                                     (http::parse-equal-sign-delimited-pairs vector 0 end #\; nil))
                                  byte 
                                  vector)))                   
                (do-collect-args (conn vector &aux byte)
                   (declare (type vector string)
                                 (optimize (speed 3) (safety 0)))
                   (loop with idx = (fill-pointer vector)
                            with vector-size = (array-total-size vector)
                            do (setq byte  (io-buffer-read-byte conn t))
                            until (member byte '#.(mapcar #'char-code '(#\Return #\Linefeed)))
                            do (unless (< idx vector-size)
                                    (multiple-value-setq (vector vector-size)
                                        (grow-chunk-string vector 100. vector-size)))
                            do (setf (aref vector idx) (code-char byte))
                            (incf idx)
                            finally (setf (fill-pointer vector) idx))
                   (ecase byte
                      (#.(char-code #\Return)
                        (read-lf conn)
                        (return-values vector #.(char-code #\Linefeed)))
                      (#.(char-code #\Linefeed)
                        (return-values vector #.(char-code #\Linefeed)))))) 
      (multiple-value-prog1
          (case byte
             (#.(char-code #\Return)
               (read-lf conn)
               (setf (fill-pointer vector) 0)
               (return-values vector #.(char-code #\Linefeed)))
             (#.(char-code #\Linefeed)
               (setf (fill-pointer vector) 0)
               (return-values vector #.(char-code #\Linefeed)))
             ;; skip over spaces until reaching the argument delimiter
             (t (do-collect-args conn vector)))
          (with-chunk-transfer-decoding-traced
              (format *error-output* "~&Parse-Chunk-Size-Args-End: ~D (~S)~&" (io-buffer-incount conn) vector)))))

(defun %read-chunk-size (conn &optional (decoder (conn-decoder conn)))
   ;(declare (values chunk-size args))
   (let* ((stream (io-buffer-stream conn))
             (mode (ctd-old-mode decoder))
             (hex-vector (allocate-input-chunk-size-vector decoder))
             (args-vector (allocate-input-chunk-args-vector decoder)))
      (unwind-protect
         (prog (chunk-size byte chunk-size-args)
            (binary-input-mode stream)
            (multiple-value-setq (chunk-size byte hex-vector)
                (read-chunk-size conn hex-vector))
            (multiple-value-setq (chunk-size-args byte args-vector)
                (parse-chunk-size-arguments  conn args-vector byte))
            (with-chunk-transfer-decoding-traced
                (format *error-output* "~&Chunk-Size: ~D ~20TChunk-Args: ~S~&" chunk-size chunk-size-args))
            ;;return the values
            (return (values chunk-size chunk-size-args)))
         ;; clean up forms
         (deallocate-input-chunk-size-vector decoder hex-vector)
         (deallocate-input-chunk-args-vector decoder args-vector)
         (set-input-mode stream mode))))

(define-condition end-of-chunk-transfer-decoding
                  (end-of-file)
                  ()
  (:documentation "Signalled when a complete HTTP resource has been successfully transferred."))

(defun %note-chunk-start (conn &optional (decoder (conn-decoder conn)))
   (multiple-value-bind (chunk-size chunk-args)
                                   (%read-chunk-size conn decoder)
       (declare (integer chunk-size)
                     (ignore chunk-args))               ; don't know what to do with these yet.-- JCMa 8/28/1996.
       (with-chunk-transfer-decoding-traced
           (format *error-output* "~&Start-Chunk: ~D~&" (io-buffer-incount conn)))
       (cond ((zerop chunk-size)
                  (setf (ctd-inside-input-chunk-p decoder) nil
                           (ctd-input-chunk-size decoder) 0
		           (ctd-input-scan-start decoder) 0
		           (ctd-input-scan-end decoder) 0
		           (ctd-input-scan-length decoder) 0
		           (ctd-input-chunk-content-length decoder) 0
		           (ctd-input-chunk-crosses-buffer-p decoder) nil)
                  #+ignore(break "End Chunked Transmission.")
	          (signal 'end-of-chunk-transfer-decoding :stream (io-buffer-stream conn)))
	        (t (let* ((start (the integer (io-buffer-incount conn)))
                              (end (max (- start chunk-size) 0))
		              (length (- start end)))
	              (setf (ctd-input-chunk-size decoder) chunk-size
                               (ctd-inside-input-chunk-p decoder) t
		               (ctd-input-scan-start decoder) start
		               (ctd-input-scan-end decoder) end
		               (ctd-input-scan-length decoder) length
		               (ctd-input-chunk-content-length decoder) length
		               (ctd-input-chunk-crosses-buffer-p decoder) (> chunk-size length)
		               (ctd-input-buffer-limit decoder) end       ; restore this count after chunk read
		               (io-buffer-incount conn) length)   ; set the end of buffer
	              (incf (ctd-input-content-length decoder) chunk-size)
	              (with-chunk-transfer-decoding-traced
		          (format *error-output* "~&Start-Count: ~D~&" (io-buffer-incount conn)))))))) 

(defun %end-of-chunk-transfer-decoding-p (decoder)
   (and (ctd-chunked-input decoder)          ; we're chunking input
           (eql 0 (ctd-input-chunk-size decoder))       ; chunk size is zero
           (not (eql 0 (ctd-input-content-length decoder)))))   ; some input has already been read

;; Returns non-null when the end of chunked input has been reached, but CHUNK-TRANSFER-DECODING-MODE-END has not be called yet
(defmethod end-of-chunk-transfer-decoding-p ((s chunk-transfer-decoding-input-stream-mixin))
   (let* ((coding-conn (coding-conn s))
             (decoder (coding-conn-decoder coding-conn)))
      (and decoder (%end-of-chunk-transfer-decoding-p decoder))))

(defun %note-chunk-continue (conn &optional (decoder (conn-decoder conn)))
   (let* ((start (the integer (io-buffer-incount conn)))
             (chunk-size (the integer (ctd-input-chunk-size decoder)))
             (chunk-content-length (the integer (ctd-input-chunk-content-length decoder)))
             (end (max (- start (- chunk-size chunk-content-length)) 0))
             (length (- start end)))
      (setf (ctd-input-scan-start decoder) start
               (ctd-input-scan-end  decoder) end
               (ctd-input-scan-length  decoder) length
               (ctd-input-buffer-limit decoder) end  ; restore this count after chunk read
               (io-buffer-incount conn) length)        ; set the end of buffer
      (incf (ctd-input-chunk-content-length decoder) length)
      (setf (ctd-input-chunk-crosses-buffer-p decoder) (> chunk-size (ctd-input-chunk-content-length decoder)))))

(defun %note-chunk-end (conn decoder)
   (with-chunk-transfer-decoding-traced
       (format *error-output* "~&End-Count: ~D~&" (io-buffer-incount conn)))
   (setf (ctd-input-chunk-size decoder) 0        ; back to no chunk size state to control new buffer operations
            (ctd-inside-input-chunk-p decoder) nil
            (io-buffer-incount conn) (ctd-input-scan-end decoder))    ; size of buffer restored
   (set-input-mode (io-buffer-stream conn) :binary)
   (with-chunk-transfer-decoding-traced
       (format *error-output* "~&End-Chunk: ~D~&" (io-buffer-incount conn)))
   (read-cr conn)
   (read-lf conn)
   (set-input-mode (io-buffer-stream conn) (ctd-old-mode decoder)))

(defmethod chunk-transfer-decoding-mode ((s chunk-transfer-decoding-input-stream-mixin))
   (let* ((conn (stream-io-buffer s))
             (decoder (conn-decoder conn)))
      (unless decoder
         (setq decoder
                  (setf (conn-decoder conn) (make-chunked-transfer-decoder)))
         (let ((coding-conn (coding-conn s)))
            (setf (coding-conn-decoder coding-conn) decoder
                     (coding-conn-saved-advance-function coding-conn) (io-buffer-advance-function conn)
                     (coding-conn-saved-listen-function coding-conn) (io-buffer-listen-function conn))))
      ; Always set correct advance and listen function for decoding mode.  -cvince 7/25/97
      (setf (io-buffer-advance-function conn) 'decoding-advance
               (io-buffer-listen-function conn) 'decoding-listen
               (ctd-chunked-input decoder) t
               (ctd-old-mode decoder) (input-mode s)
               (ctd-input-content-length decoder) 0
               (ctd-input-scan-length decoder) 0)
      (%note-chunk-start conn decoder)))

(defmethod chunk-transfer-content-length  ((s chunk-transfer-decoding-input-stream-mixin) &aux conn decoder)
   (cond ((and (setq conn (stream-io-buffer s))
                     (setq decoder (conn-decoder conn))
                     (ctd-chunked-input decoder))
              (ctd-input-content-length decoder))
            (t (error "~S is not in chunked transfer decoding mode." s)))) 

(defmethod chunk-transfer-decoding-mode-end ((s chunk-transfer-decoding-input-stream-mixin))
   (let* ((conn (stream-io-buffer s))
             (decoder (conn-decoder conn)))
      (setf (ctd-chunked-input decoder) nil
               (ctd-input-chunk-size decoder) 0
               (ctd-input-scan-start decoder) 0
               (ctd-input-scan-end decoder) 0
               (ctd-input-scan-length decoder) 0
               (ctd-input-chunk-content-length decoder) 0
               (ctd-input-chunk-crosses-buffer-p decoder) nil)))

;; Enter here when a buffer has just been emptied or new data is required.
(defun decoding-advance (stream conn readp errorp)
   (with-chunk-transfer-decoding-traced
       (format *error-output* "~&Next-Buffer-Start: ~D~&" (io-buffer-incount conn)))
   (let* ((coding-conn (coding-conn stream))
             (decoder (coding-conn-decoder coding-conn))
             (saved-advance-function (coding-conn-saved-advance-function coding-conn)))
      (prog1
         (cond ((not (and (ctd-chunked-input decoder)
                                  (or (ctd-inside-input-chunk-p decoder)   ;we're not playing with buffer lengths
                                        (eql (ctd-input-chunk-size decoder) -1))))
                    (with-chunk-transfer-decoding-traced
                        (format *error-output* "~&Next-Buffer: Standard ~D~&" (io-buffer-incount conn)))
                    (funcall saved-advance-function stream conn readp errorp))
                  ((ctd-input-chunk-crosses-buffer-p decoder)
                    (prog1
                       (funcall saved-advance-function stream conn readp errorp)
                       (unless (eql 0 (io-buffer-incount conn))
                          (%note-chunk-continue conn decoder))))
                  (t (cond ((eql -1 (ctd-input-chunk-size decoder))
                                (funcall saved-advance-function stream conn readp errorp))
                               ;; if not first chunk, continue to resignal end of chunk after the end of chunked input has been reached.-- JCMa 5/13/1999.
                               ((%end-of-chunk-transfer-decoding-p decoder)
                                 (and readp
                                         (signal 'end-of-chunk-transfer-decoding :stream (io-buffer-stream conn))))
                               (t (%note-chunk-end conn decoder)))
                      (cond ((and (eql 0 (io-buffer-incount conn)) (not readp))
                                 (setf (ctd-input-chunk-size decoder) -1)
                                 nil)
                               (t (setf (ctd-input-chunk-size decoder) 0)
                                   (%note-chunk-start conn decoder)
                                   (if (eql 0 (io-buffer-incount conn))
                                      (funcall saved-advance-function stream conn readp errorp)
                                      t)))))
         (with-chunk-transfer-decoding-traced
             (format *error-output* "~&Next-Buffer-End: ~D~&" (io-buffer-incount conn)))))) 

(defun decoding-listen (stream conn)
  (decoding-advance stream conn nil nil)) 

(declaim (inline %chunked-input-mode-p))

(defun %chunked-input-mode-p (stream)
   (let ((coding-conn (coding-conn stream))
           decoder)
      (and coding-conn 
              (setq decoder (coding-conn-decoder coding-conn))
              (ctd-chunked-input decoder))))

(defmethod chunked-input-mode-p ((stream chunk-transfer-decoding-input-stream-mixin))
   (%chunked-input-mode-p stream))

(defmethod chunked-input-mode-p (stream)
   (declare (ignore stream))
   nil)

;;;------------------------------------------------------------------- 
;;;
;;;  SPECIALIZING STREAM METHODS 
;;; 

; symmetrical CR-LF translation causes cl-http to hang. Fix sometime. -- JCMa 3/24/1996.
#+ignore
(defmethod stream-tyi ((s modal-ascii-or-binary-tcp-stream-mixin) &aux (conn (stream-io-buffer s)))
  (flet ((check-lf-char (conn)
           (let ((ch (io-buffer-tyi conn)))
             (case ch
               (#\Linefeed)
               (t (stream-untyi s ch))))))
    (declare (inline check-lf-char))
    (let ((char (io-buffer-tyi conn)))
      (case char
        (#\Return (check-lf-char conn)))
      char))) 

(defmethod stream-tyo ((s modal-ascii-or-binary-tcp-stream-mixin) char)
  (using-stream-io-buffer (io-buffer s :speedy t)
    (%io-buffer-write-byte io-buffer (char-code char))
    (when (eql char #\return)
      (%io-buffer-write-byte io-buffer (char-code #\linefeed)))))

;; OT in MCL 4.1 doesn't support read and write byte. -- JCMa 7/24/1997.
(defmethod stream-write-byte ((stream modal-ascii-or-binary-tcp-stream-mixin) byte)
  (using-stream-io-buffer (io-buffer stream :speedy t)
    (%io-buffer-write-byte io-buffer byte))) 

(defmethod stream-read-byte ((stream modal-ascii-or-binary-tcp-stream-mixin))
   (using-stream-io-buffer (io-buffer stream :speedy t) 
      (%io-buffer-read-byte io-buffer nil))) 

;; handle end-of-chunk-transfer-decoding during chunked decoding.
(defmethod stream-read-byte ((stream chunk-transfer-decoding-input-stream-mixin))
   (using-stream-io-buffer (io-buffer stream :speedy t) 
      (handler-case-if (%chunked-input-mode-p stream)
                                (%io-buffer-read-byte io-buffer nil)
         (end-of-chunk-transfer-decoding () nil)))) 

(declaim (inline %stream-write-string))

; Needed because it performs CRLF translation.
(defun %stream-write-string (s string start end &aux (conn (stream-io-buffer s)) (length (length string)))
   (macrolet ((maybe-send-tcp-buffer (conn outcount)
                        (unless *compile-definitions*
                           (error "This code won't work interpreted"))
                        `(when (eql ,outcount 0)
                             (setf (io-buffer-outcount ,conn) 0)
                             (%io-buffer-force-output ,conn nil)
                             (setq ,outcount (io-buffer-outcount ,conn))))
                    (fixnum-check-type (arg min limit)
                       `(unless (and (fixnump ,arg)
                                           (locally (declare (fixnum ,arg ,min ,limit))
                                              (and (<= ,min ,arg) (< ,arg ,limit))))
                           (setq ,arg (require-type ,arg (list 'integer ,min ,limit))))))
       (fixnum-check-type start 0 length)
       (fixnum-check-type end start length)
       (multiple-value-bind (str offset)
                                       (array-data-and-offset string)
           
           (let ((bytes (- (the fixnum end) (the fixnum start)))
                   (write-ptr (io-buffer-outptr conn))
                   (outcount (io-buffer-outcount conn)))
              (declare (type fixnum bytes outcount start end)
                            (type macptr write-ptr)
                            (optimize (speed 3) (safety 0)))
              (incf start offset)
              (incf end offset)
              (with-io-buffer-locked (conn)
                  (macrolet ((loop-body (base-string-p)
                                      (unless *compile-definitions*
                                         (error "This code will only work if compiled"))
                                      `(loop for idx fixnum upfrom start below end
                                                for char-code = ,(if base-string-p
                                                                             `(locally (declare (type (simple-array (unsigned-byte 8) (*)) str))
                                                                                 (aref str idx))
                                                                             `(char-code (aref str idx)))
                                                do (progn (maybe-send-tcp-buffer conn outcount)
                                                               (setf (%get-byte write-ptr) char-code)
                                                               (%incf-ptr write-ptr)
                                                               (decf outcount)
                                                               (when (eql char-code #.(char-code #\Return))
                                                                   (maybe-send-tcp-buffer conn outcount)
                                                                   (setf (%get-byte write-ptr) #.(char-code #\Linefeed))
                                                                   (%incf-ptr write-ptr)
                                                                   (decf outcount)
                                                                   (incf (io-buffer-bytes-written conn))))
                                                finally (progn (setf (io-buffer-outcount conn) outcount)
                                                                      (incf (io-buffer-bytes-written conn) bytes)))))
                      (if (typep str 'simple-base-string)
                         (loop-body t)
                         (loop-body nil)))))
           string)))

(defmethod stream-write-string ((s modal-ascii-or-binary-tcp-stream-mixin) string start end)
  (%stream-write-string s string start end))

(defmethod http::write-vector ((s modal-ascii-or-binary-tcp-stream-mixin) (string string) &optional start end)
  (%stream-write-string s string start end))

(defmethod http::write-vector ((stream modal-ascii-or-binary-tcp-stream-mixin) (string string) &optional start end)
   (%io-buffer-stream-write-string stream string start end)
   string)

(defmethod http::write-vector ((stream modal-ascii-or-binary-tcp-stream-mixin) (vector vector) &optional (start 0) (end (length vector)))
  (%io-buffer-stream-write-string stream vector start end)
  vector)

(defmethod http::stream-copy-until-eof ((from-stream input-file-stream) (to-stream basic-tcp-stream) &optional copy-mode
                                             &aux (conn (stream-io-buffer to-stream)))
  (declare (ignore copy-mode))
  (io-buffer-write-file conn (stream-filename from-stream))) 

(defun io-buffer-read-all-bytes-to-file (io-buffer pathname)
   (flet ((%io-buffer-read-all-bytes-to-file (io-buffer pathname)
                 (create-file pathname :if-exists :error)
                 (with-fsopen-file (pb pathname t)
                     (with-io-buffer-locked (io-buffer)
                         (loop with chunking-p =  (chunked-input-mode-p (io-buffer-stream io-buffer))
                                  while (if (eql 0 (io-buffer-incount io-buffer))
                                              (handler-case-if chunking-p
                                                                        (%io-buffer-advance io-buffer t t)
                                                 (end-of-chunk-transfer-decoding () nil))        ; input chunking awareness, relevant for http streams
                                              t)
                                  for read-buffer =  (io-buffer-inptr io-buffer)
                                  for buffer-size fixnum = (io-buffer-incount io-buffer)
                                  do (fswrite pb buffer-size read-buffer)
                                  (%incf-ptr read-buffer buffer-size)
                                  (decf (the fixnum (io-buffer-incount io-buffer)) buffer-size)
                                  sum buffer-size into count
                                  finally (incf (io-buffer-bytes-read io-buffer) count))))))
      (let* ((tmp-file (when (probe-file pathname) (gen-file-name pathname)))
                (win-p nil))
         (cond (tmp-file
                    (unwind-protect 
                       (progn (rename-file pathname tmp-file :if-exists :error)
                                  (%io-buffer-read-all-bytes-to-file io-buffer pathname)
                                  (setq win-p t))
                       (if win-p 
                          (delete-file tmp-file)
                          (rename-file tmp-file pathname :if-exists :supersede))))
                  (t (%io-buffer-read-all-bytes-to-file io-buffer pathname))))))

(defmethod http::stream-copy-until-eof ((from-stream basic-tcp-stream) (to-stream input-file-stream) &optional copy-mode
                                             &aux (conn (stream-io-buffer from-stream)))
  (declare (ignore copy-mode))
  (io-buffer-read-all-bytes-to-file conn (stream-filename to-stream)))

;; obsoleted by %io-buffer-write-ptr -- JCMa 4/29/1999.
;(defun %io-buffer-write (io-buffer read-ptr bytes)
;   (declare (type fixnum start end))
;   (macrolet ((maybe-send-tcp-buffer (io-buffer outcount)
;                        (unless *compile-definitions*
;                           (error "This code won't work interpreted"))
;                        `(when (eql ,outcount 0)
;                             (setf (io-buffer-outcount ,io-buffer) 0)
;                             (%io-buffer-force-output ,io-buffer nil)
;                             (setq ,outcount (io-buffer-outcount ,io-buffer)))))
;       (locally (declare (fixnum bytes)
;                                 (optimize (speed 3) (safety 0)))
;          (let ((write-ptr (io-buffer-outptr io-buffer))
;                  (outcount (io-buffer-outcount io-buffer)))
;             (declare (type fixnum bytes outcount start end)
;                           (type macptr write-ptr)
;                           (type macptr read-ptr))
;             (with-io-buffer-locked (io-buffer)
;                 (loop for idx upfrom 0 below bytes
;                          for byte = (%get-byte read-ptr)
;                          do (progn (maybe-send-tcp-buffer io-buffer outcount)
;                                         (setf (%get-byte write-ptr) byte)
;                                         (%incf-ptr read-ptr)
;                                         (%incf-ptr write-ptr)
;                                         (decf outcount))
;                          finally (progn (setf (io-buffer-outcount io-buffer) outcount)
;                                                (incf (the fixnum (io-buffer-bytes-written io-buffer)) bytes)))))))) 

(defun io-buffer-copy-until-eof (from-io-buffer to-io-buffer)
   (with-io-buffer-locked (from-io-buffer)
       (with-io-buffer-locked (to-io-buffer)
           (loop while (if (eql 0 (io-buffer-incount from-io-buffer))
                               (%io-buffer-advance from-io-buffer t t)    ; eof may be signalled through this -- JCMa 5/13/1999.
                               t)
                    for read-ptr =  (io-buffer-inptr from-io-buffer)
                    for buffer-size fixnum = (io-buffer-incount from-io-buffer)
                    do (%io-buffer-write-ptr to-io-buffer read-ptr 0 buffer-size)   ; replaced my %io-buffer-write -- JCMa 4/29/1999.
                    (decf (the fixnum (io-buffer-incount from-io-buffer)) buffer-size)
                    (incf (io-buffer-bytes-read from-io-buffer) buffer-size)))))

(defun io-buffer-copy-bytes (from-io-buffer to-io-buffer n-bytes) 
   (declare (fixnum bytes))
   (with-io-buffer-locked (from-io-buffer)
       (with-io-buffer-locked (to-io-buffer)
           (loop with remaining-bytes = n-bytes 
                    while (if (eql 0 (io-buffer-incount from-io-buffer))
                                (%io-buffer-advance from-io-buffer t t); eof may be signalled through this -- JCMa 5/13/1999.
                                t)
                    for read-ptr =  (io-buffer-inptr from-io-buffer)
                    for buffer-size fixnum  = (io-buffer-incount from-io-buffer)
                    for copy-n-bytes fixnum = (min remaining-bytes buffer-size)
                    do (progn (%io-buffer-write-ptr to-io-buffer read-ptr 0 copy-n-bytes)     ; replaced my %io-buffer-write -- JCMa 4/29/1999.
                                   (decf (the fixnum (io-buffer-incount from-io-buffer)) copy-n-bytes)
                                   (incf (io-buffer-bytes-read from-io-buffer) copy-n-bytes))
                    until (eql 0 (decf remaining-bytes copy-n-bytes))))))

(defmethod http::stream-copy-until-eof ((from-stream modal-ascii-or-binary-tcp-stream) (to-stream modal-ascii-or-binary-tcp-stream) 
                                                                        &optional copy-mode)
   (declare (ignore copy-mode))
   (io-buffer-copy-until-eof (stream-io-buffer from-stream) (stream-io-buffer to-stream)))

(defmethod http::stream-copy-bytes ((from-stream modal-ascii-or-binary-tcp-stream) (to-stream modal-ascii-or-binary-tcp-stream) 
                                                                n-bytes &optional copy-mode)
   (declare (ignore copy-mode))
   (io-buffer-copy-bytes (stream-io-buffer from-stream) (stream-io-buffer to-stream) n-bytes)) 

(defmethod http::stream-copy-bytes ((from-stream input-file-stream) (to-stream modal-ascii-or-binary-tcp-stream) 
                                                                  n-bytes &optional copy-mode)
   (declare (ignore copy-mode))
   (if (< n-bytes 200)                  ; there is overhead involved in using the fast readers called in io-buffer-write-file
      (let ((io-buffer (stream-io-buffer to-stream)))
         (with-io-buffer-locked (io-buffer)
             (multiple-value-bind (reader fblock)
                                             (stream-reader from-stream)
                 (loop for count fixnum upfrom 0
                          while (< count n-bytes)
                          for byte = (funcall reader fblock)
                          do (%io-buffer-write-byte io-buffer byte)))))
      (let ((start (file-position from-stream)))
         (io-buffer-write-file (stream-io-buffer to-stream) (stream-filename from-stream) start (+ start n-bytes)))))

(defmethod http::stream-copy-bytes ((from-stream modal-ascii-or-binary-tcp-stream) (to-stream output-file-stream) 
                                                                  n-bytes &optional copy-mode)
   (declare (ignore copy-mode))
   (if (< n-bytes 200)                  ; there is overhead involved in using the fast readers called in io-buffer-write-file
      (let ((io-buffer (stream-io-buffer from-stream)))
         (with-io-buffer-locked (io-buffer)
             (multiple-value-bind (writer fblock)
                                             (stream-writer from-stream)
                 (loop for count fixnum upfrom 0
                          while (< count n-bytes)
                          for byte = (%io-buffer-read-byte io-buffer t)
                          do (funcall writer fblock byte)))))
      (io-buffer-read-bytes-to-file (stream-io-buffer from-stream) (stream-filename to-stream) n-bytes)))

;; can we do this quickly without reopenning the file stream? -- JCMa 4/29/1999.
(defmethod http::stream-copy-byte-range ((from-stream input-file-stream) (to-stream modal-ascii-or-binary-tcp-stream) start end)	
   (io-buffer-write-file (stream-io-buffer to-stream) (stream-filename from-stream) start end))

(defmethod http::advance-input-buffer ((stream  modal-ascii-or-binary-tcp-stream) &optional delta)
   (let ((io-buffer (stream-io-buffer stream)))
      (with-io-buffer-locked (io-buffer)
          (cond (delta
                     (locally (declare (fixnum delta))
                        (loop with remaining-bytes = delta
                                 while (if (eql 0 (io-buffer-incount io-buffer))
                                             (%io-buffer-advance io-buffer t t)        ; not sure what to signal if no new buffer -- JCMa 7/25/1997.
                                             t)
                                 for buffer-size fixnum = (io-buffer-incount io-buffer)
                                 for copy-n-bytes fixnum = (min remaining-bytes buffer-size)
                                 do (progn (decf (io-buffer-incount io-buffer) copy-n-bytes)
                                                (incf (io-buffer-bytes-read io-buffer) copy-n-bytes))
                                 until (eql 0 (decf remaining-bytes copy-n-bytes)))))
                   (t (loop while (if (eql 0 (io-buffer-incount io-buffer))
                                           (%io-buffer-advance io-buffer t t)
                                           t)
                               for buffer-size = (the fixnum (io-buffer-incount io-buffer))
                               do (progn (decf (io-buffer-incount io-buffer) buffer-size)
                                              (incf (io-buffer-bytes-read io-buffer) buffer-size)))))))) 

(defun write-file-to-array (pathname array &optional (array-start 0) (file-start 0) file-end)
   (with-fsopen-file (pb pathname)
       (let* ((filesize (the integer (geteof pb)))
                 bytes)
          ;; check for bad arguments
          (when (or (< file-start 0) (>= file-start filesize))
              (error "FILE-START index of ~s is not between 0 & ~s" file-start filesize))
          (if file-end
             (when (or (< file-end file-start) (> file-end filesize))
                 (error "FILE-END index of ~s is not between ~s and ~s" file-end file-start filesize))
             (setq file-end filesize))
          ;; determine bytes to copy
          (setq bytes (- file-end file-start))
          ;;set the file position
          (setfpos pb file-start)
          (let ((fill-pointer (+ array-start bytes))
                  ptr)
             (cond (array
                        ;; ensure enough space
                        (when (< (array-total-size array) fill-pointer)
                            (setq array (adjust-array array fill-pointer :fill-pointer t :element-type (array-element-type array)))))
                      (t (setq array (make-array fill-pointer :fill-pointer  t :adjustable t :element-type '(unsigned-byte 8)))))
             ;; Get down to business
             (unwind-protect
                (progn
                   (setq ptr (#_newptr bytes))          ; allocate bytes
                   (fsread pb bytes ptr)        ; copy data into the pointer
                   (multiple-value-bind (data offset)
                                                   (array-data-and-offset array)
                       (%copy-ptr-to-ivector ptr 0 data (+ offset array-start) bytes)))
                ;; Deallocate memory
                (when (and ptr (not (%null-ptr-p ptr)))
                    (#_disposeptr ptr)))
             ;; Update fill-pointer and return the possibly new array and fill index
             (setf (fill-pointer array) fill-pointer)
             (values array fill-pointer))))) 

(defmethod http::binary-stream-copy-into-8-bit-array ((stream  input-file-stream) n-bytes &optional (start 0) array)
   (write-file-to-array (stream-filename stream) array start 0 n-bytes))

(defmethod http::binary-stream-copy-from-8-bit-array (from-array (stream  modal-ascii-or-binary-tcp-stream)  &optional (start 0) end)
  (%io-buffer-stream-write-string stream from-array start end)) 

(defmethod http::binary-stream-copy-into-8-bit-array ((stream  modal-ascii-or-binary-tcp-stream) n-bytes &optional (start 0) array &aux size)
   (declare (type array array)
                 (optimize (speed 3) (safety 0)))
   (flet ((make-the-array (size fill-pointer)
                (make-array size :fill-pointer fill-pointer :adjustable t :element-type '(unsigned-byte 8)))
            (adjust-the-array (array size fill-pointer)
               (adjust-array array size :fill-pointer fill-pointer :element-type (array-element-type array)))
            (new-size (size)
               (declare (integer size))
               (cond ((< size 64000) (* 2 size))
                        (t (truncate (* size 1.2))))))
      (declare (inline make-the-array adjust-the-array new-size))
      (let ((io-buffer (stream-io-buffer stream))
              (fill-pointer start))
         (with-io-buffer-locked (io-buffer)
             (cond (n-bytes
                        (setq size (+ n-bytes start))
                        (cond ((null array)
                                   (setq array (make-the-array size start)))
                                 ((< (array-total-size array) size)
                                   (setq array (adjust-the-array array size start))))
                        (%io-buffer-read-bytes-to-vector io-buffer array n-bytes fill-pointer)
                        (setf fill-pointer (+ n-bytes fill-pointer)
                                 (fill-pointer array) fill-pointer))
                      ;; the size and growth issues are open to experimentation and better
                      ;; algorithms that do less work.  7/26/95 -- JCMa.
                      (t (cond ((null array)
                                    (setq size (+ 1000 start)
                                             array (make-the-array size start)))
                                   (t (setq size (array-total-size array))))
                          (handler-case-if  (%chunked-input-mode-p stream) 
                                                     (loop while (if (eql 0 (io-buffer-incount io-buffer))
                                                                         (%io-buffer-advance io-buffer t t)
                                                                         t)
                                                              for buffer =  (io-buffer-inptr io-buffer)
                                                              for read-bytes fixnum = (io-buffer-incount io-buffer)
                                                              for new-size = (+ fill-pointer read-bytes)
                                                              do (when (< size new-size)
                                                                       (setq array (adjust-the-array array (setq size (new-size new-size)) fill-pointer)))
                                                              (multiple-value-bind (data offset)
                                                                                              (array-data-and-offset array)
                                                                  (%copy-ptr-to-ivector buffer 0 data (+ (the fixnum offset) fill-pointer) read-bytes))
                                                              (%incf-ptr (io-buffer-inptr io-buffer) read-bytes)
                                                              (decf (io-buffer-incount io-buffer) read-bytes)
                                                              (incf (io-buffer-bytes-read io-buffer) read-bytes)
                                                              (incf fill-pointer read-bytes))
                             ; ;catch end of chunk transfer decoding
                             (end-of-chunk-transfer-decoding ()))
                          (setf (fill-pointer array) fill-pointer))))
         (values array fill-pointer))))

(defmethod http::crlf-stream-copy-into-string ((stream  modal-ascii-or-binary-tcp-stream) &optional n-bytes (start 0) string &aux size)
   (declare (type string string)
                 (optimize (speed 3) (safety 0)))
   (flet ((make-the-string (size fill-pointer)
                (make-array size :fill-pointer fill-pointer :adjustable t :element-type 'base-character))
            (adjust-the-string (string size fill-pointer)
               (adjust-array string size :fill-pointer fill-pointer :element-type  (array-element-type string)))
            (new-size (size)
               (declare (integer size))
               (cond ((< size 64000) (* 2 size))
                        (t (truncate (* size 1.2))))))
      (declare (inline make-the-string adjust-the-string new-size))
      (let ((io-buffer (stream-io-buffer stream))
              (fill-pointer start))
         (with-io-buffer-locked (io-buffer)
             (cond (n-bytes
                        (setq size (+ n-bytes start))
                        (cond ((null string)
                                   (setq string (make-the-string size start)))
                                 ((< (array-total-size string) size)
                                   (setq string (adjust-the-string string size fill-pointer))))
                        (let ((char (%io-buffer-read-untyi-char io-buffer)))
                           (when char
                               (setf (aref string fill-pointer) char)
                               (incf fill-pointer)
                               (decf n-bytes)))
                        (io-buffer-read-bytes-to-vector io-buffer string n-bytes fill-pointer)
                        (setf fill-pointer (+ n-bytes fill-pointer)
                                 (fill-pointer string) fill-pointer))
                      ;; the size and growth issues are open to experimentation and better algorithms that do less work.  7/26/95 -- JCMa.
                      (t (cond ((null string)
                                    (setq size (+ 1000 start)
                                             string (make-the-string size start)))
                                   (t (setq size (array-total-size string))))
                          (let ((char (%io-buffer-read-untyi-char io-buffer)))
                             (when char
                                 (setf (aref string fill-pointer) char)
                                 (incf fill-pointer)))
                          (handler-case-if (%chunked-input-mode-p stream)
                                                    (loop while (if (eql 0 (io-buffer-incount io-buffer))
                                                                        (%io-buffer-advance io-buffer t t)
                                                                        t)
                                                             for buffer =  (io-buffer-inptr io-buffer)
                                                             for read-bytes fixnum = (io-buffer-incount io-buffer)
                                                             for new-size = (+ fill-pointer read-bytes)
                                                             do (when (< size new-size)
                                                                      (setq string (adjust-the-string string (setq size (new-size new-size)) fill-pointer)))
                                                             (multiple-value-bind (data offset)
                                                                                             (array-data-and-offset string)
                                                                 (%copy-ptr-to-ivector buffer 0 data (+ (the fixnum offset) fill-pointer) read-bytes))
                                                             (%incf-ptr (io-buffer-inptr io-buffer) read-bytes)
                                                             (decf (io-buffer-incount io-buffer) read-bytes)
                                                             (incf (io-buffer-bytes-read io-buffer) read-bytes)
                                                             (incf fill-pointer read-bytes))
                             ; ;catch end of chunk transfer decoding
                             (end-of-chunk-transfer-decoding ()))
                          (setf (fill-pointer string) fill-pointer))))
         (values string fill-pointer))))

(defun www-utils::%buffered-stream-read-delimited-line (stream delimiters eof buffer &aux (index -1) error-p delimiter)
   (declare (type string buffer)
                 (optimize (speed 3) (safety 0)))
   (labels ((maybe-advance-buffer (io-buffer)
                    (if (eql 0 (io-buffer-incount io-buffer)) (%io-buffer-advance io-buffer t t) t))
                (get-char (inptr)
                   (let ((byte (%get-unsigned-byte inptr)))
                      (%incf-ptr inptr)
                      (%code-char byte)))
                (update-counts (io-buffer delta)
                   (declare (fixnum delta))
                   (setf (io-buffer-incount io-buffer) (- (the fixnum (io-buffer-incount io-buffer)) delta)
                            (io-buffer-bytes-read io-buffer) (+ (the fixnum (io-buffer-bytes-read io-buffer)) delta)))
                (clear-delimiter (io-buffer prev-char delimiters)
                   (when (cdr delimiters)
                       (when (eql 0 (io-buffer-incount io-buffer))     ; beware of buffer boundaries
                           (unless (%io-buffer-advance io-buffer t nil)       ; non-null when new data
                              (return-from clear-delimiter)))        ; don't run off the io-buffer boundary
                       (let* ((inptr (io-buffer-inptr io-buffer))
                                 (byte (%get-unsigned-byte inptr))
                                 (char (%code-char byte)))
                          (when (and (member char delimiters :test #'eql) (not (eql prev-char char)))
                              (%incf-ptr inptr)
                              (update-counts io-buffer 1))))))
      (declare (inline get-char get-untyi-char maybe-advance-buffer update-counts clear-delimiter))
      (if (ccl:stream-eofp stream)
         (setq error-p t)
         (using-stream-io-buffer (io-buffer stream :speedy t)
            (let* ((size (array-total-size buffer))
                      (idx (fill-pointer buffer)))
               (declare (fixnum size idx start-idx))
               (handler-case-if  (%chunked-input-mode-p stream)
                                          (loop named buffer-feed
                                                   initially  (let ((char (%io-buffer-read-untyi-char io-buffer)))
                                                                   (when char
                                                                       (setf (aref buffer idx) char)
                                                                       (incf idx)))
                                                   while (maybe-advance-buffer io-buffer)
                                                   do (loop with inptr = (io-buffer-inptr io-buffer)
                                                                with incount fixnum = (io-buffer-incount io-buffer)
                                                                with count fixnum = 0
                                                                until (= incount count)
                                                                for char = (get-char inptr)
                                                                do (incf count)
                                                                until (and (member char delimiters :test #'eql)
                                                                               (setq delimiter char))
                                                                do (unless (< idx size)
                                                                        (setq size (floor (* (the fixnum size) 1.2))
                                                                                 buffer (adjust-array buffer size :element-type '#.*default-character-type*)))
                                                                do (progn (setf (aref buffer idx) char)
                                                                               (incf idx))
                                                                ;;(when *print-p* (format *trace-output* "~:C|" char))
                                                                finally (progn
                                                                              (setq index idx)
                                                                              (update-counts io-buffer count)
                                                                              (when delimiter
                                                                                  (clear-delimiter io-buffer delimiter delimiters)
                                                                                  (return-from buffer-feed))))
                                                   finally (setq error-p t))
                  ; ;catch end of chunk transfer decoding
                  (end-of-chunk-transfer-decoding () (setq error-p t))))))
      (if (eql index -1)           ; no data read
         (values (if error-p eof buffer) error-p delimiter (fill-pointer buffer))
         (values buffer error-p delimiter (setf (fill-pointer buffer) index))))) 

;; methods to be specialized.
#|  stream-decode-crlf-until-eof stream-encode-crlf-until-eof stream-copy-bytes http -> file=stream

|#
