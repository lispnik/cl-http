;;;
;;; CMUCL TCP/IP streams for CL-HTTP.
;;;
;;; This code was written by Douglas T. Crosher, has been placed in
;;; the public domain, and is provides 'as-is'.
;;;
;;; The http-stream code is adapted from the public domain CMUCL
;;; fd-stream code, see code/fd-stream.lisp. Some code and ideas were
;;; obtained from the other cl-http ports.

(in-package :www-utils)

(use-package "ALIEN")
(use-package "C-CALL")

;;; Turn internet address into string format
(defun ip-address-string (address)
  (format nil "~D.~D.~D.~D"
          (ldb (byte 8 24) address)
          (ldb (byte 8 16) address)
          (ldb (byte 8 8)  address)
          (ldb (byte 8 0)  address)))

;;;;
;;; Conditions

(define-condition network-error (error)
  ())

(define-condition domain-error (network-error)
  ())

(define-condition unknown-host-name (network-error)
  ((hostname :reader unknown-host-name-hostname 
	     :initarg :hostname))
  (:report (lambda (condition stream)
             (format stream "Unknown host name ~A"
                     (unknown-host-name-hostname condition)))))

(define-condition unknown-address (network-error)
  ((address :reader unknown-address-address :initarg :address))
  (:report (lambda (condition stream)
             (let ((address (unknown-address-address condition)))
               (format stream "Unknown address ~A"
                       (ip-address-string address))))))

(define-condition domain-resolver-error (network-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "The domain server has returned an error"))))

(define-condition unix-socket-error (network-error)
  ((function :reader unix-socket-error-function :initarg :function)
   (fd :reader unix-socket-error-fd :initarg :fd)
   (errno :reader unix-socket-error-errno :initarg :errno))
  (:report
   (lambda (condition stream)
     (let ((errno (unix-socket-error-errno condition)))
       (format stream "UNIX error '~A' on socket ~D in ~A"
	       (unix:get-unix-error-msg errno)
	       (unix-socket-error-fd condition) errno
	       (unix-socket-error-function condition))))))

(define-condition http-stream-error (network-error)
  ((function :reader http-stream-error-function :initarg :function)
   (errno :reader http-stream-error-errno :initarg :errno)
   (stream :reader http-stream-error-stream :initarg :stream))
  (:report
   (lambda (condition stream)
     (let ((errno (http-stream-error-errno condition)))
       (format stream "UNIX error '~A' in ~A on stream ~S"
	       (unix:get-unix-error-msg errno)
	       (http-stream-error-function condition)
	       (http-stream-error-stream condition))))))

(define-condition http-stream-timeout (network-error)
  ((function :reader http-stream-timeout-function :initarg :function)
   (stream :reader http-stream-timeout-stream :initarg :stream))
  (:report
   (lambda (condition stream)
     (format stream "Timeout in ~A on stream ~S"
	     (http-stream-timeout-function condition)
	     (http-stream-timeout-stream condition)))))

(define-condition remote-network-error (network-error)
  ())

(define-condition host-stopped-responding (remote-network-error)
  ())

(define-condition connection-lost (network-error)
  ())

(defmacro add-default-conditions (&rest types)
  `(progn
     ,@(mapcar
	#'(lambda (sname)
	    `(deftype ,(intern sname) () 'error))
	types)))

(add-default-conditions
 "BAD-CONNECTION-STATE"
 "CONNECTION-CLOSED"
 "CONNECTION-ERROR"
 "CONNECTION-REFUSED"
 "HOST-NOT-RESPONDING"
 "LOCAL-NETWORK-ERROR"
 "NETWORK-PARSE-ERROR"
 "NETWORK-RESOURCES-EXHAUSTED"
 "PROTOCOL-TIMEOUT")


;;;;
;;; ---------------------------------------------------------------------------
;;;; The HTTP-STREAM structure.

(defstruct (http-stream
	    (:print-function %print-http-stream)
	    (:constructor %make-http-stream)
	    (:include sys:lisp-stream	; Note: include stream if lisp-stream
		      			; not yet defined.
		      (misc #'http-stream-misc-routine)))

  (name nil)		      ; The name of this stream
  ;;
  ;;; Number of bytes per element.
  (element-size 1 :type kernel:index)
  (element-type 'base-char)   ; The type of element being transfered.
  (fd -1 :type (integer -1 4096))	; The file descriptor
  ;;
  ;; Controls when the output buffer is flushed.
  (buffering :full :type (member :full :line :none))
  ;;
  ;; Character position if known.
  (char-pos nil :type (or kernel:index null))
  ;;
  ;; T if input is waiting on FD.  :EOF if we hit EOF.
  (listen nil :type (member nil t :eof))
  ;;
  ;; The input buffer.
  (unread nil)
  (ibuf-sap nil :type (or system-area-pointer null))
  (ibuf-length nil :type (or kernel:index null))
  (ibuf-head 0 :type kernel:index)
  (ibuf-tail 0 :type kernel:index)

  ;; The output buffer.
  (obuf-sap nil :type (or sys:system-area-pointer null))
  (obuf-length nil :type (or kernel:index null))
  (obuf-tail 0 :type kernel:index)

  ;;
  ;; Timeout specified for this stream, or NIL if none.
  (timeout nil :type (or kernel:index null))

  ;;
  ;; Extra stream details for cl-http.
  ;;
  (local-port 0 :type fixnum)
  (remote-host 0 :type (unsigned-byte 32))
  (remote-port 0 :type fixnum)
  ;;
  (bytes-transmitted 0 :type kernel:index)
  (bytes-received 0 :type kernel:index)

  ;;
  ;; Chunk transfer encoding.
  ;;
  (chunk-function nil :type (or null function))
  (chunk-start 0 :type fixnum)
  ;; Null when chunking disabled.
  (chunks-transmitted 0 :type (or null fixnum))

  ;;
  ;; Chunk transfer decoding.
  ;;
  ;; Whether output input-chunking is on or off.
  (chunked-input-p nil :type boolean)
  ;; Flagged when a zero length chunk is read so that the headers can
  ;; be read without chunking.
  (chunked-input-done-p nil :type boolean)
  ;; Overall chunk length.
  (input-content-length 0 :type (unsigned-byte 29))
  ;; Remaing character in this chunk, when this is zero it's at a
  ;; chunk start.
  (input-chunk-size 0 :type (unsigned-byte 29))
  (input-chunk-size-vector nil :type (or null vector))
  (input-chunk-args-vector nil :type (or null vector))

  ;;
  ;; A lock to grab.
  (lock (mp:make-lock "Stream lock") :type mp:lock)
  ;;
  ;; The process that will receive network errors on this stream.
  (process nil :type (or null mp::process)))

(defun %print-http-stream (http-stream stream depth)
  (declare (ignore depth))
  (print-unreadable-object (http-stream stream :identity t)
    (format stream "Stream for ~A" (http-stream-name http-stream))))



(declaim (inline bytes-transmitted bytes-received))

(define bytes-transmitted (http-stream)
  "Returns the number of bytes transmitted over STREAM."
  (http-stream-bytes-transmitted http-stream))

(define (setf bytes-transmitted) (bytes http-stream)
  (setf (http-stream-bytes-transmitted http-stream) bytes))

(define bytes-received (http-stream)
  (http-stream-bytes-received http-stream))

(define (setf bytes-received) (bytes http-stream)
  (setf (http-stream-bytes-received http-stream) bytes))

;;;------------------------------------------------------------------- 
;;;
;;; HTTP 1.1 CHUNKED TRANSFER ENCODING
;;;

(eval-when (:compile-toplevel :execute)
(defmacro fixed-crlf-string (count &key prefix)
  (coerce (append prefix
		  (loop repeat count
			append '(#\Return #\Linefeed)))
	  '(simple-array base-char (*))))
)

(defun chunk-transfer-encoding-mode (stream &optional function)
  (declare (optimize (speed 3)))
;  (format t "*chunk-transfer-encoding-mode~%")
  (check-type function (or null function))
  (setf (http-stream-chunk-function stream) function))

(defun note-first-chunk (stream)
  (declare (optimize (speed 3)))
;  (format t "*note-first-chunk~%")
  ;; write anything not part of the chunk
  (force-output stream)
  ;; chunk starts at current position
  (setf (http-stream-chunk-start stream)
	(http-stream-bytes-transmitted stream))
  ;; turn on chunking
  (setf (http-stream-chunks-transmitted stream) 0))

(defun note-last-chunk (stream &optional footers-plist)
  (declare (optimize (speed 3)))
;  (format t "*note-last-chunk~%")
  ;; write the last chunk
  (force-output stream)
  ;; turn off chunking and write the Chunked-Body terminator if any
  ;; chunks were written
  (let ((transmitted (shiftf (http-stream-chunks-transmitted stream) nil)))
    (when (plusp transmitted)
      (write-string (fixed-crlf-string 1 :prefix (#\0))
		    stream)))
  (http::write-headers stream footers-plist t)
  (force-output stream))

(defun compute-chunk-info (stream)
  (declare (optimize (speed 3)))
  (let ((chunk-length (- (http-stream-bytes-transmitted stream)
			 (http-stream-chunk-start stream))))
    (cond ((> chunk-length 0)
	   ;; setup for next chunk
	   (setf (http-stream-chunk-start stream)
		 (http-stream-bytes-transmitted stream))
	   (incf (http-stream-chunks-transmitted stream))
	   ;; Prefix: <HEX excluding "0"> *HEX CRLF
	   (let ((chunk-prefix
		  (format nil "~X~A" chunk-length (fixed-crlf-string 1)))
		 ;; Suffix: CRLF
		 (chunk-suffix (fixed-crlf-string 1)))
	     (values chunk-prefix chunk-suffix)))
	  (t
	   ;; Do nothing for empty chunk
	   (values nil nil)))))


;;;; Output routines and related noise.

;;; DO-OUTPUT -- internal
;;;
;;;   Output the given noise.
;;;
(defun do-output (stream base start end)
  (declare (type http-stream stream)
	   (type (or sys:system-area-pointer (simple-array * (*))) base)
	   (type kernel:index start end)
	   (optimize (speed 3)))
  (multiple-value-bind
	(count errno)
      (alien:with-alien ((write-fds (alien:struct unix:fd-set)))
	(unix:fd-zero write-fds)
	(unix:fd-set (http-stream-fd stream) write-fds)
	(unix:unix-fast-select (1+ (http-stream-fd stream))
			       nil (alien:addr write-fds) nil 0 0))
    ;; Wait if not ready for output or if interrupted.
    (when (or (eql count 0)
	      (and (not count) (eql errno unix:eintr)))
      (unless (mp:process-wait-until-fd-usable
	       (http-stream-fd stream) :output (http-stream-timeout stream))
	(error 'http-stream-timeout :function 'do-output :stream stream))))
  (let ((length (- end start)))
    (multiple-value-bind
	  (count errno)
	(unix:unix-write (http-stream-fd stream) base start length)
      (cond ((not count)
	     (error 'http-stream-error
		    :function 'do-output :errno errno :stream stream))
	    ((not (eql count length))
	     ;; Rough load balancing heuristic: try again later.
	     (mp:process-yield)
	     (do-output stream base (the kernel:index (+ start count)) end))
	    (t
	     ;; Rough load balancing heuristic: yield after writing
	     ;; more than 256 bytes.
	     (when (> length 256)
	       (mp:process-yield)))))))

;;; FLUSH-OUTPUT-BUFFER -- internal
;;;
;;;   Flush any data in the output buffer.
;;;
(defun flush-output-buffer (stream)
  (declare (optimize (speed 3)))
  (let ((length (http-stream-obuf-tail stream)))
    (unless (= length 0)
      (do-output stream (http-stream-obuf-sap stream) 0 length)
      (setf (http-stream-obuf-tail stream) 0))))

(defmacro with-http-stream-lock-held ((http-stream) &body body)
  `(mp:with-lock-held ((http-stream-lock ,http-stream)
		       "Waiting for HTTP stream lock")
    ,@body))

;;; Character output routine.
;;;
(defun output-char-full-buffered (http-stream byte)
  (declare (type http-stream http-stream)
	   (optimize (speed 3)))
  (with-http-stream-lock-held (http-stream)
    (when (< (http-stream-obuf-length http-stream)
	     (+ (http-stream-obuf-tail http-stream) 1))
      (cond ((http-stream-chunks-transmitted http-stream)
	     (multiple-value-bind (prefix suffix)
		 (compute-chunk-info http-stream)
	       (when prefix
		 (do-output http-stream prefix 0 (length prefix)))
	       (flush-output-buffer http-stream)
	       (when suffix
		 (do-output http-stream suffix 0 (length suffix)))))
	    (t
	     (flush-output-buffer http-stream))))
    (if (eq (char-code byte) (char-code #\NEWLINE))
	(setf (http-stream-char-pos http-stream) 0)
	(incf (http-stream-char-pos http-stream)))
    (setf (sys:sap-ref-8 (http-stream-obuf-sap http-stream)
			 (http-stream-obuf-tail http-stream))
	  (char-code byte))
    (incf (http-stream-obuf-tail http-stream) 1)
    (incf (http-stream-bytes-transmitted http-stream))
    (values)))

(defun output-unsigned-byte-full-buffered (http-stream byte)
  (declare (optimize (speed 3)))
  (with-http-stream-lock-held (http-stream)
    (when (< (http-stream-obuf-length http-stream)
	     (+ (http-stream-obuf-tail http-stream) 1))
      (cond ((http-stream-chunks-transmitted http-stream)
	     (multiple-value-bind (prefix suffix)
		 (compute-chunk-info http-stream)
	       (when prefix
		 (do-output http-stream prefix 0 (length prefix)))
	       (flush-output-buffer http-stream)
	       (when suffix
		 (do-output http-stream suffix 0 (length suffix)))))
	    (t
	     (flush-output-buffer http-stream))))
    (setf (sys:sap-ref-8 (http-stream-obuf-sap http-stream)
			 (http-stream-obuf-tail http-stream))
	  byte)
    (incf (http-stream-obuf-tail http-stream) 1)
    (incf (http-stream-bytes-transmitted http-stream))
    (values)))

;;; OUTPUT-RAW-BYTES -- public
;;;
;;;   Does the actual output. If there is space to buffer the string, buffer
;;; it. If the string would normally fit in the buffer, but doesn't because
;;; of other stuff in the buffer, flush the old noise out of the buffer and
;;; put the string in it. Otherwise we have a very long string, so just
;;; send it directly (after flushing the buffer, of course).
;;;
(defun output-raw-bytes (stream thing &optional start end)
  (declare (optimize (speed 3)))
  "Output THING to stream.  THING can be any kind of vector or a sap.  If THING
  is a SAP, END must be supplied (as length won't work)."
  (declare (optimize (speed 3)))
  (let ((start (or start 0))
	(end (or end (length (the (simple-array * (*)) thing)))))
    (declare (type (integer 0 67108863) start end))
    (let* ((len (http-stream-obuf-length stream))
	   (tail (http-stream-obuf-tail stream))
	   (space (- len tail))
	   (bytes (- end start))
	   (newtail (+ tail bytes)))
      (cond ((minusp bytes) ; Error case
	     (cerror "Just go on as if nothing happened..."
		     "~S called with :END before :START!"
		     'output-raw-bytes))
	    ((zerop bytes)) ; Easy case
	    ((<= bytes space)
	     (if (sys:system-area-pointer-p thing)
		 (kernel:system-area-copy thing
				   (* start vm:byte-bits)
				   (http-stream-obuf-sap stream)
				   (* tail vm:byte-bits)
				   (* bytes vm:byte-bits))
		 (kernel:copy-to-system-area thing
				   (+ (* start vm:byte-bits)
				      (* vm:vector-data-offset vm:word-bits))
				   (http-stream-obuf-sap stream)
				   (* tail vm:byte-bits)
				   (* bytes vm:byte-bits)))
	     (setf (http-stream-obuf-tail stream) newtail)
	     (incf (http-stream-bytes-transmitted stream) bytes))
	    ((<= bytes len)
	     (cond ((http-stream-chunks-transmitted stream)
		    (multiple-value-bind (prefix suffix)
		        (compute-chunk-info stream)
		      (when prefix
			(do-output stream prefix 0 (length prefix)))
		      (flush-output-buffer stream)
		      (when suffix
			(do-output stream suffix 0 (length suffix)))))
		   (t
		    (flush-output-buffer stream)))
	     (if (sys:system-area-pointer-p thing)
		 (kernel:system-area-copy thing
		    (* start vm:byte-bits)
		    (http-stream-obuf-sap stream)
		    0
		    (* bytes vm:byte-bits))
	       (kernel:copy-to-system-area thing
	          (+ (* start vm:byte-bits)
		     (* vm:vector-data-offset vm:word-bits))
		  (http-stream-obuf-sap stream)
		  0
		  (* bytes vm:byte-bits)))
	     (setf (http-stream-obuf-tail stream) bytes)
	     (incf (http-stream-bytes-transmitted stream) bytes))
	    (t
	     ;; Send the buffer contents and thing.
	     (incf (http-stream-bytes-transmitted stream) bytes)
	     (cond ((http-stream-chunks-transmitted stream)
		    (multiple-value-bind (prefix suffix)
			(compute-chunk-info stream)
		      (when prefix
			(do-output stream prefix 0 (length prefix)))
		      (flush-output-buffer stream)
		      (do-output stream thing start end)
		      (when suffix
			(do-output stream suffix 0 (length suffix)))))
		   (t
		    (flush-output-buffer stream)
		    (do-output stream thing start end))))))))

;;; HTTP-SOUT -- internal
;;;
;;;   Routine to use to output a string. If the stream is unbuffered, slam
;;; the string down the file descriptor, otherwise use OUTPUT-RAW-BYTES to
;;; buffer the string. Update charpos by checking to see where the last newline
;;; was.
;;;
;;;   Note: some bozos (the FASL dumper) call write-string with things other
;;; than strings. Therefore, we must make sure we have a string before calling
;;; position on it.
;;; 
(defun http-sout (stream thing start end)
  (declare (optimize (speed 3)))
  (let ((start (or start 0))
	(end (or end (length (the vector thing)))))
    (declare (fixnum start end))
    (with-http-stream-lock-held (stream)
      (if (stringp thing)
	  (let ((last-newline
		 (and (find #\newline (the simple-string thing)
			    :start start :end end)
		      (position #\newline
				(the simple-string thing)
				:from-end t
				:start start
				:end end))))
	    (assert (eql (http-stream-buffering stream) :full))
	    (output-raw-bytes stream thing start end)
	    (if last-newline
		(setf (http-stream-char-pos stream) (- end last-newline 1))
		(incf (http-stream-char-pos stream) (- end start))))
	  (output-raw-bytes stream thing start end)))))



;;;; Input routines and related noise.

;;; DO-INPUT -- internal
;;;
;;;   Fills the input buffer, and returns the first character. Throws to
;;; eof-input-catcher if the eof was reached. Drops into system:server if
;;; necessary.
;;;
(defun do-input (stream)
  (declare (type http-stream stream)
	   (optimize (speed 3)))
  (let ((fd (http-stream-fd stream))
	(ibuf-sap (http-stream-ibuf-sap stream))
	(buflen (http-stream-ibuf-length stream))
	(head (http-stream-ibuf-head stream))
	(tail (http-stream-ibuf-tail stream)))
    (declare (type kernel:index head tail))
    (unless (zerop head)
      (cond ((eql head tail)
	     (setf head 0)
	     (setf tail 0)
	     (setf (http-stream-ibuf-head stream) 0)
	     (setf (http-stream-ibuf-tail stream) 0))
	    (t
	     (decf tail head)
	     (kernel:system-area-copy ibuf-sap (* head vm:byte-bits)
				      ibuf-sap 0 (* tail vm:byte-bits))
	     (setf head 0)
	     (setf (http-stream-ibuf-head stream) 0)
	     (setf (http-stream-ibuf-tail stream) tail))))
    (setf (http-stream-listen stream) nil)
    (multiple-value-bind
	  (count errno)
	(alien:with-alien ((read-fds (alien:struct unix:fd-set)))
	  (unix:fd-zero read-fds)
	  (unix:fd-set fd read-fds)
	  (unix:unix-fast-select (1+ fd) (alien:addr read-fds) nil nil 0 0))
      ;; Wait if input is not available or if interrupted.
      (when (or (eql count 0)
		(and (not count) (eql errno unix:eintr)))
	(unless (mp:process-wait-until-fd-usable
		 fd :input (http-stream-timeout stream))
	  (error 'http-stream-timeout :function 'do-input :stream stream))))
    (multiple-value-bind
	  (count errno)
	(unix:unix-read fd
			(system:int-sap (+ (system:sap-int ibuf-sap) tail))
			(- buflen tail))
      (cond ((null count)
	     (cond ((eql errno unix:ewouldblock)
		    (unless (mp:process-wait-until-fd-usable
			     fd :input (http-stream-timeout stream))
		      (error 'http-stream-timeout
			     :function 'do-input :stream stream))
		    (do-input stream))
		   (t
		    (error 'http-stream-error
			   :function 'do-input :errno errno :stream stream))))
	    ((zerop count)
	     (setf (http-stream-listen stream) :eof)
	     (throw 'eof-input-catcher nil))
	    (t
	     (incf (http-stream-ibuf-tail stream) count))))))

;;; INPUT-AT-LEAST -- internal
;;;
;;;   Makes sure there are at least ``bytes'' number of bytes in the input
;;; buffer. Keeps calling do-input until that condition is met.
;;;
(defmacro input-at-least (stream bytes)
  (let ((stream-var (gensym))
	(bytes-var (gensym)))
    `(let ((,stream-var ,stream)
	   (,bytes-var ,bytes))
       (loop
	 (when (>= (- (http-stream-ibuf-tail ,stream-var)
		      (http-stream-ibuf-head ,stream-var))
		   ,bytes-var)
	   (return))
	 (do-input ,stream-var)))))

;;; INPUT-WRAPPER -- intenal
;;;
;;;   Macro to wrap around all input routines to handle eof-error noise.
;;;
(defmacro input-wrapper ((stream bytes eof-error eof-value) &body read-forms)
  (let ((stream-var (gensym))
	(element-var (gensym)))
    `(let ((,stream-var ,stream))
       (if (http-stream-unread ,stream-var)
	   (prog1
	       (http-stream-unread ,stream-var)
	     (setf (http-stream-unread ,stream-var) nil)
	     (setf (http-stream-listen ,stream-var) nil))
	   (let ((,element-var
		  (catch 'eof-input-catcher
		    (input-at-least ,stream-var ,bytes)
		    ,@read-forms)))
	     (cond (,element-var
		    (incf (http-stream-ibuf-head ,stream-var) ,bytes)
		    (incf (http-stream-bytes-received ,stream-var) ,bytes)
		    ,element-var)
		   (t
		    (lisp::eof-or-lose
		     ,stream-var ,eof-error ,eof-value))))))))

;;; INPUT-CHARACTER -- internal
;;;
;;;   Routine to use in stream-in slot for reading string chars.
;;;
(defun input-character (http-stream eof-error eof-value)
  (declare (optimize (speed 3)))
  (input-wrapper (http-stream 1 eof-error eof-value)
     (let ((sap (http-stream-ibuf-sap http-stream))
	   (head (http-stream-ibuf-head http-stream)))
       (code-char (sys:sap-ref-8 sap head)))))

;;; INPUT-UNSIGNED-8BIT-BYTE -- internal
;;;
;;;   Routine to read in an unsigned 8 bit number.
;;;
(defun input-unsigned-8bit-byte (http-stream eof-error eof-value)
  (declare (optimize (speed 3)))
  (input-wrapper (http-stream 1 eof-error eof-value)
     (let ((sap (http-stream-ibuf-sap http-stream))
	   (head (http-stream-ibuf-head http-stream)))
       (sys:sap-ref-8 sap head))))


;;; HTTP-STREAM-READ-N-BYTES -- internal
;;;
;;; The n-bin routine.
;;; 
(defun http-stream-read-n-bytes (stream buffer start requested eof-error-p)
  (declare (type http-stream stream)
	   (type kernel:index start requested)
	   (optimize (speed 3)))
  (let* ((sap (http-stream-ibuf-sap stream))
	 (offset start)
	 (bytes requested)
	 (result
	  (catch 'eof-input-catcher
	    (loop
	      (input-at-least stream 1)
	      (let* ((head (http-stream-ibuf-head stream))
		     (tail (http-stream-ibuf-tail stream))
		     (available (- tail head))
		     (copy (min available bytes)))
		(declare (type (mod 67108864) head tail available))
		(if (typep buffer 'system-area-pointer)
		    (kernel:system-area-copy sap (* head vm:byte-bits)
					     buffer (* offset vm:byte-bits)
					     (* copy vm:byte-bits))
		    (kernel:copy-from-system-area sap (* head vm:byte-bits)
					   buffer (+ (* offset vm:byte-bits)
						     (* vm:vector-data-offset
							vm:word-bits))
					   (* copy vm:byte-bits)))
		(incf (http-stream-ibuf-head stream) copy)
		(incf offset copy)
		(decf bytes copy))
	      (when (zerop bytes)
		(return requested))))))
    (declare (type kernel:index bytes)
	     (type (mod 67108864) offset))
    (or result
	(lisp::eof-or-lose stream eof-error-p (- requested bytes)))))


;;;; Utility functions (misc routines, etc)

;;; HTTP-STREAM-MISC-ROUTINE -- input
;;;
;;;   Handle the various misc operations on http-stream.
;;;
(defun http-stream-misc-routine (stream operation &optional arg1 arg2)
  (declare (ignore arg2)
	   (optimize (speed 3)))
  (case operation
    (:listen 
     (or (not (eql (http-stream-ibuf-head stream)
		   (http-stream-ibuf-tail stream)))
	 (http-stream-listen stream)
	 (setf (http-stream-listen stream)
	       (eql (alien:with-alien ((read-fds (alien:struct unix:fd-set)))
		      (unix:fd-zero read-fds)
		      (unix:fd-set (http-stream-fd stream) read-fds)
		      (unix:unix-fast-select (1+ (http-stream-fd stream))
					     (alien:addr read-fds) nil nil
					     0 0))
		    1))))
    (:unread
     (setf (http-stream-unread stream) arg1)
     (setf (http-stream-listen stream) t))
    (:close
     (unless arg1
       (http-stream-misc-routine stream :finish-output))
     (unix:unix-close (http-stream-fd stream))
     (when (http-stream-obuf-sap stream)
       (mp:atomic-push (http-stream-obuf-sap stream) lisp::*available-buffers*)
       (setf (http-stream-obuf-sap stream) nil))
     (when (http-stream-ibuf-sap stream)
       (mp:atomic-push (http-stream-ibuf-sap stream) lisp::*available-buffers*)
       (setf (http-stream-ibuf-sap stream) nil))
     (lisp::set-closed-flame stream))
    (:clear-input
     (setf (http-stream-unread stream) nil)
     (setf (http-stream-ibuf-head stream) 0)
     (setf (http-stream-ibuf-tail stream) 0)
     (catch 'eof-input-catcher
       (loop
	(multiple-value-bind
	      (count errno)
	    (alien:with-alien ((read-fds (alien:struct unix:fd-set)))
	      (unix:fd-zero read-fds)
	      (unix:fd-set (http-stream-fd stream) read-fds)
	      (unix:unix-fast-select (1+ (http-stream-fd stream))
				     (alien:addr read-fds) nil nil 0 0))
	  (cond ((eql count 1)
		 (do-input stream)
		 (setf (http-stream-ibuf-head stream) 0)
		 (setf (http-stream-ibuf-tail stream) 0))
		((and (not count) (eql errno unix:eintr)))
		(t
		 (return t)))))))
    ((:force-output :finish-output)
     (with-http-stream-lock-held (stream)
       (cond ((http-stream-chunks-transmitted stream)
	      (multiple-value-bind (prefix suffix)
		  (compute-chunk-info stream)
		(when prefix
		  (do-output stream prefix 0 (length prefix)))
		(flush-output-buffer stream)
		(when suffix
		  (do-output stream suffix 0 (length suffix)))))
	     (t
	      (flush-output-buffer stream)))))
    (:element-type
     (http-stream-element-type stream))
    (:interactive-p
     (unix:unix-isatty (http-stream-fd stream)))
    (:line-length
     80)
    (:charpos
     (http-stream-char-pos stream))
    (:file-length
     (multiple-value-bind
	 (okay dev ino mode nlink uid gid rdev size
	       atime mtime ctime blksize blocks)
	 (unix:unix-fstat (http-stream-fd stream))
       (declare (ignore ino nlink uid gid rdev
			atime mtime ctime blksize blocks))
       (unless okay
	 (error 'http-stream-error
		:function 'http-stream-error :errno dev :stream stream))
       (if (zerop (the kernel:index mode))
	   nil
	   (truncate (the kernel:index size)
		     (http-stream-element-size stream)))))
    (:file-position
     0)))



;;;------------------------------------------------------------------- 
;;;
;;; HTTP 1.1 CHUNKED TRANSFER DECODING
;;;

(declaim (inline chunk-size-integer))

(defun chunk-size-integer (hexidecimal &optional (start 0)
			   (end (length hexidecimal)))
  "Decodes the size of a chunked transfer encoding from HEXIDECIMAL to an
  integer."
  (parse-integer hexidecimal :radix 16. :junk-allowed t :start start :end end))

;; Tracing can be done by setting http:*debug-client* to
;; :chunk-transfer-decoding
#+ignore
(defmacro with-chunk-transfer-decoding-traced (&body body)
  `(case http::*debug-client*
     (:chunk-transfer-decoding ,@body)))

(defmacro with-chunk-transfer-decoding-traced (&body body)
  (declare (ignore body))
  `(progn))

(defun read-cr (http-stream)
  (with-chunk-transfer-decoding-traced
      (format *error-output* "Read: CR "))
  (let ((char (input-character http-stream t nil)))
    (or (char= char #\return)
        (error "~@C was found when, not ~@C was expected in chunked transfer decoding."
	       char #\return))))

(defun read-lf (http-stream)
  (with-chunk-transfer-decoding-traced
      (format *error-output* "Read: LF "))
  (let ((char (input-character http-stream t nil)))
    (or (char= char #\linefeed)
        (error "~@C was found when, not ~@C was expected in chunked transfer decoding." char #\linefeed))))

(defconstant *input-chunk-size-vector-size* 6
  "Controls the standard size of the hex vector resource used to
   read http chunk sizes.")

(defun allocate-input-chunk-size-vector (http-stream)
  (cond ((http-stream-input-chunk-size-vector http-stream))
        (t
	 (let ((v (make-array *input-chunk-size-vector-size*
			      :element-type 'base-char
			      :adjustable t :fill-pointer t)))
	   (setf (fill-pointer v) 0
		 (http-stream-input-chunk-size-vector http-stream) v)
	   v))))

(defun deallocate-input-chunk-size-vector (http-stream vector)
  (setf (fill-pointer vector) 0
        (http-stream-input-chunk-size-vector http-stream) vector)) 

(defparameter *input-chunk-args-vector-size* 130
   "Controls the standard size of the vector resource used to read HTTP
   chunk arguments.") 

(defun allocate-input-chunk-args-vector (http-stream)
  (cond ((http-stream-input-chunk-args-vector http-stream))
        (t
	 (let ((v (make-array *input-chunk-args-vector-size*
			      :element-type 'base-char
			      :adjustable t :fill-pointer t)))
	   (setf (fill-pointer v) 0
		 (http-stream-input-chunk-args-vector http-stream) v)
	   v))))

(defun deallocate-input-chunk-args-vector (http-stream vector)
  (setf (fill-pointer vector) 0
        (http-stream-input-chunk-args-vector http-stream) vector))

(defun grow-chunk-string (string &optional (delta 10)
				 (size (array-total-size string)))
  (declare (values string (unsigne-byte 29))
           (type (unsigned-byte 29) delta size))
  (let ((n-size (+ size delta)))
    (values (adjust-array string n-size :element-type 'base-char
			  :fill-pointer t)
            n-size)))

(defun read-chunk-size (http-stream hex-vector)
  (declare (values (unsigned-byte 29) base-char string)
           (type string hex-vector)
           (optimize (speed 3) (safety 1)))
  (with-chunk-transfer-decoding-traced
    (format *error-output* "~&Read-Chunk-Size-Start: ~D~&"
	    (http-stream-bytes-received http-stream)))
  (let* ((hex-vector-size (array-total-size hex-vector))
         (idx2 (fill-pointer hex-vector)))
    (loop for char = (input-character http-stream t nil)
          until (member char '(#\; #\return #\linefeed) :test #'char=)
          do (unless (< idx2 hex-vector-size)
               (multiple-value-setq (hex-vector hex-vector-size)
                 (grow-chunk-string hex-vector 10. hex-vector-size)))
          do (unless (char= char #\space)
               (setf (aref hex-vector idx2) char)
               (incf idx2))
          finally (setf (fill-pointer hex-vector) idx2)
                  (with-chunk-transfer-decoding-traced
                    (format *error-output* "~&Read-Chunk-Size-End: ~D (~S)~&"
			    (http-stream-bytes-received http-stream)
			    hex-vector))
                  (return (values (chunk-size-integer hex-vector) char
				  hex-vector)))))

(defun parse-chunk-size-arguments (http-stream vector char)
  (with-chunk-transfer-decoding-traced
    (format *error-output* "~&Parse-Chunk-Size-Args-Start: ~D~&"
	    (http-stream-bytes-received http-stream)))
  (labels ((return-values (vector char)
             (let ((end (fill-pointer vector)))
               (values (if (zerop end)
                           nil
                           (http::parse-equal-sign-delimited-pairs
			    vector 0 end #\; nil))
                       char 
                       vector)))                   
           (do-collect-args (http-stream vector &aux char)
             (declare (optimize (speed 3) (safety 1)))
             (loop with idx = (fill-pointer vector)
                   with vector-size = (array-total-size vector)
                   do (setq char (input-character http-stream t nil))
                   until (member char '(#\Return #\Linefeed))
                   do (unless (< idx vector-size)
                        (multiple-value-setq (vector vector-size)
                          (grow-chunk-string vector 100. vector-size)))
                   do (setf (aref vector idx) char)
                      (incf idx)
                   finally (setf (fill-pointer vector) idx))
             (ecase char
               (#\Return
                (read-lf http-stream)
                (return-values vector #\Linefeed))
               (#\Linefeed
                (return-values vector #\Linefeed)))))
    (multiple-value-prog1
      (case char
        (#\Return
         (read-lf http-stream)
         (setf (fill-pointer vector) 0)
         (return-values vector #\Linefeed))
        (#\Linefeed
         (setf (fill-pointer vector) 0)
         (return-values vector #\Linefeed))
        (t (do-collect-args http-stream vector)))
      (with-chunk-transfer-decoding-traced
        (format *error-output* "~&Parse-Chunk-Size-Args-End: ~D (~S)~&"
		(http-stream-bytes-received http-stream) vector)))))

;;; Read the chunk size and arguments.
(defun %read-chunk-size (http-stream)
  (let* ((hex-vector (allocate-input-chunk-size-vector http-stream))
	 (args-vector (allocate-input-chunk-args-vector http-stream)))
    (unwind-protect
	(prog (chunk-size char chunk-size-args)
	      (multiple-value-setq (chunk-size char hex-vector)
                (read-chunk-size http-stream hex-vector))
	      (multiple-value-setq (chunk-size-args char args-vector)
                (parse-chunk-size-arguments http-stream args-vector char))
	      (with-chunk-transfer-decoding-traced
                (format *error-output* "~&Chunk-Size: ~D ~20TChunk-Args: ~S~&"
			chunk-size chunk-size-args))
	      ;;return the values
	      (return (values chunk-size chunk-size-args)))
      ;; clean up forms
      (deallocate-input-chunk-size-vector http-stream hex-vector)
      (deallocate-input-chunk-args-vector http-stream args-vector))))

(define-condition end-of-chunk-transfer-decoding (end-of-file)
  ()
  (:documentation "Signalled when a complete HTTP resource has been successfully transferred."))

;;;
(defun input-character-maybe-with-chunk-decoding (http-stream eof-error
						  eof-value)
  (cond ((not (http-stream-chunked-input-p http-stream))
	 (input-character http-stream eof-error eof-value))
	((http-stream-unread http-stream)
	 (prog1
	     (http-stream-unread http-stream)
	   (setf (http-stream-unread http-stream) nil)
	   (setf (http-stream-listen http-stream) nil)))
	((http-stream-chunked-input-done-p http-stream)
	 (if eof-error
	     (signal 'end-of-chunk-transfer-decoding :stream http-stream)
	     (return-from input-character-maybe-with-chunk-decoding
	       eof-value)))
	(t
	 (let ((chunk-size (http-stream-input-chunk-size http-stream)))
	   (when (zerop chunk-size) ; New chunk?
	     (setq chunk-size (%read-chunk-size http-stream))
	     (when (zerop chunk-size)
	       (setf (http-stream-chunked-input-done-p http-stream) t)
	       (if eof-error
		   (signal 'end-of-chunk-transfer-decoding :stream http-stream)
		   (return-from input-character-maybe-with-chunk-decoding
		     eof-value)))
	     (with-chunk-transfer-decoding-traced
		 (format *error-output* "~&Start-Chunk: ~D~&"
			 (http-stream-bytes-received http-stream))))
	   (let ((char (input-character http-stream eof-error eof-value)))
	     (incf (http-stream-input-content-length http-stream))
	     (decf chunk-size)
	     (setf (http-stream-input-chunk-size http-stream) chunk-size)
	     (when (zerop chunk-size)
	       ;; End of the chunk.
	       (with-chunk-transfer-decoding-traced
		   (format *error-output* "~&End-Count: ~D~&"
			   (http-stream-bytes-received http-stream)))
	       (read-cr http-stream)
	       (read-lf http-stream))
	     char)))))

(defun chunk-transfer-decoding-mode (http-stream)
  (declare (type http-stream http-stream))
  (with-chunk-transfer-decoding-traced
      (format *error-output* "Enable chunk transfer decoding on ~s~%"
	      http-stream))
  (setf (http-stream-chunked-input-p http-stream) t)
  (setf (http-stream-input-chunk-size http-stream) 0)
  (setf (http-stream-input-content-length http-stream) 0)
  (setf (http-stream-chunked-input-done-p http-stream) nil))

#+nil
(defun chunk-transfer-content-length (http-stream)
  (declare (type http-stream http-stream))
  (unless (http-stream-chunked-input-p http-stream)
    (error "~S is not in chunked transfer decoding mode." http-stream))
  (http-stream-input-content-length http-stream))

#+nil
(defun chunk-transfer-content-length-header (http-stream)
  (declare (type http-stream http-stream))
  (unless (http-stream-chunked-input-p http-stream)
    (error "~S is not in chunked transfer decoding mode." http-stream))
  (http::allocate-content-length-header 
   (http-stream-input-content-length http-stream)))

(defun chunk-transfer-decoding-mode-end (http-stream)
  (declare (type http-stream http-stream))
  (with-chunk-transfer-decoding-traced
      (format *error-output* "End chunk transfer decoding on ~s~%"
	      http-stream))
  (setf (http-stream-chunked-input-p http-stream) nil)
  (setf (http-stream-chunked-input-done-p http-stream) t)
  (http-stream-input-content-length http-stream))


;;;; Creation routine MAKE-HTTP-STREAM.

;;; MAKE-HTTP-STREAM -- Public.
;;;
;;; Returns a HTTP-STREAM for the given file descriptor.
;;;
(defun make-http-stream (fd
			 &key
			 (buffering :full)
			 timeout
			 input-buffer-p
			 (name
			  (format nil "HTTP connection on descriptor ~D" fd)))
  (declare (type kernel:index fd)
	   (type (or kernel:index null) timeout)
	   (type (member :none :line :full) buffering)
	   (optimize (speed 3)))
  "Create a HTTP stream for the given unix file descriptor.
  Element-type indicates the element type to use (as for open).
  Buffering indicates the kind of buffering to use.
  Timeout (if true) is the number of seconds to wait for input.  If NIL (the
    default), then wait forever.  When we time out, we signal IO-TIMEOUT.
  Name is used to identify the stream when printed."

  (let ((stream (%make-http-stream :fd fd
				   :name name
				   :buffering buffering
				   :timeout timeout
				   :bytes-transmitted 0
				   :bytes-received 0
				   :chunk-function nil
				   :chunk-start 0
				   :chunks-transmitted nil)))
    ;; Input
    (setf (http-stream-ibuf-sap stream)
	  (sys:without-interrupts (lisp::next-available-buffer)))
    (setf (http-stream-ibuf-length stream) lisp::bytes-per-buffer)
    (setf (http-stream-ibuf-tail stream) 0)
    ;; Character
    #+nil (setf (http-stream-in stream) #'input-character)
    (setf (http-stream-in stream) #'input-character-maybe-with-chunk-decoding)
    ;; Binary
    (setf (http-stream-bin stream) #'input-unsigned-8bit-byte)
    (setf (http-stream-n-bin stream) #'http-stream-read-n-bytes)
    (when input-buffer-p
      (setf (http-stream-in-buffer stream)
	    (make-array lisp::in-buffer-length
			:element-type '(unsigned-byte 8))))
    
    ;; Output
    (setf (http-stream-obuf-sap stream)
	  (sys:without-interrupts (lisp::next-available-buffer)))
    (setf (http-stream-obuf-length stream) lisp::bytes-per-buffer)
    (setf (http-stream-obuf-tail stream) 0)
    ;; Character
    (setf (http-stream-out stream) #'output-char-full-buffered)
    ;; Binary
    (setf (http-stream-bout stream) #'output-unsigned-byte-full-buffered)
    ;; String output
    (setf (http-stream-sout stream) #'http-sout)
    (setf (http-stream-char-pos stream) 0)
    
    ;;
    (setf (http-stream-element-size stream) 1)
    
    ;; Element-type can be either character or binary but not both.
    (setf (http-stream-element-type stream)
	  'character
	  #+nil '(unsigned-byte 8))
    
    stream))

;;;; 
;;; ---------------------------------------------------------------------------
;;; Server support.

(defun make-tcp-stream (fd port)
  (declare (type kernel:index fd)
	   (type (unsigned-byte 16) port)
	   (optimize (speed 3)))
  (multiple-value-bind (remote-address remote-port)
      (ext:get-peer-host-and-port fd)
    (let ((stream (make-http-stream fd)))
      (setf (http-stream-local-port stream) port)
      (setf (http-stream-remote-host stream) remote-address)
      (setf (http-stream-remote-port stream) remote-port)
      stream)))

;;;; 
;;; ---------------------------------------------------------------------------
;;; Client support.

(defun open-http-stream-to-host (remote-host remote-port)
  (declare (type (unsigned-byte 32) remote-host)
	   (type (unsigned-byte 16) remote-port)
	   (optimize (speed 3)))
  #+nil
  (format t "*C1 remote-host=~s remote-port=~s~%" remote-host remote-port)
  (let* ((fd (ext:connect-to-inet-socket remote-host remote-port))
	 (stream (make-http-stream fd)))
    #+nil
    (format t "*C2 remote-host=~s remote-port=~s fd=~s~%"
	    remote-host remote-port fd)
    ;; Make it non-blocking.
    #+nil (unix:unix-fcntl fd unix:f-setfl unix:fndelay)
    (multiple-value-bind (local-address local-port)
       (ext:get-socket-host-and-port fd)
      (declare (ignore local-address))
      (setf (http-stream-local-port stream) local-port)
      (setf (http-stream-remote-host stream) remote-host)
      (setf (http-stream-remote-port stream) remote-port)
      (setf (http-stream-process stream) mp:*current-process*)
      stream)))

;;;; 
;;; ---------------------------------------------------------------------------
;;; SMTP support.

(defun smtp::%open-mailer-stream (host port args)
  (declare (ignore args)
	   (optimize (speed 3)))
  (let* ((stream (open-http-stream-to-host (parse-host host) port))
	 (orig-char-in (http-stream-in stream))
	 (orig-char-out (http-stream-out stream))
	 (orig-sout (http-stream-sout stream)))
    ;; CR/LF translation.
    (flet ((char-in (stream eof-error eof-value)
	     (let ((char (funcall orig-char-in stream eof-error eof-value)))
	       (cond ((and (eql char #\Return)
			   (eql (peek-char nil stream) #\Linefeed))
		      (funcall orig-char-in stream nil nil)
		      #\Linefeed)
		     (t char))))
	   (char-out (stream char)
	     (when (eql char #\Newline)
	       (funcall orig-char-out stream #\Return))
	     (funcall orig-char-out stream char))
	   (sout (stream string start end)
	     (declare (type (simple-base-string *) string))
	     (let ((start (or start 0))
		   (end (or end (length string))))
	       (declare (fixnum start end))
	       (loop
		(let ((break (position #\Newline string
				       :start start :end end)))
		  (unless break
		    (return (funcall orig-sout stream string start end)))
		  (funcall orig-sout stream string start break)
		  (funcall orig-char-out stream #\Return)
		  (funcall orig-char-out stream #\Newline)
		  (setq start (1+ break)))))))
      (setf (http-stream-in stream) #'char-in)
      (setf (http-stream-out stream) #'char-out)
      (setf (http-stream-sout stream) #'sout))
    stream))

