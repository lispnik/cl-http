;;;
;;; CMUCL TCP/IP streams for cl-http.
;;;
;;; Code and ideas hacked from the CMUCL source and the other cl-http
;;; ports.
;;;
;;; This code was written by Douglas T. Crosher, has been placed in
;;; the public domain, and is provides 'as-is'.
;;;

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
   (errno :reader unix-socket-error-errno :initarg :errno))
  (:report (lambda (condition stream)
             (let ((errno (unix-socket-error-errno condition)))
               (format stream "UNIX error '~A' (errno ~D) in ~A"
                       (unix:get-unix-error-msg errno) errno
                       (unix-socket-error-function condition))))))

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


;;;;---------------------------------------------------------------------------

;;; Extra stream details.
;;;
(defstruct http-stream
  (local-port 0 :type fixnum)
  (remote-host 0 :type (unsigned-byte 32))
  (remote-port 0 :type fixnum)
  ;;
  (bytes-transmitted 0 :type fixnum)
  (bytes-received 0 :type fixnum)
  ;;
  (chunk-function nil :type (or null function))
  (chunk-start 0 :type fixnum)
  ;; Null when chunking disabled.
  (chunks-transmitted 0 :type (or null fixnum)))

;;; Vector of the extra stream details.
(declaim (type (simple-array (or null http-stream) (*)) *stream-details*))
(defvar *stream-details*
  (make-array unix:fd-setsize :element-type '(or null http-stream)
	      :initial-element nil))

(declaim (inline stream-detail))

(defun stream-detail (stream)
  (aref *stream-details* (system:fd-stream-fd stream)))

(declaim (inline bytes-transmitted bytes-received))

(define bytes-transmitted (stream)
  "Returns the number of bytes transmitted over STREAM."
  (http-stream-bytes-transmitted (stream-detail stream)))

(define (setf bytes-transmitted) (bytes stream)
  (setf (http-stream-bytes-transmitted (stream-detail stream)) bytes))

(define bytes-received (stream)
  (http-stream-bytes-received (stream-detail stream)))

(define (setf bytes-received) (bytes stream)
  (setf (http-stream-bytes-received (stream-detail stream)) bytes))

;;;; 
;;; ---------------------------------------------------------------------------
;;; Chunk transfer handling code

(eval-when (:compile-toplevel :execute)
(defmacro fixed-crlf-string (count &key prefix)
  (coerce (append prefix
		  (loop repeat count
			append '(#\Return #\Linefeed)))
	  '(simple-array base-char (*))))
)

(defun chunk-transfer-encoding-mode (stream &optional function)
;  (format t "*chunk-transfer-encoding-mode~%")
  (check-type function (or null function))
  (let ((info (stream-detail stream)))
    (setf (http-stream-chunk-function info) function)))

(defun note-first-chunk (stream)
;  (format t "*note-first-chunk~%")
  ;; write anything not part of the chunk
  (force-output stream)
  (let ((info (stream-detail stream)))
    ;; chunk starts at current position
    (setf (http-stream-chunk-start info)
	  (http-stream-bytes-transmitted info))
    ;; turn on chunking
    (setf (http-stream-chunks-transmitted info) 0)))

(defun note-last-chunk (stream &optional footers-plist)
;  (format t "*note-last-chunk~%")
  ;; write the last chunk
  (force-output stream)
  ;; turn off chunking and write the Chunked-Body terminator if any chunks were written
  (let* ((info (stream-detail stream))
	 (transmitted (shiftf (http-stream-chunks-transmitted info) nil)))
    (when (plusp transmitted)
      (write-string (fixed-crlf-string 1 :prefix (#\0))
		    stream)))
  (http::write-headers stream footers-plist t)
  (force-output stream))

(defun compute-chunk-info (info)
  (let ((chunk-length (- (http-stream-bytes-transmitted info)
			 (http-stream-chunk-start info))))
    (cond ((> chunk-length 0)
	   ;; setup for next chunk
	   (setf (http-stream-chunk-start info)
		 (http-stream-bytes-transmitted info))
	   (incf (http-stream-chunks-transmitted info))
	   ;; Prefix: <HEX excluding "0"> *HEX CRLF
	   (let ((chunk-prefix
		  (format nil "~X~A" chunk-length (fixed-crlf-string 1)))
		 ;; Suffix: CRLF
		 (chunk-suffix (fixed-crlf-string 1)))
	     (values chunk-prefix chunk-suffix)))
	  (t
	   ;; Do nothing for empty chunk
	   (values nil nil)))))


;;;;
;;; ---------------------------------------------------------------------------

(defun output-raw-bytes (stream thing &optional start end)
  "Output THING to stream.  THING can be any kind of vector or a sap.  If THING
  is a SAP, END must be supplied (as length won't work)."
  (declare (optimize (speed 3)))
  (let ((start (or start 0))
	(end (or end (length (the (simple-array * (*)) thing)))))
    (declare (type (integer 0 67108863) start end))
    (let* ((len (lisp::fd-stream-obuf-length stream))
	   (tail (lisp::fd-stream-obuf-tail stream))
	   (space (- len tail))
	   (bytes (- end start))
	   (newtail (+ tail bytes))
	   (info (stream-detail stream)))
      (cond ((minusp bytes) ; Error case
	     (cerror "Just go on as if nothing happened..."
		     "~S called with :END before :START!"
		     'output-raw-bytes))
	    ((zerop bytes)) ; Easy case
	    ((<= bytes space)
	     (if (sys:system-area-pointer-p thing)
		 (kernel:system-area-copy thing
				   (* start vm:byte-bits)
				   (lisp::fd-stream-obuf-sap stream)
				   (* tail vm:byte-bits)
				   (* bytes vm:byte-bits))
		 (kernel:copy-to-system-area thing
				      (+ (* start vm:byte-bits)
					 (* vm:vector-data-offset vm:word-bits))
				      (lisp::fd-stream-obuf-sap stream)
				      (* tail vm:byte-bits)
				      (* bytes vm:byte-bits)))
	     (setf (lisp::fd-stream-obuf-tail stream) newtail)
	     (incf (http-stream-bytes-transmitted info) bytes))
	    ((<= bytes len)
	     (cond ((http-stream-chunks-transmitted info)
		    (multiple-value-bind (prefix suffix)
		        (compute-chunk-info info)
		      (when prefix
			(lisp::do-output stream prefix 0
					 (length prefix) nil))
		      (lisp::flush-output-buffer stream)
		      (when suffix
			(lisp::do-output stream suffix 0
					 (length suffix) nil))))
		   (t
		    (lisp::flush-output-buffer stream)))
	     (if (sys:system-area-pointer-p thing)
		 (kernel:system-area-copy thing
		    (* start vm:byte-bits)
		    (lisp::fd-stream-obuf-sap stream)
		    0
		    (* bytes vm:byte-bits))
	       (kernel:copy-to-system-area thing
	          (+ (* start vm:byte-bits)
		     (* vm:vector-data-offset vm:word-bits))
		  (lisp::fd-stream-obuf-sap stream)
		  0
		  (* bytes vm:byte-bits)))
	     (setf (lisp::fd-stream-obuf-tail stream) bytes)
	     (incf (http-stream-bytes-transmitted info) bytes))
	    (t
	     ;; Send the buffer contents and thing.
	     (incf (http-stream-bytes-transmitted info) bytes)
	     (cond ((http-stream-chunks-transmitted info)
		    (multiple-value-bind (prefix suffix)
		         (compute-chunk-info info)
		      (when prefix
			(lisp::do-output stream prefix 0 (length prefix) nil))
		      (lisp::flush-output-buffer stream)
		      (lisp::do-output stream thing start end nil)
		      (when suffix
			(lisp::do-output stream suffix 0
					 (length suffix) nil))))
		   (t
		    (lisp::flush-output-buffer stream)
		    (lisp::do-output stream thing start end nil))))))))

(defun make-tcp-stream (fd port)
  (declare (optimize (speed 3)))
  (let ((stream (system:make-fd-stream fd :input t :output t)))
    (multiple-value-bind (remote-address remote-port)
      (ext:get-peer-host-and-port fd)
      (let ((http-stream
	     (make-http-stream
	      :local-port  port
	      :remote-host remote-address
	      :remote-port remote-port
	      :bytes-transmitted 0
	      :bytes-received 0
	      :chunk-function nil
	      :chunk-start 0
	      :chunks-transmitted nil))
	    (orig-misc (lisp::fd-stream-misc stream)))
	(setf (aref *stream-details* fd) http-stream)
	
	;; Character output
	(setf (lisp::fd-stream-out stream)
	      #'(lambda (stream byte)
		  (when (< (lisp::fd-stream-obuf-length stream)
			   (+ (lisp::fd-stream-obuf-tail stream) 1))
			(cond ((http-stream-chunks-transmitted http-stream)
			       (multiple-value-bind (prefix suffix)
			          (compute-chunk-info http-stream)
				 (when prefix
				   (lisp::do-output stream prefix 0
						    (length prefix) nil))
				 (lisp::flush-output-buffer stream)
				 (when suffix
				   (lisp::do-output stream suffix 0
						    (length suffix) nil))))
			      (t
				(lisp::flush-output-buffer stream))))
		  (if (eq (char-code byte) (char-code #\NEWLINE))
		      (setf (lisp::fd-stream-char-pos stream) 0)
		    (incf (lisp::fd-stream-char-pos stream)))
		  (setf (sys:sap-ref-8 (lisp::fd-stream-obuf-sap stream)
				       (lisp::fd-stream-obuf-tail stream))
			(char-code byte))
		  (incf (lisp::fd-stream-obuf-tail stream) 1)
		  (incf (http-stream-bytes-transmitted http-stream))
		  (values)))
	  
	;; Binary byte output; not needed yet.
	#+nil
	(setf (lisp::fd-stream-bout stream)
	      #'(lambda (stream byte)
		  (when (< (lisp::fd-stream-obuf-length stream)
			   (+ (fd-stream-obuf-tail stream) 1))
			(cond ((http-stream-chunks-transmitted http-stream)
			       (multiple-value-bind (prefix suffix)
			          (compute-chunk-info http-stream)
				 (when prefix
				   (lisp::do-output stream prefix 0
						    (length prefix) nil))
				 (lisp::flush-output-buffer stream)
				 (when suffix
				   (lisp::do-output stream suffix 0
						    (length suffix) nil))))
			      (t
			       (lisp::flush-output-buffer stream))))
		  (setf (sys:sap-ref-8 (lisp::fd-stream-obuf-sap stream)
				       (lisp::fd-stream-obuf-tail stream))
			byte)
		  (incf (lisp::fd-stream-obuf-tail stream) 1)
		  (incf (http-stream-bytes-transmitted http-stream))
		  (values)))
	#+nil
	(setf (lisp::fd-stream-bin stream) #'lisp::input-unsigned-8bit-byte)

	;; String output
	(setf (lisp::fd-stream-sout stream)
	      #'(lambda (stream thing start end)
		  (let ((start (or start 0))
			(end (or end (length (the vector thing)))))
		    (declare (fixnum start end))
		    (if (stringp thing)
			(let ((last-newline
			       (and (find #\newline (the simple-string thing)
					  :start start :end end)
				    (position #\newline
					      (the simple-string thing)
					      :from-end t
					      :start start
					      :end end))))
			  (assert (eql (lisp::fd-stream-buffering stream)
				      :full))
			  (output-raw-bytes stream thing start end)
			  (if last-newline
			      (setf (lisp::fd-stream-char-pos stream)
				    (- end last-newline 1))
			    (incf (lisp::fd-stream-char-pos stream)
				  (- end start))))
		      (output-raw-bytes stream thing start end)))))

	;; Catch buffer flushes
	(setf (lisp::fd-stream-misc stream)
	      #'(lambda (stream operation &optional arg1 arg2)
		  (case operation
			(:force-output
			 (cond ((http-stream-chunks-transmitted http-stream)
				(multiple-value-bind (prefix suffix)
			          (compute-chunk-info http-stream)
				  (when prefix
				    (lisp::do-output stream prefix 0
						     (length prefix) nil))
				  (lisp::flush-output-buffer stream)
				  (when suffix
				    (lisp::do-output stream suffix 0
						     (length suffix) nil))))
			       (t
				(lisp::flush-output-buffer stream))))
			(:finish-output
			 (cond ((http-stream-chunks-transmitted http-stream)
				(multiple-value-bind (prefix suffix)
			          (compute-chunk-info http-stream)
				  (when prefix
				    (lisp::do-output stream prefix 0
						     (length prefix) nil))
				  (lisp::flush-output-buffer stream)
				  (do ()
				      ((null (lisp::fd-stream-output-later
					      stream)))
				      (system:serve-all-events))
				  (system:serve-all-events)
				  (when suffix
				    (lisp::do-output stream suffix 0
						     (length suffix) nil))))
			       (t
				(lisp::flush-output-buffer stream)
				(do ()
				    ((null (lisp::fd-stream-output-later
					    stream))))
				(system:serve-all-events))))
			(t
			 (funcall orig-misc stream operation arg1 arg2)))))

	;; Catch character reads - needed to decode chucks.
	(setf (lisp::fd-stream-in stream)
	      #'(lambda (stream eof-error eof-value)
		  (incf (http-stream-bytes-received http-stream))
		  (funcall #'lisp::input-character stream eof-error
			   eof-value)))

	(setf (lisp::fd-stream-n-bin stream)
	      #'(lambda (stream buffer start requested eof-error-p)
		  ;; This may be wrong?
		  (incf (http-stream-bytes-received http-stream))
		  (funcall #'lisp::fd-stream-read-n-bytes
			   stream buffer start requested eof-error-p)))
	
	stream))))

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
	 (stream (system:make-fd-stream fd :input t :output t)))
    #+nil
    (format t "*C2 remote-host=~s remote-port=~s fd=~s~%"
	    remote-host remote-port fd)
    (multiple-value-bind (local-address local-port)
       (ext:get-socket-host-and-port fd)
      (declare (ignore local-address))
      (let ((http-stream
	     (make-http-stream
	      :local-port local-port
	      :remote-host remote-host
	      :remote-port remote-port
	      :bytes-transmitted 0
	      :bytes-received 0
	      :chunk-function nil
	      :chunk-start 0
	      :chunks-transmitted nil))
	    (orig-misc (lisp::fd-stream-misc stream)))
	(setf (aref *stream-details* fd) http-stream)

	;; Character output
	(setf (lisp::fd-stream-out stream)
	      #'(lambda (stream byte)
		  (when (< (lisp::fd-stream-obuf-length stream)
			   (+ (lisp::fd-stream-obuf-tail stream) 1))
			(cond ((http-stream-chunks-transmitted http-stream)
			       (multiple-value-bind (prefix suffix)
			          (compute-chunk-info http-stream)
				 (when prefix
				   (lisp::do-output stream prefix 0
						    (length prefix) nil))
				 (lisp::flush-output-buffer stream)
				 (when suffix
				   (lisp::do-output stream suffix 0
						    (length suffix) nil))))
			       (t
				(lisp::flush-output-buffer stream))))
		  (if (eq (char-code byte) (char-code #\NEWLINE))
		      (setf (lisp::fd-stream-char-pos stream) 0)
		    (incf (lisp::fd-stream-char-pos stream)))
		  (setf (sys:sap-ref-8 (lisp::fd-stream-obuf-sap stream)
				       (lisp::fd-stream-obuf-tail stream))
			(char-code byte))
		  (incf (lisp::fd-stream-obuf-tail stream) 1)
		  (incf (http-stream-bytes-transmitted http-stream))
		  (values)))
	  
	;; Binary byte output; not needed yet.
	#+nil
	(setf (lisp::fd-stream-bout stream)
	      #'(lambda (stream byte)
		  (when (< (lisp::fd-stream-obuf-length stream)
			   (+ (fd-stream-obuf-tail stream) 1))
			(cond ((http-stream-chunks-transmitted http-stream)
			       (multiple-value-bind (prefix suffix)
			          (compute-chunk-info http-stream)
				 (when prefix
				   (lisp::do-output stream prefix 0
						    (length prefix) nil))
				 (lisp::flush-output-buffer stream)
				 (when suffix
				   (lisp::do-output stream suffix 0
						    (length suffix) nil))))
			      (t
			       (lisp::flush-output-buffer stream))))
		  (setf (sys:sap-ref-8 (lisp::fd-stream-obuf-sap stream)
				       (lisp::fd-stream-obuf-tail stream))
			byte)
		  (incf (lisp::fd-stream-obuf-tail stream) 1)
		  (incf (http-stream-bytes-transmitted http-stream))
		  (values)))
	#+nil
	(setf (lisp::fd-stream-bin stream) #'lisp::input-unsigned-8bit-byte)

	;; String output
	(setf (lisp::fd-stream-sout stream)
	      #'(lambda (stream thing start end)
		  (let ((start (or start 0))
			(end (or end (length (the vector thing)))))
		    (declare (fixnum start end))
		    (if (stringp thing)
			(let ((last-newline
			       (and (find #\newline (the simple-string thing)
					  :start start :end end)
				    (position #\newline
					      (the simple-string thing)
					      :from-end t
					      :start start
					      :end end))))
			  (assert (eql (lisp::fd-stream-buffering stream)
				      :full))
			  (output-raw-bytes stream thing start end)
			  (if last-newline
			      (setf (lisp::fd-stream-char-pos stream)
				    (- end last-newline 1))
			    (incf (lisp::fd-stream-char-pos stream)
				  (- end start))))
		      (output-raw-bytes stream thing start end)))))

	;; Catch buffer flushes
	(setf (lisp::fd-stream-misc stream)
	      #'(lambda (stream operation &optional arg1 arg2)
		  (case operation
			(:force-output
			 (cond ((http-stream-chunks-transmitted http-stream)
				(multiple-value-bind (prefix suffix)
				    (compute-chunk-info http-stream)
				  (when prefix
				    (lisp::do-output stream prefix 0
						     (length prefix) nil))
				  (lisp::flush-output-buffer stream)
				  (when suffix
				    (lisp::do-output stream suffix 0
						     (length suffix) nil))))
			       (t
				(lisp::flush-output-buffer stream))))
			(:finish-output
			 (cond ((http-stream-chunks-transmitted http-stream)
				(multiple-value-bind (prefix suffix)
			          (compute-chunk-info http-stream)
				  (when prefix
				    (lisp::do-output stream prefix 0
						     (length prefix) nil))
				  (lisp::flush-output-buffer stream)
				  (do ()
				      ((null (lisp::fd-stream-output-later
					      stream)))
				      (system:serve-all-events))
				  (system:serve-all-events)
				  (when suffix
				    (lisp::do-output stream suffix 0
						     (length suffix) nil))))
			       (t
				(lisp::flush-output-buffer stream)
				(do ()
				    ((null (lisp::fd-stream-output-later
					    stream))))
				(system:serve-all-events))))
			(t
			 (funcall orig-misc stream operation arg1 arg2)))))

	 stream))))
