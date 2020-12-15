;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: EXCL; Base: 10 -*-

(in-package "EXCL")

;;;
;;; File/Stream to file/stream transfer tests
;;; These test illustrate large peformance improvement for the ACL
;;; substrate by implementing READ/WRITE SEQUENCE doing block
;;; transfer as suggested by X3J13 STREAM-DEFINITION-BY-USER,
;;; Version 1, 22-Mar-89 by David N. Gray
;;;
;;; Next we want all ports to have a David Gray's like implementation
;;; of stream provided for TCP/IP streams and working efficiently
;;; on the target hardware... - OBC
;;;

;;; (excl::transfer-test "/tmp/sample.avi" "/tmp/junk")  ~1Mbytes 350-450msec
;;; (excl::transfer-test "/tmp/sun2c.avi" "/tmp/junk")  ~7Mbytes 1-3sec
#+debug
(defun transfer-test (path1 path2)
  (with-stream-buffer-size (1024) ;optimal buffer size
    (with-open-file (str1 path1 :direction :input
		     :element-type '(unsigned-byte 8))
      (with-open-file (str2 path2 :direction :output
		       :if-exists :new-version
		       :element-type '(unsigned-byte 8))
	(print (file-length str1))
	(let ((buffer (stream-input-buffer str1))
	      (size (stream-buffer-length str1)))
	  (time (fast (loop as end = (read-sequence buffer str1 :end size)
			  do (write-sequence buffer str2 :end end)
			  until (zerop end)))))))))

;;; (excl::transfer-test0 "/tmp/sample.avi" "/tmp/junk")  ~1Mbytes 450msec
;;; (excl::transfer-test0 "/tmp/sun2c.avi" "/tmp/junk")  ~7Mbytes 3sec
#+debug
(defun transfer-test0 (path1 path2)
  (let ((buffer (make-array 1024 :element-type 'character))) ;optimal buffer size
    (with-stream-buffer-size (1024) ;optimal buffer size
      (with-open-file (str1 path1 :direction :input
		       :element-type 'character)
	(with-open-file (str2 path2 :direction :output
			 :if-exists :new-version
			 :element-type 'character)
	  (print (file-length str1))
	  (time (fast (loop as end = (read-sequence buffer str1)
			  do (write-sequence buffer str2 :end end)
			  until (zerop end)))))))))

;;; (excl::transfer-test1 "/tmp/sample.avi" "/tmp/junk")  ~1Mbytes 450msec
;;; (excl::transfer-test1 "/tmp/sun2c.avi" "/tmp/junk")  ~7Mbytes 3sec
#+debug
(defun transfer-test1 (path1 path2)
  (let ((buffer (make-array 1024 :element-type '(unsigned-byte 8)))) ;optimal buffer size
   (with-stream-buffer-size (1024) ;optimal buffer size
     (with-open-file (str1 path1 :direction :input
		      :element-type '(unsigned-byte 8))
       (with-open-file (str2 path2 :direction :output
			:if-exists :new-version
			:element-type '(unsigned-byte 8))
	 (print (file-length str1))
	 (time (fast (loop as end = (read-sequence buffer str1)
			 do (write-sequence buffer str2 :end end)
			 until (zerop end)))))))))

#+debug
(defun echo (x2 &optional (echo t))
  (when (listen x2)
    (cond (echo
	   (write-char (read-char x2))
	   (loop for i from 1 upto (- (slot-value x2 'excl::buffer-ptr) (slot-value x2 'excl::buffpos))
	       do (write-char (read-char x2))))
	  (t
	   (read-char x2)
	   (clear-input x2)))
    t))

#+debug
(defun clear0 (&optional (stream x1))
  (loop while (echo stream nil)
      do (mp:process-allow-schedule)))

#+debug
(defun clear (&optional (stream x1))
  (loop while (echo stream nil)
      do (mp:process-wait-with-timeout "Listen..." 1 #'listen stream)))

;;; Tests writing to a TCP stream. This test starts writing first,
;;; on the other end of the TCP connection (clear) is called so suck
;;; the data as fast as possible. These numbers are too high, so I
;;; question what clear-input is doing. The numbers go down to
;;; 1Mbytes in 2 seconds with a send/receive test instead of send/clear!

;;; (excl::send-test0 "/tmp/sample.avi" x1) ~9-10sec over TCP
#+debug
(defun send-test0 (path1 str2 &optional (size 512))
  (with-stream-buffer-size (size) ;optimal buffer size
    (with-open-file (str1 path1 :direction :input
		     :element-type 'character) ;type irrelevant here
      (print (file-length str1))
      (let ((buffer (stream-input-buffer str1))
	    (size (stream-buffer-length str1)))
	(time (fast (loop as end = (read-sequence buffer str1 :end size)
			do (write-sequence buffer str2 :end end)
			until (zerop end))))))))

;;; (excl::send-test1 "/tmp/sample.avi" x1) 9-10sec over TCP
#+debug
(defun send-test1 (path1 str2 &optional (size 512))
  (with-stream-buffer-size (size) ;optimal buffer size
    (with-open-file (str1 path1 :direction :input
		     :element-type 'character) ;type irrelevant here
      (print (file-length str1))
      (let ((fd1 (stream-input-fd str1))
	    (buffer (stream-input-buffer str1))
	    ;(size (stream-buffer-length str1))
	    (fd2 (stream-output-fd str2)))
	(declare (type simple-string buffer)
		 (type adim size))
	(time (fast (loop as sz = (filesys-read-buffer fd1 buffer)
			while sz
			do (filesys-write-string fd2 buffer 0 sz))
		    #+ignore ;; Fatal failure in 4.2
		    (loop as sz = (filesys-read-string fd1 buffer size)
			until (zerop sz)
			do (filesys-write-string fd2 buffer 0 sz))))))))

;;; (excl::send-test2 "/tmp/sample.avi" x1) 9-10sec over TCP
#+debug
(defun send-test2 (path1 str2 &optional (size 512))
  (with-stream-buffer-size (size) ;optimal buffer size
    (with-open-file (str1 path1 :direction :input
		     :element-type '(unsigned-byte 8)) ;type irrelevant here
      (print (file-length str1))
      (let ((fd1 (stream-input-fd str1))
	    (buffer (stream-input-buffer str1))
	    ;(size (stream-buffer-length str1))
	    (fd2 (stream-output-fd str2)))
	(declare (type simple-vector buffer)
		 (type adim size))
	(time (fast (loop as sz = (filesys-read-bytes fd1 buffer 0 size)
			until (zerop sz)
			do (filesys-write-bytes fd2 buffer 0 sz))))))))

;;; These are now (finally) the most effective transfer means
;;; (in speed and consing) from file in character or byte format
;;; to a TCP stream. We use two ACL images on same machine or
;;; two different machine for the rest of these tests.
;;;

;;; (excl::send-test3 "/tmp/sample.avi" x1) 9-10sec over TCP
#+debug
(defun send-test3 (path1 str2 &optional (size 512))
  (let ((buffer (make-array size :element-type 'character))) ;optimal buffer size
    (with-stream-buffer-size (size) ;optimal buffer size
      (with-open-file (str1 path1 :direction :input
		       :element-type 'character)
	(format t "~&Transfering ~d bytes with buffers input/output sizes = ~d/~d.~%" (file-length str1) (stream-buffer-length str1) (stream-buffer-length str2))
	(time (fast (loop as end of-type adim = (the adim (stream-read-sequence str1 buffer))
			do (write-sequence buffer str2 :end end)
			until (zerop end))))))))

;;; (excl::send-test4 "/tmp/sample.avi" x1) 9-10sec over TCP
#+debug
(defun send-test4 (path1 str2 &optional (size 512))
  (let ((buffer (make-array size :element-type '(unsigned-byte 8)))) ;optimal buffer size
    (with-stream-buffer-size (size) ;optimal buffer size
      (with-open-file (str1 path1 :direction :input
		       :element-type '(unsigned-byte 8))
	(format t "~&Transfering ~d bytes with buffers input/output sizes = ~d/~d.~%" (file-length str1) (stream-buffer-length str1) (stream-buffer-length str2))
	(time (fast (loop as end fixnum = (the fixnum (read-sequence buffer str1))
			do (write-sequence buffer str2 :end end)
			until (zerop end))))))))

;;; Note: on receive we assume the transmission is continuous
;;; and detect EOT based when receiving less than expected.
;;; This is only for testing transmission rate it would not
;;; hold for realistic transmissions...

;;; (excl::receive-test1 x1 "/tmp/junk.avi") 2sec over TCP for 1Mbytes
;;; (excl::receive-test1 x1 "/tmp/junk.avi") 14sec over TCP for 7.2Mbytes
#+debug
(defun receive-test1 (str1 path2 &optional (size 512))
  (let ((buffer (make-array size :element-type 'character))) ;optimal buffer size
    (with-stream-buffer-size (size) ;optimal buffer size
      (with-open-file (str2 path2 :direction :output
		       :element-type 'character
		       :if-exists :supersede)
	(format t "~&Receiving with buffers input/output sizes = ~d/~d.~%" (stream-buffer-length str1) (stream-buffer-length str2))
	(time (fast (loop as end of-type adim = (the adim (read-sequence buffer str1 :end size))
			do (write-sequence buffer str2 :end end)
			until (< end size))))))))

;;; (excl::receive-test2 x1 "/tmp/junk.avi") 2sec over TCP for 1Mbytes
;;; (excl::receive-test1 x1 "/tmp/junk.avi") 14sec over TCP for 7.2Mbytes
#+debug
(defun receive-test2 (str1 path2 &optional (size 512))
  (let ((buffer (make-array size :element-type '(unsigned-byte 8)))) ;optimal buffer size
    (with-stream-buffer-size (size) ;optimal buffer size
      (with-open-file (str2 path2 :direction :output
		       :element-type '(unsigned-byte 8)
		       :if-exists :supersede)
	(format t "~&Receiving with buffers input/output sizes = ~d/~d.~%" (stream-buffer-length str1) (stream-buffer-length str2))
	(time (fast (loop as end of-type adim = (the adim (read-sequence buffer str1 :end size))
			do (write-sequence buffer str2 :end end)
			until (< end size))))))))

;;; And you verify the two files are identical after complete
;;; transmission over a send-test and a receive-test side...
;;; This averages a transfer rate of 500Kbytes per second, just
;;; as expected (from tmp file through TCP stream to tmp file).

#+debug
(setq xx
  "
0                                                             1
0                                                             2
0                                                             3
0                                                             4
0                                                             5
0                                                             6
0                                                             7
0                                                             8")
