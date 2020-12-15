;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: EXCL; Base: 10 -*-

(in-package :excl)

;;; Copyright (C) 1995 Olivier (OBC).
;;;	All Rights Reserved.

(defpackage "EXCL"
  (:use) (:export "STREAM-BUFFER-LENGTH" "STREAM-INPUT-BUFFER" "WITH-STREAM-BUFFER-SIZE"))

(defmacro with-stream-buffer-size ((size) . body)
  ;; One extra position in buffer for unread from lower level
  `(let ((EXCL::STREAM-BUFFER-SIZE (1+ ,size)))
     ,@body))

(defmethod stream-input-fd ((stream fundamental-input-stream))
  (slot-value stream 'fn-in))

(defmethod stream-input-buffer ((stream fundamental-input-stream))
  (slot-value stream 'buffer))

(defmethod stream-output-buffer ((stream fundamental-output-stream))
  (slot-value stream 'out-buffer))

(defmethod stream-buffer-length ((stream fundamental-input-stream))
  (if (slot-exists-p stream 'max-out-pos)
      (slot-value stream 'max-out-pos)
    ;; One extra position in buffer for unread from lower level
    (1- (length (slot-value stream 'buffer)))))

(defmethod stream-buffer-length ((stream fundamental-output-stream))
  (if (slot-exists-p stream 'max-out-pos)
      (slot-value stream 'max-out-pos)
    ;; One extra position in buffer for unread from lower level
    (1- (length (slot-value stream 'buffer)))))

(defmethod (setf stream-buffer-length) (size (stream fundamental-output-stream))
  (if (slot-exists-p stream 'max-out-pos)
      (setf (slot-value stream 'max-out-pos) size)
    (error "No way to change the output buffer length for ~s." stream)))

(defmethod stream-output-fd ((stream fundamental-output-stream))
  (slot-value stream 'fn-out))

(defmethod stream-output-fd ((stream two-way-stream))
  (stream-output-fd (two-way-stream-output-stream stream)))

(defmethod stream-output-fd ((stream broadcast-stream))
  (loop for stream in (broadcast-stream-streams stream)
      as fd2 = (stream-output-fd stream)
      when fd2 collect fd2
      else do (error "Not an output stream ~s." stream)))

;;; Steam utilities for sequence read write and optimized throughput
;;;

(defmethod %move-buffers ((array1 vector) (array2 vector) start1 end1 start2 end2)
  (declare (type (simple-array (unsigned-byte 8) *) array1 array2)
	   (type adim start1 end1 start2 end2))
  (fast
   (loop for i of-type adim from start1 below end1
       as j of-type adim from start2 below end2
       do (setf (aref array2 j) (the (unsigned-byte 8) (aref array1 i))))))

(defmethod %move-buffers ((array1 vector) (array2 string) start1 end1 start2 end2)
  (declare (type (simple-array character *) array2)
	   (type (simple-array (unsigned-byte 8) *) array1)
	   (type adim start1 end1 start2 end2))
  (fast
   (loop for i of-type adim from start1 below end1
       as j of-type adim from start2 below end2
       do (setf (aref array2 j) (the character (int-char (the (unsigned-byte 8) (aref array1 i))))))))

(defmethod %move-buffers ((array1 string) (array2 vector) start1 end1 start2 end2)
  (declare (type (simple-array (unsigned-byte 8) *) array2)
	   (type (simple-array character *) array1)
	   (type adim start1 end1 start2 end2))
  (fast
   (loop for i of-type adim from start1 below end1
       as j of-type adim from start2 below end2
       do (setf (aref array2 j) (the (unsigned-byte 8) (char-code (the character (aref array1 i))))))))

(defmethod %move-buffers ((array1 string) (array2 string) start1 end1 start2 end2)
  (declare (type (simple-array character *) array1 array2)
	   (type adim start1 end1 start2 end2))
  (fast
   (loop for i of-type adim from start1 below end1
       as j of-type adim from start2 below end2
       do (setf (aref array2 j) (the character (aref array1 i))))))

#+ignore ;; bypassed
(defmethod nsubseq ((sequence array) (start fixnum 0) &optional (end (length sequence)))
  (locally (declare (optimize (speed 3) (safety 0)))
    (if (and (= start 0)
	     (eql end (length sequence)))
	sequence
      (make-array (- end start)
		  :element-type (array-element-type sequence)
		  :displaced-to sequence
		  :displaced-index-offset start))))

(defmacro setf-buf ((subseq buffer2 start2 end2) (nsubseq buffer1 start1 end1) . body)
  (declare (ignore subseq nsubseq))
  `(mp:without-scheduling 
     (%move-buffers ,buffer1 ,buffer2 (the adim ,start1) (the adim ,end1) (the adim ,start2) (the adim ,end2))
     (setf ,@body)))

;;; If there is a definition specialized on string get rid of it
#+debug
(user::undefmethod stream:stream-write-sequence bidirectional-multivalent-8-bit-stream string)

(defmethod stream:stream-write-sequence ((stream bidirectional-multivalent-8-bit-stream) (sequence array) &optional (start 0) (end (length sequence)))
  (declare (type (simple-array (unsigned-byte 8) *) sequence)
	   (type adim start end))
  (fast
   (let ((*last-flushed-out-pos* 0))
   (with-stream-class (bidirectional-multivalent-8-bit-stream stream)
     (let ((out-pos (sm out-pos stream))
	   (max-out-pos (sm max-out-pos stream))
	   (out-buffer (sm out-buffer stream))
	   (fn-out (sm fn-out stream))
	   (btlen (- end start)))
       (declare (type (simple-array character *) out-buffer)
		(type adim out-pos max-out-pos btlen))
       (when (>= out-pos max-out-pos)
	 (stm-bbterm-flush-buffer stream)
	 (setq out-pos *last-flushed-out-pos*))
       (let ((availen (- max-out-pos out-pos))
	     (reqlen (- end start)))
	 (declare (type adim availen reqlen))
	 ;; Need a fresh start
	 (when (and (> out-pos 0)
		    (<= availen reqlen))
	   (setf-buf (subseq out-buffer out-pos (incf out-pos availen))
		     (nsubseq sequence start (incf start availen))
		     (sm out-pos stream) out-pos)
	   (stm-bbterm-flush-buffer stream)
	   (setq out-pos *last-flushed-out-pos*
		 availen max-out-pos
		 reqlen (- end start)))
	 (loop
	   (cond ((<= reqlen (1+ availen))
		  (cond ((and (= out-pos 0)
			      (> reqlen (ash availen -1)))
			 ;; Direct bulk transfer for speed
			 (typecase sequence
			   (string 
			    (filesys-write-string fn-out sequence start end #+(version>= 4 3) stream))
			   (vector
			    (filesys-write-bytes fn-out sequence start end #+(version>= 4 3) stream))))
			(t
			 (setf-buf (subseq out-buffer out-pos (incf out-pos reqlen)) (nsubseq sequence start end)
				   (sm out-pos stream) out-pos)))
		  (return))
		 (t
		  ;; Direct bulk transfer for speed
		  (typecase sequence
		    (string 
		     (filesys-write-string fn-out sequence start (incf start availen) #+(version>= 4 3) stream))
		    (vector
		     (filesys-write-bytes fn-out sequence start (incf start availen) #+(version>= 4 3) stream)))
		  (setq out-pos 0)
		  (decf reqlen availen))))
	 (incf (slot-value stream 'bytes-transmitted) btlen)
	 sequence))))))

;;; If there is a definition specialized on string get rid of it
#+debug
(user::undefmethod stream:stream-read-sequence bidirectional-multivalent-8-bit-stream string)

(eval-when (load eval)
  (load "" :unreferenced-lib-names
	(mapcar #'ff:convert-to-lang
		'("read"))))

(ff:defforeign 'filesys-read-buffer0
    :arguments '(fixnum simple-string fixnum) ;; fd, *buf, nbytes
    :entry-point (ff:convert-to-lang "read")
    :call-direct t
    :callback nil
    :return-type :integer)

;;; This is written to avoid blocking, read the largest chunk that
;;; is readily available. Don't try reading more if you can tell the
;;; transmission is lagging. If the input is continuous the largest
;;; chuncks are always obtained until the last one. The first chunk
;;; read may be smaller than expected if there is left over from
;;; a previous transmission, this is to allow resynchronization of
;;; buffered streams.

;;; We have to work around a fatal bug that prevents us from using
;;; filesys-read-string. We use filesys-read-buffer0 when we can,
;;; however if "start" is non zero we use filesys-read-buffer which may
;;; require multiple read and copy into "sequence" to match the length.
;;; Insuring the sequence and internal stream buffer length match in this
;;; last case will insure butter performance until the substrate is fixed. -OBC
;;;
(defmethod stream:stream-read-sequence ((stream bidirectional-multivalent-8-bit-stream) (sequence array) &optional (start 0) end-or-nil)
  (declare (type adim start)
	   (type (simple-array (unsigned-byte 8) *) sequence))
  (fast
   (with-stream-class (bidirectional-multivalent-8-bit-stream stream)
     (let ((length (the adim (length sequence)))
	   (end 0)
	   (nread 0)
	   (in-buffer (sm buffer stream))
	   (in-length 0)
	   (fd (sm fn-in stream)))
       (declare (type (simple-array (unsigned-byte 8) 1) in-buffer)
		(fixnum fd)
		(type adim length end nread in-length))
       (setq end (or end-or-nil length))
       (setq in-length (the adim (1- (length in-buffer))))
       (loop
	 (when (> (sm out-pos stream) 0)
	   (stm-bbterm-flush-buffer stream))
	 (let ((outer-without-interrupts (fast *without-interrupts*)) ;bug1852
	       (*without-interrupts* t)
	       (in-pos (sm buffpos stream))
	       (max-in-pos (sm buffer-ptr stream)))
	   (declare (type adim in-pos max-in-pos nread)
		    #||(optimize (speed 2))||#)
	   (cond ((= in-pos max-in-pos)
		  (setf (sm buffpos stream) (setq in-pos 1)
			(sm buffer-ptr stream) 1)
		  ;; position 0 is reserved in case we have to unread
		  (when (or (null (fast #+(version>= 4 3) sys::*scheduler-stack-group*
					#-(version>= 4 3) mp::*scheduler-stack-group*))
			    #-(version>= 4 3) (funcall-stm-handler listen stream)
			    #+(version>= 4 3) (stream:stream-listen stream)
			    ;; This handles an error in the scheduler.
			    (fast (eq sys:*current-stack-group*
				      #+(version>= 4 3) sys::*scheduler-stack-group*
				      #-(version>= 4 3) mp::*scheduler-stack-group*)))
		    (let ((*without-interrupts* ;bug1852 bug3054
			   outer-without-interrupts))
		      (etypecase sequence
			(string
			 (cond ((zerop start) ;optimal string read
				(setq nread
				  (filesys-read-buffer0 fd sequence end))
				(cond ((> nread 0)
				       ;; For unread-char
				       (setf (aref in-buffer 0) (the character (aref sequence (1- nread))))
				       (return nread))
				      ((zerop nread)
				       (return nread))
				      (t
				       ;; Go ask for more
				       (setq nread 0)
				       (return nread))))
			       (t
				(let ((reqlen (the adim (- end start)))
					 (mread 0))
				  (declare (type adim reqlen mread))
				  (setq mread
				    (or (filesys-read-buffer fd in-buffer) -1))
				  (when (> mread 0)
				    (setf max-in-pos (the adim (+ in-pos (the adim mread)))
					  (sm buffer-ptr stream) max-in-pos)
				    (setf mread (the adim (min mread reqlen)))
				    (incf nread mread)
				    (setf-buf (subseq sequence start (the adim (incf start mread)))
					      (nsubseq in-buffer in-pos (the adim (incf in-pos mread)))
					      (sm buffpos stream) in-pos))
				  (cond ((zerop mread)
					 (return nread))
					((< mread 0)
					 (setq mread 0))
					((or (= mread reqlen)
					     (< mread in-length))
					 ;; For unread-char
					 (setf (aref in-buffer 0) (the character (aref sequence (1- nread))))
					 (return nread)))
				  ;; Otherwise go ask for more
				  nil))))
			(vector ;fast vector fill
			 (setq nread
			   (filesys-read-bytes fd sequence start end))
			 (if (> nread 0)
			     ;; For unread-char
			     (setf (aref in-buffer 0) (the character (int-char (the (unsigned-byte 8) (aref sequence (1- nread)))))))
			 (return nread)))))
		  ;; We thing there is more input coming - wait for more
		  (let ((*without-interrupts* outer-without-interrupts)) ;bug1852
		    #-(version>= 4 3)
		    (mp::wait-for-input-available-one fd stream (sm listen stream))
		    #+(version>= 4 3)
		    (funcall-in-package :wait-for-input-available-one :mp
					fd stream (sm listen stream))))
		 ((> in-pos max-in-pos) ; eof was :unread
		  (setf (sm buffpos stream) max-in-pos)
		  (return 0))
		 (t
		  (setq nread (the adim (min (the adim (- max-in-pos in-pos)) (the adim (length sequence)))))
		  (setf-buf (subseq sequence 0 nread)
			    (nsubseq in-buffer in-pos max-in-pos)
			    (sm buffpos stream) (the adim max-in-pos))
		  (return nread)))))
       (incf (slot-value stream 'bytes-received) nread)
       nread))))
