;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: EXCL; Base: 10 -*-

(in-package :excl)

;; Allegro CL IPC interface
;;
;; copyright (c) 1986-1994 Franz Inc, Berkeley, CA
;; All rights reserved.
;;
;; Permission is granted only to any individual or institution which has
;; current Allegro CL license(s) to use, copy, or modify this software,
;; provided any reproduction or distribution of binary versions of this
;; software are compiled with a licensed Allegro CL, and provided
;; that this complete copyright and permission notice is maintained, intact,
;; in all copies and supporting documentation. 
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Supplement 252 52.227-7013 (c) (1) (ii), as
;; applicable.
;; This code is provided to licensed users of Allegro CL as an example 
;; and for restricted reuse.

;;;
;;; From: smh@Franz.COM (Steve Haflich)
;;; Message-Id: <9507061631.AA29427@vapor.Franz.COM>
;;; Cc: bugs@Franz.COM
;;; Subject: Re: [spr13002] CL-HTTP (cont.) how to get a BINARY-OR-CHARACTER-STREAM? 
;;;

;; Don't bother defining input- and output-only classes, since they are
;; of little use.

(defclass bidirectional-multivalent-8-bit-stream
    (bidirectional-terminal-stream)
  ((bytes-received :initform 0 :accessor bytes-received)
   (bytes-transmitted :initform 0 :accessor bytes-transmitted)
   (stream-extensions :initform nil :accessor stream-extensions)))

(defmethod stream-extensions ((stream t))
  nil)

(defmethod bytes-received ((stream t))
  0)

(defmethod bytes-transmitted ((stream t))
  0)

(eval-when (compile eval)
  (defmacro funcall-stm-handler (slot stream &rest args)
    `(funcall (the known-function (sm ,slot ,stream)) ,stream ,@args))
  )

(defmethod stream-read-byte ((stream bidirectional-multivalent-8-bit-stream) &aux ch)
  (fast
   (with-stream-class (bidirectional-multivalent-8-bit-stream stream)
     (setq ch
       (loop
	 (when (> (sm out-pos stream) 0)
	   (stm-bbterm-flush-buffer stream))
	 (let ((outer-without-interrupts (fast *without-interrupts*)) ;bug1852
	       (*without-interrupts* t))
	   (let ((in-pos (sm buffpos stream))
		 (max-in-pos (sm buffer-ptr stream))
		 (in-buffer (sm buffer stream))
		 (nread nil)
		 (char nil)
		 (fd (sm fn-in stream)))
	     (declare (type (simple-array (unsigned-byte 8) 1) in-buffer)
		      (type adim in-pos max-in-pos)
		      (optimize (speed 2)))
	     (if* (= in-pos max-in-pos)
		then (setf (sm buffpos stream) (setq in-pos 1)
			   (sm buffer-ptr stream) 1)
		     ;; position 0 is reserved in case we have to unread
		     (if* (or (null (fast #+(version>= 4 3) sys::*scheduler-stack-group*
					  #-(version>= 4 3) mp::*scheduler-stack-group*))
			      (funcall-stm-handler listen stream)
			      ;; This handles an error in the scheduler.
			      (fast (eq sys:*current-stack-group*
					#+(version>= 4 3) sys::*scheduler-stack-group*
					#-(version>= 4 3) mp::*scheduler-stack-group*)))
			then (setq nread
			       (let ((*without-interrupts* ;bug1852 bug3054
				      outer-without-interrupts))
				 (filesys-read-buffer fd in-buffer)))
			     (if* nread
				then (setf (sm buffer-ptr stream)
				       (the adim (1+ (the adim nread))))
				     (setf char (aref in-buffer in-pos))
				     (setf (sm buffpos stream) (the adim (1+ in-pos)))
				     (return char)
				else (return nil)))
		     (let ((*without-interrupts* outer-without-interrupts)) ;bug1852
		       #-(version>= 4 3)
		       (mp::wait-for-input-available-one fd stream (sm listen stream))
		       #+(version>= 4 3)
		       (funcall-in-package :wait-for-input-available-one :mp
					   fd stream (sm listen stream)))
	      elseif (> in-pos max-in-pos) ; eof was :unread
		then (setf (sm buffpos stream) max-in-pos)
		     (return-from stream-read-byte ':eof)
		else (setf char (aref in-buffer in-pos))
		     (setf (sm buffpos stream) (the adim (1+ in-pos)))
		     (return char))))))
     ;; Dribble processing would go here.
     (if ch
	 (incf (slot-value stream 'bytes-received)))
     (or ch
	 ':eof))))

(defvar *last-flushed-out-pos* 0)

(defmethod stream-write-byte ((stream bidirectional-multivalent-8-bit-stream) byte)
  (fast
   (let ((*last-flushed-out-pos* 0))
     (declare (type adim *last-flushed-out-pos*))
   (with-stream-class (output-terminal-stream stream)
     ;; Column position tracking would go here.
     (let ((out-pos (sm out-pos stream))
	   (max-out-pos (sm max-out-pos stream))
	   (out-buffer (sm out-buffer stream)))
       (declare (type (simple-array (unsigned-byte 8) 1) out-buffer)
		(type adim out-pos max-out-pos)
		(optimize (speed 2)))
       (when (>= out-pos max-out-pos)
	 (stm-bbterm-flush-buffer stream)
	 (setq out-pos *last-flushed-out-pos*))
       (setf (aref out-buffer out-pos) byte)
       (setf (sm out-pos stream) (the adim (1+ out-pos)))
       ;; Dribble processing would go here.
       (incf (slot-value stream 'bytes-transmitted))
       byte)))))

(defmethod stream-read-char ((stream bidirectional-multivalent-8-bit-stream) &aux character)
  (fast 
   (setq character (call-next-method))
   (incf (slot-value stream 'bytes-received))
   character))

(defmethod stream-unread-char ((stream bidirectional-multivalent-8-bit-stream) character)
  (fast 
   (setq character (call-next-method))
   (decf (slot-value stream 'bytes-received))
   character))

(defmethod stream-write-char ((stream bidirectional-multivalent-8-bit-stream) character)
  (fast 
   (let ((*last-flushed-out-pos* 0))
     (setq character (call-next-method))
     (with-stream-class (output-terminal-stream stream)
       (if (plusp *last-flushed-out-pos*)
	   (setf (sm out-pos stream) *last-flushed-out-pos*))))
   (incf (slot-value stream 'bytes-transmitted))
   character))

#-sgi
(defmethod stream-write-string ((stream bidirectional-multivalent-8-bit-stream) string &optional (start 0) (end (length string)))
  (stream-write-sequence stream string start end))

;;; This may be the default method franz provides by now 2/97
;;; never got a response - OBC
#+sgi
(defmethod stream-write-string ((stream bidirectional-multivalent-8-bit-stream) string
                                &optional (start 0) (end (length string)))
  (fast
   (if (stringp string)
       (loop for i from start below end
           do (stream-write-char stream (elt string i)))
     (loop for i from start below end
	 do (stream-write-byte stream (elt string i))))
   string))

#||
Tests
==============================================================
;; Redefine to use the new stream.  Best to copy the source from ipc.cl
;; in your particular distribution.
<19> (defun ipc::make-ipc-terminal-stream (fd &optional pretty-id)
      (let ((s (make-instance 'excl::bidirectional-multivalent-8-bit-stream
		 :fn-in  fd
		 :fn-out fd)))
	(when pretty-id
	  (setf (getf (excl::stream-property-list s) :pretty-socket-id)
	    pretty-id))
	s))
Error: [package lock error text deleted]
Restart actions (select using :continue):
 0: Set the function definition of the name ipc::make-ipc-terminal-stream anyway.
[1c] <20> :cont
ipc::make-ipc-terminal-stream
<30> (setq q (ipc:open-network-stream :host "vapor" :port "finger"))
#<excl::bidirectional-multivalent-8-bit-stream to [vapor/finger] fd 7 @ #x746312>
<31> (format q "smh")
nil
<32> (write-byte (char-int #\newline) q)
10
<33> (force-output q)
nil
<34> (listen q)
t
<35> (read-line q)
"Login name: smh       			In real life: Steve Haflich
"
nil
<36> (loop as c = (stream-read-byte q)
	 until (eql c :eof)
	 collect (int-char c))
(#\D #\i #\r #\e #\c #\t #\o #\r #\y #\: ...)
<37> (close q)
t
====================
||#
