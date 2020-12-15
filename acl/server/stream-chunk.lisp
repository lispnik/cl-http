;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: EXCL; Base: 10 -*-

(in-package :excl)

;;;
;;; (c) Copyright  1996, Olivier (OBC)
;;;     All Rights Reserved.

;;; OBC dangerous minimal hooks for ACL UNIX
;;;

;;; The chunking code is really not what we need for ascii ports:
;;; we could simplify all the stuff by directly working on
;;; the character buffer and streams. However lets not make yet another
;;; version of the algorithm. The overhead of using an "8-bit-vector"
;;; will be low since it's only for writing chunk size information
;;; - not the contents.
;;;

;;;
;;; ENCODING MAC TCP API
;;;

(defmacro %put-byte (buffer char-code position)
  `(setf (elt ,buffer ,position) ,(if (numberp char-code)
				      (character char-code)
				    `(character ,char-code))))

(unless (fboundp 'stm-bbterm-flush-buffer-orig)
  (setf (fdefinition 'stm-bbterm-flush-buffer-orig)
    (fdefinition 'stm-bbterm-flush-buffer)))

#||
(METHOD STREAM-READ-CHAR (INPUT-TERMINAL-STREAM)), :OPERATOR
(METHOD STREAM-FORCE-OUTPUT (OUTPUT-TERMINAL-STREAM)), :OPERATOR
(METHOD STREAM-WRITE-CHAR (OUTPUT-TERMINAL-STREAM T)), :OPERATOR
(METHOD STREAM-WRITE-STRING (OUTPUT-TERMINAL-STREAM T)), :OPERATOR
STM-BBTERM-MISC, :OPERATOR
(:INTERNAL (METHOD STREAM-WRITE-BYTE (BIDIRECTIONAL-MULTIVALENT-8-BIT-STREAM T)) 0), :OPERATOR
(:INTERNAL (METHOD STREAM-READ-BYTE (BIDIRECTIONAL-MULTIVALENT-8-BIT-STREAM)) 0), :OPERATOR
(:INTERNAL (METHOD STREAM-READ-SEQUENCE (BIDIRECTIONAL-MULTIVALENT-8-BIT-STREAM ARRAY)) 0), :OPERATOR
(METHOD STREAM-WRITE-SEQUENCE (BIDIRECTIONAL-MULTIVALENT-8-BIT-STREAM ARRAY)), :OPERATOR
||#

(defun stm-bbterm-flush-buffer (stream)
  (let ((encoder (stream-extensions stream)))
    (cond (encoder
	   (tcp-encoder-before-tcp-send encoder stream)
	   (stm-bbterm-flush-buffer-orig stream)
	   ;; This should be done by the original flush!
	   (setf (slot-value stream 'out-pos) 0)
	   (tcp-encoder-after-tcp-send encoder stream)
	   ;; Kludge!!
	   (setq *last-flushed-out-pos* (slot-value stream 'out-pos)))
	  (t
	   (stm-bbterm-flush-buffer-orig stream)))))

(defconstant *default-character-type* 'character)

(deftype chunk-transfer-encoding-output-stream-mixin () 'bidirectional-multivalent-8-bit-stream)

(setf (find-class 'CHUNK-TRANSFER-ENCODING-OUTPUT-STREAM-MIXIN)
  (find-class 'bidirectional-multivalent-8-bit-stream))

(defmethod conn-encoder ((stream bidirectional-multivalent-8-bit-stream))
  (stream-extensions stream))

(defmethod (setf conn-encoder) (encoder (stream bidirectional-multivalent-8-bit-stream))
  (setf (stream-extensions stream) encoder))

(defmethod conn-write-count ((stream fundamental-output-stream))
  (slot-value stream 'out-pos))

(defmethod (setf conn-write-count) (count (stream fundamental-output-stream))
  (setf (slot-value stream 'out-pos) count))

(setf (fdefinition 'conn-write-bufsize) (fdefinition 'stream-buffer-length))

(setf (fdefinition '(setf conn-write-bufsize)) (fdefinition '(setf stream-buffer-length)))

(setf (fdefinition 'conn-write-buffer) (fdefinition 'stream-output-buffer))

(setf (fdefinition 'conn-bytes-transmitted) (fdefinition 'bytes-transmitted))

(setf (fdefinition '(setf conn-bytes-transmitted)) (fdefinition '(setf bytes-transmitted)))

(defun tcp-force-output (stream ignore)
  (declare (ignore ignore))
  (force-output stream))

(setf (fdefinition 'tcp-stream-conn) (fdefinition 'identity))

;;;
;;; DECODING MAC TCP API
;;;

(deftype chunk-transfer-decoding-input-stream-mixin () 'bidirectional-multivalent-8-bit-stream)

(setf (find-class 'CHUNK-TRANSFER-DECODING-INPUT-STREAM-MIXIN)
  (find-class 'bidirectional-multivalent-8-bit-stream))

(defmethod conn-decoder ((stream bidirectional-multivalent-8-bit-stream))
  (stream-extensions stream))

(defmethod (setf conn-decoder) (decoder (stream bidirectional-multivalent-8-bit-stream))
  (setf (stream-extensions stream) decoder))

(defun input-mode (stream)
  (declare (ignore stream))
  :multivalent)

(defun binary-input-mode (stream) stream)

(defun set-input-mode (stream mode) (declare (ignore stream)) mode)

(setf (fdefinition 'conn-stream) (fdefinition 'identity))

(defmethod conn-read-count ((stream fundamental-input-stream))
  (- (length (slot-value stream 'buffer)) (slot-value stream 'bufferpos)))

(defmethod (setf conn-read-count) (count (stream fundamental-input-stream))
  (setf (slot-value stream 'bufferpos) (- (length (slot-value stream 'buffer)) count)))

(setf (fdefinition 'tcp-read-byte) (fdefinition 'read-byte))
