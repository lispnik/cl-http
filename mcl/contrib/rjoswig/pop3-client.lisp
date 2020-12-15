;;;-*- Mode: Lisp; Package: POP3 -*-
;;; ===========================================================================
;;;; POP3 Access for MCL

;;; Author: Rainer Joswig, joswig@lavielle.com
;;; All rights reserved, 1998


;;; To Do:
;;;
;;; Make it platform independent for CL-HTTP
;;; More checking in the source.
;;; More testing of this code.
;;; Provide MIME decoder.
;;; Parse Mail headers


(defpackage "POP3"
  (:use "COMMON-LISP")
  (:export "STANDARD-POP-PORT"
           "*STANDARD-POP-PORT*"
           "*DEBUG-POPPER*"
           "MAILBOX-STATISTICS"
           "MAILBOX-MESSAGE-STATISTICS"
           "RETRIEVE-MESSAGE-TOP"
           "RETRIEVE-ALL-MESSAGE-TOPS"
           "RETRIEVE-MESSAGE"
           "RETRIEVE-ALL-MESSAGES"
           "MARK-MESSAGE-FOR-DELETION"
           "WITH-MAILBOX"))


(in-package "POP3")


;;; ===========================================================================
;;; TCP Interface for MCL

#+mcl
(eval-when (:compile-toplevel :execute :load-toplevel)
  (pushnew '("pop3" . 110)
           ccl::*service-name-number-alist*
           :test #'equalp))

#+mcl
(defun telnet-read-line (stream)
  (ccl::telnet-read-line stream))

#+mcl
(defun buffered-telnet-read-line (stream buffer)
  "Read a CRLF-terminated line into buffer"
  (unless (ccl::stream-eofp stream)
    (setf (fill-pointer buffer) 0)
    (let ((char nil))
      (do () ((or (null (setq char (ccl::stream-tyi stream)))
                  (and (eq char #\CR) (eq (ccl::stream-peek stream) #\LF)))
              (when char (ccl::stream-tyi stream))
              (values buffer (null char)))
        (vector-push-extend char buffer)))))

#+mcl
(defun telnet-write-line (stream string)
  (ccl::telnet-write-line stream string))

#+mcl
(defun open-tcp-stream (host port)
  (ccl::open-tcp-stream host port))

;;; ===========================================================================
;;; Resources

(defparameter *pop-line-buffer-size* 80)

(defun make-pop-line-buffer (resource &optional (size *pop-line-buffer-size*))
  (declare (ignore resource))
  (make-array size :element-type http::*standard-character-type* :adjustable t :fill-pointer t))

(defun match-pop-line-buffer-p (resource buffer size)
  (declare (ignore resource))
  (<= size (array-total-size buffer)))

(http::defresource pop-line-buffer (resource &optional (size *pop-line-buffer-size*))
  :constructor make-pop-line-buffer
  :matcher match-pop-line-buffer-p)

(http::define clear-pop-line-buffer-resource ()
  (http::clear-resource 'pop-line-buffer))

(defvar *pop-server-line-buffer* nil)

;;; ===========================================================================
;;; POP3

(defun standard-pop-port ()
  :pop3)

(defparameter *standard-pop-port*
  (standard-pop-port))

;; unused
(defparameter *debug-popper* t
  "Debuging switch to trace the activities of the Pop3 client.")

(defclass pop3-mailbox ()
  ((host :accessor mailbox-host :type (or number string) :initarg :host)
   (port :accessor mailbox-port :type number :initarg :port :initform *standard-pop-port*)
   (user :accessor mailbox-user :type string :initarg :user)
   (user-string :accessor mailbox-user-string :type string :initarg :user-string)
   (password :accessor mailbox-password :type string :initarg :password)
   (password-string :accessor mailbox-password-string :type string :initarg :password-string)
   (stream :accessor mailbox-stream)))

(defun decode-response-line (line)
  (when (stringp line)
    (or (and (>= (length line) 3)
             (string-equal "+OK" line :end1 3 :end2 3)
             :ok)
        (and (>= (length line) 4)
             (string-equal "+ERR" line :end1 4 :end2 4)
             :error))))

(defun decode-stat-response-line (line)
  (when (stringp line)
    (if (and (>= (length line) 3)
             (string-equal "+OK" line :end1 3 :end2 3)
             (= (count #\space line) 2))
      (multiple-value-bind (number pos)
                           (parse-integer line
                                          :start (1+ (position #\space line))
                                          :junk-allowed t)
        (values number (parse-integer line :start (+ 1 pos))))
      (and (>= (length line) 4)
           (string-equal "+ERR" line :end1 4 :end2 4)
           :error))))

; (decode-stat-response-line "+OK 2 4324")
; (decode-response-line "+OK")

; if we knew that the lines we got from the pop server would be of bounded length,
; we could allocate this directly on the stack.  but i don't think we do, so a resource
; is the best we can do --- still zero consing, but slightly slower.
; a much fancier scheme might allocate on the stack, trap if there is an overflow,
; and then use a resource.  but this seems a little heavyweight and an unnecessary
; optimization.  (ajb)

(defun %read-short-response (stream buffer)
  (buffered-telnet-read-line stream buffer)
  (decode-response-line buffer))

(defun %read-short-stat-response (stream buffer)
  (buffered-telnet-read-line stream buffer)
  (decode-stat-response-line buffer))

(defun read-short-response (stream &optional (buffer *pop-server-line-buffer*))
  (cond (buffer
         (setf (fill-pointer buffer) 0)
         (%read-short-response stream buffer))
        (t
         (http::using-resource (buffer pop-line-buffer 40)
           (%read-short-response stream buffer)))))

(defun read-short-stat-response (stream &optional (buffer *pop-server-line-buffer*))
  (cond (buffer
         (setf (fill-pointer buffer) 0)
         (%read-short-stat-response stream buffer))
        (t
         (http::using-resource (buffer pop-line-buffer 40)
           (%read-short-stat-response stream buffer)))))

(defun %read-long-response (stream buffer)
  (buffered-telnet-read-line stream buffer)
  (let ((response (decode-response-line buffer)))
    (if (eq :ok response)
      (values
       response
       (with-output-to-string (out-stream)
         (loop for line = (telnet-read-line stream)
               while (and line (not (equal line ".")))
               do (princ line out-stream)
               do (terpri out-stream))))
      (values response nil))))

(defun read-long-response (stream &optional (buffer *pop-server-line-buffer*))
  (cond (buffer
         (setf (fill-pointer buffer) 0)
         (%read-long-response stream buffer))
        (t
         (http::using-resource (buffer pop-line-buffer 40)
           (%read-long-response stream buffer)))))

; mailbox management

(defparameter *mailbox-table* (make-hash-table :test 'equalp))

(defun intern-mailbox (user &key password host (port *standard-pop-port*))
  (or (gethash user *mailbox-table*)
      (setf (gethash user *mailbox-table*) (make-instance 'pop3-mailbox
                                             :host host
                                             :port port
                                             :user user
                                             :user-string (concatenate 'string "USER " user)
                                             :password password
                                             :password-string (concatenate 'string "PASS " password)))))

(defmethod %open-mailbox ((mailbox pop3-mailbox))
  (with-slots (host port user-string password-string stream) mailbox
    (setf stream (open-tcp-stream host port))
    (when (eq :ok (read-short-response stream))
      (telnet-write-line stream user-string)
      (when (eq :ok (read-short-response stream))
        (telnet-write-line stream password-string)
        (when (eq :ok (read-short-response stream))
          :active)))))

; this could be better. . . (ajb)

(defun open-mailbox (mailbox)
  (unless (%open-mailbox mailbox)
    (close (mailbox-stream mailbox))
    (error "Problem opening mailbox."))
  t)

(defmethod close-mailbox ((mailbox pop3-mailbox))
  (telnet-write-line (mailbox-stream mailbox) "QUIT")
  (close (mailbox-stream mailbox))
  (setf (mailbox-stream mailbox) nil)
  nil)

(defmacro with-mailbox ((mailbox host user password &optional (port *standard-pop-port*))
                        &body body)
  `(let ((,mailbox (intern-mailbox ,user :host ,host :password ,password :port ,port)))
     (http::using-resource (*pop-server-line-buffer* pop-line-buffer 40)
       (unwind-protect (when (open-mailbox ,mailbox)
                         ,@body)
         (close-mailbox ,mailbox)))))

; preconsed command stuff
; basically, this is to prevent the continual consing of command strings. (ajb)

(defun make-new-command-table (&optional (size 1000))
  (make-array size :initial-element nil :adjustable t :fill-pointer t))

(defparameter *growth-constant* 10)

(defun adjusting-aref (array idx)
  (when (>= idx (length array))
    (adjust-array array (+ idx *growth-constant*)))
  (aref array idx))

(defparameter *command-table* (make-hash-table :test 'eq))

(defmacro make-preconsed-command-function (command-keyword string-code)
  `(defmethod obtain-preconsed-command-string ((command (eql ,command-keyword)) index)
     (let ((command-array (gethash ,command-keyword *command-table*)))
       (if command-array
         (or (adjusting-aref command-array index)
             (setf (aref command-array index) ,string-code))
         (progn
           (setf (gethash ,command-keyword *command-table*) (make-new-command-table))
           (obtain-preconsed-command-string ,command-keyword index))))))

(make-preconsed-command-function :top (concatenate 'string "TOP "
(princ-to-string index) " 0"))
(make-preconsed-command-function :dele (concatenate 'string "DELE "
(princ-to-string index)))
(make-preconsed-command-function :list (concatenate 'string "LIST "
(princ-to-string index)))
(make-preconsed-command-function :retr (concatenate 'string "RETR "
(princ-to-string index)))

; pop commands

(defmethod mailbox-statistics ((mailbox pop3-mailbox))
  "Returns the values: number of messages, size"
  (with-slots (stream) mailbox
    (telnet-write-line stream "STAT")
    (read-short-stat-response stream)))

(defmethod mailbox-message-statistics ((mailbox pop3-mailbox) (n integer))
  "Returns the values for a certain message in the mailbox: the number of
the message, its size"
  (with-slots (stream) mailbox
    (telnet-write-line stream (obtain-preconsed-command-string :list n))
    (read-short-stat-response stream)))

(defmethod retrieve-message-top ((mailbox pop3-mailbox) (n integer))
  "Returns the header lines for message n"
  (with-slots (stream) mailbox
    (telnet-write-line stream (obtain-preconsed-command-string :top n))
    (multiple-value-bind (ok? response)
                         (read-long-response stream)
      (values response ok?))))

(defmethod retrieve-all-message-tops ((mailbox pop3-mailbox))
  "Returns the header lines for all messages"
  (loop for i from 1 upto (mailbox-statistics mailbox)
        collect (retrieve-message-top mailbox i)))

(defun parse-response-for-subject-and-from (stream)
  (http::using-resource (buffer pop-line-buffer 40)
    (buffered-telnet-read-line stream buffer)
    (let ((response (decode-response-line buffer)))
      (if (eq :ok response)
        (loop with from
              with subject
              for length = (progn
                             (buffered-telnet-read-line stream buffer)
                             (length buffer))
              while (and (> length 0) (not (equal buffer ".")))
              when (string-equal "From:" buffer :end1 5 :end2 (min 5 length))
              do (setf from (copy-seq buffer))
              when (string-equal "Subject:" buffer :end1 8 :end2 (min 8 length))
              do (setf subject (copy-seq buffer))
              finally (return (values response subject from)))
        (values response nil nil)))))

(defmethod get-subject-and-from ((mailbox pop3-mailbox) (n integer))
  (let ((stream (mailbox-stream mailbox)))
    (telnet-write-line stream (obtain-preconsed-command-string :top n))
    (multiple-value-bind (ok? subject from)
                         (parse-response-for-subject-and-from stream)
      (values subject from ok?))))

;; does the delete-p parameter work?
(defmethod retrieve-message ((mailbox pop3-mailbox) (n integer) &optional (delete-p nil))
  "Returns the nth message. If delete-p is true, it stays marked for delete."
  (with-slots (stream) mailbox
    (telnet-write-line stream (obtain-preconsed-command-string :retr n))
    (multiple-value-bind (ok? response)
                         (read-long-response stream)
      (unless delete-p
        (telnet-write-line stream "RSET")
        (read-short-response stream))
      (values response ok?))))

;; does the delete-p parameter work?
(defmethod retrieve-all-messages ((mailbox pop3-mailbox) &optional delete-p)
  "Returns a list of all messages. If delete-p is true, they stay marked
for delete."
  (loop for i from 1 upto (mailbox-statistics mailbox)
        collect (retrieve-message mailbox i delete-p)))

(defmethod mark-message-for-deletion ((mailbox pop3-mailbox) (n integer))
  "Marks the nth message for deletion."
  (with-slots (stream) mailbox
    (telnet-write-line stream (obtain-preconsed-command-string :dele n))
    (read-short-response stream)))

(defmethod mark-all-messages-for-deletion ((mailbox pop3-mailbox))
  "Marks all messages for deletion."
  (loop for i from 1 upto (mailbox-statistics mailbox)
        collect (mark-message-for-deletion mailbox i)))

;;; ===========================================================================
;;; End of File




