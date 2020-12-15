;;; -*- Syntax: ansi-common-lisp; Base: 10; Package: http; Mode: LISP -*-

;;;------------------------------------------------------------------- 
;;;
;;; CMUCL HTTP CLIENT NETWORK INTERFACE
;;;

(in-package :http)

;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

;; this is the function used by the portable client code.
(defun %get-user-name+pw (url-string &optional (stream *standard-output*))
  (declare (ignore stream)
	   (values string string))
  (format t "~%~% Enter Password Information for URL ~s:~%" url-string)
  (format t "User Id: ")
  (let ((user-name (read-line)))
    (format t "Password: ")
    (let ((password (read-line)))
      (values user-name password))))
