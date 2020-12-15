;;;-*- Mode: Lisp; Package: CHAT -*-

; filtered-streams.lisp
;
; Filtered output streams
; Intended primarily for use by cl-http

(in-package "CHAT")

(export '(copy-file-through-lisp-evaluator
          copy-stream-through-lisp-evaluator
          copy-file-to-stream
          copy-stream-to-stream
          make-filtered-output-stream
          make-lisp-evaluator-stream
          make-html-line-break-stream
          include-file
          with-standard-response
          *writer* *writer-arg* *url*))

(defun copy-file-to-stream (file stream)
  (with-open-file (s file)
    (copy-stream-to-stream s stream)))

(defun copy-stream-to-stream (in-stream out-stream)
  (multiple-value-bind (reader reader-arg) (stream-reader in-stream)
    (multiple-value-bind (writer writer-arg) (stream-writer out-stream)
      (let (char)
        (loop
          (unless (setq char (funcall reader reader-arg))
            (return))
          (funcall writer writer-arg char))))))

(defclass filtered-output-stream (output-stream)
  ((destination :type 'output-stream
                :initarg :destination
                :accessor destination)
   (filter-function :type (or symbol function)
                    :initarg :filter-function
                    :accessor filter-function)
   (url :initarg :url
        :initform nil
        :accessor url)
   (destination.filter-function :accessor destination.filter-function)))

(defmethod initialize-instance :after ((stream filtered-output-stream) &key)
  (multiple-value-bind (writer arg) (stream-writer (destination stream))
    (setf (destination.filter-function stream)
          (cons (cons writer arg)
                (filter-function stream)))))

(defmethod (setf destination) :after (value (stream filtered-output-stream))
  (multiple-value-bind (writer arg) (stream-writer value)
    (let* ((writer.arg (car (destination.filter-function stream))))
      (setf (car writer.arg) writer
            (cdr writer.arg) arg))))

(defmethod (setf filter-function) :after (value (stream filtered-output-stream))
  (setf (cdr (destination.filter-function stream)) value))

(defun make-filtered-output-stream (url stream filter-function)
  (make-instance 'filtered-output-stream
    :destination stream
    :url url
    :filter-function filter-function))

(defmethod stream-tyo ((stream filtered-output-stream) char)
  (let* ((d.f (destination.filter-function stream))
         (writer.arg (car d.f)))
    (funcall (cdr d.f) (car writer.arg) (cdr writer.arg) char)))

(defmethod stream-write-byte ((stream filtered-output-stream) char)
  (let* ((d.f (destination.filter-function stream))
         (writer.arg (car d.f)))
    (funcall (cdr d.f) (car writer.arg) (cdr writer.arg) char)))

(defmethod stream-writer ((stream filtered-output-stream))
  (values #'(lambda (d.f char)
              (let ((writer.arg (car d.f)))
                (funcall (cdr d.f) (car writer.arg) (cdr writer.arg) char)))
          (destination.filter-function stream)))

(defvar *writer*)
(defvar *writer-arg*)
(defvar *url*)

; If this stream finds an open square bracket ("["),
; it reads everything up to the close square bracket ("]")
; and evaluates it in an environment where *standard-output* is
; bound to the stream arg and *writer* and *writer-arg* are
; bound to the values returned by (stream-writer stream).
; To pass a "[" in regular HTML or a "]" in lisp code,
; you need to escape it with a slash ("/").
(defun make-lisp-evaluator-stream (url stream &key package)
  (setq package (or (find-package package)
                    (error "~s is not a package" package)))
  (let ((package (or (and package (find-package package))
                     (load-time-value (find-package "FILTERED-STREAMS"))))
        (string (make-array 20
                            :element-type 'character
                            :adjustable t
                            :fill-pointer 0))
        state)
    (multiple-value-bind (writer arg) (stream-writer stream)
      (labels ((get-char (char)
                 (cond ((eql char #\[)
                        (setq state #'accumulate-string))
                       ((eql char #\/)          ; escape
                        (setq state #'escape))
                       (t (funcall writer arg char))))
               (escape (char)
                 (unless (eql char #\[)
                   (funcall writer arg #\/))
                 (funcall writer arg char)
                 (setq state #'get-char))
               (accumulate-string (char)
                 (cond ((eql char #\])
                        (unwind-protect
                          (let ((*standard-output* stream)
                                (*writer* writer)
                                (*writer-arg* arg)
                                (*url* url)
                                (*package* package))
                            (with-input-from-string (s string)
                              (loop
                                (when (stream-eofp s) (return))
                                (eval (read s)))))
                          (setq state #'get-char)
                          (setf (fill-pointer string) 0)))
                       ((eql char #\/)          ; escape
                        (setq state #'escape-accumulate-string))
                       (t (vector-push-extend char string))))
               (escape-accumulate-string (char)
                 (unless (eql char #\])
                   (vector-push-extend #\/ string))
                 (vector-push-extend char string)
                 (setq state #'accumulate-string)))
        (setq state #'get-char)
        (make-filtered-output-stream
         url
         stream 
         #'(lambda (w a char)
             (declare (ignore w a))
             (funcall state char)))))))

(defun copy-file-through-lisp-evaluator (url file stream &key package)
  (copy-file-to-stream file (make-lisp-evaluator-stream url stream :package package)))

(defun copy-stream-through-lisp-evaluator (url from-stream to-stream &key package)
  (copy-stream-to-stream from-stream (make-lisp-evaluator-stream url to-stream :package package)))

(defun include-file (file &optional no-filtering)
  (let* ((stream *standard-output*))
    (if no-filtering
      (copy-file-to-stream file stream)
      (copy-file-through-lisp-evaluator
       *url* file (make-lisp-evaluator-stream *url* stream)))))

; Turn single line feeds into <br>, and double line feeds into <p>
(defun make-html-line-break-stream (url stream)
  (let (state)
    (labels ((get-char (char)
               (if (eq char #\newline)
                 (setq state #'got-newline)
                 (write-char char stream)))
             (got-newline (char)
               (if (eq char #\newline)
                 (progn
                   (write-string "<p>" stream)
                   (write-char #\newline stream))
                 (progn
                   (write-string "<br>" stream)
                   (write-char #\newline stream)
                   (write-char char stream)))
               (setq state #'get-char)))
      (setq state #'get-char)
      (make-filtered-output-stream url stream
                                   #'(lambda (w a c)
                                       (declare (ignore w a))
                                       (funcall state c))))))

(defmacro with-standard-response ((url stream title) &body body)
  (let ((thunk (gensym)))
    `(let ((,thunk #'(lambda () ,@body)))
       (declare (dynamic-extent ,thunk))
       (funcall-with-standard-response ,url ,stream ,title ,thunk))))

(defun funcall-with-standard-response (url stream title thunk)
  (http:with-successful-response (stream :html :expires (url:expiration-universal-time url))
    (html:with-html-document (:stream stream)
      (html:with-document-preamble (:stream stream)
        (html:declare-base-reference url :stream stream)
        (html:declare-title title :stream stream))
      (html:with-document-body (:stream stream)
        (html:with-section-heading (title :stream stream)
          (funcall thunk))))))

#|

(defun test-lisp-evaluator (url stream)
  (with-standard-response (url stream "Test")
    (copy-file-through-lisp-evaluator url "totem:cl-http:test.html" stream)))

(http:export-url #u"/test.html"
                 :computed
                 :response-function 'test-lisp-evaluator
                 :expiration '(:no-expiration-header)
                 :keywords '(:cl-http :demo)
                 :documentation "Test code for the lisp evaluator filter")

|#
