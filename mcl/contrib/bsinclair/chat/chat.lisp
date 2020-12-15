;;;-*- Mode: Lisp; Package: (CHAT) -*-
;;; chat.lisp
;;;
;;; A simple chat facility for CL-HTTP
;;;
;;; To load it, (load "load-chat")
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A chat "topic" is stored in a Fred file
;;; If the file is open, which is the default for
;;; the initial group so that the operator can watch it,
;;; Then the window will be automatically scrolled whenever a new
;;; message comes in.
;;;

(defvar *chat-window* nil)

(defun get-chat-window ()
  (let ((w *chat-window*))
    (if (and w (wptr w))
      w
      (setq *chat-window*
            (make-instance 'fred-window
              :scratch-p t
              :word-wrap-p t
              :window-title "Chat")))))

(get-chat-window)

(defun include-chat-stuff ()
  (let ((in-stream (make-instance 'ccl::buffer-stream
                     :fred-item (current-key-handler (get-chat-window))
                     :start 0)))
    (copy-stream-to-stream in-stream *standard-output*)))

(defvar *email* nil)
(defvar *mode* nil)

(defun email-address (&optional (stream *standard-output*))
  (html:with-paragraph (:stream stream)
    (html:with-rendition (:bold :stream stream)
      (fresh-line stream)
      (write-string "Email: " stream))
    (html:accept-input 'html:string "EMAIL" :default *email* :size 30 :stream stream)))

(defun mode-radio-buttons (&optional (stream *standard-output*))
  (html:with-paragraph (:stream stream)
    (html:accept-input 'html:radio-button "MODE" :choices '(("Text" . "TEXT") ("HTML" . "HTML"))
                       :default (or *mode* "TEXT") :linebreaks nil :stream stream)))

(defun text-input-area (&optional (stream *standard-output*))
  (html:with-paragraph (:stream stream)
    (html:accept-input 'html:multi-line-text "MSG" :rows 10 :columns 72 :stream stream)))

(defun submit-and-reset-buttons (&optional (stream *standard-output*))
  (http:submit-and-reset-buttons stream))

(defun chat-entry-form ()
  (let ((stream *standard-output*)
        (url *url*))
    (html:with-fillout-form (:post url :stream stream)
      (email-address stream)
      (mode-radio-buttons stream)
      (text-input-area stream)
      (submit-and-reset-buttons stream))))

(defun chat-window (url stream)
  (with-standard-response (url stream "Chat")
    (copy-file-through-lisp-evaluator
     url "chat:chat;chat.html" stream
     :package (load-time-value (find-package "CHAT")))))

(defun respond-to-chat-entry (url stream query-alist)
  (declare (dynamic-extent #'clean-up))
  (http:bind-query-values (email mode msg)
                          (url query-alist)
    (setq email (or email "")
          msg (or msg ""))
    (let ((chat-window (get-chat-window)))
      (unless (http:null-string-p msg)
        (without-interrupts
          (let* ((mark (make-mark (fred-buffer chat-window) 0))
                 (fred-stream (make-instance 'ccl::buffer-stream
                                :buffer mark))
                 (filtered-stream (if (equalp mode "HTML")
                                    fred-stream
                                    (make-html-line-break-stream url fred-stream))))
            (write-string "<hr>" fred-stream)
            (fresh-line fred-stream)
            (unless (http:null-string-p email)
              (format fred-stream "<a href=\"mailto:~a\">~a</a> - "
                      email email))
            (http::write-time (get-universal-time) fred-stream)
            (write-string "<br>" fred-stream)
            (fresh-line fred-stream)
            (write-string msg filtered-stream)
            (fresh-line fred-stream)))
        (fred-update chat-window)))
    (let ((*email* email)
          (*mode* mode))
      (chat-window url stream))))

(http:export-url #u"/chat.html"
                 :html-computed-form
                 :form-function 'chat-window
                 :expiration '(:interval 0)
                 :response-function 'respond-to-chat-entry
                 :keywords '(:cl-http :chat)
                 :documentation "Bill St. Clair's Chat system")

(defun about-chat (url stream)
  (with-standard-response (url stream "About Chat")
    (copy-file-through-lisp-evaluator
     url "chat:chat;about-chat.html" stream
     :package (load-time-value (find-package "CHAT")))))  

(http:export-url #u"/about-chat.html"
                 :computed
                 :response-function 'about-chat
                 :expiration `(:interval ,(* 24 60 60))
                 :keywords '(:cl-http :chat)
                 :documentation "About Bill St. Clair's Chat system")
