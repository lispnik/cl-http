;;;-*- Mode: Lisp; Package: (CHAT) -*-
;;;
;;; load-chat.lisp
;;;
;;; Load the Chat system

(defpackage "CHAT")

(in-package "CHAT")

(provide "CHAT")

(setf (logical-pathname-translations "chat")
      (let ((path (or *load-pathname* *loading-file-source-file*)))
        (if path
          (let* ((dest-dir (make-pathname :device    (pathname-device path)
                                          :host      (pathname-host path)
                                          :directory (append
                                                      (or (pathname-directory path)
                                                          '(:absolute))
                                                      '(:wild-inferiors))
                                          :name      :wild
                                          :type      :wild))
                 (physical-dir (translate-logical-pathname dest-dir)))
            ; This is what you'll get if you load this file
            ; or evaluate this form from this buffer.
            `(("chat;**;*.*" ,physical-dir)
              ("**;*.*" ,physical-dir)))
          ; This is what you'll get if you evalute this form
          ; from the listener.
          '(("chat;**;*.*" "http::chat;**;*.*")))))

(defvar *chat-files*
  '("filtered-streams"
    "chat"))

(defun load-chat ()
  (dolist (file *chat-files*)
    (compile-load (merge-pathnames file "chat:chat;")
                  :verbose t)))

(load-chat)
