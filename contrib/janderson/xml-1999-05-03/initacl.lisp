
;; init.lisp



(defun |read-mac-comment|
  (stream char &optional (eol '(#\newline #\return #\linefeed)))
  (loop (setf char (read-char stream nil nil))
        (unless char (return))
		(when (find char eol) (return)))
  (values))

(set-syntax-from-char #\return #\space *readtable*)
(set-macro-character #\; #'|read-mac-comment| nil *readtable*)
(set-macro-character #\; #'|read-mac-comment| nil)

