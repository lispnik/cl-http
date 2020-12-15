;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :html-base; Base: 10 -*-
;;; Last edited by smishra on Sat Jun 29 15:01:00 1996

;;;;
;;;
;;; Copyright 1996, Hallvard Tr¾tteberg
;;; You can use the code if you like, but include this notice.
;;;
;;;;

(in-package :html-base)

;;
;; functions, macros and variables for parsing html documents
;;

(defun make-strings-string (strings &optional (nreverse-p nil))
  (declare (optimize (speed 3) (safety 1)))
  (when nreverse-p
    (setf strings (nreverse strings)))
  (when (and strings (null (cdr strings)) (stringp (car strings)))
    (return-from make-strings-string (car strings)))
  (let ((new-string (make-string (loop for string in strings
                                       summing (if (stringp string) (length string) 1)))))
    (loop for new-length fixnum = 0 then (+ new-length (if (stringp string) (length string) 1))
          for string in strings
          when (stringp string) do
          (replace new-string string :start1 new-length)
          else do
          (setf (schar new-string new-length) string))
    (values new-string)))

(defvar *parse-filter-html* t
  "Type expression telling the parser which markup classes to actually make instances of")

(defvar *parse-filter-strings* t
  "Boolean telling the parser whether to make strings or not")

(defmacro skipping-until ((cursor-var &optional (result-var nil) (stream t) (x-char-var nil))
                          char-var test-form
                          &optional (keep-start-char-p nil) (keep-end-char-p nil)
                          )
  (let* ((char-var (if (eq char-var t) 'char char-var))
         (test-form `(or (eql ,char-var #\&) ,test-form))
         (local-string-var (gensym "STRING"))
         (local-string-form `(the simple-string ,local-string-var))
         (read-line-form `(read-line ,stream nil nil))
         (end-form (when result-var `(setf ,result-var (when *parse-filter-strings* 
                                                         (make-strings-string strings t)))))
         (empty-string-end-form `(setf (cdr ,cursor-var) nil
                                       ,@(when result-var (cdr end-form))))
         (newline-string "
"))
    `(loop for ,local-string-var = (or (cdr ,cursor-var) ,read-line-form)
           then (if pos ,local-string-var
                    (or ,read-line-form (progn ,empty-string-end-form (return))))
           for start-pos fixnum = (+ (or (car ,cursor-var) 0)
                                     ,(cond ((characterp keep-start-char-p)
                                             `(if (eql (schar ,local-string-form (or (car ,cursor-var) 0))
                                                       ,keep-start-char-p)
                                                1 0))
                                            (keep-start-char-p 0)
                                            (t 1)))
           then (or pos 0)
           for pos = (loop for i fixnum from start-pos to (1- (length ,local-string-var))
		           ;; for ,char-var character = (schar ,local-string-form i)
		           for ,char-var = (schar ,local-string-form i)
                           when ,test-form
                           do (progn ,(when x-char-var `(setf ,x-char-var ,char-var))
                                     (return i))
                           finally (return nil))
           ,@(when result-var `(with strings = nil))
           unless ,local-string-var do
           ,empty-string-end-form
           (return)
           ,@(when result-var `(do (push (cond (pos (subseq ,local-string-var start-pos pos))
                                               ((zerop start-pos) ,local-string-var)
                                               (t (subseq ,local-string-var start-pos)))
                                         strings)))
           when pos
           when (eql (schar ,local-string-form pos) #\&) do
           (incf pos)
           (let ((semi-pos (loop for i fixnum from pos to (1- (length ,local-string-var))
                                 when (eql (schar ,local-string-form i) #\;) do
                                 (return i))))
             ,(when result-var
                `(push (if semi-pos
                         (let ((token (subseq ,local-string-var (1- pos)
                                              (if semi-pos (1+ semi-pos) (length ,local-string-var)))))
                           (or (#-:CL-HTTP special-char-for-token
                                #+:CL-HTTP html2:special-char-for-token
                                token)
                               #+:CL-HTTP (code-char (html2:parse-iso-character token))
                             #\&))
                         #\&)
                       strings))
             (setf pos (if semi-pos (1+ semi-pos) pos)))
           else do
           (setf (car ,cursor-var)
                 ,(cond ((characterp keep-end-char-p)
                         `(if (eql (schar ,local-string-form pos) ,keep-end-char-p)
                            pos (1+ pos)))
                        (keep-end-char-p `pos)
                        (t `(1+ pos)))
                 (cdr ,cursor-var) ,local-string-var
                 ,@(cdr end-form))
           (return)
           else do
           (if (let ((,char-var #\Newline)) ,test-form)
             (progn ,(when x-char-var `(setf ,x-char-var #\Newline))
                    (setf (car ,cursor-var) 0
                          (cdr ,cursor-var)
                          ,(cond ((characterp keep-end-char-p)
                                  `(if (eql (schar ,local-string-form pos) #\Newline) ,newline-string ,read-line-form))
                                 (keep-end-char-p newline-string)
                                 (t read-line-form))
                          ,@(cdr end-form))
                    (return))
             ,(when result-var
                `(push #\Newline strings)))
           )
    ))

(declaim (inline whitespace-p))

(defun whitespace-p (char)
  (declare (character char))
  #+mcl     (ccl:whitespacep         char)
  #+allegro (excl::whitespace-char-p char)
  #+lispworks (system:whitespace-char-p char)
  #-(or :mcl :allegro :lispworks) (member char '(#\space #\tab #\newline #\return #\linefeed)))

(declaim (inline name-char-p))
(defun not-name-char-p (char)
  (declare (character char))
  (or (whitespace-p char)
      (eql char #\>) (eql char #\=)))

(defun parse-start-tag (cursor stream &aux name)
  (declare (optimize (speed 3) (safety 1)))
  (let ((*parse-filter-strings* t))
    (skipping-until (cursor name stream) t (not-name-char-p char) t t))
  (let* ((html-class (html-instance-class name))
         (*parse-filter-strings* (subtypep html-class *parse-filter-html*))
         (attrs nil) attr value first-char)
    (loop
      (skipping-until (cursor nil stream first-char) t (not (whitespace-p char)) t t)
      (when (eql first-char #\>)
        (return))
      (skipping-until (cursor attr stream) t (not-name-char-p char) t t)
      (skipping-until (cursor nil stream first-char) t (not (whitespace-p char)) t t)
      (if (eql first-char #\=)
        (progn
          (skipping-until (cursor nil stream first-char) t (not (whitespace-p char)) nil t)
          (if (or (eql first-char #\") (eql first-char #\'))
            (skipping-until (cursor value stream) t (eql char first-char) nil nil)
            (skipping-until (cursor value stream) t
                            (or (whitespace-p char) (eql char #\>)) t t))
          )
        (setf value t))
      
      (push (cons attr value) attrs))

    (when (subtypep html-class *parse-filter-html*)
      (values (make-html name attrs)))
    ))

(defun parse-end-tag (cursor stream &aux name)
  (declare (optimize (speed 3) (safety 1)))
  (let ((*parse-filter-strings* t))
    (skipping-until (cursor name stream) t (not-name-char-p char) #\/ #\>))
  (let ((html-class (html-instance-class name)))
    (cond ((not (subtypep html-class *parse-filter-html*)) nil)
          ((eq html-class 'unknown-html)
           (cons html-class name))
          (t html-class))))

(defun parse-comment (cursor stream &aux comment-text)
  (declare (optimize (speed 3) (safety 1)))
  (skipping-until (cursor comment-text stream) t (eql char #\>) #\! #\>)
  #|
  (loop
    (skipping-until (cursor comment-text stream) t (eql char #\>) #\! #\>)
    (let ((i (car cursor)) (string (cdr cursor)))
      (unless (and cursor i string
                   (or (< i 2)
                       (not (eql (schar (cdr cursor) (- i 1)) #\-))
                       (not (eql (schar (cdr cursor) (- i 2)) #\-))))
        (return))))
  |#
  (let ((comment (make-instance 'html-comment)))
    (setf (html-comment-text comment) comment-text)
    (values comment)))

(defun parse-tag (cursor stream)
  (declare (optimize (speed 3) (safety 1)))
  (case (schar (cdr cursor) (or (car cursor) 0))
    (#\/ (parse-end-tag cursor stream))
    (#\! (parse-comment cursor stream))
    (t (parse-start-tag cursor stream))))

(defun html-parser-handler (html html-context)
  (typecase html
    (null             (values html-context))
    (html-environment (push-html-context html html-context))
    (symbol           (case html
                        ((t) (list (make-html 'html-document nil)))
                        (t (pop-html-context html :code html-context))))
    (cons             (pop-html-context (cdr html) :code html-context))
    (html-clause (add-html-part html html-context))
    (string      (add-html-part html html-context))
    (t           (add-html-part html html-context))))

(defun parse-html-file (file &key (html-filter *parse-filter-html*) (string-filter *parse-filter-strings*)
                             (html-handler #'html-parser-handler)
                             (html-context (when html-handler (funcall html-handler t nil))))
  (declare (optimize (speed 3) (safety 1)))
  (labels ((handle-html (html)
             (when (and html html-handler)
               (setf html-context (funcall html-handler html html-context))))
           (parse-file (stream &aux text (cursor (cons nil nil)))
             (skipping-until (cursor text stream) t (eql char #\<) t nil)
             (loop
               (handle-html text)
               (unless (cdr cursor)
                 (return))
               (handle-html (parse-tag cursor stream))
               (unless (cdr cursor)
                 (return))
               (skipping-until (cursor text stream) t (eql char #\<) #\> nil)
               )
             (values html-context))
           )
    (let ((*parse-filter-html*    html-filter)
          (*parse-filter-strings* string-filter))
      (cond ((streamp file) 
             (parse-file file))
            ((or (stringp file) (pathnamep file))
             (with-open-file (stream (pathname file))
               (parse-file stream)))
            ))
    ))

(defun tracing-tag-handler (html html-context)
  (print html)
  (html-parser-handler html html-context))

#|
(time (setf hd (parse-html-file "ccl:Lisp Projects;HTML;html-test-files;extensions.html"
                                :html-filter 'html-2:anchor
                                :string-filter nil
                                :html-handler #'tracing-tag-handler
                                )))
(write-html (car hd) t t)

(let ((*package* (find-package :netscape-2)))
  (time (setf hd (parse-html-file "ccl:Lisp Projects;HTML;calc.html"
                                  :html-handler #'tracing-tag-handler
                                  ))))
(write-html (car hd) t '(or html-class html-comment) -1)

(time (setf hd (parse-html-file "ccl:Lisp Projects;HTML;Bookmarks.html"
                                :html-filter 'html-2:anchor
                                :string-filter nil
                                )))
(write-html (car hd) t '(or html-class html-comment) -1)
|#

