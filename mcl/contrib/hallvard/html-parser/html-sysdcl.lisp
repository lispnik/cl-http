;;; Last edited by smishra on Fri Jun 28 13:55:06 1996
(defparameter *html-directory* (directory-namestring ccl:*loading-file-source-file*))

(defun html-pathname (pathname)
  (concatenate 'string *html-directory* pathname))

(setf (logical-pathname-translations "html")
      `(("base;*.*"       ,(html-pathname "*.*"))
        ("versions;*.*"   ,(html-pathname "*.*"))
        ("parser;*.*"     ,(html-pathname "*.*"))
        ("**;*.*"         ,(html-pathname "**:*.*"))
        ))

(define-system 
  (html)
  (:compile-load)
  "html:base;packages"
  "html:base;html"
  "html:versions;html-versions"
  "html:versions;html-2-0"
  "html:versions;netscape-2-0"
  "html:parser;html-parser"
 )

