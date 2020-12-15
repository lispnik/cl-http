;;;

(in-package :cl-user)

(pushnew :html-parser *features*)

;; Load PCL if necessary.
#-pcl
(progn
  (load "target:pcl/pclload")
  #+gencgc (gc :full t)
  #-gencgc (purify))

#+pcl
(eval-when (compile load eval)
  (pushnew 'compile pcl::*defmethod-times*)
  (pushnew 'compile pcl::*defclass-times*)
  (pushnew 'values  pcl::*non-variable-declarations*)
  (pushnew 'compile pcl::*defgeneric-times*))

#+cmu17
(setf c:*suppress-values-declaration* t)

(defvar *http-directory*
  ;; The default; compiling from within the cl-http/ directory.
  (ext:default-directory)
  ;; Can override the above.
  #+nil "/usr/dtc2/cmucl/cl-http/")

(unless (ignore-errors (logical-pathname-translations "HTTP"))
  (setf (logical-pathname-translations "HTTP")
    `(("**;*.*.*" ,(namestring
		    (merge-pathnames "**/*.*" *http-directory*))))))

;;; Compile and load the defsystem package.
(unless (find-package :mk)
  (let ((obj (make-pathname
	      :defaults (translate-logical-pathname "HTTP:cmucl;defsystem")
	      :type (c:backend-byte-fasl-file-type c:*backend*))))
    (when (< (or (file-write-date obj) 0)
	     (file-write-date "HTTP:cmucl;defsystem.lisp"))
      (compile-file "HTTP:cmucl;defsystem" :byte-compile t))
    (load (translate-logical-pathname "HTTP:cmucl;defsystem"))))


;;;------------------------------------------------------------------- 
;;;
;;; LispWorks system definition
;;;

(unless (ignore-errors (logical-pathname-translations "HTML-PARSER"))
  (setf (logical-pathname-translations "HTML-PARSER")
	`(("*.*.*" ,(namestring (merge-pathnames "html-parser/v8/*.*.*"
						 *http-directory*))))))

(defsystem html-parser
  :source-pathname "HTML-PARSER:"
  :components
  ("packages"
   #-CL-HTTP "tokenizer"
   #-CL-HTTP "plist"
   "defs"
   "patmatch"
   "rewrite-engine"
   "rewrite-rules"
   "html-tags"
   "html-reader"
   "html-parser"
   "html-utilities"))

(compile-system 'html-parser)
(load-system 'html-parser
	     :compile-during-load ()
	     :load-source-if-no-binary t
	     :bother-user-if-no-binary ())

;;;-------------------------------------------------------------------
;;;
;;; Default Initialization
;;;

(html-parser:initialize-parser)
