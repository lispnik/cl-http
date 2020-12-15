;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: ccl -*-

;;; (C) Copyright 1995, John C. Mallery and Christopher R. Vincent
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; AUTO-CONVERTER FROM FILE-NAME EXTENSION TO MAC-FILE-TYPE


;;Want to add output and dialog interface.  MCL dialog abstractions broken because of conflict with 
;;MAC system 7.5.1.  For now, just type (autotype-pathname your-pathname-string) to convert a file or 
;;the entire contents of a directory.  -6/28/95  cvince@ai.mit.edu 

;;Added a few lines of code to invoke dialog.  call autotype-folder with zero args, enter
;;the folder you want to walk, and choose folder.  -6/19/96  cvince@ai.mit.edu

(in-package :ccl) 

(defparameter *file-type-pathname-extension-alist*
   '(((:txt :text)  "TEXT")
      ((:lisp :lsp)  "TEXT" "CCL2")
      ((:ncsa-map)  "TEXT")
      ((:cern-map)  "TEXT")
      ((:CLASS) "clss" "java")
      ((:gif)  "GIFf" "JVWR")
      ((:gzip) "Gzip" "Gzip")
      ((:jpeg :jpg)  "JPEG" "JVWR")
      ((:pict)  "PICT")
      ;; ((:pdf) "PDF" "CARO") ;;losing 11/15/98 -- JCMa.
      ((:tiff :tff)  "TIFF")
      ((:xbm)  "XBM")
      ((:au)  "ULAW")
      ((:mov)  "MooV")
      ((:mpeg :mpg)  "MPEG" "mMPG"))
   "Associates file extensions with (FILE-TYPE FILE-CREATOR)") 

(defun get-file-type-pathname-extension-entry (pathname)
   (flet ((keyword (path)
               (intern (string-upcase (pathname-type path)) *keyword-package*)))
      (declare (inline keyword))
      (let* ((keyword (keyword pathname)))
         (values (assoc keyword *file-type-pathname-extension-alist* :test #'member) keyword))))

(defun set-mac-file-type&creator (pathname &optional stream)
   (let ((entry  (get-file-type-pathname-extension-entry pathname)))
      (when entry
          (destructuring-bind (type &optional creator) (cdr entry ) 
             (when type
                 (set-mac-file-type pathname type))
             (when creator
                 (set-mac-file-creator pathname creator)))
          (when stream
              (format stream "~&Autotype: ~A" pathname))))) 

(defgeneric autotype-pathname (pathname &optional stream)
  (:documentation "Walks the directory structure below PATHNAME and sets pathname types and creators."))

(defmethod autotype-pathname ((pathname pathname) &optional (stream *standard-output*))
   (if (directoryp pathname)
      (mapc #'autotype-pathname (directory (merge-pathnames pathname "*.*") :directories t :files t :resolve-aliases t))
      (set-mac-file-type&creator pathname stream)))

(defmethod autotype-pathname ((pathname string) &optional (stream *standard-output*))
   (autotype-pathname (pathname pathname) stream))

(defun autotype-folder ()
  (let ((returned-path (choose-directory-dialog)))
    (when returned-path (autotype-pathname returned-path))))

(defun standardize-file-pathname-type (pathname &optional stream)
   (multiple-value-bind (entry keyword)
                                   (get-file-type-pathname-extension-entry pathname)
       (when entry
           (unless (eql keyword (caar entry))
              (let* ((new-type (string-downcase (caar entry)))          ; conses
                        (new-path (make-pathname :name (pathname-name pathname) :type new-type :defaults pathname)))
                 (rename-file pathname new-path)
                 (when stream
                     (format stream "~&Standardize: ~A" new-path))))))) 

(defmethod standardize-pathname-type ((pathname pathname) &optional (stream *standard-output*))
   (if (directoryp pathname)
      (mapc #'standardize-pathname-type (directory (merge-pathnames pathname "*.*") :directories t :files t :resolve-aliases t))
      (standardize-file-pathname-type pathname stream)))

(defmethod standardize-pathname-type((pathname string) &optional (stream *standard-output*))
   (standardize-pathname-type (pathname pathname) stream))

