;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: ccl -*-
;;;
;;; Short hack to access the ANSI CL HyperSpec documentation via your favorite
;;; web browser. Kent Pitman (Harlequin Inc.) has converted the ANSI CL
;;; documentation into hypertext.
;;;
;;; File: http:mac;contrib;rjoswig;hyperspec;ansi-doc.lisp
;;;
;;; type "c-x f" while the cursor is on a symbol
;;;
;;; needs the netscape.lisp code by Mike Travers (CL-HTTP Mac example folder)
;;;
;;; puts property :ansi-cl-url on CL symbols
;;;
;;;
;;; Author: Rainer Joswig, joswig@lavielle.com
;;;
;;;
;;; Copyright, 1996, All rights reserved.
;;;
;;; Thanks to Mike Travers, Kent Pitman and Digitool.

;;; Instructions:
;;;
;;; You'll need to load the file netscape.lisp .
;;; Get the ANSI CL HyperSpec.
;;; See: http://www.harlequin.com/books/HyperSpec/FrontMatter/index.html
;;; For conditions see:
;;; http://www.harlequin.com/books/HyperSpec/FrontMatter/About-HyperSpec.html#Legal
;;; The complete ANSI CL HyperSpec can be got from:
;;; http://www.harlequin.com/books/HyperSpec/HyperSpec-3-0.tar.gz
;;; You may need tools like Stuffit Expander 4.0 and/or SunTar
;;; to unpack this file.
;;; Change the parameters *USER-ANSI-CL-URL-BASE* and *SYMBOL->URL-FILE* below.
;;; Load this file.
;;;
;;; Some functionality is accessible by keys:
;;;
;;; - You then can locate the ANSI documentation for a current symbol when
;;;   you have a Netscape web browser running, by pressing "c-x f".
;;; - You can insert a link to your own documentation for the current symbol
;;;   by pressing "c-x c-f".
;;; - You can insert a link to Harlequin's documentation for the current symbol
;;;   by pressing "c-x c-m-f".
;;;
;;; Hints:
;;;
;;; You may change the keybindings. See below.
;;; You can use a different web browser by changing *netscape-creator* in
;;; the file netscape.lisp.
;;; Use  :|MSIE| for Microsoft's Internet Explorer.
;;; 

;;; Changes:
;;; Add parameter *USER-ANSI-CL-URL-BASE*. RJ, 30. June 1996

(in-package "CCL")

(export '(*ANSI-CL-URL-BASE*
          *USER-ANSI-CL-URL-BASE*
          *SYMBOL->URL-FILE*
          GET-ANSI-CL-URL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Change these two parameters to suit your needs:

(defparameter *ansi-cl-url-base*
  "http://www.harlequin.com/books/HyperSpec/Body/"
  "The URL where the Harlequin's ANSI CL HyperSpec body resides.
To customize this you should change the variable *USER-ANSI-CL-URL-BASE* .")

(defparameter *user-ansi-cl-url-base* *ansi-cl-url-base*
  "The URL where the ANSI CL HyperSpec body files reside. This can
be on your local machine or on the network. Default is
the one at http://www.harlequin.com/books/HyperSpec/Body/,
but you can override this.")

(defparameter *symbol->url-file*
  "ccl:HyperSpec-3-0;HyperSpec;Data;Symbol-Table.text"
  "This file is from the HyperSpec 3.0 package. It defines the mappings
between ANSI Common Lisp symbols and the URLs for the documentation.
You need a local copy for this file.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; The code

(defun read-symbol->url-file (file)
  "Read the mappings from ANSI Common Lisp symbols to URLs."
  (let ((cl-package (find-package "CL")))
    (with-open-file (in file :direction :input)
      (loop for symbol-name = (read-line in nil nil)
            for url = (read-line in nil nil)
            while (and symbol-name
                       url
                       (plusp (length symbol-name))
                       (plusp (length url)))
            do (multiple-value-bind (symbol status)
                                    (find-symbol symbol-name cl-package)
                 (when status
                   (setf (get symbol :ansi-cl-url)
                         (subseq url 8)  ; an ugly constant
                         )))))))

;;; (read-symbol->url-file *symbol->url-file*)


(defmethod ed-get-ansi-cl-documentation ((w ccl:fred-mixin))
  "Show the ANSI Common Lisp documentation for the current symbol in the
fred editor."
  (multiple-value-bind (sym endp) (ccl:ed-current-sexp w)
    (if (null endp)
      (ccl::edit-anything-dialog :documentation)
      (progn
        (when (consp sym) (setq sym (car sym)))
        (if (symbolp sym)
          (show-ansi-cl-documentation sym)      
          (ccl:ed-beep))))))


(defun show-ansi-cl-documentation (sym)
  "Show the ANSI Common Lisp documentation for the symbol."
  (let ((url (get-ansi-cl-url sym)))
    (if url
      (ccl::open-url url)
      (ccl:ed-beep))))

(defun get-ansi-cl-url (sym &optional (base *user-ansi-cl-url-base*))
  "Return the URL for the ANSI Common Lisp documentation for the symbol."
  (let ((url (get sym :ansi-cl-url)))
    (when url
      (concatenate 'string base url))))


(defmethod ed-insert-url-for-current-symbol ((view fred-mixin) &optional (base *user-ansi-cl-url-base*))
  "Insert ANSI CL spec URL for current symbol."
  (let ((buffer (ccl:fred-buffer view))
        (*package* (or (ccl:fred-package view) *package*)))
    (multiple-value-bind (start end)
                         (ccl:buffer-current-sexp-bounds buffer)  ; well, it works
      (when (and start end)
        (multiple-value-bind (sym endp)
                             (ccl:ed-current-sexp view)
          (if (or (null endp) (not (symbolp sym)))
            (ccl:ed-beep)
            (let ((url (get-ansi-cl-url sym base)))
              (if url
                (progn
                  (ccl:collapse-selection view t)
                  (ccl:ed-replace-with-undo view start end
                                            (format nil "<a href=\"~a\">~a</a>" url sym)))
                (ccl:ed-beep)))))))))

(defmethod ed-insert-ansi-url-for-current-symbol ((view fred-mixin))
  "Insert Harlequin's ANSI CL spec URL for current symbol."
  (ed-insert-url-for-current-symbol view *ansi-cl-url-base*))


;;; Read the mappings, using a separate thread.
(eval-when (:load-toplevel :execute)
  (ccl:process-run-function "Read ANSI CL Symbol URLs"
                            #'read-symbol->url-file
                            *symbol->url-file*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; you might want to choose a different key binding

(ccl:comtab-set-key ccl:*control-x-comtab*
                    '(#\f)
                    'ed-get-ansi-cl-documentation
                    "Locate the ANSI Common Lisp documentation for the current symbol,
using a running web browser.")

(ccl:comtab-set-key ccl:*control-x-comtab*
                    '(:control #\f)
                    'ed-insert-url-for-current-symbol
                    "Insert ANSI CL spec URL for current symbol.")

(ccl:comtab-set-key ccl:*control-x-comtab*
                    '(:control :meta #\f)
                    'ed-insert-ansi-url-for-current-symbol
                    "Insert Harlequin's ANSI CL spec URL for current symbol.")


#+cl-http-menu
(cl-http-menu:add-item-to-open-url
 "CL: ANSI CL HyperSpec"
 (concatenate 'string
              (subseq *user-ansi-cl-url-base*
                      0
                      (- (length *user-ansi-cl-url-base*)
                         5))
              "FrontMatter/index.html")
 "The Home of the ANSI Common Lisp HyperSpec."
 :update-menu-item t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of File - Have Fun
