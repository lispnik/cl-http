;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.18
;;; Reason: Function HTTP::%MERGE-URL:  Handle the case where defaults ends in trailing / and url is a name rather than a relative URL.
;;; Written by JCMa, 10/27/99 19:06:43
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.7, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.17,
;;; W3 Presentation System 8.0, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 41, HTTP Client 49.1,
;;; HTTP Client Substrate 3.0, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, White House Publication System 25.36,
;;; WH Automatic Categorization System 15.19, HTTP Proxy Server 4.0,
;;; W4 Constraint-Guide Web Walker 41.0, W4 Examples 13.0, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.11, DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x994 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189585,
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.24),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;UTILS.LISP.422")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.422")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

;; Implement a real merge algorithm sometime really soon.
;(define merge-url (url &optional (defaults *local-context*))
;  (check-type defaults string)
;  ;; preparse for paren syntax
;  (etypecase url
;    (string)
;    (cons
;      (destructuring-bind (url-string &key host port) url
;        (let ((vh-local-context (virtual-host-local-context host (or port *standard-http-port*))))
;          ;; look for virtual hosts to reduce consing
;          (cond (vh-local-context
;                 (setq defaults vh-local-context
;                       url url-string))
;                (host
;                 (setq defaults (if (or port (setq port (url::get-port-info defaults 7 (length defaults))))
;                                    (concatenate 'string "http://" host ":" (write-to-string port :base 10.))
;                                    (concatenate 'string "http://" host))
;                       url url-string))
;                (port
;                 (setq url (concatenate 'string ":" (write-to-string port :base 10.) 
;                                        (if (eql (aref url-string 0) #\:)
;                                            (subseq url-string (or (char-position #\/ url-string 0 (length url-string))
;                                                                   (error "No directory delimiter in ~S." url-string)))
;                                            url-string))))
;                (t (setq url url-string)))))))
;  (with-fast-array-references ((string url string))
;    (let ((l (length string))
;          ch)
;      (declare (fixnum l))
;      (flet ((merge-port (string defaults)
;               (let ((pos (char-position #\: defaults 7 (length defaults) t)))
;                 (concatenate 'string (if pos (subseq defaults 0 pos) defaults) string))))
;        (declare (inline merge-port))
;        (cond ;; no url provided
;          ((zerop l) (return-from merge-url defaults))
;          ;; default the pathname 
;          ((eql (setq ch (aref string 0)) #\/)
;           (return-from merge-url (concatenate 'string defaults url)))
;          ((eql ch #\:)
;           (loop for idx upfrom 1 to (the fixnum (1- l))
;                 while (digit-char-p (aref string idx))
;                 finally (return-from merge-url
;                           (if (< 1 idx)
;                               (merge-port string defaults)
;                               (concatenate 'string defaults (subseq string 1 l))))))
;          ;;
;          ((loop for idx upfrom 0 to (the fixnum (1- l))
;                 for char = (aref string idx)
;                 do (cond ((member char '(#\/ #\?) :test #'eql)
;                           (return nil))
;                          ((and (eql char #\:)  ;found the scheme, ergo fully specified
;; Removed per PCH bug report because it prevents merge url from working for
;; other url schemes, e.g. mailto   7/4/96 -- JCMa.
;;                                (< 2 (the fixnum (- l idx)))
;;                                (eql (aref string (the fixnum (1+ idx))) #\/)
;;                                (eql (aref string (the fixnum (+ 2 idx))) #\/)
;                                )
;                           (return-from merge-url url)))
;                 finally (return nil)))
;          ;; url name
;          (t (concatenate 'string defaults "/" url)))))))

(defun %merge-url (url defaults &optional destructive-p)
  (check-type defaults string)
  (let ((l (length url))
	(buffer (and destructive-p
		     (array-has-fill-pointer-p url)
		     (adjustable-array-p url)
		     url))
	ch)
    (declare (fixnum l))
    (flet ((merge-port (string defaults)
	     (let* ((pos (char-position #\: defaults 7 (length defaults) t))
		    (context-less-port (if pos (subseq defaults 0 pos) defaults)))
	       (declare (dynamic-extent context-less-port))
	       (string-concatenate buffer context-less-port string))))
      (declare (inline merge-port))
      (with-fast-array-references ((string url string))
	(cond ;; no url provided
	  ((zerop l) (return-from %merge-url defaults))
	  ;; default the pathname 
	  ((eql (setq ch (aref string 0)) #\/)
	   (return-from %merge-url (string-concatenate buffer defaults url)))
	  ;; merge the port number
	  ((eql ch #\:)
	   (loop for idx upfrom 1 to (the fixnum (1- l))
		 while (digit-char-p (aref string idx))
		 finally (return-from %merge-url
			   (if (< 1 idx)
			       (merge-port string defaults)
			       (string-concatenate buffer defaults (subseq string 1 l))))))
	  ;; check for url scheme
	  ((loop for idx upfrom 0 to (the fixnum (1- l))
		 for char = (aref string idx)
		 do (case char
		      (#\: (return-from %merge-url url))	;found the scheme, ergo fully specified
		      ((#\/ #\?) (return nil)))
		 finally (return nil)))
	  ;; url name and defaults ends in slash
	  ((eql #\/ (aref defaults (1- (the fixnum (length defaults)))))
	   (string-concatenate buffer defaults url))
	  ;; url name and defaults does not end in slash
	  (t (string-concatenate buffer defaults "/" url)))))))

