;;; -*- Mode: lisp; Syntax: common-lisp; Package: user; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.16
;;; Reason: Function HTTP::PARSE-RANGE-HEADER:  any syntactically invalid range spec causes the header to be ignored per the spec.
;;; DEFINE-HEADER :RANGE:  update.
;;; Function HTTP::PARSE-REQUEST:  handle URLs larger than URL buffer.
;;; Written by JCMa, 10/05/99 20:35:18
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.6, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.15,
;;; W3 Presentation System 8.0, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.1, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.25, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 41, Experimental HTTP Client 49.0,
;;; Experimental HTTP Client Substrate 3.0, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, White House Publication System 25.36,
;;; WH Automatic Categorization System 15.19, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.11, DEC OSF/1 V4.0 (Rev. 110),
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
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48).

;;; Patch file for CL-HTTP version 70.16
;;; Written by JCMa, 10/07/99 16:14:24
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.6, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.16,
;;; W3 Presentation System 8.0, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.1, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.25, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 41, Experimental HTTP Client 49.0,
;;; Experimental HTTP Client Substrate 3.0, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, White House Publication System 25.36,
;;; WH Automatic Categorization System 15.19, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.11, DEC OSF/1 V4.0 (Rev. 110),
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
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48).


;;; Patch file for CL-HTTP version 70.16
;;; Written by JCMa, 10/07/99 16:07:16
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.6, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.16,
;;; W3 Presentation System 8.0, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.1, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.25, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 41, Experimental HTTP Client 49.0,
;;; Experimental HTTP Client Substrate 3.0, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, White House Publication System 25.36,
;;; WH Automatic Categorization System 15.19, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.11, DEC OSF/1 V4.0 (Rev. 110),
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
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48).


(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;HEADERS.LISP.434"
  "HTTP:SERVER;SERVER.LISP.808")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.434")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;; RFC 2616 says a an invalid range specs requires the whole header to be ignored. 10/5/99 -- JCMa.
(defun parse-range-header (string &optional (start 0) (end (length string)))
  (flet ((parse-range-entry (string s e)
           (let ((pos (char-position #\- string s e))
                 pos2 range-start range-end)
             (cond (pos
                    (setq pos2 (1+ (the fixnum pos)))
		    (handler-case-if (not *debug-server*) 
		       (unless-every
			 ((= pos s)
			  (setq range-start (parse-integer string :start s :end pos :radix 10)))
			 ((= pos2 e)
			  (setq range-end (parse-integer string :start pos2 :end e :radix 10))))
		      (error () (return-from parse-range-header nil)))
		    (when (and range-start range-end)
		      (unless (<= range-start range-end))
		      (return-from parse-range-header nil))
		    (list range-start range-end))
                   (t (return-from parse-range-header nil))))))
    (declare (inline parse-range-entry))
    (with-string-trim-bounds (*white-space-chars* string start end)
      (unless (= start end)
	(loop with pos = (char-position #\= string start end)
	      for s = (1+ (the fixnum pos)) then (1+ (the fixnum e))
	      for e = (or (char-position #\, string s end) end)
	      collect (parse-range-entry string s e) into ranges
	      until (= e end)
	      finally (return (cons (%tokenize-header-keyword string start pos) ranges)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.434")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :range
               (:header :request)
  :print-string "Range"
  :parse-function 'parse-range-header
  :print-function 'print-range-header)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.808")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define parse-request (string start end url-buffer)
  "Parses an HTTP request string."
  (declare (values method url-string http-version-keyword)
           (fixnum end)
           (optimize (speed 3)))
  (let* ((e3 (if (and (not (zerop end)) (white-space-char-p (aref string (1- end))))
                 (%fast-position-if-not white-space-char-p string :start start :end (1- end) :from-end t)
                 end))
         (s1 (and (not (zerop e3)) (char-position #\space string start e3)))
         (s2 (and s1 (%fast-position-if-not white-space-char-p string :start s1 :end e3)))
         (e2 (and s2 (or (%fast-position-if white-space-char-p string :start s2 :end e3) e3)))
         (s3 (and e2 (1+ (the fixnum (or (%fast-position-if white-space-char-p string :start e2 :end e3 :from-end t) e2)))))
         (method (and s1 (%tokenize-header-keyword string 0 s1)))
         (version (and s3 (< s3 e3) (%tokenize-header-keyword string s3 e3)))
	 (url-length (and s2 e2 (- (the fixnum e2) (the fixnum s2))))
	 url-string)
    (when url-length
      ;; Ensure URL fits in buffer
      (when (> (the fixnum url-length) (the fixnum (array-total-size url-buffer)))
	(setq url-buffer (adjust-array url-buffer url-length :fill-pointer 0 :element-type *standard-character-type*)))
      ;; Copy the url into the URL buffer
      (copy-vector-portion string s2 e2  url-buffer 0 url-length)
      (setf (fill-pointer url-buffer) url-length)
      (setq url-string (url:canonicalize-url :http url-buffer 0 url-length t)))
    ;; Ensure that URLs arrive in canonical form.
    (values method url-string version)))

