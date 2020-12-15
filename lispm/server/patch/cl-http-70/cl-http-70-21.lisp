;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.21
;;; Reason: Function TCP::CRLF-STREAM-COPY-INTO-STRING:  handle empty input buffer windows.
;;; Function URL::%MERGE-RELATIVE-URL:  new.
;;; Function (CLOS:METHOD URL:NAME-STRING (HTTP::SERVER)):  new.
;;; Function HTTP::PARSE-LOCATION-HEADER:  handle relative URLs in location headers.
;;; DEFINE-HEADER :CONTENT-LOCATION:  update.
;;; DEFINE-HEADER :LOCATION:  update.
;;; Written by JCMa, 11/18/99 19:55:33
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.8, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.20,
;;; W3 Presentation System 8.0, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.2, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.4, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Jcma 41, HTTP Proxy Server 4.1,
;;; HTTP Client Substrate 3.1, Color 427.1, Graphics Support 431.0,
;;; Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Color System Documentation 10.0, SGD Book Design 10.0, HTTP Client 49.1,
;;; Image Substrate 440.4, W4 Constraint-Guide Web Walker 41.1, W4 Examples 13.0,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.11,
;;; DEC OSF/1 V4.0 (Rev. 110),
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
;;; Add clos to mx list methods (from W:>Reti>add-clos-to-mx-list-methods.lisp.1),
;;; Its end of line patch (from W:>Reti>its-end-of-line-patch.lisp.3),
;;; Unix inbox from spoofing patch (from W:>Reti>unix-inbox-from-spoofing-patch.lisp.13),
;;; hack to treat namespace as a partial cache of domain (from W:>hes>fixes>partial-namespace-domain.lisp.5),
;;; Popup patch (from W:>Reti>popup-patch.lisp.1),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.7).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.163"
  "HTTP:SERVER;URL.LISP.375"
  "HTTP:SERVER;SERVER.LISP.811"
  "HTTP:SERVER;HEADERS.LISP.439")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;TCP-LISPM-STREAM.LISP.163")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-")

;; This may return a length shorter than nbytes due to ascii translation.  It
;; would be better if this could by pass the CRLF translation and get the
;; right length without having to do it character at a time.  This would
;; require a swicth to control whether LF is retained in input buffers etc...
;; 7/14/96 -- JCMa.
(defun crlf-stream-copy-into-string (stream &optional nbytes (start 0) string &aux size)
  (flet ((make-the-string (size fill-pointer)
           (make-array size :fill-pointer fill-pointer :adjustable t :element-type 'character))
         (adjust-the-string (string size fill-pointer)
           (let ((new-string (adjust-array string size :fill-pointer fill-pointer
                                           :element-type http::*standard-character-type*)))
             #+testing(unless (eq new-string string) (format t "New array in adjustment."))
             new-string))
         (new-size (size)
           (cond ((< size 64000) (* 2 size))
                 (t (truncate (* size 1.2))))))
    (declare (inline make-the-string adjust-the-string new-size))
    (cond (nbytes
           (setq size (+ nbytes start))
           (cond ((null string)
                  (setq string (make-the-string size start)))
                 ((< (array-total-size string) size)
                  (setq string (adjust-array string size :fill-pointer start :element-type 'character))))
           (multiple-value-bind (idx fill-pointer)
               (binary-stream-copy-into-8-bit-array stream nbytes 0 string)
             idx                                ;ignore
             (setf (fill-pointer string) fill-pointer)
             (values string fill-pointer)))
          ;; the size and growth issues are open to experimentation and better
          ;; algorithms that do less work.  7/26/95 -- JCMa.
          (t (cond ((null string)
                    (setq size (+ start 1000)
                          string (make-the-string size start)))
                   (t (setq size (array-total-size string))))
             (loop with fill-pointer = start
                   for s = fill-pointer
                   doing (multiple-value-bind (buffer offset limit)
                             (send stream :read-input-buffer)
                           (cond ((null buffer)
                                  (setf (fill-pointer string) fill-pointer)
                                  (return (values string fill-pointer)))
                                 (t (unless (= limit offset)	D,#TD1PsT[Begin using 006 escapes](1 0 (NIL 0) (NIL :ITALIC NIL) "CPTFONTI");don't copy when no data present0 111/18/99 -- JCMa.
0				      (setq fill-pointer (+ s (- limit offset)))
				      (when (> fill-pointer size)
					(setq string (adjust-the-string string (setq size (new-size fill-pointer)) fill-pointer)))
				      (copy-array-portion buffer offset limit string s fill-pointer)
				      (setf (fill-pointer string) fill-pointer))
                                    (send stream :advance-input-buffer)))))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;URL.LISP.375")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-common-lisp; Package: (URL :use (future-common-lisp) :colon-mode :internal); Base: 10 -*-")

(defun %merge-relative-url (relative-url default-url &optional (start1 0) (end1 (length relative-url)) (start2 0) (end2 (length default-url)) &aux url-pos)
  "Merges RELATIVE-URL against DEFAULT-URL and returns a new fully specified url.
DEFAULT-URL must be fully-specified."
  (declare (values merged-url))
  (cond ((eql (aref relative-url start1) #\/)
	 (multiple-value-bind (s1 e1)
	     (get-host-port-indices default-url start2 end2)
	   s1
	   (setq url-pos e1)))
	(t (let ((last-slash (char-position #\/ default-url start2 end2 t)))
	     (unless last-slash
	       (error 'parsing-error :url-string (subseq default-url start2 end2)))
	     (setq url-pos (1+ last-slash)))))
  (let* ((url-copy-size (- url-pos start2))
	 (merged-size (+ (- end1 start1) url-copy-size))
	 (merged-url (make-array merged-size :element-type http::*standard-character-type*)))
    (copy-vector-portion default-url start2 url-pos merged-url 0 url-copy-size)
    (copy-vector-portion relative-url start1 end1 merged-url url-copy-size merged-size)
    merged-url))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.811")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod url:name-string ((server server) &optional compute-p)
  (url:name-string (server-url server) compute-p))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.439")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

;; If called when (or *client* *server*), relative URLs are resolved against
;; the current url contained by these objects. 11/18/99 -- JCMa.
(defun parse-location-header (string &optional (start 0) (end (length string)))
  (with-string-trim-bounds (*white-space-chars* string start end)
    (with-bad-escaping-resignalled (string :start start :end end :reason "Bad Escaping: Ill-Formed Location Header")
      (multiple-value-bind (nstring unescaped-p new-string-p)
	  (string-unescape-special-chars string start end)	;handle broken Microsoft location headers   4/21/97 -- JCMa.
	unescaped-p				;ignore
        (flet ((relative-object ()
		 (or (and (boundp '*client*) (symbol-value '*client*))	;defined by the client not the server
		     *server*)))
	  (declare (inline relative-object))
	  (let ((relative-object (relative-object)))
	    (handler-case-if relative-object
	       (if new-string-p
		   (url:intern-url nstring :if-does-not-exist :create)
		   (url:intern-url string :start start :end end :if-does-not-exist :create))
	      (url::no-scheme-found ()
				    (url:intern-url (if new-string-p
							(url::%merge-relative-url nstring (name-string relative-object))
							(url::%merge-relative-url string (name-string relative-object) start end))
						    :if-does-not-exist :create)))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.439")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :content-location
               (:header :entity)
  :print-string "Content-Location"
  :parse-function 'parse-location-header
  :print-function 'print-location-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.439")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :location (:header :response)
  :print-string "Location"
  :parse-function 'parse-location-header
  :print-function 'print-location-header)

