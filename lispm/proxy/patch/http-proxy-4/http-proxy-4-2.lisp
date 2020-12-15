;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for HTTP-PROXY version 4.2
;;; Reason: Function (CLOS:METHOD HTTP::COMPUTE-VIA-HEADER (NULL)):  handle null headers case.
;;; Function (CLOS:METHOD HTTP::INVOKE-PROXY-SERVICE (HTTP::PROXY-SERVER-MIXIN URL:FTP-PATHNAME (EQL :GET) T)):  compute via correctly.
;;; Function (CLOS:METHOD HTTP::INVOKE-PROXY-SERVICE (HTTP::PROXY-SERVER-MIXIN URL:FTP-DIRECTORY (EQL :GET) T)):  ditto.
;;; Written by JCMa, 11/18/99 00:23:12
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
;;; Image Substrate 440.4, Ivory Revision 5, VLM Debugger 329, Genera program 8.11,
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
;;; Popup patch (from W:>Reti>popup-patch.lisp.1).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:PROXY;PROXY.LISP.26")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.26")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod compute-via-header ((header-plist null))
  (%compute-via-header nil))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.26")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

(defmethod invoke-proxy-service ((server proxy-server-mixin) (url url:ftp-pathname) (method (eql :get)) request-http-version)
  request-http-version
  (let* ((url (server-url server))
	 (pathname (url::ftp-url-pathname url))
	 (type (pathname-primary-extension pathname nil))
	 (copy-mode (or (url::%content-type-copy-mode type nil) :binary))
	 (element-type (ecase copy-mode
			 (:text 'character)
			 ((:binary :crlf) '(unsigned-byte 8))))
         (stream (server-stream server))
	 (proxy-response-headers `(:via ,(compute-via-header nil))))
    (declare (dynamic-extent proxy-response-headers))
    (multiple-value-bind (user-id pw)
	(url:user-id-and-password url)
      (setf (server-close-connection-p server) t)	;close connection for time being
      (with-successful-response (stream (case type ((:unknown nil) :text) (t type)) :location url :additional-headers proxy-response-headers)
	(case copy-mode
	  (:text
	    (with-text-stream (stream :output)
	      (www-utils:ftp-copy-file pathname stream :element-type element-type
				       :user-id (or user-id "anonymous")
				       :user-pw (or pw (www-utils::user-mail-address)))))
	  ((:binary :crlf)
	   (with-binary-stream (stream :output)
	     (www-utils:ftp-copy-file pathname stream :element-type element-type
				      :user-id (or user-id "anonymous")
				      :user-pw (or pw (www-utils::user-mail-address))))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:PROXY;PROXY.LISP.26")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-")

;;;------------------------------------------------------------------- 
;;;
;;; FTP PROXY METHODS
;;;

(defmethod invoke-proxy-service ((server proxy-server-mixin) (url url:ftp-directory) (method (eql :get)) request-http-version
				 &aux directory-listing)
  request-http-version
  (labels ((ftp-directory-info (url)
	     (multiple-value-bind (user-id pw)
		 (url:user-id-and-password url)
	       (www-utils::ftp-directory-info
		 (url::ftp-url-pathname url)
		 (or user-id "anonymous")
		 (or pw (www-utils::user-mail-address)))))
	   (write-item (path plist stream)
	     (destructuring-bind (&key length-in-bytes creation-date directory
				       #+cl-http-file-author author &allow-other-keys)
		 plist
	       (let ((ftp-url (pathname-ftp-url-string path directory))
		     (name (pathname-name path))
		     (type (pathname-type path))
		     (version (pathname-version path)))
		 (html:with-table-row (:stream stream)
		   (html:with-table-cell (:stream stream)
		     (html:with-rendition (:bold :stream stream)
		       (html:with-anchor-noted (:reference ftp-url :stream stream)
			 (fast-format stream "~A" name)
			 (when (and type (not (eql type :unspecific)))
			   (fast-format stream ".~A" type)
			   (when (and version (integerp version))
			     (fast-format stream ".~D" version))))))
		   (if creation-date
		       (html:with-table-cell (:horizontal-alignment :right :stream stream)
			 (write-standard-time creation-date stream))
		       (html:with-table-cell (:horizontal-alignment :center :stream stream)
			 (write-string "--" stream)))
		   (cond ((www-utils:pathname-directory-p path)
			  (html:with-table-cell (:horizontal-alignment :right :stream stream)))
			 (length-in-bytes
			  (html:with-table-cell (:horizontal-alignment :right :stream stream)
			    (write length-in-bytes :stream stream :escape nil :base 10.)))
			 (t (html:with-table-cell (:horizontal-alignment :center :stream stream)
			      (write-string "--" stream))))
		   #+cl-http-file-author
		   (if author
		       (html:with-table-cell (:horizontal-alignment :right :stream stream)
			 (write-string author stream))
		       (html:with-table-cell (:horizontal-alignment :center :stream stream))))))))
    (declare (inline directory-info))
    (cond ((setq directory-listing (ftp-directory-info url))
	   (let* ((title (relative-name-string url))
		  (stream (server-stream server))
		  (proxy-response-headers `(:via ,(compute-via-header nil))))
	     (declare (dynamic-extent title proxy-response-headers))
	     (with-successful-response (stream :html :status :success  :location url :additional-headers proxy-response-headers)
	       (html:with-html-document (:declare-dtd-version-p t :stream stream)
		 (html:with-document-preamble (:stream stream)
		   (html:declare-title title :stream stream))
		 (html:with-standard-document-body (:stream stream)
		   (html:with-section-heading (title :stream stream)
		     (html:horizontal-line  :stream stream)
		     (html:with-table (:cell-spacing 4 :stream stream)
		       (html:with-table-row (:stream stream)
			 (html:with-table-cell (:header-p t :horizontal-alignment :center :stream stream)
			   (write-string "URL" stream))
			 (html:with-table-cell (:header-p t :horizontal-alignment :center :stream stream)
			   (write-string "Creation Date" stream))
			 (html:with-table-cell (:header-p t :horizontal-alignment :center :stream stream)
			   (write-string "Bytes" stream))
			 #+cl-http-file-author
			 (html:with-table-cell (:header-p t :horizontal-alignment :center :stream stream)
			   (write-string "Author" stream)))
		       (loop for (path . plist) in directory-listing
			     for translated = (translated-pathname path)
			     do (write-item translated plist stream)))
		     (html:horizontal-line :stream stream)
		     (cl-http-signature stream)))))))
	  (t (error 'document-not-found :url url :method :get)))))

