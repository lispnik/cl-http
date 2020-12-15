;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for W4 version 41.2
;;; Reason: Function (CLOS:METHOD W4::GET-RESOURCE-HEADERS (W4::ACTIVITY URL:HTTP-URL)):  --
;;; Function (CLOS:METHOD W4::GET-RESOURCE-CONTENT (W4::ACTIVITY URL:HTTP-URL)):  -
;;; Written by JCMa, 11/18/99 19:44:48
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
  "HTTP:W4;WALKER.LISP.50")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W4;WALKER.LISP.50")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-")

(defmethod get-resource-headers ((activity activity) (url http-url) &key refetch-p headers (report-stream *report-stream*)
                                 &aux cached-headers)
  (cond ((and (not refetch-p) (setq cached-headers (get-value url :headers)) *cache-url-data*)
         (values cached-headers
                 (get-value url :header-status-code)
                 (get-value url :redirection)
                 t
		 (get-value url :http-version)))
        (t (let ((outgoing-headers `(,@*standard-head-robot-headers* ,@(robot-headers activity) ,.headers)))
             (declare (dynamic-extent outgoing-headers))
             (handler-case-if (not *debug-walker*) 
                (multiple-value-bind (cached-headers status-code redirection http-version)
                    (http:get-url-headers url outgoing-headers report-stream)
                  (setf (get-value url :headers) cached-headers
                        (get-value url :header-status-code) status-code
			(get-value url :http-version) http-version)
                  (when redirection
                    (setf (get-value url :redirection) redirection))
                  (values cached-headers status-code redirection nil http-version))
               (host-not-responding (err)
                                    (record-url-note activity url :error-getting-headers (report-string err))
                                    (values nil 504))
               (http::reportable-condition
                 (cond)
                 (record-url-note activity url :http-condition-getting-headers (report-string cond))
                 (values nil (http::status-code cond)))
	       (url::url-condition
		 (cond)
		 (record-url-note activity url :url-condition-getting-headers (report-string cond))
		 (values nil 400))
               (error (err)
                      (record-url-note activity url :error-getting-headers (report-string err))
                      (values nil 500)))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:W4;WALKER.LISP.50")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-")

(defmethod get-resource-content ((activity activity) (url http-url) &key refetch-p headers (report-stream *report-stream*)
                                 &aux content)
  (declare (values content headers status-code redirection retrieved-from-cache-p))
  (cond
    ((and (not refetch-p) (setq content (get-value url :content)) *cache-url-data*)
     (values content
             (get-value url :headers)
             (get-value url :content-status-code)
             (get-value url :redirection)
             t
	     (get-value url :http-version)))
    (t (let ((outgoing-headers `(,@*standard-get-robot-headers* ,@(robot-headers activity) ,.headers)))
         (declare (dynamic-extent outgoing-headers))
         (handler-case-if (not *debug-walker*) 
            (multiple-value-bind (body headers status-code redirection http-version)
                (http:get-url-headers-and-body url outgoing-headers report-stream)
              (cond ((null status-code)
                     (setf (get-value url :content-status-code) 500)
                     (abort-activity-on-resource))
                    ((< 199 status-code 300)
                     (setf (get-value url :content) body
                           (get-value url :headers) headers
                           (get-value url :content-status-code) status-code
			   (get-value url :http-version) http-version)
                     (when redirection
                       (setf (get-value url :redirection) redirection)))
                    (t (setf (get-value url :content-status-code) status-code)
                       (abort-activity-on-resource)))
              ;; return values
              (values body headers status-code redirection nil http-version))
           (host-not-responding (err)
                                (record-url-note activity url :error-getting-content (report-string err))
                                (values nil nil 504))
           (http::reportable-condition
             (cond)
             (record-url-note activity url :http-condition-getting-content (report-string cond))
             (values nil nil (http::status-code cond)))
	   (url::url-condition
	     (cond)
	     (record-url-note activity url :url-condition-getting-headers (report-string cond))
	     (values nil 400))
           (error (err)
                  (record-url-note activity url :error-getting-content (report-string err))
                  (values nil nil 500)))))))

