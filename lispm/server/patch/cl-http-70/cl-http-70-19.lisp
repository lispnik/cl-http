;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.19
;;; Reason: Function (CLOS:METHOD HTTP::DATA-UNIVERSE-START-REVALIDATOR (HTTP::DATA-UNIVERSE-REVALIDATION-MIXIN)):  fix :priority keyword.
;;; Function HTTP::%SERVER-WRITE-COMMON-LOGFILE-ENTRY:  use server-request-time and improve performance.
;;; Function (CLOS:METHOD HTTP:WRITE-ACCESS-LOG-ENTRY (HTTP::COMMON-FILE-FORMAT-MIXIN HTTP::SERVER-LOGGING-MIXIN T T)):  update.
;;; Function (CLOS:METHOD HTTP:WRITE-COMMON-LOGFILE-ENTRY (HTTP::SERVER-LOGGING-MIXIN)):  update.
;;; Function HTTP::%SERVER-WRITE-EXTENDED-COMMON-LOGFILE-ENTRY:  performance.
;;; Function (CLOS:METHOD HTTP:WRITE-ACCESS-LOG-ENTRY (HTTP::EXTENDED-COMMON-FILE-FORMAT-MIXIN HTTP::SERVER-LOGGING-MIXIN T T)):  update.
;;; Function (CLOS:METHOD HTTP::WRITE-EXTENDED-COMMON-LOGFILE-ENTRY (HTTP::SERVER-LOGGING-MIXIN)):  update.
;;; Written by JCMa, 10/29/99 21:45:08
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.7, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.18,
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
  "HTTP:SERVER;DATA-CACHE.LISP.101"
  "HTTP:SERVER;SERVER.LISP.809")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;DATA-CACHE.LISP.101")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: HTTP -*-")

(defmethod data-universe-start-revalidator ((data-universe data-universe-revalidation-mixin))
  (let ((process (data-universe-revalidator-process data-universe)))
    (cond (process
           (process-preset process #'data-universe-revalidator-main-loop data-universe)
           (process-enable process))
          (t (setq process (make-process (data-universe-revalidator-process-name data-universe)
					 :background-p t
					 :priority (data-universe-revalidator-process-priority data-universe)
                                         :restart-after-reset t
                                         :warm-boot-action :delayed-restart))
	     (setf (data-universe-revalidator-process data-universe) process)
             (process-preset process #'data-universe-revalidator-main-loop data-universe)
             (process-enable process)))
    (setf (data-universe-revalidator-run-p data-universe) t)
    process))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.809")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun %server-write-common-logfile-entry (server log-stream gmt-p delimiter)
  (%write-common-logfile-entry (host-log-name server)
			       (server-request server t)
			       (server-request-time server)
			       (server-status server)
			       (server-bytes-transmitted server)	;total bytes (not number of bytes in a document)
			       (%server-user-qualified-name server)
			       gmt-p log-stream delimiter))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.809")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-access-log-entry ((log common-file-format-mixin) (server server-logging-mixin) log-stream gmt-p)
  (%server-write-common-logfile-entry server log-stream gmt-p #\space))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.809")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-common-logfile-entry ((server server-logging-mixin) &optional (log-stream *standard-output*)
                                       (gmt-p *log-times-in-gmt*) (delimiter #\space))
  (with-string-for-null-stream (log-stream)
    (%server-write-common-logfile-entry server log-stream gmt-p delimiter)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.809")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun %server-write-extended-common-logfile-entry (server log-stream gmt-p delimiter)
  (with-header-values (user-agent referer) (server-headers server)
    (%write-extended-common-logfile-entry
      (host-log-name server)
      (server-request server t)
      (server-request-time server)
      (server-status server)
      (server-bytes-transmitted server)
      (%server-user-qualified-name server)
      user-agent 
      referer
      gmt-p log-stream delimiter)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.809")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-access-log-entry ((log extended-common-file-format-mixin) (server server-logging-mixin) log-stream gmt-p)
  (%server-write-extended-common-logfile-entry server log-stream gmt-p #\tab))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.809")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod write-extended-common-logfile-entry ((server server-logging-mixin) &optional (log-stream *standard-output*)
                                                (gmt-p *log-times-in-gmt*) (delimiter #\space))
  (with-string-for-null-stream (log-stream)
    (%server-write-extended-common-logfile-entry server log-stream gmt-p delimiter)))

