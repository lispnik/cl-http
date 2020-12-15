;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for CL-HTTP version 70.6
;;; Reason: Function HTTP:DEBUG-SERVER:  update documentation and support :conditions.
;;; Variable HTTP:*DEBUG-SERVER*:  udpate documentation.
;;; Function HTTP::%PROCESS-REQUEST:  respect :conditions value for *debug-server*.
;;; Written by JCMa, 9/16/1999 19:22:05
;;; while running on Publications Y2K Testbed from HOST6:/usr/lib/symbolics/eop-world-pub6-host6-990913.vlod
;;; with Open Genera 2.0, Genera 8.5, Logical Pathnames Translation Files NEWEST,
;;; Lock Simple 437.0, Version Control 405.0, Compare Merge 404.0, CLIM 72.0,
;;; Genera CLIM 72.0, PostScript CLIM 72.0, CLIM Documentation 72.0,
;;; Statice Runtime 466.1, Statice 466.0, Statice Browser 466.0,
;;; Statice Server 466.2, Statice Documentation 426.0, Joshua 237.3,
;;; Joshua Documentation 216.0, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Mailer 438.0, Showable Procedures 36.3,
;;; Binary Tree 34.0, Working LispM Mailer 8.0, Experimental HTTP Server 70.5,
;;; Experimental W3 Presentation System 8.0,
;;; Experimental CL-HTTP Server Interface 53.0,
;;; Experimental Symbolics Common Lisp Compatibility 4.0,
;;; Experimental Comlink Packages 6.0, Experimental Comlink Utilities 10.0,
;;; Experimental COMLINK Cryptography 2.0, Experimental Routing Taxonomy 9.0,
;;; Experimental COMLINK Database 11.13, Experimental Email Servers 12.0,
;;; Experimental Comlink Customized LispM Mailer 7.0,
;;; Experimental Dynamic Forms 14.3, Experimental Communications Linker Server 39.3,
;;; Experimental Lambda Information Retrieval System 22.2,
;;; Experimental Comlink Documentation Utilities 6.0,
;;; Experimental White House Publication System 25.18,
;;; Experimental WH Automatic Categorization System 15.8,
;;; 8-5-Genera-Local-Patches 1.38, 39-COMLINK-Local-Patches 1.11,
;;; Publications-Server-Local-Patches 1.4, Experimental HTTP Client 49.1,
;;; Experimental HTTP Client Substrate 3.0, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.16, DEC OSF/1 V4.0 (Rev. 205),
;;; 1260x932 24-bit TRUE-COLOR X Screen HOST6:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number 1719841853,
;;; Local flavor function patch (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-1.LISP.1),
;;; Get emb file host patch (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-3.LISP.1),
;;; Get mailer home location from namespace (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-4.LISP.1),
;;; Consider internet-domain-name when matching names to file hosts (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-5.LISP.1),
;;; Parse pathname patch (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-6.LISP.1),
;;; Get internal event code patch (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-8.LISP.2),
;;; AutoLogin (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-9.LISP.3),
;;; Generate an error any time there domain system tries to create a bogus host object for the local host. (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-10.LISP.2),
;;; Set Mailer UID variables for current namespace. (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-11.LISP.3),
;;; Provide Switch between EOP and MIT sources. (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-12.LISP.2),
;;; Make FS:USER-HOMEDIR look in the namespace as one strategy. (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-13.LISP.2),
;;; Local uid patch (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-14.LISP.2),
;;; Statice log clear patch (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-15.LISP.3),
;;; Make compiled-function-spec-p of CLOS class symbol return NIL instead of erring (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-16.LISP.2),
;;; Improve mailer host parsing (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-17.LISP.2),
;;; Make native domain name host patch (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-18.LISP.2),
;;; Domain query cname loop patch (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-19.LISP.2),
;;; Increase disk wired wait timeout from 30 to 90 seconds (from DISTRIBUTION|DIS-EMB-HOST:/db/eop.sct/eop/config/mail-server/patches/disk-wait-90-patch.),
;;; Checkpoint command patch (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-23.LISP.2),
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.24),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48),
;;; Increase disk wired wait timeout from 30 to 900 seconds (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-22.LISP.2),
;;; Tcp implementation error intsrumentation patch (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-24.LISP.2),
;;; Increase packet buffers patch (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-25.LISP.3),
;;; Close tcb patch (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-26.LISP.2),
;;; Get output segment patch (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-27.LISP.2),
;;; Expansion buffer hack patch (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-28.LISP.2),
;;; Nfs directory list fast patch (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-29.LISP.2),
;;; Gc report patch (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-31.LISP.2),
;;; Pathname patch (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-32.LISP.2),
;;; Pathname2 patch (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-33.LISP.1),
;;; Fix NFS brain damage. (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-34.LISP.3),
;;; Log patch (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-35.LISP.2),
;;; Bad rid error patch (from EOP:LOCAL-PATCHES;COMLINK;39;39-COMLINK-LOCAL-PATCHES-1-1.LISP.1),
;;; Copy database patch (from EOP:LOCAL-PATCHES;COMLINK;39;39-COMLINK-LOCAL-PATCHES-1-2.LISP.1),
;;; Cml bulk mail patch (from EOP:LOCAL-PATCHES;COMLINK;39;39-COMLINK-LOCAL-PATCHES-1-6.LISP.1),
;;; Encode integer date patch (from EOP:LOCAL-PATCHES;COMLINK;39;39-COMLINK-LOCAL-PATCHES-1-7.LISP.1),
;;; Fix year 199,
;;; from silly browsers (from EOP:LOCAL-PATCHES;COMLINK;39;39-COMLINK-LOCAL-PATCHES-1-8.LISP.1),
;;; Fix wddi obsolete references (from EOP:LOCAL-PATCHES;COMLINK;39;39-COMLINK-LOCAL-PATCHES-1-9.LISP.1),
;;; Ccc sign document enable services (from EOP:LOCAL-PATCHES;COMLINK;39;39-COMLINK-LOCAL-PATCHES-1-10.LISP.1),
;;; End date for wh pub default (from EOP:LOCAL-PATCHES;COMLINK;39;39-COMLINK-LOCAL-PATCHES-1-11.LISP.2),
;;; Redirect to WWW.PUB.WHITEHOUSE.GOV (from EOP:PUB;HTTP;REDIRECT-TO-PRIMARY.LISP.12),
;;; Some holiday favorites for Pete (from EOP:LOCAL-PATCHES;PUBLICATIONS;PUBLICATIONS-SERVER-LOCAL-PATCHES-1-4.LISP.2).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;UTILS.LISP.420"
  "HTTP:SERVER;VARIABLES.LISP.175"
  "HTTP:SERVER;SERVER.LISP.802")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.420")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define debug-server (&optional (debug (not *debug-server*)))
  "Toggles server debugging according to DEBUG.
DEBUG can be:

      T          Turns normal debugging on
      NIL        Turns debugging off
     :CONDITIONS Turns on debugging of unhandled HTTP conditions
     :ERRORS     Turns on normal debugging."
  (setq *debug-server* (ecase debug
                         ((nil t) (not (null debug)))
                         (:errors debug)
                         (:conditions debug))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;VARIABLES.LISP.175")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp;-*-")

(define-parameter *debug-server* nil
                  "Controls handling of internal server errors.
It can take on the values:

                              T          Turns normal debugging on
                              NIL        Turns debugging off
                              :CONDITIONS Turns on debugging of unhandled HTTP conditions
                              :ERRORS     Turns on normal debugging.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.802")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun %process-request (server stream)
  (declare (values persistent-connection-p)
	   (optimize (speed 3)))
  (labels ((preview-condition (condition)
	     (when *debug-server*
               (break (report-string condition)))
	     nil)
	   (handle-http-condition (condition stream server status-code)
             (case *debug-server*
               (:conditions (break (report-string condition))))
	     (setf (server-status server) status-code)
	     (when (close-connection-p condition) 
	       (setf (server-close-connection-p server) t))
	     (report-status condition stream)
	     (throw 'exit-http-server nil))	;closes connection
	   (handle-reportable-condition (condition)
	     (handle-http-condition condition stream server (status-code condition)))
	   (handle-parsing-error (error)
	     (handle-http-condition error stream server 400))
	   (report-error (error)
	     (typecase error
	       ((or http-condition network-error condition))
	       (t (bug-report-error error)))
	     nil))
    (declare (dynamic-extent #'handle-reportable-condition #'handle-parsing-error))
    (handler-bind
      ((error #'preview-condition)		; MCL 4.1 loses when testing CONDITION here. -- JCMa 7/24/1997.
       (reportable-condition #'handle-reportable-condition)
       (url:parsing-error #'handle-parsing-error)
       (error #'report-error))
      (catch 'exit-http-server
	(multiple-value-bind (request eof delimiter)
	    (read-delimited-line stream '(#\Linefeed #\Return) t (%server-request server))
	  delimiter				;ignore
	  ;; http-version not set, but will default via server-http-version
	  (when eof
	    (error 'request-timeout :format-string "Client dropped connection while reading request line."))
	  (setf (%server-request server) request	;capture in case of growth
		(server-request-time server) (get-universal-time))	;set the request time
	  (%execute-request server request stream))))
    ;; epilogue
    (let ((persistent-connection-p (and (server-persistent-connection-p server)	;set by positive predicate
					(not (server-close-connection-p server)))))	;errors may set this
      ;; Don't force output if there is incoming data: pipeline responses 
      (unless (and persistent-connection-p (http-input-data-available-p stream nil))
	(force-output stream))			;force output while deciding what to do next
      (incf (server-requests-completed server))	;count completed requests
      persistent-connection-p)))		;return whether to continue reading requests from the stream

