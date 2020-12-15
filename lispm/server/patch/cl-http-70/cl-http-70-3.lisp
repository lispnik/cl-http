;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for CL-HTTP version 70.3
;;; Reason: Function (CLOS:METHOD (CL:SETF HTTP::DATA-UNIVERSE-REVALIDATOR-PROCESS-PRIORITY) (LISP:INTEGER HTTP::DATA-UNIVERSE-REVALIDATION-MIXIN) :AFTER):  -
;;; Function HTTP::%PARSE-EXTENDED-COMMON-LOG-FORMAT-LOG-ENTRY:  new.
;;; Variable HTTP::*LOG-LINE-PARSER-ALIST*:  add :extended-common-log-format.
;;; Variable HTTP::*DEFAULT-LOG-LINE-PARSER*:  new variable.
;;; Function HTTP::PARSE-LOG-FILE:  update documentation.
;;; Function (CLOS:METHOD HTTP::PARSE-LOG-FILE (HTTP::DYNAMIC-LOGGIN-MIXIN T)):  use *default-log-line-parser* as default.
;;; Written by JCMa, 9/08/99 23:55:17
;;; while running on Publications Y2K Testbed from HOST6:/usr/lib/symbolics/eop-world-pub6-host6-990831.vlod
;;; with Open Genera 2.0, Genera 8.5, Logical Pathnames Translation Files NEWEST,
;;; Lock Simple 437.0, Version Control 405.0, Compare Merge 404.0, CLIM 72.0,
;;; Genera CLIM 72.0, PostScript CLIM 72.0, CLIM Documentation 72.0,
;;; Statice Runtime 466.1, Statice 466.0, Statice Browser 466.0,
;;; Statice Server 466.2, Statice Documentation 426.0, Joshua 237.3,
;;; Joshua Documentation 216.0, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Mailer 438.0, Showable Procedures 36.3,
;;; Binary Tree 34.0, Working LispM Mailer 8.0, Experimental HTTP Server 70.2,
;;; Experimental W3 Presentation System 8.0,
;;; Experimental CL-HTTP Server Interface 53.0,
;;; Experimental Symbolics Common Lisp Compatibility 4.0,
;;; Experimental Comlink Packages 6.0, Experimental Comlink Utilities 10.0,
;;; Experimental COMLINK Cryptography 2.0, Experimental Routing Taxonomy 9.0,
;;; Experimental COMLINK Database 11.7, Experimental Email Servers 12.0,
;;; Experimental Comlink Customized LispM Mailer 7.0,
;;; Experimental Dynamic Forms 14.0, Experimental Communications Linker Server 39.2,
;;; Experimental Lambda Information Retrieval System 22.0,
;;; Experimental Comlink Documentation Utilities 6.0,
;;; Experimental White House Publication System 25.5,
;;; Experimental WH Automatic Categorization System 15.5,
;;; 8-5-Genera-Local-Patches 1.0, 39-COMLINK-Local-Patches 1.11,
;;; Publications-Server-Local-Patches 1.4, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.16, DEC OSF/1 V4.0 (Rev. 205),
;;; 1260x932 24-bit TRUE-COLOR X Screen HOST6:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number 1719841853,
;;; Local flavor function patch (from EOP:CONFIG;MAIL-SERVER;PATCHES;LOCAL-FLAVOR-FUNCTION-PATCH-MARK2),
;;; Get emb file host patch (from EOP:CONFIG;MAIL-SERVER;PATCHES;GET-EMB-FILE-HOST-PATCH),
;;; Get mailer home location from namespace (from EOP:CONFIG;MAIL-SERVER;PATCHES;MAILER-ENVIRONMENT-PATCH),
;;; Consider internet-domain-name when matching names to file hosts (from EOP:CONFIG;MAIL-SERVER;PATCHES;PATHNAME-HOST-NAMEP-PATCH.LISP.2),
;;; Parse pathname patch (from EOP:CONFIG;MAIL-SERVER;PATCHES;PARSE-PATHNAME-PATCH),
;;; Get internal event code patch (from EOP:CONFIG;MAIL-SERVER;PATCHES;GET-INTERNAL-EVENT-CODE-PATCH),
;;; AutoLogin (from EOP:CONFIG;MAIL-SERVER;PATCHES;AUTO-LOGIN.LISP.1),
;;; Generate an error any time there domain system tries to create a bogus host object for the local host. (from EOP:CONFIG;MAIL-SERVER;PATCHES;DETECT-BOGUS-HOST.LISP.1),
;;; Set Mailer UID variables for current namespace. (from EOP:CONFIG;MAIL-SERVER;PATCHES;MAILER-UID.LISP.1),
;;; Provide Switch between EOP and MIT sources. (from EOP:CONFIG;MAIL-SERVER;PATCHES;MIT-SOURCE.LISP.1),
;;; Make FS:USER-HOMEDIR look in the namespace as one strategy. (from EOP:CONFIG;MAIL-SERVER;PATCHES;USER-HOMEDIR.LISP.2),
;;; Local uid patch (from EOP:CONFIG;MAIL-SERVER;PATCHES;LOCAL-UID-PATCH),
;;; Statice log clear patch (from EOP:CONFIG;MAIL-SERVER;PATCHES;STATICE-LOG-CLEAR-PATCH),
;;; Make compiled-function-spec-p of CLOS class symbol return NIL instead of erring (from EOP:CONFIG;MAIL-SERVER;PATCHES;COMPILED-FUNCTION-SPEC-P-PATCH.LISP.1),
;;; Improve mailer host parsing (from EOP:CONFIG;MAIL-SERVER;PATCHES;PARSE-MAILER-HOST-PATCH.LISP.1),
;;; Make native domain name host patch (from EOP:CONFIG;MAIL-SERVER;PATCHES;MAKE-NATIVE-DOMAIN-NAME-HOST-PATCH.LISP.1),
;;; Domain query cname loop patch (from EOP:CONFIG;MAIL-SERVER;PATCHES;DOMAIN-QUERY-CNAME-LOOP-PATCH.LISP.1),
;;; Increase disk wired wait timeout from 30 to 90 seconds (from DISTRIBUTION|DIS-EMB-HOST:/db/eop.sct/eop/config/mail-server/patches/disk-wait-90-patch.),
;;; Checkpoint command patch (from EOP:CONFIG;MAIL-SERVER;PATCHES;CHECKPOINT-COMMAND-PATCH.LISP.9),
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.24),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48),
;;; Increase disk wired wait timeout from 30 to 900 seconds (from EOP:CONFIG;MAIL-SERVER;PATCHES;DISK-WAIT-900-PATCH.LISP.1),
;;; Tcp implementation error intsrumentation patch (from EOP:USERS;COMLINK;LOCAL-PATCHES;TCP-IMPLEMENTATION-ERROR-INTSRUMENTATION-PATCH.LISP.NEWEST),
;;; Increase packet buffers patch (from EOP:USERS;COMLINK;LOCAL-PATCHES;INCREASE-PACKET-BUFFERS-PATCH.LISP.NEWEST),
;;; Close tcb patch (from EOP:USERS;COMLINK;LOCAL-PATCHES;CLOSE-TCB-PATCH),
;;; Get output segment patch (from EOP:USERS;COMLINK;LOCAL-PATCHES;GET-OUTPUT-SEGMENT-PATCH.LISP.NEWEST),
;;; Expansion buffer hack patch (from EOP:USERS;COMLINK;LOCAL-PATCHES;EXPANSION-BUFFER-HACK-PATCH.LISP.1),
;;; Nfs directory list fast patch (from EOP:USERS;COMLINK;LOCAL-PATCHES;NFS-DIRECTORY-LIST-FAST-PATCH.LISP.NEWEST),
;;; Gc report patch (from EOP:MAIL-SERVER;PATCHES;GC-REPORT-PATCH.LISP.1),
;;; Pathname patch (from EOP:MAIL-SERVER;PATCHES;PATHNAME-PATCH.LISP.2),
;;; Pathname2 patch (from EOP:MAIL-SERVER;PATCHES;PATHNAME2-PATCH.LISP.3),
;;; Fix NFS brain damage. (from EOP:MAIL-SERVER;PATCHES;NFS-PATCH.LISP.8),
;;; Log patch (from EOP:MAIL-SERVER;PATCHES;LOG-PATCH.LISP.2),
;;; Vlm namespace append patch (from EOP:MAIL-SERVER;PATCHES;VLM-NAMESPACE-APPEND-PATCH.LISP.7),
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
  "HTTP:SERVER;DATA-CACHE.LISP.90"
  "HTTP:SERVER;LOG.LISP.179")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;DATA-CACHE.LISP.90")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: HTTP -*-")

;; Allow the process priority to be adjusted while running
(defmethod (setf data-universe-revalidator-process-priority) :after ((process-priority integer) (data-universe data-universe-revalidation-mixin))
  (let ((process (data-universe-revalidator-process data-universe)))
    (when process
      (setf (process-priority process) process-priority))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.179")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

;; Consider tokenizing BROWSER, REFERER and USER.
(defun %parse-extended-common-log-format-log-entry (line &optional (delimiter #\tab) &aux (end (length line)))
  (declare (values url host method date server-version status bytes user-name browser referer)
           (fixnum l))
  (labels ((field-delimiter-char-p (x)
             (char= x delimiter))
           (white-space-p (x)
             (char= x #\space))
           (parse-exact-request (exact-request start end)
             (declare (fixnum end))
             (unless (eql start end)
               (let* ((r1 (%fast-position-if white-space-p exact-request :start start :end end))
                      (r2 (%fast-position-if white-space-p exact-request :start (1+ r1) :end end))
                      (method (%tokenize-header-keyword exact-request start r1))
                      (url (subseq exact-request (1+ r1) (or r2 end)))
                      (server-version (if r2
                                          (%tokenize-header-keyword exact-request (the fixnum (1+ r2)) end)
                                          :HTTP/0.9)))	; http 0.9 or telnet hackers don't send version
                 (declare (fixnum r1)
                          (type (or null fixnum) r2))
                 (values method url server-version))))
           (parse-field (line start end bounding-delimiter)
             (declare (fixnum start))
             (if (eql #\- (aref line start))
                 nil
                 (let ((s1 (char-position bounding-delimiter line start end))
                       (s2 (char-position bounding-delimiter line start end t)))
                   (when (and s1 s2)
                     (subseq line (1+ (the fixnum s1)) s2))))))
    (declare (inline field-delimiter-char-p parse-exact-request parse-user))
    (let (p1 p2 p3 p4 p5 p6 p7 p8 p9 p10)
      p3
      (cond
        ((and (setq p1 (%fast-position-if field-delimiter-char-p line :start 0 :end end))
              (setq p2 (%fast-position-if field-delimiter-char-p line :start (1+ (the fixnum p1)) :end end))
              (setq p3 (%fast-position-if field-delimiter-char-p line :start (1+ (the fixnum p2)) :end end))
              (setq p4 (char-position #\[ line (1+ (the fixnum p3)) end))
              (setq p5 (char-position #\] line (1+ (the fixnum p4)) end))
              (setq p6 (char-position #\" line (1+ (the fixnum p5)) end))
              (setq p7 (char-position #\" line (1+ (the fixnum p6)) end))
              (< (1+ (the fixnum p6)) p7)       ;empty request string is a bad entry  7/20/95 -- JCMa.
              (setq p8 (%fast-position-if field-delimiter-char-p line :start (+ 2 (the fixnum p7)) :end end))
              (setq p9 (%fast-position-if field-delimiter-char-p line :start (1+ (the fixnum p8)) :end end))
              (setq p10 (%fast-position-if field-delimiter-char-p line :start (1+ (the fixnum p9)) :end end)))
         (locally
           (declare (fixnum p1 p2 p3 p4 p5 p6 p7 p8))
           (multiple-value-bind (method url server-version)
               (parse-exact-request line (1+ p6) p7) 
             (let ((host (subseq line 0 p1))
                   (date (parse-gmt-time line (1+ p4) p5))
                   (status (parse-integer line :start (+ 2 p7) :end p8))
                   (bytes (parse-integer line :start (1+ p8) :end p9))
                   (user (parse-field line (1+ p2) p4 #\"))
                   (browser (parse-field line p9 p10 #\"))
                   (referer (parse-field line p10 end #\")))
               (values url host method date server-version status bytes user browser referer)))))
        (t (values nil nil :bad-record))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.179")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(define-parameter *log-line-parser-alist* '((:ncsa-1-2 . %parse-ncsa-http-1-2-log-entry)
                                            (:common-log-format . %parse-common-log-format-log-entry)
                                            (:extended-common-log-format . %parse-extended-common-log-format-log-entry)
                                            (:post-log-format . %parse-http-post-log-entry)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.179")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(define-parameter *default-log-line-parser* :common-log-format
                  "The default log format for parsing log files.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.179")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defgeneric parse-log-file (log pathname &key log-format stream)
   (:documentation "Parses the log PATHNAME according to LOG-FORMAT.
LOG-FORMAT defaults to the value of *default-log-line-parser*.
Errors are reported on STREAM."))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;LOG.LISP.179")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: LISP -*-")

(defmethod parse-log-file ((log dynamic-loggin-mixin) pathname &key (log-format *default-log-line-parser*) (stream *standard-output*))
  (with-slots (local-host) log
    (with-open-file (file pathname :direction :input)
      (using-resource (line-buffer line-buffer *line-buffer-size*)
	(loop with parser = (log-line-parser log-format)
	      with context = (concatenate 'string "HTTP://" (host-domain-name local-host))
	      for line = (read-delimited-line file '(#\return #\Linefeed) nil line-buffer)
	      while line
	      do (multiple-value-bind (url host method date server-version)
		     (funcall parser line)
		   (cond
		     ((member method '(:get :head :post))
		      (handler-case
			(let ((url-string (merge-url url context)))
			  (declare (dynamic-extent url-string))
			  (log-transaction log
					   (intern-url url-string :if-does-not-exist :create)
					   (intern-host log :domain-name host :object host)
					   method
					   date
					   server-version))
			(parsing-error () (format stream "~&Bad URL Syntax: ~S" line))))
		     (t (format stream "~&Not Parsed by ~A: ~S" log-format line)))))))))

