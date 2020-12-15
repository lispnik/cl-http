;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for HTTP-BASE-CLIENT version 49.1
;;; Reason: Build floggers from tab-delimited extended common log format log files.
;;; 
;;; Function HTTP::MAKE-TRANSACTION-CONTROLLERS-FROM-LOGFILE:  add
;;; log-format and default to *default-log-line-parser*, add status-codes.
;;; Function HTTP::MAKE-TRANSACTION-CONTROLLERS-FROM-POST-LOGFILE:  add
;;; log-format argument and default to :post-log-format, add status-codes
;;; argument.
;;; Remove function HTTP::ALLOCATE-TRANSACTION-CONTROLLERS: undefine.
;;; Function HTTP::ALLOCATE-TRANSACTION-CONTROLLERS:  add args log-format, status-codes.
;;; Function (CLOS:METHOD HTTP::ALLOCATE-TRANSACTION-CONTROLLERS (T T T CONS)):  -
;;; Function (CLOS:METHOD HTTP::ALLOCATE-TRANSACTION-CONTROLLERS (T T (EQL :GET) CONS)):  -
;;; Function (CLOS:METHOD HTTP::ALLOCATE-TRANSACTION-CONTROLLERS (T T (EQL :HEAD) CONS)):  -
;;; Function (CLOS:METHOD HTTP::ALLOCATE-TRANSACTION-CONTROLLERS (T T (EQL :GET) CL:PATHNAME)):  -
;;; Function (CLOS:METHOD HTTP::ALLOCATE-TRANSACTION-CONTROLLERS (T T (EQL :HEAD) CL:PATHNAME)):  -
;;; Function (CLOS:METHOD HTTP::ALLOCATE-TRANSACTION-CONTROLLERS (T T (EQL :POST) CL:PATHNAME)):  -
;;; Function HTTP:ALLOCATE-FLOGGER:  -
;;; Written by JCMa, 9/09/99 00:06:30
;;; while running on Publications Y2K Testbed from HOST6:/usr/lib/symbolics/eop-world-pub6-host6-990831.vlod
;;; with Open Genera 2.0, Genera 8.5, Logical Pathnames Translation Files NEWEST,
;;; Lock Simple 437.0, Version Control 405.0, Compare Merge 404.0, CLIM 72.0,
;;; Genera CLIM 72.0, PostScript CLIM 72.0, CLIM Documentation 72.0,
;;; Statice Runtime 466.1, Statice 466.0, Statice Browser 466.0,
;;; Statice Server 466.2, Statice Documentation 426.0, Joshua 237.3,
;;; Joshua Documentation 216.0, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Mailer 438.0, Showable Procedures 36.3,
;;; Binary Tree 34.0, Working LispM Mailer 8.0, Experimental HTTP Server 70.3,
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
;;; Publications-Server-Local-Patches 1.4, Experimental HTTP Client 49.0,
;;; Experimental HTTP Client Substrate 3.0, Ivory Revision 5, VLM Debugger 329,
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
  "HTTP:CLIENT;FLOGGER.LISP.50")


(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL)
  (SCT:REQUIRE-PATCH-LEVEL-FOR-PATCH '(CL-HTTP 70. 3.)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;FLOGGER.LISP.50")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun make-transaction-controllers-from-logfile (pathname http-method host port &key (log-format *default-log-line-parser*)
                                                           status-codes default-request-headers (stream *standard-output*))
  (with-open-file (file pathname :direction :input)
    (using-resource (line-buffer line-buffer *line-buffer-size*)
      (loop with parser = (log-line-parser log-format)
            with default-context = (%make-context host port)
            for line = (read-delimited-line file '(#\return #\Linefeed) nil line-buffer)
            while line
            for ctlr = (handler-case-if (not *debug-client*)
                          (multiple-value-bind (url host method date server-version status)
                              (funcall parser line)
                            host date server-version	;ignore
                            (when (and (eql method http-method)
                                       (or (null status-codes) (member status status-codes)))
                              (let ((url-string (merge-url url default-context)))
                                (handler-case 
                                  (make-instance 'transaction-control
                                                 :url (intern-url url-string)
                                                 :headers default-request-headers)
                                  (parsing-error () (format stream "~&Bad URL Syntax: ~S" line) nil)))))
                         (error () nil))
            when ctlr collect ctlr))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;FLOGGER.LISP.50")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun make-transaction-controllers-from-post-logfile (pathname host port &key (log-format :post-log-format)
                                                                status-codes default-request-headers (stream *standard-output*))
  (with-open-file (file pathname :direction :input)
    (using-resource (line-buffer line-buffer *line-buffer-size*)
      (loop with parser = (log-line-parser log-format)
            with default-context = (%make-context host port)
            for line = (read-delimited-line file '(#\return #\Linefeed) nil line-buffer)
            while line
            for ctlr = (handler-case-if nil ;;(not *debug-client*)
                          (multiple-value-bind (url host method date http-version status bytes-received bytes-transmitted user referer
                                                form-alist user-agent ua-version ua-comment)
                              (funcall parser line)
                            method http-version status bytes-received bytes-transmitted
                            (case method
                              (:bad-record (format stream "Bad Log Record"))
                              (:post
                                (when (and form-alist
                                           (or (null status-codes) (member status status-codes)))
                                  (let ((url-string (merge-url url default-context)))
                                    (handler-case 
                                      (make-instance 'post-transaction-control
                                                     :url (intern-url url-string)
                                                     :user user
                                                     :form-alist form-alist
                                                     :property-list (list :referer referer :date date :host host 
                                                                          :user-agent (list user-agent ua-version ua-comment))
                                                     :headers default-request-headers)
                                      (parsing-error () (format stream "~&Bad URL Syntax: ~S" line) nil)))))))
                         (error () nil))
            when ctlr collect ctlr)))) 


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;FLOGGER.LISP.50")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(SCL:FUNDEFINE 'ALLOCATE-TRANSACTION-CONTROLLERS)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;FLOGGER.LISP.50")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defgeneric allocate-transaction-controllers (host port method control-specs &key log-format status-codes default-request-headers))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;FLOGGER.LISP.50")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod allocate-transaction-controllers (host port method (control-specs cons) &key log-format status-codes default-request-headers)
  (declare (ignore host port default-request-headers log-format status-codes))
  (error "Unsupported allocation method, ~S." method))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;FLOGGER.LISP.50")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod allocate-transaction-controllers (host port (method (eql :get)) (control-specs cons) &key log-format status-codes default-request-headers)
   (declare (ignore log-format status-codes))
  (make-transaction-controllers-from-spec host port default-request-headers control-specs))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;FLOGGER.LISP.50")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod allocate-transaction-controllers (host port (method (eql :head)) (control-specs cons) &key log-format status-codes default-request-headers)
   (declare (ignore log-format status-codes))
  (make-transaction-controllers-from-spec host port default-request-headers control-specs))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;FLOGGER.LISP.50")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod allocate-transaction-controllers (host port (method (eql :get)) (logfile pathname) &key (log-format *default-log-line-parser*)
                                                  status-codes default-request-headers)
  (make-transaction-controllers-from-logfile logfile method host port
					     :log-format log-format :status-codes status-codes :default-request-headers default-request-headers))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;FLOGGER.LISP.50")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod allocate-transaction-controllers (host port (method (eql :head)) (logfile pathname) &key (log-format *default-log-line-parser*)
                                                  status-codes default-request-headers)
  (make-transaction-controllers-from-logfile logfile method host port
					     :log-format log-format) :status-codes status-codes :default-request-headers default-request-headers)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;FLOGGER.LISP.50")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defmethod allocate-transaction-controllers (host port (method (eql :post)) (logfile pathname) &key (log-format *default-log-line-parser*)
                                                  status-codes default-request-headers)
  (make-transaction-controllers-from-post-logfile logfile host port :log-format log-format :status-codes status-codes :default-request-headers default-request-headers))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:CLIENT;FLOGGER.LISP.50")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun allocate-flogger (host port method control-specs &key  (log-format *default-log-line-parser*) status-codes
                              default-request-headers (class 'flogger))
  "Returns a Web flogger that will make requests using the HTTP method, METHOD, to HOST on PORT.

  CONTROL-SPECS is either a list of transaction specs suitable for METHOD, or for the

  HEAD and GET methods, a pathname containing a log file.

  LOG-FORMAT controls the kind of parser applied to the log file. It defaults to *default-log-line-parser*.

  STATUS-CODES is either null or a list of acceptable status code to use when creating transaction controllers 
  from a log file.

  DEFAULT-REQUEST-HEADERS can be used to control the behavior of the flogger.
  For example, by providing '(:connection :close), neither HTTP 1.0 nor HTTP 1.1
  transactions will attempt to keep the connection between the server and client open."
   (make-instance class
       :host host
       :port port
       :http-method method
       :transaction-controllers (allocate-transaction-controllers host port method control-specs
                                                                  :log-format (ecase method 
                                                                                (:post :post-log-format)
                                                                                ((:get :head) log-format))
                                                                  :status-codes status-codes
                                                                  :default-request-headers default-request-headers)))

