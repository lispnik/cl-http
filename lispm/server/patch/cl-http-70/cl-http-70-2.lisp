;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: USER; Patch-File: T -*-
;;; Patch file for CL-HTTP version 70.2
;;; Reason: CLOS class TQ:TASK-QUEUE:  process-name and wait-whostate no longer class allocation.
;;; Function WWW-UTILS:MAKE-PROCESS:  Get priorities right, by translating from old style to new style priorities.
;;; Function WWW-UTILS:PROCESS-PRIORITY: Use the SCL version in order to assure proper translation.
;;; Written by JCMa, 9/08/99 20:15:02
;;; while running on Publications Y2K Testbed from HOST6:/usr/lib/symbolics/eop-world-pub6-host6-990831.vlod
;;; with Open Genera 2.0, Genera 8.5, Logical Pathnames Translation Files NEWEST,
;;; Lock Simple 437.0, Version Control 405.0, Compare Merge 404.0, CLIM 72.0,
;;; Genera CLIM 72.0, PostScript CLIM 72.0, CLIM Documentation 72.0,
;;; Statice Runtime 466.1, Statice 466.0, Statice Browser 466.0,
;;; Statice Server 466.2, Statice Documentation 426.0, Joshua 237.3,
;;; Joshua Documentation 216.0, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Mailer 438.0, Showable Procedures 36.3,
;;; Binary Tree 34.0, Working LispM Mailer 8.0, Experimental HTTP Server 70.1,
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
;;; Experimental WH Automatic Categorization System 15.4, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.16, DEC OSF/1 V4.0 (Rev. 205),
;;; 1260x932 24-bit TRUE-COLOR X Screen HOST6:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number 1719841853,
;;; Local flavor function patch (from DISTRIBUTION|DIS-EMB-HOST:/db/eop.sct/eop/config/mail-server/patches/local-flavor-function-patch-mark2.),
;;; Get emb file host patch (from DISTRIBUTION|DIS-EMB-HOST:/db/eop.sct/eop/config/mail-server/patches/get-emb-file-host-patch.),
;;; Get mailer home location from namespace (from DISTRIBUTION|DIS-EMB-HOST:/db/eop.sct/eop/config/mail-server/patches/mailer-environment-patch.),
;;; Consider internet-domain-name when matching names to file hosts (from DISTRIBUTION|DIS-EMB-HOST:/db/eop.sct/eop/config/mail-server/patches/pathname-host-namep-patch.lisp.~2~),
;;; Parse pathname patch (from DISTRIBUTION|DIS-EMB-HOST:/db/eop.sct/eop/config/mail-server/patches/parse-pathname-patch.),
;;; Get internal event code patch (from DISTRIBUTION|DIS-EMB-HOST:/db/eop.sct/eop/config/mail-server/patches/get-internal-event-code-patch.),
;;; AutoLogin (from DISTRIBUTION|DIS-EMB-HOST:/db/eop.sct/eop/config/mail-server/patches/auto-login.lisp.~3~),
;;; Generate an error any time there domain system tries to create a bogus host object for the local host. (from DISTRIBUTION|DIS-EMB-HOST:/db/eop.sct/eop/config/mail-server/patches/detect-bogus-host.lisp.~6~),
;;; Set Mailer UID variables for current namespace. (from DISTRIBUTION|DIS-EMB-HOST:/db/eop.sct/eop/config/mail-server/patches/mailer-uid.),
;;; Provide Switch between EOP and MIT sources. (from DISTRIBUTION|DIS-EMB-HOST:/db/eop.sct/eop/config/mail-server/patches/mit-source.lisp.~4~),
;;; Make FS:USER-HOMEDIR look in the namespace as one strategy. (from DISTRIBUTION|DIS-EMB-HOST:/db/eop.sct/eop/config/mail-server/patches/user-homedir.),
;;; Local uid patch (from DISTRIBUTION|DIS-EMB-HOST:/db/eop.sct/eop/config/mail-server/patches/local-uid-patch.),
;;; Statice log clear patch (from DISTRIBUTION|DIS-EMB-HOST:/db/eop.sct/eop/config/mail-server/patches/statice-log-clear-patch.),
;;; Make compiled-function-spec-p of CLOS class symbol return NIL instead of erring (from DISTRIBUTION|DIS-EMB-HOST:/db/eop.sct/eop/config/mail-server/patches/compiled-function-spec-p-patch.),
;;; Improve mailer host parsing (from DISTRIBUTION|DIS-EMB-HOST:/db/eop.sct/eop/config/mail-server/patches/parse-mailer-host-patch.),
;;; Make native domain name host patch (from DISTRIBUTION|DIS-EMB-HOST:/db/eop.sct/eop/config/mail-server/patches/make-native-domain-name-host-patch.),
;;; Domain query cname loop patch (from DISTRIBUTION|DIS-EMB-HOST:/db/eop.sct/eop/config/mail-server/patches/domain-query-cname-loop-patch.),
;;; Increase disk wired wait timeout from 30 to 90 seconds (from DISTRIBUTION|DIS-EMB-HOST:/db/eop.sct/eop/config/mail-server/patches/disk-wait-90-patch.),
;;; Checkpoint command patch (from DISTRIBUTION|DIS-EMB-HOST:/db/eop.sct/eop/config/mail-server/patches/checkpoint-command-patch.),
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.24),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48).

;;; Patch file for CL-HTTP version 70.2
;;; Written by JCMa, 9/08/99 22:04:28
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
;;; Experimental WH Automatic Categorization System 15.4,
;;; 8-5-Genera-Local-Patches 1.0, 39-COMLINK-Local-Patches 1.11, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.16, DEC OSF/1 V4.0 (Rev. 205),
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
;;; Redirect to WWW.PUB.WHITEHOUSE.GOV (from EOP:PUB;HTTP;REDIRECT-TO-PRIMARY.LISP.12).




(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;TASK-QUEUE.LISP.23"
  "HTTP:LISPM;SERVER;LISPM.LISP.438"
  "HTTP:LISPM;SERVER;LISPM.LISP.439")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;TASK-QUEUE.LISP.23")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: (task-queue :nicknames (tq) :use (future-common-lisp)); BASE: 10; Syntax: ANSI-Common-Lisp; -*-")

;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defclass task-queue
          ()
    ((queue :initform nil :accessor task-queue-queue)   ;queue containing the entries
     (pointer :initform nil :accessor task-queue-pointer)       ;pointer to the last cons in the queue
     (n-pending-tasks :initform 0 :accessor task-queue-pending-tasks)   ;number of incomplete tasks
     (run-p :initform t :accessor task-queue-run-p)     ;return non-null when the process should run.
     (lock :initform nil :initarg :lock :accessor task-queue-lock)
     (process-priority :initform 0 :initarg :process-priority :accessor task-queue-process-priority)
     (process :initform nil :accessor task-queue-process)       ;process running the queue
     (process-name :initform "Task Queue" :initarg :process-name :accessor task-queue-process-name)
     (wait-whostate :initform "Task Wait" :initarg :wait-whostate :accessor task-queue-wait-whostate))
  (:documentation "A mixin that executes task entries with a separate process."))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.438")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(eval-when (load)
  (unexport (intern "PROCESS-PRIORITY" :www-utils) :www-utils)
  (unintern (intern "PROCESS-PRIORITY" :process) :www-utils)
  (mapc #'(lambda (x)
            (shadowing-import x :www-utils)
            (export x :www-utils))
        (list (intern "PROCESS-PRIORITY" :si))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.439")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(define make-process (process-name &key priority quantum run-reasons background-p 
                                   restart-after-reset warm-boot-action &allow-other-keys)
  "Creates a process using a portable set of options."
  (declare (ignore background-p))
  (process:make-process process-name
                        :priority (if priority (process:make-process-priority :foreground priority) process:*default-process-priority*)
                        :quantum (or quantum si:default-quantum)
                        :run-reasons run-reasons
                        :restart-after-reset restart-after-reset
                        :warm-boot-action (ecase warm-boot-action
                                            (:delayed-restart 'process:process-warm-boot-delayed-restart)
                                            (:kill 'process:process-warm-boot-reset)
                                            (:restart 'process:process-warm-boot-restart))))

