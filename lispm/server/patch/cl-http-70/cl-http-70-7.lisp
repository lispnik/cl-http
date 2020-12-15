;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Patch-File: T -*-
;;; Patch file for CL-HTTP version 70.7
;;; Reason: Variable WWW-UTILS::*HOURLY-SERVER-TIMER*:  new.
;;; Function WWW-UTILS::HOURLY-SERVER-TIMER:  -
;;; Variable WWW-UTILS::*HOURLY-SERVER-TASKS*:  -
;;; Function WWW-UTILS::NEXT-HOURLY-UNIVERSAL-TIME:  -
;;; Function WWW-UTILS::SYNCHRONIZE-HOURLY-SERVER-TASKS:  -
;;; Function WWW-UTILS:ADD-PERIODIC-TASK:  update to support hourly.
;;; Function WWW-UTILS:DELETE-PERIODIC-TASK:  update to handle hourly.
;;; Function WWW-UTILS:SHOW-PERIODIC-TASKS:  -
;;; Function WWW-UTILS::RUN-HOURLY-SERVER-TASKS:  -
;;; Turn off HTTP server debugging every hour.
;;; Written by JCMa, 9/16/1999 19:34:19
;;; while running on Publications Y2K Testbed from HOST6:/usr/lib/symbolics/eop-world-pub6-host6-990913.vlod
;;; with Open Genera 2.0, Genera 8.5, Logical Pathnames Translation Files NEWEST,
;;; Lock Simple 437.0, Version Control 405.0, Compare Merge 404.0, CLIM 72.0,
;;; Genera CLIM 72.0, PostScript CLIM 72.0, CLIM Documentation 72.0,
;;; Statice Runtime 466.1, Statice 466.0, Statice Browser 466.0,
;;; Statice Server 466.2, Statice Documentation 426.0, Joshua 237.3,
;;; Joshua Documentation 216.0, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Mailer 438.0, Showable Procedures 36.3,
;;; Binary Tree 34.0, Working LispM Mailer 8.0, Experimental HTTP Server 70.6,
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

;;; Patch file for CL-HTTP version 70.7
;;; Written by JCMa, 9/17/1999 10:13:16
;;; while running on Publications Y2K Testbed from HOST6:/usr/lib/symbolics/eop-world-pub6-host6-990913.vlod
;;; with Open Genera 2.0, Genera 8.5, Logical Pathnames Translation Files NEWEST,
;;; Lock Simple 437.0, Version Control 405.0, Compare Merge 404.0, CLIM 72.0,
;;; Genera CLIM 72.0, PostScript CLIM 72.0, CLIM Documentation 72.0,
;;; Statice Runtime 466.1, Statice 466.0, Statice Browser 466.0,
;;; Statice Server 466.2, Statice Documentation 426.0, Joshua 237.3,
;;; Joshua Documentation 216.0, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Mailer 438.0, Showable Procedures 36.3,
;;; Binary Tree 34.0, Working LispM Mailer 8.0, Experimental HTTP Server 70.7,
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
;;; Experimental White House Publication System 25.20,
;;; Experimental WH Automatic Categorization System 15.8,
;;; 8-5-Genera-Local-Patches 1.39, 39-COMLINK-Local-Patches 1.11,
;;; Publications-Server-Local-Patches 1.4, Ivory Revision 5, VLM Debugger 329,
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
  "HTTP:LISPM;SERVER;LISPM.LISP.440"
  "HTTP:LISPM;SERVER;LISPM.LISP.441")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.440")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

;;;------------------------------------------------------------------- 
;;;
;;; HOURLY TASKS
;;;

(defvar *hourly-server-timer* nil)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.440")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defun hourly-server-timer ()
  (or *hourly-server-timer*
      (setq *hourly-server-timer* (process:create-timer-call 'run-hourly-server-tasks '()
                                                            :name "Hourly Server Tasks"))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.440")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defvar *hourly-server-tasks* nil)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.440")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(defun next-hourly-universal-time (&optional (offset 0))
  (check-type offset (integer 0 59))
  (multiple-value-bind (seconds minutes hours date month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore seconds minutes))
    (+ (encode-universal-time 0 offset hours date month year) (* 60. 60.))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.440")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

;; Set up a timer to run every hour on the half hour.
(defun synchronize-hourly-server-tasks ()
  (process:reset-timer-absolute (hourly-server-timer) (next-hourly-universal-time 30)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.440")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(define add-periodic-task (name periodicity form)
  "Add a periodic server task.
NAME is a unique string naming the task.
PERIODICITY is a weekday keyword or :daily, or :weekly or :hourly.
FORM is the form that executes the desired task."
  (check-type name string)
  (check-type form cons)
  (ecase periodicity
    ((:daily :sunday :monday :tuesday :wednesday :thursday :friday :saturday :weekly)
     (let ((entry (find name *daily-server-tasks* :test #'string-equal :key #'first)))
       (cond (entry
              (setf (second entry) periodicity
                    (third entry) form))
             (t (setq *daily-server-tasks* `(,.*daily-server-tasks* (,name ,periodicity ,form)))))))
    (:hourly
      (let ((entry (find name *hourly-server-tasks* :test #'string-equal :key #'first)))
        (cond (entry
               (setf (second entry) periodicity
                     (third entry) form))
              (t (setq *hourly-server-tasks* `(,.*hourly-server-tasks* (,name ,periodicity ,form)))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.440")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(define delete-periodic-task (name)
  "Delete a periodic server task named NAME."
  (setq *daily-server-tasks* (delete name *daily-server-tasks* :test #'string-equal :key #'first)
        *hourly-server-tasks* (delete name *hourly-server-tasks* :test #'string-equal :key #'first)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.440")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(si:add-initialization "Synchronize HTTP Timers"
                       '(progn
                          (synchronize-hourly-server-tasks)
                          (synchronize-daily-server-tasks)
                          (synchronize-idle-http-process-scavenger))
                       '(:now :login))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.440")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(si:add-initialization "Clear HTTP Timers"
                       '(mapc #'(lambda (x)
                                  (let ((timer (scl:symbol-value-globally x)))
                                    (when timer
                                      (process:clear-timer timer)
                                      (setf (scl:symbol-value-globally x) nil))))
                              '(*hourly-server-timer* *daily-server-timer* *idle-http-process-scavenger-timer*))
                       '(:before-cold))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.440")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(define show-periodic-tasks (&key (stream *standard-output*))
  "Describes the periodic server tasks on STREAM."
  (flet ((format-tasks (title tasks)
           (loop initially (format stream "~&~A~2%" title)
                 for (name periodicity form) in (sort tasks #'scl:alphalessp :key #'first)
                 for count upfrom 1
                 do (format stream "~&~D  ~A [~:(~A~)]: ~~S~" count name periodicity form))))
    (format stream "~&--------------------------------------------------------------------------------")
    (format-tasks "Daily HTTP Server Tasks" *daily-server-tasks*)
    (format stream "~&--------------------------------------------------------------------------------")
    (format-tasks "Hourly HTTP Server Tasks" *hourly-server-tasks*)
    (format stream "~&--------------------------------------------------------------------------------")))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.440")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

;; toplevel routines for getting work done everyday at midnight PST.
(defun run-hourly-server-tasks ()
  (www-utils:with-null-stream (*standard-output* *query-io*)
    (loop with week-day = (weekday)
          for (name periodicity form) in *hourly-server-tasks*
          when (case periodicity
                 (:hourly t)
                 (:weekly name (eq periodicity :sunday))
                 (t (eq periodicity week-day)))
            do (handler-case
                 (apply (car form) (cdr form))
                 (error
                   (err)
                   (log-http-server-error "Error in hourly server task, ~S:~&~A" name (report-string err))))))
  ;; Reset so we run again.
  (synchronize-hourly-server-tasks))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:LISPM;SERVER;LISPM.LISP.441")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: WWW-UTILS; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-")

(add-periodic-task "Turn Off Server Debugging" :hourly '(http::debug-server nil))
