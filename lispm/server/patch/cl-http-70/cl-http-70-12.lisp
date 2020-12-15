;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for CL-HTTP version 70.12
;;; Reason: DEFINE-HEADER :KEYWORDS:  new header to support email.
;;; Written by JCMa, 9/22/1999 19:38:43
;;; while running on Publications Y2K Testbed from HOST6:/usr/lib/symbolics/eop-world-pub6-host6-990913.vlod
;;; with Open Genera 2.0, Genera 8.5, Logical Pathnames Translation Files NEWEST,
;;; Lock Simple 437.0, Version Control 405.0, Compare Merge 404.0, CLIM 72.0,
;;; Genera CLIM 72.0, PostScript CLIM 72.0, CLIM Documentation 72.0,
;;; Statice Runtime 466.1, Statice 466.0, Statice Browser 466.0,
;;; Statice Server 466.2, Statice Documentation 426.0, Joshua 237.3,
;;; Joshua Documentation 216.0, Image Substrate 440.4,
;;; Essential Image Substrate 433.0, Mailer 438.0, Showable Procedures 36.3,
;;; Binary Tree 34.0, Working LispM Mailer 8.0, Experimental HTTP Server 70.11,
;;; Experimental W3 Presentation System 8.0,
;;; Experimental CL-HTTP Server Interface 53.0,
;;; Experimental Symbolics Common Lisp Compatibility 4.0,
;;; Experimental Comlink Packages 6.0, Experimental Comlink Utilities 10.0,
;;; Experimental COMLINK Cryptography 2.0, Experimental Routing Taxonomy 9.0,
;;; Experimental COMLINK Database 11.19, Experimental Email Servers 12.0,
;;; Experimental Comlink Customized LispM Mailer 7.0,
;;; Experimental Dynamic Forms 14.4, Experimental Communications Linker Server 39.5,
;;; Experimental Lambda Information Retrieval System 22.2,
;;; Experimental Comlink Documentation Utilities 6.0,
;;; Experimental White House Publication System 25.27,
;;; Experimental WH Automatic Categorization System 15.11,
;;; 8-5-Genera-Local-Patches 1.41, 39-COMLINK-Local-Patches 1.14,
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
;;; Attempt to fix recursive block transport (from EOP:LOCAL-PATCHES;GENERA;8-5;8-5-GENERA-LOCAL-PATCHES-1-40.LISP.1),
;;; Redirect to WWW.PUB.WHITEHOUSE.GOV (from EOP:PUB;HTTP;REDIRECT-TO-PRIMARY.LISP.12),
;;; Some holiday favorites for Pete (from EOP:LOCAL-PATCHES;PUBLICATIONS;PUBLICATIONS-SERVER-LOCAL-PATCHES-1-4.LISP.2).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;HEADERS.LISP.433")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.433")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :keywords
               (:keyword-sequence-header :email)
  :print-string "Keywords")

