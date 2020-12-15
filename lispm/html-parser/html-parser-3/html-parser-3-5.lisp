;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for HTML-PARSER version 3.5
;;; Reason: Function (CLOS:METHOD HTML-PARSER::CLOSE-TAG (HTML-PARSER::HTML-PARSER-TOKEN)):  change (name stack-tag) to stack-tag.
;;; Written by JCMa, 8/28/96 13:55:22
;;; while running on Lispm Machine Thomas Jefferson from FEP0:>ComLink-38-11-HTTP-60-D-MIT-8-3.ilod.1
;;; with Genera 8.3, Logical Pathnames Translation Files NEWEST, NFS Server 435.0,
;;; Metering 439.0, Metering Substrate 439.0, Conversion Tools 430.0, Hacks 435.0,
;;; CLIM 66.5, Genera CLIM 66.0, PostScript CLIM 66.2, CLIM Documentation 66.0,
;;; 8-3-Patches 1.30, MAC 412.7, Statice Runtime 460.4, Statice 460.1,
;;; Statice Browser 460.0, Statice Documentation 423.0, DBFS Utilities 439.0,
;;; Showable Procedures 36.2, Binary Tree 34.0, Mailer 434.0,
;;; Experimental Working LispM Mailer 6.0, HTTP Server 60.22,
;;; Experimental W3 Presentation System 2.1, CL-HTTP Server Interface 48.1,
;;; Symbolics Common Lisp Compatibility 3.0, Experimental Comlink Packages 4.2,
;;; Experimental Comlink Utilities 9.10, Experimental Routing Taxonomy 8.0,
;;; Experimental COMLINK Database 10.15, Experimental Email Servers 11.4,
;;; Experimental Comlink Customized LispM Mailer 6.6,
;;; Experimental Dynamic Forms 11.7, Experimental Communications Linker Server 38.13,
;;; Jcma 41, HTML Parser 3.4, Ivory Revision 4A, FEP 328,
;;; FEP0:>I328-loaders.flod(24), FEP0:>I328-info.flod(24), FEP0:>I328-debug.flod(24),
;;; FEP0:>I328-lisp.flod(25), FEP0:>I328-kernel.fep(44), Boot ROM version 320,
;;; Device PROM version 325, Genera application 5.6.1a1,
;;; MacIvory SCSI Manager Server 4.3.2a1, Toolbox Servers 4.2,
;;; MacIvory & RPC library 6.3.3a1, MacIvory life support 4.3.8a1,
;;; Symbolics keyboard 2.1, Macintosh System Software 7.5.3,
;;; 1152x820 Screen with Genera fonts, Machine serial number 30376, Macintosh,
;;; Symbolics Keyboard,
;;; Add support for Apple's Gestalt and Speech Managers. (from SYS:MAC;MACIVORY-SPEECH-SUPPORT.LISP.5),
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.22),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.8),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Deny some hosts access to some servers. (from CML:LISPM;HOST-SERVICE-ACCESS-CONTROL.LISP.4),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.102),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.47).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTML-PARSER:HTML-PARSER;HTML-PARSER.LISP.8")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTML-PARSER:HTML-PARSER;HTML-PARSER.LISP.8")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Package: html-parser; Base: 10; Mode: lisp -*-")

(defmethod close-tag ((tag-name html-parser-token))
  (and (tag-on-stack-p tag-name)
       (do ((stack-tag (pop-tag) (pop-tag)))
           ((eql tag-name stack-tag)
            stack-tag))))

