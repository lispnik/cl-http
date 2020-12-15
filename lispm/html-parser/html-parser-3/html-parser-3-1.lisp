;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for HTML-PARSER version 3.1
;;; Reason: Function HTML-PARSER::TAG-VALUE-ATTRIBUTE:  replace broken Sunil version with JCMa blindman's coding version.
;;; Written by JCMa, 8/22/96 18:37:48
;;; while running on Lispm Machine Thomas Jefferson from FEP0:>ComLink-38-11-HTTP-60-D-MIT-8-3.ilod.1
;;; with Genera 8.3, Logical Pathnames Translation Files NEWEST, NFS Server 435.0,
;;; Metering 439.0, Metering Substrate 439.0, Conversion Tools 430.0, Hacks 435.0,
;;; CLIM 66.5, Genera CLIM 66.0, PostScript CLIM 66.2, CLIM Documentation 66.0,
;;; 8-3-Patches 1.30, MAC 412.7, Statice Runtime 460.4, Statice 460.1,
;;; Statice Browser 460.0, Statice Documentation 423.0, DBFS Utilities 439.0,
;;; Showable Procedures 36.2, Binary Tree 34.0, Mailer 434.0,
;;; Experimental Working LispM Mailer 6.0, HTTP Server 60.14,
;;; Experimental W3 Presentation System 2.1, CL-HTTP Server Interface 48.1,
;;; Symbolics Common Lisp Compatibility 3.0, Experimental Comlink Packages 4.1,
;;; Experimental Comlink Utilities 9.9, Experimental Routing Taxonomy 8.0,
;;; Experimental COMLINK Database 10.10, Experimental Email Servers 11.3,
;;; Experimental Comlink Customized LispM Mailer 6.4,
;;; Experimental Dynamic Forms 11.7, Experimental Communications Linker Server 38.12,
;;; Jcma 41, W4 Examples 4.0, W4 Constraint-Guide Web Walker 32.1,
;;; HTTP 1.0 Base Client 35.1, Image Substrate 435.0,
;;; Essential Image Substrate 427.0, HTML Parser 3.0, dtd compiler 3.0,
;;; Ivory Revision 4A, FEP 328, FEP0:>I328-loaders.flod(24),
;;; FEP0:>I328-info.flod(24), FEP0:>I328-debug.flod(24), FEP0:>I328-lisp.flod(25),
;;; FEP0:>I328-kernel.fep(44), Boot ROM version 320, Device PROM version 325,
;;; Genera application 5.6.1a1, MacIvory SCSI Manager Server 4.3.2a1,
;;; Toolbox Servers 4.2, MacIvory & RPC library 6.3.3a1,
;;; MacIvory life support 4.3.8a1, Symbolics keyboard 2.1,
;;; Macintosh System Software 7.5.3, 1152x820 Screen with Genera fonts,
;;; Machine serial number 30376, Macintosh, Symbolics Keyboard,
;;; Add support for Apple's Gestalt and Speech Managers. (from SYS:MAC;MACIVORY-SPEECH-SUPPORT.LISP.5),
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.22),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.8),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Deny some hosts access to some servers. (from CML:LISPM;HOST-SERVICE-ACCESS-CONTROL.LISP.4),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.102),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.47),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.6).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTML-PARSER:HTML-PARSER;HTML-READER.LISP.4")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTML-PARSER:HTML-PARSER;HTML-READER.LISP.4")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Package: html-parser; Base: 10; Mode: lisp -*-")

(defun tag-value-attribute (value tag-defn)
  ;; If the attribute definition is not found, then nil is left it's place
  (loop for item in (attributes tag-defn)
	for allowed-value = (allowed-values item)
	when (typecase allowed-value
	       (null nil)
	       (cons (member value allowed-value))
	       (symbol (eql value allowed-value)))
	  return item
	finally (return nil)))

