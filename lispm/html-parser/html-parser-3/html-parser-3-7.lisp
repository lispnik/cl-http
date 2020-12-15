;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for HTML-PARSER version 3.7
;;; Reason: Variable HTML-PARSER::*SOURCE-FILE-EXTENSION*:  new.
;;; Function HTML-PARSER:INITIALIZE-PARSER:  use pathnames rather than strings.
;;; Written by JCMa, 10/29/96 18:44:29
;;; while running on Lispm Machine Thomas Jefferson from FEP0:>ComLink-38-11-HTTP-60-D-MIT-8-3.ilod.1
;;; with Genera 8.3, Logical Pathnames Translation Files NEWEST, NFS Server 435.0,
;;; Metering 439.0, Metering Substrate 439.0, Conversion Tools 430.0, Hacks 435.0,
;;; CLIM 66.5, Genera CLIM 66.0, PostScript CLIM 66.2, CLIM Documentation 66.0,
;;; 8-3-Patches 1.31, MAC 412.7, Statice Runtime 460.4, Statice 460.1,
;;; Statice Browser 460.0, Statice Documentation 423.0, DBFS Utilities 439.0,
;;; Showable Procedures 36.3, Binary Tree 34.0, Mailer 434.0,
;;; Experimental Working LispM Mailer 6.0, HTTP Server 60.55,
;;; W3 Presentation System 2.2, CL-HTTP Server Interface 48.1,
;;; Symbolics Common Lisp Compatibility 3.0, Experimental Comlink Packages 4.2,
;;; Experimental Comlink Utilities 9.14, Experimental Routing Taxonomy 8.1,
;;; Experimental COMLINK Database 10.20, Experimental Email Servers 11.8,
;;; Experimental Comlink Customized LispM Mailer 6.7,
;;; Experimental Dynamic Forms 11.10,
;;; Experimental Communications Linker Server 38.31, Jcma 41, HTML Parser 3.6,
;;; dtd compiler 4.0, Ivory Revision 4A, FEP 328, FEP0:>I328-loaders.flod(24),
;;; FEP0:>I328-info.flod(24), FEP0:>I328-debug.flod(24), FEP0:>I328-lisp.flod(25),
;;; FEP0:>I328-kernel.fep(44), Boot ROM version 320, Device PROM version 325,
;;; Genera application 5.6.1a1, MacIvory SCSI Manager Server 4.3.2a1,
;;; Toolbox Servers 4.2, MacIvory & RPC library 6.3.3a1,
;;; MacIvory life support 4.3.8a1, Symbolics keyboard 2.1,
;;; Macintosh System Software 7.5.5, 1152x820 Screen with Genera fonts,
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
;;; Ephemeral gc reclamation patch (from SYS:8-3-PATCHES;PATCH;8-3-PATCHES-1-31.LISP.3).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTML-PARSER:HTML-PARSER;HTML-PARSER.LISP.12")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTML-PARSER:HTML-PARSER;HTML-PARSER.LISP.12")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Package: html-parser; Base: 10; Mode: lisp -*-")

(defconstant *source-file-extension* "lisp")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTML-PARSER:HTML-PARSER;HTML-PARSER.LISP.12")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Package: html-parser; Base: 10; Mode: lisp -*-")

(declaim (inline file-newer-p probe-file*))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTML-PARSER:HTML-PARSER;HTML-PARSER.LISP.12")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Package: html-parser; Base: 10; Mode: lisp -*-")

(declaim (special *current-dtd*))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTML-PARSER:HTML-PARSER;HTML-PARSER.LISP.12")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Package: html-parser; Base: 10; Mode: lisp -*-")

(defun initialize-parser (&optional (doctype "-//W3C//DTD HTML 3.2 Draft//EN"))
  "This is the initialization function for initializing the DTD. The
argument is used to specify the DTD the user wishes to load. If not
compiled, the DTD compiler is loaded and invoked. The root for the
DTD is then located, and the lexer is initialized.

To modify the list of DTD's available, look at the definition of
*HTML-DTD-LIST* in defs.lisp."
  (setq *html-tags* nil)
  (tokenize-list *character-data-classes* #'tokenize-name)
  (if (symbolp *pcdata-default-container*)
      (setq *pcdata-default-container*
            (tokenize-name (symbol-name *pcdata-default-container*))))
  ;; Load the DTD
  (let* ((dtd-name (cadr (assoc doctype *html-dtd-list* :test #'string=)))
         (src-file (make-pathname :host #-MCL "HTML-PARSER" #+MCL "HTML-PARSE-SYS"
                                  :name dtd-name
                                  :type *source-file-extension*))
         (fasl-file (make-pathname :host #-MCL "HTML-PARSER" #+MCL "HTML-PARSE-SYS"
                                   :name dtd-name
                                   :type *compiled-file-extension*)))
    (declare (dynamic-extent fasl-file src-file))
    (when (or (not (probe-file* fasl-file))
              (file-newer-p src-file fasl-file))
      (load-dtd-compiler)
      (compile-file src-file))
    (setq *html-tags* nil)
    (tokenize-list *character-data-classes* #'tokenize-name)
    (load fasl-file))
  (set-html-tag-backpointers)
  (define-unknown-tag)
  (initialize-parse-dispatch-functions)
  (setq *current-dtd* doctype)
  t)

