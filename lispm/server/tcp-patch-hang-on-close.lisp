;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: t -*-
;;; Patch file for Private version 0.0
;;; Reason: Function TCP::CLOSE-TCB:  Use *TCP-CLOSE-TIMEOUT* when calling TCB-TRAVEL-THROUGH-STATES.
;;; Variable TCP::*TCP-CLOSE-TIMEOUT*:  define missing variable.
;;; Written by JCMa, 12/05/95 07:29:34
;;; while running on Lispm Machine Thomas Jefferson from FEP0:>FV-Sherfacs-Relatus-B-MIT-8-3.ilod.1
;;; with Genera 8.3, Logical Pathnames Translation Files NEWEST, NFS Server 435.0,
;;; Metering 439.0, Metering Substrate 439.0, Conversion Tools 430.0, Hacks 435.0,
;;; CLIM 66.5, Genera CLIM 66.0, PostScript CLIM 66.2, CLIM Documentation 66.0,
;;; 8-3-Patches 1.27, MAC 412.7, TeX-Common 425.1, TeX-SCT 425.1, TeX-DVI 425.0,
;;; Illustrate 425.1, Statice Runtime 460.4, Statice 460.1, Statice Browser 460.0,
;;; Statice Documentation 423.0, DBFS Utilities 439.0,
;;; Relatus Natural Language Environment 183, RELATUS Utilities 29.1,
;;; Gnoscere Representation System 13.3, Dynamic Window Hardcopy Extensions 6.0,
;;; Background Bug Reporting 12.0, Relatus Parser Semantic Perception 27.1,
;;; Showable Procedures 36.2, Binary Tree 34.0, Reference System 32.2,
;;; Semantic Inversion 19.2, Lexical Classifier 3.0, Gnoscere Activity System 6.2,
;;; Flavor Resource 1.0, Relatus Parser 5.3, Relatus Generator 6.2,
;;; Lisp System Extensions 72.0, Object Resource 36.0, Agent Utilities 45.0,
;;; Feature Vector Editor 13.3, Symbolics Common Lisp Compatibility 3.0,
;;; SHERFACS International Conflict Dataset 41.1, Inductive Interaction Detector 5.4,
;;; Experimental HTTP Server 49.3, Jcma 41, Ivory Revision 4A, FEP 328,
;;; FEP0:>I328-loaders.flod(24), FEP0:>I328-info.flod(24), FEP0:>I328-debug.flod(24),
;;; FEP0:>I328-lisp.flod(25), FEP0:>I328-kernel.fep(44), Boot ROM version 320,
;;; Device PROM version 325, Genera application 5.6.1a1,
;;; MacIvory SCSI Manager Server 4.3.2a1, Toolbox Servers 4.2,
;;; MacIvory & RPC library 6.3.3a1, MacIvory life support 4.3.8a1,
;;; Symbolics keyboard 2.1, Macintosh System Software 7.5.1,
;;; 1152x820 Screen with Genera fonts, Machine serial number 30376, Macintosh,
;;; Symbolics Keyboard,
;;; Allow selected local flavor function shadowing without compiler warnings. (from RL:UTILS;FLAVOR-COMPILER-PATCH.LISP.7),
;;; Add CLIM presentation and text style format directives. (from FV:CODE;FORMAT.LISP.20),
;;; MIT CLIM 2.1 Patches (from FV:CODE;CLIM-2-1-PATCHES.LISP.1),
;;; Add support for Apple's Gestalt and Speech Managers. (from SYS:MAC;MACIVORY-SPEECH-SUPPORT.LISP.5).


(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "SYS:IP-TCP;TCP-DEFS.LISP.3048"
  "SYS:IP-TCP;TCP.LISP.3078")


(SCT:NOTE-PRIVATE-PATCH "Patch TCP hang on close when client drops connection.")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "SYS:IP-TCP;TCP-DEFS.LISP.3048")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: Lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: Yes; -*-")

(defparameter *tcp-close-timeout*  (* 60. 5.)   ;5 seconds
  "The time in 60ths of a second to wait for a TCP connection to close.
Wait forever if this is set to NIL.")

(export (intern "*TCP-CLOSE-TIMEOUT*" :tcp) :tcp)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "SYS:IP-TCP;TCP.LISP.3078")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: Lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: Yes -*-")

(defun close-tcb (tcb &optional abort-p)
  (process:with-no-other-processes
    (when (or (tcb-read-segs tcb)
              (tcb-received-segs tcb))
      ;; the theory here is that if the LispM is closing a connection, it
      ;; is really finished with it.  There shouldn't be any input
      ;; packets, and if there are, they will never be read.  Therefore,
      ;; it isn't really a syncronous close.
;     (setq abort-p t)                          ;If abort is set, we send a reset
                                                ;in abort-tcb below -- Kalman
      (free-all-read-segs tcb)
      (free-all-received-segs tcb))
    (if abort-p
        (abort-tcb tcb)
      (let ((completed-normally nil))
        (unwind-protect
          (case (tcb-state tcb)
            ((:listen :syn-sent)
             (setq completed-normally t)
             (remove-tcb tcb))
            ((:syn-received :established :close-wait)
             (setf (tcb-substate tcb) :closing)
             (send-fin-for-tcb tcb (get-tcp-segment tcb))
             ;; the lispm hangs forever here if there is no timeout  12/5/95 -- JCMa.
             #|(tcb-travel-through-states tcb "TCP Closing" nil
                                        :syn-received :established :close-wait :last-ack
                                        :fin-wait-1 :fin-wait-2 :closing)|#
             (tcb-travel-through-states tcb "TCP Closing" *tcp-close-timeout*
                                        :syn-received :established :close-wait :last-ack
                                        :fin-wait-1 :fin-wait-2 :closing)
             (setq completed-normally t)
             (when (eq (tcb-state tcb) :closed)
               (remove-tcb tcb)
               ;; otherwise it is in :time-wait and will be removed by the background
               ))
            ((:fin-wait-1 :fin-wait-2) (setq completed-normally t))
            (:closed
             (setq completed-normally t)
             (unless (eq (tcb-substate tcb) :inactive)
               (remove-tcb tcb)))
            (otherwise (bad-tcp-connection-state tcb "close a connection")))
          (tcb-wakeup-reader tcb)
          (tcb-wakeup-writer tcb)
          (unless completed-normally
            (abort-tcb tcb)))))))

