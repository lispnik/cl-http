;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: tcp; Base: 10; Patch-File: t -*-
;;; Patch file for Private version 0.0
;;; Reason: Function (FLAVOR:METHOD :RECEIVE-TCP-SEGMENT TCP::TCP-PROTOCOL):  Patch
;;; :close-wait on reset so that it does not RESET-TCB.
;;; Written by JCMa, 7/22/95 09:52:29
;;; while running on Lispm Machine Thomas Jefferson from FEP0:>FV-Sherfacs-Relatus-B-MIT-8-3.ilod.1
;;; with Genera 8.3, Logical Pathnames Translation Files NEWEST, NFS Server 435.0,
;;; Metering 439.0, Metering Substrate 439.0, Conversion Tools 430.0, Hacks 435.0,
;;; CLIM 66.5, Genera CLIM 66.0, PostScript CLIM 66.2, CLIM Documentation 66.0,
;;; 8-3-Patches 1.24, MAC 412.7, TeX-Common 425.1, TeX-SCT 425.1, TeX-DVI 425.0,
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
;;; HTTP Server Interface 32.0, HTTP Server 37.1, Jcma 40, HTTP 1.0 Base Client 16.2,
;;; Image Substrate 435.0, Essential Image Substrate 427.0, Ivory Revision 4A,
;;; FEP 328, FEP0:>I328-loaders.flod(24), FEP0:>I328-info.flod(24),
;;; FEP0:>I328-debug.flod(24), FEP0:>I328-lisp.flod(25), FEP0:>I328-kernel.fep(44),
;;; Boot ROM version 320, Device PROM version 325, Genera application 5.6.1a1,
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
  "SYS:IP-TCP;TCP.LISP.3078")


(SCT:NOTE-PRIVATE-PATCH "Prevent reset of input buffer on tcp reset by HTTP servers.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "SYS:IP-TCP;TCP.LISP.3078")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: Lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: Yes -*-")

(defmethod (:receive-tcp-segment tcp-protocol) (seg)
  (neti:with-network-packet (seg)
    (process:with-no-other-processes
      ;; most common case is :established state.  Cache everything it needs
      (let* ((tcb (find-tcb-for-seg seg))
             (state (and tcb (tcb-state tcb)))
             (rcv.nxt (and tcb (tcb-seq-num-received tcb)))
             (rcv.lim (and tcb (tcb-seq-num-limit tcb)))
             (snd.una (and tcb (tcb-send-seq-acked tcb)))
             (snd.nxt (and tcb (tcb-next-send-seq tcb)))
             (flags (seg-flags seg))
             (seg.seq (seg-sequence seg))
             (seg.len (seg-length seg))
             (seg.lim (seq-num-sum seg.seq seg.len))
             (seg.ack (and (ldb-test seg-flag-ack-field flags) (seg-acknowledge seg)))
             (seg.urg (and (ldb-test seg-flag-urg-field flags)
                           (seq-num-sum seg.seq (seg-urgent seg))))
             (rst (ldb-test seg-flag-rst-field flags))
             (syn (ldb-test seg-flag-syn-field flags))
             (fin (ldb-test seg-flag-fin-field flags)))

        (prog segment-arrives ()
              (case state
                (:established (go established))
                ((nil :closed) (go closed))
                (:listen (go listen))
                (:syn-sent (go syn-sent))
                (otherwise (go established)))
           closed
              (cond (rst (return-tcp-segment seg))
                    (syn (receive-syn-segment self seg))
                    (T (reset-incoming-segment self seg)))
              (return-from segment-arrives)
           listen
              ;; first check for an RST
              (when rst
                (return-tcp-segment seg)
                (return-from segment-arrives))
              ;; second check for an ACK
              (when seg.ack
                (reset-incoming-segment self seg)
                (return-from segment-arrives))
              ;; third check for a SYN
              (when syn
                ;; Someday check security and precedence
                (if (eq (tcb-substate tcb) :dont-auto-syn)
                    (return-tcp-segment seg)    ;Listen has not yet accepted or rejected
                    (syn-meets-tcb tcb seg nil) ;Full listen specified, match things up
                    ;; use the incoming seg for sending out SYN.  If there was
                    ;; other control or text, wait for it to be retransmitted.
                    (send-syn-for-tcb tcb seg :syn-received))
                (return-from segment-arrives))
              ;; fourth other control or text
              (return-tcp-segment seg)
              (return-from segment-arrives)
           syn-sent
              ;; first check the ACK bit
              (when seg.ack
                (unless (seq-num-compare (tcb-initial-send-seq tcb) < seg.ack  snd.nxt)
                  (if rst
                      (return-tcp-segment seg)
                      (reset-incoming-segment self seg))
                  (return-from segment-arrives)))
              ;; second check the RST bit
              (when rst
                (if seg.ack
                    (reset-tcb tcb :reset))
                (return-tcp-segment seg)
                (return-from segment-arrives))
              ;; third check the security and precedence
              ;; fourth check the SYN bit         
              (when syn
                (syn-meets-tcb tcb seg)
                (return-from segment-arrives))
              ;; fifth, neither SYN nor RST, drop and return
              (return-tcp-segment seg)
              (return-from segment-arrives)
           established
           several-others
              ;; reject unacceptable segments
              (unless (if (zerop seg.len)
                          (if (= rcv.nxt rcv.lim)
                              (seq-num-compare rcv.nxt = seg.seq)
                              (seq-num-compare rcv.nxt  seg.seq < rcv.lim))
                          (if (= rcv.nxt rcv.lim)
                              nil
                              (or (seq-num-compare rcv.nxt  seg.seq < rcv.lim)
                                  (seq-num-compare rcv.nxt < seg.lim  rcv.lim))))
                ;; not acceptable
                (if rst
                    (return-tcp-segment seg)
                    (setf (tcb-needs-acking tcb) t)
                    (unless *some-tcb-needs-acking*
                      (setq *some-tcb-needs-acking* (zl:time)))
                    (reset-tcp-background-timer 2.)
                    (return-tcp-segment seg))
                (return-from segment-arrives))
              ;; second check the RST bit
              (when rst
                (return-tcp-segment seg)
                (case state
                  (:syn-received
                    ;; the spec says to handle :active and :passive
                    ;; differently We do, but not the way it suggests.  The
                    ;; thing doing the listen can figure out what to do if it
                    ;; was passive.  If it was active, then closing down the
                    ;; connection is the right thing.
                    (reset-tcb tcb
                               (case (tcb-substate tcb)
                                 ((:passive :closing) :reset)
                                 (:active  :refused)
                                 (otherwise (tcp-implementation-error t
                                                                      "Unknown substate ~S for TCB in :syn-received state."
                                                                      (tcb-substate tcb))))))
                  #|((:established :fin-wait-1 :fin-wait-2 :close-wait)
                   (reset-tcb tcb :reset))|#
                  ;; don't reset input buffer when receiving a reset from an
                  ;; HTTP server This is a temporary kludge until a real patch
                  ;; is available. 7/22/95 -- JCMa.
                  ((:established :fin-wait-1 :fin-wait-2)
                   (reset-tcb tcb :reset))
                  (:close-wait
                    (case (tcb-substate tcb)
                      ((:active) nil)
                      ((:passive :closing)  (reset-tcb tcb :reset))
                      (otherwise (tcp-implementation-error
                                   t "Unknown substate ~S for TCB in :close-wait state."
                                   (tcb-substate tcb)))))
                  ((:closing :last-ack :time-wait)
                   (remove-tcb tcb))
                  (otherwise (tcp-implementation-error t
                                                       "TCP state machine fell through.  Unknown state ~S with RST bit."
                                                       state)))
                (return-from segment-arrives))
              ;; third check security and precedence
              ;; fourth, check the SYN bit
              (when syn
                (when (seq-num-compare seg.seq  (tcb-initial-receive-seq tcb))
                  (reset-incoming-segment self seg)
                  (reset-tcb tcb :syn-out-of-window)
                  (return-from segment-arrives))
                ;; pretend it wasn't there and continue
                (multiple-value-setq (syn seg.seq seg.len seg.urg)
                  (remove-syn-from-segment seg seg.seq seg.len seg.urg)))
              ;; fifth check the ACK field (big clause)
              (when (null seg.ack)
                (return-tcp-segment seg)
                (return-from segment-arrives))
              ;; ACK bit is on
              (when (eq state :syn-received)
                (cond ((seq-num-compare snd.una  seg.ack  snd.nxt)
                       (setq state :established)
                       (setf (tcb-state tcb) state)
                   ;;; continue processing via fall through
                       )
                      (T (reset-incoming-segment self seg)
                         (return-from segment-arrives))))
              (cond ((or (eq state :established)        ;common case
                         (member state '(:fin-wait-1 :fin-wait-2 :close-wait :closing)))
                     ;; deal with ack information now
                     (cond ((seq-num-compare snd.una  seg.ack  snd.nxt)
                            ;; maybe new ack and/or window information
                            (multiple-value-setq (snd.una)
                              (tcb-process-ack tcb seg.ack (seg-window seg))))
                           ((seq-num-compare seg.ack < snd.una)
                            ;; duplicate ack.  Don't bother checking for larger window here.
                            )
                           (T #| (seq-num-compare seg.ack > snd.nxt) |#
                              (setf (tcb-needs-acking tcb) t)
                              (unless *some-tcb-needs-acking*
                                (setq *some-tcb-needs-acking* (zl:time)))
                              (reset-tcp-background-timer 2.)
                              (return-tcp-segment seg)
                              (return-from segment-arrives)))
                     ;; additional things to do if in various states
                     (when (and (eq state :fin-wait-1)
                                (seq-num-compare snd.una = snd.nxt))    ;FIN acknowledged
                       (setq state :fin-wait-2)
                       (setf (tcb-state tcb) state))
                     #+ignore ( (when (and (eq state :fin-wait-2)
                                           (null (tcb-send-segs tcb)))
                                  ;; The user will notice this state and
                                  ;; acknowledge his own close.  There is
                                  ;; nothing to do here.
                                  ))
                     (when (eq state :closing)
                       (when (= snd.una snd.nxt)        ;FIN acknowledged
                         (setq state :time-wait)
                         (setf (tcb-state tcb) state)
                         (setf (tcb-2MSL-timer tcb) (zl:time)))
                       (return-tcp-segment seg)
                       (return-from segment-arrives)))
                    ;; other states
                    ((eq state :last-ack)
                     ;; maybe process new acks
                     (when (seq-num-compare snd.una < seg.ack  snd.nxt)
                       (multiple-value-setq (snd.una)
                         (tcb-process-ack tcb seg.ack 0)))
                     (when (= snd.una snd.nxt)  ;FIN acknowledged
                       (remove-tcb tcb))
                     (return-tcp-segment seg)
                     (return-from segment-arrives))
                    ((eq state :time-wait)
                     (send-ack-for-tcb tcb seg :ack-while-in-time-wait)
                     (setf (tcb-2MSL-timer tcb) (zl:time))
                     (return-from segment-arrives)))
              ;; sixth check the URG bit,
              ;; seventh process segment text, and
              ;; eigth check the FIN bit
          
              ;; These are all handled by the input queuer because of
              ;; the need to compact packets, put them on the input
              ;; queue in order, and keep out-of-order packets for
              ;; later processing.  However, if the LispM is trying to close
              ;; the connection, it will not be able to handle any incoming
              ;; data.  In this case, the substate is :closing and the only
              ;; 'data' allowed is a FIN.
              (if (and (eq (tcb-substate tcb) :closing)
                       (cond (fin ( seg.len 1))
                             (T   ( seg.len 0))))
                  (progn (reset-tcb tcb :data-while-closing)
                         (reset-incoming-segment self seg))
                  (add-seg-to-input-queue tcb seg seg.seq seg.lim seg.urg fin))
              (return-from segment-arrives)))))
  nil)

