;;; -*- Mode: lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: preferably; Patch-File: Yes; -*-
;;; Patch file for Private version 0.0
;;; Written by JCMa, 3/25/94 15:46:42
;;; while running on Bill Clinton from FEP0:>FV-Sherfacs-ComLink-E-AI-8-3.ilod.1
;;; with Genera 8.3, Logical Pathnames Translation Files NEWEST, NFS Server 435.0,
;;; Metering 439.0, Metering Substrate 439.0, Conversion Tools 430.0, Hacks 435.0,
;;; CLIM 52.13, Genera CLIM 52.5, PostScript CLIM 52.2, CLIM Documentation 52.1,
;;; 8-3-Patches 1.16, MAC 412.3, TeX-Common 425.1, TeX-SCT 425.1, TeX-DVI 425.0,
;;; Illustrate 425.1, Statice Runtime 460.4, Statice 460.1, Statice Browser 460.0,
;;; Statice Documentation 423.0, DBFS Utilities 439.0, Feature Vector Editor 12.0,
;;; Symbolics Common Lisp Compatibility 2.1, Showable Procedures 36.0,
;;; SHERFACS International Conflict Dataset 40.0, Binary Tree 34.0,
;;; Inductive Interaction Detector 4.0, Communications Linker Server 3.99,
;;; Mailer 434.0, Experimental White House Distribution System 6.8,
;;; White House Email Servers 15.0, Experimental HTTP 1.0 Server 8.10,
;;; Experimental EOP HTTP server 1.9, Image Substrate 435.0,
;;; Essential Image Substrate 427.0, Ivory Revision 4A (FPA enabled), FEP 328,
;;; FEP0:>i328-loaders.flod(24), FEP0:>i328-info.flod(24), FEP0:>i328-debug.flod(24),
;;; FEP0:>i328-lisp.flod(25), FEP0:>i328-kernel.fep(44), Boot ROM version 316,
;;; Device PROM version 325, 1067x748 B&W Screen, Machine serial number 459,
;;; Add CLIM presentation and text style format directives. (from FV:SCLC-JOURNAL;SCLC-2-1.LISP.1),
;;; MIT Patches to CLIM 2.0. (from FV:CODE;CLIM-PATCHES.LISP.5),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make domain-query-host add address records for chaos and dial when querying (from CML:LISPM;DOMAIN-QUERY-COMPLETENESS.LISP.21),
;;; Don't force in the mail-x host (from CML:LISPM;MAILBOX-FORMAT.LISP.16),
;;; Find domain mail hosts internal cname bug (from CML:LISPM;FIND-DOMAIN-MAIL-HOSTS-INTERNAL-CNAME-BUG.LISP.3),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.87),
;;; ZMAIL filtering keep sites when senderrecipient selected from message. (from EOP:CODE;ZMAIL-MSG-KEEP-SITE.LISP.2),
;;; Add ZMAIL header X-URL. (from CML:LISPM;ZMAIL-URL-HEADER.LISP.8),
;;; Documents cache (from B:>hes>documents-cache.lisp.2).


;;; (C) Copyright 1994-99, John C. Mallery.
;;;     All Rights Reserved.
;;;


;;; Add content-transfer chunking generation for dynamic streams.   7/24/96 -- JCMa.
;;; Change to mode instance variables 7/22/96 -- JCMa.
;;; Efficiency improvements   3/26/96 -- JCMa.
;;; 
;;; For input from the HTTP stream: scope out :LINE-IN, :STRING-LINE-IN and count the bytes. 
;;;


;;;------------------------------------------------------------------- 
;;;
;;; MODAL ASCII TRANSLATING AND BINARY OUTPUT STREAM
;;;

(defflavor modal-ascii-translating-output-stream-mixin
        ((output-mode :ascii)                   ;modes: :ascii, :binary
         (bytes-transmitted 0))                 ;total bytes shipped to the client.
        ()
  :abstract-flavor
  (:required-flavors si:output-stream tcp::tcp-output-stream-mixin)
  (:required-methods :tyo)
  (:documentation :mixin "An output stream that translates characters from Lisp Machine character
set into ASCII for :TYO method when output-mode is non-null,
otherwise sends bytes.."))

(defmethod (bytes-transmitted modal-ascii-translating-output-stream-mixin) ()
  bytes-transmitted)

(defmethod (set-bytes-transmitted modal-ascii-translating-output-stream-mixin) (value)
  (check-type value integer)
  (setq bytes-transmitted value))

(defsetf bytes-transmitted set-bytes-transmitted)

(export 'bytes-transmitted :tcp)

;; More effective means to count actual bytes transmitted.   1/21/97 -- JCMa.
(defwhopper (:send-output-buffer modal-ascii-translating-output-stream-mixin) (seg limit explicit)
  (let ((bytes (- limit tcp::output-seg-start-idx)))
    (multiple-value-prog1
      (continue-whopper seg limit explicit)
      (incf bytes-transmitted bytes))))

(defmacro-in-flavor (put-byte modal-ascii-translating-output-stream-mixin) (byte)
  `(progn
     (unless (and si:stream-output-buffer (< si:stream-output-index si:stream-output-limit))
       (send self :setup-new-output-buffer))
     (setf (aref si:stream-output-buffer si:stream-output-index) ,byte)
     (incf si:stream-output-index)))

(defmethod (:tyo modal-ascii-translating-output-stream-mixin) (ch)
  (ecase output-mode
    (:ascii
      (cond ((char= ch #\Return)
             (put-byte #.(char-to-ascii #\return))
             (put-byte #.(char-to-ascii #\linefeed)))
            (t (put-byte (char-to-ascii ch)))))
    ((:binary :crlf)
     (put-byte ch))))

(defmacro-in-flavor (%string-out modal-ascii-translating-output-stream-mixin) (vector &optional (start 0) end)
  `(let ((vector ,vector)
         (limit (or ,end (length ,vector))))
     (declare (sys:array-register vector))
     (when (< ,start limit)
       (unless si:stream-output-buffer
         (send self :setup-new-output-buffer))
       (flet ((push-byte (buffer byte)
                (setf (aref buffer si:stream-output-index) byte)
                (incf si:stream-output-index)))
         (declare (inline push-byte))
         (ecase output-mode
           (:ascii
             (check-type vector string)
             (loop with insert-lf
                   with idx = ,start
                   doing (let ((buffer si:stream-output-buffer))
                           (declare (sys:array-register buffer))
                           (loop while (and (< idx limit)
                                            (< si:stream-output-index si:stream-output-limit))
                                 for ascii = (si:char-to-ascii (aref vector idx))
                                 do (push-byte buffer ascii)
                                    (incf idx)
                                    (when (eql ascii #.(char-to-ascii #\return))
                                      (if (< si:stream-output-index si:stream-output-limit)
                                          (push-byte buffer #.(char-to-ascii #\linefeed))
                                          (setq insert-lf t)))))
                   when (< idx limit)
                     do (send self :setup-new-output-buffer)
                        (when insert-lf
                          (setf (aref si:stream-output-buffer si:stream-output-index) #.(char-to-ascii #\linefeed))
                          (incf si:stream-output-index)
			  (setq insert-lf nil))
                   else return))
           ((:binary :crlf)
            (check-type vector vector)
            (loop with idx = ,start
                  doing (let ((buffer si:stream-output-buffer))
                          (declare (sys:array-register buffer))
                          (loop while (and (< idx limit)
                                           (< si:stream-output-index si:stream-output-limit))
                                do (push-byte buffer (aref vector idx))
                                   (incf idx)))
                  when (< idx limit)
                    do (send self :setup-new-output-buffer)
                  else return)))))))

;; Must be defined to avoid character at a time output.   1/23/97 -- JCMa.
(defmethod (:string-out modal-ascii-translating-output-stream-mixin) (vector &optional (start 0) end)
  (%string-out vector start end))

(defmethod (:line-out modal-ascii-translating-output-stream-mixin) (line &optional (start 0) end)
  (%string-out line start end)
  (put-byte #.(char-to-ascii #\return))
  (put-byte #.(char-to-ascii #\linefeed)))

(defmethod (:fresh-line modal-ascii-translating-output-stream-mixin) ()
  (flet ((put-byte (byte)
           (unless (and si:stream-output-buffer (< si:stream-output-index si:stream-output-limit))
             (send self :setup-new-output-buffer))
           (setf (aref si:stream-output-buffer si:stream-output-index) byte)
           (incf si:stream-output-index)))
    (declare (inline put-byte))
    (unless (and si:stream-output-buffer
                 (< 2 si:stream-output-index si:stream-output-limit)
                 (= (aref si:stream-output-buffer (1- si:stream-output-index)) #.(char-to-ascii #\linefeed))
                 (= (aref si:stream-output-buffer (- si:stream-output-index 2)) #.(char-to-ascii #\return)))
      (put-byte #.(char-to-ascii #\return))
      (put-byte #.(char-to-ascii #\linefeed)))
    t))

(defmethod (ascii-output-mode modal-ascii-translating-output-stream-mixin) ()
  (ecase output-mode
    ((:binary :crlf)
      (setq output-mode :ascii))
    (:ascii)))

(defmethod (binary-output-mode modal-ascii-translating-output-stream-mixin) ()
  (ecase output-mode
    ((:ascii :crlf)
      (setq output-mode :binary))
    (:binary)))

(defmethod (crlf-output-mode modal-ascii-translating-output-stream-mixin) ()
  (ecase output-mode
    ((:ascii :binary)
     (setq output-mode :crlf))
    (:crlf)))

(defmethod (output-mode modal-ascii-translating-output-stream-mixin) ()
  output-mode)

(defmethod (set-output-mode modal-ascii-translating-output-stream-mixin) (mode)
  (ecase mode
    (:ascii (ascii-output-mode self))
    (:binary (binary-output-mode self))
    (:crlf (crlf-output-mode self))))


;;;------------------------------------------------------------------- 
;;;
;;; MODAL ASCII TRANSLATING AND BINARY INPUT STREAM
;;;

(defflavor modal-ascii-translating-buffered-input-stream-mixin
        ((at-string)                            ;indirect string into si:stream-input-buffer
         (at-start nil)                         ;line start -- ascii translation starts from here
         (at-end nil)                           ;line end -- stop reading current translated line
         (at-cr-flag)                           ;CR at previous buffer end
         (input-mode :ascii)                    ;modes: :ascii, :binary, :crlf
         ;;total bytes received over the wire before ascii translation (not including chunk meta data)
         (bytes-received 0))
        ()
  (:required-flavors si:input-stream si:basic-buffered-input-stream)
  (:required-methods :tyi :untyi)
  (:documentation :mixin "An input stream that translates characters from ASCII into Lisp
Machine character set for :TYI method when ascii-input-mode is non-null,
otherwise reads bytes."))

(defmethod (input-mode modal-ascii-translating-buffered-input-stream-mixin) ()
  input-mode)

(defmethod (bytes-received modal-ascii-translating-buffered-input-stream-mixin) ()
  bytes-received)

(defmethod (set-bytes-received modal-ascii-translating-buffered-input-stream-mixin) (value)
  (check-type value integer)
  (setq bytes-received value))

(defsetf bytes-received set-bytes-received)

(export 'bytes-received :tcp)

(defmacro-in-flavor (clear-at-cr-flag modal-ascii-translating-buffered-input-stream-mixin) (position)
  `(progn
     (when (and si:stream-input-buffer
                (= (aref si:stream-input-buffer ,position) #.(si:ascii-code #\Linefeed)))
       (setf (aref at-string ,position) #\Linefeed)
       (incf ,position))
     (setf at-cr-flag nil)))

;; inline when debugged.
;; Some overlap in translation windows between :next-input and read-input-buffer.  4/22/97 -- JCMa.
(defun-in-flavor (do-ascii-translation modal-ascii-translating-buffered-input-stream-mixin) (start end)
  (check-type start integer)
  (check-type end integer)
  (let* ((ascii-input-mode-p (eql input-mode :ascii))
         (ascii-translation-p (or ascii-input-mode-p (eql input-mode :ascii-crlf))))
    (cond (ascii-translation-p
           ;; could be changed to happen once only in :next-input-buffer, if save.
           (si:change-indirect-array at-string (si:array-type at-string) (list end) si:stream-input-buffer 0)
           (let ((buffer si:stream-input-buffer)
                 (string at-string))
             (declare (sys:array-register buffer string))
             (when at-cr-flag (clear-at-cr-flag start))
             (setq at-start start               ;move position forward
                   si:stream-input-index start)
             (loop for idx upfrom start below end
                   for ch = (aref buffer idx)
                   do (when (< ch #o040)        ; ascii space
                        (setf (aref string idx) (ascii-to-char ch))
                        (when (= ch #.(si:char-to-ascii #\Return))      ;ascii CR
                          (setq at-end (incf idx))
                          (when ascii-input-mode-p      ;will translate whole buffer in CRLF mode.
                            (setq at-cr-flag t)
                            (return)))
                        ;; UNIX and Windows systems often don't send CRLF.
                        (when (= ch #.(si:char-to-ascii #\Line-Feed))   ;ascii LF
                          (setq at-end (incf idx))
                          (when ascii-input-mode-p      ;will translate whole buffer in CRLF mode.
                            (return))))
                   finally (setq at-end end))
             #+ignore
             (format *trace-output* "~&~'bDo-ASCII-Translation:~ ~D ~D (~:C)~&" at-start at-end (aref string (1- at-end)))
             ))
          (at-cr-flag
           (clear-at-cr-flag si:stream-input-index))
          (t (setq si:stream-input-index start)))))

(defwhopper (:next-input-buffer modal-ascii-translating-buffered-input-stream-mixin)
            (&optional no-wait-p &aux new-input-buffer-p)
  (unless si:stream-input-buffer
    (setq new-input-buffer-p t)
    (multiple-value-setq (si:stream-input-buffer si:stream-input-index si:stream-input-limit)
      (continue-whopper no-wait-p))
    (when si:stream-input-buffer
      (incf bytes-received (- si:stream-input-limit si:stream-input-index))))   ;one stop counting after chunking before translation
  ;; handle ASCII translations
  (when si:stream-input-buffer
    (case input-mode
      ((:ascii :ascii-crlf)
       (cond (new-input-buffer-p
              (setq at-start si:stream-input-index
                    at-end si:stream-input-limit)
              (unless at-string
                (unless (= 8 (si:array-element-byte-size si:stream-input-buffer))
                  (error "Underlying buffer has bad element size."))
                (setq at-string (make-array 2048. :type 'si:art-string :displaced-to si:stream-input-buffer)))
              ;; Set up the indirect array only once when new input buffers arrive.
              #+ignore(si:change-indirect-array at-string (si:array-type at-string)
                                        (list (array-total-size si:stream-input-buffer)) si:stream-input-buffer 0))
             (t (setq at-start (or at-end si:stream-input-index)
                      at-end si:stream-input-limit)))
       ;; Perform ASCII translations
       (do-ascii-translation at-start at-end))))
  ;; always return the binary buffer because :SETUP-NEXT-INPUT-BUFFER binds it.
  (values si:stream-input-buffer si:stream-input-index si:stream-input-limit))

#+ignore
(defmacro with-read-buffer-traced ((host) &body form)
  `(flet ((host-name ()
            (scl:send (scl:send self :foreign-host) :mail-name)))
     (progn (when (and http:*debug-client*
                       (equalp (host-name) ,host))
              (format *trace-output* "~&~v>>~:[~;*~]Index: ~S Limit: ~S Input Mode: ~A At-Start: ~S At-End: ~S AT-CR: ~S Chunking: ~S Host: ~A~"
                      '(nil nil :small) si:stream-input-buffer si:stream-input-index (and (boundp-in-instance self 'si:stream-input-limit) si:stream-input-limit)
                      (input-mode self) at-start at-end at-cr-flag (sys:symeval-in-instance scl:self 'chunked-input)
                      (host-name)))
            (multiple-value-prog1
              ,@form
              (when (and http:*debug-client*
                         (equalp (host-name) ,host))

                #+ignore(when (and (= si:stream-input-index 22) (= si:stream-input-limit 79))
                          (break "foo"))
                (format *trace-output* "~&~v<<~:[~;*~]Index: ~S Limit: ~S Input Mode: ~A At-Start: ~S At-End: ~S AT-CR: ~S Chunking: ~S~"
                        '(nil nil :small) si:stream-input-buffer si:stream-input-index
                        (and (boundp-in-instance self 'si:stream-input-limit) si:stream-input-limit)
                        (input-mode self) at-start at-end at-cr-flag (sys:symeval-in-instance scl:self 'chunked-input)))))))

(defwhopper (:read-input-buffer modal-ascii-translating-buffered-input-stream-mixin) (&optional eof no-hang-p)
  (macrolet ((handle-buffer-return (&body body)
               `(multiple-value-bind (buff index limit)
                    ,@body
                  (if buff
                      (values at-string si:stream-input-index at-end)
                      (values buff index limit)))))
    (ecase input-mode
      ((:ascii :ascii-crlf)
       (cond ((null si:stream-input-buffer)
              (handle-buffer-return
                (continue-whopper eof no-hang-p)))
             ((and at-end (< si:stream-input-index at-end))     ;more chars to read in current window
              (values (and si:stream-input-buffer at-string) si:stream-input-index at-end))
             ((and at-end (< at-end si:stream-input-limit))     ;get next window
              (do-ascii-translation at-end si:stream-input-limit)
              (values (and si:stream-input-buffer at-string) si:stream-input-index at-end))
             (t (handle-buffer-return
                  (continue-whopper eof no-hang-p)))))
      ((:binary :crlf) (continue-whopper eof no-hang-p)))))

;; Make sure that :CLOSE method remains synchronized with the logic here in
;; order to avoid packet leakage.   2/3/97 -- JCMa.
(defwhopper (:discard-current-input-buffer modal-ascii-translating-buffered-input-stream-mixin) ()
  ;; prevents ASCII translation windows from being thrown away by
  ;; advance-input buffer and set-up-new-input buffer.  1/29/97 -- JCMa.
  (cond ((and si:stream-input-buffer (member input-mode '(:ascii :ascii-crlf)) (< si:stream-input-index si:stream-input-limit))
         (cond ((and (< si:stream-input-index at-end)
                     (not (< (setq si:stream-input-index at-end) si:stream-input-limit)))
                (continue-whopper))
               ((and at-cr-flag (= (1+ at-end) si:stream-input-limit))
                (setq at-cr-flag nil)
                (continue-whopper))))
        (t (continue-whopper))))

;;; When closing, we want to really discard buffer all the way, advance state
;;; past anything intermediate, lose packets on close.  This really doesn't
;;; mean that the user hasn't read all the input.  There may be one character
;;; (#o012) and at-cr-flag so that :next-input-buffer wouldn't return it.
(defmethod (:close modal-ascii-translating-buffered-input-stream-mixin :before) (&optional ignore)
  (case input-mode
    ((:ascii :ascii-crlf)
     (when (boundp-in-instance self 'si:stream-input-limit)
       (setq si:stream-input-index si:stream-input-limit
             at-end si:stream-input-limit
             at-cr-flag nil)))))

;; the default method (:listen si:basic-buffered-input-stream :default) return
;; non-null when there are errors, screwing the result.   11/28/95 -- JCMa.
(defmethod (:listen modal-ascii-translating-buffered-input-stream-mixin) ()
  (condition-case ()
       (multiple-value-bind (buffer at-eof)
           (send self :setup-next-input-buffer t nil)
         (or (not (null buffer)) at-eof))
     (error nil)))

;; Don't be confused by trailing LFs
(defwhopper (:listen modal-ascii-translating-buffered-input-stream-mixin) ()
  (case input-mode
    (:ascii
      ;;(break "listen")
      (cond ((null si:stream-input-buffer) (continue-whopper))
            ((< si:stream-input-index at-end) t)
            ((not at-cr-flag) (continue-whopper))
            ((< si:stream-input-index si:stream-input-limit)
             (clear-at-cr-flag si:stream-input-index)
             (continue-whopper))
            (t (continue-whopper)
               (clear-at-cr-flag si:stream-input-index)
               (and si:stream-input-buffer (< si:stream-input-index si:stream-input-limit)))))
    (t (continue-whopper))))

;;; Note: With a little more state, this could reposition to anyplace
;;; inside the current STREAM-INPUT-BUFFER. We don't have that state now.
(defmethod (:set-buffer-pointer modal-ascii-translating-buffered-input-stream-mixin) (ignore)
  (error ":SET-POINTER outside of the current buffer is not supported for ~
          modal-ascii-translating-buffered-input-stream-mixin"))

(defmacro-in-flavor (tyi-internal modal-ascii-translating-buffered-input-stream-mixin) (no-hang-p eof peek-p)
  `(ecase input-mode
     ((:ascii :ascii-crlf)
      (unless (and si:stream-input-buffer (< si:stream-input-index at-end))
        (send self :setup-next-input-buffer ,no-hang-p ,eof))
      (when si:stream-input-buffer
        ,(if peek-p
             `(aref at-string si:stream-input-index)
             `(prog1 (aref at-string si:stream-input-index)
                     (incf si:stream-input-index)))))
     ((:binary :crlf)
       (unless (and si:stream-input-buffer (< si:stream-input-index si:stream-input-limit))
         (send self :setup-next-input-buffer ,no-hang-p ,eof))
       (when si:stream-input-buffer
         ,(if peek-p
              `(aref si:stream-input-buffer si:stream-input-index)
              `(prog1 (aref si:stream-input-buffer si:stream-input-index)
                      (incf si:stream-input-index)))))))

(defmethod (:tyi modal-ascii-translating-buffered-input-stream-mixin) (&optional eof)
  (tyi-internal nil eof nil))

(defmethod (:tyi-no-hang modal-ascii-translating-buffered-input-stream-mixin) (&optional eof)
 (tyi-internal t (or eof t) nil))

(defmethod (:tyipeek modal-ascii-translating-buffered-input-stream-mixin) (&optional eof)
  (tyi-internal nil eof t))

(defmethod (:untyi modal-ascii-translating-buffered-input-stream-mixin) (ch)
  (ecase input-mode
    ((:ascii :ascii-crlf)
      (cond ((and si:stream-input-buffer
                  (> si:stream-input-index 0)
                  (eql (aref at-string (1- si:stream-input-index)) ch))
             (decf si:stream-input-index))
            (t (error "Attempt to :UNTYI something different than last :TYI'ed."))))
    ((:binary :crlf)
      (cond ((and si:stream-input-buffer
                  (> si:stream-input-index 0)
                  (eql (aref si:stream-input-buffer (1- si:stream-input-index)) ch))
             (decf si:stream-input-index))
            (t (error "Attempt to :UNTYI something different than last :TYI'ed."))))))

(defun-in-flavor (untranslate-window modal-ascii-translating-buffered-input-stream-mixin)
                 (at-buffer at-string start end)
  (let ((buffer at-buffer)
        (string at-string))
    (declare (sys:array-register buffer string))
    (loop for idx downfrom (1- end) to start
          for char = (aref string idx)
          for byte = (si:char-to-ascii char)
          when (< byte #o040)
            do (setf (aref buffer idx) byte))))

(defmethod (ascii-input-mode modal-ascii-translating-buffered-input-stream-mixin) ()
  (flet ((new-input-index ()
	   (and si:stream-input-index
		si:stream-input-limit
		(< si:stream-input-index si:stream-input-limit)
		si:stream-input-index)))
    (declare (inline new-input-index))
    (ecase input-mode
      (:ascii)
      ((:binary :crlf)
       (prog1 (setq input-mode :ascii)
	      (send self :advance-input-buffer (new-input-index))))
      (:ascii-crlf ;;keep one translated window and undo the rest so standard ascii translation can work.
	(let ((next-cr (find #\return at-string :test #'eql :start si:stream-input-index :end si:stream-input-limit)))
	  (cond (next-cr
		 (setq at-end (1+ next-cr)
		       at-cr-flag t)
		 (untranslate-window si:stream-input-buffer at-string at-end si:stream-input-limit))
		(t (setq at-cr-flag si:stream-input-limit)))
	  (setq at-start si:stream-input-index
		input-mode :ascii))))))

;;The flag is there to tell you if you're seeing a CRLF which you translate to
;;CR.  Switching to binary mode would suggest that you're not at a CR in CRLF.
;;This is true in current http but might change with multiplexed streams?  I
;;changed it to clear the flag for now.
(defmethod (binary-input-mode modal-ascii-translating-buffered-input-stream-mixin) ()
  (ecase input-mode
    (:ascii
      (cond ;; revert translated bytes back to the index.
        ((and si:stream-input-index at-end (< si:stream-input-index at-end))
         (untranslate-window si:stream-input-buffer at-string si:stream-input-index at-end))
        (at-cr-flag ;; gobble any dangling LF
         (clear-at-cr-flag si:stream-input-index)))
      (setq input-mode :binary))
    (:ascii-crlf
      (when (and si:stream-input-index at-end (< si:stream-input-index at-end))
        (untranslate-window si:stream-input-buffer at-string si:stream-input-index at-end))
      (setq input-mode :binary))
    (:crlf (setq input-mode :binary))
    (:binary)))

(defmethod (crlf-input-mode modal-ascii-translating-buffered-input-stream-mixin) ()
  (ecase input-mode
    (:ascii
      (cond ;; revert translated bytes back to the index.
        ((and si:stream-input-index at-end (< si:stream-input-index at-end))
         (untranslate-window si:stream-input-buffer at-string si:stream-input-index at-end))
        (at-cr-flag ;; gobble any dangling LF
         (clear-at-cr-flag si:stream-input-index)))
      (setq input-mode :crlf))
    (:ascii-crlf
      (when (< si:stream-input-index at-end)
        (untranslate-window si:stream-input-buffer at-string si:stream-input-index at-end))
      (setq input-mode :crlf))
    (:binary (setq input-mode :crlf))
    (:crlf)))

(defmethod (ascii-crlf-input-mode modal-ascii-translating-buffered-input-stream-mixin) ()
  (ecase input-mode
    (:ascii
      (setq input-mode :ascii-crlf)
      (when at-cr-flag  ;; gobble any dangling LF
        (send self :advance-input-buffer)
        (clear-at-cr-flag si:stream-input-index)))
    ((:binary :crlf)
     (setq input-mode :ascii-crlf)
     (send self :advance-input-buffer si:stream-input-index))
    (:ascii-crlf)))

(defmethod (set-input-mode modal-ascii-translating-buffered-input-stream-mixin) (mode)
  (ecase mode
    (:ascii (ascii-input-mode self))
    (:binary (binary-input-mode self))
    (:crlf (crlf-input-mode self))
    (:ascii-crlf (ascii-crlf-input-mode self))))


;;;------------------------------------------------------------------- 
;;;
;;; COMPATIBILITY FOR OTHER LISPM STREAM CLASSES
;;;

(defmethod (ascii-input-mode si:character-stream) ()
  t)

(defmethod (binary-input-mode si:character-stream) ()
  (error "Don't know how to put ~S into binary input mode." self))

(defmethod (crlf-input-mode si:character-stream) ()
  (error "Don't know how to put ~S into CRLF input mode." self))

(defmethod (ascii-input-mode si:binary-stream) ()
 (error "Don't know how to put ~S into ascii input mode." self) t)

(defmethod (binary-input-mode si:binary-stream) ()
  t)

(defmethod (crlf-input-mode si:binary-stream) ()
  (error "Don't know how to put ~S into CRLF input mode." self))


;;;------------------------------------------------------------------- 
;;;
;;; HTTP 1.1 CHUNKED TRANFER ENCODING OUTPUT STREAM
;;;

(eval-when (compile eval load)
(defun string-to-8-bit-vector (string &optional (start 0) (end (length string)))
  (loop with length = (- end start)
        with vector = (make-array length :element-type '(unsigned-byte 8))
        for idx1 upfrom start below end
        for idx2 upfrom 0
        do (setf (aref vector idx2) (si:ascii-code (aref string idx1)))
        finally (return (values vector length)))))

(defun 8-bit-vector-to-string (vector &optional (start 0) (end (length vector)))
  (loop with length = (- end start)
        with string = (make-array length :element-type 'scl:string-char)
        for idx1 upfrom start below end
        for idx2 upfrom 0
        do (setf (aref string idx2) (si:ascii-to-char (aref vector idx1)))
        finally (return (values string length))))

(proclaim '(inline chunk-size-integer chunk-size-string))

(defun chunk-size-integer (hexidecimal &optional (start 0) (end (length hexidecimal)))
  "Decodes the size of a chunked transfer encoding from HEXIDECIMAL to an integer."
  (parse-integer hexidecimal :radix 16. :junk-allowed nil :sign-allowed nil :start start :end end))

(eval-when (compile eval load)
(defun chunk-size-string (size)
  "Encodes the size of a chunk for chunked transfer encoding from SIZE, an integer, to hexidecimal."
  (declare (values hexidecimal-size hexidecimal-digits))
  (let ((hex (write-to-string size :base 16. :readably nil)))
    (values hex (length hex)))))

;; pick the maximum 4-digit hex, FFFF
(eval-when (compile eval load)

(defconstant *default-chunk-size-vector* #.(string-to-8-bit-vector (chunk-size-string 65535)))

(defconstant *default-chunk-size-vector-length* #.(length (string-to-8-bit-vector (chunk-size-string 65535))))

(defconstant *8-bit-crlf* #.(string-to-8-bit-vector (coerce '(#\return #\linefeed) 'string))))

(proclaim '(inline write-8-bit-crlf write-chunk-size-header write-chunk-end-header))

(defun write-8-bit-crlf (vector &optional (start 0) (end (length vector)))
  (copy-array-portion *8-bit-crlf* 0 2 vector start end))

(defun write-chunk-size-header (vector &optional (start 0) (end (length vector)))
  (copy-array-portion *default-chunk-size-vector* 0 *default-chunk-size-vector-length* vector start end))

(defun insert-chunk-size (size vector &optional (start 0) (limit (length vector)))
  (multiple-value-bind (hex hex-digits)
      (chunk-size-string size)
    (declare (fcl-user::dynamic-extent hex))
    (unless (<= hex-digits limit)
      (error "Insufficient space in content-length header. ~D was allowed but ~D is needed." limit hex-digits))
    (let ((hex hex)
          (vector vector))
      (declare (sys:array-register hex vector))
      (loop for idx1 upfrom 0 below hex-digits
            for idx2 upfrom start
            for byte = (si:ascii-code (aref hex idx1))
            do (setf (aref vector idx2) byte)
            finally (loop for idx3 upfrom (1+ idx2) upto (+ idx2 (- limit hex-digits))  ;pad with spaces
                          do (setf (aref vector idx3) #o040))))
    size))

(defflavor chunk-transfer-encoding-output-stream-mixin
        ((chunk-output nil)                     ;whether output chunking is on or off
         (chunk-start 0)
         (chunk-end 0)
         (chunk-body-start 0)
         (chunk-body-end 0)
         (chunk-body-size 0)
         (content-length-start 0)
         (chunks-transmitted 0)
         (chunk-function nil))
        ()
  :abstract-flavor
  (:required-flavors si:buffered-tyo-output-stream)
  (:required-methods :tyo)
  (:documentation :mixin "An output stream that provides chunked transfer encoding output on an HTTP stream."))

(defun-in-flavor (%note-chunk-body-start chunk-transfer-encoding-output-stream-mixin) ()
  (setq content-length-start si:stream-output-index)
  (write-chunk-size-header si:stream-output-buffer
                           si:stream-output-index (incf si:stream-output-index  *default-chunk-size-vector-length*))
  (write-8-bit-crlf si:stream-output-buffer si:stream-output-index (incf si:stream-output-index 2))
  (setq chunk-end si:stream-output-limit
        si:stream-output-limit (- si:stream-output-limit 2)
        chunk-body-start si:stream-output-index))

(defun-in-flavor (%note-body-end chunk-transfer-encoding-output-stream-mixin) (buffer output-index)
  (declare (values new-output-index))
  (setq chunk-body-end output-index)
  (insert-chunk-size (- chunk-body-end chunk-body-start)
                     buffer content-length-start *default-chunk-size-vector-length*)
  (write-8-bit-crlf buffer output-index (incf output-index 2))
  (setq si:stream-output-limit chunk-end)
  (when chunk-function
    (funcall chunk-function buffer chunk-body-start chunk-body-end))
  (incf chunks-transmitted)
  (values output-index))

(defmethod (chunk-transfer-encoding-mode chunk-transfer-encoding-output-stream-mixin) (&optional function)
  (send self :setup-new-output-buffer)          ;clear leading buffer space for now.
  (check-type function (or null function))
  (setq chunk-output t
        chunk-function function
        chunks-transmitted 0
        chunk-start si:stream-output-index))

(defmethod (note-first-chunk chunk-transfer-encoding-output-stream-mixin) ()
  (%note-chunk-body-start))

(defmethod (note-last-chunk chunk-transfer-encoding-output-stream-mixin) (&optional footers-plist)
  ;; Make sure that there is an output buffer to avoid fencepost errors  1/23/97 -- JCMa.
  (unless si:stream-output-buffer (send self :setup-new-output-buffer))
  ;; Prevent chunked output buffer setup from repeating and prevent send output from reinserting chunk args
  (setq chunk-output nil)
  (cond ;; if no data written to body yet, convert to end header
    ((= chunk-body-start si:stream-output-index)
     (unless (< 3 (- si:stream-output-limit si:stream-output-index)) (send self :setup-new-output-buffer))
     (setf (aref si:stream-output-buffer si:stream-output-index) #.(si:ascii-code #\0))
     (write-8-bit-crlf si:stream-output-buffer (incf si:stream-output-index 1) (incf si:stream-output-index 2)))
    (t (multiple-value-setq (si:stream-output-index)
         (%note-body-end si:stream-output-buffer si:stream-output-index))
       (setf (aref si:stream-output-buffer si:stream-output-index) #.(si:ascii-code #\0))
       (incf si:stream-output-index)
       (write-8-bit-crlf si:stream-output-buffer si:stream-output-index (incf si:stream-output-index 2))))
  ;; write the footers
  (http::write-headers self footers-plist t))

(defwhopper (:setup-new-output-buffer chunk-transfer-encoding-output-stream-mixin) ()
  (prog1 (continue-whopper)
         (when chunk-output
           (setq chunk-start si:stream-output-index)
           (%note-chunk-body-start))))

(defwhopper (:send-output-buffer chunk-transfer-encoding-output-stream-mixin) (seg limit explicit)
  (cond ((null chunk-output)
         (continue-whopper seg si:stream-output-index explicit))
        ((zerop (- limit chunk-body-start))     ;don't send zero sized chunks
         (values seg si:stream-output-index chunk-end))
        (t (multiple-value-setq (si:stream-output-index)
             (%note-body-end seg limit))
           (continue-whopper seg si:stream-output-index explicit))))


;;;------------------------------------------------------------------- 
;;;
;;; CHUNKED TRANSFER DECODING
;;;

(defflavor chunk-transfer-decoding-input-stream-mixin
        ((chunked-input nil)                    ;whether output input-chunking is on or off
         (old-mode nil)                         ;previous input mode
         (input-content-length 0)               ;total content length of input
         (input-chunk-size 0)                   ;total length of chunk
         (input-chunk-size-vector nil)          ;resource for reading chunk size
         (input-chunk-args-vector nil)          ;resource for reading chunk size args
         (input-scan-start 0)                   ;position where buffer scan starts
         (input-scan-end 0)                     ;position where buffer scan ends
         (input-scan-length 0)                  ;length of buffer scan
         (input-chunk-content-length 0)         ;current content length
         (input-chunk-crosses-buffer-p nil)     ;whether chunk crosses packet boundaries
         (input-buffer-limit 0)                 ;limit position for input buffer
         (input-chunks-received 0))		;number of chunks received
        ()
  :abstract-flavor
  (:required-flavors si:si:buffered-input-stream modal-ascii-translating-buffered-input-stream-mixin)
  (:required-methods :tyo)
  (:documentation :mixin "An input stream that provides chunked transfer decoding input on an HTTP stream."))

(defflavor chunk-transfer-decoding-error
        (stream
	 (format-string "Error decoding chunk transfer encoded input.")
	 (format-args nil))
        (sys:connection-error)
  :initable-instance-variables
  :gettable-instance-variables
  (:documentation :error "Signalled when an error is encountered during HTTP 1.1 chunk transfer decoding."))

(defmethod (:network chunk-transfer-decoding-error) ()
  (send stream :network))

(defmethod (dbg:report chunk-transfer-decoding-error) (stream)
  (apply #'format stream format-string format-args))

(defmethod (chunk-transfer-content-length chunk-transfer-decoding-input-stream-mixin) ()
  (if chunked-input
      input-content-length
      (error "~S is not in chunked transfer decoding mode." self)))

#+ignore
(defmacro with-chunk-transfer-decoding-traced (&body body)
  `(case http:*debug-client*
     (:chunk-transfer-decoding ,@body)))

(defmacro with-chunk-transfer-decoding-traced (&body body)
  (declare (ignore body))
  `(progn))

(defmethod (format-position modal-ascii-translating-buffered-input-stream-mixin) (&optional (stream *standard-output*))
  (flet ((saref (array position)
               (if (< position (array-dimension array 0))
                 (aref array position)
                 #\circle)))
    (declare (inline saref))
  (format stream "~&~'bAt-Start:~ ~D ~'bAt-End:~ ~D ~'bIndex:~ ~D ~'bLimit:~ ~D~
                  ~&~'bStart:~ ~:C ~'bEnd:~ ~:C ~'bIndex:~ ~:C"
          at-start at-end si:stream-input-index si:stream-input-limit
          (saref at-string at-start) (saref at-string at-end) (saref at-string si:stream-input-index))))

(defconstant *input-chunk-size-hex-vector-size* 6
  "Controls the standard size of the hex vector resource used to read http chunk sizes.")

(defun-in-flavor (allocate-input-chunk-size-hex-vector chunk-transfer-decoding-input-stream-mixin) ()
  (let ((vector (or input-chunk-size-vector
		    (let ((v (make-array *input-chunk-size-hex-vector-size* :element-type 'scl:string-char :adjustable t :fill-pointer t
					 :area (sys:%area-number self))))
		      (setf input-chunk-size-vector v)
		      v))))
    (setf (fill-pointer vector) 0)
    vector))

(defparameter *input-chunk-args-vector-size* 130
   "Controls the standard size of the vector resource used to read HTTP chunk  arguments.") 

(defun-in-flavor (allocate-input-chunk-args-vector chunk-transfer-decoding-input-stream-mixin) ()
  (let ((vector (or input-chunk-args-vector
		    (let ((v (make-array *input-chunk-args-vector-size* :element-type 'scl:string-char :adjustable t
					 :fill-pointer t :area (sys:%area-number self))))
		      (setf input-chunk-args-vector v)
		      v))))
    (setf (fill-pointer vector) 0)
    vector))

(defun grow-chunk-string (string &optional (delta 10) (size (array-total-size string)))
  (declare (values new-string new-size)
           (fixnum delta size))
  (flet ((element-type (array)
           (typecase (array-element-type array)
             (character 'scl:string-char)
             (t '(unsigned-byte 8)))))
    (declare (inline element-type))
    (let ((n-size (+ size delta)))
      (values (adjust-array string n-size :element-type (element-type string) :fill-pointer t)
              n-size))))

;; internal method for setting up a new physical input buffer inside SETUP-NEXT-INPUT-BUFFER  3/19/99 -- JCMa.
(defun-in-flavor (%setup-next-physical-input-buffer chunk-transfer-decoding-input-stream-mixin) (&optional no-hang-p eof)
  (loop with at-eof
	doing (send self :discard-current-input-buffer)
	      (multiple-value-setq (si:stream-input-buffer si:stream-input-index si:stream-input-limit at-eof)
		(send self :next-input-buffer no-hang-p))
	      (with-chunk-transfer-decoding-traced
		(format *trace-output* "~&~'b~&New Physical Input Buffer: ~S idx= ~D Limit= ~D.~~&"
			si:stream-input-buffer si:stream-input-index si:stream-input-limit))
	when (and (null si:stream-input-buffer) eof (or at-eof (not no-hang-p)))
	  do (error 'si:end-of-file :stream self :format-string eof)
	while (and si:stream-input-buffer
		   ( si:stream-input-index si:stream-input-limit)
		   (not at-eof))))

(defun-in-flavor (%read-byte chunk-transfer-decoding-input-stream-mixin) ()
  (unless (< si:stream-input-index si:stream-input-limit)
    (%setup-next-physical-input-buffer nil t))
  (prog1 (aref si:stream-input-buffer si:stream-input-index)
	 (incf si:stream-input-index)))

(defun-in-flavor (%read-cr chunk-transfer-decoding-input-stream-mixin) ()
  (with-chunk-transfer-decoding-traced
    (format *trace-output* "~&~'b~&Read:~ cr (~'BIndex:~ ~D)" si:stream-input-index))
  (unless (< si:stream-input-index si:stream-input-limit)
    (%setup-next-physical-input-buffer nil t))
  (cond ((= (aref si:stream-input-buffer si:stream-input-index) #.(si:char-to-ascii #\return))
         (incf si:stream-input-index)
         t)
        (t (error 'chunk-transfer-decoding-error :stream self
		  :format-string "~@C was found when ~@C  was expected in chunked transfer decoding."
		  :format-args (list (ascii-to-char (aref si:stream-input-buffer si:stream-input-index)) #\return)))))

(defun-in-flavor (%read-lf chunk-transfer-decoding-input-stream-mixin) ()
  (with-chunk-transfer-decoding-traced
    (format *trace-output* "~&~'b~&Read:~ LF (~'BIndex:~ ~D)" si:stream-input-index))
  (unless (< si:stream-input-index si:stream-input-limit)
    (%setup-next-physical-input-buffer nil t))
  (cond ((= (aref si:stream-input-buffer si:stream-input-index) #.(si:char-to-ascii #\linefeed))
         (incf si:stream-input-index)
         t)
        (t (error 'chunk-transfer-decoding-error :stream self
		  :format-string "~@C was found when ~@C  was expected in chunked transfer decoding."
		  :format-args (list (ascii-to-char (aref si:stream-input-buffer si:stream-input-index)) #\linefeed)))))

(defun-in-flavor (read-chunk-size chunk-transfer-decoding-input-stream-mixin) ()
  (declare (values integer-size byte next-index hex-vector))
  (let* ((hex-vector (allocate-input-chunk-size-hex-vector))
	 (hex-vector-size (array-total-size hex-vector)))
    (declare (si:array-register hex-vector))
    (loop with idx = 0
          for byte = (%read-byte)
	  until (member byte '#.(mapcar #'si:ascii-code '(#\; #\return #\linefeed)) :test #'=)
          unless (< idx hex-vector-size)
            do (let ((n-size (+ hex-vector-size 10.)))
                 (setq hex-vector (adjust-array hex-vector n-size :element-type 'character :fill-pointer t)
                       hex-vector-size n-size))
          unless (= byte #.(si:ascii-code #\space))
            do (unless (member byte '#.(map 'list #'si:char-to-ascii "0123456789abcdefABCDEF"))
		 (error 'chunk-transfer-decoding-error :stream self
			:format-string "Non-hexadecimal digit ~@C found in chunk size." :format-args (list (si:ascii-to-char byte))))
	       (setf (aref hex-vector idx) (si:ascii-to-char byte))
               (incf idx)
          finally (setf (fill-pointer hex-vector) idx)
		  (when (zerop idx)
		    (error 'chunk-transfer-decoding-error :stream self
			   :format-string "No hexadecimal chunk size found in chunk header."))
		  (let ((chunk-size (chunk-size-integer hex-vector)))
		    (with-chunk-transfer-decoding-traced
		      (format *trace-output* "~&~'bChunk-Size:~ ~D ~'bNext-Index:~ ~D ~'bHex:~ ~S~&"
			      chunk-size si:stream-input-index (substring hex-vector 0 idx)))
		    (return (values chunk-size byte))))))

(defun-in-flavor (parse-chunk-size-arguments chunk-transfer-decoding-input-stream-mixin) (byte)
  (declare (values args-plist))
  (with-chunk-transfer-decoding-traced
    (format *trace-output* "~&~'bParse-Chunk-Size-Args-Start:~ ~D~&" si:stream-input-index)
    (format *trace-output* "~&~'bByte1:~ ~:C" (si:ascii-to-char byte)))
  (let ((vector (allocate-input-chunk-args-vector)))
    (declare (si:array-register vector))
    (prog1
      (ecase byte
	(#.(char-to-ascii #\Return)
	 (%read-lf)
	 nil)
	(#.(char-to-ascii #\Linefeed)
	 nil)
	(#.(char-to-ascii #\;)
	 (loop with idx = (fill-pointer vector)
	       with vector-size = (array-total-size vector)
	       for byte = (%read-byte)
	       until (member byte '#.(mapcar #'char-to-ascii '(#\Return #\Linefeed)))
	       do (unless (< idx vector-size)
		    (multiple-value-setq (vector vector-size)
		      (grow-chunk-string vector 100. vector-size)))
	       do (setf (aref vector idx) (code-char byte))
		  (incf idx)
	       finally (with-chunk-transfer-decoding-traced
			 (format *trace-output* "~&~'bByte2:~ ~:C" (si:ascii-to-char byte)))
		       (ecase byte
			 (#.(char-to-ascii #\Return)
			  (%read-lf))
			 (#.(char-to-ascii #\Linefeed)))
		       (setf (fill-pointer vector) idx)
		       (return (cond ((zerop idx) nil)
				     (t (setf (fill-pointer vector) idx)
					(http::parse-equal-sign-delimited-pairs vector 0 idx #\; nil)))))))
      (with-chunk-transfer-decoding-traced
	(format *trace-output* "~&~'bParse-Chunk-Size-Args-End:~ ~D~&" si:stream-input-index)))))

(defun-in-flavor (%read-chunk-size chunk-transfer-decoding-input-stream-mixin) ()
  (declare (values chunk-size chunk-args-plist))
  (multiple-value-bind (chunk-size byte)
      (read-chunk-size)
    (values chunk-size (parse-chunk-size-arguments byte))))

#|

(defun test-read-chunk-size (string &optional end)
  (let ((v (string-to-8-bit-vector string)))
    (multiple-value-bind (integer-size byte next-index hex-vector)
        (read-chunk-size v 0 (or end (length v)))
      (format *trace-output* "~&Size: ~D~&Byte: ~:C~&Next-Index: ~D~&Hex-Vector: ~S"
              integer-size (si:ascii-to-char byte) next-index hex-vector))))

(defun test-parse-chunk-size-args (string &optional end)
  (let* ((v (string-to-8-bit-vector string)))
    (or end (setq end (length v)))
    (multiple-value-bind (integer-size byte next-index hex-vector)
        (read-chunk-size v 0 (or end (length v)))
      (format *trace-output* "~&Size: ~D~&Byte: ~:C~&Next-Index: ~D~&Hex-Vector: ~S"
              integer-size (si:ascii-to-char byte) next-index hex-vector)
      (when integer-size
        (multiple-value-bind (chunk-size-args byte n-index)
            (parse-chunk-size-arguments byte v next-index end)
          (format *trace-output* "~&Chunk-Args: ~S~&Byte: ~:C~&Size: ~D~&Next-Index: ~D"
                  chunk-size-args (si:ascii-to-char byte) (length v) n-index)))
      nil)))

(test-read-chunk-size "12345

" 8)

(test-parse-chunk-size-args "12345

")

|#

(defflavor end-of-chunk-transfer-decoding
        ()
        (fcl-user::end-of-file)
  (:documentation :condition "Signalled when a complete HTTP resource has been successfully transferred."))

(defmethod (dbg:report end-of-chunk-transfer-decoding) (stream)
  (format stream "End of chunk transfer encoded input."))

(defun-in-flavor (%note-chunk-start chunk-transfer-decoding-input-stream-mixin) ()
  (multiple-value-bind (chunk-size chunk-args)
      (%read-chunk-size)
    (declare (ignore chunk-args))
    (cond ((zerop chunk-size)
	   ;;(break "foo")
	   (setq input-chunk-size 0
		 at-end si:stream-input-index)	;pretend last ascii translation window ended here to start the right place.              
	   (signal 'end-of-chunk-transfer-decoding :stream self))
	  (t (setq input-chunk-size chunk-size
		   at-end si:stream-input-index	;pretend last ascii translation window ended here to start the right place.
		   input-scan-start si:stream-input-index
		   input-scan-end (min (+ si:stream-input-index input-chunk-size) si:stream-input-limit)
		   input-scan-length (- input-scan-end input-scan-start)
		   input-content-length (+ input-content-length input-chunk-size)
		   input-chunk-content-length input-scan-length
		   input-chunk-crosses-buffer-p (> input-chunk-size input-chunk-content-length)
		   input-buffer-limit si:stream-input-limit
		   si:stream-input-limit input-scan-end)
	     (with-chunk-transfer-decoding-traced
	       (format *trace-output* "~&~'bScan-Start:~ ~D ~'bScan-End:~ ~D  ~:[~;~'bCross-buffer:~ yes~]~&"
		       input-scan-start input-scan-end input-chunk-crosses-buffer-p))))))

(defgeneric input-chunk-size (tcp-modal-http-stream)
  (declare (values chunk-size))
  (:documentation "Returns the size in bytes of the current input chunk.
The stream must be in chunk transfer decoding mode or an error is signalled."))

(defmethod (input-chunk-size chunk-transfer-decoding-input-stream-mixin) ()
  (if chunked-input
      input-chunk-size
      (error "The HTTP stream, ~S, was not in chunk transfer decoding mode." self)))

(export 'input-chunk-size :tcp)

(defmacro %with-binary-mode-for-chunk-transfer-decoding-input ((stream) &body body)
  `(let ((old-mode input-mode))
     (case old-mode
       (:binary . ,body)
       (t (unwind-protect
	      (progn
		(set-input-mode ,stream :binary)
		. ,body)
	    (set-input-mode ,stream old-mode))))))

(defmethod (chunk-transfer-decoding-mode chunk-transfer-decoding-input-stream-mixin) ()
  (when (and at-cr-flag si:stream-input-buffer (< si:stream-input-index si:stream-input-limit))
    (clear-at-cr-flag si:stream-input-index))
  ;; the chunk may start in the next TCP packet after the headers.
  (unless (and si:stream-input-buffer (< si:stream-input-index si:stream-input-limit))
    (send self :setup-next-input-buffer nil t)
    ;; clear any in the next buffer
    (when at-cr-flag
      (clear-at-cr-flag si:stream-input-index)))
  (setq chunked-input t
        input-content-length 0
        input-scan-length 0)
  (%with-binary-mode-for-chunk-transfer-decoding-input (self)
    (%note-chunk-start)))

(defmethod (chunk-transfer-decoding-mode-end chunk-transfer-decoding-input-stream-mixin) ()
  (setq chunked-input nil
        
        input-chunk-content-length 0
        input-chunk-crosses-buffer-p nil
        input-buffer-limit 0))

(defun-in-flavor (%note-chunk-continue chunk-transfer-decoding-input-stream-mixin) ()
  (setq at-end si:stream-input-index            ;pretend last ascii translation window ended here to start the right place.
        input-scan-start si:stream-input-index
        input-scan-end (min (+ si:stream-input-index (- input-chunk-size input-chunk-content-length)) si:stream-input-limit)
        input-scan-length (- input-scan-end input-scan-start)
        input-chunk-content-length (+ input-chunk-content-length input-scan-length)
        input-chunk-crosses-buffer-p (> input-chunk-size input-chunk-content-length)
        input-buffer-limit si:stream-input-limit
        si:stream-input-limit input-scan-end))

;;; Because ASCII translation occurs in a :next-input-buffer whopper, chunking
;;; must provide the correct window before that happens lest translation
;;; change a CR delimiting the end of the chunk. This pathology occurs only
;;; when chunks cross packet boundaries, and is especially frequent when text
;;; is shipped with only LF boundaries.   4/22/97 -- JCMa.
(defwhopper (:next-input-buffer chunk-transfer-decoding-input-stream-mixin) (&optional no-wait-p &aux at-eof)
  (cond ((null chunked-input)
         (continue-whopper no-wait-p))
        (input-chunk-crosses-buffer-p
         (multiple-value-setq (si:stream-input-buffer si:stream-input-index si:stream-input-limit at-eof)
           (continue-whopper nil))		;always wait for data, or synchronization will be lost.   5/28/99 -- JCMa.
	 (when at-eof
	   (let ((tcb (sys:symeval-in-instance self 'tcb)))
	     (if tcb
		 (error 'tcp-connection-no-more-data :connection tcb :reason "closing during an HTTP chunked read")
		 (error 'tcp-stream-closed :attempt "HTTP chunked read from" :stream self))))
	 (unless si:stream-input-buffer
	   (error "No data available while reading HTTP chunked input."))
         (%note-chunk-continue)
         ;; Update by withheld buffer window
         (incf (bytes-received self) (- input-buffer-limit input-scan-end))
         ;; Always return the binary buffer because :SETUP-NEXT-INPUT-BUFFER binds it.
         (values si:stream-input-buffer si:stream-input-index input-scan-end at-eof))
        (t (continue-whopper no-wait-p))))

(defun-in-flavor (%note-chunk-end chunk-transfer-decoding-input-stream-mixin) ()
  (setq si:stream-input-limit input-buffer-limit)
  (with-chunk-transfer-decoding-traced
    (format *trace-output* "~&~'bChunk End~ ~'bNext-Index:~ ~D ~'bBuffer-Limit:~ ~D ~'bBuffer-Window:~ ~D"
	    si:stream-input-index si:stream-input-limit (- si:stream-input-limit si:stream-input-index)))
  (%read-cr)
  (%read-lf)
  (values si:stream-input-buffer nil))

(defwhopper (:setup-next-input-buffer chunk-transfer-decoding-input-stream-mixin) (no-hang-p eof)
  (cond ((null chunked-input)
         (continue-whopper no-hang-p eof))
        (input-chunk-crosses-buffer-p
         (with-chunk-transfer-decoding-traced
           (format *trace-output* "~&~'bCross Packet Boundary~"))
         (continue-whopper no-hang-p eof))
        (t (%with-binary-mode-for-chunk-transfer-decoding-input (self)
	     (%note-chunk-end)
	     (%note-chunk-start))
           (values si:stream-input-buffer nil))))

(defwhopper (:advance-input-buffer chunk-transfer-decoding-input-stream-mixin) (&optional new-index)
  (cond ((not chunked-input) (continue-whopper new-index))
        ((or (null new-index) (= new-index input-scan-end si:stream-input-limit))
         (setq si:stream-input-index input-scan-end))
        (t (continue-whopper new-index))))

(defwhopper (:read-input-buffer chunk-transfer-decoding-input-stream-mixin) (&optional eof no-hang-p)
  (cond ((not chunked-input) (continue-whopper eof no-hang-p))
        (t (condition-case-if (or (not eof) no-hang-p) ()
                (continue-whopper eof no-hang-p)
              (end-of-chunk-transfer-decoding
                (values nil si:stream-input-index si:stream-input-limit))))))

;;;------------------------------------------------------------------- 
;;;
;;; MIX THE OUTPUT AND INPUT TOGETHER
;;;

(defflavor tcp-modal-http-stream ()
           (si:bidirectional-stream
            modal-ascii-translating-buffered-input-stream-mixin
            chunk-transfer-decoding-input-stream-mixin
            chunk-transfer-encoding-output-stream-mixin
            tcp-input-stream-mixin
            si:buffered-line-input-stream
            modal-ascii-translating-output-stream-mixin
            tcp-output-stream-mixin
            si:buffered-tyo-output-stream
           basic-tcp-stream))

(defmethod (sys:print-self tcp-modal-http-stream) (stream print-depth slashify-p)
  (declare (ignore print-depth slashify-p))
  (sys:printing-random-object (self stream :typep)
    (let ((host (send self :foreign-host)))
      (when host
        (format stream "~A" host)))))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defun make-tcp-stream
       (tcb &key (characters t) ascii-translation translation &allow-other-keys)
  (when ascii-translation
    (setf translation :ascii))
  (unwind-protect
      (let ((stream (make-instance (if characters
                                       (ecase translation
                                         (:ascii 'tcp-ascii-stream)
                                         (:modal 'tcp-modal-http-stream)
                                         (:unix 'tcp-unix-stream)
                                         ((nil) 'tcp-character-stream))
                                       (ecase translation
                                         (:ascii 'tcp-binary-ascii-stream)
                                         (:unix 'tcp-binary-unix-stream)
                                         ((nil) 'tcp-binary-stream)))
                                   :tcb tcb)))
        (setf (tcb-user-stream tcb) stream)
        (setq tcb nil)                          ;in case make-instance errs
        stream)
    (when tcb (abort-tcb tcb))))


;;;------------------------------------------------------------------- 
;;;
;;; FAST STREAM COPYING
;;;

(defun stream-copy-bytes (from-stream to-stream n-bytes &optional start)
  (check-type n-bytes integer)
  (when start
    (check-type start integer)
    (send from-stream :set-pointer start))
  (cond ;; buffer level copying
    ((and (operation-handled-p from-stream :read-input-buffer)
          (operation-handled-p to-stream :string-out))
     ;; If it can go, this mode is the most efficient by far.
     (loop with remaining = n-bytes and length 
           doing (multiple-value-bind (buffer offset limit)
                     (send from-stream :read-input-buffer)
                   (cond ((null buffer) (return nil))
                         ((< (setq length (- limit offset)) remaining)
                          (send to-stream :string-out buffer offset limit)
                          (decf remaining length)
                          (send from-stream :advance-input-buffer))
                         (t (send to-stream :string-out buffer offset (setq length (+ remaining offset)))
                            (send from-stream :advance-input-buffer length)     ;leave buffer pointer in correct position
                            (return nil))))))
    ;; resourced line level copying
    ((and (operation-handled-p from-stream :string-line-in)
          (operation-handled-p to-stream :line-out)
          ;; copying from an interval is faster using :line-in
          (not (send-if-handles from-stream :line-in-more-efficient-than-string-line-in-p)))
     ;; Not as good, but better than :line-in/:line-out
     (loop with remaining = n-bytes
           with line = (or (without-interrupts
                             (prog1 si:*stream-copy-until-eof-temp-buffer*
                                    (setq si:*stream-copy-until-eof-temp-buffer* nil)))
                           (make-array 128. :type 'art-fat-string :fill-pointer 0))
           when (multiple-value-bind (length eof neol)
                    (send from-stream :string-line-in nil line 0 (min 128. remaining))
                  (cond ((< length remaining)
                         (send to-stream (if (or neol eof) :string-out :line-out) line 0 length)
                         (decf remaining length))
                        (t (send to-stream (if (or neol eof) :string-out :line-out) line 0 remaining)
                           (setq si:*stream-copy-until-eof-temp-buffer* line)
                           (return nil)))
                  eof)
             do (signal 'sys:end-of-file)))
    ((and (operation-handled-p from-stream :line-in)
          (operation-handled-p to-stream :line-out))
     ;; Not as good, but better than :tyi/:tyo
     (loop with remaining = n-bytes
           for line = (send from-stream :line-in (min 128 remaining))
           for length = (length line)
           when (< length remaining)
             do (send to-stream :line-out line)
                (decf remaining length)
           else
             do (send to-stream :string-out line 0 remaining)
                (return nil)))
    ;; This always wins, but is incredibly slow.
    (t (loop for idx upfrom 0 below n-bytes
             for char = (send from-stream :tyi)
             when char
               do (send to-stream :tyo char)
             do (ignore idx)
                (signal 'sys:end-of-file)))))

(future-common-lisp:declaim (inline %translate-copy-mode))

(defun %translate-copy-mode (copy-mode)
  "Translates COPY-MODE from the portable CL-HTTP modes to
the Lispm TCP stream modes."
  (ecase copy-mode
    (:text :ascii)
    ((:binary :crlf) copy-mode)))

(defmacro with-output-mode ((to-stream copy-mode) &body body)
  `(let ((mode (output-mode ,to-stream))
	 (internal-copy-mode (%translate-copy-mode ,copy-mode)))
     (cond ((eq mode internal-copy-mode)
	    ,@body)
	   (t (unwind-protect
		  (progn (set-output-mode ,to-stream internal-copy-mode)
			 ,@body)
		(set-output-mode ,to-stream mode))))))

(defmacro with-input-mode ((from-stream copy-mode) &body body)
  `(let ((mode (input-mode ,from-stream))
	 (internal-copy-mode (%translate-copy-mode ,copy-mode)))
     (cond ((eq mode internal-copy-mode)
	    ,@body)
	   (t (unwind-protect
		  (progn (set-input-mode ,from-stream ,copy-mode)
			 ,@body)
		(set-input-mode ,from-stream mode))))))

(http::defmethod http::stream-copy-byte-range ((from-stream si:binary-stream) to-stream start end)
  (stream-copy-bytes from-stream to-stream (- end start) start))

(http::defmethod http::stream-copy-byte-range ((from-stream si:binary-stream) (to-stream tcp-modal-http-stream) start end)
  (with-output-mode (to-stream :binary)
    (stream-copy-bytes from-stream to-stream (- end start) start)))

(http::defmethod http::stream-copy-byte-range (from-stream (to-stream tcp-modal-http-stream) start end)
  (with-output-mode (to-stream :binary)
    (stream-copy-bytes from-stream to-stream (- end start) start)))

(http::defmethod http::stream-copy-bytes ((from-stream si:binary-stream) to-stream n-bytes &optional (copy-mode :binary))
  (declare (ignore copy-mode))
  (stream-copy-bytes from-stream to-stream n-bytes))

(http::defmethod http::stream-copy-bytes ((from-stream tcp-modal-http-stream) (to-stream si:output-stream)
                                          n-bytes &optional (copy-mode :binary))
  (with-input-mode (from-stream copy-mode)
    (stream-copy-bytes from-stream to-stream n-bytes)))

(http::defmethod http::stream-copy-bytes ((from-stream si:input-stream) (to-stream tcp-modal-http-stream)
                                          n-bytes &optional (copy-mode :binary))
  (with-output-mode (to-stream copy-mode)
    (stream-copy-bytes from-stream to-stream n-bytes)))

(http::defmethod http::stream-copy-bytes ((from-stream tcp-modal-http-stream) (to-stream tcp-modal-http-stream)
                                          n-bytes &optional (copy-mode :binary))
  (with-input-mode (from-stream copy-mode)
    (with-output-mode (to-stream copy-mode)
      (stream-copy-bytes from-stream to-stream n-bytes))))

(http::defmethod http::stream-copy-bytes ((from-stream tcp-modal-http-stream) (pathname pathname) bytes &optional (copy-mode :binary))
  (with-input-mode (from-stream copy-mode)
    (with-open-file (file-stream pathname :direction :output :if-does-not-exist :create
				 :element-type (ecase copy-mode
						 ((:binary :crlf) '(unsigned-byte 8))
						 (:text 'character))
				 :if-exists :new-version)
      (stream-copy-bytes from-stream file-stream bytes))))

(http::defmethod http::stream-copy-bytes ((from-stream tcp-modal-http-stream) to-stream n-bytes &optional (copy-mode :binary))
  (with-input-mode (from-stream copy-mode)
    (stream-copy-bytes from-stream to-stream n-bytes)))

(http::defmethod http::stream-copy-bytes (from-stream (to-stream tcp-modal-http-stream) n-bytes &optional (copy-mode :binary))
  (with-output-mode (to-stream copy-mode)
    (stream-copy-bytes from-stream to-stream n-bytes)))

(http::defmethod http::stream-copy-until-eof (from-stream (to-stream tcp-modal-http-stream) &optional (copy-mode :text))
  (with-output-mode (to-stream copy-mode)
    (si:stream-copy-until-eof from-stream to-stream)))

(http::defmethod http::stream-copy-until-eof ((from-stream tcp-modal-http-stream) to-stream &optional (copy-mode :text))
  (with-input-mode (from-stream copy-mode)
    (si:stream-copy-until-eof from-stream to-stream)))

(http::defmethod http::stream-copy-until-eof ((from-stream tcp-modal-http-stream) (to-stream tcp-modal-http-stream)
					      &optional (copy-mode :text))
  (with-input-mode (from-stream copy-mode)
    (with-output-mode (to-stream copy-mode)
      (si:stream-copy-until-eof from-stream to-stream))))

(http::defmethod http::stream-copy-until-eof ((from-stream tcp-modal-http-stream) (pathname pathname) &optional (copy-mode :text))
  (with-input-mode (from-stream copy-mode)
    (with-open-file (file-stream pathname :direction :output :if-does-not-exist :create
				 :element-type (ecase copy-mode
						 ((:binary :crlf) '(unsigned-byte 8))
						 (:text 'character))
				 :if-exists :new-version)
      (si:stream-copy-until-eof from-stream file-stream))))

(http::defmethod http::stream-copy-until-eof ((pathname pathname) (to-stream tcp-modal-http-stream) &optional (copy-mode :text))
  (with-output-mode (to-stream copy-mode)
    (with-open-file (file-stream pathname :direction :input :if-does-not-exist :error
				 :element-type (ecase copy-mode
						 ((:binary :crlf) '(unsigned-byte 8))
						 (:text 'character)))
      (si:stream-copy-until-eof file-stream to-stream))))

(http::defmethod http::stream-copy-until-eof ((from-stream si:input-stream) (to-stream t) &optional (copy-mode :text))
  (declare (ignore copy-mode))
  (si:stream-copy-until-eof from-stream to-stream))

(http::defmethod http::stream-copy-until-eof ((from-stream si:input-stream) (to-stream tcp-modal-http-stream) &optional (copy-mode :text))
  (with-output-mode (to-stream copy-mode)
    (si:stream-copy-until-eof from-stream to-stream)))

(defun binary-stream-copy-into-8-bit-array (stream n-bytes &optional (start 0) 8-bit-array)
  (check-type n-bytes integer)
  (let ((vector (or 8-bit-array
		    (make-array n-bytes :element-type  '(unsigned-byte 8)
				:fill-pointer 0 :adjustable t))))
    (cond ;; buffer level copying
      ((operation-handled-p stream :read-input-buffer)
       ;; If it can go, this mode is the most efficient by far.
       (loop with remaining = n-bytes
	     with length = 0
	     with end = start
	     for s = start then end
	     doing (multiple-value-bind (buffer offset limit)
		       (send stream :read-input-buffer)
		     (cond ((null buffer)
			    (setf (fill-pointer vector) end)
			    (return (values vector end)))
			   ((< (setq length (- limit offset)) remaining)
			    (copy-array-portion buffer offset limit vector s (setq end (+ s length)))
			    (decf remaining length)
			    (send stream :advance-input-buffer))
			   (t (setq length (+ remaining offset))
			      (copy-array-portion buffer offset length vector s (setq end (+ s remaining)))
			      (send stream :advance-input-buffer length)	;leave buffer pointer in correct position
			      (setf (fill-pointer vector) end)
			      (return (values vector end)))))))
      ;; This always wins, but is incredibly slow.
      (t (let ((vector vector))
	   (declare (sys:array-register vector))
	   (loop with end = (+ start n-bytes)
		 for idx upfrom start below end
		 for byte = (read-byte stream t)
		 do (setf (svref vector idx) byte)
		 finally (setf (fill-pointer vector) end)
			 (return (values vector end))))))))

(defun binary-stream-copy-until-eof-into-8-bit-array (stream &optional (start 0) 8-bit-array &aux size)
  (flet ((make-the-array (size fill-pointer)
	   (make-array size :fill-pointer fill-pointer :adjustable t :element-type '(unsigned-byte 8)))
	 (adjust-the-array (array size fill-pointer)
	   (let ((new-array (adjust-array array size :fill-pointer fill-pointer
					  :element-type '(unsigned-byte 8))))
	     #+testing(unless (eq new-array array) (format t "New array in adjustment."))
	     new-array))
	 (new-size (size)
	   (cond ((< size 64000) (* 2 size))
		 (t (truncate (* size 1.2))))))
    (declare (inline make-the-array adjust-the-array new-size))
    (cond ((null 8-bit-array)
	   (setq size (+ 1000 start)
		 8-bit-array (make-the-array size start)))
	  (t (setq size (array-total-size 8-bit-array))))
    (cond ;; buffer level copying
      ((operation-handled-p stream :read-input-buffer)
       ;; If it can go, this mode is the most efficient by far.
       (loop with fill-pointer = start
	     for s = fill-pointer
	     doing (multiple-value-bind (buffer offset limit)
		       (send stream :read-input-buffer)
		     (cond ((null buffer)
			    (setf (fill-pointer 8-bit-array) fill-pointer)
			    (return (values 8-bit-array fill-pointer)))
			   (t (setq fill-pointer (+ s (- limit offset)))
			      (when (> fill-pointer size)
				(setq 8-bit-array (adjust-the-array 8-bit-array (setq size (new-size fill-pointer)) fill-pointer)))
			      (copy-array-portion buffer offset limit 8-bit-array s fill-pointer)
			      (setf (fill-pointer 8-bit-array) fill-pointer)
			      (send stream :advance-input-buffer))))))
      ;; This always wins, but is incredibly slow.
      (t (let ((array 8-bit-array))
	   (declare (sys:array-register vector))
	   (loop with fill-pointer = start
		 for byte = (read-byte stream nil nil)
		 while byte
		 do (when (= size fill-pointer)
		      (setq array (adjust-the-array array (setq size (new-size size)) fill-pointer)))
		    (setf (aref array fill-pointer) byte)
		    (incf fill-pointer)
		 finally (setf (fill-pointer array) fill-pointer)
			 (return (values array fill-pointer))))))))

(http::http::defmethod http::binary-stream-copy-into-8-bit-array
                       ((from-stream si:buffered-input-stream) n-bytes &optional (start 0) 8-bit-array)
  (if n-bytes
      (binary-stream-copy-into-8-bit-array from-stream n-bytes start 8-bit-array)
      (binary-stream-copy-until-eof-into-8-bit-array from-stream start 8-bit-array)))

(defun binary-stream-copy-from-8-bit-array (array stream &optional (start 0) end)
  (cond ;; buffer level copying
    ((operation-handled-p stream :string-out)
     ;; If it can go, this mode is the most efficient by far.
     (send stream :string-out array start (or end (length array)))
     nil)
    ;; This always wins, but is incredibly slow.
    (t (let ((array array))
         (declare (sys:array-register array))
         (loop for idx upfrom start below (or end (length array))
               do (write-byte (svref array idx) stream))))))

(http::defmethod http::binary-stream-copy-from-8-bit-array
                       (from-array (stream si:basic-buffered-output-stream) &optional (start 0) end)
  (binary-stream-copy-from-8-bit-array from-array stream start end))

;; This may return a length shorter than nbytes due to ascii translation.  It
;; would be better if this could by pass the CRLF translation and get the
;; right length without having to do it character at a time.  This would
;; require a swicth to control whether LF is retained in input buffers etc...
;; 7/14/96 -- JCMa.
(defun crlf-stream-copy-into-string (stream &optional nbytes (start 0) string &aux size)
  (flet ((make-the-string (size fill-pointer)
           (make-array size :fill-pointer fill-pointer :adjustable t :element-type 'character))
         (adjust-the-string (string size fill-pointer)
           (let ((new-string (adjust-array string size :fill-pointer fill-pointer
                                           :element-type http::*standard-character-type*)))
             #+testing(unless (eq new-string string) (format t "New array in adjustment."))
             new-string))
         (new-size (size)
           (cond ((< size 64000) (* 2 size))
                 (t (truncate (* size 1.2))))))
    (declare (inline make-the-string adjust-the-string new-size))
    (cond (nbytes
           (setq size (+ nbytes start))
           (cond ((null string)
                  (setq string (make-the-string size start)))
                 ((< (array-total-size string) size)
                  (setq string (adjust-array string size :fill-pointer start :element-type 'character))))
           (multiple-value-bind (idx fill-pointer)
               (binary-stream-copy-into-8-bit-array stream nbytes 0 string)
             idx                                ;ignore
             (setf (fill-pointer string) fill-pointer)
             (values string fill-pointer)))
          ;; the size and growth issues are open to experimentation and better
          ;; algorithms that do less work.  7/26/95 -- JCMa.
          (t (cond ((null string)
                    (setq size (+ start 1000)
                          string (make-the-string size start)))
                   (t (setq size (array-total-size string))))
             (loop with fill-pointer = start
                   for s = fill-pointer
                   doing (multiple-value-bind (buffer offset limit)
                             (send stream :read-input-buffer)
                           (cond ((null buffer)
                                  (setf (fill-pointer string) fill-pointer)
                                  (return (values string fill-pointer)))
                                 (t (unless (= limit offset)	;don't copy when no data present 11/18/99 -- JCMa.
				      (setq fill-pointer (+ s (- limit offset)))
				      (when (> fill-pointer size)
					(setq string (adjust-the-string string (setq size (new-size fill-pointer)) fill-pointer)))
				      (copy-array-portion buffer offset limit string s fill-pointer)
				      (setf (fill-pointer string) fill-pointer))
                                    (send stream :advance-input-buffer)))))))))

;; requires diddling the input buffer to really work   7/23/96 -- JCMa.
;(http::defmethod http::crlf-stream-copy-into-string
;                 ((stream tcp-modal-http-stream) &optional nbytes (start 0) string)
;  (let ((mode (input-mode stream)))
;    (crlf-input-mode stream)
;    (unwind-protect 
;       (crlf-stream-copy-into-string stream nbytes start string)
;      (set-input-mode mode))))

(http::defmethod http::crlf-stream-copy-into-string ((stream tcp-modal-http-stream) &optional nbytes (start 0) string)
  (crlf-stream-copy-into-string stream nbytes start string))

(http::defmethod http::write-vector ((stream tcp-modal-http-stream) (vector vector) &optional (start 0) (end (length vector)))
  (send stream :string-out vector start end)
  vector)

(http::defmethod http::write-vector ((stream tcp-modal-http-stream) (vector string) &optional (start 0) (end (length vector)))
  (send stream :string-out vector start end)
  vector)

(http::defmethod http::advance-input-buffer ((stream tcp-modal-http-stream) &optional delta)
  (if delta
      (unless (zerop delta)
	(with-input-mode (stream :binary)
	  (loop with remaining-bytes = delta and buf-size
		doing (multiple-value-bind (buffer offset limit)
			  (send stream :read-input-buffer)
			(cond ((null buffer)
			       (error "READ-INPUT-BUFFER failed to signal EOF. This should never happen."))
			      ((> remaining-bytes (setq buf-size (- limit offset)))
			       (send stream :advance-input-buffer)
			       (decf remaining-bytes buf-size))
			      (t (send stream :advance-input-buffer (+ offset remaining-bytes))
				 (return)))))))
      (with-input-mode (stream :binary)
	(loop for buffer = (send stream :read-input-buffer)
	      do (if buffer
		     (send stream :advance-input-buffer)
		     (return))))))

;;;------------------------------------------------------------------- 
;;;
;;; COMPILE METHODS FOR FAST OPERATION
;;;

(compile-flavor-methods tcp-modal-http-stream)


#| Debugging cod

(defwhopper (:read-input-buffer tcp-modal-http-stream) (&optional eof no-hang-p)
  (si:dbg)
  (continue-whopper eof no-hang-p))

|#
