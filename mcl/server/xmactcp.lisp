;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: ccl -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MacTCP.Lisp
; Copyright 1991-1994 Apple Computer, Inc.
; Copyright 1995-96 Digitool, Inc.
; Enhancements Copyright John C. Mallery, 1995-1996
;
; TCP streams.
;
; OPEN-TCP-STREAM creates a stream that does its I/O through a TCP port.
;

; Modification History
;
; 02/22/97 JCMa handle 2 byte characters in stream-write-string and stream-tyo
; 12/4/96 JCMa  Increase default for *tcp-close-timeout* from 5 to 20 seconds.
; 11/5/96 jcma  Make %tcp-reinitialize-listening-connection respect *tcp-reinitialize-connections* switch. Add new instance variables
;;                to CONN in order to support tcp-reinitialize-connection.
; 10/03/96 slh  find-folder -> find-folder-ids (conflict w/SourceServer)
; 10/02/96 jcma tweaked stream-abort, tcp-close logic
;  9/27/96 slh  don't set ioCompletion for sync calls (trap mgr does it); fix tcp-send %inc-ptr values
;               selected (%null-ptr) -> *null-ptr*; %tcp-control-async-wait: try using #$inProgress
;               try *tcp-write-buffer-size* 4096 (> 4N+1024, N = _UPDMTU of 548, from interface file)
;               %tcp-getaddr: use rletz; try 16K read buffer size; MoveHHi DNR resource
;  9/26/96 slh  tcp-with-connection-grabbed: use gensyms
;  9/25/96 slh  combine eval-when's for easier interaction; logand #xFFFF -> oserr-word to get sign right
;  9/20/96 slh  logand #xFFFF with ff-call D0 results; eval-when around constants, macros
;  8/19/96 jcma comment out load-trap memerror
;  8/04/96 jcma/cvince bullet-proof tcp-read-bytes-to-file
;  8/04/96 jcma/cvince add *tcp-reinitialize-connections*
;  8/03/96 jcma add support for encoding/decoding on TCP buffers for transmit/recieve
;  6/30/96 jcma add stream-copy-bytes, tcp-write-file-bytes
;  5/04/96 jcma thread safe DNS operations
;  3/29/96 jcma fast connection locking
;  3/26/96 jcma major overhaul
;  3/22/96 jcma robust reset connection in stream-listen, upgrade orderly wait, rationalize more.
;  3/14/96 jcma make this really work by releasing connections only when they are destroyed.
;  3/09/96 jcma copy most critical PBs except for creates & opens.
;  3/09/96 jcma use process queues to avoid blocking.
;  3/09/96 jcma Install TCP connection resourcing & remodularize entropic abstractions
;  3/09/96 jcma Wrap memerrchk & errchk in eval-when
;  3/01/96 bill Rainer Joswig's fix to tcp-host-cname
;  2/26/96 bill Merge the cl-http "xmactcp.lisp" into this file.
;  2/26/96 bill Revert to using FF-CALL. It does the right thing now.
;  2/12/96 bill Update for new traps, wrap UPP's around calls to the resolver code.
;  2/04/96 jcma replaced all calls to slot value with faster accessors.
;  2/04/96 jcma & gb add %pb-copy and use to avoid PB collisions in print-object and stream-connection-state
;  7/11/95 jcma handle some AppleTalk configuration errors, export all conditions.
;  6/09/95 jcma signal unknown-mactcp-error in %tcp-err-disp for unknown error codes
;  6/09/95 jcma handled remaining conditions
;  6/05/95 jcma installed TCP condition system
;  6/05/95 jcma fixed tcp-stream-conn and stream-listen
;  6/05/95 jcma fixed stream-close and  new note-stream-closed
;  6/01/95 slh  :str255 -> (:string 255)
;  5/27/95 slh  no :errchk for _sysenvirons
;  3/28/95 slh  gestalt not external
;  3/11/95 slh  use gestalt bitnum arg
;  3/02/95 akh  say element-type 'base-character
;  2/15/95 slh  Poeck & JCMa's improved stream-close, %tcp-control
; ------------- 3.0d17
; 05/17/93 bill #_HOpenResFile doesn't exist in System 6. Replace it
;               with hairy working directory wrapper around #_OpenResFile,
;               if it doesn't exist.
; ------------- 3.0d8
; 01/22/93 bill Steve Weyer's fix to make stream-tyi return NIL at EOF.
; 06/05/92 bill remove (dbg length)
; 05/05/92 bill Narinder Singh's mods to add a timeout value for passive opens.
;-------------- 2.0
; 03/20/92 bill format string needed arg in (initialize-instance (binary-tcp-stream))
; 02/27/92 bill Derek's mods to ease subclassing of tcp-stream.
; -----------   2.0f3
; 02/05/92 gb   change record defs to more nearly match TCPPB.h, etc.
; 01/20/92 gb   minimal support for binary i/o.
; 12/24/91 gb   fix some bugs; look harder for the resolver.
;--------- 2.0b4
; 08/20/91 bill %get-cstr -> %get-cstring
; 05/20/91 gb   Still needs work.
; 01/10/91 bill Remove LAP
; 05/08/90 gz   Released


;;;------------------------------------------------------------------- 
;;;
;;; 
;;; 

(in-package :ccl) 

;; OpenTransport on PPCs has trouble reinitializing connections when they are lost while coming up.
;; There are other issues effecting the operation of active opens related to this problem.  This bug in Apple
;; code is worked around by releasing OT connections rather than resourcing them. Apple should fix this sometime,
;; at which point this switch should always resource the connection PB as well. -- JCMa 8/4/1996.
(defparameter *tcp-reinitialize-connections* t
  "When non-nil, rebuild pb and associated values each time, otherwise recycle entire connection.")

(defun set-tcp-connection-reinitialization (value)
  (setq *tcp-reinitialize-connections* value))

(defparameter *tcp-buffer-size* 16384)          ; was 8192

(defparameter *tcp-write-buffer-size* 4096)     ; was 1024

(defparameter *tcp-command-timeout* 30        ;  in seconds, timeouts at the TCP command level
  "Controls how many seconds TCP waits for for acknowledgements  at the TCP level from the other end of a connection.")

(defparameter *tcp-read-timeout* (* 60 2)         ;  in seconds, the time to wait for replys from the other end
  "Controls how many seconds TCP waits for replys from the other end of a connection.")

;; default timeout raised per Elmar W Schwarz' report
(defparameter *tcp-close-timeout* 20     ; in seconds, the time to wait before aborting a slow closing connection
  "The seconds to wait for a connection to close.
Production servers may wish to use 5 seconds, but fast servers may produce broken pipe
errors with slow clients. A 20-30 second timeout is reported to eliminate the broken pipe problem.")

(defparameter *tcp-maximum-number-of-connections* 20)   ; maximum supported by mactcp is 64

(defun tcp-maximum-number-of-connections ()
  *tcp-maximum-number-of-connections*)

; New function
(defun (setf tcp-maximum-number-of-connections) (max)
  (setq *tcp-maximum-number-of-connections* (require-type max '(integer 1))))

;;;------------------------------------------------------------------- 
;;;
;;; DEFINITIONS
;;; 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(open-tcp-stream)))

;; load this too because people get confused   3/14/97 -- JCMa.
(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (unless (fboundp 'errchk)
    (defmacro errchk (form)
      (let ((res (gensym)))
        `(let ((,res ,form))
           (unless (eql 0 ,res) (%err-disp ,res))
           ,res))))
  
  ; This is here because :errchk doesn't work correctly
  ; in MCL 3.0 for inline traps, e.g. #_NewPtrClear
  (unless (fboundp 'memerrchk) 
    (defmacro memerrchk (form)
      (let ((res (gensym))
            (err-code (gensym)))
        `(let ((,res ,form)
               (,err-code (require-trap #_memerror)))
           (unless (eql 0 ,err-code)
             (%err-disp ,err-code))
           ,res))))
  
  (defconstant $cacheFault -23042)
  (defconstant $tcpPBsize (record-length :tcpioPB))
  
  ;; Use this macro to control concurrent access to the same connection.
  ;; With Bill's store-conditional patch, this as fast as it is going to get.
  ;; Queuing might be an issue for some higher-level operations, but the overhead
  ;; of ensuring FIFO does not seem worth the overhead at the TCP interface level.
  (defmacro tcp-with-connection-grabbed ((conn owner whostate) &body body)
    (let ((lock-v (gensym))
          (owner-v (gensym)))
      `(let ((,lock-v (conn-lock ,conn))
             (,owner-v ,owner))
         (locally (declare (type lock ,lock-v))       ; causes everything to inline
           (unwind-protect
             (progn (process-lock ,lock-v ,owner-v ,whostate) ,@body)
             (process-unlock ,lock-v ,owner-v nil))))))
  
  ; This assumes that "conn-pb" is non-null
  (defmacro with-pb-copy ((pb conn-pb) &body body)
    `(%stack-block ((,pb $tcpPBSize))
       (%tcp-pb-copy ,conn-pb ,pb)
       ,@body))
  
  (defconstant $openresolver 1)
  (defconstant $closeresolver 2)
  (defconstant $strtoaddr 3)
  (defconstant $addrtostr 4)
  (defconstant $enumcache 5)
  (defconstant $addrtoname 6)
  
  #-ppc-target
  (defmacro with-vrefnum (vrefnum &body body)
    (let ((saved-vrefnum (gensym)))
      `(let ((,saved-vrefnum (get-vrefnum)))
         (unwind-protect
           (progn
             (set-vrefnum ,vrefnum)
             ,@body)
           (set-vrefnum ,saved-vrefnum)))))
  
  (defconstant _unimplemented #xa89f)
  
  ;;; :close-wait means other end has closed but we haven't
  (defconstant +state-name-array+
    #(:closed :listen :syn-received :syn-sent :established :fin-wait-1 :fin-wait-2 :close-wait :closing :closing-last-ack :closing-time-ack))
  
  (defmacro with-end-of-file-handled ((stream) &body body)
    `(handler-case
       (progn . ,body)
       (protocol-timeout () (error 'end-of-file :stream ,stream))))
  
  
  ;TCP csCodes
  (defconstant $ipctlGetAddr 15)
  (defconstant $TCPCreate 30)
  (defconstant $TCPPassiveOpen 31)
  (defconstant $TCPActiveOpen 32)
  (defconstant $TCPSend 34)
  (defconstant $TCPNoCopyRcv 35)
  (defconstant $TCPRcvBfrReturn 36)
  (defconstant $TCPRcv 37)
  (defconstant $TCPClose 38)
  (defconstant $TCPAbort 39)
  (defconstant $TCPStatus 40)
  (defconstant $TCPExtendedStat 41)
  (defconstant $TCPRelease 42)
  (defconstant $TCPGlobalInfo 43)
  (defconstant $TCPCtlMax 49)
  
  ;TCP event codes
  (defconstant $TCPClosing 1)
  (defconstant $TCPULPTimeout 2)
  (defconstant $TCPTerminate 3)
  (defconstant $TCPDataArrival 4)
  (defconstant $TCPUrgent 5)
  (defconstant $TCPICMPReceived 6)
  
  ;TCP termination reasons
  (defconstant $TCPRemoteAbort 2)
  (defconstant $TCPNetworkFailure 3)
  (defconstant $TCPSecPrecMismatch 4)
  (defconstant $TCPULPTimeoutTerminate 5)
  (defconstant $TCPULPAbort 6)
  (defconstant $TCPULPClose 7)
  (defconstant $TCPServiceError 8)
  
  ;ValidityFlags
  (defconstant $TCPtimeoutValue #x80)
  (defconstant $TCPtimeoutAction #x40)
  (defconstant $TCPtypeOfService #x20)
  (defconstant $TCPprecedence #x10)
  
  ;TOSFlags
  (defconstant $TCPlowDelay #x01)
  (defconstant $TCPthroughPut #x02)
  (defconstant $TCPreliability #x04)
  
  ; error codes
  (defconstant $TCPTimeout -23016)
  )

#+ppc-target
(eval-when (:compile-toplevel :execute)
  (let ((*warn-if-redefine* nil))
    (defmacro traps::_StripAddress (x) x)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (let ((*warn-if-redefine* nil))
    
    (defrecord IPParamBlock
      (qLink pointer)
      (qType integer)
      (ioTrap integer)
      (ioCmdAddr pointer)
      (ioCompletion pointer)
      (ioResult integer)
      (ioNamePtr pointer)
      (ioVRefNum integer)
      (ioCRefNum integer)
      (csCode integer)
      (ourAddress unsigned-long)
      (ourNetMask unsigned-long))
    
    (defrecord tcpCreatePB
      (rcvBuff pointer)
      (rcvBuffLen longint)                 ; should be unsigned.
      (notifyProc pointer)
      (userDataPtr pointer))
    
    (defrecord tcpReleasePB
      (rcvBuff pointer)
      (rcvBuffLen longint))
    
    (defrecord tcpOpenPB
      (ulpTimeoutValue byte)
      (ulpTimeoutAction byte)
      (validityFlags byte)
      (commandTimeoutValue byte)
      (remoteHost unsigned-long)
      (remotePort integer)
      (localHost unsigned-long)
      (localPort integer)
      (tosFlags byte)
      (precedence byte)
      (dontFrag byte)
      (timeToLive byte)
      (security byte)
      (optionCnt byte)
      (options (string 39))
      (userDataPtr pointer))
    
    (defrecord tcpSendPB
      (ulpTimeoutValue byte)
      (ulpTimeoutAction byte)
      (validityFlags byte)
      (pushFlag byte)
      (urgentFlag byte)
      (fill byte)
      (wdsPtr pointer)
      (sendFree longint)                    ; unsigned
      (sendLength unsigned-integer)
      (userDataPtr pointer))
    
    (defrecord tcpReceivePB
      (commandTimeoutValue byte)
      (fill byte)
      (markFlag byte)
      (urgentFlag byte)
      (rcvBuff pointer)
      (rcvBuffLen unsigned-integer)
      (rdsPtr pointer)
      (rdsLength unsigned-integer)
      (secondTimeStamp unsigned-integer)
      (userDataPtr pointer))
    
    (defrecord tcpClosePB
      (ulpTimeoutValue byte)
      (ulpTimeoutAction byte)
      (validityFlags byte)
      (fill byte)
      (userDataPtr pointer))
    
    (defrecord tcpAbortPB
      (userDataPtr pointer))
    
    (defrecord tcpStatusPB
      (ulpTimeoutValue byte)
      (ulpTimeoutAction byte)
      (fill1 longint)
      (remoteHost unsigned-long)
      (remotePort unsigned-integer)
      (localHost unsigned-long)
      (localPort unsigned-integer)
      (tosFlags byte)
      (precedence byte)
      (connectionState byte)
      (fill2 byte)
      (sendWindow unsigned-integer)
      (rcvWindow unsigned-integer)
      (amtUnackedData unsigned-integer)
      (amtUnreadData unsigned-integer)
      (securityLevelPtr pointer)
      (sendUnacked longint)
      (sendNext longint)
      (congestionWindow longint)
      (rcvNext longint)
      (srtt longint)
      (lastRTT longint)
      (sendMaxSegSize longint)
      (connStatPtr pointer)
      (userDataPtr pointer))
    
    (defrecord tcpGlobalInfoPB
      (tcpParamPtr pointer)
      (tcpStatsPtr pointer)
      (tcpCDBTable pointer)
      (userDataPtr pointer))
    
    (defrecord tcpIOPB
      (qLink pointer)
      (qType integer)
      (ioTrap integer)
      (ioCmdAddr pointer)
      (ioCompletion pointer)
      (ioResult integer)
      (ioNamePtr pointer)
      (ioVRefNum integer)
      (ioCRefNum integer)
      (csCode integer)
      (StreamPtr pointer)
      (variant
       ((create tcpCreatePB))
       ((release tcpReleasePB))
       ((open tcpOpenPB))
       ((send tcpSendPB))
       ((receive tcpReceivePB))
       ((close tcpClosePB))
       ((abort tcpAbortPB))
       ((status tcpStatusPB))
       ((globalinfo tcpGlobalInfoPB))))
    
    (defrecord hostinfo
      (rtnCode longint)
      (cname (string 255))
      (addr1 unsigned-long)
      (addr2 unsigned-long)
      (addr3 unsigned-long)
      (addr4 unsigned-long)
      ;This is our own extension...
      (result integer))
    
    )) ;defrecord eval-when

;;;------------------------------------------------------------------- 
;;;
;;;  TCP CONNECTIONS
;;; 

(defstruct 
   (conn                                         ; Don't bother doing slot-value for every little thing...
     (:print-function tcp-print-conn))
   pb
   write-buffer
   write-bufsize
   write-count
   read-timeout
   untyi-char
   rds
   rds-entries
   rds-offset                                    ; offset in rds to next buffer
   read-count
   read-bufptr
   lock                                          ; process locks replace without-interrupts -- JCMa 3/9/1996.
   next-resource                                 ; points at the next free connection
   serial-number                                 ; identifying number for the connection
   state                                         ; connection states: :allocated, :unallocated, :open, :closed, :listen 
   local-port                                    ; local port number
   remote-port                                   ; remote port number
   command-timeout                               ; timeout in seconds for TCP commands
   bytes-transmitted                             ; bytes transmited over the connection
   bytes-received                                ; bytes read in over the connection
   stream
   notify-procedure                      ; notification procedure cache
   tcp-buffer-size                           ; buffersize cache
   (encoder nil)                                 ; hook for application encoder on TCP write buffer  8/3/96 -- JCMa.
   (decoder nil))                                ; hook for application decoder on TCP read buffer

(defun tcp-print-conn (conn stream ignore)
  (declare (ignore ignore))
  (print-unreadable-object (conn stream :identity t)
    (case (conn-state conn)
      ((:unallocated :allocated)
       (format stream "TCP-Connection ~D ~:(~A~)" (conn-serial-number conn) (conn-state conn)))
      (t (let ((state (tcp-connection-state-name conn)))
           (case state
             (:closed
              (format stream "TCP-Connection ~D ~:(~A~)" (conn-serial-number conn) state))
             (:listen
              (format stream "TCP-Connection ~D ~:(~A~) Port: ~D" (conn-serial-number conn) 
                      state (tcp-local-port conn)))
             (:open
              (format stream "TCP-Connection ~D ~:(~A~) Port: ~D Host: ~A" 
                      (conn-serial-number conn) state
                      (tcp-local-port conn) (tcp-addr-to-str (tcp-remote-host conn))))
             (t (format stream "TCP-Connection ~D  ~:(~A~)" (conn-serial-number conn) state ))))))) ) 

;;;------------------------------------------------------------------- 
;;;
;;; MacTCP.Lisp 
;;;

;; Allows AppleTalk use of TCP with resolution turned off.
(defvar *use-resolver* t)

(defun tcp-use-name-resolver ()
  *use-resolver*)

; New function
(defun (setf tcp-use-name-resolver) (flag)
  (setq *use-resolver* (not (null flag))))

(defparameter *service-name-number-alist*
  '(("echo" . 7)
    ("discard" . 9)                     ; sink null
    ("systat" . 11)
    ("daytime" . 13)
    ("netstat"  . 15)
    ("ftp-data" . 20)
    ("ftp" . 21)
    ("telnet" . 23)
    ("smtp" . 25)
    ("time" . 37)
    ("name" . 32)                       ; (udp only)
    ("whois" . 43)                      ; usually to sri-nic
    ("domain" . 53)
    ("hostnames" . 101)                 ; usually to sri-nic
    ("http" . 80)
    ("sunrpc" . 111)
    ("rje" . 77)
    ("finger" . 79)
    ("link" . 87)                       ; ttylink
    ("supdup" . 95)
    ("iso-tsap" . 102)
    ;("x400" . 103)                      ; # ISO Mail
    ("dictionary" . 103)
    ("x400-snd" . 104)
    ("csnet-ns" . 105)
    ("pop" . 109)
    ("uucp-path" . 117)
    ("nntp" . 119)
    ("ntp" . 123)
    ("NeWS" . 144)
    ; UNIX specific services
    ;these are NOT officially assigned
    ("exec" . 512)
    ("login" . 513)
    ("shell" . 514)
    ("printer" . 515)                   ; spooler       # experimental
    ("courier" . 530)                   ; rpc           # experimental
    ("biff" . 512)                      ; (udp only) comsat
    ("who" . 513)                       ; (udp only)
    ("syslog" . 514)                    ; (udp only)
    ("talk" . 517)                      ; (udp only)
    ("route" . 520)                     ; (udp only)
    ("new-rwho" . 550)                  ; (udp only)    # experimental
    ("rmonitor" . 560)                  ; (udp only)    # experimental
    ("monitor" . 561)                   ; (udp only)    # experimental
    ("ingreslock" . 1524)
    ("imap" . 143)))

; return a signed 16-bit value given a longword
(defun oserr-word (value)
  (let ((word (logand #xFFFF value)))
    (if (logbitp 15 word)
      (- word #x10000)
      word)))

(defun tcp-service-name (port)
  (or (car (rassoc port *service-name-number-alist*))
      port))

(defun tcp-service-port-number (port-name)
  (or (cdr (assoc (require-type port-name'(or string symbol)) *service-name-number-alist* :test #'string-equal))
      (error "Unknown port ~S" port-name)))

(defvar *tcp-driver-refnum* nil)
(defvar %resolver-code% nil)
(defvar %tcp-set-result-proc% nil) 

(def-load-pointers tcp ()
  (let* ((code '(#x225f                  ; spop a1
                 #x584f                  ; addq #4,sp
                 #x205f                  ; spop a0
                 #x3168 #x0002 #x0114    ; move.w 2(a0),hostinfo.result(a0)
                 #x4ed1))                ; jmp (a1)
         (codelen (length code)))
    (setq *tcp-driver-refnum* nil)
    (setq %resolver-code% nil)
    (setq %tcp-set-result-proc% (let ((ptr (#_NewPtr :errchk (+ codelen codelen))))
                                  (dotimes (i codelen ptr)
                                    (%put-word ptr (pop code) (+ i i)))))))

(defun tcp-driver-refnum ()
  (or *tcp-driver-refnum*
      (with-pstrs ((name ".ipp"))
        (rlet ((pb hparamblockrec))
          (setf (rref pb hparamblockrec.ionameptr) name
                (rref pb hparamblockrec.ioPermssn) 0)
          (errchk (#_PBOpenSync pb))
          (setq *tcp-driver-refnum* (rref pb hparamblockrec.ioRefNum))))))

;;;------------------------------------------------------------------- 
;;;
;;; ERROR AND CONDITION HANDLING
;;;

(defparameter *tcp-error-strings*
  '((0 . "No error")
    (-19 . "Driver does not respond to read requests") ;;readErr
    (-21 . "Driver reference number does not match unit table") ;;badUnitErr
    (-22 . "Driver reference number specifies a nil handle in unit table") ;;unitEmptyErr
    (-27 . "Request aborted by KillIO") ;;abortErr
    (-28 . "Driver not open") ;;notOpenErr
    (-36 . "Data does not match in read-verify mode") ;;ioErr
    (-23000 . "Bad network configuration")
    (-23001 . "Bad IP configuration")
    (-23002 . "Missing IP or LAP configuration")
    (-23003 . "Error loading MacTCP")
    ;#define ipBadAddr -23004 /* error in getting address */
    (-23005 . "TCP connection closing")
    (-23006 . "the receive buffer area is too small") ;;#define invalidLength -23006
    (-23007 . "Request conflicts with existing connection")
    (-23008 . "Connection does not exist")
    (-23009 . "Insufficient resources to perform TCP request") 
    (-23010 . "the specified TCP stream is not open") ;;#define invalidStreamPtr -23010
    (-23011 . "an open stream is already using this receive buffer area") ;;#define streamAlreadyOpen -23011 
    (-23012 . "Connection terminated")
    (-23013 . "the receive buffer area pointer is 0") ;;#define invalidBufPtr  -23013
    ;#define invalidRDS     -23014
    ;#define invalidWDS           -23014
    (-23015 . "TCP open failed")
    (-23016 . "TCP command timeout")
    (-23017 . "A connection already exists between this local IP address and TCP port, and the specified remote IP address and TCP port") ;;#define duplicateSocket  -23017
    ;#define ipDontFragErr  -23032  /* Packet too large to send w/o fragmenting */
    (-23033 . "Destination host is not responding")
    ;#define ipNoFragMemErr -23036 /* no memory to send fragmented pkt */
    ;#define ipRouteErr  -23037 /* can't route packet off-net */
    (-23041 . "Syntax error in host name")
    ;#define cacheFault -23042
    ;#define noResultProc  -23043
    (-23044 . "No name server can be found for the specified domain name")
    (-23045 . "Domain name does not exist")
    (-23046 . "None of the known name servers are responding")
    (-23047 . "The domain name server has returned an error")
    ;#define outOfMemory  -23048
    )) 

;;;------------------------------------------------------------------- 
;;;
;;; MAPPING TCP ERROR CODES TO MACTCP CONDITIONS
;;;

;; #-200 error signalled when running on DUO with MACTCP configured for ethernet.-- JCMa 2/3/1995.
;; This error code signalled again in MCL3.0b4 and MAC CL-HTTP 0.6.2 (26.2) -- JCMa 6/3/1995.

(defparameter *tcp-error-class-alist*
  '((-23000 . tcp-bad-network-connection)
    (-23001 . tcp-bad-ip-configuration)
    (-23002 . tcp-missing-ip-or-lap-configuration)
    (-23003 . error-loading-mactcp)
    (-23004 . tcp-bad-ip-address)
    (-23005 . tcp-connection-closed)
    (-23006 . tcp-invalid-ip-packet-length)
    (-23007 . tcp-connection-conflict)
    (-23008 . tcp-connection-does-not-exist)
    (-23009 . tcp-insufficient-resources)
    (-23010 . tcp-invalid-stream-pointer)
    (-23011 . tcp-stream-already-open)
    (-23012 . tcp-connection-reset)
    (-23013 . tcp-invalid-buffer-pointer)
    (-23014 . tcp-invalid-data-structure)
    (-23015 . tcp-connection-refused)
    (-23016 . tcp-command-timeout)
    (-23017 . tcp-duplicate-socket)
    (-23032 . tcp-ip-packet-too-large)
    (-23033 . tcp-host-stopped-responding)
    (-23036 . tcp-insufficient-memory-for-packet-size)
    (-23037 . tcp-insufficient-memory-for-packet-size)
    (-23041 . tcp-syntax-error)
    (-23042 . tcp-domain-cache-fault)
    ;;#define noResultProc  -23043
    (-23044 . tcp-domain-server-not-found)
    (-23045 . tcp-unknown-domain-name)
    (-23046 . tcp-no-response-from-domain-server)
    (-23047 . domain-resolver-error)
    (-23048 . tcp-out-of-memory)
    ;; AppleTalk errors follow
    (-97 . appletalk-port-already-in-use)
    (-98 . appletalk-inactive)))

(defun %tcp-err-disp (error-number)
  (let* ((error-code (%word-to-int error-number))
         (entry (assoc error-code *tcp-error-class-alist* :test #'eq))
         (error-fn #'error))                    ; want to tail-call...
    (declare (type list entry))
    (if entry
      ;; call our own TCP error.
      (funcall error-fn (cdr entry))
      ;;formerly called standard error signalling code in MCL (%err-disp error-number)
      ;;now signals unknown error class -- JCMa 6/9/1995.
      (funcall error-fn 'unknown-mactcp-error :error-code error-code))))

;;;------------------------------------------------------------------- 
;;;
;;; TCP CONDITION CLASSES
;;;

;; These error classes are intended to support multiple network protocols.
;; All TCP specific errors are prefixed with TCP-.  All AppleTalk errors are
;; prefixed with appletalk-. Generic error classes have no prefix

(define-condition mactcp-error (error))

(define-condition mactcp-error-message-mixin
                  (mactcp-error)
                  ((error-message :initarg :error-message :reader tcp-error-message :allocation :class))
  (:report report-mactcp-error))

(defmethod report-mactcp-error ((error mactcp-error-message-mixin) stream)
  (with-slots (error-message) error
    (write-string "MACTCP: " stream)
    (write-string error-message stream)
    (write-char #\.  stream)))

(define-condition mactcp-error-code-mixin
                  (mactcp-error-message-mixin)
                  ((error-code :initarg :error-code :reader tcp-error-code
                               :allocation :class))) 

(define-condition unknown-mactcp-error
                  (mactcp-error-code-mixin mactcp-error)
                  ((error-code :initarg :error-code :reader tcp-error-code :allocation :instance)
                   (error-message :initform "-- No entry in ccl::*TCP-Error-Class-Alist*.")))

(defmethod report-mactcp-error ((error unknown-mactcp-error) stream)
  (with-slots (error-code error-message) error
    (write-string "MACTCP: Unknown Error Code " stream) 
    (write error-code :stream stream :base 10)
    (write-char #\space stream)
    (write-string error-message stream)
    (write-char #\.  stream)))

(define-condition network-error (error))

(define-condition domain-error (mactcp-error network-error))

(define-condition unknown-host-name (mactcp-error network-error))

(define-condition tcp-domain-server-not-found
                  (mactcp-error-code-mixin domain-error)
                  ((error-code :initform -23044 :allocation :class)
                   (error-message :initform "No name server can be found for the specified domain name"
                                  :allocation :class)))

(define-condition tcp-unknown-domain-name
                  (mactcp-error-code-mixin domain-error unknown-host-name)
                  ((error-code :initform -23045 :allocation :class)
                   (error-message :initform "Domain name does not exist":allocation :class)))

(define-condition tcp-no-response-from-domain-server
                  (mactcp-error-code-mixin domain-error)
                  ((error-code :initform -23046 :allocation :class)
                   (error-message :initform "None of the known name servers are responding"
                                  :allocation :class)))

(define-condition domain-resolver-error
                  (mactcp-error-code-mixin domain-error)
                  ((error-code :initform -23047 :allocation :class)
                   (error-message :initform "The domain name server has returned an error"
                                  :allocation :class)))

(define-condition local-network-error (network-error) ())

(define-condition unknown-address (local-network-error) ())

;; these errors may be internal errors or ones that were so poorly
;; documented that it is hard to tell where they fit in the condition
;; hierarchy.  Please advise if any should be moved elsewhere.
(define-condition tcp-internal-error (local-network-error) ())

(define-condition tcp-insuffucient-memory-error (tcp-internal-error) ())

(define-condition tcp-ip-packet-error (tcp-internal-error) ())

(define-condition tcp-bad-ip-address
                  (mactcp-error-code-mixin tcp-ip-packet-error)
                  ((error-code :initform -23004 :allocation :class)
                   (error-message :initform "Internal Error: Error getting address" :allocation :class)))

(define-condition tcp-invalid-ip-packet-length
                  (mactcp-error-code-mixin tcp-ip-packet-error)
                  ((error-code :initform -23006 :allocation :class)
                   (error-message :initform "Internal Error: Invalid IP packet length." :allocation :class)))

(define-condition tcp-ip-packet-too-large
                  (mactcp-error-code-mixin tcp-ip-packet-error)
                  ((error-code :initform -23032 :allocation :class)
                   (error-message :initform "Internal Error: Packet too large to send without fragmenting." :allocation :class)))

(define-condition tcp-insufficient-memory-for-packet-size
                  (mactcp-error-code-mixin tcp-ip-packet-error tcp-insuffucient-memory-error)
                  ((error-code :initform -23036 :allocation :class)
                   (error-message :initform "Internal Error: Packet too large for available memory." :allocation :class)))

(define-condition tcp-ip-routing-error
                  (mactcp-error-code-mixin tcp-ip-packet-error)
                  ((error-code :initform -23037 :allocation :class)
                   (error-message :initform "Internal Error: Unable to route packet off the network." :allocation :class)))

(define-condition tcp-invalid-stream-pointer
                  (mactcp-error-code-mixin tcp-internal-error)
                  ((error-code :initform -23010 :allocation :class)
                   (error-message :initform "Internal Error: Invalid stream pointer." :allocation :class)))

(define-condition tcp-stream-already-open
                  (mactcp-error-code-mixin tcp-internal-error)
                  ((error-code :initform -23011 :allocation :class)
                   (error-message :initform "Internal Error: Stream already open." :allocation :class)))

(define-condition tcp-invalid-buffer-pointer
                  (mactcp-error-code-mixin tcp-internal-error)
                  ((error-code :initform -23013 :allocation :class)
                   (error-message :initform "Internal Error: Invalid buffer pointer." :allocation :class)))

(define-condition tcp-invalid-data-structure
                  (mactcp-error-code-mixin tcp-internal-error)
                  ((error-code :initform -23014 :allocation :class)
                   (error-message :initform "Internal Error: Invalid datastructure." :allocation :class))) 

(define-condition tcp-duplicate-socket
                  (mactcp-error-code-mixin tcp-internal-error)
                  ((error-code :initform -23017 :allocation :class)
                   (error-message :initform "Internal Error: Duplicate socket" :allocation :class))) 

(define-condition tcp-out-of-memory
                  (mactcp-error-code-mixin tcp-insuffucient-memory-error)
                  ((error-code :initform -23048 :allocation :class)
                   (error-message :initform "Internal Error: Out of memory." :allocation :class)))

(define-condition interface-not-working (local-network-error) ())

;; not clear if this is a transient on the MAC.
(define-condition tcp-bad-network-connection
                  (mactcp-error-code-mixin interface-not-working)
                  ((error-code :initform -23000 :allocation :class)
                   (error-message :initform "Bad network configuration. Unable to initialize local network." :allocation :class)))

(define-condition tcp-bad-ip-configuration
                  (mactcp-error-code-mixin interface-not-working)
                  ((error-code :initform -23001 :allocation :class)
                   (error-message :initform "Bad IP configuration" :allocation :class)))

(define-condition tcp-missing-ip-or-lap-configuration
                  (mactcp-error-code-mixin interface-not-working)
                  ((error-code :initform -23002 :allocation :class)
                   (error-message :initform "Missing IP or LAP configuration" :allocation :class)))

(define-condition error-loading-mactcp
                  (mactcp-error-code-mixin interface-not-working)
                  ((error-code :initform -23003 :allocation :class)
                   (error-message :initform "Error loading MacTCP" :allocation :class)))

(define-condition network-resources-exhausted (local-network-error) ())

(define-condition tcp-insufficient-resources
                  (mactcp-error-code-mixin network-resources-exhausted)
                  ((error-code :initform -23009 :allocation :class)
                   (error-message :initform "Insufficient resources to perform TCP request"
                                  :allocation :class)))

(define-condition network-domain-cache-fault (local-network-error) ())

(define-condition tcp-domain-cache-fault
                  (mactcp-error-code-mixin network-domain-cache-fault)
                  ((error-code :initform -23042 :allocation :class)
                   (error-message :initform "Domain cache fault." :allocation :class)))

(define-condition remote-network-error (network-error) ())

(define-condition host-not-responding (remote-network-error) ())

(define-condition bad-connection-state (remote-network-error) ())

(define-condition tcp-connection-does-not-exist
                  (mactcp-error-code-mixin bad-connection-state)
                  ((error-code :initform -23008 :allocation :class)
                   (error-message :initform "Connection does not exist" :allocation :class)))

(define-condition tcp-connection-conflict
                  (mactcp-error-code-mixin bad-connection-state)
                  ((error-code :initform -23007 :allocation :class)
                   (error-message :initform "Request conflicts with existing connection"
                                  :allocation :class)))

(define-condition tcp-unknown-connection-state
                  (mactcp-error-message-mixin bad-connection-state)
                  ((error-message :initform "Connection is in an unknown state."
                                  :allocation :class)))

(define-condition tcp-connection-state-timeout
                  (mactcp-error-message-mixin bad-connection-state)
                  ((error-message :initform "MACTCP:  Timeout in ~S while waiting for any connection state of ~{~S~^, ~}."
                                  :allocation :class)
                   (state :initform 0 :initarg :state :reader error-connection-state)         ; current state
                   (states :initform 0 :initarg :states :reader error-connection-states)))   ; acceptable states

(defmethod report-mactcp-error ((error tcp-connection-state-timeout) stream)
  (with-slots (state states error-message) error
    (format stream error-message  
            (tcp-state-name state) (mapcar #'tcp-state-name states)))) 

(define-condition connection-closed (bad-connection-state) ())

(define-condition tcp-connection-closed
                  (mactcp-error-code-mixin connection-closed)
                  ((error-code :initform -23005 :allocation :class)
                   (error-message :initform "TCP connection closed" :allocation :class)))

(define-condition connection-lost (connection-closed))

(define-condition tcp-connection-reset
                  (mactcp-error-code-mixin connection-lost)
                  ((error-code :initform -23012 :allocation :class)
                   (error-message :initform "Connection terminated" :allocation :class)))

(define-condition host-stopped-responding
                  (host-not-responding remote-network-error) ())

(define-condition tcp-host-stopped-responding
                  (mactcp-error-code-mixin host-stopped-responding)
                  ((error-code :initform -23033 :allocation :class)
                   (error-message :initform "Destination host is not responding" :allocation :class)))

(define-condition connection-error (remote-network-error) ())

(define-condition connection-refused (connection-error) ())

;; uncertain that this error code actually maps correctly.
(define-condition tcp-connection-refused
                  (mactcp-error-code-mixin connection-refused)
                  ((error-code :initform -23015 :allocation :class)
                   (error-message :initform "TCP open failed" :allocation :class)))

(define-condition protocol-timeout (remote-network-error) ())

(define-condition tcp-command-timeout
                  (mactcp-error-code-mixin protocol-timeout)
                  ((error-code :initform -23016 :allocation :class)
                   (error-message :initform "TCP command timeout" :allocation :class)))

(define-condition network-parse-error (error) ())

(define-condition tcp-syntax-error
                  (mactcp-error-code-mixin network-parse-error)
                  ((error-code :initform -23041 :allocation :class)
                   (error-message :initform "Syntax error in host name" :allocation :class))) 

;;;------------------------------------------------------------------- 
;;;
;;; APPLETALK ERRORS
;;;

(define-condition appletalk-error (error) ())

(define-condition appletalk-driver-open-error (interface-not-working appletalk-error) ())

(define-condition appletalk-port-already-in-use
                  (mactcp-error-code-mixin appletalk-driver-open-error)
                  ((error-code :initform -97 :allocation :class)
                   (error-message :initform "AppleTalk unable to initialize; port already in use." :allocation :class)))

(define-condition appletalk-inactive
                  (mactcp-error-code-mixin appletalk-driver-open-error)
                  ((error-code :initform -98 :allocation :class)
                   (error-message :initform "AppleTalk inactive; parameter RAM not configured for this connection"
                                  :allocation :class))) 

;;;------------------------------------------------------------------- 
;;;
;;; EXPORT CONDITIONS
;;;

(export '(bad-connection-state
          connection-closed
          connection-error
          connection-lost
          connection-refused
          domain-error
          domain-resolver-error
          error-loading-mactcp
          host-not-responding
          host-stopped-responding
          interface-not-working
          local-network-error
          mactcp-error
          network-domain-cache-fault
          network-error
          network-parse-error
          network-resources-exhausted
          protocol-timeout
          remote-network-error
          tcp-bad-ip-address
          tcp-bad-ip-configuration
          tcp-bad-network-connection
          tcp-command-timeout
          tcp-connection-closed
          tcp-connection-conflict
          tcp-connection-does-not-exist
          tcp-connection-refused
          tcp-connection-reset
          tcp-connection-state-timeout 
          tcp-domain-cache-fault
          tcp-domain-server-not-found
          tcp-duplicate-socket
          tcp-host-stopped-responding
          tcp-insufficient-memory-for-packet-size
          tcp-insufficient-resources
          tcp-insuffucient-memory-error
          tcp-internal-error
          tcp-invalid-buffer-pointer
          tcp-invalid-data-structure
          tcp-invalid-ip-packet-length
          tcp-invalid-stream-pointer
          tcp-ip-packet-error
          tcp-ip-packet-too-large
          tcp-ip-routing-error
          tcp-missing-ip-or-lap-configuration
          tcp-no-response-from-domain-server
          tcp-out-of-memory
          tcp-stream-already-open
          tcp-syntax-error
          tcp-unknown-domain-name
          unknown-address
          unknown-mactcp-error
          ;;; Appletalk errors
          appletalk-driver-open-error
          appletalk-error
          appletalk-inactive
          appletalk-port-already-in-use)
        :ccl) 

;;;------------------------------------------------------------------- 
;;;
;;;  MULTITHREADED %TCP-CONTROL
;;;

(declaim (inline %tcp-check-error-return))

(defun %tcp-check-error-return (error-number &optional ignore-errors-p)
  (if (or (zerop error-number) ignore-errors-p)
    error-number
    (%tcp-err-disp error-number))) 

;;; These three replace %tcp-control. 
(defun %tcp-control-sync (pb routine-code &optional ignore-errors-p)
  (setf (rref pb tcpiopb.cscode) routine-code)
  (%tcp-check-error-return (#_PBControlSync pb) ignore-errors-p))

(defun %tcp-control-async (pb routine-code)
  (setf (rref pb tcpiopb.cscode) routine-code
        (rref pb tcpiopb.iocompletion) *null-ptr*)
  (%tcp-check-error-return (#_PBControlAsync pb)))

;; Timeout is in seconds.
#+old
(defun %tcp-control-async-wait (pb routine-code whostate &optional timeout) 
  (let ((err (%tcp-control-async pb routine-code)))
    (declare (fixnum err))
    (flet ((call-complete-p ()
             (setq err (rref pb tcpiopb.ioresult))
             ;; Positive numbers mean still working.
             ;; Zero means successful completion.
             ;; Negative means error completion.
             (not (plusp err))))
      (declare (dynamic-extent #'call-complete-p))
      ;; Give everyone else a chance to run
      (process-allow-schedule)
      ;; Unless we got the answer on that round, block until we get it.
      (unless (call-complete-p)
        (process-wait-with-timeout whostate (and timeout (* 60 (the fixnum timeout))) #'call-complete-p))
      ;; Check for successful completion outside wait function.
      (cond ((zerop err))
            ((< err 0) (%tcp-err-disp err))
            (t (%tcp-err-disp -23016)))         ; command-timeout
      err)))

(defun %tcp-control-async-wait (pb routine-code whostate &optional timeout) 
  (let ((err (%tcp-control-async pb routine-code)))
    (declare (fixnum err))
    (flet ((call-complete-p ()
             (setq err (rref pb tcpiopb.ioresult))
             ;; inProgress numbers mean still working.
             ;; Zero means successful completion.
             ;; Negative means error completion.
             (/= err #$inProgress)))
      (declare (dynamic-extent #'call-complete-p))
      ;; Give everyone else a chance to run
      (process-allow-schedule)
      ;; Unless we got the answer on that round, block until we get it.
      (unless (call-complete-p)
        (process-wait-with-timeout whostate (and timeout (* 60 (the fixnum timeout))) #'call-complete-p))
      ;; Check for successful completion outside wait function.
      (cond ((zerop err))
            ((= err #$inProgress) (%tcp-err-disp -23016))       ; command-timeout
            (t (%tcp-err-disp err)))
      err)))

;;;------------------------------------------------------------------- 
;;;
;;; This abstraction is for copying the PB on the stack to avoid collisions 
;;; with assynchronous calls to %TCP-CONTROL, which is a good way to crash.   
;;;

(declaim (inline %tcp-pb-copy))

(defun %tcp-pb-copy (src dest)
  ; I will RTFM after I finish downloading TFM.
  ; Some of the fields in src are "persistent"; copy all of
  ; them, for now
  (#_BlockMoveData src dest $tcpPBSize))

;;;------------------------------------------------------------------- 
;;;
;;;  TCP PARAMETER OPERATIONS
;;; 

;Timeout should be an arg...
(defun tcp-active-open (address port &key (bufsize *tcp-buffer-size*) (rdsentries 6) (writebufsize *tcp-write-buffer-size*) 
                                notify-proc (commandtimeout *tcp-command-timeout*)
                                (read-timeout *tcp-read-timeout*))
  (unless (integerp port)
    (setq port (tcp-service-port-number port)))
  (unless  (integerp address)
    (setq address (tcp-host-address address)))
  (let ((conn nil))
    (unwind-protect
      (setq conn (tcp-allocate-connection rdsentries read-timeout notify-proc bufsize writebufsize))
      (%tcp-active-open (conn-pb conn) address port commandtimeout read-timeout)
      (setf (conn-state conn) :open)
      (prog1 conn (setq conn nil))
      (when conn
        (tcp-deallocate-connection conn)))))

(defun %tcp-create (pb RcvBuff RcvBuffLen notifyProc)
  (setf (rref pb tcpioPB.ioCRefNum) (tcp-driver-refnum)
        (rref pb tcpioPB.create.RcvBuff) RcvBuff
        (rref pb tcpioPB.create.RcvBuffLen) RcvBuffLen
        (rref pb tcpioPB.create.notifyProc) (or notifyProc *null-ptr*))
  (%tcp-control-sync pb $TCPCreate))

; Wait for a connection (from any host, port) to us.
(defun %tcp-passive-open (pb port &optional (timeout *tcp-command-timeout*))
  (setf (rref pb tcpioPB.open.validityFlags) 0
        (rref pb tcpioPB.open.commandTimeoutValue) timeout
        (rref pb tcpioPB.open.localPort) port
        (rref pb tcpioPB.open.optionCnt) 0
        (rref pb tcpioPB.open.remoteHost) 0
        (rref pb tcpioPB.open.remotePort) 0
        (rref pb tcpioPB.open.timeToLive) 0)      ; time-to-live = 60 hops
  (%tcp-control-async pb $TCPPassiveOpen))

(defun %tcp-active-open (pb address port &optional (timeout *tcp-command-timeout*) (read-timeout *tcp-read-timeout*))
  (setf (rref pb tcpioPB.open.validityFlags) $TCPtimeoutValue    ; pay attention to timeout
        (rref pb tcpioPB.open.ulpTimeoutValue) read-timeout     ; readtime out
        (rref pb tcpioPB.open.commandTimeoutValue) timeout
        (rref pb tcpioPB.open.localPort) 0       ; default our port
        (rref pb tcpioPB.open.timeToLive) 0      ; time-to-live = 60 hops
        (rref pb tcpioPB.open.optionCnt) 0       ; What are TCP options?
        (rref pb tcpioPB.open.localHost) (%tcp-getaddr)
        (rref pb tcpioPB.open.remoteHost) address
        (rref pb tcpioPB.open.remotePort) port)
  (%tcp-control-async-wait pb $TCPActiveOpen "TCP Open" read-timeout))

(defun %tcp-getaddr ()
  (rletz ((pb :IPParamBlock))
    (setf (rref pb IPParamBlock.ioCRefNum) (tcp-driver-refnum))
    (%tcp-control-async-wait pb $ipctlGetAddr "TCP DNS Lookup")
    (values (rref pb IPParamBlock.ourAddress)
            (rref pb IPParamBlock.ourNetMask))))

;; Unsafe PB call
(defun %tcp-send (pb bufptr buflen push-p)
  (when (%i> buflen #xFFFF) 
    (report-bad-arg buflen '(integer 0 #xFFFF)))
  (%stack-block ((wds 8))
    (%put-word wds buflen 0)
    (%put-ptr wds bufptr 2)
    (%put-word wds 0 6)
    (setf (rref pb tcpioPB.send.wdsPtr) wds
          (rref pb tcpioPB.send.pushFlag) (if push-p -1 0)
          (rref pb tcpioPB.send.urgentFlag) 0
          (rref pb tcpioPB.send.validityFlags) 0)
    (%tcp-control-async-wait pb $TCPSend "TCP Out")))

(defun tcp-send (pb string push-p)
  (if (<= (length string) 1024)
    (with-cstr (buf string)
      (%tcp-send pb buf (length string) push-p))
    (multiple-value-bind (sstr start end) (get-sstring string)
      (declare (type fixnum start end))
      (%stack-block ((buf 1024))
        ; This code is untested because nobody calls TCP-SEND
        (let ((sstr-ptr (%null-ptr))
              len)
          (declare (dynamic-extent sstr-ptr)
                   (type macptr sstr-ptr)
                   (type fixnum len))
          (loop
            (setq len (- end start))
            (if (<= len 0) (return))
            (if (< 1024 len) (setq len 1024))
            (without-interrupts        ; GC protection because lisp pointers can move -- JCMa 3/9/1996.
             (%address-to-macptr sstr sstr-ptr)
             (#_BlockMoveData (%inc-ptr sstr-ptr (+ #+ppc-target -2 #-ppc-target 3 start)) buf len))
            (setq start (+ start 1024))
            (%tcp-send pb buf len (and push-p (>= start end)))))))))

(defun %address-to-macptr (address &optional (macptr (%null-ptr)))
  (%setf-macptr macptr (%int-to-ptr (%address-of address))))

(defun %tcp-rcv (pb ptr len timeout)
  (setf (rref pb tcpioPB.Receive.commandTimeoutValue) timeout
        (rref pb tcpioPB.Receive.rcvBuff) ptr
        (rref pb tcpioPB.Receive.rcvBuffLen) (require-type len '(integer 0 #xFFFF)))
  (%tcp-control-async-wait pb $TCPRcv "TCP In")
  (rref pb tcpioPB.Receive.rcvBuffLen))

(defun %tcp-bfrreturn (pb rds)
  (setf (rref pb tcpioPB.Receive.rdsPtr) rds)
  (%tcp-control-sync pb $TCPRcvBfrReturn))

(defun %tcp-nocopyrcv (pb rdsptr rdslen timeout)
  (setf (rref pb tcpioPB.Receive.commandTimeoutValue) timeout
        (rref pb tcpioPB.Receive.rdsPtr) rdsptr
        (rref pb tcpioPB.Receive.rdsLength) (require-type rdslen '(integer 0 #xFFFF)))
  (%tcp-control-async-wait pb $TCPNoCopyRcv "TCP In"))

(defun %tcp-close (pb)
  (setf (rref pb tcpioPB.close.validityFlags) 0)
  (%tcp-control-async-wait pb $TCPClose "TCP Close"))

(declaim (inline %tcp-abort))

(defun %tcp-abort (pb &optional ignore-errors)
  (%tcp-control-sync pb $TCPAbort ignore-errors))      ; needs to ignore an error

;; safe tcp abort
(defun tcp-abort (primary-pb &optional ignore-errors)
  (if primary-pb
    (with-pb-copy (pb primary-pb)
      (%tcp-abort pb ignore-errors))
    0))

(defun %tcp-release (pb)                ; This does a TCPAbort...
  (unless (%null-ptr-p (rref pb tcpioPB.StreamPtr))
    (%tcp-control-sync pb $TCPRelease)        ; vagaries of MACTCP could cause a crash if not synchronous.
    (setf (rref pb tcpioPB.StreamPtr) *null-ptr*))
  nil)

(defun tcp-release (pb)
  (unless (%null-ptr-p pb)
    (%tcp-release pb)
    (%setf-macptr pb *null-ptr*)))

(defun %pb-deallocate (pb)
  (unless (%null-ptr-p (rref pb tcpioPB.StreamPtr))
    (%tcp-control-sync pb $TCPRelease T))       ; synchronous because of release and ignore errors
  (#_DisposePtr pb))

;;;------------------------------------------------------------------- 
;;;
;;; DNS RESOLVER 
;;;


#+ppc-target
(progn
  
  (defun find-folder-ids (type)
    (rlet ((vrefnumP :signed-integer)
           (diridP   :signed-long))
      (when (eql #$noErr (#_FindFolder #$kOnSystemDisk type #$kDontCreateFolder
                          vRefNumP dirIDP))
        (values (%get-signed-word vrefnumP)
                (%get-signed-long diridP)))))
  
  (defun find-system-folder ()
    (find-folder-ids #$kSystemFolderType))
  
  (defun find-control-panels-folder ()
    (find-folder-ids #$kControlPanelFolderType))
  
  (defun HOpenResFile (vrefnum dirid name permission)
    (#_HOpenResFile vrefnum dirid name permission))
  
  ) ; end #+ppc-target progn


#-ppc-target
(progn
  
  (defun find-system-folder ()
    (let* ((wdrefnum 
            (rlet ((info :SysEnvRec))
              (#_SysEnvirons 1 info)
              (rref info SysEnvRec.sysVRefNum))))
      (rlet ((pb :hparamblockrec)
             (nameptr (:string 31)))
        (setf (rref pb :hparamblockrec.ioWDIndex) 0
              (rref pb :hparamblockrec.ionameptr) nameptr
              (rref pb :hparamblockrec.ioVrefNum) wdrefnum
              (rref pb :hparamblockrec.ioWDProcID) 0)
        (if (eql #$noErr (#_PBGetWDInfoSync pb))
          (values (rref pb :hparamblockrec.ioWDVrefNum)
                  (rref pb :hparamblockrec.ioWDDirID))
          (values nil nil)))))
  
  (defun find-control-panels-folder ()
    (let* ((vrefnum nil)
           (dirID nil))
      (when (gestalt #$gestaltFindFolderAttr #$gestaltFindFolderPresent)
        (rlet ((vrefnumP :signed-integer)
               (diridP :signed-long))
          (when (eql #$noErr 
                     (#_FindFolder 
                      #$kOnSystemDisk 
                      #$kControlPanelFolderType
                      #$kDontCreateFolder
                      vRefNumP
                      dirIDP))
            (setq vrefnum (%get-signed-word vrefnump)
                  dirid (%get-signed-long diriDP)))))
      (values vrefnum dirID)))
  
  (defun set-vrefnum (vrefnum)
    (rletZ ((pb :paramblockrec))
      (setf (pref pb :paramblockrec.iovrefnum) vrefnum)
      (errchk (#_PBSetVolSync pb))
      vrefnum))
  
  (defun get-vrefnum ()
    (rletZ ((pb :paramblockrec))
      (errchk (#_PBGetVolSync pb))
      (pref pb :paramblockrec.iovrefnum)))
  
  ; Don't need this on the PPC since #_HOpenResFile always exists there
  ; trap-available-p algorithm comes from the Think Reference
  (defun num-toolbox-traps ()
    (with-macptrs (p1 p2)
      (if (eql (%setf-macptr p1 (#_NGetTrapAddress :tool #_InitGraf))
               (%setf-macptr p2 (#_NGetTrapAddress :tool (+ #_InitGraf #x200))))
        #x0200
        #x0400)))
  
  (defmacro tool-trap-p (theTrap)
    `(not (eql 0 (logand ,theTrap #x800))))
  
  (defun trap-available-p (theTrap)
    (with-macptrs (p1 p2)
      (if (tool-trap-p theTrap)
        (unless (>= (setq theTrap (logand theTrap #x07ff)) (num-toolbox-traps))
          ; The %setf-macptr's here are to prevent consing
          (not (eql (%setf-macptr p1 (#_NGetTrapAddress :tool thetrap))
                    (%setf-macptr p2 (#_NGetTrapAddress :tool _UnImplemented)))))
        (not (eql (%setf-macptr p1 (#_NGetTrapAddress thetrap))
                  (%setf-macptr p2 (#_NGetTrapAddress :tool _UnImplemented)))))))
  
  (defvar *hopenresfile-available-p* nil)
  
  (def-load-pointers *hopenresfile-available-p* ()
    (setq *hopenresfile-available-p* (trap-available-p #_HOpenResFile)))
  
  (defun HOpenResFile (vrefnum dirid name permission)
    (if *HOpenResFile-available-p*
      (#_HOpenResFile vrefnum dirid name permission)
      (rletZ ((pb :WDPBRec))
        (setf (pref pb :WDPBRec.ioWDDirID) dirid
              (pref pb :WDPBRec.ioWDProcID) :ccl2
              (pref pb :WDPBRec.ioVRefnum) vrefnum)
        (let ((err -1))
          (unwind-protect
            (progn
              (setq err (#_PBOpenWDSync pb))
              (unless (eql 0 err)
                (%err-disp err))
              (with-vrefnum (pref pb :WDPBRec.ioVRefnum)
                (#_OpenResFile name)))
            (when (eql 0 err) (#_PBCloseWDSync pb)))))))
  
  ) ; end #+ppc-target progn


(defun %load-resolver-code ()
  (or %resolver-code%
      (multiple-value-bind (cpvrefnum cpdirid)
                           (find-control-panels-folder)
        (setq %resolver-code%
              (or (%find-dnr "cdev" "ztcp" cpvrefnum cpdirid)    ; 1.1
                  (multiple-value-bind (sysvrefnum sysdirid)
                                       (find-system-folder)
                    (%find-dnr "cdev" "mtcp" sysvrefnum sysdirid))  ; 1.0.x in system folder
                  (%find-dnr "cdev" "mtcp" cpvrefnum cpdirid)    ; 1.0.x in control panels folder
                  (error "Can't load MacTCP Domain Name Resolver"))))))

(defun %find-dnr (type creator vrefnum dirid)
  ; Returns detached handle to DNRP resource or NIL.
  (when vrefnum
    (rlet ((name (:string 255))
           (pb :hparamblockrec))
      (setf (rref pb :hparamblockrec.ionameptr) name
            (rref pb :hparamblockrec.iovrefnum) vrefnum
            (rref pb :hparamblockrec.ioDirID) dirid
            (rref pb :hparamblockrec.ioFDirIndex) 1)
      (do* ()
           ((not (eql #$noErr (#_PBHGetFinfoSync pb))) nil)
        (if (and (string= (rref pb :hparamblockrec.ioFlFndrInfo.fdType) type)
                 (string= (rref pb :hparamblockrec.ioFlFndrInfo.fdCreator) creator))
          (let* ((refnum (HOpenResFile vrefnum dirid name #$fsRdPerm)))
            (if (eql refnum -1)
              (return nil)
              (unwind-protect
                (let* ((dnrp (#_Get1IndResource "dnrp" 1)))
                  (unless (%null-ptr-p dnrp)
                    (#_DetachResource dnrp)
                    (#_MoveHHi dnrp)
                    (#_HLock dnrp)
                    (%setf-macptr dnrp (#_StripAddress (%get-ptr dnrp)))
                    (return dnrp)))
                (#_CloseResFile refnum))))
          (progn
            (setf (rref pb :hparamblockrec.ioDirID) dirid)        ; clobbered by _PBHGetFinfoSync
            (incf (rref pb :hparamblockrec.ioFDirIndex))))))))

(defun %open-resolver (&optional hosts-file)
  (unless %resolver-code%
    (let* ((err -1))
      (unwind-protect
        (progn
          (%load-resolver-code)
          (with-cstr (np (or hosts-file ""))
            (when (null hosts-file) (%setf-macptr np *null-ptr*))
            (setq err (oserr-word (ff-call %resolver-code% :ptr np :long $openresolver :d0))))
	  ;; Positive non-zero responses are possible according to Rainer Joswig.  10/18/96 -- JCMa.
	  (unless (zerop err) (%tcp-err-disp err)))
        (unless (zerop err) (%dispose-resolver))))))

(defun %close-resolver ()
  (when %resolver-code%
    (ff-call %resolver-code% :long $closeresolver :d0) 
    (%dispose-resolver)))

(defun %dispose-resolver ()
  (when %resolver-code%
    (let ((code %resolver-code%))
      (setq %resolver-code% nil)
      (%setf-macptr code (#_RecoverHandle code))
      (#_HUnlock code)
      (#_DisposeHandle :errchk code))))

(defun %tcp-enum-cache (resultproc userdataptr)
  (%open-resolver)
  (oserr-word (ff-call %resolver-code% :ptr userdataptr :ptr resultproc  :long $enumcache :d0)))

(defun %tcp-addr-to-name (pb addr resultproc userdataptr)
  (%open-resolver)
  (setf (rref pb hostinfo.result) 1)   ; initialize
  (oserr-word (ff-call %resolver-code%
                       :ptr (or userdataptr *null-ptr*)
                       :ptr (or resultproc *null-ptr*)
                       :ptr pb
                       :long addr
                       :long $addrtoname
                       :d0)))

(defun tcp-addr-to-name (pb addr)
  (let ((err (%tcp-addr-to-name pb addr %tcp-set-result-proc% nil)))
    ;; Unless we hit the cache, block until we get an answer or the DNS request times out.
    (when (eq err $cacheFault)
      (flet ((call-complete-p ()
               (not (eql (setq err (rref pb hostinfo.result)) 1))))
        (declare (dynamic-extent #'call-complete-p))
        (process-wait "DNS Lookup" #'call-complete-p)))
    ;; Check for errors
    (unless (eql err 0) (%tcp-err-disp err))
    pb))

(defun %tcp-str-to-addr (pb host-name resultproc userdataptr)
  (%open-resolver)
  (setf (rref pb hostinfo.result) 1)   ; initialize call
  (with-cstr (np host-name)
    (oserr-word (ff-call %resolver-code%
                         :ptr (or userdataptr *null-ptr*)
                         :ptr (or resultproc *null-ptr*)
                         :ptr pb
                         :ptr np
                         :long $strtoaddr
                         :d0))))

(defun tcp-str-to-addr (pb host-name)
  (let ((err (%tcp-str-to-addr pb host-name %tcp-set-result-proc% nil)))
    ;; Unless we hit the cache, block until we get an answer or the DNS request times out.
    (when (eq err $cacheFault)
      (flet ((call-complete-p ()
               (not (eq (setq err (rref pb hostinfo.result)) 1))))
        (declare (dynamic-extent #'call-complete-p))
        (process-wait "DNS Lookup" #'call-complete-p)))
    ;; Check for errors
    (unless (eql err 0) (%tcp-err-disp err))
    pb))

#|
(defun %tcp-addr-to-str (addr strptr)
  (%open-resolver)
  (oserr-word (ff-call %resolver-code% :ptr strptr :long addr :long $addrtostr :d0)))

(defun tcp-addr-to-str (addr)
  (%stack-block ((str 16))
    (%tcp-addr-to-str addr str)
    (%get-cstring str)))
|#

(defun tcp-addr-to-str (addr)
  (format nil "~D.~D.~D.~D"
          (ldb (byte 8 24) addr)
          (ldb (byte 8 16) addr)
          (ldb (byte 8 8) addr)
          (ldb (byte 8 0) addr)))

(defun tcp-ip-str-to-addr (ip-string)
  (loop with length = (length ip-string) and answer = 0 and start = 0 
        for pos in '(24 16 8 0)
        for dot = (position #\. ip-string :start start :end length)
        for number = (parse-integer ip-string :start start :radix 10 :end (or dot length))
        do (setq answer (dpb number (byte 8 pos) answer))
        while dot
        do (setq start (1+ dot))
        finally (return answer)))

(defun %tcp-ip-string-p (string)
  (loop for idx upfrom 0 below (length string)
        for char = (aref string idx)
        unless (or (digit-char-p char 10.) (eql char #\.))
        return nil
        finally (return t))) 

(defun tcp-host-address (host-name)
  (etypecase host-name
    (integer host-name)
    (string
     (cond ((eql 0 (length host-name)) (values (%tcp-getaddr)))
           ;; Domain names must go through here when resolver off.
           ;; The host file may contain the names. -- JCMa 4/22/1996.
           ((or *use-resolver* (not (%tcp-ip-string-p host-name)))
            (rlet ((pb :hostinfo))
              (tcp-str-to-addr pb host-name)
              (rref pb hostinfo.addr1)))
           ;; Only handles IP Strings.
           (t (tcp-ip-str-to-addr host-name))))))

(defun tcp-host-cname (host-address)
  (unless (integerp host-address)
    (setq host-address (tcp-host-address host-address)))
  (if *use-resolver*
    (rlet ((pb :hostinfo))               ; avoid PB collisions
      (tcp-addr-to-name pb host-address)
      (%get-cstring pb 4))
    (tcp-addr-to-str host-address)))

(defun tcp-host-info (host-name)
  (when (integerp host-name)
    (setq host-name (tcp-addr-to-str host-name)))
  (rlet ((pb :hostinfo))
    (tcp-str-to-addr pb host-name)
    (values (%get-cstring pb 4)
            (rref pb hostinfo.addr1)
            (rref pb hostinfo.addr2)
            (rref pb hostinfo.addr3)
            (rref pb hostinfo.addr4))))

;;;------------------------------------------------------------------- 
;;;
;;; IDENTIFYING TCP PROTOCOL STATES FROM THE NUMBERS 
;;; 

;;; ((0 . :closed)
;;;  (2 . :listen)
;;;  (4 . :syn-received)
;;;  (6 . :syn-sent)
;;;  (8 . :established)
;;;  (10 . :fin-wait-1)
;;;  (12 . :fin-wait-2)
;;;  (14 . :close-wait)
;;;  (16 . :closing)
;;;  (18 . :closing-last-ack)
;;;  (20 . :closing-time-ack))

(defun tcp-state-name (state-number)
  (declare (optimize (speed 3) (safety 0)))
  (if (and (integerp state-number) (<=  0 state-number 20))
    (aref +state-name-array+ (ash state-number -1))
    state-number)) 

(defun tcp-connection-state-name (conn)
  (tcp-state-name (tcp-connection-state conn))) 

;; old definition -- 8/3/96 -- JCMa.
;(defun %tcp-force-output (conn push-p)
;   (unless (eql 0 (conn-write-count conn))
;      (let ((conn-pb (conn-pb conn)))
;         (when conn-pb
;             (with-pb-copy (pb conn-pb)
;                 (%tcp-send pb (conn-write-buffer conn) (conn-write-count conn) push-p))))
;      (setf (conn-write-count conn) 0)))

;; Specializations of these generic functions are run immediately before and
;; after the tcp buffer is transmitted.  This hook can be used by applications
;; to perform encodings on the data transmitted.  For example, the CL-HTTP Web
;; Server uses this to implement chunked transfer encoding. No unsafe PB calls
;; should be done within these methods. These are in lieu of a real stream
;; buffer protocols of the form:
;;
;;      discard-current-output-buffer
;;              discard-output-buffer
;;      send-current-output-buffer
;;              send-output-buffer
;;              discard-output-buffer
;;      setup-new-output-buffer
;;              send-current-output-buffer
;;              new-output-buffer
;;      stream-output-buffer

(defgeneric tcp-encoder-before-tcp-send (encoder conn))

(defgeneric tcp-encoder-after-tcp-send (encoder conn))

;; Safe PB call
(defun %tcp-force-output (conn push-p)
  (unless (eql 0 (conn-write-count conn))
    (let ((conn-pb (conn-pb conn))
          (encoder (conn-encoder conn)))
      (when conn-pb
        (when encoder
          (tcp-encoder-before-tcp-send encoder conn))
        (with-pb-copy (pb conn-pb)
          (%tcp-send pb (conn-write-buffer conn) (conn-write-count conn) push-p)))
      (prog1 (setf (conn-write-count conn) 0)
        (when encoder
          (tcp-encoder-after-tcp-send encoder conn)))))) 

(defun tcp-force-output (conn push-p)
  (tcp-with-connection-grabbed (conn *current-process* "TCP Out")
    (%tcp-force-output conn push-p)))

(defun tcp-bfr-return (conn)
  (%setf-macptr (conn-read-bufptr conn) *null-ptr*)
  (setf (conn-read-count conn) 0)        ; Usually redundant except in clear-input..
  (with-pb-copy (pb (conn-pb conn))
    (%tcp-bfrreturn pb (conn-rds conn))))

(defun tcp-clear-input (conn)
  (setf (conn-untyi-char conn) nil)
  (unless (eql 0 (conn-read-count conn))
    (tcp-bfr-return conn)))

(defun tcp-clear-output (conn)
  (setf (conn-write-count conn) 0))

(defun tcp-eofp (conn)
  (let ((conn-pb (conn-pb conn)))
    (if conn-pb
      (with-pb-copy (pb conn-pb)
        (let ((err (%tcp-control-sync pb $TCPStatus t)))         ; quick no data wait
          (case err
            (0  (memq (rref pb tcpioPB.status.connectionState)
                      '(0                           ; Closed
                        14                          ; Close Wait
                        16                          ; Closing
                        18                          ; Last Ack
                        20)))                        ; Time Wait
            (-23008 t)                 ; connection doesn't exist
            (t (%tcp-err-disp err)))))
      t)))                           ; no pb means eof for sure!

;;;we should get the following sequence -- Karsten  2/16/1995.
;;; Can hang indefinitely so wait for :CLOSING-TIME-ACK -- Reti 2/17/1995.
;;;  No -57 error with Netscape. MacWeb and Mosaic work fine.
;;;:FIN-WAIT-1 (10)
;;;:FIN-WAIT-2 (12)
;;;:CLOSING-TIME-ACK (20)
;;;:CLOSED (0)
(defun %tcp-wait-for-orderly-close (conn timeout)
  (with-pb-copy (pb (conn-pb conn))
    (flet ((close-complete-p ()
             (member (%tcp-connection-state pb t) ' (0) :test #'eql)))
      (declare (dynamic-extent #'close-complete-p))
      (or (close-complete-p)
          (process-wait-with-timeout "TCP Close Wait" (* 60 (the fixnum timeout)) #'close-complete-p))))) 

(defun %tcp-wait-for-connection-state (conn states &optional timeout (whostate "TCP State Wait") &aux state)
  (check-type states cons)
  (with-pb-copy (pb (conn-pb conn))
    (flet ((connection-in-state-p ()
             (setq state (%tcp-connection-state pb t))
             (member state states :test #'eql)))
      (declare (dynamic-extent #'close-complete-p))
      (or (connection-in-state-p)
          (progn (process-wait-with-timeout whostate (* 60 (the fixnum timeout)) #'connection-in-state-p)
                 (unless (member state states :test #'eql)
                   (error 'tcp-connection-state-timeout :state state :states states)))))))

(defun tcp-local-port (conn)
  (let ((primary-pb (conn-pb conn)))
    (when primary-pb
      (with-pb-copy (pb primary-pb)
        (let ((err (%tcp-control-sync pb $TCPStatus T)))
          (case err
            (0 (rref pb tcpiopb.status.localport))
            ((-23005 -23008) nil)            ; no connection
            (t (%tcp-err-disp err))))))))

(defun tcp-remote-port (conn)
  (let ((primary-pb (conn-pb conn)))
    (when primary-pb
      (with-pb-copy (pb primary-pb)
        (let ((err (%tcp-control-sync pb $TCPStatus T)))
          (case err
            (0 (rref pb tcpiopb.status.remoteport))
            ((-23005 -23008) nil) ; no connection
            (t (%tcp-err-disp err))))))))

(defun tcp-remote-host (conn)
  (let ((primary-pb (conn-pb conn)))
    (when primary-pb
      (with-pb-copy (pb primary-pb)
        (let ((err (%tcp-control-sync pb $TCPStatus T)))
          (case err
            (0 (rref pb tcpiopb.status.remotehost))
            ((-23005 -23008) nil) ; no connection
            (t (%tcp-err-disp err))))))))

(defun tcp-write-byte (conn byte)
  (macrolet ((write-the-byte (buf byte count)
               `(progn (%put-byte ,buf ,byte ,count)
                       (incf (conn-bytes-transmitted conn))))
             (maybe-send-tcp-buffer (conn count)
               `(when (eql (conn-write-bufsize ,conn) ,count)
                  (%tcp-force-output ,conn nil)
                  (setq ,count (conn-write-count ,conn)))))
    (let ((count (conn-write-count conn)))
      (declare (fixnum count))
      (maybe-send-tcp-buffer conn count)
      (setf (conn-write-count conn) (1+ count))
      (write-the-byte (conn-write-buffer conn) byte count))))

;;  We're doing small hacks here. By making the two standard methods
;; for gettting a new input buffer into CLOS methods on CONN, we can add
;; around methods to allow reading of chunked content transfer encoded data
;; in http 1.1 for CL-HTTP.  This is an expedient but not desirable approach.
;;  These are in lieu of a real stream buffer protocol of the form:
;;
;;  These are consumer operations
;;
;;      read-input-buffer
;;              setup-next-input-buffer
;;      advance-input-buffer (consumer)
;;              discard-current-input-buffer
;;
;; These are producer operations
;;
;;      next-input-buffer 
;;               -- get buffer from tcp
;;      discard-input-buffer
;;               -- return buffer to tcp
;;      discard-current-input-buffer
;;              discard-input-buffer
;;      setup-next-input-buffer
;;              discard-current-input-buffer
;;              next-input-buffer
;;      stream-input-buffer

(defmethod tcp-get-next-input-buffer (conn &optional (check-eof-p t) need-more-data-p)
  (cond ((or need-more-data-p (zerop (conn-read-count conn)))
         (when (and check-eof-p (tcp-eofp conn))
           (return-from tcp-get-next-input-buffer nil))
         (let ((pb (conn-pb conn))
               (rds (conn-rds conn)))
           (%tcp-nocopyrcv pb rds (conn-rds-entries conn) (conn-read-timeout conn))
           (when (zerop (setf (conn-read-count conn) (%get-word rds)))
             (tcp-bfr-return conn)
             (when (tcp-eofp conn)     ; no data available
               (return-from tcp-get-next-input-buffer nil))
             (error "Can't read data from ~S" conn))
           (%setf-macptr (conn-read-bufptr conn) (%get-ptr rds 2))
           (setf (conn-rds-offset conn) 6)
           t))
        (t nil)))

(defmethod tcp-advance-input-buffer (conn &optional advance-p)
  (cond ((or advance-p (zerop (conn-read-count conn)))
         (let* ((rds (conn-rds conn))
                (nextbuf (conn-rds-offset conn))
                (bufptr (conn-read-bufptr conn)))
           (cond ((zerop (setf (conn-read-count conn) (%get-word rds nextbuf)))
                  (tcp-bfr-return conn))
                 (t (%setf-macptr bufptr (%get-ptr rds (+ nextbuf 2)))
                    (setf (conn-rds-offset conn) (+ nextbuf 6))))))
        ( t nil)))

(defun tcp-read-byte (conn)
  (and ;; maybe get next input buffer
   (if (zerop (conn-read-count conn)) (tcp-get-next-input-buffer conn t t) t)
   ;; read a byte
   (prog1 (%get-byte (conn-read-bufptr conn))
     (%incf-ptr (conn-read-bufptr conn))
     (incf (the fixnum (conn-bytes-received conn)))
     (when (zerop (decf (conn-read-count conn)))
       (tcp-advance-input-buffer conn t))))) 

(defun tcp-read-char (conn)
  (if (conn-untyi-char conn)
    (prog1 (conn-untyi-char conn) 
      (setf (conn-untyi-char conn) nil)
      (incf (the fixnum (conn-bytes-received conn))))
    (let ((byte (tcp-read-byte conn)))
      (and byte (code-char byte)))))

(defun tcp-write-vector (conn v start end &aux (bytes 0))
  (declare (fixnum start end bytes)
           (optimize (speed 3) (safety 0)))
  (multiple-value-bind (vector offset) 
                       (array-data-and-offset v)
    (declare (fixnum offset))
    (setq start (+ start offset))
    (let* ((writebuf (conn-write-buffer conn))
           (bufsize (conn-write-bufsize conn))) 
      (do* ((length (- (+ end offset ) start) (- length room-in-buffer))
            (bufpos (conn-write-count conn) 0)
            (room-in-buffer (- bufsize bufpos) bufsize))
           ((<= length room-in-buffer)
            (dotimes (i length (progn (incf (conn-write-count conn) length) (%tcp-force-output conn t)))
              (%put-byte writebuf (aref vector start) bufpos)
              (setq start (1+ start) bufpos (1+ bufpos))))
        (declare (fixnum length bufpos bufsize room-in-buffer))
        (dotimes (i room-in-buffer)
          (%put-byte writebuf (aref vector start) bufpos)
          (setq start (1+ start) bufpos (1+ bufpos) bytes (1+ bytes)))
        (setf (conn-write-count conn) bufsize)
        (%tcp-force-output conn t))
      (incf (conn-bytes-transmitted conn) (the fixnum bytes)))))

(defun tcp-read-vector (conn v &optional (start 0) (end (length v)) &aux (bytes 0))
  (declare (fixnum start end bytes))
  (multiple-value-bind (vector offset)
                       (array-data-and-offset v)
    (declare (fixnum offset))
    (setq start (+ start offset))
    (let ((length (- (+ end offset ) start)))
      (declare (fixnum length))
      (tcp-with-connection-grabbed (conn *current-process* "TCP In")
        (when (> length 0)
          (let ((untyi-char (conn-untyi-char conn)))
            (when untyi-char
              (setf (uvref vector start) (char-code untyi-char)
                    (conn-untyi-char conn) nil
                    bytes (1+ bytes)
                    start (1+ start)
                    length (1- length)))))
        (loop until (zerop length)
              while (if (zerop (conn-read-count conn))  (tcp-get-next-input-buffer conn nil t) t)
              do (setf (uvref vector start) (%get-unsigned-byte (conn-read-bufptr conn)))
              (incf start)
              (%incf-ptr (conn-read-bufptr conn))
              (decf length)
              (when (zerop (decf (conn-read-count conn)))
                (tcp-advance-input-buffer conn t))
              finally (incf (conn-bytes-received conn) bytes))))))

(require :mac-file-io) 

;; the only way one does much better than this is double buffered asynchronous transfer-- JCMa 4/12/1995.
;; Blasts the bits without CR-LF translation
(defun tcp-write-file (conn pathname)
  (with-fsopen-file (pb pathname)
    (let* ((write-buf (conn-write-buffer conn))
           (bufsize (the fixnum (conn-write-bufsize conn)))
           (filesize (the integer (geteof pb)))
           (number-of-buffers (the fixnum (truncate filesize bufsize)))
           (finish-bytes (the fixnum (- filesize (the integer (* number-of-buffers bufsize))))))
      (flet ((load-buffer (bytes)
               (fsread pb bytes write-buf)
               (setf (conn-write-count conn) bytes)))
        (declare (inline load-buffer))
        (loop initially (%tcp-force-output conn t)        ; ensure that the buffer is empty
              for bufnum from  number-of-buffers downto 1
              do (load-buffer bufsize)
              (%tcp-force-output conn t)
              finally (unless (zerop finish-bytes)    ; send the residual bytes
                        (load-buffer finish-bytes)
                        (%tcp-force-output conn t))
              ;; increment the number of bytes transmitted
              (incf (conn-bytes-transmitted conn) filesize))))))

(defun tcp-write-file-bytes (conn pathname bytes  &optional (start 0))
  (with-fsopen-file (pb pathname)
    (let* ((write-buf (conn-write-buffer conn))
           (bufsize (the fixnum (conn-write-bufsize conn)))
           (filesize (the integer (geteof pb)))
           (number-of-buffers (the fixnum (truncate bytes bufsize)))
           (finish-bytes (the fixnum (- bytes (the integer (* number-of-buffers bufsize))))))
      (flet ((load-buffer (bytes)
               (fsread pb bytes write-buf)
               (setf (conn-write-count conn) bytes)))
        (declare (inline load-buffer))
        ;; check for bad arguments
        (cond ((<= (+ start bytes) filesize))
              ((< start filesize)
               (error "Attempt to read past the end of ~A, whose size is ~D, not ~D." 
                      pathname filesize (+ start bytes)))
              (t (error "START is greater than the file size, ~D." filesize)))
        (setfpos pb start)        ;set file position
        (loop initially (%tcp-force-output conn t)        ; ensure that the buffer is empty
              for bufnum from  number-of-buffers downto 1
              do (load-buffer bufsize)
              (%tcp-force-output conn t)
              finally (unless (zerop finish-bytes)    ; send the residual bytes
                        (load-buffer finish-bytes)
                        (%tcp-force-output conn t))
              ;; increment the number of bytes transmitted
              (incf (conn-bytes-transmitted conn) bytes))))))

(defun %tcp-read-bytes-to-file (conn pathname bytes &optional (start 0))
  (create-file pathname :if-exists :error)
  (with-fsopen-file
    (pb pathname t)
    (unless (zerop start)
      (setfpos pb start))
    (tcp-with-connection-grabbed
      (conn *current-process* "TCP In")
      (loop with bytes-remaining = bytes
	    until (zerop bytes-remaining)
	    while (if (zerop (conn-read-count conn))  (tcp-get-next-input-buffer conn nil t) t)
	    for read-buffer =  (conn-read-bufptr conn)
	    for buffer-size = (min (conn-read-count conn) bytes-remaining)
	    do (fswrite pb buffer-size read-buffer)
            (%incf-ptr (conn-read-bufptr conn) buffer-size)
            (decf bytes-remaining buffer-size)
            (setf (conn-read-count conn) (- (conn-read-count conn) buffer-size))
	    finally (incf (conn-bytes-received conn) (- bytes bytes-remaining))))))

(defun tcp-read-bytes-to-file (conn pathname bytes &optional (start 0))
  (let* ((tmp-file (when (probe-file pathname) (gen-file-name pathname)))
	 (win-p nil))
    (cond (tmp-file
	   (unwind-protect 
             (progn (rename-file pathname tmp-file :if-exists :error)
                    (%tcp-read-bytes-to-file conn pathname bytes start)
                    (setq win-p t))
	     (if win-p 
               (delete-file tmp-file)
               (rename-file tmp-file pathname :if-exists :supersede))))
	  (t (%tcp-read-bytes-to-file conn pathname bytes start)))))

(defun tcp-close (conn)
  (tcp-with-connection-grabbed (conn *current-process* "TCP Close")
    (case (conn-state conn)
      ((:open :listen)
       (let ((state (tcp-connection-state conn)))
         ;; orginal code calls the function on stream rather than conn.
         ;; can get errors trying to output to on a closed stream.-- JCMa 2/27/1995.
         ;; formerly forced output for all states other than 0, 2, 20
         ;; doesn't make sense to do that we connection is going up or down.-- JCMa 3/23/1996.
         (when  (member state '(8) :test #'eql)
           (tcp-force-output conn t))
         (unless (zerop state)
           (with-pb-copy (pb (conn-pb conn))
             (unwind-protect
               (progn
                 (%tcp-close pb)     ;  Does the close asynchronously and code below deals with timeout. 
                 (%tcp-wait-for-orderly-close conn *tcp-close-timeout*))
               ;; Cannot exit the scope of the stack-let pb without ensuring that the asynchronous call is complete.
               ;; Must ensure that the connection is closed as well.
               (unless (zerop (tcp-connection-state conn))
                 (tcp-abort (conn-pb conn) t)))))
         (tcp-clear-input conn)     ; make sure the input buffer is cleared and reset
         (tcp-clear-output conn)))     ; make sure the output buffer is cleared
      ((:closed :allocated :unallocated))
      (t (error "The connection, ~S, is in an illegal state, ~S." conn (conn-state conn))))
    (setf (conn-state conn) :closed)))

;;;------------------------------------------------------------------- 
;;;
;;;  RESOURCING TCP CONNECTIONS
;;; 

;; Use a process queue in order to assure FIFO
(defvar *tcp-connection-queue*
  (make-process-queue "TCP Connection Resource Queue"))

(defvar *tcp-free-connections* nil)

(defvar *tcp-allocated-connections* 0)

(defvar *tcp-last-connection-serial-number* 0)

(defvar *tcp-open-stream-table* (make-hash-table :test #'equal 
                                                 :size *tcp-maximum-number-of-connections*))

(declaim (inline tcp-note-stream-closed))
;; atomically note stream closure.
(defun tcp-note-stream-closed (stream)
  (remhash stream *tcp-open-stream-table*)) 

(declaim (inline tcp-note-stream-closed))
(defun tcp-note-stream-open (stream)
  #+ignore (when (gethash stream *tcp-open-stream-table*)  ; streams were appearing multiply when talking to yourself
             (Break "Second initialize instance for ~S" stream))
  (setf (gethash stream *tcp-open-stream-table*) t))

(defun tcp-map-open-streams (function)
  (declare (dynamic-extent function))
  (flet ((fctn (s value)
           (declare (ignore value))
           (funcall function s)))
    (maphash #'fctn *tcp-open-stream-table*)))

(defun tcp-show-open-streams (&optional (stream *standard-output*))
  (flet ((show-stream (s) 
           (print s stream)))
    (declare (dynamic-extent #'show-stream))
    (maphash #'show-stream *tcp-open-stream-table*)))

(defun tcp-allocate-connection-serial-number ()
  (without-interrupts
   (incf *tcp-allocated-connections*)
   (incf *tcp-last-connection-serial-number*)))

(defun tcp-map-connection-resource (function)
  (with-process-enqueued (*tcp-connection-queue* *current-process* "TCP  map free connections")
    (loop for conn = *tcp-free-connections* then (conn-next-resource conn)
          while conn
          do (funcall function conn))))

(defun tcp-show-free-connections (&optional (stream *standard-output*))
  (tcp-map-connection-resource #'(lambda (conn) (print conn stream)))) 

;; frees all memory associated with a TCP connection
(defun %tcp-release-connection (conn)
  (without-interrupts
   (let ((pb (conn-pb conn)))
     (when pb
       (%pb-deallocate pb)
       (decf *tcp-allocated-connections*)
       (setf (conn-pb conn) nil
             (conn-write-buffer conn) nil
             (conn-write-bufsize conn) nil
             (conn-write-count conn) nil
             (conn-read-timeout conn) nil
             (conn-untyi-char conn) nil
             (conn-rds conn) nil
             (conn-rds-entries conn) nil
             (conn-rds-offset conn) nil
             (conn-read-count conn) nil
             (conn-read-bufptr conn) nil
             (conn-state conn) nil
             (conn-next-resource conn) nil
             (conn-serial-number conn) nil
             (conn-state conn) nil
             (conn-local-port conn) nil
             (conn-remote-port conn) nil
             (conn-command-timeout conn) nil
             (conn-bytes-transmitted conn) 0
             (conn-bytes-received conn) 0)))))

;; the way to shut everything down
(defun tcp-deallocate-all-connections  ()
  (flet ((release-connection (conn)
           (let ((next (conn-next-resource conn)))
             (setq *tcp-free-connections* next)
             (unless next
               (setq *tcp-last-connection-serial-number* 0))
             (%tcp-release-connection conn)))
         (close-stream (s)
           (close s :abort t)))
    (tcp-map-open-streams #'close-stream)
    (tcp-map-connection-resource #'release-connection)))

;; Before quitting ...
(defun cleanup-after-mactcp ()
  (tcp-deallocate-all-connections)
  (%close-resolver))

(pushnew 'cleanup-after-mactcp *lisp-cleanup-functions*) 

(defun %tcp-push-connection-resource (conn)
  (with-process-enqueued (*tcp-connection-queue* *current-process* "TCP Deallocate")
    (let ((next-conn *tcp-free-connections*))
      (unless (eq next-conn conn)   ; never push yourself back onto the resource.
        (when next-conn            ; Don't lose existing resources
          (setf (conn-next-resource conn) next-conn))
        (setq *tcp-free-connections* conn)))))

(defun %tcp-pop-connection-resource ()
  (with-process-enqueued (*tcp-connection-queue* *current-process* "TCP Allocate")
    (let ((conn *tcp-free-connections*))
      (when conn
        (setf *tcp-free-connections* (conn-next-resource conn)
              (conn-next-resource conn) nil))
      conn))) 

(defun tcp-deallocate-connection (conn)
  (let ((state (conn-state conn)))
    (unless (eql state :unallocated)
      (unless (eql state :closed)
        (with-pb-copy (pb (conn-pb conn))
          (%tcp-abort pb t))
        (setf (conn-state conn) :closed))
      ;; Release on deallocation to speed up allocation in this mode
      (when *tcp-reinitialize-connections*
        (%pb-deallocate (conn-pb conn))
        (setf (conn-pb conn) nil))
      ;; Clean up local state
      (setf (conn-local-port conn) nil
            (conn-remote-port conn) nil
            (conn-command-timeout conn) nil
            (conn-bytes-transmitted conn) 0
            (conn-bytes-received conn) 0
            (conn-stream conn) nil
            (conn-state conn) :unallocated))
    ;; push onto free queue
    (%tcp-push-connection-resource conn)))

(defun make-connection (tcpbufsize writebufsize rdsentries read-timeout notify-proc)
  (let ((pb (memerrchk (#_NewPtrClear (+ $tcpPBSize tcpbufsize writebufsize (+ (* 6 rdsentries) 2))))))
    (%tcp-create pb (%inc-ptr pb $tcpPBSize) tcpbufsize notify-proc)
    (make-conn :pb pb
               :write-buffer (%inc-ptr pb (+ $tcpPBSize tcpbufsize))
               :write-bufsize writebufsize
               :write-count 0
               :read-timeout read-timeout
               :untyi-char nil
               :rds (%inc-ptr pb (+ $tcpPBSize tcpbufsize writebufsize))
               :rds-entries rdsentries
               :rds-offset 0
               :read-count 0
               :read-bufptr (%null-ptr)
               :serial-number (tcp-allocate-connection-serial-number)
               :lock (make-lock)
               :next-resource nil
               :state :unallocated
               :local-port nil
               :remote-port nil
               :command-timeout nil
               :bytes-transmitted 0
               :bytes-received 0
               :tcp-buffer-size tcpbufsize
               :notify-procedure notify-proc)))

(defun %reinitialize-tcp-connection (conn tcpbufsize writebufsize rdsentries notify-proc)
  (let ((pb (conn-pb conn)))
    (when pb
      (%pb-deallocate pb))
    (setq pb (memerrchk (#_NewPtrClear (+ $tcpPBSize tcpbufsize writebufsize (+ (* 6 rdsentries) 2)))))
    (%tcp-create pb (%inc-ptr pb $tcpPBSize) tcpbufsize notify-proc)
    (setf (conn-pb conn) pb
          (conn-write-buffer conn) (%inc-ptr pb (+ $tcpPBSize tcpbufsize))
          (conn-write-bufsize conn) writebufsize
          (conn-rds conn) (%inc-ptr pb (+ $tcpPBSize tcpbufsize writebufsize))
          (conn-rds-entries conn) rdsentries)))

(defun tcp-reinitialize-connection (conn)
   (let ((write-bufsize (conn-write-bufsize conn))
           (tcp-bufsize (conn-tcp-buffer-size conn))
           (rdsentries (conn-rds-entries conn))
           (notify-proc (conn-notify-procedure conn)))
      (%reinitialize-tcp-connection conn tcp-bufsize write-bufsize rdsentries notify-proc))) 

(defun tcp-allocate-connection (rdsentries read-timeout notify-proc tcpbufsize writebufsize)
   (let ((conn (%tcp-pop-connection-resource)))
      (cond (conn  ;; because we do not release the memory there is no need to reinitialize the PB
                 (let ((pb (conn-pb conn)))
                    (cond ((or (null pb) *tcp-reinitialize-connections*)
                               (%reinitialize-tcp-connection conn tcpbufsize writebufsize rdsentries notify-proc))
                             ;; check for valid parameter blocks
                             ((not (and (macptrp pb) (not (%null-ptr-p pb))))
                               (error "Corrupted TCP Connection structure (bad parameter block): ~S" conn)))
                    (setf (conn-read-timeout conn) (min read-timeout 255))))
               ((<=  *tcp-allocated-connections* *tcp-maximum-number-of-connections*)
                 (setq conn (make-connection tcpbufsize writebufsize rdsentries read-timeout notify-proc)))
               (t #+ignore
                   (break "Insufficient TCP Connections: ~D Connections allocated limited by the maximum ~D." 
                              *tcp-allocated-connections* *tcp-maximum-number-of-connections*)
                   (error 'tcp-insufficient-resources)))
      (setf (conn-state conn) :allocated)
      conn))


;;;------------------------------------------------------------------- 
;;;
;;;  OPENING TCP STREAMS
;;;

(defclass basic-tcp-stream (input-stream output-stream)
  ((conn :initform nil :accessor %tcp-stream-conn)     ; tcp connection
   (bytes-transmitted :initform 0 :accessor %tcp-stream-bytes-transmitted)      ; value cached here after close
   (bytes-received :initform 0 :accessor %tcp-stream-bytes-received)); value cached here after close
  (:documentation "The basic TCP stream on which other classes build."))

;; This class should eventually support CR-LF translation in both directions.-- JCMa 4/18/1996.
(defclass tcp-stream
  (basic-tcp-stream)
  ()
  (:documentation "The standard ASCII TCP stream.")) 

(defclass binary-tcp-stream
  (tcp-stream io-binary-stream)
  ()
  (:documentation "The standard binary TCP stream.")) 

(defmethod initialize-instance ((s binary-tcp-stream) &key (element-type '(unsigned-byte 8) element-type-p))
  (unless (or (not element-type-p)
              (eq element-type 'unsigned-byte)          ; Shorthand ...
              (and (subtypep element-type '(unsigned-byte 8))
                   (subtypep '(unsigned-byte 8) element-type)))
    (error "element-type ~S not supported." element-type))
  (call-next-method))

(defmethod initialize-instance ((s basic-tcp-stream) &key host port 
                                (tcpbufsize *tcp-buffer-size*) (rdsentries 6) (writebufsize *tcp-write-buffer-size*)
                                notify-proc (commandtimeout *tcp-command-timeout*) (read-timeout *tcp-read-timeout*))
  (call-next-method)
  (unless (integerp port)
    (setq port (tcp-service-port-number port)))
  (when host
    (setq host (tcp-host-address host)))
  (let ((timeout (min commandtimeout 255))    ;  timeouts are a single byte
        (rd-timeout (min read-timeout 255))
        conn)
    (unwind-protect
      (progn
        (setq conn (tcp-allocate-connection rdsentries rd-timeout notify-proc tcpbufsize writebufsize))
        (setf (%tcp-stream-conn s) conn
              (conn-read-timeout conn) rd-timeout
              (conn-command-timeout conn) timeout
              (conn-stream conn) s)
        (cond (host
               (%tcp-active-open (conn-pb conn) host port timeout rd-timeout)
               (setf (conn-state conn) :open
                     (conn-remote-port conn) port))
              (t (%tcp-passive-open (conn-pb conn) port timeout)
                 ;; make sure the listening stream comes up sucessfully.
                 (%tcp-wait-for-connection-state conn '(2 4 6 8) timeout "TCP Start Listen")
                 (setf (conn-state conn) :listen
                       (conn-local-port conn) port)))
        (tcp-note-stream-open s) 
        (setq conn nil)
        s)                          ; return the stream object
      (when conn
        (tcp-deallocate-connection conn)))))

(defun open-tcp-stream (host port &key (element-type '#.*default-character-type*)
                             (tcpbufsize *tcp-buffer-size*) (rdsentries 6) (writebufsize *tcp-write-buffer-size*)
                             notify-proc (commandtimeout *tcp-command-timeout*))
  (if (subtypep element-type '#.*default-character-type*)
    (make-instance 'tcp-stream
      :host host :port port
      :tcpbufsize tcpbufsize
      :rdsentries rdsentries
      :writebufsize writebufsize 
      :notify-proc notify-proc
      :commandtimeout commandtimeout)
    (make-instance 'binary-tcp-stream
      :element-type element-type
      :host host :port port
      :tcpbufsize tcpbufsize
      :rdsentries rdsentries
      :writebufsize writebufsize 
      :notify-proc notify-proc
      :commandtimeout commandtimeout))) 

;;;------------------------------------------------------------------- 
;;;
;;;  METHODS ON TCP STREAMS
;;; 

(defmethod print-object ((s basic-tcp-stream) stream)
  (flet ((remote-host (conn)
           (let ((address (tcp-remote-host conn)))
             (when address
               (tcp-addr-to-str address)))))
    (let* ((conn (%tcp-stream-conn s))
           (state (and conn (tcp-connection-state-name conn))))
      (print-unreadable-object (s stream :type t :identity t)
        (if conn
          (format stream " ~D ~S -> ~:@(~A~)~:[~;@~:*~A~]"
                  (conn-serial-number conn) state 
                  (tcp-service-name (or (tcp-local-port conn) (tcp-remote-port conn)))
                  (remote-host conn))
          (format stream "~S" state))))
    s))

(declaim (inline tcp-stream-conn))

(defun tcp-stream-conn (stream)
  (or (%tcp-stream-conn stream) (%tcp-err-disp -23005)))     ; connection closed. 

(defmethod stream-deallocate-connection ((s basic-tcp-stream))
  (let ((conn (%tcp-stream-conn s)))
    (when conn
      ;; remember bytes transfered for logging purposes
      (setf (%tcp-stream-bytes-transmitted s) (conn-bytes-transmitted conn)
            (%tcp-stream-bytes-received s) (conn-bytes-received conn))
      ;; deallocate the connection
      (tcp-deallocate-connection conn)
      ;; make the stream forget the connection
      (setf (%tcp-stream-conn s) nil))
    s)) 

;; a big hammer to release connection memory from the TCP subtrate.
(defmethod stream-release-connection ((s basic-tcp-stream) &aux (conn (tcp-stream-conn s)))
  (tcp-with-connection-grabbed (conn *current-process* "TCP Release")
    (tcp-abort (conn-pb conn) t)
    (%tcp-release-connection conn)
    (setf (%tcp-stream-conn s) nil)
    (tcp-note-stream-closed s)) 
  s) 

(defmethod stream-read-timeout ((s basic-tcp-stream) &aux (conn (tcp-stream-conn s)))
  (conn-read-timeout conn))

(defmethod stream-set-read-timeout ((s basic-tcp-stream) timeout &aux (conn (tcp-stream-conn s)))
  (setf (conn-read-timeout conn) (min timeout 255)))     ; timeouts are one  byte

(defsetf stream-read-timeout stream-set-read-timeout) 

(declaim (inline %tcp-connection-state))

;; not parameter safe.  Use with extreme caution.
(defun %tcp-connection-state (pb &optional ignore-error-p)
  (if (eql 0 (%tcp-control-sync pb $TCPStatus ignore-error-p))
    (rref pb tcpioPB.status.connectionState)
    0))

(defun tcp-connection-state (conn)
  (let ((conn-pb (and conn (conn-pb conn))))
    (if conn-pb
      (with-pb-copy (pb conn-pb)
        (%tcp-connection-state pb t))
      0)))

(defmethod stream-connection-state ((s basic-tcp-stream))
  (tcp-connection-state (%tcp-stream-conn s))) 

(defmethod stream-connection-state-name ((s basic-tcp-stream))
  (tcp-connection-state-name (%tcp-stream-conn s))) 

(defmethod stream-eofp ((s basic-tcp-stream) &aux (conn (tcp-stream-conn s)))
  (and (null (conn-untyi-char conn))
       (eql (conn-read-count conn) 0)
       (tcp-eofp conn))) 

(defmethod stream-force-output ((s basic-tcp-stream))
  (tcp-force-output (tcp-stream-conn s) t)) 

(defmethod stream-clear-input ((s basic-tcp-stream) &aux (conn (tcp-stream-conn s)))
  (tcp-clear-input conn))

(defmethod stream-clear-output ((s basic-tcp-stream) &aux (conn (tcp-stream-conn s)))
  (tcp-clear-output conn))

(defmethod stream-abort ((s basic-tcp-stream) &aux (conn (%tcp-stream-conn s))) ;called before stream-close for abort. 
  (when conn
      (case (conn-state conn)
        ((:open :listen)
         (let ((state (tcp-connection-state conn)))
           (cond ((zerop state)) ; don't error trying to close a closed stream
                 ;; trying to avoid stepping on a connection shutting down gracefully
                 ;; verify that this actually makes sense. slh/jcma 10/2/96
                 #+no ((memq state '(14 16 18 20))
                       (%tcp-wait-for-orderly-close conn *tcp-close-timeout*)        ; wait time could be reduced
                       ;; Cannot exit the scope of the stack-let pb without ensuring that the asynchronous call is complete.
                       ;; Must ensure that the connection is closed as well.
                       (unless (zerop (tcp-connection-state conn))
                         (tcp-abort (conn-pb conn) t)))
                 (t (tcp-abort (conn-pb conn) t))) ; Ok if fails
           (tcp-clear-input conn)
           (tcp-clear-output conn)))
        ((:closed :allocated :unallocated))
        (t (error "The connection, ~S, on ~S is in an illegal state, ~S." s conn (conn-state conn))))
      (setf (conn-state conn) :closed)
      s))

(defmethod stream-local-port ((s basic-tcp-stream) &aux (conn (%tcp-stream-conn s)))
  (when conn
    (or (conn-local-port conn)
        (setf (conn-local-port conn) (tcp-local-port conn)))))

(defmethod stream-remote-port ((s basic-tcp-stream) &aux (conn (%tcp-stream-conn s)))
  (when conn
    (or (conn-remote-port conn)
        (setf (conn-remote-port conn) (tcp-remote-port conn)))))

(defmethod stream-remote-host ((s basic-tcp-stream) &aux (conn (tcp-stream-conn s)))
  (tcp-remote-host conn))

(defmethod stream-bytes-transmitted ((s basic-tcp-stream) &aux (conn (%tcp-stream-conn s)))
  (if conn
    (conn-bytes-transmitted conn)
    (%tcp-stream-bytes-transmitted s)))

(defmethod (setf stream-bytes-transmitted) ((value integer) (s basic-tcp-stream) &aux (conn (%tcp-stream-conn s)))
    (when conn
      (setf (conn-bytes-transmitted conn) value))
    (setf (%tcp-stream-bytes-transmitted s) value))

(defmethod stream-bytes-received ((s basic-tcp-stream) &aux (conn (%tcp-stream-conn s)))
  (if conn
    (conn-bytes-received conn)
    (%tcp-stream-bytes-received s)))

(defmethod (setf stream-bytes-received) ((value integer)  (s basic-tcp-stream) &aux (conn (%tcp-stream-conn s)))
    (when conn
      (setf (conn-bytes-received conn) value))
    (setf (%tcp-stream-bytes-received s) value))

;;;------------------------------------------------------------------- 
;;;
;;;  STREAM LISTEN
;;; 

;; The difficulty here is that connections that do not come up fully can lead to a closed stream state.
;; This function is intended to restart these listeners so that we continue to accept connections.
;; OT 1.1 may get confused under high rates of aborted listen start ups, and our attempts to restart
;; may timeout, depending on the value of command timeout.  We need to be very careful here so as not
;; to add to the confusion that may be occuring on the OT side.
;; Judging from the MACTCP manual 4.1-4.2,  we should have OT/Mactcp notify and automatically
;; restart the listening connection.

;; This counter keeps track of how many times listening streams have been restarted.
(defvar *reinitialized-connections* 0)

#+ignore (tcp-show-open-streams) 

(defun %tcp-reinitialize-listening-connection (conn &optional (abort-connection-p t) &aux (reinitialize-pb *tcp-reinitialize-connections*))
   (let  ((port (conn-local-port conn))
            (timeout (conn-command-timeout conn))
            (pb (conn-pb conn))) 
      (when (or abort-connection-p reinitialize-pb)
          (tcp-abort pb t))
      ;; ensure that the connection really is closed -- OT may be asynchronous -- JCMa 3/24/1996.
      (%tcp-wait-for-connection-state conn '(0) timeout "TCP Listen Stop")
      (tcp-clear-input conn)     ; clear input buffer to avoid hanging 
      (tcp-clear-output conn)    ; clear output buffer to avoid writing junk 
      ;; reset counters just in case
      (setf (conn-bytes-transmitted conn) 0
               (conn-bytes-received conn) 0)
      (when reinitialize-pb
          (tcp-reinitialize-connection conn)
          (setq pb (conn-pb conn)))
      ;; must open using the primary pb since the copies are stack allocated.
      (%tcp-passive-open pb port timeout)
      ;; don't race around this loop.
      (%tcp-wait-for-connection-state conn '(2 4 6 8) timeout "TCP Restart Listen")
      (setf (conn-state conn) :listen))          ; reset the listening state
   conn)

;; Blowing out due to a partially openned connection.
;; Unsafe call because of asynchronous call
(defmethod stream-listen ((s basic-tcp-stream))
   (flet ((ensure-listening-on-connection (pb-copy conn)
               (let ((state (rref pb-copy tcpioPB.status.connectionState)))
                  (case state
                     ((2 4 6 8))        ; connection coming up
                     ((10 12 14 16 18)        ; connection going down
                       (%tcp-wait-for-orderly-close conn 1)         ; don't wait more than 1 second here.
                       (%tcp-reinitialize-listening-connection conn t))          ; will abort if not closed already
                     (20 (%tcp-reinitialize-listening-connection conn t))    ; waiting to close
                     (0 (%tcp-reinitialize-listening-connection conn nil))       ; closed 
                     (t (error 'tcp-unknown-connection-state :error-message
                                    (format nil "Unknown connection state, ~A, while listening on ~S." (tcp-state-name state)  s))))
                  nil)))
      (declare (inline ensure-listening-on-connection))
      (let ((conn (tcp-stream-conn s)))
         (or (conn-untyi-char conn)
               (not (eql 0 (conn-read-count conn)))
               (with-pb-copy (pb (conn-pb conn))
                   (let ((err (%tcp-control-sync pb $TCPStatus T)))
                      (case err
                         (0 (or (> (rref pb tcpioPB.status.amtUnreadData) 0)
                                   (ensure-listening-on-connection pb conn)))
                         ;; connection does not exist
                         (-23008 (%tcp-reinitialize-listening-connection conn nil) nil)
                         (t (%tcp-check-error-return err))))))))) 

;;;------------------------------------------------------------------- 
;;;
;;;  STREAM CLOSE
;;; 

; Safe PB call
(defmethod stream-close ((s basic-tcp-stream) &aux (conn (%tcp-stream-conn s)))
  (when conn
    (tcp-close conn))
  (stream-deallocate-connection s)
  (tcp-note-stream-closed s)
  (call-next-method))

;;;------------------------------------------------------------------- 
;;;
;;;  STREAM READ AND WRITE OPERATIONS
;;; 

(defmethod stream-untyi ((s basic-tcp-stream) char &aux (conn (tcp-stream-conn s)))
  (tcp-with-connection-grabbed (conn *current-process* "TCP In")
    (prog1 (setf (conn-untyi-char conn) char)
      (decf (the fixnum (conn-bytes-received conn))))))

(defmethod stream-write-byte ((s basic-tcp-stream) char &aux (conn (tcp-stream-conn s)))
  (tcp-with-connection-grabbed (conn *current-process* "TCP Out")
    (tcp-write-byte conn (char-code char)))) 

(defmethod stream-tyo ((s basic-tcp-stream) char &aux (conn (tcp-stream-conn s)))
   (flet ((write-the-byte (conn byte)
                (let ((count (conn-write-count conn)))
                   (declare (fixnum count))
                   ;; maybe send the buffer
                   (when (eql (conn-write-bufsize conn) count)
                       (%tcp-force-output conn nil)
                       (setq count (conn-write-count conn)))
                   ;; ship the byte
                   (setf (conn-write-count conn) (1+ count))
                   (%put-byte (conn-write-buffer conn) byte count) 
                   (incf (conn-bytes-transmitted conn)))))
      (declare (inline write-the-byte))
      (tcp-with-connection-grabbed  (conn *current-process* "TCP Out")
         (let ((char-code (char-code char)))
            (declare (fixnum count char-code))
            (if (< char-code 256) 
               ;; Write single byte character
               (write-the-byte conn char-code)
               ;; Write 2-byte characters as in ISO-2022-JP -- JCMa 2/22/1997.
               (multiple-value-bind (high-byte low-byte) (floor char-code 256)
                   (write-the-byte conn high-byte)
                   (write-the-byte conn low-byte)))))))

;;  Avoid error when the connection times out -- JCMa 9/1/1996.
(defmethod stream-peek :around ((s basic-tcp-stream))
  (with-end-of-file-handled (s)
    (call-next-method s)))

(defmethod stream-tyi ((s basic-tcp-stream) &aux (conn (tcp-stream-conn s)))
  (tcp-with-connection-grabbed (conn *current-process* "TCP In")
    (tcp-read-char conn)))

(defmethod stream-write-string ((s basic-tcp-stream) string start end &aux (conn (tcp-stream-conn s)))
   (macrolet ((maybe-send-tcp-buffer (count bufsize conn)
                        `(when (eql ,bufsize ,count)
                             (%tcp-force-output ,conn t)
                             (setq ,count (conn-write-count ,conn))))
	      (put-byte (byte writebuf bufpos bufsize conn) 
		`(progn (maybe-send-tcp-buffer ,bufpos ,bufsize ,conn)
			(incf (conn-write-count ,conn))
			(%put-byte ,writebuf ,byte ,bufpos)
			(incf ,bufpos))))
       (multiple-value-bind (str offset)
                                       (array-data-and-offset string)
           (declare (fixnum start end offset bytes)
                         (type str string)
                         (optimize (speed 3) (safety 0)))
           (setq start (require-type (+ start offset) 'fixnum))
           (setq end (require-type (+ end offset) 'fixnum))
           (let ((bytes (- end start))
                   (writebuf (conn-write-buffer conn))
                   (bufsize (conn-write-bufsize conn)))
              (tcp-with-connection-grabbed (conn *current-process* "TCP Out")
                 (loop with bufpos = (conn-write-count conn)
		       for idx upfrom start below end
		       for char-code fixnum = (char-code (aref str idx))
		       do (if (< char-code 256)
			      ;;Write single byte character
			      (put-byte char-code writebuf bufpos bufsize conn)
			      ;;Write 2-byte characters as in ISO-2022-JP -- JCMa 2/22/1997.
			      (multiple-value-bind (high-byte low-byte) (floor char-code 256)
				(put-byte high-byte writebuf bufpos bufsize conn)
				(put-byte low-byte writebuf bufpos bufsize conn)
				(incf (the fixnum (conn-bytes-transmitted conn)))))
		       finally (incf (the fixnum (conn-bytes-transmitted conn)) bytes))))
           string))) 

(defmethod stream-read-line ((s basic-tcp-stream) &aux (conn (tcp-stream-conn s)))
  (unless (tcp-eofp conn)
    (loop with line = (make-array 100 :element-type '#.*default-character-type*
                                  :adjustable t :fill-pointer 0)
          for char = (stream-tyi s)
          until (or (null char) (eql char #\CR))
          do (vector-push-extend char line)
          finally (return (values line))))) 

(defmethod stream-write-line ((s basic-tcp-stream) string &optional (start 0) end)
  (stream-write-string string s start (or end (length string)))
  (stream-tyo s #\CR)
  (stream-force-output s)) 

;;;------------------------------------------------------------------- 
;;;
;;;  BINARY STREAM OPERATIONS
;;; 

(defmethod stream-write-byte ((s binary-tcp-stream) byte &aux (conn (tcp-stream-conn s)))
  (tcp-with-connection-grabbed (conn *current-process* "TCP Out")
    (tcp-write-byte conn (logand #xff byte))))

(defmethod stream-read-byte ((s binary-tcp-stream) &aux (conn (tcp-stream-conn s)))
  (tcp-with-connection-grabbed  (conn *current-process* "TCP In")
    (tcp-read-byte conn)))

(defmethod stream-write-vector ((s binary-tcp-stream) v start end &aux (conn (tcp-stream-conn s)))
  (tcp-with-connection-grabbed  (conn *current-process* "TCP Out")
    (tcp-write-vector conn v start end)))

(defmethod stream-read-vector ((s binary-tcp-stream) v start end &aux (conn (tcp-stream-conn s)))
  (tcp-with-connection-grabbed (conn *current-process* "TCP In")
    (tcp-read-vector conn v start end)))

;;;------------------------------------------------------------------- 
;;;
;;;  BULK COPYING OVER STREAMS
;;;

(defmethod stream-copy-until-eof (from-stream to-stream)
  (loop for line = (read-line from-stream nil)
        while line
        do (write-line line to-stream))) 

;; Blasts the bits without CR-LF translation
(defmethod stream-copy-until-eof ((from-stream input-file-stream) (to-stream basic-tcp-stream)
                                  &aux (conn (tcp-stream-conn to-stream)))
  (tcp-with-connection-grabbed (conn *current-process* "TCP Out")
    (tcp-write-file conn (stream-filename from-stream))))

(defgeneric stream-copy-bytes (from-stream to-stream  n-bytes &optional start)
  (:documentation "Copies N-BYTES starting at START from FROM-STREAM to TO-STREAM."))

(defmethod stream-copy-bytes (from-stream to-stream  n-bytes &optional (start 0))
  (stream-position from-stream start)
  (loop for bytes upfrom 0
        while (< bytes n-bytes)
        do (write-byte (read-byte from-stream) to-stream)))

;; Blasts the bits without CR-LF translation
(defmethod stream-copy-bytes ((from-stream input-file-stream) (to-stream basic-tcp-stream)
                              bytes &optional (start 0) &aux (conn (tcp-stream-conn to-stream)))
  (tcp-with-connection-grabbed (conn *current-process* "TCP Out")
    (tcp-write-file-bytes conn (stream-filename from-stream) bytes start)))

(defmethod stream-copy-bytes ((from-stream basic-tcp-stream) (to-stream output-file-stream)
                              bytes &optional (start 0) &aux (conn (tcp-stream-conn from-stream)))
  (tcp-with-connection-grabbed (conn *current-process* "TCP In")
    (tcp-read-bytes-to-file conn (stream-filename to-stream) bytes start)))

;;;------------------------------------------------------------------- 
;;;
;;; 
;;;
;;Useful little functions: read & write CRLF-terminated lines from a "clear text" 
;; connection.
(defun telnet-read-line (stream)
  "Read a CRLF-terminated line"
  (unless (stream-eofp stream)
    (let ((line (Make-Array 10 :Element-Type '#.*default-character-type*
                            :Adjustable T :Fill-Pointer 0))
          (char nil))
      (do () ((or (null (setq char (stream-tyi stream)))
                  (and (eq char #\CR) (eq (stream-peek stream) #\LF)))
              (when char (stream-tyi stream))
              (values line (null char)))
        (vector-push-extend char line)))))

(defun telnet-write-line (stream string &rest args)
  "Write a CRLF-terminated line"
  (declare (dynamic-extent args))
  (apply #'format stream string args)
  (write-char #\CR stream)
  (write-char #\LF stream)
  (force-output stream))

;;;------------------------------------------------------------------- 

(provide :mactcp) 
