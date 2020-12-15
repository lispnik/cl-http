;;;-*- Syntax: ansi-common-lisp; Base: 10; Mode: lisp; Package: ccl -*-

;;; (C) Copyright 1995, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CONDITIONS FOR MACTCP John C. Mallery (JCMa@AI.MIT.EDU)
;;;

;;; This hierarchy of network conditions is modelled on the lisp machine's
;;; condition system.
;;;
;;; It was written for MCL 2.0.1 based on the code in ccl:library;MACTCP.lisp
;;; 1/18/95 -- JCMa.
;;;
;;; 1. Are there more TCP conditions that we should be handling?
;;;
;;; 2. Do the hierarchies make sense for MACTCP ?(network documentation was
;;; unavailable at the moment this was written)
;;;
;;; 3. What was in this the previous signalling system that could lead to
;;; untyped errors?
;;;
;;; 4. Some conditions should accept more state information (e.g, host) and
;;; this should be passed in by the calling code, meaning that MACTCP should
;;; be upgraded to signal TCP errors by including calls directly in the code
;;; wherever paramaters would be needed by the condition signalled.

;;; 5. Upgrades MCL 2.0.1 to the MCL 3.0 network condition suite. -- JCMa 6/11/1995. 

(in-package :ccl) 

;;;------------------------------------------------------------------- 
;;;
;;; PATCH TO STANDARD STREAM CODE 
;;;

(proclaim '(inline note-stream-closed))

;; atomically note stream closure.
(defun note-stream-closed (stream)
   (without-interrupts
     (setq *open-tcp-streams* (delete stream ccl::*open-tcp-streams* :test #'eq))))

;; MCL 2.0.1 from ccl:library;MACTCP.lisp
;Kind of bogus, but most of the protocols don't depend on a reliable close anyhow...
(defmethod stream-close ((s modal-ascii-or-binary-tcp-stream)
                                        &aux (conn (slot-value s 'conn)))
   (when conn
       (stream-clear-input s)
       ;; orginal code calls the function on stream rather than conn.
       ;; can get errors trying to output to on a closed stream.-- JCMa 2/27/1995.
       (unless (member (tcp-connection-state s) '(0 2 20.) :test #'=)
          (tcp-stream-force-output conn t))
       (let ((pb (conn-pb conn)))
          (setf (rref pb tcpioPB.close.validityFlags) 0)
          (%tcp-control pb $TCPClose T)             ; Ok if fails (bogus)
          ;;;;we should get the following sequence -- Karsten  2/16/1995.
          ;;; Can hang indefinitely so wait for :CLOSING-TIME-ACK -- Reti 2/17/1995.
          ;;  No -57 error with Netscape. MacWeb and Mosaic work fine.
          ;;;:FIN-WAIT-1 (10)
          ;;;:FIN-WAIT-2 (12)
          ;;;:CLOSING-TIME-ACK (20)
          ;;;:CLOSED (0)
          (loop for state = (tcp-connection-state s)
                   until (or (eql  state 0)
                                 (eql state 20.))
                   finally (%tcp-release pb))
          (#_DisposPtr pb)
          (setf (slot-value s 'conn) nil)))
   ;; moved outside the when-- JCMa 5/6/1995.
   (note-stream-closed s)
   (call-next-method))


;;;------------------------------------------------------------------- 
;;;
;;; 
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
      (-23037 . tcp-insufficient-memory-for-packet-size)(-23041 . tcp-syntax-error)
      (-23041 . tcp-syntax-error)
      (-23042 . tcp-domain-cache-fault)
      ;;#define noResultProc  -23043
      (-23044 . tcp-domain-server-not-found)
      (-23045 . tcp-unknown-domain-name)
      (-23046 . tcp-no-response-from-domain-server)
      (-23047 . domain-resolver-error)
      (-23048 . tcp-out-of-memory)
      ))

#|(defun %tcp-err-disp (errno)
  (let ((err (assq (setq errno (%word-to-int errno)) *tcp-error-strings*))
        (error-fn #'error)); want to tai             l-call...
    (declare (type list err))
    (if err (funcall error-fn (cdr err)) (%err-disp errno))))|#

;; patch the primary signalling function for MACTCP
(compiler-let ((*warn-if-redfine* nil))
   (defun %tcp-err-disp (error-number)
      (let* ((error-code (%word-to-int error-number))
                (entry (assoc error-code *tcp-error-class-alist* :test #'eq))
                (error-fn #'error))      ; want to tail-call...
         (declare (type list entry))
         (if entry
            ;; call our own TCP error.
            (funcall error-fn (cdr entry))
            ;;formerly called standard error signalling code in MCL (%err-disp error-number)
            ;;now signals unknown error class -- JCMa 6/9/1995.
            (funcall error-fn 'unknown-mactcp-error :error-code error-code)))))

;; patch buggy code in MACTCP -- JCMa 4/18/1995.
(defun tcp-stream-conn (s)
   (or (slot-value s 'conn) (%tcp-err-disp -23005)))     ; connection closed.

;; Blowing out due to a partially openned connection.
;; Added conditional handling, but it would be better if the MACTCP code
;; respected the ignore errors switch. -- JCMa 4/18/1995.
(defmethod stream-listen ((s tcp-stream))
   (handler-case
      (let ((conn (tcp-stream-conn s)))
         (or (conn-untyi-char conn)
               (not (eql (conn-read-count conn) 0))
               (let ((pb (conn-pb conn)))
                  (and (eql (%tcp-control pb $TCPStatus T) 0)
                          (> (rref pb tcpioPB.status.amtUnreadData) 0)))))
      (bad-connection-state () nil)))

;;;------------------------------------------------------------------- 
;;;
;;; LISP MACHINE NETWORK CONDITION HIERARCHY
;;;
;;;
;;;sys:network-error
;;;  neti:domain-packet-format-error
;;;  neti:domain-resolver-error
;;;  neti:domain-site-resolver-error
;;;  neti:gateway-connection-error
;;;  neti:host-believed-unavailable
;;;  net:host-does-not-support-service
;;;  tcp::internet-error
;;;    tcp::udp-error
;;;      tcp::udp-no-response
;;;        tcp::udp-destination-unreachable
;;;  sys:local-network-error
;;;    net:interface-not-working
;;;    sys:network-resources-exhausted
;;;    sys:unknown-address
;;;    sys:unknown-host-name
;;;      neti:unknown-network-or-namespace-in-host-name
;;;  sys:network-stream-closed
;;;    rpc:tcp-rpc-stream-closed
;;;    tcp::tcp-stream-closed
;;;  sys:remote-network-error
;;;    sys:bad-connection-state
;;;      tcp::bad-tcp-connection-state
;;;        tcp::bad-tcp-state-in-connect
;;;      sys:connection-closed
;;;        tcp::tcp-connection-closed
;;;      sys:connection-closed-locally
;;;        tcp::tcp-connection-closed-locally
;;;      sys:connection-lost
;;;        tcp::tcp-connection-reset
;;;      sys:connection-no-more-data
;;;        tcp::tcp-connection-no-more-data
;;;      sys:host-stopped-responding
;;;        tcp::tcp-host-stopped-responding
;;;          tcp::tcp-destination-became-unreachable
;;;    sys:connection-error
;;;      sys:connection-refused
;;;        tcp::gateway-tcp-host-connection-refused
;;;        tcp::tcp-connection-refused
;;;      tcp::gateway-tcp-connection-error  [due to :required-flavors]
;;;        tcp::gateway-tcp-host-not-responding-during-connection
;;;      sys:host-not-responding-during-connection
;;;        tcp::tcp-host-not-responding-during-connection
;;;          tcp::tcp-destination-unreachable-during-connection
;;;        rpc:udp-rpc-host-not-responding
;;;      rpc:port-unavailable
;;;      tcp::tcp-connection-error  [due to :required-flavors]
;;;      rpc:udp-rpc-connection-error
;;;    sys:host-not-responding
;;;    neti:protocol-timeout
;;;    tcp::remote-tcp-error  [due to :required-flavors]
;;;  tcp::tcp-error  [due to :required-flavors]

;;;
;;; network-error               sys:network-error
;;; domain-resolver-error       neti:domain-resolver-error
;;; unknown-host-name           sys:unknown-host-name
;;; local-network-error         sys:local-network-error
;;; unknown-address             sys:unknown-address
;;; network-resources-exhausted sys:network-resources-exhausted
;;; remote-network-error        sys:remote-network-error
;;; host-not-responding         sys:host-not-responding
;;; bad-connection-state        sys:bad-connection-state
;;; connection-closed           sys:connection-closed
;;; connection-lost             sys:connection-lost
;;; host-stopped-responding     sys:host-stopped-responding
;;; connection-error            sys:connection-error
;;; connection-refused          sys:connection-refused
;;; protocol-timeout            neti:protocol-timeout
;;; network-parse-error         sys:parse-error
;;;


;;;------------------------------------------------------------------- 
;;;
;;; TCP CONDITION CLASSES
;;;

;; These error classes are intended to support multiple network protocols.
;; All TCP specific errors are prefixed with TCP-.  All AppleTalk errors are
;; prefixed with appletalk-. Generic error classes have no prefix

(define-condition mactcp-error (error))

(define-condition mactcp-error-code-mixin
                           (mactcp-error)
                           ((error-code :initarg :error-code :reader tcp-error-code
                                                :allocation :class)
                             (error-message :initarg :error-message :reader tcp-error-message :allocation :class))
   (:report report-mactcp-error))

(defmethod report-mactcp-error ((error mactcp-error-code-mixin) stream)
   (with-slots (error-message) error
       (write-string "MACTCP: " stream)
       (write-string error-message stream)
       (write-char #\.  stream)))

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

(define-condition tcp-domain-server-not-found
                           (mactcp-error-code-mixin domain-error)
                           ((error-code :initform -23044 :allocation :class)
                             (error-message :initform "No name server can be found for the specified domain name"
                                                      :allocation :class)))

(define-condition tcp-unknown-domain-name
                           (mactcp-error-code-mixin domain-error)
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
                             (error-message :initform "Bad network configuration" :allocation :class)))

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

(export
  '(bad-connection-state
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
