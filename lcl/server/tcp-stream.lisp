;;; LispWorks interface to TCP/IP streams
;;;
;;; Copyright (c) The Harlequin Group Limited, 1995.
;;;


;; NB: Within Lisp, all addresses and port numbers are in host order.

(in-package "IPC")

(lcl::def-foreign-synonym-type :int            :signed-32bit)
(lcl::def-foreign-synonym-type :short          :signed-16bit)
(lcl::def-foreign-synonym-type :byte           :signed-8bit)
(lcl::def-foreign-synonym-type :unsigned-int   :unsigned-32bit)
(lcl::def-foreign-synonym-type :unsigned-short :unsigned-16bit)
(lcl::def-foreign-synonym-type :unsigned-byte  :unsigned-8bit)

(lcl::def-foreign-synonym-type :uinteger       :unsigned-32bit)

(defvar *c-int*
    (lcl::make-foreign-pointer :type '(:pointer :int)
                               :static t))
(defvar *c-errno*
    (lcl::foreign-variable-pointer "errno"))

#|
              char   *h_name;       /* official name of host */
              char   **h_aliases;   /* alias list */
              int    h_addrtype;    /* address type */
              int    h_length;      /* length of address */
              char   **h_addr_list; /* list of addresses from name server */
|#
(lcl:def-foreign-struct hostent
    (name   :type (:pointer :character))
  (aliases  :type (:pointer (:pointer :character)))
  (addrtype :type :int)
  (length   :type :int)
  (addrlist :type (:pointer (:pointer :character))))

#|      short   sin_family;
        u_short sin_port;
        struct  in_addr sin_addr;
        char    sin_zero[8];
|#
(lcl:def-foreign-struct sockaddr-in
    (family :type :short)
  (port     :type :unsigned-short)
  (in-addr  :type :unsigned-int)
  (zero     :type (:array :unsigned-byte (8))))

(lcl:def-foreign-function (socket (:return-type :int))
    ""
  (domain   :int)
  (type     :int)
  (protocol :int))

(lcl:def-foreign-function (unix-close (:name close)
                                      (:return-type :int))
    ""
  (fildes :int))

(lcl:def-foreign-function (setsockopt (:return-type :int))
    ""
  (s        :int)
  (level    :int)
  (optname  :int)
  (optval   :pointer)
  (optlen   :int))

(lcl:def-foreign-function (bind (:return-type :int))
    ""
  (s        :int)
  (name     :pointer)
  (namelen  :int))

(lcl:def-foreign-function (c-listen (:name listen)
                                    (:return-type :int))
    ""
  (s       :int)
  (backlog :int))

(lcl:def-foreign-function (accept (:return-type :int))
    ""
  (s       :int)
  (addr    :pointer)
  (addrlen :pointer))

(lcl:def-foreign-function (fcntl (:return-type :int))
    ""
  (fildes :int)
  (cmd    :int)
  (arg    :arbitrary))

(lcl:def-foreign-function (gethostbyname (:return-type (:pointer hostent)))
    ""
  (name :simple-string))

(lcl:def-foreign-function (gethostbyaddr (:return-type (:pointer hostent)))
    ""
  (addr :array) 
  (len  :fixnum) 
  (type :fixnum))
                           
#+sparc(defun ntohs (x) x)
#+sparc(defun ntohl (x) x)
#-sparc(lcl:def-foreign-function (ntohs (:return-type :fixnum))   (n :fixnum))
#-sparc(lcl:def-foreign-function (ntohl (:return-type :uinteger)) (n :uinteger))

(lcl:def-foreign-function (getpeername-in (:name getpeername)
                                          (:return-type :fixnum))
    ""
  (fd      :fixnum)
  (name    :pointer)
  (namelen :pointer))

(lcl:def-foreign-function (c-getdomainname (:name getdomainname)
                                           (:return-type :fixnum))
    ""
  (name    :simple-string)
  (namelen :fixnum))

(lcl:def-foreign-function (get-unix-error (:name strerror)
                                          (:return-type :simple-string))
    ""
  (errnum :int))

#+(and solaris lcl4.2)
(lcl:load-foreign-libraries () '("-lsocket" "-lnsl"))

(define-condition network-error (error)
  ())

(define-condition unknown-host-name (network-error)
  ((hostname :initarg :hostname))
  (:report (lambda (condition stream)
             (format stream "Unknown host name ~A"
                     (slot-value condition 'hostname)))))

(define-condition unknown-address (network-error)
  ((address :initarg :address))
  (:report (lambda (condition stream)
             (let ((address (slot-value condition 'address)))
               (format stream "Unknown address ~A"
                       (ip-address-string address))))))

(define-condition domain-resolver-error (network-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Cannot find current domainname"))))

(define-condition unix-socket-error (network-error)
  ((function :initarg :function)
   (errno :initarg :errno))
  (:report (lambda (condition stream)
             (let ((errno (unix-socket-error-errno condition)))
               (format stream "UNIX error '~A' (errno ~D) in ~A"
                       (get-unix-error errno) errno
                       (unix-socket-error-function condition))))))

(define-condition remote-network-error (network-error)
  ())


(defvar *simple-string-buffer*
  (lcl:with-static-area (make-string 40 :element-type 'base-character)))

(defvar *address-array*
  (lcl:with-static-area (make-array 32 :element-type '(unsigned-byte 8))))

;;; Turn internet address into string format
(defun ip-address-string (address)
  (format nil "~D.~D.~D.~D"
          (ldb (byte 8 24) address)
          (ldb (byte 8 16) address)
          (ldb (byte 8 8)  address)
          (ldb (byte 8 0)  address)))

(defun internet-address (name)
  (or (internet-address-domain name)
      ;; Fall-back
      (let ((end (position #\. name)))
        (and (and end (> end 0))
             ;; Use short name, host may be local and its name server down
             (internet-address-domain (subseq name 0 end))))
      (error 'unknown-host-name :hostname name)))

;;; Like gethostbyname but returns an internet address (long network format)
(defun internet-address-domain (name)
  (with-scheduling-inhibited
      (let ((simple-name
           (if (simple-string-p name)
               name
             (progn
               (replace *simple-string-buffer* name)
               (setf (schar *simple-string-buffer* (length name)) (code-char 0))
               *simple-string-buffer*))))
      (let ((hostent (gethostbyname simple-name)))
        (and (not (zerop (lcl::foreign-pointer-address hostent)))
             (let ((bytes (subseq (concatenate 'string
                                    (lcl::foreign-string-value
                                     (lcl::foreign-value 
                                      (hostent-addrlist hostent)))
                                    (make-array 4 :initial-element #\Null))
                                  0 4)))
               (dpb (char-code (elt bytes 0)) (byte 8 24)
                    (dpb (char-code (elt bytes 1)) (byte 8 16)
                         (dpb (char-code (elt bytes 2)) (byte 8 8)
                              (char-code (elt bytes 3)))))))))))

(defvar *domainname* nil)

;;; There are more than one strategies you can use
;;; based on what OS you are using. Instead of providing
;;; each one we try one.
;;;
(defun getdomainname (&optional (where "/etc/defaultdomain"))
  (or *domainname*
      (with-static-area
          (let ((buffer (make-string 64 :element-type 'base-character)))
            (and (zerop (c-getdomainname buffer (length buffer)))
                 (setq *domainname*
                   (subseq buffer 0 (position #\Null buffer))))))
      (if (probe-file where)
          (with-open-file (stream where :direction :input)
            (setq *domainname* (read-line stream))))
      (error 'domain-resolver-error)))


(defparameter *af-inet* 2)

(defun get-host-name-by-address-array (array)
  (let ((hostent (gethostbyaddr array 4 *af-inet*)))
    (if (zerop (lcl::foreign-pointer-address hostent))
        nil
      (lcl::foreign-string-value (hostent-name hostent)))))

(defun get-host-name-by-address (address)
  (or (with-scheduling-inhibited
       (let ((array *address-array*))
         (declare (type (simple-array (unsigned-byte 8) (32)) array))
         (setf (aref array 0) (ldb (byte 8 24) address)
               (aref array 1) (ldb (byte 8 16) address)
               (aref array 2) (ldb (byte 8 8)  address)
               (aref array 3) (ldb (byte 8 0)  address))
         (get-host-name-by-address-array array)))
      (error 'unknown-address :address address)))

(defvar *sockaddr-in* nil)

(defun get-peer-host-and-port (fd)
  (with-scheduling-inhibited
   (setf (lcl::foreign-value *c-int*) (lcl::foreign-type-size 'sockaddr-in))
   (let* ((sockaddr-in (or *sockaddr-in*
                           (setq *sockaddr-in* (make-sockaddr-in))))
          (res (getpeername-in fd sockaddr-in *c-int*)))
     (if (zerop res)
         (values (ntohl (sockaddr-in-in-addr sockaddr-in))
                 (ntohs (sockaddr-in-port sockaddr-in)))
       (error 'unix-socket-error
              :function 'getpeername
              :errno (lcl::foreign-value *c-errno*))))))

(defvar *details-hash*
    (make-hash-table))

(defun make-tcp-stream (fd port)
  (multiple-value-bind (remote-address remote-port)
      (get-peer-host-and-port fd)
    (setf (gethash fd *details-hash*) 
      (list :local-port  port
            :remote-host remote-address
            :remote-port remote-port))
    (lcl::make-lisp-stream :input-handle fd :output-handle fd :auto-force t)))

(defun %listen-for-connections (port function)
  (loop
    (listen-and-attach-stream `(lispworks-listen-for-connection
                                ,port
                                ,function)
                              port      ;service
                              nil       ;announce
                              )))

(defun lispworks-listen-for-connection (fd port function)
  (funcall function (make-tcp-stream fd port) port))

(defun find-detail-from-stream (stream detail)
  (getf (gethash (lcl::extract-stream-handle stream :input) *details-hash*) detail))

(defun listen-and-attach-stream (sexp port ignore)
  (declare (ignore ignore))
  (apply (car sexp)
         (lcl::process-wait (format () "listening on port %d" port)
                            #'accept-connection
                            port)
         (cdr sexp)))

(defconstant AF_INET      2)
(defconstant INADDR_ANY   #X00000000)
(defconstant SOCK_STREAM  2)
(defconstant SOL_SOCKET   #Xffff)
(defconstant SO_REUSEADDR #X0004)
(defconstant SOMAXCONN    5)
(defconstant F_GETFL      3)
(defconstant F_SETFL      4)
(defconstant O_NDELAY     #X04)


(defun accept-connection (port)
  (unless (lcl::symbol-process-boundp '*socket*
                                      lcl::*current-process*)
    (let ((s (socket AF_INET SOCK_STREAM 0)))
      (if (= s -1)
          (error 'unix-socket-error
                 :function 'socket
                 :errno (lcl::foreign-value *c-errno*)))
      (handler-bind ((unix-socket-error 
                      #'(lambda (c)
                          (unix-close s)
                          (signal c))))
        (setf (lcl::foreign-value *c-int*) 1)
        (if (= (setsockopt s 
                           SOL_SOCKET 
                           SO_REUSEADDR 
                           *c-int* 
                           (lcl::foreign-type-size :int))
               -1)
            (error 'unix-socket-error
                   :function 'setsocketopt
                   :errno (lcl::foreign-value *c-errno*)))
        (with-scheduling-inhibited
          (unless *sockaddr-in*    
            (setq *sockaddr-in* (make-sockaddr-in)))
          (setf (sockaddr-in-family  *sockaddr-in*) AF_INET
                (sockaddr-in-port    *sockaddr-in*) port
                (sockaddr-in-in-addr *sockaddr-in*) INADDR_ANY)
          (if (= (bind s 
                       *sockaddr-in* 
                       (lcl::foreign-type-size 'sockaddr-in)) 
                 -1)
              (error 'unix-socket-error
                     :function 'bind
                     :errno (lcl::foreign-value *c-errno*))))
        (if (= (c-listen s SOMAXCONN) -1)
            (error 'unix-socket-error
                   :function 'c-listen
                   :errno (lcl::foreign-value *c-errno*)))
        (if (= (fcntl s F_GETFL *c-int*) -1)
            (error 'unix-socket-error
                   :function 'fcntl
                   :errno (lcl::foreign-value *c-errno*)))      
        (if (= (fcntl s F_SETFL (logior (lcl::foreign-value *c-int*) O_NDELAY)) -1)
            (error 'unix-socket-error
                   :function 'fcntl
                   :errno (lcl::foreign-value *c-errno*))))
      (setf (lcl::symbol-process-value '*socket*
                                       lcl::*current-process*)
        s)))
  (with-scheduling-inhibited
      (setf (lcl::foreign-value *c-int*)
        (lcl::foreign-type-size 'sockaddr-in))
    (let ((fd (accept (lcl::symbol-process-value '*socket*
                                                 lcl::*current-process*)
                      *sockaddr-in*
                      *c-int*)))
      (if (= fd -1)
          ()
        fd))))
