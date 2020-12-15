;;; -*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-
;;;
;;; (c) Copyright  1994, John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; HOST NAMESPACE
;;;
(in-package :http)

;;;------------------------------------------------------------------- 
;;;
;;; basic stuff
;;;

(defmethod print-object ((host host) stream)
  (with-slots (domain-name ip-address) host
    (print-unreadable-object (host stream :type t :identity t)
      (write-string (or domain-name ip-address "No name") stream))))

(defmethod print-object ((host-name-space host-name-space) stream)
  (with-slots (local-host) host-name-space
    (print-unreadable-object (host-name-space stream :type t :identity t)
      (write-string (or (host-domain-name local-host)
                        (host-ip-address local-host)
                        "No Name") stream))))

(defmethod host-name ((host host))
  (with-slots (domain-name ip-address) host
    (or domain-name ip-address)))

;;;------------------------------------------------------------------- 
;;;
;;; Host domain name
;;;

(define-generic host-domain-name (host &optional update-object-p)
  (:documentation "Returns the domain name for HOST.
HOST can be a parsed IP addres (integer), an IP address (string),  or
a host object. UPDATE-OBJECT-P is relevant only for the host object
case and causes the DNS name to be recached."))

; these hosts are created for each connection.  Why not cach the host
;; object so that repeated contact recycle the same host?  6/10/94 --
;; Mail-Server.
#-Genera
(defmethod host-domain-name ((host integer) &optional update-object-p)
  (declare (ignore update-object-p))
  (www-utils:domain-name-for-parsed-ip-address host))

(defmethod host-domain-name ((host string) &optional update-object-p)
  (declare (ignore update-object-p))
  (www-utils:domain-name-for-ip-address host))

(defmethod host-domain-name ((host host) &optional update-object-p)
  (declare (ignore update-object-p))
  (slot-value  host 'domain-name))

(defmethod host-short-name ((host t) &optional update-object-p)
  (let* ((dname (host-domain-name host update-object-p))
         (end (char-position #\. dname) ))
    (if end 
        (subseq dname 0 end)
        dname)))

#-Genera
(defmethod host-ip-address ((host integer))
  (www-utils:ip-address-for-parsed-ip-address host))

(defmethod host-ip-address ((host string) )
  (www-utils:ip-address-for-host-domain-name host))

#+Genera
(defmethod host-ip-address ((host neti:host))
  (www-utils::host-internet-address host))

(define-generic host-log-name (host)
  (:documentation "Returns the log name for HOST for use in the HTTP log file."))

#-Genera
(defmethod host-log-name ((host integer))
  (if *log-resolve-ip-addresses*
      (www-utils:domain-name-for-parsed-ip-address host)
      (www-utils:ip-address-for-parsed-ip-address host)))

(defmethod host-log-name ((host string))
  (if *log-resolve-ip-addresses*
      (www-utils:domain-name-for-ip-address host)
      (www-utils:ip-address-for-host-domain-name host)))

(defmethod host-log-name ((host host))
  (or (slot-value  host 'domain-name)
      (slot-value  host 'ip-address)))

;;;------------------------------------------------------------------- 
;;;
;;; REGISTERING HOSTS 
;;;

(define-generic register-host (host host-name-space)
  (:documentation "Registers HOST in HOST-NAME-SPACE."))

(defmethod register-host ((host host) (host-name-space host-name-space))
  (with-slots (domain-name ip-address) host
    (with-slots (host-table) host-name-space
      (cond-every
        (domain-name (setf (gethash domain-name host-table) host))
        (ip-address (setf (gethash ip-address host-table) host)))))
  host)

#+Genera
(defmethod register-host ((host neti:host) (host-name-space host-name-space))
  (register-host (allocate-host host-name-space host nil nil) host-name-space))

(define-generic allocate-host (host-name-space object ip-address domain-name &key class)
  (:documentation "Low level primitive that allocates a host object in HOST-NAME-SPACE.¬
according to the arguments. When the native DNS representation uses non-portable host
objects, these are passed in through the OBJECT argument. The CLASS argument
allows specalizations of the class of host allocated."))

(defmethod allocate-host ((host-name-space host-name-space) (object t) ip-address domain-name &key (class 'host))
  (register-host (make-instance class
                                :ip-address ip-address
                                :domain-name domain-name
                                :object object)
                 host-name-space))

#+Genera
(defmethod allocate-host ((host-name-space host-name-space) (host neti:host) ip-address domain-name &key (class 'host))
  ip-address  domain-name class                 ;ignore
  (make-instance class
                                                ;                :ip-address (host-ip-address host)
                 :domain-name (www-utils:host-mail-name host)
                 :object host))

(define-generic intern-host (host-name-space &key ip-address domain-name object)
  (:documentation "Pimary method to intern a host in HOST-NAME-SPACE¬with
IP-ADDRESS and/or DOMAIN-NAME.   When the native DNS representation
uses non-portable host objects, these are passed in through the OBJECTargument."))

(defmethod intern-host ((host-name-space host-name-space) &key ip-address domain-name object &aux newly-created-p)
  (declare (values interned-host newly-created-p))
  (with-slots (host-table) host-name-space
    (values 
      (cond ((and object (typep object 'host)) object)
            ((and ip-address (gethash ip-address host-table)))
            ((and domain-name (gethash domain-name host-table)))
            (domain-name
             (setq newly-created-p t)
             (allocate-host host-name-space nil nil domain-name))
            ((null object)
             (error "Attempt to allocate an empty host."))
            ((not (stringp object))
             (setq newly-created-p t)
             (allocate-host host-name-space object nil nil))
            ;; Use the more sophisticated tests in COMLINK
            ((every #'(lambda (x) (or (digit-char-p x) (char-equal x #\.))) object)
             (setq newly-created-p t)
             (allocate-host host-name-space nil object nil))
            (t (setq newly-created-p t)
               (allocate-host host-name-space nil nil object)))
      newly-created-p)))

(defmethod initialize-local-host ((host-name-space host-name-space))
  (with-slots (local-host host-table) host-name-space
    (when local-host
      (setq local-host (register-host (intern-host host-name-space :object local-host)
                                      host-name-space))))
  host-name-space)

(defmethod initialize-host-name-space ((host-name-space host-name-space) &optional (size 100.))
  (with-slots (host-table) host-name-space
    (cond (host-table (clrhash host-table ))
          (t (setq host-table (make-hash-table :test #'equalp :size (floor size 10.)))))
    (initialize-local-host host-name-space))
  host-name-space)

(defmethod initialize ((host-name-space host-name-space))
  (initialize-host-name-space host-name-space)
  host-name-space)
