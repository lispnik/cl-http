;;;; Patches for CMUCL incompatibilities and bugs that haven't been
;;;; integrated into the main source yet.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Add simple support for equalp hash tables.
;;; Initially by Paul Werkowski

(in-package "USER")

(defun slow-equalp-hash (key)
  (declare (optimize (speed 3)))
  (typecase key
    (fixnum (sxhash key))
    (integer (sxhash key))
    (string (sxhash (string-upcase key)))
    (symbol (sxhash key))
    (list (reduce #'logxor (mapcar #'slow-equalp-hash key)))
    (single-float (sxhash key))
    (double-float (sxhash key))
    (t (sxhash key))))

(define-hash-table-test 'equalp #'equalp #'slow-equalp-hash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Allow duplicate keywords in destructuring-bind.

(in-package :common-lisp)

;;; VERIFY-KEYWORDS -- internal
;;;
;;; Determine if key-list is a valid list of keyword/value pairs.  Do not
;;; signal the error directly, 'cause we don't know how it should be signaled.
;;; 
(defun verify-keywords (key-list valid-keys allow-other-keys)
  (declare (optimize (speed 3)))
  (do ((unknown-keyword nil)
       (remaining key-list (cddr remaining)))
      ((null remaining)
       (if (and unknown-keyword
		(not allow-other-keys)
		(not (lookup-keyword :allow-other-keys key-list)))
	   (values :unknown-keyword (list unknown-keyword valid-keys))
	   (values nil nil)))
    (cond ((not (and (consp remaining) (listp (cdr remaining))))
	   (return (values :dotted-list key-list)))
	  ((null (cdr remaining))
	   (return (values :odd-length key-list)))
	  ((or (eq (car remaining) :allow-other-keys)
	       (member (car remaining) valid-keys)))
	  (t
	   (setf unknown-keyword (car remaining))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; On FreeBSD, both gethostbyname and gethostbyaddr call malloc which
;;; is non-reentrant - disable interrupts around these.

(in-package "EXTENSIONS")

(defun lookup-host-entry (host)
  (declare (optimize (speed 3)))
  (if (typep host 'host-entry)
      host
      (with-alien
	  ((hostent (* hostent) 
		    (etypecase host
		      (string
		       (sys:without-interrupts
			(gethostbyname host)))
		      ((unsigned-byte 32)
		       (sys:without-interrupts
			(gethostbyaddr (htonl host) 4 af-inet))))))
	(unless (zerop (sap-int (alien-sap hostent)))
	  (make-host-entry
	   :name (slot hostent 'name)
	   :aliases
	   (collect ((results))
	     (iterate repeat ((index 0))
	       (declare (type kernel:index index))
	       (cond ((zerop (deref (cast (slot hostent 'aliases)
					  (* (unsigned #-alpha 32 #+alpha 64)))
				    index))
		      (results))
		     (t
		      (results (deref (slot hostent 'aliases) index))
		      (repeat (1+ index))))))
	   :addr-type (slot hostent 'addrtype)
	   :addr-list
	   (collect ((results))
	     (iterate repeat ((index 0))
	       (declare (type kernel:index index))
	       (cond ((zerop (deref (cast (slot hostent 'addr-list)
					  (* (unsigned #-alpha 32 #+alpha 64)))
				    index))
		      (results))
		     (t
		      (results 
		       (ntohl (deref (deref (slot hostent 'addr-list) index))))
		      (repeat (1+ index)))))))))))

