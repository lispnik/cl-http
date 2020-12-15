;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-

;;; Copyright John C. Mallery,  1995-96.
;;; All rights reserved.

;;;------------------------------------------------------------------- 
;;;
;;; CONSTRAINT DEFINITIONS
;;;
;;; The file containts definitions for the following kinds of constraints:
;;;
;;;     Circumstance 
;;;     Context 
;;;     URL
;;;     Header
;;;     Resource

;;;------------------------------------------------------------------- 
;;;
;;; CIRCUMSTANCE CONSTRAINTS
;;;
(in-package :w4)

(define-constraint-type
  not
  (:circumstance
    :documentation "Succeeds if its argument, a constraint or constraint-set, fails."
    :class circumstance-constraint-type
    :allocator ((constraints)
                (declare (ignore constraint-type))
                (ensure-list (allocate-constraint-structure constraints))))
  (constraint activity url constraint-set-or-constraint)
  (declare (ignore constraint))
  (not (satisfies-p url activity constraint-set-or-constraint)))

(define-constraint-type
  or
  (:circumstance
    :documentation "Succeeds if any of its arguments succeed, otherwise fails.
The arguments are any combination of constraints or constraint sets. They are
evaluated left to right."
    :class circumstance-constraint-type
    :allocator ((&rest constraint-structure)
                (declare (ignore constraint-type))
                (mapcar #'allocate-constraint-structure constraint-structure)))
  (constraint activity url &rest constraints)
  (declare (ignore constraint))
  (loop for item in constraints
        when (satisfies-p url activity item)
          return t
        finally (return nil)))

(define-constraint-type
  and
  (:circumstance
    :documentation "Succeeds if all of its arguments succeed, otherwise fails.
The arguments are any number of constraints or constraint sets and are
evaluated left to right."
    :class circumstance-constraint-type
    :allocator ((&rest constraint-structure)
                (declare (ignore constraint-type))
                (mapcar #'allocate-constraint-structure constraint-structure)))
  (constraint activity url &rest constraints)
  (declare (ignore constraint))
  (loop for item in constraints
        unless (satisfies-p url activity item)
          return nil
        finally (return t)))

(define-constraint-type
  if
  (:circumstance
    :class circumstance-constraint-type
    :documentation "This constraint allows conditional branching
in the constraint structure. If ANTECEDENT succeeds CONSEQUENT is applied,
otherwise ALTERNATE is applied. The success or failure of CONSEQUENT or
ALTERNATE determine the overal success or failure of the expression.
Each of these components can be either a single constraint or a constraint set."
    :allocator ((antecedent consequent &optional alternate)
                (declare (ignore constraint-type))
                (list (allocate-constraint-structure antecedent)
                      (allocate-constraint-structure consequent)
                      (allocate-constraint-structure alternate))))
  (constraint activity url antecedent consequent alternate)
  (declare (ignore constraint))
  (flet ((this-one-satisfies-p (x)
           (satisfies-p url activity x)))
    (declare (inline this-one-satisfies-p))
    (if (this-one-satisfies-p antecedent)
        (and consequent (this-one-satisfies-p consequent))
        (and alternate (this-one-satisfies-p alternate)))))


;;;------------------------------------------------------------------- 
;;;
;;; CONTEXT CONSTRAINTS
;;;

(declaim (inline depth-in-bound-p))

(defun depth-in-bound-p (depth)
;;  (format t "~&~'bDepth: ~S~&Stack:~S~" (depth) *url-stack*)
;;  (break "foo")
  (<= (depth) depth))

(define-constraint-type
  depth
  (:context
    :documentation "Succeeds while recursive Web walking is less than or equal to DEPTH.")
  (constraint activity url depth)
  (declare (ignore constraint url activity))
  (depth-in-bound-p depth))


;;;------------------------------------------------------------------- 
;;;
;;; URL CONSTRAINTS
;;;

(define-constraint-type
  no-cycles
  (:url
    :documentation "Succeeds for URLs that have not been walked during in the current acticity.")
  (constraint activity url)
  (declare (ignore constraint))
  #+ignore
  (format *standard-output* "~&(no-cycles ~A) => ~S"
          (name-string url) (not (%get-resource-time-stamp activity url)))
  (not (%get-resource-time-stamp activity url)))

(define-constraint-type
  url-scheme
  (:url
    :documentation "Succeeds when the URL refers to the scheme, SCHEME.
SCHEME can be a list of URL schemes.")
  (constraint activity url scheme)
  (declare (ignore constraint activity))
  (let ((len (length scheme)))
    (string-equal scheme (url:scheme url) :start1 0 :end1 len :start2 0 :end2 len)))

(defun domain-match-p (domain-name1 domain-name2 &optional (n-components 2))
  "If N-COMPONENTS from the right of DOMAIN-NAME1 and DOMAIN-NAME2 are equal,
this returns non-null."
  (declare (fixnum n-components))
  (loop with end1 = (length domain-name1)
        and end2 = (length domain-name2)
        repeat (1+ n-components)
        for pos1 = (position #\. domain-name1 :from-end t :end end1 :start 0)
        for pos2 = (position #\. domain-name2 :from-end t :end end2 :start 0)
        while (and pos1 pos2)
        unless (string-equal domain-name1 domain-name2
                             :start1 (1+ (the fixnum pos1)) :end1 end1
                             :start2 (1+ (the fixnum pos2)) :end2 end2)
          do (return-from domain-match-p nil)
        do (setq end1 pos1 end2 pos2)
        finally (return-from domain-match-p (if (and pos1 pos2) t nil))))

(defun ip-address-string-p (string)
  (flet ((ip-char-p (ch)
           (or (digit-char-p ch)
               (char= ch #\.))))
    (declare (dynamic-extent #'ip-char-p))
    (every #'ip-char-p string)))

;; DNS is slow so work hard to avoid invoking it.
(defun %url-host-equal (url host2)
  ;; must be using the canonical host name.  7/15/95 -- JCMa.
  (let ((host1 (url:host-string url))
        host1-object host1-ip-flag host1-ip-p host2-ip-p)
    (macrolet ((host1-object ()
                 `(or host1-object
                      (setq host1-object (url:host-object url))))
               (host1-ip-p ()
                 `(if host1-ip-flag
                      host1-ip-p
                      (prog1 (setq host1-ip-p (ip-address-string-p host1))
                             (setq host1-ip-flag t)))))
      (with-some-success (host2)
        (cond ((equalp host1 host2) t)
              ((and (setq host2-ip-p (ip-address-string-p host2))
                    (host1-ip-p))
               nil)
              ((not (or (domain-match-p host1 host2 2)
                        (host1-ip-p)
                        host2-ip-p))
               nil)
              ;; must be using the canonical host name.  7/15/95 -- JCMa.
              (t (case url:*url-host-name-resolution*
                   ((:always :preferred)
                    (handler-case-if (not *debug-walker*)
                       (host-eq (host1-object)
                                (parse-host host2))
                      (unknown-host-name () nil)))
                   (t nil))))))))

(define-constraint-type
  url-host
  (:dns-url
    :documentation "Succeeds when the URL refers to the host, HOST.
HOST can be a list of primary HOST domain names.")
  (constraint activity url host)
  (declare (ignore constraint activity))
  (%url-host-equal url host))

(define-constraint-type
  url-referrer-host
  (:dns-url
    :documentation "Succeeds when the parent url that refers to URL
refers to the host, HOST. HOST can be a list of primary HOST domain names.")
  (constraint activity url host)
  (declare (ignore constraint activity))
  (%url-host-equal (or (current-url) url) host))

(define-constraint-type
  url-port
  (:url
    :documentation "Succeeds when the URL refers to the port, PORT.
PORT can be a list of port numbers.")
  (constraint activity url port)
  (declare (ignore constraint activity))
  (with-some-success (port)
    (eql port (or (url:host-port url) 80.))))

(defun %url-directory-path (url directory-path)
  (let ((url-directory-path (url:path url)))
    (cond ((and (null url-directory-path) (null directory-path)) t)
          ((or (null url-directory-path) (null directory-path)) nil)
          (t (loop for components = directory-path then (cdr components)
                   for url-path = url-directory-path then (cdr url-path)
                   while (and components url-path)
                   unless (equal (car components) (car url-path))
                     do (return nil)
                   finally (return (if (or components url-path) nil t)))))))

(define-constraint-type
  url-directory-path
  (:url
    :documentation "Succeeds when the URL directory components exactly the same as DIRECTORY-PATH.
DIRECTORY-PATH is a list of directory components.  This is case insensitive.")
  (constraint activity url directory-path)
  (declare (ignore constraint activity))
  (%url-directory-path url directory-path))

(defun %url-subsumed-by-directory-path (url directory-path)
  (cond ((null directory-path) t)
        (t (let ((url-directory-path (url:path url)))
             (cond ((null url-directory-path) nil)
                   (t (loop for components = directory-path then (cdr components)
                            for url-path = url-directory-path then (cdr url-path)
                            while (and components url-path)
                            unless (equal (car components) (car url-path))
                              do (return nil)
                            finally (return (if components nil t)))))))))

(define-constraint-type
  url-subsumed-by-directory-path
  (:url
    :documentation "Succeeds when the URL directory components are subumed by DIRECTORY-PATH.
DIRECTORY-PATH is a list of directory components.  This is case insensitive.")
  (constraint activity url directory-path)
  (declare (ignore constraint activity))
  (%url-subsumed-by-directory-path url directory-path))

(define-constraint-type
  url-parent-subsumed-by-directory-path
  (:url
    :documentation "Succeeds when the directory components of the URL's parent are subumed by DIRECTORY-PATH.
DIRECTORY-PATH is a list of directory components.")
  (constraint activity url directory-path)
  (declare (ignore constraint activity))
  (let ((parent-url (or (parent-url) url)))
    ;;(format t "~&~'bURL: ~S~&Parent: ~S~&Stack: ~S~" url (parent-url) *url-stack*)
    (%url-subsumed-by-directory-path parent-url directory-path)))

(define-constraint-type
  url-class
  (:url
    :documentation "Succeeds when the URL is of class, CLASS.")
  (constraint activity url class)
  (declare (ignore constraint activity))
  (typep url class))

(define-constraint-type
  url-name
  (:url
    :documentation "Succeeds when name component of URL is NAME.
Fails for URLs that are not objects, e.g. paths.  This is case insensitive.")
  (constraint activity url name)
  (declare (ignore constraint activity))
  (equalp name (url:object url)))

(define-constraint-type
  url-extension
  (:url
    :documentation "Succeeds when extension component of URL is EXTENSION.
Fails for URLs that are not objects, e.g. paths.  This is case insensitive.")
  (constraint activity url extension)
  (declare (ignore constraint activity))
  (equalp extension (url:extension url)))

 (define-constraint-type
  url-search
  (:url
    :documentation "Succeeds when SUBSTRING is found anywhere 
in the fully qualified URL namestring. This is case insensitive.")
  (constraint activity url substring)
  (declare (ignore constraint activity))
  (search substring (url:name-string url) :test #'char-equal))

(define-constraint-type
  url-satisfies
  (:url
    :documentation "Applies PREDICATE to the URL object.")
  (constraint activity url predicate)
  (declare (ignore constraint activity))
  (funcall predicate url))


;;;------------------------------------------------------------------- 
;;;
;;; HEADER CONSTRAINTS
;;;

(define-constraint-type
  header-content-length
  (:header
    :documentation "Applies COMPARATOR to the CONTENT-LENGTH and SIZE.
COMPARATOR must be prepared to handle a null CONTENT-LENGTH.")
  (constraint activity url comparator size)
  (declare (ignore constraint))
  (multiple-value-bind (headers)
      (get-resource-headers activity url)
    (cond (headers
           (let ((content-length (get-header :content-length headers)))
             (funcall comparator content-length size)))
          (t nil))))

(define-constraint-type
  header-content-length-upto
  (:header
    :documentation "Succeeds if the content-length is less than or equal to SIZE.
When content-length is unavailable, it succeeds when DEFAULT is non-null, otherwise fails.")
  (constraint activity url size &optional (default t))
  (declare (ignore constraint))
  (multiple-value-bind (headers)
      (get-resource-headers activity url)
    (cond (headers
           (let ((content-length (get-header :content-length headers)))
             (if content-length (< content-length size) default)))
          (t nil))))

(define-constraint-type
  header-content-type
  (:header
    :documentation "Succeed when the content type for the resource matches CONTENT-TYPE-SPEC.
Returns NIL if the CONTENT-TYPE is not available.")
  (constraint activity url content-type-spec)
  (declare (ignore constraint))
  (multiple-value-bind (headers)
      (get-resource-headers activity url)
    (let ((content-type (get-header :content-type headers)))
      (when content-type
        (destructuring-bind (major minor &rest plist)
            content-type
          (and (eq major (first content-type-spec))
               (eq minor (second content-type-spec))
               (loop for (key value) in (cddr content-type-spec) by #'cddr
                     unless (equalp (getf plist key) value)
                       return nil
                     finally (return t))))))))

(define-constraint-type
  header-expires
  (:header
    :documentation "Applies COMPARATOR to the EXPIRES date and UNIVERSAL-TIME.
Returns NIL if the EXPIRES is not available.")
  (constraint activity url comparator universal-time)
  (declare (ignore constraint))
  (multiple-value-bind (headers)
      (get-resource-headers activity url)
    (let ((expires (get-header :expires headers)))
      (and expires
           (funcall comparator expires universal-time)))))

(define-constraint-type
  header-last-modified
  (:header
    :documentation "Applies COMPARATOR to the LAST-MODIFIED date and UNIVERSAL-TIME.
Returns NIL if LAST-MODIFIED is not available.")
  (constraint activity url comparator universal-time)
  (declare (ignore constraint))
  (multiple-value-bind (headers)
      (get-resource-headers activity url)
    (let ((last-modified (get-header :last-modified headers)))
      (and last-modified
           (funcall comparator last-modified universal-time)))))

(define-constraint-type
  header-predicate
  (:header
    :documentation "Applies PREDICATE to the parsed value of header-keyword
and whether the header was available.")
  (constraint activity url header-keyword predicate)
  (declare (ignore constraint))
  (multiple-value-bind (headers)
      (get-resource-headers activity url)
    (multiple-value-bind (value found-p)
        (get-header header-keyword headers)
      (funcall predicate value found-p))))

(define-constraint-type
  header-resource-age
  (:header
    :documentation "Succeeds when the resource age is between (or equal to) MINIMUM and MAXIMUM.
Fails if LAST-MODIFIED header is not available.")
  (constraint activity url minimum maximum)
  (declare (ignore constraint))
  (multiple-value-bind (headers)
      (get-resource-headers activity url)
    (let ((date (or (get-header :date headers) (get-universal-time)))
          (last-modified (get-header :last-modified headers)))
      (and date
           last-modified
           (<= minimum (- date last-modified) maximum)))))

(define-constraint-type
  header-robots-allowed
  (:header
    :documentation "Succeeds when robots are allowed on the host in URL.")
  (constraint activity url)
  (declare (ignore constraint))
  (case (robot-exclusion-status activity url)
    (:excluded nil)
    (:allowed t)
    (:unknown
      (let ((exclusion-url (robot-exclusion-url url)))
        (multiple-value-bind (headers status-code)
            (get-resource-headers activity exclusion-url)
          (declare (ignore headers))
          (case status-code
            (404 (note-robot-exclusion-status activity (host-object url) :allowed)
                 t)
            (t (note-robot-exclusion-status activity (host-object url) :excluded)
               nil)))))))

(define-constraint-type
  header-server
  (:header
    :documentation "Applies PREDICATE to the SERVER header.
Returns NIL if the SERVER is not available.")
  (constraint activity url predicate)
  (declare (ignore constraint))
  (multiple-value-bind (headers)
      (get-resource-headers activity url)
    (let ((server (get-header :server headers)))
      (and server
           (funcall predicate server)))))


;;;------------------------------------------------------------------- 
;;;
;;; RESOURCE CONSTRAINTS
;;;

(define-constraint-type
  resource-search
  (:html-body
    :documentation "Succeeds when substring is found in the content of resource.
Fails if the content is not text. This is case insensitive.")
  (constraint activity url substring)
  (declare (ignore constraint))
  (multiple-value-bind (content headers)
      (get-resource-content activity url)
    (when (and content headers)
      (let ((content-type (get-header :content-type headers)))
        (case (http:mime-content-type-copy-mode content-type)
          ((:text :crlf) (search substring content :test #'char-equal))
          (t nil))))))

(define-constraint-type
  resource-satisfies
  (:html-body
    :documentation "Applies PREDICATE to the content of resource.
Fails if the content is not text.")
  (constraint activity url predicate)
  (declare (ignore constraint))
  (multiple-value-bind (content headers)
      (get-resource-content activity url)
    (when (and content headers)
      (let ((content-type (get-header :content-type headers)))
        (case (http:mime-content-type-copy-mode content-type)
          ((:text :crlf) (funcall predicate content))
          (t nil))))))

