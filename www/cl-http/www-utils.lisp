;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: www-utils -*-

;;; Copyright John C. Mallery,  1994-1998.
;;; All rights reserved.

;;;------------------------------------------------------------------- 
;;;
;;;  PORTABLE WWW UTILS
;;; 
;;; These utilities compensate for functionality not available in Lisps
;;; other than the Lisp Machine.

(in-package :www-utils)

;;;------------------------------------------------------------------- 
;;;
;;; SERVER VERSION INFORMATION
;;; 

;; This value should be set by the port after loading http:mac;www-utils.lisp
(defvar cl-user::*cl-http-server-version*)

(declaim (inline %server-version))

;; Set this for each port by setting cl-user::*cl-http-server-version*
;; to a '(major minor port-major port-minor &optional port-patch-level)
(defun %server-version-info ()
  "Returns the version numbers for the server and the port."
  (declare (values major minor port-major port-minor port-patch-level))
  cl-user::*cl-http-server-version*)

#+ignore
(define server-version ()
  "Returns the server version,
for example \"CL-HTTP/41.4/MCL/1.7.1\""
  (multiple-value-bind (major minor port-major port-minor port-patch-level)
      (%server-version-info)
    (format nil "CL-HTTP/~D.~D/~A~:[~;, ~:*~A~]~:[~;~:*/~D.~D~:[~;.~D~]~]"
            major minor (lisp-implementation-type) (lisp-implementation-version)
            port-major port-minor port-patch-level))) 

;;;------------------------------------------------------------------- 
;;;
;;; 
;;; 

(defmacro with-null-stream ((&rest streams) &body body)
  "Binds STREAMS to the null-stream within BODY."
  (loop for stream in streams
        collect `(,stream (make-broadcast-stream)) into bindings
        finally (return `(let ,bindings ,@body))))

(declaim (ftype (function (&optional t) t) local-host)
         (ftype (function (t) t) host-mail-name))

(defparameter *user* nil)
(defparameter *host-for-mail-for-user* nil) 

(define user-mail-address ()
  "Returns the mail address for the current user."
  (let ((user (or *user* "anonymous"))
        (host (host-mail-name  (or *host-for-mail-for-user* (local-host)))))
    (concatenate 'string user "@" host))) 

;;;------------------------------------------------------------------- 
;;;
;;; TIME RELATED
;;;

(define-constant *month-alist*
  '((1 "Jan" "January")
    (2 "Feb" "February")
    (3 "Mar" "March")
    (4 "Apr" "April")
    (5 "May" "May")
    (6 "Jun" "June")
    (7 "Jul" "July")
    (8 "Aug" "August")
    (9 "Sep" "September")
    (10 "Oct" "October")
    (11 "Nov" "November")
    (12 "Dec" "December")))

(define translated-month (month &optional abbv)
  "Returns a number from 1 to 12, corresponding to the month passed in.  ABBV controls
   whether the full month name is expected, or only the first three letters."
  (cond ((digit-char-p (elt month 0))
         (let ((month-num (parse-integer month)))
           (or (caar (member month-num *month-alist* :key #'first))
               #+ignore (error 'ccl:parse-error "~A is a bad month." month))))
        (t (flet ((month-abbrev (item entry)
                    (string-equal item (first entry)))     ;; should be made faster?
                  (month-full (item entry)
                    (string-equal item (second entry))))
             (declare (dynamic-extent #'month-abbrev #'month-full))
             (let ((pos (rassoc month *month-alist* :test (if abbv #'month-abbrev #'month-full))))
               (cond (pos (first pos))
                     (t #+ignore (error 'ccl:parse-error "~A is a bad month." month))))))))

(define month-string (month &optional (mode :long))
  "Returns the month string for month
   mode can be any of: :short, :long, :medium, :french, :roman, :german, :italian."
  (unless (and (integerp month) (< 0 month 13))
    (error "MONTH, ~s, is not an integer from 1 through 12" month))
  (let ((entry (assoc month *month-alist* :test #'=)))
    (ecase mode
      (:short (second entry))
      (:long (third entry)))))

(define-constant *weekday-alist*
  '((0 "Mon" "Monday")
    (1"Tue" "Tuesday")
    (2 "Wed" "Wednesday")
    (3 "Thu" "Thursday")
    (4 "Fri" "Friday")
    (5 "Sat" "Saturday")
    (6 "Sun" "Sunday")))

(define day-of-the-week-string (weekday &optional (mode :long))
  "Returns the weekday string for weekday
   mode can be any of: :SHORT, :LONG, :MEDIUM, :FRENCH, :ROMAN, :GERMAN, :ITALIAN."
  (unless (and (integerp weekday) (< -1 weekday 7))
    (error "WEEKDAY, ~S, is not an integer from 0 through 6." weekday))
  (let ((entry (assoc weekday *weekday-alist* :test #'=)))
    (ecase mode
      (:short (second entry))
      (:long (third entry)))))

(defun weekday ()
  (declare (values weekday-keyword))
  (multiple-value-bind (seconds minutes hours date month year day-of-the-week)
      (decode-universal-time (get-universal-time))
    (declare (ignore seconds minutes hours date month year))
    (intern (string-upcase (day-of-the-week-string day-of-the-week :long)) http::*keyword-package*)))

;; a better time to run night time jobs, because it is midnight on the west coast.
(defun next-3am-universal-time (&optional (offset 0))
  "Returns universal time for the next 3am in the loal timezone, plusor minus OFFSET."
  (declare (fixnum offset))
  (multiple-value-bind (seconds minutes hours day month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore seconds minutes hours))
    (the bignum 
         (+ (the fixnum (* 60. 60. 3.))         ; plus 3 hours
            offset                              ;plus offset
            (encode-universal-time 59 59 23 day month year (time-zone))))))

;;;--------------------------------------------------------------------
;;;
;;;  Time Parser for RFC 850, 822 and ANSI-C formats

(defparameter *timezone-names* '((-13     "NZDT")
				 (-12 "NZT" "NZST")
				 (-9 "JST")
				 (-2 "EET" "CEST" "MEST")
				 (-1 "CET" "MET" "BST")
				 (0 "GMT" "UT" "WET")
				 (3 "ADT")
				 (4 "AST" "EDT")
				 (5 "EST" "CDT")
				 (6 "CST" "MDT")
				 (7 "MST" "PDT")
				 (8 "PST" "YDT")))

(defvar *timezone-name-table* nil)

(defun initialize-named-timezones ()
  (let ((table (make-hash-table :test #'equal)))
    ;; Offsets are CL timezones (hours West of GMT).
    ;; Names are full names . (daylight saving names).
    (loop for (offset . names) in *timezone-names*
	  do (loop for name in names
		   do (setf (gethash name table) offset)))
    (setq *timezone-name-table* table)))

(initialize-named-timezones)

(defun translated-timezone (timezone)
  (etypecase timezone
    (null 0)					; null timezones are GMT.
    (integer
      (cond ((< -43 timezone 23) timezone)	; between -42 and 24
	    (t (error "~A is an invalid timezone" timezone))))
    (string 
      (cond ((gethash timezone *timezone-name-table*))
	    ((= (length timezone) 5)
	     (let* ((hours (parse-integer timezone :start 1 :end 3))
		    (minutes (parse-integer timezone :start 3))
		    (zone (+ hours (/ minutes 60))))
	       ;; CL timezones have opposite sign from rfc822 ones!
	       (if (eql (char timezone 0) #\-)
		   zone
		   (- zone))))
	    (t (error  "~A is an invalid timezone" timezone))))))

(defconstant *time-separators* '(#\space #\: #\- #\,))

(defun tokenize-string (string &aux temp-string result)
  (flet ((index-temp-string ()
           (unless (null temp-string)
             (push (concatenate 'string temp-string) result)
             (setq temp-string nil))))
    (declare (inline index-temp-string))
    (loop for index downfrom (1- (the fixnum (length string))) to 0
          for char = (aref string index)
          when (member char *time-separators* :test #'char=)
            do (progn
                 (when (and (> index 0)
                            (eql char #\-)
                            (member (aref string (1- index)) *time-separators*))
                   (push char temp-string))
                 (index-temp-string))
          else do (push char temp-string)
          finally (index-temp-string) 
                  (return (values-list result)))))

(defun parse-rfc-850-and-822-time (time-string)
  (multiple-value-bind (x-weekday day-of-month month year hour minute seconds timezone)
      (tokenize-string time-string)
    (declare (ignore x-weekday))
    (parsed-encode-universal-time seconds minute hour day-of-month month year (or timezone "GMT") :abbv-month t)))

(defun parse-ansi-c-time (time-string)
  (multiple-value-bind (x-weekday month day-of-month hour minute seconds year timezone)
      (tokenize-string time-string)
    (declare (ignore x-weekday))                
    (parsed-encode-universal-time seconds minute hour day-of-month month year (or timezone "GMT") :abbv-month t)))

(defun parse-iso-time (time-string)
  (multiple-value-bind (year month day hour minute second)
      (tokenize-string time-string)
    (parsed-encode-universal-time second minute hour day month year "GMT")))

(defun parsed-encode-universal-time (seconds minute hour day-of-month month year
                                             timezone &key abbv-month)
  (encode-universal-time
    (parse-integer seconds)
    (parse-integer minute)
    (parse-integer hour)
    (parse-integer day-of-month)
    (translated-month month abbv-month)
    (parse-integer year)
    (if timezone (translated-timezone timezone) 0)))

(declaim (inline parse-http-time-string))

(defun parse-http-time-string (time-string)
  "This returns a universal time when a string containing a time in RFC 850, 822
    or ANSI-C format."
  (cond
    ((char= (aref time-string 3) #\space)
     (parse-ansi-c-time time-string))
    (t (parse-rfc-850-and-822-time time-string))))

;; This will not do.  We have to be able to parse timestrings into universal time. -- JCMa 12/30/1994.
(define parse-gmt-time (string &optional (start 0) (end (length string)))
  (let ((time-string (if (and (zerop start)
                              (= end (length string)))
                         string
                         (subseq string start end))))
    (declare (dynamic-extent time-string))
    (parse-http-time-string time-string)))

(define parse-universal-time (time-string &optional (start 0) end (futurep t)
                                          base-time must-have-time date-must-have-year
                                          time-must-have-second (day-must-be-valid t))
  (declare (ignore start end futurep base-time must-have-time date-must-have-year
                   time-must-have-second day-must-be-valid))
  (parse-http-time-string time-string))

;;;------------------------------------------------------------------- 
;;;
;;; PATHNAME-RELATED OPERATIONS
;;;

(declaim (special http::*default-pathname*))

(defun default-pathname (&optional defaults host default-type default-version)
  (let ((default-path http::*default-pathname*))
    (cond ((or defaults host default-type default-version)
           (let*  ((default-dir (pathname-directory default-path))
                   (path (make-pathname :host (or host (pathname-host default-path))
                                        :device (and (not host) (pathname-device default-path))
                                        :directory (and default-dir `(:absolute ,.default-dir)
                                                        :type (pathname-type default-path)
                                                        :version (pathname-version default-path)))))
             (if defaults
                 (merge-pathnames path defaults)
                 path)))
          (t default-path)))) 

(eval-when (load eval compile)
  (export 'default-pathname :www-utils))

(defun %set-pathname-version (pathname value)
  (let  ((path (pathname pathname)))
    (make-pathname :host (pathname-host path)
                   :device (pathname-device path)
                   :directory (pathname-directory path)
                   :name (pathname-name path)
                   :type (pathname-type path)
                   :version value)))

#-allegro
(defsetf pathname-version %set-pathname-version)

#+allegro
(excl:without-package-locks
  (defsetf pathname-version %set-pathname-version))

(defun %set-pathname-type (pathname value)
  (let  ((path (pathname pathname)))
    (make-pathname :host (pathname-host path)
                   :device (pathname-device path)
                   :directory (pathname-directory path)
                   :name (pathname-name path)
                   :type value
                   :version (pathname-version path))))

#-allegro
(defsetf pathname-type %set-pathname-type)

#+allegro
(excl:without-package-locks
  (defsetf pathname-type %set-pathname-type)) 

(defmethod pathname-directory-most-specific-name ((pathname pathname))
  (car (last (pathname-directory pathname)))) 

(defmethod pathname-as-directory ((pathname pathname))
  (cond ((pathname-name pathname)
         (make-pathname :host (pathname-host pathname)
                        :device (pathname-device pathname)
                        :directory (pathname-directory pathname)))
        (t pathname)))

;; Define equivalence mapping for portable case
;; Used by auto-export-pathname-url to handle bad pathname syntax.
(deftype parse-pathname-error () 
  "Error parsing a pathname."
  #-lispworks3.2 '(and condition parse-error)
  #+lispworks3.2 'conditions:pathname-error)

(eval-when (load eval compile)
  (export 'parse-pathname-error :www-utils))

;; Make array references blaze inside this macro.
(defmacro with-fast-array-references (bindings &body body)
  "Declares the arrays in bindings (var value &optional type)
as type and sets speed to 3 with safety 0 within its scope."
  (loop for (var val type) in bindings
        collect `(,var ,val) into n-bindings
        when type
          collect `(type ,type ,var) into type-dcls
        finally (return `(let ,n-bindings
                           (locally 
                             (declare (optimize (speed 3) (safety 0)) . ,type-dcls)
                             ,@body)))))

;;;------------------------------------------------------------------- 
;;;
;;; READING DELIMITED LINES
;;;

;;  second delimiter needed to be cleared, meaning that care ust be taken calling this.-- JCMa 1/15/1995.
;; NCSA server does not adhere to the spec, and sends only one Linefeed as a line terminator.-- JCMa 1/27/1995.

;; Generic function in http:server;utils.lisp   12/18/95 -- JCMa.
;; use the EOF handling to prevent running off the end and failing to detect
;; that the server is no longer handing out the bits.  7/17/95 -- JCMa.
#-(or mcl lucid)
(defmethod read-delimited-line (stream &optional (delimiters '(#\Return #\Linefeed)) eof buffer)
  "Reads a line from stream which is delimited by DELIMITERS."
  (declare (values line eof delimiter length)
           (special http::*line-buffer-size*)
           (special http::line-buffer))
  (flet ((do-it (stream delimiters buffer)
           (declare (type string buffer))
           (flet ((clear-delimiter (prev-char stream)
                    (let ((char (read-char stream nil nil)))
                      (when (and char
                                 (or (eql char prev-char)
                                     (not (member char delimiters :test #'char=))))
                        (unread-char char stream)))))
             (declare (inline clear-delimiter))
             (let* ((size (array-total-size buffer))
                    (index -1)
                    error-p delimiter)
               (handler-case
                 (with-fast-array-references ((buffer buffer string))
                   (loop initially (setf (fill-pointer buffer) 0)
                         for char = (read-char stream t eof t)
                         until (or (eql char eof) (member char delimiters :test #'char=))
                         for idx upfrom 0
                         unless (< idx size)
                           do (setq size (floor (* (the fixnum size) 1.2))
                                    buffer (adjust-array buffer size :element-type http::*standard-character-type*))
                         do (setf (aref buffer idx) char)
                            (setq index idx)
                            ;; (format t "~:C|" char)
                         finally (if (and (eql char eof) (< 0 idx))
                                     (setq error-p t)
                                     (setq delimiter char))
                                 (clear-delimiter char stream)))
                 (end-of-file  () (setq error-p t)))
               (if (= -1 index)
                   (values (if error-p eof buffer) error-p delimiter 0)
                   (values buffer error-p delimiter (setf (fill-pointer buffer) (1+ (the fixnum index)))))))))
    (if buffer
        (do-it stream delimiters buffer)
        (using-resource (line-buffer http::line-buffer http::*line-buffer-size*)
          (multiple-value-bind (buf error-p delim length)
              (do-it stream delimiters line-buffer)
            (values (if error-p eof (subseq buf 0 length)) error-p delim length))))))

#+ignore
(defmethod read-delimited-line (stream &optional (delimiters '(#\Linefeed #\cr))
                                       eof buffer &aux delim error-p)
  "Reads a line from stream which is delimited by DELIMITERS."
  (declare (values line eof delimiter length))
  (flet ((clear-delimiter (prev-char stream)
           (let ((char (read-char stream nil nil)))
             (when (and char
                        (or (eql char prev-char)
                            (not (member char delimiters :test #'char=))))
               (unread-char char stream)))))
    (declare (inline clear-delimiter))
    (macrolet ((read-it (output-string)
                 `(loop with chars-p
                        for char = (read-char stream nil eof)
                        do (cond ((eql char eof) 
                                  (if chars-p 
                                      (return)
                                      (return-from read-delimited-line (values nil t nil 0))))
                                 (chars-p)
                                 (t (setq chars-p t)))
                                                ; do (format t "~:C|" char)
                        until (member char delimiters :test #'char=)
                        do (write-char char ,output-string)
                        finally (setq delim char)
                                (clear-delimiter char stream))))
      (let ((string (if buffer
                        (with-output-to-string (s buffer)
                          (read-it s))
                        (with-output-to-string (s)
                          (read-it s)))))
        (values string error-p delim (length string))))))
;;;------------------------------------------------------------------- 
;;;
;;; INITIALIZATIONS
;;;

(define add-initialization (name form &optional keywords (list-name 'http:*server-initialization-list*))
  "Adds the initialization FORM with name to LIST-NAME.
KEYWORDS is a list. It can contain thse keywords:

          :NORMAL -- Add to initialization list.
          :NOW    -- Evaluate now and add to initialization list."
  (check-type keywords list)
  (let ((entry (find name (symbol-value list-name) :test #'string-equal :key #'first)))
    ;; evaluate the form when :now is on the keywords.
    (when (member :now keywords)
      (eval form))
    (cond (entry
           (setf (second entry) form
                 (third entry) nil
                 (cdddr entry) keywords))
          (t (push `(,name ,form nil ,@keywords) 
                   (symbol-value list-name))))))

(define run-initializations (&optional (list-name 'http:*server-initialization-list*)  redo-flag)
  "Runs the initializations on LIST-NAME."
  (loop for entry in (symbol-value list-name)
        for (nil form done-p) = entry
        when (or (not done-p) redo-flag)
          do (eval form)
             (setf (third entry) t)))

(define reset-initializations (&optional (list-name 'http:*server-initialization-list*))
  "Resets the initializations on LIST-NAME."
  (loop for entry in (symbol-value list-name)
        when (third entry)
          do (setf (third entry) t))) 

(define delete-initialization (name &optional keywords (list-name 'http:*server-initialization-list*))
  "Deletes the initialization named NAME from LIST-NAME."
  (declare (ignore keywords))
  (setf (symbol-value list-name)
        (delete name (symbol-value list-name)
                :test #'string-equal
                :key #'first)))

(define show-initializations (&key (list-name 'http:*server-initialization-list*) 
                                   (stream *standard-output*))
  "Describes the initialization list, LIST-NAME, on STREAM."
  (loop initially (format stream "~&Initializations on ~S~2%")
        for form in (symbol-value list-name)
        for count upfrom 1
        do (format stream "~&~D  ~S" count form)))

;;;------------------------------------------------------------------- 
;;;
;;; DAILY TASKS
;;; 

(define-variable *daily-server-tasks* nil
                 "Daily tasks performed by the server typically at 3am.")

(declaim (ftype (function () t) synchronize-daily-server-tasks)
         (ftype (function (t) t) report-string)
         (ftype (function (t &rest t) t) log-http-server-error))

;; Toplevel routines for getting work done everyday at midnight PST.
(define run-daily-server-tasks ()
  (with-null-stream  (*standard-output* *query-io*)
    (loop with week-day = (weekday)
	  for (name periodicity form) in *daily-server-tasks*
	  when (case periodicity
		 (:daily t)
		 (:weekly (eq week-day :sunday))
		 (t (eq periodicity week-day)))
	    do (handler-case
		 (apply (car form) (cdr form))
		 (error (err) (log-http-server-error "Error in daily server task, ~S:~&~A" name (report-string err))))))
  ;; Reset so we run again.
  (synchronize-daily-server-tasks))

(define add-periodic-task (name periodicity form)
  "Add a periodic server task.
NAME is a unique string naming the task.
PERIODICITY is a weekday keyword or :daily, or :weekly.
FORM is the form that executes the desired task."
  (check-type name string)
  (check-type periodicity (member :daily :sunday :monday :tuesday :wednesday :thursday :friday :saturday :weekly))
  (check-type form cons)
  (let ((entry (find name *daily-server-tasks* :test #'string-equal :key #'first)))
    (cond (entry
           (setf (second entry) periodicity
                 (third entry) form))
          (t (push `(,name ,periodicity ,form) *daily-server-tasks*)))))

(define delete-periodic-task (name)
  "Delete a periodic server task named NAME." 
  (setq *daily-server-tasks* (delete name *daily-server-tasks* :test #'string-equal :key #'first)))

(define show-periodic-tasks (&key (stream *standard-output*))
  "Describes the periodic server tasks on STREAM."
  (loop initially (format stream "~&Daily HTTP Server Tasks~2%")
        for (name periodicity form) in (sort *daily-server-tasks* #'string-lessp :key #'first)
        for count upfrom 1
        do (format stream "~&~D  ~A [~:(~A~)]: ~S" count name periodicity form)))

;; this is a temporary kludge -- comment out as we have implementations for ports.
#-(or mcl)
(eval-when (load eval)
  (declaim (inline fixnum-microsecond-time))
  (define fixnum-microsecond-time ()
    (get-internal-real-time))
  (export 'fixnum-microsecond-time :www-utils))

#-(or allegro)
(setf (fdefinition '%char-equal) (fdefinition 'char-equal)
      (symbol-plist '%char-equal) (symbol-plist 'char-equal))

#-(or mcl genera)
(declaim (inline pathname-external-name-string))

#-(or mcl genera)
(defun pathname-external-name-string (string &optional start end)
  "Removes any operating-system-specific characters from STRING,
returning a copy of the string."
  (declare (ignore start end))
  string)

#-genera
(defmacro let-if (condition bindings &body forms)
  `(cond (,condition
	  (let ,bindings . ,forms))
         (t . ,forms)))

;; This could cons substantially less 2/3/97 -- JCMa.
#-genera
(defun generate-message-id (&optional time uniquizer)
  "Generates a unique message ID based on TIME, universal time, 
and UNIQUIZER, a factor assuring two threads on the same machine cannot collide."
  (let* ((string (with-output-to-string (stream)
		   (write (or time (get-universal-time)) :stream stream)
		   (write uniquizer :stream stream)))
	 (digest1 (sha:sha-digest-hexadecimal-string string))
	 (digest2 (subseq digest1 0 20))
	 (host (or (local-host-domain-name)
		   (concatenate 'string "[" (local-host-ip-address) "]"))))
    (declare (dynamic-extent string digest1 digest2 host))
    (concatenate 'string "<" digest2 "@" host">")))

#-genera
(define copy-vector-portion (from-vector from-start from-end to-vector to-start to-end)
  (with-fast-array-references ((from-vector from-vector vector)
			       (to-vector to-vector vector))
    (loop for idx1 upfrom from-start below from-end
	  for idx2 upfrom to-start below to-end
	  do (setf (aref to-vector idx2) (aref from-vector idx1)))
    t))


;;;------------------------------------------------------------------- 
;;;
;;; PORTABLE INTEGER BIT VECTOR HACKING
;;;

#-genera
(define decode-bitmask (mask)
  "Decode an integer used as a binary interpretation for non-negative integers."
  (declare (values integer-list))
  (check-type mask integer)
  (loop for idx from 0 below (integer-length mask)
	unless (ldb-test (byte 1 idx) mask)
	  collect idx))

#-genera
(define map-bitmask (mask function)
  "Maps FUNCTION over every index position in MASK where the bit is 1.
This maps up from the lowest bit, which is position 1."
  (check-type mask integer)
  (loop for idx from 0 below (integer-length mask)
	unless (zerop (ldb (byte 1 idx) mask))
	  do (funcall function idx)))

#-genera
(define reverse-map-bitmask (mask function)
  "Maps FUNCTION over every index position in MASK where the bit is 1.
This maps down from the highest bit."
  (check-type mask integer)
  (loop for idx downfrom (1- (integer-length mask)) to 0
	unless (zerop (ldb (byte 1 idx) mask))
	  do (funcall function idx)))

#-genera
(defun modify-hash (table key function &aux value)
  "Combines the action of setf of gethash into one call to modify- hash. It lets
you both examine the value of key and change it. It is more efficient because
it does the lookup once instead of twice.

Finds the value associated with key in table, then calls function with key,
this value, a flag indicating whether or not the value was found.  Puts
whatever is returned by this call to function into table, associating it
with key. Returns the new value and the key of the entry. Note:  The actual
key stored in table is the one that is used on function, not the one you
supply with key."
  (declare (values new-value key))
  (multiple-value-bind (entry foundp found-key)
      (gethash key table)
    (cond (foundp
	   (setq value (funcall function found-key entry foundp))
	   (unless (eq value entry)
	     (setf (gethash found-key table) value))
	   (values  value found-key))
	  (t (setq value (funcall function key nil nil))
	     (setf (gethash key table) value)
	     (values  value key)))))

#-genera
(export 'modify-hash :www-utils)


;;;------------------------------------------------------------------- 
;;;
;;; WITH-PROCESS
;;;

#-(or Genera LispWorks)
(defmacro with-timeout ((timeout &key error-p) &body body)
  "Executes BODY and returns the values of the last form in BODY. However, if
the execution takes longer than TIMEOUT seconds, abort it. If :ERROR-P is
unsupplied or false, just return nil. If :ERROR-P is true, signal an error."
  `(progn ,timeout ,error-p . ,body))

#-Genera
(export 'with-timeout :www-utils)
