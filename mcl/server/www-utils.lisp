;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: www-utils -*-

;;; Copyright John C. Mallery,  1994-1999.
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

(defvar *month-table* nil)

(defun initialize-month-table ()
   (let ((table (make-hash-table :test #'equalp)))
      (loop for (num . names) in *month-alist*
	       do (loop for name in names
		            do (setf (gethash name table) num)))
      (setq *month-table* table)))

(initialize-month-table)

(defun translated-month (string &optional (start 0) (end (length string)))
   "Returns a number from 1 to 12, corresponding to the month passed in."
   (flet ((some-digit-char-p (string start end)
               (with-fast-array-references ((string string string))
                   (loop for idx upfrom start below end
                            when (digit-char-p (aref string idx))
                            return t
                            else  return nil
                            finally (return t)))))
      (declare (inline some-digit-char-p))
      (cond ((some-digit-char-p string start end)
                 (let ((month-num (parse-integer string :start start :end end)))
                    (if (< 0 month-num 13)
                       month-num
                       (error 'HTTP::bad-syntax-provided :format-string "~A in ~S is a bad month." :format-args (list month-num (subseq string start end))))))
               (t (let ((month (subseq string start end)))
                     (declare (dynamic-extent month))
                     (cond ((gethash month *month-table*))
                              (t (error 'HTTP::bad-syntax-provided :format-string"~A is a bad month." :format-args(list (subseq string start end))))))))))

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
(define next-3am-universal-time (&optional (offset 0) (reference-time (get-universal-time)))
  "Returns the universal time for the next 3am in the local timezone relative to REFERENCE-TIME.
OFFSET is a positive or negative number of seconds relative to 3am."
  (declare (fixnum offset) (bignum reference-time))
  (multiple-value-bind (seconds minutes hours date month year day-of-the-week)
      (decode-universal-time reference-time)
    (declare (fixnum seconds minutes hours)
	     (ignore day-of-the-week))
    (+ (if (plusp (- (the fixnum (+ seconds (* 60 minutes) (* #.(* 60 60) hours)))
		     (the fixnum #.(* 3 60 60))	;3am
		     offset))
	   #.(* 60. 60. 24.)			;plus 24 hours
	   0)
       offset					;offset
       (the bignum (encode-universal-time 0 0 3. date month year (time-zone))))))

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
   (let ((table (make-hash-table :test #'equalP)))
      ;; Offsets are CL timezones (hours West of GMT).
      ;; Names are full names . (daylight saving names).
      (loop for (offset . names) in *timezone-names*
	       do (loop for name in names
		            do (setf (gethash name table) offset)))
      (setq *timezone-name-table* table)))

(initialize-named-timezones)

(defun string-translated-timezone (timezone &optional (start 0) (end (length timezone)))
   (declare (fixnum start end))
   (flet ((hash-zone (string start end)
               (when (alpha-char-p (aref string 0))
                   (let ((key (subseq timezone start end)))
                      (declare (dynamic-extent key))
                      (gethash key *timezone-name-table*)))))
      (declare (inline hash-zone))
      (with-fast-array-references ((timezone timezone string))
          (cond ((hash-zone timezone start end))
                   ((= (- end start) 5)
                     (let* ((hours (parse-integer timezone :start (1+ start) :end (+ 3 start)))
                               (minutes (parse-integer timezone :start (+ 3 start):end end))
                               (zone (if (zerop minutes) hours (+ hours (/ minutes 60)))))
                        (declare (fixnum hours minutes))
                        ;; CL timezones have opposite sign from rfc822 ones!
                        ;; timezones may be ratios when they include minutes.
                        (if (eql (char timezone 0) #\-)
                           zone
                           (- zone))))
                   (t (error  "~A is an invalid timezone" timezone))))))

(defun translated-timezone (timezone)
   (etypecase timezone
      (null 0)					; null timezones are GMT.
      (number                           ; can be floats or ratios when zone includes minutes
        (cond ((< -43 timezone 23) timezone)	; between -42 and 24
	         (t (error "~A is an invalid timezone" timezone))))
      (string 
        (string-translated-timezone timezone))))

(defconstant *time-separators* '(#\space #\: #\- #\,))

(declaim (inline time-delimiter-position))

(defun time-delimiter-position (string start end)
   (with-fast-array-references ((string string string))
       (unless (eql start end) 
          (loop for idx fixnum upfrom start below end
                   when (member (aref string idx) *time-separators*)
                   return idx
                   finally (return nil)))))

(declaim (inline time-delimiter-position-if-not))

(defun time-delimiter-position-if-not (string start end)
   (with-fast-array-references ((string string string))
       (unless (eql start end)
          (loop for idx fixnum upfrom start below end
                   unless (member (aref string idx) *time-separators*)
                   return idx
                   finally (return nil)))))

;; Parses iso time of the format 1999-05-09-20 20:31:20
(defun parse-iso-time (string &optional (start 0) (end (length string)))
   (multiple-value-bind (start end)
                                   (http::string-trim-bounds '(#\space) string start end)
       (let* ((p1 (time-delimiter-position string  start  end))          ; year
                 (p2 (time-delimiter-position string  (1+ (the fixnum p1))  end))     ; month
                 (p3 (time-delimiter-position string  (1+ (the fixnum p2))  end))     ; day
                 (p4 (and p3 (time-delimiter-position string  (1+ (the fixnum p3))  end)))      ;seconds
                 (p5 (and p4 (time-delimiter-position string  (1+ (the fixnum p4))  end))))     ; minutes
          (encode-universal-time (if p5 (parse-integer string :start (1+ (the fixnum p5)) :end end) 0)      ; second
                                               (if p4 (parse-integer string :start (1+ (the fixnum p4)) :end p5) 0)          ; minute
                                               (if p3 (parse-integer string :start (1+ (the fixnum p3)) :end p4) 0)         ; hour
                                               (parse-integer string :start (1+ (the fixnum p2)) :end (or p3 end))         ; day
                                               (translated-month string (1+ (the fixnum p1)) p2)         ; month
                                               (parse-integer string :start start :end p1)         ; year
                                               0))))    ; GMT 

;; Parses RFC 850/822 time format -- Mon, 10 May 1999 05:00:10 GMT
;; (x-weekday day-of-month month year hour minute seconds timezone)
(defun parse-rfc-850-and-822-time (string &optional (start 0) (end (length string)))
   (declare (fixnum start end))
   (flet ((adjusted-time-zone-start (string start)
               (with-fast-array-references ((string string string))
                   (let ((char (aref string start)))
                      (if (or (eql char #\+)
                                 (alpha-char-p char))
                         start
                        (1- (the fixnum start)))))))
      (declare (inline adjusted-time-zone-start))
      (multiple-value-bind (start end)
                                      (http::string-trim-bounds '(#\space) string start end)
          (let* ((e1 (time-delimiter-position string  start  end))    ; weekday ignored but assumed here
                    (s2 (time-delimiter-position-if-not string e1 end))
                    (e2 (time-delimiter-position string  (1+ (the fixnum s2))  end))         ; day
                    (s3 (time-delimiter-position-if-not string e2 end))
                    (e3 (time-delimiter-position string  (1+ (the fixnum s3))  end))          ;  month
                    (s4 (time-delimiter-position-if-not string e3 end))
                    (e4 (time-delimiter-position string  (1+ (the fixnum s4))  end))          ;  year
                    (s5 (time-delimiter-position-if-not string e4 end))
                    (e5 (time-delimiter-position string  (1+ (the fixnum s5))  end))          ;  hour
                    (s6 (time-delimiter-position-if-not string e5 end))
                    (e6 (time-delimiter-position string  (1+ (the fixnum s6))  end))          ;  minute
                    (s7 (time-delimiter-position-if-not string e6 end))
                    (e7 (time-delimiter-position string  (1+ (the fixnum s7))  end))          ;  second
                    (s8 (and e7 (time-delimiter-position-if-not string e7 end)))
                    (e8 (and s8 (time-delimiter-position string  (1+ (the fixnum s8))  end))))          ; time zone
             (encode-universal-time (parse-integer string :start s7 :end e7)    ; second
                                                  (parse-integer string :start s6 :end e6)      ; minute
                                                  (parse-integer string :start s5 :end e5)      ; hour
                                                  (parse-integer string :start s2 :end e2)      ; day
                                                  (translated-month string  s3 e3)          ; month
                                                  (parse-integer string :start s4 :end (or e4 end))      ; year
                                                  (if s8        ; time zone 
                                                     (string-translated-timezone string (adjusted-time-zone-start string s8) (or e8 end))
                                                     0))))))

;; Parses ANSI C time format -- Mon May 10 05:00:10 1999 GMT
;; (x-weekday month day-of-month hour minute seconds year timezone)
(defun parse-ansi-c-time (string &optional (start 0) (end (length string)))
   (declare (fixnum start end))
   (flet ((adjusted-time-zone-start (string start)
               (with-fast-array-references ((string string string))
                   (let ((char (aref string start)))
                      (if (or (eql char #\+)
                                 (alpha-char-p char))
                         start
                         (1- (the fixnum start)))))))
      (declare (inline adjusted-time-zone-start))
      (multiple-value-bind (start end)
                                      (http::string-trim-bounds '(#\space) string start end)
          (let* ((e1 (time-delimiter-position string  start  end))    ; weekday ignored but assumed here
                    (s2 (time-delimiter-position-if-not string e1 end))
                    (e2 (time-delimiter-position string  (1+ (the fixnum s2))  end))          ; month
                    (s3 (time-delimiter-position-if-not string e2 end))
                    (e3 (time-delimiter-position string  (1+ (the fixnum s3))  end))          ; day
                    (s4 (time-delimiter-position-if-not string e3 end))
                    (e4 (time-delimiter-position string  (1+ (the fixnum s4))  end))          ; hour
                    (s5 (time-delimiter-position-if-not string e4 end))
                    (e5 (time-delimiter-position string  (1+ (the fixnum s5))  end))          ; minute
                    (s6 (time-delimiter-position-if-not string e5 end))
                    (e6 (time-delimiter-position string  (1+ (the fixnum s6))  end))          ; second
                    (s7 (time-delimiter-position-if-not string e6 end))
                    (e7 (time-delimiter-position string  (1+ (the fixnum s7))  end))          ; year
                    (s8 (and e7 (time-delimiter-position-if-not string e7 end)))
                    (e8 (and s8 (or (time-delimiter-position string  (1+ (the fixnum s8))  end) end))))          ; time zone
             (encode-universal-time (parse-integer string :start s6 :end e6)    ; second
                                                  (parse-integer string :start s5 :end e5)      ; minute
                                                  (parse-integer string :start s4 :end e4)      ; hour
                                                  (parse-integer string :start s3 :end e3)      ; day
                                                  (translated-month string  s2 e2)          ; month
                                                  (parse-integer string :start s7 :end (or e7 end))      ; year
                                                  (if s8        ; time zone 
                                                     (string-translated-timezone string (adjusted-time-zone-start string s8) (or e8 end))
                                                     0))))))

(declaim (inline ansi-c-time-string-p))

(define ansi-c-time-string-p (string &optional (start 0) (end (length string)))
   (declare (ignore end))
   (eql #\space (aref string (+ 3 start))))

(declaim (inline iso-time-string-p))

(define iso-time-string-p (string &optional (start 0)(end (length string)))
   (declare (ignore end))
   (digit-char-p (aref string start)))

(declaim (inline parse-http-time-string))

(defun parse-http-time-string (string &optional (start 0) (end (length string)))
  "This Returns a universal time when a string containing a time in RFC 850, 822
    or ANSI-C format."
  (cond
    ((ansi-c-time-string-p string start end)
     (parse-ansi-c-time string start end))
    ((iso-time-string-p string start end)
      (parse-iso-time string start end))
    (t (parse-rfc-850-and-822-time string start end))))

;; This will not do.  We have to be able to parse timestrings into universal time. -- JCMa 12/30/1994.
(define parse-gmt-time (string &optional (start 0) (end (length string)))
       (parse-http-time-string string start end))

(define parse-universal-time (string &optional (start 0) (end (length string)) futurep
                                                               base-time must-have-time date-must-have-year
                                                               time-must-have-second day-must-be-valid)
   (declare (ignore futurep base-time must-have-time date-must-have-year
                            time-must-have-second day-must-be-valid))
   (parse-http-time-string string start end)) 

;;;------------------------------------------------------------------- 
;;;
;;; WRITING TIME INTERVALS 
;;;

(defun decode-time-interval (interval)
   (declare (values seconds minutes hours days weeks))
   (if (< interval 60)
      (values interval 0 0 0 0)
      (multiple-value-bind (raw-minutes seconds)
                                      (truncate interval 60)
          (multiple-value-bind (raw-hours minutes)
                                          (truncate raw-minutes 60.)
              (if (zerop raw-hours)
                 (values seconds minutes 0 0 0)
                 (multiple-value-bind (raw-days hours)
                                                 (truncate raw-hours 24.)
                     (if (zerop raw-days)
                        (values seconds minutes hours 0 0)
                        (multiple-value-bind (weeks days)
                                                        (truncate raw-days 7.)
                            (if (zerop weeks)
                               (values seconds minutes hours days 0)
                               (values seconds minutes hours days weeks))))))))))

(defun %write-time-interval (interval &optional (stream *standard-output*))
   (flet ((write-component (value descriptor previous stream)
                (when (< 0 value)
                    (unless (zerop previous)
                       (write-char #\space stream))
                    (prin1 value stream)
                    (write-char #\space stream)
                    (write-string descriptor stream)
                    (when (< 1 value)
                        (write-char #\s stream)))))
      (multiple-value-bind (seconds minutes hours days weeks)
                                      (decode-time-interval interval)
          (write-component weeks "week"  0 stream)
          (write-component days "day" weeks stream)
          (write-component hours "hour"days stream)
          (write-component minutes "minute" hours stream)
          (write-component seconds "second" minutes stream))
      interval))

(defun write-time-interval (interval &optional (stream *standard-output*))
   "Writes an English description of the time interval INTERVAL to stream.
INTERVAL is in seconds. If stream is NIL, a string containing the description."
   (if stream
      (%write-time-interval interval stream)
      (with-output-to-string (string)
          (%write-time-interval interval stream))))

(export '(write-time-interval) :www-utils) 

;;;------------------------------------------------------------------- 
;;;
;;; 
;;;
(declaim (inline write-positive-fixnum))

;; Ports may choose to optimize this.   1/27/99 -- JCMa.
#-(OR Genera MCL)
(define write-positive-fixnum (fixnum &optional (base 10.) (stream *standard-output*))
  (write fixnum :stream stream :base base :escape nil))

#+MCL
(define write-positive-fixnum (integer &optional (base 10.) (stream *standard-output*))
   (check-type integer integer)
   (ccl::%pr-integer integer base stream)
   integer)

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
  (loop initially (format stream "~&Initializations on ~S~2%" list-name)
	for form in (symbol-value list-name)
	for count upfrom 1
	do (destructuring-bind (name s-exp done-p &rest keywords) form
	     (format stream "~&~D  ~A (~:[pending~;done~]) [~{~A~^, ~}] ~S" count name done-p keywords s-exp))))

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

#-(or Genera LispWorks MCL)
(defmacro with-timeout ((timeout &key error-p) &body body)
  "Executes BODY and returns the values of the last form in BODY. However, if
the execution takes longer than TIMEOUT seconds, abort it. If :ERROR-P is
unsupplied or false, just return nil. If :ERROR-P is true, signal an error."
  `(progn ,timeout ,error-p . ,body))

#-(or Genera LispWorks MCL)
(export 'with-timeout :www-utils)
