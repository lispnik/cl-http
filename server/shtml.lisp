;;;   -*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:fix :roman :normal);-*-

;;;
;;; (c) Copyright  1997, John C. Mallery
;;;     All Rights Reserved.
;;;

(in-package :http)


;;;------------------------------------------------------------------- 
;;;
;;; SERVER SIDE HTML
;;;
;;; the Netscape SHTML is documented at the bottom of:
;;; http://developer.netscape.com/library/documentation/enterprise/unix/cgibas.htm

(defvar *shtml-action-table* (make-hash-table :test #'equal)
  "A table mapping strings to functions defined for server-parsed HTML.")

(defvar *shtml-actions* nil
  "A lis actions defined for server-parsed HTML.")

(defun %shtml-action-name-string (action &optional (start 0 start-supplied-p) (end nil end-supplied-p) mung-string-p)
  (etypecase action
    (string
      (cond ((or start-supplied-p end-supplied-p)
             (nstring-downcase (subseq action start end)))
            (mung-string-p (nstring-downcase action))
            (t (string-downcase action))))
    (symbol (string-downcase (symbol-name action)))))

(eval-when (:compile-toplevel :execute :load-toplevel)
(defconstant +shtml-function-prefix+ "SHTML-ACTION-")

(defun shtml-function-name (symbol)
  (declare (values shtml-function-name))
  (intern (concatenate 'string +shtml-function-prefix+ (symbol-name symbol)) (symbol-package symbol)))
)                                       ; close eval-when

(defun shtml-action-name (shtml-function-name)
  (declare (values shtml-action-string))
  (%shtml-action-name-string (symbol-name shtml-function-name) #.(length +shtml-function-prefix+)))

(define register-shtml-action (shtml-function)
  "Registers SHTML-FUNCTION for server-parsed HTML.
SHTML-FUNCTION must be an fbound symbol."
  (check-type shtml-function (and symbol (satisfies fboundp)))
  (let ((action (shtml-action-name shtml-function)))
  (setf (gethash action *shtml-action-table*) shtml-function)
  (push-ordered shtml-function *shtml-actions* #'string<)
  action))

(define unregister-shtml-action (shtml-function)
  "Unregisters SHTML-FUNCTION for server-parsed HTML, making it no longer accessible from SHTML."
  (check-type shtml-function symbol)
  (let ((action (shtml-action-name shtml-function)))
    (declare (dynamic-extent action))
    (remhash (%shtml-action-name-string action) *shtml-action-table*)
    (setq *shtml-actions* (delete shtml-function *shtml-actions*))))

(define unregisters-all-shtml-actions ()
  "Unregisters all functions implementing server-parsed HTML actions."
  (clrhash *shtml-action-table*)
  (setq *shtml-actions* nil))

(define validate-shtml-action (string &optional (start 0) end)
  "Returns the executable function if string denotes a valid server-parsed HTML action."
  (let ((action (%shtml-action-name-string string start end t)))
    (declare (dynamic-extent action))
    (or (gethash action *shtml-action-table*) (error "Undefined SHTML action, ~S." (subseq action start end)))))

(define show-shtml-actions (&key (stream *standard-output*))
  "Describes the defined SHTML actions on stream."
  (loop for action in *shtml-actions*
        do (format stream "~&~S ~S: ~&~5TSHTML Tag: ~A~&~5T~:[Undocumented~;~:*~D~]"
                   action (arglist action) (shtml-action-name action) (documentation action 'function))))

(defmacro with-shtml-parameters ((parameters plist) &body body)
  (loop for var in parameters
        for keyword = (symbolize (symbol-name var) *keyword-package*)
        collect `(,var (getf ,plist ,keyword)) into bindings
        finally (return `(let ,bindings . ,body))))

(define-macro define-shtml-action (name parameters (&key (output-mode :text) documentation) &body body)
  "Defines an action named NAME for server-parsed HTML.
Actions are inserted into static HTML using the tag <!--#execute action=name -->.
PARAMETERS is a list of the SHTML parameters for the action. These are automatically
bound to the parameter string present in the HTML or NIL. When a parameter is an
integer, it is automatically converted to an integer. To avoid security
holes, extreme care should be exercised when converting these values to Lisp objects.
The current URL and the HTTP stream to the client are bound within BODY.
BODY may emit HTML on STREAM and access URL or HTTP:*SERVER*.
The STREAM output mode within BODY is controlled by OUTPUT-MODE,
which can be :TEXT, :BINARY, :MANUAL."
  (let ((fname (shtml-function-name name))
        (url-sym (intern "URL" *package*))
        (stream-sym (intern "STREAM" *package*))
        (body (ecase output-mode
                (:text `(www-utils::with-text-stream (stream :output) . ,body))
                (:binary `(with-binary-stream (stream :output) . ,body))
                (:manual `(progn . body)))))
    `(progn
       (defun ,fname (,url-sym ,stream-sym parameter-plist)
         #+Genera(declare (sys:function-parent ,fname define-shtml-action))
         ,documentation
         ,url-sym parameter-plist               ;ignore
         ,(if parameters
              `(with-shtml-parameters (,parameters parameter-plist)
                 ,body)
              body))
       (register-shtml-action ',fname))))

;; Tokenize actions for faster lookup
(define-header-keywords "execute" "include")

(define-generic get-shtml-operation (method parameter-plist)
  (declare (values shtml-function parameter-plist))
  (:documentation "Returns an server-parsed HTML function called at a certain byte offset.
parameter-plist is a parameter-plist of the parameter values in the SHTML element.
Specialize this method on METHOD to add new server-parsed HTML tags.
This returns a function that is called with (URL STREAM &optional parameter-plist)
where parameter-plist is an parameter-plist of parameter values passed in from the SHTML."))

(defmethod get-shtml-operation (method parameter-plist)
  (error "Unknown SHTML method, ~S with arguments ~S" method parameter-plist))

(defmethod get-shtml-operation ((method (eql :eval)) parameter-plist)
  (let ((action (getf parameter-plist :action)))
    (cond (action
           (values (validate-shtml-action action 0 (length action))
                   parameter-plist))
          (t (error "SHTML ~S element contains no action, ~S." parameter-plist)))))

(defmethod get-shtml-operation ((method (eql :include)) parameter-plist)
  (values '%shtml-include-file parameter-plist))

(eval-when (:compile-toplevel :execute :load-toplevel)
(defconstant *shtml-tag-open* "<!--#")
(defconstant *shtml-tag-close* "-->"))

(define parse-shtml-element (element &optional (start 0) (end (length element)))
  "Parses a full SHTML element and returns a funcallable function or errors."
  (declare (values function parameter-plist)
           (fixnum end))
  (let* ((end1 (- end #.(length *shtml-tag-close*)))
         (pos (position #\# element :start start :end end1))
         (start1 (1+ (the fixnum pos)))
         (pos1 (and pos (position-if #'white-space-char-p element :start start1 :end end1)))
         (method (and pos1 (%tokenize-header-keyword element start1 pos1)))
         (plist (parse-equal-sign-delimited-pairs element (1+ (the fixnum pos1)) end1 #\space t)))
    (get-shtml-operation method plist)))

(define-generic parse-shtml-template (pathname)
  (declare (values template-parameters))
  (:documentation "Parses the contents of PATHNAME and returns TEMPLATE-PARAMETERS.
TEMPLATE-PARAMETERS is a list of (START-INDEX END-INDEX FUNCTION . PARAMETER-PLIST).
START-INDEX is a byte-offset from which binary copying starts.
END-OFFSET is a byte-offset at which binary copying stops.
The byte at START-INDEX is included. The byte at END-OFFSET is excluded.
FUNCTION and PARAMETER-plist are the values returned by calling 
PARSE-SHTML-ELEMENT on a string containing an SHTML tag with indices including
the entire element. 

Each platform should specialize this method on pathname. The byte offsets
must refer to the actual bytes shipped by the server. Thus, platforms performing
CRLF translation must define this operation to use the CRLF version of PATHNAME."))

(defmethod http:parse-shtml-template (pathname)
  (declare (ignore pathname))
  (error "Not Implemented: Each platform must define this operation."))

(defmethod parse-shtml-template ((pathname string))
  (parse-shtml-template (pathname pathname)))

;; Ports need to specialize PARSE-STML-TEMPLATE.. The API is documented on the
;; generic function.  Beware of fencepost cases.  Lispm version is in
;; hHTTP:LISPM;SERVER;LISPM.LISP. Below is a reference implementation by
;; Martin Simmons intended to illustrate how the method is used not to be
;; efficient.  CHAR-TO-ASCII to and ASCII-TO-CHAR are the only non-portable
;; parts.

#-Genera
(defmethod parse-shtml-template (pathname)
  (declare (values template-parameters))
  (labels ((char-to-ascii (char) (char-code char))      ;UNIX, Win32
           (ascii-to-char (byte) (code-char byte))      ;UNIX, Win32
           (string-to-8-bit-vector (string)
             (map '(simple-array (unsigned-byte 8) 1) #'char-to-ascii string)))
    (declare (inline char-to-ascii ascii-to-char))
    (let* ((+shtml-tag-start+ (string-to-8-bit-vector "<!--#"))
           (+shtml-tag-start-length+ (length +shtml-tag-start+))
           (+shtml-tag-end+ (string-to-8-bit-vector "-->"))
           (+shtml-tag-end-length+ (length +shtml-tag-end+)))
      (with-open-file (file-stream pathname :direction :input :element-type '(unsigned-byte 8))
        (let* ((length (file-length file-stream))
               (data (make-array length :element-type '(unsigned-byte 8)))
               (positions (make-array length)))
          (dotimes (index length)
            (setf (svref positions index) (file-position file-stream)
                  (aref data index) (read-byte file-stream)))
          (let ((end-position (file-position file-stream))
                (read-start 0)
                (template-parameters '()))
            (loop (let ((tag-start (search +shtml-tag-start+ data :start2 read-start)))
                    (unless tag-start
                      (let ((read-end end-position))
                        (unless (eql read-start read-end)
                          (push (list read-start read-end) template-parameters))
                        (return)))
                    (let ((tag-end (search +shtml-tag-end+ data :start2 (+ tag-start +shtml-tag-start-length+))))
                      (unless tag-end
                        (error "Unbalanced element at byte ~D" read-start))
                      (incf tag-end +shtml-tag-end-length+)
                      (let* ((string-length (- tag-end tag-start))
                             (string (make-string string-length)))
                        (dotimes (string-index string-length)
                          (let ((char (ascii-to-char (aref data (+ tag-start string-index)))))
                            (setf (schar string string-index)
                                  (if (member char '(#\Return #\Newline))
                                      #\Space
                                      char))))
			(multiple-value-bind (function parameter-plist)
			    (http::parse-shtml-element string)
			  (push (list* read-start (aref positions tag-start)
				       function parameter-plist)
				template-parameters)))
                      (setq read-start tag-end))))
            (nreverse template-parameters)))))))


;;;------------------------------------------------------------------- 
;;;
;;; SHTML ACTIONS
;;;

#+testing
(define-shtml-action date-time (color size)
                     (:output-mode :text :documentation "Writes the current ISO date and time.")
  (with-font (:color (or color :red) :size (or size 3) :stream stream)
    (write-standard-time (get-universal-time) stream)))

(define-shtml-action date-time (format)
                     (:output-mode :text :documentation "Writes the current date and time according to FORMAT,
which can be ISO, Local, or GMT.")
  (let ((ut (get-universal-time)))
    (ecase (if format (%tokenize-header-keyword format) :iso)
      (:iso (write-standard-time ut stream))
      (:gmt (write-time ut stream 0))
      (:local (print-date-time :universal-time ut :stream stream)))))

(define-shtml-action number-of-visitors ()
                     (:output-mode :text :documentation "Writes the number of visitors to the current Web page.")
  (flet ((get-visit-count (counter)
           (let ((count (1+ (handler-case
                              (with-open-file
                                (file counter :direction :input :element-type *standard-character-type*)
                                (read file nil 0))
                              (file-error () 0)))))
             (with-open-file (file counter :direction :output :element-type *standard-character-type*
                                   :if-does-not-exist :create :if-exists :overwrite)
               (write count :base 10 :stream file))
             count)))
    (let* ((path (translated-pathname url))
           (counter (make-pathname :type "counter" :defaults path)))
      (write (get-visit-count counter) :base 10 :stream stream))))

(defun %shtml-insert-file (file url stream)
  (let* ((default (translated-pathname url))
         (path (pathname file))
         (pathname (make-pathname :name (pathname-name path) :type (pathname-type path) :version :newest :defaults default)))
    (with-open-file (file pathname :direction :input :if-does-not-exist :error)
      (stream-copy-until-eof file stream :text))))

(defun %shtml-include-file (url stream parameter-plist)
  (with-shtml-parameters ((file subnets) parameter-plist)
    (when (or (null subnets)
	      (ip-host-trusted-p (server-address *server*) (template-secure-subnets url file subnets)))
      (%shtml-insert-file file url stream))))

(define-shtml-action insert-file (file subnets)
                     (:output-mode :text :documentation "Inserts the contents of FILE, which must be a text/* file
in the same directory as the HTML file containing the tag.
SUBNETS is an optional comma separated string of IP addresses.")
  (when (or (null subnets)
	    (ip-host-trusted-p (server-address *server*) (template-secure-subnets url file subnets)))
    (%shtml-insert-file file url stream)))
