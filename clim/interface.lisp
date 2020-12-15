;;;   -*- Mode: LISP; Package: HTTP-INTERFACE; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-

;;;
;;; (c) Copyright  1994-1995, Massachusetts Institute of Technology and John C. Mallery
;;;     Written by Benjamin Renaud based on earlier code by John C.Mallery
;;;     Cleaned up   7/27/95 -- JCMa
;;;     More clean up   9/29/95 -- JCMa. 

;;;------------------------------------------------------------------- 
;;;
;;; A CLIM INTERFACE TO THE CL HTTP SERVER
;;;

(in-package :http-interface)

#+Genera
(format:defformat present-clim (:multi-arg) (arguments parameters)
  (declare (ignore parameters))
  (let* ((object (first arguments))
         (type (if format:atsign-flag (second arguments) (type-of object))))
    (clim:present object type :stream format:*format-output* :single-box (when format:colon-flag :highlighting))
    (if format:atsign-flag
        (rest (rest arguments))
        (rest arguments))))

(defclass frame-utility-mixin (standard-application-frame) ())

;; Specialize READ-FRAME-COMMAND to use keystrokes.
(defmethod read-frame-command ((frame frame-utility-mixin) &key)
  (let ((command-table (frame-command-table frame)))
    (with-command-table-keystrokes (keystrokes command-table)
      (read-command-using-keystrokes command-table keystrokes))))

(define-application-frame cl-http
                          (frame-utility-mixin)
  ((current-log :initform nil :initarg :current-log :accessor cl-http-current-log))
  (:top-level (cl-http-top-level))
  (:command-table (cl-http-command-table :inherit-from (accept-values-pane)))
  (:menu-bar nil)
  (:panes
    (display-pane :application
                  :display-after-commands nil
                  :scroll-bars :both 
                  :end-of-line-action :allow
                  :end-of-page-action :allow
                  :borders (:thickness 2)
                  :incremental-redisplay t)
    (title-pane :title :display-function 'title-displayer :height 20)
    (command-pane :command-menu :display-function 'menu-displayer
                  :incremental-redisplay t :height 50)
    (interactor-pane :interactor :scroll-bars :vertical
                     :incremental-redisplay t))
  (:layouts
    (standard
      (vertically ()
                  (2/3 display-pane)
                  title-pane
                  (1/20 command-pane)
                  (:fill interactor-pane)))))

#+Genera
(define-genera-application cl-http
  :pretty-name "Common Lisp HTTP Server" 
  :select-key #\H :width 1100. :height 850)

(defmethod cl-http-top-level ((self cl-http))
  (unwind-protect 
      (initialize-cl-http self)
    (default-frame-top-level self :prompt "CL-HTTP>")))


(defmethod menu-displayer ((frame cl-http) stream &key max-width max-height)
  (display-command-menu frame stream
                        :n-columns 3
                        :max-height max-height
                        :max-width max-width
                        :row-wise t))

(defmethod title-displayer ((frame cl-http) stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((log (cl-http-current-log frame)))
    (with-text-style (stream `(:serif :bold :large))
      (format stream "~33THTTP Server Interface"))
    (with-text-style (stream `(:serif :bold :normal))
      (format stream " - ~3TLog: ")
      (present log 'http-log :stream stream))))

(defmethod read-frame-command ((frame cl-http) &key)
  (let ((command-table (frame-command-table frame)))
    (with-command-table-keystrokes (keystrokes command-table)
      (read-command-using-keystrokes command-table keystrokes))))

(defmethod initialize-cl-http ((self cl-http)) ())

(defun cl-http-frame ()
  (typecase *application-frame*
    (cl-http *application-frame*)
    (t nil)))

(defgeneric display-pane (interface)            
  (:documentation "Returns the Display-pane.")) 

(defmethod display-pane ((self cl-http))
  (get-frame-pane self 'display-pane))

(defgeneric title-pane (interface)              
  (:documentation "Returns the Title-pane."))   

(defmethod title-pane ((self cl-http))
  (get-frame-pane self 'title-pane))

;; name collision with feature vector editor
;(defgeneric interactor-pane (interface)                
;  (:documentation "Returns the Interactor-pane."))     

(defmethod interactor-pane ((self cl-http))
  (get-frame-pane self 'interactor-pane))


;;;------------------------------------------------------------------- 
;;;
;;; Variables
;;;

(defvar *default-log-file* "http:log;log.text")


;;;------------------------------------------------------------------- 
;;;
;;; Presentation Types
;;;

(define-presentation-type http-log
                          ()
  :description "HTTP log"
  :parameters-are-types t)

(define-presentation-method present (object (type http-log) stream
                                            (view textual-view) &key)
  (with-slots (name n-transactions) object
    (etypecase object
      (null (write-string "No Log" stream))
      (http-log (format stream "~A [~A transactions]"  name n-transactions)))))

(define-presentation-method accept ((type http-log) stream (view textual-view) &key)
  (accept `(member ,@*all-logs*) :stream stream :prompt nil))

 
(define-presentation-type transaction
                          ()
  :description "transaction")

(define-presentation-method present (object (type transaction) stream
                                            (view textual-view) &key)
  (with-slots (host user time-stamp method url server-version status bytes) object
    (typecase object
      (transaction
        (let ((user-name (and user (user-name user)))
              (user-realm-name (and user (realm-name (user-realm user)))))
          (fresh-line stream)
          (format stream "~\\time\\ ~A~&~2T~A ~A~
                          ~&~2T~v[~:[~;User: ~:*~A ~]~:[~;Realm: ~:*~A ~]~
                           Status: ~A Total Bytes: ~A Server Version: ~a]~"
                  time-stamp  (host-name host)
                  method url `(:fix :roman :small) user-name user-realm-name status bytes server-version)))
      (t (write-string "No Transaction")))))

;;;------------------------------------------------------------------- 
;;;
;;; Support Functions
;;;


#+Genera
(defmethod allocate-host ((host-name-space host-name-space) (host neti:host) ip-address domain-name &key (class 'host))
  ip-address  domain-name class                 ;ignore
  (make-instance class
;                :ip-address (host-ip-address host)
                 :domain-name (host-mail-name host)
                 :object host))

;#+Genera
;(defmethod register-host ((host neti:host) (host-name-space host-name-space))
;  (register-host (allocate-host host-name-space host nil nil) host-name-space))


(defmethod url-transactions ((log http-log) (url url))
  (with-slots (url-table) log
    (values (gethash url url-table) url)))

(defmethod url-transactions ((log http-log) (url-string string))
  (multiple-value-bind (url)
      (url:intern-url url-string :if-does-not-exist :soft)
    (when url
      (url-transactions log url))))

(defmethod client-host-name ((transaction transaction))
  (with-slots (host) transaction
    (host-name host)))

(defgeneric show-url-transactions (log url-string &key stream))

(defmethod show-url-transactions ((log http-log) (url-string string) &key (stream *standard-output*))
  (multiple-value-bind (transactions url)
      (url-transactions log url-string)
    (print transactions stream)
    (loop for (method n . trns) in transactions
          do (format stream "~&~D ~A transaction~P on ~A" n method n (name-string url))
             (when (y-or-n-p "Describe them? ")
               (loop for tr in trns ;;(sort trns #'scl:alphalessp :key #'client-host-name)
                     do (describe tr))))))

(define set-log (log)
  (let ((log (or log (first (current-access-logs)))))
    (setf (cl-http-current-log (cl-http-frame)) log)
    (setq *default-log-file* (or (log-filename log) *default-log-file*))
    (update-title-pane)))

(define update-title-pane ()
  (let ((stream (title-pane (cl-http-frame))))
    (window-clear stream)
    (title-displayer (cl-http-frame) stream)))

(defun window-stream-to-application-frame (window)
  "Returns the application frame controlling window."
  (declare (inline))
  (let ((toplevel (window-top-level-window window)))
    (pane-frame toplevel)))

(defun window-fresh-viewport (window)
  "Positions the viewport over fresh window space."
  (fresh-line window)                           ;must be on the right line
  (multiple-value-bind (x y)
      (stream-cursor-position window)
    (declare (ignore x))
    (window-set-viewport-position window 0 y)
    (note-viewport-position-changed
      (window-stream-to-application-frame window) window 0 y)))

#+Genera
(define find-real-stream (stream)
   "Recurses down syn-stream indirections to find real stream."
   (cond ((and (symbolp stream)
                     (search "SYN-STREAM" (symbol-name stream) :test #'char-equal :from-end t))
              (find-real-stream (cdr (scl:locf (scl:symbol-function stream)))))
            (t stream)))

#-Genera
(define find-real-stream (stream)
   "Recurses down syn-stream indirections to find real stream."
   (cond ((symbolp stream)
              (find-real-stream  (symbol-value stream)))
            (t stream)))

(declaim (inline clim-window-p))

(define clim-window-p (stream)
  "Returns non-null of stream points at a CLIM window."
  (typep (find-real-stream stream) 'clim:sheet))

(define-macro with-fresh-viewport ((stream) &body body)
  "Ensures that output from body starts on a fresh viewport on stream."
  `(let ((window (find-real-stream ,stream)))
     (when (clim-window-p window)               
       (window-fresh-viewport window))
     ,@body))



;;;------------------------------------------------------------------- 
;;;
;;; COMMANDS
;;;

(define-cl-http-command
  (com-fresh-viewport :name t
                      :keystroke (:l :control))
  ()
  (window-fresh-viewport (interactor-pane (cl-http-frame))))

(define-cl-http-command
  (com-clear-output-history
    :name t)
  ()
  (window-clear (frame-standard-output *application-frame*)))

(define-cl-http-command
  (com-clear-input-history
    :name t)
  ()
  (window-clear (frame-standard-input *application-frame*)))

(define-cl-http-command (com-update-title :menu nil :name t)
    ()
  (let ((stream (title-pane (cl-http-frame))))
    (window-clear stream)
    (title-displayer (cl-http-frame) stream)))

(define-cl-http-command 
  (com-parse-log-file 
    :menu t
    :name t)
    ((pathname 'pathname :prompt "Log File" :default *default-log-file*)
     &key
     (port 'integer :prompt "HTTP Port" :default *standard-http-port*)
     (format `(member ,@(mapcar #'car *log-line-parser-alist*)) :prompt "Log Format" :default :common-log-format)
     (url-number 'integer :default 5000))
  (let* ((host (pathname-host (translated-pathname pathname)))
         (http::*default-log-size* url-number)
         (log (intern-access-log (host-mail-name host) :port port :if-does-not-exist :create)))
    (with-fresh-viewport (*standard-output*)
      (parse-log-file log pathname :log-format format))))

(define-cl-http-command 
   (com-show-url-transactions
     :menu t
     :name t)
   ((url 'string))
   (let ((log (cl-http-current-log (cl-http-frame))))
      (show-url-transactions log url)))

(define-cl-http-command (com-set-log :menu t :name t)
  ((log 'http-log :default (car *all-logs*) :display-default t :documentation "A log object" :prompt "Log"
        :default-type 'http-log))
  (set-log log))

(define-cl-http-command 
  (com-edit-log
    :menu t
    :name t)
  ((log 'http-log :default (car *all-logs*) :display-default t :documentation "A log object" :prompt "Log"
        :default-type 'http-log))
  (edit-log log (display-pane (cl-http-frame))))

(define edit-log (log stream &aux aborted)
  (with-slots (name n-transactions url-table filename
                          notification file-logging dynamic-logging) log
    (let ((name name) (filename filename) (notification notification) 
          (file-logging file-logging) (dynamic-logging dynamic-logging))
      (with-fresh-viewport (stream)
        (multiple-value-bind (n-name n-filename n-notification n-file-logging n-dynamic-logging)
            (restart-case
              (accepting-values (stream)
                (with-text-style (stream `(:serif :bold :large))
                  (write-string "Log Editor" stream))
                (terpri stream)
                (setq name (accept 'string :prompt "Name" :default name))
                (terpri stream)
                (write-char #\space stream)
                (write n-transactions :stream stream :base 10.)
                (write-string " Transactions" stream)
                (terpri stream)
                (setq filename (accept 'pathname :prompt " Filename" :default filename))
                (terpri stream)
                (setq notification (accept `(member :dual :tv :interface) :stream stream :default 
                                           notification
                                           :prompt " Notification"))
                (terpri stream)
                (setq file-logging (accept 'boolean :stream stream :prompt " File Logging" 
                                           :default file-logging))
                (terpri stream)
                (setq dynamic-logging (accept 'boolean :stream stream :prompt " Dynamic Logging"
                                              :default dynamic-logging))
                (values name filename notification file-logging dynamic-logging))
              (abort () (setq aborted t)))
          (unless aborted
            (setf (log-name log) n-name)
            (setf (log-filename log) n-filename)
            (setf (log-file-logging log) n-file-logging)
            (setf (log-dynamic-logging log) n-dynamic-logging)
            (setf (log-notification log) n-notification)))))))

(defmethod show-log-transactions ((log http-log) stream)
  (with-fresh-viewport (stream)
    (with-slots (url-table) log
      (maphash #'(lambda (key datum)
                   (declare (ignore key))
                   (present (third (car datum)) 'transaction :stream stream))
               url-table))))

(define-cl-http-command 
  (com-show-log-transactions
    :menu t
    :name t)
  ((log 'http-log :default (car *all-logs*) :display-default t :documentation "A log object" :prompt "Log"
        :default-type 'http-log))
  (show-log-transactions log (display-pane (cl-http-frame))))

(define-cl-http-command (com-launch-demo :menu t :name t) ()
  (http:launch-demo)
  (set-log (first (current-access-logs)))
  (com-update-title))
