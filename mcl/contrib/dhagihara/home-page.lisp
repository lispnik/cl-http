;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: text; Package: common-lisp-user -*-
;;;
;;; Daitaro Hagihara July, 1996
;;; Science & Art, Inc.; Also: Research Consultant, NYMC
;;; Long Island, NY
;;; daiyanh@interramp.com
;;;
;;; RUNNING A CL-HTTP SERVER WITH DYNAMICALLY ALLOCATED IP ADDRESSES
;;;
;;; 
;;; Applicable to:  For those accessing the Internet through commercial service
;;; providers and planning to setup a server in one's own home using CL-HTTP on a
;;; Mac, my code will provide with some basic stuff to get started.
;;; 
;;; Background:  Commercial ISP's will most likely offer only dynamic IP addresses
;;; to their customers through any foreseeable future (though situations seem to
;;; be changing somewhat).  But servers need fixed IP addresses, such as host
;;; domain names, to be really usable.  With recent popularity of
;;; videoconferencing using CU-SeeMe, this situation is a serious drawback.
;;; 
;;; My solution:  The only fixed things I get are e-mail address and personal web
;;; page URL.  I can take advantage of the latter by publishing a new web page
;;; containing a newly assigned IP address for the session of each login, since
;;; throughout this session my IP address is fixed.  So, any visitor will first
;;; connect to my personal web page; and if he chooses to do so, thereafter he can
;;; click the URL to the aforementioned IP address to transfer to CL-HTTP server
;;; at my home.
;;; 
;;; My code:  There are few things I must say about it.  First it's copyright-free
;;; except those portions indicated otherwise.  Second it's not pretty.  Third it
;;; works; but it's meant to be a working example, not a finished app.  Fourth it
;;; lacks much of error handling: if anything goes wrong, you are on your own.
;;; Finally you may have to adjust (in the order indicated) host address,
;;; directory references on the host, file name of the web page, whereabouts of
;;; home/web page supporting files, the html content, and possibly but not likely
;;; some command sequencing.  Oh and the computed home page code needs more work.
;;; With these in mind, the following are very condensed technical details:
;;; 
;;; 1) Changes made to CL-HTTP
;;;      xmactcp.lisp - SysEnvirons stopped working!
;;;      html2.lisp - simply added :LEFT option for <IMG...> command.
;;; 
;;; 2) There is a problem with MacTCP regarding IP address caching.
;;;      NS2.0 must be run at or before the point as indicated in the source.
;;; 3) Real hacks still to do
;;;      automated launching of CU-SeeMe session (like AT&T video phone)
;;;      private IRC right on the web page w/o any plug-ins or support apps
;;; 
;;; 
;;;------------------------------------------------------------------- 
;;;
;;; LAUNCH MAC CL-HTTP WITH INTERNET DEMO CONFIGURATION
;;;

;; If you load this file, you can access the URL
;; http://your.host.domain.name/ and peruse the server documentation,
;; assuming a network connection.

(in-package :cl-user)

;; Make compilation switches are set to preserve function inforation.
(setq ccl:*fasl-save-local-symbols* t
      ccl:*fasl-save-doc-strings* t
      ccl:*fasl-save-definitions* t)

;; Load the server.
#-:MAC-CL-HTTP
(load (merge-pathnames "mac-sysdcl"  ccl:*loading-file-source-file*) :verbose t)
(load (merge-pathnames "mac-sysdcl-client"  ccl:*loading-file-source-file*) :verbose t)

;; Load the init file to start the CL-HTTP demo.
;(load "http:mac;examples;init-server-internet.lisp" :verbose t) -> pushed to the end (daiyan)

;; start http services
;(http:enable-http-service) -> pushed to the end (daiyan)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Copyright © 1991 Apple Computer, Inc.
;
; Stream parsing engine for use by Telnet, POP mail and others.
;

; 11/05/91 derek        Initial coding

#|----------------- Theory of operation ----------------

 Parse-stream is called on an open stream, and is given a parse table. 
 If a sequence of characters on the stream matches an entry in the parse table,
 the corrosponding function in the table is called.  The function is passed
 the stream and a buffer containing the sequence of matching characters.
 Parse-stream will return whatever the function in the parse-table
 returns.  If there is no input on the stream, parse-stream will return
 nil.

 Using parse-stream consists of programming a parse table with sequences
 of characters to look for followed by an action function to call when the sequence
 is found.  Sequences may contain "wild-cards" and other stuff.

 ACTION-SEQUENCE = (list SEQUENCE-ITEM* ACTION-FUNCTION)
 SEQUENCE-ITEM =
                character                incoming character must match this exacly
                | t                      matches any one character
                | string                 incoming characters must match this exacly  
                | nil SEQUENCE-ITEM*     matches anthing until the following sequence is found

 Sequences and actions are added to a parse table using the add-action function,
 which expects a list of sequence information and the action function, as well as
 a parse table to add to:

 (add-action (list "-ERR" nil #\LF #'(lambda (tcp buf) (print "got it!") t)) *pop-table*)

 In this example, the sequence parse-stream is looking for is "-ERR" followed
 by any and all characters until a linefeed is found.  The action function is
 passed a stream and a buffer containing everything parse-stream has seen
 (including the "-ERR" and the linefeed).  Since the action function returns
 t, parse-stream will return t when this sequence is seen on the stream.

-- Lower level stuff: --
 Add-action translates the action-sequence into a tree, and will graft new 
 action trees onto the parse table in such a way that parse-stream only
 has to look for the current incoming character in the parse table.
 Given the following commands, these parse tables are produced:

 (setq *table* (add-action (list #\377 #\377 #'foo) nil))
 ==> ((#\377 (#\377 (#<Compiled-function Foo>))))

(add-action (list #\377 #\373 #\Home  #'foo) *table*)
 ==> ((#\377 (#\377 (#<Compiled-function Foo>)) (#\373 (#\Home (#<Compiled-function Foo>)))))

(add-action (list #\377 #\373 t #'Bar) *table*)  (indenting added)
 ==> ((#\377                                            ; If the 1st character is #\377, keep looking
       (#\377 (#<Compiled-function Foo>))               ; If the 2nd character is #\377, call Foo
       (#\373                                           ; If the 2nd character is #\373, keep looking
        (#\Home (#<Compiled-function Foo>))             ; If the 3rd character is #\Home, call Foo
        (T (#<Compiled-function Bar>)))))               ; Otherwise, call Bar
-----------------------------------------------------

USING PARSING-STREAM:

 parsing-stream is a abstract class that may be used in implementing intelligent filtering 
 streams.  It must be combined with a concrete input stream class to be usable.  In addition, the slot
 parse-table must be set.  stream-tyi (and read-char, etc.) will only return what the parse-table
 allows.

Example use of parsing-stream:

 (defclass telnet-stream (parsing-stream tcp-stream)            ; parsing-stream must appear before the concrete stream (tcp-stream).
  ()
  (:default-initargs :parse-table *telnet-stream-table*))       ; Set parse-table automatically.

|#

(in-package "CCL")

;(require "MACTCP") -> not needed (daiyan)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(parse-stream add-action parsing-stream)))

;--------- General Parser ---------

(defclass parsing-stream (input-stream)
  ((parse-buffer :type string
                 :accessor parse-buffer
                 :initarg :parse-buffer
                 :initform (make-array 512 :element-type 'character :fill-pointer 0))
   (parse-table :type list
                :accessor peruse-table
                :initarg :parse-table
                :initform nil)))

(defun test-table (a-char table-item)
  (declare (type character a-char) (optimize (safety 0) (speed 3)))
  (cond
   ((eq a-char table-item)      t)                              ; table matchs char
   ((eq t table-item)           t)                              ; table will match anything
   ((listp table-item)          (find a-char table-item))       ; table will match anything in the list
   (t                           nil)))

(defmethod stream-tyi ((stream parsing-stream))
  (when (listen stream)
    (let (a-char until-action last-action
                 (cur-action (peruse-table stream))
                 (buffer (parse-buffer stream)))
      (setf (fill-pointer buffer) 0)
      (loop
        (if (eq (length buffer) 0)
          (when (null (setq a-char (call-next-method stream)))
            (RETURN nil))
          (while (eq nil (setq a-char (call-next-method stream))))); if in the middle of a command sequence, can't be interupted
        
        (vector-push a-char buffer)
        (when (eq nil (caar cur-action))                        ; is it an until action?
          (setq cur-action (cdar cur-action))                   ; make an action based on until action
          (setq until-action cur-action))                       ; save this action - we may need it again
        
        (setq last-action cur-action)
        (setq cur-action (find a-char cur-action :key #'car :test #'test-table))        ; look for matching action based on input
        (cond
         ((null cur-action)
          (if until-action                                      ; store up characters until matching pattern found
            (progn
              (if (eq last-action until-action)                 ; If we were part way into the matching sequence...
                (setq cur-action until-action)                  ; Nop, reset action.
                
                (if (setq cur-action (find a-char UNTIL-ACTION :key #'car :test #'test-table)) ; ...maybe this byte matches the begining of until sequence 
                  (setq cur-action (cdr cur-action))            ; Oh, It was!  Keep going.
                  (setq cur-action until-action))))             ; Guess not...reset action.
            (progn
              (format t "Unexpected sequence. Input buffer = ~A.~%" buffer)
              (RETURN nil))))
         ((functionp (caadr cur-action))                        ; action is a function
          (RETURN (apply (caadr cur-action) (list stream buffer))))
         (t                                                     ; action is an action table
          (setq cur-action (cdr cur-action))))))))

(defun parse-stream (stream parse-table &optional (a-buffer nil))
  "Will return nil or a valid item from the stream. a-buffer must a vector with a fill pointer."
  (declare (optimize (safety 0) (speed 3)))
  (proclaim `(inline test-table))
  (let ((a-char (ccl:stream-tyi stream)) until-action last-action)
    (unless a-char
      (RETURN-FROM parse-stream nil))
    (if a-buffer
      (setf (fill-pointer a-buffer) 0)
      (setq a-buffer (make-array 512 :element-type 'character :fill-pointer 0)))
    (loop
      (vector-push a-char a-buffer)      
      (when (null (caar parse-table))                           ; is it an until action?
        (setq parse-table (cdar parse-table))                   ; make an action based on until action
        (setq until-action parse-table))                        ; save this action - we may need it again
      
      (setq last-action parse-table)
      (setq parse-table (find a-char parse-table :key #'car :test #'test-table))        ; look for matching action based on input
      (cond
       ((null parse-table)
        (if until-action                                        ; store up characters until matching pattern found
          (progn
            (if (eq last-action until-action)                   ; If we were part way into the matching sequence...
              (setq parse-table until-action)                   ; Nop, reset action.
              
              (if (setq parse-table (find a-char UNTIL-ACTION :key #'car :test #'test-table)) ; ...maybe this byte matches the begining of until sequence 
                (setq parse-table (cdr parse-table))            ; Oh, It was!  Keep going.
                (setq parse-table until-action))))              ; Guess not...reset action.
          (progn
            (format t "Unexpected sequence. Input buffer = ~A.~%" a-buffer)
            (RETURN nil))))
       ((functionp (caadr parse-table))                         ; action is a function
        (RETURN (apply (caadr parse-table) (list stream a-buffer))))
       (t                                                       ; action is an action table
        (setq parse-table (cdr parse-table))
        ))
      (while (eq nil (setq a-char (ccl:stream-tyi stream)))))))

;--------- Parse table maker functions (kind of flakey - don't enter the same sequence twice, always start from the top) ----------

(defun make-list-into-tree (l)
  (if (eq (length l) 2)
    (list (car l) (cdr l))
    (list (car l) (make-list-into-tree (cdr l)))))

(defun string-to-list (a-string)
  (if (stringp a-string)
    (coerce a-string 'list)
    (list a-string)))

(defun do-add-action (action action-table)
  (let ((sub-table (find (car action) action-table :test #'equal :key #'car)))
    (if (eq sub-table nil)
      (nconc action-table (list (make-list-into-tree action)))
      (do-add-action (cdr action) (cdr sub-table)))))

(defun add-action (action action-table)
  (do-add-action
   (mapcan #'string-to-list action)     ; replace strings with lists of chars
   action-table))

;------ example parse table ----

;(defvar *pop-table* nil)
;
;(setq *pop-table* (add-action (list "+OK" nil #\LF #'(lambda (tcp buf)         ; Remove linefeeds from the input buffer, and strip off the "+OK "
;                                                       (values t (subseq (delete #\LF buf) 4) )))
;                              nil))
;(add-action (list "-ERR" nil #\LF #'(lambda (tcp buf)
;                                      (values nil (subseq (delete #\LF buf) 5))))
;            *pop-table*)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;daiyan's ftp stuff - based on TCP Examples and smtp-pop-mail, both available from
;;                     digitool's user contributions

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(publish-web-page)))

(defclass ftp-stream (tcp-stream)
  ((parse-buffer :type array :accessor parse-buffer
                 :initform (make-array 10000 :element-type 'character :fill-pointer 0))))

(defvar *ftp-table* nil)        ; parse table for ordinary FTP reponses.

(defun get-ftp-response (stream &optional (parse-table *ftp-table*))
  "This will hang until valid ftp response is received."
  
  (let ((a-response '(nil)))
    (while (eq (length a-response) 1)
      (setq a-response (multiple-value-list (parse-stream stream parse-table (parse-buffer stream)))))
    (values-list a-response)))

(defun open-ftp-stream (host)
  "Returns an open stream (to an ftp server) or nil, and the servers response."
  
  (let* ((stream (make-instance 'ftp-stream :host host :port "ftp"))
         (success nil)
         (response))
    (multiple-value-setq (success response) (get-ftp-response stream))
    (if success
      (values stream response)
      (progn
        (stream-close stream)
        (values nil response)))))

(defun do-ftp-user (stream user)
  "Sends USER. Returns t or nil, 2nd value is ftp response."
  
  (telnet-write-line stream "USER ~D" user)
  (get-ftp-response stream))

(defun do-ftp-password (stream password)
  "Sends PASS. Returns t or nil, 2nd value is ftp response."
  
  (telnet-write-line stream "PASS ~D" password)
  (get-ftp-response stream))

(defun do-ftp-port (stream port)
  (let* ((addr (%tcp-getaddr))
         (addr-str (format nil "~D,~D,~D,~D"
                           (ldb (byte 8 24) addr)
                           (ldb (byte 8 16) addr)
                           (ldb (byte 8 8) addr)
                           (ldb (byte 8 0) addr)))
         (port-str (format nil "~D,~D"
                           (ldb (byte 8 8) port)
                           (ldb (byte 8 0) port))))
    (telnet-write-line stream "PORT ~D,~D" addr-str port-str)
    (get-ftp-response stream)))

(defun do-ftp-store (stream name)
  (telnet-write-line stream "STOR ~D" name)
  (get-ftp-response stream))

(defun do-ftp-quit (stream)
  "Sends QUIT. Returns t or nil, 2nd value is ftp response."
  
  (telnet-write-line stream "QUIT")
  (get-ftp-response stream))

(setq *ftp-table* (add-action (list t t t nil #\LF #'(lambda (tcp buf)          ; Remove linefeeds from the input buffer, and strip off the "+OK "
                                                       (declare (ignore tcp))
                                                       (values (parse-integer buf :end 3) (subseq buf 4 (- (length buf) 2)) )))
                              nil))

(defmacro with-ftp-stream (host user pass &body body)
  `(let ((stream nil) (response nil))
     (setq stream (open-ftp-stream ,host))
     (when stream
       (unwind-protect
         (progn
           ;--- logon stuff ---
           (setq response (do-ftp-user stream ,user))
           (setq response (do-ftp-password stream ,pass)))
         (when response
           ;--- messaging stuff
           ,@body)
         
         ;--- logout stuff
         (do-ftp-quit stream))
       (stream-close stream))
     ))

(defvar home-html
"<html>
<head>
<title>daiyan's Web Page</title>
</head>
<body>

~D

<p>This web page was created using <tt>HTML-Editor</tt> program running on
Macintosh Common Lisp version <tt>3.0kp2p2</tt>.  My current project as far as
web pages are concerned is for any visitor to be able to contact me
<strong>directly</strong> (please be discreet) through <em>CU-SeeMe</em> and
<em>URL</em> to a server program on my Macintosh SE/30 (Yes, I am an old-timer).

~D

<ol>
<li><a href=http://www.digitool.com>Macintosh Common Lisp</a>
<li>CU-SeeMe (I forgot their web address.)
<li><a href=http://www.ai.mit.edu/projects/iiip/doc/cl-http/home-page.html>
The Common Lisp Hypermedia Server (CL-HTTP)</a>
</ol>

<hr>

<address><pre>Daitaro Hagihara
(HI Science & Art, Inc.; Also: Research Consultant, NYMC)
Long Island, NY
daiyanh@interramp.com
</pre></address>

</body>
</html>")

(defvar home-html-orphan
"<p>But as you can see, a direct contact can take place only when I am logged in
<strong>and</strong> my server is running on my Mac.  Most of the time, neither
is the case.  For now, the following list will take you to relevant places for
the creation of this web page...")

(defvar home-html-contact
"<p>If you want to participate in my little experiment, then click the
<strong>open-door</strong> picture above to enter my server.  At this point, I
am fine-tuning the system and things probably aren't very interesting.  So, I
advise you to take a look at the following first, then take a hike:")

(defun publish-web-page (user-id password &key (name "home.html") (session :start) &aux (host "homepage.interramp.com"))
  "Publishes a personal web page."
  
  (with-ftp-stream host user-id password
    (let* ((port 666) (data-stream (open-tcp-stream nil port)) (addr (tcp-host-cname (%tcp-getaddr))))
      (when data-stream
        (do-ftp-port stream port)
        (do-ftp-store stream name)
        (case session
          (:start
           (telnet-write-line data-stream home-html (format nil "<a href=http://~D/home.html><img align=left src=open-door.jpeg></img><center><h1>Say hello</h1></center><center><h1>to daiyan!</h1></center></a>" (subseq addr 0 (1- (length addr)))) home-html-contact))
          (otherwise
           (telnet-write-line data-stream home-html "<img align=left src=closed-door.jpeg></img><center><h1>Welcome to</h1></center><center><h1>daiyan's web!</h1></center>" home-html-orphan)))
        (stream-close data-stream)
        (get-ftp-response stream))))
  (let ((my-url (make-instance 'url:http-path :host-string host :port 80 :path '("ir-bin" "irpwp-publish"))))
    (http:show-url my-url :stream nil :headers `(:authorization (:basic ,(base64:base64-encode-vector (concatenate 'string user-id ":" password) :characters-p t)))))
  (if (eq session :start) (pushnew #'(lambda () (publish-web-page user-id password :name name :session :end)) *lisp-cleanup-functions*)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;If using Netscape Navigator concurrently, it must be run at or before this point.


(in-package :cl-user)
(load "http:mac;examples;init-server-internet.lisp" :verbose t)
(http:enable-http-service)

(in-package :http-user)

(defmethod compute-home-page ((url url:http-computed-url) stream)
  (with-successful-response (stream :html :expires (url:expiration-universal-time url))
    (let (title (user-agent (http::remote-user-agent)))
      (setq title (format nil "<center>What's up,</center></h1><h1>~D"
                          (if (search "Macintosh" user-agent)
                            "<center>Mac friend!</center>"
                            "<center>'Net surfer!</center>")))
      (html:with-html-document (:stream stream)
        (html:with-document-preamble (:stream stream)
          (html:declare-base-reference url :stream stream)
          (html:declare-title "daiyan's Home Page" :stream stream))
        (html:with-document-body (:stream stream)
          (html:image "/intro.jpeg" "----" :alignment :left :stream stream)
          (html:with-section-heading (title :stream stream)
            (image-line :stream stream)
            (html:with-paragraph (:stream stream)
              (write-string "Run the headers test again? " stream)
              (html:note-anchor "Yes" :reference url :stream stream))
            (image-line :stream stream)
            (cl-http-signature stream)))))))

(export-url #u"/intro.jpeg"
            :jpeg-image
            :pathname (pathname "MacDisk:Desktop Folder:yosomite.jpeg")
            :expiration `(:interval ,(* 24. 60. 60.))
            :keywords '(:cl-http :demo))

(export-url #u"/home.html"
            :computed
            :response-function #'compute-home-page
            :expiration '(:no-expiration-header)
            :keywords '(:cl-http :demo)
            :documentation "Shows the headers sent by the client to the server.")

(multiple-value-bind (uid pw) (http::%get-user-name+pw "InterRamp's Home Page")
  (ccl:publish-web-page uid pw))
