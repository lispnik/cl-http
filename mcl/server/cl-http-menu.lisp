;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: (cl-http-menu use (common-lisp ccl)) -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Some menu stuff useful for CL-HTTP developers

;;; Copyright: Rainer Joswig, 1996-1997, All rights reserved

;;; Author: Rainer Joswig, joswig@lavielle.com, http://www.lavielle.com/~joswig/
;;; Developed with Macintosh Common Lisp 4.1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Introduction
;;;
;;; Use CL-HTTP-MENU:SETUP-CL-HTTP-MENU to setup a new version
;;; of the menu. If you have the client loaded or if CCL:OPEN-URL is fbound
;;; some menu items will be added. You may also want to call
;;; this function if you change CL-HTTP-MENU:*CL-HTTP-INSPECT-DESCRIPTION*,
;;; CL-HTTP-MENU:*CL-HTTP-SYSTEMS*, CL-HTTP-MENU:*CL-HTTP-STARTUP-FILES*,
;;; CL-HTTP-MENU:*EXTENSION-FILES-DESCRIPTION* or
;;; CL-HTTP-MENU:*CL-HTTP-URL-DESCRIPTIONS*.
;;;
;;; Use CL-HTTP-MENU:ADD-SYSTEM after you have loaded a new subsystem.
;;; Example: (CL-HTTP-MENU:ADD-SYSTEM'*W4-WEB-WALKER* "W4 Web Walker")
;;;
;;; You can extend the menu. See *DEVELOPMENT-ITEMS-CREATORS* and
;;; *DELIVERY-ITEMS-CREATORS*.
;;;
;;; Add your own functionality with the Tools hierarchical menu item.
;;; Use ADD-ITEM-TO-TOOL-MENU. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Changes
;;; 11/Aug/1996 - Added CL-HTTP-MENU package
;;; 11/Aug/1996 - Added preference dialog
;;; 11/Aug/1996 - Added mode to SETUP-CL-HTTP-MENU
;;; 11/Aug/1996 - Added ADD-SYSTEM
;;; 15/Aug/1996 - Added a few client stuff (enabled when feature CL-HTTP-MENU is set)
;;; 15/Aug/1996 - Added URL Table to inspect menu
;;; 15/Aug/1996 - Added two files (license, readme) to the menu
;;; 15/Aug/1996 - Fixed EVAL-ENQUEUE forms
;;; 15/Aug/1996 - Added menu for loading contribs
;;; 15/Aug/1996 - Added :cl-http-menu to *features*
;;; 31/Mar/1997 - Removed inspect preferences menu item
;;; 31/Mar/1997 - Replaced preferences menu item with different version
;;; 31/Mar/1997 - Changed adding of client menu items in MAKE-CL-HTTP-MENU
;;; 19/Apr/1997 - Changes to the preference dialog, uses present-to-string
;;; 19/Apr/1997 - Added menu for opening some URLs
;;; 19/Apr/1997 - Added menu for finding documentation
;;; 19/Apr/1997 - Allow only one preference dialog window
;;; 28/Apr/1997 - Changed the open URL stuff for updating the menu item
;;; 28/Apr/1997 - Added more help spec stuff
;;; 28/Apr/1997 - Added a startup configuration without DNS
;;; 29/Apr/1997 - Added support for more contributions
;;; 29/Apr/1997 - Contributions that are systems will be added to the
;;;               systems menu, after loading the contribution via the menu
;;; 26/Jun/1997 - Replaced most EVAL-ENQUEUE forms with PROCESS-RUN-FUNCTION.
;;; 26/Jun/1997 - Made the menu extensible. See *DEVELOPMENT-ITEMS-CREATORS* and
;;;               *DELIVERY-ITEMS-CREATORS*.
;;; 26/Jun/1997 - Added Tools menu item for development mode.
;;;  9/Jul/1997 - Fixed missing Process-Run-Function parameter
;;; 31/Aug/1997 - require scrollers, JCMA
;;; 31/Aug/1997 - reset network item, JCMA
;;; 31/Aug/1997 - FTYPE declaration for http::show-url-headers and http::show-url,
;;;               JCMA
;;; 31/Aug/1997 - Changed contrib to extension, changed HTML parser V8,
;;;               added MCF, fixed pathnames to reflect changes in 67-2
;;; 31/Aug/1997 - changed the file for the ReadMe menu-item
;;; 31/Aug/1997 - Added local browser preference. Exported GET-CURRENT-BROWSER,
;;;               SET-CURRENT-BROWSER and SELECT-AND-SET-BROWSER. Was written
;;;               by me and repackaged by M.Travers in
;;;               http:mcl:contrib:mtravers:browsercontrol:cl-http.lisp . 
;;; 04/Sep/1997 - Added Proxy system load form.
;;;               Moved requires before package definition to avoid symbol conflict.
;;; 01/Nov/1997 - Added more client stuff.
;;; 01/Nov/1997 - Prepare for the new release. 
;;; 01/Nov/1997 - Prepare for the new release. 
;;; 25/Apr/1999 - Added loading the XML Parser and changed definition of load-extension
;;;               to load lisp files if the compiled file does not exist. RJ

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; To Do
;;; - Preference dialog needs some work 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package declaration

(eval-when (compile eval load)          ; don't blowout with package conflicts
  
  (require :pop-up-menu)
  (require :scrollers) 
  
  (defpackage "CL-HTTP-MENU"
    (:use "COMMON-LISP" "CCL")
    (:export "*CL-HTTP-MENU*"
             "*CL-HTTP-INSPECT-DESCRIPTION*"
             "ADD-ITEM-TO-INSPECT"
             "*CL-HTTP-SYSTEMS*"
             "ADD-SYSTEM"
             "*CL-HTTP-STARTUP-FILES*"
             "SETUP-CL-HTTP-MENU"
             "*EXTENSION-FILES-DESCRIPTION*"
             "*CL-HTTP-URL-DESCRIPTIONS*"
             "ADD-ITEM-TO-OPEN-URL"
             "*CL-HTTP-MENU-MODE*"
             "*DEVELOPMENT-ITEMS-CREATORS*"
             "*DELIVERY-ITEMS-CREATORS*"
             "*CL-HTTP-TOOL-MENU-DESCRIPTION*"
             "ADD-ITEM-TO-TOOL-MENU"
             "GET-CURRENT-BROWSER"
             "SET-CURRENT-BROWSER"
             "SELECT-AND-SET-BROWSER"))
  
  )

(in-package "CL-HTTP-MENU") 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menu variables and constants 

(defconstant *cl-http-menu-name* "CL-HTTP"
  "The name for the CL-HTTP menu.")

(defconstant *windows-menu-name* "Windows"
  "The name of the MCL Windows menu.")

(defparameter *cl-http-menu* nil
  "The menu object for CL-HTTP - the Common Lisp Hypermedia Server")

(defparameter *inspect-cl-http-menu* nil
  "Inspect some important CL-HTTP variables and lists") 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mac specific characters

(defun add-periods (string)
  (concatenate 'string string (string (code-char 201)))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Install the CL-HTTP menu

(defun install-cl-http-menu ()
  "Installs the CL-HTTP menu just before the Windows menu."
  (assert *cl-http-menu* (*cl-http-menu*))
  (let ((cl-http-menu (find-menu *cl-http-menu-name*)))
    (when cl-http-menu
      (menu-deinstall cl-http-menu)))
  (let ((windows-menu (find-menu *windows-menu-name*)))
    (when windows-menu
      (menu-deinstall windows-menu))
    (menu-install *cl-http-menu*)
    (when windows-menu
      (menu-install windows-menu)))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; start and stop server

(defun make-start-server-menu-item ()
  (make-instance 'menu-item
    :menu-item-title "Enable HTTP Service"
    :menu-item-action 'http:enable-http-service
    :help-spec "Start serving the Web."))

(defun make-stop-server-menu-item ()
  (make-instance 'menu-item
    :menu-item-title "Disable HTTP Service"
    :menu-item-action 'http:disable-http-service
    :help-spec "Stop serving the Web.")) 

#+Open-Transport
(defun make-reset-network-menu-item ()
  (make-instance 'menu-item
    :menu-item-title "Reset Network"
    :menu-item-action 'ccl::opentransport-cleanup
    :help-spec "Reset Open Transport."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Inspect menu item

(defparameter *cl-http-inspect-description*
  nil
  "A list (name form description) of all items to inspect with a menu.
Items can be added with ADD-ITEM-TO-INSPECT.")

(defun add-item-to-inspect (name form documentation)
  "Adds a new inspect description to *cl-http-inspect-description*.
If a description with the same name exists, it will be replaced."
  (check-type name string)
  (assert (or (null documentation)
              (stringp documentation))
          (documentation))
  (let ((item (list name form documentation))
        (previous-item (assoc name *cl-http-inspect-description*
                              :test #'string-equal)))
    (if previous-item
      (setf (rest previous-item)
            (rest item))
      (setf *cl-http-inspect-description* (merge 'list
                                                 (list item)
                                                 *cl-http-inspect-description*
                                                 #'string<
                                                 :key #'first)))))

(defun list-all-urls ()
  "Returns a sorted list of all known URLs."
  (let ((urls nil))
    (flet ((add (name object)
             (declare (ignore name))
             (push object urls)))
      (url:map-url-table #'add))
    (setf urls (sort urls #'string< :key #'url:coerce-url-string))
    urls)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define the items to inspect 

(eval-when (:load-toplevel :execute)
  (add-item-to-inspect "List URLs" '(list-all-urls) "List all URLs in URL table.")
  (add-item-to-inspect "URL Table" 'url::*url-table* "The URL Table itself.")
  (add-item-to-inspect "List Logs" 'http::*all-logs* "List all known logs.")
  (add-item-to-inspect "Server Initialization List"
                       'http::*server-initialization-list*
                       "The cold initialization list for the CL-HTTP server.")
  (add-item-to-inspect "User Agent Capabilities"
                       'http:*user-agent-capabilities*
                       "Maps capabilities to user agents.
Entries are (capability &rest (user-agent &rest versions)).
No versions means that all versions handle the capability.")
  (add-item-to-inspect "Virtual Host Table"
                       'http::*virtual-host-table*
                       (documentation 'http::*virtual-host-table* 'variable))
  (add-item-to-inspect "Builtin Client Colors"
                       'netscape1.1:*built-in-client-colors*
                       (documentation 'netscape1.1:*built-in-client-colors* 'variable))
  (add-item-to-inspect "Embedded Script Languages"
                       'netscape2.0::*embedded-script-languages*
                       (documentation 'netscape2.0::*embedded-script-languages* 'variable))
  (add-item-to-inspect "HTML Event Handlers"
                       'html2:*event-handlers*
                       (documentation 'html2:*event-handlers* 'variable))
  (add-item-to-inspect "Service Name Number Alist"
                       'ccl::*service-name-number-alist*
                       (documentation 'ccl::*service-name-number-alist* 'variable))
  (add-item-to-inspect "Store and Forward Mail Hosts"
                       'smtp:*store-and-forward-mail-hosts*
                       (documentation 'smtp:*store-and-forward-mail-hosts* 'variable))
  (add-item-to-inspect "All Resources"
                       'resources::*all-resources*
                       (documentation 'resources::*all-resources* 'variable))
  (add-item-to-inspect "Builtin Backgrounds"
                       'netscape1.1:*built-in-backgrounds*
                       (documentation 'netscape1.1:*built-in-backgrounds* 'variable))
  (add-item-to-inspect "ISO Readtable"
                       'html2::*iso-readtable*
                       (documentation 'html2::*iso-readtable* 'variable))
  (add-item-to-inspect "Server Control Alist"
                       'http::*server-control-alist*
                       "A property list of streams awaiting incoming http connections on each port.")
  (add-item-to-inspect "Server Launch Initialization List"
                       'http::*server-launch-initialization-list*
                       "The initialization list for launching the CL-HTTP server.")) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Make the Inspect submenu 

(defclass inspect-item-menu-item (menu-item)
  ((form-to-inspect :accessor menu-form-to-inspect :initarg :form-to-inspect))
  (:documentation "On action inspect the value of the form-to-inspect."))

(defmethod menu-item-action ((menu-item inspect-item-menu-item))
  (let ((value (eval (menu-form-to-inspect menu-item))))
    (if (option-key-p)
      (ccl::toplevel-print (list value))
      (inspect value))))

(defun make-menu-from-inspect-menu-description ()
  (when *cl-http-inspect-description*
    (make-instance 'menu
      :menu-title "Inspect"
      :menu-items (loop for (name form documentation) in *cl-http-inspect-description*
                        collect (make-instance 'inspect-item-menu-item
                                  :menu-item-title name
                                  :help-spec documentation
                                  :form-to-inspect form))))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tools menu item

(defvar *cl-http-tool-menu-description*
  nil
  "A list of tools (name function description) you would like to experiment with.
Items should be added with ADD-ITEM-TO-TOOL-MENU.")

(defun add-item-to-tool-menu (name function documentation &key (activate nil))
  "Adds a new tool description to *cl-http-tool-menu-description*
If a description with the same name exists, it will be replaced."
  (check-type name string)
  (assert (or (null documentation)
              (stringp documentation))
          (documentation))
  (let ((item (list name function documentation))
        (previous-item (assoc name *cl-http-tool-menu-description*
                              :test #'string-equal)))
    (if previous-item
      (setf (rest previous-item)
            (rest item))
      (setf *cl-http-tool-menu-description* (merge 'list
                                                   (list item)
                                                   *cl-http-tool-menu-description*
                                                   #'string<
                                                   :key #'first))))
  (when activate
    (setup-cl-http-menu) ))

(defun make-menu-from-tool-menu-description ()
  (when *cl-http-tool-menu-description*
    (flet ((action (name)
             #'(lambda ()
                 (let ((description (assoc name
                                           *cl-http-tool-menu-description*
                                           :test #'string-equal)))
                   (if description
                     (process-run-function name (second description))
                     (message-dialog "Can't execute tool. Not specified."))))))
      (make-instance 'menu
        :menu-title "Tools"
        :menu-items (loop for (name nil documentation) in *cl-http-tool-menu-description*
                          collect (make-instance 'menu-item
                                    :menu-item-title name
                                    :help-spec documentation
                                    :menu-item-action (action name)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Register Systems on the menu 

(defun update-system-menu-for-system ()
  (flet ((update-menu (name items)
           (apply 'remove-menu-items (find-menu-item (find-menu "CL-HTTP") name)
                  (menu-items (find-menu-item (find-menu "CL-HTTP") name)))
           (apply 'add-menu-items (find-menu-item (find-menu "CL-HTTP") name) items)))
    (update-menu "Edit CL-HTTP System" (make-system-edit-file-items))
    (update-menu "Compile CL-HTTP System" (make-system-action-items :compile-load))  
    (update-menu "Load CL-HTTP System" (make-system-action-items :load))
    (update-menu "Recompile CL-HTTP System" (make-system-action-items :compile-load-always))
    #+ignore(update-menu "Compile CL-HTTP System" (make-system-action-items :compile))))

; (update-system-menu-for-system)
(eval-when (:execute :compile-toplevel :load-toplevel)
  (pushnew 'update-system-menu-for-system cl-user::*system-define-hook*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Edit the files belonging to the CL-HTTP systems
;;; If a new system is being added to *cl-http-systems* it will be
;;; automatically added to the menu.

(defun make-edit-file-menu (name)
  (let ((files (cl-user::system-files (cl-user::find-system-named name))))
    (setf files (sort files #'string<))
    (make-instance 'menu-item
      :menu-item-title (format nil "~a" name)
      :menu-item-action #'(lambda ()
                            (map nil
                                 #'ed
                                 (select-item-from-list
                                  files
                                  :window-title "Edit Files"
                                  :selection-type :disjoint))))))

(defun make-system-edit-file-items ()
  (sort (loop for system in cl-user::*systems*
              collect (make-edit-file-menu (cl-user::get-system-name system)))
        #'string-lessp
        :key #'menu-item-title))

(defun make-edit-system-menu ()
  (make-instance 'menu
    :menu-title "Edit CL-HTTP System"
    :menu-items (make-system-edit-file-items)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Perform actions on known systems

(defclass system-action-menu-item (menu-item)
  ((system-action :accessor menu-system-action :initarg :system-action)
   (system-variable :accessor menu-system-variable :initarg :system-variable)))

(defmethod menu-item-action ((menu-item system-action-menu-item))
  (let ((modules (menu-system-variable menu-item))
        (action (menu-system-action menu-item)))
    (eval-enqueue
     `(common-lisp-user::execute-system-operations
       ',modules
       '(,action)))))

(defun make-system-action-items (action)
  (sort (loop for system in cl-user::*systems*
              collect (make-instance 'system-action-menu-item
                        :menu-item-title (cl-user::get-system-name system)
                        :system-action action
                        :system-variable system))
        #'string-lessp
        :key #'menu-item-title))

(defun make-action-system-menu (action name)
  (make-instance 'menu
    :menu-title name
    :menu-items (make-system-action-items action))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A menu for the extensions 

(defclass load-file-enqueued-menu-item (menu-item)
  ((file :accessor menu-item-file :initarg :file)))

(defclass load-file-for-system-enqueued-menu-item (load-file-enqueued-menu-item)
  ((system :accessor menu-item-system :initarg :system))
  (:documentation "For loading a CL-HTTP system file."))

(defun load-extension (file)
  (let ((lisp-file (make-pathname :defaults file :type "lisp"))
        (compiled-file (make-pathname :defaults file :type "pfsl")))
    (if (and (fboundp 'compile-file)
             (probe-file lisp-file)
             (or (not (probe-file compiled-file))
                 (let ((compiled-date (file-write-date compiled-file))
                       (lisp-date (file-write-date lisp-file)))
                   (and compiled-date lisp-date
                        (< compiled-date lisp-date))))
             (y-or-n-dialog (format nil "Compile file ~a before loading?" lisp-file)))
      (eval-enqueue `(progn (compile-file ,lisp-file :output-file ,compiled-file)
                            (load ,compiled-file)))
      (process-run-function "Load Extension"
                            #'load
                            (if (probe-file compiled-file)
                              compiled-file
                              lisp-file)))))

(defmethod menu-item-action ((menu-item load-file-enqueued-menu-item))
  (let ((file (menu-item-file menu-item)))
    (if (probe-file file)
      (load-extension file)
      (message-dialog (format nil "File ~a not available." file)))))

(defmethod menu-item-action ((menu-item load-file-for-system-enqueued-menu-item))
  (flet ((load-stuff (file)
           (load file)))
    (let ((file (menu-item-file menu-item)))
      (if (probe-file file)
        (process-run-function "Load System File" #'load-stuff file)
        (message-dialog (format nil "File ~a not available." file)))))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *extension-files-description*
  (list (list "http:mcl;client;sysdcl.lisp" "Basic Client" 'cl-user::*cl-http-client*)
        (list "http:mcl;contrib;bsinclair;html-editor.lisp" "HTML Editor")
        (list "http:html-parser;v8;sysdcl.lisp" "HTML Parser v8" 'cl-user::*html-parser*)
        (list "http:html-parser;v9;mac-sysdcl.lisp" "HTML Parser v9" 'cl-user::*html-parser*)
        (list "http:mcl;lambda-ir;sysdcl.lisp" "Lambda IR" 'cl-user::*lambda-ir*)
        (list "http:examples;listener.lisp" "Listener")
        (list "http:examples;log-window.lisp" "Log Window")
        (list "http:examples;mail-archive.lisp" "Mail Archive")
        (list "http:examples;mcf095;mcf.lisp" "Meta Content Format")
        (list "http:mcl;proxy;sysdcl.lisp" "Proxy Server" 'cl-user::*cl-http-proxy*)
        (list "http:mcl;w4;sysdcl.lisp" "Web Walker" 'cl-user::*w4-web-walker*)
        (list "http:mcl;w4;sysdcl-examples.lisp" "Web Walker Demo" 'cl-user::*w4-web-walker-demo*)
        (list "http:examples;slides.lisp" "Slides")
        (list "http:examples;twistdown-tree;twistdown.lisp" "Twistdown Tree")
        (list "http:mcl;contrib;tnorderhaug;cgi;cgi-sysdcl.lisp" "CGI Support" 'cl-user::*cgi-support*)
        (list "http:contrib;janderson;XML;defsystem.lisp" "XML Parser")))

(defun extension-file-items (list)
  (let ((items (loop for (file title system) in list
                     when system
                     collect (make-instance 'load-file-for-system-enqueued-menu-item
                               :file (pathname file)
                               :system system
                               :menu-item-title title)
                     else collect (make-instance 'load-file-enqueued-menu-item
                                    :file (pathname file)
                                    :menu-item-title title))))
    (setf items (sort items #'string< :key #'menu-item-title))
    items))

(defun make-load-extensions-menu ()
  (make-instance 'menu
    :menu-title "Load Extension"
    :menu-items (extension-file-items *extension-files-description*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menu items for toggling

(defun make-toggle-menu-item (test on off name documentation)
  (make-instance 'menu-item
    :menu-item-title name
    :menu-item-checked (funcall test)
    :menu-item-action #'(lambda ()
                          (if (funcall test)
                            (funcall off)
                            (funcall on)))
    :update-function #'(lambda (menu-item)
                         (set-menu-item-check-mark menu-item (funcall test)))
    :help-spec documentation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menu items for boolean variables 

(defun make-toggle-var-item (var name documentation)
  (make-instance 'menu-item
    :menu-item-title name
    :menu-item-checked (symbol-value var)
    :menu-item-action #'(lambda ()
                          (set var (not (symbol-value var))))
    :update-function #'(lambda (menu-item)
                         (set-menu-item-check-mark menu-item (symbol-value var)))
    :help-spec documentation)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A simple "About..." dialog
;;; Information is being printed to a fred item and can be copied to the clipboard. 

(defun make-about-cl-http-dialog ()
  (let* ((window (make-instance 'window
                   :window-title "About CL-HTTP"
                   :window-type :document
                   :view-size #@(500 300)
                   :view-position #@(100 50)))
         (fred (make-instance 'fred-dialog-item
                 :view-container window
                 :view-position #@(10 10)
                 :view-size #@(480 280)))
         (buffer (fred-buffer fred))
         (f1 (list "Geneva" 10 :bold))
         (f2 (list "Geneva" 9 :plain)))
    (flet ((define-it (string1 string2)
             (buffer-set-font-spec buffer f1)
             (write-string string1 fred)
             (write-string ":" fred)
             (buffer-set-font-spec buffer f2)
             (write-string "  " fred)
             (buffer-set-font-spec buffer f2)
             (write-string string2 fred)
             (terpri fred)))
      (define-it "CL-HTTP is written by" "John C. Mallery")
      (terpri fred)
      (define-it "CL-HTTP version"
        (destructuring-bind (major minor nil nil nil)
                            (www-utils:%server-version-info)
          (format nil "~D.~D"
                  major minor)))
      (define-it "Macintosh port version"
        (destructuring-bind (nil nil port-major port-minor port-patch-level)
                            (www-utils:%server-version-info)
          (format nil "~D.~D.~D"
                  port-major port-minor port-patch-level)))
      (define-it "Lisp version" (format nil "~a ~a"
                                        (lisp-implementation-type)
                                        (lisp-implementation-version)))
      (define-it "Running on a" (format nil "~a" (machine-version)))
      (terpri fred)
      (define-it "Local Server Administration"
        "/cl-http/maintenance/configure-server.html")
      (terpri fred)
      (define-it "CL-HTTP home page" http:*cl-http-home-page-url-string*)
      (define-it "CL-HTTP mailing list" "mailto:www-cl@ai.mit.edu")
      (define-it "CL-HTTP bug reports" "mailto:bug-cl-http@ai.mit.edu")
      (define-it "CL-HTTP Mac specific bug reports"
        "mailto:bug-mac-cl-http@ai.mit.edu")
      (define-it "CL-HTTP FTP archive"
        "ftp://ftp.ai.mit.edu/pub/users/jcma/cl-http/")
      (terpri fred)
      (define-it "Digitool home page" "http://www.digitool.com/")
      (define-it "Digitool Mail Address" "mailto:info@digitool.com")
      (define-it "Digitool MCL mailing list" "mailto:info-mcl@digitool.com")
      (define-it "Digitool MCL bug reports" "mailto:bug-mcl@digitool.com")
      (define-it "Digitool MCL news group" "news:comp.lang.lisp.mcl")
      (define-it "Digitool FTP archive" "ftp://ftp.digitool.com/"))
    (ccl::%buffer-set-read-only buffer t)))

(defun make-about-menu-item ()
  (make-instance 'menu-item
    :menu-item-title "About CL-HTTP"
    :menu-item-action 'make-about-cl-http-dialog
    :help-spec "About the Common Lisp Hypermedia Server.
Lists some useful pointers.")) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display some files: Release Notes, License, Macintosh Readme 

(defclass display-file-menu-item (menu-item)
  ((file :accessor menu-item-file :initarg :file)))

(defmethod menu-item-action ((menu-item display-file-menu-item))
  (if (probe-file (menu-item-file menu-item))
    (ed (menu-item-file menu-item))
    (message-dialog (format nil
                            "~a not available."
                            (pathname-name (menu-item-file menu-item))))))

(defun make-release-notes-menu-item ()
  (make-instance 'display-file-menu-item
    :menu-item-title "Release Notes"
    :file "http:http;-release-notes-.text"
    :help-spec "Release notes for the Common Lisp Hypermedia Server."))

(defun make-license-menu-item ()
  (make-instance 'display-file-menu-item
    :menu-item-title "License"
    :file "http:http;-license-.text"
    :help-spec "License for the Common Lisp Hypermedia Server."))

(defun make-mac-readme-menu-item ()
  (make-instance 'display-file-menu-item
    :menu-item-title "Macintosh Readme"
    :file "http:mcl-read-me.text"
    :help-spec "Readme for the MCL version of the Common Lisp Hypermedia Server.")) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Startup files 

(defparameter *cl-http-startup-files*
  `((,(pathname "http:mcl;start-server.lisp")
     "Start Server with Demos with DNS"
     "This will start your server using the DNS for getting the
server name. Use it when name resolution is available.")
    (,(pathname "http:mcl;start-server-appletalk.lisp")
     "Start Server with Demos with IP over AppleTalk"
     "This will start your server without DNS and running IP
on AppleTalk. It is useful on Mac configurations without
Ethernet and without DNS access.")
    (,(pathname "http:mcl;start-server-ip-number.lisp")
     "Start Server with Demos without DNS"
     "This will start your server without DNS using
your normal IP connection. Thus you can access your server disconnected
without DNS service via the machine's IP-Number. Use this for
example on PowerBooks with Ethernet connectors while not
having Internet access."))
  "These files can start the server and make various configuration settings.
A list of (pathname title documentation). Documentations and titles are strings.")

(defun make-startup-menu-item ()
  (make-instance 'menu
    :menu-title "Startup"
    :menu-items (loop for (pathname title documentation) in *cl-http-startup-files*
                      collect (make-instance 'menu-item
                                :menu-item-title title
                                :menu-item-action (let ((file pathname))
                                                    #'(lambda ()
                                                        (process-run-function "Startup"
                                                                              #'load
                                                                              file)))
                                :help-spec documentation)))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load and Save Configurations 

(defun load-configuration ()
  (flet ((do-load-configuration (pathname)
           (let* ((directory (probe-file (make-pathname
                                          :host (pathname-host pathname)
                                          :directory (pathname-directory pathname))))
                  (pathname (choose-file-dialog :directory directory
                                                :mac-file-type :TEXT
                                                :button-string "Configure")))
             (http::standard-configure-server :pathname pathname)
             (map nil #'(lambda (window)
                          (update-window-for-changed-preference-type
                           window
                           (current-type (view-named :preference-types-item window))))
                  (windows :class 'preference-window)))))
    (process-run-function "Load Configuration"
                          #'do-load-configuration
                          (pathname http:*standard-configuration-pathname*)))) 

(defun make-load-configuration-menu-item ()
  (make-instance 'menu-item
    :menu-item-title "Load Configuration"
    :menu-item-action 'load-configuration
    :help-spec "Load a CL-HTTP configuration."))

(defun save-configuration ()
  (process-run-function
   "Save Configuration"
   #'(lambda ()
       (let ((pathname (choose-new-file-dialog
                        :directory http:*standard-configuration-pathname*
                        :prompt "Save Configuration"
                        :button-string "Save")))
         (http::save-standard-server-configuration :pathname pathname)))))

(defun make-save-configuration-menu-item ()
  (make-instance 'menu-item
    :menu-item-title "Save Configuration"
    :menu-item-action 'save-configuration
    :help-spec "Save the current CL-HTTP configuration.")) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Local Browser preference 

(http::define-preference-type
  :browser-preferences
  :name "Browser"
  :display-string "Local Browser"
  :inferiors (:browser)
  :description "Parameters related to the local browser.")

(http::define-preference :browser
  :name "Browser"
  :presentation-type `(www-present:member-sequence
                       (:Netscape-Navigator :Microsoft-Internet-Explorer))
  :prompt "Local Browser Name"
  :default-value-form :|MOSS|
  :get-value-form (ecase *browser-creator*
                    (:|MOSS| :Netscape-Navigator)
                    (:|MSIE| :Microsoft-Internet-Explorer))
  :set-value-form (setq *browser-creator*
                        (cond ((equal http::value :Netscape-Navigator) :|MOSS|)
                              ((equal http::value :Microsoft-Internet-Explorer) :|MSIE|)
                              (t (error "Unknown browser: ~a" http::value))))
  :description "The Browser you would like to use with your Mac.")

(pushnew :browser http::*remote-configuration-form-prototype*)

(defun get-current-browser ()
  (funcall (http::preference-value-getter (http::find-preference :browser))))

(defun set-current-browser (browser-name)
  (funcall (http::preference-value-setter (http::find-preference :browser))
           browser-name))

(defun select-and-set-browser ()
  (set-current-browser (first (select-item-from-list '(:Netscape-Navigator
                                                       :Microsoft-Internet-Explorer)
                                                     :window-title "Select Browser"
                                                     :selection-type :single))))
; (select-and-set-browser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setting preferences 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setting preferences: List preference information 

(defun list-preference-types ()
  "Preferences are grouped by preference types. Returns
a list of preference types sorted by preference-type-display-string."
  (let ((preference-types nil))
    (maphash
     #'(lambda (key preference-type)
         (declare (ignore key))
         (push preference-type preference-types))
     http::*preference-type-table*)
    (setf preference-types (sort preference-types
                                 #'string<
                                 :key #'http::preference-type-display-string))
    preference-types))

; (list-preference-types)

(defun list-active-preferences ()
  "These are the active preferences that govern the behaviour of
the server. Returns a list of preference object sorted by preference name."
  (let ((preferences nil))
    (http::map-standard-preferences
     #'(lambda (item)
         (push item preferences)))
    (setf preferences (sort preferences #'string< :key #'http::preference-name))
    preferences))

; (list-active-preferences)

(defun list-active-preferences-for-type (preference-type)
  "Given a preference-type it returns the list of
preferences sorted by preference-prompt."
  (unless (typep preference-type 'http::preference-type)
    (setf preference-type (http::find-preference-type preference-type)))
  (let ((preferences (loop for preference in (http::preference-type-inferiors
                                              preference-type)
                           collect (http::find-preference preference))))
    (setf preferences (sort preferences #'string<
                            :key #'http::preference-prompt))
    preferences))

; (list-active-preferences-for-type :mail-preferences) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setting preferences: Compute the view items for a given preference 

(defclass preference-menu-item (menu-item)
  ((preference-value :accessor menu-item-preference-value
                     :initarg :preference-value
                     :documentation "Each menu item for the preference menu
in the scroller area stands for a possible preference value."))) 

;; Should we signal an error?
(defun get-value-from-view (view presentation-type previous-value default)
  "Given a dialog-item and a presentation-type return the value from
the dialog-item's content. If the text of a text-dialog-item is empty,
return the default."
  (cond ((and (consp presentation-type)
              (eq 'w3p:member-sequence (first presentation-type)))
         (values (menu-item-preference-value (selected-item view)) t))
        ((eq 'www-present:boolean presentation-type)
         (values (check-box-checked-p view) t))
        (t (let ((string (dialog-item-text view)))
             (if (and (stringp string) (> (length string) 0))
               (handler-case (multiple-value-bind (value type)
                                                  (www-present:accept-from-string
                                                   presentation-type
                                                   string)
                               (if type
                                 (values value t)
                                 (values (message-dialog
                                          (format nil
                                                  "Expecting type \"~a\"."
                                                  presentation-type))
                                         nil)))
                 (www-present:input-not-of-required-type
                  ()
                  (set-dialog-item-text view (www-present:present-to-string
                                              previous-value
                                              presentation-type))
                  (values nil nil)))
               (values default t))))))

(defmethod value-changed-p (current-value new-value)
  "Same values?"
  (not (equal current-value new-value)))

(defun preference-item-dialog-action-fn (preference view)
  "Every dialog or menu item for the preference scroller should
use this function, unless it should not use an action function at all.
Handles enabling and disabling of the apply button.
Manages the window's list of changed preferences."
  (let ((current-value (funcall (http::preference-value-getter preference)))
        (window (view-container (view-container view)))
        (presentation-type (http::preference-presentation-type preference)))
    (multiple-value-bind (new-value valid-new-value-p)
                         (get-value-from-view
                          view
                          presentation-type
                          current-value
                          (funcall (http::preference-default-value-getter preference)))
      (if valid-new-value-p
        (if (value-changed-p current-value new-value)
          (let ((desc (assoc preference (window-changes window))))
            (if desc
              (when (not (equal new-value (second desc)))
                (setf (second desc) new-value))
              (progn (push (list preference new-value) (window-changes window))
                     (dialog-item-enable (view-named :apply-button window)))))
          (progn
            (setf (window-changes window)
                  (delete preference (window-changes window) :key #'first))
            (unless (window-changes window)
              (dialog-item-disable (view-named :apply-button window)))))
        (message-dialog (format nil
                                "Value is not of expected type \"~a\", reset to previous value."
                                presentation-type))))))

(defun make-item-dialog-action-function (preference view)
  #'(lambda (&rest ignore)
      (declare (ignore ignore))
      (preference-item-dialog-action-fn preference view)))

(defun get-views-for-preference (preference view position)
  "Gets a preference and computes based on the presentation type a list
of MCL views. Returns the list of views generated for
the preference object and the start position for the next view."
  (let ((presentation-type (http::preference-presentation-type preference))
        (view-font '("Geneva" 10))
        (view-size #@(140 14))
        (fred-view-size #@(200 14)))
    (cond ((and (consp presentation-type)
                (eq 'w3p:member-sequence (first presentation-type)))
           (let* ((menu-items (loop for member in (second presentation-type)
                                    collect (make-instance 'preference-menu-item
                                              :menu-item-title (princ-to-string member)
                                              :preference-value member)))
                  (menu (make-instance 'pop-up-menu
                          :menu-items menu-items
                          :default-item (or (1+ (position
                                                 (funcall (http::preference-value-getter
                                                           preference))
                                                 (second presentation-type)))
                                            1)
                          :view-position position
                          :view-font view-font
                          :help-spec (http::preference-description preference)
                          :menu-title (http::preference-name preference))))
             (loop for menu-item in (menu-items menu)
                   do (set-menu-item-action-function
                       menu-item
                       (make-item-dialog-action-function preference menu)))
             (let ((width (point-h (view-default-size menu)))
                   (height (point-v (view-default-size menu))))
               (set-view-size menu (min view-size width) height)
               (values (list menu)
                       (add-points position
                                   (make-point 0 (point-v (view-size menu))))))))
          ((eq 'www-present:boolean presentation-type)
           (let* ((text-item (make-instance 'static-text-dialog-item
                               :dialog-item-text (http::preference-name preference)
                               :view-position position
                               :help-spec (http::preference-description preference)
                               :view-font view-font))
                  (item (make-instance 'check-box-dialog-item
                          :check-box-checked-p (funcall (http::preference-value-getter
                                                         preference))
                          :view-position (add-points
                                          position
                                          (make-point
                                           (+ (point-h (string-width (http::preference-name preference)
                                                                     view-font))
                                              10)
                                           0))
                          :dialog-item-action (make-item-dialog-action-function 
                                               preference
                                               view)
                          :help-spec (http::preference-description preference)
                          :view-nick-name :setter-item)))
             (set-dialog-item-action-function item
                                              (make-item-dialog-action-function preference
                                                                                item))
             (values (list text-item item) (add-points position (make-point 0 14)))))
          (t (let* ((text-item (make-instance 'static-text-dialog-item
                                 :dialog-item-text (http::preference-name preference)
                                 :view-position position
                                 :help-spec (http::preference-description preference)
                                 :view-font view-font))
                    (item (make-instance 'fred-dialog-item
                            :view-size fred-view-size
                            :view-position (add-points
                                            position
                                            (make-point (+ (point-h (string-width (http::preference-name preference)
                                                                                  view-font))
                                                           10)
                                                        0))
                            :dialog-item-text (www-present:present-to-string
                                               (funcall
                                                (http::preference-value-getter preference))
                                               presentation-type)
                            :help-spec (http::preference-description preference)
                            :view-font view-font)))
               (set-dialog-item-action-function
                item
                (make-item-dialog-action-function preference item))
               (values (list text-item item)
                       (add-points position (make-point 0 (point-v (view-size item))))))))))

; (make-preference-window) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setting preferences: All the items for a given preference type 

(defun make-preference-items-for-preference-type (type view start-position)
  "Given a preference-type this function computes all views to display
for a dialog based on the views generated for each preference."
  (let ((preferences (list-active-preferences-for-type type))
        (all-items nil)
        (v-distance 8))
    (dolist (preference preferences)
      (multiple-value-bind (items next-start-position)
                           (get-views-for-preference preference view start-position)
        (setf start-position (add-points next-start-position
                                         (make-point 0 v-distance)))
        (setf all-items (append all-items items))))
    all-items)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setting preferences: Preferences Scroller item 

(defun make-preferences-scroller (window preference-type)
  "The preferences are being displayed by a scroller view.
Make this scroller and add all views generated for the preference-type."
  (let ((scroller (make-instance 'scroller
                    :track-thumb-p t
                    :view-position #@(10 40)
                    :view-size #@(400 120)
                    :view-nick-name :scroller)))
    (update-preferences-scroller scroller preference-type)
    (add-subviews window scroller)
    scroller))

(defun update-preferences-scroller (scroller preference-type)
  "Update the preferences scroller by recomputing all the
necessary subviews based on the information provided
by the preference-type."
  (let ((preference-items (make-preference-items-for-preference-type
                           preference-type
                           scroller
                           #@(5 5))))
    (map-subviews scroller #'(lambda (item) (remove-subviews scroller item)))
    (loop for item in preference-items
          do (add-subviews scroller item)))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setting preferences: Preferences Menu 

(defclass preference-types-menu (pop-up-menu)
  ((types :initarg :types)
   (current-type :accessor current-type)))

(defun make-preference-types-menu (window)
  "This menu controls which preferences to display based
on the selected preference-type."
  (flet ((create-menu-items (preference-types menu)
           (mapcar #'(lambda (type)
                       (make-instance 'menu-item
                         :menu-item-title (http::preference-type-display-string type)
                         :menu-item-action
                         #'(lambda ()
                             (setf (current-type menu) type) 
                             (update-window-for-changed-preference-type window type))))
                   preference-types)))
    (let* ((types (list-preference-types))
           (menu (make-instance 'preference-types-menu
                   :view-position #@(20 14)
                   :view-font '("Geneva" 12)
                   :types types
                   :view-nick-name :preference-types-item)))
      (apply #'add-menu-items menu (create-menu-items types menu))
      (setf (current-type menu) (first types))
      (add-subviews window menu)
      menu))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setting preferences: Apply changes 

(defun apply-changes-action (item)
  (flet ((get-preference-item (list) (first list))
         (get-new-value (list) (second list)))
    (let* ((window (view-container item))
           (changes (select-item-from-list
                     (window-changes window)
                     :window-title "Change CL-HTTP Settings"
                     :selection-type :disjoint
                     :table-print-function #'(lambda (item stream)
                                               (let ((preference-item (get-preference-item item)))
                                                 (format stream "Set ~a to ~a"
                                                         (http::preference-name preference-item)
                                                         (www-present:present-to-string
                                                          (get-new-value item)
                                                          (http::preference-presentation-type
                                                           preference-item))))))))
      (loop for (preference new-value) in changes
            do (funcall (http::preference-value-setter preference)
                        new-value)
            do (setf (window-changes window)
                     (delete preference (window-changes window) :key #'first)))
      (unless (window-changes window)
        (dialog-item-disable item)))))

(defun make-apply-button ()
  (make-instance 'button-dialog-item
    :view-position #@(10 200)
    :dialog-item-text "Apply Changes"
    :dialog-item-action 'apply-changes-action
    :view-nick-name :apply-button
    :dialog-item-enabled-p nil)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setting preferences: The Preference window 

(defun update-window-for-changed-preference-type (window type)
  "Updates the window by calling update-preferences-scroller
on the scroller view."
  (update-preferences-scroller (view-named :scroller window)
                               type))

(defclass preference-window (window)
  ((changes :accessor window-changes
            :initform nil
            :documentation "All the preferences that have been changed and
are not already in effect."))
  (:documentation "The preference-window class for displaying
the preferences for CL-HTTP.")) 

(defun make-preference-window ()
  "Generate the preference window for controlling the CL-HTTP preferences."
  (let ((old-windows (windows :class (find-class 'cl-http-menu::preference-window)
                              :include-invisibles t)))
    (if old-windows
      (let ((old-window (first old-windows)))
        (window-ensure-on-screen old-window)
        (window-select old-window))
      (let ((window (make-instance 'preference-window
                      :window-type :document
                      :view-size #@(440 240)
                      :view-position #@(20 60)
                      :window-title "CL-HTTP Preferences")))
        (add-subviews window
          (make-preference-types-menu window)
          (make-preferences-scroller window
                                     (first (list-preference-types)))
          (make-apply-button))))))

; (make-preference-window) 

(defun make-preferences-menu-item ()
  (make-instance 'menu-item
    :menu-item-title (add-periods "Preferences")
    :menu-item-action 'make-preference-window
    :help-spec "Set some preferences for CL-HTTP")) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setting preferences: The End 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Finding Documentation in your favorite Browser

(defun action-on-symbol (url-prefix)
  #'(lambda ()
      (let ((string (let* ((f (front-window)))
                      (multiple-value-bind (start end)
                                           (selection-range f)
                        (if (= start end)
                          (progn
                            (ed-select-current-sexp (fred-item f))
                            (multiple-value-bind (start end)
                                                 (selection-range f)
                              (if (= start end)
                                ""
                                (buffer-substring (fred-buffer f) start end))))
                          (buffer-substring (fred-buffer f) start end))))))
        (ccl::open-url (url:coerce-url-string 
                        (concatenate 'string
                                     "http://"
                                     (www-utils:local-host-ip-address)
                                     url-prefix
                                     (string-upcase
                                      (get-string-from-user "Documentation for which symbol?"
                                                            :initial-string string
                                                            :ok-text "Find"))))
                       :activate-p t
                       :no-cache t)))) 

(defun make-find-documentation-menu-item ()
  (make-instance 'menu-item
    :menu-item-title (add-periods "Find Documentation")
    :menu-item-action (action-on-symbol "/cl-http/find-documentation?")
    :help-spec "Find documentation for Lisp symbols
using the browser. The documentation will be generated by the
CL-HTTP server."))

#|
; does not work yet
(defun make-show-documentation-menu-item ()
  (make-instance 'menu-item
    :menu-item-title "Show Documentation"
    :menu-item-action (action-on-symbol "/cl-http/show-documentation?")
    :help-spec "Show documentation for Lisp symbols
using the browser. The documentation will be generated by the
CL-HTTP server."))
|# 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Open some URLs

(defparameter *cl-http-url-menu-title* "Open URL in Browser") 

(defparameter *cl-http-url-descriptions*
  nil
  "A list of (name url-string description) of urls to open in the local browser.
Items can be added with ADD-ITEM-TO-OPEN-URL.
If the URL string starts with a slash '/', then a local
URL will be assumed.")

(defun ADD-ITEM-TO-OPEN-URL (name url description &key update-menu-item)
  "Adds a new item description to *cl-http-url-descriptions*.
If a description with the same name exists, it will be replaced.
If the URL string starts with a slash '/', then a local
URL will be assumed. If update-menu is T (default is NIL) then
the associated menu-item will be updated."
  (check-type name string)
  (check-type url string)
  (assert (or (null description)
              (stringp description))
          (description))
  (let ((item (list name url description))
        (previous-item (assoc name *cl-http-url-descriptions*
                              :test #'string-equal)))
    (if previous-item
      (setf (rest previous-item) (rest item))
      (setf *cl-http-url-descriptions*
            (sort (cons item *cl-http-url-descriptions*)
                  #'string-lessp
                  :key #'first))))
  (when update-menu-item
    (update-open-url-item-if-available))
  (values))

(add-item-to-open-url "Local: this server"
		      "/"
		      "The home of this running Common Lisp Hypermedia Server.")
(add-item-to-open-url "CL-HTTP: home page at MIT AI Lab"
		      "http://www.ai.mit.edu/projects/iiip/doc/cl-http/home-page.html"
		      "The home of the Common Lisp Hypermedia Server.")
(add-item-to-open-url "CL-HTTP: mailing list archive at MIT AI Lab"
		      "http://wilson.ai.mit.edu/cl-http/archives/www-cl.html"
		      "Browse the mailing list archive of www-cl@ai.mit.edu .")
(add-item-to-open-url "Local: available programming projects"
		      "/cl-http/projects.html"
		      "Do some exciting projects to help advancing CL-HTTP.")
(add-item-to-open-url "Local: W4 Web Walker"
		      "/cl-http/w4/w4.html"
		      "W4 is an extension to CL-HTTP for web walking.")
(add-item-to-open-url "Local: W3 presentation substrate"
		      "/cl-http/w3p/w3p.html"
		      "W3 is a subsystem of CL-HTTP for CLIM-like
presentation types on the web.")
(add-item-to-open-url "Local: server configuration"
		      "/cl-http/maintenance/configure-server.html"
		      "Configure your local server with a forms-based interface.")
(add-item-to-open-url "Local: Lisp references"
		      "/cl-http/reference.html"
		      "References for Lisp programmers.")
(add-item-to-open-url "CL-HTTP: FTP site at MIT AI Lab"
		      "ftp://ftp.ai.mit.edu/pub/users/jcma/cl-http/"
		      "The FTP home of the Common Lisp Hypermedia Server.")
(add-item-to-open-url "MCL: Digitool WWW Site"
		      "http://www.digitool.com/"
		      "MCL is a cool development environment from Digitool.")
(add-item-to-open-url "MCL: newsgroup comp.lang.lisp.mcl"
		      "news:comp.lang.lisp.mcl"
		      "MCL is a cool development environment from Digitool.
Talk with other users.")
(add-item-to-open-url "MCL: Digitool FTP Site"
		      "ftp://ftp.digitool.com/"
		      "MCL is a cool development environment from Digitool.
Here you get contributions, updates and patches.")
(add-item-to-open-url "MCL: German distribution"
		      "http://www.lavielle.de/digitool/mcl.html"
		      "Lavielle distributes MCL in Germany.")
(add-item-to-open-url "CL: newsgroup comp.lang.lisp"
		      "news:comp.lang.lisp"
		      "The newsgroup for Lisp users.")

(defclass open-url-item-menu-item (menu-item)
  ((url :accessor menu-item-url :initarg :url))
  (:documentation "On action open the URL with the local browser."))

(defmethod menu-item-action ((item open-url-item-menu-item))
  (process-run-function "Open URL"
                        (lambda (item)
                          (ccl::open-url item :activate-p t))
                        (menu-item-url item)))

(defclass open-local-url-item-menu-item (open-url-item-menu-item)
  ()
  (:documentation "On action open the local URL with the local browser."))

(defmethod menu-item-action ((item open-local-url-item-menu-item))
  (process-run-function "Open URL"
                        (lambda (item)
                          (ccl::open-url
                           (concatenate 'string
                                        "http://"
                                        (www-utils:local-host-domain-name)
                                        ":"
                                        (princ-to-string http:*standard-http-port*)
                                        item)
                           :activate-p t))
                        (menu-item-url item)))

(defun make-open-url-subitems ()
  (flet ((local-url-p (url)
           (and (plusp (length url)) (char= #\/ (aref url 0)))))
    (loop for (name url description) in *cl-http-url-descriptions*
          collect (make-instance (if (local-url-p url)
                                   'open-local-url-item-menu-item
                                   'open-url-item-menu-item)
                    :menu-item-title name
                    :url url
                    :help-spec description))))

(defun update-open-url-item-if-available ()
  "After adding new urls to open, call this function to install
the new items in the menu."
  (let ((menu (find-menu "CL-HTTP")))
    (when menu
      (let ((open-url-menu (find-menu-item menu *cl-http-url-menu-title*)))
        (when open-url-menu
          (apply #'remove-menu-items open-url-menu (menu-items open-url-menu))
          (apply #'add-menu-items open-url-menu (make-open-url-subitems)))))))

(defun make-open-url-item ()
  (make-instance 'menu
    :menu-item-title *cl-http-url-menu-title*
    :help-spec "Opens various canned URLs in the Browser."
    :menu-items (make-open-url-subitems)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Open some URLs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some client menu items 

(defparameter *default-url-string* "http://")

(defun default-url-string ()
  *default-url-string*)

(defun set-default-url-string (url-string)
  (setq *default-url-string* url-string)) 

(defun get-url-from-user (prompt &key (ok-text "Enter")
                                 (initial-string (default-url-string))
                                 (window-title "Basic Client"))
  (let* ((url-string (string-trim '(#\space)
                                  (get-string-from-user prompt
                                                        :initial-string initial-string
                                                        :ok-text ok-text 
                                                        :window-title window-title)))
         (l (length url-string))
         args)
    (when (or (< l 7 )
              (not (string-equal "http://" url-string :start1 0 :end1 7 :start2 0 :end2 7)))
      (push url-string args)
      (push "http://" args))
    #|
(case (aref url-string (1- l))
      (#\/)
      (t (setq args (if args
                      (nconc args (list "/"))
                      (list url-string "/")))))
|#
    (set-default-url-string (if args
                              (apply #'concatenate 'string args)
                              url-string))))

(defmacro define-client-url-operation (name (&key title documentation (prompt "Enter URL:") (button "Show")) &body body)
  `(defun ,(intern (format nil "MAKE-~A-ITEM" name)) ()
     (make-instance 'menu-item
       :menu-item-title (add-periods ,title)
       :help-spec ,documentation
       :menu-item-action #'(lambda ()
                             (process-run-function
                              ,title
                              #'(lambda ()
                                  (let* ((url (get-url-from-user ,prompt :ok-text ,button))
                                         (new-window-p (option-key-p))
                                         (stream (if new-window-p (make-instance 'fred-window :window-title url) *standard-output*)))
                                    ,@body
                                    (when new-window-p (fred-update stream)))))))))

(declaim (ftype (url  &key headers stream) http::show-url-headers)) 

(define-client-url-operation show-url-headers
                             (:title "Show URL Headers"
                                     :documentation "Displays the Headers for URL in the top Listener. Asks for a full HTTP URL.
If you press Option while clicking on the Show button) in a Fred window.")
  (http::show-url-headers url :stream stream))

(declaim (ftype (url  &key headers stream) http::show-url))

(define-client-url-operation show-url 
                             (:title "Show URL"
                                     :documentation "Displays the data for URL in the top Listener. Asks for a full HTTP URL.
If you press Option while clicking on the Show button) in a Fred window.")
  (http::show-url url :stream stream))

(declaim (ftype (url  &key headers stream) http::show-raw-url))

(define-client-url-operation show-raw-url 
                             (:title "Show URL Raw"
                                     :documentation "Displays the data for URL in the top Listener. Asks for a full HTTP URL.
If you press Option while clicking on the Show button) in a Fred window.")
  (http::show-raw-url url :stream stream)) 

(declaim (ftype (url  &key max-forwards headers stream) http::show-url-trace))

(define-client-url-operation show-url-trace
                             (:title "Show URL Trace"
                                     :documentation "Display an HTTP 1.1 TRACE for URL in the top Listener. Asks for a full HTTP URL.
If you press Option while clicking on the Show button) in a Fred window.")
  (http::show-url-trace url :stream stream))

(declaim (ftype (url  &key headers stream) http::show-url-options ))

(define-client-url-operation show-url-options
                             (:title "Show URL Options"
                                     :documentation "Display an HTTP 1.1 OPTIONS for URL in the top Listener. Asks for a full HTTP URL.
If you press Option while clicking on the Show button) in a Fred window.")
  (http::show-url-options url :stream stream)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The main function creating the CL-HTTP menu

(defun divider-item ()
  (make-instance 'menu-item :menu-item-title "-"))

(defparameter *cl-http-menu-mode*
  :development
  "This mode determines how the CL-HTTP menu will look like.
Possible values are :delivery and :development.")

(defparameter *development-items-creators*
  nil
  "An alist of (key . function). The function should return a list of
menu items. These menu items will appear in delivery mode
in the CL-HTTP menu. Add your own functions to extend the menu.")

(defparameter *delivery-items-creators*
  nil
  "An alist of (key . function). The function should return a list of
menu items. These menu items will appear in delivery mode
in the CL-HTTP menu.  Add your own functions to extend the menu.")

(defun development-items ()
  (list (make-about-menu-item)
        (make-release-notes-menu-item)
        (make-license-menu-item)
        (make-mac-readme-menu-item)
        (when (fboundp 'ccl::open-url)
          (make-open-url-item))
        (divider-item)
        (make-preferences-menu-item)
        (make-load-configuration-menu-item)
        (make-save-configuration-menu-item)
        (divider-item)
        (make-startup-menu-item)
        (make-start-server-menu-item)
        (make-stop-server-menu-item)
        #+open-transport(make-reset-network-menu-item)
        (divider-item)
        (make-menu-from-inspect-menu-description)
        (make-find-documentation-menu-item)
        ;                        (make-show-documentation-menu-item)
        (make-menu-from-tool-menu-description)
        (divider-item)
        (make-toggle-var-item 'http:*debug-server*
                              "Debug Server"
                              "On error the server enters a break loop.
Not for production use.")
        (make-toggle-var-item 'ccl::*use-resolver*
                              "Use DNS Resolver"
                              "Controls the MCL DNS resolver code.")
        (make-toggle-var-item 'http:*log-resolve-ip-addresses*
                              "Resolve IP Adresses for Logging"
                              "All IP Addresses will be resolved for the
log. For production servers you may want to turn it off.")
        (when (or (fboundp 'http::show-url)
                  (fboundp 'http::show-url-headers)
                  (boundp 'http::*debug-client*))
          (divider-item))
        (when (boundp 'http::*debug-client*)
          (make-toggle-var-item 'http::*debug-client*
                                "Debug Client"
                                "On error the client enters a break loop.
Not for production use."))
        (when (fboundp 'http::show-url)
          (make-show-url-item))
        (when (fboundp 'http::show-url-headers)
          (make-show-url-headers-item))
        (when (fboundp 'http::show-raw-url)
          (make-show-raw-url-item))
        (when (fboundp 'http::show-url-options)
          (make-show-url-options-item))
        (when (fboundp 'http::show-url-trace)
          (make-show-url-trace-item))
        (when (fboundp 'http::enable-proxy-service ) (divider-item))
        (when (fboundp 'http::enable-proxy-service)
          (make-toggle-menu-item 'http::proxy-service-enabled-p
                                 'http::enable-proxy-service
                                 'http::disable-proxy-service
                                 "Enable Proxy"
                                 "CL-HTTP provides a proxy service."))
        (when (boundp 'http::*debug-proxy*)
          (make-toggle-var-item 'http::*debug-proxy*
                                "Debug Proxy"
                                "On error the proxy enters a break loop.
Not for production use."))
        (when (boundp 'http::*proxy-caching-p*)
          (make-toggle-var-item 'http::*proxy-caching-p*
                                "Use Caching for Proxy"
                                "The Proxy caches requests."))
        
        (divider-item)
        (make-edit-system-menu)
        (make-action-system-menu :compile-load "Compile CL-HTTP System")
        (make-action-system-menu :load "Load CL-HTTP System")
        (make-action-system-menu :compile-load-always "Recompile CL-HTTP System")
        #+ignore(make-action-system-menu :compile "Compile CL-HTTP System")
        (make-load-extensions-menu)))

(defun delivery-items ()
  (list (make-about-menu-item)
        (make-release-notes-menu-item)
        (divider-item)
        (make-preferences-menu-item)
        (make-load-configuration-menu-item)
        (make-save-configuration-menu-item)
        (divider-item)
        (make-startup-menu-item)
        (make-start-server-menu-item)
        (make-stop-server-menu-item)
        #+open-transport(make-reset-network-menu-item)
        (divider-item)
        (make-toggle-var-item 'ccl::*use-resolver*
                              "Use DNS Resolver"
                              "Controls the MCL DNS resolver code.")
        (make-toggle-var-item 'http:*log-resolve-ip-addresses*
                              "Resolve IP Adresses for Logging"
                              "All IP Addresses will be resolved for the
log. For production servers you may want to turn it off."))) 

(defun initialize-menu-creators ()
  (let ((creators (assoc :cl-http *development-items-creators*)))
    (if creators
      (setf (rest creators) #'development-items)
      (setf *development-items-creators*
            (acons :cl-http #'development-items *development-items-creators*))))
  (let ((creators (assoc :cl-http *delivery-items-creators*)))
    (if creators
      (setf (rest creators) #'delivery-items)
      (setf *delivery-items-creators*
            (acons :cl-http #'delivery-items *delivery-items-creators*))))) 

(defun make-cl-http-menu (&optional (mode *cl-http-menu-mode*))
  (initialize-menu-creators)
  (flet ((create-menu (help-spec creators)
           (let ((items (loop for (nil . fn) in creators
                              appending (funcall fn))))
             (make-instance 'menu
               :menu-title *cl-http-menu-name*
               :help-spec help-spec
               :menu-items (remove nil items)))))
    (ecase mode
      (:development (create-menu "Control and Inspect the Common Lisp Hypermedia Server."
                                 *development-items-creators*))
      (:delivery (create-menu "Common Lisp Hypermedia Server."
                              *delivery-items-creators*))))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup the CL-HTTP menu

(defun setup-cl-http-menu (&optional (mode *cl-http-menu-mode*))
  "Basic function to make sure MCL has a CL-HTTP menu.
Mode is :development or :delivery."
  (setf *cl-http-menu* (make-cl-http-menu mode))
  (install-cl-http-menu))

(eval-when (:load-toplevel :execute)
  (setup-cl-http-menu))

; (setup-cl-http-menu :delivery)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Advertize it!

(provide 'cl-http-menu)

(pushnew :cl-http-menu *features*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF - Have Fun!

