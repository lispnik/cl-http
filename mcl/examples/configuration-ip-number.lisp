;;; -*- Mode: LISP; Package: HTTP; Syntax: ANSI-Common-Lisp -*-

;;; Copyright John C. Mallery,  1994-97.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;; CONFIGURATION FILE FOR CL-HTTP 
;;;
(in-package :http)

;;  Override local host domain name with a special name.
;(setq http:*http-host-name* "MAC-Winner.ai.mit.edu")

;; The name of the mailing list to receive reports about bugs in the server.
(setq *server-maintainer* "Webmaster")

;; The standard mailing list for bug reports.
(setq *server-bug-list*
      #-(OR Genera CMU) "Bug-CL-HTTP"
      #+(OR Genera CMU) (www-utils:http-user-email-address))

;; Controls whether IP addresses are resolved when writing log entries.
;; Production servers should turn this off to avoid the overhead of DNS
;; lookup during logging.
(setq *log-resolve-ip-addresses* nil)

;; Controls whether IP addresses are resolved in all contexts other than
;; logging.
(setq *resolve-ip-addresses* nil)
(setq ccl::*use-resolver* nil)

;; Controls whether host names are DNS resolved and connonicalized on
;; the primary DNS name for the host when the URL is interned. Choices
;; are :always, :preferred, :never. Relevant when *resolve-ip-addresses*
;; is non-null.
(setq url:*url-host-name-resolution* :never)

;; When non-null, :GET and :HEAD methods auto-export static URLs when
;; they have a parent that is exported with a directory export type.
;; When *auto-export* is :ON-DEMAND, pathnames in exported directories
;; are exported as URL when they are first accessed over HTTP rather
;; than at export time.  This feature trades a fast start up of the
;; server for a small addition time when the URL is first accessed. If
;; the value is T, all URLs are exported at export time.
(setq *auto-export* :on-demand)

;; Initialize all server variables for current host
;; (reset-server-local-host-variables) and perform any other required
;; initializations.
(run-server-initializations t)

;; Set the standard port number serviced by this http server.
#-(or Allegro LispWorks Lucid CMU) (set-standard-http-port  80)

;; UNIX tends to lose with some implementation on port 80 so most people use 8000
#+(or Allegro LispWorks Lucid CMU) (set-standard-http-port 8000)

;; Maximum number of simultaneous connections.
(set-maximum-number-of-connections 20.)

;; The default mail host to use in return address when sending any email
;; associated with the WWW server, for example bug reports/
(setq *default-mailer-host* (local-host-domain-name))

;; Recache the bug report list to be *server-bug-list* @ *default-mailer-host*
(http::email-address-for-bug-reports t)

;; Recache the server mail address (used as the from field) to be *server-maintainer* @ *default-mailer-host*
(http::server-mail-address t)

;; The primary store and forward mail host at the local site.  This is the
;; domain name of the mail host. It may also by the IP address.  If this is
;; NIL, no mail will be sent by functions REPORT-BUG and SEND-MAIL-FROM."
#+(OR CCL-3 (and CMU MP))
(setq smtp:*network-mail-host*  nil)

;; This is a list of all store and forward mail hosts accessible from the
;; site.  This mail hosts will be used whenever the primary mail host is
;; unaccessible.  Mail hosts should be listed in decreasing order of priority.
#+CCL-3
(setq smtp:*store-and-forward-mail-hosts* nil)

;; Customize the message issued when server overloaded.
;(setq *overload-message* "This server is operating at capacity now. Please try later.")

;; Configure server logging

;; Common logs are written to the merge of this directory and the local host.
(setq *standard-log-directory* "http:logs;")

;; Controls whether the times in log file entries are in written in Greenwich
;; Mean Time or not.
(setq *log-times-in-gmt* t)

;; Controls whether the file log stream remains open all the time, or it is
;; reopenned for each log transaction. Production servers should keep the log
;; file stream open.
(log-file-stream-stays-open nil)

;; Controls the class of log used for logging.
;; Current options: common-file-log, extended-common-file-log, http-log, extended-http-log
(setq *log-access-log-class* 'common-file-log)

;; Make sure the log object has been initialized.
(ensure-current-log)

;; Write a common log file.
(log-file-logging-on (current-access-logs) t)

;; Show transactions in a log window
;; Turn this off in production servers to save time writing log entries to the notification window.
(log-notifications-on (current-access-logs) t)

;; Don't build a datastructure of the log entries
(log-dynamic-logging-on (current-access-logs) nil)

;; Define default subnet security for all URLs exported by this server.
(define-secure-subnets
  #|"128.52.0.0"|#                              ; MIT AI Lab
  )

;; The pathname holding the disk representation of server authentication
;; information as lisp source code.
(setq *authentication-data-pathname* (pathname "http:pw;authentication-data.lisp"))

;; Controls the security policy for side-effecting methods such
;; as PUT or DELETE.  Each security policy imposes minimum the requirements to invoke these methods. 
;; The following security policies are available:
;;
;;     :ACCESS-CONTROLLED -- Requires URLs to restrict users via either user
;;     authentication or subnet security.
;;     :AUTHENTICATED-USERS -- Requires URLs to restrict users only via user
;;     authentication.
;;     :AUTHENTICATED-USERS-ON-SECURE-SUBNETS -- Requires URLs to restrict users
;;     via both user authentication and subnet security.
;;     :LOCAL-HOST -- Requires users to be on the local host running the server.
;;     :NONE -- No users can invoke side-effecting methods.
;;     :REMOTE-HOST -- Does requires URLs to control access, but respects any
;;     global or URL level access controls.
;;     :SECURE-SUBNETS -- Requires URLs to restrict access to trusted hosts.
(setq *accept-write-methods* :access-controlled)

;; Password data is automatically initialized by the server.
(run-server-launch-initializations t)

;; If you have not already provided a password for the Webmaster,
;; use INTERN-USER to add the password and SAVE-AUTHENTICATION-OBJECT to save
;; the user object.
#+ignore
(save-authentication-object
  (intern-user :server "Webmaster"
               :password "password"
               :groups '("Webmasters")
               :email-address *bug-http-server*))

;; Intialize the random seed for MD5 Digest Authentication.  When first
;; bringing up CL-HTTP at your site, you should evaluate this form 15 times BY
;; HAND in order to build up a reasonable level of randomness in the random
;; seed. Thereafter, the server will automatically initialize this.
#+ignore(digest-authentication-random-seed t)

;; Export export the standard WWW robot control file.
;;(export-url #u"/robots.txt"
;;            :text-file
;;            :pathname (pathname "http:www;robots.text"))

;; The number of threads simultaneously listening for HTTP connections.
#+MCL
(setq *number-of-listening-processes* 5)
#+(or LispWorks Lucid CMU)
(setq *number-of-listening-processes* 1)

;; The standard color scheme to use on automatically generated HTML pages.
;; See HTML:CREATE-COLOR-SCHEME and HTML:WITH-STANDARD-DOCUMENT-BODY.
(setq *standard-color-scheme* (create-color-scheme :background :white
                                                   :foreground :black
                                                   :link :blue
                                                   :visited-link :purple
                                                   :active-link :red))
