;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: future-common-lisp; Base: 10 -*-
;;;
;;; Copyright Rainer Joswig and John C. Mallery,  1995.
;;; All rights reserved.
;;;


(in-package :future-common-lisp)

(eval-when (load eval compile)
   
  (defpackage :smtp
    (:use :future-common-lisp :www-utils)
    #+MCL
    (:import-from "CCL"
     "STREAM-TYI" "STREAM-TYO" "TCP-STREAM")
    (:export
      ;; variables
      "*DEBUG-MAILER*"
      "*NETWORK-MAIL-HOST*"
      "*STORE-AND-FORWARD-MAIL-HOSTS*"
      ;; conditions
      "MAILER-ERROR"
      "MAILER-TEMPORARY-ERROR"
      "MAILER-PERMANENT-ERROR"
      "MAILER-PROTOCOL-ERROR"
      "MAILER-TIMEOUT"
      "MAILER-CONNECTION-TIMEOUT"
      "MAILER-UNKNOWN-RECIPIENT"
      "MAILER-INCOMPLETE-DELIVERY-ERROR"
      "SMTP-ERROR"
      "UNKNOWN-SMTP-ERROR"
      "SMTP-BAD-COMMAND-SEQUENCE"
      "SMTP-HOST-NOT-AVAILABLE"
      "SMTP-INSUFFICIENT-SYSTEM-STORAGE"
      "SMTP-INVALID-MAILBOX-NAME"
      "SMTP-LOCAL-ERROR"
      "SMTP-MAILBOX-NOT-FOUND"
      "SMTP-MAILBOX-NOT-LOCAL"
      "SMTP-MAILBOX-UNAVAILABLE"
      "SMTP-NO-MORE-ROOM"
      "SMTP-PROTOCOL-VIOLATION"
      "SMTP-SYNTAX-ERROR"
      "SMTP-TRANSACTION-FAILED"
      "SMTP-UNIMPLEMENTED-COMMAND"
      "SMTP-UNIMPLEMENTED-PARAMETER"
      "SMTP-UNRECOGNIZED-COMMAND"
      ;; classes
      "MAILER-STREAM"
      ;; functions and macros
      "%SEND-EMAIL-MESSAGE"
      "DEBUG-MAILER"
      "SEND-MAIL"
      "STORE-AND-FORWARD-MAILER-HOSTS" 
      "WITH-NETWORK-MAIL-HOST")))
