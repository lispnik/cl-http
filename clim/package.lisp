;;;   -*- Mode: LISP; Package: CL-USER (really FUTURE-COMMON-LISP-USER); BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-

;;;
;;; (c) Copyright  1995, John C. Mallery.
;;;     All Rights Reserved.
;;;


;;;------------------------------------------------------------------- 
;;;
;;; HTTP-INTERFACE PACKAGE
;;;

(defpackage http-interface
   (:nicknames httpi)
   (:shadowing-import-from clim #+MCL "BOOLEAN" "INTERACTIVE-STREAM-P" "PATHNAME" "PATH" "PORT")
   (:use future-common-lisp clim www-utils url http html)
   ;; consider exporting some of these from HTTP if they are generally useful and stable.
   (:shadowing-import-from 
     http
     "*ALL-LOGS*"
     "*LOG*"
     "*LOG-LINE-PARSER-ALIST*"
     ;; "AUTHENTIFICATION-LOGNAME"
     "BYTES"
     "DYNAMIC-LOGGING"
     "FILE-LOGGING"
     "FILENAME"
     "HOST"
     "HOST-NAME"
     "HOST-NAME-SPACE"
     ;; "INITIALIZE-LOG"
     ;; "INTERN-LOG"
     "LOG-DYNAMIC-LOGGING"
     "LOG-FILE-LOGGING"
     "LOG-FILENAME"
     "LOG-NAME"
     "LOG-NOTIFICATION"
     "N-TRANSACTIONS"
     "NAME"
     "NOTIFICATION"
     "PARSE-LOG-FILE"
     "REGISTER-HOST"
     ;; "REMOTE-LOGNAME"
     "STATUS"
     "TIME-STAMP"
     "TRANSACTION"
     "URL-TABLE"))
