;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: cl-user; Base: 10 -*-

(in-package "USER")

;;; You can add a line of the form
;;; (load "C:\\[PATH-TO...]\\CL-HTTP-XX-XX\\ACL\\STARTPC.LISP")
;;; in the ALLEGRO/STARTUP.LSP file to access CL-HTTP from ACLPC
;;;
(if (and (null (find-package "HTTP"))
         (y-or-n-p "Load CL-HTTP?"))
    (load (make-pathname :name "start" :type "lisp"
			 :defaults *load-pathname*))
  (when (find-package "HTTP")
    (format t "~&;;; You can:~%;;; 1. Execute the following to start CL-HTTP:~%> (start)~%;;; 2. Resume CL-HTTP activity after an error:~%> (resume)~%")))
