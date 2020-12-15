;;;
;;; **********************************************************************
;;; This code was written by Douglas T. Crosher and has been placed in
;;; the Public domain, and is provided 'as is'.
;;;
;;; **********************************************************************
;;;
;;; Example initialisation file for CL-HTTP CMUCL; to be loaded into a
;;; dumped core.
;;;

(in-package "USER")

;;; May need to disable page protection if it causes trouble.
#+(and gencgc nil)
(setf (alien:extern-alien "enable_page_protection" alien:unsigned) 0)

;;; Load the configuration, and default demos if not already loaded.
;;;
(if (member :cl-http-examples *cl-http-options*)
    ;; Even if the examples are loaded, need to restart the log process.
    (load "http:examples;configuration")
    (load-system 'cl-http-examples
		 :compile-during-load ()
		 :load-source-if-no-binary t
		 :bother-user-if-no-binary nil))

#+W4
(unless (member :w4-web-walker-demo *cl-http-options*)
  (load-system 'w4-web-walker-demo
	       :compile-during-load ()
	       :load-source-if-no-binary t
	       :bother-user-if-no-binary nil))

#+gencgc (gc :full t)
#-gencgc (purify)

(when (or (member :enable *cl-http-options*)
	  (and (member :ask-enable *cl-http-options*)
	       (y-or-n-p "Enable HTTP Service now? ")))
  (http:enable-http-service))

;;; Multi-processing setup.
#+MP
(progn
  ;; Setup the event server timeout so that an interactive process can
  ;; act as the idle loop.
  (setf lisp::*max-event-to-sec* 0
	lisp::*max-event-to-usec* 500000)

  ;; Setup the initial process as the idle process.
  (setf mp::*idle-process* mp::*initial-process*
	mp::*idle-loop-timeout* 0.1d0)

  ;; Start a background SIGALRM driven process-yield, every 10 seconds,
  ;; in case of stuck connections. E.g. The opening of remote
  ;; connections can lockup. Since CMUCL is not yet interrupt safe this
  ;; is not suggested.
  #+nil (mp::start-sigalrm-yield 10 0)

  ;; If not interactive then run the idle loop.
  #+nil (mp::idle-process-loop)
) ; end progn MP
