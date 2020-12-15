;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4; -*-

;;; Copyright John C. Mallery,  1995-96.
;;; All rights reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; ACTIVITIES
;;;
(in-package :w4)

(define-activity
  trace-walk
  :documentation "Traces the Web Walking activity, reporting activity on REPORT-STREAM."
  :constraint-set ((no-cycles) (depth 2)
                   (header-robots-allowed)
                   ;;(header-content-type (:text :html))
                   ;;(header-last-modification #'< #.(get-universal-time))
                   )
  :operator "JCMa@ai.mit.edu"
  :actions ((trace) (trace-headers) (generate-inferiors))
  :report-stream *standard-output*)

(defun trace-web (url depth &key hosts last-modification content-type
                      directory-p (respect-robot-protocol-p t))
  (with-activity ("tracer" (:report-stream '*standard-output*)
                  :constraints `((depth ,depth)
                                 (no-cycles)
                                 ,.(when directory-p
                                     `((url-parent-subsumed-by-directory-path ,(url:path url))))
                                 ,.(when hosts
                                     `((url-referrer-host ,hosts)))
                                 ,.(when last-modification
                                     `((header-last-modification #'< ,last-modification)))
                                 ,.(when content-type
                                     `((header-content-type ,content-type)))
                                 ,.(when respect-robot-protocol-p
                                     (list '(header-robots-allowed))))
                  :actions `((trace)(trace-headers) (generate-inferiors)))
    (walk url activity)))

#|

(define-activity
  trace-walk
  :documentation "Traces the Web Walking activity, reporting activity on REPORT-STREAM."
  :constraint-set ((no-cycles) (depth 2) (url-host ("www.ai.mit.edu" "wilson.ai.mit.edu" "jefferson.ai.mit.edu"))
                   (header-robots-allowed)
                   ;;(header-content-type (:text :html))
                   ;;(header-last-modification #'< #.(get-universal-time))
                   )
  :operator "JCMa@ai.mit.edu"
  :actions ((trace) (trace-headers) (generate-inferiors))
  :report-stream *standard-output*)

(walk "http://wilson.ai.mit.edu/cl-http/cl-http.html" "trace-walk")
(walk "http://www.ai.mit.edu/" "trace-walk")

|#
