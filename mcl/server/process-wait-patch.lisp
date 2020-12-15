;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: fcl-user; Base: 10 -*-

;;; Fix PROCESS-WAIT lossage in MCL 4.0 & 3.1

;;; At 4:02 AM -0400 10/20/96, John C. Mallery wrote:
;;; >MCL 3.1f2 get an error running process-wait-with-timeout because
;;; >process-wait is invoked recursively.  This impairs the performance
;;; >of CL-HTTP 60.50 under 3.1
;;; >
;;; >A backtrace is attached.
;;; >
;;; >The bug can be reproduced by running cl-http under 3.1 just turn on
;;; >debugging with (http:debug-server) Note that restarts are available to resume execution.
;;; 
;;; I added this feature between MCL 4.0b2 and MCL 4.0f1. If a process-wait
;;; function calls process-wait, it signals an error in the offending process.
;;; This doesn't give you enough information to figure out exactly where
;;; the offending recursive call was, but it's enough to get there if
;;; you look around a bit. The reason I did this was that we discovered that
;;; you could badly crash your machine by causing recursive process waits.
;;; Apparently, CL-HTTP has been getting away with them for a while now.
;;; Just lucky, I guess. The recursive process-wait is, I believe, in the
;;; following call sequence:
;;; 
;;;   http::http-input-data-available-p
;;;     process-wait-with-timeout
;;;       process-wait
;;;         [in another process]
;;;         ccl::scheduler
;;;           ccl::%process-wait-p
;;;             data-available-p  (inside http::http-input-data-available-p)
;;;               listen
;;;                 (method stream-listen (basic-tcp-stream))
;;;                   tcp-reinitialize-listening-connection
;;;                     %tcp-wait-for-connection-state
;;;                       process-wait-with-timeout
;;; 
;;; It may be due to timing that you have only seen this in MCL 3.1, but it
;;; could certainly happen in 4.0 as well.
;;; 
;;; I can appreciate that eliminating the recursive process-wait would
;;; be inconvenient for you. In order to fix it from my side, however,
;;; I need to figure out what it means to call process-wait from within
;;; a process-wait function. The only thing that makes sense to me is
;;; that it means that the recursive process-wait function temporarily
;;; replaces the original as the process'es process-wait function. The
;;; former process-wait function needs to be pushed on a stack. When the
;;; recursive process-wait function returns true, the former one is
;;; reinstated. This is not hard to do except for one thing that I can't
;;; figure out a way around. The problem is that the recursive process-wait
;;; happens in another process. The wait function or arguments may be or contain
;;; objects with dynamic-extent, so the wait needs to actually happen on that
;;; process'es stack. It can't be right to cause a process to wait just because
;;; another process recursively calls process-wait. There's also some wierd stuff
;;; with the scheduler being called recursively, but I could arrange for the
;;; outer calls to be thrown out of if I thought this solution was correct.
;;; 
;;; I suppose the recursive process-wait could throw out and cause the scheduler
;;; to behave as if the initial process-wait function returned NIL. When the
;;; original process-wait function is called again, it will likely do the same
;;; thing it did the last time, but this time the recursive process-wait function
;;; may return true right away. I hate writing code for "likely" scenarios. The
;;; initial process-wait function may also not appreciate being thrown through
;;; unexpectedly. In particular, tcp-reinitialize-listening-connection has a
;;; passive open pending and it needs to wait for that to complete before attempting
;;; to do more I/O on the CONN-PB of the connection.
;;; 
;;; The enclosed patch will stop process-wait from erroring, which will make
;;; CL-HTTP work as it did before. I don't suggest using this patch for anything
;;; but a temporary fix. The right fix is to stop your code from doing recursive
;;; process-wait. It seems to me that the right way to do this is to remove
;;; from the STREAM-LISTEN method the code that reinitiates a dead connection.
;;; STREAM-LISTEN should just do enough very low overhead stuff to tell if a
;;; character is waiting. Some other part of your system should be responsible for
;;; noticing that a stream has closed and needs to be reinitialized. But I know
;;; very little about the architecture of CL-HTTP or how TCP streams work on the
;;; other platforms, so my suggestion may be way off-base.
;;; 
;;; The only testing I did of the patch I did was to ensure that it compiles and
;;; loads without error into MCL 4.0 and that the Lisp continues to work with
;;; the patch installed. I did not test it with CL-HTTP, nor did I test it with
;;; MCL 3.1. Since 3.1 shared most of its code with 4.0, I can't imagine that it
;;; will have a problem there.
;;; 


;;; recursive-process-wait-OK.lisp
;;;
;;; Don't error on recursive process-wait.
;;; Do the wrong thing as before 4.0f1

(in-package :ccl)

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

(defun process-wait (whostate function &rest args)
  (declare (dynamic-extent args))
  (or (apply function args)
      (let* ((p *current-process*))
        (if nil  ; *in-process-wait-function*
          (recursive-process-wait *in-process-wait-function*)
          (unwind-protect
            (progn
              (setf (process.wait-function p) (require-type function 'function)
                    (process.wait-argument-list p) args)
              (suspend-current-process whostate))
            (setf (process.wait-function p) nil
                  (process.wait-argument-list p) nil)))))
  nil)


)
