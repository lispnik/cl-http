;;;;poor mans periodic tasks in MCL without kernel hacking or undocumented internals
;;;;main idea ccl:*eventhook* may contain a lists of functions that are a hook into the event system
;;;;the functions have the first chance of processing any events 

(in-package :mtl)

(defvar *id-tasks-plist* nil)

(defun install-simple-periodic-task (id funktion interval &key (ignore-errors t))
  (let ((old-function (getf *id-tasks-plist* id))
        (new-function #'(lambda()
                          (when (zerop (mod (get-internal-run-time) interval))
                            (if ignore-errors
                              (ignore-errors (funcall funktion))
                              (funcall funktion)
                              ))
                          nil ;otherwise normal-event-processing won't get the event
                          )))
    (when old-function
      (setq ccl:*eventhook* (delete old-function ccl:*eventhook*)))
    (pushnew new-function ccl:*eventhook*)
    (setf (getf *id-tasks-plist* id)
          new-function)
    )
  )

(defun deinstall-simple-periodic-task (id)
  (setq ccl:*eventhook* (delete (getf *id-tasks-plist* id) ccl:*eventhook*))
  (remf *id-tasks-plist* id)
  )

(defun deinstall-all-periodic-tasks ()
  (do ((list (copy-list *id-tasks-plist*)(rest (rest list))))
      ((endp list))
    (deinstall-simple-periodic-task (first list)))
  )



#|
(install-simple-periodic-task
 :test
 #'(lambda()
     (multiple-value-bind
       (sekunde minute stunde tag monat jahr wochentag egal1 egal2)
       (get-decoded-Time)
       (declare (ignore stunde tag monat jahr wochentag egal1 egal2))
       (format ccl:*top-listener* "~2d:~2d" minute sekunde))
     (let ((a 'a))
       (+ 1 a))
     )
 100)

(deinstall-simple-periodic-task :test)
(deinstall-all-periodic-tasks)


|#
   
