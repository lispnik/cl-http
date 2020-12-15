(in-package :mtl)

;;;;poor mans periodic tasks in MCL with kernel hacking using undocumented internals

(defun install-simple-periodic-task (id funktion interval &key (ignore-errors t))
  (ccl::%install-periodic-task
   id
   (if ignore-errors
     #'(lambda()
         (ignore-errors
          (funcall funktion)))
     funktion)
   interval
   )
  )

(defun deinstall-simple-periodic-task (id)
  (ccl::%remove-periodic-task id)
  )

(defun deinstall-all-periodic-tasks ()
  (dolist (task ccl::*%periodic-tasks%*)
    (unless (eq task ccl::*event-dispatch-task*)
      (deinstall-simple-periodic-task task)))
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
