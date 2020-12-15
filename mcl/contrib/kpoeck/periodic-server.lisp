(in-package :http)

(defun %start-periodic-server ()
  ; create a tcp stream
  (let ((tcp-stream (%create-stream)))
    (mtl:install-simple-periodic-task
     tcp-stream
     #'(lambda () (%periodic-listen-for-connections tcp-stream))
     *server-run-interval*))
  )

(defun %periodic-listen-for-connections (stream)
  (when (listen stream)
    (case (http-stream-connection-state stream)
      ((:established)
       (let* ((client-address (www-utils::foreign-host stream))
              (client-domain-name (host-domain-name client-address)))
         ;provide the service
         (%provide-service-without-ressource stream client-domain-name client-address)
         
         ;start a new one
         (%start-periodic-server)
         
         ;kill this process
         (mtl::deinstall-simple-periodic-task stream)
         
         ))
      )
    )
  )



(defun %kill-serving ()
  ;This is not really correct, I should only close the www-streams
  (dolist (s ccl::*open-tcp-streams*)
    (close s))
  (mtl:deinstall-all-periodic-tasks))


#|
(%start-periodic-server)
(%kill-serving)
|#
