(in-package :http)

(defvar *test-stream*)

(defun %start-single-server ()
  ;open the tcp port
  (setq *test-stream* (%create-stream))
  ;start listening for connections
  (loop
    (when (listen *test-stream*)
      ;request is coming !!!!
      (case (http-stream-connection-state *test-stream*)
        ((:established)
         (let* ((client-address (www-utils::foreign-host *test-stream*))
              (client-domain-name (host-domain-name client-address)))
           (%provide-service-without-ressource *test-stream* client-domain-name client-address)
           (return)
           ))
        ))))

(defun %start-serving ()
  (unwind-protect 
    (loop
      (%start-single-server))
    (close *test-stream*))
  )

#|
(loop
  (%start-single-server))
|#

#|
(defmethod stream-close ((s tcp-stream) &aux (conn (slot-value s 'conn)))
 |#
