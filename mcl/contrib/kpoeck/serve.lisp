(in-package :http)

(defun %create-stream ()
  (make-instance 'ccl:modal-ascii-or-binary-tcp-stream
    :element-type '(unsigned-byte 8)
    :host nil
    :port 80
    :writebufsize *http-stream-buffer-size* 
    :commandtimeout 0)
  )

(defun %provide-service-without-ressource (stream client-domain-name client-address)
  (flet ((log-dropped-connection (server)
	   (set-server-status server 504)	;time out status code
	   (log-access server))
	 (handle-unhandled-error (server error)
	   (handler-case
	     (progn
	       (set-server-status server 500)
	       (log-access server)
	       (report-status-unhandled-error error stream (server-request server))
	       (finish-output stream))		; push the output
	     (ccl:connection-lost ())
	     (ccl:host-stopped-responding ())
	     (ccl:network-error ()))
	   ;; log the access after the output has been forced.
	   (log-access server)))
    (let ((server (make-server nil  stream client-domain-name client-address)))
      (let ((*server* server))
	(handler-case-if (not *debug-server*) 
	                 (provide-service server)
	                 ;; catch aborts anywhere within server.-- JCMa 12/30/1994.
	                 (http-abort () (www-utils::abort-http-stream stream))
	                 (ccl:protocol-timeout () (log-dropped-connection server))
	                 (ccl:connection-lost () (log-dropped-connection server))
	                 (ccl:host-stopped-responding () (log-dropped-connection server))
	                 (error (error) (handle-unhandled-error server error)))
	(close stream)
        )
      )
    )
  )
