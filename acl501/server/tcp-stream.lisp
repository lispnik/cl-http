;; tcp stream subset

(defpackage "IPC"
  (:use) (:export "FRANZ-HOSTNAME" 
		  "GETDOMAINNAME" 
		  "INTERNET-ADDRESS" 
		  "INTERNET-ADDRESS-DOMAIN" 
		  "PROTOCOL-ERROR" 
		  "TCP-CLIENT-ALIVE" 
		  "TCP-CLIENT-STREAM" 
		  "TCP-SERVER-STREAM" 
		  "STREAM-READ"))


(in-package "IPC")


(define-condition unknown-host-name (simple-error)
  ((address :initform 0 :initarg :address))
  (:default-initargs :format-control "host name not found for address ~D."))




