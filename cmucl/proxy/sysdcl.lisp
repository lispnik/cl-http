;;;------------------------------------------------------------------- 
;;;
;;;  SYSTEM DEFINITION CL-HTTP PROXY SERVICE
;;;

(in-package :cl-user)

(defsystem cl-http-proxy
    :source-pathname "HTTP:"
    :components
    ("proxy;utils"
     "proxy;database"			; Database
     "proxy;cache"
     "proxy;proxy-cache"
     "proxy;proxy"			; Proxy Server
     "proxy;documentation"))
