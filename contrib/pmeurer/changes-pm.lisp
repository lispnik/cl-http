
;;---------------------------------------------------------------------------
;; changes for MCL

in "http:mcl;envdecl.lisp"

(note-system-definitions
  (:cl-http "http:mcl;server;sysdcl")		;  Server definition
  (:cl-http-client-substrate "http:mcl;client;sysdcl-substrate")	; Client Substrate
  (:cl-http-proxy "http:mcl;proxy;sysdcl")	; proxy server (includes client substrate)
  (:cl-http-client "http:mcl;client;sysdcl")	;  S-Exp Browser
  (:w4-web-walker "http:mcl;w4;sysdcl")		; W4 Constraint-Guided Web Walker
  (:lambda-ir "http:mcl;lambda-ir;sysdcl")	;  Hybrid Retrieval System
  (:html-parser "http:mcl;html-parser;sysdcl")	;  HTML Parser 
  (:db-auth "http:contrib;pmeurer;dbauth;sysdcl") ; database authentication *** PM
  (:cl-http-examples "http:examples;exports")	; Server Examples
  (:w4-web-walker-demo "http:mcl;w4;sysdcl-examples")
  (:cgi-support "http:mcl;contrib;tnorderhaug;cgi;cgi-sysdcl"))	; CGI Support

(setq *standard-systems* '(:cl-http :cl-http-proxy :cl-http-client :w4-web-walker
                           :db-auth))

;;---------------------------------------------------------------------------
;; Changes for LispWorks

in "http:lw;start.lisp"

;; changed *** PM
(defvar *cl-http-options* '((:compile . :ask)
			    (:debug-info . :ask)
			    (:proxy . :ask)
			    (:w4 . :ask)
                            (:examples . :ask)
			    (:db-auth . :ask) ;; *** PM
			    (:mail-archive . :ask)
			    (:lambda-ir . :ask)
			    (:enable . :ask)))

;; changed *** PM
(defvar *cl-http-options-data*
  `((:compile "Compile CL-HTTP just in case" t)
    (:debug-info "Include full debugging information" nil)
    (:proxy "Include proxy" t)
    (:w4 "Include W4 Web Walker" nil)
    (:db-auth "Include Database Authentication" t) ;; *** PM
    (:examples "Include examples" ,(not *dump-cl-http-name*))
    (:mail-archive "Include mail-archive example" nil)
    (:lambda-ir "Include lambda-ir" nil)
    (:enable "Enable HTTP server" ,(not *dump-cl-http-name*))))

;; changed *** PM
(eval `(defsystem cl-http-start-system (:default-type :system)
	 :members ,(append (and (not *have-clim-sys*)
				'(clim-sys))
			   '(cl-http)
			   (and (cdr (assoc :proxy *cl-http-options*))
				'("HTTP-PROXY"))
			   '(http-base-client)
			   #+W4 '("W4")
			   (and (cdr (assoc :mail-archive *cl-http-options*))
				'("MAIL-ARCHIVE"))
			   (and (cdr (assoc :lambda-ir *cl-http-options*))
                                (if (cdr (assoc :mail-archive *cl-http-options*))
                                    '("LAMBDA-IR-AND-MAIL-ARCHIVE-INDEX")
                                  '("LAMBDA-IR")))
			   ;; *** PM
                           (and (cdr (assoc :db-auth *cl-http-options*))
				'(db-auth))
                           (and (cdr (assoc :examples *cl-http-options*))
				'(cl-http-examples)))
	 :rules ((:in-order-to :compile :all
			       (:requires (:load :serial))))))


;; in "http:lw;server;sysdcl.lisp"

;; added *** PM
(load "HTTP:contrib;pmeurer;dbauth;sysdcl")

;;;;------------------------------------------
;; changes for ACL 5.01

in "http:acl501;load.lisp"

;; added *** PM
(defparameter *files-db-auth*
  '("../contrib/pmeurer/sql/sql/sql-system"
    ("../contrib/pmeurer/dbauth/"
     "sql-authentication"
     "db-authentication")))

;; changed *** PM
(defun build-cl-http (&key force-compile create-fasl 
                           (proxy t)
                           (w3p t)
                           (w4  nil) ; problems loading this
                           (db-auth t)
                           )
  

  (let ((.files-loaded.))
    (if* w3p
       then (pushnew :w3p *features*))
  
    (compile-and-load-files *files-clim-sys* :force force-compile)
    (compile-and-load-files *files-cl-http*  :force force-compile)
    (compile-and-load-files *files-http-client-substrate*  
			    :force force-compile)
  
    (if* proxy
       then (compile-and-load-files *files-http-proxy* 
				    :force force-compile))
  
    (compile-and-load-files *files-http-base-client*  :force force-compile)
  
    ;; *** PM
    (if* db-auth
       then (compile-and-load-files *files-db-auth* :force force-compile))

    (if* w4
       then (compile-and-load-files *files-w4*  :force force-compile))
  

    (if* create-fasl
       then (if* (symbolp create-fasl)
	       then (setq create-fasl "cl-http.fasl"))
	    (format t "writing combined fasl: ~s~%" create-fasl)
	    (copy-files-to (nreverse .files-loaded.) create-fasl))))

