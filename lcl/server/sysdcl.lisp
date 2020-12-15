;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: cl-user; Base: 10 -*-

(in-package "CL-USER")

(defsystem
  cl-http                          
  :source-pathname "HTTP:server;"
;    :depends-on (clim-sys) ; Port your own CLIM-SYS
  :components (#+Genera
	       (:module genera
		:components
		((:system site
		  :source-pathname "SYS:SITE;"
		  :components ("HTTP.TRANSLATIONS"
			       "CL-HTTP.SYSTEM"))
		 (:system lispm
		  :source-pathname "HTTP:LISPM;"
		  :components ("HTTP.TRANSLATIONS"))
		 ;; Utility for finding procedures
		 (:module showable-procedures
		  :components
		  ("SHOWABLE-PROCEDURES")))) 
	       (:system pre-patch
		:source-pathname "HTTP:lcl;server;"
		:components ("pre-package-patch"
			     "package")) 
	       (:module server1
		:source-pathname ""
		:components
		("package"			; HTTP Packages
		 "preliminary"			; Showable procedures
		 "variables"))			; Variables and constants
	       (:system patch
		:source-pathname "HTTP:lcl;server;"
		:components ("patch"))
	       ;; Some portable utils are there
	       (:system mac
		:source-pathname "HTTP:mac;server;"
		:components ("www-utils")) 
		  
	       #+(or genera allegro LispWorks lcl4.2)
	       (:system lispstream
		:source-pathname 
		#+genera    "HTTP:LISPM;SERVER;"
		#+allegro   "HTTP:acl;server;"
		#+LispWorks "HTTP:lw;server;"
		#+lcl4.2    "HTTP:lcl;server;"
		:components
		(#+genera "TCP-LISPM-STREAM"
		 #+allegro "multivalent-stream.lisp"
		 #-genera  "tcp-stream"))
	       #+(or unix LispWorks Allegro lcl4.2 genera)
	       (:system machine
		:source-pathname 
		#-(or Allegro lcl4.2 genera) "HTTP:lw;server;"
		#+Allegro                    "HTTP:acl;server;"
		#+lcl4.2                     "HTTP:lcl;server;"
		#+genera                     "HTTP:lispm;server;"
		:components (#-genera "unix"
			     #+genera "lispm")) 
	       (:module server2
		:source-pathname ""
		:components
		("base64-encoding"		; base 64 utility
		 "md5"				; MD5 utilitity
		 "sha"				; SHA Digests based on Internet FIPS 180
		 "plist"			; property list mixin for CLOS
		 "utils"			; Server utility functions
		 "tokenizer"			; Simple tokenizer
		 "headers"			; Header hacking, including MIME
		 "host"				; Host objects and operations
		 "url"				; URL implementation
		 "html2"			; HTML authoring functions
		 "netscape-1-1"			; Netscape 1.1 html generation
		 "http-conditions"		; HTTP conditions
		 "log"				; Logging 
		 "image-maps"			; Image maps
		 "netscape-2-0"			; Netscape 2.0 HTML generation
		 "netscape-3-0"			; Netscape 3.0 HTML generation
		 "scripts"			; Client-Side Scripts
		 "authentication"		; User authentication
		 "server"			; HTTP Server
		 "cgi"				; Common Gateway Interface
		 "vrml-1-0"))			; VRML 1.0 Generation
	       #+lucid
	       (:system post-patch
		:source-pathname "HTTP:lcl;server;"
		:components ("patch-conditions"))
	       #+(or Allegro LispWorks Lcl4.2)
	       (:system tcp-interface
		:source-pathname 
		#+Allegro   "HTTP:acl;server;"
		#+LispWorks "HTTP:lw;server;"
		#+Lcl4.2    "HTTP:lcl;server;"
		:components ("tcp-interface"))
	       ))

#+(or Allegro LispWorks lcl4.2)
(defsystem cl-http-examples-platform-specific
    :source-pathname #+Allegro   "HTTP:acl;examples;"
                     #+LispWorks "HTTP:lw;examples;"
                     #+lcl4.2    "HTTP:lcl;examples;"
    :components ("configuration"  ; standard-http-port value other than 80
		 "exports"))

(defsystem cl-http-examples
    #+(or Allegro LispWorks lcl4.2)
    :depends-on 
    #+(or Allegro LispWorks lcl4.2)
    (cl-http-examples-platform-specific)
    :source-pathname "HTTP:examples;"
    :components ("configuration"	; server configuration file
		 "exports"		; server example exports
		 ))

#| MJS 31Aug95: comments this out as the files are missing
(defsystem cl-http-clim
     :source-pathname "HTTP:server;"
     :components (#+Genera
		  (:subsystem pointers
			      :components
			      ("SYS:SITE;CL-HTTP-CLIM.SYSTEM"))
		  #+Genera
		  (:subsystem cl-http 
			      :components
			      ("CL-HTTP"))
		  (:module clim
			   :source-pathname ""
			   :components
			   (#-Genera
			    CL-HTTP
			    "HTTP:lispm;server;clim-interface-package"  ; CLIM Interface
			    "HTTP:lispm;server;clim-interface"))))
|#
