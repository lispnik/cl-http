(in-package "CL-USER")

(defsystem http-base-client
    :source-pathname "HTTP:client;"
    :components (#+Genera
		 (:system pointers
			     :components
			     ("sys:site;http.translations"
			      "sys:site;http-client.system"))
		 #+Genera
		 (:module cl-http
			  :source-pathname ""
			  :components
			  (cl-http))
		 #+Genera
		 (:module image-substrate ;lispm image substrate for client
			  :source-pathname ""
			  :components
			  (image-substrate))
		 (:module client
			  :source-pathname ""
			  :components
			  (#-LispWorks image-substrate ; fails on 36xx machines  7/29/94 -- Benjamin.
				       cl-http
				       #+Genera
				       "HTTP:lispm;client;images-lispm"			; Image hacking for lispm client
				       #+Genera
				       "HTTP:lispm;client;lispm"
				       "client")))) ; WWW Client





