"http" `("cl-http;*.*"       ,(http-pathname))
   	`("http;*.*"      	 ,(http-pathname))		;10/96 addition
    	`("client;**;*.*"     ,(http-pathname "client"))
    	`("docs;**;*.*"       ,(http-pathname "docs"))
    	`("lispm;**;*.*"      ,(http-pathname "lispm"))
    	`("logs;**;*.*"       ,(http-pathname "log"))
    	`("mac;**;*.*"        ,(http-pathname "mac"))
    	`("server;**;*.*"     ,(http-pathname "server"))
    	`("sources;**;*.*"	 ,(http-pathname))
    	`("standards;**;*.*"  ,(http-pathname "standards"))
    	`("www;**;*.*"        ,(http-pathname "www"))
    	`("root;**;*.*"       ,(rooted-pathname))
    	`("**;*.*"            ,(http-pathname))

"minp"
`("**;*.*.*" ,(translate-logical-pathname "HTTP:clim;clim-sys;"))
`("*.*.*" ,(translate-logical-pathname "HTTP:clim;clim-sys;"))

"aclpc"
`("**;*.*.*" ,(translate-logical-pathname "HTTP:acl;aclpc;"))
`("*.*.*" ,(translate-logical-pathname "HTTP:acl;aclpc;"))

"html-parser"
`("html-parser;*.*.*" ,(translate-logical-pathname "HTTP:html-parser;v7;"))
`("**;*.*.*" ,(translate-logical-pathname "HTTP:html-parser;v7;"))
`("*.*.*" ,(translate-logical-pathname "HTTP:html-parser;v7;"))


