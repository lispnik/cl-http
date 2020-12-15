;;; -*- Package: "CL-USER"; -*-

"
<DOCUMENTATION>
 <DESCRIPTION>
  the parser/modeler can be loaded in either of two forms: with network and
  http support by virtue of the presence of a cl-http substrate, or limited
  to access to local files. the distinction is made based on the presence of
  the :CL-HTTP feature at load-time and becomes apparent in two ways.
  <DL>
   <DT>system identifiers</DT>
   <DD>where the http substrate is present, all system id values are interned
       as url instances. this includes pathnames, which are transformed into
       FILE-URL instances. when subsequently dereferenced, HTTP-URL instances
       are passed directly through the network services interface while FILE-URL
       instances effect file-system access.<BR/>
       where the http substrate is absent, all system id values are interned
       as pathnames and local access is the only form supported.</DD>
   <DT>default environment</DT>
   <DD>where http is present, the default context is maintained as an
       url-namestring.<BR/>
       where it is absent, the default context is maintained as a pathname.
       </DD>
   </DL>
  in order to provide a uniform interface under both conditions, standard
  cl-http interface functions are shadowed with versions which adapt to the
  circumstances.
  </DESCRIPTION>
 </DOCUMENTATION>
"
(in-package "CL-USER")

(defPackage "WWW-UTILS")

(defPackage "URL"
  (:use "WWW-UTILS" "COMMON-LISP")
  (:export "NAME-STRING"
           "URL"
           "FILE-URL"
           "HTTP-URL"
           "PARSE-URL"
           "TRANSLATED-PATHNAME"))

(defPackage "HTTP"
  (:use "WWW-UTILS" "COMMON-LISP")
  (:export "*LOCAL-CONTEXT*"
           "LOCAL-CONTEXT"))

(in-package "WWW-UTILS")

(in-package "HTTP")

(defParameter *local-context* "")

(defMethod local-context () (or *local-context* ""))

(in-package "URL")



"XMLP"
