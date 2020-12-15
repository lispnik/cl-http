;;; -*- mode: lisp; package: "CL-USER"; -*-

"
<DOCUMENTATION>
 <DESCRIPTION>
  system definition for an xml parser / dom-modeler
  </DESCRIPTION>
 <COPYRIGHT>
  <VERSION>0.44</VERSION><AUTHOR>(C) mecom gmbh 19990430</AUTHOR>
  available only from the cl-http repository and NOT to be REdistributed
  in ANY form. see cl-xml.html.
  </COPYRIGHT>
 <CHRONOLOGY>
  <DELTA DATE='19980807'>
   support for wd-19980802 namspaces. this included:
   <OL>
    <LI>explicitly binding namespace packages in a dynamic context, rather than using the prefixes.
    this has the advantage that the printname is guaranteed left untouched and renaming is
    not necessary when unwinding.</LI>
    <LI>restructuring functional structure to eliminate type-based dispatch in the parsing functions.
    this because more of the parse must be performed before it is possible to determine the
    type of the element since the prefix binding may be included in the attributes
    </OL></DELTA>
  <DELTA DATE='19980814>
   *-declaration instead of dtd-element-declaration, dtd-attlist, dtd-notation.
   dtd-element-reference eliminated.
   </DELTA>
  <DELTA DATE='19981215' VERSION='0.40'>
    <LI>attribute-based namespaces</LI>
    <LI>eliminated dependance on the reader</LI>
    <LI>included decoding reader</LI>
    <LI>class names (more or less) conforming to REC-DOM-Level-1-19981901</LI>
    <LI>clearer functional structure which parallels the XML standard's
        syntax and splits the various phases better to enable incorporating
        a generic parser</LI>
    <LI>more consistent error handling and conditions</LI>
    <LI>entity implementation conformant to xml standard dereferencing
        rules</LI>
    <LI>declaration binding direct to symbols</LI>
   </DELTA>
   <DELTA DATE='19990430' VERSION='0.44'>
    <LI>x-pointers</LI>
    </DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
"

(in-package "CL-USER")

(defParameter *xml-source-directory* nil)
(when *load-truename*
  (setf *xml-source-directory* (make-pathname
                                :directory (pathname-directory *load-truename*)
                                 :device (pathname-device *load-truename*)
                                 :host (pathname-host *load-truename*))))

(defParameter *xml-fasl-directory*
  (or (probe-file (merge-pathnames (make-pathname :directory
                                                  `(:relative
                                                    #+(and MCL (not M68K)) "ppcFASL"
                                                    #+(and MCL M68K) "m68kFASL"
                                                    #+ALLEGRO "allFASL"
                                                    #+LISPWORKS "lwFASL"))
                                   *xml-source-directory*))
      *xml-source-directory*))

(defParameter *xml-parser-files*
  `(#-cl-http
    "cl-http-compatibility"        ; definitions to minimize variations between access substrates
    "exports"                      ; interface symbols
    "types"                        ; 
    "utilities"                    ;
    "tokens"                       ; singleton classes for syntactic tokens
    "global-bindings"              ; define interface variables and functions
    "interface"                    ; the interface to the parser and data manipulation
                                   ; functions in terms of direct CLOS accessors and
                                   ; auxiliaries
    
    "character-classes"            ; predicates as required by the spec
    "xml-error"                    ; an absolutely minimal error facility
    "xml-node"                     ; the abstract constituent node class.
    "namespaces"                   ; xml-specific namespace (ie. package) management (creation, and prefix binding)
    "encoded-stream"               ; a wrapper stream which handles decoding, and entity inlining
    "stream-state"                 ; information on the state of the parsed stream for error messages
    "markup-reader"                ; primitive read/macro functions for tokenizing a stream of marked up data.
    "markup-printer"               ; primitive serialzation functions
    "document-type"                ; caches declarations within a document
    "dtd"                          ; declaration management
    "element-model"                ; (see below for the model compiler)
    "element-declaration"          ; structural / method definitions for the specific
    "attlist-declaration"          ; components and reader extensions for dtd components
    "entity-declaration"           ; 
    "entity-reference"             ; 
    "notation-declaration"         ;
    "document"                     ;
    "text-node"                    ;
    "attribute"                    ;
    "processing-instruction"       ; 
    "xml-decl-pi"                  ; special pi for the document and encoding declaration 
    "namespace-pi"                 ; special pi for namespace bindings
    "conditional-section"
    "element"
    "comment"
    "xml-parser"                   ; interface for complete xml documents
    "dtd-parser"                   ; interface for internal and external subset
    "element-model-compiler"       ; a compiler for model verification predicates
    "xptr"                         ; support for xml pointers, with access macros
    "clos-dom"                     ; an example of how to construct element/dom instances.
                                   ; including rewriting xml to html 
    ;"clos-dom-xptr"               ; an alternative which uses xptrs to locate nodes - still in progress

    #+:mcl
    "menu"                         ; menus to inspect definitions
    
    #+:cl-http
    ,@'("server"                    ; bare-bones support for document generation within the cl-http server 
       "dom-server"                ; serving clos documentation through dom intermediaries. the respective url's are defined in
                                   ; "dom-exports", which is to be loaded when the server is running only.
       "rdf"                       ; in preparation for urn support prepare the rdf/rfds packages
       )
    ))


(labels
  ((operate-on-file-set
       (files op &aux paths defaults (*readtable* (copy-readtable *readtable*)))
     (setf defaults (merge-pathnames (make-pathname :type "lisp")
                                    *xml-source-directory*))
     ;; this is necessary in order that acl can read macintosh source as is
     #+ALLEGRO
     (progn
       (set-syntax-from-char #\return #\space *readtable*)
       (set-macro-character #\; #'|read-mac-comment| nil *readtable*)
       (set-macro-character #\; #'|read-mac-comment| nil))
     (setf paths (mapcar #'(lambda (filename)
                             (or (probe-file (make-pathname :name filename
                                                            :defaults defaults))
                                 (error "file missing: ~s." filename)))
                         files))
     (when (find nil paths)
       (error "xml source files missing: ~s."
              (remove nil (mapcar #'(lambda (f p) (unless p f))
                                  files paths))))
     (mapcar op paths))
   (fasl-pathname (source)
     (make-pathname
      :name (pathname-name source)
      :type #+MCL (pathname-type *.fasl-pathname*)
      #+ALLEGRO "fasl"
      #+LISPWORKS "fasl"
      :defaults *xml-fasl-directory*))
   (needs-compiling? (source fasl
                             &aux (st (file-write-date source)) (ft (file-write-date fasl)))
     (or (null ft) (> st ft)))
   (|read-mac-comment|
       (stream char &optional (eol '(#\newline #\return #\linefeed)))
     (loop (setf char (read-char stream nil nil))
           (unless char (return))
           (when (find char eol) (return)))
     (values)))
  
  (defun load-xml
         (&key
          (files *xml-parser-files*)
          ((:verbose *load-verbose*) nil))
    (operate-on-file-set files
                         #'(lambda (source
                                    &aux (fasl (fasl-pathname source)))
                             (load (if (needs-compiling? source fasl)
                                     source fasl)))))

  (defun compile-xml
         (&key
          (files *xml-parser-files*)
          (force nil)
          ((:verbose *compile-verbose*) nil)
          load)
    (operate-on-file-set files
                         #'(lambda (source
                                    &aux (fasl (fasl-pathname source)))
                             (cond ((or force (needs-compiling? source fasl))
                                    (compile-file source :output-file fasl))
                                   (*compile-verbose*
                                    (format *trace-output*
                                            "~%;Skipping Compile: ~s." source)))
                             (when load
                               (let ((*warn-if-redefine* nil)
                                     (*load-verbose* *compile-verbose*))
                                 (load fasl)))
                             fasl))))

(load-xml :verbose t)
;; this in addition, since the loading function binds a readtable
(xmlp::patch-readtable-for-xml)

;(compile-xml)


(provide "XML")
