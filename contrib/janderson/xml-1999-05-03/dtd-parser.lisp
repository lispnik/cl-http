;;; -*- mode: LISP; package: "XML-PARSER"; -*-

"
 <DOCUMENTATION>
 <DESCRIPTION>
 <P>
 a stream parser for document type definitions requires only a sequence of
 declaration entities. in addition pi's or conditional sections can appear,
 but they're secondary. entity references are simply expanded in-line.
 </DESCRIPTION>
 <COPYRIGHT HREF='defsystem.lisp|root().descendant(1,COPYRIGHT)'/>
 <CHRONOLOGY>
  <DELTA><DATE>19971210</DATE>
  changed the interface to permit the dtd to be specified as a parameter
   </DELTA>
  <DELTA><DATE>19971218</DATE>
   eliminated the DTD-DOCUMENT. it serves no purpose, since a dtd
   cannot stand alone. it is read (normally) as a dependant on an xml document
   where the dtd name and the effective root element is determined. as such,
   in order to read the dtd stream, the dtd itself must have already been
   created in order to accept the internal subset and the name. so it's now
   passed as a parameter as the standard call. a name is supported in addition
   as a convenience interface.
   </DELTA>
  <DELTA><DATE>19980406</DATE>
   WD-names-19980327: name space management with pi's</DELTA>
  <DELTA><DATE>19980814</DATE>
   <UL>
   <LI>WD-names-19980802: namespace management as now spec'd (based on attributes
   only) is insufficient to guarantee universal identifiers. as a supplement,
   namespaces and bindings are support for document type definitions.
   this requires a dtd-specific frame and support for a pi which binds prefixes
   to namespaces.</LI>
   <LI>in the presence of 'universal' names it is not necessary to collect
    definitions in a dtd-object. the 'dtd' is now exactly the collection of
    symbols which name declarations within the dtd. READ-DTD-STREAM thus does
    not need the second argument
   </LI>
   </UL></DELTA>
  <DELTA><DATE>19981114</DATE>
   adapted to read-production interface;
   eof handled as a condition;
   augmented error reporting</DELTA>
  <DELTA><DATE>19981214</DATE>
   permit conditions sections as parse results</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
 "

(in-package "XML-PARSER")


"
<H3>'DTD' parsing</H3>
<P>
there are four productions which concern document type declarations:
<PRE>
  [28] doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S?
                         ('[' (markupdecl | PEReference | S)* ']' S?)? '>'
  [29]  markupdecl ::= elementdecl | AttlistDecl
                       | EntityDecl | NotationDecl
                       | PI | Comment
  [30]  extSubset      ::= TextDecl? extSubsetDecl
  [31]  extSubsetDecl  ::=  ( markupdecl | conditionalSect | PEReference | S )*
</PRE>
they are handled uniformly by a function which reads and processes the
declaration entities, one after the other, until EOF. the entity set is not
saved and returned: since the conditional sections permit nesting, the entities
are bound into the currently collected dtd as a side-effect during the
processing step.
"

(defMethod read-production
           ((production (eql 'xml-1.0::dtd)) *markup-stream*
            &aux
            (*preserve-whitespace* nil)
            (*namespaces* *namespaces*)
            (*processed-node* *processed-node*)
            (*comment-target* *comment-target*)
            next-node)
  (handler-bind
    ((end-of-file #'(lambda (condition)
                      (declare (ignore condition))
                      (return-from read-production *processed-node*)))
     (section-end-condition
      #'(lambda (condition)
          (declare (ignore condition))
          (return-from read-production *processed-node*)))
     (stream-error #'(lambda (condition) condition))
     (error #'(lambda (condition)
                (xml-warn production
                          "error while reading dtd:~%~a"
                          condition)
                (when *error-handler* (funcall *error-handler* condition))
                condition)))
    ;; check to see that there is in fact something in the stream
    (unless (peek-char *tag-open-char* *markup-stream* nil)
      (xml-form-error production "document contains no declarations."))
    (loop
      (setf next-node (read-production 'xml-1.0::markupdecl *markup-stream*))
      (typecase next-node
        (xml-decl-pi
         ;; should check for duplicates and double-check that the specified
         ;; encoding matches that which was detected.
         )
        (comment
         (xml-node.append-element *document* next-node))
        (declaration-markup
         ;; permit anything which complies
         )
        (t
         (xml-form-error production
                             "erroneous dtd content: ~s."
                             next-node))))

    ;; no extra reduction step since nothing is 'produced' - we just read to
    ;; eof (nb. the handler-bind above) and return the last entity read
    ;; any bindings performed in the processing step. 
    ;; the respective subsets are not reified, as it they associated with the
    ;; current document/doctype instance.
    ;; never gets here ...
    *processed-node*))


"
<H3> external subset parsing</H3>
<P>
the external subset can be parsed from several kinds of source.
files and http connections dominate.
all sources are transformed into ncoded streams, which are then parsed as the
generic XML-1.0::DTD production.
the respective transforming method binds a *local-environment* if the
location is named in order to subsequent references to be resolved relatively.
</P>
"
(defMethod read-production
           ((production (eql 'xml-1.0::extSubset)) (stream encoded-stream)
            &aux (*in-external-subset* t))
  (read-production 'xml-1.0::dtd stream)
  (when *document-type*
    (document-type.external-subset *document-type*)))

(defMethod read-production
           ((production (eql 'xml-1.0::extSubset)) (source pathname))
  (when (probe-file source)
    (with-local-context source
      (with-open-file (stream source :direction :input)
        (values (read-production production stream)
                (file-write-date source))))))
           
(defMethod read-production
           ((production (eql 'xml-1.0::extSubset)) (source stream))
  (read-production production (encoded-stream source)))

(defMethod read-production
           ((production (eql 'xml-1.0::extSubset)) (source string))
  (with-input-from-string (stream source)
    (read-production production (encoded-stream stream))))

#+CL-HTTP
(defMethod read-production :around
           ((production (eql 'xml-1.0::extSubset)) (source file-url)
            &aux pathname)
  (with-local-context source
    (setf pathname (translated-pathname source))
    (with-open-file (stream pathname
                            :direction :input :element-type 'unsigned-byte)
      (values (read-production production stream)
              (file-write-date pathname)))))

#+CL-HTTP
(defMethod read-production :around
           ((production (eql 'xml-1.0::extSubset)) (source http-url))
  (with-local-context source
    (http:with-http-request (source :get)
      (values (read-production production source)
              (let ((mod (http:get-header :last-modification)))
                (when mod (ignore-errors (parse-integer mod))))))))

(defMethod read-production
           ((production (eql 'xml-1.0::extSubset)) (source null))
  nil)

(defMethod read-production
           ((production (eql 'xml-1.0::extSubset)) (source t))
  (warn "external subset source not supported: ~s." source))

#+CCL
(defMethod read-production
           ((production (eql 'xml-1.0::extSubset)) (source fred-window))
  (with-local-context source
    (read-production production (make-concatenated-stream source))))

"
<H3> internal subset parsing</H3>
<P>
the internal subset is intended to be parsed from the same source as the
document. therefore it should already be an encoded stream and does not require
any special handling.
it just binds a collector for the declarations
files and http connections dominate.
all sources are transformed into ncoded streams, which are then parsed as the
generic XML-1.0::DTD production.
the respective transforming method binds a *local-environment* if the
location is named in order to subsequent references to be resolved relatively.
</P>
"

(defMethod read-production
           ((production (eql 'xml-1.0::intSubset)) stream)
  (let ((*document-type* (make-instance 'document-type :name nil)))
    (with-namespace-frame
      (read-production 'xml-1.0::dtd stream)
      (document-type.internal-subset *document-type*))))


(defMethod read-dtd-stream
           ((stream t)
            &rest args
            &key
            ((:attribute-class *attribute-class*)
             *attribute-class*))
  (declare (dynamic-extent args))
  (with-parser-bindings args
    (read-production 'xml-1.0::extSubset stream)))


;;;
;;; load a dtd from a source, defaulting the name as required.
;;; when loading from a pathname, the system id is generated with name-string
;;; in order that it be correct for the given the access substrate.

(defMethod load-dtd
           ((name null) (source pathname))
  (load-dtd (pathname-name source) source))

(defMethod load-dtd
           ((name t) (source pathname))
  (load-dtd name (name-string source)))

(defMethod load-dtd
           ((name string) (source t))
  (load-dtd (intern-name name) source))

(defMethod load-dtd
           ((name symbol) (source string)
            &aux
            (document-type (make-instance 'document-type :name name
                                          :system-id source)))
  (get-external-subset-content document-type)
  document-type)

#+CCL
(defMethod load-dtd
           ((name symbol) (source fred-window)
            &aux
            (document-type
             (make-instance 'document-type :name name
                            :system-id (name-string source))))
  (stream-position source 0)
  (setf (document-type.external-subset document-type)
        (read-production 'xml-1.0::ExtSubset source))
  document-type)
  



"XMLP"



