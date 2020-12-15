;;; -*- :mode: lisp; package: "XML-PARSER"; -*-

#|
<DOCUMENTATION>
<DESCRIPTION>
the "document" container for xml data streams
</DESCRIPTION>
<CHRONOLOGY>
<DATE>19971210</DATE>
 <DELTA>validation is now specified distinct from whether the external dtd is necssary.
  the document enforces it based <CODE>document.validate?</CODE> which defaults to
  the value of *validate* at the time the document was created.
  </DELTA>
 <DATE>19980223</DATE>
  <DELTA>patched xml-node.append-element for comments</DELTA>
 <DELTA><DATE>19980320</DATE>
  whitespace predicate for allegro</DELTA>
 <DELTA><DATE>19980808</DATE>
  wd-xml-names-10090802: dynamic namespace bindings via the *namespaces*
  special variable. nicknames are now modeled explicitly rather than intrinsic
  to packages - thus no explicit cleanup.</DELTA>
 <DELTA><DATE>19981218</DATE>
  added fred buffer support</DELTA>
 </CHRONOLOGY>
</DOCUMENTATION>
|#

(in-package "XML-PARSER")

#|
document    ::= prolog element Misc*
prolog      ::= XMLDecl? Misc* (doctypedecl Misc*)?
XMLDecl     ::= '<?XML' VersionInfo EncodingDecl? standalone? S? '?>'
VersionInfo ::= S 'version' Eq ('"1.0"' | "'1.0'")
Misc        ::= Comment | PI | S
|#

(defGeneric read-xml-stream
  (stream &rest args &key &allow-other-keys))

(defMethod read-xml-stream
           ((*markup-stream* encoded-stream) &rest bindings
            &key 
            ((:element-class *element-class*)
             *element-class*)
            ((:element-class-attribute *element-class-attribute*)
             *element-class-attribute*)
            ((:attribute-class *attribute-class*)
             *attribute-class*)
            &allow-other-keys)
  (declare (dynamic-extent bindings))
  (with-parser-bindings bindings
    (read-production 'xml-1.0::document *markup-stream*)))

(defMethod read-xml-stream
           ((stream stream) &rest args)
  (apply #'read-xml-stream (encoded-stream stream) args))

(defMethod read-xml-stream
           ((stream file-stream) &rest args &aux result)
  (typecase (setf result
                  (apply #'read-xml-stream (encoded-stream stream) args))
    (document (setf (xml-node.system-id result)
                    (pathname stream))))
  result)

#+CCL
(defMethod read-xml-stream
           ((source fred-window) &rest args &key &allow-other-keys)
  (with-local-context source
    ;; wrap the window in a stream in order that it has the proper interface
    ;; for decoding.
    (apply #'read-xml-stream (make-concatenated-stream source) args)))
;(read-xml-stream (second (windows)))

(defMethod read-xml-stream
           ((source pathname) &rest args &key &allow-other-keys)
  (with-local-context source
    (with-open-file (stream source :direction :input)
      (apply #'read-xml-stream stream args))))

(defMethod read-xml-stream
           ((source string) &rest args &key &allow-other-keys)
  (apply #'read-xml-stream (translate-system-id source) args))

#+CL-HTTP
(defMethod read-xml-stream
           ((source http-url) &rest args &key &allow-other-keys)
  (with-local-context source
    (http::with-http-request (source :get)
      (apply #'read-xml-stream http::remote-stream args))))

#+CL-HTTP
(defMethod read-xml-stream
           ((source file-url) &rest args &key &allow-other-keys)
  (with-local-context source
    (apply #'read-xml-stream (translated-pathname source) args)))




;;;
;;; 'loading' an xml source means at least reading the document and
;;; extracting the root element. methods specialized on the class of the root
;;; elements may perform further processing.
;;; the conditionally defined methods are not used internally, as the system
;;; identifiers are not held as strings. they are for convenience only.

#+CL-HTTP
(defMethod load-xml
           ((source string))
  (load-xml (parse-url source 0 (length source) :uninterned)))

#-CL-HTTP
(defMethod load-xml
           ((source string))
  (load-xml (pathname source)))

(defMethod load-xml
           ((source t))
  (load-xml (read-xml-stream source)))

(defMethod load-xml
           ((datum document))
  (load-xml (document.element datum)))

(defMethod load-xml
           ((datum element))
  datum)


#|
 the parsing per se involves binding the dtd and the root element.
 they are the only elements which matter. everything else is ignored
 |#


(defMethod xml-node.append-element
           ((context null) (element comment))
  ;; discard the comment
  element)

(defMethod xml-node.append-element
           ((context element) (child string))
  (setf (xml-node.content context)
        (nconc (xml-node.content context) (list child)))
  child)

(defMethod xml-node.append-element
           ((context element) (child character-data))
  (setf (xml-node.content context)
        (nconc (xml-node.content context) (list child)))
  (link-context child context)
  child)

(defMethod xml-node.append-element
           ((context element) (child entity-reference))
  (setf (xml-node.content context)
        (nconc (xml-node.content context) (list child)))
  (link-context child context)
  child)

(defMethod xml-node.append-element :after
           ((element element) (datum t))
  (slot-makunbound element 'valid?))

(defMethod xml-node.append-element
           ((context document) (child element))
  (cond ((document.element context)
         (raise-xml-condition context
                              *root-element-error-condition*
                              "duplicate root element: ~s."
                              child))
        (t
         (setf (document.element context) child)
         (link-context child context)
         child)))


(defMethod xml-node.append-element
           ((parent document) (child t))
  (typecase child
    (comment (call-next-method))
    (t (xml-warn (type-of parent)
                 "illegitimate content in xml document: ~s." child)
       nil)))

(defMethod xml-node.append-element
           ((parent document) (child character-data))
  ;; ignore strings outside of the root element
  nil)

(defMethod xml-node.append-element
           ((parent document) (child string))
  ;; ignore strings outside of the root element
  nil)

(defMethod xml-node.append-element
           ((parent document) (child document-type))
  "in the process of reading a documentation type declaration from a stream,
    a dtd element has been generated by a <!DOCTYPE ...> element.
    if the document is a dtd-document, the succeeding
    elements will be assimilated into its contents as they are read/processed.
    for both xml- and dtd-documents, the reader package is modified so that
    tags are, by default, interned in a package specific to the dtd."
  (when (document.get-doctype parent)
    (raise-xml-condition parent
                         *validity-error-condition*
                         "duplicate document type: ~s" child))
  (setf (document.doctype parent) child)
  (link-context child parent)
  (unless (or (document.standalone? parent)
              (xml-node.up-to-date? child))
    (get-external-subset-content child))
  child)

(defMethod xml-node.append-element
           ((context document) (element comment))
  (setf (document.node-comments context *comment-target*)
        (nconc (document.node-comments context *comment-target*) (list element)))
  (link-context element context)
  element)

(defMethod xml-node.append-element
           ((context document) (child dtd-node))
  (xml-node.append-element (document.doctype context) child))

(defMethod xml-node.append-element
           ((context document) (element processing-instruction))
  ;; this simply stashes them in with the comments.
  (setf (document.node-comments context *comment-target*)
        (nconc (document.node-comments context *comment-target*) (list element)))
  (call-next-method))

(defMethod xml-node.append-element
           ((context document) (child xml-decl-pi))
  ;; the document node accepts the declaration
  (setf (document.xml-decl-pi context) child)
  (link-context child context)
  child)


(defMethod xml-node.append-element
           ((context document-type) (child dtd-node)
            &aux (name (xml-node.name child)))
  (with-slots (content external) context
    (flet ((find-duplicate (original)
             (and (eq name (xml-node.name original))
                  (or (typep child (type-of original))
                      (typep original (type-of child))))))
      (when (or (find-duplicate content) (find-duplicate external))
        (raise-xml-condition 'xml-1.0::doctype
                             *redefined-entity-condition*
                             "multiply defined entity: ~a." child)))
    (document-type-augment-subset context child)))

(defMethod xml-node.append-element
           ((context document-type) (child processing-instruction))
  (document-type-augment-subset context child))

(defMethod xml-node.append-element
           ((context document-type) (child attlist-declaration))
  (document-type-augment-subset context child))

(defMethod xml-node.append-element
           ((node element-declaration) (element attlist-declaration))
  ;; this is not the official definition, but the effect is the same:
  ;; prior definitions have precedence
  (dolist (attdef (attlist-declaration.attdefs element))
    (setf (xml-node.context attdef) node))
  (setf (element-declaration.attdefs node)
        (append (element-declaration.attdefs node)
                (attlist-declaration.attdefs element)))
  element)

"XMLP"

