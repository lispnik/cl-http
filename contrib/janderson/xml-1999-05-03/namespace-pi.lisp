;;; -*- package: "XML-PARSER"; -*-

"
<DOCUMENTATION>
 <DESCRIPTION>
  namespaces are encoded in xml documents in two forms.
  <UL>
  <LI>the (current draft) spec
  describes a standard method for interpreting attributes of a certain class of
  names as binding uri values to prefixes within the logical structural bounds
  of the respective element.</LI>
  <LI> the older spec proposed a processing instruction which would specify
  the bindings with global scope. in that case they were restricted to the
  prolog of the document.
  </LI>
  </UL>

  this implementaiton combines the two proposals in a way which ensures that the
  names in a dtd - or in general in any external entity - may also be associated
  with a uri.
  it suffices to permit the processing instruction to appear at any point in the
  document, not just the prolog, and to specify dynamic scope and extent.
 </DESCRIPTION>
 <COPYRIGHT HREF='defsystem.lisp|root().descendant(1,COPYRIGHT)'/>
 <CHRONOLOGY>
  <DELTA><DATE>19980621</DATE>
   factored out from 'xml-pi.lisp'</DELTA>
  <DELTA><DATE>19980808</DATE>
   wd-xml-names-19980802: as of the moment, the 'namespace' pi does not exist.
   (see xml-namespace-19980327.lisp)
   the wd proposes attribute-based bindings on the elements instead of pi's.
   this version supports both forms: xmlns:* attributes where they appear
   and namespace pi's where they appear. it permits them anywhere. it adopts
   the content of the attribute binding for the pi. that is, only the
   pseudo-attributes 'ns' and 'prefix' are supported. the 'src' is specified
   separately in an external entity.</DELTA>
  <DELTA><DATE>19981024</DATE>
   package management collected in namespace.lisp</DELTA>
  <DELTA><DATE>19981102</DATE>
   changed syntax to read both old and new encodings (prefix/ns & xmlns:prefix)
   and to uniformly print new encoding.</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
 "

(in-package "XML-PARSER")

;;;
;;; classes for pi nodes

(defClass xml-1.0::|namespace| (xml-attribute-context processing-instruction)
  ()
  (:documentation
   "a namespace processing instruction which binds namespaces to respective
    prefixes at the specified point in the document. the bindings are
    declared either as xmlns: attributes or as a single ns attribtute and
    one or more prefix attributes.")
  (:metaclass qualified-class))

;;;
;;; element constructors and processors for namespace instructions

(defMethod initialize-instance
           ((instance xml-1.0::|namespace|) &rest initargs
            &key content)
  (apply #'call-next-method instance
         :content (read-stag-attributes
                   (encoded-stream (concatenate 'string content ">")))
         initargs)
  (with-slots (content) instance
    (initialize-instance-attributes instance
                                    (first content) (rest content))))


(defMethod reduce-production
           ((op xml-1.0::|namespace|) &key)
  (assert-namespace-bindings (xml-node.attributes op))
  (call-next-method))



(defMethod print-object
           ((*parent-node* xml-1.0::|namespace|) (stream t))
  (flet ((format-content ()
           (dolist (attribute (slot-value *parent-node* 'attributes))
             (write-char #\space stream)
             (print-object attribute stream))))
    (cond (*print-readably*
           (assert-namespace-bindings *parent-node*)
           (write-string *pi-open* stream)
           (write-string "namespace" stream)
           (format-content)
           (write-string *pi-close* stream))
          (t
           (print-unreadable-object (*parent-node* stream :type t)
             (format-content))))))


"XMLP"


