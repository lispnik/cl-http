;;; -*- package: "XML-PARSER"; -*-

"
<DOCUMENTATION>
 <DESCRIPTION>
  processing instructions provide a means for an xml-stream to pass out-of-band
  information to a parser / application.
  <P>
  for most processing instruction, a PROCESS-NODE appends the node to the
  current context element. this is the same behaviour as for standard elements,
  which may or may not be the right thing to do for a given instruction.
  for this reason the specific method on XML-NODE.APPEND-ELEMENT just defers
  upwards in the tree until some parent wants the instruction.
  <P>
  for a 'XML' pi itself - that which declares the version and encoding -
  a specific XML-NODE.APPEND-ELEMENT method is supplied to combine the
  declarations with a containing document instance.
  <P>
  for the namespace instruction, a specific PROCESS-NODE method is applied which
  loads and binds the specified DTD.

  </DESCRIPTION>
 <COPYRIGHT HREF='defsystem.lisp|root().descendant(1,COPYRIGHT)'/>
 <CHRONOLOGY>
  <DELTA><DATE>19971217</DATE>
   read-typed-processing-instruction reads parameters rather than attributes
   </DELTA>
  <DELTA><DATE>19971218</DATE>
   read-dtd-stream now requires the name</DELTA>
  <DELTA><DATE>19980406</DATE>
   wd-xml-names-19980327 support</DELTA>
  <DELTA><DATE>19980621</DATE>
   factored out xml-namespace</DELTA>
  <DELTA><DATE>19980808</DATE>
   wd-xml-names-19980802 (extended) added pseudo attributes to declaration
   for prefix bindings. xml-dec gets a new slot tohold the bindings,
   the construction function must extract them from the initialization
   args; the print function serializes them</DELTA>
  <DELTA><DATE>19981114</DATE>
   split read function into parse/reduce phases;
   augmented error reporting</DELTA> 
  <DELTA><DATE>19981218</DATE>
   class-specific accessors.</DELTA>
  <DELTA><DATE>19981220</DATE>
   processing instructions are not comment targets, they are instead stored as
   such. (nb. xml-decl-pi instances are excluded.
   </DATE>
  </CHRONOLOGY>
 </DOCUMENTATION>
"

(in-package "XML-PARSER")


;;;
;;; classes for pi nodes


(defClass processing-instruction (xml-named-node
                                  declaration-markup
                                  content-markup
                                  document-markup)
  ((name
    :accessor processing-instruction.target)
   (content
    :accessor processing-instruction.data))
  (:metaclass keyword-qualified-class)
  (:documentation
   "the class of generic processing instructions. if no class exists for the
    target name, then a generic instance is created.
    a specialization would likely parse the data as attributes.
    (see the namespace processing instruction implementation.)"))


;;;
;;; element constructors and processors for xml processing instructions.
;;; the <?XML ... ?> processing instruction is handled specially
;;; others defer to a pi-specific instantiation function which can parse the
;;; tag content as approrpriate. this class is selected through the
;;; instantiation protocol for keyword-qualified-class's, which take the target
;;; name supplied as the :name initarg to determine the effective class. 

(defMethod reduce-production
           ((node processing-instruction) &key)
  (if (in-dtd?)
    (xml-node.append-element *document-type* node)
    (xml-node.append-element *document* node))
  (setf *processed-node* node))

(defMethod reduce-production
           ((production (eql 'xml-1.0::PI))
            &key ((xml-1.0::PITarget target))
            ((xml-1.0::char data)))
  (unless *parse-suppress*
    (reduce-production (make-instance 'processing-instruction
                         :name target :content data))))
  
(defMethod pi-target-reader-macro
           (target (stream t)
            &aux data (production 'xml-1.0::PI))
  ;; discard the first character if it is a space
  (when (char= (peek-char nil stream) #\space) (read-char stream))
  ;; then read to the delimiter as 'raw' data
  (setf data (read-string-delimited-string "?>" stream nil))
  (handler-bind ;; just augment error message for context
    ((error #'(lambda (condition)
                (xml-warn production
                          "target: ~s, data: ~s."
                          target data)
                condition)))
    (reduce-production production
                       'xml-1.0::PITarget target
                       'xml-1.0::char data)))


(defMethod production-reader-macro
           ((production (eql 'xml-1.0::PI)) (stream t))
  (pi-target-reader-macro (read-markup-tag-type stream) stream))


;;;
;;;

(defMethod print-object
           ((node processing-instruction) (stream t))
  (with-slots (name content) node
    (cond (*print-readably*
           (write-string *pi-open* stream)
           (write-identifier name stream)
           (write-char #\space stream)
           (princ content stream)
           (write-string *pi-close* stream))
          (t
           (call-next-method)))))



"XMLP"


