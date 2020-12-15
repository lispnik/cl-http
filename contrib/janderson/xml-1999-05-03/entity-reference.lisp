;;; -*- mode: lisp; package: "XML-PARSER"; -*-
;;;

"
<DOCUMENTATION>
 <DESCRIPTION>
  class definitions for entities
  <UL>
   <LI>named-entity-declaration</LI>
   <LI>XML-NAMED-ENTITY-REFERENCE as a refererence to a named dtd entity</LI>
   <LI>XML-NUMERIC-ENTITY-REFERENCE as a reference to an encoded character</LI>
   </UL>
  </DESCRIPTION>
 <COPYRIGHT HREF='defsystem.lisp|root().descendant(1,COPYRIGHT)'/>
 <CHRONOLOGY>
 <DELTA><DATE>19971210</DATE>
  added process functions for element references
  </DELTA>
 <DELTA><DATE>19971217</DATE>
   <UL>
    <LI>read-typed-markup-declaration reads generic parameters rather than
    normalized attributes;
    <LI>parse '%' for pe decl by hand to allow normal reading in remained of
     entity tag content
    </UL>
   </DELTA>
 <DELTA><DATE>19971228</DATE>
  XML-NODE.VALUE ->XML-NODE.STRING, since that's the actual purpose
   </DELTA>
 <DELTA><DATE>19980223</DATE>
  print object for xml-named-entity-reference</DELTA>
 <DELTA><DATE>19980814</DATE>
  wd-xml-names-19980802: at one point, i was considering allowing
  entity declarations to specify prefix/uri bindings, but that is not
  necessary. it is sufficient to simply recognize the namespace pi's
  which the draft eliminated AND to allow them (since that are not governed
  by the standard to appear anywhere.
  </DELTA>
 <DELTA><DATE>19981220</DATE>
  entity expansion can't be handled on a per-entity basis, since it changes the
  dom structure. for example, attribute parse as a list rather than as a single
  string. as such, the specification is now a special binding.<BR/>
  prepending entitis now handled uniformly in the reduce-production method.<BR/>
  unparsed entities a distinguished class.
  </DATE>
 </CHRONOLOGY>
 <DISCUSSION>
  entity references are just instanecs which model a link to the content
  specified in the entity definition. as alternatives to this implementation,
  it would be possible to either
  <OL TYPE='a'>
   <LI> insert the definition instance directly, but this would prevent
    forward and projective references</LI>
   <LI> use a symbol with the respective name  - especially since the
    definitions are bound as properties to the symbols anyway, but this would
    make it difficult to extend.</LI>
   </UL>
  the implemented classes include
  <DL>
   <DT><CODE OMITTED='19981220'>ENTITY.EXPAND?</CODE></DT>
   <DD>specifies whether the references is to be expanded in line; named
    references delegate to the definition; character references are true by
    default</DD>
   <DT><CODE>XML-NODE.STRING</CODE></DT>
   <DD>the specialization for named-entity-reference retrieves the content
    of the referenced declaration.</DD>
   </DL>
  </DISCUSSION>
</DOCUMENTATION>
 "

(in-package "XML-PARSER")


;;;
;;; classes


(defClass entity-reference ()
  ()
  (:metaclass abstract-class))

(defClass named-entity-reference (xml-reference-node
                                  xml-named-node
                                  entity-reference)
  ()
  (:metaclass keyword-qualified-class))

(defClass parameter-entity-reference (named-entity-reference)
  ()
  (:metaclass keyword-qualified-class))

(defClass general-entity-reference (named-entity-reference)
  ()
  (:metaclass keyword-qualified-class))

(defClass unparsed-entity-reference (general-entity-reference)
  ()
  (:metaclass keyword-qualified-class))

(defClass numeric-entity-reference (xml-named-node
                                    entity-reference)
  ((content :initarg :original
            :type string)
   (character :initarg :character
              :type character
              :accessor xml-node.character))
  (:metaclass qualified-class)
  (:documentation
   "intended as a place-holder for a numeric character reference for
    model manipulation. they are not produced by the parse,
    since they are expanded as they are read."))

(defMethod (setf xml-node.name) :after
           ((name t) (node named-entity-reference))
  (setf (xml-node.content node) nil))



(defMethod entity.expand?
           ((entity parameter-entity-reference))
  *expand-parameter-entities*)

(defMethod entity.expand?
           ((entity general-entity-reference))
  *expand-general-entities*)

;;;
;;; parsing
;;;
;;; in general both parameter and general entities are prepended in their
;;; respectivly permitted contexts, while unparsed entities are left unexpanded.
;;; these latter appear in attributes and are generated whenever the cited
;;; entity includes a notation. the method is included for two reasons: first,
;;; to check the context, and second, to permit a specialized class which did,
;;; in fact, perform some form of expansion.

(defMethod reduce-production
           ((node parameter-entity-reference) &key stream)
  ;; parameter entity rules:
  ;;  in-dtd: yes: prepend to stream
  ;;  elsewhere: not recognized: if one somehow arises, it is in error.
  (cond ((in-dtd?)
         (prepend-stream node stream))
        (t
         (raise-xml-condition 'xml-1.0::PEReference
                              'entity-reference-context-error
                              :entity node)))
  node)

(defMethod reduce-production
           ((node general-entity-reference) &key stream)
  (cond ((in-dtd?)
         (raise-xml-condition 'xml-1.0::Reference
                              'entity-reference-context-error
                              :entity node))
        (t
         (prepend-stream node stream)))
  node)

(defMethod reduce-production
           ((node unparsed-entity-reference) &key stream)
  (declare (ignore stream))
  ;; never expand, just constraint the context - should never appear except when
  ;; an attribute is instantiated.
  (raise-xml-condition 'xml-1.0::Reference
                       'entity-reference-context-error
                       :entity node)
  node)

(defMethod reduce-production
           ((node numeric-entity-reference) &key stream)
  (when (entity.expand? node)
    ;; character entity references should not survive the parse. they are
    ;; transformed immediately to characters for insertion in a collected
    ;; value. therefore they should not land here.
    (raise-xml-condition 'xml-1.0::Reference
                         'entity-reference-context-warning
                         :entity node))
  (prepend-stream node stream))

(defMethod prepend-stream
           ((node entity-reference) (stream encoded-stream) &key)
  (prepend-stream (make-instance 'entity-string-stream
                   :entity node)
                  stream))


;;;
;;; model operations

(defMethod xml-node.string
           ((node named-entity-reference) &aux reference)
  ;; resolve the reference instance and cache its content string.
  ;; don't take the string if there reference target is not found, since
  ;; that would be just a "" value.
  (or (xml-node.content node)
      (setf (xml-node.content node)
            (when (setf reference (xml-node.reference node))
              (xml-node.string reference)))))

(defMethod xml-node.string
           ((node numeric-entity-reference))
  (format nil "~c" (xml-node.character node)))

(defMethod xml-node.reference
           ((node general-entity-reference))
  (or (call-next-method)
      (setf (slot-value node 'reference)
            (general-entity-declaration (xml-node.name node) *document*))))

(defMethod xml-node.reference
           ((node parameter-entity-reference))
  (or (call-next-method)
      (setf (slot-value node 'reference)
            (parameter-entity-declaration (xml-node.name node) *document*))))


(defMethod parameter-entity-reference
           ((name string))
  (parameter-entity-reference (intern-name name)))

(defMethod parameter-entity-reference
           ((name symbol))
  (make-instance 'parameter-entity-reference :name name))

(defMethod general-entity-reference
           ((name string))
  (general-entity-reference (intern-name name)))

(defMethod general-entity-reference
           ((name symbol))
  (make-instance 'general-entity-reference :name name))

(defMethod unparsed-entity-reference
           ((name string))
  (unparsed-entity-reference (intern-name name)))

(defMethod unparsed-entity-reference
           ((name symbol))
  (make-instance 'unparsed-entity-reference :name name))

(defMethod numeric-entity-reference
           ((name string))
  (numeric-entity-reference (intern-name name)))

(defMethod numeric-entity-reference
           ((name symbol))
  (make-instance 'numeric-entity-reference :name name))


;;;
;;; printing

(defMethod print-object
           ((datum parameter-entity-reference) (stream t))
  (cond (*print-readably*
         (write-char *parameter-entity-reference-open-char* stream)
         (write-identifier (xml-node.name datum) stream)
         (write-char *entity-reference-close-char* stream))
        (t
         (print-unreadable-object (datum stream :type t
                                         :identity *parse-verbose*)
           (format stream "~s" (xml-node.name datum))))))

(defMethod print-object
           ((datum general-entity-reference) (stream t))
  (cond (*print-readably*
         (write-char *general-entity-reference-open-char* stream)
         (write-identifier (xml-node.name datum) stream)
         (write-char *entity-reference-close-char* stream))
        (t
         (print-unreadable-object (datum stream :type t
                                         :identity *parse-verbose*)
           (format stream "~s" (xml-node.name datum))))))



"XMLP"

