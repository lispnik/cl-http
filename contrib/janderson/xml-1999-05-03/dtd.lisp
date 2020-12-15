;;; -*- mode: LISP; package: "XML-PARSER"; -*-
;;;

"
<DOCUMENTATION>
 <DESCRIPTION>
  support for document type definitions. these comprise the declarations for elements,
  attributes, entities, and notations.
  the early versions (< 0.42) collected the definitions established as part of
  a given definition and encapsulated then in a DTD object. later versions rely on
  universal identifiers to assert that once a definition exists, it is globally valid.
  if another appears, it replaces the first. attributes are a special case in that they
  are relative to a global element definition.
  to replace the DTD for the purpose of serialization, the DOCTYPE instance is used
  to cache declarations.
  </DESCRIPTION>
 <COPYRIGHT HREF='defsystem.lisp|root().descendant(1,COPYRIGHT)'/>
 <CHRONOLOGY>
  <DELTA><DATE>19971125</DATE>
   <LI>dtd.dtd-element-reference (t symbol)
       changed the search strategy for reference elements:
       1. search the present dtd if one is present; if that fails
       2. look for a matching dtd as a) one from which the symbol's package is
          reachable and b) in which the element is defined; if that fails,
       3. otherwise, or if no dtd is present, make an autonomous reference
   </DELTA>
  <DELTA><DATE>199711128</DATE>
   fixed DTD-ELEMENT args for RESERVED-DECLARATION </DELTA>
  <DELTA><DATE>19971218</DATE>
   the 'as' slot is superfluous: a dtd is always loaded as something
    since the name in the doctype plays the same selective role as that in
    a namespace pi</DELTA>
  <DELTA><DATE>19980223</DATE>
   patched dtd.dtd-element to prevent references to references</DELTA>
  <DELTA><DATE>19980331</DATE>
   changed reinitialize slot for dtd class to eliminate symbol conflict
   from DI package</DELTA>
  <DELTA><DATE>19980817</DATE>
   the DTD class ind the DTD-IDENTITY-CLASS meta-object are eliminated.
   definitions are now bound directly to the name symbols and simply
   collected in the DOCTYPE instance for serialization convenience.
   </DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
"

(in-package "XML-PARSER")

(defVar *dtd-write-element-references* nil)
(defVar *dtd-write-doctype* nil)

(defMethod %dtd-clear
           ((name symbol))
  (remf (symbol-plist name) 'element-declaration)
  (remf (symbol-plist name) 'notation-declaration)
  (remf (symbol-plist name) 'parameter-entity-declaration)
  (remf (symbol-plist name) 'general-entity-declaration)
  (remf (symbol-plist name) 'attribute-declaration))

(defMethod %dtd-clear
           ((dtd list))
  (dolist (sym dtd)
    (%dtd-clear sym)))

(defMethod %dtd-clear
           ((package package))
  (do-symbols (sym package) (%dtd-clear sym)))

(defMethod %dtd-clear
           ((package (eql t)))
  (map nil #'%dtd-clear (list-all-packages)))


"<H3>element lookup</H3>
 dtd's are indexed by url to make it possible for multiple dtd's with the
 same document type name, but different content, to be held simultaneously.
 a dtd must be recognised in order to map element names to the correct
 element models (through dtd-element's)
 a link to a dtd must be established under one of several circumstances:
 <OL>
 <LI>the dtd is itself being read</LI>
 <LI>the dtd was specified as the document type for an xml document which is
     being read.</LI>
 </OL>
 <P>
 the containment erlation between dtd and declaration is not, however, followed
 strictly: since the names can be namespace-qualified, qualifying again by dtd
 is redundant. the dtd implementation manages the collection in order that
 it is possible to serialize, but declarations are bound globally to the
 respective name symbols.


 <DL>
 <DT>reading a dtd
 <DD>
 when a dtd itself is read, it supplants the global dtd and collects any
 definitions present in the stream. 

 <DT>reading an xml document
 <DD>
 if the document declares a dtd, it will be located and established for the
 duration of the document. note, as mentioned above that this has no effect
 on the definitions associated with the respective names.
 </DL>
 
 "


(defMethod element-declaration
           ((id symbol) (context t)
            &optional (error-p *undefined-element-condition*)
            &aux declaration)
  (cond  ((reserved-name-p id)
          (reserved-element-declaration id))
         ((setf declaration (get id 'element-declaration))
          (element-declaration declaration context error-p))
         (error-p
          (raise-xml-condition 'xml-1.0::element
                               error-p
                              "undeclared element: ~s"
                              id )
          (make-instance 'element-declaration
            :name id :model '|xml|::ANY))))
          
(defMethod element-declaration
           ((id list) (context t) &optional (error-p *undefined-element-condition*))
  (mapcar #'(lambda (id) (element-declaration id context error-p)) id))

(defMethod element-declaration
           ((id string) (context t) &optional (error-p *undefined-element-condition*))
  (element-declaration (intern-name id) context error-p))

(defMethod (setf element-declaration)
           ((entity symbol) (id symbol) (context t))
  (setf (get id 'element-declaration) entity))

(defMethod (setf element-declaration)
           ((entity t) (id string) (context t))
  (setf (element-declaration (intern-name id)) entity))



(defMethod general-entity-declaration
           ((id symbol) (context t)
            &optional (error-p *undefined-entity-condition*)
            &aux entity)
  (cond  ((setf entity (get id 'general-entity-declaration))
          (general-entity-declaration entity context error-p))
         (error-p
          (raise-xml-condition 'xml-1.0::Reference
                               error-p
                              "undeclared general entity: ~s."
                              id )
          (make-instance 'internal-general-entity-declaration
            :name id :content ""))))

(defMethod general-entity-declaration
           ((id string) (context t) &optional (error-p *undefined-entity-condition*))
  (general-entity-declaration (intern-name id) context error-p))

(defMethod (setf general-entity-declaration)
           ((entity symbol) (id symbol) (context t))
  (setf (get id 'general-entity-declaration) entity))

(defMethod (setf general-entity-declaration)
           ((entity t) (id string) (context t))
  (setf (general-entity-declaration (intern-name id)) entity))


(defMethod parameter-entity-declaration
           ((id symbol) (context t)
            &optional (error-p *undefined-entity-condition*)
            &aux entity)
  (cond  ((setf entity (get id 'parameter-entity-declaration))
          (parameter-entity-declaration entity context error-p))
         (error-p
          (raise-xml-condition 'xml-1.0::Reference
                               error-p
                              "undeclared parameter entity: ~s."
                              id )
          (make-instance 'internal-parameter-entity-declaration
            :name id :content ""))))

(defMethod parameter-entity-declaration
           ((id string) (context t) &optional (error-p *undefined-entity-condition*))
  (parameter-entity-declaration (intern-name id) context error-p))


(defMethod (setf parameter-entity-declaration)
           ((entity symbol) (id symbol) (context t))
  (setf (get id 'parameter-entity-declaration) entity))

(defMethod (setf parameter-entity-declaration)
           ((entity t) (id string) (context t))
  (setf (parameter-entity-declaration (intern-name id)) entity))


(defMethod notation-declaration
           ((id symbol) (context t)
            &optional (error-p *undefined-notation-condition*)
            &aux notation)
  (cond  ((setf notation (get id 'notation-declaration))
          (notation-declaration notation context error-p))
         (error-p
          (raise-xml-condition 'xml-1.0::element
                               error-p
                              "undeclared notation: ~s."
                              id )
          (make-instance 'element-declaration
            :name id :model '|xml|::ANY))))
          
(defMethod notation-declaration
           ((id list) (context t)
            &optional (error-p *undefined-notation-condition*))
  (mapcar #'(lambda (id) (notation-declaration id context error-p)) id))

(defMethod notation-declaration
           ((id string) (context t)
            &optional (error-p *undefined-notation-condition*))
  (notation-declaration (intern-name id) context error-p))

(defMethod (setf notation-declaration)
           ((entity symbol) (id symbol) (context t))
  (setf (get id 'notation-declaration) entity))

(defMethod (setf notation-declaration)
           ((entity t) (id string) (context t))
  (setf (notation-declaration (intern-name id)) entity))


(defMethod attribute-declaration
           ((gi symbol)
            (attribute-name symbol)
            (context t)
            &optional
            (error-p *undefined-attribute-condition*)
            &aux entity)
  (cond  ((setf entity (element-declaration gi context))
          (attribute-declaration entity attribute-name context error-p))
         (error-p
          (raise-xml-condition 'xml-1.0::Attribute
                               error-p
                               "undeclared attribute: ~s, ~s"
                               gi attribute-name)
          (make-instance 'attribute-declaration
            :name attribute-name ))))

(defMethod attribute-declaration
           ((gi string) (attribute-name t) (context t)
            &optional (error-p *undefined-attribute-condition*))
  (attribute-declaration (intern-name gi) attribute-name context error-p))

(defMethod attribute-declaration
           ((gi t) (attribute-name string) (context t)
            &optional (error-p *undefined-attribute-condition*))
  (attribute-declaration gi (intern-name attribute-name) context error-p))

(defMethod (setf attribute-declaration)
           ((entity t) (gi string) (attribute-name t) (context t))
  (setf (attribute-declaration (intern-name gi) attribute-name context) entity))

(defMethod (setf attribute-declaration)
           ((entity t) (gi t) (attribute-name string) (context t))
  (setf (attribute-declaration gi (intern-name attribute-name) context) entity))

(defMethod (setf attribute-declaration)
           ((attdef t) (gi symbol) (attribute-name symbol) (context t)
            &aux entity)
  (cond ((setf entity (element-declaration gi))
         (setf (attribute-declaration entity attribute-name context) attdef))
        (t
         (xml-form-error 'internal  "undeclared element: ~s, ~s."
                         gi attribute-name))))

(defMethod reserved-element-declaration
           ((id symbol))
  (or (gethash id (xml-token-class.map (find-class 'reserved-element-declaration)))
      (xml-form-error 'internal "reserved element not defined: ~s." id)))


(defMethod map-declarations
           ((context (eql t)) selector operator)
  (dolist (package (list-all-packages))
    (when (string= "XMLNS" (package-name package) :end2 5)
      (map-declarations package selector operator))))

(defMethod map-declarations
           ((context package) selector operator &aux decl)
  (do-symbols (sym context)
    (when (setf decl (funcall selector sym))
      (funcall operator decl))))

(defMethod map-declarations
           ((context document-type) selector operator &aux decl)
  (flet ((do-sym (sym)
           (when (setf decl (funcall selector sym context))
             (funcall operator decl))))
    (dolist (sym (document-type.internal-subset context))
      (do-sym sym))
    (dolist (sym (document-type.external-subset context))
      (do-sym sym))))

(defun collect-declarations
       (context selector &rest args &aux list)
  (destructuring-bind (&optional
                       (collector #'(lambda (decl) (push decl list))))
                      args
    (map-declarations context selector collector)
    (nreverse list)))


(defMethod element-declarations
           ()
  (collect-declarations
   t #'(lambda (d) (typep d 'element-declaration))))
(defMethod parameter-entity-declarations
           ()
  (collect-declarations
   t #'(lambda (d) (typep d 'parameter-entity-declaration))))
(defMethod general-entity-declarations
           ()
  (collect-declarations
   t #'(lambda (d) (typep d 'general-entity-declaration))))
(defMethod notation-declarations
           ()
  (collect-declarations
   t #'(lambda (d) (typep d 'notation-declaration))))
(defMethod attribute-declarations
           (&aux list)
  (collect-declarations
   t #'element-declaration
   #'(lambda (decl)
       (setf list (append list (element-declaration.attdefs decl))))))


;(xml-node.string (make-instance 'general-entity-reference :name 'xml-1.0::|lt|))

"XMLP"
