;;; -*- mode: LISP; package: "XML-PARSER"; -*-


"
<DOCUMENTATION>
 <DESCRIPTION>
  parsing and construction of attribute declarations
  </DESCRIPTION>
 <COPYRIGHT HREF='defsystem.lisp|root().descendant(1,COPYRIGHT)'/>
 <CHRONOLOGY>
  <DELTA><DATE>19971217</DATE>
   read-typed-processing-instruction reads parameters rather than attributes
   </DELTA>
  <DELTA><DATE>19971218</DATE>
   support for NOTATION clause in attribute definitions</DELTA>
  <DELTA><DATE>19980814</DATE>
   replaced element-declaration-reference's with symbols.</DELTA>
  <DELTA><DATE>19981114</DATE>
   split read function into parse/reduce phases;
   augmented error reporting</DELTA>
  <DELTA><DATE>19981124</DATE>
   eliminated attribute declarations. use prototypes instead</DELTA>
  <DELTA><DATE>19981215</DATE>
   update *processed-node* in reduce-production for attlist declarations
   </DELTA>
  <DELTA><DATE>19981218</DATE>
   once again collecting the declarations by side-effect rather prior to
   returning them as a parsed attribute.</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
 "


(in-package "XML-PARSER")

(defClass attlist-declaration (xml-reference-node
                               comment-target
                               declaration-markup)
  ((reference
    :initarg :element-declaration :initform nil
    :type (or symbol element-declaration))
   (content
    :initarg :attdefs :initform nil
    :accessor attlist-declaration.attdefs)))



(defMethod copy-node :after
           ((datum attlist-declaration))
  (setf (attlist-declaration.attdefs datum)
        (mapcar #'copy-node (attlist-declaration.attdefs datum))))

(defun instantiate-attlist
       (context list)
  (mapcar #'(lambda (attribute)
              (setf attribute (copy-node attribute))
              (setf (xml-node.context attribute) context)
              attribute)
          list))

(defMethod attlist-declaration.element-declaration
           ((node attlist-declaration) &aux decl)
  (with-slots (reference) node
    (typecase reference
      (element-declaration reference)
      (null nil)
      (symbol (when (setf decl (element-declaration reference *document*))
                (setf reference decl))))))

;;;
;;; parsing

(defMethod reduce-production
           ((node attlist-declaration) &key &aux decl)
  (cond ((setf decl (attlist-declaration.element-declaration node))
         (dolist (attdef (attlist-declaration.attdefs node))
           (setf (attribute-declaration decl (attribute.name attdef)
                                        *document*)
                 attdef)))
        (t
         (when (xml-verbose 'attlist-declaration)
           (xml-warn 'xml::AttlistDecl "element not declared: ~s."
                     (xml-node.reference node)))))
  (xml-node.append-element *document-type* node)
  (setf *processed-node* node))

(defMethod reduce-production
           ((production (eql 'xml::AttlistDecl))
            &key
            ((xml-1.0::Name name))
            ((xml-1.0::attdef attributes)))
  (unless *parse-suppress*
    (reduce-production (make-instance 'attlist-declaration
                         :element-declaration name
                         :attdefs attributes))))


(defMethod production-reader-macro
           ((production (eql 'xml-1.0::AttlistDecl)) (stream t)
            &aux attributes name attdefs)
  ;; the attlist is of the form:
  ;;  element-name {att-name type (value | (keyword value) | keyword)}*
  (handler-bind ;; augment error message for context
    ((error #'(lambda (condition)
                (xml-warn production "name: ~s, attdef: ~s."
                          name attdefs)
                condition)))
    (multiple-value-setq (name attributes)
      (read-entity-attributes production stream))
    
    (loop (cond ((null attributes) (return))
                ((< (length attributes) 3)
                 ;; at least the name, type/domain and default spec
                 (xml-form-error production
                                 "illegitimate attribute declaration: ~s."
                                 attributes)))
          (multiple-value-bind (attdef new-attributes)
                               (pop-attdef attributes)
            (push attdef attdefs)
            (setf attributes new-attributes)))

    (reduce-production production
                       'xml-1.0::Name name
                       'xml-1.0::Attdef (nreverse attdefs))))

(defun pop-attdef
       (definition-list
         &aux name type domain mode default)
  (typecase (setf name (pop definition-list))
    ((and symbol (not null)) t)
    (t
     (xml-form-error 'XML-1.0::Attdefs
                     "erroneous attribute name: ~s."
                     type)))
  (typecase (setf type (pop definition-list))
    ((or string (and symbol (not null))) t)
    (cons (setf domain type)
          (setf type "NMTOKENS"))
    (t
     (xml-form-error 'XML-1.0::Attdefs
                     "erroneous attribute type: ~s."
                     type)))
  (when (string= "NOTATION" type)
    (setf domain (pop definition-list))
    (unless (consp domain)
      (xml-form-error 'XML-1.0::Attdefs
                      "erroneous notation domain: ~s."
                      domain)))
  (typecase (setf mode (pop definition-list))
    (string
     (cond ((string-equal mode "#FIXED")
            (unless (setf default (pop definition-list))
              (xml-form-error 'XML-1.0::Attdefs
                              "illegitimate attribute default: ~s."
                              default))
            (setf mode 'XML::\#FIXED))
           ((string-equal mode "#REQUIRED")
            (setf mode 'xml-1.0::\#REQUIRED))
           ((string-equal mode "#IMPLIED")
            (setf mode 'xml-1.0::\#IMPLIED))
           (t
            (setf default mode
                  mode nil))))
   (t
    (xml-form-error 'XML-1.0::Attdefs
                    "illegitimate attribute default: ~s."
                    mode)))
  (values (make-attdef name type domain mode default)
          definition-list))

(defun attribute-type.class
       (type)
  (cond ((string-equal type "CDATA") 'xml-1.0::@CDATA)
        ((string-equal type "ID") 'xml-1.0::@ID)
        ((string-equal type "IDREF") 'xml-1.0::@IDREF)
        ((string-equal type "IDREFS") 'xml-1.0::@IDREFS)
        ((string-equal type "ENTITY") 'xml-1.0::@ENTITY)
        ((string-equal type "ENTITIES") 'xml-1.0::@ENTITIES)
        ((string-equal type "NMTOKEN") 'xml-1.0::@NMTOKEN)
        ((string-equal type "NMTOKENS") 'xml-1.0::@NMTOKENS)
        ((string-equal type "NOTATION") 'xml-1.0::@NOTATION)
        (t
         (xml-form-error 'xml-1.0::AttDef
                         "erroneous attribute type: ~s."
                         type))))
(defun make-attdef
       (name type domain mode default) 
  (make-instance (attribute-type.class type)
    :name name
    :mode mode
    :domain domain
    :value (list default)))

;;;
;;; printing

(defMethod print-object
           ((*parent-node* attlist-declaration) stream)
  (print-unreadable-object (*parent-node* stream :type t)
    (format stream "~a ~a"
            (ignore-errors (element-declaration.name
                            (xml-node.reference *parent-node*)))
            (mapcar #'(lambda (att) (xml-node.name att))
                    (attlist-declaration.attdefs *parent-node*)))))

;;;
;;; model operations


(defMethod attribute-declaration
           ((gi symbol) (id list) (context t)
            &optional (error-p *undefined-attribute-condition*))
  (mapcar #'(lambda (id) (attribute-declaration gi id context error-p)) id))





"XMLP"
