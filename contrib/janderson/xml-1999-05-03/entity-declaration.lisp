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
  <P>
   note that the entites do NOT bind themselves when they are created.</P>
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
  unparsed entities as a distinguished class.<BR/>
  expansion can't be done on a per-entity basis, as it changes the dom
  structure</DATE>
 </CHRONOLOGY>
</DOCUMENTATION>
 "

(in-package "XML-PARSER")


;;;
;;; classes

(defClass named-entity-declaration (dtd-node)
  ((content
    :type string
    :reader entity-declaration.get-content )
   (notation
    :initform nil :initarg :notation
    :accessor entity-declaration.notation
    :type string
    :documentation
    "bind the notation specification for the given entity.
     present for BOTH general and parameter entities."))
  (:metaclass abstract-class))


(defClass internal-entity-declaration (named-entity-declaration)
  ((content
    :reader entity-declaration.content ))
  (:metaclass abstract-class))

(defClass external-entity-declaration (xml-external-node
                                       named-entity-declaration)
  ()
  (:metaclass abstract-class))

(defClass parameter-entity-declaration (named-entity-declaration)
  ()
  (:metaclass abstract-class))

(defClass general-entity-declaration (named-entity-declaration)
  ()
  (:metaclass abstract-class))

(defClass unparsed-entity-declaration (general-entity-declaration)
  ()
  (:metaclass abstract-class))


(defClass internal-parameter-entity-declaration (parameter-entity-declaration
                                                 internal-entity-declaration)
  ()
  (:metaclass keyword-qualified-class))

(defClass external-parameter-entity-declaration (parameter-entity-declaration
                                                 external-entity-declaration)
  ()
  (:metaclass keyword-qualified-class))

(defClass internal-general-entity-declaration (general-entity-declaration
                                               internal-entity-declaration)
  ()
  (:metaclass keyword-qualified-class))

(defClass external-general-entity-declaration (general-entity-declaration
                                               external-entity-declaration)
  ()
  (:metaclass keyword-qualified-class))

(defClass external-unparsed-entity-declaration (unparsed-entity-declaration
                                                external-entity-declaration)
  ()
  (:metaclass keyword-qualified-class))
  

;;;
;;; parsing

(defMethod reduce-production
           ((entity general-entity-declaration) &key)
  (setf (general-entity-declaration (xml-node.name entity) *document*) entity)
  (call-next-method))

(defMethod reduce-production
           ((entity parameter-entity-declaration) &key)
  (setf (parameter-entity-declaration (xml-node.name entity) *document*) entity)
  (call-next-method))

(defMethod reduce-production
           ((production (eql 'xml-1.0::GEDecl))
            &key
            ((xml-1.0::Name name))
            ((xml-1.0::EntityValue definition))
            ((xml-1.0::SystemLiteral system-id))
            ((xml-1.0::PublicLiteral public-id))
            ((xml-1.0::NDataDecl notation)))
  (unless *parse-suppress*
    (cond (definition
            (reduce-production 
             (make-instance 'internal-general-entity-declaration
               :name name
               :content definition)))
          ((or system-id public-id)
           (reduce-production
            (make-instance (if notation
                             'external-unparsed-entity-declaration
                             'external-general-entity-declaration)
              :name name
              :system-id system-id :public-id public-id
              :notation notation)))
          (t
           (raise-xml-condition production
                                *form-error-condition*
                                "erroneous declaration.")))))

(defMethod reduce-production
           ((production (eql 'xml-1.0::PEDecl))
            &key
            ((xml-1.0::Name name))
            ((xml-1.0::EntityValue definition))
            ((xml-1.0::SystemLiteral system-id))
            ((xml-1.0::PublicLiteral public-id)))
  (unless *parse-suppress*
    (cond (definition
            (reduce-production 
             (make-instance 'internal-parameter-entity-declaration
               :name name
               :content definition)))
          ((or system-id public-id)
           (reduce-production
            (make-instance 'external-parameter-entity-declaration
              :name name
              :system-id system-id :public-id public-id)))
          (t
           (raise-xml-condition production
                                *form-error-condition*
                                "erroneous declaration.")))))



(defMethod production-reader-macro
           ((production (eql 'xml-1.0::PEDecl)) (stream t)
            &aux name definition system-id public-id)
  (handler-bind ;; just augment error message for context
    ((error #'(lambda (condition)
                (xml-warn production
                          "name: ~s, definition: ~s, system-id: ~s, public-id: ~s."
                          name definition system-id public-id)
                condition)))
    (multiple-value-setq (name definition system-id public-id)
      (read-entity-attributes production stream))
    (reduce-production production
                       'xml-1.0::Name name
                       'xml-1.0::EntityValue definition
                       'xml-1.0::SystemLiteral system-id
                       'xml-1.0::PublicLiteral public-id)))

(defMethod production-reader-macro
           ((production (eql 'xml-1.0::GEDecl)) (stream t)
            &aux name definition system-id public-id notation)
  (handler-bind ;; just augment error message for context
    ((error #'(lambda (condition)
                (xml-warn production
                          "name: ~s, definition: ~s, system-id: ~s, public-id: ~s, notation: ~s."
                          name definition system-id public-id notation)
                condition)))
    (multiple-value-setq (name definition system-id public-id notation)
      (read-entity-attributes production stream))
    (reduce-production production
                       'xml-1.0::Name name
                       'xml-1.0::EntityValue definition
                       'xml-1.0::SystemLiteral system-id
                       'xml-1.0::PublicLiteral public-id
                       'xml-1.0::NDataDecl notation)))

(defMethod production-reader-macro
           ((production (eql 'xml-1.0::EntityDecl)) (stream t))
  (cond ((eql (peek-char t stream) *parameter-entity-marker-char*)
         (read-char stream)
         (production-reader-macro 'xml-1.0::PEDecl stream))
        (t
         (production-reader-macro 'xml-1.0::GEDecl stream))))




;;;
;;; model operations

(defMethod general-entity-declaration
           ((element general-entity-declaration) (context t) &optional error-p)
  (declare (ignore error-p))
  element)

(defMethod (setf general-entity-declaration)
           ((entity general-entity-declaration) (id symbol) (context t))
  (setf (get id 'general-entity-declaration) entity))

(defMethod parameter-entity-declaration
           ((element parameter-entity-declaration) (context t) &optional error-p)
  (declare (ignore error-p))
  element)

(defMethod (setf parameter-entity-declaration)
           ((entity parameter-entity-declaration) (id symbol) (context t))
  (setf (get id 'parameter-entity-declaration) entity))

(defMacro defGeneralEntity (name value &optional (context t))
  `(setf (general-entity-declaration ',name ,context)
         (make-instance 'internal-general-entity-declaration
           :name ',name :content ,value)))

(defMacro defParameterEntity (name value &optional (context t))
  `(setf (parameter-entity-declaration ',name ,context)
         (make-instance 'internal-parameter-entity-declaration
           :name ',name :content ,value)))

(defMethod entity-declaration.content
           ((entity internal-entity-declaration))
  (xml-node.content entity))

(defMethod entity-declaration.content
           ((entity external-entity-declaration))
  (or (and (xml-node.content entity)
           (xml-node.up-to-date? entity))
      (get-external-entity-content entity)))

(defMethod (setf entity-declaration.content)
           (definition (entity external-entity-declaration))
  (with-slots (content assertion-time modification-time) entity
    (setf content definition)
    (setf assertion-time (get-universal-time))
    (unless definition (setf modification-time nil))))

(defMethod get-external-entity-content
           ((entity external-entity-declaration)
            &aux location)
  (with-accessors ((modification-time xml-node.modification-time)
                   (public-id xml-node.public-id)
                   (system-id xml-node.system-id)
                   (content entity-declaration.content))
                  entity
    (when (setf location (or (translate-public-id public-id)
                             (translate-system-id system-id)))
      (multiple-value-setq (content modification-time)
        (get-external-entity-content location))
      content)))


(defMethod xml-node.string
           ((node named-entity-declaration))
  (entity-declaration.content node))


(setf (general-entity-declaration 'xml-1.0::|lt| nil)
      (make-instance 'internal-general-entity-declaration
        :name 'xml-1.0::|lt|
        :content "&#60;"))
(setf (general-entity-declaration 'xml-1.0::|gt| nil)
      (make-instance 'internal-general-entity-declaration
        :name 'xml-1.0::|gt|
        :content "&#62;"))
(setf (general-entity-declaration 'xml-1.0::|amp| nil)
      (make-instance 'internal-general-entity-declaration
        :name 'xml-1.0::|amp|
        :content "&#38;"))
(setf (general-entity-declaration 'xml-1.0::|apos| nil)
      (make-instance 'internal-general-entity-declaration
        :name 'xml-1.0::|apos|
        :content "&#39;"))
(setf (general-entity-declaration 'xml-1.0::|quot| nil)
      (make-instance 'internal-general-entity-declaration
        :name 'xml-1.0::|quot|
        :content "&#34;"))

;;;
;;; printing

(defMethod print-object
           ((datum external-entity-declaration) (stream t))
  (with-slots (content notation namespace-bindings
                       system-id public-id
                       name) datum
    (cond (*print-readably*
           (write-string *markup-declaration-open* stream)
           (write-string "ENTITY " stream)
           (format stream "~:[~;%~] ~s "
                   (typep datum 'dtd-parameter-entity) name)
           (write-external-id-parameters stream public-id system-id)
           (write-notation-parameter notation stream)
           (write-string *markup-declaration-close* stream))
          (t
           (print-unreadable-object (datum stream :type t
                                           :identity *parse-verbose*)
             (format stream "~s ~a bytes ~@[from ~s~]~@[/ ~s~]"
                     name (length content) system-id public-id))))))

(defMethod print-object
           ((datum internal-entity-declaration) (stream t))
  (cond (*print-readably*
         (write-string *markup-declaration-open* stream)
         (write-string "ENTITY " stream)
         (write-declaration-content datum stream)
         (write-string *markup-declaration-close* stream))
        (t
         (print-unreadable-object (datum stream
                                         :type t :identity *parse-verbose*)
           (format stream "~s" (xml-node.name datum))))))



(defMethod write-declaration-content
           ((datum internal-parameter-entity-declaration) stream)
  (write-string "% " stream)
  (write-identifier (xml-node.name datum) stream)
  (write-string " '" stream)
  (write-parameter-entity-value (xml-node.content datum)  stream)
  (write-string "'" stream))

(defMethod write-declaration-content
           ((datum external-parameter-entity-declaration) stream)
  (write-string "% " stream)
  (write-identifier (xml-node.name datum) stream)
  (write-string " " stream)
  (write-external-id-parameters (xml-node.system-id datum)
                                (xml-node.public-id datum)
                                stream))

(defMethod write-declaration-content
           ((datum internal-general-entity-declaration) stream)
  (write-identifier (xml-node.name datum) stream)
  (write-string " '" stream)
  (write-character-data-value (xml-node.content datum)  stream)
  (write-string "'" stream))

(defMethod write-declaration-content
           ((datum external-general-entity-declaration) stream)
  (write-identifier (xml-node.name datum) stream)
  (write-string " " stream)
  (write-external-id-parameters (xml-node.system-id datum)
                                (xml-node.public-id datum)
                                stream)
  (write-notation-parameter (entity-declaration.notation datum) stream))



"XMLP"

