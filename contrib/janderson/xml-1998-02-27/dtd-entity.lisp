;;; -*- mode: lisp; package ("XML-PARSER") -*-
;;;
;;; this version (C) mecom gmbh 24.11.97
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.

#|
<DOCUMENTATION>
<DESCRIPTION>
class definitions for entities
<UL>
<LI>DTD-NAMED-ENTITY</LI>
<LI>XML-NAMED-ENTITY-REFERENCE as a refererence to a named dtd entity</LI>
<LI>XML-NUMERIC-ENTITY-REFERENCE as a reference to an encoded character</LI>
</UL>
 </DESCRIPTION>
<CHRONOLOGY>
 <DATE>19971210</DATE>
  <DELTA>added process functions for element references
  </DELTA>
 <DATE>19971217</DATE>
  <DELTA>
    <LI>read-typed-markup-declaration reads generic parameters rather than
    normalized attributes;
    <LI>parse '%" for pe decl by hand to allow normal reading in remained of
     entity tag content
    </DELTA>
 <DATE>19971228</DATE>
  <DELTA>XML-NODE.VALUE ->XML-NODE.STRING, since that's the actual purpose
   </DELTA>
 <DATE>19980223</DATE>
  <DELTA>print object for xml-named-entity-reference</DELTA>
 </CHRONOLOGY>
</DOCUMENTATION>
 |#

(in-package :XML-PARSER)

(defClass dtd-named-entity (dtd-element xml-external-node)
  ((content :type string )
   (notation :initform nil :initarg :notation
             :accessor dtd-entity.notation
             :type string
             :documentation
             "bind the notation specification for the given entity. do this for
              BOTH general and parameter entities.")))


(defClass dtd-parameter-entity (dtd-named-entity)
  ())

(defClass xml-named-entity-reference (xml-reference-node xml-named-node)
  ())

(defClass xml-numeric-entity-reference (xml-named-node)
  ((content :initarg :original
            :type string)
   (character :initarg :character
              :type character
              :accessor xml-node.character)))


(defMethod node-class
           ((node (eql 'named-character-reference)) (op t) (context t))
  *named-character-reference-class*)

(defMethod node-class
           ((node (eql 'numeric-character-reference)) (op t) (context t))
  *numeric-character-reference-class*)

(defMethod node-class
           ((node (eql 'entity-declaration))
            (op (eql 'xml::entity)) (context t))
  *entity-class*)

(defMethod node-class
           ((node (eql 'parameter-entity-declaration))
            (op (eql 'xml::entity)) (context t))
  *parameter-entity-class*)

(defMethod dtd-entity.content
           ((entity dtd-named-entity))
  (or (xml-node.content entity)
      (dolist (location (xml-node.locations entity))
        (when (setf (xml-node.content entity)
                    (get-external-entity-content location))
          (return (xml-node.content entity))))))



(defMethod read-typed-markup-declaration
           ((tag-name (eql 'xml::entity)) (stream t)
            &aux name declaration class element args
                 pe?)
  (when (setf pe? (eql #\% (peek-char t stream)))
    (read-char stream))
  (setf declaration (read-markup-tag-parameters tag-name stream)) 
  (setf class
        (node-class (if pe? 'parameter-entity-declaration 'entity-declaration )
                    tag-name stream))
  (setf name (pop declaration))

  (assert (and (>= (length declaration) 1) (symbolp name)))
  (loop (setf element (pop declaration))
        (when (null element) (return))
        (case element
          (xml::public
           (setf (getf args :public-id) (pop declaration)
                 (getf args :location) (pop declaration)))
          (xml::system
           (setf (getf args :location) (pop declaration)))
          (xml::ndata
           (setf (getf args :notation) (pop declaration)))
          (t
           (when (getf args :content)
             (xml-validity-error tag-name "duplicate entity value: ~s ."
                                 element))
           (setf (getf args :content) element))))
  (apply #'make-instance class :name name args))

(defMethod xml-node.string
           ((node dtd-named-entity))
  (dtd-entity.content node))

(defMethod xml-node.string
           ((node xml-named-entity-reference))
  (or (xml-node.content node)
      (setf (xml-node.content node)
            (xml-node.string (xml-node.referent node)))))

(defMethod xml-node.string
           ((node xml-numeric-entity-reference))
  (format nil "~c" (xml-node.character node)))
  
           
(defMethod xml-node.append-element
           ((*dtd* dtd) (child dtd-named-entity))
  (when (xml-verbose 'xml-node.append-element)
    (format t "~%installing: ~s: ~s." *dtd* child))
  (unless (eq (symbol-package (xml-node.name child))
              (dtd.package *dtd*))
    (xml-form-error *dtd* "namespace mismatch: ~s."
                        (xml-node.name child)))
  (when (find (xml-node.name child) (dtd.entities *dtd*)
              :key #'xml-node.name)
    (when *xml-warn-if-redefine*
      (xml-validity-error *dtd* "multiply defined entity: ~a." child))
    (setf (dtd.entities *dtd*)
          (delete (xml-node.name child) (dtd.elements *dtd*)
                  :key #'xml-node.name)))
  (push child (dtd.entities *dtd*))
  (setf (xml-node.parent child) *dtd*)
  child)

(defMethod process-markup-element
           ((node xml-named-entity-reference) (stream t))
  (xml-node.append-element *parent-node* node))

(defMethod process-markup-element
           ((node xml-numeric-entity-reference) (stream t))
  (xml-node.append-element *parent-node* node))


(defMethod prepend-stream
           ((datum dtd-named-entity) (stream concatenated-stream)
            &aux content
            (*xml-pathname-defaults*
             (pathname (url-namestring stream nil))))
  (if (setf content (dtd-entity.content datum))
    (prepend-stream (make-instance *entity-string-stream*
                    :entity datum
                    :string content)
                  stream)
    (xml-form-error stream "entity has no content: ~s." datum)))

(defMethod print-object
           ((datum dtd-named-entity) (stream t))
  (with-accessors ((name xml-node.name)
                   (content dtd-entity.content)
                   (system xml-node.system)
                   (public-id xml-node.public-id))
                  datum
    (cond (*xml-print-readably*
           (format stream "<!ENTITY ~@[~;%~] ~s "
                   (typep datum 'dtd-parameter-entity) name)
           (if system
             (if public-id
               (format stream "PUBLIC ~s ~s>" public-id system)
               (format stream "SYSTEM ~s>" system))
             (format stream "~s>" content)))
          (t
           (print-unreadable-object (datum stream :type t
                                           :identity *xml-verbose*)
             (format stream "~s ~a bytes ~@[from ~s~]~@[/ ~s~]"
                     name (length content) system public-id))))))

(defMethod print-object
           ((datum xml-named-entity-reference) (stream t))
  (cond (*xml-print-readably*
         (format stream "&~a;" (xml-node.name datum)))
        (t
         (print-unreadable-object (datum stream :type t
                                           :identity *xml-verbose*)
             (format stream "~a" (xml-node.name datum))))))


:EOF
