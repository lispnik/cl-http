;;; -*- mode: LISP; package ("XML-PARSER")
;;;
;;; this version (C) mecom gmbh 24.11.97
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.

#|
<CHRONOLOGY>
<DATE>19971128</DATE>
<DELTA>
 <LI>dtd-element-declaration.model (dtd-element-model)
     added to retrieve from the referent declaration
 <LI>#PCDATA model is always with occurrence *
 </DELTA>
</CHRONOLOGY>
 |#

(in-package :XML-PARSER)

#|
                      element                 group
---------------------------------------------------------------
parent                (or null dtd-element)   (or null dtd-element)
content               dtd-element             (list dtd-element)
occurrence            '1...n', '*', '?', '+'  '1...n', '*', '?', '+'
connector                                     '&', '|', ','
|#


(defClass dtd-model (xml-pattern xml-node)
  ((parent
    :accessor dtd-model.parent)
   (content
    :accessor dtd-model.content)
   (occurrence
    :initarg :occurrence :initform 1
    :accessor dtd-model.occurrence))
  (:documentation
   "the <CODE>DTD-MODEL</CODE> class is the base abstract class for dtd models.
    the minimal combinations specifies an occurrence for an element
    as <CODE>DTD-MODEL.OCCURRENCE</CODE>
    <CODE>DTD-MODEL.REFERENT</CODE> binds the name of the referent.
    <CODE>DTD-MODEL.CONTENT</CODE> binds the resolved reference."))
     

(defClass dtd-element-model (dtd-model)
  ()
  (:documentation
   "the <CODE>DTD-ELEMENT-MODEL</CODE> class is the concrete specialization for
    single elements."))

(defClass dtd-reserved-model (dtd-element-model)
  ()
  (:documentation
   "the <CODE>DTD-RESERVED-MODEL</CODE> class designate model elements
    which denote reserved types (#PCDATA, CDATA ...)"))
   

(defClass dtd-model-group (dtd-model)
  ((connector
    :initarg :connector
    :accessor dtd-model.connector))
  (:documentation
   "the <CODE>DTD-MODEL-GROUP</CODE> class provides for concrete dtd model
    groups. a group specifies a connector (',' '|', '&') for a set of
    elements."))


(defMethod dtd-model.name
           ((node dtd-element-model))
  (xml-node.name (dtd-model.content node)))
(defMethod dtd-model.name
           ((node dtd-model-group))
  (xml-node.name (dtd-model.parent node)))

(defMethod dtd-element-declaration.model
           ((datum dtd-element-model) &aux (content (dtd-model.content datum)))
  (typecase content
    (dtd-element-declaration (dtd-element-declaration.model content))
    (t content)))

(defMethod (setf dtd-element-declaration.model) :around
           ((model dtd-model) (decl dtd-element-declaration))
  ;; link the model to the description.
  ;; nb. the 19971107 spec limites the name entry in an !ELEMENT to just that.
  ;; in any case we check here in the event that multiple names are allowed.
  (when (dtd-model.parent model)
           (setf model (copy-instance model)))
  (call-next-method model decl)
  (setf (dtd-model.parent model) decl))

;(dtd-element-declaration.model (top-inspect-form))

(defMethod copy-instance :after
           ((self dtd-model-group))
  (setf (dtd-model.content self)
        (mapcar #'copy-instance (dtd-model.content self))))

;;;
;;; pattern interface

(defMethod xml-pattern.attributes ((node dtd-model)) nil)

(defMethod xml-pattern.relations
           ((node dtd-element-model))
  (dtd-element-declaration.model (dtd-model.content node)))
(defMethod xml-pattern.relations
           ((node dtd-model-group))
  (dtd-model.content node))

(defMethod xml-pattern.type
           ((node dtd-model))
  (dtd-model.name node))

(defMethod xml-pattern.occurrence ((node dtd-model))
  (dtd-model.occurrence node))

(defMethod xml-pattern.wildcard? ((node dtd-model)) nil)


(defmethod dtd-model.occurrence-char ((datum dtd-model))
  (dtd-model.occurrence-char (dtd-model.occurrence datum)))

(defMethod dtd-model.occurrence-char
           ((occurrence (eql *plus-marker*)))
  #\+)

(defMethod dtd-model.occurrence-char
           ((occurrence (eql *rep-marker*)))
  #\*)

(defMethod dtd-model.occurrence-char
           ((occurrence (eql *opt-marker*)))
  #\?)



(defmethod print-model-group ((connector (eql *seq-marker*)) object-list stream)
  (format stream "(~{~A~^, ~})" object-list))
(defmethod print-model-group ((connector (eql *or-marker*)) object-list stream)
  (format stream "(~{~A~^ | ~})" object-list))
(defmethod print-model-group ((connector (eql *and-marker*)) object-list stream)
  (format stream "(~{~A~^ & ~})" object-list))
(defmethod print-model-group ((connector t) object-list stream)
  (format stream "(~{~A~^... ~})" object-list))


(defMethod print-object ((datum dtd-element-model) stream)
  (flet ((print-it (&aux (name (ignore-errors (dtd-model.name datum))))
           (format stream "~A~@[~A~]"
                   name
                   (unless (or (eql (dtd-model.occurrence datum) 1)
                               (eq name 'xml::\#pcdata))
                     (dtd-model.occurrence-char datum)))))
    (flet ((print-it-unreadably()
             (print-unreadable-object (datum stream :type t)
               (print-it))))
      (if *xml-print-readably*
        (print-it)
        (print-it-unreadably)))))


(defMethod print-object ((datum dtd-model-group) stream)
  ;(print *xml-print-readably*)
  (flet ((print-it (&aux (*xml-print-readably* t))
           (print-model-group (dtd-model.connector datum)
                              (dtd-model.content datum)
                              stream)
           (unless (eql (dtd-model.occurrence datum) 1)
             (format stream "~a" (dtd-model.occurrence-char datum)))))
    (flet ((print-it-unreadably ()
             (print-unreadable-object (datum stream :type t)
               (format stream "~s " (ignore-errors (dtd-model.name datum)))
               (print-it))))
      (if *xml-print-readably*
        (print-it)
        (print-it-unreadably)))))


(defun occurrence-name-p (elt)
  (or (eq elt *rep-marker*)
      (eq elt *opt-marker*)
      (eq elt *plus-marker*)))

(defun connector-name-p (elt)
  (or (eq elt *seq-marker*)
      (eq elt *or-marker*)
      (eq elt *and-marker*)))

(defMethod make-model ((content null))
  nil)

(defMethod make-model ((content dtd-model))
  content)

(defMethod make-model ((content symbol))
  (cond ((reserved-name-p content)
         (make-instance 'dtd-reserved-model
           :parent *parent-node*
           :content (dtd-element-reference content)
           :occurrence (if (eq content 'xml::\#PCDATA) *rep-marker* 1)))
        (t
         (make-instance 'dtd-element-model
           :parent *parent-node*
           :content (dtd-element-reference content)
           :occurrence 1))))

;; from 42-49 it would appear that the outermost model can well be restricted to
;; be a group
(defMethod make-model ((content cons))
  (if (and (null (cddr content))
           (consp (first content)))
    (make-model-expression
     (make-instance 'dtd-model-group
       :parent *parent-node*
       :content nil
       :occurrence (or (second content) 1)
       :connector nil)
     (caar content)
     (second (first content))
     (cddr (first content)))
    (make-model-expression (make-instance 'dtd-model-group
                             :parent nil; *parent-node*
                             :occurrence 1
                             :connector nil
                             :content nil)
                           (first content) (second content) (cddr content))))

(defMethod make-model-expression
           (group (model dtd-model) (separator symbol) (rest t) &aux content)
  (warn "separator missing: ~s ~s ~s." group model separator)
  (if (setf content (dtd-model.content group))
    (nconc content (list model))
    (setf (dtd-model.content group) (list model)))
  (make-model-expression group separator (first rest) (rest rest)))

(defMethod make-model-expression
           (group (model dtd-model) (separator null) (rest t) &aux content)
  (if (setf content (dtd-model.content group))
    (nconc content (list model))
    (setf (dtd-model.content group) (list model)))
  (when (null (dtd-model.connector group))
    (setf (dtd-model.connector group) *seq-marker*))
  group)

(defMethod make-model-expression
           (group (model dtd-model) (separator marker-oc) rest)
  (unless (eql (dtd-model.occurrence model) 1)
    (xml-form-error model
                    "duplicate repetition specified: ~s/~s."
                    (dtd-model.occurrence model) separator))
  (setf (dtd-model.occurrence model) separator)
  (make-model-expression group model (first rest) (rest rest)))

(defMethod make-model-expression
           (group (model dtd-model) (separator marker-sep) rest &aux content)

  (if (dtd-model.connector group)
    (unless (eq (dtd-model.connector group) separator)
      (xml-form-error group
                      "conflicting connectors specified: ~s."
                      separator))
    (setf (dtd-model.connector group) separator))
  (if (setf content (dtd-model.content group))
    (nconc content (list model))
    (setf (dtd-model.content group) (list model)))
  (make-model-expression group (first rest) (second rest) (cddr rest)))


(defMethod make-model-expression
           (group (model cons) (separator t) rest)
  (make-model-expression group (make-model model) separator rest))

(defMethod make-model-expression
           (group (model symbol) (separator t) rest)
  (make-model-expression group (make-model model) separator rest))

;;;
;;;reader support

(defMethod read-pattern-model
           ((stream string))
  (with-input-from-string (string-stream stream)
    (read-pattern-model string-stream)))

(defMethod read-pattern-model
           ((stream concatenated-stream))
  (make-model (read-markup-model stream)))

(defMethod read-pattern-model
           ((stream stream))
  (read-pattern-model (markup-stream stream)))

(set-markup-dispatch-macro #\m 'read-pattern-model t)
;; (inspect #!M(a, b))


#|

(defParameter 
  *test-models*
  '(#!ma #!m(a)
    #!ma* #!ma+ #!ma? #!m(a)* #!m(a)+ #!m(a)? #!m(a*) #!m(a+) #!m(a?)
    #!m(a, b) #!m(a, b)* #!m((a, b)*)
    #!m(a, b)+ #!m((a, b)+)
    #!m(a, b)? #!m((a, b)?)
    #!m(a | b) #!m(a | b)* #!m((a | b)*)
    #!m(a | b)+ #!m((a | b)+) #!m(a | b)? #!m((a | b)?)
    #!m((a, c), b) #!m((a, c)*, b)* #!m(((a, c)+, b)*) #!m((a, c)?, b)+
    #!m(a, (b, c)) #!m((a, (b, c))+) #!m(a, (b, c))? #!m((a, (b, c))?)
    #!m((a, c) | b) #!m((a, c) | b)* #!m(((a, c) | b)*) #!m((a, c) | b)+
    #!m(a | (b, c)) #!m((a | (b, c)*)+) #!m(a|(b,c)?)? #!m((a| (b, c))?)
    ))
(inspect *test-models*)
(inspect #!<!ELEMENT test (a | b)* >)
|#
:EOF
