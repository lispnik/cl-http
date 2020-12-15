;;; -*- mode: LISP; package: "XML-PARSER"; -*-
;;;
;;; this version (C) mecom gmbh 24.11.97
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.

"
<DOCUMENTATION>
 <COPYRIGHT HREF='defsystem.lisp|root().descendant(COPYRIGHT)'/>
 <CHRONOLOGY>
  <DATE>19971128</DATE>
  <DELTA>
   <LI>element-declaration.model (element-model)
      added to retrieve from the referent declaration
   <LI>#PCDATA model is always with occurrence *
   </DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
 "

(in-package "XML-PARSER")

#|
                      element                 group
---------------------------------------------------------------
parent                (or null dtd-element)   (or null dtd-element)
content               dtd-element             (list dtd-element)
occurrence            '1...n', '*', '?', '+'  '1...n', '*', '?', '+'
connector                                     '&', '|', ','
|#


(defClass abstract-element-model (xml-pattern xml-node)
  ((context
    :accessor element-model.context)
   (content
    :accessor element-model.content))
  (:documentation
   "the <CODE>abstract-element-model</CODE> class is the base abstract class for
    dtd models. this comproses both declared and reserved models."))
     
(defClass reserved-model (abstract-element-model)
  ()
  (:documentation
   "the <CODE>RESERVED-MODEL</CODE> class designate model elements
    which denote reserved model components (#PCDATA, ANY, EMPTY)"))

(defMethod element-model.occurrence
           ((datum reserved-model))
  (with-slots (content) datum
    (if (eq content 'xml::\#PCDATA) *rep-marker* 1)))

(defClass declared-model (abstract-element-model)
  ((occurrence
    :initarg :occurrence :initform 1
    :accessor element-model.occurrence))
  (:documentation
   "the <CODE>declared-model</CODE> class is the generic class
    for original models.
    the minimal combinations specifies an occurrence for an
    <CODE>element-model.CONTENT</CODE> element and 
    an <CODE>element-model.OCCURRENCE</CODE>."))

(defClass element-model (declared-model)
  ()
  (:documentation
   "the <CODE>element-model</CODE> class is the concrete specialization for
    single element names."))


(defClass element-model-group (declared-model)
  ((connector
    :initarg :connector
    :accessor element-model.connector))
  (:documentation
   "the <CODE>element-model-GROUP</CODE> class provides for concrete dtd model
    groups. a group specifies a connector (',' '|', '&') for a sequence of
    elements."))


(defGeneric element-model.name (datum)
  (:method ((name string)) name)
  (:method ((name symbol)) name)
  (:method ((name xml-node)) (xml-node.name name))
  (:method ((node reserved-model))
           (element-model.name (element-model.content node)))
  (:method ((node element-model))
           (element-model.name (element-model.content node)))
  (:method ((node element-model-group))
           (xml-node.name (element-model.context node))))

(defMethod element-declaration.model
           ((datum element-model) &aux (content (element-model.content datum)))
  (typecase content
    (element-declaration (element-declaration.model content))
    (t content)))


;(element-declaration.model (top-inspect-form))

(defMethod copy-node :after
           ((self element-model-group))
  (with-slots (content) self
    (setf content (mapcar #'copy-node content))))


(defGeneric element-model.occurrence-char (datum)
  (:method ((datum declared-model))
           (unless (eql (element-model.occurrence datum) 1)
             (element-model.occurrence-char (element-model.occurrence datum))))
  (:method ((datum reserved-model)) nil)
  (:method ((occurrence (eql *plus-marker*))) #\+)
  (:method ((occurrence (eql *rep-marker*))) #\*)
  (:method ((occurrence (eql *opt-marker*))) #\?))


;;; model comparison serves to determine if it is necessary to recompile
;;; the validity predicate

(defMethod element-model-equalp
           ((model1 t) (model2 t))
  (equalp model1 model2))

(defMethod element-model-equalp
           ((model1 list) (model2 list) &aux element1 element2)
  (loop (setf element1 (pop model1) element2 (pop model2))
        (when (and (null element1) (null element2)) (return t))
        (unless (element-model-equalp element1 element2)
          (return-from element-model-equalp nil))))

(defMethod element-model-equalp
           ((model1 abstract-element-model) (model2 abstract-element-model))
  (and (eql (element-model.occurrence model1) (element-model.occurrence model2))
       (element-model-equalp (element-model.content model1)
                             (element-model.content model2))))

(defMethod element-model-equalp
           ((model1 element-model-group) (model2 element-model-group))
  (and (eql (element-model.connector model1)
                   (element-model.connector model2))
       (call-next-method)))

(defmethod print-model-group ((connector (eql *seq-marker*)) object-list stream)
  (format stream "(~{~A~^, ~})" object-list))
(defmethod print-model-group ((connector (eql *or-marker*)) object-list stream)
  (format stream "(~{~A~^ | ~})" object-list))
(defmethod print-model-group ((connector (eql *and-marker*)) object-list stream)
  (format stream "(~{~A~^ & ~})" object-list))
(defmethod print-model-group ((connector t) object-list stream)
  (format stream "(~{~A~^... ~})" object-list))


(defMethod print-object ((datum reserved-model) stream)
  (if *print-readably*
    (write-identifier (ignore-errors (element-model.name datum))
                      stream)
    (print-unreadable-object (datum stream :type t)
      (format stream "~S" (ignore-errors (element-model.name datum))))))

(defMethod print-object ((datum element-model) stream)
  (flet ((print-it (&aux (name (ignore-errors (element-model.name datum))))
           (format stream "~:[~A~;~S~]~@[~A~]"
                   (stringp name) name
                   (element-model.occurrence-char datum))))
    (if *print-readably*
      (print-it)
      (print-unreadable-object (datum stream :type t)
        (print-it)))))


(defMethod print-object ((datum element-model-group) stream)
  (flet ((print-it (&aux (*print-readably* t))
           (print-model-group (element-model.connector datum)
                              (element-model.content datum)
                              stream)
           (unless (eql (element-model.occurrence datum) 1)
             (format stream "~a" (element-model.occurrence-char datum)))))
    (flet ((print-it-unreadably ()
             (print-unreadable-object (datum stream :type t)
               (format stream "~s " (ignore-errors (element-model.name datum)))
               (print-it))))
      (if *print-readably*
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

(defParameter *reserved-model* 'reserved-model)
(defParameter *element-model-group* 'element-model-group)
(defParameter *element-model* 'element-model)

(defMethod make-model ((content null))
  nil)

(defMethod make-model ((content abstract-element-model))
  content)

(defMethod make-model ((content symbol))
  (cond ((reserved-name-p content)
         (make-instance *reserved-model*
           :context *parent-node*
           :content content))
        (t
         (make-instance *element-model*
           :context *parent-node*
           :content content
           :occurrence 1))))

(defMethod make-model ((content string))
  (make-instance *element-model*
    :context *parent-node*
    :content content
    :occurrence 1))

(defMethod make-model ((content cons))
  (cond ((and (null (cddr content))
              (consp (first content)))
         (make-model-expression
          (make-instance *element-model-group*
            :context *parent-node*
            :content nil
            :occurrence (or (second content) 1)
            :connector nil)
          (caar content)
          (second (first content))
          (cddr (first content))))
        (t
         (make-model-expression (make-instance *element-model-group*
                                  :context nil; *parent-node*
                                  :occurrence 1
                                  :connector nil
                                  :content nil)
                                (first content)
                                (second content)
                                (cddr content)))))

(defMethod make-model-expression
           (group (model abstract-element-model) (separator symbol) (rest t) &aux content)
  (warn "separator missing: ~s ~s ~s." group model separator)
  (if (setf content (element-model.content group))
    (nconc content (list model))
    (setf (element-model.content group) (list model)))
  (make-model-expression group separator (first rest) (rest rest)))

(defMethod make-model-expression
           (group (model abstract-element-model) (separator null) (rest t) &aux content)
  (if (setf content (element-model.content group))
    (nconc content (list model))
    (setf (element-model.content group) (list model)))
  (when (null (element-model.connector group))
    (setf (element-model.connector group) *seq-marker*))
  group)

(defMethod make-model-expression
           (group (model abstract-element-model) (separator marker-oc) rest)
  (unless (eql (element-model.occurrence model) 1)
    (xml-form-error model
                    "duplicate repetition specified: ~s/~s."
                    (element-model.occurrence model) separator))
  (setf (element-model.occurrence model) separator)
  (make-model-expression group model (first rest) (rest rest)))

(defMethod make-model-expression
           (group (model abstract-element-model) (separator marker-sep) rest &aux content)

  (if (element-model.connector group)
    (unless (eq (element-model.connector group) separator)
      (xml-form-error group
                      "conflicting connectors specified: ~s."
                      separator))
    (setf (element-model.connector group) separator))
  (if (setf content (element-model.content group))
    (nconc content (list model))
    (setf (element-model.content group) (list model)))
  (make-model-expression group (first rest) (second rest) (cddr rest)))


(defMethod make-model-expression
           (group (model cons) (separator t) rest)
  (make-model-expression group (make-model model) separator rest))

(defMethod make-model-expression
           (group (model symbol) (separator t) rest)
  (make-model-expression group (make-model model) separator rest))

;; added in order to parse xml bnf and distinguish terminals which are
;; literal strings from other terminals and from nonterminal nodes
(defMethod make-model-expression
           (group (model string) (separator t) rest)
  (make-model-expression group (make-model model) separator rest))

#|
(defmethod make-model-expression :before
           (group model separator (rest null))
  (print (list group model separator rest)))

(defParameter 
  *test-models*
  '(#!ma #!m(a) #!m(#PCDATA) #!mANY #!mEMPTY #!m#PCDATA 
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
|#


"XMLP"
