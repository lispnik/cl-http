;;-*- Mode: Lisp; Package: XML-PARSER -*-
;;
;;	documented-definitions.lisp
;;
;;	copyright © 1992 Apple Computer, Inc.
;;                (well, the idea for styled-definitions was theirs)
;;      copyright © 1996 CNW
;;                permission is granted to apple/digitool to copy this file
;;                and/or to make it available for copying.
;;      copyright © 1997 MeCom GmbH
;;                permission is granted to apple/digitool to copy this file
;;                and/or to make it available for copying.
;;	This file will install a menu item in the Edit menu that sets the names
;;	in all of the definitions in the front window to the style specified in 
;;	*definition-style* (bold and underline look nice).  The action is 
;;	undo-able.  See the code below for an example.
;;
;;      This re-incarnation of <i>re</i>styled -definitions.lisp produces
;;      html form documents to document a set of definitions.
;;      the set of definitions may be specified as a file, a package, or a
;;      set of symbols. all cases are mapped to a set of symbols to document.
;;      the symbols must be bound to definitions.
;;      if a file is specified, then the source code will be used in addition
;;      to the definition, in order to include variable names.

(in-package :xml-parser)

(export '(definition-element
          definition-element-undefined
          definition-element-class
          make-definition-element

          initialize-content
          ))
          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Mod History
;;
#|
<DOCUMENTATION>
<DESCRIPTION>
the original utility modified the font information on an editor buffer in order
to make the text more readable. this package includes tools which perform
the same task, the mechanism is, however, radically different.
<BR>
the original utility merged the structural analysis of the source code with the
parsing of a text buffer. this version defines an abstract hierarchy of analysis
classes and permits source-specific specializations to perform the necessary
parsing.
<BR>
it also uncouples the goal operations from the analysis classes to support
not only the original styling of the source text but also other transformation
and analysis tasks, such as mapping to html, java, or odmg descriptions.
</DESCRIPTION>
<CHRONOLOGY>
<DT>02/05/92 drw
<DD>First draft for MCL 2.0 - Derek White.
<DT>960310 jaa.cnw
<DD>define and generate styles through a class lattice. (janson2@ibm.net)
<DT>960319 jaa.cnw
<DD>guarded initialize-instance for function-definitions and special-forms
 against empty body...
<DT>960417 jaa.cnw
<DD>initialize-instance (sc::function-definition) correct documentation string
 syntax (snkreutt@cip.informatik.uni-erlangen.de)
<DT>971918 jaa mecom
<DD>rewrote with distinct lexical, structural, and transformational classes.
</CHRONOLOGY>
|#

;; the style-class package comprises only the names of the forms for which
;; a style is defined. note that it does not inherit from anything. this is
;; because its internal symbols will include, for example DEFUN, DEFMACRO,
;; and all sorts of other CL symbols to which one really should not
;; bind classes...

(defPackage "DOCUMENTATION-CLASSES" (:use) (:nicknames :dc))

(defGeneric definition-element-class
  (context member source)
  (:documentation
   "determines the correct class for a definition element corresponding to the specified
    dtd element in the given context."))

(defGeneric initialize-definition
  (element)
  (:documentation
   "initializes the element content based on the definition class and source"))

(defGeneric initialize-definition-content
  (element model source))

(defGeneric transform-definition
  (element)
  (:documentation
   "initializes the element content based on the definition class and transform
    and destination."))

(defGeneric transform-definition-content
  (element transform destination))

(defGeneric restrict-source
  (context member source))

#|
 abstract classes are defined for the structural elements of the
 various definition forms. the structural analysis is accomplished by virtue
 of an association between a definition element and a dtd. where the dtd refers to
 additional substructure, the necessary subelements are instantiated and
 used to extract the respective aspects of the definition. the process
 proceeds recursively until the element has as its model PCDATA, at which point
 the content is a character string, or a s-expression, or some other elementary
 value depending on the source

 the definition elements include:
  DEFCLASS, DEFCONSTANT, DEFUN, DEFGENERIC, DEFMACRO,
  DEFMETHOD, DEFPARAMETER, DEFVAR

 the necessary constituitive elements are provided for the respective sub-forms.

 additional elements are provided by
 <LI>definining a dtd element,
 <LI>defining the respective definition element class
 <LI>providing methods to transform or restrict the source for the individual
     constitutive elements
 <LI>providing a transformation method for the repective destinations
 </P>

 all definition elements are defined in the :DC package to avoid any conflict
 with the forms defined in the :common-lisp package for definitions elements
 which correspond to lisp special forms operators.

 |#

(defClass DC::%DEFINITION-ELEMENT (xml-node)
  ((model
    :initarg :model
    :reader definition.model)
   (description
    :initarg :description
    :initform nil
    :accessor definition.description)
   (source
    :initarg :source
    :reader definition.source)
   (destination
    :initarg :destination
    :reader definition.destination)
   (transform
    :initarg :transform
    :initform nil
    :accessor definition.transform))
  (:documentation
   "<CODE>DC::DEFINITION</CODE> is the base class for all definition
    transformation classes. it binds, as <CODE>DEFINITION.SOURCE</CODE>, the
    source for information about the definition, and, as
    <CODE>DEFINITION.TRANSFORM</CODE>, a sequence of operations to perform to
    effect the transformation.
    <CODE>DEFINITION.DESCRIPTION</CODE> binds a markup element which collects
    the definition elements from the source.
    <P>
    depending on the definition class elements are collected of the type:
    <LI>name
    <LI>arguments
    <LI>documentation
    <LI>slots
    <LI>body
    <LI>references
    </P>"))

(defMethod definition-element-documentation
           (class (element symbol)
            &aux (definition (ignore-errors (dtd-element element))))
  (if definition
    (definition-element-documentation definition)
    (format nil
            "the class of definition interfaces for operator ~s.
no structure declaration was found."
            class definition)))

(defMethod definition-element-documentation
           (class (element dtd-element))
  (format nil "the class of definition interfaces for operator ~a.
the structure follows from the element:~%~a"
          class element))

;(definition-element-documentation 'defclass)

(defMethod define-definition-element-class
           ((element dtd-element)
            &aux (element-name (dtd-element.name element))
                 (class-name (intern (string element-name) :dc)))
  (unless (ignore-errors (dtd-element.model element))
    (warn "unusable model for element: ~a." element))
  (mop:ensure-class class-name
   :direct-superclasses '(dc::%definition-element)
   :direct-default-initargs `(:dtd-element ',element-name)
   :documentation (definition-element-documentation class-name element)))

(defMethod define-definition-element-class
           ((element xml-comment))
  nil)

(defMethod define-definition-element-class
           ((element symbol))
  (when (setf element (dtd-element element))
    (define-definition-element-class element)))


#|
 the initial conditions are, that a given source datum is specified to have the form
 given by the model of a specified dtd element.
  known context element, specific potential member element,
  source restricted to the context element.

 the analysis protocol includes three phases:
  member class selection:
   determine the member definition element for the dtd element with the given context
   definition element and context source data, and
  if the member element exists, then
  source restriction:
   identify the correct source data for a specific member definition element given a
   context definition element and context source data, and
  if the source can be appropriately restricted, then
  content instantiation:
   generate the correct definition element given the context definition element,
   the member dtd element and the member data from above.

 in the boundary case, the initial conditions have a null context definition element,
 the definition element is a projection of the operator present in the datum, and the
 source datum is taken as specified.

 the analysis then proceeds recursively, with each member definition element
 instantiating all possible dtd elements from the respective model.
 if it is not possible to restrict the source data, then the member element is
 eliminated.

 |#

(defMethod definition-element-undefined
           ((context t) (member t) (source t))
  (warn "no definition element defined for : ~s : ~s : ~s."
        context member source))

(defMethod definition-element-class
           ((context t) (member dtd-element) (source t))
  (definition-element-class context (dtd-element.name member) source))

(defMethod definition-element-class
           ((context t) (member symbol) (source t))
  (or (find-class (intern (string member) :dc) nil)
      (call-next-method)))

(defMethod definition-element-class
           ((context t) (member t) (source t))
  (definition-element-undefined context member source))



(defMethod make-definition-element
           ((context dc::%definition-element) (member symbol) (source t))
  (when (setf member (definition-element-class context member source))
    (make-definition-element context member source)))

(defMethod make-definition-element
           ((context dc::%definition-element) (member class) (source t))
  (when (setf source (restrict-source context (class-prototype member) source))
    (list (make-instance member :context context :source source))))

(defMethod make-definition-element
           ((context dc::%definition-element) (member dtd-element) (source t))
  (make-definition-element context (dtd-element.name member) source))


(defMethod make-definition-element
           ((context null) (member class) (source t))
  (when (setf source (restrict-source context (class-prototype member) source))
    (list (make-instance member :context context :source source))))

(defMethod make-definition-element
           ((context null) (member symbol) (source t))
  (when (setf member (definition-element-class context member source))
    (make-definition-element context member source)))

(defMethod restrict-source
           ((context t) (member t) (source t))
  source)



(defMethod initialize-instance
           ((self dc::%definition-element) &rest initargs
            &key name (dtd-element name) model)
  (setf dtd-element (definition-dtd-element dtd-element))
  (unless model (setf model (dtd-element.model dtd-element)))
  (apply #'call-next-method self
         :dtd-element dtd-element
         :model model
         initargs)

  (initialize-definition self))

(defMethod initialize-definition
           ((self dc::%definition-element))
  (setf (xml-node.content self)
        (initialize-definition-content self (definition.model self)
                                       (definition.source self))))

;(inspect (dtd-element 'argument))

(defMethod initialize-definition-content
           ((self dc::%definition-element) (model dtd-model-group) (source t))
  (case (dtd-model.connector model)
    (#.*seq-marker* (apply #'append
                 (mapcar #'(lambda (member)
                             (initialize-definition-content self member source))
                         (dtd-model.content model))))
    (#.*or-marker* (dolist (member (dtd-model.content model))
           (when (setf member (make-definition-element self member source))
             (return member))))
    (#.*and-marker* (apply #'append
                 (mapcar #'(lambda (member)
                             (or (initialize-definition-content self member source)
                                 (return-from initialize-definition-content nil)))
                         (dtd-model.content model))))))

(defMethod initialize-definition-content
           ((self dc::%definition-element) (model dtd-model) (source t))
  (definition-content-for-model self (dtd-model.content model)
    (dtd-model.occurrence model) source))

     
     
(defMethod initialize-definition-content
           ((self dc::%definition-element) (member dtd-element) (source t))
  (make-definition-element self member source))

:EOF
