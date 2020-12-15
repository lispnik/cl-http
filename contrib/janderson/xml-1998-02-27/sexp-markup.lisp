
(in-package :XML-PARSER)

#|
 an s-expression -> xml-grove parser

 is built based on a pattern/model-directed data matcher.
 the matcher provides a control-mechanism guided by a dtd-model or element
 pattern. the translation is performed here by provided methods for
 successful match which generate the respective descriptive elements.
 |#




;;; analysis is accomplished by retrieving the dtd model and generating one
;;; for each model member. 

(defMethod make-sexp-element
           ((sexp cons)
            &aux (dtd-element (dtd.dtd-element *lisp-dtd* (first sexp))))
  (if dtd-element
    (make-sexp-element-w-dtd-element dtd-element sexp)
    (mapcar #'make-sexp-element sexp)))

(defMethod make-sexp-element
           ((sexp t))
  sexp)

(defMethod make-sexp-element-w-dtd-element
           ((dtd-element dtd-element) (sexp cons))
  (let* ((type-name (dtd-element.name dtd-element))
         (*parent-node*
          (make-instance (sexp-element-class (xml-node.name *parent-node*)
                                             (first sexp)
                                             sexp)
            :declaration (first sexp)))
         (content nil))
    (when (setf content
                (make-sexp-element-w-model (dtd-element.model dtd-element)
                                           sexp))
      (dolist (child content)
        (xml-node.append-element *parent-node* child))
      *parent-node*)))

(defMethod make-sexp-element-w-model
           ((model dtd-model) (sexp cons))
  (make-sexp-element-w-model-components
   (dtd-model.occurrence model)
   (dtd-model.connector model)
   (dtd-model.content model)
   sexp))

(defMethod make-sexp-element-w-model
           (decl (model (eql 'xml::\#PCDATA)) (sexp t))
  (make-instance 'xml-element
    :declaration decl :content (list sexp)))

(defMethod make-sexp-element-w-model
           (decl (model (eql 'xml::\#PCDATA)) (sexp list))
  (make-instance 'xml-element
    :declaration decl :content sexp))


(defMethod make-sexp-element-w-model
           ((connector :seq) (occurrence 1) (element-name symbol) sexp)
  (let ((model (dtd-element.model element-name)))
    (if (eq model 'xml::\#PCDATA)
      
      (make-sexp-element-w-model (dtd-element.model symbol) sexp))))
    
    
        


(defMethod sexp-element-class
           ((element (eql 'operator)) (op t) (source list))
  'lisp-operator-element)

(defMethod sexp-element-class
           ((element (eql 'name)) (op t) (source list))
  'lisp-definition-name-element)

(defMethod sexp-element-class
           ((element (eql 'argument-list)) (op defun) (source list))
  'lisp-argument-list-element)

(defMethod sexp-element-class
           ((element (eql 'argument-list)) (op defMethod) (source list))
  'lisp-specialized-argument-list-element)

(defMethod sexp-element-class
           ((element (eql 'argument-list)) (op defMacro) (source list))
  'lisp-argument-list-element)





  (let ((*parent-node* (make-instance
                         (sexp-element-class (first sexp)










(defMethod definition-element-class
           ((context null) (member null) (source list))
  (if (setf member (first source))
    (definition-element-class context member source)
    (definition-element-undefined context member source)))

;;; defclass definition structure
(defMethod restrict-source
           ((context dc::defClass) (element dc::name) (source list))
  (second source))

(defMethod restrict-source
           ((context dc::defClass) (element dc::class-list) (source list))
  (third source))

(defMethod restrict-source
           ((context dc::defClass) (element dc::slot-list) (source list))
  (fourth source))

(defMethod restrict-source
           ((context dc::defClass) (element dc::documentation) (source list))
  (assoc :documentation (nthcdr 4 source)))

(defMethod restrict-source
           ((context dc::defClass) (element dc::metaclass) (source list))
  (assoc :metaclass (nthcdr 4 source)))

;;; defconstant definition structure
(defMethod restrict-source
           ((context dc::defConstant) (element dc::name) (source list))
  (second source))

(defMethod restrict-source
           ((context dc::defConstant) (element dc::value) (source list))
  (third source))

(defMethod restrict-source
           ((context dc::defConstant) (element dc::documentation) (source list))
  (fourth source))


;;; defun definition structure
(defMethod restrict-source
           ((context dc::defun) (element dc::name) (source list))
  (second source))

(defMethod restrict-source
           ((context dc::defun) (element dc::argument-list) (source list))
  (third source))

(defMethod restrict-source
           ((context dc::defun) (element dc::body) (source list))
  (when (typep 'string (first (nthcdr 3 source)))
    (first (nthcdr 3 source))))

(defMethod restrict-source
           ((context dc::defun) (element dc::body) (source list))
  (if (typep 'string (first (nthcdr 3 source)))
    (nthcdr 4 source)
    (nthcdr 3 source)))

;;; defgeneric definition structure
(defMethod restrict-source
           ((context dc::defgeneric) (element dc::name) (source list))
  (second source))

(defMethod restrict-source
           ((context dc::defgeneric) (element dc::argument-list) (source list))
  (third source))

(defMethod restrict-source
           ((context dc::defClass) (element dc::documentation) (source list))
  (assoc :metaclass (nthcdr 3 source)))

;;; defmacro definition structure
(defMethod restrict-source
           ((context dc::defmacro) (element dc::name) (source list))
  (second source))

(defMethod restrict-source
           ((context dc::defmacro) (element dc::argument-list) (source list))
  (third source))

(defMethod restrict-source
           ((context dc::defmacro) (element dc::documentation) (source list))
  (when (typep 'string (first (nthcdr 3 source)))
    (first (nthcdr 3 source))))

(defMethod restrict-source
           ((context dc::defmacro) (element dc::body) (source list))
  (if (typep 'string (first (nthcdr 3 source)))
    (nthcdr 4 source)
    (nthcdr 3 source)))

;;; defmethod definitions
(defun arglist-position (sexp) (position-if #'listp sexp :start 2))

(defMethod restrict-access
           ((context dc::defmethod) (element dc::name) (source list))
  (second source))

(defMethod restrict-access
           ((context dc::defmethod) (element dc::qualifier-list) (source list)
            &aux (pos (arglist-position source)))
  (when pos
    (subseq source 2 pos)))

(defMethod restrict-source
           ((context dc::defmethod) (element dc::argument-list) (source list))
  (find-if #'listp source :start 2))

(defMethod restrict-access
           ((context dc::defmethod) (element dc::documentation) (source list)
            &aux (pos (arglist-position source)))
  (when pos
    (when (typep 'string (first (nthcdr (1+ pos) source)))
      (first (nthcdr (1+ pos) source)))))

(defMethod restrict-access
           ((context dc::defmethod) (element dc::body) (source list)
            &aux (pos (arglist-position source)))
  (when pos
    (if (typep 'string (first (nthcdr (1+ pos) source)))
      (nthcdr (+ 2 pos) source)
      (nthcdr (+ 1 pos) source))))

;;; defparameter definition structure
(defMethod restrict-source
           ((context dc::defParameter) (element dc::name) (source list))
  (second source))

(defMethod restrict-source
           ((context dc::defParameter) (element dc::value) (source list))
  (third source))

(defMethod restrict-source
           ((context dc::defParameter) (element dc::documentation) (source list))
  (fourth source))



;;; defvar definition structure
(defMethod restrict-source
           ((context dc::defVar) (element dc::name) (source list))
  (second source))

(defMethod restrict-source
           ((context dc::defVar) (element dc::value) (source list))
  (third source))

(defMethod restrict-source
           ((context dc::defVar) (element dc::documentation) (source list))
  (fourth source))



;; now the member structure
(defMethod restrict-source
           ((context dc::argument) (element dc::name) (source list))
  (first source))

(defMethod initialize-element-content
           ((context dc::name) (element (eql 'PCDATA)) (source symbol))
  (list source))

(defMethod initialize-element-content
           ((context dc::argument-list) (element (eql 'ARGUMENT)) (source list)
            &aux options)
  (remove nil
          (mapcar #'(lambda (arg)
                      (cond ((and (symbolp arg)
                                  (char-equal (elt (string arg) 0) #\&))
                             (setf options arg)
                             nil)
                            ((symbolp arg)
                             (make-instance 'argument-element
                               :context context
                               :source arg))
                            ((consp arg)
                             (make-instance (if options
                                              'dc::defaulted-argument
                                              'dc::specialized-argument)
                               :context context
                               :source arg))
                            (t
                             (warn "unrecognized argument form : ~s ." arg)
                             nil)))
                  source)))

(defMethod initialize-element-content
           ((context dc::body) (element (eql 'PCDATA)) (source list))
  (list (definition.source context)))

(defMethod make-definition-element
           ((context null) (member null) (source list))
  (make-definition-element context (first source) source))
           

#|
(inspect (dtd-element 'defclass))
(inspect (dtd-element 'ARGUMENT-LIST))
(map nil #'(lambda (x)
             (print x)
             (when (typep x 'dtd-element) (print (dtd-element.model x))))
     (dtd.elements *lisp-dtd*))
(make-definition-element nil nil '(defclass test (testing) ()))
|#

:EOF
