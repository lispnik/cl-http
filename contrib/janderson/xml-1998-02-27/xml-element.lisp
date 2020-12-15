;;; -*- mode: LISP; package: ("XML-PARSER")

#|
<DOCUMENTATION>
<DESCRIPTION>
 </DESCRIPTION>
<CHRONOLOGY>
 <DATE>19971020</DATE>
  <DELTA>jaa: READ-START-TAG: UNWIND-PROTECT eingefuegt</DELTA>
 <DATE>19971023</DATE>
  <DELTA>jaa
   ATTDEFS feld im xml-element in ATTVALS umgeaendert, um den unterschied zum
   ATTDEFS im entsprechenden dtd element zu zeigen.
   </DELTA>
 <DATE>19971027</DATE>
  </DELTA> jaa rewritten to distinguish the basic markup parsing mechanism
  (in "markup-reader.lisp") from the specifics for xml (here) or for a dtd
  (in "dtd-parser.lisp").
  <LI>initialize-instance (xml-element)
      allows dtd-element-declarations by value in addition to by name
  <LI>read-typed-markup-element (symbol t)
      changed the strategy used to assert the correct dtd:
      try to find the element definition. if one is found, use its dtd.
      if none is found, assert a null dtd and the "XML-USER" package.
  </DELTA>
 <DATE>19971228</DATE>
  <DELTA>xml-element.get corected for distinct attribute mode and value
   </DELTA>
 <DATE>19971228</DATE>
  <DELTA>XML-NODE.ATTRIBUTES</DELTA>
 <DATE>19971230</DATE>
 <DELTA>node-class invocation passes the element name, as it should in order
  to elect an element-specific class for the instance</DELTA>
 </CHRONOLOGY>
</DOCUMENTATION>
 |#

(in-package :XML-PARSER)



(defMethod node-class
           ((node (eql 'element)) (name symbol) (stream t))
  (if (subtypep name 'xml-element-interface)
    name
    *xml-element-class*))

(defMethod node-class
           ((node (eql 'element)) (decl dtd-element-declaration) (stream t))
  (node-class node (dtd-element-declaration.name decl) stream))

;;;
;;;

(defClass xml-element-interface ()
  ()
  (:documentation
   "a marker class to specify conformance to the element interface."))

(defClass xml-element (xml-comment-node xml-named-node xml-element-interface)
  ((parent
    :accessor xml-element.context
    :initarg :context)
   (content
    :accessor xml-element.content
    :initarg :content)
   (name
    :accessor xml-element.declaration
    :initarg :declaration
    :type (or symbol dtd-element)
    :documentation
    "the name is initialized through a symbol which is mapped to a
     <CODE>DTD-ELEMENT</CODE>.
     the declaration then prescribes the general structure for the node.")
   (attributes
    :accessor xml-element.attributes
    :initarg :attributes
    :initform nil
    :type #+:mcl (list xml-attribute) #-:mcl list
    :documentation
    "binds a list of attributes, each of which comprises an indicator
     and a value.")
   (valid?
    :reader xml-element.get-valid?
    :writer xml-element.set-valid?
    :type symbol))
  (:documentation
   "the class of XML data elements.
    it binds the containing node as <CODE>XML-ELEMENT.CONTEXT</CODE>,
    the document type declaration element as <CODE>XML-ELEMENT.DECLARATION</CODE>,
    the instance-specific attributes as <CODE>XML-ELEMENT.ATTRIBUTES</CODE>,
    and the contents as <CODE>XML-ELEMENT.CONTENT</CODE>."))

(defMethod initialize-instance :after
           ((self xml-element) &key)
  (dolist (a (xml-element.attributes self))
    (link-nodes self a)))

(defMethod xml-node.name
           ((node xml-element))
  (xml-node.name (xml-element.declaration node)))

(defMethod xml-element.name
           ((node xml-element))
  (xml-node.name node))

(defMethod dtd-element.name
           ((node xml-element))
  (dtd-element.name (xml-element.declaration node)))

(defMethod element.dtd
           ((element xml-element))
  (element.dtd (xml-element.declaration element)))

(unless (fboundp 'xml-element-derive-valid?)
  (defMethod xml-element-derive-valid?
             ((element xml-element) (succeed t) (fail t))
    "this methods serves as a place-holder for the later model-based validation."
    t))

(defMethod dtd-element-declaration.model
           ((node xml-element) &aux (decl (xml-element.declaration node)))
  (when decl
    (dtd-element-declaration.model decl)))

(defMethod xml-element.valid?
           ((element xml-element) &optional (error t))
  (if (slot-boundp element 'valid?)
    (xml-element.get-valid? element)
    (let ((succeed nil)
          (fail nil))
      (setf succeed #'(lambda (datum inner-fail)
                        (declare (ignore datum inner-fail))
                        (xml-element.set-valid? t element)
                        (return-from xml-element.valid? t))
            fail (typecase error
                   (null #'(lambda (datum inner-succeed model)
                             (declare (ignore model))
                             (xml-element.set-valid? nil element)
                             (when *xml-verbose*
                               (warn "element invalid: ~s/~s." element datum))
                             (funcall inner-succeed datum fail)))
                   (function #'(lambda (datum inner-succeed model)
                                 (xml-element.set-valid? nil element)
                                 (funcall error datum inner-succeed model)
                                 (return-from xml-element.valid?
                                   (values nil datum model))))
                   (t #'(lambda (datum inner-succeed model)
                          (declare (ignore inner-succeed))
                          (xml-element.set-valid? nil element)
                          (return-from xml-element.valid?
                            (values nil datum model))))))
      (%xml-element.valid? element succeed fail))))

(defMethod xml-element-derive-valid?
           ((element xml-element) succeed fail
            &aux (decl (xml-element.declaration element))
                 (predicate
                  (when decl (dtd-element-declaration.predicate decl))))
  (if predicate
    (progn (xml-element.content element)
           (funcall predicate (xml-element.content element) succeed fail))
    (progn (when *xml-verbose* (warn "no declaration for element: ~s." element))
           (funcall succeed element fail))))

(defMethod %xml-element.valid?
           ((element xml-element) (succeed t) (fail t))
  (if (slot-boundp element 'valid?)
    (if (xml-element.get-valid? element)
      (funcall succeed element fail)
      (funcall fail element succeed (dtd-element-declaration.model element)))
    (xml-element-derive-valid? element succeed fail)))

(defMethod (setf xml-element.content) :after
           ((datum t) (element xml-element))
  (slot-makunbound element 'valid?))


(defMethod xml-node.get-attdef
           ((node xml-element) indicator)
  (xml-node.get-attdef (xml-element.declaration node) indicator))

(defParameter *global-attribute-definitions*
  (list (make-instance 'dtd-attdef :name 'xsl::type
                       :type 'xml:id
                       :default nil)))

(defMethod xml-node.get-attdef
           ((node t) (indicator t))
  (find indicator *global-attribute-definitions* :key #'dtd-attdef.name))

(defMethod xml-node.attributes
           ((node xml-element))
  (append (xml-element.attributes node)
          (xml-node.attributes (xml-element.declaration node))))


(defMethod xml-element.get
           ((node xml-element) indicator &aux att)
  (if (setf att (find indicator (xml-element.attributes node)
                      :key #'xml-attribute.name))
    (xml-attribute.value att)
    (when (setf att (xml-node.get-attdef node indicator))
        (case (dtd-attdef.mode att)
          ((xml::\#IMPLIED xml::\#REQUIRED) nil)
          (t (dtd-attdef.default att))))))

(defMethod xml-element.set
           ((node xml-node) indicator value &aux att)
  (if (setf att (find indicator (xml-element.attributes node)
                      :key #'xml-attribute.name))
    (setf (xml-attribute.value att) value)
    (progn
      (setf (xml-element.attributes node)
            (nconc (xml-element.attributes node)
                   (list (make-instance (node-class 'attribute indicator value)
                           :name indicator
                           :value value
                           :parent node))))
      value)))

(defMethod (setf xml-element.get)
           (datum (node xml-element) indicator)
  (xml-element.set node indicator datum))


;;; pattern/datum interface
;;;

(defMethod xml-pattern.attributes ((node xml-element))
  (xml-element.attributes node))

(defMethod xml-pattern.relations
           ((node xml-element))
  (xml-element.content node))

(defMethod xml-pattern.type
           ((node xml-element))
  ; nope (xml-node.name (dtd-model.content node))
  (or (xml-element.get node *xsl-type-attribute*)
      (xml-element.name node)))

(defMethod xml-pattern.occurrence ((node xml-element))
  (xml-element.get node 'xml::occurrence))


(defMethod xml-datum.attributes ((node xml-element))
  (xml-element.attributes node))

(defMethod xml-datum.relations
           ((node xml-element))
  (xml-element.content node))

(defMethod xml-datum.type
           ((node xml-element))
  (or (xml-element.get node *xsl-type-attribute*)
      (xml-element.name node)))



(defMethod initialize-instance-attributes
           ((node xml-element) (attribute xml-attribute) (attlist list)
            &aux old)
  (if (setf old (find (xml-attribute.name attribute) (xml-element.attributes node)
                      :key #'xml-attribute.name))
    (setf (xml-element.attributes node)
          (nsubstitute attribute old (xml-element.attributes node)))
    (push attribute (xml-element.attributes node)))
  (initialize-instance-attributes node (first attlist) (rest attlist)))

(defMethod initialize-instance-attributes
           ((node xml-element) (null null) (attlist t))
  node)

(defMethod initialize-instance-attributes
           ((node xml-element) (name t) (attlist list))
  (xml-element.set node name (first attlist))
  (initialize-instance-attributes node (second attlist) (cddr attlist)))

(defMethod initialize-instance
           ((self xml-element) &rest initargs
            &key attributes name (declaration name))
  ;(print (list 'i-i :name name :declaration declaration))
  (typecase declaration
    (null (error "a declaration must be specified for a ~s." (type-of self)))
    (symbol (setf declaration (dtd-element-reference declaration)))
    (dtd-element-reference t)
    (dtd-element (setf declaration (dtd-element-reference declaration)))
    (t (error "illegal declaration specified for a ~s: ~s."
              (type-of self) declaration)))
  (apply #'call-next-method self
         :attributes nil
         :declaration declaration
         initargs)
  (initialize-instance-attributes self (first attributes) (rest attributes))
  
  (dolist (node (xml-element.content self))
    (setf (xml-element.context node) self)))

(defMethod (setf xml-element.context)
           ((node xml-element) (datum t))
  datum)




(defMethod read-xml-element
           ((source t) &optional (*xml-element-class* *xml-element-class*)
            &aux (*package* *package*))
  (read-markup-element source))

(defMethod xml-element
           ((datum string))
  (read-xml-element datum))

(defMethod xml-element
           ((datum stream))
  (read-xml-element datum))

(defMethod xml-element
           ((node xml-element))
  node)

(defun make-element
       (decl &rest content)
   (make-instance (node-class 'element decl content)
     :declaration decl
     :content content))


(defMacro with-node-attributes
          (bindings node &rest body &aux (node-var (gensym)))
  `(let ((,node-var ,node))
     (symbol-macrolet ,(mapcar #'(lambda (binding)
                                   `(,(if (consp binding)
                                        (first binding) binding)
                                     (xml-element.get ,node-var
                                                   ,(if (consp binding)
                                                      (second binding)
                                                      binding))))
                               bindings)
       ,@body)))

;;;
;;; the specialization for otherwise indistinguished symbols constructs
;;; the tag for an element and recursively reads the stream until the
;;; matching close tag is discovered.
;;; the active dtd and package are adjusted to read relative to the respective
;;; element and element's dtd. first the declaration is located relative to
;;; the active dtd, then that declaration's own dtd is activated.

(defmethod read-typed-markup-element
           ((tag-name symbol) (stream t)
            &aux (*package* *package*)
                 (*dtd* *dtd*)
                 declaration
                 child)
  (setf declaration (dtd.dtd-element-reference *dtd* tag-name)
        *dtd* (dtd-element.dtd declaration)
        *package* (dtd.package *dtd*))
  (let* ((attributes (read-markup-tag-attributes tag-name stream))
         (empty? (eq (first (last attributes)) *close-tag-marker*))
         (*parent-node* (make-instance (node-class 'element tag-name stream)
                          :declaration tag-name
                          :attributes (if empty?
                                        (subseq attributes 0 (1- (length attributes)))
                                        attributes))))
    (unless empty?
      (loop (setf child (read-process-pcdata stream tag-name))
            (when (or (eq tag-name child) (eq child *close-tag-marker*))
              (return))))
    *parent-node*))


(defMethod process-markup-element
           ((node xml-element) (stream t))
  (xml-node.append-element *parent-node* node))


(defMethod xml-node.append-element
           ((parent xml-element) (child string))
  (setf (xml-node.children parent)
        (nconc (xml-node.children parent) (list child)))
  child)

(defMethod xml-node.append-element :after
           ((element xml-element) (datum t))
  (slot-makunbound element 'valid?))


(defMethod dtd.dtd-element
           ((parent xml-element) (id symbol))
  "resolve an element id relative to the parent's element declaration."
  (dtd.dtd-element (xml-element.declaration parent) id))

(defMethod build-content-element-reference-name
           ((parent xml-element) (name symbol))
  (build-content-element-reference-name (xml-node.name parent) name))


;;;
;;; XML-Printer
;;;

(defparameter *print-xml-form* t)
(defparameter *xml-form-indent* 0)

(defmethod print-start-tag
           ((datum xml-element) (stream t)
            &aux (*readtable* *markup-type-readtable*))
  (format stream "~A~S" *start-tag-open* (xml-element.name datum))
  (setf *readtable* *markup-attribute-readtable*)
  (format stream "~@[ ~{~A~^ ~}~]~:[~;/~]~A"
          (xml-element.attributes datum)
          (unless *xml-print-empty-close-tags* (null (xml-element.content datum)))
          *tag-close*))



(defmethod print-object
           ((*parent-node* xml-element) stream
            &aux (*readtable* *markup-pcdata-readtable*))
  (cond (*xml-print-readably*
         (print-start-tag *parent-node* stream)
         (let ((*xml-form-indent* (+ *xml-form-indent* 1))
               (*package* (markup-package (xml-element.name *parent-node*)))
               (content (xml-element.content *parent-node*)))
           (when *xml-preserve-comments*
             (dolist (comment (getf (xml-node.comments *parent-node*) *parent-node*))
               (when *print-pretty* 
                 (format stream "~&~v@t" *xml-form-indent*))
               (princ comment stream)))
           (typecase content
             (null nil)
             (cons (if (= (length content) 1)
                     (format stream "~a" (first content))
                     (dolist (x content)
                       (when *print-pretty* 
                         (format stream "~&~v@t" *xml-form-indent*))
                       (typecase x
                         (string
                          (write-string x stream))
                         (t
                          (princ x stream)))
                       (when *xml-preserve-comments*
                         (dolist (comment (getf (xml-node.comments *parent-node*) x))
                           (when *print-pretty* 
                             (format stream "~&~v@t" *xml-form-indent*))
                           (princ comment stream)))))
                   (when (and *print-pretty* (/= (length content) 1))
                     (format stream "~&~v@t" *xml-form-indent*)))
             (t (when *print-pretty*
                  (format stream "~&~v@t" *xml-form-indent*))
                (format stream "~A" content))))
         (when (or *xml-print-empty-close-tags* (xml-element.content *parent-node*))
           (print-end-tag *parent-node* stream)))
        (t 
         (print-unreadable-object (*parent-node* stream :identity *xml-verbose*
                                                 :type t)
           (format stream "~A" (xml-element.name *parent-node*))))))

#|
(let ((*xml-print-readably* t))
  (print (make-instance 'xml-element :name 'test :content (list "ASDF")
                        :attributes (list (make-instance 'xml-attribute
                                            :content "TESTING"
                                            :name 'name)))))
 |#

#|
(defun xml-attribute-form (av-list dtd-attdefs)
  (declare (ignore dtd-attdefs)) ; evtl. spaeter defaultwerte uebernehmen
  (if av-list
    (loop with stream = (make-string-output-stream)
          for (a v) on av-list by #'cddr
          do
          (if (consp v) 
            ;white space weder vor noch nach = zulaessig!
            (format stream " ~A='~{~A~^ ~}'" a v)
            (format stream " ~A='~A'" a v))
          finally return (get-output-stream-string stream))
    ""))

(defmethod print-object ((object xml-element) stream)
  (let* ((dtd-element (xml-element.dtd-element object))
         (dtd-element-name (dtd-element.name dtd-element)))
    (cond (*xml-print-readably*
           (let ((indent *xml-form-indent*))
           (format stream "~&~v@t<~A~A>" ;{ ~A = '~A'~}>
                   indent
                   dtd-element-name
                   (xml-attribute-form 
                    (xml-element.attdefs object) (dtd-element.attdefs dtd-element)))
           (let ((*xml-form-indent* (+ indent 3))
                 (content (xml-element.content object)))
             (if (listp content)
               (loop for x in content
                     if (stringp x)
                     do (format stream "~&~v@t~A" *xml-form-indent* x)
                     else do (print-object x stream))
               (print-object content stream)))
           (format stream "~&~v@t</~A>" indent dtd-element-name)))
          (t (print-unreadable-object (object stream :identity nil :type nil)
               (format stream "<xml ~A>" dtd-element-name))))))
|#
:EOF
