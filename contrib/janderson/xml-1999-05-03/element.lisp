;;; -*- mode: LISP; package: "XML-PARSER"; -*-

"
<DOCUMENTATION>
 <DESCRIPTION>
  note the hierarchy: XML-SERIALIZABLE, XML-INTERFACE, element.
  </DESCRIPTION>
 <COPYRIGHT HREF='defsystem.lisp|root().descendant(1,COPYRIGHT)'/>
 <CHRONOLOGY>
  <DELTA><DATE>19971020</DATE>
   READ-START-TAG: UNWIND-PROTECT eingefuegt</DELTA>
  <DELTA><DATE>19971023</DATE>
   ATTDEFS feld im element in ATTVALS umgeaendert, um den unterschied zum
   ATTDEFS im entsprechenden dtd element zu zeigen.
   </DELTA>
  <DELTA><DATE>19971027</DATE>
   rewritten to distinguish the basic markup parsing mechanism
   (in 'markup-reader.lisp') from the specifics for xml (here) or for a dtd
   (in 'dtd-parser.lisp').
   <UL>
    <LI>initialize-instance (element)
     allows element-declarations by value in addition to by name
    <LI>read-typed-markup-element (symbol t)
     changed the strategy used to assert the correct dtd:
     try to find the element definition. if one is found, use its dtd.
     if none is found, assert a null dtd and the 'XML-USER' package.
    </UL>
   </DELTA>
  <DELTA><DATE>19971228</DATE>
   element.get corected for distinct attribute mode and value
   </DELTA>
  <DELTA><DATE>19971228</DATE>
   XML-NODE.ATTRIBUTES</DELTA>
  <DELTA><DATE>19971230</DATE>
   node-class invocation passes the element name, as it should in order
   to elect an element-specific class for the instance</DELTA>
  <DELTA><DATE>19980303</DATE>
   added an attribute (by default XML::TYPE) to specify the type of an element if
   it is other than the tag type <BR>
   adopted element binding macro from 'simple-xml.lisp'</DELTA>  
  <DELTA><DATE>19980311</DATE>
   support for html abbreviated elements; let forms</DELTA>
  <DELTA><DATE>19980318</DATE>
   element.package for printing</DELTA>
  <DELTA><DATE>19980408</DATE>
   element.get extended with an optional default arg</DELTA>
  <DELTA><DATE>19980601</DATE>
   make-element makes literal elements only: no element type specialization.
   otherwise it's not possible to stylize serializable instances</DELTA>
  <DELTA><DATE>19980612</DATE>
   when-element-content/content-list as shortcut
   </DELTA>
  <DELTA><DATE>19980621</DATE>
   <CODE>XML-LINK-ELEMENT</CODE> added to distinguish xml element nodes which can
   serve as the origin of a link.
   the binding content and attribute accessors (LET-*) all specialize access to
   read through a reference node if a link reference is present.
   the link nodes behave similar to named entity nodes, but instead of prepending
   what is expected to be string content to a stream, they cache what is expected
   to be dom content.
   <BR>
   backed out the change from 19980601:
   elements which can be styled must now be reified as classes.
   <A HREF='/doc/FUNCTION?NAME=DOM-ELEMENT-STYLE'>DOM-ELEMENT-STYLE</A> now
   specializes on the class rather using an EQL specializer on the tag identifier.
   </DELTA>
  <DELTA><DATE>19981218</DATE>
   added a *start-tag* instance for when *parse-suppress* is asserted.</DATE>
  <DELTA DATE='19981222'>fix for 'unbound' tag GI's.</DELTA>
  <DELTA DATE='19990420'>get-precedence-list</DELTA>
  <DELTA DATE='19990427'>fix for failing link to context;
   (setf xml-node.content) now an explicit function with contextlink and
   invalidation</DELTA>
  <DELTA DATE='19990502'>pprint-html w/ default namespace bound</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
 "

(in-package "XML-PARSER")

(defClass element-interface ()
  ()
  (:documentation
   "a marker class to specify conformance to the element interface."))

(defClass element-class (keyword-qualified-class)
  ())

(defClass element (xml-attribute-context
                   comment-target
                   element-interface
                   content-markup
                   document-markup)
  ((context
    :accessor element.context
    :initarg :context)
   (content
    :reader element.content
    :writer element.set-content
    :initarg :content)
   (name
    :type symbol
    :accessor element.name
    :documentation
    "the name is initialized through a symbol which is mapped to a
     <CODE>DTD-ELEMENT</CODE>.
     the declaration then prescribes the general structure for the node.")
   (attributes
    :accessor element.attributes
    :initarg :attributes
    :initform nil
    :type attribute-list
    :documentation
    "binds a list of attributes, each of which comprises an indicator
     and a value.")
   (valid?
    :reader element.get-valid?
    :writer element.set-valid?
    :type symbol))
  (:documentation
   "the class of XML data elements.
    it binds the containing node as <CODE>element.CONTEXT</CODE>,
    the document type declaration element as <CODE>element-DECLARATION</CODE>,
    the instance-specific attributes as <CODE>element.ATTRIBUTES</CODE>,
    and the contents as <CODE>element.CONTENT</CODE>.")
  (:metaclass element-class))

;;;
;;; as class class-initarg use the initarg for the element generic identifier

(setf (class.class-initarg 'element) :name)

(defMethod class-initarg-class
           ((class element-class) (initargs list)
            &aux initarg-class)
  (flet ((string-getf (list name &aux test diff)
           (loop (unless (setf test (pop list)) (return))
                 (setf test (string test))
                 (setf diff (- (length test) (length name)))
                 (when (and (>= diff 0)(string-equal test name :start1 diff))
                   (return (pop list)))
                 (pop list))))
    (destructuring-bind (&key attributes &allow-other-keys) initargs
      (cond ((and *element-class-attribute*
                  (setf initarg-class
                        (string-getf attributes *element-class-attribute*))
                  (setf initarg-class 
                        (find-class (intern-name initarg-class) nil))
                  (find class (get-precedence-list initarg-class)))
             initarg-class)
            (t
             (call-next-method))))))

(defClass end-tag (xml-named-node 
                   content-markup)
  ())

(defClass abbreviated-element (element) () (:metaclass abstract-class))

(defClass |html|::br (abbreviated-element) ())

(defClass |html|::hr (abbreviated-element) ())


(defMethod xml-node.reference
           ((node element))
  node)

(defClass xml-link-element (xml-link-origin
                            element)
  ((link-attribute
    :initarg :link-attribute
    :accessor element.link-attribute
    :type symbol
    :documentation
    "binds the name of the attribute which, if present, designates the node as
     a link."))
  (:documentation
   "a specialized class of element node which caches the attribute identifier
    which hold a link value. if the attribute is present, then the node itself
    holds neither attribute nor element content. all references are redirected
    to a node which is retrieved from the designated resource and cached as the
    node reference.")
  (:metaclass keyword-qualified-class))

(defMethod xml-node.reference
           ((node xml-link-element))
  (with-slots (system-id reference) node
    (or (when (and reference (xml-node.up-to-date? node))
          reference)
        (when system-id
          (setf reference (get-link-content system-id)))
        node)))

;(trace get-external-entity-parsed-content)
        

(defMethod element.content
           ((datum t))
  nil)

(defMethod initialize-instance :after
           ((self element) &key type)
  (declare (ignore type)) ;; added to permit attribute to determine class
  (dolist (a (element.attributes self))
    (link-context a self)))

(defMethod initialize-instance :after
           ((self xml-link-element) &key &aux link-value)
  (with-slots (system-id link-attribute) self
    (when (and link-attribute
               (not system-id)
               (setf link-value (xml-node.get self link-attribute)))
      (setf (xml-node.system-id self) (xml-node.string link-value)))))

(defMethod initialize-instance :after
           ((self element-interface) &key declaration content attributes)
  (declare (ignore declaration  content attributes)))


(defMethod element-declaration
           ((node element) (context t)
            &optional (error-p *undefined-element-condition*))
  (element-declaration (xml-node.name node) context error-p))

(defMethod element.package
           ((node element))
  (element.package (element.name node)))

(defMethod element.package
           ((node symbol))
  (symbol-package node))

(defMethod element.package
           ((node t))
  *user-package*)


(defMethod element-declaration.model
           ((node element) &aux (decl (element-declaration node t)))
  (when decl
    (element-declaration.model decl)))

(defMethod element.valid?
           ((element element) &optional (error t))
  (if (slot-boundp element 'valid?)
    (element.get-valid? element)
    (let ((succeed nil)
          (fail nil))
      (setf succeed #'(lambda (datum inner-fail)
                        (declare (ignore datum inner-fail))
                        (element.set-valid? t element)
                        (return-from element.valid? t))
            fail (typecase error
                   (null #'(lambda (datum inner-succeed model)
                             (declare (ignore model))
                             (element.set-valid? nil element)
                             (when *parse-verbose*
                               (warn "element invalid: ~s/~s." element datum))
                             (funcall inner-succeed datum fail)))
                   (function #'(lambda (datum inner-succeed model)
                                 (element.set-valid? nil element)
                                 (funcall error datum inner-succeed model)
                                 (return-from element.valid?
                                   (values nil datum model))))
                   (t #'(lambda (datum inner-succeed model)
                          (declare (ignore inner-succeed))
                          (element.set-valid? nil element)
                          (return-from element.valid?
                            (values nil datum model))))))
      (%element.valid? element succeed fail))))

(defMethod element-derive-valid?
           ((element element) succeed fail
            &aux (decl (element-declaration element t))
                 (predicate
                  (when decl (element-declaration.predicate decl))))
  (if predicate
    (progn (element.content element)
           (funcall predicate (element.content element) succeed fail))
    (progn (when *parse-verbose* (warn "no predicate for element: ~s." element))
           (funcall succeed element fail))))

(defMethod %element.valid?
           ((element element) (succeed t) (fail t))
  (if (slot-boundp element 'valid?)
    (if (element.get-valid? element)
      (funcall succeed element fail)
      (funcall fail element succeed (element-declaration.model element)))
    (element-derive-valid? element succeed fail)))

(defMethod (setf element.content)
           ((datum list) (element element))
  (element.set-content datum element) 
  (when datum (xml-node-link-content element))

  ;; the invalidation should, stricty speaking, propagate upwards, but ...
  (slot-makunbound element 'valid?))

(defMethod reset-element.valid?
           ((element element))
  (slot-makunbound element 'valid?)
  (map nil #'reset-element.valid? (element.content element)))

(defMethod reset-element.valid?
           ((element t))
  nil)

;;; pattern/datum interface
;;;

(defMethod xml-pattern.attributes ((node element))
  (element.attributes node))

(defMethod xml-pattern.relations
           ((node element))
  (element.content node))

(defMethod xml-pattern.type
           ((node element))
  ; nope (xml-node.name (element-model.content node))
  (or (xml-node.get node *xsl-type-attribute*)
      (element.name node)))

(defMethod xml-pattern.occurrence ((node element))
  (xml-node.get node 'xml::occurrence))


(defMethod xml-datum.attributes ((node element))
  (element.attributes node))

(defMethod xml-datum.relations
           ((node element))
  (element.content node))

(defMethod xml-datum.type
           ((node element))
  (or (xml-node.get node *xsl-type-attribute*)
      (element.name node)))




(defMethod initialize-instance
           ((self element) &rest initargs
            &key attributes declaration (name declaration))
  ;(print (list 'i-i :name name :declaration declaration))
  ;; bind the name only. the declaration is available indirectly through the
  ;; name. if a string is provided, intern it - heuristically if necessary
  ;; if a symbol is provided, that's it.
  (typecase name
    ((and symbol (not null)) t)
    (element-declaration (setf name (element-declaration.name name)))
    (t (xml-form-error 'xml-1.0::element
                       "illegal declaration specified for a ~s: ~s."
                       (type-of self) name)))
  ;; initialize the element with the attributes from the definition
  (apply #'call-next-method self
         :attributes nil
         :name name
         initargs)
  
  ;; fist assert the default attributes
  (let ((defaults
          (instantiate-attlist self (element-declaration.attdefs name))))
    (initialize-instance-attributes self (first defaults) (rest defaults)))
  
  ;; then augment it with the instantial attributes
  (initialize-instance-attributes self (first attributes) (rest attributes))
  
  )

(defMethod (setf element.context)
           ((node element) (datum t))
  datum)

(defMethod document.element
           ((element element) &aux context)
  (loop
    (setf context (element.context element))
    (typecase context
      ((or document null) (return element))
      (element (setf element context))
      (t (return nil)))))



(defMethod xml-node.string
           ((node element))
  (xml-node.string (element.content node)))

(defMethod read-element
           ((source t)
            &aux (*namespaces* *namespaces*))
  (read-markup-element source))

(defMethod element
           ((datum string))
  (read-element datum))

(defMethod element
           ((datum stream))
  (read-element datum))

(defMethod element
           ((node element))
  node)

;; note that these do not attempt to specialize for the element type:
;; they make literal elements

(defun make-element
       (decl &rest content &aux attributes)
  (when (consp decl)
    (setf attributes (rest decl)
          decl (first decl)))
  (make-instance 'element
    :name decl
    :content content
    :attributes attributes))

(defun make-element*
       (decl &rest content &aux attributes)
  (when (consp decl)
    (setf attributes (rest decl)
          decl (first decl)))
  (make-instance 'element
    :name decl
    :content (apply #'list* content)
    :attributes attributes))


;;;
;;; the specialization for otherwise indistinguished symbols constructs
;;; the tag for an element and recursively reads the stream until the
;;; matching close tag is discovered.
;;; the active dtd and package are adjusted to read relative to the respective
;;; element and element's dtd. first the declaration is located relative to
;;; the active dtd, then that declaration's own dtd is activated.

(defMethod normalize-type ((type symbol))
  (when (find-class type nil)
    type))

(defMethod normalize-type
           ((type string))
  (normalize-type (intern-name (string-upcase type))))

(defMethod normalize-type ((type null)) nil)

(defMethod reduce-production
           ((element element) &key)
  (setf *processed-node* element))

(defMethod reduce-production
           ((production (eql 'XML-1.0::element))
            &key
            ((xml-1.0::STag element))
            ((xml-1.0::content content))
            ((xml-1.0::ETag end-tag)))
  (unless (or (null end-tag)
              (eq (xml-node.name end-tag) (xml-node.name element)))
    (xml-form-error 'XML-1.0::element
                    "element tag mismatch: ~s, ~s." element end-tag))
  (unless *parse-suppress*
    (setf (element.content element) content))
  (reduce-production element))

(defMethod reduce-production
           ((production (eql 'XML-1.0::STag))
            &key
            ((XML-1.0::Name name))
            ((XML-1.0::attribute attributes))
            &aux element)
  (cond (*parse-suppress*
         (setf (xml-node.name *start-tag*) (intern-name name)
               (xml-node.content *start-tag*) attributes)
         *start-tag*)
        (t
         ;; nb the attribute namespaces are asserted as they are parsed
         ;; (cf. read-stag-attributes); it remains to figure out the
         ;; namespace for the GI itself as it must be interned prior to
         ;; instantiation.
         (with-namespace-frame
           ;; intern the name (if a string) possibly in the context of namespace
           ;; binding present among the attributes, which, in turn may affect
           ;; the default namespace
           (assert-gi-namespace name attributes)
           (when (stringp name) (setf name (intern-name name)))

           (setf element (make-instance *element-class*
                           :name name
                           :attributes attributes))
           (when *rebind-default-content-namespace*
             (assert-namespace-binding *default-namespace-prefix*
                                       (symbol-package (element.name element))))
           element))))

         

(defMethod reduce-production
           ((production (eql 'XML-1.0::ETag))
            &key
            ((XML-1.0::Name name))
            ((XML-1.0::attribute attributes)))
  (when (and attributes (not *permit-etag-attributes*))
    (xml-form-error production "tag attributes not permitted."))
  ;; reuse the same attribute
  (setf (xml-node.name *end-tag*) (intern-name name)
        (xml-node.content *end-tag*) attributes)
  *end-tag*)


(defMethod production-reader-macro
          ((production (eql 'xml-1.0::STag)) (stream t)
            &aux name attributes end-char)
  (handler-bind ;; just augment error message for context
    ((error #'(lambda (condition)
                (xml-warn production
                          "name: ~s, attributes: ~s."
                          name attributes)
                condition)))
    ;; bind the default namespace for the duration of reading the attributes ONLY.
    ;; note that this means that any attribute interning (eg. xptrs)
    ;; must happen either while the tag is read, or within a new namespace
    ;; binding which observes the tag package - the actual binding happens as
    ;; the attribute list is read
    (multiple-value-setq (name attributes end-char)
      (read-entity-attributes 'xml-1.0::Stag stream))
    (values (reduce-production 'XML-1.0::Stag
                               'XML-1.0::Name name
                               'XML-1.0::attribute attributes)
            (char= end-char #\/))))

(defMethod production-reader-macro
          ((production (eql 'xml-1.0::ETag)) (stream t)
            &aux name attributes)
  (handler-bind ;; just augment error message for context
    ((error #'(lambda (condition)
                (xml-warn production
                          "name: ~s, attributes: ~s."
                          name attributes)
                condition)))
    ; a close tag generates only the symbol which names the
    ; tag. the element reader uses the token as an
    ; indicator, that the respective element is complete.
    ; both forms #!<asdf>aa</asdf> #!<asdf>aa</> are
    ; supported unless conformance is strict.
     (read-char stream)
     (cond ((and *permit-abbreviated-close-tags*
                 (eql *tag-close-char* (peek-char t stream t t t)))
            (read-char stream)
            *close-tag-marker*)
           (t
            (multiple-value-setq (name attributes)
              (read-entity-attributes 'xml-1.0::Stag stream))
            (reduce-production 'XML-1.0::ETag
                               'XML-1.0::Name name
                               'XML-1.0::attributes attributes)))))

(defun abbreviated-element-p
       (node)
  (or (typep node 'abbreviated-element)
      (and (symbolp node)
           (find-class node nil)
           (subtypep node 'abbreviated-element))))

(defmethod production-reader-macro
           ((production (eql 'xml-1.0::element)) (stream t)
            &aux
            content end-tag empty?
            (*parent-node* *parent-node*)
            (*namespaces* *namespaces*)
            (*preserve-whitespace* *preserve-whitespace*)
            stag)
  ;; bind the namespaces in preparation for reading the attributes, in order
  ;; that the scope of any assertions include the content. 
  (handler-bind ;; just augment error message for context
    ((error #'(lambda (condition)
                (xml-warn production
                          "STag: ~s, content: ~s."
                          stag content)
                condition)))
    (multiple-value-setq (stag empty?)
      (production-reader-macro 'xml-1.0::Stag stream))
    (setf *parent-node* stag)
    (unless (or empty? (abbreviated-element-p stag))
      (multiple-value-setq (content end-tag)
        (read-production 'xml-1.0::content stream)))
    (reduce-production production
                       'xml-1.0::STag stag
                       'xml-1.0::content content
                       'xml-1.0::ETag end-tag)))

;;;
;;; XML-Printer
;;;


(defun print-readably
       (element &optional (stream *standard-output*)
        &aux
        (*print-readably* t))
  (print element stream))
  
(defun pprint
       (element &optional (stream *standard-output*)
        &aux
        (*print-pretty* t))
 (print-readably element stream))

(defun pprint-html
       (element &optional (stream *standard-output*)
        &aux
        (*print-pretty* t))
  (with-default-namespace *html-package*
    (print-readably element stream)))


(defmethod print-start-tag
           ((datum element) (stream t)
            &aux (name (element.name datum)) (package (symbol-package name)))
  (write-string *start-tag-open* stream)
  (write-identifier name stream)

  ;; check that the name's namespace is bound otherwise fource a binding and
  ;; add it as an additional attribute.
  (unless (strict-namespace.prefix package)
     (write-char #\space stream)
     (write-string (package-name package) stream)
     (write-char *attribute-equals-char* stream)
     (print-attribute-value (namespace.uri package) stream)) 
     
  ;; serialize the attributes, but suppres thos which are declared #FIXED
  ;; further distinctions are possible, but a direct declared/not decision
  ;; would require an additional slot.
  (dolist (attribute (element.attributes datum))
    (unless (eq (attribute.mode attribute) 'xml-1.0::\#fixed)
      (write-char #\space  stream)
      (print-object attribute stream)))

  (cond ((typep datum 'abbreviated-element)
         (write-string *tag-close* stream)
         nil)
        ((or *print-empty-close-tags* (element.content datum))
         (write-string *tag-close* stream)
         name)
        (t
         (write-string *empty-tag-close* stream)
         nil)))

(defmethod print-object
           ((*parent-node* element) stream
            &aux
            (content (element.content *parent-node*))
            (*form-indent* (+ *form-indent* 1)))
  (cond (*print-readably*
         (with-element-namespaces *parent-node*
           (when (print-start-tag *parent-node* stream)
             (when *print-pretty* (format stream "~&~v@t" *form-indent*))
             (flet ((write-element (e)
                      (typecase e
                        (string (write-character-data-value e stream))
                        (t (print-object e stream)))))
               (typecase content
                 (null nil)
                 (cons (if (rest content)
                         (dolist (x content)
                           (write-element x)
                           (when *print-pretty* 
                             (format stream "~&~v@t" *form-indent*)))
                         (write-element (first content))))
                 ;; the following two clauses just in case
                 (t (write-element content)))))
           ;; note that although namespace prefix must appear explicitly in the
           ;; end tag, the binding is that which the start tag may have put into
           ;; effect, thus the bindings carry over - even if not as defaults
           (when (or *print-empty-close-tags* content)
             (print-end-tag *parent-node* stream))))
        (t 
         (print-unreadable-object (*parent-node* stream :identity *parse-verbose*
                                                 :type t)
           (prin1 (element.name *parent-node*) stream)))))

;;;
;;; interface macros: generation

(defMacro dom
          (decl &rest content)
  (if (consp decl)
    `(make-element (list ',(first decl) ,@(rest decl)) ,@content)
    `(make-element ',decl ,@content)))

(defMacro dom!
          (decl &rest content)
  (if (consp decl)
    `(make-element (list ,(first decl) ,@(rest decl)) ,@content)
    `(make-element ,decl ,@content)))

(defMacro dom*
          (decl &rest content)
  (if (consp decl)
    `(make-element* (list ',(first decl) ,@(rest decl)) ,@content)
    `(make-element* ',decl ,@content)))

(defMacro dom!*
          (decl &rest content)
  (if (consp decl)
    `(make-element* (list ,(first decl) ,@(rest decl)) ,@content)
    `(make-element* ,decl ,@content)))

;;;
;;; interface macros: attribute access

(defMacro let-element-attributes
          (bindings node &rest body &aux (node-var (gensym)))
  (flet ((binding (binding)
           (let ((var nil)
                 (attribute nil)
                 (default nil))
             (if (consp binding)
               (if (consp (first binding))
                 (setf var (second (first binding))
                       attribute (first (first binding))
                       default (second binding))
                 (setf var (first binding)
                       attribute (first binding)
                       default (second binding)))
               (setf var binding
                     attribute binding
                     default nil))
             `(,var
               (xml-node.get ,node-var ',attribute ,default)))))
    `(let ((,node-var (xml-node.reference ,node)))
       (symbol-macrolet ,(mapcar #'binding bindings) ,@body))))


;;;
;;; interface macros: content access

(defMethod element.content-element
           ((element element) (type symbol))
  (find type (element.content element) :key #'xml-datum.type))

(defMethod element.content-element-content
           ((element element) (type symbol))
  (setf element (find type (element.content element) :key #'xml-datum.type))
  ;(print element)
  (typecase element
    (element (first (element.content element)))
    (t nil)))

(defMethod element.content-element-content-list
           ((element element) (type symbol))
  (typecase (setf element (find type (element.content element)
                                :key #'xml-datum.type))
    (element (element.content element))
    (t nil)))

(defMethod element.content-elements
           ((element element) (type symbol))
  (remove type (element.content element)
          :key #'(lambda (e)
                   (typecase e
                     (element (xml-datum.type e))
                     (t nil)))
          :test-not #'eq))

(defMethod element.content-elements-content
           ((element element) (type symbol))
  (mapcar #'element.content
          (remove type (element.content element)
                  :key #'(lambda (e)
                           (typecase e
                             (element (xml-datum.type e))
                             (t nil)))
                  :test-not #'eq)))

(defun %parse-let-element
       (element types accessor &aux symbols bindings)
  (dolist (type types)
    (let ((symbol (if (consp type)
                    (if (consp (first type))
                      (first (first type)) (first type))
                    type))
          (source (if (consp type)
                    (if (consp (first type))
                      (second (first type)) (first type))
                    type))
          (default (if (consp type) (third type) nil)))
      (push symbol symbols)
      (push (if default
              `(,symbol (or (,accessor ,element ',source)
                            ,default))
              `(,symbol (,accessor ,element ',source)))
            bindings)))
  (values symbols (nreverse bindings)))
;(%parse-let-element 'element '(asdf wert (default "")) 'get)

(defMacro let-elements
          ((&rest types) element &rest body
           &aux (binding (make-symbol "ELEMENT")))
  (multiple-value-bind (symbols bindings)
                       (%parse-let-element binding types
                                           'element.content-element)
    `(let ((,binding (xml-node.reference ,element)))
       (let ,bindings
         (declare (ignorable ,@symbols))
         ,@body))))

(defMacro let-elements-content
          ((&rest types) element &rest body
           &aux (binding (make-symbol "ELEMENT")))
  (multiple-value-bind (symbols bindings)
                       (%parse-let-element binding types
                                           'element.content-element-content)
    `(let ((,binding (xml-node.reference ,element)))
       (let ,bindings
         (declare (ignorable ,@symbols))
         ,@body))))

(defMacro let-elements-content-list
          ((&rest types) element &rest body
           &aux (binding (make-symbol "ELEMENT")))
  (multiple-value-bind (symbols bindings)
                       (%parse-let-element binding types
                                           'element.content-element-content-list)
    `(let ((,binding (xml-node.reference ,element)))
       (let ,bindings
         (declare (ignorable ,@symbols))
         ,@body))))

(defMacro when-elements
          ((&rest types) element &rest body
           &aux (binding (make-symbol "ELEMENT")))
  (multiple-value-bind (symbols bindings)
                       (%parse-let-element binding types
                                           'element.content-element)
    `(let ((,binding (xml-node.reference ,element)))
       (let ,bindings
         (when (and ,@symbols)
           ,@body)))))

(defMacro when-elements-content
          ((&rest types) element &rest body
           &aux (binding (make-symbol "ELEMENT")))
  (multiple-value-bind (symbols bindings)
                       (%parse-let-element binding types
                                           'element.content-element-content)
    `(let ((,binding (xml-node.reference ,element)))
       (let ,bindings
         (when (and ,@symbols)
           ,@body)))))

(defMacro let-duplicate-elements
          ((&rest types) element &rest body
           &aux (binding (make-symbol "ELEMENT")))
  (multiple-value-bind (symbols bindings)
                       (%parse-let-element binding types
                                           'element.content-elements)
    `(let ((,binding (xml-node.reference ,element)))
       (let ,bindings
         (declare (ignorable ,@symbols))
         ,@body))))

(defMacro let-duplicate-elements-content
          ((&rest types) element &rest body
           &aux (binding (make-symbol "ELEMENT")))
  (multiple-value-bind (symbols bindings)
                       (%parse-let-element binding types
                                           'element.content-elements-content)
    `(let ((,binding (xml-node.reference ,element)))
       (let ,bindings
         (declare (ignorable ,@symbols))
         ,@body))))

(defMacro when-duplicate-elements
          ((&rest types) element &rest body
           &aux (binding (make-symbol "ELEMENT")))
  (multiple-value-bind (symbols bindings)
                       (%parse-let-element binding types
                                           'element.content-elements)
    `(let ((,binding (xml-node.reference ,element)))
       (let ,bindings
         (when (and ,@symbols)
           ,@body)))))

(defMacro when-element-content
          (element &aux (binding (make-symbol "ELEMENT")))
  `(let ((,binding (xml-node.reference ,element)))
     (when ,binding (first (element.content ,binding)))))

(defMacro when-element-content-list
          (element &aux (binding (make-symbol "ELEMENT")))
  `(let ((,binding (xml-node.reference ,element)))
     (when ,binding (element.content ,binding))))


(setf *end-tag* (make-instance 'end-tag))
(setf *start-tag* (make-instance 'element :name 'xmlns.1::suppressed))


#+:CCL
(progn
 (pushnew '(let-element-attributes . 2) *fred-special-indent-alist* :key #'first)
 (pushnew '(let-elements . 2) *fred-special-indent-alist* :key #'first)
 (pushnew '(let-elements-content . 2) *fred-special-indent-alist* :key #'first)
 (pushnew '(let-elements-content-list . 2) *fred-special-indent-alist* :key #'first)
 (pushnew '(let-duplicate-elements . 2) *fred-special-indent-alist* :key #'first)
 (pushnew '(let-duplicate-elements-content . 2) *fred-special-indent-alist* :key #'first)
 (pushnew '(when-elements . 2) *fred-special-indent-alist* :key #'first)
 (pushnew '(when-duplicate-elements . 2) *fred-special-indent-alist* :key #'first))

;(inspect (read-from-string "#!<a><s/></a>"))
"XMLP"
