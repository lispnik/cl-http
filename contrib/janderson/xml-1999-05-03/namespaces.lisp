;;; -*- package: "XML-PARSER"; -*-
;;;

"
<DOCUMENTATION>
 <DESCRIPTION>
  support for xml namespaces based on packages
  </DESCRIPTION>
 <COPYRIGHT HREF='defsystem.lisp|root().descendant(1,COPYRIGHT)'/>
 <CHRONOLOGY>
  <DELTA><DATE>19981218</DATE>
   fixed namespace.prefix. the nickname is no longer interesting. only the
   namespace binding and the base name are useful. the URI appears as the
   first nickname.
   added namespace.uri.</DELTA>
  <DELTA DATE='19990423'>additional intern-name methods for SYMBOL and NULL
   </DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
 "


"
<H3>package bindings</H3>
 three different package retrieval functions are necessary
<UL>
 <LI>MARKUP-NAMESPACE works with namespace names (that is the URI's) and
  recognizes the special case of the empty URI, which wd-19980916 claims
  should be no namespace which would entail uninterned names. for now, a
  default base is used instead, in order to ensure that the symbols in the
  'xml' package can always be found.</LI>
 <LI>FIND-NAMESPACE works with the dynamically managed prefix bindings
  to locate the respective namespace. it permits implicit namespace creation
  and binding so long as a validity error is not handled.</LI>
 <LI>MARKUP-IDENTIFIER-PACKAGE maps some datum (eg. an identifier) to a
  legitimate namespace package</LI>
 </UL>
"

(in-package "XML-PARSER")

(defGeneric make-namespace
  (name)
  (:method ((name string))
           (or (find-package name)
               (make-package
                (gentemp "XMLNS." *namespace-name-package*)
                :use (list *user-package* *xml-package*)
                :nicknames (list name))))
  (:method ((name null)) *user-package*)
  (:method ((name symbol)) (make-namespace (string name)))
  (:method ((name list)) (make-namespace (xml-node.string name))))

(defun markup-namespace
       (name)
  (or (find-package name)
      (when (zerop (length name)) *default-package*)))

(defMethod markup-identifier-package
           ((context t))
  ;; intended primarily for streams, where the intent is to leave the package
  ;; where it is for a new (possibly nested) stream so long as
  ;; it suffices to find the base user package.
  (markup-identifier-package (default-namespace)))

(defMethod markup-identifier-package
           ((name symbol))
  (markup-identifier-package (symbol-package name)))

(defMethod markup-identifier-package
           ((package package))
  (if (find *user-package* (package-use-list package))
    package
    *user-package*))

(defMethod markup-identifier-package
           ((package null))
  *user-package*)

"<H3>namespace operations</H3>
 <P>
 the mechanism which implements the correspondence between prefixed- and
 universal names uses the prefixes as keys into a collection of packages to
 map the local-parts of the names to symbols in the respective packages.
 the prefix binding is afforded an extent which corresponds to the time when the
 process is parsing the respective entity. both elements and physical entities
 are permitted to bind prefixes.
 the binding collection is implemented as a dynamic stack of binding frames.
 "

; these two are not used at the moment
(defMethod add-package-nickname
           ((p package) (name t))
  (rename-package p (package-name p)
                  (cons name (package-nicknames p))))

(defMethod remove-package-nickname
           ((p package) (name t))
  (rename-package p (package-name p)
                  (remove name (package-nicknames p) :test #'string-equal)))

(defun reset-namespace-bindings
       ()
  (setf *namespaces* *default-namespaces*))

(defun assert-namespace-binding
       (name package)
  ;; coerce the namespace name to a package;
  ;; if the package does not yet exist then create it with the appropriate defaults;
  ;; the uri appears as a nickname in order that the package can be found again;
  ;; the prefix is not incorporated into the package, but rather used as the key in the parser's namespace stack only;
  ;; see wd-xml-names-19980916 (or successors) section 2 for the rules governing null prefix and uri values
  (flet ((binding-error ()
           (xml-form-error 'xml-1.0::NSDecl
                           "erroneous binding: ~s ~s"
                           name package)))
    (typecase name
      (string (setf name (intern name *namespace-name-package*)))
      (null (setf name *default-namespace-prefix*))
      (symbol t)
      (t (binding-error)))
    (typecase package
      (package t)
      ((or string symbol)
       (setf package (if package (string package) ""))
       (cond ((zerop (length package))
              (unless (zerop (length name)) (binding-error))
              (setf package *user-package*))
             (t
              (setf package (make-namespace package)))))
      (t (binding-error)))
    (setf *namespaces* (acons name package *namespaces*))))

(defMethod assert-namespace-bindings
           ((bindings list) &aux binding value)
  ;; iterate over the bindings and assert them in the current namespace frame
  ;; if the binding is expressed as a list, then use the first as the prefix and
  ;; the second as the package / package name
  ;; if the binding is a namespace attribute node, then assert it. in that event the node content will have
  ;; been coerced to a package upon instantiation.
  (loop (unless (setf binding (pop bindings)) (return))
    (typecase binding
      (cons
       (apply #'assert-namespace-binding binding))
      (xml-1.0::@NAMESPACE  ;; which is the case when serializing
       (with-slots (name content) binding
         (assert-namespace-binding name content)))
      (string ; which is the case when parsing
       (unless (setf value (pop bindings)) (return))
       (when (namespace-attribute-name? binding)
         (assert-namespace-binding (namespace-attribute-name.prefix binding)
                                   value))))))

(defMethod assert-namespace-bindings
           ((element xml-attribute-context))
  (assert-namespace-bindings (xml-node.attributes element)))



(defun find-namespace
       (name)
  ;; namespace bindings take precedence over, but do not preclude, package names
  (or (rest (assoc name *namespaces* :test #'string-equal))
      (find-package name)))

(defun default-namespace
       ()
  (or (find-namespace *default-namespace-prefix*) *default-package*))


(defMacro with-namespace-frame
          (&rest body)
  `(let ((*namespaces* *namespaces*))
     ,@body))

(defMacro with-namespace-bindings
          (bindings &rest body)
  `(with-namespace-frame
     (assert-namespace-bindings ,bindings)
     ,@body))

(defMacro with-namespace-bound
  ((name namespace) &rest body)
  `(with-namespace-frame
     (assert-namespace-binding ,name ,namespace)
     ,@body))

(defMacro with-default-namespace
       (namespace &rest body)
  `(with-namespace-bound (*default-namespace-prefix* ,namespace) ,@body))

(defMacro with-element-namespaces
          (element &rest body)
  `(with-namespace-frame
     (assert-namespace-bindings ,element)
     ,@body))

;;;
;;; reader support for prefixed names

(defMethod intern-name
           ((name string) &optional (error-p t)
            &aux (colon (position #\: name)) package)
  "intern token name. by default in the package currently bound to the "" prefix.
   always copy the name to account for input buffers;
   use single colon as the package marker."
  (cond ((null colon)
         (setf name (intern (subseq name 0) (default-namespace)))
         ;(export name package)
         name)
        ((zerop colon)                  ; not in the spec, but ...
         (intern (subseq name (1+ colon)) :keyword))
        ((plusp colon)
         (setf package (find-namespace (subseq name 0 colon)))
         (cond (package
                (intern (subseq name (1+ colon)) package))
               ((functionp error-p)
                (funcall error-p
                         (subseq name 0 colon) (subseq name (1+ colon))))
               (error-p
                ;; if no namespace is bound to this prefix 
                ;; wd-19980916 requires a form error. we leave the option
                ;; to define it on the fly or take some other action
                (xml-form-error 'XML-1.0::QName
                                "unbound namespace prefix in context: ~s: ~s."
                                (subseq name 0 colon) *namespaces*))
               (t nil)))))

(defMethod intern-name
           ((name symbol) &optional error-p)
  (declare (ignore error-p))
  name)

(defMethod intern-name
           ((name null) &optional error-p)
  (declare (ignore error-p))
  nil)




(defMethod intern-gi ((name symbol)) name)
(defmethod intern-gi
           ((name string))
  ;; similar to interning a name, but in the event that the prefix is not
  ;; (yet) bound to a namespace, examine all known namespace for the
  ;; symbol AND, if a declaaration is found, AND it includes a default
  ;; attribute with the respective prefix, then (whoha!) that's the namespace
  ;; to use.
  (intern-name name #'(lambda (prefix local-part &aux package)
                        ;; when the package portion was not found, then
                        ;; search for the namespace
                        (if (setf package
                                  (find-attlist-namespace local-part prefix))
                          (intern local-part package)
                          (xml-form-error 'XML-1.0::QName
                                "unbound namespace prefix in context: ~s: ~s."
                                local-part *namespaces*)))))

(defun find-attlist-namespace
       (gi attribute-name &aux element-decl attribute-decl)
  (dolist (package (list-all-packages))
    (when (string-equal "XMLNS." (package-name package) :end2 6)
      (when
        (and (setf element-decl (find-symbol gi package))
             (setf attribute-decl
                   (find-if #'(lambda (attr)
                                (with-slots (name value) attr
                                  (and (typep attr '@NAMESPACE)
                                       (string= attribute-name name))))
                            (element-declaration.attdefs element-decl))))
        (attribute.value attribute-decl)))))

(defMethod qualified-name?
           ((name symbol))
  (not (default-namespace? (symbol-package name))))
(defMethod qualified-name?
           ((name string))
  (find *namespace-punctuation-char* name))

(defMethod qualified-name.prefix
           ((name string) &aux (pos (position *namespace-punctuation-char* name
                                              :test #'char=)))
  (intern (subseq name 0 (or pos 0)) *namespace-name-package*))

(defMethod qualified-name.prefix
           ((name symbol))
  (namespace.prefix (symbol-package name)))

(defun qualified-name.namespace
       (name)
  (find-namespace (qualified-name.prefix name)))

(defun namespace-attribute-name.prefix
       (name
        &aux
        (pos (position *namespace-punctuation-char* name :test #'char=)))
  (if pos
    (intern (subseq name (1+ pos)) *namespace-name-package*)
    (when (string= name *ns-prefix*) *default-namespace-prefix*)))

(defun find-gi-namespace
       (prefix attributes &aux first)
  ;; locate a namespace binding which matches the prefix
  (when attributes
    (setf first (pop attributes))
    (etypecase first
      (string
       (if (and (namespace-attribute-name? first)
                (string-equal (qualified-name.prefix first) prefix))
         (first attributes)
         (find-gi-namespace prefix (cdr attributes))))
      (xml-1.0::@NAMESPACE  ;; an attribute which binds a namespace name
       (if (string-equal (attribute.name first) prefix)
         (xml-node.string first)
         (find-gi-namespace prefix (cdr attributes) ))))))


(defun assert-gi-namespace
       (name attributes &aux prefix namespace)
  ;; in order to conform to A.3 of WD-xml-names-19980916, the default namespace
  ;; is bound to that of the gi while the attributes are interned. this to
  ;; force any unqualified attributes into that partition.
  ;; note that the gi's namespace's binding may appear among the attributes.
  (setf prefix (qualified-name.prefix name))
  (when (setf namespace (find-gi-namespace prefix attributes))
    (assert-namespace-binding prefix namespace))
  (unless (string-equal prefix *default-namespace-prefix*)
    (or namespace
        (setf namespace (qualified-name.namespace name))
        (xml-form-error 'xml-1.0::name
                             "no namespace: ~s."
                             name))
    (assert-namespace-binding prefix namespace)))
                        
;;; printing support for namespaces:
;;;  use the local document binding for the package if one is present;
;;;  otherwise use the first nickname attached to the package; otherwise use
;;;  the package name
;;; the predicate which tests against the default package prefix is used to
;;; decide whether the prefix needs to be printed.

(defun namespace.prefix
       (package)
  ;; 19981218: only the namespace binding  prefix and the package base name
  ;; are useful
  (or (first (rassoc package *namespaces*))
      ; (first (package-nicknames package))
      (package-name package)))

(defun strict-namespace.prefix
       (package)
  (first (rassoc package *namespaces*)))

(defun namespace.uri
       (package)
  (or (first (package-nicknames package))
      (package-name package)))

(defun namespace-attribute-name?
       (attribute-name)
  (and (string-equal *ns-prefix* attribute-name
                     :end2 (min (length attribute-name) *ns-prefix-length*))
       (or (= (length attribute-name) *ns-prefix-length*)
           (char= *namespace-punctuation-char*
                  (char (string attribute-name) *ns-prefix-length*)))))


(defun default-namespace?
       (package)
  (eq package (rest (assoc *default-namespace-prefix* *namespaces*))))
          


#|
;; package resolution no longer uses the nicknames

(defMethod add-package-nickname
           ((p package) (name t))
  (rename-package p (package-name p)
                  (cons name (package-nicknames p))))

(defMethod remove-package-nickname
           ((p package) (name t))
  (rename-package p (package-name p)
                  (remove name (package-nicknames p) :test #'string-equal)))
|#

"XMLP"
