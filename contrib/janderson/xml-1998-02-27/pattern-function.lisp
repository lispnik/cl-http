;;; -*- package: ("XML-PARSER") -*-

(in-package :XML-PARSER)

#|
 a <CODE>PATTERN-FUNCTION</CODE> provides a pattern-directed control structure
 for operating on xml elements. it combines the intensional method inheritance
 mechanism of standard CLOS generic functions with a more extensional approach
 of the sort suggested by the 'demeter' approach.
 <P>
 the goal is the following:
 xml includes neither a mechanism to declare structural similarity among members
 of different names, nor a distinction between the structure implied by the
 inclusion of an element in a context element's model and the role played
 by the member element in the context.
 <BR>
 the consequence of these limitations is that
 <LI>structural identity is obscured in an effort to achieve semantic clarity,
 <LI>structural identity is achieved at the cost of syntactic redundance, or
 <LI>semantic information is introduced in the form of default attributes
 <BR>
 in any event, a processing method must include mechamisms to analyse the
 necessary secondary information (everything in addition to minimal tags
 and content).
 <P>
 pattern functions provide succinct constructs to specify the necessary
 analysis in declarative form. the declarations are compiled to equivalent
 procedural mechanisms which then act as filters for the ultimate processing
 methods.
 <P>
 <H3>pattern functions</H3>
 the programming interface is provided by <dfn>pattern functions</DFN>, which
 play a role analogous to generic functions. generic functions
 identify a set of arguments by intensionally specified classes and construct
 effective methods by topologically selecting and sorting similarly declared
 individual methods.
 pattern functions operate on a single structured argument and separate two
 kinds of methods: <DFN>selection filters</DFN> and <DFN>actions</DFN>.
 <LI>selection filters examine an argument element to derive an effective class
  based on the element content (nb. <EM>not</EM> the element dtd). if the
  identification succeeds, the discovered effective class and the determinant
  bindings are returned.
 <LI>actions are methods restricted to specific content. since the content
  is the determinant of extensional class mmbership, the methods are, in fact,
  specialized according to effective classes.
 <PRE>
 a pattern function works as follows:
 <OL>
 <LI>it presumes a context in which a class lattice specifies the precedence
  relations among extensionally defined classes. the subtype relation is
 determined by the presence of member elements.
 <LI>for a given argument, the topologically distinct filters are
  applied in order most to least specific. for each filter set the selection
  process terminates successfully when a filter succeeds at binding its
  element variables. if it succeeds, the returned effective class establishes
  the specialization class for action methods.
  as many efffective classes are retained as succeed.
  if the selection process fails, then no action is performed.
 <LI>for each selected effective class, an effective action method is
  constructed from all applicable action methods, sorted and combined
  according to the standard clos-mop protocol.
 <LI>all such effective actions are applied to the input element.
  the result of the 'last' application is the function result.
 </OL>
 to
 invoking selected members from among a collection of individual
 method functions. in that sense it is similar to a generic function.
 where, however, the generic function orders the methods according to a
 subsumption relation derived from type precedence in a specialized signature,
 the subsumption relation among pattern methods is derived from match
 specificity.
 <BR>
 the principle of ordering by subsumption remains.
 the difference is that, where the generic function uses
  (TYPE ARG_i_METHOD_j) < (TYPE ARG_i_METHOD_k) =>
    (TYPE METHOD_J) > (TYPE METHOD_k)
 and orders methods for application from super to sub type,
 the pattern function analyses methods according to
  (SIGNATURE METHOD_j) IS_SUPERSET (SIGNATURE METHOD_k) =>
    (TYPE METHOD_j) > (TYPE METHOD_k)
 as the basis for an analogous ordering.
 <P>
 this ordering is accomplished by augmenting the definition of the pattern
 method with a signature generated from the elements matched and maintaining
 a class hierarchy among the generated classes which reflects the subset
 relations implicit in the elements.


 generic functions don't have a sufficiently flexible interface to control the
 argument values.
 therefore do it through delegation:
 1. prepare the value by matching.
 if it succeeds, then generate the type token and delegate the action to the
 action generic function.
 |#


(defMethod pattern-greaterp ((s1 string) (s2 string))
  (if (= (length s1) (length s2))
    (string-greaterp s1 s2)
    (> (length s1) (length s2))))

(defmethod pattern-greaterp ((s1 symbol) (s2 symbol))
  (pattern-greaterp (string s1) (string s2)))


(defun make-pattern-type (types &aux type class parent-types)
  (unless types (return-from make-pattern-type t))
  (setf types (sort (mapcar #'(lambda (type)
                                (setf type (string type))
                                (when (find #\+ type)
                                  (error "illegal type specified: ~a." type))
                                (setf type (intern type  *pattern-package*))
                                (unless (type-specifier-p type)
                                  (mop:ensure-class type))
                                type)
                            types)
                    #'pattern-greaterp
                    :key #'string))
  (setf type (intern (format nil "~{~a~^+~}" types) *pattern-package*))
  ;(when (type-specifier-p type) (return-from make-pattern-type type))

  (setf (get type :types) types)
  (do-symbols (known-type *pattern-package*)
    ;; collect possible parents
    (when (and (not (eq known-type type))
               (find-if #'(lambda (type) (subtypep known-type type)) types))
      (push known-type parent-types)))
  ;; exclude parents which incorporate too much
  (setf parent-types
        (remove-if #'(lambda (type)
                       (set-difference (get type :types) types))
                   parent-types))
  ;; exclude parents which are not minimal
  (setf parent-types
        (remove-if #'(lambda (ptype)
                       (find-if #'(lambda (sptype)
                                    (and (not (eq sptype ptype))
                                         (subtypep sptype ptype)))
                                parent-types))
                   parent-types))
  (setf class
        (mop:ensure-class type
                          :direct-superclasses
                          (sort parent-types #'pattern-greaterp :key #'string)))
  
  ;; redefine subclasses
  (dolist (parent parent-types)
    (setf parent (find-class parent))
    (dolist (child (class-direct-subclasses parent))
      (when (and (not (eq child class))
                 (null (set-difference (class-direct-superclasses class)
                                       (class-direct-superclasses child))))
        (mop:ensure-class (class-name child)
                          :direct-superclasses
                          (sort (cons class
                                      (set-difference (class-direct-superclasses child)
                                                      (class-direct-superclasses class)))
                                #'pattern-greaterp
                                :key #'(lambda (c)
                                         (string (class-name c))))))))

  type)

(defun pattern-precedence-lists (&aux types)
  (do-symbols (type *pattern-package*)
    (push type types))
  (setf types (sort types #'pattern-greaterp :key #'string))
  (mapcar #'(lambda (type)
              (list type
                    (remove *pattern-package*
                            (mapcar #'class-name
                                    (class-direct-superclasses
                                     (find-class type)))
                            :key #'symbol-package
                            :test-not #'eq)))
          types))

;(inspect (pattern-names))
;(make-pattern-type '(aaaa tttt bbbb))
;(make-pattern-type '(aaaa tttt bbbb xxxx))
;(make-pattern-type '(tttt bbbb))
  



(defclass pattern-function (#+:stubs clos:stub-function #-:stubs generic-function)
  ((selectors :initarg :selectors :accessor pattern-function.selectors
              :initform nil
              :type list)
   (state :initform nil :accessor pattern-function.state))
  (:documentation
   "a pattern function is a generic functions which binds, additional to its
    methods, a set of filter functions which map arguments to extensional."))

(defMethod pattern-function.property
           ((function pattern-function) prop)
  (getf (pattern-function.state function) prop))

(defMethod (setf pattern-function.property)
           (datum (function pattern-function) prop)
  (setf (getf (pattern-function.state function) prop) datum))

(defClass pattern-selector ()
  ((pattern :initarg :pattern
            :accessor pattern-selector.pattern
            :type xml-node)
   (type :initarg :type
         :accessor pattern-selector.type
         :type symbol)
   (name :initarg :name :initform nil
         :accessor pattern-selector.name)))

(defMethod print-object
           ((datum pattern-selector) stream)
  (print-unreadable-object (datum stream :type t)
    (format stream "~s (~a)" (pattern-selector.name datum) (pattern-selector.type datum))))

(defmethod pattern-greaterp ((s1 pattern-selector) (s2 pattern-selector))
  (pattern-greaterp (pattern-selector.type s1) (pattern-selector.type s2)))

(defMethod remove-selector
           ((function pattern-function) (selector pattern-selector))
  (when (setf selector 
              (or (find selector (pattern-function.selectors function))
                  (find (pattern-selector.name selector)
                        (pattern-function.selectors function)
                        :key #'pattern-selector.name)))
    (setf (pattern-function.selectors function)
        (remove selector (pattern-function.selectors function))))
  selector)

(defMethod remove-selector
           ((function pattern-function) (selector symbol))
  (when (setf selector 
              (find selector
                    (pattern-function.selectors function)
                    :key #'pattern-selector.name))
    (setf (pattern-function.selectors function)
          (remove selector (pattern-function.selectors function))))
  selector)


(defMethod add-selector
           ((function pattern-function) (selector pattern-selector))
  (remove-selector function selector)
  (setf (pattern-function.selectors function)
        (sort (cons selector (pattern-function.selectors function))
              #'pattern-greaterp)))



(defMethod initialize-instance
           ((self pattern-selector) &rest initargs &key type)
  (etypecase type
    (null (error "a type must be specified for a ~s." (type-of self)))
    (symbol (unless (type-specifier-p type)
              (error "a type must be specified for a ~s." (type-of self))))
    (cons (setf type (make-pattern-type type))))
  (apply #'call-next-method self
         :type type
         initargs))

#|
 the general method of pattern application is to:
 <OL>
 <LI>apply all selection filters in sequence to the input element(s);
    application order is determined by type subsumption among the result
    type selectors.
 <LI>collect the bound element variables; when a variable is already bound
    by the application of a more specific selection filter, the new binding is
    ignored.
 <LI>collect the resulting type selectors; eliminate all redundant types;
    generate a single subtype from the remaining types if necessary.
 <LI>use the resulting type descriminator to compute and sort the applicable
    methods as for a normal generic function.
 <LI>apply the resulting effective method to the input element...
 </OL>
 <P>
 in order for this to be useful, the members must be matched not necessarily
 based on their element name, but optionally on a supplementary role.
 to take the example of the "NR" field from an
 IMAGE database, the field appears in any number of datasets. each time it
 appears it plays the same structural role - as a key field. the semantic role
 is, hoever, each time another.
 in order to capture this information, the respective dtd is augmented with
 attribute declarations which ascribe roles to the elements as default
 attributes. since this association must be per record, the element must be
 fully qualified (record . field) before the declaration can be applied.

 the ambiguity in xml between role and structure doesn't help here...
|#

(defun alist-to-plist
       (alist &optional plist)
  (if alist
    (alist-to-plist (rest alist) (cons (or (get (caar alist) :keyword)
                                            (setf (get (caar alist) :keyword)
                                                  (intern (string (caar alist))
                                                          :keyword)))
                                        (cons (cdr (first alist))
                                              plist)))
    plist))

(defMethod match-pattern-to-content
           ((selector pattern-selector) (node t) cont env)
  (match-pattern-to-content (pattern-selector.pattern selector) node cont env))
  
(defmethod compute-pattern-arguments
           ((function pattern-function) &rest arguments &aux partials)
  (flet ((success (bindings &aux type)
           (setf type (make-pattern-type (remove-duplicates (mapcar #'first bindings))))
           (return-from compute-pattern-arguments
             (list* (class-prototype (find-class type))
                                           (alist-to-plist bindings)))))
    (dolist (selector (pattern-function.selectors function))
      (setf partials
            (append partials
                    (match-pattern-to-content selector
                                              arguments
                                              #'success
                                              nil)))))
  (list* nil (alist-to-plist partials)))

(defMacro ensure-pattern-function
          (name &key (method-class 'standard-method)
                (lambda-list '(input &key state))
                (generic-function-class 'pattern-function)
           &aux (action-function-name nil))
  (if (consp name) (setf name (second name)))
  (setf action-function-name (intern (string name) *pattern-package*))
  (assert (subtypep generic-function-class 'pattern-function))
  `(prog1
     (ensure-generic-function ',name
                              :lambda-list ',lambda-list
                              :generic-function-class ',generic-function-class
                              :method-class ',method-class)
     (ensure-generic-function ',action-function-name
                              :lambda-list '(type &key &allow-other-keys))
     (defMethod ,name
                ,lambda-list
       (let ((bindings
              (compute-pattern-arguments #',name
                                         ,@(subseq lambda-list 0
                                                   (or (position #\& lambda-list
                                                                 :test #'char-equal
                                                                 :key
                                                                 #'(lambda (s)
                                                                     (elt (string s)
                                                                          0)))
                                                       (error
                                                        "bad lambda list: ~s."
                                                        lambda-list))))))
         (setf (getf state :bindings) bindings)
         (setf (pattern-function.state #',name) state)
         (apply #',action-function-name bindings)
         (pattern-function.state #',name)))
     (defMethod ,action-function-name
                ((type t) &key &allow-other-keys)
       nil)))
       
                                 

(defMacro defpattern-selector
          (name pattern
           &aux (f-sym (gensym)) (p-sym (gensym)) (s-sym (gensym))
                function-name selector-name)
  (etypecase name
    (cons (setf function-name (first name)
                selector-name (second name)))
    ((and symbol (not null)) (setf function-name name
                                   selector-name name)))

  `(let* ((,f-sym (ensure-pattern-function ',function-name))
          (,p-sym (xml-node ,pattern))
          (,s-sym (make-instance 'pattern-selector
                    :name ',selector-name
                    :pattern ,p-sym
                    :type (make-pattern-type (pattern-names ,p-sym)))))
     (add-selector ,f-sym ,s-sym)
     ,s-sym))


(defMacro defpattern-method
          (function-name &rest qualifiers-arglist-body
           &aux qualifiers qualifier bindings body type
                (action-function-name nil)
                rest-pos aux-pos key-pos rest aux declarations)
  (setf action-function-name (intern (string function-name) *pattern-package*))
  (loop (setf qualifier (first qualifiers-arglist-body))
        (unless (and qualifier (symbolp qualifier)) (return))
        (push qualifier qualifiers)
        (pop qualifiers-arglist-body))
  (setf qualifiers (nreverse qualifiers))
  (setf bindings (pop qualifiers-arglist-body)
        body qualifiers-arglist-body)
  (loop (unless (or (stringp (first body))
                    (and (consp (first body)) (eq (caar body) 'declare)))
          (return))
        (push (pop body) declarations))
  (setf declarations (nreverse declarations))
  (setf rest-pos (position '&rest bindings)
        key-pos (position '&key bindings)
        aux-pos (position '&aux bindings))
  (when aux-pos
    (setf aux (nthcdr aux-pos bindings)
          bindings (subseq bindings 0 aux-pos)))
  (when rest-pos
    (setf rest (nthcdr rest-pos bindings)
          bindings (subseq bindings 0 rest-pos)))
  (setf type (make-pattern-type bindings))

  `(progn
     (ensure-pattern-function ',function-name)
     (flet (((setf pattern.property) (value name)
              (setf (pattern-function.property #',function-name name) value))
            (pattern.property (name)
              (pattern-function.property #',function-name name)))
       (defMethod ,action-function-name ,@qualifiers
                  ((,(gensym) ,type) ,@rest ,@(unless key-pos '(&key)) ,@bindings
                   &allow-other-keys ,@aux)
         ,@declarations
         (and ,@bindings ,@body)))))


:EOF
