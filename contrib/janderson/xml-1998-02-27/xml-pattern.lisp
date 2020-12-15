;;; -*- package: ("XML-PARSER")
;;;
;;; this version (C) mecom gmbh 24.11.97
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.
;;;
;;; this is VERY much in progress. its here only because i'm sending the
;;; development source directly. it has worked, but i'm rewriting it for
;;; a pair of continuations rahter than just a succeed...

(in-package :xml-PARSER)

#|
 <FILE-DOCUMENTATION>
 <KEYWORDS>XML, XSL, pattern-matching</KEYWORDS>
 <DESCRIPTION>
 <P>
 "markup" (in this case XML-markup) is most useful to add 'self-description'
 to data which exhibits content and structure variations. the description is
 used to determine what and how to operate on the data.
 <BR>
 this situation appears frequently when exchanging data between applications
 or systems. because of the variations markup permits, a direct approach
 based on procedurally encoded processing would require complex conditional
 filtering to perform extensional classification during internalization.
 <BR>
 a clearer method is to declare the extensional classification as markup
 patterns and to use the declarations to classify data on the basis of structure
 and/or content.
 <BR>
 the pattern-directed processing classification compares the form and/or
 content of a given data node against a pattern element. it binds matched
 elements to asserted types and uses the collected child data items to specify
 an extensional type for the parent.
 </P>
 <P>
 the pattern can be implemented in any of a number of ways:
 <UL>
 <LI>it can be encoded as an element tree of the same type as the data to be
 processed. in this case certain distinguished attributes (TYPE, OCCURRENCE, and
 VARIABLE) specify structural and identity information.
 <LI>it can include <CODE>TARGET-ELEMENT</CODE> and <CODE>ELEMENT</CODE>
 elements according to XSL.
 in this case the declarations
 <PRE>&LT;!ATTLIST TARGET-ELEMENT
         OCCURRENCE CDATA #FIXED +
         VARIABLE CDATA #FIXED TARGET-ELEMENT &GT
&LT;!ATTLIST ELEMENT
         TYPE CDATA #FIXED t
         OCCURRENCE CDATA #FIXED + &GT</PRE>
 are assumed.
 <LI>it can be an element model.
 in this case <CODE>TYPE</CODE> is identical with the element name,
 <CODE>OCCURRENCE</CODE> is that specified in the model, and no variable
 is specified. 
 </UL>
 as an extension to the XSL specification an attribute value may take on a
 compound boolean expression.

 <P>
 a pattern-tree and data tree are matched with the following mechanism.
 the control structure is continuation-based: at each step, if the test
 succeeds the default method for succeeding invokes a provided continuation 
 with the resulting environment.
 otherwise the match fails. the default method for failing is to return the
 binding environment to that point.
 <UL>
 <LI>
 the initial state is pattern-node, data-node, null-environment.
 <LI>
 the pattern type is matched agains the node type. if that succeeds, then if a
 variable is specified, the environment is augmented and the pattern attributes
 and content are matched against the node attributes content in the augmented
 environment.
 otherwise the match fails.
 <LI>
 to match the types:
 <UL>
 <LI>
 if no type attribute it provided, then for data nodes the tag name is the type
 while for pattern nodes the null type is assumed
 the comparison supports boolean expressions which it deconstructs to identity
 comparisons.
 <LI>
 if the pattern element specifies a <CODE><B>*</B></CODE> type, then
 the match type match succeeds.
 <LI>
 if the pattern element specifies a <CODE><B>*</B></CODE> or a
 <CODE><B>?</B></CODE> occurrence, then
 the element match succeeds without recursing even when type and or attribute
 specifications don't match.
 </UL>
 <LI>
 to match attributes, for each pattern attribute:
 <UL>
 <LI>for standard elements the pattern attributes <CODE>OCCURRENCE</CODE>,
     <CODE>TYPE</CODE>, and <CODE>VARIABLE</CODE> are ignored.
 <LI>for pattern elements, the attribute <CODE>ATTRIBUTES</CODE>, or if none is
     present, then the element <CODE>ATTRIBUTES</CODE> specifies the
     attributes to match.
 <LI>for each pattern attribute, a data attribute is sought with the same name
     as the pattern attribute.
 <LI>if none exists the match fails.
 <LI>if one exists, the attribute values are matched.
       the attribute values match if either
     <BR>
     they are equal, or 
     <BR>
     if the pattern attribute value is a regular expression and a
     regexp match succeeds, or
     <BR>
     if the pattern is a boolean expression for which the deconstructed matches
     succeed
 <LI>if all attributes match, the attribute match succeeds.
 </UL>
 <LI>
 to match the contents, for each pattern member element:
 <UL>
 <LI>for pattern elements, the element <CODE>ATTRIBUTES</CODE> is ignored.
 <LI>otherwise a matching member element of the data content is sought.
     the match succeeds when either
     <BR>
     the pattern value is a literal and is equal to a data content member -
     whereby text elements or treated as identical with their string content,
     <BR>
     the pattern value is a regular expression and a regexp match succeeds
     with a data content member element, or the
     <BR>
     the pattern value is itself an element and matches a
     data content member element.
 <LI>if it is possible to match all member elements, the content match succeeds.
 </UL>
 </UL>
 </DESCRIPTION>
 <CHRONOLOGY>
 <DT>19971024 jaa.mecom
 <DD>initial version supporting xml-element instances,
     simple primitive data, and function predicates
 <DT>19971031 jaa.mecom
 <DD>continuation passing control structure and boolean expressions
 <DT>19971110 jaa.mecom
 <DD>pattern markers from (?, +, *) to (!, +, *, ?) with +, *, ? meanings
     as for element models and ! meaning that the element mus tbe present.
 <DT>971114 jaa.mecom
 <DD>replaced the element-based match structure with a model-based match structure
     in order to support validation.
 </CHRONOLOGY>
 </FILE-DOCUMENTATION>
 |#

;;;
;;; the interface

(export '(pattern-succeed
          pattern-fail
          pattern&datum
          pattern&datum-expression
          pattern&datum->continue
          pattern&datum/types?
          pattern&datum/attributes->continue
          pattern-attribute&datum->continue
          pattern&datum/relations->continue
          pattern-collector
          bind-pattern-variable
          pattern&datum/relations->continue-occurrence
          ))

(defGeneric pattern-succeed
  (pattern data succeed fail result))

(defGeneric pattern-fail
  (pattern data succeed fail result why))

(defGeneric pattern&datum
  (pattern datum &key succeed fail result)
  (:documentation
   "this, the primary matcher interface function, accepts a pattern and a data
    node, and optionally a continuation function and a binding environment.
    the base implementation expects a function and a p-list respectively, but
    specialized methods for match succeed can be provided to support other
    implementations."))

(defGeneric bind-pattern-variable
  (variable datum environment)
  (:argument-precedence-order datum variable environment)
  (:documentation
   "binds the result of a match in the given environment.
    specialized for pattern-elements and for collectors.
    the collectors distinguish between repetitions, optionals, and singles."))
    


;(delete-package *pattern-package*)

(defVar *pattern-package* (defpackage "?" (:use))
  "defined to contain bindings for the alter-ego functions in pattern functions.")

(export '(XSL::TYPE XSL::ATTRIBUTES XSL::OCCURRENCE XSL::VARIABLE) :XSL)


#|
 pattern operations specify two interfaces: one for the pattern to match, and
 one for the datum to be matched.
 the first interface is specified for <CODE>XML-PATTERN</CODE> instances,
 the second for <CODE>XML-DATUM</CODE> instances.
 <P>
 the respective functions 


    the base implementation includes support for the <CODE>XML-ELEMENT</CODE>
    and <CODE>DTD-MODEL</CODE> and <CODE>XML-PATTERN-ELEMENT</CODE> classes."))
 |#

(defClass xml-pattern-element (xml-pattern xml-element)
  ()
  (:documentation "for ANY, ELEMENT, and ELEMENT-TARGET elements."))

(defMethod xml-pattern.relations
           ((node xml-pattern-element))
  (remove-if #'(lambda (type) (find type *xsl-excluded-content-types*))
             (call-next-method) :key #'xml-node.name))

(defMethod xml-pattern.attributes
           ((pattern xml-pattern-element))
  (or (xml-element.get pattern 'xsl::attributes)
      (remove 'xsl::attributes (xml-element.content pattern)
              :test-not #'eq
              :key #'xml-pattern.type)))

(defMethod xml-pattern.occurrence
           ((pattern xml-pattern-element))
  (xml-element.get pattern 'xsl::occurrence))

(defMethod xml-pattern.type
           ((node xml-pattern-element))
  ;; if no type is specified, then the type should match.
  ;; if the element has a dtd definition, that will take precedence
  (or (xml-element.get node *xsl-type-attribute*) t))

(defMethod xml-pattern.variable
           ((node xml-pattern-element))
  (xml-element.get node *xsl-variable-attribute*))

(defMethod xml-pattern.wildcard?
           ((node xml-pattern-element))
  (eq (xml-pattern.type node) 'xsl::any))

(defMethod xml-pattern.attributes ((node dtd-element-declaration)) nil)
(defMethod xml-pattern.occurrence ((node dtd-element-declaration)) 1)
(defMethod xml-pattern.relations ((node dtd-element-declaration))
  (dtd-element-declaration.model node))
(defMethod xml-pattern.type ((node dtd-element-declaration))
  (dtd-element-declaration.name node))
(defMethod xml-pattern.variable ((node dtd-element-declaration)) nil)



(defMethod element-class ((op (eql 'element)) (name (eql 'XSL::ANY)) (context t))
  *pattern-element-class*)
(defMethod element-class ((op (eql 'element)) (name (eql 'XSL::CHILDREN)) (context t))
  *pattern-element-class*)
(defMethod element-class ((op (eql 'element)) (name (eql 'XSL::TARGET-ELEMENT)) (context t))
  *pattern-element-class*)
(defMethod element-class ((op (eql 'element)) (name (eql 'XML::ELEMENT)) (context t))
  *pattern-element-class*)

;;;
;;; the success/failure functions are generic and can be specialized for
;;; individual cases. the base methods invokde a success continuation and
;;; return the constructed environment, respectively.

(defMethod pattern-succeed
           ((pattern t) (datum t) (succeed function) (fail function) (result t))
  (funcall succeed pattern datum result))

(defMethod pattern-fail
           ((pattern t) (datum t) (succeed function) (fail function)
            (result t) (why t))
  ;(inspect (list pattern datum))
  (funcall fail pattern datum result why))

;;;
;;; genereal utility for boolean expessions
;;; the base method applies a predicate to the datum. ancilliary methods
;;; support prefix boolean expressions and element models. element models are
;;; mapped to the equlivalent boolean expression.

(defMethod pattern&datum-expression
           (predicate (expression cons))
  (apply #'pattern&datum-expression-op predicate expression))

(defMethod pattern&datum-expression
           (predicate (expression dtd-model-group))
  (apply #'pattern&datum-expression-op predicate
         (dtd-model.connector expression)
         (dtd-model.content expression)))

(defMethod pattern&datum-expression
           (predicate (expression dtd-element-model))
  (funcall predicate (xml-node.name (dtd-model.content expression))))

(defMethod pattern&datum-expression
           (predicate (expression t))
  (funcall predicate expression))


(defMethod pattern&datum-expression-op
           (predicate (op (eql 'and)) &rest operands)
  (if operands
    (and (pattern&datum-expression predicate (first operands))
         (apply #'pattern&datum-expression-op predicate op (rest operands)))
    t))

(defMethod pattern&datum-expression-op
           (predicate (op (eql 'or)) &rest operands)
  (if operands
    (or (pattern&datum-expression predicate (first operands))
        (apply #'pattern&datum-expression-op predicate op (rest operands)))
    nil))

(defMethod pattern&datum-expression-op
           (predicate (op (eql 'not)) &rest operands)
  (if operands
    (not (pattern&datum-expression predicate (first operands)))
    nil))

(defMethod pattern&datum-expression-op
           (predicate (op (eql *and-marker*)) &rest operands)
  (apply 'pattern&datum-expression-op predicate 'and operands))

(defMethod pattern&datum-expression-op
           (predicate (op (eql *or-marker*)) &rest operands)
  (apply 'pattern&datum-expression-op predicate 'or operands))

(defMethod pattern&datum-expression-op
           (predicate (op (eql *declaration-tag-marker*)) &rest operands)
  (apply 'pattern&datum-expression-op predicate 'not operands))

;(make-model (with-input-from-string (stream "(| a b)") (read-markup-model stream)))

;(pattern&datum-expression #'(lambda (d) (eq d 's)) '(and (not q) (not a)))
;(pattern&datum-expression #'(lambda (d) (eq d 's)) '(or q s))
;(pattern&datum-expression #'(lambda (d) (eq d 's)) '(not a))


;;;
;;; the primary interface function is a generic succeed/fail/result
;;; continuation-passing mechanism.

(defMethod pattern&datum
           (pattern datum &key (succeed nil) (fail nil) (result nil))
  (pattern&datum->continue pattern datum
                           (or succeed
                               #'(lambda (pattern datum result)
                                   (declare (ignore pattern))
                                   (return-from pattern&datum
                                     (values result datum))))
                           (or fail
                               #'(lambda (pattern datum result what-failed)
                                   (return-from pattern&datum
                                     (values nil 
                                             (list pattern datum
                                                   result what-failed)))))
                           result))


(defMethod pattern&datum->continue
           ((pattern xml-pattern) (datum xml-datum) succeed fail result)
  ;; unless the pattern has no type the type must match
  ;; if that succeeds, then attempt to match the attributes and the content
  ;; if they succceed, pass the result to the continuation
  ;; if not, return the matched environment.
  (if (pattern&datum/types? pattern datum)
    (pattern&datum/attributes->continue
     pattern
     datum
     #'(lambda (inner-pattern inner-datum inner-result)
         (declare (ignore inner-pattern inner-datum inner-result))
         (pattern&datum/relations->continue
          pattern
          datum
          succeed
          fail
          result))
     fail
     result)
    (pattern-fail pattern datum succeed fail result 'pattern&datum->continue)))


(defMethod pattern&datum->continue
           ((pattern cons) (datum xml-datum) succeed fail result)
  (pattern&datum->continue pattern (xml-datum.content datum)
                           succeed fail result))

(defMethod pattern&datum->continue
           ((pattern t) (node t) succeed fail result)
  (if (equalp node pattern)
    (pattern-succeed pattern node succeed fail result)
    (pattern-fail pattern node succeed fail result 'pattern&datum->continue)))


(defMethod pattern-fail
           ((pattern xml-pattern-element) (datum t)
            succeed fail result
            (what (eql 'pattern&datum/relations->continue)))
  ;; if the structural match fails for a wildcard pattern element, then
  ;; try the children anyway
  (if (xml-pattern.wildcard? pattern)
    (pattern&datum/relations->continue
     pattern (xml-datum.relations datum) succeed fail result)
    (call-next-method)))

;;;
;;; name is matched by identity or through deconstructing a boolean match

(defMethod pattern&datum/types?
           ((pattern xml-pattern) (datum t))
  (pattern&datum/types? (xml-pattern.type pattern) datum))

(defMethod pattern&datum/types?
           ((pattern t) (datum xml-datum))
  (pattern&datum/types? pattern (xml-datum.type datum)))

(defMethod pattern&datum/types?
           ((pattern (eql t)) (datum t))
  datum)

(defMethod pattern&datum/types?
           ((pattern-type symbol) (datum symbol))
  (eq pattern-type datum))

(defMethod pattern&datum/types?
           ((pattern-type t) (datum t))
  (equalp pattern-type datum))

;; support for expressions
(defMethod pattern&datum/types?
           ((pattern-type-form cons) (datum t))
  (pattern&datum-expression
   #'(lambda (pattern-type)
       (pattern&datum/types? pattern-type datum))
   pattern-type-form))

(defMethod pattern&datum/types?
           ((pattern-type t) (datum cons))
  (pattern&datum/types? datum pattern-type))

;; support for pattern elements allows match so long as 

;(pattern&datum/types? '(and (not q) (not w)) 'a)
;(pattern&datum/types? '(and (not q) (not w)) '(or a s)) => t
;(pattern&datum/types? '(and (not q) (not w)) '(or a q)) => nil



;;;
;;; attributes are matched through value identity or through
;;; deconstructing a value match. since the datum attribute values are
;;; not necessarily bound directly to the datum being tested, they must be
;;; retrieved individually as required.

(defMethod deconstruct-pattern-attributes
           ((name symbol) rest)
  (values name (first rest) (rest rest)))

(defMethod deconstruct-pattern-attributes
           ((att xml-attribute) rest)
  (values (xml-attribute.name att) (xml-attribute.value att) rest))

(defMethod deconstruct-pattern-attributes
           ((el xml-element) rest)
  (values (xml-element.get el 'xsl::name) (xml-element.get el 'xsl::value) rest))

(defMethod deconstruct-pattern-attributes
           ((name t) rest)
  (warn "illegitiamte attribute: ~s." name)
  (when rest (deconstruct-pattern-attributes (first rest) (rest rest))))

;;;
;;; match attribute set by iterating over the attributes
;;; allow either a plist of a list of attribute instances.

(defMethod pattern&datum/attributes->continue
           ((pattern xml-pattern) (datum xml-datum) succeed fail result)
  (pattern&datum/attributes->continue
   (xml-pattern.attributes pattern)
   (xml-datum.attributes datum)
   succeed
   fail
   result))

(defMethod pattern&datum/attributes->continue
           ((pattern xml-pattern) (datum t) succeed fail result)
  (pattern&datum/attributes->continue
   (xml-pattern.attributes pattern) datum succeed fail result))

(defMethod pattern&datum/attributes->continue
           ((pattern-attributes cons) (datum t) succeed fail result)
  (multiple-value-bind
    (name value rest)
    (deconstruct-pattern-attributes (first pattern-attributes)
                                    (rest pattern-attributes))
    (pattern-attribute&datum->continue
     name value datum
     #'(lambda (name datum result)
         (declare (ignore name))
         (pattern&datum/attributes->continue rest datum succeed fail result))
     #'(lambda (name datum result what)
         (declare (ignore name what))
         (pattern-fail pattern-attributes datum succeed fail result
                     'pattern&datum/attributes->continue)))
    result))

(defMethod pattern&datum/attributes->continue
           ((pattern-attributes null) (datum t) succeed fail result)
  (pattern-succeed pattern-attributes datum succeed fail result))


;;;
;;; match individual attribute according to name

(defMethod pattern-attribute&datum->continue 
           (name value (datum xml-element) succeed fail result)
  (pattern-attribute&datum->continue name value
                                     (xml-element.get datum name)
                                     succeed
                                     fail result))

(defMethod pattern-attribute&datum->continue 
           (name value (datum t) succeed fail result)
  (declare (ignore value))
  (pattern-fail name datum succeed fail result))

(defMethod pattern-attribute&datum->continue 
           (name value (datum cons) succeed fail result)
  (pattern-attribute&datum->continue name datum value succeed fail result))

(defMethod pattern-attribute&datum->continue 
           (name (expression cons) datum succeed fail result)
    (if (pattern&datum-expression #'(lambda (exp-value)
                                      (pattern-attribute&datum->continue
                                       exp-value datum
                                       ;; here return the boolean result
                                       #'(lambda (&rest args)
                                           (declare (ignore args)
                                                    (dynamic-extent args))
                                           t)
                                       fail
                                       nil))
                                  expression)
      (pattern-succeed name datum succeed fail result)
      (pattern-succeed name datum succeed fail result
                       'pattern-attribute&datum->continue)))

(defMethod pattern-attribute&datum->continue
           ((name t) (value t) (datum t) succeed fail result)
  (if (equalp value datum)
    (pattern-succeed name datum succeed fail result)
    (pattern-fail name datum succeed fail result
                'pattern-attribute&datum->continue)))

(defMethod pattern-attribute&datum->continue
           (name (predicate function) (datum t) succeed fail result)
  (if (funcall predicate (xml-element.get datum name))
    (pattern-succeed name datum succeed fail result)
    (pattern-fail name datum succeed fail result
                'pattern-attribute&datum->continue)))


;;;
;;; content match

(defMethod pattern&datum/relations->continue
           ((pattern xml-pattern) (datum xml-datum) succeed fail result)
  (pattern&datum/relations->continue
   (xml-pattern.relations pattern)
   (xml-datum.relations datum)
   #'(lambda (inner-pattern inner-datum inner-result)
       (declare (ignore inner-pattern inner-datum))
       (pattern-succeed pattern datum succeed fail inner-result))
   fail
   result))

(defMethod pattern&datum/relations->continue
           ((pattern xml-pattern) (datum xml-datum) succeed fail result)
  (pattern&datum/relations->continue
   (xml-pattern.relations pattern)
   (xml-datum.relations datum)
   #'(lambda (inner-pattern inner-datum inner-result)
       (declare (ignore inner-pattern inner-datum))
       (pattern-succeed pattern datum succeed fail inner-result))
   fail
   result))


(defMethod pattern&datum/relations->continue
           ((pattern t) (datum xml-datum) succeed fail result)
  (pattern&datum/relations->continue
   pattern
   (xml-datum.relations datum)
   succeed
   fail
   result))

(defMethod pattern&datum/relations->continue
           ((pattern xml-pattern) (datum list) succeed fail result)
  (pattern&datum/relations->continue-occurrence
   pattern datum (xml-pattern.collector pattern) succeed fail result))

(defMethod pattern&datum/relations->continue
           ((pattern cons) (datum list) succeed fail result)
  (pattern&datum/relations->continue
   (first pattern) datum
   #'(lambda (inner-pattern inner-datum result)
      (declare (ignore inner-pattern inner-datum))
      (pattern&datum/relations->continue
       (rest pattern) datum succeed fail result))
   #'(lambda (pattern datum result why)
       (pattern-fail pattern datum succeed fail result why))
   result))

(defMethod pattern&datum/relations->continue
           ((pattern null) (datum t) succeed fail result)
  ;; everything succeeded
  (pattern-succeed pattern datum succeed fail result))

(defMethod pattern&datum/relations->continue
           ((pattern function) (datum t) succeed fail result)
  (if (funcall pattern datum)
    (pattern-succeed pattern datum succeed fail result)
    (pattern-succeed pattern datum succeed fail result
                     'pattern&datum/relations->continue)))
                                      
(defMethod pattern&datum/relations->continue
           ((pattern t) (datum t) (succeed t) (fail t) (result t))
  (warn "pattern type? ~s" pattern)
  (pattern-fail pattern datum succeed fail result
              'pattern&datum/relations->continue))

(defMethod pattern&datum/relations->continue
           ((pattern string) (datum cons) succeed fail result)
  (pattern&datum->continue
   pattern
   (first datum)
   #'(lambda (pattern datum result)
       (pattern-succeed pattern datum
                        succeed fail result))
   #'(lambda (&rest args)
       (declare (ignore args)
                (dynamic-extent args))
       (pattern&datum/relations->continue
        pattern (rest datum) succeed fail result))))


(defMethod xml-pattern.collector
           ((pattern xml-pattern))
  (make-pattern-collector (xml-pattern.occurrence pattern) pattern))

(defMethod make-pattern-collector
           ((occurrence null) pattern)
  (make-pattern-collector 1 pattern))

(defClass pattern-collector ()
  ((pattern :initarg :pattern :accessor pattern-collector.pattern)
   (data :initarg data :initform nil :reader pattern-collector.data
         :writer pattern-collector.set-data)
   (min :initarg :min :accessor pattern-collector.min)
   (max :initarg :max :accessor pattern-collector.max))
  (:documentation
   "a cache for matching a pattern against an element set. supports constraints
    on match count."))
(defClass count-pattern-collector (pattern-collector) ())
(defClass rep-pattern-collector (pattern-collector) ())
(defClass opt-pattern-collector (pattern-collector) ())
(defClass plus-pattern-collector (pattern-collector) ())

(defMethod (setf pattern-collector.data)
           ((data list) (collector pattern-collector))
  (when (<= (pattern-collector.min collector) (length data)
            (pattern-collector.max collector))
    (pattern-collector.set-data data collector)))

(defMethod pattern-collector-satisfied?
           ((collector pattern-collector))
  (<= (if *xml-validate?* (pattern-collector.min collector)
          1)
      (length (pattern-collector.data collector))
      (if *xml-validate?* (pattern-collector.max collector)
          most-positive-fixnum)))

(defMethod make-pattern-collector
           ((occurrence (eql *rep-marker*)) pattern)
  (make-instance 'rep-pattern-collector
    :min 0 :max most-positive-fixnum :pattern pattern))

(defMethod make-pattern-collector
           ((occurrence number) pattern)
  (make-instance 'count-pattern-collector
    :min occurrence :max occurrence :pattern pattern))

(defMethod make-pattern-collector
           ((occurrence (eql *opt-marker*)) pattern)
  (make-instance 'opt-pattern-collector
    :min 1 :max 1 :pattern pattern))

(defMethod make-pattern-collector
           ((occurrence (eql *plus-marker*)) pattern)
  (make-instance 'plus-pattern-collector
    :min 1 :max most-positive-fixnum :pattern pattern))


;;;
;;; environment manipulation functions

(defMethod bind-pattern-variable
           ((pattern t) (collector rep-pattern-collector) (environment t))
  (bind-pattern-variable pattern (pattern-collector.data collector) environment))

(defMethod bind-pattern-variable
           ((pattern t) (collector count-pattern-collector) (environment t))
  ;; without occurrence is assumed to be imply once
  (bind-pattern-variable pattern
                          (if (> (pattern-collector.max collector) 1)
                            (pattern-collector.data collector)
                            (first (pattern-collector.data collector)))
                          environment))

(defMethod bind-pattern-variable
           ((pattern t) (collector opt-pattern-collector) (environment t))
  (bind-pattern-variable pattern
                         (first (pattern-collector.data collector))
                         environment))

(defMethod bind-pattern-variable
           ((pattern t) (collector plus-pattern-collector) (environment t))
  (bind-pattern-variable pattern (pattern-collector.data collector) environment))

(defMethod bind-pattern-variable
           ((pattern xml-pattern) (datum t) (environment t))
  (bind-pattern-variable (xml-pattern.variable pattern) datum environment))

(defMethod bind-pattern-variable
           ((pattern-variable symbol) (datum t) (environment list))
  (acons pattern-variable datum environment))


(defMethod pattern&datum/relations->continue-occurrence
           ((pattern xml-pattern) (datum list)
            (collector pattern-collector) succeed fail result)
  ;; match zero or more
  (dolist (node datum)
    (setf result (pattern&datum->continue
                  pattern
                  node
                  #'(lambda (pattern inner-datum inner-result)
                      (if (push inner-datum (pattern-collector.data collector))
                        (cond ((or *xml-validate?*
                                   (not (pattern-collector-satisfied? collector)))
                               (setf result inner-result)
                               (go :next))
                              (t
                               (pattern-succeed pattern datum
                                                succeed fail
                                                (bind-pattern-variable
                                                 pattern
                                                 collector
                                                 inner-result))))
                        (pattern-fail pattern datum
                                      succeed fail
                                      result
                                      'pattern&datum/relations->continue)))
                  #'(lambda (pattern inner-datum inner-result why)
                      (declare (ignore pattern inner-datum why))
                      (setf result inner-result)
                      (go :next))
                  result))
    :next)
  (if (pattern-collector-satisfied? collector)
    (pattern-succeed pattern datum succeed fail
                     (bind-pattern-variable pattern collector result))
    (pattern-fail pattern datum succeed fail result
                'pattern&datum/relations->continue)))


;;;
;;; pattern analysis utilities

(defMethod pattern-variables
           ((pattern xml-pattern) &optional names &aux name)
  (when (setf name (xml-pattern.variable pattern))
    (setf names (cons name names)))
  (dolist (member (xml-pattern.relations pattern))
    (setf names (pattern-variables member names)))
  names)

(defMethod pattern-names
           ((pattern t) &optional names)
  names)

(defMethod pattern-names
           ((patterns list) &optional names)
  (dolist (pattern patterns)
    (setf names (pattern-names pattern names)))
  names)



;; a base xsl dtd. (see also the dtd directory)

(defParameter *xsl-dtd*
  (read-dtd-stream
   "<!ELEMENT XSL (RULE*) >
     <!ELEMENT RULE (PATTERN, ACTION) >
     <!ELEMENT ACTION #PCDATA >
     <!ELEMENT PATTERN (ELEMENT | TARGET-ELEMENT) >
     <!ELEMENT ELEMENT (ELEMENT | TARGET-ELEMENT)* >
     <!ELEMENT TARGET-ELEMENT (#PCDATA | ELEMENT)* >
     <!ATTLIST (ELEMENT TARGET-ELEMENT)
               TYPE ID #IMPLIED >"
   "XSL"))

#|
(%dtd-clear (dtd "XSL"))
(inspect (dtd "XSL"))
(setf (dtd t) nil)
(mapc #'print (dtd-instances))
|#

(defun read-pattern-element
       (from &key ((:element-class *xml-element-class*) 'xml-pattern-element)
                  ((:dtd *dtd*) *xsl-dtd*)
                  ((:package *package*) (dtd.package *dtd*)))
  (read-markup-element from))

(set-markup-dispatch-macro #\? 'read-pattern-element t) 
  
:EOF


#|
(defparameter *xe*
  (read-markup-element
   "<person>
     <beruf>Rechtsanwalt</beruf>
     <anschrift XSL:TYPE=rechnung><name>Rogenstein Rechnung</name></anschrift>
     <anschrift><name>Rogenstein 2</name></anschrift>
     </person>"))


(defparameter *xp1*
  (xml-pattern-element
   "<element TYPE=PERSON><beruf?>Rechtsanwalt</beruf><anschrift?/></person>"))

(defparameter *xp2*
  (xml-element
   "<person?><beruf>Rechtsanwalt</beruf><var1? :name=rechnung></var1></person>"))

(defparameter *xp3*
  (list (xml-element "<beruf?/>")
        (xml-element "<rechnung?/>")))
(trace match-pattern-to-datum)
(trace match-node-name)
(trace match-node-attribute)
(untrace)
(inspect (match-node *xe* *xp1* nil))
(trace bind-pattern-variable)
(inspect (match-node *xe* *xp2* nil))
(inspect (match-node *xe* *xp3* nil))
 |#
