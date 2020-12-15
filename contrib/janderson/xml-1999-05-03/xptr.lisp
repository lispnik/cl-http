;;; -*- Package: "XMLP"; -*-

"<DOCUMENTATION>
 <COPYRIGHT VERSION='0.432' AUTHOR='(C) mecom gmbh 19990302' >
  available only from the cl-http repository and NOT to be REdistributed
  in ANY form. see cl-xml.html.
  </COPYRIGHT>
 <DESCRIPTION>
  XML pointers (or xpointers) are a means to specify a set of elements in a
  document.
  <P>
  within a server, they serve three purposes
  <UL>
   <LI>the can express a query constriant, to specify an elemental fragment of a
       uri to be delivered in response to a request;</LI>
   <LI>they can express a location to be modified as part of an editing
       operation;</LI>
   <LI>they can specify an access path to locate part of a document parsed by the
       server for its internal use.</LI>
   </UL>
  in order to accomplish this, they are modeled as a combination of several
  classes:
  <UL>
   <LI><A HREF='/Doc/CLASS?NAME=XMLP::XPTR'><CODE>XPTR</CODE></A>
   <LI><A HREF='/Doc/CLASS?NAME=XMLP::XPTR-TERM'><CODE>XPTR-TERM</CODE></A>
   <LI><A HREF='/Doc/CLASS?NAME=XMLP::XPTR-LOCATION'><CODE>XPTR-LOCATION</CODE></A>
   </UL>
  <P>
  <DL>
   <DT><A HREF='/Doc/CLASS?NAME=XMLP::XPTR'><CODE>XPTR</CODE></A></DT>
   <DD>
    an <CODE>XPTR</CODE> simple binds a term and the remaining terms. in a sense
    it acts as a <CODE>CONS</CODE> cell, but affords specialization.</DD>
   <DT><A HREF='/Doc/CLASS?NAME=XMLP::XPTR-TERM'><CODE>XPTR-TERM</CODE></A></DT>
   <DD>
    a <CODE>XPTR-TERM</CODE> binds at least the term keyword. for certain
    absolute terms that is all that is bound. for others and for relative terms,
    there are additional argument bindings specific to the kind of reference.
    </DD>
   <DT><A HREF='/Doc/CLASS?NAME=XMLP::XPTR-LOCATION'><CODE>XPTR-LOCATION</CODE></A></DT>
   <DD>
    in order to perform both read and write operations on the denoted instance
    data, without needing to relocate the reference for each operation, the
    effective location is captured as a <CODE>XPTR-LOCATION</CODE>. the location
    can be used at a later point as the object of the operations
    <UL>
     <LI><CODE>(<A HREF='/Doc/FUNCTION?NAME=XMLP::XPTR-LOCATION.VALUE'>XPTR-LOCATION.VALUE</A>
          <I>location</I>)</CODE>
     <LI><CODE>(SETF (<A HREF='/Doc/FUNCTION?NAME=(CL::SETF XMLP::XPTR-LOCATION.VALUE)'>XPTR-LOCATION.VALUE/A>
          <I>location</I>) <I>value</I>)</CODE>
     </UL>
     the location is generated as a product of three values: the context datum,
     the term, and the located datum. these three values are managed as special
     bindings and interpreted at the point where the location isntance is
     created, in order to determine what to cache.
    </DD>
   </DL>
  </DESCRIPTION>
 <CHRONOLOGY>
  <DELTA DATE='19981200'>initial version</DELTA>
  <DELTA DATE='19990223'>version with binding macros</DELTA>
  <DELTA DATE='19990303'>support for #string</DELTA>
  <DELTA DATE='19990310'>
   attribute terms intern their 'string' value and test on eq to the prospective node.
  <DELTA DATE='19990420'>get-precedence-list</DELTA>
   </DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
 "

(in-package "XMLP")

(defParameter *xptr-package* (defpackage "%XPTR" (:use)))
(defVar *xptr-predecessor* nil)
(defVar *xptr-term* nil)
(defVar *xptr-successor* nil)
(defVar *xptr-wild-element-type* '%XPTR::|#element|)
(defVar *xptr-wild-string-type* '%XPTR::|#string|)
(defVar *xptr-set-p* nil
  "records that some term in the pointer has an 'all' quantifier")
(defVar *from-end* nil)
(defVar *all-selector-p* nil)
(defVar *all-quantifier* '%xptr::|all|)

(defClass xptr ()
  ((term
    :initarg :term
    :accessor xptr.term
    :type xptr-term)
   (rest
    :initarg :rest
    :accessor xptr.rest
    :type (or xptr null)))
  (:documentation
   "a <CODE>XPTR</CODE> acts as a types <CODE>CONS</CODE>.
    it binds an initial term and a remainder - which is either a successor
    pointer for the rest of the path or </CODE>NIL</CODE>."))


(defClass xptr-accessor ()
  ((locations
    :initform nil :initarg :locations
    :type (or element attribute list)
    :accessor xptr-accessor.locations
    :documentation
    "binds the set of nodes matched. for a pointer in which all terms have a
     numeric selector, this is a list of at most one element.")
   (term
    :initform nil :initarg :term
    :type xptr-term
    :accessor xptr-accessor.term
    :documentation "binds the final matched term")
   (xptr
    :initform nil :initarg :xptr
    :type xptr
    :accessor xptr-accessor.xptr
    :documentation "binds the xptr root"))
  (:documentation
   "a <CODE>XPTR-ACCESSOR</CODE> binds the results produced by resolving an xptr
    against a specific element. the result may be either a single node or a list
    of nodes, depending on the quantifiers present in the pointer"))


(defClass xptr-term ()
  ()
  (:documentation
   "the abstract class for all pointer terms."))

(defClass xptr-term-with-attributes (xptr-term)
  ((attribute-arity :allocation :class)
   (attributes
    :initarg :attributes
    :accessor xptr-term.attributes))
  (:documentation
   "a mixin-class which parses the third and later elements of an argument
    string as an attribute plist."))

(defClass xptr-term-with-quantifier (xptr-term)
  ((quantifier
    :initarg :quantifier
    :accessor xptr-term.quantifier))
  (:documentation
   "a mixin-class which parses the first element of an argument string as a
    quantifier."))

(defClass xptr-term-with-type (xptr-term)
  ((type
    :initarg :type
    :accessor xptr-term.type))
  (:documentation
   "a mixin-class which parses the first element of an argument string as a
    type."))

(defClass xptr-term-with-string (xptr-term)
  ((string
    :initform nil :initarg :string
    :accessor xptr-term.string))
  (:documentation
   "a mixin-class which parses the arguments as a string."))



(defClass xptr-term-with-subterms (xptr-term)
  ((subterm-arity :allocation :class)
   )
  (:documentation
   "a mixin-class which parses all elements of an argument string as a
    subterms."))

(defClass xptr-absolute-term (xptr-term)
  ())

(defClass xptr-relative-term (xptr-term-with-quantifier
                              xptr-term-with-type
                              xptr-term-with-attributes)
  ((attribute-arity :initform nil :allocation :class)
   (quantifier :initform 1)))

(defClass xptr-span-term (xptr-term-with-subterms)
  ((subterm-arity :initform 2 :allocation :class)
   (from
    :initarg :from
    :accessor xptr-term.from)
   (to
    :initarg :to
    :accessor xptr-term.to)))

(defClass xptr-attr-term (xptr-term-with-string)
  ((string
    :accessor xptr-term.value)))

(defClass xptr-string-term (xptr-term-with-quantifier
                            xptr-term-with-string)
  ((string
    :accessor xptr-term.value)))

(defClass %xptr::|root| (xptr-absolute-term) ())
(defClass %xptr::|origin| (xptr-absolute-term) ())
(defClass %xptr::|id| (xptr-term-with-string xptr-absolute-term) ())
(defClass %xptr::|html| (xptr-term-with-string xptr-absolute-term) ())
(defClass %xptr::|child| (xptr-relative-term) ())
(defClass %xptr::|descendant| (xptr-relative-term) ())
(defClass %xptr::|ancestor| (xptr-relative-term) ())
(defClass %xptr::|preceding| (xptr-relative-term) ())
(defClass %xptr::|following| (xptr-relative-term) ())
(defClass %xptr::|psibling| (xptr-relative-term) ())
(defClass %xptr::|fsibling| (xptr-relative-term) ())
(defClass %xptr::|span| (xptr-span-term) ())
(defClass %xptr::|string| (xptr-string-term) ())
(defClass %xptr::|attr| (xptr-attr-term) ())

(defClass xptr-error (simple-error) ())
(defClass xptr-attribute-error (xptr-error) ())
(defClass xptr-quantifier-error (xptr-error) ())
(defClass xptr-type-error (xptr-error) ())

(defGeneric parse-xptr-term-argument
  (instance stream &key &allow-other-keys)
  (:method-combination progn)
  (:documentation
   "the parser for term arguments is executed for side-effect. if an error occurs, a
    condition is raised. otherwise it reads the specific argument elements out of a stream 
    and binds them in the term instance. the instance initargs are passed in addition to the
    instance and the stream. if a given initarg is present, the read elements
    are discarded and not bound. this permits initargs to override the
    encoded values. as this runs out of an :after method in initialize-instance,
    the slot initforms act, when permitted to, as the defaults.
    <P>
    the <CODE>OR</CODE> combination terminates the process at the first error,
    should one occur."))

(defGeneric parse-xptr-term (source &optional type))

;;;
;;;

(defMethod initialize-instance
           ((self xptr) &rest initargs
            &key
            term term-type rest uri
            (start (or (position #\# uri) (position #\| uri) 0))
            (end (length uri))
            (uri-stream nil))
  (when uri (case (char uri start) ((#\# #\|) (incf start)) (t nil)))
  (unless term
    (unless uri-stream
      (setf uri-stream (make-string-input-stream uri start end)))
    (setf term (parse-xptr-term uri-stream term-type))
    (when (eql #\. (peek-char t uri-stream nil nil))
      (read-char uri-stream)
      (unless rest
        (setf rest (make-instance (class-of self) :uri-stream uri-stream
                                  :term-type (type-of term))))))
  (apply #'call-next-method
         self
         :term term
         :rest rest
         :term-type (type-of term)
         initargs))

(defMethod initialize-instance :after
           ((self xptr-term) &rest initargs
            &key
            arguments
            (argument-stream (when arguments (make-string-input-stream arguments))))
  (when argument-stream
    ;; strip the open paren, dispatch to read the content; strip the close paren
    (when (eql (peek-char t argument-stream nil nil) #\()
      (read-char argument-stream))
    (apply #'parse-xptr-term-argument self argument-stream initargs)
    (when (eql (peek-char t argument-stream nil nil) #\))
      (read-char argument-stream))))

(defMethod initialize-instance :after
           ((instance xptr-attr-term) &key)
  (with-slots (string) instance
    (when string
      (setf string (intern-name string)))))


(defun read-xptr-token
       (predicate
        &optional (stream *standard-input*) (terminator-disposition nil)
        &aux
        nc delimiter
        (output *string-buffer*))
  (multiple-value-bind (reader arg) (stream-reader stream)
    (unless (functionp predicate)
      (setf delimiter predicate
            predicate #'(lambda (c) (char= c delimiter))))
    (setf (fill-pointer output) 0)
    (loop (setf nc (funcall reader arg))
          (unless nc (error 'end-of-file :stream stream))
          (when (funcall predicate nc) (return))
          (vector-push-extend nc output))
    (case terminator-disposition
      (:discard )
      (:include (vector-push-extend nc output))
      (t (unread-char nc stream)))
    (if *parse-suppress*
      output
      (subseq output 0))))

(defMethod parse-xptr-term
           ((string string) &optional term-type)
  (parse-xptr-term (make-string-input-stream string) term-type))

(defMethod parse-xptr-term
           ((stream stream) &optional term-type
            &aux (*package* *xptr-package*)
                 term-name)
  ;; allow the type to be carried over from the previous term
  (unless (eql (peek-char t stream) #\()
    (setf term-name (read-xptr-token #\( stream))
    (unless (setf term-type (find-symbol term-name *xptr-package*))
      (error "illegitimate xptr term type: ~s." term-name)))
  (make-instance term-type :argument-stream stream))


(defMethod parse-xptr-term-argument progn
           ((self xptr-term-with-quantifier) argument-stream
            &key (quantifier nil initarg-p) &allow-other-keys
            &aux parsed-quantifier)
  (declare (ignore quantifier))
  (flet ((quantifier-error (q)
           (error 'xptr-quantifier-error
                  :format-string "illegitimate quantifier: ~s."
                  :format-arguments (list q))))
    (when (char= (peek-char t argument-stream) #\))
      (quantifier-error nil))
    (setf parsed-quantifier (read-xptr-token #\, argument-stream :discard))
    (with-slots (quantifier) self
      (unless initarg-p
        (setf quantifier parsed-quantifier))
      (unless
        (typecase quantifier
          (integer quantifier)
          (null nil)
          ((or symbol string)
           (if (or (string-equal parsed-quantifier "all")
                   (string-equal parsed-quantifier "*"))
             (setf quantifier *all-quantifier*)
             (setf quantifier (or (parse-integer (string quantifier) :junk-allowed t)
                                  (quantifier-error quantifier)))))
          (t nil))
        (quantifier-error quantifier)))
    self))
                          
(defMethod parse-xptr-term-argument progn
           ((self xptr-term-with-type) argument-stream
            &key (type nil initarg-p) &allow-other-keys
            &aux parsed-type type-name last)
  (declare (ignore type))
  (flet ((type-error (type)
           (error 'xptr-quantifier-error
                  :format-string "illegitimate type: ~s."
                  :format-arguments (list type))))
    (setf type-name (read-xptr-token #'(lambda (c)
                                         (setf last c)
                                         (or (char-equal c #\,)
                                             (char-equal c #\))))
                                     argument-stream))
    (when (eql last #\,) (read-char argument-stream))
    (unless (zerop (length type-name))
      (cond ((string-equal type-name "#element")
             (setf parsed-type *xptr-wild-element-type*))
            ((string-equal type-name "#string")
             (setf parsed-type *xptr-wild-string-type*))
            (t
             (setf parsed-type (intern-name type-name))))
      (unless initarg-p (setf (slot-value self 'type) parsed-type)))
    self))

(defMethod parse-xptr-term-argument progn
           ((self xptr-term-with-attributes) argument-stream
            &key (attributes nil initarg-p) &allow-other-keys
            &aux name value (count 0) parsed-attributes
            peek)
  (declare (ignore attributes))
  (flet ((attribute-error (message)
           (error 'xptr-quantifier-error
                  :format-string "illegitimate attribute: ~a."
                  :format-arguments (list message))))
    (with-slots (attribute-arity) self
      (loop
        (setf peek (peek-char nil argument-stream  nil nil))
        (cond ((null peek) (return))
              ((char= #\) peek) (return)))
        (setf name (read-xptr-token #'(lambda (c)
                                              (when (char= c #\))
                                                (attribute-error "value missing"))
                                              (not (xml-namechar? c)))
                                             argument-stream
                                             :discard))
        (when (zerop (length name)) (attribute-error "null name"))
        (setf name (intern-name name))
        (setf peek (peek-char nil argument-stream))
        (cond ((or (char= peek #\") (char= peek #\'))
               (read-char argument-stream)
               (setf value (read-xptr-token peek argument-stream :discard)))
              (t
               (setf value (read-xptr-token #'(lambda (c)
                                                (or (char= c #\,)
                                                    (char= c #\))))
                                            argument-stream))))
        (when (char= (setf peek (peek-char nil argument-stream)) #\,)
          (read-char argument-stream))
        (push name parsed-attributes)
        (push value parsed-attributes)
        (incf count)
        (when (and attribute-arity (>= count attribute-arity)) (return)))
      (peek-char nil argument-stream nil nil)
      (unless initarg-p
        (setf (slot-value self 'attributes) (nreverse parsed-attributes))))
    self))
           
  
(defMethod parse-xptr-term-argument progn
           ((self xptr-term-with-string) argument-stream
            &key (string nil initarg-p) &allow-other-keys
            &aux (parsed-string (make-array 0 :element-type 'character
                                            :adjustable t :fill-pointer 0))
                 char)
  (declare (ignore string))
  (loop (unless (setf char (read-char argument-stream nil nil)) (return))
        (when (eql char #\)) (unread-char char argument-stream) (return))
        (vector-push-extend char parsed-string))
  (unless initarg-p
    (setf (slot-value self 'string) parsed-string))
  self)

(defMethod parse-xptr-term-argument progn
           ((self xptr-term) (argument-stream t) &key &allow-other-keys)
  ;; the base method does nothing, just keep parsing
  self)


;;;
;;;


(defMethod print-object
           ((datum xptr) (stream t))
  ;; if printing within a node (as an attribute), then just format the terms
  ;; if printing outside of a node, preceed with a reader macro sequence
  (flet ((do-print ()
           (format stream "~a~@[.~a~]" (xptr.term datum) (xptr.rest datum))))
    (if *parent-node*
      (do-print)
      (if *print-escape*
        (let ((*parent-node* datum)) ;; nb. not the cl::*print-readably*
          (write-string "#!>" stream)
          (do-print))
        (with-slots (term rest) datum
          (print-unreadable-object (datum stream :type t)
            (format stream "~a~:[~;...~]" (type-of term) rest)))))))

(defMethod print-object
           ((datum xptr-absolute-term) (stream t))
  (if *parent-node*
    (format stream "~a()" (type-of datum))
    (call-next-method)))

(defMethod print-object
           ((datum xptr-relative-term) (stream t) &aux attributes type)
  (cond (*parent-node*
         (format stream "~a(~a" (type-of datum) (xptr-term.quantifier datum))
         (when (setf type (xptr-term.type datum))
           (write-char #\, stream)
           (if (eq type *xptr-wild-element-type*)
             (write-string "#element" stream)
             (write-identifier (xptr-term.type datum) stream))
           (when (setf attributes (xptr-term.attributes datum))
             (do ((name (pop attributes) (pop attributes))
                  (value (pop attributes) (pop attributes)))
                 ((null name))
               (write-char #\, stream)
               (write-identifier name stream)
               (write-char #\, stream)
               (format stream "~a" value)))
           (write-char #\) stream)))
        (t (call-next-method))))

(defMethod print-object
           ((datum xptr-term-with-string) (stream t))
  (if *parent-node*
    (format stream "~a(~a)"
            (type-of datum) (xptr-term.string datum))
    (call-next-method)))

(defMethod markup-reader-macro (stream (dispatch-readahead (eql #\>)))
  (read-char stream)
  (cond (*read-suppress*
         (loop (when (xml-space? (read-char stream)) (return)))
         (values))
        (t
         (make-instance 'xptr :uri-stream stream))))

;; nb. th
;(princ (make-instance 'xptr :uri "asdf/asdf#root().child(1,element)"))
;#!>root().child(1,element) 

(defun read-xptr (stream sub-char arg)
  (declare (ignore sub-char arg))
  (cond (*read-suppress*
         (loop (when (xml-space? (read-char stream)) (return)))
         (values))
        (t
         (make-instance 'xptr :uri-stream stream))))

(set-dispatch-macro-character #\# #\^ 'read-xptr)


"<H3>pointer resolution</H3>
 <P>
 an xpointer can be resolved against a xml element (dom) graph.
 this can be understood (and implemented) as a constrained walk through the
 respective graph. the steps which succeed are specified by xpointer terms.
 'child' or 'attr', for example, apply to immediate content. 'descendant', on
 the other hand, to a content node at any depth.
 this walk could also be implemented against the instance graph.
 (see xptr-clos.lisp)
 <P>
 <H4>matching</H4>
 <P>
 the base of the navigation mechanism is a collection of matching functions.
 these include base cases to perform the name/type and value-based tests for
 individual terms. the base combinations are:
 <UL>
  <LI>attr term v/s attribute node;</LI>
  <LI>element term (child, descendant, ancestor, preceeding, following,
      psibling, fsibling) v/s element node (with consequential attribute
      matching);</LI>
  <LI>attribute node v/s attribute node (as the basis for element term v/s
      element node;</LI>
  </UL>
 <BR />
 in addition to the match predicate, there are methods to locate candidates for
 the  respective term types.
 <UL>
  <LI>child term searchs the direct content elements</LI>
  <LI>predecessor does a postorder traversal backrwards</LI>
  <LI>successor does a preorder traversal forwards</LI>
  </UL>
 "
(defGeneric xptr-element-match?
  (constraint datum)
  (:documentation
   "tests all aspects of the term against the element to determine if there
    is a match."))

(defMethod xptr-element-match?
           ((constraint null) (datum t))
  t)

(defMethod xptr-element-match?
           ((constraint t) (datum null))
  nil)

(defMethod xptr-element-match?
           ((constraint symbol) (datum symbol))
  (or (eq constraint datum)
      ;; permit a subtype relation
      (and (setf constraint (find-class constraint nil))
           (setf datum (find-class datum nil))
           (find constraint (get-precedence-list datum)))))

(defMethod xptr-element-match?
           ((constraint string) (datum symbol))
  (string= constraint datum))

(defMethod xptr-element-match?
           ((constraint symbol) (datum element))
  (xptr-element-match? constraint (element.name datum)))

(defMethod xptr-element-match?
           ((constraint (eql *xptr-wild-element-type*)) (datum element))
  t)

(defMethod xptr-element-match?
           ((constraint string) (datum element))
  (xptr-element-match? constraint (element.name datum)))

(defMethod xptr-element-match?
           ((constraint string) (datum string))
  ;; perform an explicit check for zero length as i've yet to find the defined
  ;; behaviour for that case.
  (if (or (zerop (length constraint)) (search constraint datum))
    datum))

(defMethod xptr-element-match?
           ((constraint (eql *xptr-wild-string-type*)) (datum string))
  datum)



(defMethod xptr-element-match?
           ((constraint xptr-attr-term) (datum attribute))
  (or (string-equal (xptr-term.value constraint) "*")
      (eq (xptr-term.value constraint) (attribute.name datum))))

(defMethod xptr-element-match?
           ((constraint xptr-attr-term) (node element)
            &aux (name (xptr-term.value constraint)))
  (xml-node.get-attribute-if node
                             #'(lambda (a)
                                 (string= (attribute.name a) name))
                             (when (inherit-attribute? name)
                               #'xml-node.context)))

(defMethod xptr-element-match?
           ((constraint xml::@ID) (datum element))
  (xptr-element-match? constraint
                       (xml-node.get-attribute-by-type datum 'xml::@ID)))

(defMethod xptr-element-match?
           ((constraints cons) (datum t))
  (dolist (constraint constraints t)
    (unless (xptr-element-match? constraint datum)
      (return nil))))

(defMethod xptr-element-match?
           ((constraint xptr-term-with-attributes) (datum element))
  (with-slots (attributes) constraint
    (and (or (null attributes)
             (let ((attributes attributes))
               (do ((name (pop attributes) (pop attributes))
                    (value (pop attributes) (pop attributes)))
                   ((null name) t)
                 (unless (xml-node.get-attribute-if datum
                                                    #'(lambda (a)
                                                        (and (eq (attribute.name a) name)
                                                             (string= (xml-node.string a)
                                                                      value)))
                                                    (when (inherit-attribute? name)
                                                      #'xml-node.context))
                   (return nil)))))
         (or (not (next-method-p)) (call-next-method)))))

(defMethod xptr-element-match?
           ((constraint xptr-term-with-attributes) (datum string))
  (with-slots (attributes) constraint
    (null attributes)))


(defMethod xptr-element-match?
           ((constraint xptr-term-with-type) (element element))
  (with-slots (type) constraint
    (and (xptr-element-match? type element)
         (or (not (next-method-p)) (call-next-method)))))

(defMethod xptr-element-match?
           ((constraint xptr-term-with-type) (element string))
  (with-slots (type) constraint
    (and (eq type *xptr-wild-string-type*)
         (or (not (next-method-p)) (call-next-method)))))



;;; default data access functions functions

(defMethod xptr-node.parent
           ((node element))
  (let ((parent (element.context node)))
    (when (typep parent 'element)
      parent)))

(defMethod xptr-node.parent
           ((node attribute))
  (xml-node.context node))

(defMethod xptr-node.children
           ((node element))
  (element.content node))

(defMethod xptr-node.children
           ((node t))
  nil)

(defMethod xptr-node.psiblings
           ((node xml-node))
  (let* ((parent (xml-node.context node))
         (siblings (when parent (xml-node.content parent)))
         (position (when (consp siblings) (position node siblings))))
    (when position
      (subseq siblings 0 position))))

(defMethod xptr-node.fsiblings
           ((node xml-node))
  (let* ((parent (xml-node.context node))
         (siblings (when parent (xml-node.content parent)))
         (position (when (consp siblings) (position node siblings))))
    (when position
      (subseq siblings (1+ position)))))
  

"<H4>term navigation functions</H4>
 <P>
 the base navigation mechanism is to map an iterator function over all
 locations which are specified by a pointer term relative to the current term.
 those which match are then used as the basis for the next step.
discovered when following the  pointer's path,
 generated by a given term. each step constitutes a relation between an initial
 datum a term and a collection of successor data.
 <P>
 each operates constitutes a relation between three instances, the initial
 datum, the term, and the successor datum. these three state componenets are
 bound to *xptr-predecessor*, *xptr-term, and *xptr-successor* at each step.
 when a search arrives at a leaf, the three state components determine the form
 and content of the location cache.
 <P>
 the successor generation has two phases - first, a generation mechanism is
 specified based on the term, then the generated locations are passed through
 a collection function.
 "


(defMethod node-map-children
           ((datum t) (function t))
  nil)

(defMethod node-map-children
           ((node element) function)
  (map nil #'(lambda (child)
               (funcall function child))
       (if *from-end*
         (reverse (xptr-node.children node))
         (xptr-node.children node))))

(defMethod node-map-descendants
           ((datum t) (function t))
  nil)

(defMethod node-map-descendants
           ((node element) function)
  ;; nb. this is expressed her specifically for element's, but could well be
  ;; generically expressed for standard-object instances.
  (map nil #'(lambda (child)
               (unless *from-end*
                 (funcall function child))
               (node-map-descendants child function)
               (when *from-end*
                 (funcall function child)))
       (if *from-end*
         (reverse (xptr-node.children node))
         (xptr-node.children node))))

(defMethod node-map-ancestors
           ((node element) function &aux parent)
  ;; nb. this is expressed here specifically for element's, but could well be
  ;; generically expressed for standard-object instances.
  (when (setf parent (xptr-node.parent node))
    (unless *from-end*
      (funcall function parent))
    (node-map-ancestors parent function)
    (when *from-end*
      (funcall function parent))))

(defMethod node-map-ancestors
           ((node attribute) function &aux parent)
  ;; nb. this is expressed here specifically for element's, but could well be
  ;; generically expressed for standard-object instances.
  (when (setf parent (xml-node.context node))
    (unless *from-end*
      (funcall function parent))
    (node-map-ancestors parent function)
    (when *from-end*
      (funcall function parent))))

(defMethod node-map-psiblings
           ((node xml-node) function &aux (siblings (xptr-node.psiblings node)))
  ;; note that for psiblings the order is reversed from siblings
  (dolist (sibling (if *from-end* siblings (nreverse siblings)))
    (funcall function sibling)))

(defMethod node-map-fsiblings
           ((node xml-node) function &aux (siblings (xptr-node.fsiblings node)))
  (dolist (sibling (if *from-end* (nreverse siblings) siblings))
    (funcall function sibling)))


;;;
;;; matching control functions

(defMethod xptr-term-map-matched-nodes
           ((term xptr-term) (node t) (collector t))
  nil)

(defMethod xptr-map-matched-nodes
           ((xptr xptr) (list list) (collector t))
  (dolist (node list) (xptr-map-matched-nodes xptr node collector)))

(defMethod xptr-map-matched-nodes
           ((xptr xptr) (node t) (collector t))
  (xptr-term-map-matched-nodes (xptr.term xptr) node collector))


(defMethod xptr-term-map-matched-nodes
           ((term %xptr::|root|)
            (node xml-node)
            (operator t))
  (typecase (xml-node.context node)
    ((or null document) (funcall operator node))
    (element (xptr-term-map-matched-nodes term (document.element node) operator))
    (t nil)))

(defMethod xptr-term-map-matched-nodes
           ((term %xptr::|origin|)
            (node element)
            (operator t))
  (funcall operator node))


(defMethod xptr-term-map-matched-nodes
           ((term %xptr::|id|)
            (node element)
            (collector function)
            &aux (id (xptr-term.value *xptr-term*)))
  (cond (*document*
         (when (setf node (document.id-element *document* id))
           (funcall collector node)))
        (t
         (flet ((test (datum)
                  (when (string-equal id (xml-node.get-attribute-by-type datum 'id))
                    (funcall collector datum)
                    (return-from xptr-term-map-matched-nodes))))
           (test node)
           (node-map-descendants node #'test)))))

(defMethod xptr-term-map-matched-nodes
           ((*xptr-term* %xptr::|html|) (datum t) (collector function))
  nil)

(defMethod xptr-term-map-matched-nodes
           ((term %xptr::|html|)
            (node element)
            (operator t)
            &aux (name (xptr-term.value term)))
  (flet ((test (element)
           ;; relax package restrictions
           (when (and (string-equal (element.name element) "A")
                      (string-equal (xml-node.get element "NAME") name))
             (funcall operator element))))
    (test node)
    (node-map-descendants node #'test)))

(defMacro with-normalized-quantifier
          ((quantifier term) &rest body)
  `(let ((,quantifier (xptr-term.quantifier ,term))
         (*from-end* nil))
    (if (numberp ,quantifier)
      (if (minusp ,quantifier)
        (setf ,quantifier (abs ,quantifier)
              *from-end* t))
      (if (eq ,quantifier *all-quantifier*)
        (setf *all-selector-p* ,quantifier)))
    ,@body))

(defmethod xptr-term-map-matched-nodes
           ((term %xptr::|child|)
            (node element)
            (operator t))
  (with-normalized-quantifier (quantifier term)
    (node-map-children node
                       #'(lambda (child)
                          (when (xptr-element-match? term child)
                            (cond ((eq quantifier *all-quantifier*)
                                   (funcall operator child))
                                  ((zerop (decf quantifier))
                                   (funcall operator child)
                                   (return-from xptr-term-map-matched-nodes term))))))))
                       
(defmethod xptr-term-map-matched-nodes
           ((term %xptr::|descendant|)
            (node element)
            (operator t))
  (with-normalized-quantifier (quantifier term)
    (node-map-descendants node
                       #'(lambda (child)
                          (when (xptr-element-match? term child)
                            (cond ((eq quantifier *all-quantifier*)
                                   (funcall operator child))
                                  ((zerop (decf quantifier))
                                   (funcall operator child)
                                   (return-from xptr-term-map-matched-nodes term))))))))

(defmethod xptr-term-map-matched-nodes
           ((term %xptr::|ancestor|)
            (node element)
            (operator t))
  (with-normalized-quantifier (quantifier term)
    (node-map-ancestors node
                        #'(lambda (child)
                           (when (xptr-element-match? term child)
                             (cond ((eq quantifier *all-quantifier*)
                                    (funcall operator child))
                                   ((zerop (decf quantifier))
                                    (funcall operator child)
                                    (return-from xptr-term-map-matched-nodes term))))))))

(defmethod xptr-term-map-matched-nodes
           ((term %xptr::|preceding|)
            (node element)
            (operator t))
  (warn "preceding terms NYI."))

(defmethod xptr-term-map-matched-nodes
           ((term %xptr::|following|)
            (node element)
            (operator t))
  (warn "following terms NYI."))

(defmethod xptr-term-map-matched-nodes
           ((term %xptr::|psibling|)
            (node xml-node)
            (operator t))
  (with-normalized-quantifier (quantifier term)
    (node-map-psiblings
     node
     #'(lambda (child)
        (when (xptr-element-match? term child)
          (cond ((eq quantifier *all-quantifier*)
                 (funcall operator child))
                ((zerop (decf quantifier))
                 (funcall operator child)
                 (return-from xptr-term-map-matched-nodes term))))))))

(defmethod xptr-term-map-matched-nodes
           ((term %xptr::|fsibling|)
            (node xml-node)
            (operator t))
  (with-normalized-quantifier (quantifier term)
    (node-map-fsiblings
     node
     #'(lambda (child)
        (when (xptr-element-match? term child)
          (cond ((eq quantifier *all-quantifier*)
                 (funcall operator child))
                ((zerop (decf quantifier))
                 (funcall operator child)
                 (return-from xptr-term-map-matched-nodes term))))))))


(defmethod xptr-term-map-matched-nodes
           ((term %xptr::|span|)
            (node element)
            (operator t))
  nil)

(defmethod xptr-term-map-matched-nodes
           ((term %xptr::|attr|)
            (node element)
            (operator t)
            &aux
            attribute)
  (when (setf attribute (xml-node.get-attribute-if node
                                                   #'(lambda (attr)
                                                       (xptr-element-match? term attr))
                                                   nil))
    (funcall operator attribute)))

(defmethod xptr-term-map-matched-nodes
           ((term %xptr::|string|)
            (node element)
            (operator t))
  (with-normalized-quantifier (quantifier term)
    (let ((string (xptr-term.string term)))
      (node-map-children 
       node
       #'(lambda (child)
           (when (xptr-element-match? string (xml-node.string child))
             (cond ((eq quantifier *all-quantifier*)
                    (funcall operator child))
                   ((zerop (decf quantifier))
                    (funcall operator child)
                    (return-from xptr-term-map-matched-nodes term)))))))))





"<H3>pointer resolution interface</H3>
 <P>
 three high-level functions are provided: xptr-locate, xptr-get, and xptr-set
 <UL>
  <LI>xptr-locate

  <A HREF='/Doc/FUNCTION?NAME=XMLP::XPTR-LOCATE'><CODE>XPTR-LOCATE</CODE</A>
  and
  <LI>
  <A HREF='/Doc/FUNCTION?NAME=XMLP::XPTR-LOCATE-VALUE'><CODE>XPTR-LOCATE-VALUE</CODE</A>
  </UL>
 the former determines the locations in a given graph (instance or dom) which
 are denoted by a given xpointer. it does not, however return the values at
 those locations. it returns the <EM>locations</EM> themselves, in that the
 objects returned can be used as an argument to the functions
 <UL>
  <LI>
  <A HREF='/Doc/FUNCTION?NAME=XMLP::XPTR.VALUE'><CODE>XPTR.VALUE</CODE</A>
  and
  <LI>
  <A HREF='/Doc/FUNCTION?NAME=(CL::SETF XMLP::XPTR.VALUE)'><CODE>(SETF XPTR.VALUE)</CODE</A>
  </UL>
 to either read or write the denoted value.
 "

(defMethod xptr-collect-matched-nodes
           ((xptr xptr) (node xml-node) collector
            &aux (rest (xptr.rest xptr)))
  ;; map the respective term to matched nodes and either iterate on them
  ;; (if there are additional terms) or collect them (if this is the last term)
  (xptr-map-matched-nodes xptr node
                          #'(lambda (matched-node)
                              (if rest
                                (xptr-collect-matched-nodes rest matched-node
                                                            collector)
                                (funcall collector matched-node
                                         (xptr.term xptr))))))
  


(defMethod xptr-accessor
           ((xptr xptr) (node element) &aux result term *all-selector-p*)
  (xptr-collect-matched-nodes xptr node
                              #'(lambda (node last-term)
                                  (if (null term)
                                    (setf term last-term)
                                    (unless (eq term last-term)
                                      (error "term mismatch: ~s; ~s."
                                             term last-term)))
                                  (push node result)))
  (make-instance 'xptr-accessor
    :xptr xptr
    :locations (if *all-selector-p* (nreverse result) (first result))
    :term term))

(defMethod xptr-accessor
           ((xptr xptr) (*document* document))
  (xptr-accessor xptr (document.element *document*)))

(defMethod xptr-accessor
           ((xptr xptr) (node t))
  nil)



;; an element is available either as the string content, the element itself,
;; or the entire element content

(defMethod xptr-node-value
           ((datum element) (as (eql 'string)))
  (xml-node.string datum))

(defMethod xptr-node-value
           ((datum element) (as (eql 'element)))
  datum)

(defMethod xptr-node-value
           ((datum element) (as (eql 'list)))
  (xml-node.content datum))

(defMethod xptr-node-set-value
           ((value string) (datum element) (as (eql 'string)))
  (setf (element.content datum) (list value)))

(defMethod xptr-node-set-value
           ((value element) (datum element) (as (eql 'element)))
  (let* ((parent (xml-node.context datum))
         (content (when parent (xml-node.content parent)))
         (position (position datum content)))
    (when position (setf (elt content position) value))))

(defMethod xptr-node-set-value
           ((value list) (datum element) (as (eql 'list)))
  (setf (element.content datum) value))

;; an attribute is avaiable either as the string content or as
;; the attribute node

(defMethod xptr-node-value
           ((datum attribute) (as (eql 'string)))
  (xml-node.string datum))

(defMethod xptr-node-value
           ((datum attribute) (as (eql 'attribute)))
  datum)

(defMethod xptr-node-set-value
           ((value string) (datum attribute) (as t))
  (setf (attribute.value datum) (list value)))

(defMethod xptr-node-set-value
           ((value attribute) (datum attribute) (as (eql 'attribute)))
  (let* ((parent (xml-node.context datum))
         (content (when parent (xml-node.attributes parent)))
         (position (position datum content)))
    (when position (setf (elt content position) value))))


;; iterating over collections

(defMethod xptr-node-value
           ((list list) as)
  (mapcar #'(lambda (datum) (xptr-node-value datum as)) list))

(defMethod xptr-node-set-value
           (value (list list) as)
  (dolist (datum list) (xptr-node-set-value value datum as)))

(defsetf xptr-node-value xptr-node-set-value)

;;;
;;; interface functions

(defmethod xptr-set-value
           ((value t) (xptr-accessor xptr-accessor) &optional (as 'string))
  (xptr-node-set-value value (xptr-accessor.locations xptr-accessor) as))

(defMethod xptr-value
           ((xptr-accessor xptr-accessor) &optional (as 'string))
  (xptr-node-value (xptr-accessor.locations xptr-accessor) as))

(defSetf xptr-value xptr-set-value)

;;;
;;; access macros

(defMacro with-xptr-strings
          (bindings node &rest body
                    &aux (node-sym (gensym))
                    (ptr-syms (mapcar #'(lambda (b) (gensym (string (first b))))
                                      bindings)))
  `(let ((,node-sym ,node))
     (let ,(mapcar #'(lambda (binding ptr-sym)
                       (destructuring-bind (symbol xptr &optional as) binding
                         (declare (ignore symbol as))
                         (etypecase xptr
                           (xptr t)
                           (string (with-default-namespace *package*
                                     (setf xptr (make-instance 'xptr
                                                  :uri xptr)))))
                         `(,ptr-sym (xptr-accessor ,xptr ,node-sym))))
                   bindings ptr-syms)
       (symbol-macrolet ,(mapcar #'(lambda (binding ptr-sym)
                              (destructuring-bind (symbol xptr &optional (as 'string)) binding
                                (declare (ignore xptr))
                                `(,symbol (xptr-value ,ptr-sym ',as))))
                          bindings ptr-syms)
         ,@body))))

(defMacro with-xptr-elements
          (bindings node &rest body
                    &aux (node-sym (gensym))
                    (ptr-syms (mapcar #'(lambda (b) (gensym (string (first b))))
                                      bindings)))
  `(let ((,node-sym ,node))
     (let ,(mapcar #'(lambda (binding ptr-sym)
                       (destructuring-bind (symbol xptr &optional as) binding
                         (declare (ignore symbol as))
                         `(,ptr-sym (xptr-accessor ,xptr ,node-sym))))
                   bindings ptr-syms)
       (symbol-macrolet ,(mapcar #'(lambda (binding ptr-sym)
                              (destructuring-bind (symbol xptr &optional (as 'element)) binding
                                (declare (ignore xptr))
                                `(,symbol (xptr-value ,ptr-sym ',as))))
                          bindings ptr-syms)
         ,@body))))

(defMacro with-xptrs
          (bindings node &rest body
                    &aux (node-sym (gensym))
                    (ptr-syms (mapcar #'(lambda (b) (gensym (string (first b))))
                                      bindings)))
  `(let ((,node-sym ,node))
     (let ,(mapcar #'(lambda (binding ptr-sym)
                       (destructuring-bind (symbol xptr as) binding
                         (declare (ignore symbol as))
                         `(,ptr-sym (xptr-accessor ,xptr ,node-sym))))
                   bindings ptr-syms)
       (symbol-macrolet ,(mapcar #'(lambda (binding ptr-sym)
                              (destructuring-bind (symbol xptr &optional (as 'element)) binding
                                (declare (ignore xptr))
                                `(,symbol (xptr-value ,ptr-sym ',as))))
                          bindings ptr-syms)
         ,@body))))

#+:CCL
(progn
 (pushnew '(with-xptr-strings . 2) *fred-special-indent-alist* :key #'first)
 (pushnew '(with-xptr-elements . 2) *fred-special-indent-alist* :key #'first)
 (pushnew '(with-xptrs . 2) *fred-special-indent-alist* :key #'first))

"XMLP"

#|

(princ
 (mapcar #'parse-xptr-term '("root()"
                             "origin()"
                             "id(1234)"
                             "html(head)"
                             "attr(xyz)"
                             "child(1,asdf,att1,1234).(2.qwer)"
                             "descendant(1,asdf,att1,1234)"
                             "ancestor(1,asdf,att1,1234)"
                             "preceding(1,asdf,att1,1234)"
                             "following(1,asdf,att1,1234)"
                             "psibling(1,asdf,att1,1234)"
                             "fsibling(1,asdf,att1,1234)")))
(parse-xptr-term "child(1,asdf,att1,1234,attt2,'12345').(2.qwer)" )
                            


(inspect (make-instance 'xptr :uri "asdf/asdf#root().child(1,element)"))
(princ (make-instance 'xptr :uri "asdf/asdf#root().child(1,element)"))
(inspect #!>root().descendant(all,#element).string(1,test) )

;;; the following are equivalent (note the case and package/namespace fineties)

(inspect (xptr-accessor #!>root().child(1,element) 
                        #!<test><element>test<one/><two/></element></test> ))

(inspect (xptr-accessor #!>root().child(1,XMLP:ELEMENT) 
          (dom test
               (dom xmlp:element
                    "test"
                    (dom one
                         (dom element "one")
                         (dom element "two"))))))


;;; the remaining examples all use encoded forms

(inspect (xptr-accessor #!>root().child(1,element).child(all,element)
                        #!<test><element>
                                  test
                                  <element>one</element>
                                  <element>two</element>
                                  </element></test> ))

;; without root defaults to such
(inspect (xptr-accessor #!>child(1,element).(all,element)
                        #!<test><element>
                                  test
                                  <element>one</element>
                                  <element>two</element>
                                  </element></test> ))

(inspect (xptr-accessor #!>child(1,element).child(2,element)
                        #!<test><element>
                                  test
                                  <element>one</element>
                                  <element>two</element>
                                  </element></test> ))

(inspect (xptr-accessor #!>child(1,element).child(1,#string)
                        #!<test><element>
                                  test
                                  <element>one</element>
                                  <element>two</element>
                                  </element></test> ))

(inspect (xptr-accessor #!>root().descendant(all,#element)
                        #!<test><element>
                                  test
                                  <element>one</element>
                                  <element>two</element>
                                  </element></test> ))
(inspect (xptr-accessor #!>root().child(1,element).child(1,element)
                        #!<test><element>
                                  test
                                  <element>one</element>
                                  <element>two</element>
                                  </element></test> ))

(inspect (xptr-accessor #!>root().child(1,element).child(1,element)
                        #!<test><element>
                                  test
                                  <element>one</element>
                                  <element>two</element>
                                  </element></test> ))
(inspect (xptr-accessor #!>root().child(1,test).child(1,element).child(1,one)
                        #!<x><test><element>test<one/><two/></element></test></x>))

;;; watch out for default/null namespaces on attributes 
(inspect (xptr-accessor #!>child(1,element).child(1,element,xml:attr,asdf)
                        #!<test><element>
                                  test
                                  <element xml:attr="qwer">one</element>
                                  <element xml:attr="asdf">two</element>
                                  </element></test> ))

(let ((ptr #!>root().descendant(all,#element).string(1,test) )
      (dom #!<test><element>
                                  testing
                                  <element>one</element>
                                  <element>two</element>
                                  </element></test> ))
  (time (dotimes (x 100) (xptr-accessor  ptr dom))))

(with-xptrs ((test #!>root().child(1,test) list)
             (one #!>root().child(1,test).child(1,element).child(1,one) element))
  #!<x><test><element>test<one>or</one><two/></element></test></x>
  ;; returns the list content of the 'test' element and the first 'one' element
  (print (list test one)))

(with-xptrs ((test #!>root().child(1,test) list)
             (one #!>root().child(1,test).child(1,element).child(all,one) element))
  #!<x><test><element>test<one>or</one><one/></element></test></x>
  ;; returns the list content of the 'test' element and all'one' elements
  (print (list test one)))

 |#
