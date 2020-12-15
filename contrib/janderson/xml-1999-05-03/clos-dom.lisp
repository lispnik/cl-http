;;;;-*- Mode: Lisp; Package: "XML-PARSER"; -*-

"
<DOCUMENTATION>
 <DESCRIPTION>
  translates meta data on clos objects into XML
  </DESCRIPTION>
 <COPYRIGHT HREF='defsystem.lisp|root().descendant(1,COPYRIGHT)'/>
 <CHRONOLOGY>
  <DELTA><DATE>19980615</DATE>
   added support for URL instances
   </DELTA>
  <DELTA><DATE>19980621</DATE>
   DOM-ELEMENT-STYLE eliminated.<BR>
   DOM-ELEMENT now specializes on reified element-type specific classes
   rahter than using EQL identifiers in the identifier name.</DELTA>
  <DELTA><DATE>19980830</DATE>
   corrected package generation; added SYMBOL url; completed function, generic function, and
   symbol generation to produce links from list entries for package generation.
   </DELTA>
  <DELTA><DATE>19980914</DATE>
   modified LISP-DOM:TYPE implmentation to support compound types</DELTA>
  <DELTA><DATE>19981214</DATE>
   class finalization where needed; required by allegro</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
 "

(in-package "XML-PARSER")


;;;
;;; two packages are used here "CL-TYPES" for symbols which identify classes
;;; and types specified in common lisp. "LISP-DOM" for identifiers which are
;;; either non-standard types or other distinguished symbols (for example
;;; slot names)

(defPackage "LISP-DOM"
  (:use)
  (:export "ACCESSOR"
           "ARGLIST"
           "CLASS-NAME"
           "CLASS-SLOTS"
           "CLASSES"
           "DOCUMENTATION"
           "FUNCTIONS"
           "INITFORM"
           "INITARG"
           "INSTANCE-SLOTS"
           "METHODS"
           "NAME"
           "READER"
           "SUBCLASSES"
           "SUPERCLASSES"
           "WRITER"
           "SLOT-DEFINITION" ;not a class in mcl
           "SPECIALIZIERS"
           "TYPE"
           "URL"
           "VARIABLES"))

;;;
;;; if the common lisp types (see cl-types.lisp")  are already present as reified
;;; classes, then redefine the top class to make them elements and export the names.
;;; otherwise, just define the minimum classes. note that, if the package did not
;;; exist when "exports.lisp" loaded, it was created with the minimal symbols.
;;; any types or classes outside of "COMMON-LISP" must be defined as additional
;;; types.

(let (#+:ccl (*warn-if-redefine* nil))
  (cond ((find-class 'cl-types:t nil)
         (cl:defclass cl-types:t (element) ()))
        (t
         ;; a minimal hierarchy by hand. nb. this agrees with mcl 4.2, ymmv.
         (cl:defclass cl-types:t (element) ())
         (cl:defClass cl-types:array (cl-types::t) ())
         (cl:defClass cl-types:class (cl-types::t) ())
         (cl:defClass cl-types:function (cl-types::t) ())
         (cl:defClass cl-types:generic-function (cl-types:function) ())
         (cl:defClass cl-types:method (cl-types::t) ())
         (cl:defClass cl-types:sequence (cl-types::t) ())
         (cl:defClass cl-types:symbol (cl-types::t) ())
         (cl:defClass cl-types:package (cl-types::t) ())
         (cl:defClass cl-types:list (cl-types::sequence) ())
         (cl:defClass cl-types:vector (cl-types::array cl-types::sequence) ())
         (cl:defClass cl-types:string (cl-types::vector) ()))))

(cl:defClass LISP-DOM:documentation (cl-types:string) ())
(cl:defClass LISP-DOM:name (cl-types:symbol) ())
(cl:defClass LISP-DOM:class-name (LISP-DOM:name) ())
(cl:defClass LISP-DOM:arglist (cl-types:list) ())
(cl:defClass LISP-DOM:specializiers (cl-types:list) ())
(cl:defClass LISP-DOM:slot-definition (cl-types:standard-object) ())
(cl:defClass LISP-DOM:type (element) ())


"<H2>DOM Facilities for Object Models</H2>
 <P>
 an object model comprises classes in inheritance and association relations.
 the class objects contain sufficient meta-class information to represent the
 class model themselves.

 <P>
 in order to document the class model, it is mapped into a
 DOM data-structure, which can serve both as an internal database
 and be translated into a representation appropriate for a given viewer.
 a translation is provided which maps the dom elements to html.

 <P>
 for each object, two presentation forms are provided
 <UL>
  <LI>a document for a single object - class, function, etc.<BR>
   this includes the declared documentation and provides links to associated classes,
   data types, methods, etc.
  <LI>a reference to the object presents the object's name as a link.
  </UL>
 <HR>

 <H3>Interface:</H3>
 <P>
 the basic interface implements #'DOM-ELEMENT methods for the four supported
 datatypes: package, class, function, generic-function, and method.
 this, in turn, depends on facilities to generate primitive tags,
 such as anchors and namestrings.

 nb:
 the package for the html style is consistently :HTML. this is because the conventions
 for printing observe the package spec in nested elements. likewise the package for
 the clos dom elements is :LISP or :CL-TYPES. this although i don't follow the example
 'lisp.dtd' here.
 nb^2:
 you're going to need a fairly hefty processor to see speedy results here. it depends on
 how extensive the particular datum instance is, but don't be surprised by the delay.
 "

(defGeneric dom-target
  (datum)
  (:documentation "specifies the target interface context for a given datum"))

(defGeneric dom-namestring
  (datum)
  (:documentation "generates the atomic namestring to use in building a tag"))

(defGeneric dom-element
  (datum form)
  (:documentation
   "transforms the specified datum to a dom element of the specified type
    or form.
    the <I>DATUM</I> may be an element or it may be a STANDARD-OBJECT
    the <I>FORM</I> specifies :DOM, :HTML or a specific element type, which
    may be a presentation type, such as |html|::A.
    for clarity, maintain an identity between keywords used for
    generic forms, and the symbol package for element (content, presentation)
    types. following this convention, there are two forms: :DOM and :HTML,
    and packages of analogous names for individual element names.
    the present examples use only html element types..."))

(defMethod documentation
           ((class built-in-class) &optional type)
  (declare (ignore type))
  nil)

#+CCL
(unless (fboundp 'class-finalized-p) (defun class-finalized-p (x) (declare (ignore x)) t))
#+CCL
(unless (fboundp 'finalize-inheritance) (defun finalize-inheritance (x) x))

(defMethod finalize-if-needed
           ((class class))
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  class)

(defMethod finalize-if-needed
           ((class t))
  class)

#+:ALLEGRO
(progn
  ;; without this 4.3.2/nt catches a segmentation fault
  (defMethod class-class-slots
             ((class class))
    (remove :class (class-slots class) :key #'slot-definition-allocation
            :test-not #'eq))
  (defMethod class-instance-slots
             ((class class))
    (remove :instance (class-slots class) :key #'slot-definition-allocation
            :test-not #'eq))
  (defun function-name (x) (xref::object-to-function-name x)))


"<H3>Parameters</H3>
 "

(defParameter *dom-inactive-classes*
  '("CLASS" "STANDARD-CLASS" "FUNCTION" "STANDARD-OBJECT" "T")
  "specifies names of classes which cannot be the target of a link")

(defParameter *sort-trim-characters* "*/."
  "used to strip decoration from symbol names when sorting them")


" generation methods:
 <P>
 a given object should appear either as a detailed description or as a
 cross-reference, depending on the context.
 for example, a class appears in its own class frame as a detailed
 description including slots, sub-classes, and super-classes.
 it can also appear in another class's description as a cross-reference.
 to effect this behaviour, one of the two functions #'dom-element
 is passed an HTML identifier as appropriate for the context.
 "

(defMethod dom-namestring ((datum null)) "")

(defMethod dom-namestring ((datum t)) (write-to-string datum))

(defMethod dom-namestring ((datum pathname)) (namestring datum))


(defun html (element)
  (dom-element element :html))

(defMethod dom-element
           ((datum list) (form t))
  (mapcar #'(lambda (datum) (dom-element datum form)) datum))

(defMethod dom-element
           ((datum t) (form (eql :html)))
  (dom-element (dom-element datum :dom) :html))

(defMethod dom-element
           ((datum element) (form (eql '|html|::a)))
  (let-elements-content ((LISP-DOM::name "NONAME")) datum
    (dom (|html|::A '|html|::HREF (format nil "~s?NAME=~a" (xml-datum.type  datum) LISP-DOM::name))
         (write-to-string datum))))

(defMethod dom-element
           ((datum xml-node) (to t))
  "the default method for arbitrary objects which are already serializable
   is to leave them alone."
  datum)


;;;
;;; dom-specific access functions for object attributes / properties

(defMethod dom-method-specializers
           ((datum method))
  (mapcar #'(lambda (class)
              (cond ((typep class 'class)
                     (dom LISP-DOM::class (dom LISP-DOM::name (dom-namestring class))))
                    (t
                     (dom-namestring class))))
          (method-specializers datum)))

(defun dom-arglist
           (datum)
  (mapcar #'write-to-string (arglist datum)))



#| PACKAGE datatype dom model
 |#

(defMethod dom-namestring ((datum package))
  (package-name datum))

(defMethod dom-target ((datum package)) "PACKAGE")

(defMethod dom-element
           ((datum package) (form (eql :DOM))
            &aux class-tags function-tags variable-tags
            class)
  (flet ((sort-key (symbol)
           (string-trim *sort-trim-characters* (string symbol))))
    (do-symbols (symbol datum)
      (when (eq (symbol-package symbol) datum)
        (when (setf class (find-class symbol nil))
          (push (cons (sort-key symbol) (dom-element class :DOM))
                class-tags))
        (when (and (fboundp symbol) (not (macro-function symbol)))
          (push (cons (sort-key symbol) (dom-element (symbol-function symbol) :DOM))
                function-tags))
        (when (boundp symbol)
          (push (cons (sort-key symbol) (dom-element symbol :DOM))
                variable-tags))))
    (dom CL-TYPES:PACKAGE
         (dom LISP-DOM:NAME (dom-namestring datum))
         (dom* LISP-DOM:VARIABLES
              (mapcar #'rest
                      (sort variable-tags #'string-lessp :key #'first)))
         (dom* LISP-DOM:CLASSES
              (mapcar #'rest
                      (sort class-tags #'string-lessp :key #'first)))
         (dom* LISP-DOM:FUNCTIONS
              (mapcar #'rest
                      (sort function-tags #'string-lessp :key #'first))))))

(defMethod dom-element
           ((datum cl-types:package) (form (eql :HTML)))
  (let-elements-content (LISP-DOM:name) datum
    (let-elements-content-list (LISP-DOM:variables LISP-DOM:classes LISP-DOM:functions) datum
      (dom |html|::HTML
           (dom |html|::HEAD (dom-element datum '|html|::TITLE))
           (dom |html|::BODY
                (dom |html|::H3 LISP-DOM:name " (Package)")
                (dom |html|::HR)
                (dom |html|::UL
                     (dom |html|::LI (dom (|html|::A '|html|::HREF "#VARIABLES")
                                        (format nil "~d variables" (length LISP-DOM:variables))))
                     (dom |html|::LI (dom (|html|::A '|html|::HREF "#CLASSES")
                                        (format nil "~d classes" (length LISP-DOM:classes))))
                     (dom |html|::LI (dom (|html|::A '|html|::HREF "#FUNCTIONS")
                                        (format nil "~d functions" (length LISP-DOM:functions)))))
                (dom (|html|::HR '|html|::SIZE "1"))
                (dom |html|::DL
                     (dom |html|::DT (dom (|html|::A '|html|::NAME "VARIABLES") "Variables"))
                     (dom |html|::DD
                           (dom* |html|::UL
                                 (mapcar #'(lambda (element) (dom-element element '|html|::LI))
                                         LISP-DOM:variables)))
                     (dom |html|::DT (dom (|html|::A '|html|::NAME "CLASSES") "Classes"))
                     (dom |html|::DD
                           (dom* |html|::UL
                                 (mapcar #'(lambda (element) (dom-element element '|html|::LI))
                                   LISP-DOM:classes)))
                     (dom |html|::DT (dom (|html|::A '|html|::NAME "FUNCTIONS") "Functions"))
                     (dom |html|::DD
                           (dom* |html|::UL
                                 (mapcar #'(lambda (element) (dom-element element '|html|::LI))
                                   LISP-DOM:functions)))))))))

(defMethod dom-element
           ((datum cl-types:package) (form (eql '|html|::A)))
  (let-elements-content ((LISP-DOM:name "NONAME")) datum
    (dom (|html|::A '|html|::HREF (format nil "PACKAGE?NAME=~a" LISP-DOM::name))
         LISP-DOM:name)))


(defMethod dom-element
           ((datum cl-types:package) (form (eql '|html|::TITLE)))
  (let-elements-content ((LISP-DOM:name "?")) datum
    (dom |html|::TITLE LISP-DOM:name " (Package)")))




#| CLASS data type dom model
 <P>
 the dom form distinguishes standard classes from all other forms of class. the former will
 have slots descriptions and documentation while the latter may not.
 |#

(defMethod dom-namestring ((datum class)
                           &aux (name  (class-name datum)))
  (format nil "~a::~a" (package-name (symbol-package name)) name))


(defMethod dom-target ((datum class)) "CLASS")

(defMethod dom-element
           ((type null) (form (eql 'LISP-DOM:type)))
  (dom LISP-DOM:TYPE))

(defMethod dom-element
           ((type cons) (form (eql 'LISP-DOM:type))
            &aux new-content)
  (dolist (element (rest type))
    (push " " new-content)
    (push (dom-element element 'LISP-DOM:type) new-content))
  (dom* LISP-DOM:TYPE
        `("("
          ,(string (first type))
          ,@(nreverse new-content)
          ")")))

(defMethod dom-element
           ((type symbol) (form (eql 'LISP-DOM:type)))
  (let ((type-string (symbol-name type))
        (reference-type))
    (if (or (find-class type nil)
            (<= (length type-string) 4)
            (string/= "LIST" type-string :start2 (- (length type-string) 4)))
      (setf reference-type type)
      (setf type-string (subseq type-string 0 (- (length type-string) 5))
            reference-type (intern type-string (symbol-package type))))
    (cond ((subtypep reference-type 'standard-object)
           (dom (LISP-DOM:TYPE '|html|::HREF
                           (format nil "CLASS?NAME=~a::~a"
                                   (package-name (symbol-package type))
                                   type-string))
                (dom-namestring type)))
          (t
           (dom LISP-DOM:TYPE (dom-namestring type))))))


(defMethod dom-element
           ((datum standard-class) (form (eql :dom)))
  (finalize-if-needed datum)
  (dom CL-TYPES:CLASS
       (dom LISP-DOM:NAME (dom-namestring datum))
       (dom LISP-DOM:DOCUMENTATION (or (documentation datum)) "")
       (dom* LISP-DOM:CLASS-SLOTS
             (mapcar #'(lambda (slot)
                         (dom LISP-DOM:SLOT-DEFINITION
                              ; where is the documentation stored?
                              (dom LISP-DOM:NAME (dom-namestring (slot-definition-name slot)))
                              (dom-element (slot-definition-type slot)
                                           'LISP-DOM:type)))
                     (class-instance-slots datum)))
       (dom* LISP-DOM:INSTANCE-SLOTS
             (mapcar #'(lambda (slot)
                         (dom LISP-DOM::SLOT-DEFINITION
                              ; documentation
                              (dom LISP-DOM:NAME (dom-namestring (slot-definition-name slot)))
                              (dom-element (slot-definition-type slot)
                                           'LISP-DOM:type)))
                     (class-class-slots datum)))
       (dom* LISP-DOM:SUPERCLASSES
             (mapcar #'(lambda (class)
                         (dom CL-TYPES:CLASS (dom LISP-DOM::name (dom-namestring class))))
                     (class-direct-superclasses datum)))
       (dom* LISP-DOM:SUBCLASSES
             (mapcar #'(lambda (class)
                         (dom CL-TYPES:CLASS (dom LISP-DOM::name (dom-namestring class))))
                     (class-direct-subclasses datum)))
       (dom* LISP-DOM:METHODS
             (mapcar #'(lambda (method)
                         (dom CL-TYPES:METHOD (dom LISP-DOM::name (dom-namestring method))))
                     (specializer-direct-methods datum)))))

(defMethod dom-element
           ((datum class) (form (eql :DOM)))
  (finalize-if-needed datum)
  (dom CL-TYPES:CLASS
       (dom LISP-DOM:NAME (dom-namestring datum))
       (dom* LISP-DOM:SUPERCLASSES
             (mapcar #'(lambda (class)
                         (dom CL-TYPES:CLASS (dom LISP-DOM:NAME (dom-namestring class))))
                     (class-direct-superclasses datum)))
       (dom* LISP-DOM:SUBCLASSES
             (mapcar #'(lambda (class)
                         (dom CL-TYPES:CLASS (dom LISP-DOM:NAME (dom-namestring class))))
                     (class-direct-subclasses datum)))
       (dom* LISP-DOM:METHODS
             (mapcar #'(lambda (method)
                         (dom CL-TYPES:METHOD (dom LISP-DOM:NAME (dom-namestring method))))
                     (specializer-direct-methods datum)))))

(defMethod dom-element
           ((datum cl-types:class) (form (eql :HTML)))
  (let-elements-content (LISP-DOM:name LISP-DOM:documentation) datum
    (let-elements (LISP-DOM:superclasses LISP-DOM:subclasses LISP-DOM:methods
                   LISP-DOM:class-slots LISP-DOM:instance-slots)
                  datum
      (dom |html|::HTML
           (dom |html|::HEAD (dom-element datum '|html|::TITLE))
           (dom |html|::BODY
                (dom |html|::H3 (dom |html|::B LISP-DOM:name) " (Class)")
                (dom |html|::HR)
                (dom |html|::P)
                (make-instance 'xmlp::serialized-data :content LISP-DOM:documentation)
                (dom |html|::HR)
                (dom |html|::DL
                     (dom |html|::DT "Superclasses:")
                     (dom* |html|::DD
                           (apply #'append
                                  (mapcar #'(lambda (class)
                                              (list (dom-element class '|html|::A)
                                                    (dom |html|::BR)))
                                         (element.content LISP-DOM:superclasses))))
                     (dom |html|::DT "Subclasses:")
                     (dom* |html|::DD
                           (apply #'append
                                  (mapcar #'(lambda (class)
                                              (list (dom-element class '|html|::A)
                                                    (dom |html|::BR)))
                                         (element.content LISP-DOM:subclasses)))))
                (dom |html|::P)
                (dom |html|::HR)
                (dom |html|::DL
                     (dom |html|::DT "Class Slots:")
                     (dom |html|::DD
                          (dom* |html|::TABLE
                                (dom |html|::TR
                                     (dom (|html|::TH '|html|::WIDTH "125") "name")
                                     (dom (|html|::TH '|html|::WIDTH "75") "type")
                                     (dom (|html|::TH '|html|::WIDTH "200") "documentation")
                                     (dom |html|::TH "accessors"))
                                (mapcar #'(lambda (slot) (dom-element slot '|html|::TR))
                                        (element.content LISP-DOM:class-slots))))
                     (dom |html|::DT "Instance Slots:")
                     (dom |html|::DD
                          (dom* |html|::TABLE
                                (dom |html|::TR
                                     (dom (|html|::TH '|html|::WIDTH "125") "name")
                                     (dom (|html|::TH '|html|::WIDTH "75") "type")
                                     (dom (|html|::TH '|html|::WIDTH "200") "documentation")
                                     (dom |html|::TH "accessors"))
                                (mapcar #'(lambda (slot) (dom-element slot '|html|::TR))
                                        (element.content LISP-DOM::instance-slots)))))
                (dom |html|::P)
                (dom |html|::HR)
                (dom |html|::DL
                     (dom |html|::DT "Methods:")
                     (dom |html|::DD
                          (dom* |html|::TABLE
                                (dom |html|::TR
                                     (dom (|html|::TH '|html|::WIDTH "125") "name")
                                     (dom (|html|::TH '|html|::WIDTH "200") "documentation")
                                     (dom |html|::TH "specializers"))
                                (mapcar #'(lambda (method) (dom-element method '|html|::TR))
                                        (element.content LISP-DOM::methods)))))
                (dom |html|::HR))))))

(defMethod dom-element
           ((datum cl-types:class) (form (eql '|html|::A)))
  ;; don't link to classes which are excluded
  (let-elements-content ((LISP-DOM::name "NONAME")) datum
    (let* ((colon (position #\: LISP-DOM::name :from-end t))
           (start (if colon (1+ colon) 0)))
      (if (find-if #'(lambda (excluded) (string-equal excluded LISP-DOM::name :start2 start))
                   *dom-inactive-classes*)
        LISP-DOM::name
        (dom (|html|::A '|html|::HREF (format nil "CLASS?NAME=~a" LISP-DOM::name))
             LISP-DOM::name)))))

(defMethod dom-element
           ((datum cl-types:class) (form (eql '|html|::LI)))
  (dom |html|::LI (dom-element datum '|html|::A)))

(defMethod dom-element
           ((datum cl-types:class) (form (eql '|html|::TITLE)))
  (let-elements-content ((LISP-DOM::name "?")) datum
    (dom |html|::TITLE LISP-DOM::name " (Class)")))

(defMethod dom-element
           ((datum cl-types:class) (form (eql '|html|::TR)))
  (let-elements-content (LISP-DOM:name) datum
    (dom |html|::TR (dom |html|::TD LISP-DOM:name))))

(defMethod dom-element
           ((datum LISP-DOM:type) (form (eql '|html|::a)))
  (dom* (|html|::A '|html|::HREF (xml-node.get datum '|html|::HREF ""))
        (element.content datum)))

(defMethod dom-element
           ((datum LISP-DOM:type) (form (eql :HTML)))
  (if (xml-node.get datum '|html|::HREF)
    (dom-element datum '|html|::A)
    (dom* |html|::CODE
          (mapcar #'(lambda (e)
                      (typecase e
                        (element (dom-element e :HTML))
                        (string e)
                        (t (warn "unexpected type element content: ~s." e)
                           (write-to-string e))))
                  (element.content datum)))))


(defMethod dom-element
           ((datum LISP-DOM:slot-definition) (form (eql '|html|::TR)))
  (let-elements (LISP-DOM:type) datum
    (let-elements-content (LISP-DOM:name LISP-DOM:documentation
                           LISP-DOM:initform LISP-DOM:initarg LISP-DOM::reader LISP-DOM:writer
                           LISP-DOM:accessor)
                          datum
    (dom |html|::TR
         (cond ((and LISP-DOM:initform LISP-DOM::initarg)
                (dom |html|::TD "((" LISP-DOM::name LISP-DOM::initarg ")" LISP-DOM:initform ")"))
               (LISP-DOM:initform
                (dom |html|::TD "(" LISP-DOM:name LISP-DOM::initarg LISP-DOM:initform ")"))
               (LISP-DOM:initarg
                (dom |html|::TD "((" LISP-DOM:name LISP-DOM:initarg "))"))
               (t
                (dom |html|::TD LISP-DOM:name)))
         (dom |html|::TD (dom-element LISP-DOM:type :HTML))
         (dom |html|::TD (or LISP-DOM:documentation ""))
         (dom |html|::TD
              (format nil "~@[reader: ~a ~]~@[writer: ~a ~]~@[accessor: ~a ~]"
                      LISP-DOM::reader LISP-DOM:writer LISP-DOM:accessor))))))


#| function definitions:

 the form for a reference from a class or from the package yields the name and
 signature in an anchor.
 the form for the definition itself also includes the documentation string,
 the callers and the function references. in order to limit the amount of
 disk space, these descriptions are not writte to individual files.
 there are instead colated alphabetically.

(let ((*print-readably* t))
   (pprint (dom-element (dom-element #'+ :dom) :html)))
 |#

(defMethod dom-target ((datum function)) "FUNCTION")


(defMethod dom-namestring ((datum function))
  (let ((name (function-name datum)))
    (typecase name
      (cons (format nil "(~{~s~^_~})" name))
      (symbol (symbol-name name))
      (t nil))))

(defMethod dom-element
           ((datum function) (form (eql :dom))
            &aux (name (function-name datum))
                 (documentation (documentation name 'function)))
  (dom* cl-types::FUNCTION
       (dom LISP-DOM:name (dom-namestring datum))
       (dom* LISP-DOM:arglist (dom-arglist datum))
       (when documentation (list (dom LISP-DOM:documentation documentation)))))

(defMethod dom-element
           ((datum cl-types:function) (form (eql '|html|::A)))
  (let-elements-content ((LISP-DOM:name "-NONAME-")) datum
    (dom (|html|::A '|html|::HREF (format nil "FUNCTION?NAME=~a" LISP-DOM::name))
         LISP-DOM:name)))


(defMethod dom-element
           ((datum cl-types:function) (form (eql '|html|::TITLE)))
  (let-elements-content ((LISP-DOM:name "?")) datum
    (dom |html|::TITLE LISP-DOM:name " (Function)")))

(defMethod dom-element
           ((datum cl-types:function) (form (eql :html)))
  (let-elements-content (LISP-DOM:name (LISP-DOM:documentation "")) datum
    (let-elements (LISP-DOM::arglist) datum
      (dom |html|::HTML
           (dom |html|::HEAD (dom-element datum '|html|::TITLE))
           (dom |html|::BODY
                (dom |html|::DL
                     (dom |html|::DT LISP-DOM:name " (FUNCTION)" (dom |html|::HR))
                     (dom |html|::DD
                          (format nil "~a " (element.content LISP-DOM::arglist))
                          (dom |html|::HR)
                          LISP-DOM::documentation)))))))

(defMethod dom-element
           ((datum cl-types:FUNCTION) (form (eql '|html|::LI)))
  (dom |html|::LI (dom-element datum '|html|::A)))


#| function method dom model

(let ((*print-readably* t)
      (method (first (compute-applicable-methods #'dom-namestring (list "")))))
   (pprint (dom-element (dom-element method :dom) :html)))
 |#

(defMethod dom-namestring ((datum method))
  (dom-namestring (method-generic-function datum)))

(defMethod dom-target ((datum method)) "FUNCTION")


(defMethod dom-element
           ((datum method) (form (eql :DOM))
            &aux (documentation (documentation datum)))
  (dom* CL-TYPES:METHOD
        (dom LISP-DOM::NAME (dom-namestring datum))
        (dom* LISP-DOM::ARGLIST (dom-arglist datum))
        (dom* LISP-DOM::SPECIALIZERS (dom-method-specializers datum))
        (when documentation
          (list (dom LISP-DOM::DOCUMENTATION documentation)))))

(defMethod dom-element
           ((datum cl-types:method) (form (eql :HTML)))
  (let-elements-content (LISP-DOM::name (LISP-DOM::documentation "")) datum
    (let-elements (LISP-DOM::specializers LISP-DOM::arglist) datum
      (dom |html|::HTML
           (dom |html|::HEAD (dom-element datum '|html|::TITLE))
           (dom |html|::BODY
                (dom |html|::DL
                     (dom |html|::DT
                          (dom |html|::B LISP-DOM::name) " (METHOD)")
                     (dom |html|::DD
                          (dom |html|::HR)
                          (format nil "~a " (element.content LISP-DOM::arglist))
                          (dom |html|::BR)
                          (format nil "~a " (element.content LISP-DOM::specializers))
                          (dom |html|::HR)
                          LISP-DOM::documentation)))))))

(defMethod dom-element
           ((datum cl-types:method) (form (eql '|html|::A)))
  (let-elements-content ((LISP-DOM::name "NONAME") LISP-DOM::qualifiers LISP-DOM::specializers) datum
    (dom (|html|::A '|html|::HREF
                  (format nil "METHOD?NAME=(~a%20~@[~{~a~^%20~}~]~{~a~^%20~})"
                          LISP-DOM::name LISP-DOM::qualifiers LISP-DOM::specializers))
         LISP-DOM::name)))

(defMethod dom-element
           ((datum cl-types:method) (form (eql '|html|::TITLE)))
  (let-elements-content ((LISP-DOM::name "?")) datum
    (dom |html|::TITLE LISP-DOM::name " (Method)")))

(defMethod dom-element
           ((datum cl-types:method) (form (eql '|html|::TR)))
  (let-elements-content (LISP-DOM::name LISP-DOM::documentation LISP-DOM::specializers) datum
   (dom |html|::TR
        (dom |html|::TD LISP-DOM::name)
        (dom |html|::TD (or LISP-DOM::documentation ""))
        (dom* |html|::TD (when LISP-DOM::specializers
                         (apply #'append
                                (mapcar #'(lambda (specializer)
                                            (list (dom-element specializer '|html|::a)
                                                  (dom |html|::br)))
                                        LISP-DOM::specializers)))))))

(defMethod dom-element
           ((datum cl-types:method) (form (eql '|html|::LI)))
  (let-elements-content (LISP-DOM::name) datum
    (dom* |html|::LI LISP-DOM::name
          (when-elements (LISP-DOM::specializers) datum
            (append '("(") 
                    (mapcar #'(lambda (spec)
                                (if (stringp spec)
                                  spec
                                  (dom-element spec '|html|::A)))
                            (element.content LISP-DOM::specializers))
                    '(")"))))))

(defMethod dom-element
           ((datum cl-types:method) (form (eql '|html|::DT)))
  (let-elements-content (LISP-DOM::name) datum
    (dom* |html|::DT
          (when-elements (LISP-DOM::specializers) datum
            (append '("(") 
                    (mapcar #'(lambda (spec)
                                (if (stringp spec)
                                  spec
                                  (dom-element spec '|html|::A)))
                            (element.content LISP-DOM::specializers))
                    '(")"))))))

(defMethod dom-element
           ((datum cl-types:method) (form (eql '|html|::DD)))
  (let-elements-content (LISP-DOM::name) datum
    (dom* |html|::DD
          (when-elements (LISP-DOM::documentation) datum
            (element.content LISP-DOM::documentation)))))




"<H3>generic function dom forms</H3>
 <P>
 documentation and a liste of methods
"

(defMethod dom-element
           ((datum generic-function) (form (eql :DOM))
            &aux
            (documentation (documentation (function-name datum) 'function))
            (precedence #+:ccl (ccl::%gf-precedence-list datum)
                        #-:ccl nil)
            (methods (copy-list (generic-function-methods datum))))
  "generate a generic function element comprising the name, argument list,
   (optionally) the documentation, and the method set."
  (flet ((method-preceeds (sl1 sl2 &aux c1 c2)
           (flet ((test-specializer (i)
                    (when (and (typep (setf c1 (nth i sl1)) 'class)
                               (typep (setf c2 (nth i sl2)) 'class))
                      ;; hope no finalization is needed here... ?
                      (subtypep (class-name c2) (class-name c1)))))
             (if precedence
               (dolist (i precedence) (test-specializer i))
               (dotimes (i (length sl1)) (test-specializer i))))))
    (setf methods (sort methods #'method-preceeds :key #'method-specializers))
    (dom* CL-TYPES:GENERIC-FUNCTION
          (dom LISP-DOM:NAME (dom-namestring datum))
          (dom* LISP-DOM:ARGLIST (dom-arglist datum))
          (append (when documentation (list (dom LISP-DOM:DOCUMENTATION documentation)))
                  (mapcar #'(lambda (method) (dom-element method :dom))
                          methods)))))

(defMethod dom-element
           ((datum CL-TYPES:GENERIC-FUNCTION) (form (eql :HTML)))
  (let-elements-content (LISP-DOM:name (LISP-DOM:documentation "")) datum
   (let-elements (LISP-DOM:arglist) datum
    (let-duplicate-elements (((methods cl-types:method))) datum
     (dom |html|::HTML
          (dom |html|::HEAD (dom-element datum '|html|::TITLE))
          (dom |html|::BODY
               (dom |html|::DL
                    (dom |html|::DT (dom |html|::B LISP-DOM::name) " (GENERIC-FUNCTION)" (dom |html|::hr))
                    (dom |html|::DD
                         "arglist:"
                         (dom |html|::BR)
                         (dom* |html|::NOBREAK
                               (mapcar #'(lambda (arg) (dom |html|::i arg))
                                       (element.content LISP-DOM::arglist)))
                         (dom |html|::HR)
                         LISP-DOM:documentation
                         (dom |html|::HR)
                         "methods:"
                         (dom* |html|::DL
                               (apply #'append
                                      (mapcar #'(lambda (method)
                                                  (list (dom-element method '|html|::DT)
                                                        (dom-element method '|html|::DD)))
                                              methods)))))))))))

(defMethod dom-element
           ((datum CL-TYPES:GENERIC-FUNCTION) (form (eql '|html|::TITLE)))
  (let-elements-content ((LISP-DOM::name "?")) datum
    (dom |html|::TITLE LISP-DOM::name " (Generic Function)")))

(defMethod dom-element
           ((datum cl-types:GENERIC-FUNCTION) (form (eql '|html|::A)))
  (let-elements-content ((LISP-DOM:name "?")) datum
    (dom (|html|::A '|html|::HREF (format nil "FUNCTION?NAME=~a" LISP-DOM::name))
         LISP-DOM::name)))

(defMethod dom-element
           ((datum cl-types:GENERIC-FUNCTION) (form (eql '|html|::LI)))
  (dom |html|::LI (dom-element datum '|html|::A)))



"<H3>variable definitions:</H3>
 <P>
 symbols generate either a reference or a single file with
 the documentation from their definition.
"

(defMethod dom-target ((datum symbol)) "VARIABLE")

(defMethod dom-namestring ((DATUM symbol))
   (string datum))

(defMethod dom-element
           ((datum symbol) (form (eql :DOM))
            &aux (documentation (documentation datum 'variable)))
  (dom* CL-TYPES:SYMBOL
       (dom LISP-DOM:NAME (dom-namestring datum))
       (when documentation
         (list (dom LISP-DOM:DOCUMENTATION documentation)))))

(defMethod dom-element
           ((datum CL-TYPES:SYMBOL) (form (eql :HTML)))
  (let-elements-content (LISP-DOM:name (LISP-DOM:documentation "")) datum
    (dom |html|::HTML
         (dom |html|::HEAD
              (dom-element datum '|html|::TITLE))
         (dom |html|::BODY
              (dom |html|::DL
                   (dom |html|::DT LISP-DOM:name) "(SYMBOL)"
                   (dom |html|::DD
                        (dom |html|::HR)
                        LISP-DOM:documentation))))))

(defMethod dom-element
           ((datum CL-TYPES:SYMBOL) (form (eql '|html|::TITLE)))
  (let-elements-content ((LISP-DOM:name "?")) datum
    (dom |html|::TITLE "Variable " LISP-DOM:name)))

(defMethod dom-element
           ((datum cl-types:symbol) (form (eql '|html|::A)))
  ;; don't link to classes which are excluded
  (let-elements-content ((LISP-DOM:name "?")) datum
    (dom (|html|::A '|html|::HREF (format nil "SYMBOL?NAME=~a" LISP-DOM::name))
         LISP-DOM::name)))

(defMethod dom-element
           ((datum cl-types:symbol) (form (eql '|html|::LI)))
  (dom |html|::LI (dom-element datum '|html|::A)))




"
 <HR>
 <H3>URL data type dom model</H3>
 <P>
 dom generation and html support for document and for list item.
 <P>
 the export and defclass are included here in order to illustrate all
 necessary definitions in one place.
 "
#+:HTTP
(progn

(export 'LISP-DOM::URL "LISP")

(cl:defClass LISP-DOM:URL (CL-TYPES:STANDARD-OBJECT) ())

(defMethod dom-namestring ((datum url:url))
  (format nil "~a:~a" (url:scheme datum) (url:relative-name-string datum)))

(defMethod dom-target ((datum url:url)) "URL")

(defMethod dom-element
           ((datum url:url) (form (eql :dom)))
  (dom LISP-DOM:URL
       (dom LISP-DOM:NAME (dom-namestring datum))
       (dom LISP-DOM:DOCUMENTATION (or (documentation datum) ""))))

(defMethod dom-element
           ((datum LISP-DOM:URL) (form (eql :html)))
  (let-elements-content (LISP-DOM:name LISP-DOM:documentation) datum
    (dom |html|::HTML
         (dom |html|::HEAD (dom-element datum '|html|::TITLE))
         (dom |html|::BODY
              (dom |html|::H3 (dom |html|::B LISP-DOM:name) " (URL)")
              (dom |html|::HR)
              (dom |html|::P)
              LISP-DOM:documentation
              (dom |html|::HR)))))

(defMethod dom-element
           ((datum LISP-DOM:URL) (form (eql '|html|::TITLE)))
  (let-elements-content ((LISP-DOM:NAME "?")) datum
    (dom |html|::TITLE LISP-DOM::NAME " (URL)")))

)

"XMLP"


#|

(let ((*print-readably* t))
   (pprint (dom-element (dom-element #'view-size :dom) :html)))

(let ((*print-readably* t)
      (w (make-instance 'fred-window)))
   (pprint (dom-element (dom-element #'dom-element :dom) :html) w))

(let ((*print-readably* t))
   (print (dom-element (dom-element '*package* :dom) :html)))
 |#
