;;; -*- package: ("XML-PARSER") -*- 

(in-package :xml-parser)

;; see xml-test.lisp for examples


(defClass xml-record-class (mop:mop-standard-class)
  ((record-descriptor
    :initarg :record-descriptor
    :type ccl::record-descriptor
    :accessor class.record-descriptor)
   (element-descriptor
    :initarg :element-descriptor
    :type vector
    :accessor class.element-descriptor)
   (dtd-element :initarg :dtd-element :accessor class.dtd-element))
  (:documentation
   "a <CODE>xml-record-class</CODE> wraps a records with the behaviour of a
    <CODE>xml-element</CODE>. it binds specifications for a record and translates
    them into fiels access methods. it binds a <CODE>xml-element</CODE> prototype for
    each field to use as content elements to map access to the record."))

(defMethod mop:canonicalize-defclass-option
           ((key (eql :record-definition)) &rest args)
  `(:record-definition ',args))
(defMethod mop:canonicalize-defclass-option
           ((key (eql :dtd-element)) &rest args)
  `(:dtd-element (dtd-element ',(first args))))


(defClass xml-record-field (xml-element)
  ((content :reader xml-record-field.get-content
            :writer xml-record-field.set-content)
   (reader :initarg :reader :accessor xml-record-field.reader)
   (writer :initarg :writer :accessor xml-record-field.writer)))

(defClass xml-record (xml-element)
  ((record
    :accessor xml-record.record
    :initarg :record
    :type macptr
    :documentation
    "<CODE>XML-RECORD.RECORD</CODE> binds the primitive record to which
     which the instances is to act as the interface.")))

(defMethod xml-record-field-class
           ((node xml-record))
  'xml-record-field)

;; first the interpretive interface: works by searching for the field by name
;; and using the parameters closed in the field reader/writer function to
;; perform the record slot access

(defMethod xml-element.content
           ((node xml-record-field)
            &aux (context (xml-element.context node))
                 (record (when context (xml-record.record context))))
  (when record
    (funcall (xml-record-field.reader node) record)))

(defMethod (setf xml-element.content)
           ((datum t) (node xml-record-field)
            &aux (record (xml-record.record (xml-element.context node))))
  (when record
    (funcall (xml-record-field.writer node) datum record))
  datum)

(defMethod xml-record-field.record
           ((element xml-record-field) &aux context)
  (when (setf context (xml-element.context element))
    (xml-record.record context)))


(defMethod xml-element.field-content
           ((element xml-record) (field symbol))
  (setf field (find field (xml-element.content element) :key #'xml-element.name))
  (when field (xml-element.content field)))

(defMethod (setf xml-element.field-content)
           ((datum t) (element xml-record) (field symbol))
  (setf field (find field (xml-element.content element) :key #'xml-element.name))
  (when field (setf (xml-element.content field) datum)))

(defMethod xml-record-class.record
           ((class xml-record-class)
            &aux (record-descriptor (class.record-descriptor class)))
  (ccl::%new-gcable-ptr (ccl::record-descriptor-length record-descriptor)))

(defMethod make-instance
           ((class xml-record-class) &rest initargs
            &key (record (xml-record-class.record class))
            &aux (dtd-element (class.dtd-element class)))
  (apply #'call-next-method class
         :content (map 'list
                       #'(lambda (node)
                           (copy-instance node))
                       (class.element-descriptor class))
         :declaration dtd-element
         :record record
         initargs))


(defMethod initialize-instance
           ((self xml-record) &rest initargs
            &key (name (type-of self))
                 (dtd-element (ignore-errors (dtd-element name))))
  (apply #'call-next-method self
        :dtd-element dtd-element
        initargs))

(defMethod initialize-instance
           ((self xml-record-field) &rest initargs
            &key (name (type-of self)) content
                 (dtd-element (ignore-errors (dtd-element name))))
  (apply #'call-next-method self
        :dtd-element dtd-element
        initargs)
  (when content
    (setf (xml-element.content self) content)))


#+:ccl
(defun define-record (name type def &optional dont-know)
  (ccl::define-record name type def dont-know))

(defMethod verify-record-and-dtd
           ((descriptor ccl::record-descriptor) (element dtd-element))
  t)

(defMethod dtd-element.record-description
           ((element dtd-element)
            &aux (model (dtd-element-declaration.model element))
                 (elements (when (typep model 'dtd-model-group)
                             (dtd-model.content model))))
  (unless elements
    (error "no model present in ~s." (dtd-element.name element)))
  (mapcar #'(lambda (e &aux (name (dtd-element.name e))
                       (att (find :type (dtd-element-declaration.attdefs e)
                                  :key #'dtd-attdef.name )))
              (unless att (error "no type for ~s." name))
              (list (intern (string name) :keyword)
                     (dtd-attdef.default att)))
          elements))

(defMethod dtd-element.record-description
           ((element symbol))
  (dtd-element.record-description (dtd-element element)))

(defMethod compile-xml-record-definition
           ((name t) (definition null))
  nil)

(defMethod compile-xml-record-definition
           ((name t) (record-definition cons))
  (define-record (intern (string name) :keyword)
    :pointer
    (mapcar #'(lambda (fd)
                (list* (intern (string (first fd)) :keyword)
                       (rest fd)))
            record-definition)
    nil))

(defMethod mop:ensure-class-using-class
           ((class xml-record-class) (name t) &rest class-initargs
            &key record-definition
                 (record-name (compile-xml-record-definition name record-definition))
                 dtd-element
            &aux record-descriptor
                 element-descriptor)
  "defines an xml record class given a record definition.
   as a side-effect it "

  ;; es koennen folgende angaben gemacht werden
  ;;  satz-form (als beschreibung oder ueber den name)
  ;;  dtd (als element oder ueber den name)
  ;;  satz-form und dtd
  (when record-name
    (setf record-descriptor (ccl:find-record-descriptor record-name)))
  (when dtd-element
    (setf dtd-element (dtd-element dtd-element)))

  (cond ((and record-descriptor dtd-element)
         (verify-record-and-dtd record-descriptor dtd-element)
         (setf element-descriptor
               (map 'vector #'(lambda (record-field)
                                (define-field-accessor class record-field))
                    (record-descriptor-fields record-descriptor))))
        (record-descriptor
         (setf element-descriptor
               (map 'vector #'(lambda (record-field)
                                (define-field-accessor class record-field))
                    (record-descriptor-fields record-descriptor)))
         (setf dtd-element
               (make-instance (node-class 'element-decl name nil)
                 :name name
                 :qualified-name name
                 :model
                 (make-instance 'dtd-model-group
                   :content (map 'list #'xml-element.declaration element-descriptor)
                   :connector *seq-marker*))))
        (dtd-element
         (unless (setf record-name
                       (ignore-errors
                        (define-record (intern (string name) :keyword)
                          :pointer
                          (dtd-element.record-description dtd-element))))
           (error "translation from dtd to record for ~s failed." (class-name class)))
         (setf record-descriptor (ccl:find-record-descriptor record-name))
         (setf element-descriptor
               (map 'vector #'(lambda (record-field)
                                (define-field-accessor class record-field))
                    (record-descriptor-fields record-descriptor))))
  
        (t
         (error "an ~s requires a record descriptor or a dtd."
                (class-name class))))

  (apply #'call-next-method class name
         :dtd-element dtd-element
         :record-descriptor record-descriptor
         :element-descriptor element-descriptor
         class-initargs))

(defMethod define-field-accessor
           ((class xml-record-class) field-descriptor
            &aux reader writer function function-name
            (type (field-descriptor-type field-descriptor))
            (name (field-descriptor-name field-descriptor))
            (offset (field-descriptor-offset field-descriptor))
            (length (field-descriptor-length field-descriptor))
            (method nil))
  (setf reader (make-field-reader type offset length))
  (setf writer (make-field-writer type offset length))
  (setf function-name (intern (format nil "~a.~a" (class-name class) name)))
  (when (fboundp function-name)
    (setf function (symbol-function function-name))
    (setf method (make-instance 'standard-method
                   :function #'(lambda (dummy) dummy)
                   :qualifiers nil
                   :specializers (list class)
                   :name function-name))
    (add-method function method)
    (setf (slot-value method 'function)
          #'(lambda (instance)
              (funcall reader (xml-record.record instance)))))
  (setf function-name `(setf ,function-name))
  (when (fboundp function-name)
    (setf function (symbol-function function-name))
    (setf method (make-instance 'standard-method
                   :function #'(lambda (dummy1 dummy2) (eq dummy1 dummy2))
                   :qualifiers nil
                   :specializers (list (find-class t) class)
                   :name function-name))
    (add-method function method)
    (setf (slot-value method 'function)
          #'(lambda (datum instance)
              (funcall writer datum (xml-record.record instance))
              datum)))
  (let* ((dtd-element (make-instance (node-class 'element-decl name nil)
                          :name name
                          :qualified-name name
                          :model 'XML::CDATA))
         (element-reference
          (make-instance (node-class 'element-decl-ref name nil)
            :name name
            :qualified-name
            (intern (concatenate 'string
                                 (string (class-name class))
                                 "."
                                 (string name))
                    :keyword)
            :attdefs (list (make-instance 'dtd-attdef
                             :name 
                             (intern (concatenate 'string
                                                  (string (class-name class))
                                                  "."
                                                  (string name)
                                                  ".TYPE")
                                     :keyword)
                             :default type))
            :dtd-element dtd-element)))
    (make-instance 'xml-record-field
      :name name
      :dtd-element element-reference
      :reader reader
      :writer writer
      :content nil)))

(defMethod make-field-reader
           ((type cons) offset length)
  (make-field-reader (if (numberp (second type))
                       (first type)
                       (intern (concatenate 'string
                                            (string (first type)) "."
                                            (string (second type)))
                               :keyword))
                     offset length))
(defMethod make-field-reader
           ((type (eql :byte)) offset (length t))
  #'(lambda (record)
      (%get-byte record offset)))
(defMethod make-field-reader
           ((type (eql :word)) offset (length t))
  #'(lambda (record)
      (%get-word record offset)))
(defMethod make-field-reader
           ((type (eql :long)) offset (length t))
  #'(lambda (record)
      (%get-long record offset)))
(defMethod make-field-reader
           ((type (eql :string)) offset (length t))
  #'(lambda (record)
      (%get-string record offset)))
(defMethod make-field-reader
           ((type (eql :array.character)) offset length)
  #'(lambda (record)
      (%get-fstring record offset length)))
(defMethod make-field-reader
           ((type (eql :array.byte)) offset length)
  #'(lambda (record)
      (%get-vector record offset length)))
(defMethod make-field-reader
           ((type (eql :array.long)) offset length)
  #'(lambda (record
             &aux (count (/ length 4))
                  (vector (make-array count :initial-element 0)))
      (do ((i 0 (1+ i))) ((>= i count) vector)
        (setf (elt vector i) (%get-long record offset))
        (incf offset 4))))
(defMethod make-field-reader
           ((type (eql :array.word)) offset length)
  #'(lambda (record
             &aux (count (/ length 2))
                  (vector (make-array count :initial-element 0)))
      (do ((i 0 (1+ i))) ((>= i count) vector)
        (setf (elt vector i) (%get-word record offset))
        (incf offset 2))))

(defMethod make-field-writer
           ((type cons) offset length)
  (make-field-writer (if (numberp (second type))
                       (first type)
                       (intern (concatenate 'string
                                            (string (first type)) "."
                                            (string (second type)))
                               :keyword))
                     offset length))
(defMethod make-field-writer
           ((type (eql :byte)) offset (length t))
  #'(lambda (data record)
      (%put-byte record data offset)))
(defMethod make-field-writer
           ((type (eql :word)) offset (length t))
  #'(lambda (data record)
      (%put-word record data offset)))
(defMethod make-field-writer
           ((type (eql :long)) offset (length t))
  #'(lambda (data record)
      (%put-long record data offset)))
(defMethod make-field-writer
           ((type (eql :string)) offset (length t))
  #'(lambda (data record)
      (%put-string record data offset)))
(defMethod make-field-writer
           ((type (eql :array.character)) offset length)
  #'(lambda (data record)
      (%put-fstring record data offset length)))
(defMethod make-field-writer
           ((type (eql :array.byte)) offset length)
  #'(lambda (data record)
      (%put-vector record data offset length)))
(defMethod make-field-writer
           ((type (eql :array.long)) offset length)
  #'(lambda (vector record)
      (when (> (length vector) (/ length 4))
        (error "vector too large for field"))
      (dotimes (i (length vector))
        (%put-long record (elt vector i) offset)
        (incf offset 4))
      (when (< offset length)
        (%put-bytes record #x00 offset (- length offset)))
      vector))
(defMethod make-field-writer
           ((type (eql :array.word)) offset length)
  #'(lambda (vector record)
      (when (> (length vector) (/ length 2))
        (error "vector too large for field"))
      (dotimes (i (length vector))
        (%put-word record (elt vector i) offset)
        (incf offset 2))
      (when (< offset length)
        (%put-bytes record #x00 offset (- length offset)))
      vector))


:EOF
