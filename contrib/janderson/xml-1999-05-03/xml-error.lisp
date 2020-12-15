;;; -*- package: "XML-PARSER"; -*-

"
<DOCUMENTATION>
 <COPYRIGHT HREF='defsystem.lisp|root().descendant(1,COPYRIGHT)'/>
 <DESCRIPTION>
  a framework for validation and form constraints.
  </DESCRIPTION>
 </DOCUMENTATION>
"

(in-package "XML-PARSER")


;; xml conditions
;; note that several slots duplicate the slots from cl conditions. this is to
;; ensure uniform treatment of warning and conditions.
  
(defClass xml-condition (condition)
  ((stream :initform nil :initarg :stream :reader condition-stream)
   (state :initform nil :initarg :state :reader condition-state)
   (name :initarg :name :initarg :context :reader condition-name)
   (error-type :initform "XML Condition" :reader condition-type)
   (message :initarg :message :initform "")
   (message-arguments :initarg :message-arguments :initform nil))
  (:default-initargs :stream *markup-stream*))

(defMethod simple-condition-format-arguments
           ((condition xml-condition))
  (with-slots (error-type name stream message message-arguments) condition
    (list error-type name stream message message-arguments)))

(defClass xml-error (xml-condition simple-error)
  ()
  (:default-initargs
    :format-string "xml condition (~a), in context (~a~@[ in ~s~]):~%~?"))
(defClass xml-warning (xml-condition simple-warning)
  ()
  (:default-initargs
    :format-string "xml condition (~a), in context (~a~@[ in ~s~]):~%~?"))

(defClass xml-internal-error (xml-error) ())

(defMacro defXMLCondition (stem (type &rest other-supers)
                           &optional slots &rest options)
  `(progn
     (defClass ,(intern (concatenate 'string (string stem) "-CONDITION"))
       (,(intern (concatenate 'string (string type) "-CONDITION"))
        ,@other-supers)
       ,slots
       ,@options)
     (define-condition ,(intern (concatenate 'string (string stem) "-ERROR"))
                       (,(intern (concatenate 'string (string stem) "-CONDITION"))
                        ,(intern (concatenate 'string (string type) "-ERROR")))
                       ())
     (define-condition ,(intern (concatenate 'string (string stem) "-WARNING"))
                       (,(intern (concatenate 'string (string stem) "-CONDITION"))
                        ,(intern (concatenate 'string (string type) "-WARNING")))
                       ())))

(defXMLCondition form (xml)
  ((name :initarg :production :accessor condition-production)
   (error-type :initform "Form Condition")))
(defXMLCondition validity (xml)
  ((name :initarg :production :accessor condition-production)
   (error-type :initform "Validity Condition")))
(defXMLCondition dom (xml)
  ((error-type :initform "DOM Condition")))

(defXMLCondition undefined-element (validity))
(defXMLCondition redefined-element (validity))
(defXMLCondition undefined-attribute (validity))
(defXMLCondition redefined-attribute (validity))
(defXMLCondition undefined-notation (validity))
(defXMLCondition redefined-notation (validity))
(defXMLCondition undefined-entity (validity))
(defXMLCondition redefined-entity (validity))
(defXMLCondition root-element (form))
(defXMLCondition attribute-value (form))
(defXMLCondition entity-reference-context (form)
  ((message :initform "erroneous entity reference context: ~s."
            :allocation :class)))
(defXMLCondition character-category (form)
  ((message :initform "character category error: ~s is not ~s."
            :allocation :class)))
(defXMLCondition element-content (form))

(defXMLCondition section-end (xml)
  ((name :initform 'xml-1.0::]]>)
   (error-type :initform "Section End")
   (message :initform "unexpected section end")
   (message-arguments :initform nil)))

(defMethod initialize-instance
           ((instance character-category-error) &rest initargs
            &key character (category "?")
                 (message-arguments (list character category)))
  (apply #'call-next-method instance
         :message-arguments message-arguments
         initargs))

(defMethod initialize-instance
           ((instance entity-reference-context-error) &rest initargs
            &key entity
                 (message-arguments (list entity)))
  (apply #'call-next-method instance
         :message-arguments message-arguments
         initargs))

(defMethod initialize-instance :after
           ((self xml-warning) &key)
  (with-slots (state stream) self
    (setf state (stream-state stream))))

(defMethod initialize-instance :after
           ((self xml-error) &key)
  (with-slots (state stream) self
    (setf state (stream-state stream))))

(defun section-end ()
  (signal 'section-end-condition)
  (error 'section-end-error))

;;;
;;; conditions are generated with either the condition initargs, or with a
;;, string, which then serves as a supplementary message.
;;; the generic function recurses with the generated condition and then defers
;;; to either ERROR or WARN depending on the condition's class.

(defMethod raise-xml-condition
           ((context t) (condition symbol) (keyword symbol) &rest args)
  (raise-xml-condition context
                       (apply #'make-instance condition :context context
                              keyword args)
                       nil))
(defMethod raise-xml-condition
           ((context t) (condition symbol) (message string) &rest args)
  (raise-xml-condition context
                       (make-instance condition :context context
                                      :message message
                                      :message-arguments args)
                       nil))

(defMethod raise-xml-condition
           ((context t) (condition simple-error) (message t) &key)
  (error condition))

(defMethod raise-xml-condition
           ((context t) (condition simple-warning) (message t) &key)
  (warn condition))


(defMethod xml-form-error
           (context message &rest args)
  (apply #'raise-xml-condition context *form-error-condition* message args))

(defMethod  xml-warn
            ((production t) message &rest args)
  (format *error-output*
          "~%> form context: ~a: ~?" production message args))
#+CCL
(progn
 (pushnew 'raise-xml-condition ccl::*nx-never-tail-call*)
 (pushnew 'xml-form-error ccl::*nx-never-tail-call*)
 (pushnew 'xml-warn ccl::*nx-never-tail-call*))


;;;
;;; manage a list of atoms to distinguish verbosity

(defMethod (setf xml-verbose)
           ((value null) (key symbol))
  (typecase *parse-verbose*
    (symbol (when (eq key *parse-verbose*) (setf *parse-verbose* nil)))
    (cons (setf *parse-verbose* (nremove key *parse-verbose*)))))

(defMethod (setf xml-verbose)
           ((value t) (key symbol))
  (typecase *parse-verbose*
    (null (setf *parse-verbose* key))
    (symbol
     (unless (eq key *parse-verbose*)
       (setf *parse-verbose* (list *parse-verbose* key))))
    (cons (pushnew key *parse-verbose*))))

(defMethod (setf xml-verbose)
           ((value t) (keys list))
  (dolist (key keys)
    (setf (xml-verbose key) value)))


(defMethod find-xml-verbose
           ((settings cons) (what t))
  (find what settings))

(defMethod find-xml-verbose
           ((settings null) (what t))
  nil)

(defMethod find-xml-verbose
           ((settings (eql t)) (what t))
  t)

(defMethod find-xml-verbose
           ((settings t) (what t))
  (equalp settings t))


(defMethod xml-verbose
           ((what t))
  (find-xml-verbose *parse-verbose* what))


"XMLP"
