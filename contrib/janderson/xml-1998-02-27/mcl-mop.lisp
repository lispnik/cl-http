(in-package :mcl-mop)

#|
 <CHRONOLOGY>
 <DT>971013 jaa
 <DD>added guard in closeure walk for resetting class slots, to ensure that the
 class in question is a mop-standard-class.
 <CHRONOLOGY>
 |#
(eval-when (:load :compile :execute)
  (pushnew :mcl-mop cl:*features*))

(defclass metaobject ()
  ;; Vorsicht must be shadowed!
  ())

(defclass mop-standard-class (standard-class)
  ((class-slots :accessor class-slots :initform nil )
   (direct-slots :accessor class-direct-slots :initarg :direct-slots :initform nil)
   (direct-default-initargs :accessor class-direct-default-initargs :initarg :direct-default-initargs :initform nil)
   (default-initargs :accessor class-default-initargs :initform nil)))

(defmethod class-slots :around ((class mop-standard-class))
  (let ((slots (call-next-method)))
    (or slots
        (setf (class-slots class)
              (compute-class-slots class)))))

(defmethod class-default-initargs :around ((class mop-standard-class))
  (let ((initargs (call-next-method)))
    (or initargs
        (setf (class-default-initargs class)
              (compute-default-initargs class)))))

(defclass slot-definition (metaobject)
  ())

(defclass direct-slot-definition (slot-definition)
  ())

(defmethod print-object ((object direct-slot-definition) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (format stream "direct slot ~(~A~)" (slot-definition-name object))))

(defclass effective-slot-definition (slot-definition)
  ())

(defmethod print-object ((object effective-slot-definition) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (format stream "effective slot ~(~A~)" (slot-definition-name object))))

(defclass standard-slot-definition (slot-definition)
  ((allocation :reader slot-definition-allocation
               :initarg :allocation :initform :instance)
   (documentation :reader slot-definition-documentation
                  :initarg :documentation :initform nil)
   (initargs :reader slot-definition-initargs
             :initarg :initargs 
             :initform nil)
   (initform :reader slot-definition-initform
             :initarg :initform
             :initform nil)
   (initfunction :reader slot-definition-initfunction
                 :initform #'(lambda () nil))
   (name :reader slot-definition-name
         :initarg :name :initform 'noname)
   (readers :reader slot-definition-readers :initarg :readers :initform nil)
   (type :reader slot-definition-type
         :initarg :type
         :initform t)
   (writers :reader slot-definition-writers :initarg :writers :initform nil)))

(defclass standard-effective-slot-definition (standard-slot-definition effective-slot-definition)
  ())

(defclass standard-direct-slot-definition (standard-slot-definition direct-slot-definition)
  ())

(defmethod compute-class-slots ((class mop-standard-class))
  (mapcar #'(lambda (x)
              (let ((name (first x))
                    (direct-slots (rest x)))
                (compute-effective-slot-definition class name direct-slots)))
          (compute-all-slots (butlast (class-precedence-list class) 2))))

(defun compute-all-slots (cpl)
  (let (alist)
    (labels ((compute (cpl)
               (when cpl
                 (map nil #'(lambda (s)
                              (let* ((name (slot-definition-name s))
                                     (others (assoc name alist))) 
                                (if others
                                  (rplacd others (cons s (cdr others)))
                                  (push (list name s) alist))))
                      (class-direct-slots (first cpl)))
                 (compute (cdr cpl)))))
      (compute (reverse cpl)))
    alist))

(defmethod compute-default-initargs ((class mop-standard-class))
  (labels ((compute (classes)
             (if classes 
               ;; wiederholungen sind zulaessig
               (append (class-direct-default-initargs (first classes))
                       (compute (rest classes))))))
    (compute (butlast (class-precedence-list class) 2))))


(defun mapunion (fn liste &key (test #'eql) sort)
  (loop with result = nil
        for arg in liste
        for val = (funcall fn arg)
        do (loop for x in val do (pushnew x result :test test))
        finally return (if sort (nreverse result) result)))

#|
;; die beiden sind unten durch ein anders protokol ersetzt.
;; 1. die anabe der direkten slot-vereinbarung ist notwendig um die uebernahme
;; von slots aus der direkt-vereinbarung zu unterstutzen... oder?
;; 2. sonst reicht die spezialisierung von initialize-instance aus, da
;; c-e-s-d sowieso nur auf ein prototyp spezailisiert wurde.

(defmethod compute-effective-slot-definition ((class mop-standard-class) name direct-slot-definitions)
  (compute-esd-using-prototype (class-prototype (effective-slot-definition-class class name))
                           name
                           direct-slot-definitions))

(defmethod compute-esd-using-prototype ((instance standard-effective-slot-definition) slotname direct-slots)
  (make-instance (class-of instance)
    :allocation (slot-definition-allocation (first direct-slots))
        :type (slot-definition-type (first direct-slots))
        :readers (mapunion #'slot-definition-readers direct-slots)
        :writers (mapunion #'slot-definition-writers direct-slots)
        :initargs (mapunion #'slot-definition-initargs direct-slots)
        ;:initform ?
        ;:initfunction ?
        :name slotname))


;; noch eine alternative, die die default slots in die basis methods erzeugt
(defmethod compute-esd-using-prototype ((class print-method-slot-definition) slotname direct-slots
                                        &aux instance)
  (setf effective-definition (call-next-method))
  (setf (print-length effective-definition) (something-from direct-slots))
  effective-definition)
  
(defMethod initialize-instance
           ((self standard-effective-slot-definition) &rest sd-initargs
            &key direct-slot-definitions
                 (allocation (slot-definition-allocation (first direct-slot-definitions)))
                 (type (slot-definition-type (first direct-slot-definitions)))
                 (readers (mapunion #'slot-definition-readers direct-slot-definitions))
                 (writers (mapunion #'slot-definition-writers direct-slot-definitions))
                 (initargs (mapunion #'slot-definition-initargs direct-slot-definitions)))
  (apply #'call-next-method self
         :allocation allocation
         :type type
         :readers readers
         :writers writers
         :initargs initargs
         sd-initargs))

(defmethod compute-effective-slot-definition ((class mop-standard-class) name direct-slot-definitions)
  (make-instance (effective-slot-definition-class class :name name)
    :name name
    :direct-slot-definitions direct-slot-definitions))

 |#

(defmethod initialize-instance :after
           ((self standard-effective-slot-definition)
            &key direct-slot-definitions)
  (declare (ignore direct-slot-definitions))
  )


(defmethod compute-effective-slot-definition ((class mop-standard-class) name direct-slot-definitions)
  (make-instance (effective-slot-definition-class class :name name)
    :name name
    :allocation (slot-definition-allocation (first direct-slot-definitions))
    :type (slot-definition-type (first direct-slot-definitions))
    :readers (mapunion #'slot-definition-readers direct-slot-definitions)
    :writers (mapunion #'slot-definition-writers direct-slot-definitions)
    :initargs (mapunion #'slot-definition-initargs direct-slot-definitions)
    ;:initform ?
    ;:initfunction ?
    :direct-slot-definitions direct-slot-definitions))

(defun canonical-slot-definition (defform)
  (list* :name (first defform)
         (loop with readers 
               and writers
               and initargs 
               for (x y . rest) on (rest defform) by #'cddr
               nconc 
               (case x
                 ((:accessor) (push y readers) (push `(setf ,y) writers) nil)
                 ((:reader) (push y readers) nil)
                 ((:writer) (push y writers) nil)
                 ((:initarg) (push y initargs) nil)
                 (otherwise (list x y)))
               into cf-rest
               finally return 
               (let ((cf-readers (list :readers  readers))
                     (cf-writers (list :writers writers))
                     (cf-initargs (list :initargs initargs)))
                 (nconc cf-readers cf-writers cf-initargs cf-rest)))))

(defMethod canonicalize-defclass-option
           ((key t) &rest args)
  `(,key ,(first args)))

(defMethod canonicalize-defclass-option
           ((key (eql :default-initargs)) &rest args &aux c-args)
  (do ((key (pop args) (pop args))
       (value (pop args) (pop args)))
      ((null key))
    (push key c-args)
    (push `',value c-args))
  `(:direct-default-initargs (list ,@(nreverse c-args))))

(defMethod canonicalize-defclass-option
           ((key (eql :metaclass)) &rest args)
  `(:metaclass (find-class ',(first args))))

(defun canonicalize-defclass-options
       (options)
  (apply #'append
         (mapcar #'(lambda (option)
                     (apply #'canonicalize-defclass-option option))
                 options)))

(defMethod walk-closure
           (object function next)
  (funcall function object)
  (dolist (o (funcall next object))
    (walk-closure o function next)))

(defMethod ensure-class
           ((name symbol) &rest defclass-options
            &key direct-default-initargs
                 direct-slots
                 direct-superclasses
                 metaclass
                 documentation
            &allow-other-keys)
  (declare (ignore defclass-options))
  (eval`(clos::defclass ,name
           ,(mapcar #'(lambda (c)
                        (etypecase c
                          (symbol c)
                          (class (class-name c))))
                    direct-superclasses)
           ,direct-slots
           ,@(when direct-default-initargs
               `((:default-initargs ,@direct-default-initargs)))
           ,@(when metaclass
               `((:metaclass ,metaclass)))
           ,@(when documentation
               `((:documentation ,documentation))))))
           
           

(defmethod ensure-class-using-class
           ((class mop-standard-class) (name t) &rest defclass-options
            &key direct-default-initargs
                 direct-slots
                 direct-superclasses
                 name
                 metaclass
            &allow-other-keys)
  (declare (ignore direct-superclasses metaclass))
  (walk-closure class #'(lambda (c)
                          (when (typep c 'mop-standard-class)
                            (setf (class-slots c) nil
                                  (class-default-initargs c) nil)))
                #'class-direct-subclasses)
  (setf (class-direct-slots class)
        (mapcar #'(lambda (sd)
                    (apply #'make-instance
                           (apply #'direct-slot-definition-class class :name sd)
                           (canonical-slot-definition sd)))
                direct-slots)
        (class-direct-default-initargs class)
        direct-default-initargs)
  ;; ensure that the initargs are run. but note: since this mop lives in the
  ;; shadow of another, the standard initargs (which standard-class does use)
  ;; have to be removed from the initlist...
  (remf defclass-options :metaclass)
  (remf defclass-options :direct-slots)
  (remf defclass-options :direct-superclasses)
  (apply #'initialize-instance class defclass-options)
  class)

(defmethod ensure-class-using-class ((class t) (name symbol) &rest args)
  (declare (ignore args))
  nil)

(defmethod ensure-class-using-class :after
           ((class t) (name symbol) &rest args)
  (declare (ignore args))
  (setf (get name :defclass-time) (get-universal-time)))

(defMethod defclass-time ((what class)) (defclass-time (class-name what)))
(defMethod defclass-time ((what symbol)) (get what :defclass-time))

(defmethod direct-slot-definition-class (class &rest initargs)
  (declare (ignorable class initargs))
  (find-class 'standard-direct-slot-definition))

(defmethod effective-slot-definition-class (class &rest initargs)
  (declare (ignorable class initargs))
  (find-class 'standard-effective-slot-definition))

