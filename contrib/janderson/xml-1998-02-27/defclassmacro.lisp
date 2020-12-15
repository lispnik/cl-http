#|-*- Mode: Lisp; Package: (:CLOS) -*-
 <HTML><DL>
 <DT>name
 <DD>"defClassMacro.lisp"
 <DT>author
 <DD><a href="mailto:janson@ibm.net">jaa.cnw</a>
 <DT>description
 <DD>a shadowing-import of clos::defclass wraps the cl::defClass with auxiliary
     macros. there is one included here to provide a default print method.
 <DT>chronology
 <DL>
 <DT>960820 jaa.cnw 0.1
 <DD>fixed &key error on make-instance :abstract
 <DT>960822 jaa.cnw 0.1.1
 <DD>added defInterface
 <DT>961023 jaa.cnw 0.1.2
 <DD>added definition-time property
 <DT>961030 jaa.cnw 0.1.2
 <DD>rewrote it to use a specialized method dispatch method to collect
     before/primary/after results and to iterate over class options.
 <DT>961117 jaa.cnw 0.1.3
 <DD>changed the expansion method for the :PRINT-METHOD option to provide
     standard implementation for STANDARD, :BEFORE, :AFTER, :AROUND methods
     in addition to the complete method which is generated for T.
     obviates the need to repeat the initargs just in order to get them in the
     prindet representation.
 <DT>961201 jaa.cnw 0.2.0
 <DD>the multiple-value method combination ran into problems with 4.0:
     multiple metaclasses fail during internal call-next-method dispatch.
     as an alternative, the defClass form is wrapped as a DEFCLASS-FORM
     instance and passed through the combined method. this allows the :before
     and :after methods to make their effects known. it could have been done
     this way the first time through...?
 <DT>970909 jaa/bb
 <DD>mini-mop implemented; print methods by way of a metaclass
 </DL>
 <DT>copyright:
 <DD>copyright &copy; james anderson/MeCom/CNW 1997
 <BR>This program is 'free' software; you can redistribute it and/or modify
     it under the following terms:
 <UL>
 <LI>This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 <LI>This source file and the contact address,
     <a href="mailto:janson@ibm.net">janson2@ibm.net</a>,
     must be included with any distribution.
 <UL>
 </DL>
 <PRE><CODE>
|#

(unless (find-package "CLOS") (cl:defpackage :clos (:use :cl #+:ccl :ccl #+mcl-mop :mop)))
(in-package :clos)
(eval-when (:compile-toplevel :execute :load-toplevel)
  (shadow '(cl:defClass) :clos))

(export '(defClassMacro
          defClass-macro-expander
          defdefClass-expander
          defClass-option-reader
          defInterface
          class-generalization-kind

          ;; macro form wrapper slots
          defclass-form
          defclass-class-options
          defclass-class-extensions
          defclass-slot-options
          defclass-slot-extensions

          ;; macro form wrapper accessors
          defclass.set-primary
          defclass.add-before
          defclass.add-after
          defclass.primary
          defclass.before
          defclass.after
          defClass.name
          defClass.superclasses
          defClass.slots
          defClass.options
          defClass.option-value))
        
           
#|<H3>Introduction</H3>

 the CL:defClass macro make no allowances for extensions or transformations
 during macro expansion. the consequence is that any semantic extensions which
 might be appropriate for a new kind of class require a different macro.
 this doesn't work where various extensions should coexist - particularly if
 the order of (or even presence of) definition can't be constrained.
 <P>
 an alternative defClass macro is defined here which 
 <!-- 961202 eliminated in favor of wrapping the form:
 uses an alternative method-combination mode -->
 wraps the defclass in an object to allow :before and :after methods which
 augment the primary defclass form and primary methods which filter the form.
 <P>
 the methods are dispatched over the content of the class option forms which
 are present in the initial defclass definition.
 the final form which results from this iterative process is filtered to
 eliminate any slot and/or class options which are not supported by the
 standard defclass macro.
 <P>
 the expansion takes place for EACH option whcih appears in the original
 defclass form.
 the final form becomes
 <CODE>
 (progn (progn . <i>before-forms</i>)
        (prog1 <i>primary-form</i>
             . <i>after-forms</i>))
 </CODE>
 the standard <code>DEFCLASS</code> macro is shadowed to offer a single
 interface to all macros which operate on a class definition.
 a simple
 <CODE>
 (require :defClassMacro)
 (shadowing-import '(clos::defClass) <i>package</I>)
 </CODE>
 is sufficient to active the alternative expansion mechanism.
 |#

(defParameter *defclass-verbose* nil)

(defParameter *defclass-standard-class*
  #+:mcl-mop 'mop:mop-standard-class
  #-:mcl-mop 'standard-class
  "the standard metaclass is ensured to accept the args for ensure-class.")

(defParameter *defclass-standard-class-options*
  '(:metaclass :default-initargs :documentation))
(defParameter *defClass-standard-slot-options*
  '(:allocation :accessor :documentation :initarg :initform :reader :type :writer))



(defun restrict-defclass-plist (plist remain &optional result)
  "generate a plist with only specified properties"
  (if plist
    (restrict-defclass-plist (nthcdr 2 plist) remain
                             (if (find (first plist) remain)
                               (list* (second plist) (first plist) result)
                               result))
    (nreverse result)))

(defun restrict-defclass-alist (alist remain)
  "generate an association list with only specified properties"
  (remove-if-not #'(lambda (option) (find option remain))
                 alist
                 :key #'first))

(cl:defClass defclass-expansion ()
  ((option-form :initform nil :initarg :option-form
                :reader defclass.option-form
                :documentation
                "the options specified for a given expansion option.")
   (option-value :initform nil :initarg :option-value
                 :reader defclass.option-value
                 :documentation
                 "the interpreted value of the expansion option.")
   (before-forms :initform nil :initarg :before
                 :reader defclass.before :writer defclass.set-before
                 :documentation
                 "expanded forms to be executed before the primary defclass form")
   (primary-form :initform nil :initarg :primary
                 :reader defclass.primary :writer defclass.set-primary
                 :documentation
                 "THE form which is to ultimately define the class with CL:DEFCLASS")
   (after-forms :initform nil :initarg :after
                :reader defclass.after :writer defclass.set-after
                :documentation
                "expanded forms to be executed after the primary defclass form"))
  (:documentation
   "a wrapper to collect the effective defclass expansion"))

(defMethod defClass.name
           ((form defClass-expansion))
  (second (defClass.primary form)))

(defMethod (setf defClass.name)
           (name (form defClass-expansion))
  (setf (second (defClass.primary form)) name))

(defMethod defClass.superclasses
           ((form defClass-expansion))
  (third (defClass.primary form)))

(defMethod (setf defClass.superclasses)
           (classes (form defClass-expansion))
  (setf (third (defClass.primary form)) classes))

(defMethod defClass.slots
           ((form defClass-expansion))
  (fourth (defClass.primary form)))

(defMethod (setf defClass.slots)
           (slots (form defClass-expansion))
  (setf (fourth (defClass.primary form)) slots))

(defMethod defClass.options
           ((form defclass-expansion))
  (nthcdr 4 (defClass.primary form)))

(defMethod (setf defClass.options)
           (options (form defClass-expansion))
  (setf (nthcdr 4 (defClass.primary form)) options))

(defMethod (setf defClass.primary)
           (form (expansion defclass-expansion))
  (defclass.set-primary form expansion))

(defMethod defClass.add-before
           (form (expansion defclass-expansion))
  (defclass.set-before (nconc (defclass.before expansion) (list form))
    expansion)
  expansion)

(defMethod defClass.add-after
           (form (expansion defclass-expansion))
  (defclass.set-after (nconc (defclass.after expansion) (list form))
    expansion)
  expansion)

;; an extended accessor for option values by name
(defMethod defClass.option-value-named
           ((expansion defclass-expansion) option-key
            &aux (option-form (assoc option-key (defClass.options expansion))))
  ;; 970117 when should have been an if
  (if option-form
    (values
     (if (subtypep option-key 'defclass-expansion)
       (defClass.option-value (make-instance option-key
                                :primary (defClass.primary expansion)
                                :option-form option-form))
       (if (> (length option-form) 2)
         (rest option-form)
         (if (< (length option-form) 2)
           option-key
           (second option-form))))
     option-form)
    (values nil nil)))

;(setf *defclass-verbose* t)
;(setf *defclass-verbose* nil)
(defMacro clos::defClass
          (name superclasses slots &rest options
                &aux 
                before-forms primary-form after-forms
                option-key option-value
                (*defclass-verbose*
                 (if (assoc :defclass-verbose options)
                   (second (assoc :defclass-verbose options))
                   *defclass-verbose*))
                (*print-pretty* (if *defclass-verbose*
                                  t *print-pretty*))
                expander)
  
  (unless (second (assoc :metaclass options))
      (push `(:metaclass ,*defclass-standard-class*) options))
  
  (setf before-forms nil
        primary-form (list* 'defclass name superclasses slots options)
        after-forms nil)

  ;; expansion protocol:
  ;; interate over the options forms; instantiate any which are defined classes
  ;; and generate an expansion by dispatching on the option instance.
  ;; collect the resulting before and after forms and use the primary form.
  (dolist (option-form options)
    (setf option-key (first option-form))
    ; when the option has been defined as an expansion,
    (when (subtypep option-key 'defclass-expansion)
      ; generate the additional/replacement forms, (using the option, the
      ; option value, the wrapped expansion, and the original forms elements),
      (setf expander
            (make-instance option-key :primary primary-form :option-form option-form))
      ;; the option can suppress the expansion by setting a NULL value
      (when (setf option-value (defclass.option-value expander))
        ;; generate the expansion,
        (defClass-macro-expander expander name superclasses slots options)
        ;; and (if the option actually wants to do anything) merge or replace
        ;; as appropriate.
        (setf before-forms (append before-forms (defclass.before expander))
              primary-form (or (defclass.primary expander) primary-form)
              after-forms (append after-forms (defclass.after expander)))
        (when (or (eq *defclass-verbose* t)
                  (eq *defclass-verbose* :intermediate)
                  (eq *defclass-verbose* option-key))
          (format *trace-output*
                  "~%(~s ~s) expanded to:~%before~t~s~%primary~t~s~%after~t~s"
                  option-key option-value
                  before-forms primary-form after-forms)))))
    
  ;; after everything is over, clean up the forms
  ;; in particular, ensure that cl::defclass is the ultimate operator and
  ;; clean the slot and class option lists
  (setf before-forms
        (remove nil before-forms)
        after-forms
        (remove nil after-forms)
        primary-form
        `(cl:defClass ,(second primary-form)
           ,(third primary-form)
           ,(mapcar #'(lambda (slot)
                        `(,(first slot)
                          ,@(restrict-defclass-plist (rest slot)
                                                     *defclass-standard-slot-options*)))
                    slots)
           ,@(restrict-defclass-alist options *defclass-standard-class-options*)))
    
  (let* ((class-name (second primary-form))
         (superclasses (third primary-form))
         (default-initargs (rest (assoc :default-initargs options))))
    (declare (ignorable class-name superclasses default-initargs))
    (setf primary-form
          `(progn ,@before-forms
                  (prog1 (eval-when (:compile-toplevel :load-toplevel :execute)
                           ,primary-form)
                    #+:mcl-mop
                    (mop:ensure-class-using-class
                     (find-class ',class-name) ',class-name
                     :direct-slots ',slots
                     :direct-superclasses ',superclasses
                     ,@(mop:canonicalize-defclass-options options)))
                    ,@after-forms)))
    (when (or (eq *defclass-verbose* t)
              (eq *defclass-verbose* :final))
      (format *trace-output* "~%final form : ~s" primary-form))
    primary-form)


(defGeneric defclass-macro-expander
  (expansion name superclasses slots options))


#|<H3>base methods and expander definitions</H3>

 the base method just returns the form expansion unchanged and
 leaves whatever the before / after methods do untouched.
<P>
 the <CODE>defDefClass-expander</CODE> defines a class for the epansion
 keyword.
 |#

(defMethod defclass-macro-expander
           ((expansion t) name superclasses slots options)
  (declare (ignore name superclasses slots options))
  expansion)

(defMethod initialize-instance
           ((self defclass-expansion) &rest initargs
            &key option-form (option-value (second option-form)))
  ;; establish the default option value as the second element in the option form
  (declare (dynamic-extent initargs))
  (apply #'call-next-method self
         :option-value option-value
         initargs))

(defMacro defDefclass-expander
          (name &optional supers slots &rest options
           &aux (ce-slot (assoc 'defclass-class-extensions slots)))
  (unless ce-slot
    (push `(defclass-class-extensions :initform '(,name)) slots))
  `(cl:defclass ,name ,(or supers '(defclass-expansion))
            ,slots ,@options))


#|<H3>Interface Definitions</H3>
 
 the <CODE>defInterface</CODE> macro is a compact means to define and export
 generic functions for a given class.
  <P>
 the function will attempt check before it exports symbols, to ensure
 that it does not cause a conflict with something present in a using
 package. this is useful where parts of a package are loaded incrementally.
 this is not, however, guaranteed to win where using packages are also
 introduced incrementally
 |#

(defMacro defInterface
          (class-name &rest functions
           &aux (documentation (first functions))
                (symbols-to-export nil)
                (symbols-to-import nil))
  (if (stringp documentation)
    (pop functions)
    (setf documentation nil))
  (setf symbols-to-export (remove-if-not #'symbolp functions))
  (setf functions (remove-if-not #'consp functions))
  (when class-name (push class-name symbols-to-export))
  (setf symbols-to-export
        (append symbols-to-export
                (mapcar #'(lambda (function &aux (name (first function)))
                            (if (consp name) (second name) name))
                        functions)))
  (dolist (user (package-used-by-list *package*))
    (setf symbols-to-export
          (mapcar #'(lambda (symbol &aux other-symbol user-symbol)
                      (setf user-symbol
                        (find-symbol (string symbol) user))
                      (cond ((and user-symbol
                                  (not (eq user-symbol symbol)))
                             (setf other-symbol
                                   (find (string symbol)
                                         symbols-to-import
                                         :key #'string
                                         :test #'string-equal))
                             (when other-symbol
                               (error "symbol known to two users : ~s ~s"
                                      user-symbol other-symbol))
                             (unintern symbol)
                             (push user-symbol symbols-to-import)
                             user-symbol)
                            (t
                             symbol)))
                  symbols-to-export)))
  `(progn ,@(when symbols-to-import
             `((import ',symbols-to-import)))
          (export ',symbols-to-export)
          ,@(when (and documentation class-name)
              `((setf (documentation ',class-name 'interface) ,documentation)))
          ,@(when class-name
              (mapcar #'(lambda (function)
                          `(defGeneric ,@function))
                      functions))
          ',class-name))


(provide :defClassMacro)
