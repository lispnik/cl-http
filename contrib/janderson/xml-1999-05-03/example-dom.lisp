;;; -*- mode: LISP; package: "XML-PARSER"; -*-

"
<DOCUMENTATION>
<DESCRIPTION>
 these functions generate dom descriptions for standard lisp objects
<CHRONOLOGY>
 <DELTA DATE='19981210'>
 <DELTA DATE='19990418'>package berichtigt</DELTA>
 </CHRONOLOGY>
</DOCUMENTATION>
 "

(in-package "XMLP")

(defMethod make-lisp-element ((sexp t))
  (declare (ignore rest))
  (make-element (type-of sexp) (write-to-string sexp)))

(defMethod make-lisp-element
           ((sexp cons))
  (apply #'make-sexp-element sexp))

(defMethod make-sexp-element
           ((op symbol) &rest args)
  (dom sexp
       (dom operator op)
       (dom* arguments (mapcar #'make-lisp-element args))))
;(make-lisp-element '(+ 1 2))

(defMethod make-sexp-element
           ((op cons) &rest args)
  (if (eq (first op) 'lambda)
    (dom sexp
         (dom operator (make-lisp-element op))
         (dom* arguments (mapcar #'make-lisp-element args)))
    (call-next-method)))
;(pprint (make-lisp-element '((lambda (x y) x) 1 2)))


(defmethod make-sexp-element
           ((op (eql 'lambda)) &rest args&body
            &aux (arguments (first args&body)) (body (rest args&body)))
  (dom lambda
       (dom* argument-list
             (mapcar #'(lambda (name)
                         (dom argument (dom name (write-to-string name))))
                     arguments))
       (dom* body (mapcar #'make-lisp-element body))))
;(pprint (make-lisp-element '(lambda (x) x)))


(defMethod make-sexp-element
           ((op (eql 'defparameter)) &rest rest
            &aux (name (first rest)) (value (second rest)) (doc (third rest)))
  (dom* defparameter
         (dom name (write-to-string name))
         (dom value (make-lisp-element value))
         (when doc (list (dom declaration doc)))))
;(pprint (make-lisp-element '(defparameter a (+ 1 2) " a test")))

(defMethod make-sexp-element
           ((op (eql 'defclass)) &rest rest
            &aux (name (first rest)) (parents (second rest)) (slots (third rest))
            (doc (second (assoc :documentation (cdddr rest))))
            (meta (second (assoc :metaclass (cdddr rest)))))
  (dom* defclass
        (dom name (write-to-string name))
        (dom* class-list
              (mapcar #'(lambda (class) (dom class (write-to-string class)))
                      parents))
        (dom* slot-list
              (mapcar #'(lambda (slot)
                          (make-element 'slot
                                        (dom name (write-to-string (first slot)))))
                      slots))
         `(,@(when doc (list (dom documentation doc)))
           ,@(when meta (list (dom metaclass (write-to-string meta)))))))
;(pprint (make-lisp-element '(defclass x (y) ((s1 :reader x.s1)) (:documentation "none"))))
           

(defMethod make-sexp-element
           ((op (eql 'defun)) &rest rest
            &aux (name (first rest)) (arguments (second rest)) (body (cddr rest)))
  (dom defun
         (dom name (write-to-string name))
         (dom* argument-list
                (mapcar #'(lambda (name)
                            (make-element 'argument
                                          (dom name (write-to-string name))))
                        arguments))
         (dom* body (mapcar #'make-lisp-element body))))
;(pprint (make-lisp-element '(defun m (x) (+ x 1))))

(defMethod make-sexp-element
           ((op (eql 'defmethod)) &rest rest
            &aux (name (first rest)) (arguments (second rest)) (body (cddr rest)))
  (dom defmethod
       (dom name (write-to-string name))
       (dom* specialized-argument-list
             (mapcar #'(lambda (name)
                         (etypecase name
                           (symbol (dom specialized-argument
                                        (dom name (write-to-string name))))
                           (cons (dom specialized-argument
                                      (dom name (write-to-string (first name)))
                                      (dom specializer
                                           (write-to-string (second name)))))))
                     arguments))
       (dom* body (mapcar #'make-lisp-element body))))
;(pprint (make-lisp-element '(defMethod m ((x y)) (+ x 1))))



(pprint  (make-lisp-element
          '((lambda (x y z) (list x y z))
            (defun x (a) a)
            (defclass c (a s) ((s1 :initarg :s1)))
            (defMethod c-x ((a c) b) (this is a test) (to see if methods work)))))

