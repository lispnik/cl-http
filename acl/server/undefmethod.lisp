(in-package "USER")

;;;
;;; CLOS debugging utilities - OBC
;;;

(defun method-possible-qualifiers (qualifiers)
  (let ((basicquals '(:before :after :around)))
    (if (member qualifiers basicquals)
	(list qualifiers)
      (and (consp qualifiers)
	   (let ((qualifier (first qualifiers)))
	     (or (and (symbolp qualifier)
		      (fboundp qualifier))
		 (keywordp qualifiers)
		 (intersection qualifiers basicquals)))
	   qualifiers))))

;;; Here an interesting exercise: how to find
;;; the setf method without using FDEFINITION?
;;;
(defun symbol-setf-function (sym)
  (unless (symbolp sym)
    (error "~a is not a symbol." sym))
  (let ((form (nth-value 3 (get-setf-method (list sym)))))
    (if form
        (symbol-function (first form))
        (error "the symbol ~a does not have a SETF method definition." sym))))

(defmacro get-method (function-name &optional qualifiers &rest specializers)
  `(apply #'get-method-fn ',function-name ',qualifiers ',specializers))

(defmacro undefmethod (function-name &optional qualifier &rest classes)
  `(apply #'undefmethod-fn ',function-name ',qualifier ',classes))

(defun get-method-fn (function-name &optional qualifiers &rest specializers)
  (unless (let ((quals (method-possible-qualifiers qualifiers)))
	    (if quals
		(setf qualifiers quals)))
    (setf specializers (cons qualifiers specializers))
    (setf qualifiers nil))
  (unless (or (symbolp function-name)
              (and (consp function-name)
                   (eql (first function-name) 'SETF)))
    (error "Function name is not a symbol or its setf form: ~a." function-name))
  (let ((generic (fdefinition function-name)))
    (values (find-method generic qualifiers (mapcar #'(lambda (spec)
                                                        (if (consp spec)
                                                            spec
                                                          (find-class spec)))
                                                    specializers))
            generic)))

(defun undefmethod-fn (function-name &optional qualifier &rest classes)
  (multiple-value-bind (special-method generic-function)
      (apply #'get-method-fn function-name qualifier classes)
    (remove-method generic-function special-method)))
