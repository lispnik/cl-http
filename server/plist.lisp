;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http-*-

;;; Copyright John C. Mallery,  1995, 1997.
;;; All rights reserved.

;;;------------------------------------------------------------------- 
;;;
;;;  PROPERTY LIST MIXIN FOR OBJECTS
;;;

(in-package :http) 

(define-generic get-value (property-list-mixin indicator &optional default)
  (declare (values value found-p))
  (:documentation "Gets the value stored under INDICATOR from PROPERTY-LIST-MIXIN or returns DEFAULT.
This returns a second value indicating whether the value was found or not.")) 

(defmethod get-value ((plist property-list-mixin) indicator &optional default)
  (with-slots (plist) plist
    (let ((value (getf plist indicator :+not-found+)))
      (case value
        (:+not-found+
          (values default nil))
        (t (values value t))))))

(defmethod (setf get-value) (value (plist property-list-mixin) indicator &optional default)
  (declare (ignore default))
  (with-slots (plist) plist
    (setf (getf plist indicator) value)))

#+Genera
(defmethod www-utils:property-list-location ((plist property-list-mixin))
  (with-slots (plist) plist
    (scl:locf plist)))

#+Genera
(defmethod www-utils:property-list-value-location ((plist property-list-mixin) indicator &optional default)
  (with-slots (plist) plist
    (scl:locf (scl:getf plist indicator default))))

(define-generic remove-value (property-list-mixin indicator)
  (:documentation "Removes the value stored under INDICATOR from PROPERTY-LIST-MIXIN."))

(defmethod remove-value ((plist property-list-mixin) indicator)
  (with-slots (plist) plist
    (remf plist indicator)))

(define-generic map-indicators (property-list-mixin function)
  (:documentation "Maps FUNCTION over all the indicators of PROPERTY-LIST-MIXIN."))

(defmethod map-indicators ((plist property-list-mixin) function)
  (with-slots (plist) plist
    (loop for item in plist by #'cddr
          do (funcall function item))))

(define-generic map-values  (property-list-mixin function)
  (:documentation "Maps FUNCTION over all the values of PROPERTY-LIST-MIXIN."))

(defmethod map-values ((plist property-list-mixin) function)
  (with-slots (plist) plist
    (loop for item in (cdr plist) by #'cddr
          do (funcall function item))))

(define-generic property-list  (property-list-mixin)
  (:documentation "Returns the property list for property-list-mixin."))

(define-macro with-value-cached ((property-list-mixin key &key (recompute-p nil recompute-supplied-p)) &body value-form)
  "Caches the value returned by value-form on PROPERTY-LIST-MIXIN's property list under the indicator KEY.
When RECOMPUTE-P is non-null, the value is recomputed and recached.
The returned values are VALUE and RETRIEVE-FROM-CACHE-P."
  (declare (values cached-value retrieved-from-cache-p))
  (let ((form `(let ((val (getf plist ,key :+not-found+)))
                 (case val
                   (:+not-found+ 
                     (setf (getf plist ,key) (progn  . ,value-form)))
                   (t (values val t))))))      
    (cond (recompute-supplied-p
           `(with-slots (plist) ,property-list-mixin
              (cond (,recompute-p
                     (setf (getf plist ,key) (progn . ,value-form)))
                    (t ,form))))
          (t `(with-slots (plist),property-list-mixin ,form)))))
