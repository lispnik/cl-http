;;; -*- mode: LISP; package: "XML-PARSER"; -*-
;;;

"
<DOCUMENTATION>
 <DESCRIPTION>
  </DESCRIPTION>
 <COPYRIGHT HREF='defsystem.lisp|root().descendant(1,COPYRIGHT)'/>
 <CHRONOLOGY>
  <DELTA><DATE>19971205</DATE>
   replaced keyword type tags with singletons (see tokens.html)
   they are instances, and thus unique, which the reader and parser
   uses as special tags.</DELTA>
  <DELTA><DATE>19981024</DATE>
   added list type definitions</DELTA>
  <DELTA><DATE>19981218</DATE>
   type specifier for system id dependent on access substrate</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
"

(in-package "XML-PARSER")


(deftype reserved-dtd-name ()
  '(member xml::cdata xml::\#pcdata xml::\#rcdata XML::ANY XML::EMPTY))
(defType attribute-mode ()
  '(member :required :implied :fixed nil))

(defun reserved-name-p (name)
  (declare (special *xml-package*))
  (if (and (consp name) (null (rest name)))
    (reserved-name-p (first name))
    (and (eq (symbol-package name) *xml-package*)
         (typep name 'reserved-dtd-name))))

(defun reserved-name-indicator-p (char)
  (char= char #\#))

(defMacro defListType
          (type
           &optional (element-type type)
           &aux
           (list-type
            (intern (concatenate 'string (string type) "-LIST")))
           (predicate
            (intern (concatenate 'string (string type) "-LISTP"))))
  (assert (symbolp type))
  `(progn
     (defun ,predicate (x)
       (and (listp x)
            (not (find-if #'(lambda (x) (not (typep x ',element-type))) x))))
     (setf (get ',list-type :element-type) ',element-type)
     (defType ,list-type ()
       '(and list (satisfies ,predicate)))))

(defListType attribute)
(defListType element-content (or string element entity-reference character-data))


"XMLP"

