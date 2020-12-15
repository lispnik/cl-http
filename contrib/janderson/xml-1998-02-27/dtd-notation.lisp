;;; -*- mode: lisp; package ("XML-PARSER") -*-
;;;
;;; this version (C) mecom gmbh 24.11.97
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.

#|
<DOCUMENTATION>
<DESCRIPTION>
 parsing, construction, and printing of notation declarations
 </DESCRIPTION>
<CHRONOLOGY>
<DATE>19971218</DATE>
<DELTA>PRINT-OBJECT added 
 </DELTA>
 </CHRONOLOGY>
</DOCUMENTATION>
 |#


(in-package :XML-PARSER)

(defClass dtd-notation (dtd-element xml-external-node)
  ((name :accessor dtd-notation.name)))


(defMethod node-class
           ((node (eql 'notation-decl)) (op (eql 'xml::notation)) (context t))
  *notation-class*)

(defMethod read-typed-markup-declaration
           ((tag-name (eql 'xml::notation)) (stream t)
            &aux name declaration class element args)
  (setf declaration (read-markup-tag-parameters tag-name stream)) 
  (setf name (pop declaration))
  (setf class (node-class 'notation-decl tag-name stream))
  (assert (symbolp name))
  (loop (setf element (pop declaration))
        (when (null element) (return))
        (case element
          (xml::public (setf (getf args :public-id) (pop declaration)
                             (getf args :location) (pop declaration)))
          (xml::system (setf (getf args :location) (pop declaration)))
          (t
           (xml-form-error stream "illegitimate notation parameters: ~s. ~s."
                           element declaration))))
  (apply #'make-instance class :name name args))

(defMethod xml-node.append-element
           ((*dtd* dtd) (child dtd-notation))
  (when (xml-verbose 'xml-node.append-element)
    (format t "~%installing: ~s: ~s." *dtd* child))
  (unless (eq (symbol-package (xml-node.name child))
              (dtd.package *dtd*))
    (xml-form-error *dtd* "namespace mismatch: ~s."
                        (xml-node.name child)))
  (when (find (dtd-notation.name child) (dtd.notations *dtd*)
              :key #'dtd-notation.name)
    (when *xml-warn-if-redefine*
      (xml-validity-error *dtd* "multiply defined notation: ~a." child))
    (setf (dtd.notations *dtd*)
          (delete (dtd-notation.name child) (dtd.notations *dtd*)
                  :key #'dtd-notation.name)))
  (push child (dtd.notations *dtd*))
  (setf (xml-node.parent child) *dtd*)
  child)

(defMethod print-object
           ((datum dtd-notation) (stream t))
  (with-accessors ((name xml-node.name)
                   (system xml-node.system)
                   (public-id xml-node.public-id))
                  datum
    (if *xml-print-readably*
      (typecase *parent-node*
        (dtd-element-declaration (princ name))
        (t
         (format stream "<!NOTATION ~s " name)
         (if public-id
           (format stream "PUBLIC ~s~@[ ~s~]>" public-id system)
           (format stream "SYSTEM ~s>" system))))
      (print-unreadable-object (datum stream :type t)
        (format stream "~s (~s / ~s)"
                name system public-id)))))

  



:EOF
