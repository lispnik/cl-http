;;; -*- mode: lisp; package: "XML-PARSER"; -*-


"
<DOCUMENTATION>
 <DESCRIPTION>
  parsing, construction, and printing of notation declarations
  </DESCRIPTION>
 <COPYRIGHT HREF='defsystem.lisp|root().descendant(1,COPYRIGHT)'/>
 <CHRONOLOGY>
 <DELTA><DATE>19971218</DATE>PRINT-OBJECT added 
  </DELTA>
  <DELTA><DATE>19981114</DATE>
   split read function into parse/reduce phases;
   augmented error reporting</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
 "

(in-package "XML-PARSER")

;;;
;;; classes

(defClass notation-declaration (dtd-node xml-external-node)
  ((name :accessor notation-declaration.name)
   (system-id :accessor notation-declaration.system-id)
   (public-id :accessor notation-declaration.public-id))
  (:metaclass keyword-qualified-class))


;;;
;;; parsing

(defMethod reduce-production
           ((decl notation-declaration) &key)
  (setf (notation-declaration (xml-node.name decl) *document*) decl)
  (call-next-method))

(defMethod reduce-production
           ((production (eql 'XML-1.0::NotationDecl))
            &key
            ((xml-1.0::Name name))
            ((xml-1.0::PubidLiteral public-id))
            ((xml-1.0::SystemLiteral system-id)))
  (unless *parse-suppress*
    (reduce-production (make-instance 'notation-declaration
                         :name name
                         :system-id system-id
                         :public-id public-id))))

(defMethod production-reader-macro
           ((production (eql 'XML-1.0::NotationDecl)) (stream t)
            &aux name system-id public-id)
  (handler-bind ;; just augment error message for context
    ((error #'(lambda (condition)
                (xml-warn production
                          "name: ~s, system-id: ~s, public-id ~s."
                          name system-id public-id)
                condition)))
    (multiple-value-setq (name system-id public-id)
      (read-entity-attributes production stream))
    (reduce-production production
                       'xml-1.0::Name name
                       'xml-1.0::PubidLiteral public-id
                       'xml-1.0::SystemLiteral system-id)))


;;;
;;; model operations

(defMethod notation-declaration
           ((datum notation-declaration) (context t) &optional error-p)
  (declare (ignore error-p))
  datum)

(defMethod (setf notation-declaration)
           ((entity notation-declaration) (id symbol) (context t))
  (setf (get id 'notation-declaration) entity))

;;;
;;; printing

(defMethod print-object
           ((datum notation-declaration) (stream t))
  (with-accessors ((name xml-node.name)
                   (system-id xml-node.system-id)
                   (public-id xml-node.public-id))
                  datum
    (if *print-readably*
      (typecase *parent-node*
        (element-declaration (princ name))
        (t
         (format stream "<!NOTATION ~s " name)
         (if public-id
           (format stream "PUBLIC ~s~@[ ~s~]>" public-id system-id)
           (format stream "SYSTEM ~s>" system-id))))
      (print-unreadable-object (datum stream :type t)
        (format stream "~s (~s / ~s)"
                name system-id public-id)))))


  
#|
;; ok
(read-production
 'XML-1.0::NotationDecl
 (markup-stream "asdf SYSTEM 'ww/ss/xx'  >"))


;; ok
(read-production
 'XML-1.0::NotationDecl
 (markup-stream "asdf PUBLIC '-/ssss/ssss' 'ww/ss/xx' >"))

;; ok
(read-production
 'XML-1.0::NotationDecl
 (markup-stream "asdf PUBLIC '-/ssss/ssss' >"))

;; should fail
(read-production
 'XML-1.0::NotationDecl
 (markup-stream "asdf SYSTEM 'ww/ss/xx' PUBLIC '-/ssss/ssss' >"))

;; should fail
(read-production
 'XML-1.0::NotationDecl
 (markup-stream "asdf PUBLIC '-/ssss/ssss'  SYSTEM 'ww/ss/xx'>"))





|#


"XMLP"
