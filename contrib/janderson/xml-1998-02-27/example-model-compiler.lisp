;;; -*- mode: LISP; package: ("XML-PARSER")

#|
<DOCUMENTATION>
<DESCRIPTION>
 this file contains tests and examples for the xml/dtd model predicate compiler.
 the compiler augments a dtd element declaration with a predicate function which
 determines whether a given element <EM>content</EM> conforms to the declared model.

 <P>
 the examples are drawn from the lisp.dtd which accompanies the parser.
 there are also several functions to transform s-expressions into the equivalent
 (abbreviated) elements for test and demonstration purposes.
 </DESCRIPTION>
<CHRONOLOGY>
 <DATE>19971127</DATE>
  <DELTA>for first version of the model compiler</DELTA>
  </DELTA>
 </CHRONOLOGY>
</DOCUMENTATION>
 |#

(in-package :xml-parser)

(setf *xml-verbose* '(compile-model-predicate))
(setf *xml-verbose* t)
(setf *xml-verbose* nil)
(setf *xml-compiler-verbose* t)


;;; compile / apply a model predicate

(defun cmp (model) (compile-model-predicate model))
(defmethod imp ((predicate function))
  (pprint (function-lambda-expression predicate)
          (make-instance 'fred-window :view-size #@(1024 128))))
(defmethod imp ((model dtd-model))
  (pprint (model-predicate model)
          (make-instance 'fred-window :view-size #@(1024 128))))

(defun tmp (pred element &key (result t) (stream *trace-output*))
  (flet ((report (&rest values)
           (format stream "~%(~s ~s) -> ~s (was ~s should be ~s)"
                   pred element values (not result) result)))
    (funcall pred element
             #'(lambda (datum fail)
                 (unless result (report datum fail))
                 (if result
                   (return-from tmp nil)
                   (return-from tmp (list datum fail))))
             #'(lambda (datum succeed model)
                 (when result (report datum succeed model))
                 (if (not result)
                   (return-from tmp nil)
                   (return-from tmp (list datum succeed model)))))))

(defun tmp* (pred &rest specs)
  (remove nil
          (mapcar #'(lambda (spec)
                      (unless (consp spec) (setf spec (list spec)))
                      (apply #'tmp pred spec))
                  specs)))


;;; first a very simple model from the lisp dtd. note that the predicate is
;;; applied to element content and not to elements directly.

(xml-element.valid? #!<LISP::ATOM>test</LISP::ATOM>)
(xml-element.valid? #!<LISP::TYPE>test</LISP::TYPE>)

(defParameter *p1* (cmp #!m(LISP::ATOM*)))
;(imp *p1*)
(tmp* *p1* '((#!<LISP::ATOM>test</LISP::ATOM>))
      '((#!<LISP::ATOM>test</LISP::ATOM>
         #!<LISP::ATOM>test</LISP::ATOM>))
      '((#!<LISP::ATOM>test</LISP::ATOM>
         #!<LISP::TYPE>test</LISP::TYPE>)
        :result nil))

(defParameter *p2* (cmp #!m(LISP::ATOM | LISP::TYPE)*))
(tmp* *p2* '((#!<LISP::ATOM>test</LISP::ATOM>))
      '((#!<LISP::TYPE>test</LISP::TYPE>))
      '((#!<LISP::ATOM>test</LISP::ATOM>
         #!<LISP::ATOM>test</LISP::ATOM>))
      '((#!<LISP::NAME>test</LISP::NAME>
         #!<LISP::ATOM>test</LISP::ATOM>) :result nil)
      '((#!<LISP::ATOM>test</LISP::ATOM>
         #!<LISP::TYPE>test</LISP::TYPE>))
      '((#!<LISP::TYPE>test</LISP::TYPE>
         #!<LISP::ATOM>test</LISP::ATOM>
         #!<LISP::TYPE>test</LISP::TYPE>
         #!<LISP::TYPE>test</LISP::TYPE>
         #!<LISP::TYPE>test</LISP::TYPE>
         #!<LISP::TYPE>test</LISP::TYPE>
         #!<LISP::ATOM>test</LISP::ATOM>
         #!<LISP::TYPE>test</LISP::TYPE>
         #!<LISP::ATOM>test</LISP::ATOM>)))

(defParameter *p3* (cmp #!m(LISP::ATOM, LISP::TYPE)*))
(tmp* *p3* '((#!<LISP::ATOM>test</LISP::ATOM>) :result nil)
      '((#!<LISP::ATOM>test</LISP::ATOM>
         #!<LISP::ATOM>test</LISP::ATOM>) :result nil)
      '((#!<LISP::ATOM>test</LISP::ATOM>
         #!<LISP::TYPE>test</LISP::TYPE>))
      '((#!<LISP::TYPE>test</LISP::TYPE>
         #!<LISP::ATOM>test</LISP::ATOM>) :result nil)
      '((#!<LISP::ATOM>atom</LISP::ATOM>
         #!<LISP::TYPE>type</LISP::TYPE>
         #!<LISP::ATOM>atom</LISP::ATOM>
         #!<LISP::TYPE>type</LISP::TYPE>
         #!<LISP::ATOM>atom</LISP::ATOM>
         #!<LISP::TYPE>type</LISP::TYPE>)))

(defParameter *p4* (cmp #!m(LISP::ATOM, LISP::TYPE, LISP::DOCUMENTATION?)))
(tmp *p4* (list #!<LISP::ATOM>test</LISP::ATOM>
                #!<LISP::TYPE>test</LISP::TYPE>
                #!<LISP::DOCUMENTATION>asasas</LISP::DOCUMENTATION>))
(tmp *p4* (list #!<LISP::ATOM>test</LISP::ATOM>
                #!<LISP::TYPE>test</LISP::TYPE>))


;;; now try to validate against the full dtd.
;;; note that for this the dtd must be loaded...
;;; (look on the menubar for "Markup":"Load DTD":"lisp"

(tmp* #'xml-element-derive-valid?
     #!<LISP::DEFPARAMETER><NAME>asdf</NAME><VALUE>1234</VALUE>
        </LISP::DEFPARAMETER>
     #!<LISP::DEFPARAMETER>
        <NAME>asdf</NAME>
        <VALUE>1234</VALUE>
        <DOCUMENTATION>this is a test</DOCUMENTATION>
        </LISP::DEFPARAMETER>
     #!<LISP::DEFPARAMETER>
        <NAME>asdf</NAME>
        <VALUE>1234</VALUE>
        </LISP::DEFPARAMETER>
      '(#!<LISP::DEFPARAMETER>
        <VALUE>1234</VALUE>
        <NAME>asdf</NAME>
        <DOCUMENTATION>this is a test</DOCUMENTATION>
        </LISP::DEFPARAMETER> :result nil)
     #!<LISP::DEFVAR>
        <NAME>asdf</NAME>
        <VALUE>1234</VALUE>
        <DOCUMENTATION>this is a test</DOCUMENTATION>
        </LISP::DEFVAR>
     #!<LISP::DEFVAR>
        <NAME>asdf</NAME>
        <VALUE>1234</VALUE>
        </LISP::DEFVAR>
     '(#!<LISP::DEFVAR>
        <NAME>asdf</NAME>
        <DOCUMENTATION>this is a test</DOCUMENTATION>
        </LISP::DEFVAR>  :result nil)
     #!<LISP::DEFVAR>
        <NAME>asdf</NAME>
        </LISP::DEFVAR>

     #!<LISP::ARGUMENT><NAME>arg-1</NAME></LISP::ARGUMENT>

     #!<LISP::ARGUMENT-LIST>
        <ARGUMENT><NAME>arg-1</NAME></ARGUMENT>
        <ARGUMENT><NAME>arg-2</NAME></ARGUMENT>
        <AUX>aux</AUX>
        <DEFAULT-ARGUMENT>
         <NAME>aux-arg-1</NAME>
         <DEFAULT>a default value</DEFAULT>
         </DEFAULT-ARGUMENT>
         </LISP::ARGUMENT-LIST>

     #!<LISP::BODY>
        <SEXP><OPERATOR>this</OPERATOR>
        <ATOM>is</ATOM><ATOM>a</ATOM></ATOM><ATOM>test</ATOM>
        </SEXP>
        <SEXP><OPERATOR>to</OPERATOR>
        <ATOM>see</ATOM><ATOM>if</ATOM></ATOM><ATOM>the</ATOM><ATOM>defun</ATOM><ATOM>model</ATOM><ATOM>works</ATOM>
        </SEXP>
        </LISP::BODY>


     #!<LISP::DOCUMENTATION>this is a test</LISP::DOCUMENTATION>

     #!<LISP::DEFUN>
        <NAME>asdf</NAME>
        <ARGUMENT-LIST>
        <ARGUMENT><NAME>arg-1</NAME></ARGUMENT>
        <ARGUMENT><NAME>arg-2</NAME></ARGUMENT>
        </ARGUMENT-LIST>
        <DOCUMENTATION>this is a test</DOCUMENTATION>
        <BODY><SEXP><OPERATOR>this</OPERATOR>
        <ATOM>is</ATOM><ATOM>a</ATOM></ATOM><ATOM>test</ATOM>
        </SEXP>
        <SEXP><OPERATOR>to</OPERATOR>
        <ATOM>see</ATOM><ATOM>if</ATOM></ATOM><ATOM>the</ATOM><ATOM>defun</ATOM><ATOM>model</ATOM><ATOM>works</ATOM>
        </SEXP>
        </BODY>
        </LISP::DEFUN>

     #!<LISP::LISP>
        <DEFUN>
         <NAME>asdf</NAME>
         <ARGUMENT-LIST>
          <ARGUMENT><NAME>arg-1</NAME></ARGUMENT>
          <ARGUMENT><NAME>arg-2</NAME></ARGUMENT>
          </ARGUMENT-LIST>
         <DOCUMENTATION>this is a test</DOCUMENTATION>
         <BODY>
          <SEXP><OPERATOR>this</OPERATOR>
               <ATOM>is</ATOM><ATOM>a</ATOM></ATOM><ATOM>test</ATOM>
           </SEXP>
          <SEXP>
           <OPERATOR>to</OPERATOR>
           <ATOM>see</ATOM><ATOM>if</ATOM></ATOM><ATOM>the</ATOM><ATOM>defun</ATOM><ATOM>model</ATOM><ATOM>works</ATOM>
           </SEXP>
          </BODY>
         </DEFUN>

        <DEFMETHOD>
         <NAME>asdf</NAME>
         <SPECIALIZED-ARGUMENT-LIST>
          <SPECIALIZED-ARGUMENT><NAME>arg-1</NAME><SPECIALIZER>a class</SPECIALIZER></SPECIALIZED-ARGUMENT>
          <SPECIALIZED-ARGUMENT><NAME>arg-2</NAME></SPECIALIZED-ARGUMENT>
          </SPECIALIZED-ARGUMENT-LIST>
         <DOCUMENTATION>this is a test for defmethod</DOCUMENTATION>
         <BODY><SEXP><OPERATOR>this</OPERATOR>
                <ATOM>is</ATOM><ATOM>a</ATOM></ATOM><ATOM>test</ATOM>
                </SEXP>
               <SEXP><OPERATOR>to</OPERATOR>
                <ATOM>see</ATOM><ATOM>if</ATOM></ATOM><ATOM>the</ATOM><ATOM>defun</ATOM><ATOM>model</ATOM><ATOM>works</ATOM>
                </SEXP>
               </BODY>
         </DEFMETHOD>

        <SEXP><OPERATOR>print</OPERATOR><ATOM>a-ok</ATOM></SEXP>

        <ATOM>EOF</ATOM>
  
        </LISP::LISP>)


;; htis code generates elements for (some) lisp forms

(defMethod make-lisp-element ((from t)) (make-element 'lisp::atom (write-to-string from)))
(defmethod make-lisp-sexp-element
           ((op (eql 'lambda)) &rest args&body
            &aux (arguments (first args&body)) (body (rest args&body)))
  (make-element 'lisp::lambda
                (apply #'make-element 'lisp::argument-list
                       (mapcar #'(lambda (name)
                                   (make-element 'lisp::argument
                                                 (make-element 'lisp::name (write-to-string name))))
                               arguments))
                (apply #'make-element 'lisp::body (mapcar #'make-lisp-element body))))
(defMethod make-lisp-sexp-element
           ((op t) &rest args)
  (apply #'make-element 'lisp::sexp
         (make-element 'lisp::operator (write-to-string op))
         (mapcar #'make-lisp-element args)))
(defMethod make-lisp-sexp-element
           ((op cons) &rest args)
  (apply #'make-element 'lisp::sexp
         (make-lisp-element op)
         (mapcar #'make-lisp-element args)))

(defMethod make-lisp-sexp-element
           ((op (eql 'defparameter)) &rest rest
            &aux (name (first rest)) (value (second rest)) (dcl (third rest)))
  (apply #'make-element 'lisp::defparameter
         (make-element 'lisp::name (write-to-string name))
         (make-element 'lisp::value (write-to-string value))
         (when dcl (list (make-element 'lisp::declaration (write-to-string dcl))))))

(defMethod make-lisp-sexp-element
           ((op (eql 'defclass)) &rest rest
            &aux (name (first rest)) (parents (second rest)) (slots (third rest))
            (doc (second (assoc :documentation (cdddr rest))))
            (meta (second (assoc :metaclass (cdddr rest)))))
  (apply #'make-element 'lisp::defclass
         (make-element 'lisp::name (write-to-string name))
         (apply #'make-element 'lisp::class-list
                (mapcar #'(lambda (class) (make-element 'class (write-to-string class)))
                        parents))
         (apply #'make-element 'lisp::slot-list
                (mapcar #'(lambda (slot)
                            (make-element 'lisp::slot
                              (make-element 'lisp::name (write-to-string (first slot)))))
                        slots))
         `(,@(when doc `((make-element 'lisp::documentation documentation)))
           ,@(when meta `((make-element 'lisp::metaclass (write-to-string meta)))))))
           

(defMethod make-lisp-sexp-element
           ((op (eql 'defun)) &rest rest
            &aux (name (first rest)) (arguments (second rest)) (body (cddr rest)))
  (make-element 'lisp::defun
         (make-element 'lisp::name (write-to-string name))
         (apply #'make-element 'lisp::argument-list
                (mapcar #'(lambda (name)
                            (make-element 'lisp::argument
                                          (make-element 'lisp::name (write-to-string name))))
                        arguments))
         (apply #'make-element 'lisp::body (mapcar #'make-lisp-element body))))

(defMethod make-lisp-sexp-element
           ((op (eql 'defmethod)) &rest rest
            &aux (name (first rest)) (arguments (second rest)) (body (cddr rest)))
  (make-element 'lisp::defmethod
         (make-element 'lisp::name (write-to-string name))
         (apply #'make-element 'lisp::specialized-argument-list
                (mapcar #'(lambda (name)
                            (etypecase name
                              (symbol (make-element 'lisp::specialized-argument
                                                    (make-element 'lisp::name (write-to-string name))))
                              (cons (make-element 'lisp::specialized-argument
                                                    (make-element 'lisp::name (write-to-string (first name)))
                                                    (make-element 'lisp::specializer
                                                                  (write-to-string (second name)))))))
                        arguments))
         (apply #'make-element 'lisp::body (mapcar #'make-lisp-element body))))



(defMethod make-lisp-element ((from cons)) (apply #'make-lisp-sexp-element from))
(let ((*xml-print-readably* t)
      (*print-pretty* t))
  (print (apply #'make-element 'lisp::lisp
                (mapcar #'make-lisp-element
                        '(((lambda (x y) (+ x y)) 1 2)
                          (defun x (a) a)
                          (defclass c (a s) ((s1 :initarg :s1)))
                          (defMethod c-x ((a c) b) (this is a test) (to see if methods work)))))))
(xml-element.valid? (apply #'make-element 'lisp::lisp
                #!<LISP::DEFPARAMETER>
                       <VALUE>1234</VALUE>
                       <NAME>asdf</NAME>
                       <DOCUMENTATION>this is a test</DOCUMENTATION>
                       </LISP::DEFPARAMETER>
                       (mapcar #'make-lisp-element
                        '(((lambda (x y) (+ x y)) 1 2)
                           
                          (defun x (a) a)
                          (defclass c (a s) ((s1 :initarg :s1)))
                          (defMethod c-x ((a c) b) (this is a test) (to see if methods work))
                          eof)))
                    nil)

