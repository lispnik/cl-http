;;; -*- mode: LISP; package: "XML-PARSER"; -*-

"
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
  <DELTA><DATE>19981213</DATE>
   including whitespace handling</DELTA>
 </CHRONOLOGY>
</DOCUMENTATION>
 "

(in-package "XML-PARSER")
(patch-readtable-for-xml)

(setf *parse-verbose* '(compile-model-predicate validate))
(setf *parse-verbose* t)
(setf *parse-verbose* nil)
(setf *compiler-verbose* t)
(setf *compiler-verbose* nil)

;; this is necessary in order that the models match, otherwise the included
;; whitespace makes them invalid !!
(setf *preserve-whitespace* nil)

(defParameter *test-element* nil)

;;; compile / apply a model predicate

(defun cmp (model) (compile-model-predicate model))
(defmethod imp ((predicate function))
  (pprint (function-lambda-expression predicate)
          (make-instance 'fred-window :view-size #@(1024 128))))
(defmethod imp ((model element-model))
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

(element.valid? #!<ATOM>test</ATOM>)
(element.valid? #!<TYPE>test</TYPE>)

(defParameter *p1* (cmp #!m(ATOM*) ))
;(imp *p1*)
(tmp* *p1* '((#!<ATOM>test</ATOM>))
      '((#!<ATOM>test</ATOM>
         #!<ATOM>test</ATOM>))
      '((#!<ATOM>test</ATOM>
         #!<TYPE>test</TYPE>)
        :result nil))

(defParameter *p2* (cmp #!m(ATOM | TYPE)*) )
(tmp* *p2* '((#!<ATOM>test</ATOM>))
      '((#!<TYPE>test</TYPE>))
      '((#!<ATOM>test</ATOM>
         #!<ATOM>test</ATOM>))
      '((#!<NAME>test</NAME>
         #!<ATOM>test</ATOM>) :result nil)
      '((#!<ATOM>test</ATOM>
         #!<TYPE>test</TYPE>))
      '((#!<TYPE>test</TYPE>
         #!<ATOM>test</ATOM>
         #!<TYPE>test</TYPE>
         #!<TYPE>test</TYPE>
         #!<TYPE>test</TYPE>
         #!<TYPE>test</TYPE>
         #!<ATOM>test</ATOM>
         #!<TYPE>test</TYPE>
         #!<ATOM>test</ATOM>)))

(defParameter *p3* (cmp #!m(ATOM, TYPE)*) )
(tmp* *p3* '((#!<ATOM>test</ATOM>) :result nil)
      '((#!<ATOM>test</ATOM>
         #!<ATOM>test</ATOM>) :result nil)
      '((#!<ATOM>test</ATOM>
         #!<TYPE>test</TYPE>))
      '((#!<TYPE>test</TYPE>
         #!<ATOM>test</ATOM>) :result nil)
      '((#!<ATOM>atom</ATOM>
         #!<TYPE>type</TYPE>
         #!<ATOM>atom</ATOM>
         #!<TYPE>type</TYPE>
         #!<ATOM>atom</ATOM>
         #!<TYPE>type</TYPE>)))

(defParameter *p4* (cmp #!m(ATOM, TYPE, DOCUMENTATION?) ))
(tmp *p4* (list #!<ATOM>test</ATOM>
                #!<TYPE>test</TYPE>
                #!<DOCUMENTATION>asasas</DOCUMENTATION>))
(tmp *p4* (list #!<ATOM>test</ATOM>
                #!<TYPE>test</TYPE>))


;;; now try to validate against the full dtd.
;;; note that for this the dtd for lisp must be loaded...

;(read-dtd-stream #p"dtd:lisp.dtd")


(setf *test-element* #!<NAME>asdf</NAME> )

(setf *test-element*
#!<DEFPARAMETER>
   <VALUE>1234</VALUE>
   <NAME>asdf</NAME>
   <DOCUMENTATION>this is a test</DOCUMENTATION>
   </DEFPARAMETER> )
(time (element.valid? *test-element*))

(setf *test-element*
#!<DEFPARAMETER>
   <VALUE>1234</VALUE>
   <NAME>asdf</NAME>
   <DOCUMENTATION>this is a test</DOCUMENTATION>
   </DEFPARAMETER> )
(time (element.valid? *test-element*))

(setf *test-element*
#!<DEFPARAMETER>
   <NAME>asdf</NAME>
   <VALUE>1234</VALUE>
   <DOCUMENTATION>this is a test</DOCUMENTATION>
   </DEFPARAMETER>)
(time (element.valid? *test-element*))


;; note that the predicates are generated on demand, which means that the
;; first application takes "longer"
;; reset the respective element's validity in order to test the time exclusive
;; of predicate compilation.

(reset-element.valid? *test-element*)


(tmp* #'element-derive-valid?
     #!<DEFPARAMETER><NAME>asdf</NAME><VALUE>1234</VALUE>
        </DEFPARAMETER>
     #!<DEFPARAMETER>
        <NAME>asdf</NAME>
        <VALUE>1234</VALUE>
        <DOCUMENTATION>this is a test</DOCUMENTATION>
        </DEFPARAMETER>
     #!<DEFPARAMETER>
        <NAME>asdf</NAME>
        <VALUE>1234</VALUE>
        </DEFPARAMETER>
      '(#!<DEFPARAMETER>
        <VALUE>1234</VALUE>
        <VALUE>1234</VALUE>
        <VALUE>1234</VALUE>
        <NAME>asdf</NAME>
        <DOCUMENTATION>this is a test</DOCUMENTATION>
        </DEFPARAMETER> :result nil)
     #!<DEFVAR>
        <NAME>asdf</NAME>
        <VALUE>1234</VALUE>
        <DOCUMENTATION>this is a test</DOCUMENTATION>
        </DEFVAR>
     #!<DEFVAR>
        <NAME>asdf</NAME>
        <VALUE>1234</VALUE>
        </DEFVAR>
     '(#!<DEFVAR>
        <NAME>asdf</NAME>
        <DOCUMENTATION>this is a test</DOCUMENTATION>
        </DEFVAR>  :result nil)
     #!<DEFVAR>
        <NAME>asdf</NAME>
        </DEFVAR>

     #!<ARGUMENT><NAME>arg-1</NAME></ARGUMENT>

     #!<ARGUMENT-LIST>
        <ARGUMENT><NAME>arg-1</NAME></ARGUMENT>
        <ARGUMENT><NAME>arg-2</NAME></ARGUMENT>
        <AUX>aux</AUX>
        <DEFAULT-ARGUMENT>
         <NAME>aux-arg-1</NAME>
         <DEFAULT>a default value</DEFAULT>
         </DEFAULT-ARGUMENT>
         </ARGUMENT-LIST>

     #!<BODY>
        <SEXP><OPERATOR>this</OPERATOR>
        <ATOM>is</ATOM><ATOM>a</ATOM><ATOM>test</ATOM>
        </SEXP>
        <SEXP><OPERATOR>to</OPERATOR>
        <ATOM>see</ATOM><ATOM>if</ATOM><ATOM>the</ATOM><ATOM>defun</ATOM><ATOM>model</ATOM><ATOM>works</ATOM>
        </SEXP>
        </BODY>


     #!<DOCUMENTATION>this is a test</DOCUMENTATION>

     #!<DEFUN>
        <NAME>asdf</NAME>
        <ARGUMENT-LIST>
        <ARGUMENT><NAME>arg-1</NAME></ARGUMENT>
        <ARGUMENT><NAME>arg-2</NAME></ARGUMENT>
        </ARGUMENT-LIST>
        <DOCUMENTATION>this is a test</DOCUMENTATION>
        <BODY>
         <SEXP>
          <OPERATOR>this</OPERATOR>
          <ATOM>is</ATOM><ATOM>a</ATOM><ATOM>test</ATOM>
          </SEXP>
        <SEXP><OPERATOR>to</OPERATOR>
        <ATOM>see</ATOM><ATOM>if</ATOM><ATOM>the</ATOM><ATOM>defun</ATOM><ATOM>model</ATOM><ATOM>works</ATOM>
        </SEXP>
        </BODY>
        </DEFUN>
        )

(setf *test-element*
     #!<LISP>
        <DEFUN>
         <NAME>asdf</NAME>
         <ARGUMENT-LIST>
          <ARGUMENT><NAME>arg-1</NAME></ARGUMENT>
          <ARGUMENT><NAME>arg-2</NAME></ARGUMENT>
          </ARGUMENT-LIST>
         <DOCUMENTATION>this is a test</DOCUMENTATION>
         <BODY>
          <SEXP><OPERATOR>this</OPERATOR>
               <ATOM>is</ATOM><ATOM>a</ATOM><ATOM>test</ATOM>
           </SEXP>
          <SEXP>
           <OPERATOR>to</OPERATOR>
           <ATOM>see</ATOM><ATOM>if</ATOM><ATOM>the</ATOM><ATOM>defun</ATOM><ATOM>model</ATOM><ATOM>works</ATOM>
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
                <ATOM>is</ATOM><ATOM>a</ATOM><ATOM>test</ATOM>
                </SEXP>
               <SEXP><OPERATOR>to</OPERATOR>
                <ATOM>see</ATOM><ATOM>if</ATOM><ATOM>the</ATOM><ATOM>defun</ATOM><ATOM>model</ATOM><ATOM>works</ATOM>
                </SEXP>
               </BODY>
         </DEFMETHOD>

        <SEXP><OPERATOR>print</OPERATOR><ATOM>a-ok</ATOM></SEXP>

        <ATOM>EOF</ATOM>
  
        </LISP>)

(time (element.valid? *test-element*))
(reset-element.valid? *test-element*)
(time (element.valid? *test-element*))
(let ((count 0)) (map-node *test-element* #'(lambda (e) (declare (ignore e))
                                             (incf count))
                           #'(lambda (n) (typep n 'element)))
     count)

;; a 7300/200 yielded 12ms for a 50 element grove, or (/ 12 50.0) = 240Ms/node
;; the model complexity will play a factor as well


