;;; -*- package ("XML-PARSER") -*-

#|
<DOCUMENTATION>
<DESCRIPTION>
processing instructions provide a means for an xml-stream to pass "out-of-band"
information to a parser / application.
<P>
for most processing instruction, a PROCESS-NODE appends the node to the
current parent element. this is the same behaviour as for standard elements,
which may or may not be the right thing to do for a given instruction.
for this reason the specific method on XML-NODE.APPEND-ELEMENT just defers
upwards in the tree until some parent wants the instruction.
<P>
for a "XML" pi itself - that which declares the version and encoding -
a specific XML-NODE.APPEND-ELEMENT method is supplied to combine the
declarations with a containing document instance.
<P>
for the namespace instruction, a specific PROCESS-NODE method is applied which
loads and binds the specified DTD.

</DESCRIPTION>
<CHRONOLOGY>
<DATE>19971210</DATE>
<DELTA><CODE>XML-XML</CODE>modified for "standalone" rather
 than "required markup" - value range (:yes :no nil)
<DATE>19971217</DATE>
<DELTA>read-typed-processing-instruction reads parameters rather than
 attributes
 </DELTA>
<DATE>19971218</DATE>
 <DELTA>read-dtd-stream now requires the name</DELTA>
</CHRONOLOGY>
</DOCUMENTATION>
|#

(in-package :xml-parser)


;;;
;;; classes for pi nodes

(defMethod node-class
           ((node (eql 'pi)) (op (eql 'xml::xml)) (stream t))
  *xml-xml-class*)

(defMethod node-class
           ((node (eql 'pi)) (op (eql 'xml::namespace)) (stream t))
  *xml-namespace-class*)


(defClass pi-node (xml-node)
  ())

(defClass xml-xml (pi-node)
  ((version
    :initarg :version :initform 0.0
    :accessor xml-xml.version
    :type number)
   (encoding
    :initarg :encoding :initform nil
    :accessor xml-xml.encoding
    :type (or null string))
   (standalone
    :initarg :standalone? :initform t
    :accessor xml-xml.standalone?
    :type (member :yes :no nil))))


(defClass xml-namespace (pi-node xml-external-node xml-named-node)
  ((locations :initarg :href)
   (name :initarg :as :accessor xml-namespace.as)))

;;;
;;; element constructors and processors for xml processing instructions
;;; the general reader/constructor defers to a pi-specific reader/constructor
;;; which can parse the tag content and /or successors as approrpriate for
;;; the processing instruction

(defMethod read-typed-processing-instruction
           ((target symbol) (stream t)
            &aux (*package* (markup-package target))
                 args)
  (setf args (read-markup-tag-parameters target stream))
  (cond ((eq (first (last args)) *processing-instruction-tag-marker*)
         (setf (rest (last args 2)) nil)
         (apply #'make-processing-instruction target args))
        (t
         (xml-validity-error target
                             "illegitimate processing instruction form: ~s."
                             args))))

;;;
;;; for processing instructions
;;;
;;; this version (C) mecom gmbh 24.11.97
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.

(defMethod make-processing-instruction
           ((op (eql 'xml::xml)) &rest args)
  (let ((pi-function
         #'(lambda (&key ((xml::|version| |version|))
                         ((xml::version version) |version|)
                         ((xml::|standalone| |standalone|))
                         ((xml::standalone standalone) |standalone|)
                         ((xml::encoding encoding))
                    &aux node)
             (setf node (make-instance (node-class 'pi op nil)))
             (when version
               (setf (xml-xml.version node)
                     (etypecase version
                       (number version)
                       ((or string symbol)
                        (read-from-string (string version))))))
             (setf (xml-xml.standalone? node)
                   (cond ((string-equal "yes" (string standalone))
                          :yes)
                         ((string-equal "no" (string standalone))
                          :no)
                         (t
                          (when standalone
                            (xml-validity-error
                             op "illegitimate standalone option: ~s"
                             standalone))
                          nil)))
             (when encoding
               (setf (xml-xml.encoding node) encoding))
             node)))
  (handler-case
    (apply pi-function args)
    (error (condition)
           (xml-form-error op "error during pi interpretation of form: ~s: ~a"
                           args condition)))))

(defMethod make-processing-instruction
           ((op (eql 'xml::namespace)) &rest args)
  (let ((pi-function
         #'(lambda (&key ((xml::href href)) ((xml::as as)))
             (etypecase href
               (string (if (char= (elt href 0) #\()
                         (setf href (read-from-string href))
                         (setf href (list href))))
               (null t)
               (symbol (setf href (list (string href)))))
             (check-type as (or string symbol))
             (make-instance (node-class 'pi op nil) :href href :as as))))
    (handler-case
      (apply pi-function args)
      (error (condition)
             (xml-form-error op "error during pi interpretation of form: ~s: ~a"
                             args condition)))))
  

;;;
;;; the <?XML ... ?> processing instruction is handled specially
;;; others are just collected in the document by default.

(defMethod process-markup-element :after
           ((node pi-node) (stream t))
  (xml-node.append-element *parent-node* node)
  node)

(defMethod xml-node.append-element
           ((parent xml-node) (child pi-node))
  ;; these migrate upwards until some parent wants them
  (xml-node.append-element (xml-node.parent parent) child))

(defMethod xml-node.append-element
           ((parent xml-document) (child xml-xml))
  ;; these migrate upwards until some parent wants them
  (setf (xml-document.xml-pi parent) child)
  (setf (xml-document.standalone? parent) (xml-xml.standalone? child))
  child)



(defMethod initialize-instance :after
           ((self xml-namespace) &key)
  (unless (xml-node.name self)
    (setf (xml-node.name self)
          (location.name (first (xml-node.content self))))))

(defMethod location.name
           ((location null))
  nil)

(defMethod location.name
           ((location pathname))
  (pathname-name location))

(defMethod process-markup-element
           ((node xml-namespace) (stream t) &aux dtd)
  (when (xml-verbose 'process-markup-element)
    (format *trace-output* "~%<!-- binding namespace: ~a -->" node))
  (setf dtd (read-dtd-stream (xml-node.locations node)
                             (xml-namespace.as node)))
  (cond ((typep dtd 'dtd)
         dtd)
        (t
         (xml-cell-error node "can't locate the specified dtd.")
         nil)))

(defMethod print-object
           ((datum xml-xml) (stream t))
  (flet ((format-content ()
           (format stream
                   "~@[ VERSION='~a'~]~@[ standalone='~a'~]~@[ ENCODING='~a'~]"
                  (xml-xml.version datum)
                  (case (xml-xml.standalone? datum)
                    (:yes "yes")
                    (:no "no")
                    (t nil))
                  (xml-xml.encoding datum))))
    (cond (*xml-print-readably*
            (format stream "~A?XML" *start-tag-open*)
            (format-content)
            (format stream " ?~A" *tag-close*))
          (t
           (print-unreadable-object (datum stream :type t)
             (format-content))))))

(defMethod print-object
           ((datum xml-namespace) (stream t))
  (flet ((format-content ()
           (format stream "~@[ HREF=~s~]~@[ AS=~s~]"
                   (xml-node.locations datum)
                   (xml-node.name datum))))
    (cond (*xml-print-readably*
           (format stream "~A?XML-NAMESPACE " *start-tag-open*)
           (format-content)
           (format stream "?~A" *tag-close*))
          (t
           (print-unreadable-object (datum stream :type t)
             (format-content))))))

:EOF

