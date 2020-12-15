;;; -*- package: ("XML-PARSER") -*-
;;;
;;; this version (C) mecom gmbh 24.11.97
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.

#|
<DOCUMENTATION>
<DESCRIPTION>
definitions for
<UL>
<LI><CODE>XML-NODE</CODE>,
<LI><CODE>XML-NAMED-NODE</CODE>
<LI><CODE>XML-REFERENCE-NODE</CODE> and
<LI><CODE>XML-EXTERNAL-NODE</CODE>
</UL>
provide the base structure and behaviour for XML-form data structures and
for the inclusion of external data by reference.
</DESCRIPTION>
<CHRONOLOGY>
<DATE>19971218</DATE>
<DELTA>xml-external-node slots for public and system distinguished hard-coded.
 </DELTA>
</DOCUMENTATION>
|#


(in-package :XML-PARSER)

;;;
;;;

(defClass xml-node ()
  ((parent
    :initarg :parent :initform nil
    :accessor xml-node.parent
    :type (or null xml-node)
    :documentation
    "binds a link back to the containing node.")
   (content
    :initarg :children :initarg :content :initform nil
    :accessor xml-node.children :accessor xml-node.content
    :type #+:mcl (list (or string xml-node)) #-:mcl list
    :documentation
    "binds the node's content. this list can include primitive data and nodes
     among its members."))
  (:documentation
   "<CODE>XML-NODE</CODE> is the abstract root class for all objects which
    can appear in an xml document."))


(defMethod link-nodes
           ((parent xml-node) (child xml-node))
  (setf (xml-node.parent child) parent))

(defMethod link-nodes
           ((parent xml-node) (child t))
  )

(defMethod xml-node-link-content
           ((self xml-node) &aux content)
  (typecase (setf content (xml-node.content self))
    (cons  (dolist (c content) (link-nodes self c)))
    (t nil)))

(defMethod initialize-instance :after
           ((self xml-node) &key)
  (xml-node-link-content self))

(defGeneric xml-node.append-element
  (parent child)
  #|(:argument-precedence-order child parent)|#)

(defMethod xml-node.append-element
           ((parent xml-node) (child xml-node))
  (setf (xml-node.children parent)
        (nconc (xml-node.children parent) (list child)))
  (setf (xml-node.parent child) parent)
  child)

(defMethod xml-node.delete-element
           ((parent xml-node) (child xml-node))
  (setf (xml-node.children parent) (nremove child (xml-node.children parent)))
  (setf (xml-node.parent child) nil)
  child)


(defMethod xml-node.append-element
           ((parent null) (child t))
  (when (xml-verbose 'xml-node.append-element)
    (format *trace-output* "~%no parent: ~s." child))
  child)

(defMethod xml-node.append-element
           ((parent-list list) (child standard-object))
  (dolist (parent parent-list)
    (xml-node.append-element parent (copy-instance child))))

(defMethod xml-node.append-element
           ((parent-list list) (child t))
  (dolist (parent parent-list)
    (xml-node.append-element parent child)))

(defMethod xml-node.append-element
           ((parent t) (child t))
  child)

;;;
;;;

(defClass xml-datum ()
  ((name
    :initarg :name :initform nil
    :accessor xml-datum.name
    :type symbol
    :documentation
    "binds a name for a node. in a concrete class this bind a symbol or
     a reference to a dtd element.")
   (content
    :initarg :content :initform nil
    :accessor xml-datum.content :accessor xml-datum.relations
    :type t
    :documentation
    "binds the node's content. this can bind atomic data or data objects."))
  (:documentation
   "<CODE>XML-DATUM</CODE> binds a name an a data-object.
    it also serves as an abstract specializer which stipulates the data
    side of the pattern-matching interface:
    <UL>
    <LI><CODE>XML-DATUM.ATTRIBUTES</CODE>
    <LI><CODE>XML-DATUM.RELATIONS</CODE>
    <LI><CODE>XML-DATUM.TYPE</CODE>
    </UL>
    see also <CODE>XML-PATTERN</CODE>."))


(defClass xml-named-node (xml-datum xml-node)
  ((name
    :accessor xml-node.name))
  (:documentation
   "<CODE>XML-NAMED-NODE</CODE> is the abstract root class nodes which
    bind a name."))

(defMethod xml-node.name
           ((node null))
  nil)

(defMethod xml-node.name
           ((name list))
  (first name))
            
(defMethod xml-node.name
           ((name symbol))
  name)


;;;
;;;

(defClass xml-reference-node (xml-node)
  ((referent
    :initarg :referent :initform nil
    :accessor xml-node.referent
    :type xml-node))
  (:documentation
   "<CODE>XML-REFERENCE-NODE</CODE> is the abstract root class nodes refer
    to another node."))



(defClass xml-comment-node (xml-node)
  ((comments
    :initarg :comments :initform nil
    :accessor xml-node.comments
    :type #+:mcl (list xml-comment) #-:mcl list)))

;;;
;;;

(defClass xml-external-node (xml-node)
  ((locations
    :initarg :locations :initform nil
    :accessor xml-node.locations
    :type (list (or string url)))
   (public-id
    :initarg :public-id :initform nil
    :accessor xml-node.public-id
    :type string))
  (:documentation
   "<CODE>XML-EXTERNAL-NODE</CODE> is the abstract root class for
    dtd elements which may incorporate an external definition by
    reference.<BR>
    <CODE>XML-NODE.LOCATIONS</CODE> binds a set of locations
    which serve as a search path. the standard calls for a single location,
    which is bound as a length=1 list.
    <CODE>XML-NODE.PUBLIC-ID</CODE> binds the public id, should one be
    provided."))

(defMethod initalize-instance
           ((self xml-external-node) &rest initargs
            &key system (location system)
                 (locations (when location (list location))))
  (apply #'call-next-method self
         :locations locations
         initargs))

(defMethod xml-node.system
           ((node xml-external-node))
  (first (xml-node.locations node)))

(defMethod (setf xml-node.system)
           (datum (node xml-external-node))
  (setf (xml-node.locations node) (list datum)))

(defMethod (setf xml-node.system)
           ((datum list) (node xml-external-node))
  (setf (xml-node.locations node) datum))

;;;
;;; abstract class for dtd elements

(defClass dtd-node (xml-named-node)
  ()
  (:documentation
   "<CODE>DTD-NODE</CODE> is the abstract root class for all components of a
    document type description."))

;;;
;;; abstract interface class for patterns

(defClass xml-pattern ()
  ()
  (:documentation
   "the <CODE>XML-PATTERN</CODE> class serves as an abstract specializer
    which stipulates the pattern side of the pattern-matching interface:
    <UL>
    <LI><CODE>XML-PATTERN.ATTRIBUTES</CODE>
    <LI><CODE>XML-PATTERN.RELATIONS</CODE>
    <LI><CODE>XML-PATTERN.OCCURRENCE</CODE>
    <LI><CODE>XML-PATTERN.TYPE</CODE>
    <LI><CODE>XML-PATTERN.VARIABLE</CODE>
    </UL>
    see also <CODE>XML-DATUM</CODE>."))


;;;
;;; print functions

(defMethod print-object :around
           ((datum xml-node) (stream t))
  "an error handler for node printing"
  (handler-case (call-next-method)
    (error (condition)
           (ignore-errors
            (let ((string (with-output-to-string (stream)
                            (if *xml-print-readably*
                              (format stream "can't print instance of ~a: ~a"
                                      (type-of datum) condition)
                              (print-unreadable-object (datum stream
                                                              :type t)
                                (format stream "raised error: ~s/~a."
                                        condition
                                        (simple-condition-format-string
                                         condition)))))))
              (warn string)
              (write-string string stream)))
           nil)))

(defMethod print-object
           ((datum xml-node) (stream t))
  "the base method for the most primitive nodes just continues recursively"
  (if *xml-print-readably*
    (princ (xml-node.content datum) stream)
    (print-unreadable-object (datum stream :type t :identity t))))

(defMethod print-object
           ((datum xml-named-node) (stream t))
  "the base method for the most primitive nodes just continues recursively"
  (cond (*xml-print-readably*
         (print-start-tag datum stream)
         (princ (xml-node.content datum) stream)
         (print-end-tag datum stream))
        (t
         (print-unreadable-object (datum stream :type t :identity t)
           (format stream "~S" (xml-node.name datum))))))

(defMethod print-start-tag
           ((datum xml-named-node) (stream t)
            &aux (*readtable* *markup-type-readtable*))
  (format stream "~A~S~A" *start-tag-open* (xml-node.name datum) *tag-close*))

(defmethod print-end-tag
           ((datum xml-named-node) (stream t)
            &aux (*readtable* *markup-type-readtable*))
  (format stream "~A~S~A" *end-tag-open* (xml-node.name datum) *tag-close*))

#|
(let ((*xml-print-readably* t))
  (print (make-instance 'xml-node :content "ASDF"))
  (print (make-instance 'xml-named-node :content "QWER" :name 'test)))

 |#


:EOF


   
