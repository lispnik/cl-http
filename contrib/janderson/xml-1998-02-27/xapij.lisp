;;; -*- package: ("XAPIJ") -*-
;;;
;;; this version (C) mecom gmbh 24.11.97
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.


#|
<DOCUMENTATION>
<CHRONOLOGY>
<DT>19971107 jaa
<DD>conformance to
    <A HREF="http://xml.datachannel.com/xml/dev/XAPIJ1p0.html">XapiJ</A> - or at
    least those aspects which make sense in CLOS.
</CHRONOLOGY>
<DESCRIPTION>
the XapiJ proposal comprises two suggestions: an interface and a class structure.
for the most part, it makes sense to adopt the proposal, since it supports
dialog between the programming-language domains. in particular, the proposed
interface to the xml-data tree (grove) and the parser interface are provided.
the proposal doesn't, prescribe a class structure, sothe "standard" interface
is simply provided as a secondary set of generic functions. note that the
parameter order for the "set-" functions is that of java and not that for setf.
i have not yet paid attention to return values for mutators.

there are, however, several
aspects of the proposal which make no sense for CLOS. they are implemented with
the appropriate analogous mechanisms.
<DL>
<DT>factory
<DD>special factory objects are not necessary. we just provide a parameter binding
for the parser class, which value the specified function uses.
<DT>IContent / IElement
<DD>i don't understand why they separate the interface to the containment hierarchy
    form that for the type and attribute hierarchy.
    are defined by the spec they are identical. i haven't looked at all the
    prototype implementations, but the msxml wrapper just split the access to the
    underlying com.ms.xml.Element, added a parent hierarchy which has no
    necessary relation to that of the referent element, and implemented the
    IContent interface for Element anyway... ?
<DT>get-type
<DD>returns a symbol rather than a string, since that is the name of the
    dtd-element which is bound and the latter makes no sense at all in the presence
    of interned symbols
<DT>attributes
<DD>the interface proposes encpasulating attributes, yet it specified only the
    functions for name and value. the prototype implementation extends this with
    methods for value conversion, type predicates, and comparison.
    since the context of use is limited, the polymorphic benefits of encapsulation
    are also limited. since clos supports a polymorphic comparison, this
    encapsulation is unnecessary.
    attributes are implemented instead as a p-list
</DL>
</DESCRIPTION>
</DOCUMENTATION>
 |#

(defPackage "XAPIJ" (:use "CL" #+:CCL "CCL" "XML-PARSER"))
(in-package "XAPIJ")


(export '(get-type
          set-type
          get-attributes
          add-atribute
          remove-attribute
          get-parent
          set-parent
          get-contents
          add-content
          remove-content
          get-name
          set-name
          get-value
          set-value
          create-ixml-processor
          read-xml
          ))

(defParameter *xml-processor-class* 'xml-processor)

(defClass xml-processor ()
  ())

(defMethod get-type
           ((node xml-element))
  (xml-element.name node))

(defMethod set-type
           ((node xml-element) (type symbol))
  (setf (xml-element.declaration node) (dtd-element type)))

(defMethod get-attributes
           ((node xml-element))
  (xml-element.attributes node))

(defMethod set-attributes
           ((node xml-element) (attributes list))
  (setf (xml-element.attributes node) attributes))

(defMethod add-attribute
           ((node xml-element) (name t) (value t))
  (setf (getf (xml-element.attributes node) name) value))

(defMethod remove-attribute
           ((node xml-element) (name t))
  (remf (xml-element.attributes node) name))

(defMethod get-parent
           ((node xml-element))
  (xml-element.context node))

(defMethod set-parent
           ((node xml-element) (parent xml-element))
  (setf (xml-element.context node) parent))

(defMethod set-parent
           ((node xml-element) (parent null))
  (setf (xml-element.context node) parent))

(defMethod get-contents
           ((node xml-element))
  (xml-element.content node))

(defMethod add-content
           ((node xml-element) (new xml-element) (preceeding-node xml-element))
  (if (setf preceeding-node (member preceeding-node (xml-element.content node)))
    (progn (push new (rest preceeding-node))
           (xml-element.content node))
    (if (xml-element.content node)
      (nconc (xml-element.content node) (list new))
      (setf (xml-element.content node) (list new)))))

(defMethod remove-content
           ((node xml-element) (old xml-element))
  (setf (xml-element.content node) (nremove node (xml-element.content))))


(defMethod get-type
           ((node xml-element))
  (xml-element.name node))

(defMethod set-type
           ((node xml-element) (type symbol))
  (setf (xml-element.declaration node) (dtd-element type)))

(defMethod get-attributes
           ((node xml-element))
  (xml-element.attributes node))

(defMethod set-attributes
           ((node xml-element) (attributes list))
  (setf (xml-element.attributes node) attributes))

(defMethod add-attribute
           ((node xml-element) (name t) (value t))
  (setf (getf (xml-element.attributes node) name) value))

(defMethod remove-attribute
           ((node xml-element) (name t))
  (remf (xml-element.attributes node) name))

(defMethod get-parent
           ((node xml-element))
  (xml-element.context node))

(defMethod set-parent
           ((node xml-element) (parent xml-element))
  (setf (xml-element.context node) parent))

(defMethod set-parent
           ((node xml-element) (parent null))
  (setf (xml-element.context node) parent))

(defMethod get-contents
           ((node xml-element))
  (xml-element.content node))

(defMethod add-content
           ((node xml-element) (new xml-element) (preceeding-node xml-element))
  (if (setf preceeding-node (member preceeding-node (xml-element.content node)))
    (progn (push new (rest preceeding-node))
           (xml-element.content node))
    (if (xml-element.content node)
      (nconc (xml-element.content node) (list new))
      (setf (xml-element.content node) (list new)))))

(defMethod remove-content
           ((node xml-element) (old xml-element))
  (setf (xml-element.content node) (nremove node (xml-element.content))))

(defMethod get-value
           ((node xml-element) (name t))
  (xml-element.get node name))

(defMethod set-value
           ((node xml-element) (name t) (value t))
  (xml-element.set node name value))



(defun create-ixml-processor (&rest initargs)
  (apply #'make-instance *xml-processor-class* initargs))

(defMethod read-xml
           ((processor xml-processor) (stream t))
  (read-markup-stream stream))

:EOF
