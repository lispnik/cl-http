;;; -*- :mode: lisp; package: ("XML-PARSER")
;;;
;;; this version (C) mecom gmbh 24.11.97
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.
;;;
;;; see "dtd-parser.lisp" for application
#|
<DOCUMENTATION>
<DESCRIPTION>
the "document" container for document definition streams
</DESCRIPTION>
<CHRONOLOGY>
<DATE>19971210</DATE>
 <DELTA>documents noew accept comments.
  </DELTA>
<DATE>19971218</DATE>
 <DELTA>this is now obsolete. standalone dtd documents don't make any real sense.
  the dtd is usually be read as extension of an xml document. as such a first
  class object for the document is not necessary. the dtd readding functions
  read directly into and return the respecitve dtd object itself.
</CHRONOLOGY>
</DOCUMENTATION>
|#

(in-package :xml-parser)

(defMethod node-class
           ((node (eql 'dtd-document)) (op t) (context t))
  *dtd-document-class*)

(defClass dtd-document (xml-comment-node)
  ((content
    :initarg :dtd :initform nil
    :accessor dtd-document.dtd :reader document.dtd)
   (url
    :initarg :url :initform nil
    :accessor dtd-document.url)))




(defMethod initialize-instance :after
           ((self dtd-document) &key)
  (setf (dtd-document.url self)
        (url-namestring (dtd-document.url self) self)))


(defMethod xml-node.append-element
           ((parent dtd-document) (child dtd))
  (when (dtd-document.dtd parent)
    (xml-validity-error parent "multiple dtd's: ~s" child))
  (setf (dtd-document.dtd parent) child)
  child)

(defMethod xml-node.append-element
           ((parent dtd-document) (child t))
  ;; discard everything except the dtd
  (warn "extraeneous dtd element ignored: ~s." child)
  child)

(defMethod xml-node.append-element
           ((parent dtd-document) (child xml-comment))
  (cache-comment-element parent child))

;; this remains here although it is spurious
(defMethod xml-node.append-element
           ((parent dtd-document) (child doctype))
  (when (or (doctype.public child) (doctype.system child))
    (warn "ignoring external dtd specification: ~a: ~@[~a] ~@[~a]."
          (doctype.name child) (doctype.public child) (doctype.system child)))
  (when (doctype.internal child)
    (warn "ignoring internal dtd specification: ~a."
          (doctype.name child)))
  (xml-node.append-element parent
                           (make-instance (node-class 'document-type-definition
                                                      'doctype
                                                      nil)
                             :name (doctype.name child)
                             :url (doctype.url child)
                             ;; since we're reading the dtd here, reset content
                             :elements nil
                             :entities nil
                             :notations nil
                             :reinitialize t))
  child)

  
:EOF

  
