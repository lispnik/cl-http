;;; -*- mode: LISP; package ("XML-PARSER")
;;;
;;; this version (C) mecom gmbh 24.11.97
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.

#|
<DOCUMENTATION>
<DESCRIPTION>
 global bindings
 </DESCRIPTION>
<CHRONOLOGY>
 <DATE>971124</DATE>
  <DELTA>*xml-warn-if-undefined* in dtd.dtd-qualified-element</DELTA>
 <DATE>19971125</DATE>
  <DELTA>
   <LI>dtd.dtd-element-reference (t symbol)
      changed the search strategy for reference elements:
      1. search the present dtd if one is present; if that fails
      2. look for a matching dtd as a) one from which the symbol's package is
         reachable and b) in which the element is defined; if that fails,
      3. otherwise, or if no dtd is present, make an autonomous reference
   </DELTA>
 <DATE>199711128</DATE>
  <DELTA> fixed DTD-ELEMENT args for DTD-RESERVED-DECLARATION </DELTA>
 <DATE>19971218</DATE>
  <DELTA>the 'as' slot is superfluous: a dtd is always loaded "as" something
   since the name in the doctype plays the same selective role as that in
   a namespace pi</DELTA>
 <DATE>19980223</DATE>
  <DELTA>patched dtd.dtd-element to prevent references to references</DELTA>
 </CHRONOLOGY>
</DOCUMENTATION>
|#



(in-package :XML-PARSER)

(defVar *dtd-write-element-references* nil)
(defVar *dtd-write-doctype* nil)

(defMethod node-class
           ((node (eql 'document-type-definition)) (op t) (context t))
  *document-type-definition-class*)

(defClass dtd-identity-class (standard-class)
  ((name&url-map
    :initform (make-hash-table :test #'equal)
    :initarg :name&url-map
    :allocation :class
    :accessor class-name&url-map
    :documentation
    "maps (or name url) -> dtd and dtd -> (cons name url)")
   (reinitialize? :initform nil
                  :initarg :reinitialize?
                  :accessor class-reinitialize?))
  (:documentation
   "dtd-specific metaclass which caches instances of all subclasses."))

(defMethod make-instance
           ((class dtd-identity-class) &rest initargs
            &key name url package
                 (reinitialize (class-reinitialize? class) reinitialize-provided)
            &allow-other-keys
            &aux instance url-namestring)
  (when reinitialize-provided
    (setf initargs (copy-list initargs))
    (remf initargs :reinitialize))
  (unless (setf url-namestring (url-namestring url class))
    (xml-validity-error class "dtd url value missing or invalid: ~s." initargs))
  (setf name (canonical-dtd-name name))

  (unless package
    (setf package (make-markup-package name))
    #| don't bother making a nickname for now
    (unless (or (null url-namestring)
                (find url-namestring (package-nicknames package)
                      :test #'string-equal))
      (rename-package package name
                      (cons url-namestring (package-nicknames package))))|#)

  (with-accessors ((name&url-map class-name&url-map))
                  class
    (cond ((setf instance (gethash url-namestring name&url-map))
           (when reinitialize
             (setf instance (apply #'reinitialize-instance instance
                                   :url url-namestring initargs))))
          (t
           (setf instance (apply #'call-next-method class
                                 :url url-namestring
                                 :package package
                                 initargs))
           (setf (gethash instance name&url-map) (cons name url-namestring))
           (setf (gethash url-namestring name&url-map) instance)
           instance))
    (pushnew instance (gethash name name&url-map))
    instance))

(defun dtd-instances
       (&aux instances)
  (maphash #'(lambda (key value) (declare (ignore key))
               (when (typep value 'dtd)
                 (pushnew value instances)))
           (class-name&url-map (find-class 'dtd)))
  instances)
;(mapc #'print (dtd-instances))

(defClass dtd (xml-named-node xml-comment-node)
  ((name
    :initarg :name
    :reader dtd.name :writer dtd.set-name
    :type string)
   (url
    :initarg :url
    :accessor dtd.url)
   (package
    :initarg :package :initform nil
    :accessor dtd.package)
   (content
    :initarg :elements  :initform nil
    :accessor dtd.elements
    :accessor dtd-elements #|wg. kompatibilitaet|#)
   (entities
    :initarg :entities :initform nil
    :accessor dtd.entities
    :type #+:mcl (list dtd-named-entity) #-:mcl list)
   (notations
    :initarg :notations :initform nil
    :accessor dtd.notations
    :type  #+:mcl (list dtd-notation) #-:mcl list)
   (root
    :initform nil
    :accessor dtd-root :accessor dtd.root
    :initarg :root)
   (file
    :initarg :file :initform nil
    :accessor dtd.file
    :documentation
    "binds the pathname of the file which defines the dtd. should be a url...")
   (generator
    :initform nil
    :accessor dtd.xml-generator)
   (creation-time
    :initform (get-universal-time)
    :accessor dtd.creation-time)
   (public-id
    :initform nil :initarg :public-id
    :accessor dtd.public-id)
   (system-id
    :initform nil :initarg :system-id
    :accessor dtd.system-id)
   (validate?
    :initform t :initarg :validate?
    :accessor dtd.validate?
    :type t))
  (:metaclass dtd-identity-class)
  (:documentation
   "a <CODE>DTD</CODE> instance collects the element definitions from a given
    document type definition."))

(defMethod copy-instance :after
           ((self dtd))
  (setf (dtd.elements self) (mapcar #'copy-instance (dtd.elements self))))

(defMethod dtd.package ((dtd null)) *markup-user-package*)

(defMethod %dtd-clear
           ((dtd dtd))
  (setf (dtd.notations dtd) nil
        (dtd.elements dtd) nil
        (dtd.entities dtd) nil))


(defMethod dtd ((name null)) nil)
(defMethod dtd ((names list)) (find-if #'dtd names))
(defMethod dtd ((name symbol)) (dtd (string name)))
(defMethod dtd ((datum dtd)) datum)
(defMethod dtd ((id pathname))
  (when (setf id (probe-file id))
    (dtd (namestring id))))
(defMethod dtd ((package package))
  (maphash #'(lambda (key dtd &aux (test nil))
               (declare (ignore key))
               (when (and (typep dtd 'dtd)
                          (setf test (dtd.package dtd))
                          (or (eq test package)
                              (find package
                                    (package-use-list test))))
                 (return-from dtd dtd)))
           (class-name&url-map (find-class 'dtd))))

(defMethod dtd
           ((name string) &aux (class (find-class 'dtd)) dtd)
  (when (find-if #'lower-case-p name)
    (setf name (string-upcase name)))
  (typecase (setf dtd (gethash name (class-name&url-map class)))
    (dtd dtd)
    (cons (first dtd))))

(defMethod dtd ((id t)) (dtd (canonical-dtd-name id)))

(defMethod element.dtd
           ((element symbol) &aux (element-package (symbol-package element)))
  (maphash #'(lambda (key dtd &aux (dtd-package nil))
               (declare (ignore key))
               (when (and (typep dtd 'dtd)
                          (setf dtd-package (dtd.package dtd))
                          (or (eq element-package dtd-package)
                              (find element-package
                                    (package-use-list dtd-package)))
                          (find element (dtd.elements dtd)
                                :key #'dtd-element.name))
                 (return-from element.dtd dtd)))
           (class-name&url-map (find-class 'dtd)))
  nil)



(defMethod dtd.file :before
           ((dtd dtd))
  (unless (slot-value dtd 'file)
    (setf (slot-value dtd 'file) (dtd.file (dtd.url dtd)))))

(defMethod dtd.file
           ((file symbol))
  (dtd.file (string-downcase (string file))))

(defMethod dtd.file
           ((file string))
  (dtd.file (pathname file)))

(defMethod dtd.file
           ((file pathname))
  (merge-pathnames file *dtd-pathname-defaults*))

(defMethod canonical-dtd-name ((as string)) (string-upcase as))
(defMethod canonical-dtd-name ((as symbol)) (string as))
(defMethod canonical-dtd-name ((as null))
  (xml-cell-error nil "illegitimate dtd name: ~s." as))
(defMethod canonical-dtd-name ((as t))
  (xml-cell-error nil "illegitimate dtd name: ~s." as))


(defMethod (setf dtd.name)
           (name (dtd dtd) &aux old (class (class-of dtd)))
  (setf name (canonical-dtd-name name))
  (when (setf old (gethash dtd (class-name&url-map class)))
    (remhash (first old) (class-name&url-map class)))
  (setf (gethash dtd (class-name&url-map class))
        (cons name (url-namestring (dtd.url dtd) dtd)))
  (pushnew dtd (gethash name (class-name&url-map class)))
  (dtd.set-name name dtd))



(defMethod dtd.validate?
           ((dtd t))
  nil)

(defMethod delete-dtd
           ((dtd t))
  (delete-dtd (dtd dtd)))

(defMethod delete-dtd
           ((dtd null))
  nil)

(defMethod delete-dtd
           ((dtd dtd) &aux (class (class-of dtd)) name&url)
  (when (setf name&url (gethash dtd (class-name&url-map class)))
    (remhash dtd (class-name&url-map class))
    (remhash (rest name&url) (class-name&url-map class))
    (when (null (setf (gethash (first name&url) (class-name&url-map class))
                      (remove dtd (gethash (first name&url)
                                           (class-name&url-map class)))))
      (remhash (first name&url) (class-name&url-map class)))
    dtd))

(defMethod (setf dtd)
           ((value null) (dtd t))
  (when (setf dtd (dtd dtd))
    (setf (dtd dtd) nil)))

(defMethod (setf dtd)
           ((value null) (dtd (eql t)))
  "delete all dtd definitions"
  (let ((class (find-class 'dtd)))
    (clrhash (class-name&url-map class))))
;(setf (dtd t) nil)

(defMethod (setf dtd)
           ((value null) (dtd dtd))
  (delete-dtd dtd))

(defmethod print-object ((object dtd) stream)
  (if (and *xml-print-readably* (dtd.elements object))
    (write-dtd object stream)
    (print-unreadable-object (object stream :identity nil :type t)
      (format stream "~A <~s> {~d}"
              (dtd.name object) (dtd.url object)
              (length (dtd.elements object))))))

(defun write-dtd (object &optional (file-or-stream t)
                         &aux (*xml-print-dtd-attlists* t)
                              (*package* (dtd.package object)))
  (labels ((write-doctype (stream)
             (format stream "<!DOCTYPE ~a>" (dtd.name object)))
           (write-stream (stream)
             (when *dtd-write-doctype*
               (write-doctype stream))
             (dolist (entity (dtd.entities object))
               (when *xml-preserve-comments*
                 (print entity stream)
                 (dolist (comment (getf (xml-node.comments object) entity))
                   (print comment))))
             (dolist (element (dtd.elements object))
               (typecase element
                 (dtd-element-reference
                  (when *dtd-write-element-references*
                    (print element stream)))
                 (t
                  (print element stream)))
               (when *xml-preserve-comments*
                 (dolist (comment (getf (xml-node.comments object) element))
                   (print comment)))))
           (write-file (file)
             (with-open-file (stream file :direction :output)
               (write-stream stream))))
    (typecase file-or-stream
      (null (write-file (dtd.file object)))
      (string (write-file file-or-stream))
      (pathname (write-file file-or-stream))
      (stream (write-stream file-or-stream))
      (t (write-stream *standard-output*)))))


      
(defun defined-dtd-elements
       ()
  (apply #'append (mapcar #'dtd.elements (dtd-instances))))


;;;
#| element lookup
 dtd's are indexed by url to make it possible for multiple dtd's with the
 same document type name, but different content, to be held simultaneously.
 a dtd must be recognised in order to map element names to the correct
 element models (through dtd-element's)
 a link to a dtd must be established under one of three circumstances:
 <OL>
 <LI>the dtd is itself being read</LI>
 <LI>the dtd was specified as the document type for an xml document which is
     being read.</LI>
 <LI>the dtd was specified as a supplementary dtd with a namespace
     specification</LI>
 </OL>
 in each case the relation between an element name and the element definition
 must be resolve slightly differently.

 the base state - when nothing is being read - can either establish a
 global, default dtd or it can specify a null dtd.

 <DL>
 <DT>reading a dtd
 <DD>
 when a dtd itself is read, it supplants the global dtd and installs as the
 active reader a package with a name identical to its own.
 the read elements are named from this package and interned in the dtd by
 virtue of stream succession. an existing package will be reused if it exists.
 if none exists a new one will be created.
 this means that more than one element definition may exist with the same
 name, but different models, in distinct dtd's.

 <DT>reading an xml document
 <DD>
 if the document declares a dtd, it will be located and established along with
 its package as the active dtd and package. this means that symbols are read
 by default into that package. they are then compatible with the dtd and
 will be resolved with respect to it.
 should a processing instruction appear for an alternative dtd, the dtd is
 read and the nickname is associated with the package if it is not already.
 this means that unqualified symbols are read into the default package, as
 before. qualified symbols are read into the explicitly specified package
 and resolved as specified below.
 <DT>in the 'presence' of alternative namespaces.
 <DD>
 <OL>
 <LI> when reading a dtd, element definitions with a type named from another
 package are disallowed. an element referenced must be resolved with respect to
 the dtd implied by the name's symbol-package.
 <LI> when reading an xml document, element definitions resolve with respect
 to the declared dtd if the packages are identical and to the dtd implied by
 the name's symbol-package if the packages are not identical.
 </OL>
 </DL>
 
 |#


(defMethod dtd-element
           ((id symbol))
  (if (reserved-name-p id)
    (dtd-reserved-declaration id)
    (dtd.dtd-element *dtd* id)))

(defMethod dtd-element
           ((id list))
  (mapcar #'dtd-element id))

(defMethod dtd-element
           ((element dtd-element))
  element)

(defMethod dtd.dtd-element
           ((dtd null) (id symbol))
  ;; if no dtd is specified, then simply generate an 'uninterned' element
  (if (setf dtd (dtd (symbol-package id)))
    (dtd.dtd-element dtd id)
    (make-instance (node-class 'element-decl id nil)
      :qualified-name id
      :name id)))

(defMethod dtd.dtd-element
           ((dtd dtd) (id symbol) &aux element new-dtd)
  ;; either the element is present in the dtd, or search for a dtd
  ;; in which it is present.
  (flet ((element-not-present ()
           (progn (xml-validity-error dtd "element not present in dtd: ~s" id)
                  (setf element
                        (make-instance (node-class 'element-decl id dtd)
                          :qualified-name id
                          :name id))
                  (xml-node.append-element dtd element)
                  element)))
    (if (setf element
              (find-if #'(lambda (def)
                           (typecase def
                             (dtd-element-reference nil)
                             (dtd-element (eq id (dtd-element.name def)))
                             (t nil)))
                       (dtd.elements dtd)))
      (let ((q-name (build-qualified-dtd-element-name dtd id)))
        (or (dtd.dtd-qualified-element dtd q-name)
            (let ((element (make-instance
                             (node-class 'element-decl id dtd)
                             :qualified-name q-name
                             :name id)))
              (xml-node.append-element dtd element)
              element)))
      (if (setf new-dtd (dtd (symbol-package id)))
        (if (eq dtd new-dtd)
          (element-not-present)
          (dtd.dtd-element new-dtd id))
        (element-not-present)))))

(defMethod dtd.dtd-element
           ((dtd dtd) (id string))
  (dtd.dtd-element dtd (intern (string-upcase id) (dtd.package dtd))))

(defMethod dtd.dtd-element
           ((dtd t) (id t) &aux new)
  (typecase (setf new (dtd dtd))
    (dtd (dtd.dtd-element new id))
    (t (error "can't locate dtd: ~s." dtd))))
  
(defMethod dtd.dtd-qualified-element
           ((dtd dtd) (q-name symbol))
  (cond ((find q-name (dtd.elements dtd)
               :key #'dtd-element.qualified-name))
        (*xml-warn-if-undefined*
         (xml-validity-error dtd "element not present in dtd: ~s" q-name))))



(defMethod dtd-entity
           ((id symbol))
  (dtd.dtd-entity *dtd* id))

(defMethod dtd.dtd-entity
           ((dtd null) (id symbol))
  (make-instance 'dtd-named-entity
    :name id
    :content ""
    :parent nil))

(defMethod dtd.dtd-entity
           ((dtd dtd) (id symbol))
  (cond ((find id (dtd.entities dtd) :key #'xml-node.name))
        (t
         (xml-validity-error dtd "entity not present in dtd: ~s." id)
         (dtd.dtd-entity nil id))))


(defMethod dtd.dtd-notation
           ((dtd null) (id symbol))
  (make-instance 'dtd-notation
    :name id
    :content ""
    :parent nil))

(defMethod dtd.dtd-notation
           ((dtd dtd) (id symbol))
  (cond ((find id (dtd.notations dtd) :key #'xml-node.name))
        (t
         (xml-validity-error dtd "notation not present in dtd: ~s." id)
         (dtd.dtd-notation nil id))))


(defMethod xml-node.append-element
           ((*dtd* dtd) (child t))
  (if (xml-verbose 'xml-node.append-element)
    (warn "ignoring illegitimate dtd element: ~s." child)))

;;;
;;; element references permit forward references within a dtd
;;; if the element is not yet known, then the symbol is held

(defMethod dtd-element-reference
           ((id symbol))
  (if (reserved-name-p id)
    (dtd-reserved-declaration id)
    (dtd.dtd-element-reference *dtd* id)))


(defMethod dtd.make-element-reference
           ((dtd dtd) id q-name &aux element)
  (setf element (make-instance (node-class 'element-decl-ref id dtd)
                  :name id
                  :qualified-name q-name))
  (xml-node.append-element dtd element)
  element)

(defMethod dtd.dtd-element-reference
           ((dtd t) (id symbol)
            &aux new-dtd
            (q-name (build-content-element-reference-name *parent-node* id)))
  ;; either find the qualified reference, find a dtd with the unqualified
  ;; and proceed from there, or make a reference w/o dtd.
  (flet ((new-element (dtd) (dtd.make-element-reference dtd id q-name)))
    (cond ((find q-name (dtd.elements dtd)
                 :key #'dtd-element.qualified-name))
          ((setf new-dtd (element.dtd id))
           (or (find q-name (dtd.elements new-dtd)
                     :key #'dtd-element.qualified-name)
               (new-element new-dtd)))
          (t 
           (new-element dtd)))))



(defMethod dtd.dtd-element-reference
           ((dtd null) (id symbol)
            &aux new-dtd
            (q-name (build-content-element-reference-name *parent-node* id)))
  ;(print (list 'd-e-r :name id :q-name q-name))
  (if (setf new-dtd (element.dtd id))
    (or (find q-name (dtd.elements new-dtd)
              :key #'dtd-element.qualified-name)
        (dtd.make-element-reference new-dtd id q-name))
    (make-instance (node-class 'element-decl-ref id nil)
      :name id
      :qualified-name q-name)))

(defMethod dtd-add-element
           ((dtd dtd) (element dtd-element) &aux elements)
  (if (setf elements (dtd.elements dtd))
    (nconc elements (list element))
    (setf (dtd.elements dtd) (list element)))
  (setf (xml-node.parent element) dtd)
  element)

(defMethod dtd-remove-element
           ((dtd dtd) (element dtd-element))
  (setf (dtd.elements dtd)
        (delete (dtd-element.qualified-name element) (dtd.elements dtd)
                :key #'dtd-element.qualified-name))
  element)


(defMethod xml-node.append-element
           ((*dtd* dtd) (child dtd-element)
            &aux (name (dtd-element.name child)) old)
  (when (xml-verbose 'xml-node.append-element)
    (format t "~%installing: ~s: ~s." *dtd* child))
  ;; the name of the element must be visible in the dtd's package
  (unless (eq (find-symbol (string name) (dtd.package *dtd*)) name)
    (xml-form-error *dtd* "namespace mismatch: ~s." name))
  (cond ((find child (dtd.elements *dtd*)))
        ((setf old
                   (find (dtd-element.qualified-name child) (dtd.elements *dtd*)
                         :key #'dtd-element.qualified-name))
         (when *xml-warn-if-redefine*
           (xml-validity-error *dtd* "multiply defined element: ~a (as ~s/~s)."
                               child (dtd-element.qualified-name child) old))
         (dtd-remove-element *dtd* old)
         (dtd-add-element *dtd* child))
        (t
         (dtd-add-element *dtd* child)))
  child)


#|
 qualified naming of dtd elements depends on two things
 <OL>
 <LI>the name is placed in a dtd-specific package by the reader, which yields
     uniqueness among dtd's
 <LI>the model hierarchy introduces qualification relative to the parent
     element.
 </OL>
 this makes it possible to ascribe attributes to a particular member element
 as well as to the element in general.
 |#

(defun build-qualified-dtd-name (name &key (dtd *dtd*) path)
  ;; dtd-name/path/slotname. Basisklasse im DTD, daher (cdr path)
  (if *dtd*
    (let ((pathstr (if (cdr path) (format nil "~{~A/~}" (cdr path)) "")))
      (intern (format nil "~A/~A~A" (dtd.name dtd) pathstr name)))
    name))

(defMethod build-qualified-dtd-element-name
           ((dtd dtd) (name symbol))
  ;; the default method just forces the symbol into the dtd's package
  (let ((package (dtd.package dtd)))
    (if (eq (symbol-package name) package)
      name
      (intern (string name) package))))

(defMethod build-qualified-dtd-element-name
           ((dtd null) (name symbol))
  (if (setf dtd (dtd (symbol-package name)))
    (build-qualified-dtd-element-name dtd name)
    name))

(defun build-qualified-element-name
       (name)
  (build-qualified-dtd-element-name *dtd* name))


(defun build-element-reference-name
       (name)
  (build-content-element-reference-name *parent-node* name))

(defMethod build-content-element-reference-name
           ((dtd dtd) (name symbol))
  (build-qualified-dtd-element-name dtd name))

(defMethod build-content-element-reference-name
           ((parent dtd-element) (name symbol))
  (build-content-element-reference-name (dtd-element.name parent) name))

(defMethod build-content-element-reference-name
           ((parent symbol) (name symbol))
  (or (get parent name)
      (setf (get parent name) (intern (format nil "~a.~a" parent name)
                                      (symbol-package name)))))

(defMethod build-content-element-reference-name
           ((parent null) (name symbol))
  name)

(defMethod build-content-element-reference-name
           ((parent t) (name symbol))
  name)




:EOF
