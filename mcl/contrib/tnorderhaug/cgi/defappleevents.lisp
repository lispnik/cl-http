;; -*-Mode: LISP; Package: DEFAPPLEEVENTS; Base: 10; Syntax: Common-lisp -*-
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;
;; defappleevents -- high level access to appleevent records
;;
;; Author: Ray Pelletier
;;         pelletier@cmu.edu
;;

(defpackage "DEFAPPLEEVENTS"
  (:export "DEFAPPLEEVENT"
           "GET-APPLEEVENT-ATTRIBUTE"
           "GET-APPLEEVENT-ATTRIBUTE-AEDESC"
           "PUT-APPLEEVENT-ATTRIBUTE"
           "GET-APPLEEVENT-PARAMETER"
           "GET-APPLEEVENT-PARAMETER-AEDESC"
           "PUT-APPLEEVENT-PARAMETER"
           "PUT-APPLEEVENT-OPTIONAL-PARAMETER"))

(in-package "DEFAPPLEEVENTS")

;;
;;(defappleevent <name> <aeclass> <aeid> (<aeparameter>*) (<aeattribute>*))
;;
;;   <aeparameter> :== (<name> <key> {:desired-type <key>} {:optional t/nil})
;;   <aeattribute> :== (<name> <key> {:desired-type <key>})
;;

;structures used by defappleevent macro-expander
;
(defstruct parsed-defapplevent
  name
  class
  id
  parameters
  attributes)

(defstruct name-key name key)

(defstruct (parsed-attribute (:include name-key))
  (desired-type #$typeWildCard))

(defstruct (parsed-parameter (:include name-key))
  (desired-type #$typeWildCard)
  optional)

;(defvar *appleevent-parsings* (make-hash-table))

(defvar *collapse-primitive-descriptors* t)
;if non-nil we translate descriptors into lisp types when possible

;;
(defmacro defappleevent (name class id parameters attributes)
  (let ((params  (mapcar #'(lambda (x) (parse-parameter x)) parameters))
        (attribs (mapcar #'(lambda (x) (parse-attribute x)) attributes)))
    (let* ((a-d (make-parsed-defapplevent
                 :name       name
                 :class      (symbol-value class)
                 :id         (symbol-value id)
                 :parameters params
                 :attributes attribs)))
      `(progn
         ,(compose-appleevent-constructor a-d)
         ,(compose-appleevent-predicate a-d)
         ,@(compose-appleevent-accessors a-d)
         ',name))))

;; fns used to expand DEFAPPLEEVENT
;;
(defun parse-attribute (attribute)
  (apply #'make-parsed-attribute
         :name (first attribute)
         :key  (second attribute)
         (cddr attribute)))

(defun parse-parameter (parameter)
  (apply #'make-parsed-parameter
         :name (first parameter)
         :key  (second parameter)
         (cddr parameter)))

(defun compose-appleevent-constructor (a-d)
  (let ((constructor-name (intern (concatenate 'string
                                               "MAKE-"
                                               (string (parsed-defapplevent-name a-d)))))
        (target-holder    (gentemp "TARGET"))
        (aevent-holder    (gentemp "AEVT"))
        (aedesc-holder    (gentemp "ADDRESS-DESC"))
        (paramkeyspecs    (mapcar #'compose-keyspecs (parsed-defapplevent-parameters a-d)))
        (attribkeyspecs   (mapcar #'compose-keyspecs (parsed-defapplevent-attributes a-d))))
    (let ((param-setters 
           (mapcar #'(lambda (key param)
                       (compose-param-initializer aevent-holder key param))
                   paramkeyspecs
                   (parsed-defapplevent-parameters a-d)))
          (attrib-setters 
           (mapcar #'(lambda (key attrib)
                       (compose-attrib-initializer aevent-holder key attrib))
                   attribkeyspecs
                   (parsed-defapplevent-attributes a-d))))
      
      `(defun ,constructor-name (,aevent-holder ,target-holder &key
                                                (return-id #$kAutoGenerateReturnID)
                                                (transaction-id #$kAnyTransactionID)
                                                ,@paramkeyspecs ,@attribkeyspecs)
         (with-aedescs (,aedesc-holder)
           (ae-error (#_AECreateAppleEvent
                      ,(parsed-defapplevent-class a-d)
                      ,(parsed-defapplevent-id a-d)
                      (if (macptrp ,target-holder)
                        ,target-holder
                        (lisp->aedesc ,target-holder ,aedesc-holder))
                      return-id
                      transaction-id
                      ,aevent-holder)))
         ,@param-setters
         ,@attrib-setters
         ,aevent-holder))))

(defun compose-keyspecs (name-key)
  (let* ((name (name-key-name name-key))
         (present (intern (concatenate 'string (symbol-name name) "-PRESENT-P"))))
    (when (keywordp name)
      (setf name (intern (symbol-name name))))
    `(,name nil ,present)))

(defun compose-attrib-initializer (aevent-holder keyspec attrib)
  `(when ,(third keyspec)
     (put-appleevent-attribute
      ,aevent-holder
      ,(parsed-attribute-key attrib)
      ,(first keyspec))))

(defun compose-param-initializer (aevent-holder keyspec attrib)
  `(when ,(third keyspec)
     (,(if (parsed-parameter-optional attrib)
         'put-appleevent-optional-parameter
         'put-appleevent-parameter)
      ,aevent-holder
      ,(parsed-parameter-key attrib)
      ,(first keyspec))))
;;
(defun compose-appleevent-predicate (a-d)
  (let ((aevent-holder (gentemp "AEVENT"))
        (predicate-name (intern (concatenate 'string
                                             (string (parsed-defapplevent-name a-d))
                                             "-P"))))
    `(defun ,predicate-name (,aevent-holder)
       (let ((*collapse-primitive-descriptors* t))
         (and (eql ,(parsed-defapplevent-class a-d)
                   (get-appleevent-attribute ,aevent-holder #$keyEventClassAttr #$typekeyWord))
              (eql ,(parsed-defapplevent-id a-d)
                   (get-appleevent-attribute ,aevent-holder #$keyEventIdAttr #$typekeyWord)))))))

;;
(defun compose-appleevent-accessors (a-d)
  (let ((name (parsed-defapplevent-name a-d)))
    (nconc
     (mapcar #'(lambda (a) (compose-attribute-reader name a))
             (parsed-defapplevent-attributes a-d))
     (mapcar #'(lambda (a) (compose-attribute-aedesc-reader name a))
             (parsed-defapplevent-attributes a-d))
     (mapcar #'(lambda (a) (compose-attribute-writer name a))
             (parsed-defapplevent-attributes a-d))
     (mapcar #'(lambda (p) (compose-parameter-reader name p))
             (parsed-defapplevent-parameters a-d))
     (mapcar #'(lambda (p) (compose-parameter-aedesc-reader name p))
             (parsed-defapplevent-parameters a-d))
     (mapcar #'(lambda (p) (compose-parameter-writer name p))
             (parsed-defapplevent-parameters a-d)))))

(defun compose-attribute-reader (name attribute)
  (let ((aeholder    (gentemp "AEVENT"))
        (reader-name (intern (concatenate 'string
                                          (string name)
                                          "-"
                                          (string (parsed-attribute-name attribute))))))
    `(defun ,reader-name (,aeholder)
       (get-appleevent-attribute 
        ,aeholder
        ,(parsed-attribute-key attribute)
        ,(parsed-attribute-desired-type attribute)))))

(defun compose-attribute-aedesc-reader (name attribute)
  (let ((aeholder    (gentemp "AEVENT"))
        (descholder  (gentemp "AEDESC"))
        (reader-name (intern (concatenate 'string
                                          (string name)
                                          "-"
                                          (string (parsed-attribute-name attribute))
                                          "-AEDESC"))))
    `(defun ,reader-name (,descholder ,aeholder)
       (get-appleevent-attribute-aedesc
        ,aeholder
        ,(parsed-attribute-key attribute)
        ,(parsed-attribute-desired-type attribute)
        ,descholder))))

(defun compose-attribute-writer (name attribute)
  (let ((aeholder    (gentemp "AEVENT"))
        (dataholder  (gentemp "DATA"))
        (reader-name (intern (concatenate 'string
                                          (string name)
                                          "-"
                                          (string (parsed-attribute-name attribute))))))
    `(defun (setf ,reader-name) (,dataholder ,aeholder)
       (put-appleevent-attribute
        ,aeholder
        ,(parsed-attribute-key attribute)
        ,dataholder))))

(defun compose-parameter-reader (name parameter)
  (let ((aeholder    (gentemp "AEVENT"))
        (reader-name (intern (concatenate 'string
                                          (string name)
                                          "-"
                                          (string (parsed-parameter-name parameter))))))
    `(defun ,reader-name (,aeholder)
       (get-appleevent-parameter
        ,aeholder
        ,(parsed-parameter-key parameter)
        ,(parsed-parameter-desired-type parameter)))))

(defun compose-parameter-aedesc-reader (name parameter)
  (let ((aeholder    (gentemp "AEVENT"))
        (descholder  (gentemp "AEDESC"))
        (reader-name (intern (concatenate 'string
                                          (string name)
                                          "-"
                                          (string (parsed-parameter-name parameter))
                                          "-AEDESC"))))
    `(defun ,reader-name (,aeholder ,descholder)
       (get-appleevent-parameter-aedesc
        ,aeholder
        ,(parsed-parameter-key parameter)
        ,(parsed-parameter-desired-type parameter)
        ,descholder))))

(defun compose-parameter-writer (name parameter)
  (let ((aeholder    (gentemp "AEVENT"))
        (dataholder  (gentemp "DATA"))
        (reader-name (intern (concatenate 'string
                                          (string name)
                                          "-"
                                          (string (parsed-parameter-name parameter))
                                          "-AEDESC"))))
    `(defun (setf ,reader-name) (,dataholder ,aeholder)
       (,(if (parsed-parameter-optional parameter)
           'put-appleevent-optional-parameter
           'put-appleevent-parameter)
        ,aeholder
        ,(parsed-parameter-key parameter)
        ,dataholder))))

;;
;; access performed by these guys
;;
(defun get-appleevent-attribute (aevent attr-key desired-type)
  (let ((type)
        (size))
    (%stack-block ((type-ptr 4)
                   (size-ptr 4))
      (ae-error (#_AESizeOfAttribute aevent attr-key type-ptr size-ptr))
      (setf type (%get-ostype type-ptr))
      (setf size (%get-unsigned-long size-ptr)))
    (case type
      (#.#$typeAEList
       (with-aedescs (the-aelist)
         (ae-error (#_AEGetAttributeDesc aevent attr-key #$typeAEList the-aelist))
         (aelist->lisp the-aelist)))
      (#.#$typeAERecord
       (with-aedescs (the-aerecord)
         (ae-error (#_AEGetAttributeDesc aevent attr-key #$typeAERecord the-aerecord))
         (aerecord->lisp the-aerecord)))
      (t
       (%stack-block ((data size)
                      (actual-type 4)
                      (actual-size 4))
         (ae-error (#_AEGetAttributePtr aevent attr-key desired-type actual-type data size actual-size))
         (type&pointer->lisp type data (%get-long actual-size)))))))

(defun get-appleevent-attribute-aedesc (aevent attr-key desired-type aedesc)
  (ae-error (#_AEGetAttributeDesc aevent attr-key desired-type aedesc)))

(defun put-appleevent-attribute (aevent attr-key data)
  (if (macptrp data) ;;If macptr, assume it is a descriptor
    (ae-error (#_AEPutAttributeDesc aevent attr-key data))
    (with-aedescs (aedesc)
      (lisp->aedesc data aedesc)
      (ae-error (#_AEPutAttributeDesc aevent attr-key aedesc)))))

(defun get-appleevent-parameter (aevent param-key desired-type)
  (let ((type)
        (size))
    (%stack-block ((type-ptr 4)
                   (size-ptr 4))
      (ae-error (#_AESizeOfParam aevent param-key type-ptr size-ptr))
      (setf type (%get-ostype type-ptr))
      (setf size (%get-unsigned-long size-ptr)))
    (case type
      (#.#$typeAEList
       (with-aedescs (the-aelist)
         (ae-error (#_AEGetParamDesc aevent param-key #$typeAEList the-aelist))
         (aelist->lisp the-aelist)))
      (#.#$typeAERecord
       (with-aedescs (the-aerecord)
         (ae-error (#_AEGetParamDesc aevent param-key #$typeAERecord the-aerecord))
         (aerecord->lisp the-aerecord)))
      (t
       (%stack-block ((data size)
                      (actual-type 4)
                      (actual-size 4))
         (ae-error (#_AEGetParamPtr aevent param-key desired-type actual-type data size actual-size))
         (type&pointer->lisp type data (%get-long actual-size)))))))

(defun get-appleevent-parameter-aedesc (aevent param-key desired-type aedesc)
  (ae-error (#_AEGetParamDesc aevent param-key desired-type aedesc)))

(defun put-appleevent-parameter (aevent param-key data)
  (if (macptrp data) ;;If macptr, assume it is a descriptor
    (ae-error (#_AEPutParamDesc aevent param-key data))
    (with-aedescs (aedesc)
      (lisp->aedesc data aedesc)
      (ae-error (#_AEPutParamDesc aevent param-key aedesc)))))

(defun put-appleevent-optional-parameter (aevent param-key data)
  (put-appleevent-parameter aevent param-key data)
  (let ((*collapse-primitive-descriptors* nil)
        (optional-params))
    (declare (ignore-if-unused *collapse-primitive-descriptors*))
    (setf optional-params
          (rest
           (ignore-errors
            (get-appleevent-attribute aevent #$keyOptionalKeywordAttr #$typeAEList))))
    (unless (find param-key optional-params :key #'third)
      (put-appleevent-attribute aevent #$keyOptionalKeywordAttr
                                `(:aelist (:aedesc ,#$typeKeyword ,param-key) ,@optional-params)))))

;; Translate a LISP descriptor into an :aedesc record
;;
;;(lisp->aedesc <ldisc> aedesc)
;;
;;   <ldesc> :- data     ;;strings, symbols, integers, floats, pathnames
;;		(:aedesc desctype <ldesc>)
;;		(:aedesc desctype <macptr>)  ;;zone ptr only
;;		(:aedesc desctype <macptr> <size>)
;;		(:aelist <desc>*)
;;		(:aerecord {<key> <desc>}*)
;;		(:aedataaray (type size {<common> <size>}) <macptr> <size>)
;;		(:aepackedarray (type size {<common> <size>}) <macptr> <size>)
;;		;no;;(:aehandlearray type <handles> size)
;;		;NO;;(:aedescarray <macptr> <size>)
;;		;NO;;(:aekeydescarray <macptr> <size>)
;;              (:aeobject <desired-class> <container> <keyform> <keydata>)

(defmethod lisp->aedesc ((integer integer) descriptor)
  (%stack-block ((int-ptr 4))
    (%put-long int-ptr integer)
    (ae-error (#_AECreateDesc #$typeInteger int-ptr 4 descriptor))
    descriptor))

(defmethod lisp->aedesc ((string string) descriptor)
  (with-cstrs ((cstring string))
    (ae-error (#_AECreateDesc #$typeChar cstring (length string) descriptor))
    descriptor))

(defmethod lisp->aedesc ((symbol symbol) descriptor)
  (lisp->aedesc (symbol-name symbol) descriptor))

(defmethod lisp->aedesc ((char character) descriptor)
  (lisp->aedesc (string char) descriptor))

(defmethod lisp->aedesc ((float float) descriptor)
  (let ((float-string (format nil "~f" float)))
    (lisp->aedesc `(:aedesc ,#$TypeFloat ,float-string) descriptor)))

(defmethod lisp->aedesc ((pathname pathname) descriptor)
  (let ((fsspec (make-record (:fsspec :storage :pointer))))
    ;;fsmakefsspec doesn't seem to like stack allocated blocks
    (unwind-protect
      (lisp->aedesc `(:aedesc ,#$typeFSS ,(pathname->fsspec pathname fsspec) (record-size fssspec))
                    descriptor)
      (dispose-record fsspec))))

(defun pathname->fsspec (pathname fsspec)
  ;; NB: this will only work on full pathnames
  (let ((namestring 
         (namestring
          (translate-logical-pathname 
           (merge-pathnames pathname (user-homedir-pathname))))))
    (with-returned-pstrs ((pname namestring))
      (#_FSMakeFSSpec 0 0
       pname
       fsspec)))
  fsspec)

(defmethod lisp->aedesc ((list list) descriptor)
  (ecase (first list)
    (:aedesc
     (let ((desctype (second list))
           (data     (third  list)))
       (if (macptrp data)
         (let ((size (or (fourth list) (pointer-size data))))
           (ae-error (#_AECreateDesc desctype data size descriptor)))
         (list->aedesc-by-type desctype data descriptor))))
    
    (:aelist
     (list->aelist (rest list) descriptor))
    
    (:aerecord
     (list->aerecord (rest list) descriptor))
    
    (:aedataarray 
     (let* ((factor (second list))
            (data   (third list))
            (size   (or (fourth list) (pointer-size data))))
       (list->aedataarray factor data size descriptor)))
    
    (:aepackedarray
     (let* ((factor (second list))
            (data   (third list))
            (size   (or (fourth list) (pointer-size data))))
       (list->aepackedarray factor data size descriptor)))
    
    ;(:aehandlearray
    ; (let* ((type   (second list))
    ;        (data   (third list))
    ;        (size   (or (fourth list) (pointer-size data))))
    ;   (list->aeHandlearray type data size descriptor)))
    ;
    ;(:aedescarray
    ; (let* ((data   (second list))
    ;        (size   (or (third list) (pointer-size data))))
    ;   (list->aedescarray data size descriptor)))
    
    (:aeobject
     (let ((desired-class (second list))
           (container     (third list))
           (keyform       (fourth list))
           (keydata       (fifth list)))
       (lisp->aedesc
        `(:aedesc :|obj |
                  (:aerecord
                   :|want| (:aedesc ,#$typeType ,desired-class)
                   :|from| ,container
                   :|form| (:aedesc ,#$typeEnumerated ,keyform)
                   :|seld| ,keydata))
        descriptor)))
    ))

;;
;; We are given type and data from the list (:aedesc <type> <data>)
;; If <data> is compatible with <type> we pack the descriptor,
;; if not, we try to coerce.
;;;
(defmethod list->aedesc-by-type ((desctype t) data descriptor)
  (with-aedescs (temp)    ;assume coercion
    (lisp->aedesc data temp)
    (ae-error (#_AECoerceDesc temp desctype descriptor)))
  descriptor)

(defmethod list->aedesc-by-type ((desctype (eql #$typeBoolean)) data descriptor)
  (case data
    ((t nil)
     (%stack-block ((bool-ptr 1))
       (%put-byte bool-ptr (if data -1 0))
       (ae-error (#_AECreateDesc desctype bool-ptr 1 descriptor)))
     descriptor)
    (otherwise (call-next-method))))

(defmethod list->aedesc-by-type ((desctype (eql #$typeNull)) data descriptor)
  (declare (ignore data))
  (ae-error (#_AECreateDesc desctype (%null-ptr) 0 descriptor))
  descriptor)

(defmethod list->aedesc-by-type ((desctype (eql #$typeTrue)) data descriptor)
  (declare (ignore data))
  (ae-error (#_AECreateDesc desctype (%null-ptr) 0 descriptor))
  descriptor)

(defmethod list->aedesc-by-type ((desctype (eql #$typeFalse)) data descriptor)
  (declare (ignore data))
  (ae-error (#_AECreateDesc desctype (%null-ptr) 0 descriptor))
  descriptor)

;; If data is not a keyword then coerce 
;; (System 7.0.1 doesn't know how to coerce ostypes... oh well)

#+clicc (defvar *keyword-package* (find-package "KEYWORD"))

#+clicc (defun keywordp (thing)
          (and (symbol-p thing)
               (eql (symbol-package thing) *keyword-package*)))

(defun keyword->aedesc-by-type (desctype keyword descriptor)
  (%stack-block ((keyword-ptr 4))
    (%put-ostype keyword-ptr keyword)
    (ae-error (#_AECreateDesc desctype keyword-ptr 4 descriptor)))
  descriptor)

(defmethod list->aedesc-by-type ((desctype (eql #$typeEnumerated)) data descriptor)
  (if (keywordp data)
    (keyword->aedesc-by-type desctype data descriptor)
    (call-next-method)))

(defmethod list->aedesc-by-type ((desctype (eql #$typeKeyword)) data descriptor)
  (if (keywordp data)
    (keyword->aedesc-by-type desctype data descriptor)
    (call-next-method)))

(defmethod list->aedesc-by-type ((desctype (eql #$typeProperty)) data descriptor)
  (if (keywordp data)
    (keyword->aedesc-by-type desctype data descriptor)
    (call-next-method)))

(defmethod list->aedesc-by-type ((desctype (eql #$typeType)) data descriptor)
  (if (keywordp data)
    (keyword->aedesc-by-type desctype data descriptor)
    (call-next-method)))

;; Process address types
(defmethod list->aedesc-by-type ((desctype (eql #$typeApplSignature)) data descriptor)
  (if (keywordp data)
    (keyword->aedesc-by-type desctype data descriptor)
    (call-next-method)))

;;; Alias records 
(defmethod list->aedesc-by-type ((desctype (eql #$typeAlias)) data descriptor)
  (if (pathnamep data)
    (let ((alias (pathname->alias-handle data)))
      (unwind-protect
        (with-dereferenced-handles ((alias-pointer alias))
          (lisp->aedesc `(:aedesc ,#$typeAlias ,alias-pointer ,(pointer-size alias)) descriptor))
        (dispose-record alias))
      descriptor)
    (call-next-method)))

(defun pathname->alias-handle (pathname)
  (let ((fsspec (make-record (:fsspec :storage :pointer))))
    ;;fsmakefsspec doesn't like stack allocated blocks (what a mess!!)
    (unwind-protect
      (progn
        (pathname->fsspec (pathname pathname) fsspec)
        (%stack-block ((aliashandle 4))
          (#_NewAlias (%null-ptr) fsspec aliashandle)
          (%get-ptr aliashandle)))
      (dispose-record fsspec))))


;;
;; descriptor lists and keyword lists
;;
(defun list->aelist (list descriptor)
  (ae-error (#_AECreateList (%null-ptr) 0 nil descriptor))
  (dolist (var list descriptor)
    (with-aedescs (one-aedesc)
      (lisp->aedesc var one-aedesc)
      (ae-error (#_AEPutDesc descriptor 0 one-aedesc)))))

(defun list->aerecord (list descriptor)
  (ae-error (#_AECreateList (%null-ptr) 0 t descriptor))
  (do ((key (first list) (first list))
       (contents (second list) (second list)))
      ((null list) descriptor)
    (setf list (cddr list))
    (with-aedescs (contents-descriptor)
      (lisp->aedesc contents contents-descriptor)
      (ae-error (#_AEPutKeyDesc descriptor key contents-descriptor)))))

(defun list->aedataarray (factoring-info data data-size descriptor)
  (let* ((item-type (first factoring-info))
         (item-size (second factoring-info))
         (common-info (third factoring-info))
         (common-info-size (when (macptrp common-info)
                             (or (fourth factoring-info) (pointer-size common-info))))
         (size 8))
    (when (macptrp common-info)
      (incf size common-info-size))
    (%stack-block ((pointer size))
      ;; Initialize factoring record for AEPutArray to work?
      (%put-ostype pointer item-type)
      (%put-long pointer item-size 4)
      (when (macptrp common-info)
        (#_BlockMove common-info
         (%int-to-ptr (+ 8 (%ptr-to-int pointer)))
         common-info-size))
      ;;
      (ae-error (#_AECreateList pointer size nil descriptor))
      (ae-error (#_AEPutArray descriptor #$kAEDataArray
                 data item-type item-size
                 (/ data-size (+ item-size (- size 8)))))))
  descriptor)

(defun list->aepackedarray (factoring-info data data-size descriptor)
  (let* ((item-type (first factoring-info))
         (item-size (second factoring-info))
         (common-info (third factoring-info))
         (common-info-size (when (macptrp common-info)
                             (or (fourth factoring-info) (pointer-size common-info))))
         (size 8))
    (when (macptrp common-info)
      (incf size common-info-size))
    (%stack-block ((pointer size))
      ;; Initialize factoring record for AEPutArray to work?
      (%put-ostype pointer item-type)
      (%put-long pointer item-size 4)
      (when (macptrp common-info)
        (#_BlockMove common-info
         (%int-to-ptr (+ 8 (%ptr-to-int pointer)))
         common-info-size))
      ;;
      (ae-error (#_AECreateList pointer size nil descriptor))
      (ae-error (#_AEPutArray descriptor #$kAEPackedArray
                 data item-type item-size
                 (/ data-size (+ item-size (- size 8)))))))
  descriptor)

;untested
(defun list->aehandlearray (item-type data data-size descriptor)
  (%stack-block ((pointer 4))
    ;; Initialize factoring record for AEPutArray to work
    (%put-ostype pointer item-type)
    (ae-error (#_AECreateList pointer 4 nil descriptor))
    (ae-error (#_AEPutArray descriptor #$kAEHandleArray
               data item-type 0 (/ data-size 4))))
  descriptor)

;;
;this doesn't work like I'd expected
;the call to AEPutArray is not ignoring the itemType and itemSize parameters
;;
(defun list->aedescarray (aedesc-array size descriptor)
  (ae-error (#_AECreateList (%null-ptr) 0 nil descriptor))
  (ae-error (#_AEPutArray descriptor #$kAEDescArray
             aedesc-array #$typeKeyword
             (record-length :aedesc) (/ size (record-length :aedesc))))
  descriptor)

;ditto
(defun list->aekeydescarray (aekeydesc-array size descriptor)
  (ae-error (#_AECreateList (%null-ptr) 0 t descriptor))
  (ae-error (#_AEPutArray descriptor #$kAEKeyDescArray
             aekeydesc-array 0
             (record-length :aekeydesc) (/ size (record-length :aekeydesc))))
  descriptor)


;;
;; converts appleevent descriptor into LISP descriptor
;;

;; **Warning**
; If we don't know how to interpret the type, we allocate space
; from the heap to store the data and return the pointer.
; The user has to dispose of the pointer themselves.
;;
(defmethod type&pointer->lisp ((type t) pointer size)
  ;;default, make copy of pointer
  (let* ((pointer-copy (#_NewPtr size)))
    (#_BlockMove pointer pointer-copy size)
    `(:aedesc ,type ,pointer-copy)))

(defun construct-primitive-type (type thing)
  (if *collapse-primitive-descriptors*
    thing
    `(:aedesc ,type ,thing)))

(defmethod type&pointer->lisp ((type (eql #$typeBoolean)) pointer size)
  (declare (ignore size))
  (construct-primitive-type type (not (zerop (%get-byte pointer)))))

(defmethod type&pointer->lisp ((type (eql #$typeChar)) pointer size)
  (%stack-block ((chars (1+ size)))
    (#_BlockMove pointer chars size)
    (%put-byte chars 0 size)
    (construct-primitive-type type (%get-cstring chars))))

(defmethod type&pointer->lisp ((type (eql #$typeInteger)) pointer size)
  (declare (ignore size))
  (construct-primitive-type type (%get-long pointer)))

(defmethod type&pointer->lisp ((type (eql #$typeShortInteger)) pointer size)
  (declare (ignore size))
  (construct-primitive-type type (%get-word pointer)))

(defmethod type&pointer->lisp ((type (eql #$typeFloat)) pointer size)
  (type&sane->lisp type pointer size))

(defmethod type&pointer->lisp ((type (eql #$typeShortFloat)) pointer size)
  (type&sane->lisp type pointer size))

(defmethod type&pointer->lisp ((type (eql #$typeExtended)) pointer size)
  (type&sane->lisp type pointer size))

(defmethod type&pointer->lisp ((type (eql #$typeComp)) pointer size)
  (type&sane->lisp type pointer size))

(defun type&sane->lisp (type pointer size)
  (with-aedescs (sane-string-desc)
    (ae-error (#_AECoercePtr type pointer size #$typeChar sane-string-desc))
    (let ((the-handle (rref sane-string-desc :aedesc.dataHandle)))
      (with-dereferenced-handles ((sane-string (rref sane-string-desc :aedesc.dataHandle)))
        (construct-primitive-type
         type
         (read-from-string
          (let ((*collapse-primitive-descriptors* t))
            (type&pointer->lisp #$typeChar sane-string (#_GetHandleSize the-handle)))))))))

(defmethod type&pointer->lisp ((type (eql #$typeMagnitude)) pointer size)
  (declare (ignore size))
  (construct-primitive-type type (%get-unsigned-long pointer)))

(defmethod type&pointer->lisp ((type (eql #$typeFSS)) pointer size)
  (declare (ignore size))
  (construct-primitive-type type (%path-from-fsspec pointer)))

(defmethod type&pointer->lisp ((type (eql #$typeNull)) pointer size)
  (declare (ignore pointer size))
  `(:aedesc ,type))

(defmethod type&pointer->lisp ((type (eql #$typeTrue)) pointer size)
  (declare (ignore pointer size))
  `(:aedesc ,type))

(defmethod type&pointer->lisp ((type (eql #$typeFalse)) pointer size)
  (declare (ignore pointer size))
  `(:aedesc ,type))

(defmethod type&pointer->lisp ((type (eql #$typeEnumerated)) pointer size)
  (declare (ignore size))
  `(:aedesc ,type ,(%get-ostype pointer)))

(defmethod type&pointer->lisp ((type (eql #$typeKeyword)) pointer size)
  (declare (ignore size))
  `(:aedesc ,type ,(%get-ostype pointer)))

(defmethod type&pointer->lisp ((type (eql #$typeProperty)) pointer size)
  (declare (ignore size))
  `(:aedesc ,type ,(%get-ostype pointer)))

(defmethod type&pointer->lisp ((type (eql #$typeType)) pointer size)
  (declare (ignore size))
  `(:aedesc ,type ,(%get-ostype pointer)))

(defmethod type&pointer->lisp ((type (eql #$typeApplSignature)) pointer size)
  (declare (ignore size))
  `(:aedesc ,type ,(%get-ostype pointer)))

(defun aelist->lisp (the-aelist)
  (%stack-block ((count 4))
    (ae-error (#_AECountItems the-aelist count))
    (let ((items)
          (c (%get-long count)))
      (dotimes (n c `(:aelist ,@items))
        (push (aelist[n]->lisp the-aelist (- c n)) items)))))

(defun aelist[n]->lisp (aelist n)
  (let ((type)
        (size))
    (%stack-block ((type-ptr 4)
                   (size-ptr 4))
      (ae-error (#_AESizeOfNthItem aelist n type-ptr size-ptr))
      (setf type (%get-ostype type-ptr))
      (setf size (%get-unsigned-long size-ptr)))
    (case type
      (#.#$typeAEList
       (with-aedescs (another-aelist)
         (%stack-block ((keyw 4))
           (%put-ostype keyw #$typeWildCard)
           (ae-error (#_AEGetNthDesc aelist n #$typeAEList keyw another-aelist))
           (aelist->lisp another-aelist))))
      (#.#$typeAERecord
       (with-aedescs (another-aelist)
         (%stack-block ((keyw 4))
           (%put-ostype keyw #$typeWildCard)
           (ae-error (#_AEGetNthDesc aelist n #$typeAERecord keyw another-aelist))
           (aerecord->lisp another-aelist))))
      (t
       (%stack-block ((data size)
                      (actual-type 4)
                      (actual-size 4)
                      (keyw 4))
         (ae-error (#_AEGetNthPtr aelist n type keyw actual-type data size actual-size))
         (type&pointer->lisp type data (%get-long actual-size)))))))

(defun aerecord->lisp (aerecord)
  (%stack-block ((count 4))
    (ae-error (#_AECountItems aerecord count))
    (let ((items)
          (c (%get-long count)))
      (dotimes (n c `(:aerecord ,@items))
        (setf items
              (nconc (aerecord[n]->lisp aerecord (- c n)) items))))))

(defun aerecord[n]->lisp (aerecord n)
  (let ((type)
        (size))
    (%stack-block ((type-ptr 4)
                   (size-ptr 4))
      (ae-error (#_AESizeOfNthItem aerecord n type-ptr size-ptr))
      (setf type (%get-ostype type-ptr))
      (setf size (%get-unsigned-long size-ptr)))
    (case type
      (#.#$typeAEList
       (with-aedescs (aelist)
         (%stack-block ((keyw 4))
           (%put-ostype keyw #$typeWildCard)
           (ae-error (#_AEGetNthDesc aerecord n #$typeAEList keyw aelist))
           (list (%get-ostype keyw) (aelist->lisp aelist)))))
      (#.#$typeAERecord
       (with-aedescs (another-aerecord)
         (%stack-block ((keyw 4))
           (%put-ostype keyw #$typeWildCard)
           (ae-error (#_AEGetNthDesc aerecord n #$typeAERecord keyw another-aerecord))
           (list (%get-ostype keyw) (aerecord->lisp another-aerecord)))))
      (t
       (%stack-block ((data size)
                      (actual-type 4)
                      (actual-size 4)
                      (keyw 4))
         (ae-error (#_AEGetNthPtr aerecord n type keyw actual-type data size actual-size))
         (list (%get-ostype keyw) (type&pointer->lisp (%get-ostype actual-type) data (%get-long actual-size))))))))
