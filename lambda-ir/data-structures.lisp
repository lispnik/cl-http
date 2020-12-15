;;;   -*- Mode: LISP; Package: lambda-ir; BASE: 10; SYNTAX: ansi-common-lisp -*-

;;; (C) Copyright 1997, Andrew J. Blumberg (blumberg@ai.mit.edu).
;;;     All Rights Reserved.
;;;
;;;
;;;------------------------------------------------------------------- 
;;;
;;; Basic data-structures underlying system.
;;;

(in-package :lambda-ir)

;;;------------------------------------------------------------------- 
;;;
;;; Hashed arrays 
;;;

(defun initialize-hashed-array (hashed-array &key in-access-hash in-linear-structure in-name-structure (size 0))
  (with-slots (access-hash linear-structure name-structure) hashed-array
    (setf access-hash (or in-access-hash (make-hash-table :test 'equalp :size size))
          name-structure (or in-name-structure (make-adjustable-array size))
          linear-structure (or in-linear-structure (make-adjustable-array size)))))

(defun make-hashed-array (&key access-hash name-structure linear-structure (size 0))
  (let ((result (%create-unnamed-object 'hashed-array)))
    (initialize-hashed-array result 
                             :in-access-hash access-hash 
                             :in-name-structure name-structure
                             :in-linear-structure linear-structure
			     :size size)
    result))

(defgeneric hashed-array-length (hashed-array)
  (:documentation "Returns the length of HASHED-ARRAY."))

(defmethod hashed-array-length ((hashed-array hashed-array))
  (with-slots (linear-structure) hashed-array
    (length linear-structure)))

(defgeneric access-hashed-array (hashed-array key &key array-access if-does-not-exist)
  (:documentation "Accesses the entry of HASHED-ARRAY specified by KEY."))

(defmethod access-hashed-array ((hashed-array hashed-array) key &key array-access (if-does-not-exist :error))
  (declare (ignore array-access))
  (with-slots (access-hash linear-structure) hashed-array
    (let ((look-up (gethash key access-hash)))
      (if (or (null look-up)
              (>= look-up (length linear-structure)))
          (ecase if-does-not-exist
            (:error 
              (error "There is no hash entry in hashed-array ~s for key ~s." hashed-array key))
            (:soft
              nil))
          (aref linear-structure (gethash key access-hash))))))

(defmethod access-hashed-array ((hashed-array hashed-array) (key number) &key array-access (if-does-not-exist :error))
  (unless array-access
    (return-from access-hashed-array (call-next-method)))
  (with-slots (linear-structure name-structure) hashed-array
    (if (>= key (length linear-structure))
        (ecase if-does-not-exist
          (:error 
            (error "There is no hash entry in hashed-array ~s for key ~s." hashed-array key))
          (:soft
            nil))
        (values (aref linear-structure key)
                (aref name-structure key)))))

(defgeneric item-index-in-hashed-array (hashed-array key)
  (:documentation "Returns the index in HASHED-ARRAY given by KEY."))

(defmethod item-index-in-hashed-array ((hashed-array hashed-array) key)
  (with-slots (access-hash) hashed-array
    (gethash key access-hash)))

(defgeneric add-to-hashed-array (hashed-array item key)
  (:documentation "Adds ITEM to HASHED-ARRAY, accessible via KEY."))

(defmethod add-to-hashed-array ((hashed-array hashed-array) item key)
  (with-slots (access-hash linear-structure name-structure) hashed-array
    (setf (gethash key access-hash) (multi-vector-push-extend 
                                      key name-structure
                                      item linear-structure))))

(defgeneric update-hashed-array (hashed-array item key)
  (:documentation "Sets the element of HASHED-ARRAY keyed by KEY to ITEM."))

(defmethod update-hashed-array ((hashed-array hashed-array) item (key number))
  (with-slots (linear-structure) hashed-array
    (setf (aref linear-structure key) item)))

(defmethod update-hashed-array ((hashed-array hashed-array) item key)
  (with-slots (access-hash linear-structure) hashed-array
    (let ((in-p (gethash key access-hash)))
      (if in-p
          (setf (aref linear-structure in-p) item)
          (add-to-hashed-array hashed-array item key)))))

(defmacro looping-over-hashed-array ((hashed-array item-name name-name) &body body)
  `(with-slots (linear-structure) ,hashed-array
     (loop for ,item-name across linear-structure
           for ,name-name across name-structure
           do ,@body)))

;; numerological-hashed-arrays are hashed arrays which have numbers as the indexing elements; 
;; they are never accessed directly, although one might loop over their array part.

(defun make-numerological-hashed-array (&key access-hash name-structure linear-structure)
  (let ((numerological-hashed-array (%create-unnamed-object 'numerological-hashed-array)))
    (initialize-hashed-array numerological-hashed-array 
                             :in-access-hash access-hash
                             :in-name-structure name-structure
                             :in-linear-structure linear-structure)
    numerological-hashed-array))

(defmethod update-hashed-array ((hashed-array numerological-hashed-array) item key)
  (with-slots (access-hash linear-structure) hashed-array
    (let ((in-p (gethash key access-hash)))
      (if in-p
          (setf (aref linear-structure in-p) item)
          (add-to-hashed-array hashed-array item key)))))

(defmethod increment-hashed-array-value ((hashed-array numerological-hashed-array) key)
  (with-slots (access-hash linear-structure) hashed-array
    (let ((in-p (gethash key access-hash)))
      (if in-p
          (incf (aref linear-structure in-p))
          (add-to-hashed-array hashed-array 1 key)))))

;;;------------------------------------------------------------------- 
;;;
;;; Caching definitions
;;;

;; two possible different instantiations of the caching concept

;; the corner turns on speed for a-lists somewhere around 10 elements, depending on machine type.
;; however, they are much more compact, which is important for certain applications (see token stuff herein).

(defgeneric initialize-cache (caching-object)
  (:documentation "Initializes the storage of CACHING-OBJECT."))

(defmethod initialize-cache ((caching-object caching-mixin))
  (error "You must specify a particular type of cache."))

(defmethod initialize-cache ((caching-object table-caching-object))
  (with-slots (storage) caching-object
    (setf storage (make-hash-table :test 'equalp))))

(defmethod initialize-cache ((caching-object a-list-caching-object))
  (with-slots (storage) caching-object
    (setf storage '())))

(defmethod initialize-cache :after ((caching-object a-list-caching-object-with-freshness))
  (with-slots (freshness-storage) caching-object
    (setf freshness-storage '())))

(defmethod initialize-cache :after ((caching-object table-caching-object-with-freshness))
  (with-slots (freshness-storage) caching-object
    (setf freshness-storage (make-hash-table :test 'equalp))))

(defgeneric clear-cache (caching-object)
  (:documentation "Clears the storage of a caching object."))

(defmethod clear-cache ((caching-object table-caching-object))
  (with-slots (storage) caching-object
    (clrhash storage)))

(defmethod clear-cache :after ((caching-object table-caching-object-with-freshness))
  (with-slots (freshness-storage) caching-object
    (clrhash freshness-storage)))

(defmethod clear-cache ((caching-object a-list-caching-object))
  (initialize-cache caching-object))

(defgeneric access-cache (caching-object tag)
  (:documentation "Get item indexed by TAG from CACHING-OBJECT."))

(defmethod access-cache ((caching-object table-caching-object) tag)
  (with-slots (storage) caching-object
    (gethash tag storage)))

(defmethod access-cache ((caching-object a-list-caching-object) tag)
  (with-slots (storage) caching-object
    (cdr (assoc tag storage :test 'equalp))))

(defgeneric fresh-p (caching-object tag)
  (:documentation "Returns the VALIDITY of the caching object indexed by tag."))

(defmethod fresh-p ((caching-object table-caching-object-with-freshness) tag)
  (with-slots (freshness-storage) caching-object
    (gethash tag freshness-storage)))

(defmethod fresh-p ((caching-object a-list-caching-object-with-freshness) tag)
  (with-slots (freshness-storage) caching-object
    (cdr (assoc tag freshness-storage :test 'equalp))))
    
(defmethod fresh-p ((caching-object caching-mixin) tag)
  (declare (ignore tag))
  t)

(defgeneric set-freshness-value (caching-object tag boolean)
  (:documentation "Sets the freshness value of the caching-object in question."))

(defmethod set-freshness-value ((caching-object table-caching-object-with-freshness) tag boolean)
  (with-slots (freshness-storage) caching-object
    (setf (gethash tag freshness-storage) boolean)))

(defmethod set-freshness-value ((caching-object a-list-caching-object-with-freshness) tag boolean)
  (with-slots (freshness-storage) caching-object
    (set-a-list freshness-storage tag boolean)))

(defmacro mark-unfresh (object tag)
  `(set-freshness-value ,object ,tag nil))

(defmacro mark-fresh (object tag)
  `(set-freshness-value ,object ,tag t))
    
(defgeneric update-cache (object tag value)
  (:documentation "Updates the cache."))

(defmethod update-cache ((caching-object table-caching-object) tag cached-value)
  (with-slots (storage) caching-object
    (setf (gethash tag storage) cached-value)
    cached-value))

(defmethod update-cache ((caching-object a-list-caching-object) tag cached-value)
  (with-slots (storage) caching-object
    (set-a-list storage tag cached-value)
    cached-value))

(defmethod update-cache :after ((caching-object freshness-mixin) tag cached-value)
  (declare (ignore cached-value))
  (mark-fresh caching-object tag))

(defgeneric destructive-update-cache (object tag code)
  (:documentation "Destructively modifies the cache."))

(defmethod destructive-update-cache (caching-object tag code)
  (funcall code (access-cache caching-object tag)))

(defmethod destructive-update-cache :after ((caching-object freshness-mixin) tag cached-value)
  (declare (ignore cached-value))
  (mark-fresh caching-object tag))

(defmacro use-cached-value-if-available ((caching-object tag update-cache-p &optional (copy-p nil)) &body body)
  `(if ,update-cache-p
       (or (and (fresh-p ,caching-object ,tag) (access-cache ,caching-object ,tag))
           (if ,copy-p
               (update-cache ,caching-object ,tag (copy ,@body))
               (update-cache ,caching-object ,tag ,@body)))
       (or (access-cache ,caching-object ,tag) ,@body)))

;;;------------------------------------------------------------------- 
;;;
;;; Tagged arrays; can be accessed according to tags. 
;;;

(defgeneric initialize-tagged-array (tagged-object &key in-linear-object in-tags in-names size)
  (:documentation "Sets up the tagged-array object."))

(defmethod initialize-tagged-array ((tagged-array tagged-array) &key in-linear-object in-tags in-names size)
  (declare (ignore in-names))
  (unless size (setq size 0))
  (with-slots (linear-object tags) tagged-array
    (setf linear-object (or in-linear-object (make-adjustable-array size))
          tags (or in-tags (make-adjustable-array size)))
    (initialize-cache tagged-array))
  tagged-array)

(defmethod initialize-tagged-array ((tagged-array hashed-tagged-array) &key in-linear-object in-tags in-names size)
  (unless size (setq size 0))
  (with-slots (linear-object tags names) tagged-array
    (setf linear-object (or in-linear-object (make-adjustable-array size))
          names (or in-names (make-adjustable-array size))
          tags (or in-tags (make-adjustable-array size)))
    (initialize-cache tagged-array))
  tagged-array)

(defun make-tagged-array (&key linear-object tags size)
  (let ((tagged-array (%create-unnamed-object 'tagged-array)))
    (initialize-tagged-array tagged-array :in-linear-object linear-object :in-tags tags :size size)
    tagged-array))

(defun make-hashed-tagged-array (&key linear-object tags names size)
  (let ((tagged-array (%create-unnamed-object 'hashed-tagged-array)))
    (initialize-tagged-array tagged-array :in-linear-object linear-object :in-tags tags :in-names names :size size)
    tagged-array))

#|

(defgeneric produce-updated-tagged-array (tagged-object tag-or-tags)
  (:documentation "Updates tagged object."))

(defmethod produce-updated-tagged-array ((tagged-array tagged-array) tag-or-tags)
  (with-slots (linear-object tags) tagged-array
    (let ((output-array (make-adjustable-array)))
      (loop for idx upfrom 0
            for item-tags across tags
            do (cond ((set-intersection item-tags tag-or-tags)
                      (vector-push-extend idx output-array))))
      output-array)))

(defmethod produce-updated-tagged-array ((tagged-array hashed-tagged-array) tag-or-tags)
  (with-slots (linear-object tags names) tagged-array
    (let ((output-hashed-array (make-hashed-array)))
      (loop for idx upfrom 0
            for name across names
            for item-tags across tags
            do (cond ((set-intersection item-tags tag-or-tags)
                      (add-to-hashed-array output-hashed-array idx name))))
      output-hashed-array)))

|#

(defgeneric produce-updated-tagged-array (tagged-object tag-or-tags)
  (:documentation "Updates tagged object."))

(defmethod produce-updated-tagged-array ((tagged-array tagged-array) tag-or-tags)
  (with-slots (linear-object tags) tagged-array
    (let ((output-array (make-adjustable-array)))
      (loop for idx upfrom 0
            for item-tags across tags
            do (cond ((set-intersection item-tags tag-or-tags)
                      (vector-push-extend idx output-array))))
      output-array)))

(defmethod produce-updated-tagged-array ((tagged-array hashed-tagged-array) tag-or-tags)
  (with-slots (linear-object tags names) tagged-array
    (let ((old-array (access-cache tagged-array tag-or-tags))
	  (length (length linear-object)))
      (cond ((and old-array (= (1+ (hashed-array-length old-array)) length))
             (add-to-hashed-array old-array 
                                  (1- length)
                                  (aref names (1- length)))
             old-array)
            (t (let ((output-hashed-array (make-hashed-array :size length)))
                 (loop for idx upfrom 0
                       for name across names
                       for item-tags across tags
                       do (cond ((set-intersection item-tags tag-or-tags)
                                 (add-to-hashed-array output-hashed-array idx name))))
                 output-hashed-array))))))

(defgeneric access-tagged-array (tagged-array key tag &key array-access if-does-not-exist)
  (:documentation "Accesses TAGGED-ARRAY via KEY as if only items with tag TAG existed."))

(defun internal-access-tagged-array (tagged-array key tag if-does-not-exist)
  (let ((sequence (use-cached-value-if-available (tagged-array tag t)
                    (produce-updated-tagged-array tagged-array tag))))
    (if (>= key (length sequence))
        (ecase if-does-not-exist
          (:soft nil)
          (:error (error "The index ~s is out of range for tagged-array ~s under tag ~s." key tagged-array tag)))
        (elt sequence key))))

(defmethod access-tagged-array ((tagged-array tagged-array) key tag &key array-access (if-does-not-exist :error))
  (declare (ignore array-access))
  (with-slots (linear-object) tagged-array
    (let ((index (internal-access-tagged-array tagged-array key tag if-does-not-exist)))
      (when index
        (elt linear-object index)))))

(defun internal-access-hashed-tagged-array (tagged-array key tag array-access if-does-not-exist)
  (access-hashed-array
    (use-cached-value-if-available (tagged-array tag t)
      (produce-updated-tagged-array tagged-array tag))
    key
    :array-access array-access
    :if-does-not-exist if-does-not-exist))

(defmethod access-tagged-array ((tagged-array hashed-tagged-array) key tag &key array-access (if-does-not-exist :error))
  (with-slots (linear-object) tagged-array
    (let ((index (internal-access-hashed-tagged-array tagged-array key tag array-access if-does-not-exist)))
      (when index
        (elt linear-object index)))))
         
(defgeneric access-tags (tagged-array key tag &key array-access if-does-not-exist)
  (:documentation "Returns the tags for a given object."))

(defmethod access-tags ((tagged-array tagged-array) key tag &key array-access (if-does-not-exist :error))
  (declare (ignore array-access))
  (with-slots (tags) tagged-array
    (let ((index (internal-access-tagged-array tagged-array key tag if-does-not-exist)))
      (when index
        (aref tags index)))))

(defmethod access-tags ((hashed-tagged-array tagged-array) key tag &key array-access (if-does-not-exist :error))
  (with-slots (tags) hashed-tagged-array
    (let ((index (internal-access-hashed-tagged-array hashed-tagged-array key tag array-access if-does-not-exist)))
      (when index
        (aref tags index)))))

(defgeneric modify-tags (tagged-array key tag new-tags &key array-access if-does-not-exist)
  (:documentation "Adjusts the tags of a given object."))

(defmethod modify-tags ((tagged-array tagged-array) key tag new-tags &key array-access (if-does-not-exist :error))
  (declare (ignore array-access))
  (with-slots (tags) tagged-array
    (let ((index (internal-access-tagged-array tagged-array key tag if-does-not-exist)))
      (when index
        (loop for tag in (set-difference new-tags (listify (aref tags index)))
              do (mark-unfresh tagged-array tag))
        (setf (aref tags index) new-tags)))))

(defmethod modify-tags ((hashed-tagged-array hashed-tagged-array) key tag new-tags &key array-access (if-does-not-exist :error))
  (with-slots (tags) hashed-tagged-array
    (let ((index (internal-access-hashed-tagged-array hashed-tagged-array key tag array-access if-does-not-exist)))
      (when index
        (loop for tag in (set-difference new-tags (listify (aref tags index)))
              do (mark-unfresh hashed-tagged-array tag))
        (setf (aref tags index) new-tags)))))

(defgeneric get-index-in-hashed-tagged-array (tagged-array key tag)
  (:documentation "Returns the index for a named object in a tagged hashed array."))

(defmethod get-index-in-hashed-tagged-array ((tagged-array hashed-tagged-array) key tag)
  (item-index-in-hashed-array
    (use-cached-value-if-available (tagged-array tag t)
      (produce-updated-tagged-array tagged-array tag))
    key))

(defgeneric absolute-index (tagged-array key &key tag)
  (:documentation "Finds the index of key in tagged-array."))

;; this is a losing hack that needs to be removed as soon as possible.  AJB --- 4/24/97 00:23:27

(defmethod absolute-index ((tagged-array hashed-tagged-array) key &key (tag (default-tags)))
  (with-slots (names tags) tagged-array
    (loop for n downfrom (1- (length names)) to 0
          do (when (and (equalp key (aref names n)) (degenerate-member tag (aref tags n)))
               (return-from absolute-index n)))
    nil))

(defgeneric add-to-tagged-array (tagged-array thing-to-add tag)
  (:documentation "Adds THING-TO-ADD to TAGGED-ARRAY with tag TAG."))

(defmethod add-to-tagged-array ((tagged-array tagged-array) thing-to-add tag)
  (with-slots (linear-object tags) tagged-array
    (mark-unfresh tagged-array tag)
    (multi-vector-push-extend thing-to-add linear-object
                              tag tags)))

(defmethod add-to-tagged-array ((tagged-array tagged-array) thing-to-add (tag-list list))
  (with-slots (linear-object tags) tagged-array
    (loop for tag in tag-list
          do (mark-unfresh tagged-array tag))
    (multi-vector-push-extend thing-to-add linear-object
                              tag-list tags)))
    
(defgeneric add-to-hashed-tagged-array (tagged-array thing-to-add tag name)
  (:documentation "Adds THING-TO-ADD to TAGGED-ARRAY under tag TAG keyed by NAME."))

(defmethod add-to-hashed-tagged-array ((tagged-array hashed-tagged-array) thing-to-add tag name)
  (with-slots (linear-object tags names) tagged-array
    (mark-unfresh tagged-array tag)
    (multi-vector-push-extend thing-to-add linear-object
                              name names
                              tag tags)))

(defmethod add-to-hashed-tagged-array ((tagged-array hashed-tagged-array) thing-to-add (tag-list list) name)
  (with-slots (linear-object tags names) tagged-array
    (loop for tag in tag-list
          do (mark-unfresh tagged-array tag))
    (multi-vector-push-extend thing-to-add linear-object
                              name names
                              tag-list tags)))

(defparameter *array-adjustment-slop* .10)

(defun insert-adjust (thing array index)
  (let ((length (length array)))
    (unless (< index length)
      (adjust-array array (+ index (round (* *array-adjustment-slop* length)))))
    (unless (< index (fill-pointer array))
      (setf (fill-pointer array) (1+ index)))
    (setf (aref array index) thing)))

(defmethod insert-into-hashed-tagged-array ((tagged-array hashed-tagged-array) thing-to-add tag name index)
  (with-slots (linear-object tags names) tagged-array
    (let ((cached-array (access-cache tagged-array tag)))
      (update-hashed-array cached-array index name)
      (insert-adjust thing-to-add linear-object index)
      (insert-adjust name names index)
      (insert-adjust tag tags index))))

(defmethod insert-into-hashed-tagged-array ((tagged-array hashed-tagged-array) thing-to-add (tag-list list) name index)
  (with-slots (linear-object tags names) tagged-array
    (let ((cached-array (access-cache tagged-array tag-list)))
      (update-hashed-array cached-array index name)
      (insert-adjust thing-to-add linear-object index)
      (insert-adjust name names index)
      (insert-adjust tag-list tags index))))

(defgeneric tagged-array-length (tagged-array tag)
  (:documentation "Gets the length of TAGGED-ARRAY as viewed by TAG."))

(defmethod tagged-array-length ((tagged-array hashed-tagged-array) tag)
  (hashed-array-length
    (use-cached-value-if-available (tagged-array tag t)
      (produce-updated-tagged-array tagged-array tag))))

(defmethod tagged-array-length ((tagged-array tagged-array) tag)
  (length
    (use-cached-value-if-available (tagged-array tag t)
      (produce-updated-tagged-array tagged-array tag))))

(defmethod tagged-array-length ((tagged-array tagged-array) (tag (eql nil)))
  (with-slots (linear-object) tagged-array
    (length linear-object)))

(defmethod tagged-array-length ((tagged-array hashed-tagged-array) (tag (eql nil)))
  (with-slots (linear-object) tagged-array
    (length linear-object)))

(defgeneric get-view-of-tagged-array (tagged-array tag-or-tags)
  (:documentation "Returns an untagged structure from TAGGED-ARRAY according to TAG."))

(defmethod get-view-of-tagged-array ((hashed-tagged-array hashed-tagged-array) tag-or-tags)
  (use-cached-value-if-available (hashed-tagged-array tag-or-tags t)
    (produce-updated-tagged-array hashed-tagged-array tag-or-tags)))

(defmethod get-view-of-tagged-array ((tagged-array tagged-array) tag-or-tags)
  (use-cached-value-if-available (tagged-array tag-or-tags t)
    (produce-updated-tagged-array tagged-array tag-or-tags)))

(defmethod get-view-of-tagged-array (tagged-array (tag-or-tags (eql t)))
  (with-slots (linear-structure) tagged-array
    linear-structure))





