;;;   -*- Mode: LISP; Package: lambda-ir; BASE: 10; SYNTAX: ansi-common-lisp -*-

;;; (C) Copyright 1997, Andrew J. Blumberg (blumberg@ai.mit.edu).
;;;     All Rights Reserved.
;;;
;;;
;;;------------------------------------------------------------------- 
;;;
;;; Definitions for documents and related notions.
;;;

;;;------------------------------------------------------------------- 
;;;
;;; Tokens and lexical features 
;;;

(in-package :lambda-ir)

;;; parameters

(defparameter *hard-sparsification-bound* 37)
(defparameter *sparsification-fraction* 0.02)

;;;

(defun make-token-feature ()
  (%create-unnamed-object 'token-feature))

(defun make-simple-lexical-feature (in-datum use-in-mask use-out-mask)
  (let ((lexical-feature (%create-unnamed-object 'simple-lexical-feature)))
    (with-slots (datum in-mask out-mask)
                lexical-feature
      (setf datum in-datum
            in-mask use-in-mask
            out-mask use-out-mask))
    (initialize-cache lexical-feature)
    (initialize-token-stat-storage lexical-feature)
    lexical-feature))

(defun make-general-lexical-feature (in-datum in-masks in-index in-combination-function)
  (let ((lexical-feature (%create-unnamed-object 'general-lexical-feature)))
    (with-slots (datum masks index-for-masks combination-function)
                lexical-feature
      (setf datum in-datum
            masks in-masks
            index-for-masks in-index
            combination-function in-combination-function)
      (initialize-cache lexical-feature)
      (initialize-token-stat-storage lexical-feature)
      lexical-feature)))

(defun make-token (datum &key (stat-storage nil))
  (let ((token (make-instance 'skinny-token-mixin :datum datum)))
    (initialize-cache token)
    (initialize-token-stat-storage token :default-value stat-storage)
    token))

(defun make-lexical-feature-set (name constraints in-lexical-features in-doc-function in-feature-function)
  (let ((lexical-feature-set (make-instance 'lexical-feature-set)))
    (with-slots (object-name pre-applied-constraints lexical-features document-storage-function feature-storage-function)
                lexical-feature-set
      (setf object-name name 
            pre-applied-constraints constraints
            lexical-features in-lexical-features
            document-storage-function in-doc-function
            feature-storage-function in-feature-function))
    lexical-feature-set))

;;;------------------------------------------------------------------- 
;;;
;;; Token clusters
;;;

(defun make-token-cluster (&key (in-hard-sparsification-bound *hard-sparsification-bound*) 
				(in-sparsification-fraction *sparsification-fraction*)
				access-hash linear-structure)
  (let ((hashed-array (%create-unnamed-object 'token-cluster)))
    (initialize-hashed-array hashed-array :in-access-hash access-hash :in-linear-structure linear-structure)
    (with-slots (hard-sparsification-bound sparsification-fraction) hashed-array
      (setf hard-sparsification-bound in-hard-sparsification-bound
            sparsification-fraction in-sparsification-fraction))
    hashed-array))

(defmethod number-of-tokens ((token-cluster token-cluster))
  (with-slots (linear-structure) token-cluster
    (length linear-structure)))

(defgeneric get-hard-sparsification-bound (token-cluster)
  (:documentation "Returns the absolute bound for sparsifying bit-vectors in TOKEN-CLUSTER; 
bit-vectors smaller than this will be converted to arrays by the sparsification
code, irrespective of relative sizes."))

(defmethod get-hard-sparsification-bound ((token-cluster token-cluster))
  (with-slots (hard-sparsification-bound) token-cluster
    hard-sparsification-bound))

(defgeneric get-sparsification-fraction (token-cluster)
  (:documentation "Returns the relative bound for sparsifying bit-vectors in TOKEN-CLUSTER; 
bit-vectors with a smaller fraction on will be converted to arrays by
the sparsification code, irrespective of absolute size."))

(defmethod get-sparsification-fraction ((token-cluster token-cluster))
  (with-slots (sparsification-fraction) token-cluster
    sparsification-fraction))

(defgeneric get-token-index (token-cluster token)
  (:documentation "Gets the index of a token from a cluster."))

(defmethod get-token-index ((token-cluster token-cluster) (token token-object))
  (item-index-in-hashed-array token-cluster (token-datum token)))

(defmacro get-token-via-index (token-cluster number)
  `(access-hashed-array ,token-cluster ,number))
        
(defmethod access-hashed-array ((hashed-array token-cluster) (key number) &key array-access (if-does-not-exist :error))
  (declare (ignore array-access))
  (with-slots (linear-structure)
              hashed-array
    (if (>= key (length linear-structure))
        (ecase if-does-not-exist
          (:error (error "Key ~s is out of bounds for token-cluster ~s." key hashed-array))
          (:soft nil))
        (aref linear-structure key))))

(defmethod add-to-hashed-array ((hashed-array token-cluster) item (key string))
  (with-slots (access-hash linear-structure name-structure)
              hashed-array
    (setf (gethash key access-hash) (vector-push-extend item linear-structure))))

(defmacro looping-over-token-cluster ((hashed-array item-name) &body body)
  `(with-slots (linear-structure)
               ,hashed-array
     (loop for ,item-name across linear-structure
           do ,@body)))

(defgeneric loop-over-satisfying-tokens (token-structure constraint function-to-apply)
  (:documentation "Loops over tokens in token-cluster which satisfy constraint, applying function-to-apply."))

(defmethod loop-over-satisfying-tokens ((token-cluster token-cluster) constraint function-to-apply)
  (with-slots (linear-structure)
              token-cluster
    (loop-over-satisfying-tokens linear-structure constraint function-to-apply)))

(defmethod loop-over-satisfying-tokens ((token-array array) constraint function-to-apply)
  (loop for token across token-array
        do (if (pure-filter token constraint)
               (funcall function-to-apply token))))

;;;------------------------------------------------------------------- 
;;;
;;; A specialization to do stemming and related mappings
;;;

(defun make-mapping-token-cluster (&key access-hash linear-structure in-mapping-functions in-mapping-tables)
  (let ((hashed-array (%create-unnamed-object 'mapping-token-cluster)))
    (initialize-hashed-array hashed-array :in-access-hash access-hash :in-linear-structure linear-structure)
    (with-slots (mapping-functions mapping-tables) hashed-array
      (setf mapping-functions in-mapping-functions
            mapping-tables in-mapping-tables))
    hashed-array))

(defgeneric update-stem-table (token-cluster object)
  (:documentation "Updates the stem table."))

(defmethod update-stem-table ((token-cluster mapping-token-cluster) (object skinny-token-mixin))
  (with-slots (mapping-functions mapping-tables) token-cluster
    (loop for function in mapping-functions
          for table in mapping-tables
          for value = (funcall function (token-datum object))
          for lookup = (gethash value table)
          do (if (listp lookup)
                 (unless (member object lookup)
                   (setf (gethash value table) (append lookup (list object))))
                 (setf (gethash value table) (list object))))))

(defmethod create-mapping-table (function (token-cluster mapping-token-cluster))
    (with-slots (linear-structure) token-cluster
      (let ((output-table (make-hash-table :size (length linear-structure) :test 'equalp)))
	(loop for item across linear-structure
	      for item-word = (token-datum item)
	      for stemmed-item = (funcall function item-word)
	      for value = (gethash stemmed-item output-table)
	      do (if (listp value)
		     (setf (gethash stemmed-item output-table) (append value (list item)))
		     (setf (gethash stemmed-item output-table) (list item))))
	output-table)))

(defmethod add-stemming-function ((document-context document-context) stemming-function)
  (%add-stemming-function (obtain-token-cluster document-context) stemming-function))

(defmethod %add-stemming-function ((token-cluster mapping-token-cluster) stemming-function)
  (with-slots (mapping-functions mapping-tables) token-cluster
    (push stemming-function mapping-functions)
    (push (create-mapping-table stemming-function token-cluster) mapping-tables)))

(defmethod stemmed-lookup ((token-cluster mapping-token-cluster) string)
  (with-slots (mapping-tables mapping-functions) token-cluster
    (let ((output (make-bit-vector (bit-vector-length))))
      (flet ((internal-wired-or (item)
               (sparse-bit-ior (get-token-stats item) output output)))
        (declare (dynamic-extent #'internal-wired-or))
        (loop for table in mapping-tables
              for function in mapping-functions
              for lookup = (gethash (funcall function string) table)
              when lookup
                do (mapc #'internal-wired-or lookup)))
      output)))

;;;------------------------------------------------------------------- 
;;;
;;; Tokenizers 
;;;

(defun make-tokenizer (name tokenizing-code initialization-function-code storage-function-code output-function-code format-list)
  (let ((tokenizer (%create-unnamed-object 'tokenizer)))
    (with-slots (object-name code initialization-function output-function storage-function formats-supported)
                tokenizer
      (setf object-name name
            code tokenizing-code
            initialization-function initialization-function-code
            storage-function storage-function-code
            output-function output-function-code
            formats-supported format-list))
    tokenizer))

(defgeneric raw-tokenize (tokenizer token-cluster document &optional tokenizer-args)
  (:documentation "Performs tokenization."))

(defmethod raw-tokenize ((tokenizer tokenizer) (token-cluster token-cluster) document &optional tokenizer-args)
  (with-slots (code initialization-function storage-function output-function)
              tokenizer
    (apply code (nconc (list tokenizer document token-cluster initialization-function storage-function output-function) tokenizer-args))))

(defun bit-vector-resource-matcher (resource object size)
  (declare (ignore resource))
  (> (length object) size))

(defun bit-vector-resource-constructor (resource size)
  (declare (ignore resource))
  (make-bit-vector size))

(www-utils:defresource bit-vector-buffer (&optional (size 9600))
  :constructor bit-vector-resource-constructor
  :matcher bit-vector-resource-matcher)

(defun bvs-tokenizer-init (item)
  (zero-bit-vector item))

(defun bvs-tokenizer-update (bit-vector token-index)
  (set-bit bit-vector token-index 1))
    
(defun bvs-output-function (bit-vector)
  bit-vector)

(defun make-bit-vector-storing-tokenizer (name tokenizing-code size increment index format-list)
  (let ((tokenizer (%create-unnamed-object 'bitvector-storing-tokenizer)))
    (with-slots (object-name code initialization-function output-function storage-function formats-supported
                             bit-vector default-size size-increment string-index)
                tokenizer
      (setf object-name name
            code tokenizing-code
            initialization-function #'bvs-tokenizer-init
            storage-function #'bvs-tokenizer-update
            output-function #'bvs-output-function
            default-size size
            size-increment increment
            string-index index
            formats-supported format-list))
    tokenizer))

(defparameter *number-of-retries* 10)

(defmethod %merely-apply-lexical-features (document-universe (document document) tokenizer tokenizer-output features)
  (apply-lexical-features document-universe (object-name tokenizer) document tokenizer-output features))

(defgeneric tokenize-and-apply-lexical-features (document-context document &key load-type tag format document-object prior-tokenization-work)
  (:documentation "Applies FEATURE in DOCUMENT-CONTEXT to DOCUMENT."))

(defmethod tokenize-and-apply-lexical-features ((document-context document-context) document &key (load-type (default-load-type))
                                                (tag (default-tags)) format document-object prior-tokenization-work)
  (multiple-value-bind (document-universe tokenizer cluster lexical-features) (obtain-universe-tokenizer-cluster-feature document-context)
    (cond (prior-tokenization-work
	   (%merely-apply-lexical-features document-universe document tokenizer prior-tokenization-work lexical-features))
	  (t (%tokenize-and-apply-lexical-features document-universe cluster tokenizer document lexical-features :load-type load-type
						   :tag tag :format format :document-object document-object)))))

(defgeneric %tokenize-and-apply-lexical-features (document-universe token-cluster tokenizer document features
                                                                    &key load-type tag format document-object)
  (:documentation "Applies lexical features as necessary."))

(defmethod %tokenize-and-apply-lexical-features (document-universe token-cluster (tokenizer bitvector-storing-tokenizer) (document document)
                                                                   features &key (load-type (default-load-type))
                                                                   (tag (default-tags)) (format nil)
                                                                   (document-object nil))
  (declare (ignore document-object))
  (let* ((use-document (get-document document-universe document :tag tag))
         (raw-document-data (load-document use-document load-type format nil document-universe)))
    (labels ((attempt-lexification (document-universe tokenizer raw-document-data features size retry-count)
               (with-slots (object-name default-size size-increment string-index) tokenizer
                 (handler-case
		   (www-utils:using-resource (storage-vector bit-vector-buffer size)
                       (apply-lexical-features document-universe object-name document
                                               (tokenize document-universe object-name raw-document-data 
                                                         :tokenizer-args (list :storage storage-vector))
                                               features))
                   (subscript-out-of-bounds () (attempt-lexification document-universe tokenizer
                                                                     raw-document-data features (+ size size-increment) (1+ retry-count)))))))
      (attempt-lexification document-universe tokenizer raw-document-data features (number-of-tokens token-cluster) 0))))

(defmethod %tokenize-and-apply-lexical-features (document-universe token-cluster (tokenizer bitvector-storing-tokenizer) (document sequence)
                                                                   features &key load-type tag format document-object)
  (declare (ignore load-type tag format))
  (labels ((attempt-lexification (document-universe tokenizer document-data features size retry-count)
             (with-slots (object-name default-size size-increment string-index) tokenizer
               (when (> retry-count *number-of-retries*)
                 (format *standard-output* "Failure!")
                 (return-from attempt-lexification))
               (handler-case
		 (www-utils:using-resource (storage-vector bit-vector-buffer size)
		   (if document-object
		       (apply-lexical-features document-universe object-name document-object
					       (tokenize document-universe object-name document-data 
							 :tokenizer-args (list :storage storage-vector))
					       features)
		       (tokenize document-universe object-name document-data 
				 :tokenizer-args (list :storage storage-vector))))
                 (subscript-out-of-bounds () (attempt-lexification document-universe tokenizer document-data features
                                                                   (+ size size-increment) (1+ retry-count)))))))
    (attempt-lexification document-universe tokenizer document features (number-of-tokens token-cluster) 0)))

(defgeneric sparsify-token-cluster (document-universe token-cluster)
  (:documentation "Loops over the tokens in TOKEN-CLUSTER, converting bit-vectors to arrays
if the cardinality is either absolutely smaller hard-sparsification-bound or is smaller 
as a fraction of total documents in DOCUMENT-UNIVERSE than sparsification-fraction."))

(defmethod sparsify-token-cluster (document-universe token-cluster)
  (let ((tokens-to-fix 0)
        (document-number (number-of-documents document-universe))
        (hard-sparsification-bound (get-hard-sparsification-bound token-cluster))
        (sparsification-fraction (get-sparsification-fraction token-cluster)))
    (looping-over-token-cluster (token-cluster token)
      (let ((token-stats (get-token-stats token)))
	(when (typep token-stats 'bit-vector)
	  (let ((word-size (bit-vector-cardinality token-stats)))
	    (when (or (< word-size hard-sparsification-bound) (< (/ word-size document-number) sparsification-fraction))
	      (initialize-token-stat-storage token :default-value (bit-vector->sparse-array (get-token-stats token) :adjustable t))
	      (incf tokens-to-fix))))))
	tokens-to-fix))

;;;------------------------------------------------------------------- 
;;;
;;; Documents
;;;

(defun initialize-document (document in-name in-pdi in-label in-format)
  (with-slots (object-name pdi label format) document
    (initialize-cache document)
    (setf object-name in-name
          pdi (parse-pdi in-pdi)
          label in-label
          format in-format)))

(defun make-document (name pdi formats &optional labels)
  (let ((document (%create-unnamed-object 'document)))
    (initialize-document document name pdi labels formats)
    document))

(defgeneric get-document-pdi (document load-type)
  (:documentation "Accesses the pdi for DOCUMENT according to LOAD-TYPE."))

(defmethod get-document-pdi ((document document) load-type)
  (with-slots (pdi) document
    (second (assoc load-type pdi))))

(defgeneric load-document (document load-type &optional format cache-p document-universe)
  (:documentation "Loads DOCUMENT in a format specified by LOAD-TYPE."))

;; the distinction between LOAD-TYPE and IN-FORMAT herein is the following :
;; LOAD-TYPE specifies the file-type, whereas IN-FORMAT specifies a particular function to apply to that
;; file type.

(defmethod load-document ((document document) load-type &optional in-format cache-p (document-universe (default-document-universe)))
  (with-slots (format) document
    (let ((format-name (or in-format (car format))))
      (flet ((load-doc (document-universe document load-type format-name)
               (with-slots (access-codes) document-universe
                 (let ((use-pdi (get-document-pdi document load-type)))
                   (if use-pdi
                       (funcall (gethash format-name access-codes) use-pdi)
                       (error "There is no pdi corresponding to load-type ~s in document ~s." load-type document))))))
        (use-cached-value-if-available (document format-name cache-p t)
          (load-doc document-universe document load-type format-name))))))

;;;------------------------------------------------------------------- 
;;;
;;; Document Universes 
;;;

(defun initialize-document-universe (document-universe &key in-documents in-tokens in-tokenizers in-access-codes in-supported-formats)
  (with-slots (documents tokens access-codes supported-formats tokenizers)
              document-universe
    (setf documents (or in-documents (make-hashed-tagged-array)) 
          tokens (or in-tokens (make-hash-table :test 'equalp))
          tokenizers (or in-tokenizers (make-hashed-array))
          access-codes (or in-access-codes (make-hash-table :test 'equalp))
          supported-formats (or in-supported-formats nil))))

(defun make-document-universe (&key documents tokens tokenizers access-codes supported-formats)
  (let ((document-universe (%create-unnamed-object 'document-universe)))
    (initialize-document-universe document-universe 
                                  :in-documents documents 
                                  :in-tokens tokens 
                                  :in-tokenizers tokenizers 
                                  :in-access-codes access-codes 
                                  :in-supported-formats supported-formats)
    document-universe))

(defgeneric add-access-code (document-universe format code)
  (:documentation "Adds an access code to the document-universe."))

(defmethod add-access-code ((document-universe document-universe) format code)
  (with-slots (access-codes) document-universe
    (setf (gethash format access-codes) code)))

(defgeneric get-documents (document-universe &optional tag)
  (:documentation "Returns an array of documents from DOCUMENT-UNIVERSE."))

(defmethod get-documents (document-universe &optional (tag (default-tags)))
  (with-slots (documents)
              document-universe
    (get-view-of-tagged-array documents tag)))

(defmacro loop-over-documents ((document-universe document-name) &body body)
  `(with-slots (linear-structure)
               (get-documents ,document-universe)
     (loop for ,document-name across linear-structure
           do ,@body)))

(defgeneric add-document (document-context document &key tag type-list code-list index overwrite)
  (:documentation "Adds DOCUMENT to universe in DOCUMENT-CONTEXT."))

(defmethod add-document ((document-context document-context) document &key (tag (default-tags)) type-list code-list index overwrite)
  (let ((document-universe (obtain-document-universe document-context)))
    (%add-document document-universe document :tag tag :type-list type-list :code-list code-list :index index :overwrite overwrite)))

(defgeneric %add-document (document-universe document &key tag type-list code-list index overwrite)
  (:documentation "Adds document to document universe."))

(defmethod %add-document ((document-universe document-universe) document &key (tag (default-tags)) (type-list (default-type)) code-list index overwrite)
 (declare (ignore overwrite))
  (with-slots (documents supported-formats access-codes) document-universe
    (cond ((subset type-list supported-formats)
	   (if index
	       (insert-into-hashed-tagged-array documents document tag (object-name document) index)
	       (add-to-hashed-tagged-array documents document tag (object-name document))) )
          (code-list
           (setf supported-formats (append type-list supported-formats))
           (loop for type in type-list
                 with counter = 0
                 do (or (gethash type access-codes)
                        (progn
                          (setf (gethash type access-codes) (elt code-list counter))
                          (incf counter))))
	   (if index
	       (insert-into-hashed-tagged-array documents document tag (object-name document) index)
	       (add-to-hashed-tagged-array documents document tag (object-name document))))
          (t (error "No access code provided for new type ~s in universe ~s." type-list document-universe)))))

(defmethod %add-document :around ((document-universe document-universe) document &key (tag (default-tags)) type-list code-list index overwrite)
  (declare (ignore type-list code-list index))
  (cond ((and (not overwrite) (get-document document-universe (object-name document) :tag tag :if-does-not-exist :soft))
	 (throw :already-exists "Document already exists."))
        (t (adjust-token-storage document-universe (number-of-documents document-universe :all t))
           (call-next-method))))

(defgeneric make-and-add-document (document-universe name pdi formats &key labels tag type-list code-list)
  (:documentation "Makes and adds a document to the universe."))

(defmethod make-and-add-document (document-universe name pdi formats &key (labels nil)
                                                    (tag (default-tags)) (type-list (default-type)) code-list)
  (add-document document-universe (make-document name pdi formats labels) :tag tag :type-list type-list :code-list code-list))

(defgeneric add-tokenizer (document-universe tokenizer &key mapping-p)
  (:documentation "Adds tokenizer and initializes a new token storage area for said tokenizer."))

(defmethod soft-add-tokenizer ((document-universe document-universe) tokenizer)
  (with-slots (tokenizers) document-universe
    (add-to-hashed-array tokenizers tokenizer (object-name tokenizer))))

(defmethod add-tokenizer ((document-universe document-universe) tokenizer &key (mapping-p nil))
  (with-slots (tokenizers tokens) document-universe
    (let ((name (object-name tokenizer)))
      (add-to-hashed-array tokenizers tokenizer name)
      (setf (gethash name tokens) (if mapping-p (make-mapping-token-cluster) (make-token-cluster))))))

(defgeneric get-tokenizer-and-cluster (document-universe tokenizer)
  (:documentation "Gets the token cluster and the tokenizer from DOCUMENT-UNIVERSE corresponding to TOKENIZER."))

(defmethod get-tokenizer-and-cluster ((document-universe document-universe) (tokenizer string))
  (with-slots (tokens tokenizers) document-universe
    (values (gethash tokenizer tokens)
            (access-hashed-array tokenizers tokenizer))))

(defgeneric get-tokenizer (document-universe tokenizer)
  (:documentation "Gets the tokenizer."))

(defmethod get-tokenizer ((document-universe document-universe) (tokenizer string))
  (with-slots (tokenizers) document-universe
    (access-hashed-array tokenizers tokenizer)))

(defmacro loop-over-token-clusters ((document-universe cluster-name) &body body)
  `(with-slots (tokens) ,document-universe
     (loop for ,cluster-name being the hash-values of tokens
           do ,@body)))

(defgeneric get-document (document-universe key &key array-access tag if-does-not-exist)
  (:documentation "Gets the document indexed by KEY from DOCUMENT-UNIVERSE subject to TAG constraints."))

(defmethod get-document ((document-universe document-universe) key &key (array-access nil) (tag (default-tags)) (if-does-not-exist :error))
  (with-slots (documents) document-universe
    (access-tagged-array documents key (if (listp tag) (car tag) tag)
                         :array-access array-access :if-does-not-exist if-does-not-exist)))

(defmethod get-document ((document-universe document-universe) (key document) &key array-access tag if-does-not-exist)
  (declare (ignore tag if-does-not-exist array-access))
  key)

;; more cruft that needs to be done right.  AJB --- 4/24/97 01:22:50

(defgeneric absolute-get-document (document-universe key)
  (:documentation ""))

(defmethod absolute-get-document ((document-universe document-universe) (key number))
  (with-slots (documents) document-universe
    (with-slots (linear-object) documents
      (aref linear-object key))))

(defgeneric get-document-tags (document-universe key &key tag if-does-not-exist)
  (:documentation "Returns the tags for document indexed by KEY in DOCUMENT-UNIVERSE subject to TAG constraints."))

(defmethod get-document-tags ((document-universe document-universe) (document document)
                              &key (tag (default-tags)) (if-does-not-exist :error))
  (get-document-tags document-universe (object-name document) :tag tag :if-does-not-exist if-does-not-exist))

(defmethod get-document-tags ((document-universe document-universe) key &key (tag (default-tags)) (if-does-not-exist :error))
  (with-slots (documents) document-universe
    (access-tags documents key tag :if-does-not-exist if-does-not-exist)))

;; still more cruft; this whole absolute- family of functions shouldn't exist.  fix soon.  AJB --- 4/24/97 01:32:11

(defgeneric absolute-get-document-tags (document-universe key)
  (:documentation ""))

(defmethod absolute-get-document-tags ((document-universe document-universe) (key number))
  (with-slots (documents) document-universe
    (with-slots (tags) documents
      (aref tags key))))

(defgeneric modify-document-tags (document-universe key new-tags &key tag if-does-not-exist)
  (:documentation "Modifies the tags for document indexed by KEY in DOCUMENT-UNIVERSE subject to TAG constraints."))

(defmethod modify-document-tags ((document-universe document-universe) key new-tags &key (tag (default-tags)) (if-does-not-exist :error))
  (with-slots (documents) document-universe
    (modify-tags documents key tag new-tags :if-does-not-exist if-does-not-exist)))

(defgeneric get-document-index (document-universe key &key tag)
  (:documentation "Returns the index of document hashed by KEY in DOCUMENT-UNIVERSE subject to TAG."))

(defmethod get-document-index ((document-universe document-universe) key &key (tag (default-tags)))
  (with-slots (documents) document-universe
    (get-index-in-hashed-tagged-array documents key tag)))

(defgeneric absolute-document-index (document-universe key &key tag)
  (:documentation "Returns the absolute index of document in document-universe."))

(defmethod absolute-document-index ((document-universe document-universe) key &key (tag (default-tags)))
  (with-slots (documents) document-universe
    (absolute-index documents key :tag tag)))

(defgeneric number-of-documents (document-universe &key tag all)
  (:documentation "Returns the number of documents in a document-universe."))

(defmethod number-of-documents ((document-universe document-universe) &key (tag (default-tags)) (all nil))
  (with-slots (documents) document-universe
    (tagged-array-length documents (if all nil tag))))

(defgeneric tokenize (document-universe tokenizer document &key load-type format tag cache-p load-cache-p tokenizer-args)
  (:documentation "Tokenizes a document."))

(defmethod tokenize ((document-universe document-universe) (tokenizer string) document
                     &key (load-type (default-load-type)) (format nil) (tag (default-tags))
                     (cache-p nil) (load-cache-p nil) (tokenizer-args nil))
  (let ((use-document (get-document document-universe document :tag tag)))
    (use-cached-value-if-available (use-document tokenizer cache-p t)
      (multiple-value-bind (use-token-cluster use-tokenizer)
          (get-tokenizer-and-cluster document-universe tokenizer)
        (raw-tokenize use-tokenizer use-token-cluster
                      (load-document use-document load-type format load-cache-p document-universe) tokenizer-args)))))

(defmethod tokenize ((document-universe document-universe) (tokenizer string) (document sequence)
                     &key format load-type tag cache-p load-cache-p tokenizer-args)
  (declare (ignore load-type tag cache-p load-cache-p format))
  (multiple-value-bind (use-token-cluster use-tokenizer) (get-tokenizer-and-cluster document-universe tokenizer)
    (raw-tokenize use-tokenizer use-token-cluster document tokenizer-args)))
           
(defgeneric get-tokenization-information (document tokenizer)
  (:documentation "Gets the cached tokenization information in DOCUMENT produced by TOKENIZER."))

(defmethod get-tokenization-information ((document document) (tokenizer string))
  (access-cache document tokenizer))

(defmethod get-tokenization-information ((document document) (tokenizer tokenizer))
  (get-tokenization-information document (object-name tokenizer)))

(defgeneric sparsify-document-universe (document-universe)
  (:documentation "Sparsifies all token-clusters in document universe."))

(defmethod sparsify-document-universe ((document-universe document-universe))
  (loop-over-token-clusters (document-universe current-cluster)
    (sparsify-token-cluster document-universe current-cluster)))

;;;------------------------------------------------------------------- 
;;;
;;; An example of storing tokenization information in documents.
;;;

(defgeneric initialize-token-stat-storage (object &key label default-value)
  (:documentation "Initializes token-storage information in document."))

(defmethod initialize-token-stat-storage ((object document) &key (label (default-label)) default-value)
  (update-cache object label (or default-value (make-numerological-hashed-array))))

(defmethod initialize-token-stat-storage ((object skinny-token-mixin) &key (label (default-label)) default-value)
  (update-cache object label (or default-value (make-bit-vector))))

(defgeneric update-token-stats (token-cluster object token-rep &key label value)
  (:documentation "Updates the information regarding a particular token in the document."))

(defmethod update-token-stats ((token-cluster token-cluster) (object document) (token-rep number) &key (label (default-label)) value)
  (declare (ignore value))
  (increment-hashed-array-value (access-cache object label) token-rep))

(defmethod update-token-stats ((token-cluster token-cluster) (object skinny-token-mixin) (document-index number)
                               &key (label (default-label)) (value 1))
  (flet ((update-code (item)
           (typecase item
             (bit-vector (set-bit item document-index value))
             (array (vector-push-extend document-index item)))))
    (declare (dynamic-extent #'update-code))
    (destructive-update-cache object label #'update-code)))

(defmethod update-token-stats ((token-cluster mapping-token-cluster) (object skinny-token-mixin) (document-index number)
                               &key (label (default-label)) (value 1))
  (flet ((update-code (item)
           (typecase item
             (bit-vector (set-bit item document-index value)) 
             (array (vector-push-extend document-index item)))))
    (declare (dynamic-extent #'update-code))
    (destructive-update-cache object label #'update-code)
    (update-stem-table token-cluster object)))

(defgeneric get-token-stats (object &key label)
  (:documentation "Pulls the token information out of the document."))

(defmethod get-token-stats (object &key (label (default-label)))
  (access-cache object label))

(defmethod get-token-stats ((object (eql nil)) &key label)
  (declare (ignore label))
  nil)

(defun adjust-token-storage (document-universe number-of-documents &key (label (default-label)))
  (when (>= number-of-documents (bit-vector-length))
    (with-slots (tokens) document-universe
      (increment-bit-vector-size)
      (flet ((update-tokens (name token-cluster)
               (declare (ignore name))
               (looping-over-token-cluster (token-cluster object)
		 (let ((stats (access-cache object label)))
		   (typecase stats
		     (bit-vector (update-cache object label (copy-bit-vector (access-cache object label) (make-bit-vector))))
		     (array nil))))))
        (declare (dynamic-extent #'update-tokens))
        (maphash #'update-tokens tokens)))))

(defun forcible-token-intern (token token-cluster)
  (with-slots (access-hash linear-structure)
              token-cluster
    (setf (gethash (token-datum token) access-hash)
          (vector-push-extend token linear-structure))))

(defun %token-intern (datum token-cluster &optional (store-p nil) (ephemeral-p nil))
  (with-slots (access-hash linear-structure)
              token-cluster
    (cond (store-p (if (gethash datum access-hash)
                       (aref linear-structure (gethash datum access-hash))
                       (let* ((use-datum (if ephemeral-p
                                             (copy datum)
                                             datum))
                              (token (make-token use-datum)))
                         (setf (gethash use-datum access-hash) (vector-push-extend token linear-structure))
                         token)))
          (t (if (gethash datum access-hash)
                 (aref linear-structure (gethash datum access-hash))
                 nil)))))

(defun %token-intern-returning-index (datum token-cluster &optional (store-p nil) (ephemeral-p nil))
  (with-slots (access-hash linear-structure)
              token-cluster
    (let ((index (gethash datum access-hash)))
      (cond (store-p (if index
                         index                 
                         (let* ((use-datum (if ephemeral-p
                                               (copy datum)
                                               datum))
                                (token (make-token use-datum)))
                           (setf (gethash use-datum access-hash) (vector-push-extend token linear-structure))
                           (gethash use-datum access-hash))))
            (t (or index nil))))))


(defun token-intern (datum tokenizer &optional (document-universe (default-document-universe)) (store-p nil) (ephemeral-p nil))
  (let ((token-cluster (get-tokenizer-and-cluster document-universe tokenizer)))
    (%token-intern datum token-cluster store-p ephemeral-p)))


;;;------------------------------------------------------------------- 
;;;
;;; Feature application to tokens; an example
;;;

(defun pure-filter (token constraint-object)
  (let ((use-token (list token)))
    (declare (dynamic-extent use-token))
    (if (perform-computation constraint-object use-token nil)
        t
        nil)))

(defgeneric constraint-transform (constraints document-data token-cluster)
  (:documentation "Filters DOCUMENT-DATA such that only tokens that satisfy CONSTRAINTS remain."))

(defmethod constraint-transform ((constraints null) document-data token-cluster)
  (declare (ignore token-cluster))
  document-data)

(defmethod constraint-transform (constraints (document-data number) token-cluster)
  (flet ((map-function (index value)
           (if (pure-filter (get-token-via-index token-cluster index) constraints)
               (set-mask-bit-p index value t)
               value)))
    (declare (dynamic-extent #'map-function))
    (map-bit-vector-accumulating document-data #'map-function 0)))

(defmethod constraint-transform (constraints (document-data bit-vector) token-cluster)
  (let ((result (make-bit-vector (length document-data))))
    (flet ((map-function (index)
             (if (pure-filter (get-token-via-index token-cluster index) constraints)
                 (set-bit result index 1))))
      (declare (dynamic-extent #'map-function))
      (map-bit-vector document-data #'map-function))
    result))

(defmethod constraint-transform (constraints (document-data array) token-cluster)
  (declare (ignore token-cluster))
  (let ((result (make-array 0 :adjustable t :fill-pointer t)))
    (loop for token in document-data
          do (if (pure-filter token constraints)
                 (vector-push-extend token result)))
    result))

(defmethod constraint-transform (constraints (document-data-list list) token-cluster)
  (loop for document-datum in document-data-list
        collect (constraint-transform constraints document-datum token-cluster)))

(defgeneric feature-present-in-data (lexical-feature data document document-index token-cluster
                                                     document-storage-function feature-storage-function)
  (:documentation "Determins if LEXICAL-FEATURE is present in DATA and takes appropriate action."))

(defmethod feature-present-in-data ((lexical-feature token-feature) (data bit-vector) document
                                    document-index token-cluster document-storage-function feature-storage-function)
  (flet ((update-function (index)
           (cond-every
             (document-storage-function (funcall document-storage-function document index))
             (feature-storage-function (funcall feature-storage-function token-cluster (get-token-via-index token-cluster index)
                                                document-index)))))
    (declare (dynamic-extent #'update-function))
    (map-bit-vector data #'update-function)))

(defmethod feature-present-in-data ((lexical-feature simple-lexical-feature) (data bit-vector) document
                                    document-index token-cluster document-storage-function feature-storage-function)
  (declare (ignore token-cluster))
  (with-slots (in-mask out-mask) lexical-feature
    (when (and (not (bit-vector-empty-p (bit-vector-intersection in-mask data)))
               (bit-vector-empty-p (bit-vector-intersection out-mask data)))
      (cond-every
        (document-storage-function
          (funcall document-storage-function document lexical-feature))
        (feature-storage-function
          (funcall feature-storage-function lexical-feature document-index))))))

(defmethod feature-present-in-data ((lexical-feature general-lexical-feature) (data bit-vector) document
                                    document-index token-cluster document-storage-function feature-storage-function)
  (declare (ignore token-cluster))
  (with-slots (masks combination-function) lexical-feature
    (when (apply combination-function
                 (loop for mask in masks
                       collect (bit-vector-intersection mask data)))
      (cond-every
        (document-storage-function (funcall document-storage-function document lexical-feature))
        (feature-storage-function (funcall feature-storage-function lexical-feature document-index))))))

(defmethod feature-present-in-data ((lexical-feature general-lexical-feature) (data list) document
                                    document-index token-cluster document-storage-function feature-storage-function)
  (declare (ignore token-cluster))
  (with-slots (masks index-for-masks combination-function) lexical-feature
    (when (apply combination-function
                 (loop for mask in masks
                       for index in index-for-masks
                       collect (bit-vector-intersection mask (elt data index))))
      (cond-every
        (document-storage-function (funcall document-storage-function document lexical-feature))
        (feature-storage-function (funcall feature-storage-function lexical-feature document-index))))))

(defun apply-lexical-features (document-universe tokenizer document tokenizer-output lexical-feature-sets &key (tags (default-tags)))
  (let ((token-cluster (get-tokenizer-and-cluster document-universe tokenizer))
        (document-index (absolute-document-index document-universe (object-name document) :tag tags)))
    (loop for lexical-feature-set in lexical-feature-sets
          do (apply-lexical-feature-set lexical-feature-set document document-index tokenizer-output token-cluster))))

(defun apply-lexical-feature-set (lexical-feature-set document document-index document-data token-cluster)
  (with-slots (pre-applied-constraints lexical-features document-storage-function feature-storage-function) lexical-feature-set
    (let ((process-document-data (constraint-transform pre-applied-constraints document-data token-cluster)))
      (loop for lexical-feature in lexical-features
            do (feature-present-in-data lexical-feature process-document-data 
                                        document document-index
                                        token-cluster document-storage-function 
                                        feature-storage-function)))))
