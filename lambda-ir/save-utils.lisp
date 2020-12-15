;;;   -*- Mode: LISP; Package: lambda-ir; BASE: 10; SYNTAX: ansi-common-lisp -*-

;;; (C) Copyright 1997, Andrew J. Blumberg (blumberg@ai.mit.edu).
;;;     All Rights Reserved.
;;;
;;;
;;;------------------------------------------------------------------- 
;;;
;;; Saving the universe 
;;;

(in-package :lambda-ir)

(defparameter *saved-universe* nil)

;(load-document-universe "w:>blumberg>newdumptest")
;(save-document-universe *saved-universe* "http:lambda-ir;examples;cl-http-universe")

(defparameter *save-temp* nil)

(defun save-object (object pathname)
  (dump-forms-to-file (pathname pathname) (list `(setq *save-temp* ',object))))

(defun load-object (pathname)
  (load pathname :verbose nil :print nil)
  *save-temp*)

(defparameter *increment-table* (make-hash-table))

(defun set-increment (type increment)
  (setf (gethash type *increment-table*) increment))

(defun get-increment (type)
  (unless (gethash type *increment-table*)
    (error "No increment count exists for type ~s ~%" type)))

(defgeneric incremental-write-object (object pathname)
  (:documentation "Performs an incremental dump of OBJECT to files pointed to by PATHNAME."))

(defmethod incremental-write-object ((object array) pathname)
  (let* ((increment (get-increment 'array))
         (object-size (length object))
         (number (ceiling (/ object-size increment)))
         (pathnames-for-assembly (list object-size 'array)))
    (loop for n upfrom 0 below number
          for start = (* n increment)
          for end = (min object-size (* (1+ n) increment))
          do (let ((extension (string (gensym))))
               (save-object (subseq object start end) (concatenate 'string pathname "-" extension))
               (push extension pathnames-for-assembly)))
    (save-object (reverse pathnames-for-assembly) pathname)))

(defmethod incremental-write-object ((object list) pathname)
  (let* ((increment (get-increment 'array))
         (object-size (length object))
         (number (ceiling (/ object-size increment)))
         (pathnames-for-assembly (list object-size 'list)))
    (loop for n upfrom 0 below number
          for start = (* n increment)
          for end = (min object-size (* (1+ n) increment))
          do (let ((extension (string (gensym))))
               (save-object (subseq object start end) (concatenate 'string pathname "-" extension))
               (push extension pathnames-for-assembly)))
    (save-object (reverse pathnames-for-assembly) pathname)))

(defmethod incremental-write-object ((object hash-table) pathname)
  (let* ((increment (get-increment 'hash-table))
         (object-size (hash-table-size object))
         (pathnames-for-assembly (list object-size 'hash-table))
         (key-buffer (make-array increment :fill-pointer t))
         (value-buffer (make-array increment :fill-pointer t)))
    (flet ((do-it (key value)
             (if (vector-push key key-buffer)
                 (vector-push value value-buffer)
                 (let ((key-name (string (gensym)))
                       (value-name (string (gensym))))
                   (save-object key-buffer (concatenate 'string pathname "-" key-name))
                   (save-object value-buffer (concatenate 'string pathname "-" value-name))
                   (setf (fill-pointer key-buffer) 0
                         (fill-pointer value-buffer) 0)
                   (vector-push key key-buffer)
                   (vector-push value value-buffer)
                   (push key-name pathnames-for-assembly)
                   (push value-name pathnames-for-assembly)))))
      (setf (fill-pointer key-buffer) 0
            (fill-pointer value-buffer) 0)
      (maphash #'do-it object)
      (let ((key-name (string (gensym)))
            (value-name (string (gensym))))
        (save-object key-buffer (concatenate 'string pathname "-" key-name))
        (save-object value-buffer (concatenate 'string pathname "-" value-name))
        (push key-name pathnames-for-assembly)
        (push value-name pathnames-for-assembly))
      (save-object (reverse pathnames-for-assembly) pathname))))

(defmethod incremental-write-object ((object hashed-array) pathname)
  (let ((access-hash-pathname (concatenate 'string pathname "-" "access-hash"))
        (linear-structure-pathname (concatenate 'string pathname "-" "linear-structure"))
        (name-structure-pathname (concatenate 'string pathname "-" "name-structure")))
    (with-slots (access-hash linear-structure name-structure) object
      (incremental-write-object access-hash access-hash-pathname)
      (incremental-write-object linear-structure linear-structure-pathname)
      (incremental-write-object name-structure name-structure-pathname)
      (save-object (list 'hashed-array access-hash-pathname name-structure-pathname linear-structure-pathname) pathname))))

(defmethod %incremental-load-object ((type (eql 'array)) pathname extension-list)
  (let ((results (make-array (car extension-list) :fill-pointer t)))
    (setf (fill-pointer results) 0)
    (loop for pathname-extension in (cdr extension-list)
          do (let ((use-pathname (concatenate 'string pathname "-" pathname-extension)))
               (loop for item across (load-object use-pathname)
                     do (vector-push item results))))
    results))

(defmethod %incremental-load-object ((type (eql 'list)) pathname extension-list)
  (loop for pathname-extension in (cdr extension-list)
        for use-pathname = (concatenate 'string pathname "-" pathname-extension)
        for object = (load-object use-pathname)
        append object))

(defmethod %incremental-load-object ((type (eql 'hash-table)) pathname extension-list)
  (let ((results (make-hash-table :test 'equalp :size (car extension-list)))
        (use-extension-list (cdr extension-list)))
    (loop for n upfrom 0 below (length use-extension-list) by 2
          do (let* ((key-pathname (concatenate 'string pathname "-" (elt use-extension-list n)))
                    (value-pathname (concatenate 'string pathname "-" (elt use-extension-list (1+ n))))
                    (key-buffer (load-object key-pathname))
                    (value-buffer (load-object value-pathname)))
               (loop for key across key-buffer
                     for value across value-buffer
                     do (setf (gethash key results) value))))
    results))

(defmethod %incremental-load-object ((type (eql 'hashed-array)) pathname extension-list)
  (declare (ignore pathname))
  (make-hashed-array :access-hash (incremental-load-object (first extension-list))
                     :name-structure (incremental-load-object (second extension-list))
                     :linear-structure (incremental-load-object (third extension-list))))

(defmethod %incremental-load-object ((type (eql 'token-cluster)) pathname extension-list)
  (declare (ignore pathname))
  (make-token-cluster :access-hash (incremental-load-object (first extension-list))
                      :linear-structure (incremental-load-object (third extension-list))))
       
(defun incremental-load-object (pathname)
  (let ((assembly-data (load-object pathname)))
    (%incremental-load-object (car assembly-data) pathname (cdr assembly-data))))

(defun save-stripped-cluster-image (cluster)
  (with-slots (linear-structure)
              cluster
    (let ((store-array (make-array (length linear-structure))))
      (loop for idx upfrom 0
            for token across linear-structure
            do (setf (aref store-array idx)
                     (token-datum token)))
      store-array)))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

#-Genera
(defun load-document-universe (file-name)
  (load file-name)
  *saved-universe*)

#-Genera
(defun save-document-universe (document-universe save-file)
  (setf *save-temp* document-universe)
  (with-open-file (stream #p"http:lambda-ir;examples;dump.temp" :direction :output
                          :if-does-not-exist :create :if-exists :overwrite)
    (format stream "(defmacro return-obj () *save-temp*) ~%")
    (format stream "(setf *saved-universe* (return-obj)) ~%"))
  (compile-file #p"http:lambda-ir;examples;dump.test" :output-file save-file)
  (delete-file #p"http:lambda-ir;examples;dump.test"))

#+Genera
(defun save-document-universe (document-universe save-file-name save-file-directory)
  (let ((document-pathname (concatenate 'string save-file-directory save-file-name "-" "documents"))
        (temp-linear-object)
        (temp-tokens))
    (with-slots (documents tokens) document-universe
      (with-slots (linear-object)
                  documents
        (incremental-write-object linear-object document-pathname)
        (setf temp-linear-object linear-object)
        (setf temp-tokens tokens)
        (setf linear-object save-file-name)
        (flet ((do-it (key value)
                 (let ((token-pathname (concatenate 'string save-file-name "-" "tokens" "-" (string (gensym)))))
                   (incremental-write-object value (concatenate 'string save-file-directory token-pathname))
                   (setf (gethash key tokens) token-pathname))))
          (maphash #'do-it tokens))
        (sys:dump-forms-to-file (pathname (concatenate 'string save-file-directory save-file-name))
                                (list `(setq *saved-universe* ',document-universe)))
        (setf linear-object temp-linear-object)
        (setf tokens temp-tokens)))))

#+Genera
(defun coerce-token-cluster (hashed-array)
  (with-slots (linear-structure access-hash) hashed-array
    (make-token-cluster :access-hash access-hash :linear-structure linear-structure)))

#+Genera
(defun load-document-universe (file-name file-directory)
  (load (concatenate 'string file-directory file-name))
  (with-slots (documents tokens) *saved-universe*
    (with-slots (linear-object) documents
      (if (stringp linear-object)
          (setf linear-object (incremental-load-object (concatenate 'string file-directory linear-object "-documents")))))
    (flet ((do-it (key value)
             (if (stringp value)
                 (setf (gethash key tokens)
                       (coerce-token-cluster (incremental-load-object (concatenate 'string file-directory value)))))))
      (maphash #'do-it tokens)))
  *saved-universe*)

(defmethod make-load-form ((document-universe document-universe))
  (with-slots (documents tokens tokenizers access-codes supported-formats)
              document-universe
    `(make-document-universe :documents ',documents 
                             :tokens ',tokens
                             :tokenizers ',tokenizers
                             :access-codes ',access-codes
                             :supported-formats ',supported-formats)))

(defmethod make-load-form ((hashed-array hashed-array))
  (with-slots (access-hash linear-structure name-structure)
              hashed-array
    `(make-hashed-array :access-hash ',access-hash
                        :linear-structure ',linear-structure
                        :name-structure ',name-structure)))

(defmethod make-load-form ((hashed-tagged-array hashed-tagged-array))
  (with-slots (linear-object tags names)
              hashed-tagged-array
    `(make-hashed-tagged-array :linear-object ',linear-object
                               :tags ',tags
                               :names ',names)))

(defmethod make-load-form ((tagged-array tagged-array))
  (with-slots (linear-object tags)
              tagged-array
    `(make-tagged-array :linear-object ',linear-object
                        :tags ',tags)))

(defmethod make-load-form ((document document))
  (with-slots (object-name pdi label format)
              document
    `(make-document ',object-name ',pdi ',format ',label)))

(defmethod make-load-form ((url url:url))
  `(url:intern-url ',(url:name-string url)))

(defmethod make-load-form ((skinny-token skinny-token-mixin))
  (with-slots (datum)
              skinny-token
    `(make-token ',datum :stat-storage ',(access-cache skinny-token *default-label*))))

(defmethod make-load-form ((token-cluster token-cluster))
  (with-slots (access-hash linear-structure)
              token-cluster
    `(make-token-cluster :access-hash ',access-hash
                         :linear-structure ',linear-structure)))
                        
(defmethod make-load-form ((tokenizer tokenizer))
  (with-slots (object-name code initialization-function storage-function output-function formats-supported)
              tokenizer
    `(make-tokenizer ',object-name ',code ',initialization-function ',storage-function ',output-function ',formats-supported)))
