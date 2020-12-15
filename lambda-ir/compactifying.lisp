;;;   -*- Mode: LISP; Package: lambda-ir; BASE: 10; SYNTAX: ansi-common-lisp -*-

;;; (C) Copyright 1997, Andrew J. Blumberg (blumberg@ai.mit.edu).
;;;     All Rights Reserved.
;;;
;;;
;;;------------------------------------------------------------------- 

(defparameter tokens-to-fix 0)

(looping-over-token-cluster ((get-tokenizer-and-cluster wh-universe "tok") token)
  (when (< (bit-vector-cardinality (get-token-stats token)) 20)
    (initialize-token-stat-storage token :default-value (bit-vector->sparse-array (get-token-stats token) :adjustable t))
    (incf tokens-to-fix)))

(simple-bottom-up-search wh-universe "tok" '("fielding"))

(trace sparse-bit-and)
(trace sparse-bit-ior)
(trace sparse-bit-andc2)

(with-slots (documents) wh::wh-universe
  (with-slots (linear-object) documents
    (loop for document across linear-object
	  do (with-slots (storage) document
	       (setf storage nil)))))


(set-increment 'array 5000)
(set-increment 'hash-table 5000)

(save-document-universe wh::wh-universe "MIT-FILE-SERVER:>blumberg>wh-saves>full-wh-universe")

(time (setf wh::wh-universe (load-document-universe "full-wh-universe-4" "http:lambda-ir;wh;universe-images;")))

(save-document-universe wh::wh-universe "full-wh-universe-4" "MIT-FILE-SERVER:>blumberg>wh-saves>")

(set-increment 'array 25000)
(set-increment 'hash-table 25000)

(save-document-universe wh::wh-universe "MIT-FILE-SERVER:>blumberg>wh-saves>full-wh-universe-3")

(save-object wh::wh-universe #p"wilson.ai.mit.edu:>blumberg>kahuna")

(defmethod make-load-form ((document-id db::document-id))
  `(db::intern-document-id ,(db:document-id-string document-id)))

(setf wh-universe (load-object #p"wilson.ai.mit.edu:>blumberg>kahuna"))

;;

(defun extract-length (token)
  (let ((stats (get-token-stats token)))
    (typecase stats
      (bit-vector (bit-vector-cardinality stats))
      (array (length stats)))))

(defun compare-tokens (token-1 token-2)
  (> (extract-length token-1) (extract-length token-2)))

(defun frequency-sort (token-cluster)
  (with-slots (access-hash linear-structure) token-cluster
    (sort linear-structure #'compare-tokens)
    (loop for item across linear-structure
	  for idx upfrom 0
	  while item
	  do (setf (gethash (token-datum item) access-hash) idx))))

(time (frequency-sort wh-cluster))
