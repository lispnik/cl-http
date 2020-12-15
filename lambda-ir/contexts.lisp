;;;   -*- Mode: LISP; Package: lambda-ir; BASE: 10; SYNTAX: ansi-common-lisp -*-

;;; (C) Copyright 1997, Andrew J. Blumberg (blumberg@ai.mit.edu).
;;;     All Rights Reserved.
;;;
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CLASS DEFINITIONS
;;;

(in-package :lambda-ir)

(defun make-document-context (&key in-document-universe in-tokenizer-names in-tokenizers in-lexical-features)
  (initialize-document-context (%create-unnamed-object 'document-context) 
                               :in-document-universe in-document-universe
                               :in-tokenizer-names in-tokenizer-names
                               :in-tokenizers in-tokenizers
                               :in-lexical-features in-lexical-features))

;; Using loop collection avoids the copying reverse   11/27/97 -- JCMa.
(defun initialize-document-context (context-object &key in-document-universe in-tokenizer-names in-tokenizers in-lexical-features)
  (let ((tokenizer-name-list in-tokenizer-names)
        (tokenizer-list in-tokenizers)
        (token-cluster-list nil))
    (if tokenizer-list
        (loop for item in tokenizer-name-list
              do (push (get-tokenizer-and-cluster in-document-universe item) token-cluster-list))
        (loop for item in tokenizer-name-list
              do (multiple-value-bind (token-cluster tokenizer)
                     (get-tokenizer-and-cluster in-document-universe item)
                   (push token-cluster token-cluster-list)
                   (push tokenizer tokenizer-list))))
    (with-slots (document-universe token-clusters tokenizers tokenizer-names lexical-features) context-object
      (setf document-universe in-document-universe
            tokenizers (reverse tokenizer-list)
            tokenizer-names tokenizer-name-list
            lexical-features in-lexical-features
            token-clusters (reverse token-cluster-list)))
    context-object))

(defgeneric obtain-token-clusters (context)
  (:documentation "Returns the token-cluster from the CONTEXT."))

(defmethod obtain-token-cluster ((context document-context))
  (with-slots (token-clusters) context
    (car token-clusters)))

(defmethod obtain-document-universe ((context document-context))
  (with-slots (document-universe) context
    document-universe))

(defmethod obtain-universe-tokenizer-feature ((context document-context))
  (with-slots (document-universe tokenizers lexical-features) context
    (values document-universe (car tokenizers) (car lexical-features))))

(defmethod obtain-universe-tokenizer-cluster-feature ((context document-context))
  (with-slots (document-universe tokenizers token-clusters lexical-features) context
    (values document-universe (car tokenizers) (car token-clusters) (car lexical-features))))
