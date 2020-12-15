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

(define-class-local name-mixin nil () object-name)

(define-class-local hashed-array
                    hashed-array
  ()
  access-hash  
  linear-structure  
  name-structure)

(define-class-local numerological-hashed-array nil (hashed-array))

(define-class-local caching-mixin
                    cache
  ()
  storage)

(define-class-local table-caching-object nil (caching-mixin))
(define-class-local a-list-caching-object nil (caching-mixin))
(define-class-local freshness-mixin nil () freshness-storage)
(define-class-local table-caching-object-with-freshness nil (freshness-mixin table-caching-object))
(define-class-local a-list-caching-object-with-freshness
                    nil
  (freshness-mixin a-list-caching-object))

(define-class-local tagged-array
                    t-array
  (a-list-caching-object-with-freshness)
  linear-object
  tags)

(define-class-local hashed-tagged-array
                    h-t-array
  (tagged-array)
  names)

(define-class-local base-computation-type
                    nil
  (name-mixin)
  code
  expected-arguments
  sorting-factor)

(define-class-local computation-substrate
                    nil
  (name-mixin)
  substrate-type)

(define-class-local static-base-computation
                    nil
  (computation-substrate a-list-caching-object-with-freshness))

(define-class-local dynamic-base-computation
                    nil
  (computation-substrate a-list-caching-object))

(define-class-local composite-computation-type
                    nil
  (base-computation-type)
  subordinate-computations
  inherited-arguments
  personal-arguments)

(define-class-local static-composite-computation
                    nil
  (static-base-computation))

(define-class-local dynamic-composite-computation
                    nil
  (dynamic-base-computation))

(define-class-local constraint-set
                    nil
  (composite-computation-type))

(define-class-local conjunctive-constraint-set
                    nil
  (constraint-set))

(define-class-local disjunctive-constraint-set
                    nil
  (constraint-set))

(define-class-local token-object
                    token
  (name-mixin)
  datum)

(define-class-local skinny-token-mixin
                    nil
  (token-object a-list-caching-object))

(define-class-local token-feature
                    nil
  (token-object))

(define-class-local simple-lexical-feature
                    nil
  (skinny-token-mixin)
  in-mask
  out-mask)

(define-class-local general-lexical-feature
                    nil
  (skinny-token-mixin)
  masks
  index-for-masks
  combination-function)

(define-class-local lexical-feature-set
                    nil
  (name-mixin)
  pre-applied-constraints
  lexical-features
  document-storage-function
  feature-storage-function)

(define-class-local token-cluster
                    nil
  (hashed-array)
  hard-sparsification-bound
  sparsification-fraction)

(define-class-local mapping-token-cluster
                    nil
  (token-cluster)
  mapping-functions
  mapping-tables)

(define-class-local tokenizer
                    tokenizer
  (name-mixin)
  code
  initialization-function
  storage-function
  output-function
  formats-supported)

(define-class-local bitvector-storing-tokenizer
                    tokenizer
  (tokenizer)
  default-size
  string-index
  size-increment)

(define-class-local document
                    doc
  (name-mixin a-list-caching-object)
  pdi   
  label  
  format)

(define-class-local document-universe
                    doc-u
  (name-mixin)

  documents                                     ; a hashed-tagged-array
  tokens                                        ; a hash-table indexed by tokenizers
  tokenizers                                    ; a hashed-array of tokenizers
  access-codes                                  ; a hash-table of code objects detailing access to the documents
  supported-formats)                            ; a list of permissible formats for accessing documents

(define-class-local document-context
                    nil
  (name-mixin)
  document-universe
  token-clusters
  tokenizers
  lexical-features
  tokenizer-names)

(define-class-local search-constraint
                    nil
  ()
  search-arguments
  args-to-search-specifier-function
  limiting-arguments
  limiting-function)

(define-class-local search-constraint-set
                    nil
  ()
  search-constraints
  combination-function)
