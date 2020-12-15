;;; -*- mode: lisp; package "CL-USER" -*-

(in-package :cl-user)

(let ((*default-pathname-defaults* (if *load-truename*
                                     (make-pathname :name nil
                                                    :defaults *load-truename*)
                                     *default-pathname-defaults*)))
  (let (#+:allegro
        (sys:*load-search-list* (list (make-pathname :type "fasl"
                                                     :defaults *default-pathname-defaults*)
                                      (make-pathname :type "lisp"
                                                     :defaults *default-pathname-defaults*))))
  (declare (special *default-pathname-defaults*))
  (map nil #'load
       '(
         "exports"                      ; publish interface symbols
         "types"                        ; 
         "tokens"
         "global-bindings"              ; define interface variables and functions
         "interface"                    ; the interface to the parser and data manipulation
                                        ; functions in terms of direct CLOS accessors and
                                        ; auxiliaries

         "xml-node"                     ; the abstract constituent node class.
         "markup-reader"                ; primitive read/macro functions for tokenizing
                                        ; a stream of marked up data.

         "doctype"
         "dtd-element"                  ; element definitions
         "dtd"                          ; specifics for document type definitions
         "dtd-element-declaration"      ; structural / method definitions for the specific
         "dtd-element-reference"        ; components and reader extensions for dtd
         "dtd-attdef"                   ; components
         "dtd-model"                    ; (see below for the model compiler)
         "dtd-entity"                   ; 
         "dtd-notation"                 ;
         ; "dtd-document"               ; 19971218 eliminated
                                        ; 
         "xml-document"                 ; specifics for xml documents
         "xml-text"                     ; structural / method definitions for the specific
         "xml-attribute"                ; components ...
         "xml-pi"                       ; 
         "xml-element"                  ; ... and parsing and internalization for
         "xml-comment"                  ; xml-markup streams and for
                                        ; 
         "xml-parser"                   ; complete xml documents
         "dtd-parser"                   ; adds general reader support for dtd
                                        ; components and for
                                        ; complete document type descriptions
         "XapiJ"                        ; provisional interfaces according to the
                                        ; respective standards

         "xml-error"                    ; an absolutely minimal error facility
         "xml-pattern"                  ; an element pattern matcher

         "dtd-model-compiler"           ; a compiler for model verification predicates
         
         #+:mcl
         "menu"                         ; menus to inspect definitions

         #+:cl-http
         "server"                       ; bare-bones support for document
                                        ; generation within the cl-http server
         ))))

(provide :xml)
