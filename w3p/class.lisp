;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: www-present -*-

;;; (C) Copyright 1996, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;
;;; Christopher R. Vincent
;;; cvince@ai.mit.edu
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CLASSES FOR W3P PRESENTATION SYSTEM
;;;

(in-package :www-present)

(defclass presentation-type-class () ()
  (:documentation "Internal classes built on presentation-type-class allow method dispatch on presentation-types"))

(defclass presentation-type ()
    ((name :initarg :name :accessor presentation-type-name)
     (class :initarg :class :accessor presentation-type-class)
     (parameters :initarg :parameters :initform nil :accessor presentation-type-parameters)
     (options :initarg :options :initform nil :accessor presentation-type-options)
     (option-accept-args :initarg :option-accept-args :initform nil :accessor presentation-type-option-accept-args)
     (superiors :initarg :superiors :initform nil :accessor presentation-type-superiors)
     (inherit-from :initarg :inherit-from :initform nil :accessor presentation-type-inherit-from)
     (description :initarg :description :initform nil :accessor presentation-type-description)
     (class-prototype :initarg :class-prototype :initform nil :accessor presentation-type-class-prototype))
  (:documentation 
    "The class for presentation-type
OPTION-ACCEPT-ARGS is an alist matching options to presentation-types and arguments to accept
SUPERIORS is a list of presentation-type instances to inherit from
CLASS-PROTOTYPE provides an instance to use in generic function dispatch"))
