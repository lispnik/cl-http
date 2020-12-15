;;; -*- mode: lisp; package ("DOM") -*-
;;;
;;; this version (C) mecom gmbh 24.11.97
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.
;;;
;;; this is just a reminder for now, that a DOM-conforming interface would be
;;; nice.

(defPackage "DOM" (:use "CL" #+:ccl "CCL" "XML-PARSER"))

(in-package :DOM)

;;; named character and numeric character references

(defMethod node.get-replacement-text
           ((datum xml-numeric-entity-reference))
  (dtd-entity.content (xml-node.referent datum)))


(defMethod node.original
           ((datum xml-numeric-entity-reference))
  (xml-node.content datum))

(defMethod node.character
           ((datum xml-numeric-entity-reference))
  (xml-node.value datum))

:EOF
