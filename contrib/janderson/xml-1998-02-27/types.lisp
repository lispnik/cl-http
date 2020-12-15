;;; -*- mode: LISP; package ("XML-PARSER")
;;;
;;; this version (C) mecom gmbh 24.11.97
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.

#|
<DOCUMENTATION>
<DESCRIPTION>
</DESCRIPTION>
<CHRONOLOGY>
<DATE>19971205</DATE>
 <DELTA>jaa<BR>
 added markers. they are instances, and thus unique, which the reader and parser
 uses as special tags. they are now objects rather than uninterned symbols to
 distinguish them for dispatching - which begins to matter in malformed data.
  </DELTA>
</CHRONOLOGY>
</DOCUMENTATION>
 |#


(in-package :XML-PARSER)


(deftype reserved-dtd-name ()
  '(member xml::cdata xml::\#pcdata xml::\#rcdata
    XML::ANY XML::EMPTY))

(defun reserved-name-p (name)
  (if (and (consp name) (null (rest name)))
    (reserved-name-p (first name))
    (or (typep name 'reserved-dtd-name)
        (and (symbolp name)
             (reserved-name-indicator-p (char (string name) 0))))))

(defun reserved-name-indicator-p (char)
  (char= char #\#))


:EOF
