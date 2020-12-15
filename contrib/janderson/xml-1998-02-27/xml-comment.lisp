;;; -*- mode: lisp; package "XML-PARSER" -*-

(in-package :xml-parser)

(defMethod node-class
           ((node (eql 'comment)) (op t) (context t))
  *comment-class*)

(defClass xml-comment (xml-node)
  ((content
    :accessor xml-comment.data)))

(defMethod print-object
           ((datum xml-comment) stream)
  (if *xml-print-readably*
    (typecase *parent-node*
      (dtd-element (write-string (xml-comment.data datum) stream))
      (t
       (let ((*parent-node* datum))
         (format stream "<!-- ~a -->" (xml-comment.data datum)))))
    (print-unreadable-object (datum stream :type t)
      (format stream "-- ~a" (xml-comment.data datum)))))


(defMethod normalize-comment-data-type
           ((comment xml-comment) (data string))
  data)

(defMethod normalize-comment-data-type
           ((comment xml-comment) (data list))
  (format nil "~{~a~^ ~}" (remove 'xml::-- data)))

(defMethod normalize-comment-data-type
           ((comment xml-comment) (data t))
  (write-to-string data))

(defMethod normalize-comment-data
           ((comment xml-comment))
  (setf (xml-comment.data comment)
        (normalize-comment-data-type comment (xml-comment.data comment))))


(defmethod initialize-instance :after
           ((self xml-comment) &key)
  (normalize-comment-data self))

(defMethod read-typed-markup-declaration
       ((name (eql 'xml::--)) (stream t))
  (make-instance (node-class 'comment name stream)
    :content (read-string-delimited-string "-->" stream t)))

(defMethod xml-node.append-element
           ((node dtd-element-declaration) (element xml-comment))
  (push element (dtd-element-declaration.comments node))
  element)

;;; augment the parent with the 
(defun cache-comment-element
       (parent comment &aux key)
  (setf key (if (listp (xml-node.content parent))
              (or (first (last (xml-node.content parent))) parent)
              (xml-node.content parent)))
  (setf (getf (xml-node.comments parent) key)
        (nconc (getf (xml-node.comments parent) key) (list comment))))

(defMethod xml-node.append-element
           ((parent xml-comment-node) (element xml-comment))
  (cache-comment-element parent element))

(defMethod process-markup-element
           ((element xml-comment) (stream t))
  (when *xml-preserve-comments*
    (xml-node.append-element *parent-node* element)))



:EOF
