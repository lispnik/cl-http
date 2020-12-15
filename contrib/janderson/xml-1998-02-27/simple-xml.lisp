;;; -*- Package: ("SIMPLE-XML") -*-
;;;
;;; this is an absolutely minimal and nonconforming xml processor.
;;; it reads the main document entity, skipping all processing
;;; instructions, declarations, and definitions. it returns only the
;;; single element in the document. it recognizes entities for <>&
;;; and translates them on input and output. no other entites are
;;; supported.

(defPackage "SIMPLE-XML" (:nicknames "SXML"))
(in-package "SXML")
(export '(simple-element
          element.name
          element.parent
          element.content
          element.attributes
          element-write
          element-read
          element.content-element
          element-bind
          element-typep
          element-append
          document-read))

(defVar *element-level* 0)

(defClass simple-element ()
  ((name
    :initarg :name
    :initform 'no-name
    :accessor element.name
    :type string)
   (parent
    :initarg :parent
    :initform nil
    :accessor element.parent
    :type (or simple-element null))
   (content
    :initarg :content
    :initform nil
    :accessor element.content
    :type list)
   (attributes
    :initarg :attributes
    :initform nil
    :accessor element.attributes
    :type string)))

(defMethod element.name ((element string)) "")

(defMethod initialize-instance :after
           ((self simple-element) &key &aux name)
  (typecase (setf name (element.name self))
    (string t)
    (null (error "element type missing: ~s." self))
    (symbol (setf (element.name self) (string name)))
    (t (error "element type illegal: ~s." self)))
  (dolist (element (element.content self))
    (etypecase element
      (simple-element (setf (element.parent element) self))
      (string t))))

(defun simple-element
       (name &rest content)
  (make-instance 'simple-element
    :name (string name)
    :content content))

(defMethod element-append
           ((element simple-element) (string string))
  (setf (element.content element)
        (nconc (element.content element) (list string))))

(defMethod element-append
           ((element simple-element) (child simple-element))
  (setf (element.content element)
        (nconc (element.content element) (list child)))
  (setf (element.parent child) element))
        


(defMethod element-write
           ((element simple-element) stream
            &aux (content (element.content element))
                 (name (element.name element))
                 (attributes (element.attributes element)))
  (format stream "<~a~@[ ~a~]" name attributes)
  (if *print-pretty*
    (if (> (length content) 1)
      (let ((*element-level* (1+ *element-level*)))
        (dolist (e content)
          (fresh-line stream)
          (dotimes (x *element-level*) (write-char #\space stream))
          (element-write e stream))
        (fresh-line stream)
        (dotimes (x *element-level*) (write-char #\space stream)))
      (element-write (first content) stream))
    (dolist (e content)
      (element-write e stream)))
  (format stream "</~a>" name))

(defMethod element-write
           ((string string) stream &aux char)
  (multiple-value-bind (writer arg)
                       (stream-writer stream)
    (dotimes (x (length string))
      (setf char (char string x))
      (case char
        (#\< (write-string "&lt" stream))
        (#\> (write-string "&gt" stream))
        (#\& (write-string "&amp" stream))
        (t (funcall writer arg char))))))

(defmethod element-read-type
           (stream)
  (peek-char #\< stream)
  (multiple-value-bind (reader arg) (stream-reader stream)
    (funcall reader arg)
    (let ((string (make-array 8 :element-type 'character
                              :fill-pointer 0
                              :adjustable t))
          (char nil))
        (loop (setf char (funcall reader arg))
              (case char
                (#\> (unread-char char stream) (return string) )
                ((#\space #\return #\linefeed #\tab)
                 (return string))
                (t
                 (vector-push-extend char string)))))))

(defun skip-string
       (end stream &aux char)
  (multiple-value-bind (reader arg) (stream-reader stream)
    (loop (setf char (or (funcall reader arg)
                         (error 'end-of-file :stream stream)))
          (when (char-equal end char)
            (return))
          (when (char-equal #\[ char)
            (skip-string #\] stream)))))

(defun read-attributes
       (stream &aux char (attributes (make-array 8 :adjustable t
                                                 :fill-pointer 0
                                                 :element-type
                                                 'character)))
  (multiple-value-bind (reader arg) (stream-reader stream)
    (loop (setf char (or (funcall reader arg)
                         (error 'end-of-file :stream stream)))
          (when (char-equal #\> char)
            (return))
          (vector-push-extend char attributes)))
  attributes)

(defun skip-entity
       (stream)
  (skip-string #\; stream))

(defun skip-tag
       (stream)
  (skip-string #\> stream))

(defun close-tag-read
       (stream)
  (skip-tag stream)
  nil)

(defun pi-read
       (stream)
  (skip-tag stream)
  (element-read stream))

(defun definition-read
       (stream)
  (skip-tag stream)
  (element-read stream))

(defun element-read
       (stream &aux (type (element-read-type stream))
                    (element nil)
                    (attributes nil)
                    (child nil))
  (case (elt (string type) 0)
    (#\/ (close-tag-read stream))
    (#\? (pi-read stream))
    (#\! (definition-read stream))
    (t
     (setf attributes (read-attributes stream))
     (setf element (make-instance 'simple-element
                     :name type
                     :attributes attributes))
     (loop
       (setf child (element-read-content stream))
       (etypecase child
         (string (element-append element child))
         (simple-element (element-append element child))
         (null (return-from element-read element)))))))

(defun element-read-content
       (stream)
  (if (char= #\< (peek-char nil stream))
    (element-read stream)
    (multiple-value-bind (reader arg) (stream-reader stream)
      (let ((string (make-array 8 :element-type 'character
                                :fill-pointer 0
                                :adjustable t))
            (char nil))
        (loop (setf char (funcall reader arg))
              (case char
                (#\&
                 (setf char
                       (string
                        (first (read-delimited-list #\; stream))))
                 (cond ((char-equal char "amp") (setf char #\&))
                       ((char-equal char "lt") (setf char #\<))
                       ((char-equal char "gt") (setf char #\>))
                       (t
                        (error "unknown escape: &~a;." char)))
                 (vector-push-extend char string))
                (#\<
                 (stream-untyi stream char)
                 (return string))
                (nil
                 (error 'end-of-file :stream stream))
                (t
                 (vector-push-extend char string))))))))

(defMethod element-typep
           ((element simple-element) (type string))
  (string-equal type (element.name element)))

(defMethod element.content-element
           ((element simple-element) (type symbol))
  (element.content-element element (string type)))

(defMethod element.content-element
           ((element simple-element) (type string))
  (find type (element.content element)
        :test #'string-equal :key #'element.name))

(defMacro element-bind
          ((&rest types) element &rest body
           &aux (binding (make-symbol "ELEMENT")))
  `(let ((,binding ,element))
     (let ,(mapcar #'(lambda (type)
                       `(,type
                         (element.content-element ,binding ',type)))
                   types)
       ,@body)))
                    
;;;
;;;

(defun document-read
       (stream &aux element)
  (loop (typecase (setf element (element-read stream))
          (simple-element (return element))
          (null nil)
          (t (warn "unexpected element: ~s." element)))))
        
#|
(defparameter *test*
   (sxml-element 'test
               "asdf" (sxml-element 'test2 "qwer" "<>")))

(print-element *test* *standard-output*)
(element-bind (name value) entry (print (list name value)))
(element.content (element.content-element *test* 'test2))

(with-input-from-string
  (stream
   "<?xml version='1.0'>
    <!DOCTYPE asdf [<!ELEMENT asdf #PCDATA>]>
    <asdf attribute1='first' attribute2='second'>testing</asdf>")
  (document-read stream))
 |#
(provide "SIMPLE-XML")
