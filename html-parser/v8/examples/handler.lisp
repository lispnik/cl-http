;;; -*- Mode: Lisp -*-
;;; Last edited by smishra on Wed Jul 30 23:24:55 1997

;;; File: handler.lisp
;;; (c) Copyright 1996-97, Sunil Mishra (smishra@cc.gatech.edu)
;;;     All Rights Reserved

(in-package :html-parser)

;;; Handler management
(eval-when (load compile eval)
  (defvar *tag-handlers* nil))

(defstruct handler
  tags with-attributes without-attributes test function)

(defun call-handler (item)
  (if (atom *tag-handlers*)
      (funcall (handler-function *tag-handlers*) item)
      (let ((handler-rec (find (typecase item
                                 (abstract-tag-instance (name item))
                                 (t #t"PCDATA"))
                               *tag-handlers*
                               :key #'handler-tags
                               :test #'member)))
        (and handler-rec
             (funcall (handler-function handler-rec) item)))))

(defmacro with-tag-handler ((tags handler-fn &key with-attributes without-attributes test)
                            &body body)
  ;; Most of this code is error testing
  "This macro is for defining the contexts in which parse structures that will
be returned to the caller (via the handler function) may be constructed. It
defines a handler on a group of tags. All arguments may take either symbols,
quoted symbols or strings as input data.

TAGS is a tag or a list of tags for which a context may be established. A value
        of t may be supplied to represent any tag, in which case no arguments
        may be defined. (This follows from the fact that PCDATA can have no
        attributes.)

WITH-ATTRIBUTES is a list of attributes that must be present for a context
        to be opened. It cannot share tags with WITHOUT-ATTRIBUTES.

WITHOUT-ATTRIBUTES is a list of attributes that must be absent for a context
        to be opened. It cannot share tags with WITH-ATTRIBUTES.

TEST is a generalized test function that is called with the data type (either
        :OPEN-TAG or :PCDATA) that must return true for a context to be
        opened.

A context is opened only if the conjunction of all the above conditions hold.

HANDLER-FN is the handler function that must be called after an open context
        is closed. It is called with the data item for which the context was
        opened as the sole argument."
  (flet ((to-string (name)
           (etypecase name
             (symbol (symbol-name name))
             (string name)
             (list (assert (eql (car name) 'quote)
                           ()
                           "~A is not a valid name" name)
                   (symbol-name (cadr name)))))
         (test-tag (tag with-attrs without-attrs)
           (let ((tag-defn (tag-definition tag)))
             (assert (or (member tag *character-data-classes*)
                         tag-defn)
                     ()
                     "~A does not correspond to a tag name or character data class" tag)
             (assert (every #'(lambda (attr) (find attr (attributes tag-defn) :key #'name))
                            with-attrs)
                     ()
                     "~A contains undefined attribute names" with-attrs)
             (assert (every #'(lambda (attr) (find attr (attributes tag-defn) :key #'name))
                            without-attrs)
                     ()
                     "~A contains undefined attribute names" without-attrs)
             (assert (null (intersection with-attrs without-attrs))
                     ()
                     "~A and ~A cannot share tokens" with-attrs without-attrs)))
         (true (&rest args)
           (declare (ignore args))
           t))
    (declare (dynamic-extent #'to-string #'test-tag))
    (let* ((tag-strings (cond ((eql tags t) t)
                              ((atom tags) (list (to-string tags)))
                              (t (mapcar #'to-string tags))))
           (tag-tokens (if (eql tags t)
                           t
                           (mapcar #'tokenize-name tag-strings)))
           (with-attr-strings (mapcar #'to-string with-attributes))
           (with-attr-tokens (mapcar #'tokenize-name with-attr-strings))
           (without-attr-strings (mapcar #'to-string without-attributes))
           (without-attr-tokens (mapcar #'tokenize-name without-attr-strings)))
      (cond ((eql tag-strings t)
             (assert (and (null with-attr-strings) (null without-attr-strings))
                     ()
                     "with-attributes and without-attributes cannot be specified when tags is t")
             (assert (null *tag-handlers*)
                     ()
                     "No other handlers may be specified with a universal specification"))
            (t (mapc #'(lambda (tag-name)
                         (test-tag tag-name with-attr-tokens without-attr-tokens))
                     tag-tokens)
               (assert (or (null *tag-handlers*) (consp *tag-handlers*))
                       ()
                       "No other handlers may be specified with a universal specification")))
      `(let ((*tag-handlers* ,(if (eql tag-strings t)
                                  `(make-handler
                                    :tags t
                                    :test ,(or test #'true)
                                    :function ,handler-fn)
                                  `(cons (make-handler
                                          :tags (mapcar #'tokenize-name ',tag-strings)
                                          :with-attributes (mapcar #'tokenize-name ',with-attr-strings)
                                          :without-attributes (mapcar #'tokenize-name ',without-attr-strings)
                                          :test ,(or test #'true)
                                          :function ,handler-fn)
                                    *tag-handlers*))))
        (declare (dynamic-extent *tag-handlers*))
        ,@body))))

(defun open-context-p (tag-type tag-data)
  (flet ((match-attr-val-pair (attr-name attr-val-pair)
           ;; conditionalized to avoid belowing out on this: 
           ;; ATTR-NAME: $SRC$
           ;; ATTR-VAL-PAIR: (NIL . $BORDER-0$) -- 8/18/96 -- JCMa.
           (let ((attr (car attr-val-pair)))
             (and attr-name attr (eql attr-name (name attr))))))
    (declare (dynamic-extent #'match-attr-val-pair))
    (if (consp *tag-handlers*)
        (let ((handler-rec (find (ecase tag-type
                                   (:pcdata #t"PCDATA")
                                   (:open-tag (name tag-data)))
                                 *tag-handlers*
                                 :key #'handler-tags
                                 :test #'member)))
          (and handler-rec
               (or (and (eql tag-type :pcdata)
                        (funcall (handler-test handler-rec) :pcdata tag-data))
                   (and (subsetp (handler-with-attributes handler-rec) (attr-values tag-data)
                                 :test #'match-attr-val-pair)
                        (null (intersection (handler-without-attributes handler-rec)
                                            (attr-values tag-data)
                                            :test #'match-attr-val-pair))
                        (funcall (handler-test handler-rec) tag-type tag-data)))))
        (funcall (handler-test *tag-handlers*) tag-type tag-data))))

(define-html-parser-context handler-parse-context (tags)
  :use-variables open-contexts
  :on-open-tag (:any
                (cond ((open-context-p :open-tag it)
                       (save it)
                       (push it open-contexts))
                      ((or (eq tags t)
                           (member (name it) tags)
                           (and (typep it 'unknown-tag-instance)
                                (member #t"UNKNOWN" tags)))
                       (save it))))
  :on-pcdata (cond ((open-context-p :pcdata it)
                    (save it)
                    (call-handler it))
                   ((or (eq tags t) (member #t"PCDATA" tags))
                    (save it)))
  :on-close-tag (:any
                 (if (member it open-contexts)
                   (call-handler it))))

(define-html-parser parse-with-handlers (&key (tags t))
  :initialization (and (listp tags) (tokenize-list tags #'tokenize-name))
  :transitions (:start (handler-parse-context tags))
               (handler-parse-context t :end))

(defun html-parse-result (filename &aux html-doc)
  (unless *current-dtd*
    (initialize-parser))
  (with-tag-handler (html #'(lambda (tag-data) (setq  html-doc tag-data)))
    (parse-with-handlers (file->string filename)))
  html-doc)
