;;; -*- package: ("XML-PARSER") -*-
;;;
;;; this version (C) mecom gmbh 24.11.97
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.

#|
<DOCUMENTATION>
 <DESCRIPTION>
  </DESCRIPTION>
 <CHRONOLOGY>
  <DELTA><DATE>19971125</DATE>
   XML-USER package </DELTA>
  <DELTA><DATE>19971203</DATE>
   method for <CODE>PARSE-POSITION</CODE> for use in error or
   other messages.</DELTA>
  <DELTA><DATE>19971205</DATE>jaa<BR/>
   added HANDLER-CASE to primary interface functions READ-PROCESS-PCDATA,
   READ-PROCESS-CDATA
  <DELTA><DATE>19971217</DATE>
   <UL><LI>'%' is a terminating character
   <LI>added read-entity-data to distinguish the effect of '%' from its
    behaviour in pcdata
   </UL>
   </DELTA>
  <DELTA><DATE>19980108</DATE>
   made READ-CONTENT-MODE robust in the presence of SGML syntax
   <DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
 |#

(in-package :xml-parser)

;; readtable definition is at the end, as allegro insists on preexisting
;; function definitions

(defun make-markup-package
       (name)
  (or (find-package name)
      (make-package name :use (list *markup-package*))))

(defMethod markup-package
           ((stream t))
  (if (find *markup-package* (package-use-list *package*))
    *package*
    *markup-user-package*))

(defMethod markup-package
           ((name symbol))
  (or (symbol-package name) *markup-user-package*))



(defParameter *entity-string-stream* 'entity-string-stream)
(defClass parse-position ()
  ((source :initarg :source :initform ""
           :reader parse-position.source
           :type (or dtd-named-entity stream))
   (element :initarg :element :initform *processed-node*
            :reader parse-position.element
            :type (or null xml-node))
   (offset :initarg :offset :initform 0
           :reader parse-position.offset
           :type integer)))
#+:ccl
(defClass entity-string-stream (ccl::string-input-stream)
  ((entity :initarg :entity :initform nil
           :reader stream.entity)))
#+:allegro
(defClass entity-string-stream (excl::string-input-stream)
  ((entity :initarg :entity :initform nil
           :reader stream.entity)))
#-(or ccl allegro)
(error "no definition for entity-string-stream is possible")

(defMethod print-object
           ((datum parse-position) stream)
  (print-unreadable-object (datum stream)
    (format stream "~s @ ~s (after ~s)"
            (parse-position.source datum) (parse-position.offset datum)
            (parse-position.element datum))))

;;; see also "dtd-entity.lisp" for the method which prepends an entity to
;;; the stream

(defMethod prepend-stream
           ((datum string) (stream concatenated-stream))
  (prepend-stream (make-string-input-stream datum) stream))

(defMethod prepend-stream
           ((datum symbol) (stream concatenated-stream))
  (prepend-stream (string datum) stream))

(defMethod prepend-stream
           ((datum null) (stream concatenated-stream))
  stream)

(defMethod prepend-stream
           ((pre-stream stream) (stream concatenated-stream))
  ;; this appears to work ok for parameter entities, since the occasion
  ;; arises only at token boundaries
  (setf (rest (concatenated-stream-streams stream))
        (cons (first (concatenated-stream-streams stream))
              (rest (concatenated-stream-streams stream))))
  (setf (first (concatenated-stream-streams stream)) pre-stream)
  stream)

(defMethod markup-stream
           ((stream stream))
  (make-concatenated-stream stream))

(defMethod markup-stream
           ((stream concatenated-stream))
  stream)


(defMethod parse-position
           ((stream null))
  (make-instance 'parse-position))

(defMethod parse-position
           ((stream concatenated-stream))
  (parse-position (first (concatenated-stream-streams stream))))

(defMethod parse-position
           ((stream file-stream))
  (make-instance 'parse-position
    :source (namestring stream) :offset (stream-position stream)))

(defMethod parse-position
           ((stream entity-string-stream))
  (make-instance 'parse-position 
    :source (stream.entity stream) :offset (stream-position stream)))

(defMethod parse-position
           ((stream string-stream))
  (make-instance 'parse-position
    :source stream :offset (stream-position stream)))

(defMethod stream-position ((stream null))
  -1)

(defMethod stream-position ((stream file-stream)) (file-position stream))

(defMethod stream-position ((stream string-stream))
  #+:CCL (slot-value stream 'ccl::index)
  #+:ALLEGRO (slot-value stream 'excl::charpos)
  #-(or CCL ALLEGRO) 0)

(defMethod stream-position ((stream concatenated-stream))
  (stream-position (first (concatenated-stream-streams stream))))


;;;
;;; delimited strings are read by augmenting a temporary store until the
;;; delimiter appears. for this purpose a string stream is used...

(defparameter *string-buffer-stream* (make-string-output-stream))

(defun read-delimited-string
           (predicate &optional (stream *standard-input*) (read-terminator nil)
            &aux (output *string-buffer-stream*) nc)
  "read the content of an element as a string."
  (loop (setf nc (peek-char nil stream nil nil))
        (unless (and nc (not (funcall predicate nc))) (return))
        (write-char (read-char stream) output))
  (values (get-output-stream-string *string-buffer-stream*)
          (when read-terminator (read-char stream))))

(defun read-string-delimited-string
       (string &optional (stream *standard-input*) (read-terminator nil)
               (count 0) (length (length string)))
  (multiple-value-bind
    (read-string terminator)
    (read-delimited-string
     #'(lambda (char)
         (if (char= char (char string count))
           (if (>= (incf count) length)
             count
             nil)
           (progn (setf count 0) nil)))
     stream
     t)
    (if read-terminator
      (concatenate 'string read-string
                   (make-string 1 :initial-element terminator))
      (subseq read-string 0 (max 0 (- (length read-string) (1- length)))))))

;;;
;;; the interface function all perform necessary binding to ensure that the
;;; correct read table is in place.
;;; the -stream version also binds the reader package

(defMethod read-markup-stream
           ((stream string) &optional (type t))
  (with-input-from-string (string-stream stream) (read-markup-stream string-stream type)))

(defMethod read-markup-stream
           ((stream stream) &optional (type t))
  (read-markup-stream (markup-stream stream) type))

(defMethod read-markup-stream
           ((stream concatenated-stream) &optional (type t))
  "tokenize and parse a stream with the markup-specific readtable.
   the default package is assured to be one in which the standard xml symbols are present."
  (let ((results nil)
        (form nil)
        (*readtable* *markup-pcdata-readtable*)
        (*package* (markup-package stream))
        (eof '#.(gensym)))
    (loop (setf form (read-process-pcdata stream nil nil eof))
          (when (eq eof form)
            (return-from read-markup-stream (nreverse results)))
          (when (typep form type)
            (push form results)))))

(defMethod read-markup-stream
           ((pathname t) &optional (type t))
  (with-open-file (stream pathname :direction :input)
    (read-markup-stream stream type)))


(defMethod read-markup-element
           ((stream string))
  (with-input-from-string (string-stream stream)
    (read-markup-element string-stream)))

(defMethod read-markup-element
           ((stream concatenated-stream))
  (read-pcdata stream))

(defMethod read-markup-element
           ((stream stream))
  (read-markup-element (markup-stream stream)))

;;;
;;; the standard proposes regions in the unicode space which fit various
;;; categories. for now, just note them

(defun xml-base-char? (char) (alpha-char-p char))
(defun xml-ideographic-char? (char) (declare (ignore char)) nil)
(defun xml-name-char? (char) (graphic-char-p char))


;;;
;;; read tags into the current package by default, or allow the package
;;; to be specified for specific forms (eg dtd elements)
;;; note that it may be presented with a list of tag names - as in an attlist

(defun read-markup-tag-type
       (stream &optional (*package* *package*)
               &aux (*readtable* *markup-type-readtable*) tag)
  (flet ((bad-tag (tag)
           (xml-form-error stream "illegitimate tag: ~a." tag)
           nil))
    (flet ((markup-tag? (tag)
             (if (let* ((string (string tag))
                        (first (char string 0)))
                     (and (or (xml-base-char? first)
                              (xml-ideographic-char? first)
                              (char= first #\_)
                              (char= first #\:))
                          (not (find-if-not #'xml-name-char? string
                                            :start 1))))
               tag
               (bad-tag tag))))
      (setf tag (read stream t t t))
      (typecase tag
        (symbol (if (eq tag 'xml::--)
                  tag
                  (markup-tag? tag)))
        (string (markup-tag? (intern tag)))
        (cons (mapcar #'(lambda (x)
                          (typecase x
                            (symbol (markup-tag? x))
                            (string (markup-tag? (intern x)))
                            (marker-tag x)
                            (t (bad-tag x))))
                      tag))
        (marker-tag tag)
        (t (bad-tag tag))))))

(defun read-markup-model
       (stream &aux (*readtable* *markup-model-readtable*)
               (model nil) char)
  (handler-bind
    ((stream-error #'(lambda (condition) condition))
     (error #'(lambda (condition)
                (warn "error in model in stream ~s:~%~a"
                      (parse-position stream) condition)
                (when *xml-handle-errors*
                  (warn "(ANY assumed)")
                  (return-from read-markup-model 'XML::ANY)))))
    ;; read the model; skip '-' and 'O' occurrence markers from sgml
    ;; and catch a missing model
    (loop (if (char-equal #\> (peek-char t stream t t t))
            (xml-form-error stream "missing content model."))
          (setf model (read stream t t t))
          (unless (or (symbolp model) (consp model))
            (xml-form-error stream "illegitimate content model: ~s." model))
          (unless (and (symbolp model)
                       (or (string-equal "O" (string model))
                           (string-equal "-" (string model))))
            (return)))
    ;; allow a dangling occurrence as the NEXT character in the stream
    (setf char (peek-char nil stream nil nil t))
    (if (find char "*+?")
      (list model (read stream t t t))
      model)))


;;;
;;; utilities for attribute normalization
;;; perform case folding during reading - if there is no dtd present that's not
;;; possible, since the elements are not linked to a definition

(defParameter *xml-upcased-attribute-types*
  '(xml::ID xml::IDREF xml::IDREFS xml::NMTOKEN xml::NMTOKENS xsl::TYPE))

(defMethod id-attribute?
           ((what t))
  nil)

(defMethod id-attribute?
           ((what symbol)
            &aux (attdef (when (and *parent-node* *dtd*)
                           (xml-node.get-attdef *parent-node* what))))
  (and attdef (find (dtd-attdef.type attdef) *xml-upcased-attribute-types*)))
;(id-attribute? 'xsl::type)
    
(defMethod normalize-attribute-value
           ((attribute t) (value string))
  ;; this should compress whitespace and perform parameter substitution
  (setf value (substitute-if #\space #'whitespacep value))
  (when (id-attribute? attribute)
    (setf value (intern (if (find-if #'lower-case-p value)
                          (string-upcase value)
                          value))))
  value)

(defMethod normalize-attribute-value
           ((attribute t) (value symbol))
  (when (and (id-attribute? attribute)
             (find-if #'lower-case-p (string value)))
    (setf value (intern (string-upcase (string value)))))
  value)

(defMethod normalize-attribute-value
           ((attribute t) (value list))
  (mapcar #'(lambda (v) (normalize-attribute-value attribute v)) value))


(defMethod normalize-attribute-value
           ((attribute t) (value t))
  value)

(defun normalize-attribute-list
       (rest &optional (list rest) &aux attribute)
  (cond ((consp (rest rest))
         (typecase (setf attribute (first rest))
           (string (setf (first rest) (setf attribute (intern (first rest)))))
           (symbol t)
           (t (xml-cell-error list "illegitimate attribute name: ~s."
                              attribute)))
         (cond ((eq attribute *open-tag-section-marker*)
                ;; where a section is present, stop normalizing
                list)
               (t
                (setf (second rest)
                      (normalize-attribute-value attribute (second rest)))
                (normalize-attribute-list (cddr rest) list))))
        (t
         list)))
;(normalize-attribute-list '(xml::id "asd" qwer " s d d"))

;;;
;;; these two differ in that the first replaces '=' with space and treats
;;; quotes as string delimiters

(defMethod read-markup-tag-attributes
           ((tag-name symbol) stream &aux (*readtable* *markup-attribute-readtable*))
  (normalize-attribute-list (read-delimited-list #\> stream t)))

(defMethod read-markup-tag-parameters
           ((tag-name symbol) (stream t)
            &aux (*readtable* *markup-attribute-readtable*))
  (read-delimited-list #\> stream t))


(defMethod read-markup-tag-parameters
           ((tag-name (eql *tag-end-marker*)) (stream t))
  nil)

(defMethod read-markup-tag-parameters
           ((tag-name t) (stream t))
  (xml-form-error stream "illegitimate tag: ~s." tag-name))


(defun element-reader-macro
       (stream char &aux (op (read-markup-tag-type stream)))
  "element reader macro for markup streams"
  (declare (ignore char))
  (unless op
    (xml-form-error stream "illegitimate processing instruction operator: ~s."
                    op))
  (read-typed-markup-element op stream))

(defun restricted-name-reader-macro
       (stream char &aux (*readtable* *markup-type-readtable*))
  ;; locate or intern the symbol in the markup package.
  ;; if it is new, then export it
  (declare (ignore char))
  (let ((string (concatenate 'string "#" (string (read stream t t t)))))
    (multiple-value-bind (token new?)
                         (intern string *markup-package*)
      (when new?
        (ignore-errors (export token *markup-package*)))
      token)))

#| parameter entities are always parsed.
  nb. this is in itself also obsolete
(defun parameter-entity-reader-macro
       (stream char &aux (*readtable* *markup-attribute-readtable*))
  "reader macro for entities for immediate substitution.
   the attribute readtable is used, since it shouldn't upcase"
  (declare (ignore char))
  (let* ((name (intern (read-delimited-string #'(lambda (c) (char= c #\;))
                                              stream  t)
                       *package*))
         (entity (dtd-entity name)))
    (if entity
      (xml-node.value entity)
      (progn
        (xml-form-error *dtd* "entity not declared: ~s." name)
        (values)))))
 |#

(defun parsed-parameter-entity-reader-macro
       (stream char)
  "read the entity reference, resolve it to a value, and recursively read
   with whatever readtable is active"
  (declare (ignore char))
  (let* ((name (intern (read-delimited-string #'(lambda (c) (char= c #\;))
                                              stream  t)
                       *package*))
         (entity (dtd-entity name)))
    (unless entity
      (xml-validity-error stream "entity not declared: ~s" name)
      (return-from parsed-parameter-entity-reader-macro (values)))
    (prepend-stream entity stream)
    (values)))


(defun entity-reader-macro
       (stream char
        &aux reference (verify? (xml-document.validate? *document-context*)))
  "reader macro for restricted &-entities markup streams"
  (declare (ignore char))
  (setf reference (read-delimited-string #'(lambda (c) (char= c #\;)) stream t))
  (cond ((zerop (length reference))
         (values))
        ((char-equal #\# (char reference 0))
         ;;; &#<number>; is a numeric entity reference
         (if (= (length reference) 1)
           (values)
           (let ((character 
                 (code-char (if (char-equal #\x (elt reference 1))
                              (parse-integer reference :start 2 :radix 16
                                             :junk-allowed (not verify?))
                              (parse-integer reference :start 2 :radix 16
                                             :junk-allowed (not verify?))))))
             (make-instance (node-class 'numeric-character-reference
                                        reference stream)
               :character character
               :original reference))))
        (t
         ;;; &<name>; is a named entity reference
         (setf reference (intern reference))
         (make-instance (node-class 'named-character-reference reference stream)
           :name reference))))

(defMethod read-typed-markup-element
           ((op (eql *processing-instruction-tag-marker*)) (stream stream))
  (read-typed-processing-instruction (read-markup-tag-type stream) stream))

(defMethod read-typed-processing-instruction
           ((tag t) (stream t))
  (warn "unknown processing instruction form : ~s . ~{~s~^ ~}"
        tag (read-markup-tag-parameters tag stream)))

(defMethod read-typed-markup-element
           ((typed (eql *section-tag-marker*)) (stream stream))
  (read-typed-section-element (read-markup-tag-type stream *markup-package*)
                              stream))

(defMethod read-typed-section-element
           ((tag t) (stream t))
  (warn "unknown section form : ~s ~{~s~^ ~}"
        tag (read-markup-tag-parameters tag stream)))

(defMethod read-typed-markup-element
           ((type (eql *declaration-tag-marker*)) (stream stream))
  (read-typed-markup-declaration (read-markup-tag-type stream) stream))

(defMethod read-typed-markup-declaration
           ((tag t) (stream t))
  (warn "unknown markup declaration form : ~s ~{~s~^ ~}"
        tag (read-markup-tag-parameters tag stream)))


(defMethod read-typed-section-element
           ((type (eql 'XML::CDATA)) (stream t))
  (read-process-cdata stream type))

(defMethod read-typed-section-element
           ((type (eql 'XML::INCLUDE)) (stream t))
  (let ((data (read-cdata stream type)))
    (typecase data
      (xml-text (setf data (xml-node.content data)))
      (string t)
      (t (return-from read-typed-section-element (values))))
    (first (last (read-markup-stream data)))))

(defMethod read-typed-section-element
           ((type (eql 'XML::IGNORE)) (stream t))
  (read-cdata stream type)
  (values))


;;;
;;; markup dispatch takes a postfix argument character which specifies
;;; how to interpret the subsequent data - and thus which reader to use.
;;; the readers are bound to properties on the function name:
;;; eg. #\! for dtd stuff
;;;     #\? for patterns
;;;     #\m for models

(defun markup-dispatching-reader-macro
       (stream char arg
        &aux (*package* (markup-package stream)) (element-spec nil))
  "returns the first element read in the stream."
  (declare (ignore arg))
  (cond ((setf element-spec (get 'markup-dispatching-reader-macro
                                     (setf char (peek-char t stream))))
         (when (rest element-spec)
           (read-char stream))
         (funcall (first element-spec) stream))
        (t
         (error "unknown markup macro character: ~s." char))))


(defun set-markup-dispatch-macro (character function &optional (consume? t))
  (setf function (cons function consume?))
  (when (alpha-char-p character)
    (setf (get 'markup-dispatching-reader-macro
               (if (lower-case-p character)
                 (char-upcase character) (char-downcase character)))
        function))
  (setf (get 'markup-dispatching-reader-macro character)
        function)
  function)

(set-markup-dispatch-macro #\"
                           #'(lambda (stream)
                               (first (read-markup-stream (read stream t t t))))
                           nil)
(set-markup-dispatch-macro #\< 'read-markup-element nil)


;;;
;;; a close tag generates only the symbol which names the tag
;;; the function context which recursively invoked the uses the token as
;;; the indicator that the respective element is complete.
;;; #!<asdf>aa</asd> #!<asdf>aa</>

(defMethod read-typed-markup-element
           ((tag (eql *close-tag-marker*)) (stream t)
            &aux tag-name form)
  (cond ((eql *tag-close-char* (peek-char t stream t t t))
         (read-char stream)
         *close-tag-marker*)
        (t
         (setf tag-name (read-markup-tag-type stream))
         (setf form (read-delimited-list *tag-close-char* stream t))
         (unless (null form)
           (xml-form-error tag-name "illegal close tag: ~s." form))
         tag-name)))

(defMethod process-markup-element
           ((element symbol) (context t))
  ;; these are usually end tags
  element)

;;; the default side-effect functions only distinguish between a return value
;;; and none. the (values) form is intended to inform the reader to ignore
;;; the element.

(defMethod process-markup-element
           ((element t) (context t))
  element)

(defMethod process-markup-element
           ((node string) (stream t))
  (xml-node.append-element *parent-node* node))

(defMethod process-markup-element
           ((elements cons) (context t) &aux result)
  (dolist (element elements)
    (setf result (process-markup-element element context)))
  result)

(defMethod process-markup-element
           ((element null) (context t))
  (values))


#|
 the basic data reader functions. there are two: one each for pcdata and cdata.

 one must keep in mind that these functions read as much for side-effect as for
 results, as the 'xml processor' runs a read-process loop analogous to the lisp
 run-time read-eval-print loop. the difference is that the process step is
 internal to the reader. as the relation hierarchy is not implicit in the
 lexical form, it is constructed explicitly, internal to the reader, during the
 processing step. this allows the relation to depend on the element class.
 |#

(defun make-text-element-optionally
       (string &optional tag stream (node 'text-element)
               &aux (text-class (node-class node tag stream)))
  (cond ((eq text-class 'string) string)
        ((null text-class) nil)
        (t (make-instance text-class :content string))))

(defun read-pcdata
       (stream
        &optional tag-name (eof-p t) (eof-value nil)
        &aux
        (*readtable* *markup-pcdata-readtable*)
        element
        (nc (peek-char (not *xml-preserve-whitespace*)
                       stream eof-p eof-value t)))
  "read 'parsed character data'. generate text element or leave it to the
   reader to read entity references or tagged elements. text is optionally
   left as a literal or wrapped in an element. invoke the process step for
   elements here."
  (setf element
        (cond ((eq nc eof-value) eof-value)
              ((or (char= nc *entity-reference-open-char*)
                   (char= nc *tag-open-char*))
               (read stream eof-p eof-value t))
              (t
               (setf element
                     (read-delimited-string #'(lambda (c)
                                                (or (char= c *entity-reference-open-char*)
                                                    (char= c *tag-open-char*)))
                                            stream))
               (unless *xml-preserve-whitespace*
                 (setf element
                       (string-right-trim #(#\space #\newline #\tab
                                            #\return #\linefeed)
                                          element)))
               (if (zerop (length element))
                 (read-pcdata tag-name eof-p eof-value)
                 (make-text-element-optionally element tag-name stream
                                               'parsed-character-data)))))
  element)

(defun read-process-pcdata
       (stream &optional tag-name (eof-p t) (eof-value nil) &aux element)
  (setf element (read-pcdata stream tag-name eof-p eof-value))
  (if *xml-markup-ignore*
    (values)
    (setf *processed-node* (process-markup-element element stream))))

(defun read-cdata
       (stream &optional tag-name
        &aux
        element
        (close-count 0))
  (setf element (read-delimited-string
                 #'(lambda (char)
                     (case close-count
                       ((nil) (when (char= *cdata-close-char* char)
                                (setf close-count 1))
                        nil)
                       (1 (setf close-count
                                (if (char= *cdata-close-char* char)
                                  (1+ close-count) 0))
                        nil)
                       (2 
                        (if (char= *tag-close-char* char)
                         t
                         (setf close-count nil)))))
                 stream))
  (make-text-element-optionally element tag-name stream
                                'character-data)
  element)

(defun read-process-cdata
       (stream &optional tag-name &aux element)
  (setf element (read-cdata stream tag-name))
  (if *xml-markup-ignore*
    (values)
    (setf *processed-node* (process-markup-element element stream))))
        

(defun read-entity-data
       (stream
        &optional tag-name (eof-p t) (eof-value nil)
        &aux
        (*readtable* *markup-entity-readtable*)
        element
        (nc (peek-char t stream eof-p eof-value t)))
  "read 'entity data'. generate text element or leave it to the
   reader to read entity references or tagged elements. text is optionally
   left as a literal or wrapped in an element. this is to be consistent with
   pcdata, but should in any event be a form error, since we're reading entities
   here. invoke the process step for elements here."
  (setf element
        (cond ((eq nc eof-value) eof-value)
              ((or (char= nc *entity-reference-open-char*)
                   (char= nc *p-entity-reference-open-char*)
                   (char= nc *tag-open-char*))
               (read stream eof-p eof-value t))
              (t
               (setf element
                     (read-delimited-string #'(lambda (c)
                                                (or (char= c *entity-reference-open-char*)
                                                    (char= c *p-entity-reference-open-char*)
                                                    (char= c *tag-open-char*)))
                                            stream))
               ;; don't bother to trim whirespace, since the free text is
               ;; a form error in any case.
               (if (zerop (length element))
                 (read-entity-data tag-name eof-p eof-value)
                 (make-text-element-optionally element tag-name stream
                                               'parsed-character-data)))))
  element)

(defun read-process-entity-data
       (stream &optional tag-name (eof-p t) (eof-value nil) &aux element)
  (setf element (read-entity-data stream tag-name eof-p eof-value))
  (if *xml-markup-ignore*
    (values)
    (setf *processed-node* (process-markup-element element stream))))

;;;
;;; these are provisional:
;;; the relation to cl-http url's is, for now, unclear

(defMethod url-namestring
           ((url string) (context t))
  (if (find-if #'lower-case-p url)
    (string-upcase url)
    url))

(defMethod url-namestring
           ((url pathname) (context t))
  (when (and (not (zerop (length (namestring url))))
             (setf url (probe-file url)))
    (url-namestring (namestring url) context)))

(defMethod url-namestring
           ((url file-stream) (context t))
  (ignore-errors (url-namestring (pathname url) context)))

(defMethod url-namestring
           ((stream concatenated-stream) (context t))
  (url-namestring (first (concatenated-stream-streams stream)) context))

(defMethod url-namestring
           ((stream string-stream) (context t))
  (format nil "internal://DTD/~x"
          #+:CCL (sxhash (slot-value stream 'ccl::my-string))
          #-:CCL (gentemp "STRING-")))

(defMethod url-namestring
           ((url null) (context t))
  nil)

(defMethod url-namestring
           ((url t) (context t))
  nil)

(defMethod url-namestring.name
           ((url-namestring string) &aux start end)
  (setf start (position-if #'(lambda (c) (find c "\\/:")) url-namestring
                           :from-end t)
        end (position #\. url-namestring :from-end t))
  (subseq url-namestring (if start (1+ start) 0)
          (if end end (length url-namestring))))

;; these are here until we decide how much of the cl-http url implementation
;; to adopt
(defMethod url
           ((url string))
  (pathname (if (find-if #'lower-case-p url)
              (string-upcase url)
              url)))

(defMethod url
           ((url pathname))
  (url (namestring url)))

(defMethod url
           ((url stream))
  (url (or (ignore-errors (pathname url)) #p"")))

(defMethod url
           ((url t))
  #p"")

(defMethod url
           ((url null))
  nil)

;;; external data management
;;; for now until i decide how to link to cl-http

(defMethod get-external-entity-content
           ((location pathname) &aux length buffer)
  (when (setf location (probe-file (merge-pathnames location
                                                    *xml-pathname-defaults*)))
    (with-open-file (stream location :direction :input
                            :element-type *xml-encoding-element-type*)
      (setf length (file-length stream))
      (setf buffer (make-string length
                                :element-type  *xml-encoding-element-type*))
      (dotimes (i length)
        (setf (char buffer i) (read-char stream))))
    buffer))

(defMethod get-external-entity-content
           ((location string))
  (get-external-entity-content (pathname location)))

;;;
;;; readtable specifications

;;;
;;; instantiate a readtable with syntax and dispatch functions appropriate for
;;; the syntax of markup streams

(eval-when (:execute :load-toplevel :compile-toplevel)
  (flet ((markup-char-name-reader (name)
           #'(lambda (stream char)
               (declare (ignore stream char))
               name)))
    (setf *start-tag-open* "<")
    (setf *end-tag-open* "</")
    (setf *tag-close* ">")
    (setf *tag-open-char* #\<)
    (setf *tag-close-char* #\>)
    (setf *entity-reference-open-char* #\&)
    (setf *p-entity-reference-open-char* #\%)

    ;; the pcdata readtable is the base. it is set to 'react' to tag markers
    ;; and entity markers only. tag close markers are set to issue a warning.
    (setf *markup-pcdata-readtable* (copy-readtable nil))
    (set-syntax-from-char #\" #\a *markup-pcdata-readtable*)
    (set-syntax-from-char #\# #\a *markup-pcdata-readtable*)
    (set-macro-character #\& 'entity-reader-macro
                         nil *markup-pcdata-readtable*)
    (set-syntax-from-char #\' #\a *markup-pcdata-readtable*)
    (set-syntax-from-char #\, #\a *markup-pcdata-readtable*)
    (set-syntax-from-char #\; #\a *markup-pcdata-readtable*)
    (set-macro-character #\< 'element-reader-macro
                         nil *markup-pcdata-readtable*)
    ; set the syntax for tag close to issue a warning and skip over it
    (set-syntax-from-char #\> #\) *markup-pcdata-readtable*)
    (set-macro-character #\> (markup-char-name-reader *tag-end-marker*)
                         nil *markup-pcdata-readtable*)
    (set-syntax-from-char #\\ #\a *markup-pcdata-readtable*)
    (set-syntax-from-char #\` #\a *markup-pcdata-readtable*)
    (set-syntax-from-char #\| #\a *markup-pcdata-readtable*)
    (setf (readtable-case *markup-pcdata-readtable*) :preserve)

    (setf *markup-model-readtable* (copy-readtable *markup-pcdata-readtable*))
    (set-macro-character #\| (markup-char-name-reader *or-marker*)
                         nil *markup-model-readtable*)
    (set-macro-character #\& (markup-char-name-reader *and-marker*)
                         nil *markup-model-readtable*)
    (set-macro-character #\, (markup-char-name-reader *seq-marker*)
                         nil *markup-model-readtable*)
    (set-macro-character #\* (markup-char-name-reader *rep-marker*)
                         nil *markup-model-readtable*)
    (set-macro-character #\? (markup-char-name-reader *opt-marker*)
                         nil *markup-model-readtable*)
    (set-macro-character #\+ (markup-char-name-reader *plus-marker*)
                         nil *markup-model-readtable*)
    (set-macro-character #\% 'parsed-parameter-entity-reader-macro
                         nil *markup-model-readtable*)
    (set-macro-character #\# 'restricted-name-reader-macro
                         t *markup-model-readtable*)

    ;; the attribute readtable derives from the pcdata readtable, but
    ;; prior to eliminating the special handling for '(' and ')'
    ;; it includes the macro for '|' to support enumerated attribute types
    (setf *markup-attribute-readtable* (copy-readtable *markup-pcdata-readtable*))
    (set-macro-character #\" (get-macro-character #\" *readtable*)
                         nil *markup-attribute-readtable*)
    (set-macro-character #\' (get-macro-character #\" *readtable*)
                         nil *markup-attribute-readtable*)
    (set-syntax-from-char #\= #\space *markup-attribute-readtable*)
    (set-macro-character #\/ (markup-char-name-reader *close-tag-marker*)
                         nil *markup-attribute-readtable*)
    (set-macro-character #\[ (markup-char-name-reader *open-tag-section-marker*)
                         nil *markup-attribute-readtable*)
    (set-macro-character #\] (markup-char-name-reader *close-tag-section-marker*)
                         nil *markup-attribute-readtable*)
    (set-macro-character #\? (markup-char-name-reader *processing-instruction-tag-marker*)
                         nil *markup-attribute-readtable*)
    (set-macro-character #\% 'parsed-parameter-entity-reader-macro
                         nil *markup-attribute-readtable*)
    (set-macro-character #\# 'restricted-name-reader-macro
                         t *markup-attribute-readtable*)
    (set-macro-character #\| (markup-char-name-reader *or-marker*)
                         nil *markup-attribute-readtable*)
    


    ;; the type readtable derives from the pcdata readtable with the additions
    ;; of syntax for declaration markers, pi markers, and parameter entity
    ;; markers. the '!' macro is unique since it must recognize sections
    (setf *markup-type-readtable* (copy-readtable *markup-pcdata-readtable*))
    (setf (readtable-case *markup-type-readtable*) :upcase)
    (set-macro-character #\! #'(lambda (stream char)
                                 (declare (ignore char))
                                 (if (char= #\[ (peek-char nil stream))
                                   (progn (read-char stream)
                                          *section-tag-marker*)
                                   *declaration-tag-marker*))
                         nil *markup-type-readtable*)
    (set-macro-character #\/ (markup-char-name-reader *close-tag-marker*)
                         nil *markup-type-readtable*)
    (set-macro-character #\? (markup-char-name-reader *processing-instruction-tag-marker*)
                         nil *markup-type-readtable*)
    (set-macro-character #\% 'parsed-parameter-entity-reader-macro
                         nil *markup-type-readtable*)
    (set-macro-character #\# 'restricted-name-reader-macro
                         t *markup-type-readtable*)

    (set-syntax-from-char #\( #\a *markup-pcdata-readtable*)
    (set-syntax-from-char #\) #\a *markup-pcdata-readtable*)

    (setf *markup-entity-readtable* (copy-readtable *markup-pcdata-readtable*))
    (set-macro-character #\" (get-macro-character #\" *readtable*)
                         nil *markup-entity-readtable*)
    (set-macro-character #\' (get-macro-character #\" *readtable*)
                         nil *markup-entity-readtable*)
    (set-macro-character #\% 'parsed-parameter-entity-reader-macro
                         nil *markup-entity-readtable*)
    

    (set-dispatch-macro-character #\# #\!
                                  'markup-dispatching-reader-macro
                                  *readtable*)
    ))

:EOF



