;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: www-present -*-

;;; (C) Copyright 1996, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;
;;; Christopher R. Vincent
;;; cvince@ai.mit.edu
;;;
;;;------------------------------------------------------------------- 
;;;
;;; BASIC PRESENTATION SYSTEM FOR THE WORLD-WIDE WEB
;;;

#|

standard-methods.lisp defines some standard presentation methods. applications that
define new presentation methods should contain a file like this.

|#

(in-package :www-present)

;;;------------------------------------------------------------------- 
;;;
;;; DEFAULT PRESENTATION METHODS
;;;

;; Not very useful because this by-passes the condition system and
;; prevents handling conditions. Furtermore, it requires duplication of
;; the presentation lattice for applications that do want to signal
;; conditions. Cut over to signalling 3/28/97 -- JCMa.
#+ignore
(define-default-presentation-method handle-input-error (object type stream view &key)
  "catch unhandled calls to handle-input-error, return nil for both values"
 (declare (ignore object stream view))
 (values nil nil))

(define-default-presentation-method handle-input-error (object type stream view &key)
  "catch unhandled calls to handle-input-error, signal a condition."
  (declare (ignore stream view))
  (input-not-of-required-type object type))

(define-default-presentation-method presentation-typep (object type)
  (with-presentation-type-decoded (type-name) type
    (typep object type-name)))

(define-default-presentation-method accept (type stream view &key)
  (declare (ignore view))
  (let ((object (read stream nil nil)))
    (cond ((presentation-typep object type) object)
          (t (handle-input-error object type)))))

(define-default-presentation-method present (object type stream view &key)
  (declare (ignore view))
  (write object :stream stream)
  object)

(define-default-presentation-method accept-present-default (type stream view default default-supplied-p 
                                                                 present-p query-identifier &key)
  (declare (ignore present-p query-identifier))
  (cond (default-supplied-p (present default type :stream stream :view view))
        (t (present type 'presentation-type-specifier :stream stream :view view))))

(define-default-presentation-method presentation-subtypep (type putative-supertype &key)
  "highly specific, default method ignores options and parameters"
  (with-presentation-type-decoded (supertype-name) putative-supertype
    (with-presentation-type-decoded (type-name) type
      (cond ((presentation-type-superior-p type-name supertype-name) t)
            (t nil)))))

(define-default-presentation-method presentation-type-specifier-p (type &key)
  t)

(define-default-presentation-method describe-presentation-type (type stream plural-count &key)
  (declare (ignore plural-count))
  (with-presentation-type-decoded (type-name) type
    (let ((some-vowels '(#\a #\e #\i #\o))
          (name-string (symbol-name type-name)))
      (cond ((member (char name-string 0) some-vowels :test #'char-equal)
             (write-string "an " stream))
            (t (write-string "a " stream)))
      (write-string (string-downcase name-string) stream))
    type))

;;;------------------------------------------------------------------- 
;;;
;;; BOOLEAN
;;;

(define-presentation-method presentation-typep (value (type boolean))
  (declare (ignore value)) 
  t)

(define-presentation-method accept ((type boolean) stream (view textual-view) &key)
  (let ((value (read-token stream)))
    (cond ((string-equal value "NO") (setq value nil))
          ((string-equal value "NIL") (setq value nil))
          (t (setq value t)))
    (unless (presentation-typep value type)
      (handle-input-error value type))
    value))

(define-presentation-method present (value (type boolean) stream (view textual-view) &key)
  (cond (value (write-string "Yes" stream))
        (t (write-string "No" stream)))
  value)

;;;------------------------------------------------------------------- 
;;;
;;; SYMBOL
;;;

(define-presentation-method accept ((type symbol) stream (view textual-view) &key)
  (let* ((string (read-token stream))
         (end (length string))
         (colon-pos (position #\: string :end end :from-end t))
         (package (find-package 
                    (cond ((and colon-pos (char-equal #\: (char string (the fixnum (1- colon-pos)))))
                           (subseq string 0 (the fixnum (1- colon-pos))))
                          (colon-pos 
                           (subseq string 0 colon-pos))
                          (t nil))))
         (symbol-name (subseq string (the fixnum (1+ (or colon-pos -1))) end)))
    (declare (dynamic-extent end colon-pos package))
    (multiple-value-bind (symbol returned)
        (cond (package (intern symbol-name package))
              (t (intern symbol-name)))
      (declare (ignore returned))
      symbol)))

(define-presentation-method present (symbol (type symbol) stream (view textual-view) &key acceptably)
  "force package info to be printed when :acceptably is non-null
this is still an issue, but this should work for most cases."
  (cond (acceptably
         (multiple-value-bind (found status) 
             (find-symbol (symbol-name symbol))
           (declare (ignore found))
           (write-string (package-name (symbol-package symbol)) stream)
           (case status
             (:external (write-char #\: stream))
             (t (write-string "::" stream)))
           (write-string  (symbol-name symbol) stream)))
        (t (write-string (symbol-name symbol) stream)))
  symbol)

;;;------------------------------------------------------------------- 
;;;
;;; NULL
;;;

(define-presentation-method presentation-typep (object (type null))
  (not object))

(define-presentation-method accept ((type null) stream (view textual-view) &key)
  (let* ((value (read-token stream))
         (end (length value)))
    (when (or (string-equal value "NONE" :start1 0 :end1 end :start2 0 :end2 4)
              (string-equal value "NIL" :start1 0 :end1 end :start2 0 :end2 3))
      (setq value nil))
    (unless (presentation-typep value type)
      (handle-input-error value type))
    value))

(define-presentation-method present (object (type null) stream (view textual-view) &key)
  (declare (ignore object))
  (write-string "None" stream)
  nil)

(define-presentation-method describe-presentation-type ((type null) stream plural-count &key)
  (declare (ignore plural-count))
  (write-string "None" stream))

;;;------------------------------------------------------------------- 
;;;
;;; KEYWORD
;;;

(define-presentation-method accept ((type keyword) stream (view textual-view) &key)
  (let* ((string (read-token stream))
         (end (length string))
         (colon-pos (position #\: string :end end :from-end t))
         (symbol (cond (colon-pos 
                        (intern (subseq string (the fixnum (1+ colon-pos)) end) :keyword))
                       (t (intern string :keyword)))))
    (declare (dynamic-extent end colon-pos))
    (unless symbol 
      (handle-input-error string type))
    symbol))

(define-presentation-method present (keyword (type keyword) stream (view textual-view) &key)
  (write keyword :stream stream)
  keyword)

;;;------------------------------------------------------------------- 
;;;
;;; COMPLEX
;;;

(define-presentation-method presentation-typep (number (type complex))
  (or (null type)
      (and (presentation-typep (realpart number) type)
           (presentation-typep (imagpart number) type))))

;;;------------------------------------------------------------------- 
;;;
;;; REAL
;;;

(define-presentation-method presentation-typep (number (type real))
  (with-presentation-type-decoded (type-name) type
    (and (typep number type-name)
         (or (null low) 
             (>= number low))
         (or (null high) 
             (<= number high)))))

(define-presentation-method present (number (type real) stream (view textual-view) &key)
  (write number :base base :radix radix :stream stream)
  number)

(define-presentation-method describe-presentation-type :after ((type real) stream plural-count &key)
                            (declare (ignore plural-count))
                            (when (or low high)
                              (cond ((and low high) 
                                     (write-string " between " stream)
                                     (present low 'real :stream stream)
                                     (write-string " and " stream)
                                     (present high 'real :stream stream))
                                    (low
                                     (write-string " greater than " stream)
                                     (present low 'real :stream stream))
                                    (high
                                     (write-string " less than " stream)
                                     (present high 'real :stream stream)))))

;;;------------------------------------------------------------------- 
;;;
;;; INTEGER
;;;

(define-presentation-method presentation-typep (number (type integer))
  (and (typep number 'integer)
       (or (null low) 
           (>= number low))
       (or (null high) 
           (<= number high))))

;;;------------------------------------------------------------------- 
;;;
;;; FLOAT
;;;

(define-presentation-method presentation-typep (number (type float))
  (and (typep number 'float)
       (or (null low) 
           (>= number low))
       (or (null high) 
           (<= number high))))

;;;------------------------------------------------------------------- 
;;;
;;; CHARACTER
;;;

(define-presentation-method present (character (type character) stream (view textual-view) &key)
  (write-char character stream))

(define-presentation-method accept ((type character) stream (view textual-view) &key)
  (let ((char (read-char stream nil nil)))
    (unless (presentation-typep char type)
      (handle-input-error char type))
    char))

;;;------------------------------------------------------------------- 
;;;
;;; BASIC-STRING
;;;

(define-presentation-method presentation-typep (string (type basic-string))
  (typep string 'string))

(define-presentation-method accept ((type basic-string) stream (view textual-view) &key)
  (let ((string (read-line stream nil nil)))
    (unless (presentation-typep string type)
      (handle-input-error string type))
    string))

(define-presentation-method present (string (type basic-string) stream (view textual-view) &key)
  (write-string string stream)
  string)

;;;------------------------------------------------------------------- 
;;;
;;; STRING
;;;

(define-presentation-method presentation-typep (string (type string))
  (and (typep string 'string)
       (cond (length (eql length (length string)))
             (t t))))

;;;------------------------------------------------------------------- 
;;;
;;; BOUNDED-STRING
;;;

(define-presentation-method presentation-typep (string (type bounded-string))
  (and (typep string 'string)
       (cond (max-length (<= (length string) max-length))
             (t t))))

(define-presentation-method describe-presentation-type ((type bounded-string) stream plural-count &key)
  (declare (ignore plural-count))
  (cond (max-length 
         (write-string "a string with maximum length " stream)
         (present max-length 'integer :stream stream))
        (t (write-string "a string" stream))))

;;;------------------------------------------------------------------- 
;;;
;;; PATHNAME
;;;

(define-presentation-method present (pathname (type pathname) stream (view textual-view) &key)
  (when pathname(write-string (namestring pathname) stream))
  pathname)

(define-presentation-method accept ((type pathname) stream (view textual-view) &key 
                                    (default *default-pathname-defaults*))
  (let* ((string (read-line stream nil nil))
         (pathname (when string (pathname string))))
    (when (and pathname default  merge-default)
      (setq pathname 
            (merge-pathnames pathname
                             (make-pathname :host (pathname-host default)
                                            :device (pathname-device default)
                                            :directory (pathname-directory default)
                                            :name (pathname-name default)
                                            :type default-type
                                            :version default-version)
                             default-version)))
    (unless (presentation-typep pathname type)
      (handle-input-error pathname type))
    pathname))

;;;------------------------------------------------------------------- 
;;;
;;; EXISTING-PATHNAME
;;;

(define-presentation-method presentation-typep (pathname (type existing-pathname))
  (when pathname
    (probe-file pathname)))

(define-presentation-method describe-presentation-type ((type existing-pathname) stream plural-count &key)
  (declare (ignore plural-count))
  (write-string "a pathname corresponding to an existing file" stream))

;;;------------------------------------------------------------------- 
;;;
;;; COMPLETION
;;;

;; no actual completion right now.  one-of and some-of types can still be used, though.

(define-presentation-method present (object (type completion) stream (view textual-view) &key)
  (write-string (funcall name-key object) stream)
  object)

;;;------------------------------------------------------------------- 
;;;
;;; MEMBER
;;;

(define-presentation-method presentation-typep (value (type member))
  (member value elements :test #'eql))

;;;------------------------------------------------------------------- 
;;;
;;; MEMBER-SEQUENCE
;;;

(define-presentation-method presentation-typep (value (type member-sequence))
  (member value sequence :test test))

;;;------------------------------------------------------------------- 
;;;
;;; MEMBER-ALIST
;;;

(define-presentation-method presentation-typep (value (type member-alist))
  (member value alist :test test :key value-key))

;;;------------------------------------------------------------------- 
;;;
;;; SUBSET-COMPLETION
;;;

(define-presentation-method present (object (type subset-completion) stream (view textual-view) &key)
  (cond ((and object (atom object))
         (write-string (funcall name-key object) stream)
         object)
        (object (write-string (funcall name-key (car object)) stream)
                (loop for item in (cdr object)
                      do (when separator (write-char separator stream))
                      do (when echo-space (write-char #\Space stream))
                      do (write-string (funcall name-key item) stream))
                object)
        (t (present object 'null :stream stream :view +textual-view+))))

;;;------------------------------------------------------------------- 
;;;
;;; SUBSET
;;;

(define-presentation-method presentation-typep (object (type subset))
  (cond ((null object) t)
        ((atom object) (member object elements :test #'eql))
        ((listp object) (subsetp object elements :test #'eql))
        (t nil)))

;;;------------------------------------------------------------------- 
;;;
;;; SUBSET-SEQUENCE
;;;

(define-presentation-method presentation-typep (object (type subset-sequence))
  (cond ((null object) t)
        ((atom object) (member object sequence :test test))
        ((listp object) (subsetp object sequence :test test))
        (t nil)))

;;;------------------------------------------------------------------- 
;;;
;;; SUBSET-ALIST
;;;

(define-presentation-method presentation-typep (object (type subset-alist))
  (cond ((null object) t)
        ((atom object) (member object alist :test test :key value-key))
        ((listp object) (subsetp object alist :test test :key value-key))
        (t nil)))

;;;------------------------------------------------------------------- 
;;;
;;; SEQUENCE
;;;

(define-presentation-method presentation-typep (sequence (type sequence))
  (and (typep sequence 'sequence)
       (every #'(lambda (item) (presentation-typep item type)) sequence)))

(define-presentation-method present (sequence (type sequence) stream (view textual-view) &key)
  (cond (sequence
         (let ((last (1- (length sequence)))
               (pos 0))
           (map nil
                #'(lambda (item)
                    (present item type :stream stream :view view)
                    (unless (= pos last)
                      (when separator (write-char separator stream))
                      (when echo-space (write-char #\Space stream)))
                    (incf pos))
                sequence)))
        (t (present sequence 'null :stream stream :view view)))
  sequence)

(define-presentation-method accept ((type sequence) stream (view textual-view) &key)
  (flet ((accept-element (stream)
           (loop for char = (peek-char nil stream nil nil t)
                 when (or (null char) (char-equal char separator) (member char '(#\tab #\Return #\Linefeed)))
                   do (return (accept-from-string type (coerce result 'string) :view view))
                 else collect char into result
                      and do (read-char stream nil nil t))))
    (declare (inline accept-element))
    (let ((sequence
            (loop for char = (peek-char nil stream nil nil t)
                  when (or (null char) (member char '(#\tab #\Return #\Linefeed)))
                    do (return result)
                  else when (char-equal char separator)
                         do (progn (read-char stream nil nil t)
                                   (when (and echo-space (char-equal (peek-char nil stream nil nil t)  #\Space))
                                     (read-char stream nil nil t)))
                  else 
                    collect (accept-element stream) into result)))
      (unless (presentation-typep sequence `(sequence ,type))
        (handle-input-error sequence `(sequence ,type)))
      sequence)))

(define-presentation-method describe-presentation-type :after ((type sequence) stream plural-count &key)
                            (declare (ignore plural-count))
                            (when type
                              (write-string " of which each element is " stream)
                              (describe-presentation-type type stream)))

;;;------------------------------------------------------------------- 
;;;
;;; SEQUENCE-ENUMERATED
;;;

(define-presentation-method presentation-typep (sequence (type sequence-enumerated))
  (when (eql (length sequence) (length types))
    (loop for item in sequence
          for item-type in types
          unless (presentation-typep item item-type)
            do (return nil)
          finally (return t))))

(define-presentation-method present (sequence (type sequence-enumerated) stream (view textual-view) &key)
  (cond (sequence
         (present (car sequence) (car types) :stream stream :view view)
         (loop for item in (cdr sequence)
               for item-type in (cdr types)
               do (when separator (write-char separator stream))
               do (when echo-space (write-char #\Space stream))
               do (present item item-type :stream stream :view view)))
        (t (present sequence 'null :stream stream sequence view)))
  sequence)

(define-presentation-method accept ((type sequence-enumerated) stream (view textual-view) &key)
  (flet ((accept-element (stream type)
           (loop for char = (peek-char nil stream nil nil t)
                 when (or (null char) (char-equal char separator) (member char '(#\tab #\Return #\Linefeed)))
                   do (return (accept-from-string type (coerce result 'string) :view view))
                 else collect char into result
                      and do (read-char stream nil nil t))))
    (declare (inline accept-element))
    (let* ((types-list (copy-list types))
           (sequence
             (loop for char = (peek-char nil stream nil nil t)
                   when (or (null char) (member char '(#\tab #\Return #\Linefeed)))
                     do (return result)
                   else when (char-equal char separator)
                          do (progn (read-char stream nil nil t)
                                    (when (and echo-space (char-equal (peek-char nil stream nil nil t)  #\Space))
                                      (read-char stream nil nil t)))
                   else 
                     collect (accept-element stream (pop types-list)) into result)))
      (unless (presentation-typep sequence type)
        (handle-input-error sequence type))
      sequence)))

;;;------------------------------------------------------------------- 
;;;
;;; MIXED-SEQUENCE
;;;

(define-presentation-method presentation-typep (sequence (type mixed-sequence))
  (consp sequence))

(define-presentation-method present (sequence (type mixed-sequence) stream (view textual-view) &key)
  "sequence may contain any type of elements, good for presenting a nested list of misc objects."
  (flet ((present-item (item)
           (typecase item
             (cons (present item 'mixed-sequence :stream stream :view view))
             (t (present item (presentation-type-of item) :stream stream :view view)))))
    (declare (inline present-item))
    (cond (sequence
           (present-item (car sequence))
           (loop for item in (cdr sequence)
                 do (when separator (write-char separator stream))
                 do (when echo-space (write-char #\Space stream))
                 do (present-item item)))
          (t (present sequence 'w3p:null :stream stream :view view)))
    sequence))

;;;------------------------------------------------------------------- 
;;;
;;; OR
;;;

(define-presentation-method presentation-typep (object (type or))
  (some #'(lambda (type) (presentation-typep object type)) types))

(define-presentation-method accept ((type or) stream (view textual-view) &key default default-type)
  "since this isn't interactive, need to store input.  catch parse errrors"
  (let ((string (read-line stream nil nil t)))
    (block loop-attemps
      (loop for type in types
            do (block try-type
                 (handler-bind ((presentation-parse-error
                                  #'(lambda (condition)
                                      (declare (ignore condition))
                                      (return-from try-type (values nil nil)))))
                   (with-input-from-string (string-stream string)
                     (with-presentation-type-decoded (type-name parameters options) type
                       (multiple-value-bind 
                         (returned-object returned-type)
                           (funcall-presentation-generic-function accept type-name parameters options 
                                                                  type string-stream view 
                                                                  :default default :default-type default-type)
                         (return (values returned-object (or returned-type type))))))))
            finally (handle-input-error string type)))))

(define-presentation-method present (object (type or) stream (view textual-view) &key)
  (loop for type in types 
        when (presentation-typep object type) do (return (present object type :stream stream :view view))
        finally (present object (presentation-type-of object) :stream stream :view view)))

;;;------------------------------------------------------------------- 
;;;
;;; AND
;;;

(define-presentation-method presentation-typep (object (type and))
  (labels ((verify-type (type)
             (cond ((atom type) (presentation-typep object type))
                   ((eql 'satisfies (first type)) (funcall (second type) object))
                   ((eql 'not (first type)) (not (verify-type (second type))))
                   (t (presentation-typep object type)))))
    (and (presentation-typep object (first types))
         (loop for type in (cdr types)
               unless (verify-type type) return nil
               finally (return t)))))

(define-presentation-method accept ((type and) stream (view textual-view) &key)
  (let ((value (accept (first types) :stream stream :view view)))
    (unless (presentation-typep value type)
      (handle-input-error value type))))

(define-presentation-method present (object (type and) stream (view textual-view) &key)
  (present object (first types) :stream stream :view view)
  (values object type))

;;;------------------------------------------------------------------- 
;;;
;;; TOKEN-OR-TYPE
;;;

(define-presentation-method presentation-typep (object (type token-or-type))
  (or (presentation-typep object type)
      (when (or (symbolp object) (stringp object))
        (let ((token-string (etypecase object
                              (symbol (symbol-name object))
                              (string object))))
          (member token-string tokens :test #'string-equal :key #'(lambda (item) 
                                                                    (etypecase item 
                                                                      (symbol (symbol-name item)) 
                                                                      (string item))))))))
(define-presentation-method accept ((type token-or-type) stream (view textual-view) &key)
  (let ((value-string (read-token stream)))
    (cond ((presentation-typep value-string `(token-or-type ,tokens ,type))
           value-string)
          (t (multiple-value-bind (returned-object returned-type)
                 (accept-from-string type value-string :view view)
               (unless returned-type
                 (handle-input-error value-string type))
               returned-object)))))

(define-presentation-method present (object (type token-or-type) stream (view textual-view) &key)
  (cond ((presentation-typep object type) (present object type :stream stream :view view))
        ((symbolp object) (write-string (string-upcase (symbol-name object)) stream))
        ((stringp object) (write-string (string-upcase object) stream))))

;;;------------------------------------------------------------------- 
;;;
;;; NULL-OR-TYPE
;;;

(define-presentation-method presentation-typep (object (type null-or-type))
  (or (presentation-typep object type)
      (presentation-typep object 'null)))

(define-presentation-method accept ((type null-or-type) stream (view textual-view) &key)
  (let ((string (read-line stream nil nil t)))
    (cond ((string-equal (read-token-from-string string) "NONE") (values nil 'NULL))
          (t (multiple-value-bind 
               (returned-object returned-type)
                 (block try-type
                   (handler-bind ((presentation-parse-error
                                    #'(lambda (condition)
                                        (declare (ignore condition))
                                        (return-from try-type (values nil nil)))))
                     (with-input-from-string (string-stream string)
                       (with-presentation-type-decoded (type-name parameters options) type
                         (multiple-value-bind 
                           (returned-object returned-type)
                             (funcall-presentation-generic-function accept type-name parameters options 
                                                                    type string-stream view)
                           (values returned-object (or returned-type type)))))))
               (cond (returned-type (values returned-object returned-type))
                     (t (handle-input-error string type))))))))

(define-presentation-method present (object (type null-or-type) stream (view textual-view) &key)
  (cond ((null object) (present object 'null :stream stream :view view))
        (t (present object type :stream stream :view view))))

(define-presentation-method describe-presentation-type ((type null-or-type) stream plural-count &key)
  (declare (ignore plural-count))
  (write-string "either " stream)
  (describe-presentation-type type stream)
  (write-string ", or " stream)
  (describe-presentation-type 'null stream))

;;;------------------------------------------------------------------- 
;;;
;;; TYPE-OR-STRING
;;;

(define-presentation-method presentation-typep (object (type type-or-string))
  (or (presentation-typep object type)
      (presentation-typep object 'string)))

;;;------------------------------------------------------------------- 
;;;
;;; UP
;;;

(define-presentation-method accept ((type type-or-string) stream (view textual-view) &key)
  (let ((string (read-line stream nil nil t)))
    (multiple-value-bind 
      (returned-object returned-type)
        (block try-type
          (handler-bind ((presentation-parse-error
                           #'(lambda (condition)
                               (declare (ignore condition))
                               (return-from try-type (values nil nil)))))
            (with-input-from-string (string-stream string)
              (with-presentation-type-decoded (type-name parameters options) type
                (multiple-value-bind 
                  (returned-object returned-type)
                    (funcall-presentation-generic-function accept type-name parameters options 
                                                           type string-stream view)
                  (values returned-object (or returned-type type)))))))
      (cond (returned-type (values returned-object returned-type))
            (t (values string 'type-or-string))))))

(define-presentation-method present (object (type type-or-string) stream (view textual-view) &key)
  (cond ((presentation-typep object type) (present object type :stream stream :view view))
        ((stringp object) (write-string object stream))
        (t (write object :stream stream))))

;;;------------------------------------------------------------------- 
;;;
;;; EXPRESSION
;;;

(define-presentation-method presentation-typep (object (type expression))
  (declare (ignore object))              
  t)                                  

(define-presentation-method present (expression (type expression) stream (view textual-view) &key acceptably)
  (cond (acceptably (prin1 expression stream))
        (t (princ expression stream)))
  expression)
