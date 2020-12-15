(in-package :LIS)

#|
 the format is
 ...
 Begin Data base <NAME>;
 Passwords:
    # <PASSWORD> ;
    ...
 Items:
 << # >> <NAME> <TYPE> <LENGTH>;
 ...

 Sets:
         << Set No: # >>
 Name: <NAME><TYPE><REFERENCES>
 Entry: <FIELD>(<KEY-ID>),
        ...
 Capacity: <CAPACITY><ENTRIES>
 End.

 |#

(defpackage "IMAGE-SCHEMA" (:use) (:nicknames :IS))

(defParameter *schema-readtable* (copy-readtable nil))
(set-macro-character #\< #'(lambda (stream char)
                             (declare (ignore char))
                             (read-delimited-list #\> stream t))
                     nil *schema-readtable*)
(set-syntax-from-char #\> #\) *schema-readtable*)
(set-syntax-from-char #\, #\space *schema-readtable*)
(set-syntax-from-char #\; #\space *schema-readtable*)
(set-macro-character #\: #'(lambda (stream char)
                             (declare (ignore stream char))
                             '|:|)
                     nil *schema-readtable*)

(defClass image-dtd (clos:dtd)
  ((passwords :initarg :passwords :accessor image-dtd.passwords :type #+:mcl (list symbol) #-:mcl list)))
   

(defClass image-dtd-element (clos:dtd-element)
  ())

(defun schema-section (list section &optional (start 0))
  (loop (unless (setf start (position section list :start start))
          (return nil))
        (when (eq (nth (1+ start) list) '|:|)
          (return start))
        (incf start)))
;(schema-section (top-inspect-form) 'is::passwords)

(defMethod translate-image-item-type
           ((type symbol) (length integer) &aux count count-length)
  (multiple-value-setq (count count-length) (parse-integer (string type) :junk-allowed t))
  (cond ((and (integerp count) (plusp count))
         `(:array ,(translate-image-item-type (intern (subseq (string type) count-length) :is)
                                              length)
                  ,count))
        (t
         (warn "illegal type specifier: (~a ~a)." type length)
         (translate-image-item-type 'is::x length))))

(defMethod translate-image-item-type
           ((type (eql 'is::x)) (length integer))
  `(:array :character ,length))

(defMethod translate-image-item-type
           ((type (eql 'is::z)) (length integer))
  `(:array :byte ,length))
(defMethod translate-image-item-type
           ((type (eql 'is::j)) (length (eql 1)))
  :word)
(defMethod translate-image-item-type
           ((type (eql 'is::j)) (length (eql 2)))
  :long)
(defMethod translate-image-item-type
           ((type (eql 'is::k)) (length (eql 1)))
  :word)
(defMethod translate-image-item-type
           ((type (eql 'is::k)) (length (eql 2)))
  :long)

(defMethod parse-image-schema
           ((stream stream)
            &aux (*readtable* *schema-readtable*)
                 (*package* (find-package :IS))
                 data elt dtd)
  (loop (unless (setf elt (read stream nil nil))
          (return))
        (push elt data))
  (when (typep (setf dtd (parse-image-schema (nreverse data))) 'clos:dtd)
    (setf (dtd.root dtd) nil)
    (setf (dtd.file dtd) (pathname stream)))
  dtd)

(defMethod parse-image-schema
           ((data list)
            &aux position
                 (base (search '(is::begin is::data is::base) data))
                 (passwords (schema-section data 'is::passwords 0))
                 (items (schema-section data 'is::items (if passwords (1+ passwords) 0)))
                 (sets (schema-section data 'is::sets (if items (1+ items) 0)))
                 (end (position 'is::end. data))
                 (item-elements nil)
                 (set-elements nil))
  (when (and base passwords items sets end)
    (setf base (nth (+ base 3) data))
    (setf passwords (subseq data (+ 2 passwords) items)
          items (subseq data (+ 2 items) sets)
          sets (subseq data (+ 2 sets) end))
    
    (setf position 0)
    (setf passwords (remove nil (mapcar #'(lambda (password)
                                            (incf position)
                                            (when (evenp position)
                                              password))
                                        passwords)))

    (let ((positions nil))
      (setf position 0)
      (dolist (e items)
        (when (consp e)
          (push position positions))
        (incf position))
      (setf positions (nreverse positions))
      (setf items (mapcar #'(lambda (start &optional next &aux entry)
                             (setf entry (if next
                                         (subseq items (+ start 1) next)
                                         (subseq items (+ start 1))))
                             entry)
                          positions (append (rest positions) (list nil)))))

    (let ((positions nil))
      (setf position 0)
      (dolist (e sets)
        (when (and (consp e) (consp (car e)) (eq (caar e) 'is::set))
          (push position positions))
        (incf position))
      (setf positions (nreverse positions))
      (setf sets (mapcar #'(lambda (start &optional next &aux entry items capacity set-no)
                             (setf entry (if next
                                           (subseq sets (+ start 3) next)
                                           (subseq sets (+ start 3))))
                             (setf set-no (fourth (first (nth start sets))))
                             (setf items (remove-if #'consp (subseq entry 5)))
                             (when (setf capacity (schema-section items 'is::capacity))
                               (setf items (subseq items 0 capacity)))
                             (list (subseq entry 0 3) items set-no))
                         positions (append (rest positions) (list nil)))))

    (setf item-elements
          (mapcar #'(lambda (item)
                      (apply #'(lambda (name type length)
                                 (make-instance (node-class 'element-decl name nil)
                                   :name name
                                   :qualified-name (intern (concatenate 'string
                                                                        (string base)
                                                                        "."
                                                                        (string name))
                                                           :is)
                                   :model clos::*model.pcdata*
                                   :attdefs (list (make-instance 'dtd-attdef
                                                    :name :type
                                                    :qualified-name (intern (concatenate 'string
                                                                                         (string name)
                                                                                         ".TYPE")
                                                                            :IS)
                                                    :default (translate-image-item-type type length)))))
                             item))
                  items))
    (setf item-elements (sort item-elements #'string-lessp
                              :key #'(lambda (e) (string (dtd-element.name e)))))
    (setf set-elements
          (mapcar #'(lambda (set &aux (name (first (first set))) (items (second set))
                                 (set-no (third set)))
                      (make-instance (node-class 'element-decl name nil)
                        :name name
                        :qualified-name (intern (concatenate 'string
                                                             (string base)
                                                             "."
                                                             (string name))
                                                :is)
                        :model (make-instance 'clos:dtd-model-group
                                 :connector *seq-marker*
                                 :content
                                 (mapcar #'(lambda (item)
                                             (dtd-element (intern (concatenate 'string
                                                                               (string base)
                                                                               "."
                                                                               (string item))
                                                                  :is)))
                                         items))
                        :attdefs (list (make-instance 'dtd-attdef
                                         :name :set-no
                                         :qualified-name (intern (concatenate 'string
                                                                              (string name)
                                                                              ".SET-NO")
                                                                 :IS)
                                         :default set-no))))
               sets))
    (setf set-elements (sort set-elements #'string-lessp
                             :key #'(lambda (e) (string (dtd-element.name e)))))
    (when *load-verbose*
      (inspect item-elements)
      (inspect set-elements))
    (make-instance 'image-dtd
      :name base
      :passwords passwords
      :elements (append set-elements item-elements))))

(defMethod parse-image-schema
           ((input pathname))
  (with-open-file (stream input :direction :input) (parse-image-schema stream)))

(defmethod translate-image-schema
           ((input pathname))
  (let ((dtd (parse-image-schema input)))
    (when (typep dtd 'clos:dtd)

      (with-open-file (output (make-pathname :type "DTD" :defaults (dtd.file dtd))
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (mapc #'(lambda (element &aux attributes)
                  (print element output)
                  (when (setf attributes (dtd-element.attdefs element))
                    (format output "~%<!ATTLIST ~A ~{~%          ~A~^~}>"
                            (dtd-element.name element)
                            attributes)))
              (dtd.elements dtd))))))
    

#|

(inspect (with-open-file (stream (choose-file-dialog) :direction :input)
           (parse-image-schema stream)))
(inspect (parse-image-schema (top-inspect-form)))
(translate-image-schema (choose-file-dialog))

(inspect (dtd-element.record-description 'is::dbase.labortests))
(inspect (clos:class-instances (find-class 'dtd-element)))

(defClass test (xml-record-node)
  ()
  (:record-definition (field1 :word)
                      (field2 :long)
                      (field3 (:string 7))
                      (field4 (:array :character 8))
                      (field5 (:array :byte 8)))
  (:metaclass xml-record-node-class))
 |#
