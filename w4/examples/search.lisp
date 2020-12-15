;;; -*- Mode: lisp; Syntax: Ansi-Common-Lisp; Base: 10; Package: w4 -*-

;;; (C) Copyright 1995, Andrew J. Blumberg.
;;;     All Rights Reserved.
;;;


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(in-package :w4)

(defun listify-string (string &optional (length (length string)))
  (flet ((white-space-p (ch)
           (member ch '(#\space #\Return #\Linefeed #\tab) :test #'char=)))
    (loop for start = (position-if-not #'white-space-p string :start (or end 0) :end length)
          while start
          for end = (or (position-if #'white-space-p string :start start :end length) length)
          collect (subseq string start end)
          until (= end  length))))


;;;------------------------------------------------------------------- 
;;;
;;; STRING ORIENTED TOKENIZER
;;;

#+slow
(defparameter *word-delimiters* '(#\Space #\! #\" #\$ #\& #\' #\( #\) #\* #\+ #\, #\- #\. #\/
                                  #\: #\; #\= #\? #\[ #\\ #\] #\_ #\` #\{ #\| #\}
                                  #\Linefeed #\Return))
#+slow
(defun word-delimiter-p (char)
  (member char *word-delimiters* :test #'char-equal))

(declaim (inline word-delimiter-p))

(defun word-delimiter-p (char)
  (let ((char-code (char-code char)))
    (or (eql char-code #.(char-code #\Return))
        (eql char-code #.(char-code #\Linefeed))
        (<= #.(char-code #\space) char-code 47)
        (member char-code '#.(mapcar #'char-code '(#\: #\; #\= #\? #\[ #\\ #\] #\_ #\` #\{ #\| #\})) :test #'eq))))

(defun text-plain-next-token (string pos &optional (length (length string)))
  (with-fast-array-references ((string string string))
    (loop with pointer = pos
          doing (loop for idx upfrom pointer below length
                      for char = (schar string idx)
                      until (word-delimiter-p char)
                      finally (cond ((< pointer idx)
                                     (return-from text-plain-next-token
                                       (values (subseq string pointer idx) (1+ idx))))
                                    ((= idx length)
                                     (return-from text-plain-next-token (values nil length)))
                                    (t (setq pointer (1+ idx))))))))

(defun text-html-next-token (string pos &optional (length (length string)))
  (with-fast-array-references ((string string string))
    (loop named top
          with pointer = pos
          doing (loop with skip-element-p = nil
                      for idx upfrom pointer below length
                      for char = (schar string idx)
                      until (and (not skip-element-p)
                                 (word-delimiter-p char))
                      unless (or skip-element-p
                                 (when (char-equal char #\<)
                                   (setq skip-element-p t)))
                        collect char into chars
                      else do ;; close on close or open
                             (when (member char '(#\>) :test #'char-equal)
                               (setq skip-element-p nil))
                      finally (cond (chars
                                     ;;(format t "~&text-html-next-token: ~S" (coerce chars 'string))
                                     (return-from text-html-next-token
                                       (values (coerce chars 'string) (1+ idx))))
                                    ((= idx length)
                                     (return-from text-html-next-token (values nil length)))
                                    (t (return (setq pointer (1+ idx)))))))))


;;;------------------------------------------------------------------- 
;;;
;;; FEATURE ORIENTED TOKENIZER
;;;

(defclass feature
          ()
    ((name :initarg :name :accessor feature-name)
     (count :initarg :count :accessor feature-count)))

(defmethod print-object ((feature feature) stream)
  (print-unreadable-object (feature stream :type t :identity t)
    (with-slots (name) feature
      (write-string (or name "No name yet") stream))))

(defvar *feature-table* (make-hash-table :test #'equalp))

(defun intern-feature (name &optional (if-does-not-exist :create)
                            (feature-table *feature-table*) copy-name-p)
  (declare (values feature newly-created-p))
  (cond ((gethash name feature-table))
        (t (ecase if-does-not-exist
             (:soft nil)
             (:create
               (let* ((key (if copy-name-p (copy-seq name) name))
                      (feature (make-instance 'feature :name key :count 0)))
                 (setf (gethash key feature-table) feature)
                 (values feature t)))
             (:error (error "No feature named, ~A, was found." name))))))

(defmacro with-interned-feature ((string &key (start 0) end (variable 'feature)
                                         (feature-table '*feature-table*)) &body body)
  `(let ((key (subseq ,string ,start ,end))
         (,variable nil))
     (declare (dynamic-extent key))
     (setq ,variable (intern-feature key :create ,feature-table t))
     ,@body))

(defun text-plain-next-feature (string pos &optional (length (length string)))
  (with-fast-array-references ((string string string))
    (loop with pointer = pos
          doing (loop for idx upfrom pointer below length
                      for char = (schar string idx)
                      until (word-delimiter-p char)
                      finally (cond ((< pointer idx)
                                     (with-interned-feature (string :start pointer :end idx)
                                       (return-from text-plain-next-feature
                                         (values feature (1+ idx)))))
                                    ((= idx length)
                                     (return-from text-plain-next-feature (values nil length)))
                                    (t (setq pointer (1+ idx))))))))

(defun text-html-next-feature (string pos &optional (length (length string)))
  (with-fast-array-references ((string string string))
    (loop named top
          with pointer = pos
          doing (loop with skip-element-p and start and last
                      for idx upfrom pointer below length
                      for char = (schar string idx)
                      until (and (not skip-element-p)
                                 (word-delimiter-p char))
                      unless (or skip-element-p
                                 (when (char-equal char #\<)
                                   (setq skip-element-p t)))
                        do (cond (start (setq last idx))
                                 (t (setq start idx)))
                      else do ;; close on close or open
                             (when (member char '(#\>) :test #'char-equal)
                               (setq skip-element-p nil))
                      finally (cond ((and start last (< start last))
                                     (with-interned-feature (string :start start :end (1+ last))
                                       (return-from text-html-next-feature
                                         (values feature (1+ idx)))))
                                    ((= idx length)
                                     (return-from text-html-next-feature (values nil length)))
                                    (t (return (setq pointer (1+ idx)))))))))

(defparameter *tokenizer* #'text-plain-next-token)

(defmacro with-content-type-tokenizer ((content-type-spec) &body body)
  `(destructuring-bind (&optional major minor &rest params)
       ,content-type-spec
     (declare (ignore major params))
     (let ((*tokenizer* (ecase minor
                          (:html (fdefinition 'text-html-next-feature))
                          (:plain (fdefinition 'text-plain-next-feature)))))
       ,@body)))

(declaim (inline tokenizer))

(defun tokenizer ()
  *tokenizer*)

(defun string-to-list (string &optional (length (length string)))
  (loop with pos = 0 and item
        with tokenizer = (tokenizer)
        while (< pos length)
        do (multiple-value-setq (item pos)
             (funcall tokenizer string pos length))
        when item
          collect item))


;;;------------------------------------------------------------------- 
;;;
;;; FREQUENCY TABLE
;;;

(defclass table-storage ()
    ((table :initform (make-hash-table :test #'equalp) :accessor table)
     (key :initform 0 :accessor counter-key)))

(defmethod initialize-storage (list (box table-storage))
  (loop for word in list
        do (store word 0 box)))

(defmethod init-key-count ((box table-storage))
  (key-store 0 box))

(defmethod key-store (value (box table-storage))
  (setf (counter-key box) value))

(defmethod key-ref ((box table-storage))
  (counter-key box))

(defmethod reference (key (box table-storage))
  (gethash key (table box)))

(defmethod store (key item (box table-storage))
  (setf (gethash key (table box)) item))

(defsetf reference store)

(defun make-box ()
  (make-instance 'table-storage))

(defmethod parse-string (pattern string (box table-storage) &aux (length (length string)))
  (with-slots (table) box
    ;;(format t "~&~'bPattern:~ ~S" pattern)
    (loop with n = 0 and tokenizer = (tokenizer)
          while (< n length)
          do (multiple-value-bind (token pos)
                 (funcall tokenizer string n)
                                                ;  (format t "~&~'b~S~" token)
               (cond
                 ((null token) nil)
                 ((member token pattern :test #'eq)
                  (incf (gethash token table)))
                 (t (incf (counter-key box))))
               (setq n pos)))))

(defmethod get-string-data (list string (box table-storage))
  (init-key-count box)
  (initialize-storage list box)
  (parse-string list string box))

(defun generate-frequencies-string (list string)
  (let ((box (make-box)))
    (declare (dynamic-extent box))
    (get-string-data list string box)
    (loop for word in list
          collect (if (zerop (key-ref box))
		      0.0
		      (/ (float (reference word box)) (key-ref box))))))

(defun generate-score-string (word-list weight-list string)
  (let ((results (generate-frequencies-string word-list (concatenate 'string string " "))))
    (apply #'+ (mapcar #'* results weight-list))))

(defmethod get-score-cache (activity)
  (with-activity-value-cached (activity :score-cache)
    (make-hash-table :test #'equalp)))

(defun get-score (url activity)
  (gethash url (get-score-cache activity)))

(defun %put-score (url activity score)
  (setf (gethash url (get-score-cache activity)) score))

(defsetf get-score %put-score)

(defun %compute-score (url activity words weights string)
  (setf (get-score url activity) (generate-score-string words weights string)))

(defun get-or-compute-score (url activity words weights)
  (or (get-score url activity)
      (multiple-value-bind (content headers status-code)
          (get-resource-content activity url)
        (declare (ignore status-code))
        (if content
            (handler-case-if (not *debug-walker*)
               (with-content-type-tokenizer ((get-header :content-type headers))
                 (%compute-score url activity words weights content))
              (error (err)
                     (record-url-note activity url :error-computing-search-score (report-string err))
                     -1))
            -1))))

(defvar *resource-score-alist* nil)

(defmacro with-resource-search ((&rest ignore) &body body)
  (declare (ignore ignore))
  `(let ((*resource-score-alist* nil)) ,@body))

(defmethod note-good-resource ((activity activity) (url http-url) score)
  (push (list* score url) *resource-score-alist*))

(defmethod sorted-resource-score-alist ((activity activity) &optional recompute-p)
  (with-activity-value-cached (activity :sorted-resource-score-alist :recompute-p recompute-p)
    (let ((good-resources *resource-score-alist*))
      (and good-resources
           (sort good-resources #'> :key #'car)))))

(defun report-scored-resource (url score stream)
  (html:with-rendition (:bold :stream stream)
    (html:note-anchor (name-string url) :reference url :stream stream)
    (html:break-line :stream stream)
    (write-string "Score: " stream)
    (write score :base 10 :stream stream)))

(defmethod check-and-maybe-report-score ((activity activity) (url http-url) threshold words weights
                                         stream)
  (let ((score (get-or-compute-score url activity words weights)))
    (cond ((and score (>= score threshold))
           (note-good-resource activity url score)
           (html:enumerating-item (stream)
             (report-scored-resource url score stream))
           (force-output stream)
           score)
          (t (html:enumerating-item (stream)
               (html:note-anchor (name-string url) :reference url :stream stream))
             (force-output stream)))))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

; query-list a list of lists; each sub-list has the form (string variable)

;;; a computed forms interface to the search module. . . .

(defgeneric compute-customized-search-form (url title description-string instruction-string query-list stream))

(defmethod compute-customized-search-form ((url url:http-form) title description-string instruction-string query-list stream)
  (macrolet ((with-query-environment ((label &key paragraph-p break-line-p) &body body)
               `(flet ((do-it ()
                         (http::with-rendition (:bold :stream stream)
                           (fresh-line stream)
                           (write-string ,label stream)
                           (when ,break-line-p (html:break-line :stream stream))
                           ,@body)))
                  (declare (dynamic-extent #'do-it))
                  (cond (,paragraph-p
                         (html:with-paragraph (:stream stream) (do-it)))
                        (t (do-it))))))
    (with-conditional-get-response (stream :html :expires (url:expiration-universal-time url))
      (html:with-html-document (:stream stream)
        (html:with-document-preamble (:stream stream)
          (html:declare-base-reference url :stream stream)
          (html:declare-title title :stream stream))
        (html:with-standard-document-body (:stream stream)
          (html:with-section-heading (title :stream stream)
            (http::image-line :stream stream)
            (html:with-paragraph (:stream stream)
              (write-string description-string stream))
            (write-string instruction-string stream)
            (html:with-fillout-form (:post url :stream stream)
              (dolist (entry query-list)
                (destructuring-bind ((label &key paragraph-p break-line-p)
                                     input-type query &rest args) entry
                  (with-query-environment
                    (label :paragraph-p paragraph-p :break-line-p break-line-p)
                    (apply #'html:accept-input input-type query :stream stream args))))
              (html:with-verbatim-text (:fresh-line nil :stream stream)
                (write-string "          " stream)
                (html:accept-input 'html:reset-button "Reset" :stream stream)
                (write-string "          " stream)
                (html:accept-input 'html:submit-button "Submit" :stream stream))
              (http::image-line :stream stream)
              (cl-http-signature stream))))))))
         
(define-action-type
  salton-check-url
  (:standard
    :documentation "Action for returning a node if it's score is above the threshold.")
  (action activity url threshold words weights stream)
  (declare (ignore action))
  (check-and-maybe-report-score activity url threshold words weights stream))

(define-action-type
  generate-salton-sorted-inferiors
  (:generator
    :documentation "Walks sorted inferiors."
    :class generator-type)
  (action activity url words weights)
  (declare (ignore action))
  (flet ((sort-inferiors-by-score (x y)
           (< (get-or-compute-score x activity words weights)
              (get-or-compute-score y activity words weights))))
    (declare (dynamic-extent #'sort-inferiors-by-score))
    (values (stable-sort (url-inferiors-satisfying-activity url activity) #'sort-inferiors-by-score)
	    t)))

(defun write-search-legend (url operator words weights threshold depth subdirectories-p
                                minimize-dns-p respect-no-robots-p hosts constraints stream)
  (html:with-paragraph (:stream stream)
    (html:with-enumeration (stream :itemize)
      (html:enumerating-item (stream)
        (write-string "Start URL: " stream)
        (html:note-anchor (name-string url) :reference url :stream stream))
      (html:enumerating-item (stream)
        (write-string "Operator: " stream)
        (html:note-anchor operator :reference (concatenate 'string "mailto:" operator) :stream stream))
      (html:enumerating-item (stream)
        (format stream "Words: ~{~A~^, ~}" words))
      (html:enumerating-item (stream)
        (format stream "Weights: ~{~D~^, ~}" weights))
      (html:enumerating-item (stream)
        (format stream "Threshold: ~D " threshold))
      (html:enumerating-item (stream)
        (format stream "Depth: ~:[unlimited~;~:*~D~]" depth))
      (html:enumerating-item (stream)
        (format stream "Only Subdirectories: ~:[no~;yes~]" subdirectories-p))
      (html:enumerating-item (stream)
        (format stream "Minimize DNS: ~:[no~;yes~]" minimize-dns-p))
      (html:enumerating-item (stream)
        (format stream "Respect No Robots: ~:[no~;yes~]" respect-no-robots-p))
      (html:enumerating-item (stream)
        (format stream "Constrain to hosts: ~:[no~;~:*~{~A~^, ~}~]" hosts))
      (html:enumerating-item (stream)
        (write-string "Constraints: " stream)
        (if constraints
            (html:with-emphasis (:code :stream stream)
              (html:with-verbatim-text (:fresh-line nil :width 120 :stream stream)
                (write constraints :stream stream :escape nil :pretty t :base 10.)))
            (write-string "no" stream))))))

(defgeneric perform-search (url words weights threshold &key operator depth subdirectories-p minimize-dns-p
                                respect-no-robots-p hosts constraints stream))

(defmethod perform-search ((url http-url) words weights threshold &key (operator http:*server-mail-address*)
                           depth subdirectories-p minimize-dns-p respect-no-robots-p
                           hosts constraints (stream *standard-output*))
  (let ((title  "W4 Constraint-Guided Web Search"))
    (html:with-html-document (:stream stream)
      (html:with-document-preamble (:stream stream)
        (html:declare-title title :stream stream))
      (html:with-standard-document-body (:stream stream)
        (html:with-section-heading (title :stream stream)
          ;; tell the user what is going on.  Clean this up more
          (write-search-legend url operator words weights threshold depth subdirectories-p
                               minimize-dns-p respect-no-robots-p hosts constraints stream)
          (let* ((*feature-table* (make-hash-table :test #'equalp))
                 (features (mapcar #'intern-feature words)))
            (declare (dynamic-extent *feature-table* features))
            (with-activity ("Enumerate-Useful-Documents"
                            (:operator operator
                             :url-host-name-resolution (if minimize-dns-p :never :preferred))
                            :constraints `((no-cycles)
                                           ,.(when depth
                                               `((depth ,depth)))
                                           ,.(when subdirectories-p
                                               `((url-subsumed-by-directory-path ,(url:path (intern-url url)))))
                                           ,@(when hosts
                                               `((url-host ,hosts)))
                                           ,@(when constraints constraints)
                                           ,@(when respect-no-robots-p
                                               (list '(header-robots-allowed)))
                                           (or ((header-content-type (:text :html)))
                                               ((header-content-type (:text :plain)))))
                            :actions `((html-with-enumeration
                                         ((salton-check-url ,threshold ,features ,weights ,stream)
                                          (generate-salton-sorted-inferiors ,features ,weights))
                                         ,stream :itemize)))
              (http::image-line :stream stream)
              (with-resource-search ()
                (html:with-section-heading ("Checking Documents ...." :stream stream) 
                  (html:with-paragraph (:stream stream)
                    (write-string "As W4 checks documents, it displays them. Documents
matching the search criteria appear in bold along with their score." stream))
                  (walk url activity))
                (html:with-section-heading ("Sorted Search Results" :stream stream)
                  (let ((score-alist (sorted-resource-score-alist activity)))
                    (cond (score-alist
                           (html:with-enumeration (stream :itemize)
                             (loop for (score . url) in score-alist 
                                   do (html:enumerating-item (stream)
                                        (report-scored-resource url score stream)))))
                          (t (html:with-paragraph (:stream stream)
                               (write-string "No resources were found that matched the search criteria." stream)))))))
              (http::image-line :stream stream)
              (cl-http-signature stream))))))))

(defun %clean-up-weights (keys key-weights)
  (let ((len-k (length keys))
        (len-w (length key-weights)))
    (cond ((= len-k len-w) key-weights)
          ((< len-k len-w)
           (butlast key-weights (- len-w len-k)))
          (t (append key-weights
                     (make-list (- len-k len-w)
                                :initial-element (car (last key-weights))))))))

(defmethod respond-to-search ((url http-form) stream query-alist)
  (flet ((clean-up-weights (keys weights)
           (let ((len-k (length keys))
                 (len-w (length weights)))
             (cond ((zerop len-w)
                    (make-list len-k :initial-element 100))
                   ((= len-k len-w) weights)
                   ((< len-k len-w)
                    (butlast weights (- len-w len-k)))
                   (t (append weights
                              (make-list (- len-k len-w)
                                         :initial-element (car (last weights)))))))))
    (bind-query-values (root-url operator search-pattern weights threshold depth subdirectories-p hosts
                                 origin-host-only-p  minimize-dns-p respect-no-robots-p constraints)
                       (url query-alist)
      (let* ((start-url (intern-url root-url))
             (operator (string-trim '(#\space #\tab) operator))
             (keys (string-to-list search-pattern))
             (key-weights (clean-up-weights keys (mapcar #'read-from-string (listify-string weights))))
             (doc-threshold (or (read-from-string threshold) .001))
             (max-depth (when depth
                       (unless (null-string-p (setq depth (string-trim '(#\space #\tab) depth)))
                         (parse-integer depth))))
             (subdirs-p (equalp subdirectories-p "yes"))
             (dns-p (equalp minimize-dns-p "yes"))
             (robots-p (equalp respect-no-robots-p "yes"))
             (host-list (listify-string hosts))
             (constraint-list (read-constraints-from-string constraints)))
        (when (equalp origin-host-only-p "yes")
          (pushnew (url:host-string start-url) host-list :test #'equalp))
        (with-successful-response (stream :html :expires (url:expiration-universal-time url))
          (cond (keys (perform-search start-url keys key-weights doc-threshold
                                      :operator operator
                                      :depth max-depth
                                      :subdirectories-p subdirs-p
                                      :minimize-dns-p dns-p
                                      :respect-no-robots-p robots-p
                                      :hosts host-list
                                      :constraints constraint-list
                                      :stream stream))
                (t  (let ((title  "W4 Search Specification Problems"))
                      (html:with-html-document (:stream stream)
                        (html:with-document-preamble (:stream stream)
                          (html:declare-title title :stream stream))
                        (html:with-standard-document-body (:stream stream)
                          (html:with-section-heading (title :stream stream)
                            (http::image-line :stream stream)
                            (write-string "No search keys were provided.  Please try again." stream))
                          (http::image-line :stream stream)
                          (cl-http-signature stream)))))))))))

(defmethod compute-salton-search-form ((url url:http-form) stream)
  (compute-customized-search-form
    url
    "W4 Constraint-Guided Search"
    "Walk the Web in search of the documents of your choice!"
    "<p>This form allow you to explore a region of the Web and find documents
based on the weighted frequencies of keywords. Use the starting URL
with predefined constraints such depth, subdirectory, or host limits to
control the area explored. Choose words, a weight for each word, and a
threshold to specify when a document should be selected.</p>

<p>Note that the contribution of a word (unit weight) appearing in every sentence
of a document is on the order of 0.04; please adjust thresholds and
weights accordingly.</p>

<p><a href=\"/cl-http/find-constraints?\">Additional constraints</a> may be
specified in the constraints box.</a>.</p>"

    '((("URL [starting point]: " :paragraph-p t :break-line-p t)
       html:string "ROOT-URL" :size 80)
      (("Operator [Email Address]: " :paragraph-p t :break-line-p t)
       html:string "OPERATOR" :size 30)
      (("Words [search keys]: " :paragraph-p t :break-line-p t)
       html:string "SEARCH-PATTERN" :size 50)
      (("Weights [key relevance]: " :paragraph-p t :break-line-p t) html:string "weights" :size 30)
      (("Threshold [minimum score for good documents]: " :paragraph-p t :break-line-p t)
       html:string "THRESHOLD" :size 5 :default ".001")
      (("Depth [maximum steps explored]: " :paragraph-p t :break-line-p t)
       html:string "DEPTH" :size 5)
      (("Origin Host Only [explore no other servers]:")
       html:radio-button "ORIGIN-HOST-ONLY-P" :choices (("Yes" . "Yes") ("No" . "No"))
       :default "Yes" :linebreaks nil)
      (("Only Subdirectories:" :paragraph-p t)
       html:radio-button "SUBDIRECTORIES-P"
       :choices (("Yes" . "Yes") ("No" . "No")) :default "No" :linebreaks nil)
      (("Hosts [limit walk to servers]: " :paragraph-p t :break-line-p t)
       html:string "HOSTS" :size 80)
      (("Minimize DNS [non-canonical host names]: " :paragraph-p t)
       html:radio-button "MINIMIZE-DNS-P"
       :choices (("Yes" . "Yes") ("No" . "No")) :default "Yes" :linebreaks nil)
      (("Respect No Robots:" :paragraph-p t)
       html:radio-button "RESPECT-NO-ROBOTS-P"
       :choices (("Yes" . "Yes") ("No" . "No")) :default "Yes" :linebreaks nil)
      (("Constraints [specify a list of <a href=\"/cl-http/find-constraints?\">constraints</a>]:"
        :paragraph-p t :break-line-p t)
       html:multi-line-text "CONSTRAINTS"))
    stream))


