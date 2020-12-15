;;; -*- Package: ("HTTP"); -*-

(in-package "HTTP")
;;;
;;; exports for dom documentation functions
;;;
;;; 19990430 capitalized directory names

(export-url #u"/Doc/PACKAGE?"
            :search
            :response-function
            #'(lambda (url stream)
                (let* ((keys (search-keys url))
                       (name (second (assoc :name keys)))
                       (package nil)
                       (*print-pretty*
                        (if (assoc :pretty keys) t *print-pretty*))
                       (mime-type (header-cond
                                   ((:text :xml) :xml)
                                   (t :html))))
                  (with-successful-response (stream (list :text mime-type))
                    (cond (name
                           (setf package (find-package name))
                           (if package
                             (dom-documentation-response
                              package
                              stream mime-type)
                             (dom-documentation-response
                              (format nil "package ~s not found." name)
                              stream mime-type)))
                          (t
                           (dom-documentation-response
                            (format nil "no package name specified: ~s." keys)
                            stream mime-type))))))
            :search-parser #'parse-search-info-as-query-alist)

(export-url #u"/Doc/CLASS?"
            :search
            :response-function
            #'(lambda (url stream)
                (let* ((keys (search-keys url))
                       (name (second (assoc :name keys)))
                       (class nil)
                       (*print-pretty*
                        (if (assoc :pretty keys) t *print-pretty*))
                       (mime-type (header-cond
                                   ((:text :xml) :xml)
                                   (t :html))))
                  (with-successful-response (stream (list :text mime-type))
                    (cond ((and name
                                (setf name (ignore-errors (read-from-string name))))
                           (setf class (find-class name nil))
                           (if class
                             (dom-documentation-response
                              class
                              stream mime-type)
                             (dom-documentation-response
                              (format nil "class ~s not found." name)
                              stream mime-type)))
                          (t
                           (dom-documentation-response
                            (format nil "no class name specified: ~s." keys)
                            stream mime-type))))))
            :search-parser #'parse-search-info-as-query-alist)

(export-url #u"/Doc/FUNCTION?"
            :search
            :response-function
            #'(lambda (url stream)
                (let* ((keys (search-keys url))
                       (name (second (assoc :name keys)))
                       (*print-pretty*
                        (if (assoc :pretty keys) t *print-pretty*))
                       (mime-type (header-cond
                                   ((:text :xml) :xml)
                                   (t :html))))
                  (with-successful-response (stream (list :text mime-type))
                    (cond ((and name
                                (setf name (ignore-errors (read-from-string name))))
                           (if (fboundp name)
                             (dom-documentation-response
                              (symbol-function name)
                              stream mime-type)
                             (dom-documentation-response
                              (format nil "function ~s not found." name)
                              stream mime-type)))
                          (t
                           (dom-documentation-response
                            (format nil "no function name specified: ~s." keys)
                            stream mime-type))))))
            :search-parser #'parse-search-info-as-query-alist)

(export-url #u"/Doc/METHOD?"
            :search
            :response-function
            #'(lambda (url stream)
                (let* ((keys (search-keys url))
                       (name (second (assoc :name keys)))
                       (specializers nil)
                       (qualifiers nil)
                       (method nil)
                       (function nil)
                       (*print-pretty*
                        (if (assoc :pretty keys) t *print-pretty*))
                       (mime-type (header-cond
                                   ((:text :xml) :xml)
                                   (t :html))))
                  (with-successful-response (stream (list :text mime-type))
                    (cond ((and name
                              (consp (setf name (ignore-errors (read-from-string name)))))
                         (setf qualifiers (remove-if-not #'keywordp name))
                         (setf name (set-difference name qualifiers))
                         (cond ((setf qualifiers
                                      (ignore-errors (mapcar #'find-class (rest name))))
                                (setf name (first name))
                                (if (and (fboundp name)
                                         (typep (setf function (symbol-function name))
                                                'generic-function))
                                  (if (setf method (find-method function
                                                                qualifiers
                                                                specializers))
                                    (dom-documentation-response
                                     method
                                     stream mime-type)
                                    (dom-documentation-response
                                     (format nil "method not found: ~s ~s ~s"
                                             name qualifiers specializers)))
                                  (dom-documentation-response
                                   (format nil "generic function not found: ~s." name)
                                   stream mime-type)))
                               (t
                                (dom-documentation-response
                                   (format nil "specializier(s) ~s not found." (rest name))
                                   stream mime-type))))
                        (t
                         (dom-documentation-response
                            (format nil "no method specified: ~s." keys)
                            stream mime-type))))))
            :search-parser #'parse-search-info-as-query-alist)

(export-url #u"/Doc/SYMBOL?"
            :search
            :response-function
            #'(lambda (url stream)
                (let* ((keys (search-keys url))
                       (name (second (assoc :name keys)))
                       (*print-pretty*
                        (if (assoc :pretty keys) t *print-pretty*))
                       (mime-type (header-cond
                                   ((:text :xml) :xml)
                                   (t :html))))
                  (with-successful-response (stream (list :text mime-type))
                    (cond ((and name
                                (setf name (ignore-errors (read-from-string name))))
                           (if (boundp name)
                             (dom-documentation-response
                              name
                              stream mime-type)
                             (dom-documentation-response
                              (format nil "symbol ~s not bound." name)
                              stream mime-type)))
                          (t
                           (dom-documentation-response
                            (format nil "no symbol specified: ~s." keys)
                            stream mime-type))))))
            :search-parser #'parse-search-info-as-query-alist)

(export-url #u"/Doc/URL?"
            :search
            :response-function
            #'(lambda (url stream)
                (let* ((keys (search-keys url))
                       (name (or (second (assoc :string keys))
                                 (second (assoc :name keys))))
                       (target-url nil)
                       (*print-pretty*
                        (if (assoc :pretty keys) t *print-pretty*))
                       (mime-type (header-cond
                                   ((:text :xml) :xml)
                                   (t :html))))
                  (with-successful-response (stream (list :text mime-type))
                    (cond (name
                           (setf target-url (intern-url (merge-url name (local-context))
                                                 :if-does-not-exist :soft))
                           (if target-url
                             (dom-documentation-response
                              target-url
                              stream mime-type)
                             (dom-documentation-response
                              (format nil "url ~s not found." name)
                              stream mime-type)))
                          (t
                           (dom-documentation-response
                            (format nil "no url name specified: ~s." keys)
                            stream mime-type))))))
            :search-parser #'parse-search-info-as-query-alist)

(export-url #u"/Doc/SOURCE-FILE?"
            :search
            :response-function
            #'(lambda (url stream)
                (let* ((keys (search-keys url))
                       (name (or (second (assoc :pathname keys))
                                 (second (assoc :name keys))))
                       (*print-pretty*
                        (if (assoc :pretty keys) t *print-pretty*))
                       (mime-type (header-cond
                                   ((:text :xml) :xml)
                                   (t :html)))
                       (pathname nil))
                  (with-successful-response (stream (list :text mime-type))
                    (cond (name
                           (setf pathname (probe-file name))
                           (if pathname
                             (dom-document-source-file pathname stream mime-type)
                             (dom-documentation-response
                              (format nil "source file ~s not found." name)
                              stream mime-type)))
                          (t
                           (dom-documentation-response
                            (format nil "no file name specified: ~s." keys)
                            stream mime-type))))))
            :search-parser #'parse-search-info-as-query-alist)

"XML"
