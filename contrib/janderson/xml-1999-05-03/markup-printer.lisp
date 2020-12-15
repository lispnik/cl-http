;;; -*- mode: lisp; package: "XML-PARSER"; -*-

(in-package "XML-PARSER")

(defParameter *character-data-escaped-characters*
  (map 'vector #'char-code "<>%&"))

(defParameter *attribute-data-escaped-characters*
  (map 'vector #'char-code "<&'\""))

(defParameter *entity-data-escaped-characters*
  (map 'vector #'char-code "%'\""))

(defun write-attribute-name
       (name stream
        &aux
        (namestring (string name))
        (at (position *attribute-namespace-punctuation-char* namestring)))
  ;; for attribute names: if they are element-relative then they are always
  ;; written unqualified.
  (if at
    (write-string namestring stream :end at)
    (write-identifier name stream)))
  
(defMethod write-system-identifier
           ((system string) stream)
  (write-char *attribute-quote-char* stream)
  (write-system-id-value system stream)
  (write-char *attribute-quote-char* stream))

#+CL-HTTP
(defMethod write-system-identifier
           ((system url) stream)
  (write-char *attribute-quote-char* stream)
  (write-system-id-value (name-string system) stream)
  (write-char *attribute-quote-char* stream))

(defMethod write-system-identifier
           ((system pathname) stream)
  (write-char *attribute-quote-char* stream)
  (write-system-id-value (namestring system) stream)
  (write-char *attribute-quote-char* stream))

  
(defun write-external-id-parameters
       (system public stream)
  (cond (public
         (write-string "PUBLIC " stream)
         (write-char *attribute-quote-char* stream)
         (write-public-id-value system stream)
         (write-char *attribute-quote-char* stream)
         (when system
           (write-char #\space stream)
           (write-system-identifier system stream)))
        (system
         (write-string "SYSTEM " stream)
         (write-system-identifier system stream))))


(defun write-notation-parameter
       (notation stream)
  (when notation
    (write-string "NOTATION " stream)
    (write-char *attribute-quote-char* stream)
    (write-notation-value notation stream)
    (write-char *attribute-quote-char* stream)))

(defun write-attribute-string
       (string stream)
  (write-char *attribute-quote-char* stream)
  (write-attribute-data-value string stream)
  (write-char *attribute-quote-char* stream))



(defun write-character-data-value
       (string stream)
  (write-string-escaping-characters string stream *character-data-escaped-characters*))

(defun write-attribute-data-value
       (string stream)
  (write-string-escaping-characters string stream *attribute-data-escaped-characters*))

(defun write-parameter-entity-value
       (string stream)
  (write-string-escaping-characters string stream *entity-data-escaped-characters*))

(defun write-notation-value
       (string stream)
  (write-string string stream))

(defun write-system-id-value
       (string stream)
  (write-string string stream))

(defun write-public-id-value
       (string stream)
  (write-string string stream))



(defun write-string-escaping-characters
       (string stream escaped-characters
        &aux (start 0) (end (length string)) code (i start))
  "write data string content to stream, escaping special characters"
  (when (> end 0)
    (loop (setf code (char-code (aref string i)))
          (when (find code escaped-characters)
            (unless (= start i)
              (write-string string stream :start start :end i))
            (format stream "&#x~3,'0x;" code))
          (when (>= (incf i) end) (return))
          (setf start i))
    (unless (= start i)
      (write-string string stream :start start :end i))))


(defun write-string-escaping-characters
       (string stream escaped-characters &aux (start 0) (end (length string)) code (i start))
  "write data string content to stream, escaping special characters"
  (unless (zerop end)
    (loop (setf code (char-code (aref string i)))
          (when (find code escaped-characters)
            (unless (= start i)
              (write-string string stream :start start :end i))
            (format stream "&#x~3,'0x;" code)
            (setf start (1+ i)))
          (when (>= (incf i) end) (return)))
    (unless (= start i)
      (write-string string stream :start start :end i))))

;(write-character-data-value "" *trace-output*)
;(write-character-data-value "0<>1234" *trace-output*)
;(write-attribute-data-value "0<&'1234" *trace-output*)
;(write-string-escaping-characters "asdf" *trace-output* #(60))

#|
(defun write-identifier
       (symbol stream &aux (package (symbol-package symbol)))
  ; (when (string-equal symbol "a") (break))
  (cond ((eq symbol *default-namespace-prefix*)
         (write-string (string *ns-prefix*) stream))
        (t
         (when (and package
                    (not (eq package *xml-package*))
                    (not (default-namespace? package)))
           (write-string (string (namespace.prefix package)) stream)
           (write-char *namespace-punctuation-char* stream))
         (write-string (symbol-name symbol) stream)))
  symbol)
|#

;; destination kann ein Stream, nil oder erweiterbarer character array sein
;; liefert einen String, falls destination = nil, sonst das Symbol
(defun write-identifier
       (symbol destination &aux (package (symbol-package symbol)))
   (or
    (cond ((eq symbol *default-namespace-prefix*)
           (format destination "~A" *ns-prefix*))
          ((and package (not (eq package *xml-package*)) (not (default-namespace? package)))
           (format destination "~A~A~A" 
             (namespace.prefix package) *namespace-punctuation-char* (symbol-name symbol)))
          (t (format destination "~A" (symbol-name symbol))))
    symbol))

(defun write-identifier-list
       (list stream &aux id)
  (write-char #\( stream)
  (loop (setf id (pop list))
        (typecase id
          (symbol (write-identifier id stream))
          (t (print-object id stream)))
        (unless list (return))
        (write-char #\space stream))
  (write-char #\) stream))



"XMLP"
