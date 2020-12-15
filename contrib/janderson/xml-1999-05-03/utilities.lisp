;;; -*- package: "XML-PARSER"; -*-
;;;

"
<DOCUMENTATION>
 <DESCRIPTION>
  substrate-independent handling for system id's
  </DESCRIPTION>
 <COPYRIGHT HREF='defsystem.lisp|root().descendant(1,COPYRIGHT)'/>
 <CHRONOLOGY>
  <DELTA><DATE>19981120</DATE>
   new</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
 "

(in-package "XML-PARSER")

(defMethod system-id.timestamp
           ((id pathname))
  (when (setf id (probe-file id))
    (file-write-date id)))

#-CL-HTTP
(defMethod  local-string
            ((id string))
  (when (and (> (length id) 7) (string-equal id "file://" :end1 7))
    (setf id (subseq id 7)))
  (nsubstitute #\: #\/ id)
  (if (find #\: id :test #'char=)
    (if (eql (char id 0) #\:)
      (subseq id 1)
      (concatenate 'string ":" id))
    id))

;; 19991024 use in both cases for now #-CL-HTTP
(defMethod system-id.timestamp
           ((id string))
  (file-write-date (translate-system-id id)))
;;(local-string "file:///asdf/qwer.ertz")

#+CL-HTTP
(defMethod system-id.timestamp
           ((id file-url))
  (system-id.timestamp (translated-pathname id)))

#+CL-HTTP
(defmethod system-id.timestamp
           ((location http-url) &aux modification-time)
  (http:with-http-request (location :head)
    (when  (setf modification-time (http:get-header :last-modification))
      (parse-integer  modification-time :junk-allowed t))))


(defMethod local-string
           ((pathname pathname))
  (namestring pathname))

(defMethod name-string
           ((pathname pathname) &optional (compute-p nil)
            &aux (namestring (namestring pathname)))
  (declare (ignore compute-p))
  (nsubstitute #\/ #\: namestring)
  (setf namestring
        (if (eql (char namestring 0) #\/)
          (concatenate 'string "file://" namestring)
          (concatenate 'string "file:///" namestring))))

(defMethod name-string
           ((pathname logical-pathname) &optional (compute-p nil))
  (declare (ignore compute-p)) (break)
  (namestring (translate-logical-pathname pathname)))

(defMethod local-string
           ((pathname logical-pathname))
  (namestring (translate-logical-pathname pathname)))

#+CCL
(defMethod name-string
           ((source fred-window) &optional (compute-p nil))
  (declare (ignore compute-p))
  (name-string (pathname source)))

#+CCL
(defMethod local-string
           ((source fred-window))
  (or (ignore-errors (namestring (pathname source)))
      ""))

#+CL-HTTP
(defMacro with-local-context
          (source &rest body)
  ;; by this point the location - whether as a pathname when without cl-http, or
  ;; as an url - when with cl-http, is fully specified and the name-string
  ;; serves as the local context
  `(let ((*local-context* (name-string ,source)))
     ,@body))

#-CL-HTTP
(defMacro with-local-context
          (source &rest body)
  ;; by this point the location - whether as a pathname when without cl-http, or
  ;; as an url - when with cl-http, is fully specified and the name-string
  ;; serves as the local context
  `(let ((*local-context* (local-string ,source)))
     ,@body))


(defMethod stream-pathname
           ((stream stream))
  (pathname stream))

(defMethod stream-pathname
           ((stream concatenated-stream))
  (stream-pathname (first (concatenated-stream-streams stream))))

(defMethod stream-pathname
           ((stream t))
  nil)


(defun xml-file-p
       (pathname)
  (with-open-file (stream pathname :direction :input 
                          :if-does-not-exist nil)
    (when stream (eql #\< (peek-char t stream nil nil)))))
;(xml-file-p (choose-file-dialog)) (xml-file-p "asdf")

#+CL-HTTP
(defmethod translate-system-id
           ((system-id string))
  (parse-url (http:merge-url system-id (local-context))))

#-CL-HTTP
(defmethod translate-system-id
           ((system-id string))
  (merge-pathnames (local-string system-id) (local-context)))

#-CL-HTTP
(defmethod translate-system-id
           ((system-id pathname))
  (translate-system-id (namestring system-id)))

#+CCL
(defMethod string-stream-string
           ((stream string-stream))
  (slot-value stream 'ccl::my-string))

#|
(with-local-context #p"asdf:qwer.xml" (translate-system-id "yxcv.dtd"))
(with-local-context "file:///asdf/qwer/" (translate-system-id "asdf.qwer"))
(with-local-context (choose-file-default-directory )
  (translate-system-id "dtd/lisp.dtd"))
(with-local-context (pathname (choose-file-default-directory ))
  (print (local-context)))

|#

"XMLP"






