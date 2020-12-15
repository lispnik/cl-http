(in-package :HTTP)

#|
 here a simple example of generation within the cl-http server.
 (note that the http server must already have been present for the required
  xml files to have been loaded...)
 two approaches are possible:
 either one can generate an xml stream procedurally, or
 one can construct the document and print it.
 for the sake of illustration, the email example file is loaded below and
 later re-serialized to a http client.
 |#

(let ((stream *standard-output*))
  (with-xml-document ('testing :stream stream :dtd "http://asdf/asdf.dtd")
    (with-xml-element ('asdf :stream stream)
      (write-string "content content" stream))))

(let ((stream *standard-output*))
  (with-xml-document ('testing :stream stream
                               :dtd '("http://asdf/public.dtd"
                                      "http://asdf/system.dtd"))
    (with-xml-element ('asdf :stream stream)
      (write-string "content content" stream))))

(let ((stream *standard-output*))
  (with-xml-document ('testing :stream stream
                               :dtd '("http://asdf/public.dtd"
                                      "http://asdf/system.dtd"))
    (with-xml-element ('asdf :stream stream
                             :attributes '(("test" "one") ("test" "two")))
      (write-string "content content" stream))))


(defParameter *xml-doc* (xml-parser:read-xml-stream #4p"xml:email.xml"))

(defun xml-response
       (url stream &aux (xml-parser:*xml-print-readably* t))
  (declare (ignore url))
  (http:with-successful-response (stream '(:text :xml))
    (inspect *xml-doc*)
    (princ *xml-doc* stream)))

(export-url #u"/xml-test"
            :computed
            :response-function #'xml-response)

:EOF
