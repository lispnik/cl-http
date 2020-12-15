;;; -*- Package: ("HTTP") -*-


(in-package "HTTP")

#|
 here a simple example of generation within the cl-http server.
 (note that the http server must already have been present for the required
  xml files to have been loaded...)
 two general approaches are possible:
 1. one can generate an xml stream procedurally. this is the approach taken
    for html generation in the base cl-http server. the with-xml-* forms
    perform this way.
 2. one can construct/read/transform a "document object model" (that's what
    the w3 calls them) and print it.

 aside from the simple examples, the email example file is loaded and later
 re-serialized to a http client.
 |#

;; first simple generation examples
(let ((stream *standard-output*)
      (xmlp::*print-readably* t)
      (*print-pretty* t))
  (xmlp:with-document ('testing :stream stream :public "http://asdf/asdf.dtd")
    (with-element ('asdf :stream stream)
      (write-string "content content" stream))))

(let ((stream *standard-output*)
      (xmlp::*print-readably* t)
      (*print-pretty* t))
  (xmlp:with-document ('testing :stream stream
                               :public "http://asdf/public.dtd"
                               :system "http://asdf/system.dtd")
    (with-element ('asdf :stream stream)
      (write-string "content content" stream))))

(let ((stream *standard-output*)
      (xmlp::*print-readably* t)
      (*print-pretty* t))
  (xmlp:with-document ('testing :stream stream
                               :public "http://asdf/public.dtd"
                               :system "http://asdf/system.dtd")
    (with-element ('asdf :stream stream
                             :attributes '(("test" "one") ("test" "two")))
      (write-string "content content" stream))))


;; next, load a document and serve it

(defParameter *xml-doc* (xml-parser:read-xml-stream #4p"xml:email.xml"))

(defun xml-response
       (url stream &aux (xmlp::*print-readably* t))
  (declare (ignore url))
  (http:with-successful-response (stream '(:text :xml))
    (inspect *xml-doc*)
    (princ *xml-doc* stream)))

(export-url #u"/xml-test"
            :computed
            :response-function #'xml-response)


"XML"
