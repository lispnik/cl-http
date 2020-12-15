;;; -*- Package: ("HTTP"); -*-

(in-package "HTTP")

"<DOCUMENTATION>
  <DESCRIPTION>
  server interface to generation functions
  <P>
  they will attempt to serve in the form which the client accepts.
  which means xml-capable clients get xml, html-capable get the
  xml restyled to html elements...

  see <A HREF='/Doc/FILE?PATHNAME=Source;XML:dom-exports.lisp'>dom-exports.lisp</A>
 for the url exports.
 "

(defMethod dom-documentation-response
           (datum stream (mime-type (eql :xml))
                  &aux (xmlp::*print-readably* t))
  (princ (make-instance 'xmlp::document
           :doctype nil
           :element (xmlp:dom-element datum :DOM))
         stream))

(defMethod dom-documentation-response
           (datum stream (mime-type (eql :html))
            &aux (xmlp::*print-readably* t) element)
  ;; html is printed with the default namespace set so as to leave the tokens unqualified.
  (setf element (xmlp:dom-element datum :HTML))
  (typecase element
    (xmlp:element
     (xmlp:with-default-namespace xmlp::*html-package*
       (print-object element stream))
     element)
    (t
     (call-next-method))))
    
(defMethod dom-documentation-response
           (datum stream (mime-type t))
  (xml-1.0:with-element ('html :stream stream)
    (xml-1.0:with-element ('head :stream stream)
      (xml-1.0:with-element ('title :stream stream)
        (write-string "error..." stream)))
    (xml-1.0:with-element ('body :stream stream)
      (format stream "can't generate documentation for (~s ~s ~s)"
              datum stream mime-type))))


(defMethod dom-documentation-response
           ((datum string) stream (mime-type (eql :xml)))
  (XML-1.0:with-xml-document ('error :stream stream)
    (xml-1.0:with-element ('error :stream stream)
      (write-string datum stream))))

(defMethod dom-documentation-response
           ((datum string) stream (mime-type (eql :html)))
  (xml-1.0:with-element ('html :stream stream)
      (xml-1.0:with-element ('head :stream stream)
        (xml-1.0:with-element ('title :stream stream)
          (write-string "error..." stream)))
      (xml-1.0:with-element ('body :stream stream)
        (write-string datum stream))))


(defMethod dom-document-source-file
           ((pathname pathname) (stream stream) (mime-type (eql :html))
            &aux (eof (gensym "EOF")))
  (with-open-file (input pathname :direction :input)
    (xml-1.0:with-element ('html :stream stream)
      (xml-1.0:with-element ('head :stream stream)
        (xml-1.0:with-element ('title :stream stream)
          (write-string (pathname-name pathname)  stream)))
      (xml-1.0:with-element ('body :stream stream)
        (do ((form (read input nil eof) (read input nil eof)))
            ((eq form eof))
          (typecase form
            (string (write-string form stream))
            (t
             (xml-1.0:with-element ('pre :stream stream)
               (pprint form stream)))))))))

(defMethod dom-document-source-file
           ((pathname pathname) (stream stream) (mime-type (eql :xml))
            &aux (eof (gensym "EOF")))
  (with-open-file (input pathname :direction :input)
    (XML-1.0:with-xml-document ('source-file :stream stream)
      (do ((form (read input nil eof) (read input nil eof)))
          ((eq form eof))
        (typecase form
          (string (write-string form stream))
          (t
           (xml-1.0:with-element ('pre :stream stream)
             (pprint form stream))))))))
      
"XML"
