(in-package :XMLP)

#|
 here a simple example of xml access to macptr records
 |#

(defrecord test (field1 :word) (field2 :long) (field3 (:string 7)))
(defGeneric test.field1 (test))
(defGeneric (setf test.field1) (datum test))
(defGeneric (setf test.field2) (datum test))
(defGeneric (setf test.field3) (datum test))

(clos::defClass test (xml-record)
  ()
  (:record-definition (field1 :word)
                      (field2 :long)
                      (field3 (:string 7))
                      (field4 (:array :character 8))
                      (field5 (:array :byte 8)))
  (:metaclass xml-record-class))

(let ((test (ccl::%new-gcable-ptr (record-length :test))))
  (rset test test.field1 1)
  (rset test test.field2 2)
  (rset test test.field3 "asdf")
  (%put-fstring test "12345678" 14 8)
  (%put-vector test #(1 2 3 4 5 6 7 8) 22 8)
  (let ((i (make-instance 'test :record test))
        (*xml-print-readably* t))
    (inspect i)
    (print i)
    (incf (test.field1 i))
    (setf (test.field2 i) -100)
    (setf (test.field3 i) "qwert")
    (print i)
    (list (xml-element.field-content i :field1)
          (xml-element.field-content i :field2)
          (xml-element.field-content i :field3)
          (xml-element.field-content i :field4)
          (xml-element.field-content i :field5))))

:EOF
