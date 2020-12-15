
(in-package "XMLP")

(ed (choose-file-dialog))
(defparameter *test-doc* (read-xml-stream (choose-file-dialog)))
(pprint *test-doc*)
(inspect *test-doc*)
(element.valid? (document.element *test-doc*))


;; this forces recompilation of the predicates: first delete them;
(dolist (d (document-type.element-declarations (document.doctype *test-doc*)))
  (setf (element-declaration.predicate d) nil))
;; then delete the cached validity values
(reset-element.valid? (document.element *test-doc*))

;; switch elements here to 'break' it.
(let ((e1 (first (element.content (document.element *test-doc*))))
      (e2 (second (element.content (document.element *test-doc*)))))
  (rotatef (first (element.content e1)) (first (element.content e2))))

