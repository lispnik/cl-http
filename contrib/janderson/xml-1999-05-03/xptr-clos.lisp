;;; -*- Package: "XMLP"; -*-

"<DOCUMENTATION>
 <DESCRIPTION>
  these functions sketch an XPTR based interface to clos instrances.
  they are factored out as they are not essential to its use for dom trees and
  they are have not kept sync with changes in the main xptr implementation.
  <BR />
  the locations are analogous to those in a dom graph.
  any terms which might have no
  immediate meaning can be understood as denoting that instance which would
  appear in the position denoted in the equivalent serialized encoding.
  there are, for example, a number of path terms which specify the next node
 by position relative to the current instance in the equivalent element graph.
 this position is not necessarily 'within' or 'below' the current instance.
 for this purpose the access protocol for tractable instances specifies that a
 node must either implement the function
 <A HREF='/Doc/FUNCTION?NAME=XMLP::XPTR-NODE.PARENT'><CODE>XPTR-NODE.PARENT</CODE</A>
 or it must bind one fo the slots <CODE>PARENT</CODE> or <CODE>CONTEXT</CODE>.

  </DESCRIPTION>
 <CHRONOLOGY>
  <DELTA DATE='19990420'>get-precedence-list</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
"
(in-package "XMLP")

(defMethod xptr-element-match?
           ((constraint symbol) (datum standard-object))
  ;; this is unrolled since it takes about a third the time of the generic
  ;; composition...
  (and (setf constraint (find-class constraint nil))
       (find constraint (get-precedence-list (class-of datum)))))

(defMethod xptr-node.parent
           ((node standard-object))
  (cond ((slot-exists-p node 'context)
         (slot-value node 'context))
        ((slot-exists-p node 'parent)
         (slot-value node 'parent))
        (t
         nil)))

(defMethod xptr-node-map-children
           ((*xptr-predecessor* standard-object) function
            &aux (class (class-of *xptr-predecessor*))
                 reader)
  (dolist (slot (class-class-slots class))
    (when (setf reader (first (slot-readers slot)))
      (funcall function (funcall reader *xptr-predecessor*))))
  (dolist (slot (class-instance-slots class))
    (when (setf reader (first (slot-readers slot)))
      (funcall function (funcall reader *xptr-predecessor*)))))

(defMethod xptr-node-map-ancestors
           ((*xptr-predecessor* standard-object) function)
  (let ((parent (xptr-node.parent *xptr-predecessor*)))
    (when parent
      (funcall function parent)
      (xptr-node-map-ancestors parent function))))

(defMethod xptr-term-map-successors
           ((*xptr-term* %xptr::|child|) (datum standard-object) collector)
  (xptr-node-map-children datum collector))

(defMethod xptr-term-map-successors
           ((*xptr-term* %xptr::|descendant|) (datum standard-object) collector)
  (xptr-node-map-descendants datum collector))

(defMethod xptr-term-map-successors
           ((*xptr-term* %xptr::|ancestor|) (datum standard-object) collector)
  (xptr-node-map-ancestors datum collector))

(defMethod xptr-term-map-successors
           ((*xptr-term* %xptr::|preceding|) (datum standard-object) collector)
  (warn "NYI: (xptr-term-map-successors ~a ~a ~a)" term datum collector))

(defMethod xptr-term-map-successors
           ((*xptr-term* %xptr::|following|) (datum standard-object) collector)
  (warn "NYI: (xptr-term-map-successors ~a ~a ~a)" term datum collector))

(defMethod xptr-term-map-successors
           ((*xptr-term* %xptr::|psibling|) (datum standard-object) collector)
  (warn "NYI: (xptr-term-map-successors ~a ~a ~a)" term datum collector))

(defMethod xptr-term-map-successors
           ((*xptr-term* %xptr::|fsibling|) (datum standard-object) collector)
  (warn "NYI: (xptr-term-map-successors ~a ~a ~a)" term datum collector))


(defMethod xptr-term-map-location
           ((*xptr-term* %xptr::|root|)
            (*xptr-predecessor* standard-object)
            collector)
  ;; root tries to walk back up the containment tree to locate the top-most
  ;; node
  (let ((root datum))
    (xptr-node-map-ancestors *xptr-predecessor* #'(lambda (new-parent)
                                                    (setf root new-parent)))
    (funcall collector root)))

(defMethod xptr-term-map-location
           ((*xptr-term* xptr-relative-term)
            (*xptr-predecessor* standard-object)
            collector)
  (with-slots (quantifier) *xptr-term*
    (let ((i 0))
      (xptr-term-map-successors *xptr-term*
                                *xptr-predecessor*
                                #'(lambda (element)
                                    (when (xptr-element-match? *xptr-term*
                                                               element)
                                      (when (and (numberp quantifier)
                                                 (> (incf i) quantifier))
                                        (return-from xptr-term-map-location))
                                      (funcall collector element)))))))



(defMethod xptr-map-location
           ((collector function) (xptr null) (datum standard-object)
            &optional path)
  (funcall collector (apply #'make-xptr-location datum path)))

(defMethod xptr-map-location
           (collector (xptr xptr) (datum standard-object) &optional path)
  (xptr-term-map-location (xptr.term xptr)
                          datum
                          #'(lambda (term _datum)
                              (declare (ignore term))
                              (xptr-map-location collector
                                                 (xptr.rest xptr)
                                                 _datum
                                                 (list* (xptr.term xptr)
                                                        datum
                                                        path)))))

(defMethod xptr.value
           ((xptr xptr) (datum standard-object)
            &aux results)
  (xptr-map-location xptr datum #'(lambda (l)
                                    (push (xptr-location-value l) results)))
  (nreverse results))
