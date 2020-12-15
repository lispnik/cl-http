;;;-*- Mode: Lisp; Package: ccl -*-


#|
On Fri, 9 Aug 1996, Christopher Vincent wrote:

>
> Is the following a bug in MCL?  CL-HTTP has been losing on the mac
> recently because it tries to use strings with fill-pointers for
> generic dispatch on string.
>
> -christopher
>


There's a comment in the source code for this case which says "WRONG !!!".
I guess it wasn't saying it loud enough ...

|#

(in-package "CCL")

(defvar *general-vector-class* (find-class 'general-vector))

(defvar *ivector-vector-classes*
  (vector (find-class 'short-float-vector)
          (find-class 'unsigned-long-vector)
          (find-class 'long-vector)
          (find-class 'unsigned-byte-vector)
          (find-class 'byte-vector)
          (find-class 'base-string)
          (find-class 'extended-string)
          (find-class 'unsigned-word-vector)
          (find-class 'word-vector)
          (find-class 'double-float-vector)
          (find-class 'bit-vector)))


(setf (%svref *ppc-class-table* ppc::subtag-vectorH)
      #'(lambda (v)
          (let* ((subtype (%array-header-subtype v)))
            (declare (fixnum subtype))
            (if (eql subtype ppc::subtag-simple-vector)
              *general-vector-class*
              (%svref *ivector-vector-classes*
                      (ash (the fixnum (- subtype ppc::min-cl-ivector-subtag))
                           (- ppc::ntagbits)))))))
