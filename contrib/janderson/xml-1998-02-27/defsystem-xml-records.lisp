;;; -*- mode: lisp; package "CL-USER" -*-

(in-package :cl-user)

(require :xml)
(unless (find-package "CLOS")
  (defPackage "CLOS" (:use "CCL" "COMMON-LISP")))
#|
 these files constitute a utility for treating mac-ptr records as if they were
 xml-elements. the purpose it to offer an analogous interface to data
 which dumped from a datbase and data serialized from the net.
 |#
(let ((*default-pathname-defaults* #+:mcl (if *load-truename*
                                            (directory-namestring *load-truename*)
                                            *default-pathname-defaults*)))
  (declare (special *default-pathname-defaults*))
  (map nil #'cl::load
       '("defClass-macro"
         "mcl-mop-package"
         "mcl-mop"
         "macptr-access"
         "xml-record-node"
         )))
