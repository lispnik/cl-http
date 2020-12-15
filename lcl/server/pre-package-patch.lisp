;;;; PRE-PACKAGE-PATCH.LISP
;;;

(in-package "CL-USER")

;;; Allow package-name as argument to package-nicknames
(defadvice (package-nicknames string-arg) (package)
  (if (packagep package)
      (advice-continue package)
    (advice-continue (find-package package))))
    

