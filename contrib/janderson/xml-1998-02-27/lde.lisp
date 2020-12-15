(in-package :cl-user)

;;; here several auxiliary / utility files. we have them always loaded
;;; they'r ehere as much so that you can decide if you want to use them or
;;; to supplant them, as to actually load them.
;;;
;;; this presumes mcl 4.1

(pushnew #p"entwicklung-server:source:lisp:" *module-search-path*
         :test #'equalp)
(defpackage :clos)
(require :clos-utilities)
;#-:mcl-mop (require 'mcl-mop-package)
;(require 'mcl-mop)
(require :macptr-access)                ; for extended accessors to pointer data
;(require :identity-class)
;(require :defclassmacro)

