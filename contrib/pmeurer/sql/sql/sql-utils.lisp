;;;-*- Mode: Lisp; Package: SQL -*-

;; Portable SQL module
;; Version 0.86
;; Copyright (C) Paul Meurer 1999. All rights reserved.
;; paul.meurer@hit.uib.no

;; Documentation and the license agreement can be found in file 
;; "sql-odbc-documentation.lisp".
;; Bug reports and suggestions are highly welcome.

;; Status: Experimental

(in-package :sql)

(enable-sql-reader-syntax)

;(setf *default-database* (car *connected-databases*))

(defgeneric db-object-info (database object))

(defun object-info (object)
  (db-object-info *default-database* object))

(defun object-exists-p (object)
  (not (null (object-info object))))

(defmethod db-object-info ((database oracle-mixin) object)
  (select [table-name] [table-type] [owner] :from [all-catalog]
          :where (with-slots (exp1 exp2 exp3) object
                   (if exp2
                       [and [= [table-name] (sql exp2)]
                            [= [owner] (sql exp1)]]
                     [= [table-name] (sql exp1)]))))
