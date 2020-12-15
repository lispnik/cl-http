;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;; ODBC module for MCL, LWW and ACL/Windows
;; Version 0.85
;; Copyright (C) Paul Meurer 1999. All rights reserved.
;; paul.meurer@hit.uib.no
;;
;; Documentation and the license agreement can be found in file 
;; "sql-odbc-documentation.lisp".
;; Bug reports and suggestions are highly welcome.

(in-package :common-lisp-user)

(defpackage "ODBC"
  (:use "SQL" "COMMON-LISP" #+mcl "CCL" "FFC")
  (:import-from "SQL"
                "DB-DISCONNECT" "DB-COMMIT" "DB-ROLLBACK" 
                "DB-OPEN-QUERY" "DB-CLOSE-QUERY" "DB-CANCEL-QUERY"
                "DB-QUERY" "DB-FETCH-QUERY-RESULTS" 
                "DB-MAP-QUERY" "DB-MAP-BIND-QUERY"
                "DB-EXECUTE-COMMAND" "DB-EXECUTE-PARAMETERIZED"
                "DB-PREPARE-STATEMENT" 
                ;"DB-QUERY-PARAMETERIZED"
                "DB-QUERY-OBJECT"
                "DB-DESCRIBE-COLUMNS"
                "SQL-EXPRESSION"
                "SQL-STREAM"
                ;;"SQL-CHUNK-STREAM"
                "MAKE-SQL-STREAM"
                ;;"CHUNK-LENGTH"
                ;;"DB-OPEN-STREAM" ;"DB-CLOSE-STREAM"
                ;;"DB-READ-CHUNK" "DB-WRITE-CHUNK" "NEXT-CHUNK"
                "COLUMN"
                "%DB-BIND-EXECUTE" "%DB-RESET-QUERY"
                "OPEN-SQL-STREAM"
                "SQL-STRING-STREAM"
                "FUNDAMENTAL-INPUT-STREAM"
                "FUNDAMENTAL-CHARACTER-STREAM"
                "FUNDAMENTAL-OUTPUT-STREAM"
                "STREAM-READ-CHAR"
                "STREAM-PEEK-CHAR"
                "STREAM-READ-SEQUENCE"
                "STREAM-WRITE-SEQUENCE"
                
                "BUFFER" "BUFFER-SIZE" "BUFFER-POSITION"

                "%DB-EXECUTE" ; "%PREPARE-QUERY-VARS"
                "%INITIALIZE-QUERY" "QUERIES" "QUERY-ACTIVE-P"
                ;;"%DISPOSE-QUERY-VARS"
                "%READ-QUERY-DATA"
                "COLUMN-NAMES"
                "*BINARY-FORMAT*" "DB-DATA-SOURCES" ))
