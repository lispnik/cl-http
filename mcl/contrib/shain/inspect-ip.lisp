;;; -*- Syntax: Ansi-Common-Lisp; Package: inspector; Base: 10; Mode: lisp -*-
;;; (C) Copyright 1997, Digitool.
;;;     All Rights Reserved.
;;;

;; You may want to make this available to MCL CL-HTTP users; it adds an "IP Number"
;; line to integer value inspector windows.

(in-package "INSPECTOR")

; Bump count for IP address line
(defmethod compute-line-count ((num integer)) 
  (let ((res 14))
    (unless (< 0 num 4000) (decf res))   ; not a roman number
    (unless (<= 0 num 255) (decf res))   ; not a character
    (unless (<= #x-1000 (ash num -16) #xfff) 
      (decf res))                       ; not a point
    res))

; Add IP address line
(defmethod line-n ((num integer) n)
  (if (and (>= n 7) (not (< 0 num 4000))) (incf n))   ; maybe skip roman.
  (if (and (>= n 8) (not (<= 0 num 255))) (incf n))   ; maybe skip character.
  (if (and (>= n 9) (not (<= #x-1000 (ash num -16) #xfff)))
               (incf n))                ; maybe skip point
  (let* ((type :static)
         (neg? (< num 0))
         (norm (if neg? 
                 (+ num (expt 2 (max 32 (* 4 (round (+ (integer-length num) 4) 4)))))
                 num)))
    (ecase n
      (0  (values num
                (if (fixnump num)
                  "Fixnum:      "
                  "Bignum:      ")
                type "~s"))
      (1  (values (float num)
                  "Scientific:  " type
                  (if (< num 0) "~8,2e" "~7,2e")))
      (2  (values (if (zerop num) "illegal" (log num 2)) 
                  "Log base 2:  " type "~d"))
      (3  (values norm
                  "Binary:      " type
                  (if neg? "#b...~b" "#b~b")))
      (4  (values norm
                  "Octal:       " type
                  (if neg? "#o...~o" "#o~o")))
      (5  (values num
                  "Decimal:     " type "~d."))
      (6  (values norm
                  "Hex:         " type
                  (if neg? "#x...~x" "#x~x")))
      (7  (values (format nil "~@r" num)
                  "Roman:       " type "~a"))
      (8  (values (code-char num)
                  "Character:   " type "~s"))
      (9  (values (point-string num)
                  "As a point:  " type "~a"))
      (10 (values (ccl::ensure-simple-string (prin1-to-string num))
                  "Abbreviated: "
                  type #'format-abbreviated-string))
      (11 (values (universal-time-string num)
                  "As time:     " type "~a"))
      (12 (if (< num 0)
            (values most-negative-fixnum 'most-negative-fixnum type '("~d." t))
            (values most-positive-fixnum 'most-positive-fixnum type '("~d." t))))
      (13 (values (ccl::tcp-addr-to-str num)
                  "As IP address: " type "~A")))))
