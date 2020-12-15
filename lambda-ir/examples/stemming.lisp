;;;   -*- Mode: LISP; Package: lambda-ir; BASE: 10; SYNTAX: ansi-common-lisp -*-

;;; (C) Copyright 1997, Andrew J. Blumberg (blumberg@ai.mit.edu).
;;;     All Rights Reserved.
;;;
;;;
;;;------------------------------------------------------------------- 
;;;
;;; Stemming Code (based on pascal code written by Mark Sanderson)
;;;

(defparameter *suffix-list-2* '(("ational" "ate")
                                ("ization" "ize")
                                ("isation" "ize")
                                ("iveness" "ive")
                                ("fulness" "ful")
                                ("ousness" "ous")
                                ("biliti" "ble")
                                ("tional" "tion")
                                ("entli" "ent")
                                ("ousli" "ous")
                                ("ation" "ate")
                                ("alism" "al")
                                ("aliti" "al")
                                ("iviti" "ive")
                                ("ator" "ate")
                                ("enci" "ence")
                                ("anci" "ance")
                                ("izer" "ize")
                                ("iser" "ize")
                                ("abli" "able")
                                ("alli" "al")
                                ("eli" "e")))

(defparameter *suffix-list-3* '(("icate" "ic")
                                ("ative" "")
                                ("alize" "al")
                                ("alise" "al")
                                ("iciti" "ic")
                                ("ical" "ic")
                                ("ness" "")
                                ("ful" "")))

(defparameter *suffix-list-4* '(("ement" "")
                                ("ance" "")
                                ("ence" "")
                                ("able" "")
                                ("ible" "")
                                ("ment" "")
                                ("sion" "")
                                ("tion" "")
                                ("ant" "")
                                ("ent" "")
                                ("ism" "")
                                ("ate" "")
                                ("iti" "")
                                ("ous" "")
                                ("ive" "")
                                ("ize" "")
                                ("ise" "")
                                ("al" "")
                                ("er" "")
                                ("ic" "")
                                ("ou" "")))

(defparameter *words-not-to-stem* '("a"          "about"        "above"        "across"
                                    "after"        "afterwards"   "again"        "against"
                                    "all"          "almost"       "alone"        "along"
                                    "already"      "also"         "although"     "always"
                                    "am"         "among"        "amongst"      "amoungst"
                                    "amount"       "an"         "and"          "another"
                                    "any"          "anyhow"       "anyone"       "anything"
                                    "anyway"       "anywhere"     "are"          "around"
                                    "as"         "at"         "back"       "be"
                                    "became"       "because"      "become"       "becomes"
                                    "becoming"     "been"         "before"       "beforehand"
                                    "behind"       "being"        "below"        "beside"
                                    "besides"      "between"      "beyond"       "bill"
                                    "both"         "bottom"       "but"          "by"
                                    "call"       "can"          "cannot"       "cant"
                                    "co"         "computer"   "con"          "could"
                                    "couldnt"      "cry"          "de"         "describe"
                                    "detail"     "do"         "done"         "down"
                                    "due"          "during"       "each"         "eg"
                                    "eight"        "either"       "eleven"       "else"
                                    "elsewhere"    "empty"        "enough"       "etc"
                                    "even"         "ever"         "every"        "everyone"
                                    "everything"   "everywhere"   "except"       "few"
                                    "fifteen"      "fify"         "fill"         "find"
                                    "fire"       "first"        "five"         "for"
                                    "former"       "formerly"     "forty"        "found"
                                    "four"         "from"         "front"        "full"
                                    "further"      "get"          "give"         "go"
                                    "had"          "has"          "hasnt"      "have"
                                    "he"         "hence"        "her"          "here"
                                    "hereafter"    "hereby"       "herein"       "hereupon"
                                    "hers"         "herself"      "him"          "himself"
                                    "his"          "how"          "however"      "hundred"
                                    "i"            "ie"           "if"           "in"
                                    "inc"          "indeed"       "interest"     "into"
                                    "is"           "it"         "its"          "itself"
                                    "keep"         "last"         "latter"       "latterly"
                                    "least"        "less"         "ltd"          "made"
                                    "many"         "may"          "me"         "meanwhile"
                                    "might"        "mill"       "mine"         "more"
                                    "moreover"     "most"         "mostly"       "move"
                                    "much"         "must"         "my"         "myself"
                                    "name"       "namely"       "neither"      "never"
                                    "nevertheless" "next"         "nine"         "no"
                                    "nobody"       "none"         "noone"        "nor"
                                    "not"          "nothing"      "now"          "nowhere"
                                    "of"         "off"          "often"        "on"
                                    "once"         "one"          "only"         "onto"
                                    "or"         "other"        "others"       "otherwise"
                                    "our"          "ours"         "ourselves"    "out"
                                    "over"         "own"          "part"         "per"
                                    "perhaps"      "please"       "put"          "rather"
                                    "re"         "same"         "see"          "seem"
                                    "seemed"       "seeming"      "seems"        "serious"
                                    "several"      "she"          "should"       "show"
                                    "side"         "since"        "sincere"      "six"
                                    "sixty"        "so"         "some"         "somehow"
                                    "someone"      "something"    "sometime"     "sometimes"
                                    "somewhere"    "still"        "such"         "system"
                                    "take"         "ten"          "than"         "that"
                                    "the"          "their"        "them"         "themselves"
                                    "then"         "thence"       "there"        "thereafter"
                                    "thereby"      "therefore"    "therein"      "thereupon"
                                    "these"        "they"         "thick"        "thin"
                                    "third"        "this"         "those"        "though"
                                    "three"        "through"      "throughout"   "thru"
                                    "thus"         "to"         "together"     "too"
                                    "top"          "toward"       "towards"      "twelve"
                                    "twenty"       "two"          "un"         "under"
                                    "until"        "up"         "upon"         "us"
                                    "very"         "via"          "was"          "we"
                                    "well"         "were"         "what"         "whatever"
                                    "when"         "whence"       "whenever"     "where"
                                    "whereafter"   "whereas"      "whereby"      "wherein"
                                    "whereupon"    "wherever"     "whether"      "which"
                                    "while"        "whither"      "who"          "whoever"
                                    "whole"        "whom"         "whose"        "why"
                                    "will"         "with"         "within"       "without"
                                    "would"        "yet"          "you"          "your"
                                    "yours"        "yourself"     "yourselves"
                                    ))

(defun build-find-suffix-table (suffix-list)
  (let ((output-table (make-hash-table)))
    (loop for (suffix result) in suffix-list
          do (setf (gethash (aref suffix (1- (length suffix))) output-table)
                   (append (gethash (aref suffix (1- (length suffix))) output-table)
                           (list (list suffix result)))))
    output-table))

(defparameter *suffix-table-2* (build-find-suffix-table *suffix-list-2*))
(defparameter *suffix-table-3* (build-find-suffix-table *suffix-list-3*))
(defparameter *suffix-table-4* (build-find-suffix-table *suffix-list-4*))
(defparameter *words-not-to-stem-table* (make-hash-table :size (length *words-not-to-stem*) :test 'equalp))

(loop for item in *words-not-to-stem*
      do (setf (gethash item *words-not-to-stem-table*) t))

(defgeneric find-suffix (suffix-storage-device string &optional index)
  (:documentation ""))

(defmethod find-suffix ((suffix-storage-device hash-table) string &optional (index (1- (fill-pointer string))))
  (find-suffix (gethash (aref string index) suffix-storage-device) string (1- index)))

(defmethod find-suffix ((suffix-storage-device (eql nil)) string &optional index)
  (declare (ignore index string))
  nil)

(defmethod find-suffix ((suffix-storage-device list) string &optional index)
  (declare (ignore index))
  (loop for (suffix result) in suffix-storage-device
        with length = (fill-pointer string)
        with suffix-length = (length suffix)
        for predicate = (and (> length suffix-length)
                             (string-equal string suffix :start1 (- length suffix-length)))
        until predicate
        finally (when predicate
                  (return (list suffix result)))))

(defun measure (string &optional (length (fill-pointer string)))
  (flet ((internal-vowel-p (idx)
           (vowel-p string idx))
         (pos-if-not (function start)
           (loop for idx upfrom start below length
                 until (not (funcall function idx))
                 finally (return idx)))
         (pos-if (function start)
           (loop for idx upfrom start below length
                 until (funcall function idx)
                 finally (return idx))))
    (declare (dynamic-extent #'internal-vowel-p #'pos-if-not #'pos-if))
    (loop with idx = 0
          with count = 0
          while (< idx length)
          do (if (> idx 0) (incf count))
          do (setf idx (pos-if-not #'internal-vowel-p (pos-if #'internal-vowel-p idx)))
          finally (return count))))

(defun vowel-p (string index)
  (let ((character (aref string index)))
    (or (member character '(#\a #\e #\i #\o #\u))
        (and (> index 1)
             (not (member (aref string (1- index)) '(#\a #\e #\i #\o #\u)))
             (char-equal character #\y)))))
  
(defun contains-vowel-p (string &optional (length (fill-pointer string)))
  (loop for idx upfrom 0 below length
        thereis (vowel-p string idx)))

(defun has-suffix (string suffix)
  (let ((difference (- (fill-pointer string) (length suffix))))
    (when (> difference -1)
      (string-equal string suffix :start1 difference))))

(defun cvc (string &optional (length (fill-pointer string)))
  (if (>= length 3)
      (if (and (not (vowel-p string (- length 3)))
               (vowel-p string (- length 2))
               (not (vowel-p string (- length 1)))
               (not (or (char-equal (aref string (- length 1))
                                    #\w)
                        (char-equal (aref string (- length 2))
                                    #\y))))
          t)))

(defun handle-trailing-s (string)
  (let ((length (fill-pointer string)))
    (if (char-equal #\s (aref string (1- length)))
        (cond ((string-equal string "sses" :start1 (- length 4))
               (decf (fill-pointer string) 2))
              ((string-equal string "ies" :start1 (- length 3))
               (decf (fill-pointer string) 2))
              ((not (char-equal #\s (aref string (- length 2))))
               (decf (fill-pointer string) 1))
              (t nil)))))

(defun handle-trailing-y (string)
  (let ((length (fill-pointer string)))
    (when (and (char-equal (aref string (1- length)) #\y)
               (contains-vowel-p string))
      (setf (aref string (1- length)) #\i))))

(defun handle-ed-ing (string decrement)
  (when (contains-vowel-p string (- (fill-pointer string) decrement))
    (decf (fill-pointer string) decrement)
    (if (or (has-suffix string "at")
            (has-suffix string "bl")
            (has-suffix string "iz"))
        (vector-push-extend #\e string)
        (if (> (fill-pointer string) 2)
            (let* ((index (1- (fill-pointer string)))
                   (char (aref string index)))
              (if (and (char-equal char (aref string (1- index)))
                       (not (or (char-equal char #\s)
                                (char-equal char #\z)
                                (char-equal char #\l))))
                  (decf (fill-pointer string))))
            (if (and (= (measure string) 1)
                     (cvc string))
                (vector-push-extend #\e string))))))

(defun handle-verb-endings (string &optional (length (fill-pointer string)))
  (if (and (has-suffix string "eed")
           (> (measure string (- length 3)) 0))
      (decf (fill-pointer string))
      (cond ((has-suffix string "ed")
             (handle-ed-ing string 2))
            ((has-suffix string "ing")
             (handle-ed-ing string 3))
            (t nil))))

(defun step1 (string)
  (handle-trailing-s string)
  (handle-verb-endings string)
  (handle-trailing-y string))
  
(defun step234 (string suffixes test-value)
  (let ((result (find-suffix suffixes string)))
    (when result
      (if (> (measure string (- (fill-pointer string) (length (car result))))
             test-value)
          (progn
            (decf (fill-pointer string) (length (car result)))
            (loop for item across (cadr result)
                  do (vector-push-extend item string)))))))

(defun step5 (string &optional (length (fill-pointer string)))
  (when (char-equal (aref string (1- length)) #\e)
    (let ((measure (measure string length)))
      (cond ((> measure 1) (decf (fill-pointer string)))
            ((and (= measure 1) 
                  (cvc string (1- length)))
             (decf (fill-pointer string))))))
  (setf length (1- (fill-pointer string)))
  (when (and (char-equal (aref string length) #\l)
             (char-equal (aref string (1- length)) #\l)
             (> (measure string) 1))
    (decf (fill-pointer string))))

(defun stem (string)
  (unless (or (< (length string) 4) (gethash string *words-not-to-stem-table*))
    (let ((use-string (make-array (length string) :adjustable t :fill-pointer t :element-type http::*standard-character-type*)))
      (loop for item across string 
            for idx upfrom 0
            do (setf (aref use-string idx) item))
      (if (> (length string) 2)
          (step1 use-string))
      (step234 use-string *suffix-table-2* 0)
      (step234 use-string *suffix-table-3* 0)
      (step234 use-string *suffix-table-4* 1)
      (step5 use-string)
      (return-from stem use-string)))
  string)

