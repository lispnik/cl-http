;;-*- package: (CCL) -*- 

;; additional macptr access functions
;; 19960000 ...
;; 19971218 move to package CCL

(in-package :CCL)

(export '(%put-bytes
          %put-fstring
          %put-vector
          %put-ntstring
          %get-bytes
          %get-fstring
          %get-vector
          %get-ntstring))


#|<H3>Utilities:</H3>
 string accessors with optional trailing null
 |#

(defun %put-bytes
       (ptr byte offset size)
  (dotimes (i size)
    (%put-byte ptr byte (+ offset i))))

(defun %get-fstring                     ; get a fixed length string
       (ptr offset size
        &aux (end (+ offset size))
             (string (make-string size :initial-element #\space))
             (byte 0))
  (do ((i offset (1+ i))) ((>= i end) string)
    (setf byte (%get-byte ptr i))
    (setf (elt string (- i offset)) (code-char byte))))

(defun %get-vector                     ; get a fixed vector
       (ptr offset size
        &aux (end (+ offset size))
             (vector (make-array size :initial-element #x00)))
  (do ((i offset (1+ i))) ((>= i end) vector)
    (setf (elt vector (- i offset)) (%get-byte ptr i))))

(defun %get-ntstring                     ; get a string with OPTIONAL null term.
       (ptr offset size
        &aux (end (+ offset size))
             (string (make-array size :element-type 'character :fill-pointer 0))
             (byte 0))
  (do ((i offset (1+ i))) ((>= i end) string)
    (setf byte (%get-byte ptr i))
    (when (zerop byte) (return string))
    (vector-push (code-char byte) string)))

(defun %put-fstring                     ; put a fixed length string
       (ptr string offset size
        &aux (end (+ offset (min (length string) size))))
  (when (> (length string) size)
    (error "string too large for field"))
  (do ((i offset (1+ i))) ((>= i end))
    (%put-byte ptr (char-code (elt string (- i offset))) i))
  (when (< (length string) size)
    (%put-bytes ptr #.(char-code #\space) end (- size (length string))))
  string)

(defun %put-vector                     ; put a fixed vector
       (ptr vector offset size
        &aux (end (+ offset (min (length vector) size))))
  (when (> (length vector) size)
    (error "vector too large for field"))
  (do ((i offset (1+ i))) ((>= i end))
    (%put-byte ptr (elt vector (- i offset)) i))
  (when (< (length vector) size)
    (%put-bytes ptr #x00 end (- size (length vector))))
  vector)

(defun %put-ntstring                     ; put a string with OPTIONAL null term.
       (ptr string offset size
        &aux (end (+ offset (min (length string) size))))
  (when (> (length string) size)
    (error "string too large for field"))
  (do ((i offset (1+ i))) ((>= i end))
    (%put-byte ptr (char-code (elt string (- i offset))) i))
  (when (< (length string) size)
    (%put-byte ptr 0 end))
  string)


;(trace %put-fstring)
;(trace %put-bytes)
;(untrace)

(provide :macptr-access)
