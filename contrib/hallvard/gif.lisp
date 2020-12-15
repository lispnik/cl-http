;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: common-lisp-user; Base: 10 -*-
;;;
;;; Lempel-Ziv compression
;;;
;;; Hallvard Traetteberg
;;; Sect. of Information Systems, SINTEF Tele og Data
;;; P.O.Box 124 Blindern, N-0314 Oslo, Norway
;;; Tlf: +47 2206 7983 or +47 2206 7300, Fax: +47 2206 7350
;;; Email: Hallvard.Tretteberg@informatics.sintef.no


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defpackage :gif
  (:use #-CL-HTTP "COMMON-LISP" #+CL-HTTP "FUTURE-COMMON-LISP"))

(in-package :gif)

#|
(macrolet ((*comtab*-set-insert-char-key (mod key char)
             `(comtab-set-key *comtab* '(,mod ,key)
                              #'(lambda (ignore) (declare (ignore ignore))
                                        (ed-insert-char (front-window) ,char)))))
  (*comtab*-set-insert-char-key :meta #\/ #\\)
  (*comtab*-set-insert-char-key :meta #\z #\')
  (*comtab*-set-insert-char-key :control #\/ #\|))
|#

;;; optimization

(declaim (optimize (safety 1) (speed 3)))


;;;------------------------------------------------------------------- 
;;;
;;; CODE STREAM STUFF
;;;

(defconstant +stream-byte-size+ 8)

(defstruct (code-stream
             (:print-function (lambda (code-stream stream level)
                                (declare (ignore level))
                                (print-unreadable-object (code-stream stream :type t :identity t)))))
  (binary-stream nil)
  (buf 0)
  (buf-size 0))

(defun open-code-stream (thing direction)
  (let ((stream (cond ((or (stringp thing) (pathnamep thing))
                       (open thing :direction direction :element-type `(unsigned-byte ,+stream-byte-size+)
                             :if-exists :rename-and-delete :if-does-not-exist :create))
                      ((numberp thing)
                       (make-array thing :fill-pointer 0 :element-type `(unsigned-byte ,+stream-byte-size+))))))
    (make-code-stream :binary-stream stream)))

(defun flush-code-stream (stream)
  (let ((binary-stream (code-stream-binary-stream stream))
        (code (code-stream-buf stream)))
    (cond ((streamp binary-stream) (write-byte code binary-stream))
          ((vectorp binary-stream) (vector-push code binary-stream)))))

(defun close-code-stream (stream)
  (let ((binary-stream (code-stream-binary-stream stream)))
    (flush-code-stream stream)
    (when (streamp binary-stream)
      (close binary-stream))))

(defmacro with-open-code-stream ((stream-var . open-args) &body body)
  `(let ((,stream-var (open-code-stream . ,open-args)))
     (unwind-protect (progn . ,body)
       (close-code-stream ,stream-var))))

(defun write-code (code size stream)
  (declare (optimize (safety 0) (speed 3)))
  (let ((buf (code-stream-buf stream))
        (buf-size (code-stream-buf-size stream))
        (binary-stream (code-stream-binary-stream stream)))
    (declare (fixnum code size buf buf-size))
    (when (and (eql size +stream-byte-size+) (zerop buf-size))
      (cond ((streamp binary-stream) (write-byte code binary-stream))
            ((vectorp binary-stream) (vector-push code binary-stream)))
      (return-from write-code code))
    (setf (ldb (byte size buf-size) buf) code)
    (incf buf-size size)
    (loop
      (when (< buf-size +stream-byte-size+)
        (return))
      (let ((code (ldb (byte +stream-byte-size+ 0) buf)))
        (cond ((streamp binary-stream) (write-byte code binary-stream))
              ((vectorp binary-stream) (vector-push code binary-stream))))
      (setf buf (ash buf (- +stream-byte-size+)))
      (decf buf-size +stream-byte-size+))
    (setf (code-stream-buf stream) buf
          (code-stream-buf-size stream) buf-size))
  (values code))

(defun flush-code-stream-buffer (code-buffer max-size code-stream)
  (let ((code-vector (code-stream-binary-stream code-buffer)))
    (unless (or (plusp max-size)
                (zerop (code-stream-buf-size code-buffer)))
      (vector-push (code-stream-buf code-buffer) code-vector))
    (unless (< (length code-vector) max-size)
      (dotimes (i (write-code (length code-vector) 8 code-stream))
        (write-code (aref code-vector i) 8 code-stream))
      (setf (fill-pointer code-vector) 0))))

(defun read-code (size stream)
  (declare (optimize (safety 0) (speed 3)))
  (let ((buf (code-stream-buf stream))
        (buf-size (code-stream-buf-size stream)))
    (declare (fixnum size buf buf-size))
    (when (and (eql size +stream-byte-size+) (zerop buf-size))
      (return-from read-code (read-byte (code-stream-binary-stream stream) nil nil)))
    (loop
      (unless (< buf-size size)
        (decf buf-size size)
        (setf (code-stream-buf stream) (ash buf (- size))
              (code-stream-buf-size stream) buf-size)
        (return (ldb (byte size 0) buf)))
      (let ((byte (read-byte (code-stream-binary-stream stream) nil nil)))
        (cond (byte (setf (ldb (byte +stream-byte-size+ buf-size) buf) byte))
              ((zerop buf-size) (return nil))
              (t (setf buf-size 0)
                 (return buf))))
      (incf buf-size +stream-byte-size+))))


;;;------------------------------------------------------------------- 
;;;
;;; STRING TABLE STUFF
;;;

(defconstant +max-code-size+ 12)
(defconstant +max-string-table-size+ (ash 1 +max-code-size+))

(defstruct (string-table
             (:constructor make-string-table
              (&optional (vector (make-array +max-string-table-size+ :fill-pointer 0 :adjustable t))
                         (hash (make-hash-table :test #'eql :size +max-string-table-size+))))
             (:print-function (lambda (table stream level)
                                (declare (ignore level))
                                (print-unreadable-object (table stream :type t :identity t)))))
  (vector nil)
  (hash nil))

(defun string-table-code-size (table)
  (integer-length (1- (length (string-table-vector table)))))

(declaim (inline make-string-table-code))
(defun make-string-table-code (table-string byte)
  (declare (fixnum table-string byte)
           (optimize (safety 0) (speed 3)))
  (the fixnum (+ table-string (the fixnum (ash byte +max-code-size+)))))

(defun add-table-string (table-string byte table)
  (let* ((vector (string-table-vector table))
         (hash (string-table-hash table))
         (index (vector-push (make-string-table-code table-string byte) vector)))
    (declare (fixnum table-string byte)
             (optimize (safety 0) (speed 3)))
    (setf (gethash (make-string-table-code table-string byte) hash) index)
    (values index)))

(defun set-table-root (root table)
  (let ((vector (string-table-vector table)))
    (declare (fixnum root)
             (optimize (safety 0) (speed 3)))
    (setf (aref vector root) (make-string-table-code root root)))
  (values root))

(defun initialise-string-table (code-size table)
  (let* ((code-size (if (< code-size 2) 2 code-size))
         (alphabet-size (ash 1 code-size)))
    (declare (fixnum code-size alphabet-size)
             (optimize (safety 0) (speed 3)))
    (dotimes (i alphabet-size)
      (set-table-root i table))
;; add special gif-codes
    (set-table-root alphabet-size table)
    (set-table-root (1+ alphabet-size) table)
    (setf (fill-pointer (string-table-vector table)) (+ alphabet-size 2)))
  (values table))

(defun clear-table-code (code-size) (ash 1 (if (< code-size 2) 2 code-size)))
(defun end-of-codes-code (code-size) (1+ (clear-table-code code-size)))

(defun find-table-string (table-string byte table)
  (declare (fixnum table-string byte)
           (optimize (safety 0) (speed 3)))
  (gethash (make-string-table-code table-string byte) (string-table-hash table) nil))

(defun find-table-root (root table)
  (declare (ignore table)
           (fixnum root)
           (optimize (safety 0) (speed 3)))
  (values root))

(defun first-table-string-byte (table-string table)
  (let ((vector (string-table-vector table))
        next-table-string)
    (loop
      (unless (eq next-table-string table-string)
        (return (aref vector (1+ next-table-string))))
      (setf next-table-string table-string
            table-string (aref vector table-string)))))

(defun write-table-string (table-string code-size code-stream table)
  (declare (fixnum table-string code-size)
           (optimize (safety 0) (speed 3)))
  (let* ((vector (string-table-vector table))
         (next-table-string (aref vector table-string)))
    (declare (fixnum next-table-string))
    (unless (eq table-string next-table-string)
      (write-table-string next-table-string code-size code-stream table))
    (write-code (aref vector (1+ table-string)) code-size code-stream)))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defun compress (image code-stream)
  (let* ((byte-size (image-color-depth image))
         (table (initialise-string-table byte-size (make-string-table)))
         (code-size (string-table-code-size table))
         (raster (local-image-raster image))
         (code (row-major-aref raster 0))
         (table-string (find-table-root code table))
         (code-buffer (open-code-stream 255 :output)))
    (write-code (clear-table-code byte-size) code-size code-buffer)
    (dotimes (i (1- (array-total-size raster)))
      (setf code (row-major-aref raster (1+ i)))
      (let ((next-table-string (find-table-string table-string code table)))
        (unless next-table-string
          (add-table-string table-string code table)
          (write-code table-string code-size code-buffer)
          (flush-code-stream-buffer code-buffer 250 code-stream)
          (let ((next-code-size (string-table-code-size table)))
            (declare (fixnum next-code-size))
            (cond ((> next-code-size +max-code-size+)
                   (initialise-string-table byte-size table)
                   (write-code (clear-table-code byte-size) code-size code-buffer)
                   (setf code-size (string-table-code-size table)))
                  ((> next-code-size code-size)
                   (setf code-size next-code-size))))
          (setf next-table-string (find-table-root code table)))
        (setf table-string next-table-string)))
    (write-code table-string code-size code-buffer)
    (write-code (end-of-codes-code byte-size) code-size code-buffer)
    (flush-code-stream-buffer code-buffer 0 code-stream)
    (values table)))


;;;------------------------------------------------------------------- 
;;;
;;; BASIC FUNCTION FOR WRITING GIF BLOCK TO THE GIF-STREAM
;;;

(defmethod write-block ((block cons) byte-stream)
  (write-code (car block) (cdr block) byte-stream))

(defmethod write-block ((block character) byte-stream)
  (write-code (char-code block) 8 byte-stream))

(defmethod write-block ((block fixnum) byte-stream)
  (write-code block 8 byte-stream))

(defmethod write-block ((block string) byte-stream)
  (loop for char across block
        do (write-code (char-code char) 8 byte-stream)))

(defmethod write-block ((block array) byte-stream)
  (dotimes (i (array-total-size block))
    (write-code (aref block i) 8 byte-stream)))

(defconstant +data-sub-block-size+ (1- (ash 1 8)))

(defmethod write-blocks (blocks byte-stream)
  (if (listp blocks)
      (mapc #'(lambda (block) (write-block block byte-stream)) blocks)
      (write-block blocks byte-stream)))

(defun write-data-block (vector byte-stream)
  (let ((length (length vector)))
    (unless (zerop length)
      (write-code (mod length +data-sub-block-size+) 8 byte-stream))
    (dotimes (i length)
      (when (and (zerop (mod (- length i) +data-sub-block-size+)) (plusp i))
        (write-code +data-sub-block-size+ 8 byte-stream))
      (let ((code (aref vector i)))
        (when (characterp code)
          (setf code (char-code code)))
        (write-code code 8 byte-stream)))
    (write-code 0 8 byte-stream)))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defstruct block
  (label nil))

(defmethod write-block ((block block) byte-stream)
  (declare (ignore byte-stream)))

(defmethod write-block :before ((block block) byte-stream)
  (let ((label (block-label block)))
    (if (numberp label)
        (write-code label 8 byte-stream)
        (write-blocks label byte-stream))))

(defstruct (image (:include block (label #\,)))
  (width 0)
  (height 0)
  (color-depth 8)
  (color-map nil))

(defstruct (local-image
             (:include image)
             (:constructor make-image (x y width height &optional (color-depth nil)
                                         (color-map (and color-depth (make-color-map color-depth)))
                                         (raster (make-raster width height color-depth)))))
  (x 0)
  (y 0)
  (raster nil))

(defmethod write-block ((block local-image) byte-stream)
  (write-blocks `((,(local-image-x block) . 16) ; image x offset
                  (,(local-image-y block) . 16) ; image y offset
                  (,(image-width block) . 16)   ; image width
                  (,(image-height block) . 16)  ; image height
                  (,(1- (image-color-depth block)) . 3) ; pixel/byte - 1
                  (0 . 3)                       ; ignore
                  (0 . 1)                       ; sequential order
                  (,(if (image-color-map block) 1 0) . 1)       ; no local color map
                  )
                byte-stream)
  (when (image-color-map block)
    (write-block (image-color-map block) byte-stream))
  (let ((code-size (image-color-depth block)))
    (write-code (if (< code-size 2) 2 code-size) 8 byte-stream))
  (compress block byte-stream)
  (write-code 0 8 byte-stream))

(defstruct (gif (:include image))
  (background-color 0)
  (blocks nil))

(defmethod write-screen-descriptor ((block gif) byte-stream)
  (declare (ignore byte-stream)))

(defmethod write-block ((block gif) byte-stream)
;; logical screen descriptor
  (write-screen-descriptor block byte-stream)
  (write-block (image-color-map block) byte-stream)
  (mapc #'(lambda (image) (write-block image byte-stream)) (gif-blocks block))
  (write-block #\; byte-stream)                 ; gif terminator
  )

(defun write-gif (gif filename)
  (with-open-code-stream (byte-stream filename :output)
                         (write-block gif byte-stream))
  (values gif))

(defstruct (gif-87a (:include gif (label "GIF87a"))))

(defmethod write-screen-descriptor ((gif gif-87a) byte-stream)
  (write-blocks `((,(image-width gif) . 16)     ; screen width
                  (,(image-height gif) . 16)    ; screen height
                  (,(1- (image-color-depth gif)) . 3)   ; pixel/byte - 1
                  (0 . 1)                       ; future use
                  (,(1- (image-color-depth gif)) . 3)   ; color resolution - 1
                  (,(if (image-color-map gif) 1 0) . 1) ; color map follows
                  (,(gif-background-color gif) . 8)     ; background color
                  (0 . 8)                       ; separator
                  )
                byte-stream))

(defstruct (gif-89a (:include gif-87a (label "GIF89a"))))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defun make-color-map (color-depth)
  (make-array (* (ash 1 color-depth) 3) :fill-pointer 0 :initial-element 0
              :element-type `(unsigned-byte 8)))

(defun make-raster (width height color-depth)
  (make-array (list height width) :initial-element 0
              :element-type `(unsigned-byte ,color-depth)))

(defun create-gif (standard width height &optional (color-depth 8) (background-color 0)
                            (color-map (make-color-map color-depth))
                            (blocks (list (make-image 0 0 width height color-depth nil))))
  (let* ((maker (case standard
                  ((87 :87a) (values #'make-gif-87a))
                  ((89 :89a) (values #'make-gif-89a))
                  (t (error "Unrecognized gif standard: ~a" standard))))
         (gif (funcall maker :width width :height height :color-depth color-depth :color-map color-map
                       :blocks blocks :background-color background-color)))
    (values gif)))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defstruct (extension (:include block (label #\!))))

;(defmethod write-block :after ((block extension) byte-stream)
; (write-code 0 8 byte-stream))

(defstruct (graphic-control (:include extension (label '(#\! #xF9))))
  (transparent-color nil)
  (disposal-method nil)
  (delay-time 0))

(defmethod write-block ((block graphic-control) byte-stream)
  (write-blocks `((,4 . 8)                      ; block-size
                  (,(if (graphic-control-transparent-color block) 1 0) . 1)     ; image has transparent color?
                  (0 . 1)                       ; user input?
                  (,(case (graphic-control-disposal-method block)
                      ((nil) 0)
                      (:none 1)
                      (:background 2)
                      (:previous 3)
                      (t 0)) . 3)
                  (0 . 3)                       ; reserved
                  (,(graphic-control-delay-time block) . 16)
                  (,(or (graphic-control-transparent-color block) 0) . 8)
                  (0 . 8))
                byte-stream))

;;;

(defstruct (comment (:include extension (label '(#\! #xFE))))
  (string ""))

(defmethod write-block ((block comment) byte-stream)
  (write-data-block (comment-string block) byte-stream))

;;;

(defstruct (plain-text (:include extension (label '(#\! #x01)))))

(defmethod write-block ((block plain-text) byte-stream)
  (write-blocks `((12 . 8)                      ; block-size
                  (0 . ,(* 8 12)))              ; the rest is empty, i.e. NYI
                byte-stream)
  (write-data-block #() byte-stream))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defstruct (application (:include extension (label '(#\! #xFF))))
  (identifier nil))

(defmethod write-application-data ((block application) byte-stream)
  (declare (ignore byte-stream)))

(defmethod write-block ((block application) byte-stream)
  (write-blocks `((11 . 8))                     ; block-size
                byte-stream)
  (write-block (application-identifier block) byte-stream)
  (write-application-data block byte-stream))

(defstruct (netscape (:include application (identifier "NETSCAPE2.0")))
  (loop-count 0))

(defmethod write-application-data ((block netscape) byte-stream)
  (write-blocks `((3 . 8)                       ; block-size
                  (1 . 8)                       ; ???
                  (,(netscape-loop-count block) . 16)   ; loop count
                  (0 . 8))
                byte-stream))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

#|
(defparameter cm (make-color-map 8))
(add-color cm 255 0 0)
(add-color cm 0 255 0)
(add-color cm 0 0 255)
;(find-color g 1 255 0 #'color-distance)

(defparameter ra (make-raster 50 50 8))
(draw-rectangle ra 15 15 30 30 2)
(draw-rectangle ra 5 5 25 25 1)
(draw-rectangle ra 20 20 5 5 0)
;(draw-rectangle ra 45 45 25 25 1)
;(draw-rectangle ra 55 55 30 30 2)
;(draw-rectangle ra 60 60 5 5 0)

(defparameter blocks (list
                       (make-netscape :loop-count 0)
                       (make-comment :string "Made by Hallvard, upper left corner")
                       (make-graphic-control :delay-time 100)
                       (make-image 0 0 50 50 8 nil ra)

                       (make-comment :string "Made by Hallvard, lower right corner")
                       (make-graphic-control :delay-time 100)
                       (make-image 50 50 50 50 8 nil ra)))

(defparameter g (create-gif 89 100 100 8 0 cm blocks))
(time (write-gif g "test.gif"))
(untrace write-block)
|#

(defun default-image-fun (image image-fun)
  (let ((gif image))
    (when (gif-p gif)
      (setf image (car (gif-blocks image))))
    (when (image-p image)
      (setf image (funcall image-fun image)))
    (unless image
      (setf image (funcall image-fun gif))))
  (values image))

(defun add-color (color-map red green blue)
  (let* ((color-map (default-image-fun color-map #'image-color-map))
         (index (length color-map)))
    (vector-push red color-map) 
    (vector-push green color-map) 
    (vector-push blue color-map)
    (values (truncate index 3))))

(defun color-equal (r1 g1 b1 r2 g2 b2)
  (and (eql r1 r2) (eql g1 g2) (eql b1 b2)))

(defun color-distance (r1 g1 b1 r2 g2 b2)
  (let ((dr (- r1 r2)) (dg (- g1 g2)) (db (- b1 b2)))
    (+ (* dr dr) (* dg dg) (* db db))))

(defun find-color (color-map red green blue &optional (match-fun #'color-equal))
  (let ((color-map (default-image-fun color-map #'image-color-map))
        (min-i nil) (min-val nil))
    (dotimes (i (truncate (length color-map)) min-i)
      (let* ((j (* i 3))
             (val (funcall match-fun red green blue 
                           (aref color-map j) (aref color-map (1+ j)) (aref color-map (+ j 2)))))
        (cond ((or (eq val t) (eql val 0))
               (return i))
              ((and (numberp val) (<= val (or min-val val)))
               (setf min-val val min-i i)))))))

(defun draw-rectangle (image x y width height color)
  (declare (fixnum x y width height color)
           (optimize (safety 0) (speed 3)))
  (when (gif-p image)
    (setf image (car (gif-blocks image))))
  (when (image-p image)
    (setf image (local-image-raster image)))
  (dotimes (dy height)
    (declare (fixnum dy))
    (dotimes (dx width)
      (declare (fixnum dx))
      (setf (aref image (the fixnum (+ y dy)) (the fixnum (+ x dx))) color))))
