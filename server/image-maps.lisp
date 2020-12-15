;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-

;;; (C) Copyright 1995, Christopher R. Vincent
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; IMAGE-MAPS
;;;

(in-package :http)

;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(declaim (inline make-point))

(defun make-point (x y)
  (make-instance 'point :x x :y y))

(declaim (inline make-rect))

(defun make-rectangle (p1 p2)
  (make-instance 'rectangle :point1 p1 :point2 p2))

(defun make-image-map (type &rest args)
  (apply #'make-instance (ecase type
                           (:ncsa 'ncsa-image-map)
                           (:cern 'cern-image-map)
                           (:computed 'computed-image-map))
         args))

(declaim (inline make-point))

(defun make-circle (p r)
  (let* ((x (x-of p))
         (y (y-of p))
         (p1 (make-point (- x r) (- y r)))
         (p2 (make-point (+ x r) (+ y r))))
    (make-instance 'circle :point1 p1 :point2 p2 :center p :radius r)))

(defun make-oval (p1 p2)
  (make-instance 'oval :point1 p1 :point2 p2))

(defun make-polygon (points)
  (loop for i in points
        minimize (x-of i) into x-min
        maximize (x-of i) into x-max
        minimize (y-of i) into y-min
        maximize (y-of i) into y-max fixnum
        finally (return (make-instance 'polygon
                                       :point1 (make-point x-min y-min)
                                       :point2 (make-point x-max y-max)
                                       :point-list points))))

;;;------------------------------------------------------------------- 
;;;
;;; PRINT METHODS
;;;

(defmethod print-object ((region region) stream)
  (with-slots (bounding-shape) region
    (print-unreadable-object (region stream :type t :identity t)
      (when (slot-boundp region 'bounding-shape)
        (print-object bounding-shape stream)))))

(defmethod print-object ((point point) stream)
  (with-slots (x y) point
    (print-unreadable-object (point stream :type t :identity t)
      (when (and (slot-boundp point 'x)
                 (slot-boundp point 'y))
        (format stream "~D, ~D" x y)))))

(defmethod print-object ((shape shape) stream)
  (with-slots (point1 point2) shape
    (print-unreadable-object (shape stream :type t :identity t)
      (when (and (slot-boundp shape 'point1)
                 (slot-boundp shape 'point2))
        (format stream "~S, ~S" point1 point2)))))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(defmacro with-point-coordinates ((point1 point2) &body body)
  `(let ((x1 (slot-value ,point1 'x))
         (y1 (slot-value ,point1 'y))
         (x2 (slot-value ,point2 'x))
         (y2 (slot-value ,point2 'y)))
     (declare (integer x1 y1 x2 y2))
     . ,body))

(define-generic point-coordinates (point)
  (declare (values x y)))

(defmethod point-coordinates ((point point))
  (with-slots (x y) point
    (values x y)))

(defmethod distance-from ((p1 point) x1 y1)
  (declare (integer x1 y1))
  (with-slots (x y) p1
    (sqrt (+ (expt (- x1 x) 2)
             (expt (- y1 y) 2)))))

(defmethod distance-between ((p1 point) (p2 point))
  (with-point-coordinates (p1 p2)
    (sqrt (+ (expt (- x1 x2) 2)
             (expt (- y2 y1) 2)))))

(defmethod point-equal-p ((p1 point) (p2 point))
  (and (= (x-of p1) (x-of p2))
       (= (y-of p1) (y-of p2))))

(define-generic shape-hit-p (test-shape x y)
  (:documentation "True if the point defined by x and y is within a given shape."))

(defmethod shape-hit-p ((test-rect rectangle) x y) 
  (declare (ignore x y)) t)

(defmethod shape-hit-p ((test-circ circle) x y)
  (with-slots (center) test-circ
    (> (radius test-circ) (distance-from center x y))))

;; how much floating point arithmatic to do?
(defmethod shape-hit-p ((test-oval oval) x y)
  (with-point-coordinates ((point1 test-oval) (point2 test-oval))
    (let* ((height (the integer (- y2 y1)))
           (scaler (the single-float (coerce (/ height (the integer (- x2 x1))) 'single-float)))
           (radius (the single-float (coerce (/ height 2) 'single-float))))
      (> radius (sqrt (+ (expt (- (* scaler x) (+ (* scaler x1) radius)) 2)
                         (expt (- y (+ y1 radius)) 2)))))))

(defmethod shape-hit-p ((test-poly polygon) x y)
                                                ; Extend horizontal line to the right of point, count crosses.
  (loop with crosses = 0
        with points = (point-list test-poly)
        for v1 in points
        for v2 in (cdr points)
        do (with-point-coordinates (v1 v2)
             (cond ((eq (>= y1 y) (>= y2 y)) nil)
                                                ; Simple case
                   ((and (>= x1 x) (>= x2 x))
                    (setq crosses (1+ crosses)))
                                                ; Calculate slope, find x crossing
                   ((> (+ (* (- y y1)
                             (/ (- x1 x2)
                                (- y1 y2)))
                          x1)
                       x)
                    (setq crosses (1+ crosses)))))
        finally (return (oddp crosses))))

;; test the bounding rectangle for better performance.
(defmethod shape-hit-p :around ((test-shape shape) x y)
  (with-point-coordinates ((point1 test-shape) (point2 test-shape))
    (cond ((or (and (>= x x1) (<= x x2) (>= y y1) (<= y y2))
               (and (>= x x2) (<= x x1) (>= y y2) (<= y y1)))
           (call-next-method))
          (t nil))))

(defun get-region-hit (region-list x y)
  (loop for i in region-list
        when (shape-hit-p (bounding-shape i) x y)
          return (destination i)
        finally (return nil)))

(defun get-points-hit (proximity-list x y)
  (cond (proximity-list
         (loop for i in proximity-list
               with closest-url = (destination (car proximity-list))
               with min-distance = (distance-from (bounding-shape (car proximity-list)) x y)
               for i-distance = (distance-from (bounding-shape i) x y)
               when (< i-distance min-distance)
                 do (progn (setq closest-url (destination i))
                           (setq min-distance i-distance))
               finally (return closest-url)))
        (t nil)))

(define-generic get-image-map-hit (test-map x y)
  (:documentation "Given an instance of image-map-data and x,y coords, should always return a URL."))

(defmethod get-image-map-hit ((test-map cern-image-map) x y)
  (with-slots (region-list) test-map
    (or (get-region-hit region-list x y)
        (im-url-default test-map))))

(defmethod get-image-map-hit ((test-map ncsa-image-map) x y)
  (with-slots (region-list proximity-list) test-map
    (or (get-region-hit region-list x y)
        (get-points-hit proximity-list x y)
        (im-url-default test-map))))

;; abstract up sometime.
(defmethod get-image-map-hit ((test-map computed-image-map) x y)
  (with-slots (region-list proximity-list) test-map
    (or (get-region-hit region-list x y)
        (get-points-hit proximity-list x y)
        (im-url-default test-map))))

;;;------------------------------------------------------------------- 
;;;
;;; PARSING UTILITIES
;;;

;; This needs to return an exported url because it is too messy to
;; arrange on the-fly exports from an image map object.   9/2/95 -- JCMa.
(defun parse-destination (raw-text)
  (flet ((intern-exported-url (destination)
           (let* ((url-string (%merge-url destination (local-context)))
                  (url (intern-url url-string :if-does-not-exist :create)))
             (cond
               ((not (url:local-url-p url)))    ;remote url. 
               ((and url (translation-method url)))     ; Proceed if already exported.
               ((and *auto-export* (setq url (auto-export-pathname-url url-string))))
               (t (error "The destination URL, ~A, was not already exported and could not be auto-exported."
                         url-string)))
             url)))
    (declare (inline intern-exported-url))
    (let ((destination (string-left-trim "0123456789,() " raw-text)))
      (if (null-string-p destination)
          nil
          (intern-exported-url destination)))))

(defun parse-default (line)
  (flet ((trim-default (x)
           (let* ((len (length x))
                  (start-key (search "def" line :test #'char-equal :start1 0 :end1 3 :start2 0 :end2 len))
                  (pos1 (or (position-if #'white-space-char-p x :start start-key :end len) 0))
                  (pos2 (position-if-not #'white-space-char-p x :start pos1 :end len)))
             (if (zerop pos1)
                 x
                 (subseq x pos2 len)))))
    (declare (dynamic-extent #'trim-default))
    (parse-destination (trim-default line))))

(defun error-checked-default (default alternate pathname)
  (unless (or default alternate)
    (error "Default URL must be set in map file ~S." pathname))
  default)

;;;------------------------------------------------------------------- 
;;;
;;; PARSE CERN FORMAT
;;;

(defun parse-cern-coords (raw-coords)
  (loop with x and y and start and coord-list
        for i from 0 to (- (length raw-coords) 1)
        for first-char = (char raw-coords i)
        do (case first-char
             (#\( (setq start (+ i 1)))
             (#\, (setq x (parse-integer raw-coords :start start :end i))
              (setq start (+ i 1)))
             (#\) (setq y (parse-integer raw-coords :start start :end i))
              (setq coord-list (cons (make-point x y) coord-list))))
        finally (return coord-list)))

(defmacro with-parsed-cern-line ((key line &key (url-var 'url-string) (raw-coordinates-var 'raw-coordinates))
                                 &body body)
  `(let* ((key-len (length ,key))
          (line-len (length ,line))
          (start (string-search ,key ,line 0 key-len 0 line-len))
          (end-url (position-if-not #'white-space-char-p ,line :start start :end line-len :from-end t))
          (end-coord (position-if #'white-space-char-p ,line :start start :end end-url :from-end t))
          (,url-var (subseq ,line (1+ end-coord) (1+ end-url)))
          (,raw-coordinates-var (subseq ,line start end-coord)))
     (declare (fixnum key-len line-len start end-url end-coord))
     ,@body))

(defun parse-cern-rectangle (line)
  (with-parsed-cern-line ("rect" line)
    (destructuring-bind (first . rest) (parse-cern-coords raw-coordinates)
      (make-instance 'region
                     :destination (parse-destination url-string)
                     :bounding-shape (make-rectangle first (car rest))))))

(defun parse-cern-circle (line)
  (with-parsed-cern-line ("circ" line)
    (let ((start-radius (1+ (the fixnum (char-position #\) line start line-len)))))
      (make-instance 'region
                     :destination (parse-destination url-string)
                     :bounding-shape (make-circle (car (parse-cern-coords raw-coordinates))
                                                  (parse-integer line :start start-radius :junk-allowed t))))))

(defun parse-cern-polygon (line)
  (with-parsed-cern-line ("poly" line)
    (let* ((point-list (parse-cern-coords raw-coordinates))
           (last (last point-list)))
      (unless (point-equal-p (first point-list) (first last))
        (setf last `(,.last ,(first point-list))))
      (make-instance 'region
                     :destination (parse-destination url-string)
                     :bounding-shape (make-polygon point-list)))))

(defun parse-cern-image-map-file (pathname &aux (keyword-pkg *keyword-package*))
  "Parses an CERN format file from pathname and returns an image map object."
  (flet ((tag-keyword (line &aux (start (position-if-not #'white-space-char-p line)))                   
           (symbolize (subseq line start (+ start 3)) keyword-pkg)))
    (declare (inline tag-keyword))
    (with-open-file (file-stream pathname :direction :input)
      (loop with default
            for line = (read-line file-stream nil nil)
            while line
            for tag = (cond ((> (length line) 3) (tag-keyword line))
                            (t nil))
            for item = (case tag
                         (:def (parse-default line))
                         (:rec (parse-cern-rectangle line))
                         (:cir (parse-cern-circle line))
                         (:pol (parse-cern-polygon line))
                         (otherwise nil))
            when (eq tag :def)
              do (setq default item)
            else when item collect item into regions
            finally (return (make-image-map :cern
                                            :url-default (error-checked-default default nil pathname)
                                            :region-list regions))))))


;;;------------------------------------------------------------------- 
;;;
;;; PARSE NCSA FORMAT
;;;

(defun parse-ncsa-coords (raw-coords)
  (setq raw-coords (concatenate 'string " " raw-coords))
  (loop with x and y and coord-list
        for i from 1 to (1- (length raw-coords))
        for first-char = (char raw-coords i)
        do (case first-char
             (#\, (setq y (parse-integer raw-coords :start (1+ i) :junk-allowed t))
              (setq coord-list (append coord-list (list (make-point x y)))))
             (#\space)
             (otherwise (if (eql (char raw-coords (1- i)) #\space)
                            (setq x (parse-integer raw-coords :start i :junk-allowed t)))))
        finally (return coord-list)))

(defmacro with-parsed-ncsa-line ((key line &key (url-var 'url-string) (raw-coordinates-var 'raw-coordinates))
                                 &body body)
  `(let* ((key-len (length ,key))
          (line-len (length ,line))
          (start (search ,key ,line :test #'char-equal :start1 0 :end1 key-len :start2 0 :end2 line-len))
          (end-tag (position-if #'white-space-char-p ,line :start (the fixnum (+ start key-len)) :start start :end line-len))
          (start-url (position-if-not #'white-space-char-p ,line :start end-tag :end line-len))
          (end-url (position-if #'white-space-char-p ,line :start start-url :end line-len))
          (,url-var (subseq ,line start-url end-url))
          (,raw-coordinates-var (subseq ,line end-url line-len)))
     (declare (fixnum line-len start end-tag start-url end-url key-len))
     ,@body))

(defun parse-ncsa-point (line)
  (with-parsed-ncsa-line ("point" line)
    (make-instance 'region
                   :destination (parse-destination url-string)
                   :bounding-shape (car (parse-ncsa-coords raw-coordinates)))))

(defun parse-ncsa-rectangle (line)
  (with-parsed-ncsa-line ("rect" line)
    (destructuring-bind (first . rest) (parse-ncsa-coords raw-coordinates)
      (make-instance 'region
                     :destination (parse-destination url-string)
                     :bounding-shape (make-rectangle first (car rest))))))

(defun parse-ncsa-circle (line)
  (with-parsed-ncsa-line ("circ" line)
    (destructuring-bind (first . rest) (parse-ncsa-coords raw-coordinates)
      (make-instance 'region
                     :destination (parse-destination url-string)
                     :bounding-shape (make-circle first (round (distance-between first (car rest))))))))

(defun parse-ncsa-oval (line)
  (with-parsed-ncsa-line ("oval" line)
    (destructuring-bind (first . rest) (parse-ncsa-coords raw-coordinates)
      (make-instance 'region
                     :destination (parse-destination url-string)
                     :bounding-shape (make-oval first (car rest))))))

(defun parse-ncsa-polygon (line)
  (with-parsed-ncsa-line ("poly" line)
    (let* ((point-list (parse-ncsa-coords raw-coordinates))
           (last (last point-list)))
      (unless (point-equal-p (first point-list) (first last))
        (setf last `(,.last ,(first point-list))))
      (make-instance 'region
                     :destination (parse-destination url-string)
                     :bounding-shape (make-polygon point-list)))))

(defun parse-ncsa-image-map-file (pathname &aux (keyword-pkg *keyword-package*))
  "Parses an NCSA format file from pathname and returns an image map object."
  (flet ((tag-keyword (line &aux (start (position-if-not #'white-space-char-p line)))                   
           (symbolize (subseq line start (+ start 3)) keyword-pkg)))
    (declare (inline tag-keyword))
    (with-open-file (file-stream pathname :direction :input)
      (loop with default
            for line = (read-line file-stream nil nil)
            while line
            for tag = (cond ((> (length line) 3) (tag-keyword line))
                            (t nil))
            for item = (case tag
                         (:def (parse-default line))
                         (:rec (parse-ncsa-rectangle line))
                         (:cir (parse-ncsa-circle line))
                         (:pol (parse-ncsa-polygon line))
                         (:ova (parse-ncsa-oval line))
                         (:poi (parse-ncsa-point line))
                         (otherwise nil))
            when (eq tag :def)
              do (setq default item)
            else unless (or (null item) (eq tag :poi))
                   collect item into regions
            else unless (null item) collect item into points
            finally (return (make-image-map :ncsa
                                            :url-default (error-checked-default default points pathname)
                                            :region-list regions
                                            :proximity-list points))))))

;;;------------------------------------------------------------------- 
;;;
;;; PARSE MAP FILES
;;;

(define-generic parse-map-file (pathname method)
  (:documentation "Parses a map file of given pathname and format to an instance of image-map-data."))

(defmethod parse-image-map (pathname (method (eql :cern)))
  (parse-cern-image-map-file pathname))

(defmethod parse-image-map (pathname (method (eql :ncsa)))
  (parse-ncsa-image-map-file pathname))

(defmethod parse-image-map (map-data (method (eql :computed)))
  (declare (ignore map-data)))
