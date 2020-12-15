;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: (vrml-test use (common-lisp vrml1.0)) -*-
;;;------------------------------------------------------------------- 
;;;
;;; VRML1.0 EXAMPLES
;;;
;;; Copyright 1996, Rainer Joswig.
;;; All rights reserved.
;;;
;;; Author: Rainer Joswig, joswig@lavielle.com
;;; 
;;; Purpose: Examples for use with the VRML package distributed with CL-HTTP.


;;;------------------------------------------------------------------- 
;;;
;;; PACKAGE DECLARATIONS
;;;
;;;
;;; This example package just uses the Common Lisp package and the VRML1.0
;;; package.
;;;
;;; The VRML1.0 package exports the functionality to create VRML version 1.0
;;; conforming output.

(cl:defpackage "VRML-TEST"
  (:use "COMMON-LISP" "VRML1.0")
  (:nicknames "VT"))

(cl:in-package :vrml-test)


;;;------------------------------------------------------------------- 
;;;
;;; UTILITY FUNCTIONS
;;;
;;; This is just a shorthand to write the examples to files.

(defun write-vrml-file (function pathname)
  "Write the output of FUNCTION to a new file called PATHNAME. The function
takes one argument, the output stream."
  (with-open-file (out pathname :direction :output :if-exists :supersede)
    (funcall function out)))


;;;------------------------------------------------------------------- 
;;;
;;; EXAMPLE 1: SIMPLE SCENE
;;;

(defun simple-scene (out-stream)
  (with-vrml-world (:stream out-stream)
    (with-separator-group (out-stream)
      (translation-node* out-stream 0.1 0.7 -8)
      (cube-node out-stream :width 1.1))))

(let ((file (pathname "http:examples;vrml;scene1.wrl")))
  (unless (probe-file file)
    (write-vrml-file #'simple-scene file)))

(http:export-url #u"/cl-http/vrml/scene1.wrl"
                 :vrml-world
                 :pathname "http:examples;vrml;scene1.wrl"
                 :documentation "Scene 1: A simple scene with a cube."
                 :keywords '(:cl-http :demo :vrml))

;;;------------------------------------------------------------------- 
;;;
;;; EXAMPLE 2: COLORED SIMPLE SCENE
;;;

(defun colored-simple-scene (out-stream)
  (with-vrml-world (:stream out-stream)
    (with-separator-group (out-stream)
      (let-fields ((color color :red 1.0 :green 0.5 :blue 0.1))
        (material-node out-stream :diffuse-color color))
      (translation-node* out-stream 0.1 0.7 -8)
      (cylinder-node out-stream :radius 3.1))))

(let ((file (pathname "http:examples;vrml;scene2.wrl")))
  (unless (probe-file file)
    (write-vrml-file #'colored-simple-scene file)))

(http:export-url #u"/cl-http/vrml/scene2.wrl"
                 :vrml-world
                 :pathname "http:examples;vrml;scene2.wrl"
                 :documentation "Scene 2: A colored simple scene with a cylinder."
                 :keywords '(:cl-http :demo :vrml))

(defun colored-simple-scene-fn (url out-stream)
  (http:with-conditional-get-response (out-stream :vrml :expires (url:expiration-universal-time url))
    (colored-simple-scene out-stream)))

(http:export-url #u"/cl-http/vrml/scene2-fn.wrl"
                 :computed
                 :response-function 'colored-simple-scene-fn
                 :documentation "Scene 2-fn: A colored simple scene with a cylinder.
Exported as a function."
                 :keywords '(:cl-http :demo :vrml))


;;;------------------------------------------------------------------- 
;;;
;;; EXAMPLE 3: CUBES
;;;
;;; Create an a array of cubes.
;;;
;;; This is really a simple example. It could be extended with all kinds
;;; of fancy stuff: axis, descriptions for cubes, etc.
;;; It should use some different scaling.

(defun create-example-array (i-dimension j-dimension max)
  "Creates a float 2d array with random values."
  (let ((array (make-array (list i-dimension j-dimension)
                           :element-type 'float)))
    (loop for i from 0 below i-dimension
          do (loop for j from 0 below j-dimension
                   do (setf (aref array i j)
                            (* 2 (random max)))))
    array))

(defun cube-array (stream array &key (cube-size 0.6) inserter)
  "Displays the contents of a 2d array as a rectangular collection of cubes.
INSERTER is a function with two arguments:
 a) a function that outputs a cube
 b) the output stream
INSERTER can be used to output additional information for each cube. It is
an example how to use Functional Programming to make our operations
extensible."
  (flet ((default-inserter (fun stream)
           (funcall fun stream)))
    (unless inserter
      (setf inserter #'default-inserter))
    (with-separator-group (stream)
      (loop for i from 0 below (array-dimension array 0)
            for x from 0.0 by 1.0
            do (loop for j from 0 below (array-dimension array 1)
                     and z from 0.0 by 1.0
                     for element = (aref array i j)
                     do (with-separator-group (stream)
                          (funcall inserter
                                   #'(lambda (stream)
                                       (translation-node* stream x (/ element 2) z)
                                       (cube-node stream
                                                  :width cube-size
                                                  :depth cube-size
                                                  :height element))
                                   stream)))))))

(defun example3 (stream)
  "Output a 5x4 array of float values as rectangular arrangement of cubes."
  (with-vrml-world (:stream stream)
    (cube-array stream
                (create-example-array 5 4 (random 3.0)))))

(let ((file (pathname "http:examples;vrml;scene3.wrl")))
  (unless (probe-file file)
    (write-vrml-file #'example3 file)))

(http:export-url #u"/cl-http/vrml/scene3.wrl"
                 :vrml-world
                 :pathname "http:examples;vrml;scene3.wrl"
                 :documentation "An 5x4 array of float values as rectangular arrangement of cubes."
                 :keywords '(:cl-http :demo :vrml))

(defun example3a (stream)
  "Similar to example3. Shows the use of the inserter parameter.
Changes the color of each cube."
  (flet ((random-inserter (fun stream)
           (let-fields ((color color
                               :red (random 1.0)
                               :green (random 1.0)
                               :blue (random 1.0)))
             (material-node stream :diffuse-color color)
             (funcall fun stream))))
    (with-vrml-world (:stream stream)
      (cube-array stream
                  (create-example-array 5 6 (random 3.0))
                  :inserter #'random-inserter))))

#|
(let ((file (pathname "http:examples;vrml;scene4.wrl")))
  (unless (probe-file file)
    (write-vrml-file #'example3a file)))

(http:export-url #u"/cl-http/vrml/scene4.wrl"
                 :vrml-world
                 :pathname "http:examples;vrml;scene4.wrl"
                 :documentation "An 5x4 array of float values as rectangular arrangement of cubes.
Randomly colored."
                 :keywords '(:cl-http :demo :vrml))
|#

(defun compute-example3a (url stream)
  (http:with-conditional-get-response (stream :vrml :expires (url:expiration-universal-time url))
    (example3a stream)))

(http:export-url #u"/cl-http/vrml/scene4.wrl"
                 :computed
                 :response-function #'compute-example3a
                 :documentation "An 5x4 array of float values as rectangular arrangement of cubes.
Randomly colored and exported as a function."
                 :keywords '(:cl-http :demo :vrml))


;;;------------------------------------------------------------------- 
;;;
;;; EXAMPLE 4: SURFACE
;;;
;;;
;;; This examples is from the area of scientific visualization.
;;; Similar (although more sophisticated) output usually is being
;;; created with software like Macsyma, Axiom or Matlab.
;;;
;;; The example surface is an complex double sine. The absolute value
;;; of the complex result is the height and the phase angle specifies
;;; the color.
;;;
;;; The example shows how to generate surfaces with triangles, where the
;;; triangles (being the faces) all may have different colors.

(defconstant 2pi (* pi 2)
  "Two times pi.")

(defconstant pi/3 (/ pi 3.0)
  "Pi divided by 3.0")

(defun create-array (fn xn x0 dx yn y0 dy
                        &key (element-type t) array)
  "Fill a 2d array with values from a function.
FN takes two arguments: x and y.
XN is the number of samples on the x-axis.
X0 is the start point on the x-axis.
DX is the step width on the x-axis.
YN is the number of samples on the y-axis.
Y0 is the start point on the y-axis.
DY is the step width on the y-axis.
ELEMENT-TYPE is the element-type of the array to create.
ARRAY is a default array. If ARRAY equals NIL, than a new array is being created."
  (unless (arrayp array)
    (setf array (make-array (list xn yn) :element-type element-type)))
  (loop for i fixnum from 0 below xn
        for x from x0 by dx
        do (loop for j fixnum from 0 below yn
                 for y from y0 by dy
                 do (setf (aref array i j)
                          (funcall fn x y))))
  array)

(defun array-to-point-list (array &key (key #'identity))
  "Creates a list of points. The points being the grid of the array.
KEY is a function which extracts the height from the array content.
It calls (ALLOCATE-RESOURCE 3D-FLOAT-VECTOR ...) to create the points."
  (loop with points = nil
        for i downfrom (1- (array-dimension array 0)) downto 0 
        and x downfrom (float (1- (array-dimension array 0))) by 1.0
        do (loop for j downfrom (1- (array-dimension array 1)) downto 0
                 and z downfrom (float (1- (array-dimension array 1))) by 1.0
                 do (push (allocate-resource 3d-float-vector
                                             (:x x :y (funcall key (aref array i j)) :z z))
                          points))
        finally (return points)))

(defun array-to-color-list (array key)
  "Creates a list of color values from the array.
KEY is a function which extracts the color from the array content."
  (loop with color-list = nil and i-dimension = (array-dimension array 0)
        and j-dimension = (array-dimension array 1)
        for i downfrom (1- i-dimension) above 0
        do (loop for j downfrom (1- j-dimension) above 0
                 for color = (funcall key (aref array i j))
                 do (push color color-list))
        finally (return color-list)))

(defun array-to-face-index-list (array)
  "Creates a list of face indices from the array. Faces are triangles."
  (loop with index-list = nil and i-dimension = (array-dimension array 0)
        and j-dimension = (array-dimension array 1)
        for i downfrom (1- i-dimension) above 0
        do (loop for j downfrom (1- j-dimension) above 0
                 for index = (+ (* i-dimension j) i)
                 do (push -1 index-list)
                    (push index index-list)
                    (push (1- index) index-list)
                    (push (- index i-dimension) index-list)
                    (push -1 index-list)
                    (push (1- index) index-list)
                    (push (- index i-dimension 1) index-list)
                    (push (- index i-dimension) index-list))
        finally (return index-list)))

(defun hsv->rgb (h s v)
  "Converts color from HSV to RGB.
H is between 0.0 and 2pi.
S and V are between 0.0 and 1.0.
Returns values for R, G and B.
Sure Genera has this already.
See: Computer Graphics, Principles and Practice, Second Edition in C,
     by Foley/van Dam/Feiner/Hughes, Section 13.3.4, The HSV Color Model."
  (assert (<= 0.0 h 2pi) (h))
  (assert (<= 0.0 s 1.0) (s))
  (assert (<= 0.0 v 1.0) (v))
  (if (zerop s)
      (values v v v)
      (let* ((i (truncate (setf h (/ (if (>= h 2pi) 0.0 h)
                                     pi/3))))
             (f (- h i)))
        (let ((p (* v (- 1.0 s)))
              (q (* v (- 1.0 (* s f))))
              (t1 (* v (- 1.0 (* s (- 1.0 f))))))
          (ecase i
            (0 (values v  t1 p))
            (1 (values q  v  p))
            (2 (values p  v  t1))
            (3 (values p  q  v))
            (4 (values t1 p  v))
            (5 (values v  p  q)))))))

(defun hsv->color (h s v)
  "Converts color from HSV to VRML RGB color."
  (multiple-value-bind (red green blue)
      (hsv->rgb h s v)
    (assert (and (<= 0.0 red 1.0) (<= 0.0 green 1.0) (<= 0.0 blue 1.0))
            (red green blue))
    (let ((r (coerce red 'single-float))
	  (g (coerce green 'single-float))
	  (b (coerce blue 'single-float)))
      (allocate-resource color (:red r :green g :blue b)))))

(defun surface-from-array (stream array height-key color-key)
  "Outputs a surface build by triangles to stream. Source for the data
is a 2d ARRAY.
HEIGHT-KEY extracts the height from the 2d array content.
COLOR-KEY extracts the color from the 2d array content."
  (with-separator-group (stream)
    (let ((points nil) (color-list nil))
      (unwind-protect
          (let ((index-list (array-to-face-index-list array))
                (color-index-list (loop for i from 0 below (* (1- (array-dimension array 0))
                                                              (1- (array-dimension array 1)))
                                        collect i
                                        collect i)))    ; output color for both triangles
            (setf points (array-to-point-list array :key height-key)
                  color-list (array-to-color-list array color-key))
            (material-binding-node stream :per-face-indexed)
            (material-node stream :diffuse-color color-list)
            (coordinate3-node stream points)
            (indexed-face-set-node stream
                                   :coordinate-index index-list
                                   :material-index color-index-list))
        (loop for point in points do (deallocate-resource '3d-float-vector point))
        (loop for color in color-list do (deallocate-resource 'color color))))))

(defun example4 (stream xn x0 dx yn y0 dy)
  "Outputs a surface generated by a double complex sine function."
  (with-vrml-world (:stream stream)
    (surface-from-array stream
                        (create-array #'(lambda (x y)
                                          (let ((c (sin (sin (complex x y)))))
                                            (list (min 20.0 (abs c)) (+ pi (phase c)))))
                                      xn x0 dx yn y0 dy)
                        #'first
                        #'(lambda (item) (hsv->color (second item) 0.5 0.5)))))

(defun example4a (stream)
  "Outputs a surface generated by a double complex sine function."
  (example4 stream 30 0.0 0.12 30 0.0 0.12))

#|(write-vrml-file #'example4a "http:examples;vrml;scene5.wrl")

(http:export-url #u"/cl-http/vrml/scene5.wrl"
                 :vrml-world
                 :pathname "http:examples;vrml;scene5.wrl"
                 :documentation "A surface generated by a double complex sine function"
                 :keywords '(:cl-http :demo :vrml))|#

(defun compute-example4a (url stream)
  (http:with-conditional-get-response (stream :vrml :expires (url:expiration-universal-time url))
    (example4a stream)))

(http:export-url #u"/cl-http/vrml/scene5.wrl"
                 :computed
                 :response-function #'compute-example4a
                 :documentation "A surface generated by a double complex sine function"
                 :keywords '(:cl-http :demo :vrml))

(defun compute-example4b (url stream query-alist)
  (http:bind-query-values (xn x0 x1 yn y0 y1)
                          (url query-alist)
    (handler-case (let* ((xn (www-present:accept-from-string '(integer 5 50) xn))
                         (x0 (www-present:accept-from-string '(float 0.0 5.0) x0))
                         (x1 (www-present:accept-from-string '(float 0.0 5.0) x1))
                         (yn (www-present:accept-from-string '(integer 5 50) yn))
                         (y0 (www-present:accept-from-string '(float 0.0 5.0) y0))
                         (y1 (www-present:accept-from-string '(float 0.0 5.0) y1)))
                    (assert (< x0 x1))
                    (assert (< y0 y1))
                    (let ((dx (float (/ (- x1 x0) xn)))
                          (dy (float (/ (- y1 y0) yn))))
                      (http:with-successful-response (stream :vrml :expires (url:expiration-universal-time url))
                        (example4 stream xn x0 dx yn y0 dy))))
      (condition (condition)
         (declare (ignore condition))
         (http:redirect-request http:*server* #u"/cl-http/vrml/vrml.html")))))

(http:export-url #u"/cl-http/vrml/scene5a.wrl"
                 :html-form
                 :pathname "http:www;cl-http;vrml;vrml.html"
                 :response-function #'compute-example4b
                 :documentation "A surface generated by a double complex sine function"
                 :keywords '(:cl-http :demo :vrml))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3d Life
;;;
;;; T.b.d.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example Tree
;;;
;;; T.b.d.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example  Rotation
;;;
;;; T.b.d.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example  Rooms
;;;
;;; T.b.d.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example 2 Plant
;;;
;;; T.b.d.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of File

