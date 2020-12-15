;;; -*- Mode: lisp; Syntax: Ansi-Common-Lisp; Base: 10; Package: (vrml1.0 :use (common-lisp www-utils)) -*-

;;; VRML1.0 - A PACKAGE FOR GENERATING VRML 1.0 MARKUP IN ASCII TEXT
;;;
;;;  Copyright 1996, Rainer Joswig.
;;;  All rights reserved.
;;;
;;;  Enhancements Copyright 1996, John C. Mallery.
;;;  All rights reserved.
;;;
;;;  
;;;  Comments and suggestions to: joswig@lavielle.com, bug-cl-http@ai.mit.edu

;;;  Written with MCL 3.1b2c3.

;;;  April, 6th 1996 : created
;;;  April, 7th 1996 : untested
;;;  April, 9th 1996 : read VRML1.0c SPEC ( http://vag.vrml.org/vrml10c.html ),
;;;                    added some textures stuff,
;;;                    missing: extensible nodes, isA fields, images
;;;  Massaged for CL-HTTP inclusion, LispM operation.   4/9/96 -- JCMa.
;;;  Documentation, release preparation, efficiency enhancements.   4/10/96 -- JCMa.
;;;  Jun, 28th 1996  : print-list: replaced WHEN by WHILE
;;;  Jul, 1th 1996   : cylinder-node: upcase Cylinder
;;;  Jul, 2th 1996   : added type declarations for defstruct slots
;;;                  : changed printer functions:
;;;                      print-list now uses write-string
;;;                      color field printing resolution reduced to 0.001

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage :vrml1.0
    (:use common-lisp www-utils)
    (:nicknames "VRML")
    (:shadow "ALLOCATE-RESOURCE" "DEALLOCATE-RESOURCE" "USING-RESOURCE")
    (:export
      "ALLOCATE-RESOURCE"
      "ASCII-TEXT-NODE"
      "COLOR"
      "COLOR-BLUE"
      "COLOR-GREEN"
      "COLOR-P"
      "COLOR-RED"
      "CONE-NODE"
      "COORDINATE3-NODE"
      "CUBE-NODE"
      "CYLINDER-NODE"
      "DEALLOCATE-RESOURCE"
      "DIRECTIONAL-LIGHT-NODE"
      "FONT-STYLE-NODE"
      "INDEXED-FACE-SET-NODE"
      "INDEXED-LINE-SET-NODE"
      "INFO-NODE"
      "LET-FIELDS"
      "MATERIAL-NODE"
      "MATERIAL-BINDING-NODE"
      "MATRIX-TRANSFORM-NODE"
      "NORMAL-NODE"
      "NORMAL-NODE*"
      "NORMAL-BINDING-NODE"
      "ORTHOGRAPHIC-CAMERA-NODE"
      "PERSPECTIVE-CAMERA-NODE"
      "POINT-LIGHT-NODE"
      "POINT-SET-NODE"
      "ROTATION"
      "ROTATION-NODE"
      "ROTATION-NODE*"
      "ROTATION-AMOUNT"
      "ROTATION-P"
      "ROTATION-X"
      "ROTATION-Y"
      "ROTATION-Z"
      "SCALE-NODE"
      "SCALE-NODE*"
      "SHAPE-HINTS-NODE"
      "SPHERE-NODE"
      "SPOT-LIGHT-NODE"
      "TEXTURE2-NODE"
      "TEXTURE2-COORDINATE2-NODE"
      "TEXTURE2-TRANSFORM-NODE"
      "TRANSFORM-NODE"
      "TRANSLATION-NODE"
      "TRANSLATION-NODE*"
      "USE"
      "2D-FLOAT-VECTOR"
      "2D-FLOAT-VECTOR-P"
      "2D-FLOAT-VECTOR-X"
      "2D-FLOAT-VECTOR-Y"
      "3D-FLOAT-VECTOR"
      "3D-FLOAT-VECTOR-P"
      "3D-FLOAT-VECTOR-X"
      "3D-FLOAT-VECTOR-Y"
      "3D-FLOAT-VECTOR-Z"
      "WITH-ANCHOR-GROUP"
      "WITH-DEFINITION"
      "WITH-GROUP"
      "WITH-LEVEL-OF-DETAIL-GROUP"
      "WITH-WWW-INLINE-GROUP"
      "WITH-SEPARATOR-GROUP"
      "WITH-SWITCH-GROUP"
      "WITH-TRANSFORM-SEPARATOR-GROUP"
      "WITH-VRML-WORLD"))

  )                                             ;close eval-when

(in-package :vrml1.0)

(pushnew :vrml1.0 *features*)                   ;advise lisp about the module.


;;;------------------------------------------------------------------- 
;;;
;;; CHARATERS
;;;

(defconstant *white-space-characters* '(#\space #\tab #\Return #\newline))

(defconstant *invalid-node-name-characters* `(#\space #\' #\` #\" #\/ #\{ #\} #\# #\+ #\.))

(define valid-node-name-p (name) 
  (www-utils:with-fast-array-references ((string name))
    (when (digit-char-p (aref string 0))
      (return-from valid-node-name-p nil))
    (loop for idx upfrom 0 below (length string)
          for char = (aref string idx)
          unless (or (member char *invalid-node-name-characters* :test #'eql)
                     (char-bits char))
            return nil
          finally (return t))))

(deftype node-name ()
  '(and string (satisfies valid-node-name-p)))

;; some vrml float can also be integers.  4/10/96 -- JCMa.
(deftype float-or-integer (&optional (low '*) (high '*))
  `(or (single-float ,low ,high)
       (integer ,low ,high)))

(define-macro deftype-set-or-all (name set)
  "Define a part set type."
  (let ((pred-name (intern (concatenate 'string (symbol-name name) "-TYPE-P"))))
    `(progn
       (define ,pred-name (list)
         (every #'(lambda (item) (member item ',set)) list))
       (deftype ,name ()
         '(or (eql :all)
              (member ,@set)
              (and list
                   (satisfies ,pred-name)))))))


;;;------------------------------------------------------------------- 
;;;
;;; FIELDS
;;;

(defstruct color
  (red 0.0 :type (single-float 0.0 1.0))
  (green 0.0 :type (single-float 0.0 1.0))
  (blue 0.0 :type (single-float 0.0 1.0)))

(setf (documentation 'color 'structure)
      "Fields containing one (SFColor) or zero or more (MFColor) RGB colors.
Each color is written to file as an RGB triple of floating point numbers in
ANSI C floating point format, in the range 0.0 to 1.0. For example:

[ 1.0 0. 0.0, 0 1 0, 0 0 1 ]

is an MFColor field containing the three colors red, green, and blue.")

(defstruct rotation
  (x 0.0 :type float-or-integer)
  (y 0.0 :type float-or-integer)
  (z 0.0 :type float-or-integer)
  (amount 0.0 :type float-or-integer))

(setf (documentation 'rotation 'structure)
      "A field containing an arbitrary rotation. SFRotations are written to
file as four floating point values separated by whitespace. The 4 values
represent an axis of rotation followed by the amount of right-handed rotation
about that axis, in radians. For example, a 180 degree rotation about the Y
axis is:

0 1 0  3.14159265")

(defstruct 2d-float-vector
  (x 0.0 :type float-or-integer)
  (y 0.0 :type float-or-integer))

(setf (documentation '2d-float-vector 'structure)
      "Field containing a two-dimensional vector. SF2d-float-vectors are written to file
as a pair of floating point values separated by whitespace.")

(defstruct 3d-float-vector
  (x 0.0 :type float-or-integer)
  (y 0.0 :type float-or-integer)
  (z 0.0 :type float-or-integer))

(setf (documentation '3d-float-vector 'structure)
      "Field containing a three-dimensional vector. SF3d-float-vectors are written to
file as three floating point values separated by whitespace.")


;;;------------------------------------------------------------------- 
;;;
;;; RESOURCE STUBS
;;;

;; These can be later upgraded to a full resourcing system if necessary.
;; 4/10/96 -- JCMa.
(defun allocate-color (&key red green blue)
  "Allocates a RGB color.
Each color is float in the range 0.0 to 1.0."
  (check-type red single-float)
  (check-type green single-float)
  (check-type blue single-float)
  (make-color :red red :green green :blue blue))

(defun allocate-rotation (&key x y z amount)
  "Allocates an arbitrary rotation.  The 4 values represent an axis of
rotation followed by the amount of right-handed rotation about that axis, in
radians."
  (check-type x float-or-integer)
  (check-type y float-or-integer)
  (check-type z float-or-integer)
  (check-type amount float-or-integer)
  (make-rotation :x x :y y :z z :amount amount))

(defun allocate-2d-float-vector (&key x y)
  "Allocates a 2 dimensional vector of floats."
  (check-type x float-or-integer)
  (check-type y float-or-integer)
  (make-2d-float-vector :x x :y y))

(defun allocate-3d-float-vector (&key x y z)
  "Allocates a 3 dimensional vector of floats."
  (check-type x float-or-integer)
  (check-type y float-or-integer)
  (check-type z float-or-integer)
  (make-3d-float-vector :x x :y y :z z))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defun allocate-resource-form (name args)
    "Availble resources are: COLOR, ROTATION, 2d-float-vector, 3d-float-vector."
    (ecase name
      (color
        (destructuring-bind (&key red green blue) args
          `(allocate-color :red ,red :green ,green :blue ,blue)))
      (rotation
        (destructuring-bind (&key x y z amount) args
          `(allocate-rotation :x ,x :y ,y :z ,z :amount ,amount)))
      (2d-float-vector
        (destructuring-bind (&key x y) args
          `(allocate-2d-float-vector :x ,x :y ,y)))
      (3d-float-vector
        (destructuring-bind (&key x y z) args
          `(allocate-3d-float-vector :x ,x :y ,y :z ,z))))))

;; Stubs for future resourcing.
(defmacro allocate-resource (name args)
  (allocate-resource-form name args))

(defun deallocate-resource (name resource)
  (declare (ignore name resource)))

(defmacro let-fields (bindings &body body)
  "BINDINGS is a series of (variable resource &rest args).
VARIABLE is bound to an object from RESOURCE which is initialized with ARGS.
Availble resources are: COLOR, ROTATION, 2D-FLOAT-VECTOR, 3D-FLOAT-VECTOR."
  (loop for (variable resource . args) in bindings
        collect `(,variable ,(allocate-resource-form resource args)) into clauses
        collect variable into vars
        finally (return `(let ,clauses
                           (declare (dynamic-extent ,. vars))
                           . ,body))))

;;;------------------------------------------------------------------- 
;;;
;;; PRINTING SUPPORT
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun print-list (start list sep end printer stream)
    (loop initially (write-string start stream)
          for (item . more) = list then more
          do (funcall printer item stream)
          while more
          do (write-string sep stream)
          finally (write-string end stream)))

  (defun print-as-symbol (item stream)
    (let ((string (substitute #\_ #\- (symbol-name item) :test #'eql)))
      (declare (dynamic-extent string))
      (write-string string stream)))

  (defvar *type-writer-method-alist* nil
    "An alist of all the printing methods for various VRML types.")

  (defun %register-vrml-type-writer (type writer)
    (check-type type keyword)
    (check-type writer (and symbol (satisfies fboundp)))
    (let ((entry (assoc type *type-writer-method-alist*)))
      (if entry
          (setf (cdr entry) writer)
          (push `(,type . ,writer) *type-writer-method-alist*))))

  (defun %unregister-vrml-type-writer (type)
    (setq *type-writer-method-alist* (delete type *type-writer-method-alist* :key #'car)))

  (declaim (inline %get-vrml-type-writer-name))

  (defun %get-vrml-type-writer-name (type)
    (or (cdr (assoc type *type-writer-method-alist* :test #'eq))
        (error "The VRML type, ~S, is unknown and has no writer method." type)))

  (declaim (inline %get-vrml-type-writer))

  (defun %get-vrml-type-writer (type)
    (symbol-function (%get-vrml-type-writer-name type)))

  (defmacro %write-list-or-item (item stream type)
    (let ((writer (%get-vrml-type-writer-name type)))
      `(typecase ,item
         (cons
           (print-list "[" ,item ", " "]" (function ,writer) ,stream))
         (t (,writer ,item ,stream)))))

;; compile writer replaces earlier interpreted version
  (defmacro write-vrml-type (stream value type)
    "Writes a VRML object, VALUE, of type TYPE on STREAM."
    (typecase type
      (keyword `(,(%get-vrml-type-writer-name type) ,value ,stream))
      (t `(funcall (%get-vrml-type-writer ,type) ,value ,stream))))

;; Backward compatible definition for old code. 4/10/96 -- JCMa.
  (defun print-value (stream value type)
    (funcall (%get-vrml-type-writer type) value stream))

  (defun %define-vrml-type-writer (type arglist body-forms)
    (let* ((type-keyword (http:symbolize (string type) http:*keyword-package*))
           (name (http:symbolize (format nil "WRITE-VRML-~A" type-keyword))))
      `(progn
         (define ,name ,arglist
           ,(format nil "Writes the VRML ~A to STREAM." type-keyword)
           ,@body-forms)
         (%register-vrml-type-writer ,type-keyword ',name)
         ',name)))

  (defmacro define-vrml-type-writer (type arglist &body body)
    "Top-level method for defining writers for VRML types.
ARGLIST is a list of the object and stream arguments. 
BODY is the code the writes the object to the stream."
    (%define-vrml-type-writer type arglist body))

  )                                             ;close eval-when


;;;------------------------------------------------------------------- 
;;;
;;; DEFINE VRML TYPE WRITERS
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)

;;SFBitMask
;;
;;A single-value field that contains a mask of bit flags. Nodes that use this
;;field class define mnemonic names for the bit flags. SFBitMasks are written
;;to file as one or more mnemonic enumerated type names, in this format:
;;
;;( flag1 | flag2 | ... )
;;
;;If only one flag is used in a mask, the parentheses are optional. These
;;names differ among uses of this field in various node classes.
;;
;;No more than 32 separate flags may be defined for an SFBitMask.
  (define-vrml-type-writer bitmask (bitmask stream)
    (etypecase bitmask
      (cons (print-list "(" bitmask "|" ")" #'print-as-symbol stream))
      (symbol (print-as-symbol bitmask stream))))

;;SFBool
;;
;;A field containing a single boolean (true or false) value. SFBools may be
;;written as 0 (representing FALSE), 1 (representing TRUE), TRUE, or FALSE.

  (define-vrml-type-writer bool (bool stream)
    (write-string (if bool "TRUE" "FALSE") stream))

  (define-vrml-type-writer color (color stream)
    (check-type color color)
    (format stream "~,3f" (color-red color))
    (write-char #\space stream)
    (format stream "~,3f" (color-green color))
    (write-char #\space stream)
    (format stream "~,3f" (color-blue color)))

;;SFEnum
;;
;;A single-value field that contains an enumerated type value. Nodes that use
;;this field class define mnemonic names for the values. SFEnums are written
;;to file as a mnemonic enumerated type name. The name differs among uses of
;;this field in various node classes.
  (define-vrml-type-writer enum (enum stream)
    (print-as-symbol enum stream))

;;SFFloat/MFFloat
;;
;;Fields that contain one (SFFloat) or zero or more (MFFloat) single-precision
;;floating point number. SFFloats are written to file in ANSI C floating point
;;format. For example:
;;
;;[ 3.1415926, 12.5e-3, .0001 ]
;;is an mffloat field containing three values.
  (define-vrml-type-writer float (float stream)
    (check-type float float-or-integer)
    (princ float stream))

  (define-vrml-type-writer image (image stream)
    (declare (ignore stream))
    (error "Don't know how to print ~a." image))

;;SFLong/MFLong
;;
;;Fields containing one (SFLong) or zero or more (MFLong) 32-bit integers.
;;SFLongs are written to file as an integer in decimal, hexadecimal (beginning
;;with '0x') or octal (beginning with '0') format. For example:
;;
;;[ 17, -0xE20, -518820 ]
;;is an mflong field containing three values.
  (define-vrml-type-writer long (long stream)
    (princ long stream))

;;SFMatrix
;;
;;A field containing a transformation matrix. SFMatrices are written to file
;;in row-major order as 16 floating point numbers separated by whitespace. For
;;example, a matrix expressing a translation of 7.3 units along the X axis is
;;written as:
;;
;;1 0 0 0  0 1 0 0  0 0 1 0  7.3 0 0 1
  (define-vrml-type-writer matrix (matrix stream)
    (check-type matrix array)
    (loop for i upto 3
          do (loop for j upto 3
                   do (princ (aref matrix i j) stream)
                   do (write-char #\space stream))
          do (terpri stream)))

;; a typed-checked version that ensures legal characters are used as node names.
  (define-vrml-type-writer node-name (node-name stream)
    (check-type node-name node-name)
    (prin1 node-name stream))

  (define-vrml-type-writer rotation (rotation stream)
    (princ (rotation-x rotation) stream)
    (write-char #\space stream)
    (princ (rotation-y rotation) stream)
    (write-char #\space stream)
    (princ (rotation-z rotation) stream)
    (write-char #\space stream)
    (princ (rotation-amount rotation) stream))

;;SFString/MFString
;;
;;Fields containing one (SFString) or zero or more (MFString) ASCII string
;;(sequence of characters). Strings are written to file as a sequence of ASCII
;;characters in double quotes (optional if the string doesn't contain any
;;whitespace). Any characters (including newlines and '#') may appear within
;;the quotes. To include a double quote character within the string, precede
;;it with a backslash. For example:
  (define-vrml-type-writer string (string stream)
    (check-type string string)
    (prin1 string stream))

  (define-vrml-type-writer 2d-float-vector (2d-float-vector stream)
    (princ (2d-float-vector-x 2d-float-vector) stream)
    (write-char #\space stream)
    (princ (2d-float-vector-y 2d-float-vector) stream))

  (define-vrml-type-writer 3d-float-vector (3d-float-vector stream)
    (princ (3d-float-vector-x 3d-float-vector) stream)
    (write-char #\space stream)
    (princ (3d-float-vector-y 3d-float-vector) stream)
    (write-char #\space stream)
    (princ (3d-float-vector-z 3d-float-vector) stream))

;; These must come last so that prior writers are available for inline
;; compilation.  4/10/96 -- JCMa.

  (define-vrml-type-writer m-color (m-color stream)
    (%write-list-or-item m-color stream :color))

  (define-vrml-type-writer m-float (m-float stream)
    (%write-list-or-item m-float stream :float))

  (define-vrml-type-writer m-long (m-long stream)
    (%write-list-or-item m-long stream :long))

  (define-vrml-type-writer m-string (m-string stream)
    (%write-list-or-item m-string stream :string))

  (define-vrml-type-writer m-2d-float-vector (m-2d-float-vector stream)
    (%write-list-or-item m-2d-float-vector stream :2d-float-vector))

  (define-vrml-type-writer m-3d-float-vector (m-3d-float-vector stream)
    (%write-list-or-item m-3d-float-vector stream :3d-float-vector))

  )                                             ;close eval-when


;;;------------------------------------------------------------------- 
;;;
;;; FIELD WRITERS
;;;

(defmacro %write-field (name value type stream)
  "Write the name + value field of a VRML command."
  `(progn (terpri ,stream)
          (write-string "  " ,stream)
          (princ ,name ,stream)
          (write-char #\space ,stream)
          (write-vrml-type ,stream ,value ,type)))

(defmacro %conditional-write-field (name value type stream)
  "Write the name + value field of a VRML command."
  `(let ((val ,value))
     (when val
       (%write-field ,name val ,type ,stream))))


;;;------------------------------------------------------------------- 
;;;
;;; MACROS
;;;

(define-macro with-vrml-world ((&key (version :|1.0|) (stream '*standard-output*)) &body body)
  "Write the VRML1.0 header to stream. Only one node is allowed."
  `(progn 
;     (terpri ,stream)
     ,(ecase version
        (:|1.0| `(write-string "#VRML V1.0 ascii" ,stream)))
     (terpri ,stream)
     (terpri ,stream)
     . ,body))

(define write-comment (string &key (stream *standard-output*))
  "Writes string as a VRML comment on STREAM."
  (fresh-line stream)
  (write-string "# " stream)
  (write-string string stream)
  (fresh-line stream))

(defmacro with-vrml-node ((name stream) &body argument-forms)
  "Write a VRML command named NAME on STREAM.
ARGUMENT-FORMS are forms that write the command arguments to stream."
  `(progn (terpri ,stream)
          (princ ,name ,stream)
          (write-string " {" ,stream)
          ,@argument-forms
          (terpri ,stream)
          (write-char #\} ,stream)))


;;;------------------------------------------------------------------- 
;;;
;;; PRIMITIVE SHAPES
;;;

(deftype-set-or-all cone-parts (:sides :bottom))
   
(define cone-node (stream &key height bottom-radius parts)
  "This node represents a simple cone whose central axis is aligned with the
y-axis. By default, the cone is centered at (0,0,0) and has a size of -1 to
+1 in all three directions. The cone has a radius of 1 at the bottom and a
height of 2, with its apex at 1 and its bottom at -1. The cone has two
parts: the sides and the bottom.

The cone is transformed by the current cumulative transformation and is
drawn with the current texture and material.

If the current material binding is PER-PART or PER-PART-INDEXED, the first
current material is used for the sides of the cone, and the second is used
for the bottom. Otherwise, the first material is used for the entire cone.

When a texture is applied to a cone, it is applied differently to the sides
and bottom. On the sides, the texture wraps counterclockwise (from above)
starting at the back of the cone. The texture has a vertical seam at the
back, intersecting the yz-plane. For the bottom, a circle is cut out of the
texture square and applied to the cone's base circle. The texture appears
right side up when the top of the cone is rotated towards the -Z axis.

PARTS
     :SIDES       The conical part
     :BOTTOM      The bottom circular face
     :ALL         All parts"
  (check-type height (or null number))
  (check-type bottom-radius (or null number))
  (check-type parts cone-parts)
  (with-vrml-node ("Cone" stream)
    (%conditional-write-field "height" height :float stream)
    (%conditional-write-field "bottomRadius" bottom-radius :float stream)
    (%conditional-write-field "parts" parts :bitmask stream)))

(define cube-node (stream &key width height depth)
  "This node represents a cuboid aligned with the coordinate axes. By default,
the cube is centered at (0,0,0) and measures 2 units in each dimension, from
-1 to +1. The cube is transformed by the current cumulative transformation
and is drawn with the current material and texture. A cube's width is its
extent along its object-space X axis, its height is its extent along the
object-space Y axis, and its depth is its extent along its object-space Z
axis.

If the current material binding is PER-PART, PER-PART-INDEXED, PER-FACE, or
PER-FACE-INDEXED, materials will be bound to the faces of the cube in this
order: front (+Z), back (-Z), left (-X), right (+X), top (+Y), and bottom (-Y).

Textures are applied individually to each face of the cube; the entire
texture goes on each face. On the front, back, right, and left sides of the
cube, the texture is applied right side up. On the top, the texture appears
right side up when the top of the cube is tilted toward the camera. On the
bottom, the texture appears right side up when the top of the cube is tilted
towards the -Z axis."
  (check-type width (or null number))
  (check-type height (or null number))
  (check-type depth (or null number))
  (with-vrml-node ("Cube" stream)
    (%conditional-write-field "height" height :float stream)
    (%conditional-write-field "width" width :float stream)
    (%conditional-write-field "depth" depth :float stream)))

(deftype-set-or-all cylinder-parts (:sides :top :bottom))

(define cylinder-node (stream &key radius height parts)
  "This node represents a simple capped cylinder centered around the y-axis. By
default, the cylinder is centered at (0,0,0) and has a default size of -1 to
+1 in all three dimensions. The cylinder has three parts: the sides, the top
(y = +1) and the bottom (y = -1). you can use the radius and height fields
to create a cylinder with a different size.

the cylinder is transformed by the current cumulative transformation and is
drawn with the current material and texture.

if the current material binding is per-part or per-part-indexed, the first
current material is used for the sides of the cylinder, the second is used
for the top, and the third is used for the bottom. otherwise, the first
material is used for the entire cylinder.

when a texture is applied to a cylinder, it is applied differently to the
sides, top, and bottom. on the sides, the texture wraps counterclockwise
(from above) starting at the back of the cylinder. the texture has a
vertical seam at the back, intersecting the yz-plane. for the top and
bottom, a circle is cut out of the texture square and applied to the top or
bottom circle. the top texture appears right side up when the top of the
cylinder is tilted toward the +z axis, and the bottom texture appears right
side up when the top of the cylinder is tilted toward the -z axis.

parts
     :sides   the cylindrical part
     :top     the top circular face
     :bottom  the bottom circular face
     :all     all parts"
  (check-type radius (or null number))
  (check-type height (or null number))
  (check-type parts cylinder-parts)
  (with-vrml-node ("Cylinder" stream)
    (%conditional-write-field "radius" radius :float stream)
    (%conditional-write-field "height" height :float stream)
    (%conditional-write-field "parts" parts :bitmask stream)))

(define sphere-node (stream &key radius)
  "This node represents a sphere. By default, the sphere is centered at the
origin and has a radius of 1. The sphere is transformed by the current
cumulative transformation and is drawn with the current material and
texture.

A sphere does not have faces or parts. Therefore, the sphere ignores
material and normal bindings, using the first material for the entire sphere
and using its own normals. When a texture is applied to a sphere, the
texture covers the entire surface, wrapping counterclockwise from the back
of the sphere. The texture has a seam at the back on the yz-plane.
The radius should be positive. Special uses of negative radius spheres
should be replaced by extension nodes."
  (check-type radius (or null number))
  (with-vrml-node ("Sphere" stream)
    (%conditional-write-field "radius" radius :float stream)))


;;;------------------------------------------------------------------- 
;;;
;;; CHANGING THE POSITION, ROTATION
;;;

(define translation-node (stream 3d-float-vector)
  "This node defines a translation by a 3D vector."
  (with-vrml-node ("Translation" stream)
    (%write-field "translation" 3d-float-vector :3d-float-vector stream)))

(define translation-node* (stream x y z)
  (let-fields ((field 3d-float-vector :x x :y y :z z))
    (translation-node stream field)))

(define rotation-node (stream rotation)
  "This node defines a 3D rotation about an arbitrary axis through the origin.
The rotation is accumulated into the current transformation, which is
applied to subsequent shapes."
  (with-vrml-node ("Rotation" stream)
    (%write-field "rotation" rotation :rotation stream)))

(define rotation-node* (stream x y z amount)
  (let-fields ((field rotation :x x :y y :z z :amount amount))
    (rotation-node stream field)))

(define scale-node (stream 3d-float-vector)
  "This node defines a 3D scaling about the origin. If the components of the
scaling vector are not all the same, this produces a non-uniform scale."
  (with-vrml-node ("Scale" stream)
    (%conditional-write-field "scaleFactor" 3d-float-vector :3d-float-vector stream)))

(define scale-node* (stream x y z)
  (let-fields ((field 3d-float-vector :x x :y y :z z))
    (scale-node stream field)))

(define transform-node (stream &key translation rotation scale-factor scale-orientation center)
  (with-vrml-node ("Transform" stream)
    (%conditional-write-field "translation" translation :3d-float-vector stream)
    (%conditional-write-field "rotation" rotation :rotation stream)
    (%conditional-write-field "scale-factor" scale-factor :3d-float-vector stream)
    (%conditional-write-field "scale-orientation" scale-orientation :rotation stream)
    (%conditional-write-field "center" center :3d-float-vector stream)))

(setf (documentation 'transform-node 'function)
      "This node defines a geometric 3D transformation consisting of (in order) a
(possibly) non-uniform scale about an arbitrary point, a rotation about an
arbitrary point and axis, and a translation. the transform node

(transform-node stream
                :translation t1
                :rotation r1
                :scale-factor s
                :scale-orientation r2
                :center t2)

is equivalent to the sequence:

(translation-node stream :translation t1)
(translation-node stream :translation t2)
(rotation-node stream :rotation r1)
(rotation-node stream :rotation r2)
(scale-node stream :scale-factor s)
(rotation-node stream :rotation -r2)
(translation-node stream :translation -t2)")

(define matrix-transform-node (stream matrix)
  "This node defines a geometric 3D transformation with a 4 by 4 matrix. Only
matrices that are the result of rotations, translations, and non-zero (but
possibly non-uniform) scales must be supported. Non-invertible matrices
should be avoided.

Matrices are specified in row-major order, so, for example, a
MatrixTransform representing a translation of 6.2 units along the local Z
axis would be specified as:

(matrix-transform-node stream
                       (make-array '(4 4)
                                   :initial-contents '((1 0 0 0)
                                                       (0 1 0 0)
                                                       (0 0 1 0)
                                                       (0 0 6.2 1))))"
  (with-vrml-node ("matrixtransform" stream)
    (%write-field "matrix" matrix :matrix stream)))


;;;------------------------------------------------------------------- 
;;;
;;; TEXT AND FONT
;;;

(deftype text-justification ()
  '(or null (member :left :right :center)))

(define ascii-text-node (stream string &key width spacing justification)
  "This node represents strings of text characters from the ASCII coded
character set. The first string is rendered with its baseline at (0,0,0).
All subsequent strings advance y by -( size * spacing). See FONT-STYLE for a
description of the size field. 

JUSTIFICATION determines the placement of the strings in the x dimension.

        :LEFT (the default) places the left edge of each string at x=0. 
        :CENTER places the center of each string at x=0.
        :RIGHT places the right edge of each string at x=0. 

Text is rendered from left to right, top to bottom in the font set by
FontStyle.

WIDTH specifies the maximum rendered widths (in object space) for the strings.
If the text is too long, it must be scaled to fit within this width. The
default is to use the natural width of each string. If there are not enough
values in the width field, the last value will be used for the remaining
strings. Setting any value to 0 indicates the natural width should be used for
that string.

The text is transformed by the current cumulative transformation and is
drawn with the current material and texture.

Textures are applied to 3D text as follows. The texture origin is at the
origin of the first string, as determined by the justification. The texture
is scaled equally in both S and T dimensions, with the font height
representing 1 unit. S increases to the right. The T origin can occur
anywhere along each character, depending on how that character's outline is
defined.

JUSTIFICATION
     :LEFT     Align left edge of text to origin
     :CENTER   Align center of text to origin
     :RIGHT    Align right edge of text to origin"
  (check-type justification text-justification)
  (with-vrml-node ("AsciiText" stream)
    (%write-field "string" string :m-string stream)
    (%conditional-write-field "width" width :float stream)
    (%conditional-write-field "spacing" spacing :float stream)
    (%conditional-write-field "justification" justification :enum stream)))

(deftype font-family () '(or null (member :serif :sans :typewriter)))

(deftype font-style () '(or null (member :none :bold :italic)))

(define font-style-node (stream &key family style size)
  "This node defines the current font style used for all subsequent
ASCII-TEXT.  Font attributes only are defined. It is up to the browser to
assign specific fonts to the various attribute combinations. SIZE specifies
the height (in object space units) of glyphs rendered and determines the
vertical spacing of adjacent lines of text.

FAMILY

     :SERIF       Serif style (such as Times-Romana)
     :SANS        Sans Serif Style (such as Helvetica)
     :TYPEWRITER  Fixed pitch style (such as Courier)

STYLE
     :NONE        No modifications to family
     :BOLD        Embolden family
     :ITALIC      Italicize or Slant family"
  (check-type family font-family)
  (check-type style font-style)
  (with-vrml-node ("FontStyle" stream)
    (%conditional-write-field "family" family :enum stream)
    (%conditional-write-field "style" style :enum stream)
    (%conditional-write-field "size" size :float stream)))


;;;------------------------------------------------------------------- 
;;;
;;; MATERIAL
;;;

(define material-node (stream &key diffuse-color emissive-color transparency
                              ambient-color specular-color shininess)
  (with-vrml-node ("Material" stream)
    (%conditional-write-field "diffuseColor" diffuse-color :m-color stream)
    (%conditional-write-field "emissiveColor" emissive-color :m-color stream)
    (%conditional-write-field "transparency" transparency :float stream)
    (%conditional-write-field "ambientColor" ambient-color :m-color stream)
    (%conditional-write-field "specularColor" specular-color :m-color stream)
    (%conditional-write-field "shininess" shininess :float stream)))

(setf (documentation 'material-node 'function)
      "This node defines the current surface material properties for all subsequent
shapes. Material sets several components of the current material during
traversal. Different shapes interpret materials with multiple values
differently. To bind materials to shapes, use a MaterialBinding node.

The fields in the Material node determine the way light reflects off of an
object to create color:

        AMBIENT-COLOR reflects ambient light evenly from all parts of an
        object regardless of viewing and lighting angles.

        DIFFUSE-COLOR reflects all VRML light sources depending on the angle
        of the surface with respect to the light source. The more directly the
        surface faces the light, the more diffuse light reflects.

        SPECULAR-COLOR and shininess determine the specular highlights, e.g.,
        the shiny spots on an apple. When the angle from the light to the
        surface is close to the angle from the surface to the viewer, the
        SPECULAR-COLOR is added to the diffuse and ambient color calculations.
        Lower shininess values produce soft glows, while higher values result
        in sharper, smaller highlights.

        EMISSIVE-COLOR models `glowing' objects. This can be useful for
        displaying radiosity-based models (where the light energy of the room
        is computed explicitly), or for displaying scientific data.

        TRANSPARENCY is how `clear' the object is, with 1.0 being completely
        transparent, and 0.0 completely opaque.

The lighting parameters defined by the Material node are the same parameters
defined by the OpenGL lighting model. For a rigorous mathematical
description of how these parameters should be used to determine how surfaces
are lit, see the description of lighting operations in the OpenGL
Specification. Several of the OpenGL parameters (such as light attenuation
factors) are also left unspecified in VRML. Also note that OpenGL specifies
the specular exponent as a non-normalized 0-128 value, which is specified as
a normalized 0-1 value in VRML (simply multiplying the VRML \"shininess\"
value by 128 to translate to the OpenGL specular exponent parameter).

We assume that there is an implicit white ambient light of intensity 1.0 in
any VRML scene.

Issues for Low-End Rendering Systems. For rendering systems that do not
support the full OpenGL lighting model, the following simpler lighting model
is recommended:

A TRANSPARENCY value of 0 is completely opaque, a value of 1 is completely
transparent. Applications need not support partial transparency, but should
support at least fully transparent and fully opaque surfaces, treating
transparency values >= 0.5 as fully transparent.

Specifying only EMISSIVE-COLOR and no diffuse, specular, emissive, or
ambient colors is the way to specify pre-computed lighting. It is expected
that browsers will be able to recognize this as a special case and optimize
their computations. For example:

(material stream
          :ambient-color nil :diffuse-color nil :specular-color nil
          :emissive-color [ 0.1 0.1 0.2, 0.5 0.8 0.8 ])

notice that an empty list, specified by [], is different than not specifying
the field. an empty list explicitly means no value; if nothing is specified,
the default value will be used.

many low-end pc rendering systems are not able to support the full range of
the vrml material specification. for example, many systems do not render
individual red, green and blue reflected values as specified in the
specular-color field. the following table describes which material fields are
typically supported in popular low-end systems and suggests actions for
browser implementors to take when a field is not supported.

keyword          supported?      suggested action

ambient-color    no              ignore
diffuse-color    yes             use as base color
specular-color   no              ignore
emissive-color   no              ignore, unless all others are empty
shininess        yes             use
transparency     no              ignore

rendering systems which do not support specular color may nevertheless
support a specular intensity. the following recommendation gives reasonable
results for both plastic highlights where the specular color is white and
also for metallic highlights where the specular color is the same as or a
ratio of the diffuse color. to get the specular coefficient, take the
highest specular component and, if the same component of the diffuse color
is nonzero, divide by the diffuse. if the diffuse component is zero, use the
specular component. in other symbols:

  max_channel = red;
  if specular[green] > specular[max_channel] then max_channel = green;
  if specular[blue] > specular[max_channel] then max_channel = blue;
  if diffuse[max_channel] > 0 then
    ks = max(spec[mc]/diff[mc],1) else ks = spec[mc]

if a system supports ambient intensity but not color, the intensity should
be derived by taking the dot product of the specified rgb ambient value with
the vector [.299 .587 .114]. this adjusts the value to compensate for the
variable sensitivity of the eye to colors. for more on this, see [fvdh],
13.33, page 589. if a rendering system does not support per-object ambient
values, it should set the ambient value for the entire scene at the average
ambient value of all objects contained in the top level file.

it is also expected that simpler rendering systems will be unable to support
both lit and unlit objects in the same world.

many vrml implementations will support only either multiple diffuse colors
with a single value for all other fields, or multiple emissive colors with
one transparency value and no (empty, '[]') values for all other fields.
more complicated uses of the material node should be avoided.")

(deftype material-binding ()
  '(or null (member :default :overall :per-part :per-face :per-vertex
                    :per-part-indexed :per-face-indexed :per-vertex-indexed)))

(define material-binding-node (stream binding)
  (check-type binding material-binding)
  (with-vrml-node ("MaterialBinding" stream)
    (%write-field "value" binding :enum stream)))

(setf (documentation 'material-binding-node 'function)
      "Material nodes may contain more than one material. This node specifies how
the current materials are bound to shapes that follow in the scene graph.
Each shape node may interpret bindings differently. For example, a Sphere
node is always drawn using the first material in the material node, no
matter what the current Material-Binding, while a Cube node may use six
different materials to draw each of its six faces, depending on the
Material-Binding.

The bindings for faces and vertices are meaningful only for shapes that are
made from faces and vertices. Similarly, the indexed bindings are only used
by the shapes that allow indexing.

When multiple material values are needed by a shape, the previous Material
node should have at least as many materials as are needed, otherwise results
are undefined.

Note that some rendering systems do not support per-vertex material changes.
Browsers that do not support per-vertex colors should average the colors
specified when a PER-VERTEX binding is used.

BINDINGS
     :DEFAULT            Use default binding
     :OVERALL            Whole object has same material
     :PER-PART           One material for each part of object
     :PER-PART-INDEXED   One material for each part, indexed
     :PER-FACE           One material for each face of object
     :PER-FACE-INDEXED   One material for each face, indexed
     :PER-VERTEX         One material for each vertex of object
     :PER-VERTEX-INDEXED One material for each vertex, indexed")


;;;------------------------------------------------------------------- 
;;;
;;; GROUPS
;;;

(define-macro with-group ((stream) &body body)
  `(with-vrml-node ("Group" ,stream)
     ,@body))

(deftype render-culling ()
  '(or null (member :on :off :auto)))

(define-macro with-separator-group ((stream &key render-culling) &body body)
  "This group node performs a push (save) of the traversal state before
traversing its inferiors and a pop (restore) after traversing them. This
isolates the separator's inferiors from the rest of the scene graph. A
separator can include lights, cameras, coordinates, normals, bindings, and
all other properties.

Separators can also perform render culling. Render culling skips over
traversal of the separator's inferiors if they are not going to be rendered,
based on the comparison of the separator's bounding box with the current
view volume. Culling is controlled by RENDER-CULLING. These are set
to AUTO by default, allowing the implementation to decide whether or not to
cull.

RENDER-CULLING
     :ON    Always try to cull to the view volume
     :OFF   Never try to cull to the view volume
     :AUTO  Implementation-defined culling behavior"
  `(with-vrml-node ("Separator" ,stream)
     (%conditional-write-field "renderCulling" ,render-culling :enum ,stream)
     ,@body))

(define-macro with-transform-separator-group ((stream) &body body)
  "This group node is similar to the separator node in that it saves state
before traversing its inferiors and restores it afterwards. However, it saves
only the current transformation; all other state is left as is. This node
can be useful for positioning a camera, since the transformations to the
camera will not affect the rest of the world, even through the camera will
view the world. Similarly, this node can be used to isolate transformations
to light sources or other objects.

This node is being deprecated because it is expected that future versions of
VRML will not allow properties to `leak out' of group nodes; that is, all
group-like nodes will behave like Separators, making TransformSeparator
obsolete."
  `(with-vrml-node ("TransformSeparator" ,stream)
     ,@body))

(define-macro with-switch-group ((stream &key inferior) &body body)
  "This group node traverses one or none of its inferiors. One can use this node
to switch on and off the effects of some properties or to switch between
different properties.

INFERIOR specifies the index of the inferior to traverse, where the
first inferior has index 0.

A value of -1 (the default) means do not traverse any inferiors."
  `(with-vrml-node ("Switch" ,stream)
     (%conditional-write-field "whichChild" ,inferior :long ,stream)
     ,@body))


;;;------------------------------------------------------------------- 
;;;
;;; DEFINITIONS
;;;

;; this has some problems.  Needs more structure to emit the correct
;; arguments.  4/10/96 -- JCMa.
(define-macro with-definition ((stream name) &rest body)
  "Defines an object named NAME.
DEF objectname objecttype { fields  inferiors }

Only the object type and curly braces are required; nodes may or may not
have a name, fields, and inferiors.

Node names must not begin with a digit, and must not contain spaces or
control characters, single or double quote characters, backslashes, curly
braces, the plus character or the period character."
  `(progn (terpri ,stream)
          (write-string "DEF " ,stream)
          (princ ,name ,stream)
          (write-string " " ,stream)
          ,@body))

(define use (stream name)
  "A node may be the inferior of more than one group. 

  This is called `instancing' (using the same instance of a node multiple times,
                                     called `aliasing' or `multiple references' by other systems), and is
  accomplished by using the `USE' keyword.

  The DEF keyword gives a node a name. DEF also creates a copy of the node.  The
  USE keyword indicates that a named node should be used again. If several nodes
  were given the same name, then the last DEF encountered during parsing `wins'
  (the occurence of multiple nodes with the same name is strongly discouraged
       and may cause problems in the future). def/use is limited to a single file       ;
  there is no mechanism for use'ing nodes that are def'ed in other files. refer
  to the `general syntax' section of this specification for the legal syntax of
  node names.

  a name goes into scope as soon as the def is encountered, and does not go out
  of scope until another def of the same name or end-of-file are encountered.
  nodes cannot be shared between files (you cannot use a node that was def'ed
                                            inside the file to which a wwwinline refers).

  for example, rendering this world will result in three spheres being drawn.
  both of the spheres are named 'joe'           ; the second (smaller) sphere is drawn
  twice:

  (let ((stream *standard-output*))
    (with-vrml-world (:stream stream)
      (with-separator-group (stream)
        (with-definition (stream \"joe\")
                         (sphere-node stream))
        (translation-node* stream 2 0 0)
        (with-separator-group (stream)
          (with-definition (stream \"joe\")
                           (sphere-node stream :radius 0.2)))
        (translation-node* stream 2 0 0)
        (use stream \"joe\"))))"
  (terpri stream)
  (write-string "use " stream)
  (princ name stream))


;;;------------------------------------------------------------------- 
;;;
;;; LINKS
;;;

(deftype anchor-map () '(or null (member :none :point)))

(define-macro with-anchor-group ((stream name &key description map) &body body)
  "Map is one of (:none :point)."
  `(with-vrml-node ("WWWAnchor" ,stream)
     (%write-field "name" ,name :node-name ,stream)
     (%conditional-write-field "description" ,description :string ,stream)
     (%conditional-write-field "map" ,map :enum ,stream)
     ,@body))

(setf (documentation 'with-anchor-group 'function)
      "The WWWAnchor group node loads a new world into a VRML browser when one
of its inferiors is chosen, assuming it is a .wrl file. Other types of URLs
may be handled differently. Exactly how a user `chooses' a inferior of the
WWWAnchor is up to the VRML browser; typically, clicking on one of its
inferiors with the mouse will result in the new world replacing the current
world. A WWWAnchor with an empty (\"\") name does nothing when its inferiors
are chosen. If WWWAnchors are nested, the most deeply nested WWWAnchor is the
one which is chosen.

The name is an arbitrary URL, as defined in RFC 1738, with relative URL
semantics, as defined in RFC 1808. Browsers which require additional
information to be associated with following a link are encouraged to create an
extension node with additional fields.

WWWAnchor behaves like a Separator, pushing the traversal state before
traversing its inferiors and popping it afterwards.

The description field in the WWWAnchor allows for a friendly prompt to be
displayed as an alternative to the URL in the name field. Ideally, browsers
will allow the user to choose the description, the URL or both to be
displayed for a candidate WWWAnchor.

The WWWAnchor's map field is an enumerated value that can be either NONE
(the default) or point. if it is point then the object-space coordinates of
the point on the object the user chose will be added to the url in the name
field, with the syntax `?x,y,z'.

a wwwanchor may be used to take the viewer to a particular viewpoint in a
virtual world by specifying a url ending with `#cameraname', where
cameraname is the name of a camera defined in the world. for example:

(with-anchor-group
  (stream \"http://www.school.edu/vrml/someworld.wrl#overview\")
  (cube stream))

specifies an anchor that puts the viewer in the someworld world looking
from the camera named overview when the cube is chosen. if no world is
specified, then the current world is implied; for example:

(with-anchor-group
  (stream \"#doorway\")
  (sphere stream))

will take the viewer to the viewpoint defined by the doorway camera in the
current world when the sphere is chosen.

map
     :none  do not add information to the url
     :point add object-space coordinates to url")

(define-macro with-www-inline-group ((stream name &key bounding-box-size bounding-box-center) &body body)
  "The WWWInline node reads its inferiors from anywhere in the World Wide Web.
Exactly when its inferiors are read is not defined; reading the inferiors may
be delayed until the WWWInline is actually displayed. A WWWInline with an
empty name does nothing. The name is an arbitrary URL.

The effect of referring to a non-VRML URL in a WWWInline node is undefined.

If the WWWInline's bounding-box-size field specifies a non-empty bounding box (a
bounding box is non-empty if at least one of its dimensions is greater than
zero), then the WWWInline's object-space bounding box is specified by its
bounding-box-size and bounding-box-center fields. This allows an implementation to quickly
determine whether or not the contents of the WWWInline might be visible.
This is an optimization hint; if the true bounding box of the contents of
the WWWInline is different from the specified bounding box results will be
undefined. Browsers may draw the bounding box in wireframe while they are
fetching the contents."
  `(with-vrml-node ("WWWInline" ,stream)
     (%write-field "name" ,name :node-name ,stream)
     (%conditional-write-field "bboxSize" ,bounding-box-size :3d-float-vector ,stream)
     (%conditional-write-field "bboxCenter" ,bounding-box-center :3d-float-vector ,stream)
     ,@body))


;;;------------------------------------------------------------------- 
;;;
;;; POINTS, LINES, FACES
;;;

(define coordinate3-node (stream points)
  "This node defines a set of 3D coordinates to be used by a subsequent
IndexedFaceSet, IndexedLineSet, or PointSet node. This node does not produce
a visible result during rendering; it simply replaces the current
coordinates in the rendering state for subsequent nodes to use."
  (with-vrml-node ("Coordinate3" stream)
    (%write-field "point" points :m-3d-float-vector stream)))

(define indexed-face-set-node (stream &key coordinate-index material-index normal-index texture-coordinate-index)
  (with-vrml-node ("IndexedFaceSet" stream)
    (%conditional-write-field "coordIndex" coordinate-index :m-long stream)
    (%conditional-write-field "materialIndex" material-index :m-long stream)
    (%conditional-write-field "normalIndex" normal-index :m-long stream)
    (%conditional-write-field "textureCoordIndex" texture-coordinate-index :m-long stream)))

(setf (documentation 'indexed-face-set-node 'function)
      "This node represents a 3D shape formed by constructing faces (polygons) from
vertices located at the current coordinates. INDEXEDFACESET uses the indices
in its COORDINATE-INDEX to define polygonal faces. An index of -1 separates
faces (so a -1 at the end of the list is optional).

The vertices of the faces are transformed by the current transformation
matrix.

Treatment of the current material and normal binding is as follows: The
PER-PART and PER-FACE bindings specify a material or normal for each face.
PER-VERTEX specifies a material or normal for each vertex. The corresponding
-INDEXED bindings are the same, but use the MATERIAL-INDEX or NORMAL-INDEX
indices. The DEFAULT material binding is equal to OVERALL. The DEFAULT normal
binding is equal to PER-VERTEX-INDEXED; if insufficient normals exist in the
state, vertex normals will be generated automatically. When MATERIAL-INDEX or
NORMAL-INDEX are specified on a per vertex basis, an index of -1 is used in
the same way as COORDINATE-INDEX.

Explicit texture coordinates (as defined by TEXTURE-COORDINATE2) may be bound
to vertices of a polygon in an indexed shape by using the indices in the
TEXTURE-COORDINATE-INDEX field. Therefore, the length of the COORDINATE-INDEX
field and the length of the TEXTURE-COORDINATE-INDEX field should be equal, if
textures are defined. This allows a vertex to have different texture indices
on different faces (television on six sides of a cube).

As with all vertex-based shapes, if there is a current texture but no
texture coordinates are specified, a default texture coordinate mapping is
calculated using the bounding box of the shape. The longest dimension of the
bounding box defines the S coordinates, and the next longest defines the T
coordinates. The value of the S coordinate ranges from 0 to 1, from one end
of the bounding box to the other. The T coordinate ranges between 0 and the
ratio of the second greatest dimension of the bounding box to the greatest
dimension. If any dimensions are equal, then the ordering of X>Y>Z is
chosen. An index of -1 is used to separate TEXTURE-COORDINATE-INDEX faces, in the
same way as COORDINATE-INDEX.

Be sure that the indices contained in the COORDINATE-INDEX, MATERIAL-INDEX,
NORMAL-INDEX, and TEXTURE-COORDINATE-INDEX fields are valid with respect to the
current state, or errors will occur.")

(define indexed-line-set-node (stream &key coordinate-index material-index normal-index texture-coordinate-index)
  "This node represents a 3D shape formed by constructing polylines from
vertices located at the current coordinates. IndexedLineSet uses the indices
in its coordinate-index field to specify the polylines. An index of -1 separates
one polyline from the next (thus, a final -1 is optional). the current
polyline has ended and the next one begins.

The coordinates of the line set are transformed by the current cumulative
transformation.

Treatment of the current material and normal binding is as follows: The
PER-PART binding specifies a material or normal for each segment of the
line. The PER-FACE binding specifies a material or normal for each polyline.
PER-VERTEX specifies a material or normal for each vertex. The corresponding
-INDEXED bindings are the same, but use the material-index or normal-index
indices. The DEFAULT material binding is equal to OVERALL. The DEFAULT
normal binding is equal to PER-VERTEX-INDEXED; if insufficient normals exist
in the state, the lines will be drawn unlit. The same rules for texture
coordinate generation as IndexedFaceSet are used."
  (with-vrml-node ("IndexedLineSet" stream)
    (%conditional-write-field "coordIndex" coordinate-index :m-long stream)
    (%conditional-write-field "materialIndex" material-index :m-long stream)
    (%conditional-write-field "normalIndex" normal-index :m-long stream)
    (%conditional-write-field "textureCoordIndex" texture-coordinate-index :m-long stream)))

(define point-set-node (stream &key start number-of-points)
  "This node represents a set of points located at the current coordinates.
POINT-SET uses the current coordinates in order, starting at the index
specified by START. The number of points in the set is specified by
NUMBER-OF-POINTS. A value of -1 for this field indicates that all remaining
values in the current coordinates are to be used as points.

The coordinates of the point set are transformed by the current cumulative
transformation. The points are drawn with the current material and texture.

Treatment of the current material and normal binding is as follows:  PER-PART,
PER-FACE, and PER-VERTEX bindings bind one material or normal to each point.
The DEFAULT material binding is equal to OVERALL. The DEFAULT normal binding
is equal to PER-VERTEX. The start is also used for materials or normals when
the binding indicates that they should be used per vertex."
  (with-vrml-node ("pointSet" stream)
    (%conditional-write-field "startIndex" start :long stream)
    (%conditional-write-field "numPoints" number-of-points :long stream)))


;;;------------------------------------------------------------------- 
;;;
;;; NORMALS
;;;

(define normal-node (stream 3d-float-vector)
  "This node defines a set of 3D surface normal vectors to be used by
vertex-based shape nodes (INDEXED-FACESET, INDEXED-LINESET, POINT-SET) that
follow it in the scene graph. This node does not produce a visible result
during rendering; it simply replaces the current normals in the rendering
state for subsequent nodes to use. This node contains one multiple-valued
field that contains the normal vectors.

To save network bandwidth, it is expected that implementations will be able
to automatically generate appropriate normals if none are given. However,
the results will vary from implementation to implementation."
  (with-vrml-node ("Normal" stream)
    (%write-field "translation" 3d-float-vector :3d-float-vector stream)))

(define normal-node* (stream x y z)
  (let-fields ((field 3d-float-vector :x x :y y :z z))
    (normal-node stream field)))

(deftype normal-binding ()
  '(or null (member :default :overall :per-part :per-face :per-vertex
                    :per-part-indexed :per-face-indexed :per-vertex-indexed)))

(define normal-binding-node (stream binding)
  "This node specifies how the current normals are bound to shapes that follow
in the scene graph. Each shape node may interpret bindings differently.

The bindings for faces and vertices are meaningful only for shapes that are
made from faces and vertices. Similarly, the indexed bindings are only used
by the shapes that allow indexing. For bindings that require multiple
normals, be sure to have at least as many normals defined as are necessary;
otherwise, errors will occur.

Browsers that do not support per-vertex normals should average the normals
specified when a PER_VERTEX binding is used.

BINDINGS
     :DEFAULT            Use default binding
     :OVERALL            Whole object has same normal
     :PER-PART           One normal for each part of object
     :PER-PART-INDEXED   One normal for each part, indexed
     :PER-FACE           One normal for each face of object
     :PER-FACE-INDEXED   One normal for each face, indexed
     :PER-VERTEX         One normal for each vertex of object
     :PER-VERTEX-INDEXED One normal for each vertex, indexed"
  (check-type binding normal-binding)
  (with-vrml-node ("NormalBinding" stream)
    (%write-field "value" binding :enum stream)))


;;;------------------------------------------------------------------- 
;;;
;;; SHAPE HINTS
;;;

(deftype vertex-ordering ()
  '(or null (member :unknown-ordering :clockwise :counterclockwise)))

(deftype shape-type ()
  '(or null (member :unknown-shape-type :solid)))

(deftype face-type ()
  '(or null (member :unknown-face-type :convex)))

(define shape-hints-node (stream &key vertex-ordering shape-type face-type crease-angle)
  (check-type vertex-ordering vertex-ordering)
  (check-type shape-type shape-type)
  (check-type face-type face-type)
  (with-vrml-node ("NormalBinding" stream)
    (%conditional-write-field "vertexOrdering" vertex-ordering :enum stream)
    (%conditional-write-field "shapeType" shape-type :enum stream)
    (%conditional-write-field "faceType" face-type :enum stream)
    (%conditional-write-field "creaseAngle" crease-angle :long stream)))

(setf (documentation 'shape-hints-node 'function)
      "The SHAPE-HINTS node indicates that INDEXED-FACE-SETS are solid, contain
ordered vertices, or contain convex faces.

These hints allow VRML implementations to optimize certain rendering features.
Optimizations that may be performed include enabling back-face culling and
disabling two-sided lighting. For example, if an object is solid and has
ordered vertices, an implementation may turn on backface culling and turn off
two-sided lighting. To ensure that an Indexed-Face-Set can be viewed from either
direction, set SHAPE-TYPE to be :UNKNOWN-SHAPE-TYPE.

If you know that your shapes are closed and will alwsys be viewed from the
outside, set vertex-ordering to be either :CLOCKWISE or :COUNTERCLOCKWISE
(depending on how you built your object), and set shape-type to be solid.
placing this near the top of your vrml file will allow the scene to be
rendered much faster:

     (shapehints stream
                 :vertex-ordering  :clockwise
                 :shape-type       :solid)
     

the shape-hints node also affects how default normals are generated. when an
indexed-face-set has to generate default normals, it uses :crease-angle to
determine which edges should be smoothly shaded and which ones should have a
sharp crease. the crease angle is the angle between surface normals on
adjacent polygons. for example, a crease angle of .5 radians (the default
value) means that an edge between two adjacent polygonal faces will be smooth
shaded if the normals to the two faces form an angle that is less than .5
radians (about 30 degrees). otherwise, it will be faceted.

vertex-ordering
     :unknown-ordering    ordering of vertices is unknown
     :clockwise           face vertices are ordered clockwise
                          (from the outside)
     :counterclockwise    face vertices are ordered counterclockwise
                          (from the outside)
shape type
     :unknown-shape-type  nothing is known about the shape
     :solid               the shape encloses a volume

face type
     :unknown-face-type   nothing is known about faces
     :convex              all faces are convex")


;;;------------------------------------------------------------------- 
;;;
;;; TEXTURE
;;;

(deftype wrap-type () '(or null (member :repeat :clamp)))

;;; omitted image parameter
(define texture2-node (stream &key url wraps wrapt)
  (check-type wraps wrap-type)
  (check-type wrapt wrap-type)
  (with-vrml-node ("Texture2" stream)
    (%conditional-write-field "filename" url :string stream)
    (%conditional-write-field "wrapS" wraps :enum stream)
    (%conditional-write-field "wrapT" wrapt :enum stream)))

(setf (documentation 'texture2-node 'function)
      "This property node defines a texture map and parameters for that map. This
map is used to apply texture to subsequent shapes as they are rendered.

The texture can be read from the URL specified by URL. To turn off texturing,
set URL to an empty string (\"\").  Implementations should support at least
the JPEG image file format, with PNG strongly recommended. Due to legal
issues, we do not require supporting the GIF format, though many existing
scenes contain GIF files.

Renderers which support transparent texture maps should pay attention to any
alpha channel information in the texture map. This allows for `cookie-cutter'
effects (trees, people, etc).

Textures can also be specified inline by setting the image field to contain
the texture data. Supplying both image and URL will result in undefined
behavior.

Texture images may be one component (grey-scale), two component (grey-scale
plus transparency), three component (full RGB color), or four-component (full
RGB color plus transparency). An ideal VRML implementation will use the
texture image to modify the diffuse color and transparency of an object's
material (specified in a Material node), then performing any lighting
calculations using the rest of the object's material properties with the
modified diffuse color to produce the final image. The texture image modifies
the diffuse color and transparency depending on how many components are in the
image, as follows:

  1. Diffuse color is multiplied by the grey-scale values in the texture
     image.
  2. Diffuse color is multiplied by the grey-scale values in the texture
     image, material transparency is multiplied by transparency values in
     texture image.
  3. RGB colors in the texture image replace the material's diffuse color.
  4. RGB colors in the texture image replace the material's diffuse color,
     transparency values in the texture image replace the material's
     transparency.

Browsers may approximate this ideal behavior to increase performance. One
common optimization is to calculate lighting only at each vertex and combining
the texture image with the color computed from lighting (performing the
texturing after lighting). Another common optimization is to perform no
lighting calculations at all when texturing is enabled, displaying only the
colors of the texture image.

WRAP
     :REPEAT  Repeats texture outside 0-1 texture coordinate range
     :CLAMP   Clamps texture coordinates to lie within 0-1 range")

(define texture2-transform-node (stream &key translation rotation scale-factor center)
  "This node defines a 2D transformation applied to texture coordinates. This
affects the way textures are applied to the surfaces of subsequent shapes.
The transformation consists of (in order) a non-uniform scale about an
arbitrary center point, a rotation about that same point, and a translation.
This allows a user to change the size and position of the textures on
shapes."
  (with-vrml-node ("Texture2Transform" stream)
    (%conditional-write-field "translation" translation :2d-float-vector stream)
    (%conditional-write-field "rotation" rotation :float stream)
    (%conditional-write-field "scaleFactor" scale-factor :2d-float-vector stream)
    (%conditional-write-field "center" center :2d-float-vector stream)))

(define texture-coordinate2-node (stream point)
  "This node defines a set of 2D coordinates to be used to map textures to the
vertices of subsequent POINT-SET, INDEXED-LINE-SET, or INDEXED-FACE-SET objects.
It replaces the current texture coordinates in the rendering state for the
shapes to use.

Texture coordinates range from 0 to 1 across the texture. The horizontal
coordinate, called S, is specified first, followed by the vertical
coordinate, T."
  (with-vrml-node ("TextureCoordinate2" stream)
    (%conditional-write-field "point" point :2d-float-vector stream)))


;;;------------------------------------------------------------------- 
;;;
;;; LIGHTS
;;;

(define point-light-node (stream &key on intensity color location)
  "This node defines a point light source at a fixed 3D location. A point
source illuminates equally in all directions; that is, it is omni-
directional.

A light node defines an illumination source that may affect subsequent
shapes in the scene graph, depending on the current lighting style. Light
sources are affected by the current transformation. A light node under a
separator should not affect any objects outside that separator (although
some rendering systems do not currently support this).

Light intensity must be in the range 0.0 to 1.0, inclusive."
  (with-vrml-node ("PointLight" stream)
    (%conditional-write-field "on" on :bool stream)
    (%conditional-write-field "intensity" intensity :float stream)
    (%conditional-write-field "color" color :color stream)
    (%conditional-write-field "location" location :3d-float-vector stream)))

(define directional-light-node (stream &key on intensity color direction)
  "This node defines a directional light source that illuminates along rays
parallel to a given 3-dimensional vector.

A light node defines an illumination source that may affect subsequent
shapes in the scene graph, depending on the current lighting style. Light
sources are affected by the current transformation. A light node under a
separator does not affect any objects outside that separator.

Light intensity must be in the range 0.0 to 1.0, inclusive."
  (with-vrml-node ("DirectionalLight" stream)
    (%conditional-write-field "on" on :bool stream)
    (%conditional-write-field "intensity" intensity :float stream)
    (%conditional-write-field "color" color :color stream)
    (%conditional-write-field "direction" direction :3d-float-vector stream)))

(define spot-light-node (stream &key on intensity color location direction drop-off-rate cut-off-angle)
  "This node defines a spotlight light source. A spotlight is placed at a fixed
location in 3-space and illuminates in a cone along a particular DIRECTION.
The intensity of the illumination drops off exponentially as a ray of light
diverges from this direction toward the edges of the cone. The rate of
drop-off and the angle of the cone are controlled by the DROP-OFF-RATE and
CUT-OFF-ANGLE fields.

A light node defines an illumination source that may affect subsequent
shapes in the scene graph, depending on the current lighting style. Light
sources are affected by the current transformation. A light node under a
separator should not affect any objects outside that separator (although
some rendering systems do not currently support this).

Light intensity must be in the range 0.0 to 1.0, inclusive."
  (with-vrml-node ("SpotLight" stream)
    (%conditional-write-field "on" on :bool stream)
    (%conditional-write-field "intensity" intensity :float stream)
    (%conditional-write-field "color" color :color stream)
    (%conditional-write-field "location" location :3d-float-vector stream)
    (%conditional-write-field "direction" direction :3d-float-vector stream)
    (%conditional-write-field "dropOffRate" drop-off-rate :float stream)
    (%conditional-write-field "cutOffAngle" cut-off-angle :float stream)))


;;;------------------------------------------------------------------- 
;;;
;;; LEVEL OF DETAIL
;;;

(define-macro with-level-of-detail-group ((stream &key range center) &body body)
  "This group is used to allow applications to switch between various
representations of objects automatically. The inferiors of this node typically
represent the same object or objects at varying levels of detail, from highest
detail to lowest. LEVEL-OF-DETAIL must be used as a separator, since using
LEVEL-OF-DETAIL to side-effect state (such as color or translation) causes
implementation difficulties for browser writers. To ensure that
LEVEL-OF-DETAIL is used properly, we recommend that each inferior of the
LEVEL-OF-DETAIL be contained in its own Separator node.

The distance from the viewpoint (transformed into the local coordinate space
of the LEVEL-OF-DETAIL node) to the specified center point of the
LEVEL-OF-DETAIL is calculated, in object coordinates. If the distance is less
than the first value in the ranges array, then the first inferior of the
LEVEL-OF-DETAIL is drawn. If between the first and second values in the ranges
array, the second inferior is drawn, etc. If there are N values in the ranges
array, the LEVEL-OF-DETAIL should have N+1 inferiors.  Specifying too few
inferiors will result in the last inferior being used repeatedly for the
lowest levels of detail; if too many inferiors are specified, the extra
inferiors will be ignored. Each value in the ranges array should be greater
than the previous value, otherwise results are undefined.

Authors should set LEVEL-OF-DETAIL ranges so that the transitions from one
level of detail to the next are barely noticeable. Applications may treat the
range field as a hint, and might adjust which level of detail is displayed to
maintain interactive frame rates, to display an already-fetched level of
detail while a higher level of detail (contained in a WWWInline node) is
fetched, or might disregard the author-specified ranges for any other
implementation-dependent reason. Authors should not use LEVEL-OF-DETAIL nodes
to emulate simple behaviors, because the results will be undefined. For
example, using an LEVEL-OF-DETAIL node to make a door appear to open when the
user approaches probably will not work in all browsers.

It is expected that in a future version of VRML the LEVEL-OF-DETAIL node will
be defined to behave as a Separator node, not allowing its inferiors to affect
anything after it in the scene graph. To ensure future compatibility, it is
recommended that all inferiors of all LEVEL-OF-DETAIL nodes be Separator
nodes."  
  `(with-vrml-node ("LOD" ,stream)
     (%conditional-write-field "range" ,range :m-float ,stream)
     (%conditional-write-field "center" ,center :3d-float-vector ,stream)
     ,@body))


;;;------------------------------------------------------------------- 
;;;
;;; CAMERA
;;;

(define perspective-camera-node (stream &key position orientation focal-distance
                                        height-angle near-distance far-distance)
  "A perspective camera defines a perspective projection from a viewpoint. The
viewing volume for a perspective camera is a truncated right pyramid.

By default, the camera is located at (0,0,1) and looks along the negative
z-axis; the position and orientation fields can be used to change these
values. The height-angle field defines the total vertical angle of the
viewing volume.

See more on cameras in the OrthographicCamera description."
  (with-vrml-node ("PerspectiveCamera" stream)
    (%conditional-write-field "position" position :3d-float-vector stream)
    (%conditional-write-field "orientation" orientation :rotation stream)
    (%conditional-write-field "focalDistance" focal-distance :float stream)
    (%conditional-write-field "heightAngle" height-angle :float stream)
    (%conditional-write-field "nearDistance" near-distance :float stream)
    (%conditional-write-field "farDistance" far-distance :float stream)))

(define orthographic-camera-node (stream &key position orientation focal-distance
                                         height-angle near-distance far-distance)
  (with-vrml-node ("OrthographicCamera" stream)
    (%conditional-write-field "position" position :3d-float-vector stream)
    (%conditional-write-field "orientation" orientation :rotation stream)
    (%conditional-write-field "focalDistance" focal-distance :float stream)
    (%conditional-write-field "heightAngle" height-angle :float stream)
    (%conditional-write-field "nearDistance" near-distance :float stream)
    (%conditional-write-field "farDistance" far-distance :float stream)))

(setf (documentation 'orthographic-camera-node 'function)
      "An orthographic camera defines a parallel projection from a viewpoint.
This camera does not diminish objects with distance, as a PERSPECTIVE-CAMERA
does.  The viewing volume for an orthographic camera is a rectangular
parallele piped (a box).

By default, the camera is located at (0,0,1) and looks along the negative
z-axis; the position and orientation fields can be used to change these
values (orientation is a rotation from this angle). The height field defines
the total height of the viewing volume.

A camera can be placed in a VRML world to specify the initial location of
the viewer when that world is entered. VRML browsers will typically modify
the camera to allow a user to move through the virtual world.

The results of traversing multiple cameras are undefined; to ensure consistent
results, place multiple cameras underneath one or more Switch nodes, and set
the WITH-SWITCH's INFERIOR keyword so that only one is traversed.  By
convention, these non-traversed cameras may be used to define alternate entry
points into the world; these entry points may be named by simply giving the
cameras a name (using DEF); see WITH-ANCHOR-NOTED for a conventional way of
specifying an entry point in a URL.

Cameras are affected by the current transformation, so you can position a
camera by placing a transformation node before it in the scene graph . The
default position and orientation of a camera is at (0,0,1) looking along the
negative z-axis, with the positive y-axis up.

The position and orientation fields of a camera are sufficient to place a
camera anywhere in space with any orientation. The orientation field can be
used to rotate the default view direction (looking down -z-, with +y up) so
that it is looking in any direction, with any direction 'up'.

FOCAL-DISTANCE is not to be confused with focal length used to describe a lens
in optics. Instead, this is the distance from the Camera to a point in space
along the vector defined by the view direction given by the Camera's angles
and position. This point in space is where the `viewer' is focused attention.
This value is a hint only and may be used by a browser to define the speed of
travel for flying or walking.

For example, a FOCAL-DISTANCE of 5 means the object of primary concern is 5
meters from the camera and the browser should adjust flying speed to reach
that point in a reasonable amount of time. If the distance was 50 meters then
perhaps the browser can use this as a hint to travel 10 times faster.

HEIGHT-ANGLE of a PERSPECTIVE-CAMERA can be used to simulate a particular lens
and camera. To calculate a HEIGHT-ANGLE use the following formula:

        HEIGHT-ANGLE = (* 2.0 (atan (/ (/ HEIGHT 2) FOCAL-LENGTH)

Where height is the height of the scene at the desired FOCAL-LENGTH (distance
from position to center of attention).")


;;;------------------------------------------------------------------- 
;;;
;;; INFO
;;;

(define info-node (stream string)
  "This class defines an information node in the scene graph. This node has no
effect during traversal. It is used to store information in the scene graph,
typically for application-specific purposes, copyright messages, or other
strings.

New uses of named info nodes for conveying syntactically meaningfull
information are deprecated. By deprecated, we mean the feature is discouraged
and alternatives should be used if they exist. It is recommended that VRML's
extensibility features be used for extensions, not the Info node."
  (with-vrml-node ("Info" stream)
    (%write-field "string" string :string stream)))


;;;------------------------------------------------------------------- 
;;;
;;; EXAMPLES
;;;

#|

(define example-1 (stream)
  (with-vrml-world (:stream stream)
    (let-fields ((direction 3d-float-vector :x 0 :y 0 :z -1)
                 (camera-position 3d-float-vector :x 4.0 :y 2.0 :z 10.0)
                 (camera-orientation rotation
                                     :x 0.0 :y 0.0 :z 0.0
                                     :amount 0.0)
                 (diffuse-color1 color :red 1.0 :green 0.0 :blue 0.0)
                 (diffuse-color2 color :red 0.0 :green 0.0 :blue 1.0)
                 (translation 3d-float-vector :x -2.4 :y 0.2 :z 1)
                 (rotation rotation :x 0 :y 1 :z 1 :amount 0.9))
      (with-separator-group (stream)
        (directional-light-node stream :direction direction)
        (perspective-camera-node stream
                                 :position camera-position
                                 :orientation camera-orientation
                                 :focal-distance 10.84)
        (with-separator-group (stream)
          (material-node stream :diffuse-color diffuse-color1)
          (translation-node* stream 3 0 1)
          (sphere-node stream :radius 2.3))
        (with-separator-group (stream)
          (material-node stream :diffuse-color diffuse-color2)
          (transform-node stream
                          :translation translation
                          :rotation rotation)
          (cube-node stream))))))

(defun write-vrml-example (url stream)
  (http:with-successful-response (stream :vrml :content-location url)
    (example-1 stream)))

(http:export-url #u"/vrml.wrl"
                 :computed
                 :response-function #'write-vrml-example)

#+mcl
(time (with-open-file (out (ccl:choose-new-file-dialog)
                           :direction :output :if-exists :supersede)
        (example-1 out)))

|#
