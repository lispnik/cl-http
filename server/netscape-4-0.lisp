;;;   -*- Mode: lisp; Package: (netscape4.0 :use (future-common-lisp ns3.0 www-utils url)); BASE: 10; Syntax: ansi-common-lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-

;;;
;;; (c) Copyright 1997, John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; INTERFACE FOR AUTHORING HTML USING NETSCAPE 4.0 EXTENSIONS
;;;
;;; Issues:
;;;
;;; 1. Conditional comments (based on Javascript predicate evaluation) not implemented.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; PACKAGE DEFINITION
;;;

(eval-when (load eval compile)

  (defpackage :netscape4.0
    (:use future-common-lisp ns3.0 www-utils url)
    (:nicknames ns4.0)
    (:import-from html2
     "%WRITE-COMMAND-KEY-ARG"
     "%ISSUE-COMMAND"
     "%WITH-ENVIRONMENT"
     "ISSUE-COMMAND"
     "ISSUE-COMMAND*"
     "WITH-ENVIRONMENT")
    (:import-from ns2.0
     "JAVA-SCRIPT1.2")
    (:shadowing-import-from html3.2
      "*STANDARD-COLOR-SCHEME*"
      "COLOR-SCHEME"
      "CREATE-COLOR-SCHEME"
      "DECLARE-LINK"
      "ENUMERATE-ITEM-LIST"
      "ENUMERATING-ITEM"
      "WITH-CENTERING"
      "WITH-DIVISION"
      "WITH-ENUMERATION"
      "WITH-PARAGRAPH"
      "WITH-PARAGRAPH-STYLE"
      "WITH-SECTION-HEADING"
      "WITH-STANDARD-DOCUMENT-BODY")
    (:export
      "*STANDARD-COLOR-SCHEME*"
      "COLOR-SCHEME"
      "CREATE-COLOR-SCHEME"
      "DECLARE-LINK"
      "ENUMERATE-ITEM-LIST"
      "ENUMERATING-ITEM"
      "JAVA-SCRIPT1.2"
      "WITH-CENTERING"
      "WITH-DIVISION"
      "WITH-ENUMERATION"
      "WITH-LAYER"
      "WITH-PARAGRAPH"
      "WITH-PARAGRAPH-STYLE"
      "WITH-SECTION-HEADING"
      "WITH-STANDARD-DOCUMENT-BODY"
      "WITHOUT-LAYERS-CAPABILITY"))
   
  (let ((html-pkg (find-package :ns3.0))
        (netscape-pkg (find-package :netscape4.0)))
    (do-external-symbols (sym html-pkg)
      (export (intern (symbol-name sym) netscape-pkg) netscape-pkg)))
  )                                             ;close eval-when

(in-package :netscape4.0)

;;;------------------------------------------------------------------- 
;;;
;;; LAYERS
;;;
;;; http://developer.netscape.com/library/documentation/communicator/layers/index.htm

(defun %get-layer-class (arg)
  (ecase arg
    (:fixed "LAYER")
    (:in-flow "ILAYER")))

(defun %write-pixel-or-percent-argument (stream option value)
  (if (<= 0 value 1)
      (let ((w (concatenate 'string
                            (write-to-string (floor (* value 100)) :base 10. :escape nil)
                            "%")))
        (declare (dynamic-extent w))
        (%write-command-key-arg stream option w))
      (%write-command-key-arg stream option value t)))

(defun %make-layer-clip-args (clip-x1 clip-y1 clip-x2 clip-y2)
  (flet ((prepare-arg (value)
           (cond ((<= 0 value 1)
                  (let ((val (write-to-string (floor (* value 100)) :base 10. :escape nil)))
                    (declare (dynamic-extent val))
                    (concatenate 'string val "%")))
                 (t (check-type value integer)
                    value))))
    (values (prepare-arg clip-x1)
            (prepare-arg clip-y1)
            (prepare-arg clip-x2)
            (prepare-arg clip-y2))))

(defun %write-layer-arguments (stream name reference visibility background background-url
                                      x-page-origin y-page-origin x-origin y-origin width height z-index parent
                                      clip-x1 clip-y1 clip-x2 clip-y2 events)
  (cond-every
    (name (%write-command-key-arg stream "NAME" name))
    (x-page-origin (%write-pixel-or-percent-argument stream "PAGEX" x-page-origin))
    (y-page-origin (%write-pixel-or-percent-argument stream "PAGEY" y-page-origin))
    (x-origin (%write-pixel-or-percent-argument stream "LEFT" x-origin))
    (y-origin (%write-pixel-or-percent-argument stream "TOP" y-origin))
    (background
      (%write-command-key-arg stream "BGCOLOR" (color-mapping background t)))
    (background-url
      (%write-command-key-arg stream "BACKGROUND" (url:coerce-url-string background-url)))
    (reference
      (%write-command-key-arg stream "SRC" (url:coerce-url-string reference t)))
    (width (%write-command-key-arg stream "WIDTH" width t))
    (height (%write-command-key-arg stream "height" height t))
    (z-index
      (typecase z-index
        (integer (%write-command-key-arg stream "Z-INDEX" z-index t))
        (t (check-type parent string)
           (unless parent
             (error "When z-index is ~S, PARENT must be specified." z-index parent))
           (%write-command-key-arg stream
                                   (ecase z-index
                                     (:above "ABOVE")
                                     (:below "BELOW"))
                                   parent))))
    (visibility
      (unless (member visibility '(:show :hide :inherit))
        (error "visibility is ~S, which is not one of :SHOW, :HIDE or :INHERIT." visibility))
      (%write-command-key-arg stream "VISIBILITY" visibility))
    ((or clip-x2 clip-y2)
     (cond ((and clip-x2 clip-y2 clip-x1 clip-y1)
            (multiple-value-bind (x1 y1 x2 y2)
                (%make-layer-clip-args clip-x1 clip-y1 clip-x2 clip-y2)
              (declare (dynamic-extent x1 y1 x2 y2))
              (fast-format stream " CLIP=~D,~D,~D,~D" x1 y1 x2 y2)))
           (t (error "WITH-LAYER: Incomplete arguments for clipping box:~
                      CLIP-X1: ~S~&CLIP-Y1: ~S~&CLIP-X2: ~S~&CLIP-Y2:~S"
                     clip-x1 clip-y1 clip-x2 clip-y2))))
    (events
      (dolist (event events)
        (html2::%write-input-type-event-arg stream event)))))

(define-macro with-layer ((&key name (class :fixed) reference visibility background background-url
                                x-page-origin y-page-origin x-origin y-origin z-index parent width height
                                (clip-x1 0) (clip-y1 0) clip-x2 clip-y2 events (stream '*output-stream*)) &body body)
  "Establishes a layer in the HTML output on STREAM.
Layers provide idependent control of different regions of an HTML page and allow
overlay operations. Javascript operations make it possible to manipulate layers
dynamically on the client side. 
See: http://developer.netscape.com/library/documentation/communicator/layers/index.htm

NAME (optional) is a string identifying the layer.
CLASS (optional) is either :FIXED or :IN-FLOW.
REFERENCE (optional) is a URL containing HTML for display in the layer.

VISIBILITY (optional) is one of :SHOW, :HIDE or :INHERIT. The default is to
inherit the visibility of the parent.

BACKGROUND  (optional) is a color for the background.
BACKGROUND-URL (optional) is  an image URL to use as the background. 
Layers are transparent so that lower layers show through transparent areas.
These attributes will make a layer opaque.

X-PAGE-ORIGIN (optional) is the x-origin in pixels relative to the enclosing document. 
Y-PAGE-ORIGIN (optional) is the y-origin in pixels relative to the enclosing document.

X-ORIGIN (optional) is the X-origin in pixels relative to the enclosing layer. 
Y-ORIGIN (optional) is the Y-origin in pixels relative to the enclosing layer.

For positioned layers, the origin is the the upper-left hand corner of layer
containing the layer. Coordinates increase downwards and rightwards.  For
in-flow layers, the origin is the current position in the broswer rendering
when the layer is encountered. X and Y provides offset from the default
positioning.

WIDTH (optional) is the number of pixels on the horizontal axis before
wrapping begins.  If elements like images extend beyond width, the layer
will be extended horizontally.

HEIGHT (optional) is the number of pixels on the vertical axis.  If the
contents do not fit within HEIGHT, the layer will be extended vertically.
HEIGHT is intended to provide a reference height for percentage attributes of
inferior layers. HEIGHT defaults to the minimum height that contains all the
contents.

Z-INDEX (optional) is the position of the layer relative to other layers. The
default behavior is to stack new layers above previous layers. Z-INDEX can be
a number indicating the stacking of the layers.  Positive values place the
layer above its parent whereas negative values place it below. If Z-INDEX is
either :ABOVE or :BELOW, the PARENT argument must supply the name of an
earlier layer.

The clipping box is the visible area within the layer. CLIP-X1, CLIP-Y1 is the
origin and CLIP-X2, CLIP-Y2 is the extent of the box. All values are in
pixels.

Coordinate parameters may be specified as proportions relative to the
corresponding component of the enclosing layer or document. A proportion is a
float between zero and 1. Parameters accepting floats are: X-PAGE-ORIGIN,
Y-PAGE-ORIGIN, X-ORIGIN, Y-ORIGIN, WIDTH, HEIGHT, CLIP-X1, CLIP-Y1, CLIP-X2,
CLIP-Y2.

EVENTS (optional) is a list of client-side events processed when the form is
submitted."
  `(%with-environment (,(typecase class
                          (keyword
                            (%get-layer-class class))
                          (t `(%get-layer-class ,class)))
                       :stream ,stream)
                      (%write-layer-arguments
                        ,stream ,name ,reference ,visibility ,background ,background-url
                        ,x-page-origin ,y-page-origin ,x-origin ,y-origin ,width ,height ,z-index
                        ,parent ,clip-x1 ,clip-y1 ,clip-x2 ,clip-y2 ,events)
     ,@body))

(define-macro without-layers-capability ((&key (stream '*output-stream*)) &body body)
  "Any HTML emitted within BODY is ignored by layers capable clients but can be used
to provide alternate displays for clients without layers capability."
  `(with-environment ("NOLAYER" :stream ,stream)
     ,@body))
