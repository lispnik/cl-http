;;;-*- Syntax: Common-Lisp; Base: 10; Mode: lisp; Package: gd -*-

;;;**************************************************************************
;;; Provide a lisp interface to gd 1.2 
;;;
;;;  Rob Malouf <malouf@stanford.edu>
;;;             2-March-1997
;;;

;;; This is a first attempt at an interface with the libgd library for Allegro
;;; Common Lisp 4.3.  To install it, you first need to build libgd so that ACL
;;; can read it.  How you do that will depend on your system.  For example, if
;;; you use gcc under Solaris, you need to compile each source file with the
;;; "-fPIC" option and build the library using "ld -G".  The details on how to
;;; do this for your system are in chapter 10 of the ACL manual.  

;;; gd 1.2 was written by Thomas Boutell <boutell@boutell.com> and is
;;; currently distributed by boutell.com, Inc.

;;; gd 1.2 is copyright 1994, 1995, Quest Protein Database Center, Cold Spring
;;; Harbor Labs. Permission granted to copy and distribute this work provided
;;; that this notice remains intact. Credit for the library must be given to
;;; the Quest Protein Database Center, Cold Spring Harbor Labs, in all derived
;;; works. This does not affect your ownership of the derived work itself, and
;;; the intent is to assure proper credit for Quest, not to interfere with
;;; your use of gd. If you have questions, ask. ("Derived works" includes all
;;; programs that utilize the library. Credit must be given in user-visible
;;; documentation.)

;;***************************************************************************
;; Define package and export symbols

(defpackage "GD")
    
(provide 'gd)
(in-package gd)

(require :foreign)

;; ---> Change this path to point to where you've got libgd installed <---

(if (member :dlfcn *features*)
    (load "/usr/local/lib/libgd.so")
  (load "/usr/local/lib/libgd.o"))

;;***************************************************************************
;; Export functions from libgd

(ff:defforeign-list
    '((image-create
       :arguments (fixnum fixnum)
       :return-type :integer
       :unconverted-entry-name "gdImageCreate")
      (image-destroy
       :arguments (integer)
       :return-type :void
       :unconverted-entry-name "gdImageDestroy")
      (image-set-pixel
       :arguments (integer integer integer integer)
       :return-type :void
       :unconverted-entry-name "gdImageSetPixel")
      (image-get-pixel
       :arguments (integer integer integer)
       :return-type :integer
       :unconverted-entry-name "gdImageGetPixel")
      (image-line
       :arguments (integer integer integer integer integer integer)
       :return-type :void
       :unconverted-entry-name "gdImageLine")
      (image-rectangle
       :arguments (integer integer integer integer integer integer)
       :return-type :void
       :unconverted-entry-name "gdImageRectangle")
      (image-filled-rectangle
       :arguments (integer integer integer integer integer integer)
       :return-type :void
       :unconverted-entry-name "gdImageFilledRectangle")
      (image-bounds-safe
       :arguments (integer integer integer)
       :return-type :integer
       :unconverted-entry-name "gdImageBoundsSafe")
      (image-char
       :arguments (integer integer integer integer character integer)
       :return-type :void
       :unconverted-entry-name "gdImageChar")
      (image-char-up
       :arguments (integer integer integer integer character integer)
       :return-type :void
       :unconverted-entry-name "gdImageCharUp")
      (image-string 
       :arguments (integer integer integer integer string integer)
       :return-type :void
       :unconverted-entry-name "gdImageString")
      (image-string-up 
       :arguments (integer integer integer integer string integer)
       :return-type :void
       :unconverted-entry-name "gdImageStringUp")
      (image-polygon
       :arguments (integer integer integer integer)
       :return-type :void
       :unconverted-entry-name "gdImagePolygon")
      (image-filled-polygon
       :arguments (integer integer integer integer)
       :return-type :void
       :unconverted-entry-name "gdImageFilledPolygon")
      (image-color-allocate
       :arguments (integer integer integer integer)
       :return-type :integer
       :unconverted-entry-name "gdImageColorAllocate")
      (image-color-closest
       :arguments (integer integer integer integer)
       :return-type :integer
       :unconverted-entry-name "gdImageColorClosest")
      (image-color-exact
       :arguments (integer integer integer integer)
       :return-type :integer
       :unconverted-entry-name "gdImageColorExact")
      (image-color-deallocate
       :arguments (integer integer)
       :return-type :void
       :unconverted-entry-name "gdImageColorDeallocate")
      (image-color-transparent
       :arguments (integer integer)
       :return-type :void
       :unconverted-entry-name "gdImageColorTransparent")
      (image-arc
       :arguments (integer integer integer integer integer integer integer 
		   integer)
       :return-type :void
       :unconverted-entry-name "gdImageArc")
      (image-fill-to-border
       :arguments (integer integer integer integer integer)
       :return-type :void
       :unconverted-entry-name "gdImageFillToBorder")
      (image-fill
       :arguments (integer integer integer integer)
       :return-type :void
       :unconverted-entry-name "gdImageFill")
      (image-copy
       :arguments (integer integer integer integer integer integer integer 
		   integer)
       :return-type :void
       :unconverted-entry-name "gdImageCopy")
      (image-copy-resized
       :arguments (integer integer integer integer integer integer integer 
		   integer integer integer)
       :return-type :void
       :unconverted-entry-name "gdImageCopyResized")
      (image-set-brush
       :arguments (integer integer)
       :return-type :void
       :unconverted-entry-name "gdImageBrush")
      (image-set-tile
       :arguments (integer integer)
       :return-type :void
       :unconverted-entry-name "gdImageTile")
      (image-set-style
       :arguments (integer integer integer)
       :return-type :void
       :unconverted-entry-name "gdImageStyle")
      (image-interlace
       :arguments (integer integer)
       :return-type :void
       :unconverted-entry-name "gdImageInterlace")))

(export '(image-create image-destroy image-set-pixel image-get-pixel	
	  image-line image-rectangle image-filled-rectangle 
	  image-bounds-safe image-char image-char-up image-string 
	  image-string-up image-polygon image-filled-polygon 
	  image-color-allocate image-color-closest image-color-exact
	  image-color-deallocate image-color-transparent image-arc
	  image-fill-to-border image-fill image-copy
	  image-copy-resized image-set-brush image-set-tile image-set-style
	  image-interlace))

;;***************************************************************************
;; Provide access to image structure fields.

(ff:def-c-type (image :in-foreign-space) :struct
	       (pixels * :char)
	       (sx :int)
	       (sy :int)
	       (colors-total :int)
	       (red 256 :int)
	       (green 256 :int)
	       (blue 256 :int)
	       (get-transparent :int)
	       (polyInts :int)
	       (polyAllocated :int)
	       (brush :int)
	       (tile :int)
	       (brushColorMap :int)
	       (tileColorMap :int)
	       (styleLength :int)
	       (stylePos :int)
	       (style :int)
	       (get-interlaced :int))

(export '(image-sx image-sy image-colors-total image-red image-green
	  image-blue image-get-transparent image-get-interlaced))

;;***************************************************************************
;; Provide access to font structure fields.

(ff:def-c-type (ptr :in-foreign-space) * :integer)

(ff:def-c-type (font :in-foreign-space) :struct
	       (nchars :int)
	       (offset :int)
	       (w :int)
	       (h :int)
	       (data :int))

(export '(font-nchars font-w font-h))

(defparameter font-tiny 
    (ptr (ff:get-extern-data-address "gdFontTiny")))
(defparameter font-small 
    (ptr (ff:get-extern-data-address "gdFontSmall")))
(defparameter font-medium-bold 
    (ptr (ff:get-extern-data-address "gdFontMediumBold")))
(defparameter font-large 
    (ptr (ff:get-extern-data-address "gdFontLarge")))
(defparameter font-giant 
    (ptr (ff:get-extern-data-address "gdFontGiant")))

(export '(font-tiny font-small font-medium-bold font-large font-giant))

;;***************************************************************************
;; Define some constants

(defparameter dash-size 4)
(defparameter styled -2)
(defparameter brushed -3)
(defparameter styled-brushed -4)
(defparameter tiled -5)

(export '(dash-size styled brushed styled-brushed tiled))

;;***************************************************************************
;; Interface between gd's C library I/O and our streams 
;;
;; This is pretty tricky, and I bet it'll cause trouble some day.  What we do
;; is flush out all pending output to the stream, get its file descriptor,
;; duplicate it, open it as a stdio stream, do our I/O, and close the stream.

(ff:defforeign-list 
    '((dup
       :arguments (integer)
       :return-type :integer
       :unconverted-entry-name "dup")
      (fdopen
       :arguments (integer string)
       :return-type :integer
       :unconverted-entry-name "fdopen")
      (fclose
       :arguments (integer)
       :return-type :void
       :unconverted-entry-name "fclose")
      (c-image-gif 
       :arguments (integer integer)
       :return-type :void
       :unconverted-entry-name "gdImageGif")))

(defun image-gif (image stream)
  (force-output stream)
  (let* ((fd (dup (excl:stream-output-fn stream)))
	 (out (fdopen fd "w")))
    (c-image-gif image out)
    (fclose out)))

(export '(image-gif))
