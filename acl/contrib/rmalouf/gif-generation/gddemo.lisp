;;;-*- Syntax: Common-Lisp; Base: 10; Mode: lisp; Package: user -*-

;;;**************************************************************************
;;; Demonstration of the lisp interface to gd 1.2 
;;;
;;;  Rob Malouf, 2-March-1997
;;;

;;; This is a Lisp version of "gddemo.c"

;; Create output image, 128 by 128 pixels.
(setq im-out (gd:image-create 128 128))

;; First color allocated is background. 
(setq white (gd:image-color-allocate im-out 255 255 255))

;; Set transparent color.
(gd:image-color-transparent im-out white)

(setq red (gd:image-color-allocate im-out 255 0 0))
(setq green (gd:image-color-allocate im-out 0 255 0))
(setq blue (gd:image-color-allocate im-out 0 0 255))

;; Rectangle
(gd:image-line im-out 8 8 120 8 green)	
(gd:image-line im-out 120 8 120 120 green)	
(gd:image-line im-out 120 120 8 120 green)	
(gd:image-line im-out 8 120 8 8 green)	

;; Circle 
(gd:image-arc im-out 64 64 30 10 0 360 blue)

;; Arc
(gd:image-arc im-out 64 64 20 20 45 135 blue)

;; Flood fill
(gd:image-fill im-out 4 4 blue)

;; Text 
(gd:image-string im-out gd:font-giant 16 16 "hi" red)
(gd:image-string-up im-out gd:font-small 32 32 "hi" red)

;; Make output Image- interlaced (allows "fade in" in some viewers,
;; and in the latest web browsers) 
(gd:image-interlace im-out 1)

(setq out  (open "demoout.gif" :direction :output))

;; Write GIF 
(gd:image-gif im-out out)
(close out)
(gd:image-destroy im-out)

