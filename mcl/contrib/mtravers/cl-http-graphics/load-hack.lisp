;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

(in-package :cl-user)

;; access the URL #u"/fractal-form.html"

;; options are :load :compile :eval-load :compile-load-always, :compile-load
(define-system 
  (cl-http-graphics)
  (:compile-load)
  ;; load MCL libraries to support apple events 
  (:in-order-to-load :compile-load  "ccl:examples;appleevent-toolkit")
  (:in-order-to-load :compile-load  "ccl:examples;pict-scrap")
  ;; A utility for turning the Mac PICT structure into GIF (or JPEG) files, using
  ;; an external application controlled by AppleEvents.  See the file for info
  ;; on obtaining the external app.
  "http:mac;contributions;miketravers;cl-http-graphics;pict-to-gif"
  ;; Some minor extensions/patches to CL-HTTP that allow the example to work better.
  "http:mac;contributions;miketravers;cl-http-graphics;html-patches"
  ;; An example of a web page that displays computed graphics.
  "http:mac;contributions;miketravers;cl-http-graphics;fractal-form")


