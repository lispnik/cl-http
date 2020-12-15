;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10 -*-

(in-package :HTTP)

;;; Provided for user extensions.
;;; You can also use:
;;; (add-export-pathname "http:custom;your-exports")
;;; to add additional exports.
;;;

;;; Add AVI type video/msvideo
(http::define-url-export-types
    (:avi-video :avi (:video :msvideo) :copy-mode :binary
		:alternate-extensions (:avv)))
