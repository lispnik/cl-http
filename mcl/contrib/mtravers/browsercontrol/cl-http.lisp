;;; -*- Syntax: Ansi-Common-Lisp; Package: CCL; Base: 10; Mode: lisp -*-

(in-package :ccl)

#| #######################################################################

Manage some browser-control parameters from CL-HTTP's preference
system.

written by Rainer Joswig, repackaged by Mike Travers

####################################################################### |#

;(require :browser-control)
; (require :cl-http)

(http::define-preference-type
  :browser-preferences
  :name "Browser"
  :display-string "Local Browser"
  :inferiors (:browser)
  :description "Parameters related to the local browser.")

(http::define-preference :browser
  :name "Browser"
  :presentation-type `(www-present:member-sequence
                       (:Netscape-Navigator :Microsoft-Internet-Explorer))
  :prompt "Local Browser Name"
  :default-value-form :|MOSS|
  :get-value-form (ecase *browser-creator*
                    (:|MOSS| :Netscape-Navigator)
                    (:|MSIE| :Microsoft-Internet-Explorer))
  :set-value-form (setq *browser-creator*
                        (cond ((equal http::value :Netscape-Navigator) :|MOSS|)
                              ((equal http::value :Microsoft-Internet-Explorer) :|MSIE|)
                              (t (error "Unknown browser: ~a" http::value))))
  :description "The Browser you would like to use with your Mac.")

(pushnew :browser http::*remote-configuration-form-prototype*)

(defun get-current-browser ()
  (funcall (http::preference-value-getter (http::find-preference :browser))))

(defun set-current-browser (browser-name)
  (funcall (http::preference-value-setter (http::find-preference :browser))
           browser-name))

(defun select-and-set-browser ()
  (set-current-browser (first (select-item-from-list '(:Netscape-Navigator
                                                       :Microsoft-Internet-Explorer)
                                                     :window-title "Select Browser"
                                                     :selection-type :single))))
