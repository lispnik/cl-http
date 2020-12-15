; -*- Syntax: ansi-common-lisp; Base: 10; Mode: lisp; Package: ccl; -*-
;
; launch-browser.lisp - Launch a browser (defaults to NetScape) & send it a URL
; Example call: (launch-browser :url "http://www.apple.com/default.html")

(in-package "CCL")

(require "APPLEEVENT-TOOLKIT")

; returns vrefnum, fsid
(defun vol-info (path-or-num)  
  (%stack-iopb (pb np)
    (%put-volume-name np path-or-num)
    (%put-word pb (if (%izerop (%get-byte np)) 0 -1) $ioVolIndex)
    (%put-word pb 0 $ioVRefNum)
    (file-errchk (#_PBHGetVInfoSync pb) path-or-num)
    (values (%get-signed-word pb $ioVRefNum)
            (rref pb :HVolumeParam.ioVFSID))))

; check local volumes first
(defun get-creator-path (creator &aux local-vols other-vols)
  (dolist (volume (directory "*:"))
    (multiple-value-bind (vrefnum fsid)
                         (vol-info volume)
      (if (zerop fsid)
        (push vrefnum local-vols)
        (push vrefnum other-vols))))
  (dolist (vrefnum (nconc (sort local-vols #'>)
                          (sort other-vols #'>)))
    (rlet ((pb :DTPBRec
               :ioNamePtr (%null-ptr)
               :ioVRefnum vrefnum)
           (fsspec :fsspec))
      (when (eql 0 (#_PBDTGetPath pb))
        (setf (rref pb :DTPBRec.ioNamePtr) (%inc-ptr fsspec #.(get-field-offset :fsspec.name))
              (pref pb :DTPBRec.ioIndex) 0
              (pref pb :DTPBRec.ioFileCreator) creator)
        (when (eql 0 (#_PBDTGetAPPL pb))
          (setf (pref fsspec :fsspec.vRefnum) vrefnum
                (pref fsspec :fsspec.parID) (pref pb :DTPBRec.ioAPPLParID))
          (let ((path (probe-file (%path-from-fsspec fsspec))))
            (when path
              (return path))))))))

(defun launch-application (filename &optional stay-in-background-p)
  (rlet ((fsspec :FSSpec)
         (pb :launchParamBlockRec
             :launchBlockID #$extendedBlock
             :launchEPBLength #$extendedBlockLen
             :launchControlFlags (+ #$launchContinue
                                      #$launchNoFileFlags
                                      (if stay-in-background-p #$launchDontSwitch 0))
             :launchAppSpec fsspec
             :launchAppParameters (%null-ptr)))
    (with-pstrs ((name (mac-namestring (probe-file filename))))
      (#_FSMakeFSSpec 0 0 name fsspec))
    (when (zerop (#_LaunchApplication pb))
      filename)))

(defun create-sign-target (the-desc signature)
  (multiple-value-bind (psnhigh psnlow)
                       (find-process-with-signature signature)
    (when psnhigh
      (create-psn-target the-desc psnhigh psnlow))))

(defun create-ourl (the-desc the-target string &rest create-keywords)
  (declare (dynamic-extent create-keywords))
  (apply 'create-appleevent the-desc :|WWW!| :|OURL| the-target
         create-keywords)
  (ae-put-parameter-char the-desc #$keyDirectObject string))

(defun try-for-n-seconds (seconds thunk)
  (let* ((end-time (ceiling (+ (#_LMGetTicks) (* seconds 60))))
         (result nil)
         (wait-function #'(lambda ()
                            (or (setq result (funcall thunk))
                                (> (#_LMGetTicks) end-time)))))
    (declare (dynamic-extent wait-function))
    (process-wait (format nil "~A second wait" seconds) wait-function)
    result))

(defun send-ourl-to-netscape (command-string)
  (with-aedescs (appleevent reply target)
    (when (try-for-n-seconds 30 #'(lambda ()
                                    (create-sign-target target :MOSS)))
      (create-ourl appleevent target command-string)
      (send-appleevent appleevent reply :reply-mode :no-reply)
      (ae-get-parameter-char reply #$keyDirectObject nil))))

(defun launch-browser (&key (signature :|MOSS|) url stay-in-background-p)
  (let ((app (get-creator-path signature)))
    (when app
      (launch-application app stay-in-background-p))
    (when url
      (send-ourl-to-netscape url))))

; end
