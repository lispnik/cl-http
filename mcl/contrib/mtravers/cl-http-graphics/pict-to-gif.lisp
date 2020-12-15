(in-package :cl-user)

#| ######################################################################

 Write out PICT structures as GIF or JPEG files (using an external utility).

 Author: Michael Travers (mt@media.mit.edu)

 Last modified: Thursday, March 30 1995, 9:19pm

-------------------------------------------------------------------------

Convert PICTs to GIF or JPEG files, using 

Interface:
(pict-to-gif <pict-handle> <output-filename> &key <keys>)

Keywords:
:format        :gif or :jpeg (:gif is the default)
:transparency  t to generate a GIF with transparency  (nil is default)

Requires:
The presece of the clip2gif application (by Yves Piguet). To obtain this, see
the URL:

  http://iawww.epfl.ch/Staff/Yves.Piguet/clip2gif-home/clip2gif.html

Converting to JPEGs requires Quicktime to be installed as well.

Notes: 
This interface is somewhat crude and not suitable for production use
especially in a multi-threaded environment. The technique used for getting
the data to the clip2gif application requires switching different
applications to the foreground in succession -- very ugly. There are a couple
of possibilites for fixing this:

1) instead of using the clipboard, write a PICT file and pass a pointer to
it. There is code below to do this, but it isn't quite working. 

2) With the next release of clip2gif (0.5), it ought to be possible to
pass PICTs directly as AppleEvent parameters.

Also, the switching back and forth between apps can sometimes hang the
system. This is most common when MCL starts up clip2gif, and sometimes
happens after a GIF file is generated. I have no idea what the problem
is.


###################################################################### |#

(require :appleevent-toolkit)
(require :pict-scrap)

;;; The pict storage will get released when the scrap is overwritten
(defun pict-to-gif (pict gif-pathname &rest keys)
  (put-scrap :pict pict)
  (lisp-to-front)
  (apply #'convert-to-gif gif-pathname keys))

;;; clip2gif takes other parameters as well; they should be supported.
;;; input-file nil means that the input will come from the clipboard
(defun convert-to-gif (pathname &key (input-file nil) (format :gif) (transparency nil))
  (with-aedescs (appleevent reply target)
    (when (or (null (find-named-process "Clip2GIF"))
              (null input-file))      ; bring clip2gif to front to get clipboard
      (launch-clip2gif))
    (create-named-process-target target "Clip2GIF")
    ;; convert
    (if input-file
      (create-odoc appleevent target (list input-file))
      (create-appleevent appleevent #$kCoreEventClass #-ccl-3 #$kAEOpen #+ccl-3 #$kAEOpenDocuments target))
    ;; specify output file
    (with-aedescs (path-desc type-desc transparency-desc)
      ;; saving (fltp) as type 
      (create-type-descriptor type-desc 
                              (case format
                                (:gif :|GIFf|)
                                (:jpeg :|JPEG|)
                                (:pict :|PICT|)
                                (t (error "Unknown format ~A" format))))
      (ae-error (#_AEPutParamDesc appleevent :|fltp| type-desc))
      ;; in (kfil)
      (create-fsspec-record path-desc pathname)
      (ae-error (#_AEPutParamDesc appleevent :|kfil| path-desc))
      (create-boolean-descriptor transparency-desc transparency)
      (ae-error (#_AEPutParamDesc appleevent :|trcy| transparency-desc))
      (send-appleevent appleevent reply :reply-mode :wait-reply)
      nil)))

#|
This implements an alternate way to pass a pict to clip2gif: via a file.
However, it doesn't work -- Lisp hangs after the gif is generated. 

(defun pict-to-gif (pict gif-pathname &rest keys)
  (let ((pict-pathname (merge-pathnames gif-pathname :type "pict")))
    (pict-to-file pict pict-pathname)
    (apply #'convert-to-gif pict-pathname gif-pathname :input-file pict-pathname keys)
    (delete-file pict-pathname)))

(defun pict-to-file (pict-handle pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :element-type '(unsigned-byte 32))
    (dotimes (n 128) (write-byte 0 stream))     ; write the pict file header
    (let ((PICT-size (#_GetHandleSize pict-handle)))
      (with-dereferenced-handles ((pict-pointer pict-handle))
        (dotimes (long (ceiling pict-size 4))
          (write-byte (%get-unsigned-long pict-pointer (* 4 long)) stream)))))
  (set-mac-file-type pathname :|PICT|)
  (set-mac-file-creator pathname :|ttxt|))
|#


;;; Application launching

;;; will select if already launched (which is what we want)
(defun launch-clip2gif ()
  (multiple-value-bind (psnhi psnlo)
                       (launch-application :creator :|c2gf|)
    (wait-for-front-process psnhi psnlo)))

;;; from toolbox-ref
(defun launch-application (&key creator fsspec)
  (unless (= (count-if-not #'null (list creator fsspec))
             1)
        (error "invalid arguments"))
  (rlet ((theFsspec :fsspec))
    (when creator
      (%get-creator-path creator theFsspec)
      (setf fsspec theFsspec))
    (rlet ((lpb :LaunchParamBlockRec
              :launchBlockID #$extendedBlock
              :launchEPBLength #$extendedBlockLen
              :launchFileFlags 0
              :launchControlFlags (+ #$launchContinue #$launchNoFileFlags)
              :launchAppSpec fsspec
              :launchAppParameters (%null-ptr)))
    (if  (= (#_LaunchApplication lpb) #$noErr)
      (values (rref lpb :LaunchParamBlockRec.launchProcessSN.highLongOfPSN)
              (rref lpb :LaunchParamBlockRec.launchProcessSN.LowLongOfPSN))))))

;;; from toolbox-ref
(defun %get-creator-path (creator fsspec)
  (let ((devs (directory "*:")))
    (dolist (vrefnum (sort (mapcar 'volume-number devs) #'>))
      (rlet ((pb :DTPBRec
                 :ioNamePtr (%null-ptr)
                 :ioVRefnum vrefnum))
        (when (= (#_PBDTGetPath pb) #$noErr)
          (setf (rref pb :DTPBRec.ioNamePtr)
                (%inc-ptr fsspec (get-field-offset :fsspec.name))
                (pref pb :DTPBRec.ioIndex) 0
                (pref pb :DTPBRec.ioFileCreator) creator)
          (when (= (#_PBDTGetAPPL pb) #$noErr)
            (setf (pref fsspec :fsspec.vRefnum) vrefnum
                  (pref fsspec :fsspec.parID) (pref pb :DTPBRec.ioAPPLParID))
            ;; alanr: sometimes(often) the desktop files are damaged. Make sure that the
            ;; path returned actually exists before returning.
            (when (probe-file (%path-from-fsspec fsspec))
              (return-from %get-creator-path (values)))))))))

;;; Additional AEvent routines

(defun create-type-descriptor (the-desc type)
  (%stack-block ((dataptr 4))
    (%put-ostype dataptr type)
    (#_AECreateDesc #$typeType dataptr 4 the-desc)))

(defun create-boolean-descriptor (the-desc value)
  (%stack-block ((dataptr 4))
    (%put-long  dataptr (if value -1 0))
    (#_AECreateDesc #$typeBoolean dataptr 4 the-desc)))


;;; modified from create-alias-record, in finder-open.lisp
(defun create-fsspec-record (the-desc path)
  (let ((namestring (mac-namestring path)))
    (rlet ((fsspec :fsspec))
      (with-pstrs ((name namestring))
        (#_fsmakefsspec -1 -1 name fsspec))
      (#_AECreatedesc #$typeFSS fsspec (record-length :fsspec) the-desc)
      the-desc)))

;;; Processes

(defun get-current-process ()
  (rlet ((psn :ProcessSerialNumber))
    (#_GetCurrentProcess psn)
    (values (rref psn :ProceSsSerialNumber.highLongOfPSN)
            (rref psn :ProceSsSerialNumber.lowLongOfPSN))))

(defun get-front-process ()
  (rlet ((psn :ProcessSerialNumber))
    (#_GetFrontProcess psn)
    (values (rref psn :ProceSsSerialNumber.highLongOfPSN)
            (rref psn :ProceSsSerialNumber.lowLongOfPSN))))

(defun set-front-process (psnhi psnlo)
  (rlet ((psn :ProcessSerialNumber))
    (setf (rref psn :ProceSsSerialNumber.highLongOfPSN) psnhi
          (rref psn :ProceSsSerialNumber.lowLongOfPSN) psnlo)
    (#_SetFrontProcess psn)))

;;; wait for a process to come to the front
(defun wait-for-front-process (hi lo)
  (do ()
      ((multiple-value-bind (fronthi frontlo)
                            (get-front-process)
         (and (eql hi fronthi)
              (eql lo frontlo))))))

(defun lisp-to-front ()
  (multiple-value-bind (hi lo)
                       (get-current-process)
    (set-front-process hi lo)
    (wait-for-front-process hi lo)))
