(in-package :http-user)

#| ######################################################################

 Edge replacement fractals over the web

 Author: Michael Travers (mt@media.mit.edu)

 Last modified: Sunday April 2,1995 9:48pm

-------------------------------------------------------------------------

The method and some examples are from Prusinkiewcz & Lindenmayer, The
Algorithmic Beauty of Plants, Springer-Verlag 1990.  There are much prettier
pictures to be generated from the techniques in that book -- this was a 
quick hack to get some generated web graphics out there.

Interface:
Via URL http:<your-host>//fractal-form.html

Requires:
- CL-HTTP 
- pict-to-gif.lisp
- http-patches.lisp

Todo:
- real user documentation
- figure out how to clean up files
- speed up
- add invisible edges or pen-up/down commands
- get rid of window; draw offscreen


###################################################################### |#


(defun d2r (degrees)
  (* degrees #.(/ pi 180)))

(defclass fractal ()
  ((name :initarg :name :initform nil :reader fractal-name)
   (depth :initarg :depth)
   (angle :initarg :angle)
   (start :initarg :start)
   (productions :initarg :productions)))

(defvar *library* nil)

(defun deffractal (name depth angle start productions)
  (if (find-fractal-by-name name)
    (error "There is already a fractal named ~A" name)
    (push (make-instance 'fractal 
            :name name :depth depth :angle angle :start start :productions productions)
          *library*)))

(defun find-fractal-by-name (name)
  (find name *library* :key #'fractal-name :test #'string-equal))

(defmethod draw-fractal ((f fractal) sheet-xsiz sheet-ysiz)
  (with-slots (depth angle start productions) f
    (let ((dtheta (d2r angle))
          theta x y drawing side-length 
          (xmax 0) (xmin 0) (ymax 0) (ymin 0))
      (labels ((draw-fractal-line-1 (type depth)
                 (if (zerop depth)
                   (forward (case type
                              (red ccl:*red-color*)
                              (blue ccl:*blue-color*)
                              (green ccl:*green-color*)
                              (black ccl:*black-color*)
                              (t (error "I don't know how to ~A" type))))
                   (dolist (element (or (cadr (assoc type productions))
                                        (error "I don't know how to ~A" type)))
                     (doit element (1- depth)))))
               (doit (command depth)
                 (case command
                   (right (incf theta dtheta))
                   (left (decf theta dtheta))
                   (t (if (member command productions :key #'car)
                        (draw-fractal-line-1 command depth)
                        (error "I don't know how to ~A" command)))))
               (forward (color)
                 (incf x (* side-length (cos theta)))
                 (incf y (* side-length (sin theta)))
                 (if drawing
                   (ccl:with-fore-color color
                     (#_LineTo (round x) (round y)))
                   (setf xmax (max x xmax)
                         xmin (min x xmin)
                         ymax (max y ymax)
                         ymin (min y ymin)))))
        (setf x 0 y 0 theta 0 side-length 1)
        (setf drawing nil)
        (dolist (command start)
          (doit command depth))
        (let* ((drawing-xsiz (- xmax xmin))
               (drawing-ysiz (- ymax ymin))
               (scale (min (/ (1- sheet-xsiz) drawing-xsiz)
                           (/ (1- sheet-ysiz) drawing-ysiz))))
          (setf drawing t)
          (setf x (* scale (- xmin))
                y (* scale (- ymin))
                side-length scale
                theta 0)
          (#_MoveTo (round x) (round y))
          (dolist (command start)
            (doit command depth)))))))

;;; ideally this should be an offscreen gworld.
(defvar *fractal-window* (make-instance 'ccl:window :view-size #@(400 400) :window-title "Fractal Window"))

(defmacro make-picture (view &body body)
  `(ccl:rlet ((rect :rect :topleft #@(0 0) :bottomright (ccl:view-size ,view)))
     (ccl:with-focused-view ,view
     (let ((picture (#_OpenPicture rect)))
       ,@body
       (#_ClosePicture)
       picture))))

(defmethod make-pict ((f fractal) xsiz ysiz)
  (ccl:set-view-size *fractal-window* xsiz ysiz)
  (make-picture *fractal-window*
    (draw-fractal f xsiz ysiz)))

(defmethod view ((f fractal) &optional (xsiz 400) (ysiz 400))
  (let ((w (make-instance 'ccl:window :view-size (ccl:make-point xsiz ysiz) :window-title "Fractal")))
    (ccl:with-focused-view w
      (draw-fractal f xsiz ysiz))))

;;; compatibility with old system
(defun make-fancy-fractal (spec depth &optional x y z)
  (declare (ignore x y z))
  (view (make-instance 'fractal 
          :angle (car spec)
          :start (caddr spec)
          :productions (cadddr spec)
          :depth depth)))

;;; minimal!
(defparameter *user-doc*
  "This is a fractal generator that works via edge replacement.  
To start, choose a library entry and press the Library button. This will fill 
in the form and give you an example of the specification language.")

;;; HTTP server stuff
(defmethod compute-fractal-form ((url url:http-form) stream &optional image-url fractal)
   (labels ((fractal-slot (slotname)
                  (when fractal (slot-value fractal slotname)))
                (fractal-slot-string (slotname)
                   (when fractal (princ-to-string (slot-value fractal slotname))))
                (fractal-production-string (production-name)
                   (let ((production (find production-name (fractal-slot 'productions) :key #'car)))
                      (when production
                          (princ-to-string (cadr production))))))
      (with-successful-response (stream :html :expires (url:expiration-universal-time url))
          (html:with-html-document (:stream stream)
             (html:with-document-preamble (:stream stream)
	        (html:declare-base-reference url :stream stream)
	        (html:declare-title "Fractal Form" :stream stream))
             (html:with-document-body (:stream stream)
	        (html:with-section-heading ("Fractal Form" :stream stream)
                   (write-string *user-doc* stream)
	           (html:with-fillout-form (:post url :stream stream)
	              (html:with-paragraph (:stream stream)
	                 (with-rendition (:bold :stream stream)
		             (fresh-line stream)
		             (write-string "Depth:  " stream))
                         (html:accept-input 'html:radio-button "Depth" 
                                                      :choices '(("0" . 0) ("1 " . 1) ("2 " . 2) ("3 " . 3) ("4 " . 4) ("5 " . 5) ("6 " . 6))
                                                      :default (fractal-slot 'depth)
                                                      :linebreaks nil
                                                      :inline t
                                                      :stream stream))
                      (html:with-paragraph (:stream stream)
	                 (with-rendition (:bold :stream stream)
		             (fresh-line stream)
		             (write-string "Angle:  " stream))
                         (html:accept-input 'html:radio-button "Angle" 
                                                      :choices '(("60 " . 60) ("90 " . 90) ("Other " . other))
                                                      :default (fractal-slot 'angle)
                                                      :linebreaks nil
                                                      :inline t
                                                      :stream stream)
                         (html:accept-input 'html:string "Other-angle"
                                                      :default (and fractal
                                                                          (not (member (fractal-slot 'angle) '(60 90)))
                                                                          (fractal-slot-string 'angle))
                                                      :stream stream
                                                      :size 10))
                      (html:with-paragraph (:stream stream)
	                 (with-rendition (:bold :stream stream)
		             (fresh-line stream)
		             (write-string "Starting shape " stream))
                         (html:accept-input 'html:string "start"
                                                      :default (fractal-slot-string 'start)
                                                      :stream stream))
                      (html:with-paragraph (:stream stream)
	                 (with-rendition (:bold :stream stream)
		             (fresh-line stream)
		             (write-string "Black " stream))
                         (html:accept-input 'html:string "black"
                                                      :default (fractal-production-string 'black)
                                                      :size 70
                                                      :stream stream))
                      (html:with-paragraph (:stream stream)
	                 (with-rendition (:bold :stream stream)
		             (fresh-line stream)
		             (write-string "Red " stream))
                         (html:accept-input 'html:string "red"
                                                      :size 70
                                                      :default (fractal-production-string 'red)
                                                      :stream stream))
                      (html:with-paragraph (:stream stream)
	                 (with-rendition (:bold :stream stream)
		             (fresh-line stream)
		             (write-string "Blue " stream))
                         (html:accept-input 'html:string "blue"
                                                      :size 70
                                                      :default (fractal-production-string 'blue)
                                                      :stream stream))
                      (html:with-paragraph (:stream stream)
	                 (with-rendition (:bold :stream stream)
		             (fresh-line stream)
		             (write-string "Green " stream))
                         (html:accept-input 'html:string "green"
                                                      :size 70
                                                      :default (fractal-production-string 'green)
                                                      :stream stream))
                      (html:accept-input 'html:submit-button "Submit" :display-string "Use" :stream stream)
                      (write-string " the above parameters, or " stream)
                      (html:accept-input 'html:reset-button "Reset" :stream stream)
                      (write-string " them, or " stream)
                      (html:accept-input 'html:submit-button "Enter" :display-string "Enter" :stream stream)
                      (write-string " them into the library as " stream)
                      (html:accept-input 'html:string "Name" :size 20 :stream stream)
                      (write-string "." stream)
	              (html:horizontal-line :stream stream)
                      (html:with-paragraph (:stream stream)
                         (with-rendition (:bold :stream stream)
                             (fresh-line stream)
                             (write-string "Or choose from the library: " stream))
                         (html:accept-input 'html:radio-button "Library-entry" 
                                                      :choices (mapcar #'(lambda (f) (cons (fractal-name f) (fractal-name f))) *library*)
                                                      :default (fractal-slot 'name)
                                                      :linebreaks nil
                                                      :stream stream))
                      (html:with-paragraph (:stream stream)
                         (html:accept-input 'html:submit-button "Library" :display-string "Library" :stream stream))
                      (html:horizontal-line :stream stream)
                      (when image-url
                          (netscape:with-centering (:stream stream)
                             (image image-url "a computed fractal" :stream stream))
                          ;; store as [Name]
                          ))))))))


(defun process-param (param)
  (let ((*read-eval* nil))              ; frustrate sharpies
    (when param
      (read-from-string param))))

;;; the files that get generated should be deleted -- but when?
(defmethod respond-to-fractal-form  ((url url:http-form) stream query-alist)
  (bind-query-values (depth angle other-angle start black red blue green library library-entry enter name)
                     (url query-alist)
    (let* ((fractal (if library
                      (or (and library-entry (find-fractal-by-name library-entry))
                          (error "Library entry ~A not found" library-entry))
                      (make-instance 'fractal
                        :name name
                        :depth (process-param depth)
                        :angle (if (string-equal angle "other")
                                 (process-param other-angle)
                                 (process-param angle))
                        :start (process-param start)
                        :productions (list (list 'black (process-param black))
                                           (list 'red (process-param red))
                                           (list 'blue (process-param blue))
                                           (list 'green (process-param green))))))
           (pict (make-pict fractal 400 400))
           (image-name (string (gensym "fractal")))
           (pathname (make-pathname :host "http" :directory "www" :name image-name :type "gif"))
           (image-url (parse-url (merge-url (concatenate 'string "/" image-name ".gif")))))
      (when enter
        (push fractal *library*))
      (cl-user::pict-to-gif pict
                            pathname
                            :transparency t)
      (export-url image-url :gif-image :pathname pathname)
      (compute-fractal-form url stream image-url fractal))))

(deffractal "Hex Gosper" 4 60 '(red)
 '((red (red left blue left left blue right red right right red red right blue left))
   (blue (right red left blue blue left left blue left red right right red right blue))))

(deffractal "Fancy Quilt" 4 90 
  '(red right red right red right red)
  '((red (red green right red right red right red right blue red))
    (blue (blue red left blue left blue left blue left green blue))
    (green (red right red left red left red red left red left red left red red))))
    
(deffractal "Brick" 4 90 '(red right red right red right red)
  '((red (red blue left red right red left red left blue red))
    (blue (red blue left blue right blue left red left red red))))

(deffractal "Necker Ring" 4 90 '(black right black right black right black right black right black)
  '((black (black left black left left black right black))))

(deffractal "Sierpinski Gasket" 6 60 '(red)
  '((red (blue left red left blue))
    (blue (red right blue right red))))
    

(export-url #u"/fractal-form.html"
	    :html-computed-form
	    :form-function #'compute-fractal-form
	    :expiration '(:no-expiration-header)
	    :response-function #'respond-to-fractal-form)


