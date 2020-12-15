;;; -*- Syntax: Ansi-Common-Lisp; Base: 10; Package: images; Mode: lisp -*-


D,#TD1PsT[Begin using 006 escapes](1 0 (NIL 0) (NIL :ITALIC NIL) "CPTFONTI");;based on the command IMAGES:SHOW-IMAGE
0(defun (2 0 (NIL 0) (NIL :BOLD NIL) "CPTFONTCB")show-image 0(image &key source-rectangle destination-position (scale 1) (stream *standard-output*))
  (unless source-rectangle
    (multiple-value-bind (width height)
	(image-size image)
      (setq source-rectangle (list 0 0 width height))))
  (unless destination-position
    (setq destination-position :cursor))
  1;;
0  (cond ((not (operation-handled-p stream :draw-image-instance))
	 ;; :Output destination someplace not a window.
	 (with-image-raster (raster image)
	   (destructuring-bind (left top right bottom) source-rectangle
	     (let ((height (- bottom top)))
	       (graphics:with-room-for-graphics (t (ceiling (* height scale)))
		 (graphics:draw-image raster 0 (- height) :scale scale :scale-y -1
				      :image-left left :image-top top
				      :image-right right :image-bottom bottom))))))
	((neq destination-position :cursor)
	 (lexpr-send stream :draw-image-instance image
		     (first destination-position) (second destination-position)
		     scale source-rectangle))
	(t
	 (fresh-line)   
	 (multiple-value-bind (nil y)
	     (send stream :read-cursorpos)
	   (lexpr-send stream :draw-image-instance image 0 y
		       scale source-rectangle)
	   (dw:with-output-truncation ()
	     (send stream :set-cursorpos nil
		   (+ y
		      (destructuring-bind (ignore top ignore bottom) source-rectangle
			(let ((height (- bottom top)))
			  (ceiling (* height scale))))
		      (send stream :line-height))))))))

(defmethod 2(read-image-in-format image-file-format)0 (stream &rest options)
  (unless read-function
    (error "The ~A format cannot read files." self))
  (destructuring-bind
    (image width height &rest args)
      (multiple-value-list (apply read-function stream options))
    (ecase calling-sequence
      (:image
	image)
      (:raster
	(apply #'make-image-from-raster image :width width :height height (copy-list args))))))

(defun 2read-image 0(name format stream)
  (let ((image (read-image-in-format (find-image-file-format format) stream)))
    (cond-every
      ((null (image-name image))
       (setf (image-name image) (unique-image-name name))))
    (setf (image-file image) nil		1;no file
0	  (image-file-format image) format)
    (set-default-image image)
    image))
