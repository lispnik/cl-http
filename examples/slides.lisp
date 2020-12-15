;;;   -*- Mode: LISP; Package: (slides :use (future-common-lisp ns1.1)); BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-

;;;
;;; (c) Copyright  1995, John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; WEB SLIDE SHOWS
;;;
;;; This is a quick hack that allows you to put up Slides for talks on the Web
;;; and display them via Netscape 1.1N or clients supporting Netscape's
;;; extensions. See the end of the file for an example that exercises the
;;; features.

(eval-when (load eval compile)
   
  (defpackage slides
    (:use future-common-lisp ns1.1)
    (:export "DEFINE-SLIDE-SHOW")))             ;close eval-when

(in-package :slides)

(defvar *slides-package* (find-package :slides))

(defun coerce-to-package (symbol &optional (package *slides-package*))
  (if (eq (symbol-package symbol) package)
      symbol
      (intern (symbol-name symbol) package)))


;;;------------------------------------------------------------------- 
;;;
;;; CLASSES
;;;

(defclass background-mixin
          ()
    ((background :initform nil :initarg :background :accessor slide-background)
     (background-url :initform nil :initarg :background-url :accessor slide-background-url)
     (foreground :initform nil :initarg :foreground :accessor slide-foreground)
     (link :initform nil :initarg :link :accessor slide-link)
     (visited-link :initform nil :initarg :visited-link :accessor slide-visited-link)
     (active-link :initform nil :initarg :active-link :accessor slide-active-link)))

(defclass auto-advance-mixin
          ()
    ((display-interval :initform nil :initarg :display-interval :accessor display-interval)))

(defclass slide-show
          (background-mixin)
    ((name :initform nil :initarg :name :accessor slide-show-name)
     (url :initform nil :initarg :url :accessor slide-show-url)
     (number-of-slides :initform 0 :initarg :number-of-slides :accessor number-of-slides)
     (slides :initform nil :initarg slides :accessor slide-show-slides)
     (display-interval :initform nil :initarg :display-interval :accessor display-interval)
     (continuous-p :initform nil :initarg :continuous-p :accessor slide-show-continuous-p)))

(defclass basic-slide
          ()
    ((number :initform 0 :initarg :number :accessor slide-number)
     (slide-show :initform nil :initarg :slide-show :accessor slide-show)))

(defclass title-mixin
          ()
    ((title :initform nil :initarg :title :accessor title)
     (icon :initform nil :initarg :icon :accessor icon)
     (line :initform t :initarg :line :accessor line)))

(defclass slide
          (auto-advance-mixin title-mixin background-mixin basic-slide)
    ((body :initform nil :initarg :body :accessor slide-body)))

(defclass title-slide
          (title-mixin auto-advance-mixin background-mixin basic-slide)
    ((author :initform nil :initarg :author :accessor author)
     (institution :initform nil :initarg :institution :accessor institution)))

(defclass bullet
          ()
    ((text :initform nil :initarg :text :accessor text)
     (icon :initform nil :initarg :icon :accessor icon)))

(defclass bullets
          ()
    ((bullets :initform nil :initarg bullet :accessor bullets)
     (enumeration-style :initform nil :initarg :enumeration-style :accessor enumeration-style)))

(defclass table
          ()
    ((rows :initform nil :initarg :rows :accessor table-rows)
     (caption :initform nil :initarg :caption :accessor caption)
     (caption-alignment :initform nil :initarg :caption-alignment :accessor caption-alignment)
     (caption-size :initform nil :initarg :caption-size :accessor caption-size)
     (cell-spacing :initform nil :initarg :cell-spacing :accessor cell-spacing)
     (cell-padding :initform nil :initarg :cell-padding :accessor cell-padding)
     (border :initform nil :initarg :border :accessor border)))

(defclass table-alignment-mixin
          ()
    ((horizontal-alignment :initform nil :initarg :horizontal-alignment :accessor horizontal-alignment)
     (vertical-alignment :initform nil :initarg :vertical-alignment :accessor vertical-alignment)))

(defclass table-row
          (table-alignment-mixin)
    ((cells :initform nil :initarg :cells :accessor table-row-cells)))

(defclass table-cell
          (table-alignment-mixin)
    ((text :initform nil :initarg :text :accessor table-cell-text)
     (header-p :initform nil :initarg :header-p :accessor header-p)
     (break-lines-p :initform nil :initarg :break-lines-p :accessor break-lines-p)
     (column-span :initform nil :initarg :column-span :accessor column-span)
     (row-span :initform nil :initarg :row-span :accessor row-span)))

(defmethod print-object ((slide-show slide-show) stream)
  (with-slots (name) slide-show
    (print-unreadable-object (slide-show stream :type t :identity t)
      (write-string (or name "No name yet") stream))))

(defmethod print-object ((basic-slide basic-slide) stream)
  (with-slots (number) basic-slide
    (print-unreadable-object (basic-slide stream :type t :identity t)
      (write (or number "No number yet")
             :base 10.
             :stream stream))))

(defmethod print-object ((bullets bullets) stream)
  (with-slots (enumeration-style) bullets
    (print-unreadable-object (bullets stream :type t :identity t)
      (when enumeration-style
        (write enumeration-style :escape nil :stream stream)
        (write-char #\space stream))
      (write (length (bullets bullets))
             :base 10.
             :stream stream))))


;;;------------------------------------------------------------------- 
;;;
;;; UTILITIES
;;;

(defvar *standard-font-size* 6)

(defmacro with-font-decremented ((&key (delta 1)) &body body)
  `(let ((*standard-font-size* (- *standard-font-size* ,delta))) ,@body))

(defvar *standard-rendition* :bold)

(defmacro with-standard-rendition ((rendition) &body body)
  `(let ((*standard-rendition* ,rendition)) ,@body))

(defun validate-enumeration-style (enumeration-style)
  (or (assoc enumeration-style html2::*enumeration-styles*)
      (error "~S is not one of the known enumeration-styles." enumeration-style)))

(defun execute-operation (stream operation args)
  (ecase operation
    (:font-size
      (destructuring-bind (size &rest n-args) args
        (check-type size integer)
        (with-font-size (size :stream stream)
          (%operation-dispatch stream n-args))))
    (:rendition
      (destructuring-bind (rendition &rest n-args) args
        (html:with-rendition (rendition :stream stream)
          (%operation-dispatch stream n-args))))
    (:break-line
      (break-line :stream stream)
      (%operation-dispatch stream args))
    (:blink
      (with-blinking (:stream stream)
        (%operation-dispatch stream args)))
    (:center
      (with-centering (:stream stream)
        (%operation-dispatch stream args)))
    (:note-anchor
      (destructuring-bind (text &key reference local-reference tag) args
        (with-anchor-noted (:reference reference :local-reference local-reference
                                       :tag tag :stream stream)
          (%operation-dispatch stream text))))
    (:emphasis
      (destructuring-bind (emphasis &rest n-args) args
        (html:with-emphasis (emphasis :stream stream)
          (%operation-dispatch stream n-args))))
    (:paragraph
      (html:with-paragraph (:stream stream)
        (%operation-dispatch stream args)))
    (:table
      (destructuring-bind
        ((&key caption border cell-spacing cell-padding (caption-alignment :top) (caption-size 3)) &rest n-args)
          args
        (with-table (:caption caption :border border :cell-spacing cell-spacing :cell-padding cell-padding
                              :caption-alignment caption-alignment :caption-size caption-size :stream stream)
          (%operation-dispatch stream n-args))))
    (:table-row
      (destructuring-bind ((&key horizontal-alignment vertical-alignment) &rest n-args) args
        (with-table-row  (:horizontal-alignment horizontal-alignment :vertical-alignment vertical-alignment
                                                :stream stream)
          (%operation-dispatch stream n-args))))
    (:cell
      (destructuring-bind
        ((&key header-p horizontal-alignment vertical-alignment (break-lines-p t) column-span row-span) &rest n-args) args
        (with-table-cell (:header-p header-p :horizontal-alignment horizontal-alignment :vertical-alignment vertical-alignment
                                    :break-lines-p break-lines-p :column-span column-span :row-span row-span :stream stream)
          (%operation-dispatch stream n-args))))))

(defun %operation-dispatch (stream arg)
  (etypecase arg
    (string (write-string arg stream))
    (keyword
      (ecase arg
        (:break-line (break-line :stream stream))))
    (cons
      (typecase (car arg)
        (keyword
          (execute-operation stream (car arg) (cdr arg)))
        (t (loop for item in arg
                 do (%operation-dispatch stream item)))))))

(defmacro operation-dispatch (stream spec &rest parameters)
  `(let ((args (list* ,@parameters ,spec)))
     (declare (dynamic-extent args))
     (%operation-dispatch ,stream args)))

(defun get-title-from-dispatch-spec (spec)
  (typecase spec
    (string spec)
    (cons
      (loop for item in spec
            when (stringp item)
              return item
            do (destructuring-bind (head . args) spec
                 (typecase head
                   (string (return head))
                   (keyword
                     (case head
                       (:note-anchor
                         (return (get-title-from-dispatch-spec (first args))))))))
            finally (error "Could not extraqct the title from ~S." spec)))))


;;;------------------------------------------------------------------- 
;;;
;;; INTERNING OBJECTS
;;;

(defvar *slide-shows* nil)

(defun intern-slide-show (name &key (if-does-not-exist :create) (class 'slide-show))
  (etypecase name
    (string
      (or (find name *slide-shows* :test #'equalp :key #'slide-show-name)
          (ecase if-does-not-exist
            (:soft nil)
            (:create
              (let ((show (make-instance class :name name)))
                (http::push-ordered show *slide-shows* #'string< :key #'slide-show-name)
                (values show t))))))
    (slide-show name)))


;;;------------------------------------------------------------------- 
;;;
;;; BUILDING STRUCTURE
;;;

(declaim (inline %nth-slide))

(defun %nth-slide (num slide-show)
  (with-slots (slides) slide-show
    (nth (max num 0) slides)))

(defmethod nth-slide ((number integer) (slide-show slide-show))
  (%nth-slide number slide-show))

(defmethod nth-slide ((number string) (slide-show slide-show))
  (let ((num (if (http:null-string-p number) 0 (parse-integer number :junk-allowed t))))
    (%nth-slide num slide-show)))

(defmethod nth-slide ((number (eql nil)) (slide-show slide-show))
  (%nth-slide 0 slide-show))

(defmethod slide-show-title-slide ((slide-show slide-show))
  (%nth-slide 0 slide-show))


(defmethod nth-slide-url-string ((slide-show slide-show) (number integer))
  (with-slots (url) slide-show
    (cond (url
           (concatenate 'string (url:name-string  url) (write-to-string number :base 10)))
          (t (error "No Export URL is available for ~S." slide-show)))))

(defmethod nth-slide-url-string ((slide basic-slide) (number integer))
  (with-slots (slide-show) slide
    (nth-slide-url-string slide-show number)))

(defmacro check-argument (class argument type)
  `(unless (and (not (null ,argument)) (typep ,argument ',type))
     (error "~S was ~S, which is not a ~A as required by a ~A."
            ',argument ,argument ',type ',class )))

(defun instantiate-element (spec)
  (destructuring-bind (element-class . init-args) spec
    (let ((element (make-instance (coerce-to-package element-class *slides-package*))))
      (initialize-element element init-args)
      element)))

(defgeneric initialize-element (slide-element init-args)
  (declare (values slide-element)))

(defmethod initialize-element ((slide-show slide-show) init-args)
  (macrolet ((maybe-push-arg (arg)
               `(when ,arg
                  (push ,arg default-args)
                  (push ,(http:symbolize (symbol-name arg) http:*keyword-package*) default-args)))
             (instantiate-inferior (spec defaults)
               `(cond (,defaults
                       (let ((args (append ,spec ,defaults)))
                         (declare (dynamic-extent args))
                         (instantiate-element args)))
                      (t (instantiate-element ,spec)))))
    (destructuring-bind (&key title slides keywords documentation url
                              background background-url foreground link visited-link active-link
                              display-interval continuous-p &allow-other-keys) init-args 
      (let ((default-args nil))
        (declare (dynamic-extent default-args))
        (maybe-push-arg background)
	(maybe-push-arg background-url)
        (maybe-push-arg foreground)
        (maybe-push-arg link)
        (maybe-push-arg visited-link)
        (maybe-push-arg active-link)
        (maybe-push-arg slide-show)
        (maybe-push-arg display-interval)
        (loop for slide in (cons title slides)
              for idx upfrom 0
              for object = (instantiate-inferior slide `(:number ,idx . ,default-args))
              collect object into result
              
              finally (setf (slide-show-slides slide-show) result
                            (number-of-slides slide-show) (1+ idx)))
        (when display-interval
          (check-type display-interval (integer 1)))
        (setf (slide-show-continuous-p slide-show) continuous-p
              (display-interval slide-show) display-interval)
        (when url
          (flet ((write-slide (url stream)
                   (let ((number (first (url:search-keys url)))
                         (slide-show (url:search-database (url:search-parent url))))
                     (http:with-successful-response (stream :html)
                       (write-element (or (nth-slide number slide-show)
                                          (nth-slide 0 slide-show))
                                      stream)))))
            (setf (slide-show-url slide-show)
                  (http:export-url url
                                   :search
                                   :response-function #'write-slide
                                   :search-database slide-show
                                   :keywords `(,@keywords :slide-show)
                                   :documentation documentation))))
        slide-show))))

(defmethod initialize-element :before ((background-mixin background-mixin) init-args)
  (flet ((get-color (spec &optional url-ok-p)
           (cond ((null spec) nil)
                 (url-ok-p
                  (or (background-url spec nil)
                      (and (color-mapping spec) spec)))
                 ((color-mapping spec) spec))))
    (destructuring-bind (&key background background-url foreground link visited-link active-link
                              &allow-other-keys) init-args
      (setf (slide-background background-mixin) (get-color background)
	    (slide-background-url background-mixin) (background-url background-url nil)
            (slide-foreground background-mixin) (get-color foreground)
            (slide-link background-mixin) (get-color link)
            (slide-visited-link background-mixin) (get-color visited-link)
            (slide-active-link background-mixin) (get-color active-link))
      background-mixin)))

(defmethod initialize-element :before ((auto-advance-mixin auto-advance-mixin) init-args)
  (destructuring-bind (&key display-interval &allow-other-keys) init-args
    (when display-interval
      (check-argument auto-advance-mixin display-interval (integer 1)))
    (setf (display-interval auto-advance-mixin) display-interval)
    auto-advance-mixin))
 
(defun instantiate-slide-show (name &rest init-args)
  (let ((slide-show (intern-slide-show name :if-does-not-exist :create)))
    (initialize-element slide-show init-args)
    slide-show))

(defmacro define-slide-show (name &key url title slides
                             background background-url foreground link visited-link active-link
                             keywords documentation continuous-p display-interval)
  `(instantiate-slide-show ',(string name)
                           :title ,title
                           :slides ,slides
                           :url ,url
                           :continuous-p ,continuous-p
                           :display-interval ,display-interval
                           :background ,background
			   :background-url ,background-url
                           :foreground ,foreground
                           :link ,link
                           :visited-link ,visited-link
                           :active-link ,active-link
                           :keywords ',keywords
                           :documentation ',documentation))


;;;------------------------------------------------------------------- 
;;;
;;; BULLET METHODS
;;;

(defmethod initialize-element ((bullets-object bullets) init-args)
  (destructuring-bind (&key (enumeration-style :itemize) bullets &allow-other-keys) init-args
    (check-argument bullets bullets cons)
    (validate-enumeration-style enumeration-style)
    (setf (enumeration-style bullets-object) enumeration-style)
    (setf (bullets bullets-object) (loop for bullet in bullets
                                         collect (instantiate-element bullet))))
  bullets-object)

(defmethod write-element ((bullets-object bullets) stream)
  (with-slots (enumeration-style bullets) bullets-object
    (html:with-enumeration (stream enumeration-style)
      (dolist (bullet bullets)
        (write-element bullet stream)))))

(defmethod initialize-element ((bullet bullet) init-args)
  (destructuring-bind (&key text icon &allow-other-keys) init-args
    (check-argument bullet text (or string cons))
    (setf (text bullet) text)
    (setf (icon bullet) icon)
    bullet))

(defmethod write-element ((bullet bullet) stream)
  (with-slots (text) bullet
    (%operation-dispatch stream text)))

(defmethod write-element :around ((bullet bullet) stream)
  (with-slots (icon) bullet
    (with-font-size (*standard-font-size* :stream stream)
      (with-rendition (*standard-rendition* :stream stream)
        (html:enumerating-item (stream :icon-url icon)
          (call-next-method bullet stream))))))

;;;------------------------------------------------------------------- 
;;;
;;; TABLE METHODS
;;;

(defmethod initialize-element ((table table) init-args)
  (destructuring-bind ((&key caption border cell-spacing cell-padding (caption-alignment :top) (caption-size 3))
                       &rest rows) init-args
    (check-argument table rows cons)
    (setf (table-rows table) (mapcar #'instantiate-element rows)
          (caption table) caption
          (caption-alignment table) caption-alignment
          (caption-size table) caption-size
          (cell-spacing table) cell-spacing
          (cell-padding table) cell-padding
          (border table) border))
  table)

(defmethod initialize-element ((table-row table-row) init-args)
  (destructuring-bind ((&key horizontal-alignment vertical-alignment) &rest cells) init-args
    (check-argument table-row cells cons)
    (setf (table-row-cells table-row) (mapcar #'instantiate-element cells)
          (horizontal-alignment table-row) horizontal-alignment
          (vertical-alignment table-row) vertical-alignment))
  table-row)

(defmethod initialize-element ((table-cell table-cell) init-args)
  (destructuring-bind ((&key header-p horizontal-alignment vertical-alignment (break-lines-p t) column-span row-span)
                       &rest text) init-args
    (setf (table-cell-text table-cell) text
          (header-p table-cell) header-p
          (horizontal-alignment table-cell) horizontal-alignment
          (vertical-alignment table-cell) vertical-alignment
          (break-lines-p table-cell) break-lines-p
          (column-span table-cell) column-span
          (row-span table-cell) row-span))
  table-cell)

(defmethod write-element ((table table) stream)
  (with-slots (rows caption border cell-spacing cell-padding
                    caption-alignment caption-size) table
    (with-centering (:stream stream)
      (with-table  (:caption caption :border border :cell-spacing cell-spacing :cell-padding cell-padding
                             :caption-alignment caption-alignment :caption-size caption-size :stream stream)
        (dolist (row rows)
          (write-element row stream))))))

(defmethod write-element ((table-row table-row) stream)
  (with-slots (cells horizontal-alignment vertical-alignment) table-row
    (with-table-row  (:horizontal-alignment horizontal-alignment :vertical-alignment vertical-alignment
                                            :stream stream)
      (dolist (cell cells)
        (write-element cell stream)))))

(defmethod write-element ((table-cell table-cell) stream)
  (with-slots (text header-p horizontal-alignment vertical-alignment break-lines-p
                    column-span row-span) table-cell
    (with-table-cell (:header-p header-p :horizontal-alignment horizontal-alignment :vertical-alignment vertical-alignment
                                :break-lines-p break-lines-p :column-span column-span :row-span row-span :stream stream)
      (%operation-dispatch stream text))))


;;;------------------------------------------------------------------- 
;;;
;;; SLIDE METHODS
;;;

(defmethod initialize-element ((slide basic-slide) init-args)
  (destructuring-bind (&key number slide-show &allow-other-keys) init-args
    (check-argument basic-slide number integer)
    (check-argument basic-slide slide-show slide-show)
    (setf (slide-number slide) number
          (slide-show slide) slide-show))
  slide)

(defmethod initialize-element :after ((slide title-mixin) init-args)
  (flet ((get-line-argument (line)
           (cond ((null line) nil)
                 ((eql line t)
                  (flet ((write-title-line (stream)
                           (html:horizontal-line :fresh-line t :stream stream)))
                    #'write-title-line))
                 (t (flet ((write-title-line (stream)
                             (html:break-line :stream stream)
                             (image line "----------" :alignment :left :vertical-space 2 :stream stream)
                             (html:break-line :stream stream)))
                      #'write-title-line))))
         (get-icon-argument (icon)
           (cond ((null icon) nil)
                 (t (flet ((write-title-icon (stream)
                             (image icon "----------" :alignment :left :vertical-space 4 :stream stream)))
                      #'write-title-icon)))))
    (destructuring-bind (&key title icon line &allow-other-keys) init-args
      (check-argument title-mixin title (or string cons))
      (setf (title slide) (http:ensure-list title)
            (icon slide) (get-icon-argument icon)
            (line slide) (get-line-argument line)))
    slide))

(defmethod initialize-element :after ((slide title-slide) init-args)
  (destructuring-bind (&key author institution &allow-other-keys) init-args
    (check-argument title-slide author (or string cons))
    (check-argument title-slide institution (or string cons))
    (setf (author slide) (http:ensure-list author)
          (institution slide) (http:ensure-list institution)))
  slide)

(defmethod initialize-element :after ((slide slide) init-args)
  (destructuring-bind (&key body &allow-other-keys) init-args
    (check-argument slide body cons)
    (setf (slide-body slide) (instantiate-element body))
    slide))


;;;------------------------------------------------------------------- 
;;;
;;; WRITING SLIDES
;;;

(defmethod write-element ((slide basic-slide) stream)
  (html:with-html-document (:stream stream)
    (write-preamble slide stream)
    (write-body slide stream)))

(defmethod write-preamble :around ((slide basic-slide) stream)
  (html:with-document-preamble (:stream stream)
    (call-next-method slide stream)))

(defmethod write-preamble ((slide title-mixin) stream)
  (with-slots (title) slide
    (html:declare-title (get-title-from-dispatch-spec title) :stream stream)))

(defmethod write-preamble :after ((slide auto-advance-mixin) stream)
  (with-slots (number display-interval slide-show) slide
    (when display-interval
      (let ((next (1+ number)))
        (cond ((< next (number-of-slides slide-show))
               (declare-refresh-rate display-interval
                                     :url (nth-slide-url-string slide-show next)
                                     :stream stream))
              ((slide-show-continuous-p slide-show)
               (declare-refresh-rate display-interval
                                     :url (nth-slide-url-string slide-show 0)
                                     :stream stream)))))))

(defmethod write-body :around ((slide basic-slide) stream)
  (html:with-document-body (:stream stream)
    (call-next-method slide stream)))

(defmethod write-body :around ((slide background-mixin) stream)
  (with-slots (background background-url foreground link visited-link active-link) slide
    (with-document-body (:background background 
                         :background-url background-url
                         :foreground foreground
                         :link link
                         :visited-link visited-link
                         :active-link active-link
                         :stream stream)
      (call-next-method slide stream))))

(defmethod write-slide-navigation-buttons ((slide basic-slide) stream)
  (with-slots (number slide-show) slide
    (fresh-line stream)
    (with-verbatim-text (:fresh-line t :stream stream)
      (let ((next (1+ number))
            (num (number-of-slides slide-show)))
        (when (< next num)
          (note-anchor "Next" :reference (nth-slide-url-string slide-show next) :stream stream))
        (unless (zerop number)
          (when (< next num)
            (write-string " | " stream))
          (note-anchor "Previous" :reference (nth-slide-url-string slide-show (1- number)) :stream stream)
          (write-string " | " stream)
          (note-anchor "Title" :reference (slide-show-url slide-show) :stream stream))
        (unless (zerop number)
          (write-string " | " stream)
          (with-anchor-noted (:local-reference "title" :stream stream)
            (write-string "Slide " stream)
            (write number :base 10 :stream stream)))))))

(defmethod write-body ((slide title-slide) stream)
  (with-slots (title author institution) slide
    (with-centering (:stream stream)
      (note-anchor "" :tag "title" :stream stream)
      (operation-dispatch stream title :paragraph :font-size 6 :rendition :bold)
      (html:with-paragraph (:stream stream)
        (operation-dispatch stream author :font-size 5 :rendition :bold)
        (html:break-line :stream stream)
        (operation-dispatch stream institution :font-size 5)))
    (write-slide-navigation-buttons slide stream)))

(defmethod write-body ((slide slide) stream)
  (with-slots (title icon line enumeration-style body number) slide
    (when icon (funcall icon stream))
    (operation-dispatch stream title :font-size 6 :rendition :bold)
    (when line (funcall line stream))
    (with-font-decremented ()
                           (write-element body stream))
    (when line (funcall line stream))
    (write-slide-navigation-buttons slide stream)))

#+ignore
(defmethod write-body ((slide slide) stream)
  (with-slots (title icon line enumeration-style body number) slide
    (let ((columns (if icon 2 1)))
      (with-centering (:stream stream)
        (with-font-decremented ()
                               (with-table (:border 3 :cell-spacing 3 :cell-padding 3 :stream stream)
                                 (with-table-row (:stream stream)
                                   (when icon
                                     (with-table-cell (:break-lines-p t :stream stream)
                                       (funcall icon stream)))
                                   (with-table-cell (:break-lines-p t :stream stream)
                                     (write-multi-line-element title stream 6 :bold)))
                                 (when line
                                   (with-table-row (:stream stream)
                                     (with-table-cell (:break-lines-p t :horizontal-alignment :center
                                                                      :column-span columns :stream stream)
                                       (funcall line stream))))
                                 (with-table-row (:stream stream)
                                   (with-table-cell (:break-lines-p t :column-span columns :stream stream)
                                     (write-element body stream)))
                                 (with-table-row (:stream stream)
                                   (with-table-cell (:break-lines-p t :column-span columns :stream stream)
                                     (write-slide-navigation-buttons slide stream)))))))))


;;;------------------------------------------------------------------- 
;;;
;;; TESTING CODE
;;;

#+ignore
(define-slide-show
  "test-talk"
  :url #U"test?"
  :keywords (:test)
  :documentation "This is a test slide show using the web."
  :background-url :rock-smblue
  :foreground :red
  :link :random
  :visited-link :random
  #|  :continuous-p t
  :display-interval 10 |#
  :title `(:title-slide :title (:note-anchor ("The Common Lisp Hypermedia Server:"
                                              :break-line "An Exercise in Virtue")
                                :reference "http://www.ai.mit.edu/projects/iiip/doc/cl-http/home-page.html")
                        :author (:note-anchor "John C. Mallery"
                                 :reference "http://www.ai.mit.edu/people/jcma/jcma.html")
                        :institution ((:note-anchor "Artificial Intelligence Laboratory"
                                       :reference "http://www.ai.mit.edu/")
                                      :break-line
                                      (:note-anchor "Massachusetts Institute of Technology"
                                       :reference "http://web.mit.edu/")))
  :slides `((:slide :title (:center "Test Slide")
                    :icon ,#U"/cl-http/icons/construction.gif"
                    :line t ;;,#U"/cl-http/icons/line-pastel.gif"
                    :body (:bullets
                            :enumeration-style :definition
                            ;; :enumerate :itemize :plain :menu :directory :definition
                            :bullets ((:bullet :icon ,#U"/cl-http/icons/redball.gif"
                                               :text "Use lisp because it wins.")
                                      (:bullet :icon ,#U "/cl-http/icons/redball.gif"
                                               :text ((:note-anchor "CL-HTTP"
                                                       :reference"http://www.ai.mit.edu/projects/iiip/doc/cl-http/home-page.html")
                                                      " is " (:rendition :italic "really")" easy to hack.")))))
            (:slide :title "The second Test Slide"
                    :icon ,#U"/cl-http/icons/construction.gif"
                    :line t ;;,#U"/cl-http/icons/line-pastel.gif"
                    :body (:bullets
                            :enumeration-style :itemize
                            ;; :enumerate :itemize :plain :menu :directory :definition
                            :bullets ((:bullet :icon ,#U"/cl-http/icons/redball.gif"
                                               :text "Use lisp because it wins.")
                                      (:bullet :icon ,#U "/cl-http/icons/redball.gif"
                                               :text "CL-HTTP is really easy to hack.")
                                      (:bullet :icon ,#U "/cl-http/icons/redball.gif"
                                               :text "Use some nice icons."))))
            (:slide :title "The Table Test Slide"
                    :icon ,#U"/cl-http/icons/construction.gif"
                    :line t ;;,#U"/cl-http/icons/line-pastel.gif"
                    :body (:table (:caption "test table"
                                   :caption-alignment :bottom
                                   :border 3 :cell-spacing 2 :cell-padding 30)
                           (:table-row (:horizontal-alignment :center)
                            (:table-cell (:vertical-alignment :middle)
                             "Foo")
                            (:table-cell (:vertical-alignment :middle)
                             "Bar"))
                           (:table-row (:horizontal-alignment :center)
                            (:table-cell (:vertical-alignment :middle)
                             "Foo")
                            (:table-cell (:vertical-alignment :middle)
                             "Bar"))))))
