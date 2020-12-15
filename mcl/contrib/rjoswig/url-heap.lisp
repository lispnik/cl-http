;;;-*- Mode: Lisp; Package: (URL-HEAP :use ("COMMON-LISP" "HTML3.2" "W3P")) -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Combining the CL-HTTP client and WOOD: Checks your favorite sites for updates

;;;  for MCL 4.1, CL-HTTP and WOOD
;;;  Copyright: Rainer Joswig, joswig@lavielle.com
;;;  Date: 13.May 1997

#-ccl(cl:error "This code is for MCL only")

(cl:defpackage "URL-HEAP"
  (:use "COMMON-LISP" "HTML3.2" "W3P"))

(cl:in-package "URL-HEAP")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Diverse stuff

; (setf ccl:*browser-creator* :msie)

(assert (member :wood *features*) ()   ; this is not sufficient
        "WOOD not loaded.")
(assert (member :w4 *features*) ()
        "CL-HTTP basic client not loaded.")
(ccl:require :progress-indicator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; URL-STORE abstraction

(defclass store () ())

(defvar *url-store* nil
  "The store for URLS.")

(defvar *url-store-path* "ccl:urls.wood"
  "The path for the URL store.")

; (setf *url-store-path* "ccl:urls.wood")
; (delete-file "ccl:urls.wood")
; (wood:gc-pheap-file *url-store-path*)

(defgeneric create-url-store (type path))

(defgeneric get-all-urls-from-store (store)
  (:documentation "Returns a list of URLS."))

(defgeneric put-url-in-store (store url))

(defgeneric update-url-in-store (store url))

(defgeneric get-url-from-store-by-string (store string))

(defgeneric open-store (path))

; (setf *url-store* (open-store "ccl:urls.wood"))

(defgeneric close-store (store))

; (close-store *url-store*)

(defmacro with-store ((store-var path) &body body)
  `(let (,store-var)
     (unwind-protect (progn (setf ,store-var (open-store ,path))
                            ,@body)
       (close-store ,store-var))))


(defgeneric set-modification-info (store url &key force-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Store based on WOOD pheap


(defclass wood-store (store wood:pheap) ())

(defmethod create-url-store ((type (eql :wood)) filename)
  (let ((pheap (wood:open-pheap filename 
                                :if-does-not-exist :create
                                :pheap-class (find-class 'wood-store))))
    (setf (wood:root-object pheap)
          (wood:p-list pheap "Urls" (wood:p-make-btree pheap)))
    (wood:close-pheap pheap)))

(defun url-pheap-table (pheap)
  (let ((root (wood:p-load (wood:root-object pheap))))
    (unless (and (listp root)
               (eql 2 (length root))
               (equal "Urls" (first root))
               (wood:p-btree-p (second root)))
      (error "~s does not appear to be a url file" pheap))
    (second root)))

(defmethod get-all-urls-from-store ((store wood-store))
  (let ((urls nil))
    (wood:p-map-btree (url-pheap-table store)
                      #'(lambda (string url)
                          (declare (ignore string))
                          (push (wood:p-load url) urls)))
    urls))

(defmethod put-url-in-store ((store wood-store) url)
  (wood:with-transaction (store)
    (let ((string->url (url-pheap-table store))
          (string (persistent-url-string url)))
      (if (wood:p-btree-lookup string->url string)
        (wood:p-store store url :store-slots-again)
        (setf (wood:p-btree-lookup string->url string) url))
      url)))

(defmethod update-url-in-store ((store wood-store) url)
  (wood:with-transaction (store)
    (wood:p-store store url :store-slots-again))
  url)

(defmethod get-url-from-store-by-string ((store wood-store) string)
  (let ((string->url (url-pheap-table store)))
    (wood:p-load (wood:p-btree-lookup string->url string))))

(defmethod open-store (path)
  (unless (probe-file path)
    (create-url-store :wood path))
  (setf *url-store*
        (wood:open-pheap path :pheap-class (find-class 'wood-store))))

(defmethod close-store ((store wood-store))
  (wood:close-pheap store))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class persistent-url

(defclass persistent-url ()
  ((string :accessor persistent-url-string :initarg :string
           :documentation "The URL string")
   (title :accessor persistent-url-title :initarg :title :initform "" )
   (comment :accessor persistent-url-comment :initarg :comment :initform "")
   (changed :accessor persistent-url-changed :initform nil
            :documentation "Has this URL been changed since the last check?")
   (last-check :accessor persistent-url-last-check :initform 0
               :documentation "When has this URL been checked last?")
   (last-update :accessor persistent-url-last-update :initform nil
                :documentation "When has this URL been changed?")
   (last-size :accessor persistent-url-last-size :initform nil
              :documentation "What was the last size of the data at the URL?")
   (last-visited :accessor persistent-url-last-visited :initform nil
                 :documentation "When has the browser last visited this page?")
   (last-error :accessor persistent-url-last-error :initform nil
                 :documentation "What was the last error while checking this URL?")
   (last-error-time :accessor persistent-url-last-error-time :initform nil
                 :documentation "When did the last error happen?")
   (last-connect-time :accessor persistent-url-last-connect-time :initform nil
                      :documentation "How long did it take to check this URL?")
   (check-interval :accessor persistent-url-check-interval :initform 259200
                   :initarg :check-interval
                   :documentation "How often should this URL been checked?
Default is every three days.")
   (server :accessor persistent-url-server :initform nil
           :documentation "Which server software provides the URL?")))


(defmethod print-object ((url persistent-url) stream)
  (print-unreadable-object (url stream :type t :identity t)
    (format stream "~a" (persistent-url-string url))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CL-HTTP client code


(defun handle-get-error (persistent-url condition)
  (setf (persistent-url-last-error persistent-url) condition
        (persistent-url-last-error-time persistent-url) (get-universal-time))
  (when (typep condition 'http::reportable-condition)
    (let ((server (http:get-header :server (http::http-transaction-headers 
                                            condition))))
      (when server
        (values nil nil server)))))

(defun get-url-modified-headers (url persistent-url headers)
  (handler-case
    (let ((returned-headers (http:get-url-headers url headers)))
      (values (http:get-header :last-modified returned-headers)
              (http:get-header :content-length returned-headers)
              (http:get-header :server returned-headers)))
    (condition (condition)
               (handle-get-error persistent-url condition)
               nil)))


(defun get-document-size-from-document (url persistent-url)
  (handler-case
    (multiple-value-bind (document headers)
                         (http:get-url-headers-and-body url)
      (values (or (http:get-header :content-length headers)
                  (length document))
              (http:get-header :server headers)))
    (condition (condition)
               (handle-get-error persistent-url condition)
               nil)))


(defmethod set-modification-info :around ((store wood-store) url &key (force-p nil))
  (wood:with-transaction (store)
    (call-next-method store url :force-p force-p)))

(defmethod set-modification-info :around (store url &key (force-p nil))
  (with-simple-restart (abort
                        "Exit updating url ~a in store ~a"
                        (persistent-url-string url)
                        store)
    (call-next-method store url :force-p force-p)))

(defun check-for-modified-header-info (url url-object)
  (let ((http:*resolve-ip-addresses* t)
        (start-time (get-universal-time)))
    (multiple-value-bind (modified size server)
                         (get-url-modified-headers url-object url nil)
      (set-document-info-from-parameters url
                                         server
                                         size
                                         start-time
                                         (get-universal-time)
                                         modified)
      (values modified size))))

(defun check-for-document-size-info (url url-object)
  (let* ((http:*resolve-ip-addresses* t)
         (start-time (get-universal-time)))
    (multiple-value-bind (size server)
                         (get-document-size-from-document url-object url)
      (set-document-info-from-parameters url
                                         server
                                         size
                                         start-time
                                         (get-universal-time)
                                         start-time)
      size)))

(defun set-document-info-from-parameters (url server size start-time end-time
                                                    &optional modified)
  (macrolet ((set-if (check accessor &optional value)
               `(when ,check (setf (,accessor url) ,(or value check)))))
    (set-if server persistent-url-server)
    (set-if size persistent-url-last-size)
    (set-if modified persistent-url-last-update)
    (set-if end-time persistent-url-last-check)
    (set-if (and start-time end-time)
            persistent-url-last-connect-time
            (- end-time start-time))))


(defmethod set-modification-info (store url &key (force-p nil))
  (let ((last-update (persistent-url-last-update url))
        (last-size (persistent-url-last-size url)))
    (flet ((changed? (modified size)
             (when (or modified size)
               (if (and modified last-update)
                 (setf (persistent-url-changed url)
                       (> modified last-update))
                 (when (and size last-size)
                   (setf (persistent-url-changed url)
                         (not (= size last-size))))))))
      (setf (persistent-url-changed url) nil)
      (when (or force-p
                (and (persistent-url-last-check url)
                     (> (get-universal-time)
                        (+ (persistent-url-last-check url)
                           (persistent-url-check-interval url)))))
        (let ((url-object (url:url (persistent-url-string url))))
          (multiple-value-bind (modified size)
                               (check-for-modified-header-info url url-object)
            (if (or modified size)
              (changed? modified size)
              (changed? nil (check-for-document-size-info url url-object)))))
        (update-url-in-store store url))))
  url)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Presentation Part

(define-presentation-view-class html-table-view :inherits-from (html-view))

(define-presentation-type table ()
  :inherit-from t)

(define-presentation-type table-item ()
  :inherit-from t)

;;; Table Headers

(define-presentation-generic-function table-headers-method
  table-headers
  (w3p::type-key w3p::parameters type view &key &allow-other-keys)
  (:documentation ""))

(defun table-headers (type view)
  (with-presentation-type-decoded (type-key parameters nil) type
    (www-present::call-presentation-generic-function
     table-headers (type-key parameters type view))))


;;; Table Item Type

(define-presentation-generic-function table-item-type-method
  table-item-type
  (w3p::type-key w3p::parameters type view &key &allow-other-keys)
  (:documentation ""))

(defun table-item-type (type view)
  (with-presentation-type-decoded (type-key parameters nil) type
    (www-present::call-presentation-generic-function
     table-item-type (type-key parameters type view))))


;;; Get Table Slot Printer

(define-presentation-generic-function get-table-slot-printer-method
  get-table-slot-printer
  (w3p::type-key w3p::parameters type view name &key &allow-other-keys)
  (:documentation ""))

(defun get-table-slot-printer (type view name)
  (with-presentation-type-decoded (type-key parameters nil) type
    (www-present::call-presentation-generic-function
     get-table-slot-printer (type-key parameters type view name))))


;;; Present Object in HTML Table

(define-presentation-method present (object
                                     (type table-item)
                                     stream
                                     (view html-table-view)
                                     &key)
  (netscape3.0:with-table-row (:stream stream :background :grey-light-very)
    (loop for (name nil) in (table-headers type view)
          for printer = (get-table-slot-printer type view name)
          do (netscape3.0:with-table-cell (:stream stream)
               (funcall printer object stream)))))

;;; Present HTML Table

(define-presentation-method present (objects
                                     (type table)
                                     stream
                                     (view html-table-view)
                                     &key)
  (let ((item-type (table-item-type type view)))
    (netscape3.0:with-table (:stream stream :border 0)
      (netscape3.0:with-table-row (:stream stream :background "#A0A0A0")
        (netscape3.0:with-table-cell (:stream stream
                                      :header-p t
                                      :column-span (length (table-headers item-type view)))
          (princ (table-name type view) stream)))
      (present-table-headers item-type stream view)
      (mapc #'(lambda (object)
                (present object item-type :stream stream :view view))
            objects)))
  objects)


;;; Define Table Slot Printer

(defmacro define-table-slot-printer ((object-var type view name) &body body)
  `(define-presentation-method get-table-slot-printer
     ((type ,type) (view ,view) (name (eql ,name)) &key)
     #'(lambda (,object-var stream)
         ,@body)))


;;; Table Name

(define-presentation-generic-function table-name-method
  table-name
  (w3p::type-key w3p::parameters type view &key &allow-other-keys)
  (:documentation ""))

(defun table-name (type view)
  (with-presentation-type-decoded (type-key parameters nil) type
    (www-present::call-presentation-generic-function
     table-name (type-key parameters type view))))

(define-presentation-method table-name
  ((type table) (view html-table-view) &key)
  "" "Table")

;;; Present Table Headers

(define-presentation-generic-function present-table-headers-method
  present-table-headers
  (w3p::type-key w3p::parameters type stream view &key &allow-other-keys)
  (:documentation ""))

(defun present-table-headers (type stream view)
  (with-presentation-type-decoded (type-key parameters nil) type
    (www-present::call-presentation-generic-function
     present-table-headers (type-key parameters type stream view))))

(define-presentation-method present-table-headers
  ((type table-item) stream (view html-table-view) &key)
  (netscape3.0:with-table-row (:stream stream :background "#A8A8A8")
    (flet ((header (header)
             (netscape3.0:with-table-cell (:stream stream :header-p t)
               (princ (second header) stream))))
      (mapc #'header (table-headers type view)))))


;;;; Specific Presentation Types

(defun print-time-short (time stream)
  (multiple-value-bind (second minute hour day month year)
                       (decode-universal-time time)
    (format stream "~a.~a.~a ~a:~a:~a" day month year hour minute second)))

(define-presentation-view-class url-table-view :inherits-from (html-table-view))
(define-presentation-view-class url-error-view :inherits-from (html-table-view))

(define-presentation-view +url-table-view+ url-table-view)
(define-presentation-view +url-error-view+ url-error-view)



;;; URL List Table


(define-presentation-type url-list ()
  :inherit-from table)

(define-presentation-type persistent-url () 
  :inherit-from table-item)

(define-presentation-method table-item-type
  ((type url-list) (view html-table-view) &key)
  'persistent-url)


(defmacro cell-item (accessor printer)
  (let ((sym (gensym "VAR")))
    `(let ((,sym (,accessor url)))
       (if ,sym (,printer ,sym stream) (princ "-" stream)))))


(define-table-slot-printer (url persistent-url html-table-view :site)
  (let ((title (persistent-url-title url))
        (name (persistent-url-string url)))
    (note-anchor (if title title name) :reference name :stream stream)))

(define-table-slot-printer (url persistent-url html-table-view :changed)
  (princ (if (persistent-url-changed url) "Yes" "No") stream))

(define-table-slot-printer (url persistent-url html-table-view :modified)
  (cell-item persistent-url-last-update print-time-short))

(define-table-slot-printer (url persistent-url html-table-view :checked)
  (cell-item persistent-url-last-check print-time-short))

(define-table-slot-printer (url persistent-url html-table-view :time)
  (cell-item persistent-url-last-connect-time princ))

(define-table-slot-printer (url persistent-url html-table-view :next)
  (let ((time (+ (persistent-url-check-interval url)
                   (persistent-url-last-check url))))
      (when time (print-time-short time stream))))

(define-table-slot-printer (url persistent-url html-table-view :server)
  (cell-item persistent-url-server princ))

(define-table-slot-printer (url persistent-url html-table-view :error-time)
  (cell-item persistent-url-last-error-time print-time-short))

(define-table-slot-printer (url persistent-url html-table-view :reason)
  (cell-item persistent-url-last-error
               (lambda (error stream)
                 (cond ((typep error 'http::reportable-condition)
                        (princ (http::http-reason error) stream))
                       (t (princ (substitute #\space
                                             #\-
                                             (symbol-name (class-name (class-of error))))
                                 stream))))))


;;; Table View

(define-presentation-method table-headers
  ((type persistent-url) (view url-table-view) &key)
  '((:site "Site")
    (:changed "Changed")
    (:modified "Modified")
    (:checked "Checked")
    (:time "Time")
    (:next "Next")
    (:server "Server")))

(define-presentation-method table-name
  ((type url-list) (view url-table-view) &key)
  "" "Site Overview")


;;; Error View

(define-presentation-method table-headers
  ((type persistent-url) (view url-error-view) &key)
  '((:site "Site")
    (:error-time "Error Time")
    (:reason "Reason")
    (:modified "Modified")
    (:checked "Checked")
    (:time "Time")
    (:next "Next")
    (:server "Server")))

(define-presentation-method table-name
  ((type url-list) (view url-error-view) &key)
  "" "Errors")


(define-presentation-view-class modified-sites-of-store-view
  :inherits-from (html-view))

(define-presentation-view +modified-sites-of-store-view+
  modified-sites-of-store-view)

(define-presentation-type store () 
  :inherit-from t)

(define-presentation-method present (store
                                     (type store)
                                     stream
                                     (view modified-sites-of-store-view)
                                     &key)
  (flet ((url-recent-error (url)
           (when (and (persistent-url-last-check url)
                      (persistent-url-last-error-time url))
             (> (persistent-url-last-error-time url)
                (- (persistent-url-last-check url) (* 3 60 60))))))
    (let* ((urls (get-all-urls-from-store store))
           (error-urls (remove-if-not #'url-recent-error urls))
           (non-error-urls (remove-if #'url-recent-error urls)))
      (let* ((changed-urls (remove-if-not #'persistent-url-changed non-error-urls))
             (unchanged-urls (remove-if #'persistent-url-changed non-error-urls))
             (contacted-unchanged-urls (remove-if-not #'persistent-url-last-update
                                                      unchanged-urls))
             (uncontacted-unchanged-urls (remove-if #'persistent-url-last-update
                                                    unchanged-urls)))
        (flet ((sorter (list)
                 (sort list #'> :key #'persistent-url-last-update)))
          (let ((changed-urls (sorter changed-urls))
                (contacted-unchanged-urls (sorter contacted-unchanged-urls)))
            (present (append changed-urls contacted-unchanged-urls uncontacted-unchanged-urls)
                     'url-list
                     :stream stream
                     :view +url-table-view+))))
      (present error-urls
               'url-list
               :stream stream
               :view +url-error-view+))))
  

(defun print-modified-sites-page (store stream
                                             &key (update-p t) (force-p nil) (print-info-p nil))
  (when update-p
    (flet ((update-fn (url) (set-modification-info store url :force-p force-p)))
      (let ((urls (get-all-urls-from-store store)))
        (if (and (or update-p force-p) print-info-p)
          (progn
            (ccl::with-shown-progress (update "Contacting sitesä" :denominator (length urls))
              (dolist (url urls)
                (ccl::show-progress update :note (persistent-url-title url))
                (funcall #'update-fn url)))
           ; #+speak (ccl:without-interrupts
           ;          (ccl:speak
           ;           (format nil "~D site~:P changed."
           ;                   (count t urls :key #'persistent-url-changed))))
            )
          (mapc #'update-fn urls )))))
  (with-html-document (:stream stream)
    (with-document-preamble (:stream stream)
      (declare-title "Modified pages" :stream stream))
    (with-document-body (:stream stream :background :white)
      (present store 'store :stream stream :view +modified-sites-of-store-view+))))

#|

; Fill the URL store

(let* ((one-day (* 60 60 23))
       (three-days (* 60 60 (- (* 24 3) 1)))
       (quarter-day (* 60 60 11))
       (url-descriptions
       `(("http://devworld.apple.com/dev/newton/newtondev.shtml"
          "Newton Developer World at Apple" ,three-days)
         ("http://www.macintouch.com/" "Macintouch" ,quarter-day)
         ("http://www.apple.com/" "Apple USA" ,three-days)
         ("http://www.apple.de/" "Apple Germany" ,three-days)
         ("http://www.ogrady.com/" "PowerBook info by Ogrady" ,one-day)
         ("http://www.macweek.com/" "MacWeek" ,one-day)
         ("http://www.newton.apple.com/" "Newton at Apple" ,three-days)
         ("http://www.maccentral.com/" "MacCentral" ,one-day)
         ("http://www1.theonly.com/index.html" "Newton: TheOnly" ,one-day)
         ("http://www.digitool.com/" "Digitool" ,three-days)
         ("http://www.ai.mit.edu/" "MIT AI Lab" ,three-days)
         ("http://wilson.ai.mit.edu/cl-http/cl-http.html" "CL-HTTP home page" ,three-days)
         ("http://www.harlequin.com/full/new.html" "Harlequin" ,three-days)
         ("http://www.franz.com" "Franz, Inc." ,three-days)
         ("http://www.bbtech.com/" "Blackboard Technology" ,three-days)
         ("http://www.nichimen.com/" "Nichimen" ,three-days)
         ("http://www.sun.com/" "SUN USA" ,three-days)
         ("http://sk8.research.apple.com/" "SK8 at Apple" ,three-days)
         ("http://www.cs.berkeley.edu/~russell/new.html" "AIMA" ,three-days)
         ("http://www.mkp.com/new/noteworthy.htm" "Morgan Kaufmann" ,three-days)
         ("http://www.lisp.de/" "Lisp in Germany" ,one-day)
         ("http://www.lavielle.de/" "Lavielle German" ,one-day)
         ("http://www-mitpress.mit.edu/new-releases.tcl" "MIT Press" ,three-days)
         ("http://d3.informatik.uni-wuerzburg.de/" "D3" ,three-days)
         ("http://www.lav.de/" "LAV German" ,one-day)
         ("http://www.lavielle.com/" "Lavielle English" ,one-day)
         ("http://www.rinzai.com/ni/" "Newton Intelligence" ,one-day )
         ("http://www.newtoninfo.com/" "Newton Info" ,one-day)
         ("http://www.macinsider.com/" "Mac Insider" ,one-day)
         ("http://www.um.org/" "User Modelling Inc" ,three-days)
         ("http://www.cyc.com/" "Cyc" ,three-days)
         ("http://www.artcom.de/~tim/DylanEvangelists/DylanEvangelists.html"
          "Dylan Evangelists" ,three-days)
         ("http://www.codemist.tc/" "Codemist Ltd." ,three-days)
         ("http://stony-brook.scrc.symbolics.com/www/index.html" "Symbolics Inc." ,three-days)
         ("http://kogs-www.informatik.uni-hamburg.de/~moeller/home.html"
          "Ralf M&ouml;ller" ,three-days))))
  (ccl::with-shown-progress (task "Put to Store" :denominator (length url-descriptions))
    (with-store (store *url-store-path*)
      (mapcar #'(lambda (description)
                  (destructuring-bind (url title &optional (interval (* 3 86400)))
                                      description
                    (ccl::show-progress task :note url)
                    (put-url-in-store store (make-instance 'persistent-url
                                              :string url
                                              :title title
                                              :check-interval interval))))
              url-descriptions))))


; show what's in the URL store

(ccl:process-run-function
 "Look for modified sites"
 (lambda ()
   (with-open-file (stream "Interactive:test.html" :direction :output :if-exists :supersede)
     (with-store (store *url-store-path*)
       (print-modified-sites-page store stream :update-p nil :print-info-p t)))
   (ccl:open-url "file:///Interactive/test.html" :no-cache t :activate-p t)))


; update the information in the URL store and show

(ccl:process-run-function
 "Look for modified sites"
 (lambda ()
   (with-open-file (stream "Interactive:test.html" :direction :output :if-exists :supersede)
     (with-store (store *url-store-path*)
       (print-modified-sites-page store stream :update-p t :force-p t :print-info-p t)))
   (ccl:open-url "file:///Interactive/test.html" :no-cache t :activate-p t)))

; force update the information in the URL store and show

(ccl:process-run-function
 "Look for modified sites"
 (lambda ()
   (with-open-file (stream "Interactive:test.html" :direction :output :if-exists :supersede)
     (with-store (store *url-store-path*)
       (print-modified-sites-page store stream :update-p t :print-info-p t)))
   (ccl:open-url "file:///Interactive/test.html" :no-cache t :activate-p t)))

(inspect
 (with-store (store *url-store-path*)
   (get-all-urls-from-store store)))

|#

#+cl-http-menu
(cl-http-menu:add-item-to-tool-menu
 "Look for modified sitesä"
 #'(lambda ()
     (with-open-file (stream "Interactive:test.html" :direction :output :if-exists :supersede)
       (with-store (store *url-store-path*)
         (print-modified-sites-page store stream :update-p t :print-info-p t)))
     (ccl:open-url "file:///Interactive/test.html" :no-cache t :activate-p t))
 "Looks for updates of your favorite URLs."
 :activate t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of File


