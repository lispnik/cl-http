;;; -*- package: "XML-PARSER"; -*-


(in-package "XML-PARSER")

(defParameter *markup-menu-title* "Markup")
(defParameter *dtd-menu-title* "DTD's ...")
(defParameter *load-menu-title* "Load DTD ...")
(defParameter *element-menu-title* "Elements ...")
(defParameter *dtd-directory* "dtd")

(defMethod make-node-menu-item
           ((node xml-node))
  (make-instance 'menu-item
    :menu-item-title (node-title node)
    :menu-item-action #'(lambda () (inspect node))))

(defMethod node-title
           ((node xml-node))
  (string (xml-node.name node)))

(defMethod node-title
           ((node symbol))
  (string node))

(defun sort-nodes
       (nodes)
  (sort (copy-list nodes) #'string-lessp
        :key #'(lambda (node) (node-title node))))
  

(defun make-element-menu-items
       (&aux (alpha-elements nil))
  (flet ((cache-element (e)
           (let* ((id (elt (string (element-declaration.name e)) 0))
                  (cache (assoc id alpha-elements :test #'char-equal)))
             (if cache
               (push e (rest cache))
               (push (list id e) alpha-elements)))))
    (mapcar #'cache-element (element-declaration-instances))
    (setf alpha-elements (sort alpha-elements #'char-lessp :key #'first))
    (mapcar #'(lambda (elements)
                (make-instance 'menu
                  :menu-title (format nil "~c elements (~d)"
                                      (first elements)
                                      (length (rest elements)))
                  :menu-items
                  (mapcar #'make-node-menu-item
                          (sort-nodes (rest elements)))))
            alpha-elements)))

(defun make-dtd-menu-items
       ()
  (mapcar #'(lambda (dtd)
              (make-instance 'menu
                :menu-title
                (format nil "~a <~a>" (document-type.name dtd) (xml-node.system-id dtd))
                :menu-items
                (list*
                 (make-instance 'menu-item
                   :menu-item-title
                   "Inspect"
                   :menu-item-action
                   #'(lambda () (inspect dtd)))
                 (mapcar #'(lambda (opt &aux (title (first opt))
                                             (accessor (second opt))
                                             (nodes (funcall accessor dtd))) 
                             (make-instance 'menu
                               :menu-title
                               (format nil "~a (~d)" title (length nodes))
                               :menu-items
                               (mapcar #'(lambda (node)
                                           (make-node-menu-item node))
                                       (sort-nodes nodes))))
                         `(("Elements ..." ,#'document-type.element-declarations)
                           ("&Entities ..." ,#'document-type.general-entity-declarations)
                           ("%Entities ..." ,#'document-type.parameter-entity-declarations)
                           ("Notations ..." ,#'document-type.notation-declarations))))))
          (document-type-instances)))

(defun make-load-menu-items ()
  (mapcar #'(lambda (path)
              (make-instance 'menu-item
                :menu-item-title (pathname-name path)
                :menu-item-action
                #'(lambda ()
                    (process-run-function
                     (format nil "Load DTD: <~a>" path)
                     'run-dtd-load-process
                     (pathname-name path)
                     path))))
          (let ((path (make-pathname
                       :directory
                       (directory-namestring (choose-file-default-directory))
                       :name :WILD
                       :type "DTD")))
            (directory path))))

(defun run-dtd-load-process
       (name source)
  (when *load-verbose*
    (format *trace-output* "loading dtd from source: ~s..." source))
  (handler-case
    (with-cursor *watch-cursor*
      (load-dtd name source))
    (error (condition)
           (when
             (y-or-n-dialog
              (format nil "an error occurred while loading dtd <~a>:~%~a~%in node ~s."
                      source condition *processed-node*)
              :yes-text "Retry")
             (run-dtd-load-process name source)))))
;(read-dtd-stream (choose-file-dialog) nil)
                    
(defun update-doctype-menu
       (&optional (menu (let ((menu (find-menu *markup-menu-title*)))
                          (when menu (find-menu-item menu *dtd-menu-title*)))))
    (when menu
      (apply #'remove-menu-items menu (menu-items menu))
      (add-menu-items menu
                      (make-instance 'menu-item
                        :menu-item-title "Update"
                        :menu-item-action
                        #'(lambda ()
                            (with-cursor *watch-cursor*
                              (update-doctype-menu))))
                      (make-instance 'menu-item
                        :menu-item-title "-"))
      (apply #'add-menu-items menu (make-dtd-menu-items))))

(defun update-element-menu
       (&optional (menu (let ((menu (find-menu *markup-menu-title*)))
                          (when menu (find-menu-item menu *element-menu-title*)))))
    (when menu
      (apply #'remove-menu-items menu (menu-items menu))
      (add-menu-items menu
                      (make-instance 'menu-item
                        :menu-item-title "Update"
                        :menu-item-action
                        #'(lambda ()
                            (with-cursor *watch-cursor*
                              (update-element-menu))))
                      (make-instance 'menu-item
                        :menu-item-title "-"))
      (apply #'add-menu-items menu (make-element-menu-items))))

(defun update-dtd-load-menu
       (&optional (menu (let ((menu (find-menu *markup-menu-title*)))
                          (when menu (find-menu-item menu *load-menu-title*)))))
  (when menu
    (apply #'remove-menu-items menu (menu-items menu))
    (add-menu-items menu
                    (make-instance 'menu-item
                      :menu-item-title "Choose DTD..."
                      :menu-item-action
                      #'(lambda ()
                          (let ((path (choose-file-dialog)))
                            (process-run-function
                             (format nil "Load DTD: <~a>" path)
                             'run-dtd-load-process
                             (pathname-name path)
                             path))))
                    (make-instance 'menu-item
                      :menu-item-title "Update"
                      :menu-item-action
                      #'(lambda ()
                          (with-cursor *watch-cursor*
                            (update-dtd-load-menu))))
                    (make-instance 'menu-item
                      :menu-item-title "-"))
    (apply #'add-menu-items menu (make-load-menu-items))))

(defParameter *markup-menu*
  (make-instance 'menu
    :menu-title *markup-menu-title*
    :menu-items
    (list (make-instance 'menu
            :menu-title *dtd-menu-title*
            :menu-items nil)
          (make-instance 'menu
            :menu-title *element-menu-title*
            :menu-items nil)
          (make-instance 'menu
            :menu-title *load-menu-title*
            :menu-items nil)
          (make-instance 'window-menu-item
            :menu-item-title "Read DTD Buffer"
            :menu-item-action #'(lambda () (read-dtd-buffer))))))

(defMethod read-dtd-buffer
           ((window fred-window) &aux (pathname (pathname (front-window))))
  (cond ((or (string-equal "DTD" (pathname-type pathname)) (command-key-p))
         (process-run-function
          (format nil "Read Buffer DTD: <~a>" pathname)
          'run-dtd-load-process
          (pathname-name pathname)
          window))
        (t
         (ed-beep))))


(let ((old (find-menu (menu-title *markup-menu*))))
  (when old (menu-deinstall old))
  (menu-install *markup-menu*))

(update-doctype-menu)
(update-element-menu)
(update-dtd-load-menu)

(defMethod read-dtd-stream :after
           ((source t) &rest args)
  (declare (Ignore args source))
  (update-doctype-menu))

"XMLP"
