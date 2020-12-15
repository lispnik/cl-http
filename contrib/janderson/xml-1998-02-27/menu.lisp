;; -*- package: ("XML-PARSER")
;;;
;;; this version (C) mecom gmbh 24.11.97
;;; available only from the cl-http repository and NOT to be REdistributed
;;; in ANY form. see cl-xml.html.

(in-package :XML-PARSER)

(defParameter *markup-menu-title* "Markup")
(defParameter *dtd-menu-title* "DTD's ...")
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
           ((node dtd-element-reference))
  (string (dtd-element-reference.qualified-name node)))

(defun sort-nodes
       (nodes)
  (sort (copy-list nodes) #'string-lessp
        :key #'(lambda (node) (node-title node))))
  

(defun make-element-menu-items
       (&aux (alpha-elements nil))
  (flet ((cache-element (e)
           (let* ((id (elt (string (dtd-element.name e)) 0))
                  (cache (assoc id alpha-elements :test #'char-equal)))
             (if cache
               (push e (rest cache))
               (push (list id e) alpha-elements)))))
    (mapcar #'cache-element (defined-dtd-elements))
    (setf alpha-elements (sort alpha-elements
                               #'char-lessp
                               :key #'first))
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
                (format nil "~a <~a>" (dtd.name dtd) (dtd.url dtd))
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
                         `(("Elements ..." ,#'dtd.elements)
                           ("Entities ..." ,#'dtd.entities)
                           ("Notations ..." ,#'dtd.notations))))))
          (dtd-instances)))

(defun run-dtd-load-process
       (path)
  (when *load-verbose*
    (format *trace-output* "loading dtd from path: ~s..." path))
  (handler-case
    (let ((dtd (dtd (pathname-name path))))
      (if dtd
        (if (y-or-n-dialog (format nil
                                   "clear existing definitions from ~%~s?"
                                   dtd))
          (%dtd-clear dtd))
        (setf dtd
              (make-instance (node-class 'document-type-definition
                                         nil
                                         path)
                :name (pathname-name path)
                :url (url-namestring path nil))))
      (with-cursor *watch-cursor*
        (read-dtd-stream path dtd)))
    (error (condition)
           (when
             (y-or-n-dialog
              (format nil "an error occurred while loading dtd <~a>:~%~a"
                      path condition)
              :yes-text "Retry")
             (run-dtd-load-process path)))))
                    
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

(defParameter *markup-menu*
  (make-instance 'menu
    :menu-title *markup-menu-title*
    :menu-items
    (list (make-instance 'menu
            :menu-title *dtd-menu-title*
            :menu-items
            (list (make-instance 'menu-item
                    :menu-item-title "Update"
                    :menu-item-action
                    #'(lambda ()
                        (with-cursor *watch-cursor*
                          (update-doctype-menu))))
                  (make-instance 'menu-item
                    :menu-item-title "-")))
          (make-instance 'menu
            :menu-title *element-menu-title*
            :menu-items
            (list (make-instance 'menu-item
                    :menu-item-title "Update"
                    :menu-item-action
                    #'(lambda ()
                        (with-cursor *watch-cursor*
                          (update-element-menu))))
                  (make-instance 'menu-item
                    :menu-item-title "-")))
          (make-instance 'menu
            :menu-title "Load DTD"
            :menu-items
            (mapcar #'(lambda (path)
                        (make-instance 'menu-item
                          :menu-item-title (pathname-name path)
                          :menu-item-action
                          #'(lambda ()
                              (process-run-function
                               (format nil "Load DTD: <~a>" path)
                               'run-dtd-load-process
                               path))))
                    (let ((path (make-pathname
                                 :directory
                                 (directory-namestring *dtd-pathname-defaults*)
                                 :name :WILD
                                 :type "DTD")))
                      (directory path)))))))


(let ((old (find-menu (menu-title *markup-menu*))))
  (when old (menu-deinstall old))
  (menu-install *markup-menu*))

(defMethod read-dtd-stream :after
           ((source stream) (dtd dtd))
  (declare (ignore dtd))
  (update-doctype-menu))

:EOF
