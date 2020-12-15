(defun model-group-p (content)
  (and (consp content)
       (or (cddr content)
           (consp (car content)))))

(defun simple-element-p (content)
  (and (atom content) content))

(defun unary-model-p (content)
  (and (consp content)
       (null (rest content))
       (simple-element-p (first content))))

(defun make-model-group (content)
  (let (connector
        models)
    (labels ((make (content)
               (cond ((not (consp content))
                      nil)
                     ((occurrence-name-p (first content))
                      (let ((elt (make-model (second content))))
                        (print :setting)
                        (setf (dtd-model.occurrence elt) (first content))
                        (push elt models))
                      (make (cddr content)))
                     ((connector-name-p (first content))
                      (unless (eq (first content) connector)
                        (if (null connector) 
                          (setf connector (first content))
                          (xml-form-error content
                                          "multiple connectors in a model group: ~s/~s."
                                          connector (first content))))
                      (make (cdr content)))
                     (t
                      (push (make-model (first content)) models)
                      (make (cdr content))))))
      (make content)
      (make-instance 'dtd-model-group
        :parent *parent-node*
        :content models
        :connector connector))))


(defMethod make-model ((content cons))
  (if (null (cdr content))
    (make-model (first content))
    (cond ((unary-model-p content)
           (make-instance 'dtd-element-model
             :parent *parent-node*
             :content (dtd-element-reference (first content))))
          ((model-group-p content) (print :model-group-p)
           (make-model-group (reverse content)))
          (t
           (make-instance 'dtd-element-model
               :parent *parent-node*
               :content (if (simple-element-p (first content))
                          (first content)
                          (make-model (first content)))
               :occurrence (second content))))))

(defMethod make-model ((content null))
  nil)

(defMethod make-model ((content dtd-model))
  content)

(defMethod make-model ((content symbol))
  (cond ((reserved-name-p content)
         (make-instance 'dtd-element-model
           :parent *parent-node*
           :content (dtd-element-reference content)
           :occurrence 1))
        ((simple-element-p content)
         (make-instance 'dtd-model-group
           :parent *parent-node*
           :content (dtd-element-reference content)
           :occurrence 1))
        (t
         (xml-form-error *parent-node*
                         "illegitimate model content: ~s." content))))
