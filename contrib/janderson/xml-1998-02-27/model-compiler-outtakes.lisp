`(lambda (content-list succeed fail)
     (let ((*model* ,model)
           (iterate nil)
           (rest-cont (function
                          ,(model-content-predicate nil nil connector rest))))
       (declare (special *model*))
        (setf iterate
              (function
               (lambda (content-list)
                 (if (null content-list)
                   (model-succeed nil succeed fail)
                   (,(model-element-predicate model)
                    content-list
                    (succeed-with (_content-list _fail)
                                  (declare (ignore _fail))
                                  (funcall iterate (rest _content-list)))
                    #|(fail-with (_content-list _succeed _model)
                               (declare (ignore _content-list _succeed _model))
                               (funcall rest-cont content-list succeed fail))|#
                    (fail-with (_content-list _succeed _model)
                               (declare (ignore _content-list _succeed _model))
                               (model-succeed (cons nil content-list)
                                              succeed fail))
                    )))))
      (funcall iterate content-list)))
