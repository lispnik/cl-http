;;; -*- Syntax: Ansi-Common-Lisp; Package: html-parser; Base: 10; Mode: lisp -*-

;;; File: html-parser.lisp
;;; Last edited by smishra on Thu Oct 22 17:25:05 1998

;;; (c) Copyright 1996-98, Sunil Mishra (smishra@cc.gatech.edu)
;;;     All Rights Reserved

(in-package :html-parser)

;;;----------------------------------------
;;; Parser Initialization

(declaim (inline file-newer-p probe-file*))

(defun file-newer-p (file1 file2)
  (> (file-write-date file1) (file-write-date file2)))

(defun probe-file* (pathname)
  #+(and (or lispworks mcl cmu) (not cl-http))
  (probe-file pathname)
  #+cl-http
  (http::probe-file* pathname))

(defun initialize-parser (&optional (doctype *default-dtd*))
  "This is the initialization function for initializing the DTD. The
argument is used to specify the DTD the user wishes to load. If not
compiled, the DTD compiler is loaded and invoked. The root for the
DTD is then located, and the lexer is initialized.

To modify the list of DTD's available, look at the definition of
*HTML-DTD-LIST* in defs.lisp."
  (setq *html-tags* nil)
  ;; Load the DTD
  (let* ((dtd-name (cadr (assoc doctype *html-dtd-list* :test #'string=)))
         (src-file (make-pathname :host "HTML-PARSER"
                                  :name dtd-name
                                  :type *source-file-extension*))
         (fasl-file (make-pathname :host "HTML-PARSER"
                                   :name dtd-name
                                   :type *compiled-file-extension*)))
    (declare (dynamic-extent fasl-file src-file))
    (when (or (not (probe-file* fasl-file))
              (file-newer-p src-file fasl-file))
      (compile-file src-file))
    (load fasl-file))
  (set-html-tag-backpointers)
  (set-html-tag-default-containers)
  (define-unknown-tag)
  (initialize-parse-dispatch-functions)
  (setq *current-dtd* doctype)
  t)

;;;----------------------------------------

;;;----------------------------------------
;;; Utility functions

(declaim (inline last-saved-structure tag-on-stack-p))

(defun last-saved-structure (parser)
  (let ((pd (find-if #'(lambda (tag-data)
                         (pd-storep tag-data))
                     (parser-stack parser))))
    (and pd (pd-instance pd))))

(defun tag-on-stack-p (tag parser)
  (some #'(lambda (pd)
            (eql (name (pd-instance pd)) (name tag)))
        (parser-stack parser)))

(declaim (inline valid-content-p))

(defun valid-content-p (tag-name container)
  ;; This is a separate function because it ought to be a great deal
  ;; more complex. The function should check with the containment rules
  ;; to ensure that the tag-name can be contained within, taking the
  ;; algebra into consideration. I don't need that.
  (member tag-name (flatten (contains container))))

(defun valid-container-p (container tag-name)
  (or (eql (class-name (class-of container)) 'unknown-tag-instance)
      (and (valid-content-p tag-name container)
           (not (member tag-name (exclusions container))))))

(defun find-unknown-tag-container (parser)
  ;; Find the first tag with non-empty content, and return it
  ;; Can a tag that has become empty due to exclusions contain an unknown tag?
  ;;   Tentatively, yes.
  (loop with search-start = nil
        for pd in (parser-stack parser)
        for tag-instance = (pd-instance pd)
        for tag-defn = (instance-of tag-instance)
        unless search-start
          do (setq search-start tag-instance)
        when (or (typep tag-instance 'unknown-tag-instance) (contains tag-defn))
          return tag-instance
        when (inclusions tag-defn)
          return search-start))

(defmethod find-tag-container (parser (tag-instance unknown-tag-instance))
  (find-unknown-tag-container parser))

(defmethod find-tag-container (parser (token html-name-token))
  ;; 1. Look at next tag
  ;; 2. If token is in content, then set last-container to that
  ;; 3. If token is in exclusions, then set search-start to the next tag,
  ;;    and set last-container to nil
  ;; 4. If token is in inclusions, then return the tag at search-start
  ;; 5. When the stack is exhausted, return last-container
  (loop with search-start = nil
        and last-container = nil
        for pd in (parser-stack parser)
        for tag-instance = (pd-instance pd)
        unless search-start
          do (setq search-start tag-instance)
        unless last-container
          do (setq last-container (and (valid-container-p tag-instance token) tag-instance))
        if (member token (exclusions tag-instance))
          do (setq last-container nil search-start nil)
        else if (member token (inclusions tag-instance))
          return search-start
        finally (return last-container)))

(defmethod find-tag-container (parser (tag-instance abstract-tag-instance))
  (find-tag-container parser (name tag-instance)))

;;;----------------------------------------

;;;----------------------------------------
;;; Context expansion

(defun sort-forms-by-separators (forms separators)
  (when forms
    (assert (member (car forms) separators))
    (flet ((context-form-p (form)
             (find form separators)))
      (declare (dynamic-extent #'context-form-p))
      (loop for last-keyword = forms then next-keyword
            for next-keyword = (member-if #'context-form-p (cdr last-keyword))
            if (member (car last-keyword) keywords :key #'car)
              do (error "Repeated keyword ~A" (car last-keyword))
            collect (ldiff last-keyword next-keyword) into keywords
            while next-keyword
            finally (return (apply #'values
                                   (mapcar #'(lambda (separator)
                                               (cdr (assoc separator keywords)))
                                           separators)))))))

(defun make-executable-context-form (condition form it-var form-substs)
  (flet ((make-token (object)
           (if (typep object 'html-name-token)
	       object
	       (tokenize-name (string object)))))
    (declare (inline make-token))
    (let ((re-form (rewrite-sexp form :parser-defn :recursive t :duplicates t
				 :bindings form-substs)))
      (cond ((eq condition :any) re-form)
            ((atom condition)
             `((when (eq (name ,it-var) ,(make-token condition))
                 ,.re-form)))
            (t `((when (member (name ,it-var) ',(mapcar #'make-token condition))
                   ,.re-form)))))))

(defun %save-tag (it its-pd parser)
  (let ((container (last-saved-structure parser)))
    (typecase it
      (abstract-tag-instance
       (unless (pd-storep its-pd)
         (setf (pd-storep its-pd) t)
         (when container
           (setf (part-of it) container)
           (push it (parts container)))))
      (t (when container
           (push it (parts container)))))))

(defun open-tag (it start end parser)
  (let ((stack (parser-stack parser))
        (container (or (eq (name it) *html-root*)
                       (find-tag-container parser it))))
    (cond ((eq (name it) *html-root*)
           (when stack
             (close-tag *html-root* start end parser)))
          (container
           (close-tags-before-container container start end parser))
          (t (open-tag (make-instance (parser-tag-instance-class parser)
                         :instance-of (tag-definition (or (default-container it)
                                                          (car (containers it)))))
                       start end parser))))
  (let ((its-pd (make-tag-parser-data :instance it :start-pos start :end-pos end)))
    (push its-pd (parser-stack parser))
    (when (parser-open-tag-fn parser)
      (funcall (parser-open-tag-fn parser) it its-pd start end parser))))

(defun close-tag (tag start end parser)
  (when (tag-on-stack-p tag parser)
    (loop for rest-pds on (parser-stack parser)
	  for its-pd = (car rest-pds)
	  for it = (pd-instance its-pd)
	  when (parser-save-fragments parser)
            do (setf (html-fragment it)
		     (subreference (parser-input parser) (pd-start-pos its-pd) end))
	  do (setf (parts it) (nreverse (parts it)))
             (when (parser-close-tag-fn parser)
	       (funcall (parser-close-tag-fn parser) it its-pd start end parser))
	  until (eq (name tag) (name it))
	  finally (setf (parser-stack parser) (cdr rest-pds)))))

(defun close-tags-before-container (container start end parser)
  (let ((stack (parser-stack parser)))
    (or (eq (pd-instance (car stack)) container)
        (do ((rest-pds stack (cdr rest-pds)))
            ((or (null (cdr rest-pds))
                 (eq (pd-instance (cadr rest-pds)) container))
             (if (null (cdr rest-pds))
               (error "Tag ~A not found on stack" container)
               (close-tag (pd-instance (car rest-pds)) start end parser)))))))

(defun add-pcdata-as-needed (it start end parser)
  (let ((container (find-tag-container parser #t"PCDATA")))
    (when (or (and container (eq container (car (parser-stack parser))))
              (not (stringp it))
              (notevery #'html-whitespace-p it))
      (if container
        (close-tags-before-container container start end parser)
        (open-tag (make-instance (parser-tag-instance-class parser)
                    :instance-of (tag-definition *pcdata-default-container*))
                  start end parser))
      (when (parser-pcdata-fn parser)
        (funcall (parser-pcdata-fn parser) it start end parser)))))

;;; There is a small problem with the way return values from contexts
;;; work. Let me give the example of my template matcher. The body
;;; matcher context returns #t"body" when it is done. If the document
;;; does not have an explicity </body>, the close of body appears as
;;; an *eof*. So, the end result of the parse is the parsed body tag.
;;; However, the program would like to have the parsed HTML tag as
;;; the end result. There is no way to reconcile this conflict at
;;; present, and is quite frankly not worth the effort. It will most
;;; likely involve a fairly clever language extension.

;;; Add an on-eof statement to the contexts. That way, if the parser
;;; runs out of text, we can tell it what to do. It will do that, and
;;; the exit value shall be set to HTML. While closing the remaining
;;; open tags, the body forms shall be run, which will set the space
;;; model of the matcher-data, while the exit value shall not be
;;; affected since one has already been set. I think this solution
;;; shall take care of this issue adequately.

(defmacro ignoring-exits (&rest forms)
  `(let ((ignore-exits t))
    (declare (special ignore-exits))
    ,.forms))

(defmacro define-html-parser-context (name arguments
				      &rest forms
				      &aux (exitp-var (gensym))
				           (exit-value-var (gensym))
					   (parser-var (intern "PARSER" *package*))
					   (it-var (intern "IT" *package*))
					   (its-pd-var (intern "ITS-PD" *package*))
					   (start-var (intern "START" *package*))
					   (end-var (intern "END" *package*))
					   (save-var (intern "SAVE" *package*))
					   (exit-context-var (intern "EXIT-CONTEXT" *package*)))
  (let ((form-substs `((?exitp-var . ,exitp-var)
		       (?exit-value-var . ,exit-value-var)
		       (?exit-context . ,exit-context-var)
		       (?save . ,save-var)
		       (?it . ,it-var)
		       (?its-pd . ,its-pd-var))))
    (declare (dynamic-extent form-substs))
    (multiple-value-bind
	  (use-variables on-open-tag on-pcdata on-close-tag on-eof)
	(sort-forms-by-separators forms
				  '(:use-variables :on-open-tag :on-pcdata :on-close-tag :on-eof))
      (declare (dynamic-extent use-variables on-open-tag on-pcdata on-close-tag on-eof))

      `(defun ,name (parser ,.arguments
		     ,@(unless (member '&aux arguments) '(&aux))
		     ,.use-variables ,exitp-var ,exit-value-var ignore-exits)
	(declare (special ignore-exits))
	(labels ((,save-var (it &optional its-pd)
                   (%save-tag it its-pd parser))
                 ,.(when on-open-tag
		     `((execute-open-tag-forms (,it-var ,its-pd-var ,start-var ,end-var ,parser-var)
                         ,it-var ,start-var ,end-var ,parser-var
                         (unless (or (member ',name (pd-contexts ,its-pd-var)) ,exitp-var)
		           (push ',name (pd-contexts ,its-pd-var))
			   ,.(loop for form in on-open-tag
				   nconc (make-executable-context-form (car form) (cdr form)
								       it-var form-substs))))))
                 ,.(when on-close-tag
                     `((execute-close-tag-forms (,it-var ,its-pd-var ,start-var ,end-var
                                                         ,parser-var)
                         ,it-var ,its-pd-var ,start-var ,end-var ,parser-var
                         (unless ,exitp-var
                           ,.(loop for form in on-close-tag
                                   nconc (make-executable-context-form (car form) (cdr form)
                                                                       it-var form-substs))))))
                 ,.(when on-pcdata
                     `((execute-pcdata-forms (,it-var ,start-var ,end-var ,parser-var
                                                      &aux (,its-pd-var nil))
                         ,its-pd-var ,it-var ,start-var ,end-var ,parser-var
                         (unless ,exitp-var
                           ,.(make-executable-context-form :any on-pcdata it-var form-substs)))))
		 ,.(when on-eof
		     `((execute-eof-forms (,start-var ,end-var ,parser-var)
			,start-var ,end-var ,parser-var
			(unless ,exitp-var
			  ,.(make-executable-context-form :any on-eof nil form-substs))))))
          (declare (dynamic-extent ,@(when on-open-tag '(#'execute-open-tag-forms))
                                   ,@(when on-close-tag '(#'execute-close-tag-forms))
                                   ,@(when on-pcdata '(#'execute-pcdata-forms))
				   ,@(when on-eof '(#'execute-eof-forms)))
                   (inline ,save-var))
          (setf (parser-open-tag-fn parser) ,(when on-open-tag '#'execute-open-tag-forms)
                (parser-close-tag-fn parser) ,(when on-close-tag '#'execute-close-tag-forms)
                (parser-pcdata-fn parser) ,(when on-pcdata '#'execute-pcdata-forms))
	  ;; test the tags on stack with the open-tag forms
	  (let ((queue (reverse (parser-stack parser))))
	    (declare (dynamic-extent queue))
	    (loop for pd in queue
		  do (execute-open-tag-forms (pd-instance pd) pd (pd-start-pos pd)
					     (pd-end-pos pd) parser)
		  when ,exitp-var
		    return (values ,exit-value-var nil)))
	  ;; get more tokens from the input
	  (loop with token-type and token and token-start and token-end
		do (multiple-value-setq (token-type token token-start token-end)
		     (next-html-token parser))
		   (case token-type
		     ((:comment :declaration) nil)
		     (:open-tag (open-tag token token-start token-end parser))
		     (:close-tag (close-tag token token-start token-end parser))
		     ((:character :pcdata :entity)
		      (add-pcdata-as-needed token token-start token-end parser))
		     (t (assert (eq token-type *eof*))
			,@(when on-eof
				`((execute-eof-forms token-start token-end parser)))
			(close-tag *html-root* token-start token-end parser)))
		until (or ,exitp-var (eq token-type *eof*))
		finally (return (values ,exit-value-var (eq token-type *eof*)))))))))

;;;----------------------------------------

;;;----------------------------------------
;;; Parser expanstion

(defun make-context-transitions (transitions parser-var last-context-var last-value-var)
  `(cond ,.(mapcar #'(lambda (transition)
                       (if (eq (car transition) :start)
                         (let ((next-context (cadr transition)))
                           (assert (null (cddr transition)))
                           `((eq ,last-context-var :start)
                             ,(if (eq next-context :end)
                                `(values :end ,last-value-var)
                                `(values ',(car next-context)
                                         (,(car next-context) ,parser-var ,.(cdr next-context))))))
                         (let ((next-context (caddr transition))
                               (switch-test (cadr transition)))
                           (assert (null (cdddr transition)))
                           `(,(cond ((eq switch-test t)
				     `(eq ,last-context-var ',(car transition)))
				    ((or (symbolp switch-test) (stringp switch-test))
				     `(and (eq ,last-context-var ',(car transition))
                                           (find-method #'name nil
                                                        (list (class-of ,last-value-var)) nil)
				           (eq (name ,last-value-var)
					       ,(tokenize-name (string switch-test)))))
				    ((typep switch-test 'html-name-token)
				     `(and (eq ,last-context-var ',(car transition))
                                           (find-method #'name nil
                                                        (list (class-of ,last-value-var)) nil)
				           (eq (name ,last-value-var) ,switch-test)))
				    ((atom switch-test)
				     (error "Unrecognized test condition ~A" switch-test))
				    ((eq (car switch-test) 'function)
				     `(and (eq ,last-context-var ',(car transition))
				           (funcall ,switch-test ,last-value-var)))
				    ((eq (car switch-test) :eval)
				     `(and (eq ,last-context-var ',(car transition))
				           (eq ,last-value-var (progn ,(cdr switch-test)))))
				    (t (error "Unrecognized test condition ~A" switch-test)))
                             ,(if (eq next-context :end)
                                `(values :end ,last-value-var)
                                `(multiple-value-bind
                                   (,last-value-var eofp)
                                   (,(car next-context) ,parser-var ,.(cdr next-context))

                                   (if eofp
                                     (values :end ,last-value-var)
                                     (values ',(car next-context) ,last-value-var))))))))
	    transitions)
    (t (error "Unspecified action for transition condition"))))

(defmacro define-html-parser (name args &rest forms
                              &aux (input-var (gensym))
			           (save-frag-var (gensym))
				   (parser-var (gensym))
				   (last-context-var (gensym))
				   (last-value-var 'return-value)
				   (tag-instance-class-var (gensym))
				   (unknown-instance-class-var (gensym)))
  (multiple-value-bind
    (initialization transitions)
    (sort-forms-by-separators forms '(:initialization :transitions))
    (declare (dynamic-extent initialization transitions))

    (let* ((key-args (member '&key args))
           (post-key-args (when (null key-args)
                            (or (member '&rest args)
                                (member '&aux args)))))
      `(progn
         (defun ,name (,input-var ,.(ldiff args (or key-args post-key-args))
                                  &key ((:save-fragments ,save-frag-var) nil)
				       ((:html-tag-instance-class ,tag-instance-class-var)
					'html-tag-instance)
				       ((:unknown-tag-instance-class ,unknown-instance-class-var)
					'unknown-tag-instance)
                                       ,.(if key-args (cdr key-args) post-key-args))
           (flet ((do-transition (,parser-var ,last-context-var ,last-value-var)
                    ,(make-context-transitions transitions parser-var
                                               last-context-var last-value-var)))
             ,.initialization
             (loop with ,last-context-var = :start
                   and ,last-value-var
                   and ,parser-var = (make-parser
				      :input ,input-var
				      :save-fragments ,save-frag-var
				      :tag-instance-class ,tag-instance-class-var
				      :unknown-instance-class ,unknown-instance-class-var)
                   do (multiple-value-setq (,last-context-var ,last-value-var)
                        (do-transition ,parser-var ,last-context-var ,last-value-var))
                   until (eq ,last-context-var :end)
                   finally (return ,last-value-var))))))))

;;;----------------------------------------

;;;----------------------------------------
;;; At startup...

(initialize-parser)

;;;----------------------------------------
