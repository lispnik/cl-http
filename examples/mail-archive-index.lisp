;;;   -*- mode: lisp; package: http; base: 10; syntax: ansi-common-lisp; default-character-style: (:fix :roman :normal);-*-

;;; (C) Copyright 1997, Andrew J. Blumberg & John C. Mallery
;;;     All Rights Reserved.
;;;
;;;
;;;------------------------------------------------------------------- 
;;;
;;; Adds indexing functionality to the mail-archive.
;;; See the files in http:lambda-ir;* for the background code.
;;;
;;;
;;; To index a mail archive, you need to specify the classes to use by either setting
;;; the special variables or providing the classes as export arguments.
;;;
;;; Make all mail archive index:
;;;
;;; (setq *mail-archive-class* 'indexing-mail-archive
;;;       *message-class* 'indexing-message
;;;       *thread-class* 'indexing-thread)
;;; 
;;; Export a mail archive.
;;; 
;;; Note that all classes must be passed in and that recache-p must be T or
;;; the indexing code loses.   4/23/97 -- JCMa.
;;; (http:export-url "http://wilson.ai.mit.edu/cl-http/archives/www-cl.html"
;;;              :mail-archive
;;;              :mailing-list "WWW-CL@ai.mit.edu"
;;;              :pathname "wilson:>http>mail>WWW-CL.text"
;;;              :title "World Wide Web and Common Lisp"
;;;              :recache-p t
;;;              :mail-archive-class 'http:indexing-mail-archive
;;;              :message-class 'http:indexing-message
;;;              :thread-class 'http:indexing-thread)
;;;
;;; provide calling interface 4/23/97 -- JCMa.

;;; (http:export-url "http://wilson.ai.mit.edu/cl-http/archives/www-cl.html"
;;;              :mail-archive
;;;              :mailing-list "WWW-CL@ai.mit.edu"
;;;              :pathname "wilson:>http>mail>WWW-CL.text"
;;;              :title "World Wide Web and Common Lisp"
;;;              :recache-p t
;;;              :mail-archive-class 'http:indexing-mail-archive
;;;              :message-class 'http:indexing-message
;;;              :thread-class 'http:indexing-thread)



(in-package :http)

(defclass indexing-mail-archive 
          (mail-archive)
    ())

(defclass indexing-thread
          (thread)
    ())

(defclass indexing-message
          (message)
    ())

;; export classes people need to reference 4/23/97 -- JCMa.
(export '(indexing-mail-archive indexing-thread indexing-message) :http)

;; add condition handling 4/24/97 -- JCMa.
(defmacro handling-mail-archive-indexing-errors ((mail-archive &key operation) &body body)
  `(handler-case-if (not *debug-server*)
      (progn . ,body)
     (error (error)
	    (let ((error-type (type-of error)))
	      (report-bug *bug-http-server*
			  (format nil "Mail HyperArchive Indexing Error: ~S" error-type)
			  "~:[~;Operation: ~:*~A~]~&URL: ~A~&Error: ~S~:[~;~&Error Report: ~:*~A~]"
			  ,operation (name-string (ma-url ,mail-archive)) error-type (report-string error))))))


;;;------------------------------------------------------------------- 
;;;
;;; DOCUMENT BUFFER RESOURCE
;;;

;; add thread safe document buffer 4/23/97 -- JCMa.
(defparameter *document-buffer-size* 5000
  "The default size of document buffers.")

(defun make-document-buffer (resource &optional (size *document-buffer-size*))
  (declare (ignore resource))
  (make-array size :element-type *standard-character-type* :adjustable t :fill-pointer t))

(defun match-document-buffer-p (resource buffer size)
  (declare (ignore resource))
  (<= size (array-total-size buffer)))

;; this must allocate and deallocate rapidly
(defresource document-buffer (resource &optional (size *document-buffer-size*))
  :constructor make-document-buffer
  :matcher match-document-buffer-p)

(define clear-document-buffer-resource ()
  (clear-resource 'document-buffer))

(defvar *document-buffer* nil
  "Holds the document buffer for a server instance.")

(define-macro with-document-buffer ((&key (size *document-buffer-size*)) &body body)
  `(using-resource (*document-buffer* document-buffer ,size)
     ,@body))


;;;------------------------------------------------------------------- 
;;;
;;; SCANNING MESSAGE LINES
;;;

;; Smarten up for efficiency 4/23/97 -- JCMa.
(defmethod scan-message-line :after ((mail-archive indexing-mail-archive) stream delimiters eof buffer)
  (declare (ignore delimiters eof stream))
  (let* ((buff-size (fill-pointer buffer))
         (doc-buffer *document-buffer*)
         (size (array-total-size doc-buffer))
         (start (fill-pointer doc-buffer))
         (end (+ start (the fixnum buff-size))))
    ;; Adjust the Document array as necessary
    (unless (< end size)
      ;; resource tracks allocated resources. Need to be able to handle the growth factor.  4/23/97 -- JCMa.
      (setq doc-buffer (adjust-array doc-buffer (max end (floor (* (the fixnum size) 1.1)))
                                     :element-type *standard-character-type* :fill-pointer start)
            *document-buffer* doc-buffer))
    ;; Use fast copying
    (www-utils:copy-vector-portion buffer 0 buff-size doc-buffer start end)
    (setf (fill-pointer doc-buffer) end)
    doc-buffer))

(defun index-mail-message (mail-archive document-name)
  (with-slots (search-url) mail-archive
    (handling-mail-archive-indexing-errors (mail-archive :operation "Index Document")
      (let ((document (lambda-ir::make-document document-name 
						`((:url ,(msg-make-url-string search-url document-name))) 
						'(:text))))
	(catch :already-exists
	  (lambda-ir::add-document lambda-ir::*document-context* document
				   :type-list '(:text)
				   :code-list (list #'lambda-ir::read-text-file))
	  (lambda-ir::tokenize-and-apply-lexical-features lambda-ir::*document-context*
							  *document-buffer* 
							  :document-object 
							  document))))))

(defmethod scan-message-body :around ((mail-archive indexing-mail-archive) file-stream message-delimiter plist)
  (declare (ignore file-stream message-delimiter))
  (with-document-buffer (:size *document-buffer-size*)	;use a resource   4/23/97 -- JCMa.
    (setf (fill-pointer *document-buffer*) 0)
    (multiple-value-prog1
      (call-next-method)
      (index-mail-message mail-archive (first plist)))))

;; Abstract cliche and cache  4/23/97 -- JCMa.
(defmethod mail-archive-tag ((mail-archive indexing-mail-archive)&optional recompute-p)
  (with-value-cached (mail-archive :mail-archive-tag :recompute-p recompute-p)
    (symbolize (name-string (ma-url mail-archive)) *keyword-package*)))

(defmethod initialize-mail-archive :before ((mail-archive indexing-mail-archive) file-stream)
  (declare (ignore file-stream))
  (lambda-ir::set-default-tags (mail-archive-tag mail-archive))
  (or (lambda-ir::default-document-universe)
      (lambda-ir::set-default-document-universe (lambda-ir::set-up-universe))))

(defun update-threading-information-in-index (mail-archive &optional (start 0) end)
  (with-slots (messages) mail-archive
    (handling-mail-archive-indexing-errors (mail-archive :operation "UPDATE-THREADING-INFORMATION-IN-INDEX")
      (let ((universe (lambda-ir::default-document-universe))
	    (use-end (or end (length messages))))
	(loop for n upfrom start below use-end 
	      for msg = (elt messages n)
	      for thread = (msg-thread msg)
	      for number = (msg-index msg)
	      when thread
		do (lambda-ir::modify-document-tags
		     universe number
		     ;; LIR should return a consistent data structure so that less consing can be done  4/23/97 -- JCMa.
		     `(,@(lambda-ir::listify (lambda-ir::get-document-tags universe number)) ,(thread-index thread))))))))

(defmethod initialize-mail-archive :after ((mail-archive indexing-mail-archive) file-stream)
  (declare (ignore file-stream))
  (update-threading-information-in-index mail-archive))

(defmethod incremental-update-mail-archive :around ((mail-archive indexing-mail-archive) old-size old-length new-length)
  (declare (ignore old-size old-length new-length))
  (with-slots (messages) mail-archive
    (lambda-ir::set-default-tags (mail-archive-tag mail-archive))
    (let ((number-of-messages (length messages)))
      (multiple-value-prog1
        (call-next-method)
        (update-threading-information-in-index mail-archive number-of-messages)))))


;;;------------------------------------------------------------------- 
;;;
;;; FULL TEXT SEARCH FORM
;;;

;; don't duplicate instructions   4/23/97 -- JCMa.
(defparameter *archive-search-instructions*
              "Enter a search query consisting of relevant terms.  Terms with a (+) before them
(+cl-http) must be included in the target document.  Terms with a (-) before
them must not be included in the returned documents.")

;; tidy up   4/23/97 -- JCMa.
(defun %write-archive-search-form (url stream description &optional default-substring)
  (flet ((write-heading (stream)
	   (with-rendition (:bold :stream stream)
	     (fast-format stream "Search HyperArchive"))))
    (with-standard-document-body (:stream stream)
      (with-section-heading (#'write-heading :alignment :left :stream stream)
	(with-paragraph (:stream stream)
	  (with-centering (:stream stream)
	    (ns4.0:with-table (:border nil :width .85 :background :grey-light-very :cell-spacing 0 :cell-padding 6 :stream stream)
	      (with-table-row (:stream stream)
		(with-table-cell (:stream stream)
		  (with-font (:size 2 :stream stream)
		    (format stream description)))))))
	(with-fillout-form (:post url :stream stream)
	  (accept-input 'hidden "OPERATION" :default "SEARCH" :stream stream)
	  (with-paragraph (:stream stream)
	    (with-rendition (:bold :stream stream)
	      (fast-format stream "Find Documents Satisfying the Query"))
	    (break-line :stream stream)
	    (accept-input 'string "SUBSTRING" :default default-substring :size 50 :stream stream))
	  (with-paragraph (:stream stream)
	    (with-rendition (:italic :stream stream)
	      (fast-format stream "Return to search"))))))))

;; provide introspective operation to add the facility to the view bar.
(defmethod ma-write-summary ((mail-archive indexing-mail-archive) stream (view (eql :search)))
  (with-slots (url) mail-archive
    (%write-archive-search-form url stream *archive-search-instructions*)))

(defgeneric archive-perform-search (mail-archive stream substring)
  (:documentation "Performs search."))

;; fix bugs and clean up   4/23/97 -- JCMa.
(defmethod archive-perform-search (mail-archive stream substring)
  (horizontal-line :stream stream)
  (html:with-section-heading ("Search Results" :stream stream)
    (multiple-value-bind (msg-indexes number-of-returns)
	;; this should return a count and accept an ordering argument (forward
	;; or backward) and possibly a sort predicate 4/23/97 -- JCMa.
	(lambda-ir::perform-search-constraint lambda-ir::*document-context* lambda-ir::archive-search-constraint substring (mail-archive-tag mail-archive))
      number-of-returns
      (cond (msg-indexes
	     (with-paragraph (:stream stream)
	       (format stream "~D document~:P satisfy the search specification: "
		       (length msg-indexes))
	       (fast-format stream "[~I]."
			    (with-rendition (:italic :stream stream)
			      (write-string substring stream))))
	     (with-emphasis (:quotation :stream stream)
	       (with-enumeration (stream :definition)
		 (with-rendition (:bold :stream stream)
		   (loop for message-name in (nreverse msg-indexes)
			 for msg = (ma-get-message mail-archive message-name)
			 do (with-paragraph (:stream stream)
			      (if msg
				  (msg-write-summary msg stream :date-author-subject)
				  ;; this can lose for some unknown reason.  4/23/97 -- JCMa.
				  (enumerating-item (stream)
				    (with-rendition (:italic :stream stream)
				      (fast-format stream "The message ~A is unavailable." message-name))))))))))
	    (t (with-paragraph (:stream stream)
		 (format stream "Lamentably, no documents satisfy ~s." substring)))))))

;; Include with-successful-response for proper operation  4/23/97 -- JCMa.
(defmethod ma-execute-form-operation ((operation (eql :search)) url stream query-alist)
  (bind-query-values (substring)
		     (url query-alist)
    (let* ((mail-archive (mail-archive url))
	   (ma-url (ma-url mail-archive))
	   (title (or (get-value ma-url :title) (name-string ma-url))))
      (with-successful-response (stream :html :last-modification (ma-cache-time mail-archive)
					:content-language (languages url)
					:cache-control '(:no-cache t))
	(flet ((write-title (stream)
		 (with-centering (:stream stream)
		   (write-string title stream)
		   (break-line :stream stream))))
	  (declare (dynamic-extent #'write-title))
	  (lambda-ir::set-default-tags (mail-archive-tag mail-archive))
	  (with-mail-archive-document-body (mail-archive stream)
	    (with-section-heading (#'write-title :stream stream)
	      (ma-write-description mail-archive stream)
	      (msg-write-message mail-archive :archive-info stream)
	      (msg-write-message nil :horizontal-line stream)
	      (%write-archive-search-form url stream *archive-search-instructions* substring)
	      (archive-perform-search mail-archive stream substring)
	      (horizontal-line :stream stream)
	      (ma-write-signature mail-archive stream))))))))
