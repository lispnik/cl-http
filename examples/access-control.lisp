;;;   -*- Mode: LISP; Package: http; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-

;;;
;;; (c) Copyright  1995-96, John C. Mallery
;;;     All Rights Reserved.
;;;


;;;------------------------------------------------------------------- 
;;;
;;; DISPLAYING & EDITING AUTHENTICATION INFORMATION
;;;

(in-package :http)

(eval-when (compile eval load)
  (unless (fboundp 'image-line)

;; Spuce up computed lines by using this instead of  html:horizontal-line 
    (defparameter *image-line-url* "/cl-http/icons/line-rain.gif")

    (defun image-line (&key (stream html:*output-stream*) (fresh-line t))
      (when fresh-line
        (fresh-line stream))
      (html:with-paragraph (:stream stream)
        (html:image *image-line-url* "----" :stream stream)))

    (mapc #'(lambda (x) (export x :http)) '(image-line *image-line-url*))))

(define-parameter *describe-realm-url* "/cl-http/describe-realm?")

(define-parameter *describe-group-url* "/cl-http/describe-group?")

(define-parameter *describe-access-control-url* "/cl-http/describe-access-control?")

(define-parameter *describe-user-url* "/cl-http/describe-user?")

(define-parameter *edit-user-url* "/cl-http/edit-user?")

(define-parameter *edit-user-form-url* "/cl-http/edit-user.html")

(defmethod describe-realm-url ((realm realm))
  (let ((url-string (concatenate 'string *describe-realm-url* (realm-name realm))))
    #u url-string))

(defmethod describe-group-url ((group group))
  (let ((url-string (concatenate 'string *describe-group-url* (qualified-name group))))
    #u url-string))

(defmethod describe-access-control-url ((access-control access-control))
  (let ((url-string (concatenate 'string *describe-access-control-url* (qualified-name access-control))))
    url-string))

(defmethod describe-user-url ((user user))
  (let ((url-string (concatenate 'string *describe-user-url* (qualified-name user))))
    url-string))

(defmethod edit-user-url ((user user))
  (let ((url-string (concatenate 'string *edit-user-url* (qualified-name user))))
    url-string))

(defmacro with-user-description-url ((&key (user '(server-user-object *server*)) (function-var 'url-fctn)) &body body)
  `(let ((,function-var (if (allow-user-access-p #u*edit-user-url* ,user :get) #'edit-user-url #'describe-user-url)))
     ,@body))


;;;------------------------------------------------------------------- 
;;;
;;; URL
;;;

(defmethod display-url-authentication-status ((url url:authentication-mixin) stream)
  (with-successful-response (stream :html :expires (url:expiration-universal-time url)
                                    :cache-control (url:response-cache-control-directives url)
                                    :content-language (languages url))
    (let ((title "Access Control Status"))
      (html:with-html-document (:declare-dtd-version-p t :stream stream)
        (html:with-document-preamble (:stream stream)
          (html:declare-base-reference url :stream stream)
          (html:declare-title title :stream stream))
        (html:with-document-body (:stream stream)
          (html:with-section-heading (title :stream stream)
            (image-line :stream stream)
            (let ((realm (url:authentication-realm url))
                  (capabilities (url:capabilities url))
                  (authorization (get-raw-header :authorization))
                  (user (current-user-object)))
              (html:with-section-heading ("Current Resource" :stream stream)
                (html:with-rendition (:bold :stream stream)
                  (html:note-anchor (url:name-string url) :reference url :stream stream))
                (cond (realm
                       (html:with-paragraph (:stream stream)
                         (write-string "This resource is part of the " stream)
                         (html:with-rendition (:bold :stream stream)
                           (write-string (realm-name realm) stream))
                         (write-string " authentication realm." stream))
                       (html:with-paragraph (:stream stream)
                         (cond (capabilities
                                (write-string "Access capabilities are granted for: " stream)
                                (display-access-control capabilities stream))
                               (t (write-string "Access is granted to any user in the above realm." stream)))))
                      (t (html:with-paragraph (:stream stream)
                           (write-string "This resource is not assigned to an authentication realm." stream))))
                (cond
                  (authorization
                   (html:with-paragraph (:stream stream)
                     (write-string "The client's request for the document included the authorization header:" stream))
                   (html:with-paragraph (:stream stream)
                     (html:with-rendition (:bold :stream stream)
                       (write-string "Authorization " stream))
                     (html:with-enumeration (stream :plain)
                       (dolist (item authorization)
                         (html:enumerating-item (stream)
                           (html:write-string-quoting-specials item stream))))))
                  (t (html:with-paragraph (:stream stream)
                       (write-string "There was no authorization header field included with the client request for this document."
                                     stream)))))
              (when user 
                (html:with-section-heading ("Current User" :stream stream)
                  (describe-user user stream))))
            (image-line :stream stream)
            (cl-http-signature stream)))))))

(export 'display-url-authentication-status :http)


;;;------------------------------------------------------------------- 
;;;
;;; GROUPS
;;;

(defmethod display-group-users ((group group) &optional (stream html:*output-stream*))
  (html:with-section-heading ("Users" :stream stream)
    (html:with-enumeration (stream :enumerate)
      (with-user-description-url (:function-var url-fctn)
        (loop for user in (sort (group-users group) #'string< :key #'user-name)
              for user-name = (user-name user)
              for url = (funcall url-fctn user)
              do (html:enumerating-item (stream)
                   (html:note-anchor user-name :reference url :stream stream)))))))

(defmethod describe-group ((group group) &optional (stream html:*output-stream*))
  (let ((realm (group-realm group)))
    (html:with-section-heading ("Description" :stream stream)
      (html:with-enumeration (stream :itemize)
        (html:enumerating-item (stream)
          (html:with-rendition (:italic :stream stream)
            (write-string "Realm: " stream))
          (html:note-anchor (realm-name realm) :reference (describe-realm-url realm) :stream stream))
        (html:enumerating-item (stream)
          (html:with-rendition (:italic :stream stream)
            (write-string "Authentication: " stream))
          (write (realm-scheme realm) :stream stream :escape nil))))
    (display-group-users group stream)))


;;;------------------------------------------------------------------- 
;;;
;;; ACCESS CONTROLS
;;;

(defmethod display-access-control ((access-control access-control) &optional (stream html:*output-stream*))
  "Displays groups and users able to access the URL access group, ACCESS-CONTROL, on STREAM."
  (let ((alist (access-control-alist access-control))
        (default-groups (access-control-default-groups access-control))
        (default-users (access-control-default-users access-control)))
    (with-user-description-url (:function-var url-fctn)
      (macrolet ((write-cognoscienti ((set) &body form)
                   `(if ,set
                        (loop for (item . more) = ,set then more
                              while item
                              do ,@form
                                 (when more (write-string ", " stream))
                              finally (write-char #\. stream))
                        (write-string "none." stream))))
        (flet ((enumerate-method (method groups users)
                 (html:enumerating-item (stream)
                   (html:with-rendition (:bold :stream stream)
                     (write method :escape nil :stream stream))
                   (html:with-enumeration (stream :itemize)
                     (html:enumerating-item (stream)
                       (html:with-rendition (:bold :stream stream)
                         (write-string "Groups: " stream))
                       (write-cognoscienti (groups)
                                           (html:note-anchor (group-name item) :reference (describe-group-url item)
                                                             :stream stream)))
                     (html:enumerating-item (stream)
                       (html:with-rendition (:bold :stream stream)
                         (write-string "Users: " stream))
                       (write-cognoscienti (users)
                                           (html:note-anchor (user-name item) :reference (funcall url-fctn item) :stream stream)))))))
          (declare (dynamic-extent #'enumerate-method))
          (html:with-enumeration (stream :plain)
            (loop for (method groups users) in alist
                  do (enumerate-method method groups users))
            (when (or default-groups default-users)
              (enumerate-method :default default-groups default-users))))))))

(defmethod describe-access-control ((access-control access-control) &optional (stream html:*output-stream*))
  (let ((realm (access-control-realm access-control)))
    (html:with-section-heading ("Description" :stream stream)
      (html:with-enumeration (stream :itemize)
        (html:enumerating-item (stream)
          (html:with-rendition (:italic :stream stream)
            (write-string "Realm: " stream))
          (html:note-anchor (realm-name realm) :reference (describe-realm-url realm) :stream stream))
        (html:enumerating-item (stream)
          (html:with-rendition (:italic :stream stream)
            (write-string "Authentication: " stream))
          (write (realm-scheme realm) :stream stream :escape nil))))
    (html:with-section-heading ("Methods" :stream stream)
      (display-access-control access-control stream))))


;;;------------------------------------------------------------------- 
;;;
;;; USERS
;;;

(defmethod describe-user ((user user) &optional (stream html:*output-stream*))
  "Describes USER's capabilities in HTML on STREAM."
  (let ((realm (user-realm user))
        (groups (user-groups user))
        (email-address (user-email-address user))
        (personal-name (user-personal-name user)))
    (flet ((write-entry (heading string stream)
             (html:with-rendition (:bold :stream stream)
               (write-string heading stream)
               (write-char #\: stream)
               (write-char #\space stream))
             (write-string string stream)
             (html:break-line :stream stream)))
      (html:with-paragraph (:stream stream)
        (write-entry "Username" (user-name user) stream)
        (cond-every
          (personal-name
            (write-entry "Personal Name" personal-name stream))
          (email-address
            (let ((mailto (html:note-anchor email-address :reference (concatenate 'string "mailto:" email-address)
                                            :stream nil)))
              (declare (dynamic-extent mailto))
              (write-entry "Email Address" mailto stream))))
        (write-entry "Realm"
                     (html:note-anchor (realm-name realm) :reference (describe-realm-url realm) :stream nil)
                     stream)
        (html:with-rendition (:bold :stream stream)
          (write-string "Groups: " stream))
        (cond (groups
               (loop for (group . more) = groups then more
                     do (html:note-anchor (group-name group) :reference (describe-group-url group) :stream stream)
                        (if more
                            (write-string ", " stream)
                            (write-char #\. stream))
                     while more))
              (t (write-string "none.")))))))


;;;------------------------------------------------------------------- 
;;;
;;; REALMS
;;;

(defmethod display-realm-users ((realm realm) &optional (stream html:*output-stream*))
  (macrolet ((write-user-property (property value stream)
               `(when ,value
                  (html:break-line :stream ,stream)
                  (html:with-rendition (:italic :stream ,stream)
                    (write-string ,property ,stream))
                  (write-string ,value ,stream))))
    (let ((users (sorted-users realm :predicate #'string<)))
      (declare (dynamic-extent users))
      (html:with-section-heading ("Users" :stream stream)
        (html:with-enumeration (stream :enumerate)
          (with-user-description-url (:function-var url-fctn)
            (loop for user in users
                  for user-name = (user-name user)
                  for url = (funcall url-fctn user)
                  for name = (user-personal-name user)
                  for email = (user-email-address user)
                  do (html:enumerating-item (stream)
                       (html:with-rendition (:bold :stream stream)
                         (html:note-anchor user-name :reference url :stream stream))
                       (write-user-property "Name: " name stream)
                       (let ((mailto (html:note-anchor email :reference (concatenate 'string "mailto:" email)  :stream nil)))
                         (declare (dynamic-extent mailto))
                         (write-user-property "Email: " mailto stream))))))))))

(defmethod display-realm-groups ((realm realm) &optional (stream html:*output-stream*))
  (let ((groups (sorted-groups realm :predicate #'string<)))
    (declare (dynamic-extent groups))
    (html:with-section-heading ("Groups" :stream stream)
      (html:with-enumeration (stream :enumerate)
        (with-user-description-url (:function-var url-fctn)
          (loop for group in groups
                for group-name = (group-name group)
                for group-url = (describe-group-url group)
                do (html:enumerating-item (stream)
                     (html:with-rendition (:bold :stream stream)
                       (html:note-anchor group-name :reference group-url :stream stream))
                     (html:break-line :stream stream)
                     (html:with-rendition (:italic :stream stream)
                       (write-string "Users: " stream))                   
                     (loop for users = (sort (group-users group) #'string< :key #'user-name) then (cdr users)
                           while users
                           as (user) = users
                           as name = (user-name user)
                           as url = (funcall url-fctn user)
                           do (html:note-anchor name :reference url :stream stream)
                              (if (cdr users)
                                  (write-string ", " stream)
                                  (write-char #\. stream))))))))))

(defmethod display-realm-access-controls ((realm realm) &optional (stream html:*output-stream*))
  (let ((access-controls (sorted-access-controls realm :predicate #'string<)))
    (declare (dynamic-extent access-controls))
    (html:with-section-heading ("Access Controls" :stream stream)
      (html:with-enumeration (stream :enumerate)
        (loop for access-control in access-controls
              for access-control-name = (access-control-name access-control)
              for access-control-name-url = (describe-access-control-url access-control)
              do (html:enumerating-item (stream)
                   (html:with-rendition (:bold :stream stream)
                     (html:note-anchor access-control-name 
                                       :reference access-control-name-url
                                       :stream stream))
                   (display-access-control access-control stream)))))))

(defmethod describe-realm ((realm realm) &optional (stream html:*output-stream*))
  (html:with-section-heading ("Description" :stream stream)
    (html:with-enumeration (stream :itemize)
      (html:enumerating-item (stream)
        (html:with-rendition (:italic :stream stream)
          (write-string "Authentication: " stream))
        (write (realm-scheme realm) :stream stream :escape nil))))
  (display-realm-groups realm stream)
  (display-realm-access-controls realm stream)
  (display-realm-users realm stream))

;;;------------------------------------------------------------------- 
;;;
;;; RESPONSE FUNCTIONS
;;;

(defmethod respond-to-describe-realm ((url url:http-search) stream)
  (with-slots (url:search-keys) url
    (destructuring-bind (&optional realm-string) url:search-keys
      (with-successful-response (stream :html :cache-control (url:response-cache-control-directives url)
                                        :content-language (languages url))
        (html:with-html-document (:declare-dtd-version-p t :stream stream)
          (cond
            ((or (null realm-string) (null-string-p realm-string))
             (let ((title (format nil "~A (~D) Access Control Realms" (local-host-domain-name) *standard-http-port*)))
               (html:with-document-preamble (:stream stream)
                 (html:declare-base-reference url :stream stream)
                 (html:declare-title title :stream stream))
               (html:with-document-body (:stream stream)
                 (html:with-section-heading (title :stream stream)
                   (image-line :stream stream)
                   (dolist (realm (sorted-realms :predicate #'string<))
                     (let ((heading (concatenate 'string "Realm: "
                                                 (html:note-anchor (realm-name realm)
                                                                   :reference (describe-realm-url realm) :stream nil))))
                       (declare (dynamic-extent heading))
                       (html:with-section-heading (heading :stream stream)
                         (describe-realm realm stream))
                       (image-line :stream stream)))))))
            (t (let* ((realm-name (string-trim " " realm-string))
                      (realm (intern-realm realm-name :if-does-not-exist :soft))
                      (title (format nil "Realm: ~A "
                                     (if realm
                                         (html:note-anchor
                                           realm-name :reference (describe-realm-url realm) :stream nil)
                                         realm-name))))
                 (declare (dynamic-extent realm-name title))
                 (html:with-document-preamble (:stream stream)
                   (html:declare-base-reference url :stream stream)
                   (html:declare-title title :stream stream))
                 (html:with-document-body (:stream stream)
                   (html:with-section-heading (title :stream stream)
                     (image-line :stream stream)
                     (describe-realm realm stream))
                   (image-line :stream stream)))))
          (cl-http-signature stream))))))

(defmacro %respond-to-describe-acl-unit (url stream var-name type-string intern-function describe-function)
  `(with-slots (url:search-keys) ,url
     (destructuring-bind (&optional ,var-name &rest extra) url:search-keys
       (with-successful-response (,stream :html :cache-control (url:response-cache-control-directives url)
                                  :content-language (languages url))
         (let ((title (format nil ,(format nil "~:(~A~): ~~:(~~A~~)" (substitute #\space #\- type-string :test 'eql)) ,var-name)))
           (html:with-html-document (:declare-dtd-version-p t :stream ,stream)
             (html:with-document-preamble (:stream ,stream)
               (html:declare-base-reference url :stream ,stream)
               (html:declare-title title :stream ,stream))
             (html:with-document-body (:stream ,stream)
               (html:with-section-heading (title :stream ,stream)
                 (image-line :stream ,stream)
                 (cond
                   ((null ,var-name)
                    (write-string
                      ,(format nil "No ~A and realm was specified. You need to pass in realm|~:*~A-name." type-string)
                      ,stream))
                   (extra
                    (format nil "The extraneous arguments, ~S, were provided. You need to pass in realm|~Aname." extra ,type-string))
                   (t (let* ((len (1+ (position-if-not #'white-space-char-p ,var-name :start 0 :end (length ,var-name)
                                                       :from-end t)))
                             (pos1 (position-if-not #'white-space-char-p ,var-name :start 0 :end len))
                             (pos2 (or (position #\| ,var-name :start pos1 :end len) len))
                             (realm-name (subseq ,var-name pos1 pos2))
                             (unit-name (subseq ,var-name (1+ pos2) len))
                             (realm (intern-realm realm-name :if-does-not-exist :soft))
                             (unit nil))
                        (declare (dynamic-extent realm unit-name))
                        (cond ((and realm (setq unit (,intern-function realm unit-name :if-does-not-exist :soft)))
                               (,describe-function unit ,stream))
                              (realm
                               (format ,stream "No ~A named, ~A, exists in the realm, ~A." ',type-string unit-name realm-name))
                              (t (format ,stream "No realm named, ~A, exists on the server." realm-name))))))
                 (image-line :stream ,stream)
                 (cl-http-signature ,stream)))))))))

(defmethod respond-to-describe-group ((url url:http-search) stream)
  (%respond-to-describe-acl-unit url stream realm+group "group" intern-group describe-group))

(defmethod respond-to-describe-access-control ((url url:http-search) stream)
  (%respond-to-describe-acl-unit url stream realm+access-control "access-control" intern-access-control describe-access-control))

(defmethod respond-to-describe-user ((url url:http-search) stream)
  (%respond-to-describe-acl-unit url stream realm+user "user" intern-user describe-user))


;;;------------------------------------------------------------------- 
;;;
;;; EDITING USERS
;;;

(defun %write-edit-user-form (url stream &optional user-id password email-address groups personal-name realm)
  (macrolet ((write-prompt (prompt stream)
               `(html:with-table-cell
                  (:horizontal-alignment :right :stream stream)
                  (with-rendition (:bold :stream ,stream)
                    (write-string ,prompt ,stream))))
             (accept-simple-field (input-type prompt query default stream)
               `(html:with-table-row (:stream ,stream)
                  (write-prompt ,prompt ,stream)
                  (html:with-table-cell
                    (:horizontal-alignment :center :vertical-alignment :middle :stream ,stream)
                    (accept-input ,input-type ,query :default ,default :stream ,stream
                                  :size (max 25 (length ,default))))))
             (accept-selection (stream prompt query choices &optional default)
               `(html:with-table-row (:stream ,stream)
                  (write-prompt ,prompt ,stream)
                  (html:with-table-cell (:horizontal-alignment :center :vertical-alignment :middle :stream ,stream)
                    (let ((n-choices (length ,choices)))
                      (if (zerop n-choices)
                          (with-rendition (:italic :stream stream) (write-string "None" ,stream))
                          (accept-input 'select-choices ,query :choices ,choices :default ,default
                                        :sequence-p t :size (min 5 n-choices) :stream ,stream)))))))
    (html:with-paragraph (:stream stream)
      (html:with-fillout-form (:post url :stream stream)
        ;; hidden fields so that we know where we started
        (cond-every
          (realm
            (accept-input 'hidden "ORIGINAL-REALM" :default (write-to-armor-plated-string (realm-name realm)) :stream stream))
          (user-id
            (accept-input 'hidden "ORIGINAL-USER-ID" :default (write-to-armor-plated-string user-id) :stream stream)))
        (html:with-table (:stream stream)
          (accept-simple-field 'string "User-ID (required): " "USER-NAME" user-id stream)
          (accept-simple-field 'string "Personal Name: " "PERSONAL-NAME" personal-name stream)
          (accept-simple-field 'string "Email Address: " "EMAIL-ADDRESS" email-address stream)
          (if realm
              (html:with-table-row (:stream stream)
                (write-prompt "Realm (required): " stream)
                (html:with-table-cell (:horizontal-alignment :center :vertical-alignment :middle :stream stream)
                  (note-anchor (realm-name realm) :reference (describe-realm-url realm) :stream stream)))
              (accept-selection stream "Realm: " "REALM" (mapcar #'realm-name (sorted-realms))))
          (let ((group-names (mapcar #'group-name groups)))
            (declare (dynamic-extent group-names))
            (accept-selection stream "Groups: " "GROUPS" group-names group-names))
          (accept-simple-field 'string "New Group: " "NEW-GROUP" nil stream)
          (accept-simple-field 'password "New Password: " "PASSWORD1" nil stream)
          (accept-simple-field 'password "Confirm Password: " "PASSWORD2" nil stream)
          (cond-every
            (password
              (html:with-table-row (:stream stream)
                (write-prompt "Remove Password: " stream)
                (html:with-table-cell (:horizontal-alignment :center :vertical-alignment :middle :stream stream)
                  (accept-input 'radio-button "REMOVE-PASSWORD" :choices '(("Yes" . "YES") ("No" . "NO"))
                                :default "NO" :linebreaks nil :stream stream))))
            (user-id
              (html:with-table-row (:stream stream)
                (write-prompt "Delete User: " stream)
                (html:with-table-cell (:horizontal-alignment :center :vertical-alignment :middle :stream stream)
                  (accept-input 'radio-button "DELETE-USER" :choices '(("Yes" . "YES") ("No" . "NO"))
                                :default "NO" :linebreaks nil :stream stream)))))
          (html:with-table-row (:stream stream)
            (write-prompt "Action: " stream)
            (html:with-table-cell (:horizontal-alignment :center :stream stream)
              (with-verbatim-text (:fresh-line nil :width 30 :stream stream)
                (html:accept-input 'html:reset-button "Reset" :stream stream)
                (write-string "     " stream)
                (html:accept-input 'html:submit-button "Submit" :stream stream)))))))))

(defmethod write-edit-user-form ((user user) url &optional (stream html:*output-stream*))
  (let ((personal-name (user-personal-name user))
        (email-address (user-email-address user))
        (realm (user-realm user))
        (groups (user-groups user))
        (username (user-name user))
        (password-digest (user-password-digest user)))
    (%write-edit-user-form url stream username (not (null password-digest))
                           email-address groups personal-name realm)))

(defmethod respond-to-edit-user (url stream)
  (with-slots (url:search-keys) url
    (let* ((user (parse-authentication-object (first url:search-keys) #'intern-user))
           (title (if user
                      (format nil "Edit User: ~A" (user-name user))
                      (format nil "Unknown User: ~A" (first url:search-keys)))))
      (with-successful-response (stream :html :cache-control (url:response-cache-control-directives url)
                                        :content-language (languages url))
        (html:with-html-document (:declare-dtd-version-p t :stream stream)
          (html:with-document-preamble (:stream stream)
            (html:declare-base-reference url :stream stream)
            (html:declare-title title :stream stream))
          (html:with-document-body (:stream stream)
            (html:with-section-heading (title :stream stream)
              (image-line :stream stream)
              (cond (user
                     (write-edit-user-form user *edit-user-form-url* stream))
                    (t (format stream "There is no user named, ~A, on this server.~
                                       You need to pass in realm-name|user-name."
                               (first url:search-keys)))))
            (image-line :stream stream)
            (cl-http-signature stream)))))))

(defmethod compute-edit-user-form ((url url:http-form) stream)
  (with-successful-response (stream :html :expires (url:expiration-universal-time url)
                                    :cache-control (url:response-cache-control-directives url)
                                    :content-language (languages url))
    (html:with-html-document (:declare-dtd-version-p t :stream stream)
      (html:with-document-preamble (:stream stream)
        (html:declare-base-reference url :stream stream)
        (html:declare-title "Create User" :stream stream))
      (html:with-document-body (:stream stream)
        (html:with-section-heading ("Create User" :stream stream)
          (image-line :stream stream)
          (%write-edit-user-form *edit-user-form-url* stream)
          (image-line :stream stream)
          (cl-http-signature stream))))))

(defmethod respond-to-edit-user-form ((url url:http-form) stream query-alist)
  (flet ((clean-up (item)
           (and item (not (null-string-p (setq item (string-trim '(#\space #\tab #\return #\Linefeed) item)))) item))) 
    (macrolet ((clean-up-vars (&rest vars)
                 `(progn ,.(loop for var in vars collect `(setq ,var (clean-up ,var)))))
               (report-result (stream format-string &rest format-args)
                 `(enumerating-item (stream)
                    ,(if format-args
                         `(format ,stream ,format-string ,@format-args)
                         `(write-string ,format-string ,stream)))))
      (bind-query-values (original-realm original-user-id user-name personal-name email-address
                                         groups new-group password1 password2 realm delete-user remove-password)
                         (url query-alist)
        (clean-up-vars user-name personal-name email-address new-group password1 password2 realm delete-user remove-password)
        (let* ((realm (intern-realm (if original-realm (read-from-armor-plated-string original-realm) realm)
                                    :if-does-not-exist :soft))
	       (user-id (or user-name (and original-user-id (read-from-armor-plated-string original-user-id))))
               (user (and realm user-id (intern-user realm user-id :if-does-not-exist :soft)))
               (title (format nil "Edit User: ~A" (if user (user-name user) user-id))))
          (with-successful-response (stream :html :cache-control (url:response-cache-control-directives url)
                                            :content-language (languages url))
            (html:with-html-document (:declare-dtd-version-p t :stream stream)
              (html:with-document-preamble (:stream stream)
                (html:declare-title title :stream stream)
                (html:declare-base-reference url :stream stream))
              (html:with-document-body (:stream stream)
                (html:with-section-heading (title :stream stream)
                  (image-line :stream stream)
                  (html:with-section-heading ("Results" :stream stream)
                    (html:with-paragraph (:stream stream)
                      (html:with-enumeration (stream :itemize)
                        (cond ((null realm)
                               (report-result stream "No Realm supplied."))
                              (user
                               (cond ((equalp delete-user "YES")        ;delete user object
                                      (unintern-user realm user)
                                      (report-result stream "User deleted.")
                                      (setq user nil))
                                     (t (cond-every
                                          ((not (equalp user-id (user-name user)))    ;rename user
                                           (let ((pw (user-password-digest user)))
                                             (unintern-user realm user)
                                             (setq user (intern-user realm user-id :if-does-not-exist :create))
                                             (setf (user-password-digest user) pw))
                                           (report-result stream "User renamed."))
                                          ((equalp remove-password "YES")       ;remove password
                                           (setf (user-password-digest user) nil)
                                           (report-result stream "Password Removed."))))))
                              (t (setq user (intern-user realm user-id :if-does-not-exist :create))
                                 (report-result stream "New user created.")))
                        (when user
                          (let ((i-groups (loop for g in (ensure-list groups)
                                                collect (intern-group realm g)))
                                (n-group (intern-group realm new-group :if-does-not-exist :soft))
                                (pw nil))
                            (cond-every
                              (password1 ;;change the user's password
                                (cond ((and password2 (string= password1 password2))
                                       (report-result stream "Password updated.")
                                       (setq pw password1))
                                      (t (report-result stream "Password not updated; the two passwords do not match."))))
                              (new-group
                                (if n-group
                                    (push n-group i-groups)
                                    (report-result stream "Attempt to add user to a non-existant group, ~A." new-group))))
                            (update-user user
                                         :personal-name personal-name
                                         :email-address email-address
                                         :password pw
                                         :groups i-groups)
                            (save-authentication-data)
                            (report-result stream "Authentication data saved."))))))
                  (when user
                    (image-line :stream stream)
                    (html:with-section-heading ((user-name user) :stream stream)
                      (html:with-paragraph (:stream stream)
                        (write-edit-user-form user *edit-user-form-url* stream))))
                  (image-line :stream stream)
                  (cl-http-signature stream))))))))))


;;;------------------------------------------------------------------- 
;;;
;;; EXPORTS
;;;

;; All the realm description URLs are restricted to webmasters at your site
;; unless you relax the restriction yourself.  See the configuration file for
;; giving Webmaster a password so that you can view these URLs.  The user
;; description URL has been left open because it is used in the documentation.
(export-url #u*describe-realm-url*
            :search
            :response-function #'respond-to-describe-realm
            :keywords '(:cl-http :maintenance)
            :documentation "Describe an access control realm."
            :authentication-realm :server
            :capabilities :webmasters
            :private t
            :proxy-revalidate t
            :language :en)

(export-url #u*describe-group-url*
            :search
            :response-function #'respond-to-describe-group
            :private t
            :proxy-revalidate t
            :language :en
            :keywords '(:cl-http :maintenance)
            :documentation "Describe a particular access control group in a realm.
The syntax is realm-name|group-name."
            :authentication-realm :server
            :capabilities :webmasters)

(export-url #u*describe-access-control-url*
            :search
            :response-function #'respond-to-describe-access-control
            :private t
            :proxy-revalidate t
            :language :en
            :keywords '(:cl-http :maintenance)
            :documentation "Describe a particular access control unit in a realm.
The syntax is realm-name|access-control-name."
            :authentication-realm :server
            :capabilities :webmasters)

(export-url #u*describe-user-url*
            :search
            :response-function #'respond-to-describe-user
            :private t
            :proxy-revalidate t
            :language :en
            :keywords '(:cl-http :maintenance)
            :documentation "Describe the access capabilities for a particular user in a realm.
The syntax is realm-name|user-name."
            #|:authentication-realm :server
            :capabilities :webmasters|#)

(export-url #u*edit-user-url*
            :search
            :response-function #'respond-to-edit-user
            :keywords '(:cl-http :maintenance)
            :documentation "Edit the access capabilities for a particular user in a realm.
The syntax is realm-name|user-name."
            :authentication-realm :server :capabilities :webmasters)

(export-url #u*edit-user-form-url*
            :html-computed-form
            :form-function #'compute-edit-user-form
            :response-function #'respond-to-edit-user-form
            :keywords '(:cl-http :maintenance)
            :documentation "A form to edit user access control."
            :authentication-realm :server
            :capabilities :webmasters
            :private t
            :proxy-revalidate t
            :language :en)
