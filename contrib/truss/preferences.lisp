John,

I have been busy writing a bit of code for creating persistent
preferences in CL-HTTP.  I have just assembled that code into a
single file and added some comments.  I am including it below.

Enjoy.

 -Tom.
________________________________________________________________________
Thomas A. Russ,  Senior Research Scientist                   tar@isi.edu    
USC/Information Sciences Institute              WWW:  http://www.isi.edu
4676 Admiralty Way, Marina del Rey, CA 90292          (310) 822-1511x775

=========  File follows this line ========
;;;-*- Mode: Lisp; Package: (HTTP-USER (:use HTTP CL)) -*-

(in-package "HTTP-USER")

;;;;
;;;;    (C) Copyright 1997 by University of Southern California (USC)
;;;;
;;;;	All copyrighted material contained in this file may be copied and
;;;;	redistributed, provided that the copies are not made or distributed
;;;;	for resale, the USC copyright notice and reference to the source
;;;;	of the file appear, and that notice is given that copying is by
;;;;	permission of Univeristy of Southern California.
;;;;
;;;; ________________________________________________________________________
;;;; Thomas A. Russ,  Senior Research Scientist                   tar@isi.edu    
;;;; USC/Information Sciences Institute              WWW:  http://www.isi.edu
;;;; 4676 Admiralty Way, Marina del Rey, CA 90292          (310) 822-1511x775
;;;;
;;;; 
;;;;  PERSISTENT SERVER-SIDE PREFERENCE MANAGEMENT CODE
;;;;         for CL-HTTP
;;;;
;;;;   This package provides a means for maintaining and managing
;;;;   persistent preference information for users.  It requires that
;;;;   users be logged in, which implies some form of access control
;;;;   for the URLs that the CL-HTTP server is using.
;;;;
;;;;   There are two classes of user accounts:  Those that are shared and
;;;;   those that are not shared.  Preferences for accounts that are not
;;;;   shared are maintained on a per-user basis and affect the user 
;;;;   regardless of from where they connect.  Shared accounts affect only
;;;;   connections from a particular host.  This allows the use of 
;;;;   preferences with "guest" accounts as long as the guests don't all
;;;;   log in from the same machine.  [OK, it's a hack, but it's the best
;;;;   I could come up with.]
;;;;
;;;;   Preferences are defined using two macros:
;;;;      DEFPREFERENCE-GROUP
;;;;      DEFPREFERENCE
;;;;
;;;;   Preferences are bound using the macro
;;;;      BIND-PREFERENCES
;;;;
;;;;   Inside the scope of this macro, the current user's preferences
;;;;   can be accesssed by using
;;;;        PREFERENCE-VALUE
;;;;
;;;;   There is a short example at the bottom of the file.
;;;;
;;;;   Preferences can be editted by using the URL "edit-preferences"
;;;;     (see export-url below)
;;;;
;;;;   Preferences can be managed by using
;;;;      READ-PREFERENCES
;;;;      WRITE-PREFERENCES
;;;;
;;;;  The following variable may need to be customized:
;;;;     *SAVE-PREFERENCE-DIRECTORY*


;;;
;;;  PREFERENCE RELATED VARIABLES
;;;

(defvar *save-preference-directory* "CL-HTTP:preferences;"
  "Pathname to a directory where individual user preference files are stored.")

(defvar *shared-user-accounts* '("tourist" "guest")
  "List of user names whose information should be maintained on a per-host basis")

(defvar *current-preference* nil "Bound by BIND-PREFERENCE macro")

(defvar *user-preference-table* (make-hash-table))
   ;; ((<group0> <item00> <item01> ...)
   ;;  (<group1> <item10> <item11> ...))

(defparameter *preference-types* nil)


(defun reset-prefernces ()
  (setq *preference-types* nil))

;;;
;;;  PREFERENCE DEFINITION AND HANDLING MACROS
;;;

(defun add-preference-group (name)
  ;; Add a group entry for "name", preserving original order.
  (unless (assoc name *preference-types*)
    (setq *preference-types* (nconc *preference-types* (list (list name))))))

(defmacro preference-pretty-name (x) `(second ,x))
(defmacro preference-type (x) `(third ,x))
(defmacro preference-subtype (x) `(sixth ,x))  ;; Type of elements;
(defmacro preference-value-list (x) `(let ((v (fourth ,x)))
				       (if (functionp v) (funcall v) v)))
(defmacro preference-default-value (x) `(fifth ,x))

(defun add-preference (name pretty-name type group values predicate default)
  (declare (ignore predicate))
  (let ((found (assoc group *preference-types*))
	item subtype)
    (unless found
      (warn "Reference to undefined group.  Creating one.")
      (setf (get group :pretty-preference-group-name) (symbol-name group))
      (add-preference-group group)
      (setq found (assoc group *preference-types*))
      (assert found))
    ;; Found is now guaranteed.
    (setq item (assoc name (rest found)))
    (when (and (member type '(:single-choice :multiple-choice))
	       (consp values)
	       (not (every #'consp values)))
      (setq values
	    (loop for v in values
		  unless (consp v)
		  collect (cons (html:fast-format nil "~A" v) v)
		  else collect v)))
    (when (and (eq type :multiple-choice)
	       default
	       (atom default))
      (setq default (list default)))
    ;; Determine subtype;
    (let ((testValues (if (functionp values) (funcall values) values)))
      (case type
	((:multiple-choice :single-choice)
	 (cond ((every #'(lambda (x) (stringp (cdr x))) testValues)
		(setq subtype :string))
	       ((some #'(lambda (x) (stringp (cdr x))) testValues)
		(warn "Strings mixed in with other types in preference ~A.  All return values will be strings!" name)
		(setq subtype :string))
	       ((every #'(lambda (x) (or (numberp (cdr x)) (keywordp (cdr x))))
		       testValues)
		(setq subtype :keyword))
	       (t (setq subtype :atom))))
	(t (if (keywordp testValues)
	       (setq subtype :keyword)
	       (setq subtype type)))) )
    (if item
	(setf (rest item)
	      `(,pretty-name ,type ,values ,default ,subtype))
	(nconc found `((,name ,pretty-name ,type ,values ,default ,subtype)))) ))


(defmacro defpreference-group (name pretty-name &key short-name tab-image selected-tab-image)
  "Defines a preference group with the symbol NAME.  PRETTY-NAME is a string
with a display name for use in the editting form.  SHORT-NAME is used for a
quick index to other parts of the preferences.  TAB-IMAGE and 
SELECTED-TAB-IMAGE are paths suitable for the HTML:IMAGE routine that can
be used for a graphical rather than a text-based image."

  (let ((namevar (gensym "NAME"))
	(pnamevar (gensym "PNAME")))
  `(let ((,namevar ,name)
	 (,pnamevar ,pretty-name))
     (progn (setf (get ,namevar :preference-group-pretty-name) ,pnamevar)
	    (setf (get ,namevar :preference-group-short-name) (or ,short-name (string ,namevar)))
	    (setf (get ,namevar :preference-group-tab-image) ,tab-image)
	    (setf (get ,namevar :preference-group-selected-tab-image) ,selected-tab-image)
	    (add-preference-group ,namevar)))))

(defmacro defpreference (name pretty-name type group
			      &key values predicate default)
  "Defines a particalur preference with the symbol NAME.  PRETTY-NAME is a 
string with a display name for use in the editting form.  TYPE is one of
   :multiple-choice  -- A number of options can be selected.
   :single-choice    -- A single choice from a list
   :integer          -- An integer
   :number           -- A number
   :boolean          -- A toggle option
   :string           -- A string value
GROUP is the preference group to which this particular preference belongs.
VALUES is used to list the values that can be chosen for the single and
multiple choice options.  The format is the same as that used for the single
and multiple choice HTML:ACCEPT-INPUT form.
PREDICATE is currently not used.
DEFAULT is the default value for this preference."

  `(add-preference ',name ,pretty-name ,type ',group ,values ,predicate ,default))

(defmacro bind-preferences (&rest body)
  "Macro to establish a preference binding for the current user.  Note that
the current user must be known for this to have any effect."
  `(let ((*current-preference* (lookup-user-preference)))
     ,@body) )



;;;;;;
;;;;;;  PREFERENCE FORMS & OTHER INFORMATION
;;;;;;


(defun lookup-user-preference ()
  (let ((user (current-user-object)))
    (when user
      (get-user-preference user))))

(defun find-user-preference (realm userName)
  (multiple-value-bind (user error)
      (ignore-errors (intern-user realm userName))
    (if user
	(get-user-preference user)
        (format *error-output* "~%ERROR: ~A" error))))

(defun get-user-preference (user)
  (let* ((userName (user-name user))
	 (preference (gethash user  *user-preference-table*)))
    (cond ((null preference) nil)
	  ((member userName *shared-user-accounts* :test #'string=)
	   (let* ((*log-resolve-ip-addresses* t)
		  (hostName (http1::host-log-name *server*)))
	     (cdr (assoc hostName preference))))
	  (t preference)) ))

(defun lookup-default-preference (preference)
  (loop for (nil . values) in *preference-types*
	as match = (assoc preference values)
	when match return (preference-default-value match)) )

(defun preference-value (preference)
  "Given the name PREFERENCE, the value of that preference item is
returned."
  (or (cdr (assoc preference *current-preference*))
      (lookup-default-preference preference)))

(defun preference-value-p (preference value)
  "Returns NIL if the current value of PREFERENCE is not equal to VALUE."
  (equal value (preference-value preference)))

(defun lookup-preference-type (preference)
  (loop for (nil . values) in *preference-types*
	as match = (assoc preference values)
	when match return (preference-type match)))

(defun lookup-preference-subtype (preference)
  (loop for (nil . values) in *preference-types*
	as match = (assoc preference values)
	when match return (preference-subtype match)))

(defun insure-proper-preference-type (preference newValueList)
  (case (lookup-preference-type preference)
    (:string (cond ((consp newValueList)
		    (loop for v in newValueList
			  when (stringp v)
			  collect v
			  else collect (html:fast-format nil "~S" v)))
		   ((stringp newValueList) newValueList)
		   ((null newValueList) "")
		   (t (html:fast-format nil "~S" newValueList))))
    (:integer (cond ((consp newValueList)
		     (loop for v in newValueList
			   when (integerp v)
			   collect v
			   else when (numberp v)
			   collect (floor v)
			   else collect (lookup-default-preference preference)))
		    ((integerp newValueList) newValueList)
		    ((numberp newValueList) (floor newValueList))
		    (t (lookup-default-preference preference))))
    (:number (cond ((consp newValueList)
		    (loop for v in newValueList
			  when (numberp v)
			  collect v
			  else collect (lookup-default-preference preference)))
		    ((numberp newValueList) newValueList)
		    (t (lookup-default-preference preference))))
    (otherwise newValueList)) )

(defun set-preference-value (user preference newValueList)
  (let ((userName (and user (user-name user))))
    (setq newValueList (insure-proper-preference-type preference newValueList))
    (if (null *current-preference*)
	(setf *current-preference* `((,preference ,@newValueList)))
        (let ((currentPreference (assoc preference *current-preference*)))
	  (if (null currentPreference)
	      (setq *current-preference*
		    (nconc *current-preference*
			   `((,preference ,@newValueList))))
	      (setf (cdr currentPreference) newValueList)) ))

    ;; Set Global Preference:
    (if (member userName *shared-user-accounts* :test #'string=)
	(let* ((*log-resolve-ip-addresses* t)
	       (hostName (http1::host-log-name *server*)))
	  (setf (gethash user *user-preference-table*)
		(cons `(,hostName ,@*current-preference*)
		      (gethash user *user-preference-table*))))
      (setf (gethash user *user-preference-table*)
	    *current-preference*)) ))

(defun write-preference-file (userName userObject)
  "Write the preference file for user with name USERNAME and object USEROBJECT"
  (let ((filename (html:fast-format nil "~A~A.lisp"
				    *save-preference-directory*
				    userName)))

    (with-open-file (f filename :direction :output :if-exists :supersede)
      (html:fast-format f 
	   ";;; CL-HTTP Preference file for user ~A~%;;;  Saved "
	   userName)
      (http::write-standard-time (get-universal-time) f)
      (fresh-line f)
      (fresh-line f)
      (pprint (gethash userObject *user-preference-table*) f)
      (terpri f))))

(defun write-preference-files (realm)
  "Write preference files for all currently known users in REALM."
  (url:map-users realm #'write-preference-file))

(defun read-preference-file (userName userObject)
  "Read the preference file for user with name USERNAME and object USEROBJECT"
  (let ((filename (html:fast-format nil "~A~A.lisp"
				    *save-preference-directory*
				    userName)))
    (when (probe-file filename)
      (setf (gethash userObject *user-preference-table*)
	    (with-open-file (f filename :direction :input)
			    (read f nil nil)))) ))

(defun read-preference-files (realm)
  "Write preference files for all currently known users in REALM."
  (url:map-users realm #'read-preference-file) )


(defun write-preference-item (item stream)
  (ecase (preference-type item)
    ((:string :integer :number)
     (html:with-rendition (:bold :stream stream)
       (write-string (preference-pretty-name item) stream)
       (write-string ":&nbsp;" stream))
     (html:accept-input 'string (symbol-name (first item))
       :default (and (preference-value (first item))
		     (html2:fast-format nil "~S" 
			     (preference-value (first item))))
       :stream stream :size (if (eql (preference-type item) :string)
				20 8)))
    (:single-choice
     (html:with-rendition (:bold :stream stream)
       (write-string (preference-pretty-name item) stream))
     (html2:accept-input 'radio-button (symbol-name (first item))
	    :choices (preference-value-list item)
	    :linebreaks t
	    :default (preference-value (first item))
	    :stream stream))
    (:multiple-choice
     (html:with-rendition (:bold :stream stream)
       (write-string (preference-pretty-name item) stream))
     (html2:accept-input 'checkbox (symbol-name (first item))
	    :choices (preference-value-list item)
	    :linebreaks t
	    :default (preference-value (first item))
	    :stream stream))
    (:boolean
      (html2:accept-input 'checkbox (symbol-name (first item))
	    :choices `((,(preference-pretty-name item) . t))
	    :linebreaks nil
	    :default (list (preference-value (first item)))
	    :stream stream))) )

(defun make-anchor-name (string)
  (setq string (copy-seq string))
  (nsubstitute #\Space #\_ string)
  (nsubstitute #\- #\_ string)
  string)

(defun emit-preference-heading (group type stream)
  ;; Emits preference heading for "group".  "Type" should be :text or :image.
  ;; If type is :image, then image files will be used if they exist.
  ;; Otherwise a textual name and a section heading will be used.
  (html:note-anchor "" :stream stream
		    :tag (make-anchor-name (or (get group :preference-group-short-name) 
						(symbol-name group))))
  (flet ((emit-text (groupReference currentGroup stream)
	   (let ((groupName (or (get groupReference :preference-group-short-name) 
				  (symbol-name groupReference))))
	     (when (eq groupReference currentGroup)
	       (write-char #\* stream))
	     (html:note-anchor groupName 
			       :local-reference (make-anchor-name groupName)
			       :stream stream)
	     (when (eq groupReference currentGroup)
	       (write-char #\* stream)) )))
    (flet ((emit-image (groupReference currentGroup stream)
	     (let ((groupImage (if (eq groupReference currentGroup)
				   (get groupReference :preference-group-selected-tab-image)
				   (get groupReference :preference-group-tab-image)))
		   (groupName (or (get groupReference :preference-group-short-name) 
				  (symbol-name groupReference))))
	       (if groupImage
		   (ns2.0:with-anchor-noted
		       (:stream stream 
		        :local-reference (make-anchor-name groupName))
		       (if (consp groupImage)
			   (ns2.0:image (first groupImage) groupName
					:stream stream
					:width (second groupImage) :height (third groupImage)
					:border 0 :alignment :bottom)
			    (ns2.0:image (first groupImage) groupName
					:stream stream
					:border 0 :alignment :bottom)))
		   (emit-text groupReference currentGroup stream)) )))
      (unless (eql type :image)
	(with-section-heading ((or (get group :preference-group-pretty-name) 
				   (get group :preference-group-short-name) 
				   (symbol-name group))
			       :stream stream))
	(write-string "Go to: " stream))
      (loop for (g . v) in *preference-types*
	    when v
	    do (if (eql type :image)
		   (emit-image g group stream)
		   (emit-text  g group stream)))
      (html:accept-input 'submit 
	 (html2:fast-format nil "SUBMIT-~A" group)
	 :display-string "Submit"
	 :stream stream)
      (html:break-line :stream stream) )))

(defun serve-preference-editting-form (url stream)
  (with-successful-response (stream :html
			      :expires (url:expiration-universal-time url))
    (html:with-html-document (:stream stream)
      (html:with-document-preamble (:stream stream)
        (html:declare-base-reference url :stream stream)
        (html:declare-title "Edit Preferences" :stream stream))
      (html:with-document-body (:stream stream)
       (html2:with-fillout-form (:post url :stream stream :name "EditForm")
        (ns1.1:horizontal-line :stream stream :size 2)
        (bind-preferences
          (loop for (group . values) in *preference-types*
		when values
		do (emit-preference-heading group :image stream)
		   (with-emphasis (:quotation :stream stream)
		     (loop for item in values
			   do (with-paragraph (:stream stream)
			       (write-preference-item item stream))))
		   (ns1.1:horizontal-line :stream stream :size 2)) )
	(submit-and-reset-buttons stream))))) )

(defun process-preference-editting-form (url stream query-alist)
  (let ((thisUser (current-user-object))
	(*package* *keyword-package*)
	(userPackage (find-package "CL-USER")))
    (flet ((safe-read-from-string (string)
	     (ignore-errors
	       (and string (read-from-string string nil nil)))))
    (flet ((submit-button-key-p (key)
	     (let ((keyName (symbol-name key)))
	       (and (> (length keyname) 6)
		    (string= "SUBMIT" keyName :end2 6))))
	   (interpret-form-value (key value multip)
	    (if (or multip (listp value))
		(case (lookup-preference-subtype key)
		  (:string value)
		  (:keyword
		   (loop for v in value
			 collect (safe-read-from-string v)))
		  (otherwise
		   (let ((*package* userPackage))
		     (loop for v in value
			   collect (safe-read-from-string v)))))
	        (case (lookup-preference-type key)
		  (:multiple-choice
		   (list (safe-read-from-string value)))
		  (:string value)
		  (otherwise
		   (if (eql (lookup-preference-subtype key) :keyword)
		       (safe-read-from-string value)
		     (let ((*package* userPackage))
		       (safe-read-from-string value))))))) )
      (with-successful-response (stream :html
				 :expires (url:expiration-universal-time url))
        (html:with-html-document (:stream stream)
	  (html:with-document-preamble (:stream stream)
            (html:declare-base-reference url :stream stream)
            (html:declare-title "Edit Preferences" :stream stream))
	  (html:with-document-body (:stream stream)
	    (if thisUser
		(bind-preferences
		 (loop for (key value multip) in query-alist
		       unless (submit-button-key-p key)
		       do (set-preference-value thisuser key 
			   (interpret-form-value key value multip)))
		 (write-preference-file (user-name thisUser) thisUser)
		 (write-string "Preferences updated." stream))
	      (write-string 
	       "No logged in user found.  Preferences could not be updated."
	       stream))))) ))))



(defun show-user-preferences (realm &optional (stream *standard-output*) 
			      &key compactP)
  "This function prints out the user preferences.  Useful for debugging."
  (url:map-users realm 
    #'(lambda (userName userObject)
	(let ((prefs (gethash userObject *user-preference-table*)))
	  (when prefs
	    (html:fast-format stream "~%User ~A:" userName)
	    (if compactP
		(pprint prefs stream)
	        (loop for item in prefs
		      do (pprint item stream)))
	    (terpri stream)) ))) )


;;;;;;
;;;;;;  PREFERENCE EXAMPLES
;;;;;;

(export-url #u"/edit-preferences"
            :html-computed-form
            :form-function #'serve-preference-editting-form
            :response-function #'process-preference-editting-form
	    :authentication-realm :browser
	    :capabilities '((:default :browse :edit))
            :expiration `(:interval 0)
            :keywords '(:preference :edit)
            :documentation "A form interface for editting user preferences.")



;;;;;;
;;;;;;  PREFERENCE EXAMPLES
;;;;;;

#|

;;;
;;;  Preference Groups are used to organize preferences into coherent
;;;   units for automatically generating the editting forms.  Each
;;;   group has its own heading.  Within a group, preferences are displayed
;;;   in the order that they are defined.
;;;
;;;   DEFPREFERENCE GroupSymbol  GroupPrettyName
;;;       &key GroupShortName
;;;

(defpreference-group :global "Global Preferences" :short-name "Global")
(defpreference-group :browse "Browsing Preferences" :short-name "Browse")
(defpreference-group :edit "Editting Preferences" :short-name "Edit")
(defpreference-group :save "File Preferences" :short-name "File")


  ;; Global Preference

(defpreference :display-image "Display Images" :boolean :global :default t)

 ;; Browsing preferences

(defpreference :definition-location "Location of Definition"
  :multiple-choice :browse
  :values '(("Top of Form" . :top) ("Bottom of Form" . :bottom))
  :default :top)
(defpreference :child-depth "Normally Displayed Depth of Child Tree"
  :integer :browse
  :default 3)

 ;; Editting Preferences

(defpreference :modification-tracking "Log Which Modifications"
  :single-choice :edit
  :values '(("None" . :none) ("All" . :all) ("Last only" . :last))
  :default :none)


;;; These preferences can then be used in dynamic pages to control
;;;   the generation of output.  For example:

(bind-preferences
  (when (preference-value :display-image)
    (html:image ....))
  ...
  )

;;;
;;; More advanced groups that use images for display rather than text
;;;  strings.  This works best if the images are available in both
;;;  a selected form and a non-selected form.
;;;
;;;  For testing, the icons can be obtained via anonymous ftp from
;;;   isi.edu in the directory pub/loom/icons/
;;;
;;;

(defpreference-group :general "Global Preferences" :short-name "Global"
  :tab-image          '("icons/global-tab.gif"     62 22)
  :selected-tab-image '("icons/global-tab-sel.gif" 62 22))
(defpreference-group :browse "Browsing Preferences" :short-name "Browse"
  :tab-image          '("icons/browse-tab.gif"     62 22)
  :selected-tab-image '("icons/browse-tab-sel.gif" 62 22))
(defpreference-group :edit "Editting Preferences" :short-name "Edit"
  :tab-image          '("icons/edit-tab.gif"     62 22)
  :selected-tab-image '("icons/edit-tab-sel.gif" 62 22))
(defpreference-group :save "File Preferences" :short-name "File"
  :tab-image          '("icons/file-tab.gif"     62 22)
  :selected-tab-image '("icons/file-tab-sel.gif" 62 22))

|#
