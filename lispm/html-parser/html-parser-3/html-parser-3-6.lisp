;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: html-parser; Base: 10; Patch-File: t -*-
;;; Patch file for HTML-PARSER version 3.6
;;; Reason: Function (CLOS:METHOD HTML-PARSER:PARSE-HTML (STRING)):  remove (declare
;;; (special tags)) and add tags arg to (add-pcdata-as-needed input-data tags).
;;; Function HTML-PARSER::OPEN-TAG:  remove tags special declaration and add tags arg.
;;; Function HTML-PARSER::ADD-PCDATA-AS-NEEDED:  add tags argument.
;;; Function HTML-PARSER::PUSH-TAG:  add tags.
;;; Written by JCMa, 10/18/96 16:53:19
;;; while running on Lispm Machine Thomas Jefferson from FEP0:>ComLink-38-11-HTTP-60-D-MIT-8-3.ilod.1
;;; with Genera 8.3, Logical Pathnames Translation Files NEWEST, NFS Server 435.0,
;;; Metering 439.0, Metering Substrate 439.0, Conversion Tools 430.0, Hacks 435.0,
;;; CLIM 66.5, Genera CLIM 66.0, PostScript CLIM 66.2, CLIM Documentation 66.0,
;;; 8-3-Patches 1.31, MAC 412.7, Statice Runtime 460.4, Statice 460.1,
;;; Statice Browser 460.0, Statice Documentation 423.0, DBFS Utilities 439.0,
;;; Showable Procedures 36.3, Binary Tree 34.0, Mailer 434.0,
;;; Experimental Working LispM Mailer 6.0, HTTP Server 60.49,
;;; W3 Presentation System 2.2, CL-HTTP Server Interface 48.1,
;;; Symbolics Common Lisp Compatibility 3.0, Experimental Comlink Packages 4.2,
;;; Experimental Comlink Utilities 9.14, Experimental Routing Taxonomy 8.1,
;;; Experimental COMLINK Database 10.20, Experimental Email Servers 11.8,
;;; Experimental Comlink Customized LispM Mailer 6.7,
;;; Experimental Dynamic Forms 11.10,
;;; Experimental Communications Linker Server 38.30, Jcma 41,
;;; HTTP 1.0 Base Client 35.3, Image Substrate 435.0,
;;; Essential Image Substrate 427.0, W4 Examples 5.0,
;;; W4 Constraint-Guide Web Walker 32.2, HTML Parser 3.5, Ivory Revision 4A, FEP 328,
;;; FEP0:>I328-loaders.flod(24), FEP0:>I328-info.flod(24), FEP0:>I328-debug.flod(24),
;;; FEP0:>I328-lisp.flod(25), FEP0:>I328-kernel.fep(44), Boot ROM version 320,
;;; Device PROM version 325, Genera application 5.6.1a1,
;;; MacIvory SCSI Manager Server 4.3.2a1, Toolbox Servers 4.2,
;;; MacIvory & RPC library 6.3.3a1, MacIvory life support 4.3.8a1,
;;; Symbolics keyboard 2.1, Macintosh System Software 7.5.5,
;;; 1152x820 Screen with Genera fonts, Machine serial number 30376, Macintosh,
;;; Symbolics Keyboard,
;;; Add support for Apple's Gestalt and Speech Managers. (from SYS:MAC;MACIVORY-SPEECH-SUPPORT.LISP.5),
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.22),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.8),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Deny some hosts access to some servers. (from CML:LISPM;HOST-SERVICE-ACCESS-CONTROL.LISP.4),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.102),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.47),
;;; Ephemeral gc reclamation patch (from SYS:8-3-PATCHES;PATCH;8-3-PATCHES-1-31.LISP.3),
;;; Prevent reset of input buffer on tcp reset by HTTP servers. (from HTTP:LISPM;W4;RECEIVE-TCP-SEGMENT-PATCH.LISP.6).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTML-PARSER:HTML-PARSER;HTML-PARSER.LISP.9"
  "HTML-PARSER:HTML-PARSER;PACKAGES.LISP.3")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTML-PARSER:HTML-PARSER;HTML-PARSER.LISP.9")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Package: html-parser; Base: 10; Mode: lisp -*-")

(defun push-tag (tag-instance tags &optional store-p call-handler-p)
  (let ((container (find-tag-container tag-instance)))
    (cond (container
           (close-tag-before-container container)
           (and store-p (store-tag-instance tag-instance call-handler-p))
           (push tag-instance *tag-stack*))
          ((or (default-container tag-instance) (containers tag-instance))
           (open-tag (make-instance
		      'html-tag-instance
		      :instance-of (tag-definition
				    (or (default-container tag-instance)
					(car (containers tag-instance)))))
		     tags)
           (and store-p (store-tag-instance tag-instance call-handler-p))
           (push tag-instance *tag-stack*))
          ;; we have the root tag making an appearance
          (t (assert (null *tag-stack*))
             (and store-p (store-tag-instance tag-instance call-handler-p))
             (push tag-instance *tag-stack*)))))
;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTML-PARSER:HTML-PARSER;HTML-PARSER.LISP.9")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Package: html-parser; Base: 10; Mode: lisp -*-")

(defun add-pcdata-as-needed (pcdata tags)
  ;; If the PCDATA is only a space, then it should not close a tag that
  ;; cannot contain PCDATA. If the last stacked tag can contain PCDATA,
  ;; then it should be included as usual. The only space string I am
  ;; interested in is " ", because any other space would be impossible in
  ;; a non-pre context. And all those containers that cannot contain PCDATA
  ;; are not allowed in a pre context anyway.
  (declare (special open-contexts))
  (let* ((pcdata-token (tokenize-name "PCDATA"))
         (container (find-tag-container pcdata-token)))
    (when (or (notevery #'html-whitespace-p pcdata)
              (and container (eql container (car *tag-stack*))))
      (cond (container
             (close-tag-before-container container))
            (t (open-tag (make-instance 'html-tag-instance
					:instance-of (tag-definition
						      *pcdata-default-container*))
			 tags)))
      (cond ((open-context-p :pcdata pcdata)
             (push-pcdata pcdata)
             (call-handler pcdata))
            ((and (> open-contexts 0)
                  (or (eql tags t) (member pcdata-token tags)))
             (push-pcdata pcdata))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTML-PARSER:HTML-PARSER;HTML-PARSER.LISP.9")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Package: html-parser; Base: 10; Mode: lisp -*-")

(defmethod parse-html ((input string) &optional (tags t))
  (and (listp tags) (tokenize-list tags #'tokenize-name))
  (initialize-stack)
  (initialize-tags)
  (do ((*html-string* input)
       (*current-pos* 0)
       (*input-length* (length input))
       (open-contexts 0)
       (input-type nil)
       (input-data nil))
      ((eql input-type *eof*)
       (close-all-tags)
       #+debug (car (instances (tag-definition *html-root*))))
    (declare (special open-contexts))
    (multiple-value-setq (input-type input-data)
      (next-html-token))
    (case input-type
      ((:comment :declaration)
       ;; Do nothing
       )
      (:open-tag
       (open-tag input-data tags))
      (:close-tag
       (close-tag input-data))
      ((:character :pcdata :entity)
       (add-pcdata-as-needed input-data tags))
      (t (assert (eql input-type *eof*))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTML-PARSER:HTML-PARSER;HTML-PARSER.LISP.9")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Package: html-parser; Base: 10; Mode: lisp -*-")

(defun open-tag (tag-instance tags)
  (declare (special open-contexts))
  ;; Ensure the root context exists before any other tag is put on
  (when (and (null *tag-stack*)
             (not (eql (name tag-instance) *html-root*)))
    (open-tag (make-instance 'html-tag-instance
                             :instance-of (tag-definition *html-root*)) tags))
  ;; Call the appropriate method for pushing a new tag on the *tag-stack*
  (cond ((eql (name tag-instance) *html-root*)
         ;; The root tag is priveledged
         (and *tag-stack* (close-all-tags))
         (cond ((open-context-p :open-tag tag-instance)
                (incf open-contexts)
                (push-tag tag-instance tags t t))
               (t (push-tag tag-instance tags))))
        ((open-context-p :open-tag tag-instance)
         (incf open-contexts)
         (push-tag tag-instance tags t t))
        ((and (> open-contexts 0)
              (or (eql tags t)
                  (member (name tag-instance) tags)
                  (and (eql (class-name (class-of tag-instance))
                            'unknown-tag-instance)
                       (member (tokenize-name "UNKNOWN") tags))))
         (push-tag tag-instance tags t))
        (t (push-tag tag-instance tags))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTML-PARSER:HTML-PARSER;PACKAGES.LISP.3")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-Common-Lisp; Package: cl-user; Base: 10; Mode: lisp -*-")

(mapc #'(lambda (x) (export (intern x :html-parser) :html-parser))
      '("PARTS" "PART-OF"))
