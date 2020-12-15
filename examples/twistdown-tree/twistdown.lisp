;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: HTML3.2; Base: 10 -*-
;;;
;;; (C) Copyright 1997-1998 by Christopher Vincent (java applet), Rodney S. Daughtrey
;;;     (lisp code below), and John C. Mallery (CL-HTTP substrate).
;;;     All Rights Reserved.
;;;
;;;---------------------------------------------------------------------------------


;; This file implements a high-level Lisp interface to a java applet (written by 
;; Christopher Vincent) for producing interactive twist-down trees.  It's also
;; a good example of how to write high-level Lisp interfaces to Java applets (that
;; are driven by applet parameter tags) using CL-HTTP.


(in-package :html3.2)

(export '(*default-twistdown-tree-applet-horizontal-space*
	  compute-twistdown-tree-applet-vertical-space
	  generate-twistdown-tree))


;;;------------------------------------------------------------------- 
;;;
;;; LISP INTERFACE TO APPLET
;;;

(defun default-node-name-printer (node)
  "Default function used by GENERATE-TWISTDOWN-TREE that, given a node, 
returns the string to print to represent that node on the client side."
  (with-output-to-string (stream)
    (print-object node stream)))

(defun default-node-url-producer (node)
  "Default function used by GENERATE-TWISTDOWN-TREE that, given a node, 
returns the URL to associate with that node."
  (declare (ignore node))
  http:*cl-http-home-page-url-string*)

(defun default-node-frame-producer (node)
  "Default function used by GENERATE-TWISTDOWN-TREE that, given a node, 
returns the frame to associate with that node."
  (declare (ignore node))
  ; leave target blank for applet default
  "")

(defun default-initial-display-p (node)
  "Default function used by GENERATE-TWISTDOWN-TREE that, given a node, 
decides whether or not to initially display the node's children."
  (declare (ignore node))
  nil)

(defun default-node-font-color-producer (node)
  "Default function used by GENERATE-TWISTDOWN-TREE that, given a node, 
decides what color index to make its label."
  (declare (ignore node))
  1)

(defun allocate-node-id ()
  "Allocates a unique ID for a node."
  (declare (special *node-id-counter*))
  (let ((id (format nil "n~D" *node-id-counter*)))
    (incf *node-id-counter*)
    id))

(defun intern-node-id (node node-id children-node-ids)
  "Adds a node to a hash table of nodes, keyed on the node's ID (*not* the Lisp object), and 
records the Lisp object associated with the ID and the node's childrens' IDs."
  (declare (special *node-->node-info-table*))
  (setf (gethash node-id *node-->node-info-table*)
	(cons node children-node-ids)))

(defun node-id-children-ids (node-id)
  "Given a node ID, returns its childrens' IDs."
  (declare (special *node-->node-info-table*))
  (rest (gethash node-id *node-->node-info-table*)))

(defun node-id-node (node-id)
  "Given a node ID, returns its associated Lisp object."
  (declare (special *node-->node-info-table*))
  (first (gethash node-id *node-->node-info-table*)))

(defgeneric walk-tree-recording-information (node child-producer-function)
  (:documentation "Walks the tree (using the root node and child producer 
function supplied by the user) and stores the nodes of the tree in a hash table for
later reference."))

(defmethod walk-tree-recording-information (node child-producer-function)
  (let* ((children-node-ids
	   (loop for child-node in (funcall child-producer-function node)
		 collect (walk-tree-recording-information child-node child-producer-function)))
	 (node-id
	   (allocate-node-id)))
    (intern-node-id node node-id children-node-ids)
    node-id))

(defun note-top-level-java-tree-parameter (stream node-id-list)
  (note-java-parameter "top" #'(lambda (stream)
				 (format stream "~{~A~^,~}" node-id-list))
		       stream))

(defgeneric note-java-tree-parameter (stream node-id children-node-ids pretty-name-function
					     url-producer-function frame-producer-function show-children-p-function
					     font-color-producer-function)
  (:documentation "Outputs a java applet parameter given the information passed in.  The format for tree parameters for the applet
is as follows:
    <param name=\"N<n>\" value=\"<string-to-draw>,<font color index>,<url>,<frame>,<show-children-p>,<children-1-name>,...,<children-n-name>\">"))

(defmethod note-java-tree-parameter (stream node-id children-node-ids pretty-name-function url-producer-function
					    frame-producer-function show-children-p-function font-color-producer-function)
  (let ((node (node-id-node node-id)))
    (note-java-parameter
      node-id
      #'(lambda (stream)
	  (format stream "\"~A,~A,~A,~A,~A,~{~A~^,~}\""
		  (funcall pretty-name-function node)
		  (funcall font-color-producer-function node)
		  (funcall url-producer-function node)
		  (funcall frame-producer-function node)
		  (if (funcall show-children-p-function node) "yes" "no")
		  children-node-ids))
      stream)))

(defgeneric generate-java-parameters (stream node-id pretty-name-function url-producer-function 
					     frame-producer-function show-children-p-function
					     font-color-producer-function)
  (:documentation "Function which walks the tree (using the information in the hash table, 
not the child producer function) and outputs the tree parameters for the applet."))

(defmethod generate-java-parameters (stream node-id pretty-name-function url-producer-function frame-producer-function 
					    show-children-p-function font-color-producer-function)
  (let ((children-node-ids (node-id-children-ids node-id)))
    (note-java-tree-parameter stream node-id children-node-ids
			      pretty-name-function url-producer-function frame-producer-function show-children-p-function
			      font-color-producer-function)
    (loop for child-node-id in children-node-ids
	  do (generate-java-parameters stream child-node-id pretty-name-function url-producer-function
				       frame-producer-function show-children-p-function
				       font-color-producer-function))))

(defparameter *default-twistdown-tree-applet-horizontal-space* 300.
  "The default width of the applet.  Users can change this if desired.")

(defgeneric compute-twistdown-tree-applet-vertical-space (root-node &key font-size line-spacing)
  (:documentation "Function to return the vertical space to be used by the applet.  The default 
method considers font-size and line-spacing."))

(defmethod compute-twistdown-tree-applet-vertical-space (root-node &key font-size line-spacing)
  (declare (ignore root-node font-size) (special *node-->node-info-table*))
  (let ((number-of-nodes (hash-table-count *node-->node-info-table*)))
    (* (1+ number-of-nodes) line-spacing)))

(declaim (inline intern-color))

(defun intern-color (color)
  (etypecase color
    ((or keyword cons)
     (ns1.1:color-mapping color))
    (string color)))

(defun note-twistdown-tree-palette-java-parameter (palette stream)
  (when palette
    (note-java-parameter
      "palette"
      (loop with palette-string = ""
	    with length = (length palette)
	    for counter from 1
            for palette-entry in palette
	    do (setq palette-string (concatenate 'string palette-string (intern-color palette-entry)))
	       (unless (= counter length)
		 (setq palette-string (concatenate 'string palette-string ",")))
	    finally (return palette-string))
      stream)))

(defun %generate-twistdown-tree (stream root-object-list child-producer
					&optional (object-printer #'default-node-name-printer)
					(url-producer #'default-node-url-producer)
					(frame-producer #'default-node-frame-producer)
					(initial-display-predicate #'default-initial-display-p)
					(font-color-producer #'default-node-font-color-producer)
					(node-indentation 15)
					(line-spacing 15)
					(font-size 10)
					(palette nil)
					(background-color-index 0)
					(marker-color-index 1)
					(context-url nil)
					(debug-p nil)
					(applet-alignment :top)
					applet-name
					applet-alternate-text
					applet-horizontal-space
					applet-vertical-space)
  (let ((*node-id-counter* 0)
	(*node-->node-info-table* (make-hash-table :test #'equal))
	root-node-id-list)
    (declare (special *node-id-counter* *node-->node-info-table*))
    ;; First, walk the tree(s) and record each java parameter name, node, and its children parameter names
    ;; in a hash table, and save the node id's of the root nodes
    (setq root-node-id-list
	  (loop for root-object in root-object-list
		collect (walk-tree-recording-information root-object child-producer)))
    (with-java-applet ("hdir.class"
		       (or applet-horizontal-space *default-twistdown-tree-applet-horizontal-space*)
		       (or applet-vertical-space
			   (compute-twistdown-tree-applet-vertical-space
			     (first root-object-list) :font-size font-size :line-spacing line-spacing))
		       applet-alignment
		       :stream stream
		       :name applet-name
		       :alternate-text applet-alternate-text)
      ;; ...then walk the tree again (using the hash table) and generate the java parameters
      (dolist (root-node-id root-node-id-list)
	(generate-java-parameters stream root-node-id
				  object-printer url-producer frame-producer
				  initial-display-predicate font-color-producer))
      ;; Finally, generate the special top-level java parameter
      (note-top-level-java-tree-parameter stream root-node-id-list)
      (note-java-parameter "bg_color" background-color-index stream)
      (note-java-parameter "marker_color" marker-color-index stream)
      (note-java-parameter "x_space" node-indentation stream)
      (note-java-parameter "y_space" line-spacing stream)
      (note-java-parameter "font_size" font-size stream)
      (note-java-parameter "debug" (if debug-p "yes" "no") stream)
      (when palette 
	(note-twistdown-tree-palette-java-parameter palette stream))
      (when context-url
	(note-java-parameter "context_url" context-url stream)))))

(defun generate-twistdown-tree (stream root-object-list child-producer
				       &key (object-printer #'default-node-name-printer)
				       (url-producer #'default-node-url-producer)
				       (frame-producer #'default-node-frame-producer)
				       (initial-display-predicate #'default-initial-display-p)
				       (font-color-producer #'default-node-font-color-producer)
				       (node-indentation 15)
				       (line-spacing 15)
				       (font-size 10)
				       (palette nil)
				       (background-color-index 0)
				       (marker-color-index 1)
				       (context-url nil)
				       (debug-p nil)
				       (applet-alignment :top)
				       applet-name
				       applet-alternate-text
				       applet-horizontal-space applet-vertical-space)
  "The main function to call to produce a twist-down Java UI on the client-side based on 
your Lisp object hierarchy on the server side.  Some specific notes:

-- The child-producer function is only called once for each node
-- The code is safe across threads
-- The same Lisp object can appear more than once in the hierarchy, i.e. for any hierarchy your 
   child-producer function can produce, the Lisp code will generate the Java parameters to 
   display it correctly, even if the hierarchy you want to display isn't strictly a tree.

Arguments to the function are as follows:

Required args:
  STREAM: 
       The stream to which to output the java applet.
  ROOT-OBJECT-LIST: 
       A list of root (Lisp) objects that you want to display.
  CHILD-PRODUCER: 
       A function which returns a list of (Lisp) objects which are the children of the parent object passed in.

Keywords:
  OBJECT-PRINTER:
       Function to call on the nodes in your Lisp object hierarchy to produce
       the string to display for each node.
  URL-PRODUCER: 
       Function to call on the nodes in your Lisp object hierarchy to produce
       the URL to associate with that node.
  FRAME-PRODUCER:
       Function to call on the nodes in your Lisp object hierarchy to produce
       the HTML frame to associate with that node.
  INITIAL-DISPLAY-PREDICATE: 
       Function to call on the nodes in your Lisp object hierarchy to determine
       whether to initially display a node's children.
  NODE-INDENTATION:
       Amount of space (in pixels) to indent nodes under their parent nodes.  
       This is an applet-level setting (not per-node).
  LINE-SPACING:
       Amount of space (in pixels) between nodes vertically.  Applet-level setting.
  FONT-SIZE:
       Font used for the node names.  Applet-level setting.
  PALETTE:
       Color palette used by the applet.  List of colors, where a color can be a
       hex string (i.e. \"#FFFF00\"), an RGB triple (i.e. '(255 255 0)), or a color keyword
       as defined by NS1.1:*BUILT-IN-CLIENT-COLORS* (i.e. :AQUAMARINE).  The list may contain
       mixed color representations (i.e. ((255 255 0) :AQUAMARINE)).
  BACKGROUND-COLOR-INDEX:
       Index (i.e. an integer >= 0) to specify a color in the palette to be used for the background color of the applet.
  MARKER-COLOR-INDEX:
       Index (i.e. an integer >= 0) to specify a color in the palette to be used for the color of the twistdown triangle
       marker.
  CONTEXT-URL:
       Node URLs are merged against this absolute URL.  If unspecified, all node URLs must be absolute.
  DEBUG-P: 
       If non-NIL, debugging information is printed
  APPLET-ALIGNMENT:
       How the applet is aligned with respect to the client window.  See the 
       ALIGNMENT argument to WITH-JAVA-APPLET.
  APPLET-NAME: 
       See the NAME keyword argument to WITH-JAVA-APPLET.
  APPLET-ALTERNATE-TEXT
       See the ALTERNATE-TEXT keyword argument to WITH-JAVA-APPLET.
  APPLET-HORIZONTAL-SPACE 
       See the HORIZONTAL-SPACE keyword argument to WITH-JAVA-APPLET.
  APPLET-VERTICAL-SPACE
       See the VERTICAL-SPACE keyword argument to WITH-JAVA-APPLET."
  (%generate-twistdown-tree stream root-object-list child-producer object-printer url-producer frame-producer
			    initial-display-predicate font-color-producer node-indentation
			    line-spacing font-size palette background-color-index marker-color-index
			    context-url debug-p applet-alignment applet-name
			    applet-alternate-text applet-horizontal-space applet-vertical-space))


;;;------------------------------------------------------------------- 
;;;
;;; EXPORTS
;;;

;; Export all the java binary .class files
(http:export-url #u"/cl-http/twistdown-tree/"
		 :directory
		 :pathname "http:examples;twistdown-tree;java;"
                 :immediate-export t)


;;;------------------------------------------------------------------- 
;;;
;;; EXAMPLE USAGE
;;;

(defparameter *tree-1* '("Bob"
			 ("Sue" ("Emily") ("Joe") ("Wayne"))
			 ("Carl"
			  ("Ed") ("Jim")
			  ("Jane"
			   ("Brian"
			    ("Tony"
			     ("Chip")))))))

(defparameter *tree-2* '("Greg"
			 ("Peter" ("Bobby"))
			 ("Marcia" ("Jan" ("Cindy")))))

(defun example-font-color-producer (node)
  (declare (ignore node))
  2)

(defmethod compute-twistdown-tree-example ((url http-url) stream)
  (http:with-successful-response (stream :html)
    (with-html-document (:stream stream)
      (with-document-preamble (:stream stream)
	(declare-title "TwistDown Tree Example" :stream stream))
      (with-document-body (:background :white :stream stream)
	(with-section-heading ("TwistDown Tree Example" :alignment :center :stream stream)
	  (horizontal-line :stream stream)
	  (with-paragraph (:stream stream)
	    (with-emphasis (:quotation  :stream stream)
	      (fast-format stream "This example illustrates how high-level functional interfaces in Common Lisp can
leverage client-side Java to achieve a more inspiring user experience. Here, Java Applets are defined in advance
and exported as Java byte code.  As it emits dynamic HTML, Lisp generates parameters to invoke the applets.")
	      (with-enumeration (stream :itemize)
		(enumerating-item (stream)
		  (fast-format stream "Click on the arrows to expand or collapse nodes."))
		(enumerating-item (stream)
		  (fast-format stream "Click on the text of a node to visit the associated URL.")))))
	  ;; One line of code displays the tree!
	  (with-paragraph (:alignment :center :stream stream)
	    (generate-twistdown-tree stream (list *tree-1* *tree-2*) #'rest
				     :object-printer #'first
				     :palette '(:turquoise-medium :yellow :black)
				     :font-color-producer #'example-font-color-producer
				     :applet-horizontal-space 200))
	  (with-paragraph (:stream stream)
	    (with-emphasis (:quotation  :stream stream)
	      (fast-format stream "If you think of any cool enhancements, please let us know on ~I.  For applet documentation see ~I."
			   (note-anchor "WWW-CL@ai.mit.edu" :reference "mailto:WWW-CL@ai.mit.edu" :stream stream)
			   (note-anchor "http://mit.edu/cvince/www/java/kbcw/hdir/" :reference "http://mit.edu/cvince/www/java/kbcw/hdir/" :stream stream))))
	  (horizontal-line :stream stream)


	  (http:cl-http-signature stream))))))

(http:export-url #u"/cl-http/twistdown-tree/twistdown.html"
		 :computed
		 :response-function #'compute-twistdown-tree-example)



