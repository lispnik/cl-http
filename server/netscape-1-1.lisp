;;;   ;;; -*- Mode: lisp; Package: netscape1.1; BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-

;;;
;;; (c) Copyright 1995, John C. Mallery
;;;     All Rights Reserved.
;;;


;;;------------------------------------------------------------------- 
;;;
;;; INTERFACE FOR AUTHORING HTML USING NETSCAPE 1.1 EXTENSIONS
;;;
;;; Specification from http://home.netscape.com/assist/net_sites/html_extensions.html

(eval-when (load eval compile)
  (let ((html-pkg (find-package :html2))
        (netscape-pkg (find-package :netscape1.1)))
    (do-external-symbols (sym html-pkg)
      (export (intern (symbol-name sym) netscape-pkg) netscape-pkg))))  ;close eval-when

(in-package :netscape1.1)


;;;------------------------------------------------------------------- 
;;;
;;; SIMPLE STUFF
;;;

(define-macro with-centering ((&key (stream '*output-stream*)) &body body)
  "Centers the contents of BODY."
  `(with-environment
     ("CENTER" :stream ,stream)
     ,@body))

(define-macro with-blinking ((&key (stream '*output-stream*) (fresh-line t)) &body body)
  "Blinks the contents of BODY."
  `(with-environment
     ("BLINK" :fresh-line ,fresh-line :stream ,stream)
     ,@body))

(define-macro without-line-breaks ((&key (stream '*output-stream*)) &body body)
  "Prevents line breaks within the scope of BODY."
  `(with-environment ("NOBR" :stream ,stream) ,@body))

(define break-word (&key  (stream *output-stream*))
  "Advises the client where a word break may be inserted on STREAM."
  (issue-command "WBR" stream))

(declaim (inline break-line))

(defconstant *break-line-clear-options* '(:left :right :all))

(define break-line (&key clear (stream *output-stream*))
  "Issues a line break on STREAM.
CLEAR can be any of:  
     :LEFT  -- move down lines until the left margin is clear of images.
     :RIGHT -- move down lines until the right margin is clear of images.
     :ALL   -- move down lines until the right margin is clear of images."
  (%issue-command ("BR" stream)
    (when clear
      (unless (member clear *break-line-clear-options*)
        (error "~S is not one of the known options, ~S." clear *break-line-clear-options*))
      (%write-command-key-arg stream "CLEAR" clear))))

;; defined in http://home.netscape.com/assist/net_sites/pushpull.html
(define declare-refresh-rate (interval &key url (stream *output-stream*))
  "Causes the browser to refresh the display every INTERVAL (in seconds)
when asserted in the document preamble. If URL is provided, the client
will get (reload) url after INTERVAL."
  (check-type interval integer)
  (cond (url
         (let ((args (concatenate 'string (write-to-string interval :base 10. :escape nil)
                                  "; URL="
                                  (url:coerce-url-string url))))
           (declare (dynamic-extent args))
           (declare-meta-info args :header :refresh :stream stream)))
        (t (declare-meta-info interval :header :refresh :stream stream))))

(defmacro with-server-push-response ((stream) &body body)
  "Use this macro to refresh the client display under the server control
from any response function. Each block must be written within a local macro WITH-BLOCK.
Typically an application, will call WITH-BLOCK in a loop that sleeps for
an interval between each iteration. WITH-BLOCK takes the arguments:
  (stream &key content-type content-length content-length content-location (force-output t))

    CONTENT-TYPE is required should be a keyword denoting a mime content type
                 (see: HTTP:MIME-CONTENT-TYPE-KEYWORDS)
    FORCE-OUTPUT should be non-null to transmit output to the client for display
                 after executing BODY.
    SLEEP-INTERVAL is the number of seconds to sleep after executing a block, but before
                 executing the next.

The other header arguments are optional, but recommended. WITH-SERVER-PUSH-RESPONSE
replaces any calls to HTTP:WITH-SUCCESSFUL-RESPONSE or variants."
  (let ((boundary (http::mime-multipart-boundary-string :multipart-mixed-replace)))
    `(macrolet ((with-block ((stream &key last-block-p (force-output t) sleep-interval
                                     content-type content-length content-location) &body body)
                  `(http::with-mime-multipart-block :multipart-mixed-replace
                                                    (,stream
                                                     :last-block-p ,last-block-p
                                                     :boundary ,,boundary
                                                     :force-output ,force-output
                                                     :sleep-interval ,sleep-interval
                                                     :content-type ,content-type
                                                     :content-length ,content-length
                                                     :content-location ,content-location)
                     ,@body)))
       (http:with-successful-response (,stream :multipart-mixed-replace
                                       :location (http:server-url http:*server*))
         (handler-case 
           (progn ,@body)
           (bad-connection-state () t))))))     ;exit via client abort

(declaim (inline %write-width-argument))

(defun %write-width-argument (width stream)
  (if (<= 0 width 1)
      (let ((w (concatenate 'string
                            (write-to-string (floor (* width 100)) :base 10. :escape nil)
                            "%")))
        (declare (dynamic-extent w))
        (%write-command-key-arg stream "WIDTH" w))
      (%write-command-key-arg stream "WIDTH" width t)))

(defconstant *line-alignments* '(:left :right :center))

(define horizontal-line (&key size width alignment (shade t) fresh-line (stream *output-stream*))
  "Writes a horizontal line across the output on STREAM.
SIZE is an integer indicating how thick the line should be.
WIDTH is an integer specifying the absolute length of the line in pixels
or a float between zero and one indicating the percent of the window with to occupy.
ALIGNMENT is one of :left, :right or :center.
SHADE turns on shading."
  (when fresh-line (fresh-line stream))
  (%issue-command ("HR" stream)
    (cond-every
      (size (%write-command-key-arg stream "SIZE" size t))
      (width
        (%write-width-argument width stream))
      (alignment
        (unless (member alignment *line-alignments*)
          (error "~S is not one of the known alignments, ~S" alignment *line-alignments*))
        (%write-command-key-arg stream "ALIGN" (symbol-name alignment)))
      ((not shade)
       (%write-command-key-arg stream "NOSHADE")))))

(defconstant *paragraph-alignments* '(:left :right :center))

(define-macro with-paragraph ((&key alignment (stream '*output-stream*)) &body body)
  "Establishes a paragraph environment.
ALIGNMENT can be any of :LEFT, :RIGHT or :CENTER, defaulting to :LEFT."
  `(%with-environment
     ("P" :stream ,stream)
     (let ((align ,alignment))
       (when align
         (unless (member align *paragraph-alignments*)
           (error "~S is not one of the known alignments, ~S" align *paragraph-alignments*))
         (%write-command-key-arg ,stream "ALIGN" (symbol-name align))))
     ,@body))

(declaim (inline %check-font-size))

(defun %check-font-size (size)
  (unless (< 0 size 8)
    (error "SIZE, ~S, is not an integer from 1 through 7." size)))

(define declare-default-font-size (size &key (stream *output-stream*))
  "Declares the base font size from which all relative font size changes are determined.
The default size is 3."
  (%issue-command ("BASEFONT" stream)
    (%check-font-size size)
    (%write-command-key-arg stream "SIZE" size t)))

(define-macro with-font-size ((size &key (stream '*output-stream*)) &body body)
  "Declares the current font size to be SIZE within BODY."
  `(let ((size ,size))
     (%check-font-size size)
     (%with-environment ("FONT" :fresh-line nil :stream ,stream)
                        (%write-command-key-arg ,stream "SIZE" size t)
       ,@body)))

(define declare-search-index (&key prompt url (stream *output-stream*))
  "Declares that the current document is searchable."
  (declare (notinline %write-command-key-arg))
  (%issue-command ("ISINDEX" stream :fresh-line t :trailing-line t)
    (cond-every
      (url
        (%write-command-key-arg stream "HREF" (coerce-url-string url)))
      (prompt
        (%write-command-key-arg stream "PROMPT" prompt)))))

;;;------------------------------------------------------------------- 
;;;
;;; COLOR MAPPING IN THE BODY ARGUMENTS
;;; 

(define encode-color (red green blue)
  "Encodes a 256 bit color mix of RED, GREEN, and BLUE as a hexadecimal string."
  (check-type red (integer 0 255) "an integer between 0 and 255")
  (check-type green (integer 0 255) "an integer between 0 and 255")
  (check-type blue (integer 0 255) "an integer between 0 and 255")
  (macrolet ((hexdecimal-digit (hexdecimal position)
               `(aref "0123456789ABCDEF" (ldb (byte 4 ,position) ,hexdecimal))))
    (let ((string (make-array 7 :element-type 'standard-char)))
      (www-utils:with-fast-array-references 
        ((string string string))
        (setf (aref string 0) #\#
              (aref string 1) (hexdecimal-digit red 4)
              (aref string 2) (hexdecimal-digit red 0)
              (aref string 3) (hexdecimal-digit green 4)
              (aref string 4) (hexdecimal-digit green 0)
              (aref string 5) (hexdecimal-digit blue 4)
              (aref string 6) (hexdecimal-digit blue 0))
        string))))

(define decode-color (hexcolor)
  "Decodes HEXCOLOR and returns the 256-bit mix of RED, GREEN, and BLUE."
  (declare (values red green blue))
  (unless (eql (aref hexcolor 0) #\#)
    (error "~S is not one of the known client color mappings." hexcolor))
  (values (parse-integer hexcolor :start 1 :end 3 :radix 16)
          (parse-integer hexcolor :start 3 :end 5 :radix 16)
          (parse-integer hexcolor :start 5 :end 7 :radix 16)))

(defvar *built-in-client-color-mapping-table* nil)

(defvar *built-in-client-colors* nil)

(defvar *number-of-built-in-client-colors* 0)

(define %define-color-mapping (keyword hexcolor index &optional rgb-list)
  (check-type keyword keyword)
  (check-type hexcolor string)
  (unless rgb-list
    (setq rgb-list (multiple-value-list (decode-color hexcolor))))
  (setf (gethash keyword *built-in-client-color-mapping-table*) hexcolor
        (get keyword 'color-index) index
        (gethash hexcolor *built-in-client-color-mapping-table*) keyword
        (gethash rgb-list *built-in-client-color-mapping-table*) hexcolor
        (gethash index *built-in-client-color-mapping-table*) hexcolor )) 

(define define-color-mapping (keyword red green blue)
  "Associates the color name KEYWORD with the 256 bit mix of RED, GREEN, and BLUE.
Each primary color must be an integer between 0 and 255."
  (let* ((index (get keyword 'color-index))
         (hexcolor (encode-color red green blue))
         (rgb-list (list red green blue))
         (new-p (not index)))
    (unless new-p
      (setq index (1+ *number-of-built-in-client-colors*)))
    (%define-color-mapping keyword hexcolor index rgb-list)
    (unless new-p
      (setf *built-in-client-colors* `(,.*built-in-client-colors* ,keyword))
      (incf *number-of-built-in-client-colors*))
    hexcolor)) 

(defun %define-client-color-mappings (mappings)
  (unless *built-in-client-color-mapping-table*
    (setq *built-in-client-color-mapping-table* (make-hash-table :test #'equalp :size 100)))
  (loop initially (clrhash *built-in-client-color-mapping-table*)
        for (keyword . value) in mappings
        for idx upfrom 0
        do (%define-color-mapping keyword value idx)
        collect keyword into keywords
        finally (setq *built-in-client-colors* keywords
                      *number-of-built-in-client-colors* (1+ idx))))

(define color-mapping (color &optional (error-p t))
  "Returns the color map specifier for COLOR.
when color is :RANDOM, a randomly selected color is returned.
When COLOR is a hexidecimal color, it is returned.
When COLOR is three element list denoting a 256-bit red, green, and blue,
the hexidecimal color string is returned. "
  (typecase color
    (string
      (with-fast-array-references ((color color))
        (cond ((eql (aref color 0) #\#) color)
              ((gethash (http:symbolize color http:*keyword-package*) *built-in-client-color-mapping-table*))
              (error-p (error "~S is not one of the known client color mappings." color))
              (t nil))))
    (keyword
      (cond ((eql color :random) (random-color))
            ((gethash color *built-in-client-color-mapping-table*))
            (error-p (error "~S is not one of the known client color mappings." color))
            (t nil)))
    (cons (apply #'encode-color color))
    (t (if error-p
           (error "~S is not one of the known client color mappings." color)
           nil))))

(define color-mapping-keyword (color-specifier &optional (error-p t))
  "Returns the color map keyword for COLOR-SPECIFIER,
which can be a hexidecimal color, the index number for the color,
or a 256-bit list denoting a mixture of red, green, and blue. "
  (or (typecase color-specifier
        (string
          (gethash color-specifier *built-in-client-color-mapping-table*))
        (number 
          (gethash color-specifier *built-in-client-color-mapping-table*))
        (cons
          (let* ((hexcolor (gethash color-specifier *built-in-client-color-mapping-table*))
                 (keyword (and hexcolor (gethash hexcolor *built-in-client-color-mapping-table*))))
            (cond (keyword)
                  (error-p (apply #'encode-color color-specifier))
                  (t nil))))
        (t nil))
      (and error-p 
           (error "~S is not one of the known client color mappings." color-specifier))))

(define random-color ()
  "Returns a random built-in color."
  (declare (values color color-index))
  (let ((color (floor (random (* 100 *number-of-built-in-client-colors*)) 100)))
    (values (gethash color *built-in-client-color-mapping-table*) color)))

(declaim (inline random-color-keyword))

(define random-color-keyword ()
  "Returns the keyword for a random built-in color."
  (color-mapping-keyword (random-color)))

(define invent-random-color (&optional red green blue)
  "Invents a random color by randomly selecting the color mixture.
RED, GREEN, or BLUE can be be supplied to constrain the random selection to
any omitted primary colors."
  (flet ((random-255 ()
           (floor (random (* 100 255)) 100)))
    (encode-color (or red (random-255))
                  (or green (random-255))
                  (or blue (random-255)))))

(defun inverse-color (color)
  "Returns the inverse color for COLOR."
  (declare (values new-color))
  (flet ((inverse (c)
           (declare (fixnum c))
           (abs (- 255 c))))
    (declare (inline inverse))
    (multiple-value-bind (r g b)
        (decode-color (color-mapping color t))
      (encode-color (inverse r) (inverse g) (inverse b)))))

(defun add-color (color &optional red green blue)
  "Adds the RGB components RED, GREEN, BLUE to COLOR.
COLOR is a hexadecimal color or a color keyword.
RED, GREEN, BLUE are integers between -255 and +255."
  (declare (values new-color))
  (check-type red integer)
  (check-type green integer)
  (check-type blue integer)
  (flet ((add-color (c delta)
                    (declare (fixnum c delta))
                    (max (min (the fixnum (+ c delta)) 255) 0)))
    (multiple-value-bind (r g b)
        (decode-color (color-mapping color t))
      (encode-color (add-color r red) (add-color g green) (add-color b blue)))))
 

(defmacro define-client-color-mappings (&rest mappings)
  `(%define-client-color-mappings ',mappings))

;; available for Netscape 1.1N from http://www.infi.net/wwwimages/colorindex.html
(define-client-color-mappings
  (:Aqua . "#00FFFF")                           ;HTML 3.2
  (:Aquamarine . "#70DB93")
  (:Aquamarine-Medium . "#32CD99")
  (:Black . "#000000")                          ;HTML 3.2
  (:Blue . "#0000FF")
  (:Blue-Cadet . "#5F9F9F")
  (:Blue-Corn-Flower . "#42426F")
  (:Blue-Light . "#C0D9D9")
  (:Blue-Medium . "#3232CD")
  (:Blue-Midnight . "#2F2F4F")
  (:Blue-Midnight-New . "#00009C")
  (:Blue-Navy . "#23238E")
  (:Blue-Neon . "#4D4DFF")
  (:Blue-Rich . "#5959AB")
  (:Blue-Sky . "#3299CC")
  (:Blue-Slate . "#007FFF")
  (:Blue-Slate-Dark . "#6B238E")
  (:Blue-Slate-Medium . "#7F00FF")
  (:Blue-Steel . "#236B8E")
  (:Blue-Steel-Light . "#8F8FBD")
  (:Blue-Violet . "#9F5F9F")
  (:Brass . "#B5A642")
  (:Bronze . "#8C7853")
  (:Bronze-II . "#A67D3D")
  (:Brown . "#A62A2A")
  (:Brown-Dark . "#5C4033")
  (:Brown-Dark-Very . "#5C4033")
  (:Chocolate-Bakers . "#5C3317")
  (:Chocolate-Semi-Sweet . "#6B4226")
  (:Copper . "#B87333")
  (:Copper-Cool . "#D98719")
  (:Coral . "#FF7F00")
  (:Cyan . "#00FFFF")
  (:Feldspar . "#D19275")
  (:Firebrick . "#8E2323")
  (:Fuchsia . "#FF00FF")                        ;HTML 3.2
  (:Gold . "#CD7F32")
  (:Gold-Bright . "#D9D919")
  (:Gold-Old . "#CFB53B")
  (:Goldenrod . "#DBDB70")
  (:Goldenrod-Medium . "#EAEAAE")
  ;;(:Green . "#00FF00")
  (:Green . "#008000")                          ;HTML 3.2
  (:Green-Copper . "#527F76")
  (:Green-Copper-Dark . "#4A766E")
  (:Green-Dark . "#2F4F2F")
  (:Green-Forest . "#238E23")
  (:Green-Forest-Medium . "#6B8E23")
  (:Green-Hunter . "#215E21")
  (:Green-Lime . "#32CD32")
  (:Green-Olive-Dark . "#4F4F2F")
  (:Green-Pale . "#8FBC8F")
  (:Green-Sea . "#238E68")
  (:Green-Sea-Medium . "#426F42")
  (:Green-Spring . "#00FF7F")
  (:Green-Spring-Medium . "#7FFF00")
  (:Green-Yellow . "#93DB70")
  (:Green-Yellow . "#99CC32")
  (:Gray . "#808080")                           ;HTML 3.2
  (:Grey . "#C0C0C0")
  (:Grey-Dim . "#545454")
  (:Grey-Light . "#A8A8A8")
  (:Grey-Light-Very . "#CDCDCD")
  (:Grey-Slate-Dark . "#2F4F4F")
  (:Khaki . "#9F9F5F")
  (:Lime . "#00FF00")                           ;HTML 3.2
  (:Magenta . "#FF00FF")
  ;;(:Maroon . "#8E236B")
  (:Maroon . "#800000")                         ;HTML 3.2
  (:Navy . "#000080")                           ;HTML 3.2
  (:Olive . "#808000")                          ;HTML 3.2
  (:Orange . "#FF7F00")
  (:Orange-Mandarian . "#E47833")               ;historical typo
  (:Orange-Mandarin . "#E47833")
  (:Orchid . "#DB70DB")
  (:Orchid-Dark . "#9932CD")
  (:Orchid-Medium . "#9370DB")
  (:Pink . "#BC8F8F")
  (:Pink-Neon . "#FF6EC7")
  (:Pink-Spicy . "#FF1CAE")
  (:Plum . "#EAADEA")
  (:Purple . "#800080")                         ;HTML 3.2
  (:Purple-Dark . "#871F78")
  (:Quartz . "#D9D9F3")
  (:Red . "#FF0000")                            ;HTML 3.2
  (:Red-Indian . "#4E2F2F")
  (:Red-Orange . "#FF2400")
  (:Red-Violet . "#CC3299")
  (:Red-Violet-Medium . "#DB7093")
  (:Rose-Dusty . "#856363")
  (:Salmon . "#6F4242")
  (:Scarlet . "#8C1717")
  (:Sienna . "#8E6B23")
  ;;(:Silver . "#E6E8FA")
  (:Silver . "#C0C0C0")                         ;HTML 3.2
  (:Sky-Summer . "#38B0DE")
  (:Tan . "#DB9370")
  (:Tan-Dark . "#97694F")
  (:Tan-New . "#EBC79E")
  (:Teal . "#008080")                           ;HTML 3.2
  (:Thistle . "#D8BFD8")
  (:Turquoise . "#ADEAEA")
  (:Turquoise-Dark . "#7093DB")
  (:Turquoise-Medium . "#70DBDB")
  (:Violet . "#4F2F4F")
  (:Wheat . "#D8D8BF")
  (:White . "#FFFFFF")                          ;HTML 3.2
  (:Wood-Dark . "#855E42")
  (:Wood-Light . "#E9C2A6")
  (:Wood-Medium . "#A68064")
  (:Yellow . "#FFFF00"))                        ;HTML 3.2


;;;------------------------------------------------------------------- 
;;;
;;;  Predefined backgrounds (courtesey of Netscape)
;;;
;;; If you intend to serve them, please copy these to your own machine and replace the 
;;; URL as appropriate in the definition below.

(defvar *built-in-background-mapping-table* nil)

(defvar *built-in-backgrounds* nil)

(defvar *number-of-built-in-backgrounds* 0)

(defun %define-background-mappings (mappings)
  (unless *built-in-background-mapping-table*
    (setq *built-in-background-mapping-table* (make-hash-table :test #'equal :size 100)))
  (loop for (keyword . value) in mappings
        for idx upfrom 0
        do (check-type keyword keyword)
           (check-type value string)
           (setf (gethash keyword *built-in-background-mapping-table*) value
                 (gethash idx *built-in-background-mapping-table*) value
                 (gethash value *built-in-background-mapping-table*) keyword)
        collect keyword into keywords
        finally (setq *built-in-backgrounds* keywords
                      *number-of-built-in-backgrounds* (1+ idx))))

(define random-background-url ()
  "Returns a random built-in background."
  (declare (values url-string index))
  (let ((index (floor (random (* 100 *number-of-built-in-backgrounds*)) 100)))
    (values (gethash index *built-in-background-mapping-table*) index)))

(declaim (inline random-background-url-keyword))

(define random-background-url-keyword ()
  "Returns the keyword for a random built-in background."
  (background-url-keyword (random-background-url)))

(define background-url (url-or-keyword &optional (error-p t))
  "Returns the background URL for URL-OR-KEYWORD.
URL-OR-KEYWORD can be a keyword denoting a named URL or :random.
It can also be a url object or a url string."
  (typecase url-or-keyword
    (string url-or-keyword)
    (keyword
      (cond ((eql url-or-keyword :random-url) (random-background-url))
            ((gethash url-or-keyword *built-in-background-mapping-table*))
            (error-p (error "~S is not one of the known client background mappings." url-or-keyword))
            (t nil)))
    (url:url (url:coerce-url-string url-or-keyword))
    (t (if error-p
           (error "~S is not one of the known client background mappings." url-or-keyword)
           nil))))

(define background-url-keyword (background-url &optional (error-p t))
  "Returns the color map keyword for background-url"
  (cond ((gethash background-url *built-in-background-mapping-table*))
        (error-p (error "~S is not one of the known client background URL mappings." background-url))
        (t nil)))

(defmacro define-background-mappings (&rest mappings)
  `(%define-background-mappings ',mappings)) 

(define-background-mappings
  (:aluminum-brushed . "http://home.netscape.com/assist/net_sites/bg/metal/brushed_aluminum.gif")
  (:aluminum-gray . "http://home.netscape.com/assist/net_sites/bg/metal/gray_aluminum.gif")
  (:clouds . "http://home.netscape.com/assist/net_sites/bg/water/clouds.gif")
  (:dots-1960 . "http://home.netscape.com/assist/net_sites/bg/dots/1960_dots.gif")
  (:dots-grey . "http://home.netscape.com/assist/net_sites/bg/dots/grey_dots.gif")
  (:dots-multicolor . "http://home.netscape.com/assist/net_sites/bg/dots/multicolor1_dots.gif")
  (:fabric-gray . "http://home.netscape.com/assist/net_sites/bg/fabric/gray_fabric.gif")
  (:fabric-pink . "http://home.netscape.com/assist/net_sites/bg/fabric/pink_fabric.gif")
  (:fabric-yellow . "http://home.netscape.com/assist/net_sites/bg/fabric/yellow_fabric.gif")
  (:marble-70s . "http://home.netscape.com/assist/net_sites/bg/marble/70s_marble.gif")
  (:marble-blue . "http://home.netscape.com/assist/net_sites/bg/marble/blue_marble.gif")
  (:marble-greenred . "http://home.netscape.com/assist/net_sites/bg/marble/greenred_marble.gif")
  (:marble-lavender . "http://home.netscape.com/assist/net_sites/bg/marble/lavender_marble.gif")
  (:marble-olivepink . "http://home.netscape.com/assist/net_sites/bg/marble/olivepink_marble.gif")
  (:marble-purple1 . "http://home.netscape.com/assist/net_sites/bg/marble/purple_marble1.gif")
  (:marble-purple2 . "http://home.netscape.com/assist/net_sites/bg/marble/purple_marble2.gif")
  (:marble-purpleblue . "http://home.netscape.com/assist/net_sites/bg/marble/purpleblue_marble.gif")
  (:marble-redgray . "http://home.netscape.com/assist/net_sites/bg/marble/redgray_marble.gif")
  (:marble-swirl-lavender . "http://home.netscape.com/assist/net_sites/bg/marble/lavender_swirl_marble.gif")
  (:marble-tanblue . "http://home.netscape.com/assist/net_sites/bg/marble/tanblue_marble.gif")
  (:metal-corrugated . "http://home.netscape.com/assist/net_sites/bg/metal/corrugated_metal.gif")
  (:paper-blue . "http://home.netscape.com/assist/net_sites/bg/paper/blue_paper.gif")
  (:paper-bluesand . "http://home.netscape.com/assist/net_sites/bg/paper/bluesand_paper.gif")
  (:paper-bluewhite . "http://home.netscape.com/assist/net_sites/bg/paper/bluewhite_paper.gif")
  (:paper-bright . "http://home.netscape.com/assist/net_sites/bg/paper/bright_paper.gif")
  (:paper-greenwhite . "http://home.netscape.com/assist/net_sites/bg/paper/greenwhite_paper.gif")
  (:paper-lavender . "http://home.netscape.com/assist/net_sites/bg/paper/lavender_paper.gif")
  (:paper-multidot . "http://home.netscape.com/assist/net_sites/bg/paper/multidot_paper.gif")
  (:paper-olive . "http://home.netscape.com/assist/net_sites/bg/paper/olive_paper.gif")
  (:paper-olivered . "http://home.netscape.com/assist/net_sites/bg/paper/olivered_paper.gif")
  (:paper-orange . "http://home.netscape.com/assist/net_sites/bg/paper/orange_paper.gif")
  (:paper-peach . "http://home.netscape.com/assist/net_sites/bg/paper/peach_paper.gif")
  (:paper-purple . "http://home.netscape.com/assist/net_sites/bg/paper/purple_paper.gif")
  (:paper-red . "http://home.netscape.com/assist/net_sites/bg/paper/red_paper.gif")
  (:paper-redwhite . "http://home.netscape.com/assist/net_sites/bg/paper/redwhite_paper.gif")
  (:paper-smblue . "http://home.netscape.com/assist/net_sites/bg/paper/smblue_paper.gif")
  (:paper-smbluewhite . "http://home.netscape.com/assist/net_sites/bg/paper/smbluewhite_paper.gif")
  (:paper-summer . "http://home.netscape.com/assist/net_sites/bg/paper/summer_paper.gif")
  (:paper-tan . "http://home.netscape.com/assist/net_sites/bg/paper/tan_paper.gif")
  (:paper-teal . "http://home.netscape.com/assist/net_sites/bg/paper/teal_paper.gif")
  (:paper-wind . "http://home.netscape.com/assist/net_sites/bg/paper/wind_paper.gif")
  (:paper-yellow . "http://home.netscape.com/assist/net_sites/bg/paper/yellow_paper.gif")
  (:raindrops-dark . "http://home.netscape.com/assist/net_sites/bg/water/raindrops_dark.gif")
  (:raindrops-light . "http://home.netscape.com/assist/net_sites/bg/water/raindrops_light.gif")
  (:rock-blue . "http://home.netscape.com/assist/net_sites/bg/rock/blue_rock.gif")
  (:rock-gray . "http://home.netscape.com/assist/net_sites/bg/rock/gray_rock.gif")
  (:rock-multicolor1 . "http://home.netscape.com/assist/net_sites/bg/rock/multicolor1_rock.gif")
  (:rock-multicolor2 . "http://home.netscape.com/assist/net_sites/bg/rock/multicolor2_rock.gif")
  (:rock-multicolor3 . "http://home.netscape.com/assist/net_sites/bg/rock/multicolor3_rock.gif")
  (:rock-red . "http://home.netscape.com/assist/net_sites/bg/rock/red_rock.gif")
  (:rock-smblue . "http://home.netscape.com/assist/net_sites/bg/rock/smblue_rock.gif")
  (:rock-yellow . "http://home.netscape.com/assist/net_sites/bg/rock/yellow_rock.gif")
  (:stucco-gray . "http://home.netscape.com/assist/net_sites/bg/stucco/gray_stucco.gif")
  (:stucco-green . "http://home.netscape.com/assist/net_sites/bg/stucco/green_stucco.gif")
  (:stucco-red . "http://home.netscape.com/assist/net_sites/bg/stucco/red_stucco.gif")
  (:stucco-smgreen . "http://home.netscape.com/assist/net_sites/bg/stucco/smgreen_stucco.gif")
  (:stucco-yellow . "http://home.netscape.com/assist/net_sites/bg/stucco/yellow_stucco.gif")
  (:weave-blue . "http://home.netscape.com/assist/net_sites/bg/weave/blue_weave.gif")
  (:weave-bluewind . "http://home.netscape.com/assist/net_sites/bg/weave/bluewind_weave.gif")
  (:weave-embossed . "http://home.netscape.com/assist/net_sites/bg/weave/embossed_weave.gif")
  (:weave-funkyblue . "http://home.netscape.com/assist/net_sites/bg/weave/funkyblue_weave.gif")
  (:weave-lipurple . "http://home.netscape.com/assist/net_sites/bg/weave/lipurple_weave.gif")
  (:weave-yellow . "http://home.netscape.com/assist/net_sites/bg/weave/yellow_weave.gif"))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

(eval-when (compile load eval)
  (defconstant *body-color-argument-alist* '((:background-url ."BACKGROUND")
                                             (:background . "BGCOLOR")
                                             (:foreground . "TEXT")
                                             (:link . "LINK")
                                             (:visited-link . "VLINK")
                                             (:active-link . "ALINK")))
   
  (defun %body-arg-key (arg-key)
    (or (cdr (assoc (http:symbolize arg-key http:*keyword-package*) *body-color-argument-alist* :test #'eq))
        (error "Unknown body argument, ~S." arg-key)))
  )

(defun body-arguments (background-url background foreground link visited-link active-link)
  (macrolet ((collect (vars)
               (flet ((%body-arg-value (value url-ok-p)
                        (if url-ok-p
                            `(typecase ,value
                               (keyword (background-url ,value))
                               (t (url:coerce-url-string ,value nil nil)))
                            `(color-mapping ,value))))
                 `(let ((args nil))
                    (cond-every
                      ,.(loop for var in (reverse vars)
                              for url-ok-p = (eql var 'background-url)
                              collect `(,var
                                        (push (list ,(%body-arg-key var)
                                                    ,(%body-arg-value var url-ok-p))
                                              args))))
                    args))))
    
    (collect (background-url background foreground link visited-link active-link))))

(define-macro with-document-body ((&key background-url background foreground link visited-link active-link
                                        (stream '*output-stream*)) &body body)
  "Asserts the contents of BODY are the body of the document (see BODY).

  BACKGROUND-URL   -- an image URL to use as the background 
  BACKGROUND       -- color for the background.
  FOREGROUND       -- color for the foreground.
  LINK             -- color for links
  VISITED-LINK     -- color for visited links.
  ACTIVE-LINK      -- color for active links.

  See the variable *BUILT-IN-CLIENT-COLORS* for a complete list of colors 
  built into the client. For information on how to use these, 
  see: http://home.netscape.com/assist/net_sites/bg/index.html
  The variable *built-in-backgrounds* contains a list of backgrounds
  and please use BACKGROUND-URL to map the keyword into the URL. 
  Note that Background URLs must be specified as either URL strings or 
  interned URLS because keywords are interpreted as referring to built-in 
  colors. For a sampling of backgrounds, 
  see: http://home.netscape.com/assist/net_sites/bg/backgrounds.html"
   `(with-environment
        ("BODY" :arguments (body-arguments ,background-url ,background ,foreground
                                           ,link ,visited-link ,active-link)
          :stream ,stream)
        ,@body))


;;;------------------------------------------------------------------- 
;;;
;;; LIST EXTENSIONS
;;;

(defconstant *enumerate-bullet-types*
             '(:capital-letters "A" :small-letters  "a" :large-roman  "I" :small-roman  "i" :arabic "1"))

(defconstant *itemize-bullet-types* '(:solid-disc "disc" :circle "circle" :square "square"))

(declaim (inline bullet-type))

(defun bullet-type (style type)
  (macrolet ((get-type (type plist)
               `(and ,type (or (getf ,plist ,type)
                               (error "Bullet type. ~S, is not one of the known types, ~S."
                                      ,type
                                      (loop for item in ,plist by #'cddr
                                            collect item))))))
    (case style
      (:enumerate (get-type type *enumerate-bullet-types*))
      (:itemize (get-type type *itemize-bullet-types*))
      (t (and type
              (error "Bullet types are not available for enumeration styles, ~S." style))
         nil))))

(defvar *enumeration-style* nil)

(defun enumerate-typed-item (stream continuation icon-url type)
  (flet ((icon-url? (command icon-url type-string)
           (cond ((and type-string icon-url)
                  (concatenate 'string command " TYPE=\"" type-string "\""
                               " SRC=" (coerce-url-string icon-url)))
                 (type-string
                  (concatenate 'string command " TYPE=\"" type-string "\""))
                 (icon-url
                  (concatenate 'string command " SRC=" (coerce-url-string icon-url)))
                 (t command))))
    (declare (inline icon-url?))
    (fresh-line stream)
    (issue-command (icon-url? "LI" icon-url (bullet-type *enumeration-style* type)) stream)
    (prog1 (funcall continuation stream)
           (fresh-line stream))))

(define-macro enumerating-item ((stream &key icon-url head type) &body body)
  "TYPE allows the default styles of enumeration to be overridden for the styles:

  ENUMERATE = :CAPITAL-LETTERS, :SMALL-LETTERS, :LARGE-ROMAN, :SMALL-ROMAN,
  or :ARABIC (the default).

  ITEMIZE = :SOLID-DISC, :CIRCLE, :SQUARE"
  `(funcall html2::*enumeration-function* ,stream
            #'(lambda (,stream) ,@body)
            ,icon-url ,(or head type)))

(defun enumeration-style-commands (style &optional compact type)
  (declare (values open-command close-command))
  (flet ((compact? (command compact type-string)
           (cond ((and type-string compact)
                  (concatenate 'string command " TYPE=\"" type-string "\"" " COMPACT"))
                 (type-string
                  (concatenate 'string command " TYPE=\"" type-string "\""))
                 (compact
                  (concatenate 'string command " COMPACT"))
                 (t command))))
    (declare (inline compact?))
    (let ((entry (assoc style html2::*enumeration-styles*)))
      (cond (entry
             (destructuring-bind (open . close) (cdr entry)
               (values (compact? open compact (bullet-type style type)) close)))
            (t (error "Unknown enumeration style, ~A." style))))))

(define-macro with-enumeration ((stream style &key compact type) &body body)
  "Enumerates items according to STYLE.
STYLE can be :ENUMERATE :ITEMIZE :PLAIN :MENU :DIRECTORY :DEFINITION
COMPACT tries to keep the entry compact.
TYPE allows the default styles of enumeration to be overridden for the styles:

     ENUMERATE = :CAPITAL-LETTERS, :SMALL-LETTERS, :LARGE-ROMAN, :SMALL-ROMAN,
                 or :ARABIC (the default).

     ITEMIZE = :SOLID-DISC, :CIRCLE, :SQUARE

Each item for enumeration is called inside the enumerating-item macro.
this macro accepts a keyword argument ICON-URL, which is useful when
an icon is desired as the bullet.

You must use the ENUMERATING-ITEM from the same package for reliable results."
  `(multiple-value-bind (open close)
       (enumeration-style-commands ,style ,compact ,type)
     (with-environment (open :close-command close :stream ,stream)
       (let ((*enumeration-style* ,style)
             (html2::*enumeration-function* (case ,style
                                              (:plain #'html2::enumerate-plain-item)
                                              (:definition #'html2::enumerate-definition-item)
                                              ((:enumerate :itemize) #'enumerate-typed-item)
                                              (t #'html2::enumerate-normal-item))))
         ,@body))))

(define enumerate-item-list (item-list &key (style :itemize) type compact (stream *output-stream*))
  (with-enumeration (stream style :compact compact :type type)
    (dolist (item item-list)
      (enumerating-item (stream)
        (write item :stream stream :escape nil)))))

;;;------------------------------------------------------------------- 
;;;
;;; TABLES as described in http://home.netscape.com/assist/net_sites/tables.html
;;;

(defconstant *table-horizontal-alignments* '(:left :center :right))

(defconstant *table-vertical-alignments* '(:top :middle :bottom :baseline))

(defun %write-table-environment-arguments (stream width horizontal-alignment vertical-alignment &optional no-wrap column-span row-span)
  (macrolet ((check-alignment-arg (alignment possible-alignments)
               `(unless (member ,alignment ,possible-alignments)
                  (error "~S is not one of the possible alignments, ~S, for ~S."
                         ,alignment ,possible-alignments
                         ,(http:symbolize (symbol-name alignment) http:*keyword-package*)))))
    (cond-every
      (width (%write-width-argument width stream))
      (horizontal-alignment
        (check-alignment-arg horizontal-alignment *table-horizontal-alignments*)
        (%write-command-key-arg stream "ALIGN" horizontal-alignment))
      (vertical-alignment
        (check-alignment-arg vertical-alignment *table-vertical-alignments*)
        (%write-command-key-arg stream "VALIGN" vertical-alignment))
      (no-wrap
        (%write-command-key-arg stream "NOWRAP"))
      (column-span
        (%write-command-key-arg stream "COLSPAN" column-span t))
      (row-span
        (%write-command-key-arg stream "ROWSPAN" row-span t)))))

(defun %write-table-arguments (stream height width border cell-spacing cell-padding)
  (cond-every
    (height (%write-command-key-arg stream "HEIGHT" height t))
    (width (%write-width-argument width stream))
    (border
      (if (integerp border)
          (%write-command-key-arg stream "BORDER" border t)
          (%write-command-key-arg stream "BORDER")))
    (cell-spacing
      (%write-command-key-arg stream "CELLSPACING" cell-spacing t))
    (cell-padding
      (%write-command-key-arg stream "CELLPADDING" cell-padding t))))

(define-macro with-caption ((&key (alignment :top) (stream '*output-stream*)) &body body)
  "Asserts the contents of BODY is a caption within a table environment.
:ALIGNMENT can be :TOP or :BOTTOM."
  `(%with-environment
     ("ALIGN" :stream ,stream)
     (%write-command-key-arg ,stream "ALIGN"
                             (ecase ,alignment
                               (:top "TOP")
                               (:bottom "BOTTOM")))
     ,@body))

(define-macro with-table-row ((&key width horizontal-alignment vertical-alignment (stream '*output-stream*)) &body body)
  "Asserts that the contents of BODY is a row in a table."
  `(%with-environment
     ("TR" :fresh-line t :stream ,stream)
     (%write-table-environment-arguments ,stream ,width ,horizontal-alignment ,vertical-alignment)
     ,@body))

(define-macro with-table-cell ((&key width header-p horizontal-alignment vertical-alignment (break-lines-p t)
                                     column-span row-span (stream '*output-stream*)) &body body)
  "Asserts that the contents of BODY is a cell withing a table environment
    WIDTH is an integer specifying the absolute length of the line in pixels or
    a float between zero and one indicating the percent of the window with to occupy.
    HEADER-P             controls whether the cell is a header cell or an ordinary cell.
    HORIZONTAL-ALIGNMENT can be any of :LEFT, :CENTER, or :RIGHT.
    VERTICAL-ALIGNMENT   can be any of :TOP, :MIDDLE, :BOTTOM, or :BASELINE.
    BREAK-LINES-P        prevents lines from being broken to fit the width of a cell.
    COLUMN-SPAN          is an integer that controls the number of columns a cell spans.
    ROW-SPAN             is an integer that controls the number of columns a cell spans."
  `(let ((tag (if ,header-p "TH" "TD")))
     (%with-environment
       (tag :fresh-line nil :stream ,stream)
       (%write-table-environment-arguments ,stream ,width ,horizontal-alignment ,vertical-alignment
                                           (not ,break-lines-p) ,column-span ,row-span)
       ,@body)))

(define-macro with-table ((&key caption height width border cell-spacing cell-padding
                                (caption-alignment :top) (caption-size 3)
                                (stream '*output-stream*)) &body body)
  "Establishes a table environment with BODY.

  CAPTION is a string positioned at CAPTION-ALIGNMENT, :either :TOP or :BOTTOM.

  HEIGHT is an integer specifying the absolute height of the table in pixels.

  WIDTH is an integer specifying the absolute length of the table in pixels or a
  float between zero and one indicating the percent of the window with to
  occupy.

  BORDER is either T, NIL or an integer.

  CELL-SPACING is an integer, defaulting to 2, that controls the space between
  cells.

  CELL-PADDING is an integer, defaulting to 1 that controls the space between
  text contained in a cell and the wall."
   `(%with-environment ("TABLE" :fresh-line t :stream ,stream)
                       (%write-table-arguments ,stream ,height ,width ,border ,cell-spacing ,cell-padding)
      ,.(cond (caption
               `((with-caption (:alignment ,caption-alignment :stream ,stream)
                   (with-font-size (,caption-size :stream ,stream)
                     (write-string ,caption ,stream)))))
              (t caption-alignment caption-size ; ignore
                 nil))
      ,@body))


;;;------------------------------------------------------------------- 
;;;
;;; IMAGE EXTENSIONS
;;;

(defconstant *image-alignment-values* '(:left :right :top :texttop :middle :absmiddle :baseline :bottom :absbottom))

;; html spec says to always provide alternative text
(defun %note-image (stream image-url alternative-text alignment accept-coordinates-at-url
                           border vertical-space horizontal-space width height)
  (flet ((write-element (stream image-url image-url-string alignment alternative-text accept-coordinates-at-url)
           (flet ((alignment-value (alignment)
                    (unless (member alignment *image-alignment-values*)
                      (error "Unknown alignment, ~S, for an image." alignment))
                    (symbol-name alignment))
                  (write-integer-arg (stream option value)
                    (check-type value integer)
                    (%write-command-key-arg stream option value t)))
             (declare (inline alignment-value write-integer-arg))
             (%issue-command ("IMG" stream)
               ;; Automagically insert image sizes when algorithms available.
               (when (and image-url (not (or width height)) http:*image-sizes-default-automatically*)
                 (multiple-value-setq (width height)
                   (url:image-size image-url)))
               (cond-every
                 (image-url-string
                   (%write-command-key-arg stream "SRC" image-url-string))
                 (alignment
                   (%write-command-key-arg stream "ALIGN" (alignment-value alignment)))
                 (alternative-text
                   (check-type alternative-text string)
                   (%write-command-key-arg stream "ALT" alternative-text))
                 (accept-coordinates-at-url (%write-command-key-arg stream "ISMAP"))
                 (border (write-integer-arg stream "BORDER" border))
                 (vertical-space (write-integer-arg stream "VSPACE" vertical-space))
                 (horizontal-space (write-integer-arg stream "HSPACE" horizontal-space))
                 (width (write-integer-arg stream "WIDTH" width))
                 (height (write-integer-arg stream "HEIGHT" height)))))))
    (declare (dynamic-extent #'write-element))
    (let* ((url-string (url:name-string-without-search-suffix image-url nil))
           (real-image-url (typecase image-url
                             (string nil)
                             (t (intern-url url-string)))))
      (declare (dynamic-extent url-string))
      (case accept-coordinates-at-url
        ((nil :no-url)
         (write-element stream real-image-url url-string alignment alternative-text accept-coordinates-at-url))
        (t (with-anchor-noted (:reference (if (eq accept-coordinates-at-url t)
                                              url-string
                                              (url:name-string-without-search-suffix accept-coordinates-at-url nil))
                               :stream stream)
             (write-element stream real-image-url url-string alignment alternative-text accept-coordinates-at-url)))))))

(declaim (inline image))

(define image (image-url alternative-text
                         &key (alignment :left) accept-coordinates-at-url
                         border vertical-space horizontal-space width height
                         (stream *output-stream*))
  (%note-image stream image-url alternative-text alignment accept-coordinates-at-url
               border vertical-space horizontal-space width height))

(setf (documentation 'ns1.1:image 'function)
      "IMAGE-URL is the URL for an image and ALTERNATIVE-TEXT is the text to display when the image
   is not loaded.

  ACCEPT-COORDINATES-URL can be:
   
                * URL to which coordinates will be returned when the user
                clicks on the image.

                * T, indicating that returned coordinates should go to a
                search URL version of IMAGE-URL
                
                * :NO-URL, indicating not to emit an anchor for accepting the
                returns but to mark the IMG as a coordinate search.

   ALIGNMENT can be:

        HTML2 Arugments

                :TOP    -- align with the tallest item on the line.
                :MIDDLE -- align with the baseline with the middle of the image.
                :BOTTOM -- align with the baseline of the current line with the image.
        
        Text Flow Options

                :LEFT -- float down and over to the next available space on
                the left margin, and subsequent text wraps around the right
                side of the image.

                :RIGHT -- float down and over to the next available space on
                the right margin, and subsequent text wraps around the left
                side of the image.

        Semi-Random Options

                :TEXTTOP -- align the image top with the top of the current
                line.

                :ABSMIDDLE -- aling the middle of the image with the middle of
                the current line.

                :ABSBOTTOM -- align the image bottom with the bottom of the
                current line.

    BORDER is an integer indicating the thickness of the border with which to
    surround the image.

    VERTICAL-SPACE is an integer indicating the amount of vertical space
    above and below a floating image.

    HORIZONTAL-SPACE is an integer indicating the amount of horizontal space
    above and below a floating image.

    Allow browsers to layout the display before the image has loaded and thus
    eliminate the delay for the user otherwise incurred.

   WIDTH is width of the image in pixels.

   HEIGHT is the height of the image in pixels.

When IMAGE-URL is an interned URL on the local server and a method has been defined
to ascertain the size of the image, IMAGE will automatic emit the correct values
for height and width.  Any values passed in by the user will override the automatically
generated ones. This behavior is controlled by HTTP:*IMAGE-SIZES-DEFAULT-AUTOMATICALLY*.
Sizes are computed by URL:IMAGE-SIZE.")

;;;------------------------------------------------------------------- 
;;;
;;; 
;;;


#|

(defvar *table-test* '(foo bar baz)) 

(defmethod test-netscape-tables ((url url:http-computed-url) stream)
  (http:with-conditional-get-response (stream :html :expires (url:expiration-universal-time url))
    (let ((title "Test Netscape Tables"))
      (with-html-document (:stream stream)
        (with-document-preamble (:stream stream)
          (declare-title title :stream stream)
          (declare-default-font-size 4 :stream stream)
          #+ignore (declare-refresh-rate 1 :stream stream))
        (with-document-body (:background (random-color)
                             :foreground (random-color)
                             :link (random-color)
                             :visited-link (random-color)
                             :active-link (random-color)
                             :stream stream)
          (with-centering (:stream stream)
            (with-section-heading (title :stream stream)
              (multiple-value-bind (user-agent version)
                  (http:current-user-agent)
                (break-line :stream stream)
                (with-rendition (:bold :stream stream)
                  (format stream "User-Agent: ~S" user-agent)
                  (break-line :stream stream)
                  (format stream "Version: ~S" version)
                  (break-line :stream stream)))
              (with-blinking (:stream stream)
                (note-anchor "Try again" :reference url  :stream stream))
              (horizontal-line :alignment :center :width .75 :stream stream)
              (with-table (:caption "This is a test Table" :caption-alignment :bottom :caption-size 7
                                    :stream stream :border (random 5) :cell-padding (random 20))
                (with-table-row (:stream stream)
                  (with-rendition (:bold :stream stream)
                    (loop for item in *table-test*
                          do (with-table-cell (:header-p t :stream stream)
                               (with-font-size ((1+ (random 6)) :stream stream)
                                 (write-string (string item) stream))))))
                (loop for item in *table-test*
                      do (with-table-row (:stream stream)
                           (loop for item2 in *table-test*
                                 for size downfrom 6
                                 do (with-table-cell (:horizontal-alignment :center
                                                      :vertical-alignment :middle
                                                      :stream stream)
                                      (with-font-size (size :stream stream)
                                        item2   ; ignore
                                        (write-string (string item) stream)))))))
              (horizontal-line :alignment :center :size 3 :stream stream)))))))) 

(http:export-url #u"/test.html"
                 :html-computed
                 :response-function #'test-netscape-tables)
|#
