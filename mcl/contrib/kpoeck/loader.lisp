#|
Loader for Multithreading WWW-serving in MCL 2.0.1
Comments, complaints, Coffee-Mugs or T-Shirts too

 Karsten A. Poeck,
 poeck@informatik.uni-wuerzburg.de
 http://wi6a76.informatik.uni-wuerzburg.de/HTMLs/ls6-info/Assis/poeck/poeck.html


|#
(push
 (list "multi;*.*" (directory-namestring ccl:*loading-file-source-file*))
 (logical-pathname-translations "http")
 )


;;;;Mac Tcp Hacks, will be in ccl 3.0
#-:ccl-3 (compile-load "http:multi;stream-close") ;The final stream-close patch :-)
#-:ccl-3 (compile-load "http:multi;tcp-control") ;Otherwise %tcp-control will wait for a connection


;;;Base functionality for both servers
(compile-load "http:multi;serve")

;;; single thread servers
(compile-load "http:multi;simple-server")

#|
simple looping single thread server

start with (http::%start-serving)
stop with command-dot
|#


;;;Package for multithreading
(compile-load "http:multi;mlt-package")

;The simple *event-hook* based Multithreading
(compile-load "http:multi;timer-karsten")

;the MCL 2.0.1 Kernel base Multithreading
#+no (compile-load "http:multi;timer-kernel")
       
;;; the periodic server for either Multithreading package
(compile-load "http:multi;periodic-server")

#|
start with

(http::%start-periodic-server)

stop with

(http::%kill-serving)
|#
