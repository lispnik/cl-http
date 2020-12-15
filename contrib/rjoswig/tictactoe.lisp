;;; -*- Syntax: Ansi-Common-Lisp; Package: cl-user; Base: 10; Mode: lisp -*-

#|

Hi,

this is an attempt to see whether we can get some interactive VRML
examples. It is a simple TIC-TAC-TOE (the Lisp side
does only random moves). It exports #u"/new-game" and #u"/game?"
You'll need a VRML-capable browser (Netscape with Live3d was fine
for me).

It use also clickable items in the VRML world.

Remember, it is just a hack.

For a game use the URL:  /new-game .


|#

(in-package :cl-user)

(defclass game ()
  ((board :accessor game-board)
   (next-to-move :accessor game-next-to-move :initform :white)
   (id :accessor game-id)
   (lock :accessor game-lock :initform (make-lock))
   (move-number :accessor game-move-number :initform 0)
   (history :accessor game-history :initform '())))

(defparameter *games* (make-hash-table))


(defun make-game ()
  (let ((game (make-instance 'game)))
    (with-lock-grabbed ((game-lock game))
      (setf (game-board game) (make-array '(3 3) :initial-element :empty)
            (game-id game) (random 240828302348384082342344343)
            (gethash (game-id game) *games*) game)
      game)))

(defun find-game (id)
  (gethash id *games*))

(define-condition game-error (error)
   ((game :initarg :game :accessor game-error--game)))

(define-condition game-not-next-to-move (game-error)
  ((color :initarg :color :accessor game-error--color)))

(define-condition game-board-position-not-empty (game-error)
  ((color :initarg :color :accessor game-error--color)
   (x :initarg :x :accessor game-error--x)
   (y :initarg :y :accessor game-error--y)))

(defun opposite (color)
  (ecase color
    (:white :black)
    (:black :white)))

(defmethod make-move ((game game) x y color)
  (let ((result (with-lock-grabbed ((game-lock game))
                  (cond ((not (equal color (game-next-to-move game)))
                         (make-condition 'game-not-next-to-move :game game :color color))
                        ((not (equal :empty (aref (game-board game) x y)))
                         (make-condition 'game-board-position-not-empty :game game :color color :x x :y y))
                        (t (setf (aref (game-board game) x y) color
                                 (game-next-to-move game) (opposite (game-next-to-move game))
                                 (game-history game) (cons (list color x y) (game-history game))
                                 (game-move-number game) (1+ (game-move-number game))))))))
    
    (if (typep result 'game-error)
      (error result)
      result)))

(defmethod print-object ((game game) stream)
  (with-lock-grabbed ((game-lock game))
    (format stream "~%game id: ~a, move number: ~a, next to move: ~a~%"
            (game-id game)
            (game-move-number game)
            (game-next-to-move game))
    (loop for i below 3
          do (terpri stream)
          do (loop for j below 3
                   for nil = nil then (princ " " stream)
                   do (princ (aref (game-board game) i j) stream)))
    (terpri stream)))

(defmethod win-p ((game game) color)
  (let ((board (game-board game)))
    (loop for i below 3
          when (= (loop for j below 3
                        count (equal (aref board i j) color))
                  3)
          do (return-from win-p (list :row i)))
    (loop for j below 3
          when (= (loop for i below 3
                        count (equal (aref board i j) color))
                  3)
          do (return-from win-p (list :column j)))
    (when (equal (loop for i below 3
                       for j below 3
                       count (equal (aref board i j) color))
                 3)
      (return-from win-p (list :diagonal 0)))
    (when (equal (loop for i downfrom 2 downto 0
                       for j below 3
                       count (equal (aref board i j) color))
                 3)
      (return-from win-p (list :diagonal 1)))))

(defmethod win-p :around ((game game) color)
  (with-lock-grabbed ((game-lock game))
    (let ((win (call-next-method game color)))
      (when win
        (setf (game-history game)
              (cons (list :win color win) (game-history game))
              (game-next-to-move game) :end-of-game))
      win)))


#|

(defparameter *g1* (make-game))

(make-move *g1* 1 1 :white)
(make-move *g1* 1 1 :white)
(make-move *g1* 1 1 :black)


(defparameter *g1* (make-game))

(make-move *g1* 1 0 :white)
(win-p *g1* :white)
(make-move *g1* 2 0 :black)
(win-p *g1* :black)
(make-move *g1* 1 1 :white)
(win-p *g1* :white)
(make-move *g1* 2 1 :black)
(win-p *g1* :black)
(make-move *g1* 1 2 :white)
(win-p *g1* :white)


            
|#

(defmethod display-game-in-vrml ((game game) stream)
  (with-lock-grabbed ((game-lock game))
    (vrml1.0:with-vrml-world (:stream stream)
      (vrml1.0:with-separator-group (stream)
        (let ((board (game-board game)))
          (loop for x below 3
                do (loop for y below 3
                         for content = (aref board x y)
                         do (vrml1.0:with-separator-group (stream)
                              (vrml1.0:translation-node* stream x y 0)
                              (display-board-item content stream game x y)))))))))

(defmethod game-winner ((game game))
  (let ((item (first (game-history game))))
    (and item (equal (first item) :win) (second item))))
 
(defmethod display-board-item ((item (eql :empty)) stream game x y)
  (vrml1.0:let-fields ((color vrml1.0:color :red 1.0 :green 1.0 :blue 1.0))
    (vrml1.0:material-node stream :diffuse-color color))
  (if (equal (game-next-to-move game) :end-of-game)
    (vrml1.0:with-anchor-group (stream "new-game")
      (vrml1.0:cube-node stream :width 0.5 :height 0.5 :depth 0.05))
    (vrml1.0:with-anchor-group (stream (format nil "game?~a+~a+~a+~a"
                                               (game-id game)
                                               (game-move-number game)
                                               x y))
      (vrml1.0:cube-node stream :width 0.5 :height 0.5 :depth 0.05))))

(defmethod display-board-item ((item (eql :white)) stream game x y)
  (declare (ignore x y))
  (vrml1.0:let-fields ((color vrml1.0:color :red 0.2 :green 0.2 :blue 0.7))
    (if (and (equal (game-next-to-move game) :end-of-game))
      (if (equal item (game-winner game))
        (vrml1.0:let-fields ((ecolor vrml1.0:color :red 0.2 :green 0.2 :blue 0.7))         
          (vrml1.0:material-node stream
                                 :diffuse-color color
                                 :emissive-color ecolor))
        (vrml1.0:material-node stream
                               :diffuse-color color :transparency 0.8))
      (vrml1.0:material-node stream :diffuse-color color)))
  (vrml1.0:cube-node stream :width 0.6 :height 0.6 :depth 0.6))

(defmethod display-board-item ((item (eql :black)) stream game x y)
  (declare (ignore x y))
  (vrml1.0:let-fields ((color vrml1.0:color :red 0.2 :green 0.7 :blue 0.2))         
    (if (and (equal (game-next-to-move game) :end-of-game))
      (if (equal item (game-winner game))
        (vrml1.0:let-fields ((ecolor vrml1.0:color :red 0.2 :green 0.7 :blue 0.2))         
          (vrml1.0:material-node stream
                                 :diffuse-color color
                                 :emissive-color ecolor))
        (vrml1.0:material-node stream
                               :diffuse-color color :transparency 0.8))
      (vrml1.0:material-node stream :diffuse-color color)))
  (vrml1.0:cube-node stream :width 0.6 :height 0.6 :depth 0.6))

#|
(with-open-file (out "ccl:test.wrl" :direction :output :if-exists :supersede)
  (display-game-in-vrml *g1* out)
  (terpri out))

|#


(defmethod enumerate-possible-moves ((game game))
  (let ((board (game-board game)))
    (loop with moves = nil
          for i below 3
          do (loop for j below 3
                   when (equal :empty (aref board i j))
                   do (push (list i j) moves))
          finally (return moves))))
          

(defmethod computer-move ((game game))
  (let ((moves (enumerate-possible-moves game)))
    (when moves
      (let ((move (nth (random (length moves)) moves)))
        (make-move game (first move) (second move) (game-next-to-move game))))))



(defmethod respond-to-display-game ((url url:http-search) stream)
  (http:with-conditional-get-response (stream :vrml :expires (url:expiration-universal-time url))
    (with-slots (url:search-keys) url
      (let ((id (w3p:accept-from-string 'integer (first url:search-keys)))
            (move-number (w3p:accept-from-string 'fixnum (second url:search-keys)))
            (x (w3p:accept-from-string '(fixnum 0 2) (third url:search-keys)))
            (y (w3p:accept-from-string '(fixnum 0 2) (fourth url:search-keys))))
        (let ((game (find-game id)))
          (when game
            (let ((color (game-next-to-move game)))
              (make-move game x y color)
              (if (win-p game color)
                (display-game-in-vrml game stream)
                (progn
                  (computer-move game)
                  (win-p game (opposite color))
                  (display-game-in-vrml game stream))))))))))

(defmethod respond-to-new-game (url stream)
  (http:with-conditional-get-response (stream :vrml :expires (url:expiration-universal-time url))
    (let ((game (make-game)))
      (display-game-in-vrml game stream))))

(http:export-url #u"/new-game"
                 :computed
                 :response-function #'respond-to-new-game)


(http:export-url #u"/game?"
                 :search
                 :response-function #'respond-to-display-game)
